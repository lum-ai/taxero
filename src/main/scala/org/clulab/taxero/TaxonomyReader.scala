package org.clulab.taxero

import scala.io.Source
import scala.collection.mutable
import ai.lum.common.StringUtils._
import ai.lum.common.ConfigUtils._
import ai.lum.common.ConfigFactory
import ai.lum.common.TryWithResources.using
import ai.lum.odinson._
import ai.lum.odinson.lucene.search._
import ai.lum.odinson.utils.QueryUtils
import org.clulab.embeddings.{WordEmbeddingMap => Word2Vec}
import org.clulab.processors.fastnlp.FastNLPProcessor


case class Match(
  result: Seq[String],
  count: Int,
  evidence: Seq[Evidence],
)

case class MatchTokensAndSentence(tokens: Seq[String], sentence: Evidence)
case class Evidence(docID: Int, sentence: String)

case class ScoredMatch(
  query: Seq[String],
  result: Seq[String],
  count: Int,
  similarity: Double,
  score: Double, // this is the score used for ranking, probably made from count and similarity
  evidence: Seq[Evidence], // this is for storing the sentences the extractions come from
)

object TaxonomyReader {
  def fromConfig: TaxonomyReader = {
    val config = ConfigFactory.load()
    println("\tloaded config...")
    val extractorEngine = ExtractorEngine.fromConfig
    println("\textractor engine started...")
    val wordEmbeddings = new Word2Vec(config[String]("taxero.wordEmbeddings"))
    println("\tword embeddings loaded...")
    val numEvidenceDisplay = config.get[Int]("taxero.numEvidenceDisplay").getOrElse(3)
    new TaxonomyReader(extractorEngine, wordEmbeddings, numEvidenceDisplay)
  }
}

class TaxonomyReader(
  val extractorEngine: ExtractorEngine,
  val wordEmbeddings: Word2Vec,
  val numEvidenceDisplay: Int
) {

  val proc = new FastNLPProcessor

  def getRankedHypernyms(tokens: Seq[String], lemmatize: Boolean): Seq[ScoredMatch] = {
    val items = if (lemmatize) convertToLemmas(tokens) else tokens
    val extractors = mkHypernymExtractors(items, lemmatize)
    val matches = getMatches(extractors)
    rankMatches(items, matches)
  }

  def getRankedHyponyms(tokens: Seq[String], lemmatize: Boolean): Seq[ScoredMatch] = {
    val items = if (lemmatize) convertToLemmas(tokens) else tokens
    val extractors = mkHyponymExtractors(items, lemmatize)
    val matches = getMatches(extractors)
    rankMatches(items, matches)
  }

  def getRankedCohyponyms(tokens: Seq[String], lemmatize: Boolean): Seq[ScoredMatch] = {
    val items = if (lemmatize) convertToLemmas(tokens) else tokens
    val extractors = mkCohyponymExtractors(items, lemmatize)
    val matches = getMatches(extractors)
    rankMatches(items, matches)
  }

  def getExpandedHypernyms(pattern: Seq[String], n: Int, lemmatize: Boolean): Seq[ScoredMatch] = {
    // start query set with the provided query
    val allQueries = mutable.HashSet(pattern)
    // add the n closest cohyponyms to the query set
    allQueries ++= getRankedCohyponyms(pattern, lemmatize).take(n).map(_.result)
    // start an empty map for the hypernym candidate counts
    val hypernymCounts = new Consolidator
    // count hypernym candidates
    for {
      q <- allQueries
      m <- getRankedHypernyms(q, lemmatize)                 // getHypernyms changed to getRankedHypernyms
    } hypernymCounts.add(m.result, m.count, Nil)
    // add the heads of each hypernym to the results
    for (candidate <- hypernymCounts.keys) {
      hypernymCounts.add(getHead(candidate))
    }
    // add the head of the original pattern to the results
    hypernymCounts.add(getHead(pattern))
    // return scored hypernyms
    rankMatches(pattern, hypernymCounts.getMatches)
  }

  def executeGivenRules(tokens: Seq[String], rules: String, lemmatize: Boolean): Seq[ScoredMatch] = {
    val items = if (lemmatize) convertToLemmas(tokens) else tokens
    val extractors = mkExtractorsFromRules(items, rules, lemmatize)
    val matches = getMatches(extractors)
    rankMatches(items, matches)
  }

  def getMatches(extractors: Seq[Extractor]): Seq[Match] = {
    val matches = for (m <- extractorEngine.extractNoState(extractors)) yield getResultTokens(m)
    // count matches so that we can add them to the consolidator efficiently
    val groupedMatches = matches.toList
      .groupBy(_.tokens)
      // get the count of how many times this result appeared and all the sentences where it happened
      .mapValues(vs => (vs.length, vs.map(v => v.sentence)))
    // count matches and return them
    val consolidator = new Consolidator(proc)
    for ((tokens, (count, sentences)) <- groupedMatches.toSeq) {
      consolidator.add(tokens, count, sentences)
    }
    // return results
    consolidator.getMatches
  }

  def getResultTokens(mention: Mention): MatchTokensAndSentence = {
    val args = mention.arguments
    val m = args.get("result") match {
      // if there is a captured mention called "result" then that's the result
      case Some(matches) => matches.head
      // if there is no named result, then use the whole match as the result
      case None => mention
    }
    val tokens = extractorEngine.getTokensForSpan(m)
    val sentence = extractorEngine
      .getTokens(mention.luceneDocId, extractorEngine.displayField)
      .mkString(" ")
    MatchTokensAndSentence(tokens, Evidence(mention.luceneDocId, sentence))
  }

  def rankMatches(query: Seq[String], matches: Seq[Match]): Seq[ScoredMatch] = {
    matches
      .filterNot(m => m.result == query)
      .map(m => scoreMatch(query, m))
      .sortBy(-_.score)
  }

  def scoreMatch(query: Seq[String], m: Match): ScoredMatch = {
    val count = m.count
    val evidence = m.evidence.slice(0, numEvidenceDisplay)
    val similarity = 1e-4 + (1/(1+scala.math.exp(-(similarityScore(query, m.result)))))
    val score = scala.math.log1p(count) * similarity
    ScoredMatch(query, m.result, count, similarity, score, evidence)
  }

  def mkEmbedding(tokens: Seq[String]): Array[Double] = {
    wordEmbeddings.makeCompositeVector(tokens)
  }

  def getHead(tokens: Seq[String]): Seq[String] = {
    // FIXME this function is supposed to return the syntactic head of the provided tokens,
    // but it currently just returns the last token
    Seq(tokens.last)
  }

  def similarityScore(query: Seq[String], result: Seq[String]): Double = {
    // 2. get embedding for MWEs
    //    a. embedding of head word
    //    b. average of all word embeddings
    //    c. weighted average (more weight to the head)
    //    d. robert's model
    // 3. frequency * cosineSimilarity(emb(query), emb(result))
    val q = mkEmbedding(query)
    val r = mkEmbedding(result)
    Word2Vec.dotProduct(q, r)
  }

  // tokens changed to lemmas as the first argument  
  def mkLemmaPattern(lemmas: Seq[String]): String = {
//    println("LEMMAS: " + lemmas.mkString(" "))
    lemmas
      .map(x => s"[lemma=${QueryUtils.maybeQuoteLabel(x)}]")
      .mkString(" ")
    // for "other" "dogs" ---> ["other", "dog']
    // [lemma=other] [lemma=dog]
  }

  def mkNormPattern(tokens: Seq[String]): String = {
//    println("TOKENS: " + tokens.mkString(" "))
    tokens
      .map(x => s"[norm=${QueryUtils.maybeQuoteLabel(x)}]")
      .mkString(" ")
  }

  def mkHypernymExtractors(tokens: Seq[String], lemmatize: Boolean): Seq[Extractor] = {
    mkExtractorsFromFile(tokens, "hypernym-rules.yml", lemmatize)
  }

  def mkHyponymExtractors(tokens: Seq[String], lemmatize: Boolean): Seq[Extractor] = {
    mkExtractorsFromFile(tokens, "hyponym-rules.yml", lemmatize)
  }

  def mkCohyponymExtractors(tokens: Seq[String], lemmatize: Boolean): Seq[Extractor] = {
    mkExtractorsFromFile(tokens, "cohyponym-rules.yml", lemmatize)
  }

  /** gets the contents of a rule file, as well as a tokenized query
   *  and returns the corresponding odinson extractors
   */
  def mkExtractorsFromFile(tokens: Seq[String], rulefile: String, lemmatize: Boolean): Seq[Extractor] = {
    using (Source.fromResource(rulefile)) { rules =>
      mkExtractorsFromRules(tokens, rules.mkString, lemmatize)
    }
  }

  def mkExtractorsFromRules(tokens: Seq[String], rules: String, lemmatize: Boolean): Seq[Extractor] = {
    val variables = if (lemmatize) {
      Map("query" -> mkLemmaPattern(tokens))
    } else Map("query" -> mkNormPattern(tokens))
    extractorEngine.compileRuleString(rules.mkString, variables)
  }

  // --------------------------------------------

  def convertToLemmas(words: Seq[String]): Seq[String] = {
    val s = words.mkString(" ")
    val doc = proc.mkDocument(s)
    proc.tagPartsOfSpeech(doc) 
    proc.lemmatize(doc)
    doc.clear()
    val sentence = doc.sentences.head
    // return the lemmas
    sentence.lemmas.get
  }

}
