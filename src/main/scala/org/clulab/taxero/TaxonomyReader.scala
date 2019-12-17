package org.clulab.taxero

import scala.io.Source
import scala.collection.mutable
import ai.lum.odinson._
import ai.lum.odinson.lucene.search._
import ai.lum.common.StringUtils._
import ai.lum.common.ConfigUtils._
import ai.lum.common.ConfigFactory
import ai.lum.common.TryWithResources.using
import org.clulab.embeddings.word2vec.Word2Vec

case class Match(
  result: Seq[String],
  count: Int,
)

case class ScoredMatch(
  query: Seq[String],
  result: Seq[String],
  count: Int,
  score: Double,
)

object TaxonomyReader {
  def fromConfig: TaxonomyReader = {
    val config = ConfigFactory.load()
    val extractorEngine = ExtractorEngine.fromConfig
    val wordEmbeddings = new Word2Vec(config[String]("taxero.wordEmbeddings"))
    val contextEmbeddings = new Word2Vec(config[String]("taxero.contextEmbeddings"))
    new TaxonomyReader(extractorEngine, wordEmbeddings, contextEmbeddings)
  }
}

class TaxonomyReader(
  val extractorEngine: ExtractorEngine,
  val wordEmbeddings: Word2Vec,
  val contextEmbeddings: Word2Vec,
) {

  def getHypernyms(tokens: Seq[String]): Seq[Match] = {
    getMatches(mkHypernymQueries(tokens))
  }

  def getRankedHypernyms(tokens: Seq[String]): Seq[ScoredMatch] = {
    rankMatches(tokens, getHypernyms(tokens))
  }

  def getHyponyms(tokens: Seq[String]): Seq[Match] = {
    getMatches(mkHyponymQueries(tokens))
  }

  def getRankedHyponyms(tokens: Seq[String]): Seq[ScoredMatch] = {
    rankMatches(tokens, getHyponyms(tokens))
  }

  def getCohyponyms(tokens: Seq[String]): Seq[Match] = {
    getMatches(mkCohyponymQueries(tokens))
  }

  def getRankedCohyponyms(tokens: Seq[String]): Seq[ScoredMatch] = {
    rankMatches(tokens, getCohyponyms(tokens))
  }

  def getExpandedHypernyms(pattern: Seq[String], n: Int): Seq[ScoredMatch] = {
    // start query set with the provided query
    val allQueries = mutable.HashSet(pattern)
    // add the n closest cohyponyms to the query set
    allQueries ++= getRankedCohyponyms(pattern).take(n).map(_.result)
    // start an empty map for the hypernym candidate counts
    val hypernymCounts = new Counter
    // count hypernym candidates
    for {
      q <- allQueries
      m <- getHypernyms(q)
    } hypernymCounts.add(m.result, m.count)
    // add the heads of each hypernym to the results
    for (candidate <- hypernymCounts.keys) {
      hypernymCounts.add(getHead(candidate))
    }
    // add the head of the original pattern to the results
    hypernymCounts.add(getHead(pattern))
    // return scored hypernyms
    rankMatches(pattern, hypernymCounts.getMatches)
  }

  def getMatches(queries: Seq[OdinsonQuery]): Seq[Match] = {
    // get matches
    val matches = for {
      query <- queries
      results = extractorEngine.query(query)
      scoreDoc <- results.scoreDocs
      odinsonMatch <- scoreDoc.matches
      result = extractorEngine.getTokens(scoreDoc.doc, odinsonMatch)
    } yield result.toSeq
    // count matches and return them
    val counter = new Counter
    matches.foreach(counter.add)
    counter.getMatches
  }

  def rankMatches(query: Seq[String], matches: Seq[Match]): Seq[ScoredMatch] = {
    matches
      .map(m => scoreMatch(query, m))
      .sortBy(-_.score)
  }

  def scoreMatch(query: Seq[String], m: Match): ScoredMatch = {
    ScoredMatch(query, m.result, m.count, similarityScore(query, m.result, m.count))
  }

  def mkEmbedding(tokens: Seq[String]): Array[Double] = {
    wordEmbeddings.makeCompositeVector(tokens)
  }

  def getHead(tokens: Seq[String]): Seq[String] = {
    Seq(tokens.last)
  }

  def similarityScore(query: Seq[String], result: Seq[String], freq: Double = 1): Double = {
    // 2. get embedding for MWEs
    //    a. embedding of head word
    //    b. average of all word embeddings
    //    c. weighted average (more weight to the head)
    //    d. robert's model
    // 3. frequency * cosineSimilarity(emb(query), emb(result))
    val q = mkEmbedding(query)
    val r = mkEmbedding(result)
    freq * Word2Vec.dotProduct(q, r)
  }

  def mkPattern(tokens: Seq[String]): String = {
    tokens.map(t => "\"" + t.escapeJava + "\"").mkString(" ")
  }

  def mkHypernymQueries(tokens: Seq[String]): Seq[OdinsonQuery] = {
    mkQueries(tokens, "hypernym-rules.txt")
  }

  def mkHyponymQueries(tokens: Seq[String]): Seq[OdinsonQuery] = {
    mkQueries(tokens, "hyponym-rules.txt")
  }

  def mkCohyponymQueries(tokens: Seq[String]): Seq[OdinsonQuery] = {
    mkQueries(tokens, "cohyponym-rules.txt")
  }

  def mkQueries(tokens: Seq[String], rulefile: String): Seq[OdinsonQuery] = {
    using (Source.fromResource(rulefile)) { rules =>
      val variables = Map(
        "query" -> mkPattern(tokens),
        "chunk" -> "( [tag=/J.*/]{,3} [tag=/N.*/]+ (of [tag=DT]? [tag=/J.*/]{,3} [tag=/N.*/]+)? )",
      )
      rules.mkString
        .replaceVariables(variables)
        .split("""\s*\n\s*\n\s*""")
        .map(extractorEngine.compiler.compile)
    }
  }

}
