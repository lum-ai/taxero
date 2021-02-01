package org.clulab.taxero

import java.io.{BufferedWriter, File, FileWriter}

import ai.lum.common.ConfigFactory
import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Failure, Success, Try}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable

object OntologyExtender extends App with LazyLogging {
  // given a directory with ontology files, where each file has one or more ontology leaf paths as headers (e.g., Event/HealthAndDisease/Illness) and a set of examples for the leaf of the ontology, produces corresponding files with the example sets enriched with hyponyms and co-hyponyms


  // get the reader
  val config: Config = ConfigFactory.load()
  val reader = TaxonomyReader.fromConfig

  // get mode
  val manualEval = config.apply[Boolean]("ontologyExtender.manualEval")
  // get dirs
  val ontologyDirectory = config.apply[File]("ontologyExtender.ontologyDir")
  val outputDir = config.apply[File]("ontologyExtender.outputDir")

  // get settings
  val similarityToHeaderTreshold = config.apply[Double]("ontologyExtender.similarityToHeaderTreshold")
  val inclOriginalLeaf = config.apply[Boolean]("ontologyExtender.inclOriginalLeaf")
  val lemmatize = config.apply[Boolean]("ontologyExtender.lemmatize")
  val onlyQueryByLeafHeaderTerms = config.apply[Boolean]("ontologyExtender.onlyQueryByLeafHeaderTerms")
  val countThreshold = config.apply[Int]("ontologyExtender.countThreshold")
  val addExamplesProportionallyToCurrentNum = config.apply[Boolean]("ontologyExtender.addExamplesProportionallyToCurrentNum")
  val proportionToExpandBy = config.apply[Double]("ontologyExtender.proportionToExpandBy")
  val maxExamplesToAddPerOntologyLeafDefault = config.apply[Int]("ontologyExtender.maxExamplesToAddPerOntologyLeaf")

  // create directories and files
  outputDir.mkdir()
  val files = ontologyDirectory.listFiles()

  val (termToLeaf, allHeaders) = getTermToLeafMap(ontologyDirectory)

  var examplesAddedPerFile = 0
  var numOfFilesSucceeded = 0
  for (file <- files) {
    Try {
      val outfile = new File(outputDir, file.getName.replace(".txt", ".csv"))
      // retrieve existing examples
      val source = Source.fromFile(file)
      val lines = source.getLines().toList
      source.close()
      val (header, examples) = lines.partition(_.startsWith("#"))
      val existingExampleLemmas = examples.flatMap(_.split(" "))
      val maxExamplesToAddPerOntologyLeaf = if (addExamplesProportionallyToCurrentNum) (examples.length * proportionToExpandBy).toInt else maxExamplesToAddPerOntologyLeafDefault

      // only the last item on the path in the header is used for querying,
      // but there could be multiple header lines in one ontology file
      // fixme: does not handle conj-linked lists of terms that include compounds, e.g., smallCatsLargeDogsAndOtherPets
      val headerQueries = header
        .map(_.split("/").last)
        .mkString("/")
        .split("And|Or") // catsDogsAndBirds => cats, Dogs, Birds
        .mkString("/")
        .split("(?=[A-Z])")
        .mkString(" ")
        .toLowerCase()
        .split("/ ")
        .distinct

      // lemmas in the header - used for filtering examples unrelated to the ontology leaf; use full header, not only the last item in the header because while some terms there are to generic to use for queries, they help to narrow down the general topic of the ontology leaf
      val headerLemmas = getHeaderLemmas(header)
      val headerLemmasNested = getHeaderLemmasNonFlat(header.head) // just take the first header

      val queries = if (onlyQueryByLeafHeaderTerms) {
        headerQueries
      } else {
        (headerQueries ++ examples).distinct
      }

      val resultsFromAllTerms = new ArrayBuffer[ScoredMatch]()
      val singleWordQueryResult = new ArrayBuffer[ScoredMatch]()
      val multiWordQueryResult = new ArrayBuffer[ScoredMatch]()


      for (eg <- queries.filter(_.length > 0).map(_.toLowerCase())) {
        val egAsSeq = eg.split(" ")
        // hyponym results
        val results = reader.getRankedHyponyms(egAsSeq, lemmatize)
        for (r <- results) if(isSimilarToLeafWithDecay(r, headerLemmasNested, similarityToHeaderTreshold) && seenMoreThanK(r, countThreshold)) resultsFromAllTerms.append(r)
        // cohyponym results
        val coResults = reader.getRankedCohyponyms(egAsSeq, lemmatize)
        for (r <-coResults) if(isSimilarToLeafWithDecay(r, headerLemmasNested, similarityToHeaderTreshold) && seenMoreThanK(r, countThreshold)) resultsFromAllTerms.append(r)

        if (manualEval) {
          val (singleWord, multiWord) = results.partition(_.query.length < 2)

          for (sw <- singleWord) if(isSimilarToLeafWithDecay(sw, headerLemmasNested, similarityToHeaderTreshold) && seenMoreThanK(sw, countThreshold)) singleWordQueryResult.append(sw)
          for (mw <- multiWord) if(isSimilarToLeafWithDecay(mw, headerLemmasNested, similarityToHeaderTreshold) && seenMoreThanK(mw, countThreshold)) multiWordQueryResult.append(mw)

          val (singleWordCohyp, multiWordCohyp) = coResults.partition(_.query.length < 2)
          for (sw <- singleWordCohyp) if(isSimilarToLeafWithDecay(sw, headerLemmasNested, similarityToHeaderTreshold) && seenMoreThanK(sw, countThreshold))  singleWordQueryResult.append(sw)
          for (mw <- multiWordCohyp) if(isSimilarToLeafWithDecay(mw, headerLemmasNested, similarityToHeaderTreshold) && seenMoreThanK(mw, countThreshold)) multiWordQueryResult.append(mw)
        }
      }

      // used for filtering out results that match queries
      val cleanQueries = queries.filter(_.length > 0).map(_.toLowerCase())

      val sortedResults = if (manualEval) {
        returnResultsForManualEval(resultsFromAllTerms, cleanQueries, headerLemmasNested)
      } else {
        resultsFromAllTerms.filter(res => !cleanQueries.contains(res.result.mkString(" ").toLowerCase)).sortBy(-_.score).map(res => res.result.mkString(" ") + "\n")
      }

      val sortedResultsSingleWord = if (manualEval) {
        returnResultsForManualEval(singleWordQueryResult, cleanQueries, headerLemmasNested)
      } else Seq.empty

      val sortedResultsMultiWord = if (manualEval) {
        returnResultsForManualEval(multiWordQueryResult, cleanQueries, headerLemmasNested)
      } else Seq.empty


      val bw = new BufferedWriter(new FileWriter(outfile))
      if (inclOriginalLeaf) {
        bw.write(header.mkString("\n"))
        bw.write("\n" + examples.filter(_.length > 0).mkString("\n"))
        bw.write("\n")
      }

      if (manualEval) {
        bw.write("\nresult\tquery\tscore\tsimilarity\tsimilarity_to_header\trelevant\tp@5\tp@10\n")
        bw.write("ALL QUERIES:\n")
        bw.write(getStringToWrite(sortedResults, maxExamplesToAddPerOntologyLeaf))
        bw.write("SINGLE-WORD QUERIES:\n")
        bw.write(getStringToWrite(sortedResultsSingleWord, maxExamplesToAddPerOntologyLeaf))
        bw.write("MULTI-WORD QUERIES:\n")
        bw.write(getStringToWrite(sortedResultsMultiWord, maxExamplesToAddPerOntologyLeaf))
        bw.close()
      } else {
        bw.write(getStringToWriteDistinctTokens(sortedResults, maxExamplesToAddPerOntologyLeaf, header.head, allHeaders, existingExampleLemmas))
        bw.close()
      }

    } match {
      case Success(_) => {
        logger.info(s"extended ontology leaf ${file.getName}")
        numOfFilesSucceeded += 1
      }

      case Failure(e) => logger.error(s"failed to extend ontology leaf ${file.getName}", e)
    }

  }

  logger.info(s"FILES SUCCEEDED:\t $numOfFilesSucceeded")

  def seenMoreThanK(result: ScoredMatch, k: Int): Boolean = {
    result.count > k
  }

  def lemmaAlreadyExistsInExamples(token: String, examples: Seq[String]) = {
    examples.contains(reader.convertToLemmas(Seq(token)).head)
  }

  def isSimilarToLeafHeader(result: ScoredMatch, header: Seq[String], similarityToHeaderTreshold: Double): Boolean = {
    reader.similarityScore(result.result.map(_.toLowerCase()), header) > similarityToHeaderTreshold
  }

  def decayingScore(result: ScoredMatch, header: Seq[Seq[String]]): Double = {
    val scoreWeights = new ArrayBuffer[Double]()
    var highestScore = 1.0
    scoreWeights.append(highestScore)
    for (i <- 1 to header.length-1) {
      val nextScore = highestScore / 2
      scoreWeights.append(nextScore)
      highestScore = nextScore
    }

    var overallScore = 0.0
    val scoreWeightsReversed = scoreWeights.reverse
    for ((node, ind) <- header.zipWithIndex) {
      overallScore += reader.similarityScore(result.result.map(_.toLowerCase()), node) * scoreWeightsReversed(ind)
    }
    overallScore

  }

  def isSimilarToLeafWithDecay(result: ScoredMatch, header: Seq[Seq[String]], similarityToHeaderThreshold: Double): Boolean = {
    val score = decayingScore(result, header)
    score > similarityToHeaderThreshold
  }

  def getTermToLeafMap(ontologyDirPath: File): (Map[String, ArrayBuffer[String]], Seq[String]) = {
    // map every token in existin examples to all the ontology leaf paths/headers it occures in
    val files = ontologyDirPath.listFiles()
    val termToHeaderMap = mutable.Map[String, ArrayBuffer[String]]()
    val allHeaders = new ArrayBuffer[String]()

    for (file <- files) {
      Try {
        val source = Source.fromFile(file)
        val lines = source.getLines().toList
        source.close()
        val (header, examples) = lines.partition(_.startsWith("#"))
        allHeaders.append(header.head)
        val distinctExamples = examples.flatMap(eg => eg.split(" ")).distinct
        for (de <- distinctExamples) {
          if (termToHeaderMap.contains(de)) {
            termToHeaderMap(de).append(header.head)
          } else {
            termToHeaderMap(de) = new ArrayBuffer[String]()
            termToHeaderMap(de).append(header.head)
          }
        }
      }  match {
      case Success(_) => None
      case Failure(e) => logger.error(s"failed to get examples from ${file.getName}", e)
    }


    }
    (termToHeaderMap.toMap, allHeaders)
  }

  def existsInOtherLeaves(currentTerm: String, termToLeaves: Map[String, Seq[String]], topHeader: String): Boolean = {
    if (!termToLeaves.contains(currentTerm)) return false // the term does not exist as an example in any of the ontology leaves
    if (termToLeaves(currentTerm).length > 1) return true // the term exists in more than one ontology leaf
    if (termToLeaves(currentTerm).mkString("/") != topHeader) return true // there exists one leaf with the current term as an example and it is not the current ontology leaf (=topHeader)
    false
  }

  def findMostSimilarHeader(token: String, otherNodeHeaders: Seq[String]): String = {
    val scores = new ArrayBuffer[Double]()
    for (h <- otherNodeHeaders) {
      val headerLemmas = getHeaderLemmas(Seq(h))
      val score = reader.similarityScore(Seq(token), headerLemmas)
      scores.append(score)
    }
    val scoresWithIdx = scores.zipWithIndex
    val sortedScoresWithIdx = scoresWithIdx.sortBy(_._1).reverse
    val maxHeader = otherNodeHeaders(sortedScoresWithIdx.head._2)
    maxHeader

  }

  def mostSimilarToCurrentLeaf(token: String, currentHeader: String, otherNodeHeaders: Seq[String]): Boolean = {
    //- in distinct result terms, if it's most similar to current header, return true and keep the term; currently not used---filtering too aggressive
    val maxHeader = findMostSimilarHeader(token, otherNodeHeaders)
    if (maxHeader == currentHeader) return true else return false

  }

  // not currently used
  def checkIfQueryIsRelevant(results: Seq[ScoredMatch], fullHeader: Seq[String]): Boolean = {

    val hypernymResults = for {
      res <- results.slice(0,3)
      hypernymRes = reader.getRankedHypernyms(res.result, lemmatize)
      if hypernymRes.nonEmpty
    } yield hypernymRes.head.result

    if (hypernymResults.nonEmpty) {
      val hypernymTermsLemmatized = reader.convertToLemmas(hypernymResults.flatten)
      // just get distinct words from the header
      val headerLemmas = getHeaderLemmas(fullHeader)
      if (headerLemmas.intersect(hypernymTermsLemmatized).nonEmpty) return true else return false
    } else return false
  }

  def getHeaderLemmas(fullHeader: Seq[String]): Seq[String] = {
    reader.convertToLemmas(fullHeader
      .map(_.split("/").tail.mkString("/")) // exclude generic term Event
      .mkString("/") // combine all header lines (multiples possible)
      .split("And|Or") // split on conj
      .mkString("/") // put back together with "/"
      .split("(?=[A-Z])") // split on caps to get words in compounds
      .mkString("/")
      .toLowerCase()
      .split("/").distinct)
  }

  def getHeaderLemmasNonFlat(fullHeader: String): Seq[Seq[String]] = {
    // used for decaying similarity score to the header, e.g., in a path #Events/IllnessAndDisease, the similarity score weight decay will be applied to Event, but not to Illness and Disease
    // as the latter two are at the same level in the ontology
    val toReturn = fullHeader
      .replace("#", "").replace(" ", "")
      .split("/")
      .map(_.split("And|Or|(?=[A-Z])").filter(_.length >0).map(_.toLowerCase()).toSeq)
    toReturn.toSeq
  }

  def returnResultsForManualEval(results: Seq[ScoredMatch], cleanQueries: Seq[String], headerLemmas: Seq[Seq[String]]): Seq[String] = {
    results.filter(
       res => !cleanQueries.contains(res.result.mkString(" ").toLowerCase))
       .sortBy(-_.score)
       .map(res => res.result.mkString(" ") + "\t" + res.query.mkString(" ") + "\t" + res.score.toString + "\t" + res.similarity.toString + "\t" + decayingScore(res, headerLemmas).toString + "\n")
  }

  def getStringToWrite(results: Seq[String], maxExamplesToAddPerOntologyLeaf: Int): String = {
    val string = results.distinct.filter(_.length > 0).slice(0, maxExamplesToAddPerOntologyLeaf).mkString("")
    string
  }

  def getStringToWriteDistinctTokens(results: Seq[String], maxExamplesToAddPerOntologyLeaf: Int, topHeader: String, otherHeaders: Seq[String], existingExampleLemmas: Seq[String]): String = {
    // some of the filtering is only applied while writing the examples to ontology leaf files - for manual eval we keep the collocation results from taxero intact
    val string = results.distinct.filter(_.length > 0)
      .flatMap(res => res.replace("\n","").split(" ")).distinct
      .filter(term => !lemmaAlreadyExistsInExamples(term, existingExampleLemmas))
      .filter(term => term.toLowerCase() == term)
//      .filter(term => mostSimilarToCurrentLeaf(term, topHeader, otherHeaders))
      .filter(term => !existsInOtherLeaves(term, termToLeaf, topHeader))
      .slice(0, maxExamplesToAddPerOntologyLeaf).mkString("\n")
    logger.info(s"num of nodes added:\t ${string.split("\n").length} for leaf: $topHeader")
    string
  }

}
