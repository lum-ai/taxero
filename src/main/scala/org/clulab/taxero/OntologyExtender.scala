package org.clulab.taxero

import java.io.{BufferedWriter, File, FileWriter}

import ai.lum.common.ConfigFactory
import ai.lum.common.ConfigUtils._
import com.typesafe.config.Config

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Failure, Success, Try}
import com.typesafe.scalalogging.LazyLogging

object OntologyExtender extends App with LazyLogging {
  // given a directory with ontology files, where each file has the ontology leaf header and a set of examples for the leaf of the ontology,
  // produces corresponding files with the example sets enriched with hyponyms and co-hyponyms


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
  val maxExamplesToAddPerOntologyLeaf = config.apply[Int]("ontologyExtender.maxExamplesToAddPerOntologyLeaf")

  // create directories and files
  outputDir.mkdir()
  val files = ontologyDirectory.listFiles()

  for (file <- files) {
    Try {
      val outfile = new File(outputDir, file.getName.replace(".txt", ".txt"))
      // retrieve existing examples
      val lines = Source.fromFile(file).getLines().toList
      val (header, examples) = lines.partition(_.startsWith("#"))

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
//        for (r <- results) if (isSimilarToLeafHeader(r, headerLemmas, similarityToHeaderTreshold)) resultsFromAllTerms.append(r)
        for (r <- results) if(isSimilarToLeafWithDecay(r, headerLemmasNested, similarityToHeaderTreshold)) resultsFromAllTerms.append(r)
        // cohyponym results
        val coResults = reader.getRankedCohyponyms(egAsSeq, lemmatize)
        for (r <-coResults) if(isSimilarToLeafWithDecay(r, headerLemmasNested, similarityToHeaderTreshold)) resultsFromAllTerms.append(r)

        if (manualEval) {
          val (singleWord, multiWord) = results.partition(_.query.length < 2)

          for (sw <- singleWord) if(isSimilarToLeafWithDecay(sw, headerLemmasNested, similarityToHeaderTreshold)) singleWordQueryResult.append(sw)
          for (mw <- multiWord) if(isSimilarToLeafWithDecay(mw, headerLemmasNested, similarityToHeaderTreshold)) multiWordQueryResult.append(mw)

          val (singleWordCohyp, multiWordCohyp) = coResults.partition(_.query.length < 2)
          for (sw <- singleWordCohyp) if(isSimilarToLeafWithDecay(sw, headerLemmasNested, similarityToHeaderTreshold))  singleWordQueryResult.append(sw)
          for (mw <- multiWordCohyp) if(isSimilarToLeafWithDecay(mw, headerLemmasNested, similarityToHeaderTreshold)) multiWordQueryResult.append(mw)
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
//        bw.write(lines.head)
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
        bw.write(getStringToWrite(sortedResults, maxExamplesToAddPerOntologyLeaf))
        bw.close()
      }

    } match {
      case Success(_) => logger.info(s"extended ontology leaf ${file.getName}")
      case Failure(e) => logger.error(s"failed to extend ontology leaf ${file.getName}", e)
    }
  }

  def isSimilarToLeafHeader(result: ScoredMatch, header: Seq[String], similarityToHeaderTreshold: Double): Boolean = {
    // todo: When calculating the embedding for the name/header of the leaf, add decay, that is decrease the weight of the nodes on the path as we move up the ontology (decrease node weight by, for example, half every step up the ontology)
    reader.similarityScore(result.result.map(_.toLowerCase()), header) > similarityToHeaderTreshold
  }

  def decayingScore(result: ScoredMatch, header: Seq[Seq[String]]): Double = {
    val scoreWeights = new ArrayBuffer[Double]()
    var highestScore = 1.0
    scoreWeights.append(highestScore)
    for (i <- 1 to header.length-1) {
      //      println("->>" + i)
      val nextScore = highestScore / 2
      scoreWeights.append(nextScore)
      highestScore = nextScore
    }
    //    for (sc <- scoreWeights.reverse) println(sc)

    var overallScore = 0.0
    val scoreWeightsReversed = scoreWeights.reverse
    for ((node, ind) <- header.zipWithIndex) {
      overallScore += reader.similarityScore(result.result.map(_.toLowerCase()), node) * scoreWeightsReversed(ind)
      println("node: " + node.mkString("|"))
      println("sim score: " + reader.similarityScore(result.result.map(_.toLowerCase()), node))
      println("multiplier: " + scoreWeightsReversed(ind))
      println("added score" + reader.similarityScore(result.result.map(_.toLowerCase()), node) * scoreWeightsReversed(ind))


    }
    println("SCORE: " + overallScore)
    overallScore

  }

  def isSimilarToLeafWithDecay(result: ScoredMatch, header: Seq[Seq[String]], similarityToHeaderThreshold: Double): Boolean = {
    val score = decayingScore(result, header)
    score > similarityToHeaderThreshold

//    println("SCORE: " + overallScore)
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

    val toReturn = fullHeader
      .replace("#", "").replace(" ", "")
      .split("/")
      .map(_.split("And|Or|(?=[A-Z])").filter(_.length >0).map(_.toLowerCase()).toSeq)
//    println(toReturn)
//      .map(_.split("/").tail.mkString("/"))
//      .map(_.split("And|Or|(?=[A-Z])"))
    //      .filter(_!="/").map(_.toLowerCase()))
//      .map(reader.convertToLemmas(_))

//    for (tr <- toReturn) println("=>" + tr.mkString("|"))
//        .map(_.split("And|Or|(?=[A-Z])"))
//        .mkString("/")
//        .map()
//        .split("And|Or|(?=[A-Z])")
//        .map(_.toLowerCase()).distinct
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

}
