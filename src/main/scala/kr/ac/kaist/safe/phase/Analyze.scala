/**
 * *****************************************************************************
 * Copyright (c) 2016-2018, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.phase
import scala.io.Source
import kr.ac.kaist.safe.SafeConfig
import kr.ac.kaist.safe.analyzer._
import kr.ac.kaist.safe.analyzer.console.{Console, Interactive, WebConsole}
import kr.ac.kaist.safe.analyzer.domain.CKeyObject.NMap
import kr.ac.kaist.safe.analyzer.domain.{AbsObj, AbsState}
import kr.ac.kaist.safe.analyzer.domain.DefaultHeap.HeapMap
import kr.ac.kaist.safe.analyzer.html_debugger.HTMLWriter
import kr.ac.kaist.safe.nodes.cfg.CFG
import kr.ac.kaist.safe.util._
import kr.ac.kaist.safe.web.WebServer
import kr.ac.kaist.safe.analyzer.model.GLOBAL_LOC

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Success, Try}

import com.google.gson.Gson
import java.io._



case class JSONOutput(key: String, value: String)
// Analyze phase
// Heap has been constructed already.
case object Analyze extends PhaseObj[(CFG, Semantics, TracePartition, HeapBuildConfig, Int), AnalyzeConfig, (CFG, Int, TracePartition, Semantics)] {
  val name: String = "analyzer"
  val help: String = "Analyze JavaScript source files."

  def apply(
    in: (CFG, Semantics, TracePartition, HeapBuildConfig, Int),
    safeConfig: SafeConfig,
    config: AnalyzeConfig
  ): Try[(CFG, Int, TracePartition, Semantics)] = {
    val (cfg, sem, initTP, heapConfig, iter) = in

    var interOpt: Option[Interactive] =
      if (config.console) Some(new Console(cfg, sem, heapConfig, iter))
      else None

    // calculate fixpoint
    val fixpoint =
      if (config.timeLog) new FixpointTime(sem, interOpt, "block.csv", "func.csv")
      else new Fixpoint(sem, interOpt)
    val (iters, duration, pointsToSet) = fixpoint.compute(iter + 1)

    // display duration time
    if (config.time) {
      println(s"iteration number: $iters")
      println(f"The analysis took $duration%.9f s.")
    }

    // Report errors.
    val excLog = sem.excLog
    if (!safeConfig.testMode && excLog.hasError) {
      println(cfg.fileName + ":")
      println(excLog)
    }

    // print html file: {htmlName}.html
    val exitCP = ControlPoint(cfg.globalFunc.exit, initTP)
    config.htmlName.foreach(name => {
      HTMLWriter.writeHTMLFile(cfg, sem, None, s"$name.html")
    })

    // dump exit state
    if (config.exitDump) {
      val state = sem.getState(exitCP)
      println(state.toString)
    }

    // pointer analysis

    pointerAnalysis(cfg, sem.getState(exitCP), pointsToSet, config.ptrSetFile.get, safeConfig.fileNames(0))

    Success((cfg, iters, initTP, sem))
  }

  def reprPointsTo(locSet: domain.LocSet) : String = {

    var list = new mutable.StringBuilder("")

    locSet.foreach(loc => {
      list = (list ++= "'" + loc.toString + "', ")
    })
    list = list.dropRight(2)
    list.toString()
  }

  def writeToFile(list: List[JSONOutput]): Unit = {
    val pw = new PrintWriter(new File("safeOutput.json" ))
    pw.write("{ ")

    for((x,i) <- list.view.zipWithIndex) {
      pw.write("\"(".concat(x.key) + ")\"")
      pw.write(":")
      pw.write("\"[".concat(x.value) + "]\"")
      if (i != list.size -1){
        pw.write(", ")
      }
    }
    pw.write(" }")
    pw.close()
  }

  def pointerAnalysis(cfg: CFG, absHeap: AbsState, pointsToSet: mutable.Map[(String, Int), domain.LocSet], ptrSetFile: String, srcFile: String): Unit = {

    // Need all the variables
    //    val userVars = cfg.getUserVars
    //
    //    val locs = absHeap.heap.getLocSet
    //    val userLocs = locs.filter(loc => loc.isUser || loc == GLOBAL_LOC)
    //
    //    val globalLocs = locs.filter(loc => loc == GLOBAL_LOC)
    //
    //    val pointsToMap = userLocs.map(loc => {
    //      absHeap.heap.getMap.get(loc)
    //    })
    //    //  => map.get(loc).map(toStringLoc(loc, _, isConcrete(loc)))
    //    var nmap : NMap = null
    //    globalLocs.foreach(loc => {
    ////      absHeap.heap.getLocInfo(loc, absHeap.heap)
    //      val locations = absHeap.heap.get(loc)
    //      nmap = locations.nmap
    //    })
    //
    ////    nmap.map
    //    userVars.foreach(uservar => {
    //      val pointsTo = nmap.map.get(uservar.text)
    //      println(s"points to size of $uservar:")
    //      println(s"${pointsTo}")
    //    })
    //
    //
    ////    userVars.foreach(v => {
    ////      globalVar.asInstanceOf[AbsObj].nmap.map.get(v.text)
    ////    })
    //
    //    println(userLocs.toString)

    //

    //    println("Points to set")
    val source = Source.fromFile(ptrSetFile)
    var ptrSet = new ListBuffer[(String, Int)]()
    val gson = new Gson
    var map = scala.collection.mutable.Map[String, String]()
    var list = List.empty[(JSONOutput)]

    for (line <- source.getLines()){
      val refinedLine = line.split(" ")
      ptrSet += ((refinedLine(0),refinedLine(1).toInt))
    }

    ptrSet.foreach(i => {
      var pointsTo = domain.LocSet()
      var pointsToStr = ""
      try {
        pointsTo = pointsToSet.get(i).get
        pointsToStr = reprPointsTo(pointsTo)
        println("Variable name: ".concat(i._1) + ", Line number:  " + i._2 )
        println("Points to Set: ".concat(pointsTo.toString))
        list = JSONOutput(srcFile + "-" + i._1 + "-" + i._2, pointsToStr):: list
      }catch{
        case _: Throwable => {
          println("Invalid variable & line number combination provided: " + i._1 + " " + i._2)
          list = JSONOutput(srcFile + "-" + i._1 + "-" + i._2, pointsToStr) :: list
        }
      }


    })

//    val jsonString = gson.toJson(list)
//    val pw = new PrintWriter(new File("safeOutput.json" ))
//    pw.write(jsonString)
//    pw.close
    writeToFile(list)




    sys.exit(1)

    //    while(true) {
    //      println("Enter variable name followed by line number to prints its points to set")
    //      val name = scala.io.StdIn.readLine("Variable name? ")
    //      println("Line Number? ")
    //      val lineNumber = scala.io.StdIn.readInt()
    //
    //      val pointsTo = pointsToSet.get((name, lineNumber))
    //
    //      pointsTo match {
    //        case Some(value) => println("Points to Set: ".concat(pointsTo.toString))
    //        case None => println("Error in key")
    //      }
    //
    //    }

  }

  def defaultConfig: AnalyzeConfig = AnalyzeConfig()
  val options: List[PhaseOption[AnalyzeConfig]] = List(
    ("silent", BoolOption(c => c.silent = true),
      "messages during analysis are muted."),
    ("console", BoolOption(c => c.console = true),
      "REPL-style console debugger."),
    ("time", BoolOption(c => c.time = true),
      "display duration time."),
    ("time-log", BoolOption(c => c.timeLog = true),
      "log duration time for each function."),
    ("exitDump", BoolOption(c => c.exitDump = true),
      "dump the state of the exit state of a given CFG"),
    ("out", StrOption((c, s) => c.outFile = Some(s)),
      "the analysis results will be written to the outfile."),
    ("html", StrOption((c, s) => c.htmlName = Some(s)),
      "the resulting CFG with states will be drawn to the {string}.html"),
    ("pointer", StrOption((c, s) => c.pointer = Some(s)),
      "provide variable name to compute its points to set"),
    ("line", NumOption((c, s) => c.line = Some(s)),
      "provide line number for pointer analysis"),
    ("ptrSetFile", StrOption((c, s) => c.ptrSetFile = Some(s)),
      "provide line number for pointer analysis"),
  )
}

// Analyze phase config
case class AnalyzeConfig(
  var silent: Boolean = false,
  var console: Boolean = false,
  var time: Boolean = false,
  var timeLog: Boolean = false,
  var exitDump: Boolean = false,
  var outFile: Option[String] = None,
  var htmlName: Option[String] = None,
  var pointer: Option[String] = None,
  var line: Option[Int] = None,
  var ptrSetFile: Option[String] = None,
) extends Config
