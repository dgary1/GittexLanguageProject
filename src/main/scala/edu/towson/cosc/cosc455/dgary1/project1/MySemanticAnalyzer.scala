package edu.towson.cosc.cosc455.dgary1.project1

import java.awt.Desktop
import java.io.{File, IOException}

import scala.collection.mutable

class MySemanticAnalyzer {
  var beginningTree : List[String] = Nil
  var endTree : List[String] = Nil
  var index = 0

  def parse() : Unit = {

  }
  /* * Hack Scala/Java function to take a String filename and open in default web browswer. */
  def openHTMLFileInBrowser(htmlFileStr : String) = {
    val file : File = new File(htmlFileStr.trim)
    println(file.getAbsolutePath)
    if (!file.exists())
      sys.error("File " + htmlFileStr + " does not exist.")

    try {
      Desktop.getDesktop.browse(file.toURI)
    }
    catch {
      case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
      case e: Exception => sys.error("He's dead, Jim!")
    }
  }
}
