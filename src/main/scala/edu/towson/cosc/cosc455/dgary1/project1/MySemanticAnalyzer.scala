package edu.towson.cosc.cosc455.dgary1.project1

import java.awt.Desktop
import java.io.{File, IOException, PrintWriter}
import scala.collection.mutable

class MySemanticAnalyzer {
  // In order to convert the Stack of Strings I have created throughout the SyntaxAnalyzer, I must be able to reverse that stack and then,
  // pop each of the top elements and convert them accordingly depending on what grammar rule they fit. In order to do that, I think that using
  // a pattern matching approach. Ex: case CONSTANTS.DOCB => parsableTree = "<!DOCTYPE html>\n<html>\n<head>\n" :: parsableTree;
  // current = resTree.pop(); where parsableTree would be the tree that would be converted into a string file that would be executed to open in a browser
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
