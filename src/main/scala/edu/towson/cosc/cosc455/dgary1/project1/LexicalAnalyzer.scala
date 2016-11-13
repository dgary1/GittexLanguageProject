package edu.towson.cosc.cosc455.dgary1.project1

/**
  * Created by davidgary on 11/13/16.
  */
trait LexicalAnalyzer {
  def addChar() : Unit
  def getChar() : Char
  def getNextToken() : Unit
  def lookup() : Boolean
}
