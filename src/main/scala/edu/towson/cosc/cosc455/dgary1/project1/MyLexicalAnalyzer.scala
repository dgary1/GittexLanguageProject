package edu.towson.cosc.cosc455.dgary1.project1

import scala.collection.immutable
/**
  * Created by davidgary on 11/13/16.
  */
class MyLexicalAnalyzer extends LexicalAnalyzer {
  var truth : Boolean = true
  val lexemes : List[String] = List("\\BEGIN", "\\END", "\\TITLE[", "\\USE[", "\\DEF[", "\\PARE", "\\PARB", "]", "#", "*", "**", "+", "\\", "[", "(", ")", "![", "=")
  var character : Char = ' '
  var stringofChar : String = ""
  val position = -1
  // using a mutable queue because we want to add, remove, or change the elements of the queue
  var token = new scala.collection.mutable.Queue[Char]

  def addChar() : Unit = {
    stringofChar = stringofChar + character
  }
  def getChar() : Char = {
    Compiler.fileContents.charAt(Compiler.position)
    Compiler.position = Compiler.position + 1
  }
  override def getNextToken() : Unit = {
    character = getChar()
    // checks to see if char retrieved is a space
    if(character.equals(' ')) {
      while(character.equals(' ')) {
        character = getChar()
      }
      // if char equals a special char, then it'll start creating a token
    }
    if (lexemes.contains(character.toString)) {
      Compiler.currentToken = character.toString
      if (character.equals('=') || character.equals(']'))
        character = getChar()
    } else if (character.equals('\\') || character.equals('*') || character.equals('#') || character.equals('+') || character.equals('[') || character.equals('!'
    )) {
      addChar()
      character = getChar()
      // the compiler will continue to build the token if it does not hit a special char
      while(!(character.equals('\n') || character.equals('[') || character.equals('(') || character.equals('\r') || character.equals(']') || character.equals(')') || character.equals('\\') || character.equals('*'))) {
        addChar()
        character = getChar()
      }
      // method lookUp checks to see if the token is a legitimate token or not
      if (lookUp()) {
        Compiler.currentToken = stringofChar
        stringofChar = ""
        character = getChar()
        // if not, throws a lexical error because its not a token and exists program
      } else {
        println("Lexical Error: Token does not exist.")
        System.exit(1)
      }
    } else if (text()) {
        while (text()) {
          Compiler.Parser.truth = true
          addChar()
          character = getChar()
        }
      Compiler.currentToken = stringofChar
      stringofChar = ""
    }
  }
  def lookUp() : Boolean = {
    var flag = true
    if(!lexemes.contains(stringofChar.toUpperCase))
      flag = false
    flag
  }
  def text() : Boolean = {
    truth = false
    for (number <- '0' to '9') {
      if(character.equals(number))
        truth = true
    }
    for (lowerChar <- 'a' to 'z') {
      if (character.equals(lowerChar))
        truth = true
    }
    for (upperChar <- 'A' to 'Z') {
      if (character.equals(upperChar))
        truth = true
    }
    if (character.equals(',') || character.equals('.') || character.equals('?') || character.equals('_') || character.equals('/'))
      truth = false
    truth
  }
}
