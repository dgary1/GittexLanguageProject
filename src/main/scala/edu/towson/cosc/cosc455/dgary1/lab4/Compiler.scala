package edu.towson.cosc.cosc455.dgary1.lab4

/**
  * COSC 455 Programming Languages: Implementation and Design
  * Lab 4 - A Simple Compiler
  * Josh Dehlinger
  * A simple compiler used for the simple English grammar in Section 2.2 of Adam Brooks Weber's
  * "Modern Programming Languages" book. Parts of this code was adapted from Robert Sebesta's "Concepts
  * of Programming Languages".
  *
  * This compiler assumes that the source file containing the sentences to parse are provided as the first
  * runtime argument. Within the source file, the compiler assumes that each sentence to parse is provided
  * on its own line.
  */

import scala.io.Source

object Compiler {

  // currentToken variable
  var currentToken : String = ""

  val Scanner = new LexicalAnalyzer
  val Parser = new SyntaxAnalyzer

  def main(args: Array[String]) = {

    // get input file name from command line argument
    val filename = args(0)

    // for each line read from file, scan and parse
    for (line <- Source.fromFile(filename).getLines()) {
      // get the first token
      Scanner.start(line)

      // Parse the given sentence against the given grammar. We assume that the
      // sentence, <S>, production is the start state.
      Parser.Sentence()

      // If a syntax error was discovered, print that the sentence follows the grammar.
      if(Parser.getError)
        println("The sentence '" + line + "' does not follow the BNF grammar.")
      // If no syntax error was found, print that the sentence does not follow the grammar.
      else
       println("The sentence '" + line + "' follows the BNF grammar.")

      println
    }
  }

}
