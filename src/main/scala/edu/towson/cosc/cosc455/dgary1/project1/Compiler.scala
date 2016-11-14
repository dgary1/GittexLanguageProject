package edu.towson.cosc.cosc455.dgary1.project1
import scala.collection.mutable
import scala.io.Source

object Compiler {

  var fileContents : String = ""
  var currentToken : String = ""
  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val Eval = new MySemanticAnalyzer
  var index: Int = 0

  def main(args : Array[String]) = {
    // check usage
    checkFile(args)
    readFile(args(0))

    // get first Token
    Scanner.getNextToken()
    Parser.gittex()
    //calls start state of BNF is SyntaxAnalyzer
    Eval.parse()
  }

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()

  }
  def checkFile(args: Array[String]) = {
    if (args.length != 1 ){
      println("Usage Error: wrong number of args fool!")
      System.exit(1)
    }
    else if(!args(0).endsWith((".mkd"))){
      println("Usage Error: wrong extension fool!")
      System.exit(1)
    }
  }

}
