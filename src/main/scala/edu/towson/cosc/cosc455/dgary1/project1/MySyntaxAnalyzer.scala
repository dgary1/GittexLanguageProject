package edu.towson.cosc.cosc455.dgary1.project1


class MySyntaxAnalyzer extends SyntaxAnalyzer {
  var parsableTree = new scala.collection.mutable.Stack[String]

  def gittex() : Unit = {
    // first, we are checking to see if the DOCB tag is present in the test case
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
      // pushing the tag into the Stack
      parsableTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      // next, we are going onto the rest of the grammar defined by our grammar rules
      title()
      variableDefine()
      body()
      // checks for the ending tag of the test case
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        parsableTree.push(Compiler.currentToken)
        if (Compiler.fileContents.length - Compiler.position > 15) {
          println("Syntax Error: Nothing after the \\END")
          System.exit(1)
        }
      }
    } else {
      println("Syntax Error: \\DOCB not found.")
      System.exit(1)
    }
    // Syntax error is thrown if the Compiler cannot find the beginning tag of the test case
  }
  def title() : Unit = {
    // Compiler checks to see if the beginning tag of Title is present
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
      parsableTree.push(CONSTANTS.TITLEB)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parsableTree.push(CONSTANTS.BRACKETE)
        Compiler.Scanner.getNextToken()
      } else {
        println("Syntax Error: Illegal token. End bracket of Title is missing.")
        System.exit(1)
      }
    } else {
      println("Syntax Error: Title tag is not present")
      System.exit(1)
    }
  }
  def body() : Unit
  def paragraph() : Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      parsableTree.push(CONSTANTS.PARAB)
      variableDefine()
      innerText()
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
        parsableTree.push(Compiler.currentToken)
      } else {
        println("Syntax Error: The ending tag for paragraph is not present.")
        System.exit(1)
      }
    } else {
      println("Syntax Error: The beginning tag for paragraph is not present.")
      System.exit(1)
    }
  }
  def innerText() : Unit
  def heading() : Unit
  def variableDefine() : Unit
  def variableUse() : Unit
  def bold() : Unit
  def italics() : Unit
  def listItem() : Unit
  def innerItem() : Unit
  def link() : Unit
  def image() : Unit
  def newline() : Unit
}
