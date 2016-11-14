package edu.towson.cosc.cosc455.dgary1.project1

import scala.collection.mutable

class MySyntaxAnalyzer extends SyntaxAnalyzer {
  var parsableTree = new scala.collection.mutable.Stack[String]
  var truth : Boolean = false

  override def gittex() : Unit = {
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
        if (Compiler.fileContents.length - Compiler.index > 15) {
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
  override def title() : Unit = {
    // Compiler checks to see if the beginning tag of Title is present
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
      parsableTree.push(CONSTANTS.TITLEB)
      Compiler.Scanner.getNextToken()
      // checking to see if the bracket is there, if not => will result in syntax error prompt
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parsableTree.push(CONSTANTS.BRACKETE)
        Compiler.Scanner.getNextToken()
      } else {
        println("Syntax Error: Illegal token. Beginning bracket of Title is missing.")
        System.exit(1)
      }
    } else {
      println("Syntax Error: Title tag is not present")
      System.exit(1)
    }
  }
  override def body() : Unit = {
    innerText()
    paragraph()
    newline()
    // this will loop back to body and in NewLine it has an DOCE where it has the possibility to stop that loop
    while(!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
      Compiler.Scanner.getNextToken()
      body()
    }
  }
  override def paragraph() : Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
      parsableTree.push(CONSTANTS.PARAB)
      variableDefine()
      innerText()
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
        parsableTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      } else {
        println("Syntax Error: The ending tag for paragraph is not present.")
        System.exit(1)
      }
    } else {
      println("Syntax Error: The beginning tag for paragraph is not present.")
      System.exit(1)
    }
  }
  override def innerText() : Unit = {
    // pattern matching the current token to go to the next possibility given that there are multiple possible paths
    Compiler.currentToken match {
      case CONSTANTS.USEB => variableUse()
        innerText()
      case CONSTANTS.HEADING => heading()
        innerText()
      case CONSTANTS.BOLD => bold()
        innerText()
      case CONSTANTS.ITALICS => italics()
        innerText()
      case CONSTANTS.LISTITEM => listItem()
        innerText()
      case CONSTANTS.IMAGEB => image()
        innerText()
      case CONSTANTS.LINKB => link()
        innerText()
      case CONSTANTS.NEWLINE => newline()
        innerText()
      // text
      case _ => {
        if(truth) {
          parsableTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
        } else {
          println("Syntax Error: Text required.")
          System.exit(1)
        }
      }
    }
  }
  override def heading() : Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
      parsableTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    } else {
      println("Syntax Error: Heading tag is not present")
      System.exit(1)
    }
  }
  override def variableDefine() : Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
      parsableTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    } else {
      println("Syntax Error: Beginning tag of Variable Definition is not present.")
      System.exit(1)
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)) {
      parsableTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    } else {
      println("Syntax Error: Beginning tag of Equation Sign is not present.")
      System.exit(1)
    }
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
      parsableTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    } else {
      println("Syntax Error: Beginning tag of the Bracket is not present.")
      System.exit(1)
    }
  }
  override def variableUse() : Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
      parsableTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parsableTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      } else {
        println("Syntax Error: The End Bracket is not present.")
        System.exit(1)
      }
    } else {
      println("Syntax Error: Beginning tag of Use Variable is not present.")
      System.exit(1)
    }
  }
  override def bold() : Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
      parsableTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    } else {
      println("Synatx Error: Bold tag is not present.")
      System.exit(1)
    }
  }
  override def italics() : Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)) {
      parsableTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    } else {
      println("Syntax Error: Beginning tag of italics is not present.")
      System.exit(1)
    }
  }
  override def listItem() : Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
      parsableTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      innerItem()
      // recursive call
      listItem()
    } else {
      println("Syntax Error: Missing tag is not present for the list.")
      System.exit(1)
    }
  }
  override def innerItem() : Unit = {
    Compiler.currentToken match {
      case CONSTANTS.USEB => variableUse()
        innerItem()
      case CONSTANTS.BOLD => bold()
        innerItem()
      case CONSTANTS.ITALICS => italics()
        innerItem()
      case CONSTANTS.LINKB => link()
        innerItem()
      // case of reqtext/text
      case _ => {
        if (truth) {
          text()
          innerItem()
        }
      }
    }
  }
  override def link() : Unit = {
    // checks beginning tag of link
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
      parsableTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      // after initial tag, looks for text
      if (truth) {
        parsableTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      // after next token, checks to see if bracket is there for the link
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parsableTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        // next, checks for beginning address tag
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
          parsableTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          // again, looks for text
          if(truth) {
            parsableTree.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
          } else {
            println("Syntax Error: Text required.")
            System.exit(1)
          }// to end, looks for ending tag to finish analzying token
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
            parsableTree.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
          } else {
            println("Syntax Error: End tag of the Address tag not present.")
            System.exit(1)
          }
        } else {
          println("Syntax Error: Beginning tag of the Address tag not present.")
          System.exit(1)
        }
      } else {
        println("Syntax Error: Bracket tag not present.")
        System.exit(1)
      }
    } else {
      println("Syntax Error: Beginning tag of Link not present.")
    }
  }
  override def image() : Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB)) {
      parsableTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
      if (truth)
        text()
      else {
        println("Syntax Error: Image Text required, not found.")
        System.exit(1)
      }
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parsableTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
          parsableTree.push(Compiler.currentToken)
          Compiler.Scanner.getNextToken()
          if (truth)
            text()
          else {
            println("Syntax Error: Image Text required, not found.")
            System.exit(1)
          }
          if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
            parsableTree.push(Compiler.currentToken)
            Compiler.Scanner.getNextToken()
          } else {
            println("Syntax Error: End tag of Image Address is not present.")
            System.exit(1)
          }
        } else {
          println("Syntax Error: Beginning tag of Image Address is not present.")
          System.exit(1)
        }
      } else {
        println("Syntax Error: Beginning bracket tag of Image Address is not present.")
        System.exit(1)
      }
    } else {
      println("Syntax Error: Beginning tag of Image tag is not present.")
      System.exit(1)
    }
  }
  override def newline() : Unit = {
    if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
      parsableTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
  }
  def text() : Unit = {
    while (!truth) {
      parsableTree.push(Compiler.currentToken)
      Compiler.Scanner.getNextToken()
    }
  }
}
