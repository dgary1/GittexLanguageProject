package edu.towson.cosc.cosc455.dgary1.lab4;

/*
COSC 455 Programming Languages: Implementation and Design
Lab 4 - A Simple Lexical Analyzer
Josh Dehlinger

This lexical analyzer simple finds lexemes separated by a single space and places it in the Compiler
classes currentToken global String. The lexical analyzer here takes a source line and does a character-
by-character analysis to determine lexemes/tokens. Note that this lexical analyzer does not lookup a
lexeme to find its "class" to determine its token type, as shown in the book. This lexical analyzer
also limits each lexeme/token to 100 characters or less.
*/

import java.lang.*;
import java.util.List;
import java.util.ArrayList;

public class LexicalAnalyzer {

	private String sourceLine;
	private char[] lexeme = new char [100];
	private char nextChar;
	private int lexLength;
	private int position;
    private List<String> lexems = new ArrayList<String>();

		
	// The main driver of this class. This method takes a "program", in this case a single line of text in
	// the form of a sentence, and gets the first lexeme/token. 
	public void start(String line){

        initializeLexems();
		sourceLine = line;
		position = 0;
		
		getChar();		
		getNextToken();		
	}
	
	// This method does a character-by-character analysis to get the next token and set it in the Compiler
	// class's currentToken global String variable. This simple lexical analyzer does not differentiate 
	// between letters, digits and other special characters - it simply looks for characters, spaces and
	// end of line characters to determine relevant tokens. 
	public void getNextToken() {
		lexLength = 0;
		
		// Ignore spaces and add the first character to the token
		getNonBlank();
		addChar();
		getChar();
		
		// Continue gathering characters for the token
		while((nextChar != '\n') && (nextChar != ' ')){
			addChar();
			getChar();
		}			
		
		// Convert the gathered character array token into a String
		String newToken = new String(lexeme);

        if(lookup(newToken.substring(0, lexLength)))
            Compiler.currentToken_$eq(newToken.substring(0, lexLength));
	}

    // Add the legal lexems to the language
    private void initializeLexems(){
        lexems.add("\n");lexems.add("teh"); lexems.add("a");
        lexems.add("dawg"); lexems.add("kat"); lexems.add("rat");
        lexems.add("ates"); lexems.add("lovez"); lexems.add("hatez");
    }

    private boolean lookup(String candidateToken){
        if(!lexems.contains(candidateToken)){
            Compiler.Parser().setError();
            System.out.println("LEXICAL ERROR - '" + candidateToken + "' is not recognized.");
            return false;
        }
        return true;
    }
	
	// This method gets the next character from the "program" string.
	private void getChar(){
		if (position < sourceLine.length())
			nextChar = sourceLine.charAt(position++);
		else nextChar = '\n';
	}
	
	// A helper method to determine if the current character is a space.
	private boolean isSpace(char c){
        return c == ' ';
	}
	
	// A helper method to get the next non-blank character.
	private void getNonBlank(){
		while(isSpace(nextChar)) {
            getChar();
        }
	}
	
	/*
	This method adds the current character the the token after checking to make
	sure that the length of the token isn't too long, a lexical error in this
	case.
	*/
    private void addChar(){
		if(lexLength <= 98){
			lexeme[lexLength++] = nextChar;
			lexeme[lexLength] = 0;
		}
		else{
			System.out.println("LEXICAL ERROR - The found lexeme is too long!");
									
			if(!isSpace(nextChar)){
				while(!isSpace(nextChar)){					
					getChar();						
				}
			}
			lexLength = 0;					
			getNonBlank();
			addChar();
			
		}
	}
}
