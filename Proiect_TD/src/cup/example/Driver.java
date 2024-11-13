package cup.example;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import java_cup.runtime.*;

class Driver {

	public static void main(String[] args) throws Exception {
		
		  ComplexSymbolFactory f = new ComplexSymbolFactory();
		  
		  File file = new File("input.txt");
		  
		  FileInputStream fis = null;
		  
		  try {
		    fis = new FileInputStream(file);
		  } catch (IOException e) {
		    e.printStackTrace();
		  } 
		  
		  Lexer lexer = new Lexer(f, fis);
		  Symbol symbol;
		  
		  while ((symbol = lexer.next_token()).sym != sym.EOF) {
			  System.out.println(symbol);
		  }
		  
		  
	}
	
}