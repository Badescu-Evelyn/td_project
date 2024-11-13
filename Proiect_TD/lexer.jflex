package cup.example;
import java_cup.runtime.ComplexSymbolFactory;
import java_cup.runtime.ComplexSymbolFactory.Location;
import java_cup.runtime.Symbol;
import java.lang.*;
import java.io.InputStreamReader;

%%

%class Lexer
%implements sym
%public
%unicode
%line
%column
%cup
%char
%{
	

    public Lexer(ComplexSymbolFactory sf, java.io.InputStream is){
		this(is);
        symbolFactory = sf;
    }
	public Lexer(ComplexSymbolFactory sf, java.io.Reader reader){
		this(reader);
        symbolFactory = sf;
    }
    
    private StringBuffer sb;
    private ComplexSymbolFactory symbolFactory;
    private int csline,cscolumn;

    public Symbol symbol(String name, int code){
		return symbolFactory.newSymbol(name, code,
						new Location(yyline+1,yycolumn+1, yychar), // -yylength()
						new Location(yyline+1,yycolumn+yylength(), yychar+yylength())
				);
    }
    public Symbol symbol(String name, int code, String lexem){
	return symbolFactory.newSymbol(name, code, 
						new Location(yyline+1, yycolumn +1, yychar), 
						new Location(yyline+1,yycolumn+yylength(), yychar+yylength()), lexem);
    }
    
    protected void emit_warning(String message){
    	System.out.println("scanner warning: " + message + " at : 2 "+ 
    			(yyline+1) + " " + (yycolumn+1) + " " + yychar);
    }
    
    protected void emit_error(String message){
    	System.out.println("scanner error: " + message + " at : 2" + 
    			(yyline+1) + " " + (yycolumn+1) + " " + yychar);
    }
%}

Newline    			   = \r | \n | \r\n
Whitespace 			   = [ \t\f] | {Newline}
Number     			   = (-)?[0-9]+
Real_Value			   = ((-)?([0-9]+)(\.[0-9]+)?)|((-)?\.?[0-9]+)
String_Literal		   = "\"" ( [^\"\n\\] | "\\" . )* "\""
/* comments */
Comment = {TraditionalComment} | {EndOfLineComment}
TraditionalComment = "/" {CommentContent} \+ "/"
EndOfLineComment = "//" [^\r\n]* {Newline}
CommentContent = ( [^] | \+[^/] )

ident = ([:jletter:] | "" ) ([:jletterdigit:] | [:jletter:] | "" )*


%eofval{
    return symbolFactory.newSymbol("EOF",sym.EOF);
%eofval}

%state CODESEG

%%  

<YYINITIAL> {

  {Whitespace} 					{                              }
  ";"          					{ return symbolFactory.newSymbol("SEMI", SEMI); }
  ","		                    { return symbolFactory.newSymbol("COMMA", COMMA); }
  "+"          					{ return symbolFactory.newSymbol("PLUS", PLUS); }
  "-"          					{ return symbolFactory.newSymbol("MINUS", MINUS); }
  "*"          					{ return symbolFactory.newSymbol("TIMES", TIMES); }
  "n"          					{ return symbolFactory.newSymbol("UMINUS", UMINUS); }
  "("          					{ return symbolFactory.newSymbol("LPAREN", LPAREN); }
  ")"          					{ return symbolFactory.newSymbol("RPAREN", RPAREN); }
  ":="		   					{ return symbolFactory.newSymbol("ASSIGNMENT", ASSIGNMENT); }
  
  "/"          					{ return symbolFactory.newSymbol("DIVIDE", DIVIDE); }
  "**"          				{ return symbolFactory.newSymbol("POWER", POWER); }
  "mod"          				{ return symbolFactory.newSymbol("MOD", MOD); }
  "and"							{ return symbolFactory.newSymbol("AND", AND); }
  "or"							{ return symbolFactory.newSymbol("OR", OR); }
  "not"							{ return symbolFactory.newSymbol("NOT", NOT); } 
  "<>"							{ return symbolFactory.newSymbol("NOTEQUAL", NOTEQUAL); }
  "<"							{ return symbolFactory.newSymbol("LESS", LESS); }
  "<="							{ return symbolFactory.newSymbol("LESSEQ", LESSEQ); }
  ">"							{ return symbolFactory.newSymbol("GREATER", GREATER); }
  ">="							{ return symbolFactory.newSymbol("GREATEREQ", GREATEREQ); }
  "="							{ return symbolFactory.newSymbol("EQ", EQ); }
  
  "function"   					{ return symbolFactory.newSymbol("FUNCTION", FUNCTION); }
  "return"						{ return symbolFactory.newSymbol("RETURN", RETURN); }   
  "endfunction"					{ return symbolFactory.newSymbol("ENDFUNCTION", ENDFUNCTION); }
  "string"						{ return symbolFactory.newSymbol("STRING", STRING); }
  "integer"						{ return symbolFactory.newSymbol("INTEGER", INTEGER); }
  "real"						{ return symbolFactory.newSymbol("REAL", REAL); }
  "program"						{ return symbolFactory.newSymbol("PROGRAM", PROGRAM); }
  "endprogram"					{ return symbolFactory.newSymbol("ENDPROGRAM", ENDPROGRAM); }
  "if"							{ return symbolFactory.newSymbol("IF", IF); }
  "then"						{ return symbolFactory.newSymbol("THEN", THEN); }
  "else"						{ return symbolFactory.newSymbol("ELSE", ELSE); }
  "endif"						{ return symbolFactory.newSymbol("ENDIF", ENDIF); }
  "while"						{ return symbolFactory.newSymbol("WHILE", WHILE); }
  "do"							{ return symbolFactory.newSymbol("DO", DO); }
  "enddo"						{ return symbolFactory.newSymbol("ENDDO", ENDDO); }
  "for"							{ return symbolFactory.newSymbol("FOR", FOR); }
  "to"							{ return symbolFactory.newSymbol("TO", TO); }
  "do"							{ return symbolFactory.newSymbol("DO", DO); }
  "endfor"						{ return symbolFactory.newSymbol("ENDFOR", ENDFOR); }
  
  "read"						{ return symbolFactory.newSymbol("READ", READ); }
  "write"						{ return symbolFactory.newSymbol("WRITE", WRITE); }
  "clear"						{ return symbolFactory.newSymbol("CLEAR", CLEAR); }
  "move"						{ return symbolFactory.newSymbol("MOVE", MOVE); }
  "draw"						{ return symbolFactory.newSymbol("DRAW", DRAW); }
  "set color"					{ return symbolFactory.newSymbol("SET_COLOR", SET_COLOR); }
  "set line"					{ return symbolFactory.newSymbol("SET_LINE", SET_LINE); }
  
  {Number}     					{ return symbolFactory.newSymbol("NUMBER", NUMBER, Integer.parseInt(yytext())); }
  {ident}	   					{ return symbolFactory.newSymbol("IDENTIFIER", IDENTIFIER, yytext()); }
  {String_Literal}				{ return symbolFactory.newSymbol("LITERAL", LITERAL, String.valueOf(yytext())); }
  {Real_Value}					{ return symbolFactory.newSymbol("REALVAL", REALVAL, Float.parseFloat(yytext())); }
  
  
  
  

	

  
}



// error fallback
.|\n          { emit_warning("Unrecognized character '" +yytext()+"' -- ignored"); }