/*
 * Simple Caclulater Lexer
 * Lexer.flex
 */

// section 1: user code copied verbatim

%%

// section 2: options and declarations

%class Lexer
%unicode
%line
%column
%type Token
%state Comment
%state StartString

%{
   // Code in here is copied verbatim into the Lexer class
	// a counter for the nested comments, and a string to build
   int myCounter = 0;
   String buildString = "";

	// multiple returns back to the token class
 	private Token token(tok type) {
 		return new Token(type, yyline+1, yycolumn+1, null);
 	}
  private Token token(tok type, String message, boolean hadError) {
    return new Token(type, yyline+1, message, hadError);
  }
 	private Token token(tok type, Object lexeme) {
    	return new Token(type, yyline+1, yycolumn+1, lexeme);
 	}
%}

 // define some "macro" regular expressions
 Identifier = [a-z][_A-Za-z0-9]*
 IntegerLiteral = [0-9]+
 Type = [A-Z][a-zA-Z_0-9]*

 // Define some simple comments
 SingleLineParen =[(][*].*[*][)]
 HypenComments = [-][-].*
 //ParenComments = [(][*].*([*][)]){0, 1}

%%

// Section 3...lexical rules
<YYINITIAL> {
 	":"     { return token(tok.COLON); }
 	","     { return token(tok.COMMA); }
 	"."     { return token(tok.DOT); }
  "@"     { return token(tok.AT); }
  "~" 	{return token(tok.TILDE); }
  [Ii][Ss][Vv][Oo][Ii][Dd]			{return token(tok.ISVOID); }
  "*" 	{return token(tok.TIMES); }
  "/"     { return token(tok.DIVIDE); }
  "+" 	{return token(tok.PLUS);}
  "-" 	{return token(tok.MINUS); }
  "=" 	{ return token(tok.EQUALS); }
  "<=" 	{return token(tok.LE); }
  "=>" 	{return token(tok.RARROW); }
  "<" 	{return token(tok.LT); }
  [Nn][Oo][Tt]	 	{return token(tok.NOT); }
  "<-" 	{return token(tok.LARROW); }

  "{" 	{return token(tok.LBRACE); }
  "(" 	{return token(tok.LPAREN); }
  "}" 	{return token(tok.RBRACE); }
  ";" 	{return token(tok.SEMI); }
  ")" 	{return token(tok.RPAREN);}

	// After the boring ones
	// Case insensitive words!
 	[Ee][Ll][Ss][Ee]  		{ return token(tok.ELSE); }
 	[Ee][Ss][Aa][Cc] 		{return token(tok.ESAC); }
  [Cc][Aa][Ss][Ee]  { return token(tok.CASE); }
  [Cc][Ll][Aa][Ss][Ss]	{ return token(tok.CLASS); }
 	[f][Aa][Ll][Ss][Ee] 	{return token(tok.FALSE); }
 	[Ff][Ii] 	{return token(tok.FI); }
 	[Ii][Ff] 	{return token(tok.IF); }
 	[Ii][Nn] 	{return token(tok.IN); }
 	[Ii][Nn][Hh][Ee][Rr][Ii][Tt][Ss]	{return token(tok.INHERITS); }
 	[Ll][Ee][Tt] 	{return token(tok.LET); }
 	[Ll][Oo][Oo][Pp]	{return token(tok.LOOP); }
 	[Nn][Ee][Ww]	 	{return token(tok.NEW); }
 	[Oo][Ff]	 		{return token(tok.OF); }
 	[Pp][Oo][Oo][Ll]	{return token(tok.POOL);}
 	[Ss][Ee][Mm][Ii] 	{return token(tok.SEMI); }
 	[Tt][Hh][Ee][Nn] 	{return token(tok.THEN); }
 	[t][Rr][Uu][Ee] 	{return token(tok.TRUE); }
 	[Ww][Hh][Ii][Ll][Ee] {return token(tok.WHILE);}
	
	// These two
	// that didn't give us more correct cases :c
 	"self" 			{return token(tok.IDENT, yytext());}
	"SELF_TYPE"		{return token(tok.TYPE, yytext());}

	
	// Create a string! Initialize it as nothing
  \" {
    buildString = "";
    yybegin(StartString);
    }
	




	// Boring type and identifier and single line paren thing
	{Type} 	{ return token(tok.TYPE, yytext()); }
	{Identifier}	{ 
			String temp = yytext();
			int isZero = temp.indexOf("0");
			if(temp.substring(0, 1).equals("0") && temp.length() > 1)
				return token(tok.STRING, "Invalid Number", true);
			else
				return token(tok.IDENT, yytext());
			}
	{IntegerLiteral}	{ return token(tok.INT, yytext()); }

	{SingleLineParen}	{
	//	System.out.println("Why not!");
	}
	{HypenComments} { /* ignore */ }

	// the exiting comments
	// NESTED comments, start the counter at lvl0
	"(*" {
		myCounter = 0;
		yybegin(Comment);
	}

	[ \t\n\s]  { /* ignore */ }
	
	// And something bad slipped through!
	[^]	{ // catchall?
		return token(tok.STRING, "Invalid Character", true);
	
		}
}

<StartString>{
  <<EOF>> {
		//System.out.println("ERROR: " + yyline + 1 +  ": Lexer: ");
		return token(tok.STRING, "END OF FILE", true);
		}
		
	
	// Not sure, but supposed to escape \"
  (\\\") {
            buildString += yytext();
            if(buildString.length() >= 1024){
              return token(tok.STRING, "Max String Length", true);
            }

        }
		
	// NULL
	\x00 {
		return token(tok.STRING, "Invalid String", true);
		}
	// OTHER BAD THING
	\xd {
		return token(tok.STRING, "Invalid String", true);
		}
		
	\xa  {
        return token(tok.STRING, "Invalid String", true);
      }

	 // This is when you hit the REAL end of string
    \" {
        yybegin(YYINITIAL);
        return token(tok.STRING, buildString);
		
        }
	// Newlines are ok sometimes	
    \\n|\\f|\\r {
		// catches a typed newline
        buildString += yytext();
        if(buildString.length() >= 1024){
          return token(tok.STRING, "Max String Length", true);
          }
      }
	 \\0 {
	 	buildString += yytext();
        if(buildString.length() >= 1024){
          return token(tok.STRING, "Max String Length", true);
          }
	 }
	  

	// otherwise grab a thing and keep adding it on!
    .|\s {
        buildString += yytext();
        if(buildString.length() >= 1024){
          return token(tok.STRING, "Max String Length", true);
        }
      }




  }
 <Comment>{
  <<EOF>> {
		//System.out.println("ERROR: " + yyline + 1 +  ": Lexer: ");
		return token(tok.STRING, "END OF FILE", true);
		}

	// eat up whitespace
  	[ \t\n]  {
		//System.out.println("My text whitespace:" + yytext());
		}
	
	// Gave no testcases
	// Supposed to check for comments in strings
	[\"].*[^*][^)].*[\"] {}
	[\"].*[^(][^*].*[\"] {}
	

	// Ooh, a nested comment, add one to the level counter!
	"(*"	{
  //System.out.println("open: "+yytext());

				myCounter ++;

			}
			
			
	// Some nested comment is closing. Subtract one from the counter
	// If you're not already at 0
	// If you're at 0, go back to the normal state!!
	"*)" 	{
  //System.out.println("close: "+ yytext() + myCounter);

				if(myCounter == 0)
					yybegin(YYINITIAL);
				else
					myCounter --;

			}
			
	// Otherwise just gobble up input
	.|\s {
    //System.out.println("other: "+yytext());
    /* ignore */
  }

}
