// Token.java

/**
 * Store values associated with a token
 */

public class Token {
    private Object lexeme;
    private tok tokType;
    private int line, col;
    private boolean hadError;
    private String message;

    public Token(tok type, int line, String message, boolean error) {
      this.tokType = type;
      this.line = line;
      this.message = message;
      this.hadError = error;
    }

    public Token(tok type, int line, int col, Object value) {
        this.tokType = type;
        this.line = line;
        this.col = col;
        this.lexeme = value;

        if(type.toString() == "integer"){
          String number = value.toString();
          try{
            this.lexeme = Integer.parseInt(number);

          }catch(Exception e){
            // System.out.println(e);
            this.message = "Max int length";
            hadError = true;
          }

        }
    }

    public boolean hadError(){
      return hadError;
    }
    public int getLine(){
      return line;
    }
    public String getMessage(){
      return message;
    }


    public String toString() {
        if(this.lexeme == null) {
            return this.line + "\n"+ this.tokType;
        } else {
            return this.line + "\n" + this.tokType + "\n" + this.lexeme;
        }
    }
}
