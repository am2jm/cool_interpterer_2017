public enum tok {
    AT("at"),
    CASE("case"),
    CLASS("class"),
    COLON("colon"),
    COMMA("comma"),
    DIVIDE("divide"),
    DOT("dot"),
    ELSE("else"),
    EQUALS("equals"),
    ESAC("esac"),
    FALSE("false"),
    FI("fi"),
    IDENT("identifier"),
    IF("if"),
    IN("in"),
    INHERITS("inherits"),
    INT("integer"),
    ISVOID("isvoid"),
    LARROW("larrow"),
    LBRACE("lbrace"),
    LE("le"),
    LET("let"),
    LOOP("loop"),
    LPAREN("lparen"),
    LT("lt"),
    MINUS("minus"),
    NEW("new"),
    NOT("not"),
    OF("of"),
    PLUS("plus"),
    POOL("pool"),
    RARROW("rarrow"),
    RBRACE("rbrace"),
    RPAREN("rparen"),
    SEMI("semi"),
    STRING("string"),
    THEN("then"),
    TILDE("tilde"),
    TIMES("times"),
    TRUE("true"),
    TYPE("type"),
    WHILE("while");

    private final String name;

    private tok(String s) {
        this.name = s;
    }

    public String toString() {
        return name;
    }
}
