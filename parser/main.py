import sys
from tok import tokens
from tok import precedence
from lex import LexToken
import yacc as yacc

#open file and read all input
tokens_filename = sys.argv[1]
tokens_filehandle = open(tokens_filename, 'r')
tokens_lines = tokens_filehandle.readlines()
tokens_filehandle.close()

#read the tokens from the .cl-lex
def get_token_line():
        global tokens_lines
        result = tokens_lines[0].strip()
        tokens_lines = tokens_lines[1:]
        return result

pa2_tokens = []

#convert tokens to form acceptable to ply
while tokens_lines != []:
    line_number = get_token_line()
    token_type = get_token_line()
    token_lexeme = token_type
    if token_type in ['identifier', 'integer', 'type', 'string']:
        token_lexeme = get_token_line()
    pa2_tokens = pa2_tokens + \
        [(line_number, token_type.upper(), token_lexeme)]

class PA2Lexer(object):
    def token(whatever):
        global pa2_tokens
        if pa2_tokens == []:
            return None
        (line, token_type, lexeme) = pa2_tokens[0]
        pa2_tokens = pa2_tokens[1:]
        tok = LexToken()
        tok.type = token_type
        tok.value = lexeme
        tok.lineno = line
        tok.lexpos = 0
        return tok

pa2lexer = PA2Lexer()


#(line_number, AST_node_type, AST_node_child1, AST_node_child2)

# a class-list -list
def p_program_classlist(p):
    'program : classlist'
    p[0] = p[1]

def p_classlist_none(p):
    'classlist : '
    p[0] = []

def p_classlist_some(p):
    'classlist : class SEMI classlist'
    p[0] = [p[1]] + p[3]
# end of the classlist parsing code

def p_class_noinherit(p):
    'class : CLASS type LBRACE featurelist RBRACE'
    p[0] = (p.lineno(1), 'class_noinherit', p[2], p[4])

def p_class_inherit(p):
    'class : CLASS type INHERITS type LBRACE featurelist RBRACE'
    p[0] = (p.lineno(1), 'class_inherit', p[2], p[4], p[6])

## here's what a TYPE is
def p_type(p):
    'type : TYPE'
    p[0] = (p.lineno(1), p[1])

## and here's an IDENTIFIER
def p_identifier(p):
    'identifier : IDENTIFIER'
    p[0] = (p.lineno(1), p[1])

## list of featuresm zero or more
def p_featurelist_none(p):
    'featurelist : '
    p[0] = []

def p_featurelist_some(p):
    'featurelist : feature SEMI featurelist'
    p[0] = [p[1]] + p[3]
## end of the FEATURELIST

## beginning of FORMALLIST code, zero or more
def p_formallist_none(p):
    'formallist : '
    p[0] = []

def p_formal(p):
    'formal : identifier COLON type'
    p[0] = ( p[1], p[3])

def p_formallist_some(p):
    'formallist : COMMA formal formallist'
    p[0] = [p[2]] + p[3]

def p_formallist_begin_some(p):
    'formallist_begin : formal formallist'
    p[0] = [p[1]] + p[2]

def p_formallist_begin_none(p):
    'formallist_begin : '
    p[0] = []
# end of the formallist code

# different type of feature
def p_feature_attributenoinit(p):
    'feature : identifier COLON type'
    p[0] = (p.lineno(1), 'attribute_no_init', p[1], p[3])

def p_feature_method(p):
    'feature : identifier LPAREN formallist_begin RPAREN COLON type LBRACE exp RBRACE'
    p[0] = (p.lineno(1), 'method', p[3], p[1], p[6], p[8])

def p_feature_attributeinit(p):
    'feature : identifier COLON type LARROW exp'
    p[0] = (p.lineno(1), 'attribute_init', p[1], p[3], p[5])
# end of feature code

## the beginning of all of the different types of expressions
## everything NOT an expression is above here


def p_exp_assign(p):
    'exp : identifier LARROW exp'
    p[0] = (p.lineno(2), p[1], 'assign', p[3])

#static dispatch
def p_exp_static_dispatch(p):
    'exp : exp AT type DOT identifier LPAREN expbegin RPAREN'
    p[0] = (p.lineno(2), p[1], 'static_dispatch',p[3], p[5], p[7])

#dynamic dispatch
def p_exp_dynamic_dispatch(p):
    'exp : exp DOT identifier LPAREN expbegin RPAREN'
    p[0] = (p.lineno(2), p[1], 'dynamic_dispatch', p[3], p[5])


# the top part of self_dispatch
def p_exp_self_dispatch(p):
    'exp : identifier LPAREN expbegin RPAREN'
    p[0] = (p.lineno(2), p[1], 'self_dispatch',p[3])



#case and handling it, one or more of the items
def p_exp_case(p):
    'exp : CASE exp OF epointlistbegin ESAC'
    p[0] = (p.lineno(1), p[2], 'case', p[4])

def p_epoint_list_begin_some(p):
    'epointlistbegin : exppoint SEMI epointlist'
    p[0] = [p[1]] + p[3]

def p_epoint_list_none(p):
    'epointlist : '
    p[0] = []

def p_epoint_list_some(p):
    'epointlist : exppoint SEMI epointlist'
    p[0] = [p[1]] + p[3]

def p_exppoint(p):
    'exppoint : identifier COLON type RARROW exp'
    p[0] = (p.lineno(1), p[1], 'exppoint', p[3], p[5])
# end of case code


# block and its semi colon list one or more
def p_exp_block(p):
    'exp : LBRACE expbegin2 RBRACE'
    p[0] = (p.lineno(1), p[2], 'block')

def p_exp_list_begin_some2(p):
    'expbegin2 : exp SEMI explist2'
    p[0] = [p[1]] + p[3]

def p_exp_list_none2(p):
    'explist2 : '
    p[0] = []

def p_exp_list_some2(p):
    'explist2 : exp SEMI explist2'
    p[0] = [p[1]] + p[3]
# end block exp-list

def p_exp_newtype(p):
    'exp : NEW type'
    p[0] = (p.lineno(1), p[2], 'new')

# list for self_dispatch : zero or more
def p_exp_list_begin_some(p):
    'expbegin : exp explist'
    p[0] = [p[1]] + p[2]

def p_exp_list_begin_none(p):
    'expbegin : '
    p[0] = []

def p_exp_list_none(p):
    'explist : '
    p[0] = []

def p_exp_list_some(p):
    'explist : COMMA exp explist'
    p[0] = [p[2]] + p[3]
# end of self-dispatch list, *

# if
def p_exp_if(p):
    'exp : IF exp THEN exp ELSE exp FI'
    p[0] = (p.lineno(1), p[2], 'if', p[4], p[6])

#while
def p_exp_while(p):
    'exp : WHILE exp LOOP exp POOL'
    p[0] = (p.lineno(1), p[2], 'while', p[4])

# isvoid
def p_exp_isvoid(p):
    'exp : ISVOID exp'
    p[0] = (p.lineno(1), p[2], 'isvoid')

# let bindings now, list after the first setup
def p_exp_let(p):
    'exp : LET identifier COLON type opassign letlist IN exp'
    p[0] = (p.lineno(1), p[2], 'let', p[4], p[5], p[6], p[8])
    #           0         1-id   2   3-type 4-op  5-list  6-exp

def p_exp_opassign_some(p):
    'opassign : LARROW exp'
    p[0] = (p.lineno(1), p[2])

def p_exp_opassign_none(p):
    'opassign : '
    p[0] = ('no-init')

# def p_let_innerop(p):
#     'innerop : identifier COLON type opassign'
#     p[0] = (p.lineno(1), p[1], p[3], p[4])

def p_exp_letlist_none(p):
    'letlist : '
    p[0] = []

# def p_exp_letlistbegin_some(p):
#     'letlistbegin : innerop letlist'
#     p[0] = [p[1]] + p[2]

def p_exp_letlist_some(p):
    'letlist : COMMA identifier COLON type opassign letlist'
    p[0] = [(p.lineno(1), p[2], p[4], p[5])] + p[6]
    #               0       1     2     3       4
## end of let bindinggggs

## basic match and stuff below it
## math and simple compatirons and
## integers and booleans
def p_exp_times(p):
    'exp : exp TIMES exp'
    p[0] = (p.lineno(2), p[1], 'times', p[3])

def p_exp_divide(p):
    'exp : exp DIVIDE exp'
    p[0] = (p.lineno(2), p[1], 'divide', p[3])

def p_exp_add(p):
    'exp : exp PLUS exp'
    p[0] = (p.lineno(2), p[1], 'plus', p[3])

def p_exp_minus(p):
    'exp : exp MINUS exp'
    p[0] = (p.lineno(2), p[1], 'minus', p[3])

def p_exp_tilde(p):
    'exp : TILDE exp'
    p[0] = (p.lineno(1), p[2], 'negate')

def p_exp_lt(p):
    'exp : exp LT exp'
    p[0] = (p.lineno(2), p[1], 'lt', p[3])

def p_exp_le(p):
    'exp : exp LE exp'
    p[0] = (p.lineno(2), p[1], 'le', p[3])

def p_exp_equals(p):
    'exp : exp EQUALS exp'
    p[0] = (p.lineno(2), p[1], 'eq', p[3])

def p_exp_not(p):
    'exp : NOT exp'
    p[0] = (p.lineno(1), p[2], 'not')

def p_exp_paren(p):
    'exp : LPAREN exp RPAREN'
    p[0] = p[2]

def p_exp_identifier(p):
    'exp : IDENTIFIER'
    p[0] = (p.lineno(1), p[1], 'identifier')

def p_exp_integer(p):
    'exp : INTEGER'
    p[0] = (p.lineno(1), p[1], 'integer')

def p_exp_string(p):
    'exp : STRING'
    p[0] = (p.lineno(1), p[1], 'string')

def p_exp_false(p):
    'exp : FALSE'
    p[0] = (p.lineno(1), p[1], 'bool')

def p_exp_true(p):
    'exp : TRUE'
    p[0] = (p.lineno(1), p[1], 'bool')
#
# end of basic parsing
# below here is parse error-handling


def p_error(p):
    if p:
         print("ERROR: " + p.lineno + ": Parser: parse error near " + p.type)
         exit(1)
    else:
         print("Syntax error at EOF", )
# Build the parser
parser = yacc.yacc()
ast = parser.parse(lexer=pa2lexer)

ast_filename  = (sys.argv[1])[:-4] + "-ast"
fout = open(ast_filename, 'w')


## done with the setup stuff
## this will print the right things to the document
## all functions we can call when needed
def print_list(ast, print_element_function):
    fout.write(str(len(ast)) + "\n")
    for elem in ast:
        print_element_function(elem)

def print_identifier(ast):
    fout.write(str(ast[0]) + "\n")
    fout.write(ast[1] + "\n")

def print_exp_list(ast):
    for item in ast:
        print_exp(item)

def print_formal(ast):
    for item in ast:
        print_identifier(item)

def print_let_list(ast):
    for item in ast:
        print_isbindinit(item[1], item[2], item[3])

def print_isbindinit(ident, istype, opass):

    if opass == 'no-init':
        fout.write("let_binding_no_init" + "\n")
        print_identifier(ident)
        print_identifier(istype)
    else:
        fout.write("let_binding_init" + "\n")
        print_identifier(ident)
        print_identifier(istype)
        #print("opass",opass[1][2])
        print_exp(opass[1])

# print the type of items that are in a case list
def print_exppoint_list(ast):
    #print(ast)
    fout.write(str(len(ast))+"\n")
    for item in ast:
        print_identifier(item[1])
        print_identifier(item[3])
        print_exp(item[4])

def print_feature(ast):
    if ast[1] == "method" :
        fout.write("method\n")
        print_identifier(ast[3])
        print_list(ast[2], print_formal)
        print_identifier(ast[4])
        print_exp(ast[5])
    elif ast[1] == "attribute_init":
        fout.write("attribute_init\n")
        print_identifier(ast[2])
        print_identifier(ast[3])
        print_exp(ast[4])
    else:
        fout.write("attribute_no_init\n")
        print_identifier(ast[2])
        print_identifier(ast[3])

def print_class(ast):
    print_identifier(ast[2])
    if ast[1] == "class_inherit":
        fout.write("inherits\n")
        print_identifier(ast[3])
        print_list(ast[4], print_feature)
    else:
        fout.write("no_inherits\n")
        print_list(ast[3], print_feature)

## basically a giant case that will
## print all the different types of expressions
def print_exp(ast):

    fout.write(str(ast[0])+ "\n")
    if ast[2] in ["assign"]:
        fout.write(ast[2] +"\n")
        print_identifier(ast[1])
        print_exp(ast[3])
    elif ast[2] in ["let"]:
        fout.write(ast[2] + "\n") # let
        fout.write(str(1+len(ast[5]))+ "\n")
        print_isbindinit(ast[1], ast[3], ast[4]);
        print_let_list(ast[5])
        print_exp(ast[6])
    elif ast[2] in ["block"]:
        fout.write(ast[2] + "\n")
        fout.write(str(len(ast[1])) + "\n")
        print_exp_list(ast[1])
    elif ast[2] in ["static_dispatch"]:
        fout.write(ast[2] + "\n")
        print_exp(ast[1])
        print_identifier(ast[3])
        print_identifier(ast[4])
        fout.write(str(len(ast[5])) + "\n")
        print_exp_list(ast[5])
    elif ast[2] in ["dynamic_dispatch"]:
        fout.write(ast[2] + "\n")
        print_exp(ast[1])
        print_identifier(ast[3])
        fout.write(str(len(ast[4])) + "\n")
        print_exp_list(ast[4])
    elif ast[2] in ["self_dispatch"]:
        fout.write(ast[2] + "\n")
        print_identifier(ast[1])
        fout.write(str(len(ast[3])) + "\n")
        print_exp_list(ast[3])
    elif ast[2] in ["case"]:
        fout.write(ast[2] + "\n")
        print_exp(ast[1])
        print_exppoint_list(ast[3])
    elif ast[2] in ["if"]:
        fout.write(ast[2] +"\n")
        print_exp(ast[1])
        print_exp(ast[3])
        print_exp(ast[4])
    elif ast[2] in ["negate", "not", "isvoid"]:
        fout.write(ast[2] +"\n")
        print_exp(ast[1])
    elif ast[2] in ["eq", "lt", "le", "plus", "minus", "divide", "times", "while"]:
        fout.write(ast[2] +"\n")
        print_exp(ast[1])
        print_exp(ast[3])
    elif ast[2] in ["new"]:
        fout.write(ast[2] + "\n")
        print_identifier(ast[1])
    elif ast[2] in ["identifier"]:
        fout.write(ast[2] + "\n")
        print_identifier(ast)
    elif ast[2] in ["integer", "string"]:
        fout.write(ast[2] + "\n")
        fout.write(ast[1] + "\n")
    elif ast[2] in ["bool"]:
        fout.write(ast[1]+ "\n")


## final thing that DOES everything
## calls all the other functions on the enture thing
def print_program(ast):
    if len(ast) == 0:
        print("ERROR: 1: Parser: No Classes")
    else:
        print_list(ast, print_class)

## and finishing
print_program(ast)
fout.close()
