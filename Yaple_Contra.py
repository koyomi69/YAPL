import ply.lex as lex
import ply.yacc as yacc
import sys

##########################################################################################################################################
                                                            # LEXER 
##########################################################################################################################################

# Reserved words
reserved_words = (
    'struct', 'list', 'return', 'func', 'if', 'then', 'elseif', 'else', 'for', 'to', 'break', 'step', 'begin', 'end',
    'print', 'str', 'int', 'double', 'char', 'true', 'false'
)

# All Tokens
tokens = (
    'VAR',          # For Variable Name
    'STRUCT',       # Struct Use    
    'LIST',         # Lists 
    'FUNC',         # Functions
    'RETURN',       # Return Statement 
    'IF',           # IF Statement
    'ELSEIF',       # Compound IF Statement
    'ELSE',         # Else Statement    
    'FOR',          # For Loop
    'TO',           # For Loops syntax
    'THEN',         # IF Statement syntax
    'STEP',         # For telling how to increment loop
    'BREAK',        # Breaking a Loop
    'BEGIN',        # Begin Loop
    'END',          # End Loop
    'PRINT',        # For Display
    'STR',          # String
    'INT',          # Integers
    'DOUBLE',       # Decimal Numbers
    'CHAR',         # ASCII Characters
    'TRUE',         # True part of Bool
    'FALSE',        # False part of Bool
    'PLUS',         # +
    'MINUS',        # -
    'PLUSPLUS',     # ++
    'MINUSMINUS',   # --
    'MULT',         # *
    'DIV',          # /
    'MOD',          # %     
    'GRT',          # >
    'LET',          # <
    'GE',           # >=
    'LE',           # <=
    'EEQ',          # ==
    'NEQ',          # !=
    'ANDAND',       # &&
    'OROR',         # ||
    'LPAR',         # (
    'RPAR',         # )
    'LBRA',         # {
    'RBRA',         # }
    'LBRACK',       # [
    'RBRACK',       # ]
    'EQ',           # =
    'COMMA',        # ,
    'SEMICOLON',    # ;
    'DOT',          # .
    'NEWLINE',      # \n
    'SYMBOL'        # $
)

# Rules defined in the form of Regular Expressions 
t_ignore = ' \t\r\v'
t_STRUCT = r'struct'
t_SYMBOL = r'\$'
t_LIST = r'list'
t_FUNC = r'func'
t_RETURN = r'return'
t_IF = r'if'
t_ELSEIF = r'elseif'
t_ELSE = r'else'
t_FOR = r'for'
t_TO = r'to'
t_BREAK = r'break'
t_STEP = r'step'
t_THEN = r'then'
t_BEGIN = r'begin'
t_END = r'end'
t_PRINT = r'print'
t_TRUE = r'true'
t_FALSE = r'false'
t_PLUS = r'\+'
t_MINUS = r'\-'
t_PLUSPLUS = r'\+\+'
t_MINUSMINUS = r'\-\-'
t_MULT = r'\*'
t_DIV = r'/'
t_MOD = r'%'
t_GRT = r'>'
t_LET = r'<'
t_GE = r'>='
t_LE = r'<='
t_EEQ = r'=='
t_NEQ = r'!='
t_ANDAND =  r'&&'
t_EQ = r'='
t_OROR = r'\|\|'
t_LPAR = r'\('
t_RPAR = r'\)'
t_LBRA = r'{'
t_RBRA = r'}'
t_LBRACK = r'\['
t_RBRACK = r'\]'
t_COMMA = r','
t_SEMICOLON = r';'
t_DOT = r'\.'

def t_VAR(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    if t.value in reserved_words:
        t.type = t.value.upper()
    return t

def t_DOUBLE(t):
    r'[-+]?[0-9]*\.?[0-9]+'
    t.value = float(t.value)
    return t

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_STR(t):
    r'\".*?\"'
    return t

def t_CHAR(t):
    r'\'.*?\''
    return t        

def t_error(t):
    print ("Invalid Syntax '%s' at line: '%d'" % (t.value[0], t.lineno))
    t.lexer.skip(1)

def t_NEWLINE(t):
    r'\n+'
    t.lexer.lineno += len(t.value) 
    pass

##########################################################################################################################################
                                                            # PARSER 
##########################################################################################################################################

precedence=(
    ('left','ANDAND','OROR'),
    ('left','GRT','LET','GE','LE'),
    ('left','PLUS','MINUS'),
    ('left','MULT','DIV','MOD'),
)

def p_start(p):
    'start : contra'
    p[0] = p[1] 

def p_contra_node(p):
    'contra : node contra'
    p[0] = [p[1]] + p[2]

def p_contra_empty(p):
    'contra : '
    p[0] = [ ]

def p_node(p):
    'node : statement'
    p[0] = ('statement', p[1])

def p_statement(p):
    'statement : if_stmts'
    p[0] = p[1]

def p_statement_if(p):
    '''if_stmts : IF LPAR expr RPAR THEN LBRA statements RBRA last
                | IF LPAR expr RPAR THEN LBRA statements RBRA ELSEIF LPAR expr RPAR THEN LBRA statements RBRA ELSE LBRA statements RBRA last
                | IF LPAR expr RPAR THEN LBRA statements RBRA ELSEIF LPAR expr RPAR THEN LBRA statements RBRA ELSEIF LPAR expr RPAR THEN LBRA statements RBRA ELSE LBRA statements RBRA last 
                | IF LPAR expr RPAR THEN LBRA statements RBRA ELSE LBRA statements RBRA last'''
    if len(p) == 10:
        p[0] = ('if', p[3], p[7])
    elif len(p) == 14:
        p[0] = ('else', p[3], p[7], p[11])
    elif len(p) == 22:
        p[0] = ('elseif', p[3], p[7], p[11], p[15], p[19])
    else:
        p[0] = ('elseifelseif', p[3], p[7], p[11], p[15], p[19], p[23], p[27])

def p_statement_while(p):
    'statement : BEGIN LBRA statements RBRA END LPAR expr RPAR last'
    p[0] = ('dowhile', p[3], p[7])

def p_statement_for(p):
    'statement : FOR LPAR VAR EQ expr TO expr inc_step RPAR LBRA statements RBRA last'
    p[0] = ('for', p[3], p[5], p[7], p[8], p[11])

# def p_statement_func(p):
#     '''statement : FUNC VAR LPAR multi RPAR LBRA statements RBRA last
#                  | VAR LPAR multi RPAR last'''
#     if len(p) == 6:
#         p[0] = ('f_call', p[1], p[3])
#     else:
#         p[0] = ('f_def', p[2], p[4], p[7])

def p_statement_break(p):
    'statement : BREAK SEMICOLON'
    p[0] = ('break', None)

def p_inc_step(p):
    'inc_step : STEP expr'
    p[0] = p[2]

def p_statement_paran(p):
    '''statement : LPAR expr RPAR
                 | expr'''
    if len(p) == 4:
        p[0] = p[2]
    else:
        p[0] = p[1]

def p_statement_assign(p):
    'statement : VAR EQ expr last'
    p[0] = ('=', p[1], p[3])

def p_statement_inc(p):
    'statement : VAR PLUSPLUS last'
    p[0] = ('inc', p[1])

def p_statement_dec(p):
    'statement : VAR MINUSMINUS last'
    p[0] = ('dec', p[1])

def p_statement_print(p):
    '''statement : PRINT LPAR expr RPAR last
               | PRINT lstI last'''
    if len(p) == 6:
        p[0] = ('print', p[3], p[5])
    else:
        p[0] = ('print', p[2])

def p_end(p):
    'last : SYMBOL'
    p[0] = ('last', p[1])

# def p_statement_return(p):
#     '''statement : RETURN expr last
#                  | RETURN last'''
#     if len(p) == 4:
#         p[0] = ('return', p[2])
#     else:
#         p[0] = ('return', None)

def p_statement_list(p):
    'statement : list'
    p[0] = p[1]

def p_statement_newline(p):
    'statement : NEWLINE'
    p[0] = ('newline', None)

def p_statements(p):
    '''statements : statement statements
                  | empty'''
    if len(p) == 3:
        p[0] = [p[1]] + p[2]
    else:
        p[0] = []

def p_expr_binop(p):
    '''expr : expr PLUS expr
            | expr MINUS expr
            | expr MULT expr
            | expr DIV expr
            | expr MOD expr'''
    p[0] = ('binaryop', p[1], p[2], p[3])

def p_expr_bgroup(p):
    '''expr : LPAR expr PLUS expr RPAR
            | LPAR expr MINUS expr RPAR
            | LPAR expr MULT expr RPAR
            | LPAR expr DIV expr RPAR
            | LPAR expr MOD expr RPAR'''
    p[0] = ('binaryop', p[2], p[3], p[4])

def p_expr_compop(p):
    '''expr : expr GRT expr
            | expr LET expr
            | expr GE expr
            | expr LE expr
            | expr EEQ expr
            | expr NEQ expr  
            | expr ANDAND expr
            | expr OROR expr'''
    p[0] = ('compareop', p[1], p[2], p[3])

def p_expr_cgroup(p):
    '''expr : LPAR expr GRT expr RPAR
            | LPAR expr LET expr RPAR
            | LPAR expr GE expr RPAR
            | LPAR expr LE expr RPAR
            | LPAR expr EEQ expr RPAR
            | LPAR expr NEQ expr RPAR
            | LPAR expr ANDAND expr RPAR
            | LPAR expr OROR expr RPAR'''
    p[0] = ('compareop', p[2], p[3], p[4])

def p_expr_var(p):
    'expr : VAR'
    p[0] = ('var', p[1])

def p_expr_num(p):
    '''expr  : INT
             | DOUBLE'''
    p[0] = ('number', p[1])

def p_expr_str(p):
    'expr : STR'
    p[0] = ('string', p[1])

def p_expr_char(p):
    'expr : CHAR'
    p[0] = ('char', p[1])

def p_expr_checkBool(p):
    'expr : bool'
    p[0] = ('bool', p[1])

def p_expr_bool(p):
    '''bool : TRUE
            | FALSE'''
    p[0] = p[1]

def expr_list(p):
    'expr : lstI'
    p[0] = p[1]

def p_index(p):
    '''index : VAR
             | INT
             | DOUBLE'''
    p[0] = p[1]

def p_multi(p):
    '''multi : expr COMMA multi
             | single
             | empty'''
    if len(p) == 4:
        p[0] = [p[1]] + p[3]  
    elif len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = []  

def p_single(p):
    'single : expr'
    p[0] = [p[1]]

def p_list_more(p):
    'lstI : VAR LPAR index RPAR'
    p[0] = ('lstI', p[1], p[3])

def p_list(p):
    '''list : lstI
            | LIST VAR EQ LBRACK multi RBRACK last
            | LIST VAR LBRACK index RBRACK EQ expr last'''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 8:
        p[0] = ('lstD', p[2], p[5])
    else:
        p[0] = ('lstA', p[2], p[4], p[7])

def p_empty(p):
    'empty : '

def p_error(p):
    print ("Invalid Syntax '%s' at line: '%d'" % (p.value[0], p.lineno))
    exit(1)

##########################################################################################################################################
                                                            # INTERPRETER
##########################################################################################################################################
def eval_start(res, dic):
    for v in res:
        if (v[0] == 'statement'):
            eval_stmt(v[1], dic)

def add_to_dic(name_toAdd, value_toAdd, dic):
    dic[1][name_toAdd] = value_toAdd

def update_dic(name_toUpdate, value_toUpdate, dic):
    if name_toUpdate in dic[1]:
        dic[1][name_toUpdate] = value_toUpdate
    elif not dic[0] == None:
        return update_dic(name_toUpdate, value_toUpdate, dic[0])

def look_in_dic(name_toLook, dic):
    if name_toLook in dic[1]:
        return dic[1][name_toLook]
    elif dic[0] == None:
        return None
    else:
        return look_in_dic(name_toLook, dic[0])

def eval_expr(res, dic):
    etype = res[0]

    if etype == 'number':
        return res[1]
    elif etype == 'string':
        return res[1]
    elif etype == 'char':
        if len(res[1]) > 3:
            print("Invalid Character! Program exiting")
            exit(1)
        return res[1]
    elif etype == 'bool':
        return res[1]
    elif etype == 'binaryop':
        operator = res[2]
        left_expr = eval_expr(res[1], dic)
        right_expr = eval_expr(res[3], dic)
        if operator == "+":
            return left_expr + right_expr
        elif operator == "-":
            return left_expr - right_expr
        elif operator == '*':
            return left_expr * right_expr
        elif operator == '/':
            if right_expr == 0:
                print("Zero Error! Program exiting")
                exit(1)
            return left_expr / right_expr
        elif operator == '%':
            return left_expr % right_expr
        else:
            print('Runtime Error! Operator:'+ operator + 'Unknown')
            exit(1)

    elif etype == 'var':
        try:
            return look_in_dic(res[1], dic)
        except:
            print("Lookup Error! Variable " + res[1] + " not found!")
            exit(1)

    elif etype == 'compareop':
        operator = res[2]
        left = eval_expr(res[1], dic)
        right = eval_expr(res[3], dic)

        if operator == '>':
            return left > right
        elif operator == '>=':
            return left >= right
        elif operator == '<':
            return left < right
        elif operator == '<=':
            return left <= right
        elif operator == '==':
            return left == right
        elif operator == '!=':
            return left != right
        elif operator == '&&':
            return left and right
        elif operator == '||':
            return left or right
        else:
            print('Runtime Error! Operator:'+ operator + 'Unknown')
            exit(1)
    
    elif etype == 'lstI':
        if (look_in_dic(res[1], dic)):
            return look_in_dic(res[1], dic)[int(res[2])]
        else:
            print("Lookup Error! Variable " + res[1] + " not found!")
            exit(1)  

def eval_stmt(res, dic):
    stype = res[0]

    if stype == 'print':
        print ('ANS: ', eval_expr(res[1], dic))

    elif stype == '=':
        # First we return the value of the variable and stores it in a num variable
        # Then we search the lookup table to find the variable and then get its value.
        # If variable is not present, then add it into the lookup  Table
        # Else simply update its value
        num = eval_expr(res[2], dic)
        if (not look_in_dic(res[1], dic)):
            add_to_dic(res[1], num, dic)
        update_dic(res[1], num, dic)

    elif stype == 'inc':
        # First we search the lookup table to find the variable and then get its value.
        # If variable is present, then update its value
        # Else print error and exit
        if (look_in_dic(res[1], dic) != None):
            update_dic(res[1], look_in_dic(res[1], dic) + 1, dic)
        else:
            print('Not Found! Program is exiting')
            exit(1)

    elif stype == 'dec':
        # First we the search the lookup table to find the variable and then get its value.
        # If variable is present, then update its value
        # Else print error and exit
        if (look_in_dic(res[1], dic) != None):
            update_dic(res[1], look_in_dic(res[1], dic) - 1, dic)
        else:
            print('Not Found! Program is exiting')
            exit(1)

    elif stype == 'lstD':
        # First we return the values of the list and store it in list variable
        # Then we search the lookup table to find the variable and then get its values.
        # If variable is not present, then add it into the lookup  Table
        # Else simply update its value
        num_list = []
        for loop in res[2]:
            num_list.append(eval_expr(loop, dic))
        if (not look_in_dic(res[1], dic)):
            add_to_dic(res[1], num_list, dic)
        update_dic(res[1], num_list, dic)

    elif stype == 'lstA':
        # First we search the lookup table to find the list and then get its values.
        # If variable is not present, print error and exit
        # Else simply update its value at that specific index with the value to be assigned with   
        if res[2] > len(look_in_dic(res[1], dic)):
            print('Out of Order Index! Program Exiting')
            exit(1)
        if (not look_in_dic(res[1], dic)):
            print('Value not Found! Program Exiting')
            exit(1)
        look_in_dic(res[1], dic)[int(res[2])] = eval_expr(res[3], dic) 

    elif stype == 'if':
        if (eval_expr(res[1], dic)):
            for loop in res[2]:
                eval_stmt(loop, dic)

    elif stype == 'else':
        if (eval_expr(res[1], dic)):
            for loop in res[2]:
                eval_stmt(loop, dic)
        else:
            for loop in res[3]:
                eval_stmt(loop, dic)
        
    elif stype == 'elseif':
        if (eval_expr(res[1], dic)):
            for loop in res[2]:
                eval_stmt(loop, dic)
        elif (eval_expr(res[3], dic)):
            for loop in res[4]:
                eval_stmt(loop, dic)
        else:
            for loop in res[5]:
                eval_stmt(loop, dic)

    elif stype == 'elseifelseif':
        if (eval_expr(res[1], dic)):
            for loop in res[2]:
                eval_stmt(loop, dic)
        elif (eval_expr(res[3], dic)):
            for loop in res[4]:
                eval_stmt(loop, dic)
        elif (eval_expr(res[5], dic)):
            for loop in res[6]:
                eval_stmt(loop, dic)
        else:
            for loop in res[7]:
                eval_stmt(loop, dic)   

    elif stype == 'for':
        s_num = eval_expr(res[2], dic)
        e_num = eval_expr(res[3], dic)
        counter = eval_expr(res[4], dic)
        cond_exp = res[5]

        if (not look_in_dic(res[1], dic)):
            add_to_dic(res[1], s_num, dic)
        update_dic(res[1], s_num, dic)

        s_var = look_in_dic(res[1], dic)


    elif stype == 'dowhile':
        for loop in res[1]:
            eval_stmt(loop, dic)
        while eval_expr(res[2], dic):
            for loop in res[1]:
                eval_stmt(loop, dic)    

##########################################################################################################################################
                                                            # Main 
##########################################################################################################################################

if __name__ == '__main__':
    print('Lexer Starting')
    lexer = lex.lex()
    print('Parser Starting')
    parser = yacc.yacc()
    mode = sys.argv[1]
    environ_list = [None, {}]

    print()
    if mode == "Read":
        with open(sys.argv[2]) as f:
            data = f.read()
        eval_start(parser.parse(data, lexer=lexer), environ_list)

    elif mode == "Input":
        while True:
            data = input('<<) ')
            if data == "q":
                exit()
            eval_start(parser.parse(data, lexer=lexer), environ_list)
    else:
        print("Error! Wrong Argument")
        exit()

