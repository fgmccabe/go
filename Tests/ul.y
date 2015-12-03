/*
 * Grammar file for UL
 */

%token ID INT SEMI EQUAL PRINT HALT LPAR RPAR
%left PLUS MINUS
%left STAR SLASH 
%nonassoc UMINUS
%start program

%%

 program : statements ;

statements : statements statement
 | statement
 ;

statement : assignment 
 | print
 | halt 
 ;

assignment : ID EQUAL exp SEMI 
 ;

print : PRINT exp SEMI
 ;

halt : HALT SEMI 
 ;

exp : ID 		
 | INT			
 | exp PLUS exp         
 | exp MINUS exp        
 | exp STAR exp         
 | exp SLASH exp        
 | MINUS exp   %prec UMINUS  
 | LPAR exp RPAR    
 ;

