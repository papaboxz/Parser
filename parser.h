/*
	Filename:			parser.h
	Compiler:			MS Visual Studio 2019
	Author:				Emmett Janssens, 040858839
	Course:				CST8152 - Compilers, Lab Section: 012
	Assignment:			3
	Date: December		December 5th, 2019
	Professor:			Sv. Ranev
	Purpose:			To create the constants and function prototypes to be used in the parser
	Function List:		
						parser()
						program()
						opt_statements()
						statements()
						statement()
						statements_p()
						input_statement()
						variable_list()
						variable_identifier()
						variable_list_p()	
						output_statement()
						output_options()
						iteration_statement()
						assignment_statement()
						assignment_expression()
						arithmetic_expression()
						unary_arithmetic_expression()
						additive_arithmetic_expression()
						additive_arithmetic_expression_p()
						multiplicative_arithmetic_expression()
						multiplicative_arithmetic_expression_p()
						primary_arithmetic_expression()
						selection_statement()
						pre_condtiion()
						conditional_expression()
						logical_OR_expression()
						logical_OR_expression_p()
						logical_AND_expression()
						logical_AND_expression_p()
						relational_expression()
						primary_a_relational_expression()
						relational_expression_opt()
						primary_s_relational_expression()
						primary_string_expression()
						string_expression()
						string_expression_p()

*/

#ifndef PARSER_H_
#define PARSER_H_

#include "token.h"

//No Attribute Value
#define NA -1

//Keyword Constants for Sanity
#define ELSE 0
#define FALSE 1
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE 9

//Arithmetic Operator Constants for Sanity
#define PLUS 0
#define MINUS 1
#define MULT 2
#define DIV 3

//Logical Operator Constants for Sanity
#define AND 0
#define OR  1

//Relational Operator Constants for Sanity
#define EQ 0
#define NE 1
#define GT 2
#define LT 3

//Next Token
static Token lookahead;

//Error Count
extern int synerrno = 0; 

//String Literal Table
extern pBuffer str_LTBL;

//Keyword Table
extern char* kw_table[];

//Current Line Count
extern int line;

//Used to Get the Next Token
extern Token malar_next_token(void);

//Parser Work Functions
void match(int, int);
void syn_eh(int sync_token_code);
void syn_printe();
void gen_incode(char*);

//Parser Grammar Functions
void parser(void);
void program(void);
void opt_statements(void);
void statements(void);
void statement(void);
void statements_p(void);
void input_statement(void);
void variable_list(void);
void variable_identifier(void);
void variable_list_p(void);
void output_statement(void);
void output_options(void);
void iteration_statement(void);
void assignment_statement(void);
void assignment_expression(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_p(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_p(void);
void primary_arithmetic_expression(void);
void selection_statement(void);
void pre_condition(void);
void conditional_expression(void);
void logical_OR_expression(void);
void logical_OR_expression_p(void);
void logical_AND_expression(void);
void logical_AND_expression_p(void);
void relational_expression(void);
void primary_a_relational_expression(void);
void relational_expression_opt(void);
void primary_s_relational_expression(void);
void primary_string_expression(void);
void string_expression(void);
void string_expression_p(void);

#endif // !PARSER_H_

