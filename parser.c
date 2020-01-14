/*
	Filename:			parser.c
	Compiler:			MS Visual Studio 2019
	Author:				Emmett Janssens, 040858839
	Course:				CST8152 - Compilers, Lab Section: 012
	Assignment:			3
	Date: December		December 5th, 2019
	Professor:			Sv. Ranev
	Purpose:			To implement the grammar functions that parse a text file
	Function List:
						match()
						syn_eh()
						syn_printe()
						gen_incode()

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
						pre_condtion()
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

#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "buffer.h"
#include "parser.h"

/*
	Purpose:			To match Tokens fed to the parser from the Scanner
						
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions: 
						malar_next_token()
						syn_eh()
						syn_printe()

	Parameters:			pr_token_code		- The desired token code to be matched with
						pr_token_attribute	- The desired token attribute to be matched with

	Return Value:		NA
	Algorithm:			1. Check if the Token to be matched is a keyword, logical operator, arithmetic operator
						   or relational operator and compare the Token code and attribute
							a. If the code AND attribute match the desired token code and attribute get the next token
							b. If the next token is an error token report the error, get the next tokek, increment the #of errors and return
							c. If the code AND attribute don't match the desired token code and attribute call syn_eh and return
						2. If the Token isn't a keyword, logical operator, arithmetic operator or relational operator 
							a. If the Token is SEOF return
							b. If the Token is not SEOF performed steps a-c from part 1

*/
void match(int pr_token_code, int pr_token_attribute)
{
	//Keyword, Logical Operator, Arithmetic Operator, Relational Operator
	if (pr_token_code == KW_T || pr_token_code == LOG_OP_T || pr_token_code == ART_OP_T
		|| pr_token_code == REL_OP_T)
	{
		if (pr_token_code == KW_T && pr_token_attribute == lookahead.attribute.kwt_idx
			|| pr_token_code == LOG_OP_T && pr_token_attribute == lookahead.attribute.log_op
			|| pr_token_code == ART_OP_T && pr_token_attribute == lookahead.attribute.arr_op
			|| pr_token_code == REL_OP_T && pr_token_attribute == lookahead.attribute.rel_op)
		{
			lookahead = malar_next_token();

			if (lookahead.code == ERR_T)
			{
				syn_printe();
				lookahead = malar_next_token();
				++synerrno;
				return;
			}
		}
		else
		{
			syn_eh(pr_token_code);
			return;
		}
	}
	//Any other token
	else
	{
		if (pr_token_code == lookahead.code && lookahead.code == SEOF_T)
		{
			return;
		}
		else if (pr_token_code == lookahead.code && lookahead.code != SEOF_T)
		{
			lookahead = malar_next_token();

			if (lookahead.code == ERR_T)
			{
				syn_printe();
				lookahead = malar_next_token();
				++synerrno;
				return;
			}
		}
		else
		{
			syn_eh(pr_token_code);
			return;
		}
	}
}

/*
	Purpose:			In the case of a syntax error traverse through the file until the 
						token that was to be matched with or SEOF is found

	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:
						syn_printe()
						exit()
						malar_next_token()

	Parameters:			sync_token_code		The Token code the parser will continuosly search the text file for 

	Return Value:		NA
	Algorithm:			1. Call syn_printe()
						2. Increment the number of syntax errors
						3. Continue getting Tokens until you get the sync_token_code
							a. If the parser finds the SEOF Token before the sync_token_code ext the program
						4. If the the sync_token code is found before SEOF, make sure the token is not SEOF and return

*/
void syn_eh(int sync_token_code)
{
	syn_printe();
	++synerrno;

	//Search for the sync Token
	while (sync_token_code != lookahead.code)
	{
		//Exit if SEOF is found
		if (lookahead.code == SEOF_T)
		{
			exit(synerrno);
		}

		lookahead = malar_next_token();
	}

	//Ensure the Token is not SEOF
	if (lookahead.code != SEOF_T)
	{
		lookahead = malar_next_token();
	}

	return;
}

/*
	Purpose:			To print the syntax error Tokens
	Author:				Svillen Ranev
	Version:			1.0
	Called Functions:
						printf()
						b_mark()

	Parameters:			NA

	Return Value:		NA
	Algorithm:			1. Print that there is a syntax error and the line number in the file that syntax error
						   ocurred on.
						2. Print the code and any associated attribute of the Token causing the error

*/
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		b_mark(str_LTBL, t.attribute.str_offset);
		printf("%s\n", b_location(str_LTBL));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/

/*
	Purpose:			To print that a portion of the grammar has been parsed
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:
						printf()

	Parameters:			NA

	Return Value:		NA
	Algorithm:			
*/
void gen_incode(char* string)
{
	printf("%s", string);
}

/*
	<parser> -> 
		<program> SEOF_T

	FIRST(<parser>) -> {PLATYPUS}
*/
void parser(void)
{
	lookahead = malar_next_token();
	program(); match(SEOF_T, NA);
	gen_incode("PLATY: Source file parsed\n");
}

/*
	<program> ->
		PLATYPUS { <opt_statements> }

	FIRST(<program>) -> {PLATYPUS}
*/
void program(void)
{
	match(KW_T, PLATYPUS); match(LBR_T, NA);
	opt_statements();
	match(RBR_T, NA);
	gen_incode("PLATY: Program parsed\n");
}

/*
	<opt_statements> ->
		<statements>

	FIRST(opt_statements) -> {AVID, SVID, IF, WHILE, READ, WRITE}
*/
void opt_statements(void)
{
	if (lookahead.code == AVID_T || lookahead.code == SVID_T || 
		(lookahead.code == KW_T && lookahead.attribute.kwt_idx == IF || lookahead.attribute.kwt_idx == WHILE || lookahead.attribute.kwt_idx == READ || lookahead.attribute.kwt_idx == WRITE))
	{
		statements();
		return;
	}
	else if (lookahead.code == ERR_T)
	{
		gen_incode("PLATY: Opt_statements parsed\n");
		return;
	}

	gen_incode("PLATY: Opt_statements parsed\n");
}

/*
	<statements> ->
	<statement> <statements_p>

	FIRST(<statements>) -> {AVID, SVID, IF, WHILE, READ, WRITE}
*/
void statements(void)
{
	statement(); statements_p();
}

/*
	<statement> ->
		<assignment statement>
	  | <selection statement>
	  | <iteration statement>
	  | <input statement>
	  | <output statement>

	FIRST(<statement>) -> {AVID, SVID, IF, WHILE, READ, WRITE}
*/
void statement(void)
{
	if (lookahead.code == AVID_T || lookahead.code == SVID_T)
	{
		assignment_statement();
	}
	else if (lookahead.code == KW_T && lookahead.attribute.kwt_idx == IF)
	{
		selection_statement();
	}
	else if (lookahead.code == KW_T && lookahead.attribute.kwt_idx == WHILE)
	{
		iteration_statement();
	}
	else if (lookahead.code == KW_T && lookahead.attribute.kwt_idx == READ)
	{
		input_statement();
	}
	else if (lookahead.code == KW_T && lookahead.attribute.kwt_idx == WRITE)
	{
		output_statement(); 
	}
	else
	{
		syn_printe(); 
	}
}

/*
	<statements_p> ->
		<statement> <statements_p> | e

	FIRST(<statements_p>) -> {AVID, SVID, IF, WHILE, READ, WRITE}
*/
void statements_p(void)
{
	if (lookahead.code == AVID_T || lookahead.code == SVID_T ||
		(lookahead.code == KW_T && lookahead.attribute.kwt_idx == IF || lookahead.attribute.kwt_idx == WHILE || lookahead.attribute.kwt_idx == READ || lookahead.attribute.kwt_idx == WRITE))
	{
		statement(); statements_p();
	}
}

/*
	<input statement> ->
		READ ( <variable list> );

	FIRST(<input statement>) -> {READ}
*/
void input_statement(void)
{
	match(KW_T, READ); match(LPR_T, NA); variable_list(); 
	match(RPR_T, NA); match(EOS_T, NA);

	gen_incode("PLATY: Input statement parsed\n");
}

/*
	<variable list> ->
		<variable identifier> <variable list p>

	FIRST(<variable list>) -> {AVID_T, SVID_T}
*/
void variable_list(void)
{
	variable_identifier(); variable_list_p();
}

/*
	<variable identifier> ->
		AVID_T | SVID_T

	FIRST(<variable identifier>) -> {AVID_T, SVID_T}
*/
void variable_identifier()
{
	if (lookahead.code == AVID_T)
	{
		match(AVID_T, NA);
	}
	else if (lookahead.code == SVID_T)
	{
		match(SVID_T, NA);
	}
	else
	{
		syn_printe(); 
	}
}

/*
	<variable list p> ->
		, <variable list> | e

	FIRST(<variable list p>) -> {,, e}
*/
void variable_list_p()
{
	if (lookahead.code == COM_T)
	{
		match(COM_T, NA); variable_list();
		return;
	}
	else if (lookahead.code == ERR_T)
	{
		syn_printe(); 
	}

	gen_incode("PLATY: Variable list parsed\n");
}

/*
	<output statement> ->
		WRITE ( <output options> );

	FIRST(<output statements>) -> {WRITE}
*/
void output_statement(void)
{
	match(KW_T, WRITE); match(LPR_T, NA); output_options(); 
	match(RPR_T, NA); match(EOS_T, NA);
	gen_incode("PLATY: Output statement parsed\n");
}

/*
	<output options> ->
		<variable list> | STR_T | e

	FIRST(<output options>) -> {AVID_T, SVID_T, STR_T, e}
*/
void output_options(void)
{
	if (lookahead.code == AVID_T || lookahead.code == SVID_T)
	{
		variable_list(); 
		return;
	}
	else if (lookahead.code == STR_T)
	{
		match(STR_T, NA);
		gen_incode("PLATY: Output list (string literal) parsed\n");
		return;
	}
	else if (lookahead.code == ERR_T)
	{
		syn_printe(); 
	}

	gen_incode("PLATY: Output list (empty) parsed\n");
}

/*
	<iteration statement> ->
		WHILE <pre-condition> ( <conditional expression> )
		REPEAT { <statements> };
*/
void iteration_statement(void)
{
	match(KW_T, WHILE);
	pre_condition();
	match(LPR_T, NA);
	conditional_expression();
	match(RPR_T, NA); match(KW_T, REPEAT);
	match(LBR_T, NA);
	statements();
	match(RBR_T, NA); match(EOS_T, NA);
	gen_incode("PLATY: Iteration statement parsed\n");
}

/*
	<assignment statement> ->
		<assignment expression>

	FIRST(<assignment statement>) -> {AVID, SVID}
*/
void assignment_statement(void)
{
	assignment_expression(); match(EOS_T, NA);
	gen_incode("PLATY: Assignment statement parsed\n");
}

/*
	<assignment expression> ->
		AVID = <arithmetic expression>
	  | SVID = <string expression>

	FIRST(<assignment expression>) -> {AVID, SVID}
*/
void assignment_expression(void)
{
	if (lookahead.code == AVID_T)
	{
		match(AVID_T, NA); match(ASS_OP_T, NA); arithmetic_expression();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed\n");
	}
	else if (lookahead.code == SVID_T)
	{
		match(SVID_T, NA); match(ASS_OP_T, NA); string_expression();
		gen_incode("PLATY:  Assignment expression (string) parsed\n");
	}
	else
	{
		syn_printe();
	}
}

/*
	<arithmetic expression> ->
		<unary arithmetic expression>
	  | <additive arithmetic expression>

	FIRST(<arithmetic expression>) -> {-, +, AVID_T, FPL_T, INL_T, (}
*/
void arithmetic_expression(void)
{
	if (lookahead.code == ART_OP_T && (lookahead.attribute.arr_op == PLUS || lookahead.attribute.arr_op == MINUS))
	{
		unary_arithmetic_expression();
	}
	else if (lookahead.code == AVID_T || lookahead.code == FPL_T || lookahead.code == INL_T || lookahead.code == LPR_T)
	{
		additive_arithmetic_expression();
	}
	else
	{
		syn_printe(); 
	}

	gen_incode("PLATY: Arithmetic expression parsed\n");
}

/*
	<unary arithmetic expression> ->
		+ <primary arithmetic expression>
	  | - <primary arithmetic expression>
*/
void unary_arithmetic_expression(void)
{
	if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == PLUS)
	{
		match(ART_OP_T, PLUS); primary_arithmetic_expression();
	}
	else if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == MINUS)
	{
		match(ART_OP_T, MINUS); primary_arithmetic_expression();
	}
	else
	{
		syn_printe();
	}

	gen_incode("PLATY: Unary arithmetic expression parsed\n");
}

/*
	<additive arithmetic expression> ->
		<multiplicative arithmetic expression> <additive arithmetic expression p>

	FIRST(<additive arithmetic expreesion>) -> {AVID_T, FPL_T, INL_T, (}
*/
void additive_arithmetic_expression(void)
{
	multiplicative_arithmetic_expression(); additive_arithmetic_expression_p();
}

/*
	<additive arithmetic expression p> ->
		+ <multiplicative arithmetic expression> <additive arithmetic expression p> |
		- <multiplicative arithmetic expression> <additive arithmetic expression p> |
		e
*/
void additive_arithmetic_expression_p(void)
{
	if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == PLUS)
	{
		match(ART_OP_T, PLUS);
		multiplicative_arithmetic_expression();
		additive_arithmetic_expression_p();
	}
	else if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == MINUS)
	{
		match(ART_OP_T, MINUS);
		multiplicative_arithmetic_expression();
		additive_arithmetic_expression_p();
	}
	else if (lookahead.code == ERR_T)
	{
		syn_printe();
	}
	else
	{
		return;
	}

	gen_incode("PLATY: Additive arithmetic expression parsed\n");
}

/*
	<multiplicative arithmetic expression> ->
		<primary arithmetic expression> <multiplicative arithmetic expression p>

	FIRST(<multiplicative arithmetic expression>) -> {AVID_T, FPL_T, INL_T, (}
*/
void multiplicative_arithmetic_expression(void)
{
	primary_arithmetic_expression(); multiplicative_arithmetic_expression_p();
}

/*
	<multiplicative arithmetic expression p> ->
		* <multiplicative arithmetic expression> <multiplicative arithmetic expression p> |
		/ <multiplicative arithmetic expression> <multiplicative arithmetic expression p> |
		e
*/
void multiplicative_arithmetic_expression_p(void)
{
	if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == MULT)
	{
		match(ART_OP_T, MULT);
		primary_arithmetic_expression();
		multiplicative_arithmetic_expression_p();
	}
	else if (lookahead.code == ART_OP_T && lookahead.attribute.arr_op == DIV)
	{
		match(ART_OP_T, DIV);
		primary_arithmetic_expression();
		multiplicative_arithmetic_expression_p();
	}
	else if (lookahead.code == ERR_T)
	{
		syn_printe();
	}
	else
	{
		return;
	}

	gen_incode("PLATY: Multiplicative arithmetic expression parsed\n");
}

/*
	GRAMMAR:
	<primary arithmetic expression> -> AVID_T | FPL_T | INL_T | (<arithmetic expression>)

	FIRST(<primary arithmetic expression>) -> {AVID_T, FPL_T, INL_T, (}
*/
void primary_arithmetic_expression(void)
{
	if (lookahead.code == AVID_T)
	{
		match(AVID_T, NA);
	}
	else if (lookahead.code == FPL_T)
	{
		match(FPL_T, NA);
	}
	else if (lookahead.code == INL_T)
	{
		match(INL_T, NA);
	}
	else if (lookahead.code == LPR_T)
	{
		match(LPR_T, NA);
		arithmetic_expression();
		match(RPR_T, NA);
	}
	else
	{
		syn_printe(); 
	}

	gen_incode("PLATY: Primary arithmetic expression parsed\n");
}

/*
	<selection statement> ->
		IF <pre-condition> ( <conditional expression> ) THEN { <opt_statements> }
		ELSE { <opt_statements> };

	FIRST(<selection statement>) -> {IF}
*/
void selection_statement(void)
{
	match(KW_T, IF); pre_condition(); match(LPR_T, NA); conditional_expression(); match(RPR_T, NA);
	match(KW_T, THEN); match(LBR_T, NA); opt_statements(); match(RBR_T, NA); match(KW_T, ELSE);
	match(LBR_T, NA); opt_statements(); match(RBR_T, NA); match(EOS_T, NA);

	gen_incode("PLATY: Selection statement parsed\n");
}

/*
	<pre-condition> ->
		TRUE | FALSE

	FIRST(<pre-condition>) -> {TRUE, FALSE}
*/
void pre_condition(void)
{
	if (lookahead.code == KW_T && lookahead.attribute.kwt_idx == TRUE)
	{
		match(KW_T, TRUE);
	}
	else if (lookahead.code == KW_T && lookahead.attribute.kwt_idx == FALSE)
	{
		match(KW_T, FALSE);
	}
	else
	{
		syn_printe(); 
	}
}

/*
	<conditional expression> ->
		<logical OR expression>

	FIRST(<conditional expression>) -> {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void conditional_expression(void)
{
	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed\n");
}

/*
	<logical OR expression> ->
		<logical AND expression> <logical OR expression p>

	FIRST(<logical OR expression>) -> {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void logical_OR_expression(void)
{
	logical_AND_expression(); logical_OR_expression_p();
}

/*
	<logical OR expression p> ->
		.OR. <logical OR expression> | e

	FIRST(<logical OR expression p>) -> {.OR., e}
*/
void logical_OR_expression_p(void)
{
	if (lookahead.code == LOG_OP_T && lookahead.attribute.arr_op == OR)
	{
		match(LOG_OP_T, OR); logical_OR_expression();
	}
	else if (lookahead.code == ERR_T)
	{
		syn_printe(); 
	}
	else
	{
		return;
	}

	gen_incode("PLATY: Logical OR expression parsed\n");
}

/*
	<logical AND expression> ->
		<relational expression> <logical AND expression p>

	FIRST(<logical AND expression>) -> {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void logical_AND_expression(void)
{
	relational_expression(); logical_AND_expression_p();
}

/*
	<logical AND expression p> ->
		.AND. <logical AND expression> | e

	FIRST(<logical AND expression p>) -> {.AND., e}
*/
void logical_AND_expression_p(void)
{
	if (lookahead.code == LOG_OP_T && lookahead.attribute.arr_op == AND)
	{
		match(LOG_OP_T, AND); logical_AND_expression();
	}
	else if (lookahead.code == ERR_T)
	{
		syn_printe(); 
	}
	else
	{
		return;
	}

	gen_incode("PLATY: Logical AND expression parsed\n");
}

/*
	<relational expression> ->
		<primary a_relational expression> <relational expression options> <primary a_relational expression>
	  | <primary s_relational expression> <relational expression options> <primary s_relational expression>

	  FIRST(<relational expression>) -> {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void relational_expression(void)
{
	if (lookahead.code == AVID_T || lookahead.code == FPL_T || lookahead.code == INL_T)
	{
		primary_a_relational_expression(); relational_expression_opt(); primary_a_relational_expression();
	}
	else if (lookahead.code == SVID_T || lookahead.code == STR_T)
	{
		primary_s_relational_expression(); relational_expression_opt(); primary_s_relational_expression();
	}
	else
	{
		syn_printe();
	}

	gen_incode("PLATY: Relational expression parsed\n");
}

/*
	<primary a_relational expression> ->
		AVID_T
	  | FPL_T
	  | INL_T

	FIRST(<primary a_relational expression>) -> {AVID_T, FPL_T, INL_T}
*/
void primary_a_relational_expression(void)
{
	if (lookahead.code == AVID_T)
	{
		match(AVID_T, NA);
	}
	else if (lookahead.code == FPL_T)
	{
		match(FPL_T, NA);
	}
	else if (lookahead.code == INL_T)
	{
		match(INL_T, NA);
	}
	else
	{
		syn_printe(); 
	}

	gen_incode("PLATY: Primary a_relational expression parsed\n");
}


/*
	<relational expression options> ->
		== | <> | > | <

	FIRST(<relational expression options>) -> {==, <>, >, <}
*/
void relational_expression_opt(void)
{
	if (lookahead.code == REL_OP_T && lookahead.attribute.rel_op == EQ)
	{
		match(REL_OP_T, EQ);
	}
	else if (lookahead.code == REL_OP_T && lookahead.attribute.rel_op == NE)
	{
		match(REL_OP_T, NE);
	}
	else if (lookahead.code == REL_OP_T && lookahead.attribute.rel_op == GT)
	{
		match(REL_OP_T, GT);
	}
	else if (lookahead.code == REL_OP_T && lookahead.attribute.rel_op == LT)
	{
		match(REL_OP_T, LT);
	}
	else
	{
		syn_printe(); 
	}
}

/*
	<primary s_relational expression> ->
		<primary string expression>

	FIRST(<primary s_relational expression>) -> {SVID_T, STR_T}
*/
void primary_s_relational_expression(void)
{
	primary_string_expression();
	gen_incode("PLATY: Primary s_relational expression parsed\n");
}

/*
	<primary string expression> ->
		SVID_T | STR_T

	FIRST(primary string expression) -> {SVID_T, STR_T}
*/
void primary_string_expression(void)
{
	if (lookahead.code == SVID_T)
	{
		match(SVID_T, NA);
	}
	else if (lookahead.code == STR_T)
	{
		match(STR_T, NA);
	}
	else 
	{
		syn_printe();
	}

	gen_incode("PLATY: Primary string expression parsed\n");
}

/*
	<string expression> ->
		<primary string expression> <string expression p>

	FIRST(<string expression>) -> {SVID_T, STR_T}
*/
void string_expression(void)
{
	primary_string_expression(); string_expression_p(); 
}

/*
	<string expression p> ->
		<< <string expression> | e

	FIRST(<string expression p>) -> {<<, e}
*/
void string_expression_p(void)
{
	if (lookahead.code == SCC_OP_T)
	{
		match(SCC_OP_T, NA);
		string_expression();
	}
	else if (lookahead.code == ERR_T)
	{
		syn_printe(); 
	}
	else
	{
		gen_incode("PLATY: String expression parsed\n");
	}
}