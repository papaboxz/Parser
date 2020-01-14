/*
	File Name:		table.h
	Compiler:		MS Visual Studio 2019
	Author:			Emmett Janssens, 040858839
	Course:			CST8152 - Compilers, Lab Section: 012
	Assignment:		2
	Date:			November 10, 2019
	Professor:		Sv.Ranev
	Purpose:		To declare and define the attributes that represent
					the transitional table of the scanner and declare 
					the functions used in the finite state machine to 
					finalize the tokens produced by the scanner
	Function List:
					aa_func02()
					aa_func03()
					aa_func05()
					aa_func08()
					aa_func10()
					aa_func11_12()
*/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
 *    '\0' or one of 255,0xFF,EOF
 */

/*  Special case tokens processed separately one by one
 *  in the token-driven part of the scanner
 *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
 *  white space
 *  !!comment , ',' , ';' , '-' , '+' , '*' , '/', << ,
 *  .AND., .OR. , SEOF,
 */
 

#define ES 11 /* Error state  with no retract */
#define ER 12 /* Error state  with retract */
#define IS -1 /* Inavalid state */

#define RTE 1 /*Value to set errno to in the case of a run time error*/

/* State transition table definition */

// REPLACE *CN* WITH YOUR COLUMN NUMBER  

#define TABLE_COLUMNS 8
/*transition table - type of states defined in separate table */
int st_table[][TABLE_COLUMNS] =
{
	/* State 00 */ {1, 6, 4, ES, ES, 9, ER, ES},
	/* State 01 */ {1, 1, 1, 2, 3, ES, 2, 2},
	/* State 02 */ {IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 03 */ {IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 04 */ {ES, 4, 4, 7, 5, 5, 5, 5},
	/* State 05 */ {IS, IS, IS, IS, IS, IS, IS},
	/* State 06 */ {ES, 6, ES, 7, ES, ES, 5, 5},
	/* State 07 */ {8, 7, 7, 8, 8, 8, 8, 8},
	/* State 08 */ {IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 09 */ {9, 9, 9, 9, 9, 10, ER, 9},
	/* State 10 */ {IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 11 */ {IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 12 */ {IS, IS, IS, IS, IS, IS, IS, IS},
};

/* Accepting state table definition */
// REPLACE *N1*, *N2*, and *N3* WITH YOUR NUMBERS
#define ASWR 0 /* accepting state with retract */
#define ASNR 1 /* accepting state with no retract */
#define NOAS 2 /* not accepting state */

int as_table[ ] = {NOAS, NOAS, ASWR, ASNR, NOAS, ASWR, NOAS, NOAS, ASWR, NOAS, ASNR, ASNR, ASWR};

//Accepting state function declarations
Token aa_func02(char *lexeme); 
Token aa_func03(char *lexeme);
Token aa_func05(char *lexeme);
Token aa_func08(char *lexeme);
Token aa_func10(char *lexeme);
Token aa_func11_12(char *lexeme);

/* defining a new type: pointer to function (of one char * argument) 
   returning Token
*/  

typedef Token (*PTR_AAF)(char *lexeme);

//Accepting function callback array
PTR_AAF aa_table[ ] = 
{
	NULL, 
	NULL,
	aa_func02,
	aa_func03,
	NULL,
	aa_func05,
	NULL,
	NULL,
	aa_func08,
	NULL,
	aa_func10,
	aa_func11_12,
	aa_func11_12
};

//Array of function pointers
Token (*p[ ])(char*) = 
{
	NULL,
	NULL, 
	aa_func02, 
	aa_func03,
	NULL,
	aa_func05,
	NULL,
	NULL,
	aa_func08,
	NULL,
	aa_func10,
	aa_func11_12,
	aa_func11_12
};


/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char * kw_table []=
{
	"ELSE",
	"FALSE",
	"IF",
	"PLATYPUS",
	"READ",
	"REPEAT",
	"THEN",
	"TRUE",
	"WHILE",
	"WRITE"   
};

#endif
                     