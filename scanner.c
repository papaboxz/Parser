/*
	File Name:		scanner.c
	Compiler:		MS Visual Studio 2019
	Author:			Emmett Janssens, 040858839
	Course:			CST8152 - Compilers, Lab Section: 012
	Assignment:		2
	Date:			November 10, 2019
	Professor:		Sv.Ranev
	Purpose:		To define the functions used to initialize the scanner,
					process input from the scanner and to create and finalize
					tokens from that input
	Function List:
					scanner_init()
					malar_next_token()
					get_next_state()
					char_class()
					aa_func02()
					aa_func03()
					aa_func05()
					aa_func08()
					aa_func10()
					aa_func11_12()
					isKeyword()
*/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <math.h>
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern pBuffer str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static pBuffer lex_buf;	/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf;	/*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */ 
static int char_class(char c);				/* character class function */
static int get_next_state(int, char);		/* state machine function */
static int iskeyword(char * kw_lexeme);		/*keywords lookup functuion */


/*
	Purpose:			To initialize the properites of the scanner
	Author:				Sv.Ranev
	Version:			1.0
	Called Functions:
						b_isEmpty()
						b_rewind()
						b_clear()
	Parameters:
						pBuffer psc_buf 
	Return Value: 
						exit status - If the buffer used is a valid buffer or otherwise
	Algorithm:
						1. Check if the buffer is empty
						2. If the buffer is valid rewind and clear the buffer
						3. Set the line count to 1 and point the scanner buffer to the buffer to be read from
*/
int scanner_init(pBuffer psc_buf) {
  	if(b_isEmpty(psc_buf)) return EXIT_FAILURE;	/*1*/

	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS; /*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

/*
	Purpose:			To build tokens using characters read in from a buffer and to process
						those tokens by setting their attributes and codes. 
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:
						b_getc()
						b_retract()
						strcpy()
						b_mark()
						b_getcoffset()
						get_next_state()
						b_addc()
						b_allocate()
						b_location()
						b_free()
	Parameters:
						N/A
	Return Value:
						t - A token that has had its attributes set based on the characters
							read in from a buffer
	Algorithm:
						1. Process the special characters [+, -, {, } etc] that can be read in from the buffer.
						   Each character has its own if statement(s)
						2. Process the tokens and keywords:
							a. Set the lexeme start location to the current get offset of the scanner buffer
							b. While the scanner is not in an accepting state navigate through the transition
							   table using the get_next_state() method until an accepting state is reached
							c. If the accepting state is also a retracting state, retract the scanner buffer
							d. Set the lexeme end location to the current get offset of the scanner buffer
							e. Create a new buffer to hold the lexeme
							f. Retract the scanner buffer until the get offset is equal to the lexeme's start location
							g. Add characters from the scanner buffer to the lexeme buffer until the end of the lexeme has been reached
							h. Add the \0 character to the lexeme buffer to make the lexeme a c-string
							i. Create a pointer pointing to the start of the lexeme buffer
							j. Call one of the accepting state functions using the scanner's current state, passing the lexeme pointer
							   as the parameter. This will finalize the token's properties.
							k. Free the lexeme buffer
*/
Token malar_next_token(void)
{
	//The token whose properties will be set
	Token t = { 0 };

	//The current character in the buffer
	unsigned char c;

	//The current state of the transition table
	int state = 0;

	//The start location of the lexeme in the scanner buffer
	short lexstart;

	//The end location of the lexeme in the scanner buffer
	short lexend;

	//Character used to check the success of the get_c() method in nested if statements
	char temp;

	//Used to check if the correct value of the b_getcoffset() is correct
	short tempOffset;

	//1. Process the special tokens
	while (1)
	{
		//Reset the state
		state = 0;
		
		//Get the next character from the scanner buffer
		c = b_getc(sc_buf);

		//Ensure that b_getc() was successful
		if (c == -2)
		{
			errno = RTE;
			strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
			t.code = ERR_T; 
			return t;
		}

		//End of File
		if (c == '\0')
		{
			t.attribute.seof = SEOF_0;
			t.code = SEOF_T;
			return t;
		}
		else if (c == EOF || c == 255)
		{
			t.attribute.seof = SEOF_EOF;
			t.code = SEOF_T;
			return t;
		}
		//End of Statement
		else if (c == ';')
		{
			t.code = EOS_T;
			return t;
		}
		//Blank Space / End of Line
		else if (c == ' ' || c == '\t')
		{
			continue;
		}
		else if (c == '\n')
		{
			++line;
			continue;
		}
		//Assignment Operator
		else if (c == '=')
		{
			temp = b_getc(sc_buf);

			//Ensure getc() was successful
			if (temp == -2)
			{
				errno = RTE;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				t.code = ERR_T;
				return t;
			}

			//Equals Operator
			if (temp == '=')
			{
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
			}

			//Retract if the next character is not = and check if the retract was successful
			if (b_retract(sc_buf) == -1)
			{
				errno = RTE;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				t.code = ERR_T;
				return t;
			}

			t.code = ASS_OP_T;
			return t;
		}
		//Comma
		else if (c == ',')
		{
			t.code = COM_T;
			return t;
		}
		//Left Parenthesis
		else if (c == '(')
		{
			t.code = LPR_T;
			return t;
		}
		//Right Parenthesis
		else if (c == ')')
		{
			t.code = RPR_T;
			return t;
		}
		//Left Brace
		else if (c == '{')
		{
			t.code = LBR_T;
			return t;
		}
		//Right Brace
		else if (c == '}')
		{
			t.code = RBR_T;
			return t;
		}
		//Less Than Operator
		else if (c == '<')
		{
			//Get the next character in the scanner buffer
			temp = b_getc(sc_buf);

			//Ensure b_getc() was successful
			if (temp == -2)
			{
				errno = RTE;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				t.code = ERR_T;
				return t;
			}

			//Not-equals relational operator <>
			if (temp == '>')
			{
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;
				return t;
			}

			//String concatenation operator <<
			if (temp == '<')
			{
				t.code = SCC_OP_T;
				return t;
			}

			//If the sequence of characters does not match <> or << retract and verify it was successful
			if (b_retract(sc_buf) == -1)
			{
				errno = RTE;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				t.code = ERR_T;
				return t;
			}

			//Found a < sign
			t.code = REL_OP_T;
			t.attribute.rel_op = LT;
			return t;
		}
		//Greater Than Operator
		else if (c == '>')
		{
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;
		}
		//Addition Operator
		else if (c == '+')
		{
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;
		}
		//Subtraction Operator
		else if (c == '-')
		{
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;
		}
		//Multiplcation Operator
		else if (c == '*')
		{
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;
		}
		//Division Operator
		else if (c == '/')
		{
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;
		}
		//Comments
		else if (c == '!')
		{
			temp = b_getc(sc_buf);

			//Ensure that b_getc() was successful
			if (temp == -2)
			{
				errno = RTE;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				t.code = ERR_T;
				return t;
			}
			
			if (temp == '!')
			{
				while (1)
				{
					temp = b_getc(sc_buf);

					//Ensure that b_getc() was successful
					if (temp == -2)
					{
						errno = RTE;
						strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
						t.code = ERR_T;
						return t;
					}

					if (temp == '\0' || temp == 'SEOF')
					{
						if (b_retract(sc_buf) == -1)
						{
							errno = RTE;
							strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
							t.code = ERR_T;
							return t;
						}

						t.code = ERR_T; 
						return t; 
					}

					if (temp == '\n')
					{
						++line;
						break;
					}
				}
				continue;
			}
			//If there is an invalid commment
			else
			{
				if (b_retract(sc_buf) == -1)
				{
					errno = RTE;
					strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
					t.code = ERR_T;
					return t;
				}

				temp = b_getc(sc_buf);

				//Ensure that b_getc() was successful
				if (temp == -2)
				{
					errno = RTE;
					strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
					t.code = ERR_T;
					return t;
				}

				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = temp;
				t.code = ERR_T;

				while (c != '\n')
				{
					c = b_getc(sc_buf);

					//Ensure that b_getc() was successful
					if (c == -2)
					{
						errno = RTE;
						strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
						t.code = ERR_T;
						return t;
					}
				}

				++line;
				return t; 
			}
		}
		//Logical Operators
		else if (c == '.')
		{
			int charIndex = 0; 

			//Move through the scanner buffer ensuring that the sequence of characters
			//needed for .AND. and .OR. are there
			while (1)
			{
				c = b_getc(sc_buf);

				if (c == -2)
				{
					errno = RTE;
					strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
					t.code = ERR_T;
					return t;
				}

				if ((c == 'A' || c == 'O') && charIndex == 0)
				{
					++charIndex;
				}
				else if ((c == 'N' || c == 'R') && charIndex == 1)
				{
					++charIndex;
				}
				else if (c == 'D' && charIndex == 2)
				{
					++charIndex;
				}
				else if (c == '.' && charIndex == 2)
				{
					t.attribute.log_op = 1;
					t.code = LOG_OP_T;
					return t;
				}
				else if ((c == '.') && charIndex == 3)
				{
					t.attribute.log_op = 0;
					t.code = LOG_OP_T;
					return t;
				}
				else
				{
					break;
				}
			}

			//If the sequence of characters is invalid retract the buffer and set the error token
			for (int i = 0; i <= charIndex; i++)
			{
				if (b_retract(sc_buf) == -1)
				{
					errno = RTE;
					strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
					t.code = ERR_T;
					return t;
				}
			}

			strcpy(t.attribute.err_lex, ".");
			t.code = ERR_T;
			return t;
		}

		//2.
		//Set the start of the lexeme buffer to the current get position in the scanner buffer
		lexstart = b_mark(sc_buf, b_getcoffset(sc_buf));

		if (lexstart == -1)
		{
			errno = RTE;
			strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
			t.code = ERR_T;
			return t;
		}
		/*
			While the transition table is not in an accepting state move through the states of the
			transition table using the characters received from the scanner buffer until an
			accepting state has been reached
		*/
		while (1)
		{
			//Get the next state
			state = get_next_state(state, c);

			//If the current state is not an accepting state
			if (as_table[state] == 2)
			{
				c = b_getc(sc_buf);

				if (c == -2)
				{
					errno = RTE;
					strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
					t.code = ERR_T;
					return t;
				}
			}
			else
			{
				break;
			}
		}

		//Retract if the accepting state is also a retracting state
		if (as_table[state] == 0)
		{
			if (b_retract(sc_buf) == -1)
			{
				errno = RTE;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				t.code = ERR_T;
				return t;
			}
		}

		//Set the end of the lexeme to the current get offset of the scanner buffer
		lexend = b_getcoffset(sc_buf);

		if (lexend == -1)
		{
			errno = RTE;
			strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
			t.code = ERR_T;
			return t;
		}

		//Create the lexeme buffer
		lex_buf = b_allocate(200, 0, 'f');
	
		if (lex_buf == NULL)
		{
			errno = RTE;
			strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
			t.code = ERR_T;
			return t;
		}

		//Retract the scanner buffer until lexstart is reached
		while (lexstart != b_getcoffset(sc_buf) + 1)
		{
			if (b_retract(sc_buf) == -1)
			{
				errno = RTE;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				t.code = ERR_T;
				return t;
			}
		}

		//For the length of the lexeme add characters from the scanner buffer to the lexeme buffer
		while (1)
		{
			tempOffset = b_getcoffset(sc_buf);

			if (tempOffset == -1)
			{
				errno = RTE;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				t.code = ERR_T;
				return t;
			}

			if (tempOffset == lexend)
			{
				break;
			}

			c = b_getc(sc_buf);

			if (c == -2)
			{
				errno = RTE;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				t.code = ERR_T;
				return t;
			}

			if (c == '\n')
			{
				++line;
			}

			if (b_addc(lex_buf, c) == NULL)
			{
				errno = RTE;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				t.code = ERR_T;
				return t;
			}
		}

		//Make the lexeme a c-string
		if (b_addc(lex_buf, '\0') == NULL)
		{
			errno = RTE;
			strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
			t.code = ERR_T;
			return t;
		}

		//Create a pointer pointing to the start of the lexeme buffer
		char* lexeme = b_location(lex_buf);

		if (lexeme == NULL)
		{
			errno = RTE;
			strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
			t.code = ERR_T;
			return t;
		}

		//Finalize the properties of the token by calling an accepting state function based on the current state
		t = (*p[state])(lexeme);

		//Free the lexeme buffer
		b_free(lex_buf);
		return t;
	}
}

/*
Purpose:			Based on the current state of the transition table and the
					character of the scanner buffer just read find the next state
					to go to
Author:				Sv.Ranev
Version:			1.0
Called Functions:
					char_class()
Parameters:
					state - The current state of the transition table
					c     - The character from the scanner buffer just read
Return Value:
					next  - The next state in the transition table based on the character
					        read in from the scanner buffer
Algorithm:
					1. Find the next column of the transition table to move to 
					2. Get the next state in the transition table to move to based on the
					   current state and the next column
					3. Ensure that the next state is not an illegal state (IS)
*/
int get_next_state(int state, char c)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];

	assert(next != IS);

	#ifdef DEBUG
		if (next == IS) {
			printf("Scanner Error: Illegal state:\n");
			printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
			exit(1);
		}
	#endif
	
	return next;
}

/*
	Purpose:			To find the column of the transition table to which the character c belongs
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:
						N/A
	Parameters:
						char c - The character to find what column of the transition table it belongs
	Return Value:
						val - the column to which the character belongs
	Algorithm:
*/
int char_class(char c)
{
	int val = 0;

	//A-Z && a-z
	if ((c > 64 && c < 91) || (c > 96 && c < 123))
	{
		val = 0;
	}
	//1-9
	else if (c > 48 && c < 58)
	{
		val = 2;
	}
	//0
	else if (c == 48)
	{
		val = 1;
	}
	//.
	else if (c == 46)
	{
		val = 3;
	}
	//@
	else if (c == 64)
	{
		val = 4;
	}
	//"
	else if (c == 34)
	{
		val = 5; 
	}
	//EOF
	else if (c == '\0' || c == '-1')		
	{
		val = 6;
	}
	//Other
	else
	{
		val = 7;
	}

    return val;
}

/*
	Purpose:			To determine if the lexeme is a keyword and initialize the corresponding token properties 
						or if the lexeme is not a keyword initialize the corresponding token properties for an AVID
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:
						isKeyword()
						strncpy()
	Parameters:
						char lexeme[] - Character array used to represent the lexeme
	Return Value:
						A token that is either initialized as a keyword or as an AVID
	Algorithm:
						1. Check if the lexeme is a keyword
						2. If it is set the token properties accordingly
						3. Otherwise, treat the lexeme as an AVID and set the token properties accordingly
*/
Token aa_func02(char lexeme[])
{
	//The token whose properties are to be set
	Token VID = { 0 };

	if (lexeme == NULL)
	{
		errno = RTE;
		strcpy(VID.attribute.err_lex, "RUN TIME ERROR: ");
		VID.code = ERR_T;
		return VID;
	}

	//Check if the lexeme is a keyword
	int keyword = iskeyword(lexeme);
	 
	//If so set the token properties for a keyword
	if (keyword != -1)
	{
		VID.attribute.kwt_idx = keyword;
		VID.code = KW_T;
		return VID;
	}

	//Otherwise, set the token properties for an AVID
	strncpy(VID.attribute.vid_lex, lexeme, VID_LEN);
	VID.attribute.vid_lex[VID_LEN] = '\0';
	VID.code = AVID_T;

	return VID;
}

/*
	Purpose:			To initialize the properties of a token assuming the lexeme is a SVID
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:
						strncpy()
	Parameters:
						char lexeme[] - Character array used to represent the lexeme
	Return Value:
						A token that is has its properties initialized as a SVID
*/
Token aa_func03(char lexeme[])
{
	//Token whose properties are to be set
	Token SVID = { 0 };

	if (lexeme == NULL)
	{
		errno = RTE;
		strcpy(SVID.attribute.err_lex, "RUN TIME ERROR: ");
		SVID.code = ERR_T;
		return SVID;
	}

	//Set the properties of the token assuming its an SVID
	strncpy(SVID.attribute.vid_lex, lexeme, VID_LEN - 1);
	SVID.attribute.vid_lex[VID_LEN - 1] = '@';
	SVID.attribute.vid_lex[VID_LEN] = '\0';
	SVID.code = SVID_T;

	return SVID;
}

/*
	Purpose:			To initialize the properties of a token assuming the lexeme is a DIL
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:
						atoi()
						pow()
						strlen()
						strncpy()
						strcpy()
	Parameters:
						char lexeme[] - Character array used to represent the lexeme
	Return Value:
						A token that is has its properties initialized as a DIL
						An error token otherwise
	Algorithm:
						1. Check to see if the lexeme were to be converted to an integer is would a valid PLATYPUS integer
							a. If the lexeme is not a valid integer and its length is too long to be represented by the error attribute
							b. Otherwise
*/
Token aa_func05(char lexeme[])
{
	//Token whose properties are to set
	Token DIL = { 0 };

	if (lexeme == NULL)
	{
		errno = RTE;
		strcpy(DIL.attribute.err_lex, "RUN TIME ERROR: ");
		DIL.code = ERR_T;
		return DIL;
	}

	//If the lexeme was converted to an int ensure that it is a valid 2-byte integer and has the correct number of digits
	if (atoi(lexeme) <= (pow(2, 15) - 1) && atoi(lexeme) >= 0 && strlen(lexeme) <= INL_LEN)
	{
		DIL.attribute.get_int = atoi(lexeme);
		DIL.code = INL_T;
	}
	else
	{
		//If the lexeme is too long even for the error attribute to hold
		if (strlen(lexeme) > ERR_LEN)
		{
			//Copy as much of the lexeme as is legal
			strncpy(DIL.attribute.err_lex, lexeme, ERR_LEN - 3);

			//Add three dots
			for (int i = 0; i < 3; i++)
			{
				DIL.attribute.err_lex[ERR_LEN - 3 + i] = '.';
				DIL.code = ERR_T;
			}
		}
		else
		{
			strcpy(DIL.attribute.err_lex, lexeme);
			DIL.code = ERR_T;
		}
	}

  return DIL;
}

/*
	Purpose:			To initialize the properties of a token assuming the lexeme is a FPL
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:
						atof()
						pow()
						strtof()
						strlen()
						strncpy()
						strcpy()
	Parameters:
						char lexeme[] - Character array used to represent the lexeme
	Return Value:
						A token that is has its properties initialized as a DIL
						An error token otherwise
	Algorithm:
						1. Check to see if the lexeme were to be converted to an float is would a valid PLATYPUS float
							a. If the lexeme is not a valid float and it is too long to be represented as an error add three dots
							b. Otherwise copy the lexeme into the error attribute of the token
*/
Token aa_func08(char lexeme[]) 
{
	//The Token whose attributes are to be set
	Token FPL = { 0 };

	if (lexeme == NULL)
	{
		errno = RTE;
		strcpy(FPL.attribute.err_lex, "RUN TIME ERROR: ");
		FPL.code = ERR_T;
		return FPL;
	}

	//Ensure the converted lexeme is a valid float
	if (atof(lexeme) < (pow(2, 31) - 1) && atof(lexeme) >= 0)
	{
		//If the lexeme holds a value too small to be represented by a float
		if (atof(lexeme) != 0 && (float)atof(lexeme) == 0)
		{

		}
		else
		{
			FPL.attribute.flt_value = strtof(lexeme, NULL);
			FPL.code = FPL_T;
			return FPL;
		}
	}

	//If the float value of the lexeme is invalid and the lexeme is longer than is allowed for an error attribute
	if (strlen(lexeme) > ERR_LEN || atof(lexeme) != 0 && (float)atof(lexeme) == 0)
	{
		//Copy as much of the lexeme as is legal
		strncpy(FPL.attribute.err_lex, lexeme, ERR_LEN - 3);

		//Add the 3 dots
		for (int i = 0; i < 3; i++)
		{
			FPL.attribute.err_lex[ERR_LEN - 3 + i] = '.';
			FPL.code = ERR_T;
		}
	}
	else
	{
		strcpy(FPL.attribute.err_lex, lexeme);
		FPL.code = ERR_T;
	}

	return FPL;
}

/*
	Purpose:			To initialize the properties of a token assuming the lexeme is a string
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:
						strlen()
						b_addc()

	Parameters:
						char lexeme[] - Character array used to represent the lexeme
	Return Value:
						A token that is has its properties initialized as a string
*/
Token aa_func10(char lexeme[])
{
	Token STRLIT = { 0 };

	if (lexeme == NULL)
	{
		errno = RTE; 
		strcpy(STRLIT.attribute.err_lex, "RUN TIME ERROR: ");
		STRLIT.code = ERR_T;
		return STRLIT;
	}

	STRLIT.attribute.str_offset = str_LTBL->addc_offset;
	for (unsigned int i = 1; i < strlen(lexeme) - 1; i++)
	{
		if (b_addc(str_LTBL, lexeme[i]) == NULL)
		{
			errno = RTE;
			strcpy(STRLIT.attribute.err_lex, "RUN TIME ERROR: ");
			STRLIT.code = ERR_T;
			return STRLIT;
		}
	}

	if (b_addc(str_LTBL, '\0') == NULL)
	{
		errno = RTE;
		strcpy(STRLIT.attribute.err_lex, "RUN TIME ERROR: ");
		STRLIT.code = ERR_T;
		return STRLIT;
	}
	
	STRLIT.code = STR_T;

	return STRLIT;
}
 
/*
	Purpose:			To initialize the properties of a token assuming the lexeme is an error
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:
						strlen()
						strcpy()
	Parameters:
						char lexeme[] - Character array used to represent the lexeme
	Return Value:
						A token that is has its properties initialized as an error
	Algorithm:
						1. Verify the lexeme is a valid length to be represented by token error attribute
							a. If so copy only as much of the lexeme as is legal and add three dots
*/
Token aa_func11_12(char lexeme[])
{
	//The token whose attributes are to be set
	Token err = { 0 };

	if (lexeme == NULL)
	{
		errno = RTE; 
		strcpy(err.attribute.err_lex, "RUN TIME ERROR: ");
		err.code = ERR_T;
		return err;
	}

	//Verify that the lexeme is of a valid length
	if (strlen(lexeme) > ERR_LEN)
	{
		//Copy as much of the string as is legal
		strncpy(err.attribute.err_lex, lexeme, ERR_LEN - 3);

		//Add the 3 dots
		for (int i = 0; i < 3; i++)
		{
			err.attribute.err_lex[ERR_LEN - 3 + i] = '.';
		}
	}
	else
	{
		strcpy(err.attribute.err_lex, lexeme);
	}

	err.code = ERR_T;
	
   return err;
}

/*
	Purpose:			To initialize the properties of a token assuming the lexeme is a string
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:
						strcmp()
	Parameters:
						char* kw_lexeme - Character array used to represent the lexeme
	Return Value:
						01 - if the lexeme is a keyword
						-1 - if the lexeme is not a keyword
*/
int iskeyword(char* kw_lexeme) 
{ 
	if (kw_lexeme == NULL)
	{
		return -1; 
	}

	//One by one compare the elements of the keyword array to the lexeme to see if there is a match
	for (int i = 0; i < KWT_SIZE; i++)
	{
		if (strcmp(kw_lexeme, kw_table[i]) == 0)
		{
			return i;
		}
	}

	return -1; 
}
