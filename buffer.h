/******************************************************************************
	File Name:		buffer.h
	Compiler:		MS Visual Studio 2019
	Author:			Emmett Janssens, 040858839
	Course:			CST8152 - Compilers, 302
	Assignment:		1
	Date:			October 1, 2019
	Professor:		Sv. Ranev
	Purpose:		To declare the functions the buffer will use and to 
					declare/define the macros and constants used by the buffer
	Function List:	b_allocate()
					b_addc()
					b_clear()
					b_free()
					b_isfull()
					b_limit()
					b_capacity()
					b_mark()
					b_mode()
					b_incfactor()
					b_load()
					b_isEmpty()
					b_getc()
					b_eob()
					b_print()
					b_compact()
					b_rflag()
					b_retract()
					b_reset()
					b_getcoffset()
					b_rewind()
					b_location()
*******************************************************************************/

#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 (-1)         /* operation failure return value -1 */
#define RT_FAIL_2 (-2)         /* operation failure return value -2 */
#define LOAD_FAIL (-2)         /* load fail return value */
#define RT_PASS_0 (0)		   /* operation success return value 0 */
#define RT_PASS_1 (1)		   /* operation success return value 1 */
#define RT_BUFF_FAIL (NULL)    /* operation failure concering the buffer */
#define RT_INC_FAIL (0x100)	   /* operation failure retrieving the increment factor of the buffer*/

#define DEFAULT_INIT_CAPACITY 200   /* default initial buffer capacity */
#define DEFAULT_INC_FACTOR 15       /* default increment factor */

/* You should add your own constant definitions here */

/* Add your bit-masks constant definitions here */
#define DEFAULT_FLAGS  0xFFFC
#define SET_EOB		   0x0002
#define RESET_EOB	   0xFFFD
#define CHECK_EOB	   0x0002
#define SET_R_FLAG     0x0001
#define RESET_R_FLAG   0xFFFE
#define CHECK_R_FLAG   0x0001

/* user data type declarations */
typedef struct BufferDescriptor {
    char *cb_head;   /* pointer to the beginning of character array (character buffer) */
    short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
    short addc_offset;  /* the offset (in chars) to the add-character location */
    short getc_offset;  /* the offset (in chars) to the get-character location */
    short markc_offset; /* the offset (in chars) to the mark location */
    char  inc_factor; /* character array increment factor */
    char  mode;       /* operational mode indicator*/
    unsigned short flags;     /* contains character array reallocation flag and end-of-buffer flag */
} Buffer, *pBuffer;
/*typedef Buffer *pBuffer;*/

/* function declarations */
Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode);
pBuffer b_addc      (pBuffer const pBD, char symbol);
int     b_clear     (Buffer * const pBD);
int		b_isfull    (Buffer * const pBD);
short   b_limit     (Buffer * const pBD);
short   b_capacity  (Buffer * const pBD);
void    b_free		(Buffer* const pBD);
short   b_mark      (pBuffer const pBD, short mark);
int		b_mode      (Buffer * const pBD);
size_t  b_incfactor (Buffer * const pBD);
int     b_load      (FILE * const fi, Buffer * const PBD);
int		b_isEmpty   (Buffer * const pBD);
char	b_getc      (Buffer * const pBD);
int		b_eob       (Buffer * const pBD);
int		b_print     (Buffer * const pBD, char nl);
Buffer* b_compact   (Buffer * const pBD, char symbol);
char	b_rflag     (Buffer * const pBD);
short	b_retract   (Buffer * const pBD);
short	b_reset     (Buffer* const pBD);
short	b_getcoffset(Buffer * const pBD);
int		b_rewind    (Buffer * const pBD);
char*   b_location  (Buffer* const pBD);

#endif

