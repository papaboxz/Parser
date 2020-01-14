/************************************************************************************************************************************************************
	File Name:		buffer.c
	Compiler:		MS Visual Studio 2019
	Author:			Emmett Janssens, 040858839
	Course:			CST8152 - Compilers, 302
	Assignment:		1
	Date:			October 2, 2019
	Professor:		Sv. Ranev
	Purpose:		To define the functions the buffer will use
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
************************************************************************************************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include "buffer.h"

/************************************************************************************************************************************************************
	Purpose:			To allocate memory for a new buffer and initialize its properties with the paramters of the function
	Author:				Emmett Janssens
	Version:			1.2
	Called Functions:	calloc(), malloc(), free()

	Parameters:			short init_capacity (0 to SHRT_MAX - 1 INCLUSIVE)
						char  inc_factor    (0 to 255 INCLUSIVE)
						char  o_mode        (a, f or m)

	Return Value:		NULL - If there is an error
						newBuffer - The newly created and initialized buffer struct (On success)

	Algorithm:			1. Check if the parameters entered are valid
						2. Set the default properties of the buffer if the initial capacity is 0
						3. Allocate memory for the Buffer struct and the backing array using calloc() and malloc() respectively
						4. Initialize the properties of the buffer struct based on the combination of parameters
						5. Return the newly created buffer
************************************************************************************************************************************************************/
Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode)
{
	//Pointer to the new buffer struct about to be created
	Buffer* newBuffer;
	//Pointer to the start of the char array backing the buffer struct
	char* charArr;

	//Verify that a valid operating mode has been entered
	if (o_mode != 'a' && o_mode != 'f' && o_mode != 'm')
	{
		return RT_BUFF_FAIL;
	}

	//Verify that an increment factor within the valid range has been entered
	if ((unsigned char)inc_factor < 0 || (unsigned char)inc_factor > 255)
	{
		return RT_BUFF_FAIL;
	}

	//Verify that an initial capcaity for the buffer within the valid range has been entered
	if (init_capacity < 0 || init_capacity > SHRT_MAX - 1)
	{
		return RT_BUFF_FAIL;
	}

	//Set defaults if the initial capacity is 0
	if (init_capacity == 0)
	{
		init_capacity = DEFAULT_INIT_CAPACITY;

		if (o_mode == 'a' || o_mode == 'm')
		{
			inc_factor = DEFAULT_INC_FACTOR;
		}
		else if (o_mode == 'f')
		{
			inc_factor = 0;
		}
	}

	//Attempt to allocate memory for the Buffer
	Buffer* tempBuffer = calloc(1, sizeof(Buffer));

	if (tempBuffer == NULL)
	{
		return RT_BUFF_FAIL;
	}

	newBuffer = tempBuffer;

	//Attempt to allocate memory for the character array
	char* tempArr = malloc(init_capacity*sizeof(char));

	if (tempArr == NULL)
	{
		return RT_BUFF_FAIL;
	}

	charArr = tempArr;

	//Handle the properties of the Buffer for the different operating modes
	if (o_mode == 'f' || (unsigned char)inc_factor == 0 && init_capacity > 0)
	{
		newBuffer->mode = 0;
		newBuffer->inc_factor = 0;
	}
	else if (o_mode == 'a')
	{
		newBuffer->mode = 1;
		newBuffer->inc_factor = (unsigned char)inc_factor;
	}
	else if (o_mode == 'm' && inc_factor >= 1 && inc_factor <= 100)
	{
		newBuffer->mode = -1;
		newBuffer->inc_factor = (unsigned char)inc_factor;
	}
	else
	{
		free(newBuffer);
		free(charArr);
		return RT_BUFF_FAIL;
	}

	//Set the capacity of the buffer
	newBuffer->capacity = init_capacity;

	//Set the backing array of the buffer
	newBuffer->cb_head = charArr;

	//Set the default flags of the buffer
	newBuffer->flags = DEFAULT_FLAGS;

	return newBuffer;
}

/************************************************************************************************************************************************************
	Purpose:			To add a new character to the buffer and resize the buffer if needed
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	realloc()
	Parameters:			pBuffer const pBD
						char		  symbol

	Return Value:		NULL - If there is an error
						pBD  - If the addition of the new character to the buffer and if the buffer's potential resize was sucessful

	Algorithm:			1. Check if the parameters entered are valid
						2. Set the r_flag field of the buffer to 0
						3. If the buffer still has room to add a new character do so and return pBD
						4. If the buffer is full but has reached max capacity return NULL
						5. If the buffer is full but has not reached max capacity, handle the calculation of the new buffer capacity per the operating mode
						6. Reallocate memory for the new size of buffer
						7. Add symbol to the resized buffer
						8. Set the r_flag field of the buffer to 1
************************************************************************************************************************************************************/
pBuffer b_addc(pBuffer const pBD, char symbol)
{
	//Used to calculate the space available for the buffer to grow in to
	short space_available = 0;
	//Used to calculate the new increment of the buffer based on the space available
	short new_inc = 0;
	//The new capacity of the buffer once it has been resized
	short new_capacity = 0;

	//Verify pBD exists
	if (pBD == NULL)
	{
		return RT_BUFF_FAIL;
	}

	//Bitwise operation to set r_flag to 0
	pBD->flags = pBD->flags & RESET_R_FLAG;

	//If the buffer does not need to grow add the new character
	if (pBD->addc_offset < pBD->capacity)
	{
		pBD->cb_head[pBD->addc_offset] = symbol;
		++pBD->addc_offset;
		return pBD;
	}

	//If buffer needs to resize but it is at max capacity
	if (pBD->capacity == SHRT_MAX - 1)
	{
		//Don't resize
		return RT_BUFF_FAIL;
	}

	//If the buffer needs to be resized and the mode is 0 (fixed)
	if (pBD->mode == 0)
	{
		//Don't resize
		return RT_BUFF_FAIL;
	}
	//If the buffer needs to be resized and the mode is 1 (additive)
	else if (pBD->mode == 1)
	{
		//If resizing the buffer will be smaller than the max size of a short, grow the array by increment factor
		if (pBD->capacity + (unsigned char)pBD->inc_factor < SHRT_MAX - 1 && (short)(pBD->capacity + (unsigned char)pBD->inc_factor) > 0)
		{
			new_capacity = pBD->capacity + (unsigned char)pBD->inc_factor;
		}
		//If resizing the buffer will cause the capacity of the buffer to become larger than a short can represent and is truncated positively set to max
		else if (pBD->capacity + pBD->inc_factor > SHRT_MAX - 1 && (short)(pBD->capacity + pBD->inc_factor) > 0)
		{
			new_capacity = SHRT_MAX - 1;
		}
		//If resizing the buffer will cause the capacity of the buffer to become larger than a short can represent and is truncated negatively do not resize
		else
		{
			return RT_BUFF_FAIL;
		}
	}
	//If the buffer needs to be resized and the mode is -1 (multiplicative)
	else if (pBD->mode == -1)
	{
		//Calculate the multiplicative increment factor
		space_available = SHRT_MAX - pBD->capacity - 1;
		new_inc = space_available * (unsigned char)pBD->inc_factor / 100;
		new_capacity = pBD->capacity + new_inc;

		//If the buffer can no longer grow but has not reached maximum capacity
		if (new_inc == 0 && pBD->capacity < SHRT_MAX - 1)
		{
			new_capacity = SHRT_MAX - 1;
		}
	}
	else
	{
		return RT_BUFF_FAIL;
	}

	//To test if resizing the backing array of the buffer is possible
	char* tempArr = realloc(pBD->cb_head, new_capacity * sizeof(char));

	if (tempArr == NULL)
	{
		return RT_BUFF_FAIL;
	}

	//Set the array pointer to the new memory location
	pBD->cb_head = tempArr;
	pBD->capacity = new_capacity;
	pBD->cb_head[pBD->addc_offset++] = symbol;

	//Set the flags to notify that the buffer has been resized
	pBD->flags = pBD->flags | SET_R_FLAG;

	return pBD;
}

/************************************************************************************************************************************************************
	Purpose:			To clear the contents of the buffer
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	N/A
	Paramters:			Buffer * const pBD

	Return Value:		-1 If unsuccessful
						 1 if successful

	Algorithm:			
************************************************************************************************************************************************************/
int b_clear(Buffer* const pBD)
{
	//Verify that pBD exists
	if (pBD == NULL)
	{
		return RT_FAIL_1;
	}

	//Reset the values of the buffer
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	pBD->flags = DEFAULT_FLAGS;
	return 1;
}

/************************************************************************************************************************************************************
	Purpose:			To free the memory used by the buffer struct and its backing char array
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	free()
	Paramters:			Buffer * const pBD

	Return Value:		N/A

	Algorithm:			
************************************************************************************************************************************************************/
void b_free(Buffer* const pBD)
{
	//Verify pBD exists
	if (pBD == NULL)
	{
		return;
	}

	//Free the memory of the Buffer
	free(pBD->cb_head);
	free(pBD);
}

/************************************************************************************************************************************************************
	Purpose:			To check if the buffer's size has reached the Buffer's capacity
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	N/A

	Parameters:			Buffer * const pBD

	Return Value:		-1 If unsuccessful
						 1 If the buffer is full
						 0 If the buffer is not full

	Algorithm:			
************************************************************************************************************************************************************/
int b_isfull(Buffer* const pBD)
{
	//Verify the Buffer exists
	if (pBD == NULL)
	{
		return RT_FAIL_1;
	}

	//Check if the Buffer is full
	if (pBD->addc_offset == pBD->capacity)
	{
		return 1;
	}

	return 0;
}

/************************************************************************************************************************************************************
	Purpose:			To check the buffer's current size
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	N/A

	Parameters:			Buffer * const pBD

	Return Value:		-1 If unsuccessful
						 pBD->addc_offset (the current size of the buffer) if successful

	Algorithm:			
************************************************************************************************************************************************************/
short b_limit(Buffer* const pBD)
{
	//Verify the Buffer exists
	if (pBD == NULL)
	{
		return RT_FAIL_1;
	}

	return pBD->addc_offset;
}

/************************************************************************************************************************************************************
	Purpose:			To check the capacity of the buffer
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	N/A

	Parameters:			Buffer * const pBD

	Return Value:		-1 If unsuccessful
						The current size of the buffer if successful

	Algorithm:			
************************************************************************************************************************************************************/
short b_capacity(Buffer* const pBD)
{
	//Verify the Buffer exists
	if (pBD == NULL)
	{
		return RT_FAIL_1;
	}

	return pBD->capacity;
}

/************************************************************************************************************************************************************
	Purpose:			To set the mark offset of the buffer to the mark parameter
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	N/A

	Parameters:			Buffer * const pBD
						short mark (0 - b_limit(pBD))

	Return Value:		-1 If unsuccessful
						The currently set mark offset of the buffer if the function is successful

	Algorithm:			
************************************************************************************************************************************************************/
short b_mark(pBuffer const pBD, short mark)
{
	//Verify the Buffer exists
	if (pBD == NULL)
	{
		return RT_FAIL_1;
	}

	//Verify the value of mark is valid
	if (mark < 0 || mark > b_limit(pBD))
	{
		return RT_FAIL_1;
	}

	pBD->markc_offset = mark;
	return mark;
}

/************************************************************************************************************************************************************
	Purpose:			To check the current operating mode of the buffer
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	N/A

	Parameters:			Buffer * const pBD

	Return Value:		-2 If unsuccessful
						The currently set operating mode of the buffer if the function is successful

	Algorithms:			
************************************************************************************************************************************************************/
int b_mode(Buffer* const pBD)
{
	//Verify the buffer exists
	if (pBD == NULL)
	{
		printf("The Buffer you are trying to get the mode of does not exist\n");
		return RT_FAIL_2;
	}

	return pBD->mode;
}

/************************************************************************************************************************************************************
	Purpose:			To check the current increment factor of the buffer
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	N/A

	Parameters:			Buffer * const pBD

	Return Value:		0x100 If unsuccessful
						The currently set mark offset of the buffer if the function is successful

	Algorithm:			
************************************************************************************************************************************************************/
size_t b_incfactor(Buffer* const pBD)
{
	//Verify the Buffer exists
	if (pBD == NULL)
	{
		printf("The Buffer you are trying to get the increment factor of of does not exist\n");
		return RT_INC_FAIL;
	}

	return (unsigned char)pBD->inc_factor;
}

/************************************************************************************************************************************************************
	Purpose:			To read characters from an opened file and add them to the buffer
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	b_addc(), fgetc(), ungetc(), feof()

	Parameters:			FILE * const fi
						Buffer * const pBD

	Return Value:		-1 if there is a an error with the buffer or file
						-2 if the buffer can no longer be added to
						 The number of characters read if the function is successful

	Algorithm:			1. Verify if the buffer and file exist
						2. Loop until the end of the file is reached adding character by character to the buffer
						3. Return the number of characters read
************************************************************************************************************************************************************/
int b_load(FILE* const fi, Buffer* const pBD)
{
	//The number of characters read from the file
	int charsRead = 0;
	//The character to be added to the buffer
	int newChar;

	//Verify the buffer exists
	if (pBD == NULL)
	{
		return RT_FAIL_1;
	}

	//Verify the file exists
	if (fi == NULL)
	{
		return RT_FAIL_1;
	}

	while (1)
	{
		newChar = fgetc(fi);

		//If the end of file is reached stop looping
		if (feof(fi))
		{
			break;
		}

		//If the buffer can no longer be added to 
		if (b_addc(pBD, (char)newChar) == NULL)
		{
			ungetc(newChar, fi);
			return LOAD_FAIL;
		}

		charsRead++;
	}

	return charsRead;
}

/************************************************************************************************************************************************************
	Purpose:			To check if the buffer is empty
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	N/A

	Parameters:			Buffer * const pBD

	Return Value:		-1 if there is a an error with the buffer
						 1 if the buffer is empty
						 0 if the buffer is not empty

	Algorithm:			
************************************************************************************************************************************************************/
int b_isEmpty(Buffer* const pBD)
{
	//Verify the buffer exists
	if (pBD == NULL)
	{
		return RT_FAIL_1;
	}

	if (pBD->addc_offset == 0)
	{
		return 1;
	}

	return 0;
}

/************************************************************************************************************************************************************
	Purpose:			To get a character from the buffer at a specified offset
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	N/A

	Parameters:			Buffer * const pBD

	Return Value:		-2 if the Buffer does not exist
						 0 if the get offset has reached the end of the buffer
						 the character located at the get offset if successful

	Algorithm:			1. Verify if the buffer exists
						2. Verify that the get offset is not at the end of the buffer
							a. If so set the end of buffer flag field of the buffer and return 0
						3. Return the character at the get offset and increment the get offset
************************************************************************************************************************************************************/
char b_getc(Buffer* const pBD)
{
	//Verify the buffer exists
	if (pBD == NULL)
	{
		return RT_FAIL_2;
	}

	//If you are trying to retrieve an element in the buffer past the end of the buffer
	if (pBD->getc_offset == pBD->addc_offset)
	{
		pBD->flags = pBD->flags | SET_EOB;
		return 0;
	}

	pBD->flags = pBD->flags & RESET_EOB;

	return pBD->cb_head[pBD->getc_offset++];
}

/************************************************************************************************************************************************************
	Purpose:			To check the value of the end of buffer flag field
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	N/A

	Parameters:			Buffer * const pBD

	Return Value:		-1 if the buffer does not exist

	Algorithm:			
************************************************************************************************************************************************************/
int b_eob(Buffer* const pBD)
{
	//Verify that pBD exists
	if (pBD == NULL)
	{
		return RT_FAIL_1;
	}
	return (pBD->flags & CHECK_EOB) >> 1;
}

/************************************************************************************************************************************************************
	Purpose:			To print array of characters stored in the buffer to the console
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	getc(), b_eob()

	Parameters:			Buffer * const pBD
						char	       nl

	Return Value:		-1 if the buffer does not exist
						charsPrinted - the number of characters printed if the function is successful

	Algorithm:			1. Verify the buffer exists
						2. Do while loop until the end of buffer is reached
						3. Add a new line character as per the nl parameter
						4. Return the characters printed
************************************************************************************************************************************************************/
int b_print(Buffer* const pBD, char nl)
{
	//Variable to store the character retrieved from the buffer to be printed
	char getChar = ' ';
	//The number of characters that were printed from the buffer
	int  charsPrinted = 0;

	//Verify pBD exists
	if (pBD == NULL)
	{
		return RT_FAIL_1;
	}

	//Print the contents of the buffer
	do
	{
		getChar = b_getc(pBD);

		if (b_eob(pBD) == 1)
		{
			break;
		}

		printf("%c", getChar);
		++charsPrinted;

	} while (1);

	//Add a new line character if needed
	if (nl != 0)
	{
		printf("\n");
	}

	return charsPrinted;
}

/************************************************************************************************************************************************************
	Purpose:			To shrink the backing array of the buffer to the minimum size required + 1
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	N/A

	Parameters:			Buffer * const pBD
						char	       symbol

	Return Value:		NULL if the function is unsuccessful
						pBD if the function is successful

	Algorithm:			1. Verify the buffer exists
						2. Verify the buffer is not at max capacity
						3. Attempt to shrink the buffer
						4. Add the new symbol to the buffer if possible
						5. Set the resize flag of the buffer
						6. Return pBD
************************************************************************************************************************************************************/
Buffer* b_compact(Buffer* const pBD, char symbol)
{
	//Verify the buffer exists
	if (pBD == NULL)
	{
		return NULL;
	}

	//Verify if the buffer is at max capacity
	if (pBD->addc_offset == SHRT_MAX)
	{
		return NULL;
	}

	//Attempt to resize buffer
	char* tempArr = realloc(pBD->cb_head, (pBD->addc_offset + 1) * sizeof(char));
	if (tempArr == NULL)
	{
		return NULL;
	}

	//Add symbol
	pBD->cb_head = tempArr;
	pBD->capacity = pBD->addc_offset + 1;
	pBD->cb_head[pBD->addc_offset++] = symbol;
	pBD->flags = pBD->flags & RESET_R_FLAG;

	return pBD;
}

/************************************************************************************************************************************************************
	Purpose:			To check the value of the resize flag of the buffer
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	N/A

	Parameters:			Buffer * const pBD

	Return Value:		-1 if the function is unsuccessful
						pBD if the function is successful

	Algorithm:			
************************************************************************************************************************************************************/
char b_rflag(Buffer* const pBD)
{
	//Verify the buffer exists
	if (pBD == NULL)
	{
		return RT_FAIL_1;
	}

	return pBD->flags & CHECK_R_FLAG;
}

/************************************************************************************************************************************************************
	Purpose:			To decrement the get offset of the buffer
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	N/A

	Parameters:			Buffer * const pBD

	Return Value:		-1 if the function is unsuccessful
						The new value of the get offset of the buffer if the function is successful

	Algorithm:			
************************************************************************************************************************************************************/
short b_retract(Buffer* const pBD)
{
	//Verify the buffer exists
	if (pBD == NULL)
	{
		return RT_FAIL_1;
	}

	//decrement
	pBD->getc_offset--;

	return  pBD->getc_offset;
}

/************************************************************************************************************************************************************
	Purpose:			To set the value of the get offset to the mark offset of the buffer
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	N/A

	Parameters:			Buffer * const pBD

	Return Value:		-1 if the function is unsuccessful
						the new value of the get offset of the buffer if the function is successful

	Algorithm:			
************************************************************************************************************************************************************/
short b_reset(Buffer* const pBD)
{
	//Verify the buffer exists
	if (pBD == NULL)
	{
		return RT_FAIL_1;
	}

	//Set get offset
	pBD->getc_offset = pBD->markc_offset;

	return pBD->getc_offset;
}

/************************************************************************************************************************************************************
	Purpose:			To check the value of the get offset of the buffer
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	N/A

	Parameters:			Buffer * const pBD

	Return Value:		-1 if the function is unsuccessful
						the value of the get offset of pBD if the function is successful

	Algorithm:		
************************************************************************************************************************************************************/
short b_getcoffset(Buffer* const pBD)
{
	//Verify pBD exists
	if (pBD == NULL)
	{
		return RT_FAIL_1;
	}

	return pBD->getc_offset;
}

/************************************************************************************************************************************************************
	Purpose:			Set the get offset and the mark offset of the buffer to 0
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	N/A

	Parameters:			Buffer * const pBD

	Return Value:		-1 if the function is unsuccessful
						 0 if the function is successful

	Algorithm:			
************************************************************************************************************************************************************/
int b_rewind(Buffer* const pBD)
{
	//Verify the buffer exists
	if (pBD == NULL)
	{
		return RT_FAIL_1;
	}

	//Set the values
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	return 0;
}

/************************************************************************************************************************************************************
	Purpose:			To get a pointer to the string started at the mark offset of the buffer
	Author:				Emmett Janssens
	Version:			1.0
	Called Functions:	N/A

	Parameters:			Buffer * const pBD

	Return Value:		NULL if the function is unsuccessful
						a pointer to the string of characters starting at the mark offset if the function is successful

	Algorithm:			
************************************************************************************************************************************************************/
char* b_location(Buffer* const pBD)
{
	//The pointer that will potentially point to the string of characters
	char* loc;

	//Verify the buffer exists
	if (pBD == NULL)
	{
		return NULL;
	}

	//Point loc to the location of the start of the string in memory
	loc = &pBD->cb_head[pBD->markc_offset];
	return loc;
}

