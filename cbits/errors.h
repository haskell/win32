#ifndef _MY_ERRORS_H
#define _MY_ERRORS_H

#include <stdio.h>

/* There's two ways we can generate error messages - with different tradeoffs:
 * If we do a function call, we have to use a static buffer.
 * If we use a macro and ANSI C's string splicing, we have to use constant
 * strings - and accept a certain amount of overhead from inserting the
 * boilerplate text.
 */

#define USE_FUNCTIONS 0

#if USE_FUNCTIONS
extern char* ErrorMsg(char* where, char* what);
#else
#define ErrorMsg(where,what) "Error " what " raised in function " where
#endif

#ifdef TARGET_GHC
#define ErrorString(where) ErrorWin(where)
#else
#define ErrorString(where) "Error raised in function " where
#endif

#define MallocError(where) "malloc failed inside " where

#ifdef TARGET_GHC
extern char* ErrorWin(char* where);
extern char* ErrorWithCode(char* where, DWORD err);
#else
static char* ErrorWithCode(char* where, DWORD errc);
static char* ErrorWin(char* where);

static char* ErrorWithCode(char* where, DWORD errc)
{
    static char buffer[1000]; /* space for our message   */
    static char what[1000];   /* space for Win32 message */
    LPVOID lpMsgBuf = what;

    FormatMessage( 
	FORMAT_MESSAGE_FROM_SYSTEM,
	NULL,
	errc,
	MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
	(LPTSTR) lpMsgBuf,
	1000,
	NULL 
	);
    _snprintf(buffer, 1000, "Error %s raised in function %s", what, where);
    buffer[999] = '\0'; /* paranoia! */
    return buffer;
}

static char* ErrorWin(char* where)
{ return (ErrorWithCode(where,GetLastError())); }

#endif /* !TARGET_GHC */

#define BadRgnTest(x) (x == 0 || x == GDI_ERROR)

#endif /* _MY_ERRORS_H */

