#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

/* There's two ways we can generate error messages - with different tradeoffs:
 * If we do a function call, we have to use a static buffer.
 * If we use a macro and ANSI C's string splicing, we have to use constant
 * strings - and accept a certain amount of overhead from inserting the
 * boilerplate text.
 *
 * Why the concern about performance? Error messages are only generated
 * in exceptional situations    -- sof 9/98
 *
 * sof 9/98 : Removed use of non-standard (and wimpy :-) snprintf().
 */
#if defined(TARGET_GHC) || defined (USE_FUNCTIONS)

char* ErrorMsg(char* where, char* what)
{
    static char *buffer = NULL;
    static int  buflen = 0;
    int    len;

#define ERRMSG_TEXT_LEN 40
    len = strlen (where) + strlen (what);
    
    if ( buflen == 0 || buflen < len + ERRMSG_TEXT_LEN ) {
       buflen = len + ERRMSG_TEXT_LEN;
       buffer = (char*)malloc(sizeof(char) * buflen);
       if (buffer == NULL ) {
         return "ErrorMsg: malloc() failed";
       }
    }
    sprintf(buffer, "Error %s raised in function %s", what, where);
    buffer[buflen-1]='\0';

    return buffer;
}

char* ErrorWithCode(char* where, DWORD err)
{
    static char *buffer;
    static int  buflen = 0;
    char *what;
    int len;

    FormatMessage( 
	(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ALLOCATE_BUFFER) ,
	NULL,
	err,
	MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), /* Default language */
	(LPTSTR) &what,
	0,
	NULL 
	);

    len = strlen(what) + strlen(where);

/* Number of chars which we decorate the FormatMessage() string
   (and the user-defined loc string) with.
*/
#define ERRWIN_TEXT_LEN  64

    /* Check whether we need to expand the result buffer... */
    if ( buflen == 0 || buflen < ERRWIN_TEXT_LEN ) {
       buflen = len + ERRWIN_TEXT_LEN;
       buffer = (char*)malloc(sizeof(char) * buflen);
       if ( buffer == NULL ) {
         return("ErrorWin: malloc() failed.");
       }
    }
    sprintf(buffer, "%s: %s (error code: %x)", where, what, err);
    LocalFree(what);    
    buffer[buflen-1]='\0';  /* paranoia! */
#ifdef DEBUG
    MessageBox( NULL, buffer, "Error", MB_OK | MB_ICONINFORMATION );
#endif
    return buffer;
}

char* ErrorWin(char* where)
{ return (ErrorWithCode(where, GetLastError())); }


#endif /* defined(TARGET_GHC) || defined (USE_FUNCTIONS) */
