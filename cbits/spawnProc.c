/*
 * Spawning a Win32 child, redirecting its input and outputs
 * to standard file descriptors.
 *
 * Based on code snippets found in misc MSDN articles.
 *
 * This is the C support routine for Win32Spawn.hs
 */
#include <windows.h>
#include <io.h>
#include <fcntl.h>

/*
 * Function: mkAnonPipe
 *
 * Purpose:  create an anonymous pipe with read and write ends being
 *           optionally (non-)inheritable.
 */
static
BOOL
mkAnonPipe( HANDLE* pHandleIn,  BOOL isInheritableIn,
	    HANDLE* pHandleOut, BOOL isInheritableOut
	  )
{
  HANDLE hTemporaryIn  = NULL;
  HANDLE hTemporaryOut = NULL;
  BOOL status;
  SECURITY_ATTRIBUTES sec_attrs;

  /* Create inheritable security attributes */
  sec_attrs.nLength = sizeof(SECURITY_ATTRIBUTES); 
  sec_attrs.lpSecurityDescriptor = NULL; 
  sec_attrs.bInheritHandle = TRUE;

  /* Create the anon pipe with both ends inheritable */
  status = 
    CreatePipe( ((!isInheritableIn)  ? &hTemporaryIn  : pHandleIn),
		((!isInheritableOut) ? &hTemporaryOut : pHandleOut),
		&sec_attrs,
		0);

  if ( !status ) {
    /* Unable to create the pipe; just give up. */
    return FALSE;
  }
  
  if (!isInheritableIn) {
  /* Make the read end non-inheritable */
    status = DuplicateHandle( GetCurrentProcess(), hTemporaryIn,
			      GetCurrentProcess(), pHandleIn,
			      0,
			      FALSE, /* non-inheritable */
			      DUPLICATE_SAME_ACCESS);

    if ( !status ) {
      return FALSE;
    }
    CloseHandle(hTemporaryIn);
  }

  if (!isInheritableOut) {
    /* Make the write end non-inheritable */
    status = DuplicateHandle( GetCurrentProcess(), hTemporaryOut,
			      GetCurrentProcess(), pHandleOut,
			      0,
			      FALSE, /* non-inheritable */
			      DUPLICATE_SAME_ACCESS);

    if ( !status ) {
      return FALSE;
    }
    CloseHandle(hTemporaryOut);
  }
  
  return TRUE;
}

int
spawnProc ( char* cmd
	  , int* pFdInWrite
	  , int* pFdOutRead
	  , int* pFdErrRead
          )
{
  BOOL status;
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  HANDLE hStdinReadEnd,  hStdinWriteEnd;
  HANDLE hStdoutReadEnd, hStdoutWriteEnd;
  HANDLE hStderrReadEnd, hStderrWriteEnd;
  /*
  HANDLE hCurrentStdout, hCurrentStderr, hCurrentStdin;
  */

  /* Create the stdin anonymous pipe */
  if ( !mkAnonPipe( &hStdinReadEnd, TRUE,
		    &hStdinWriteEnd, FALSE) ) {
    /* Unable to create the pipe; just give up. */
    return -1;
  }
  
  /* Create the stdout anonymous pipe */
  if ( !mkAnonPipe( &hStdoutReadEnd, FALSE,
		    &hStdoutWriteEnd, TRUE) ) {
    /* Unable to create the pipe; just give up. */
    CloseHandle(hStdinReadEnd);
    CloseHandle(hStdinWriteEnd);
    return -1;
  }

  /* Create the stderr anonymous pipe */
  if ( !mkAnonPipe( &hStderrReadEnd, FALSE,
		    &hStderrWriteEnd, TRUE) ) {
    /* Unable to create the pipe; just give up. */
    CloseHandle(hStdinReadEnd);
    CloseHandle(hStdinWriteEnd);
    CloseHandle(hStdoutReadEnd);
    CloseHandle(hStdoutWriteEnd);
    return -1;
  }
  
  ZeroMemory(&si, sizeof(si));
  si.cb = sizeof(si);
  si.hStdInput  = hStdinReadEnd;
  si.hStdOutput = hStdoutWriteEnd;
  si.hStdError  = hStderrWriteEnd;
  si.dwFlags    = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
  si.wShowWindow = SW_HIDE;
  
  /* Passing in the pipe handles via STARTUPINFO is slightly
     easier, so let's stick with that for now.
  hCurrentStdin  = GetStdHandle(STD_INPUT_HANDLE);
  hCurrentStdout = GetStdHandle(STD_OUTPUT_HANDLE);
  hCurrentStderr = GetStdHandle(STD_ERROR_HANDLE);
  SetStdHandle(STD_INPUT_HANDLE, hStdinReadEnd);
  SetStdHandle(STD_OUTPUT_HANDLE, hStdoutWriteEnd);
  SetStdHandle(STD_ERROR_HANDLE, hStderrWriteEnd);
  */

  status = CreateProcess ( NULL,
			   cmd,
			   NULL,
			   NULL,
			   TRUE, /* inherit handles */
			   NORMAL_PRIORITY_CLASS,
			   NULL,
			   NULL,
			   &si,
			   &pi);
			   
  /*
  SetStdHandle(STD_INPUT_HANDLE,  hCurrentStdin);
  SetStdHandle(STD_OUTPUT_HANDLE, hCurrentStdout);
  SetStdHandle(STD_ERROR_HANDLE,  hCurrentStderr);
  */

  CloseHandle(hStdinReadEnd);
  CloseHandle(hStdoutWriteEnd);
  CloseHandle(hStderrWriteEnd);

  if (!status) {
    CloseHandle(hStdinWriteEnd);
    CloseHandle(hStdoutReadEnd);
    CloseHandle(hStderrReadEnd);
    return -1;
  }
  
  /* No need for these, close them. */
  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);
  
  *pFdInWrite = (int)_open_osfhandle((long)hStdinWriteEnd, _O_TEXT);
  *pFdOutRead = (int)_open_osfhandle((long)hStdoutReadEnd, _O_TEXT);
  *pFdErrRead = (int)_open_osfhandle((long)hStderrReadEnd, _O_TEXT);

  return 0;
}
