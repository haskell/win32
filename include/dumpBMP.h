#include <windows.h>

/* There's currently no #define that indicate whether we're
   compiling a .hc file. */

extern void CreateBMPFile(LPCSTR pszFileName, HBITMAP hBmp, HDC hDC);
