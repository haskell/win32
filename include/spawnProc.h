#ifndef __SPAWNPROC_H
#define __SPAWNPROC_H

#include <windows.h>

extern int spawnProc(LPTSTR cmd, int* pFdInWrite, int* pFdOutRead, int* pFdErrRead);

#endif /* __SPAWNPROC_H */
