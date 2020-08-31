#ifndef _ALPHABLEND_H
#define _ALPHABLEND_H

#include <windows.h>

BOOL c_AlphaBlend ( HDC hdcDest, int nXOriginDest, int nYOriginDest, int nWidthDest, int hHeightDest
                  , HDC hdcSrc, int nXOriginSrc, int nYOriginSrc, int nWidthSrc, int nHeightSrc
                  , PBLENDFUNCTION pblendFunction);

#endif /* _ALPHABLEND_H */
