#ifndef __HSGDI_H
#define __HSGDI_H

#include "ghcconfig.h"

#include <windows.h>

#ifndef INLINE
# if defined(_MSC_VER)
#  define INLINE extern __inline
# elif defined(__HUGS__)
#  define INLINE INLINE_ONLY
# else
#  define INLINE extern inline
# endif
#endif

INLINE COLORREF rgb(BYTE r, BYTE g, BYTE b) { return RGB(r, g, b); }
INLINE BYTE getRValue(COLORREF color) { return GetRValue(color); }
INLINE BYTE getGValue(COLORREF color) { return GetGValue(color); }
INLINE BYTE getBValue(COLORREF color) { return GetBValue(color); }

INLINE COLORREF pALETTERGB(BYTE r, BYTE g, BYTE b) {
  return PALETTERGB(r, g, b);
}
INLINE COLORREF pALETTEINDEX(WORD w) {
  return PALETTEINDEX(w);
}

#ifdef __WINE_WINDOWS_H
INLINE UINT mAKEROP4(UINT op1, UINT op2) { return 0; }
#else
INLINE UINT mAKEROP4(UINT op1, UINT op2) { return MAKEROP4(op1, op2); }
#endif

INLINE UINT prim_MenuItemFromPoint(HWND wnd, HMENU menu, LPPOINT p_pt) {
  return MenuItemFromPoint(wnd, menu, *p_pt);
}
INLINE HWND prim_ChildWindowFromPoint(HWND parent, LPPOINT p_pt) {
  return ChildWindowFromPoint(parent, *p_pt);
}
INLINE HWND prim_ChildWindowFromPointEx(HWND parent, LPPOINT p_pt, UINT flags) {
  return ChildWindowFromPointEx(parent, *p_pt, flags);
}

#endif /* __HSGDI_H */
