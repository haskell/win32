#ifndef __HSWIN32_H
#define __HSWIN32_H

#include "config.h"

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

INLINE UINT castPtrToUINT(void *p) { return (UINT)p; }
INLINE void *castUINTToPtr(UINT n) { return (void *)n; }
INLINE LONG castFunPtrToLONG(void *p) { return (LONG)p; }
INLINE WORD hIWORD(DWORD w) { return HIWORD(w); }
INLINE WORD lOWORD(DWORD w) { return LOWORD(w); }

INLINE LANGID prim_LANGIDFROMLCID(LCID id) {
  return LANGIDFROMLCID(id);
}
INLINE LANGID prim_MAKELANGID(LANGID primary, LANGID sub) {
  return MAKELANGID(primary, sub);
}
INLINE LCID prim_MAKELCID(LANGID id, WORD sort) {
  return MAKELCID(id, sort);
}
INLINE LANGID prim_PRIMARYLANGID(LANGID id) {
  return PRIMARYLANGID(id);
}
INLINE LANGID prim_SUBLANGID(LCID id) {
  return SUBLANGID(id);
}
INLINE WORD prim_SORTIDFROMLCID(LCID id) {
  return SORTIDFROMLCID(id);
}

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

#endif /* __HSWIN32_H */
