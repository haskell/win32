#ifndef __HSWIN32_H
#define __HSWIN32_H

#define UNICODE
#include <windows.h>

#ifndef INLINE
# if defined(_MSC_VER)
#  define INLINE extern __inline
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

void UnmapViewOfFileFinaliser(void *);

void CloseHandleFinaliser(HANDLE);

void FreeLibraryFinaliser(HMODULE);

void DeleteObjectFinaliser(HGDIOBJ);

#endif /* __HSWIN32_H */
