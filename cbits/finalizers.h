/*
 * Finalizers used throughout the Win32 library - 
 * on the ghc side we supply them in a separate .c,
 * for Hugs we inline a copy.
 */
#ifndef _FINALIZERS_H
#define _FINALIZERS_H

extern void deleteObj(HANDLE h);

#endif /* _FINALIZERS_H */
