
#include <windows.h>
#include "finalizers.h"

void
deleteObj(HANDLE h)
{
  if (h == NULL)
    return;
  else {
    DeleteObject(h);
  }
}

