#if __GLASGOW_HASKELL__ < 711
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif