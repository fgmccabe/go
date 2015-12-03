/*
   Public declarations for the dictionary functions for April
*/
#ifndef _DICT_H_
#define _DICT_H_

void initSymbolClass(void);
void initDict();			/* Initialize the dictionary */

ptrI newSymbol(const char *name);
ptrI newEnumSym(const char *name);
ptrI newUniSymbol(const uniChar *name);
ptrI newEnumSymbol(const uniChar *name);
ptrI symbolPresent(uniChar *s);

void installSymbol(symbPo s);
funpo getescape(symbPo l);	/* Get an escape code */
char *escapeName(int code);
funpo escapeCode(unsigned int code);

void install_escapes(void);

void restartDictionary(globalGcPo G);

#endif
