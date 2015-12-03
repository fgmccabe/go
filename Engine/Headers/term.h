/*
 * Header file giving the interface for reading and writing terms 
 */
#ifndef _TERM_H_
#define _TERM_H_

retCode outCell(ioPo f,ptrPo x,long depth,long prec,logical alt);
retCode outType(ioPo f,ptrPo x,long depth,long prec,logical alt);
retCode decodeTerm(ioPo in,heapPo P,heapPo R,ptrPo tgt,uniChar *errorMsg,long msgSize);
retCode skipEncoded(ioPo in,uniChar *errorMsg, long msgLen);

retCode quoteString(ioPo f,uniChar *s,long len);
retCode outXML(ioPo f,ptrPo x,long depth,long prec,logical alt) ;
retCode xmlQuoteString(ioPo f,uniChar *s,long len);
#endif
