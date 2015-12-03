/*
 * Header file giving the hash table interface
 */
#ifndef _HASH_TABLE_H_
#define _HASH_TABLE_H_

typedef struct _hash_table_ {
  ptrI sign;				/* == hashClass */
  long size;				/* how long is the hash table? */
  ptrI data[ZEROARRAYSIZE];
} HashTableRec, *hashTablePo;

extern ptrI hashClass;

static inline logical isHash(objPo o)
{
  return hasClass(o,hashClass);
}

static inline logical IsHash(ptrI X)
{
  return isHash(objV(X));
}

static inline hashTablePo hashV(ptrI X)
{
  assert(isobj(X) && IsHash(X));
  return (hashTablePo)(X&PTR_MASK);
}

static inline long hashTableSize(hashTablePo h)
{
  return h->size;
}

#define HashCellCount(size) CellCount(sizeof(HashTableRec)+size*sizeof(ptrI))

void initHashClass(void);

ptrI newHash(heapPo H,long len);
retCode termHash(ptrPo p,uinteger *hash);
retCode searchHash(hashTablePo table,ptrPo key,ptrPo val);
retCode insertHash(heapPo H,ptrPo tb,ptrI key,ptrI val);
retCode deleteHash(hashTablePo table,ptrPo key);
integer hashSize(ptrI t);

#endif

