/* InvertedIndex.h
   by Ken Hartness
   2012/05/01
   Creates an InvertedIndex type that maintains lists of positions where a key can be found.
 */

#if ! defined(_INVERTED_INDEX_H)
#define _INVERTED_INDEX_H 1

#include <db.h>

#if ! defined(NO_RECORD)
#define NO_RECORD 0xffffffff
#endif

typedef
    struct {
		int length;
		unsigned position[127];
	} InvertedData;

typedef
    struct {
		char * key;
		int length, current;
		unsigned * position;
	} InvertedList;

typedef
    struct {
		DB * invertedLists;
		int current;
		char * lastKey;
		InvertedData last;
		int keySize;
	} InvertedIndex;

InvertedIndex * openInvertedIndex(char *, int);
int addToInvertedIndex(InvertedIndex *, char *, unsigned);
unsigned findFirstInvertedIndex(InvertedIndex *, char *);
unsigned findNextInvertedIndex(InvertedIndex *);
void closeInvertedIndex(InvertedIndex *);
int findInvertedList(InvertedIndex *, InvertedList *);

#endif