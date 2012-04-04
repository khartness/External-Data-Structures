#if ! defined(_BTINDEX_H)
#define _BTINDEX_H 1

#include "DAFile.h"

#define KEY_SIZE 30
#define MAX_BUCKET_SIZE 4096
/* with minimal separators, assume maximum would be list of 1-character
   strings, along with a one-byte length and an integer for the link.
   Actually, since you probably only have 26 of those, at most 26*26 = 676
   2-character strings, leaving 423 3-character strings in a 4096 block area. */
#if MAX_BUCKET_SIZE <= (26 * 6 + 2 * sizeof(short) + sizeof(int)
#define BUCKET_SIZE ((MAX_BUCKET_SIZE - 2*sizeof(short) - sizeof(int)) / (sizeof(int) + 2))
#else
#if MAX_BUCKET_SIZE <= (26 * 6 + 26 * 26 * 7 + 2 * sizeof(short) + sizeof(int)
#define BUCKET_SIZE (26 + (MAX_BUCKET_SIZE - 2*sizeof(short) - sizeof(int) - 26 * 6) / (sizeof(int) + 3))
#else
#define BUCKET_SIZE (26 + 26 * 26 + (MAX_BUCKET_SIZE - 2*sizeof(short) - sizeof(int) - 26 * 6 - 26 * 26 * 7) / (sizeof(int) + 4))
#endif
#endif

#define NO_RECORD 0xffffffff

typedef
    struct {
        unsigned short leaf;
        unsigned short length;
        unsigned link[BUCKET_SIZE+1];
        char key[BUCKET_SIZE][KEY_SIZE];
    } BTNode;

typedef
    struct {
        unsigned pos;
        unsigned frequency;
        BTNode rec;
        } CacheEntry;

typedef
    struct {
        DAFile * file;
        unsigned root;
        CacheEntry cache[8];
        int cacheSize;
    } BTIndex;

BTIndex * createBTIndex(char *);
BTIndex * openBTIndex(char *);
unsigned findKey(char *, BTIndex *);
void addKey(char *, unsigned, BTIndex *);

#endif
