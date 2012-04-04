#if ! defined(_BTINDEX_H)
#define _BTINDEX_H 1

#include "DAFile.h"

#define KEY_SIZE 30
#define MAX_BUCKET_SIZE 4096
#define BUCKET_SIZE ((MAX_BUCKET_SIZE - 2*sizeof(short) - sizeof(int)) / (sizeof(int) + KEY_SIZE))

#pragma pack(1)
typedef
    struct {
        unsigned short leaf;
        unsigned short length;
        unsigned link[BUCKET_SIZE+1];
        char key[BUCKET_SIZE][KEY_SIZE];
    } BTNode;
#pragma pack(8)

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
