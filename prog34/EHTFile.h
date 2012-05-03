/**
 * EHTFile.h
 * by Ken Hartness
 * v. 1.0, 2012/04/30
 * EHTFILE and function prototypes to support extendable hash tables.
 * Consider generalizing further by allowing a function pointer to a hash
 * function so each EHTFile can use a different one. This one assumes a
 * function named hash is available.
 */
#if ! defined(_EHTFILE_H)
#define _EHTFILE_H 1

#if ! defined(NO_RECORD)
#define NO_RECORD 0xffffffff
#endif

typedef
    struct {
		unsigned short length;
		char buffer[4096 - sizeof(short)];
	} BucketRecord;

typedef
    struct {
		unsigned link;
		char key[1];
	} IndexOverlay;

typedef
    struct {
		unsigned tablePos[512];
		unsigned bucketPos[512];
	} HashTableCache;

typedef
    struct {
		DAFile * bucketIndex;
		DAFile * bucketFile;
		int keyLength;
		unsigned tableSize;
		BucketRecord * bucket;
		unsigned bucketNumber;
		HashTableCache * cache;
	} EHTFILE;

EHTFILE * openEHTFile(char *, int);
unsigned readEHTFile(EHTFILE *, char *);
int addEHTFile(EHTFILE *, char *, unsigned);
void closeEHTFile(EHTFILE *);

#endif
