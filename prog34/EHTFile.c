#include <string.h>
#include "EHTFile.h"
#include "DAFile.h"

/* external reference to hash function */
unsigned hash(char *);

static void initCache(EHTFILE * file) {
    int i;

    file->bucketNumber = 0;
    file->bucket = (BucketRecord *)malloc(sizeof(BucketRecord));
    file->cache = (HashTableCache *)malloc(sizeof(HashTableCache));
    for ( i = 0; i < 512; ++i )
        file->cache->tablePos[i] = file->cache->bucketPos[i] = 0;
}

static void createIndex(EHTFILE * file, char * ehtFilename, char * bucketFilename) {
    unsigned recnum, bucket;
    BucketRecord rec;

    file->bucketIndex = openDirectAccessFile(ehtFilename, "w", sizeof(unsigned), sizeof(unsigned));
    file->bucketFile = openDirectAccessFile(bucketFilename, "w", sizeof(BucketRecord), 0);
    file->tableSize = 128;
    writeHeader(&(file->tableSize), file->bucketIndex);
    bucket = 0;
    for ( recnum = 0; recnum < file->tableSize; ++recnum )
        writeDirect(&bucket, file->bucketIndex, recnum);
    writeHeader(NULL, file->bucketIndex);

    rec.length = 0;
    writeDirect(&rec, file->bucketFile, 0);
    writeHeader(NULL, file->bucketFile);
}

EHTFILE * openEHTFile(char * filename, int keySize) {
    char bucketIndexName[512];
    char bucketFilename[512];
    EHTFILE * file = (EHTFILE *)malloc(sizeof(EHTFILE));
    if ( file == NULL )
        return NULL;

    strncpy(bucketIndexName, filename, 508);
    strcat(bucketIndexName, ".eht");
    strncpy(bucketFilename, filename, 508);
    strcat(bucketFilename, ".bkt");

    file->keyLength = keySize;
    file->bucketIndex = openDirectAccessFile(bucketIndexName, "r", sizeof(unsigned), sizeof(unsigned));
    if ( file->bucketIndex == NULL )
        createIndex(file, bucketIndexName, bucketFilename);
    else {
        file->bucketFile = openDirectAccessFile(bucketFilename, "r", sizeof(BucketRecord), 0);
        if ( file->bucketFile == NULL ) {
            closeDirect(file->bucketIndex);
            createIndex(file, bucketIndexName, bucketFilename);
        } else
            readHeader(&(file->tableSize), file->bucketIndex);
    }
    initCache(file);
    return file;
}

static void getBucket(EHTFILE * file, unsigned recordPos) {
	unsigned entry = recordPos % 512;
	unsigned bucketPos;

	if ( file->cache->tablePos[entry] == recordPos ) {
		if ( file->bucketNumber != file->cache->bucketPos[entry] ) {
			file->bucketNumber = file->cache->bucketPos[entry];
			readDirect(file->bucket, file->bucketFile, file->bucketNumber);
		}
	} else {
		readDirect(&bucketPos, file->bucketIndex, recordPos);
		if ( bucketPos != file->bucketNumber ) {
			file->bucketNumber = bucketPos;
			readDirect(file->bucket, file->bucketFile, bucketPos);
		}
		file->cache->bucketPos[entry] = bucketPos;
		file->cache->tablePos[entry] = recordPos;
	}
}

unsigned readEHTFile(EHTFILE * file, char * key) {
    unsigned recordPos = hash(key) % file->tableSize;
    char * ptr;
    int i;

    getBucket(file, recordPos);
    for ( i = 0, ptr = file->bucket->buffer; i < file->bucket->length; ++i, ptr += sizeof(unsigned) + file->keyLength ) {
		IndexOverlay * indexEntry = (IndexOverlay *)ptr;
        if ( strcmp(key, indexEntry->key) == 0 )
            return indexEntry->link;
	}
	return 0;
}

static int splitBucket(EHTFILE * file, char * key, unsigned position) {
	BucketRecord oldBkt, newBkt;
	char * oldPtr = oldBkt.buffer;
	char * newPtr = newBkt.buffer;
	char * ptr = file->bucket->buffer;
	unsigned newHashKey = hash(key);
	unsigned newHashPos = newHashKey % file->tableSize;
	int numDifferent = 0;
	int i;
	unsigned entrySize = sizeof(unsigned) + file->keyLength;

	oldBkt.length = 0;
	newBkt.length = 0;
	for ( i = 0; i < file->bucket->length; ++i ) {
		IndexOverlay * original = (IndexOverlay *)ptr;
		unsigned oldHashKey = hash(original->key);
		if ( oldHashKey != newHashKey )
		    ++numDifferent;
		if ( oldHashKey % file->tableSize == newHashPos ) {
		    IndexOverlay * newEntry = (IndexOverlay *)newPtr;
		    newEntry->link = original->link;
		    strcpy(newEntry->key, original->key);
		    newPtr += entrySize;
		    newBkt.length++;
		} else {
		    IndexOverlay * newEntry = (IndexOverlay *)oldPtr;
		    newEntry->link = original->link;
		    strcpy(newEntry->key, original->key);
		    oldPtr += entrySize;
		    oldBkt.length++;
		}
		ptr += entrySize;
	}
    if ( numDifferent == 0 ) {
        fprintf(stderr, "Unable to process this key!\n");
        exit(1);
	}
	if ( oldBkt.length > 0 ) {
	    IndexOverlay * newEntry = (IndexOverlay *)newPtr;
	    newEntry->link = position;
	    strcpy(newEntry->key, key);
	    newBkt.length++;
	    writeDirect(&oldBkt, file->bucketFile, file->bucketNumber);
	    file->bucketNumber = getDALength(file->bucketFile);
	    writeDirect(&newBkt, file->bucketFile, file->bucketNumber);
	    memcpy(file->bucket, &newBkt, sizeof(newBkt));
	    writeHeader(NULL, file->bucketFile);

	    /* update bucket index */
	    if ( file->cache->tablePos[newHashPos % 512] != newHashPos )
			file->cache->tablePos[newHashPos % 512] = newHashPos;
		file->cache->bucketPos[newHashPos % 512] = file->bucket;
		writeDirect(&(file->bucket), file->bucketIndex, newHashPos);
	    return 1;
	} else
	    return 0;
}

static void doubleHashTable(EHTFILE * file) {
	unsigned i;

	if ( file->tableSize < 512 ) { /* update cache to reflect bucket index */
		for ( i = 0; i < file->tableSize; ++i ) {
		    file->cache->bucketPos[i + file->tableSize] = file->cache->bucketPos[i];
		    file->cache->tablePos[i + file->tableSize] = file->cache->tablePos[i];
		    writeDirect(&(file->cache->bucketPos[i]), file->bucketIndex, i + file->tableSize);
		}
	} else {
		unsigned bucketPos;
    	for ( i = 0; i < file->tableSize; ++i ) {
    		readDirect(&bucketPos, file->bucketIndex, i);
    		writeDirect(&bucketPos, file->bucketIndex, i + file->tableSize);
		}
	}
	file->tableSize *= 2;
	writeHeader(NULL, file->bucketIndex);
}

int addEHTFile(EHTFILE * file, char * key, unsigned position) {
	int entrySize = file->keyLength + sizeof(unsigned);
	int bucketSize = sizeof(file->bucket->buffer) / entrySize;

	if ( readEHTFile(file, key) != 0 )
	    return 1;

	if ( file->bucket->length < bucketSize ) {
		IndexOverlay * indexEntry = (IndexOverlay *)(file->bucket->buffer + file->bucket->length * entrySize);
		indexEntry->link = position;
		strncpy(indexEntry->key, key, file->keyLength);
		file->bucket->length++;
		writeDirect(file->bucket, file->bucketFile, file->bucketNumber);
	} else
		while ( ! splitBucket(file, key, position) )
		    doubleHashTable(file);
}

void closeEHTFile(EHTFILE * file) {
	writeHeader(NULL, file->bucketIndex);
	writeHeader(NULL, file->bucketFile);
	closeDirect(file->bucketIndex);
	closeDirect(file->bucketFile);
	free(file);
}
