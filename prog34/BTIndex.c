#include "BTIndex.h"
#include <stdlib.h>
#include <memory.h>

typedef
    struct {
		int length;
		char data[MAX_BUCKET_SIZE - sizeof(int)];
	} PackedRecord;

typedef
    struct {
		unsigned pos;
		unsigned char length;
		char key[255];
	} KeyLinkPair;

/* simulate packing  to see if record will overfill */
static int fullRecord(BTNode * node, char * key) {
	int length = sizeof(int) + sizeof(char) + strlen(key) + sizeof(int);
	int i;

	for ( i = 0; i < node->length; ++i )
	    length += sizeof(int) + sizeof(char) + strlen(node->key[i]);
	return length <= MAX_BUCKET_SIZE - sizeof(int);
}

static int findMiddle(BTNode * node) {
	int length = sizeof(int); /* length at front of packed record */
	int middle;

	for ( middle = 0; length < MAX_BUCKET_SIZE / 2; ++middle )
	    length += sizeof(int) + sizeof(char) + strlen(node->key[i]);
	return middle;
}

static int packRecord(PackedRecord * rec, BTNode * node) {
	int i;
	char * ptr = rec->data;
	KeyLinkPair * klptr;
	int slen, length;

	length = 0;
	for ( i = 0; i < node->length && length < sizeof(rec->data); ++i ) {
		slen = strlen(node->key[i]);
		length += sizeof(int) + sizeof(char) + slen;
		if ( length > sizeof(rec->data) - sizeof(int) )
		    return 0;

		klptr = (KeyLinkPair *)ptr;
		klptr->pos = node->link[i];
		klptr->length = slen;
		strcpy(klptr->key, node->key[i]);
		ptr += sizeof(klptr->pos) + sizeof(klptr->length) + slen;
	}
	*(unsigned *)ptr = node->link[i];
	length += sizeof(unsigned);

	if ( node->leaf )
	    rec->length = - length;
	else
	    rec->length = length;

	return 1;
}

static int unpackRecord(BTNode * node, PackedRecord * rec) {
	int i;
	char * ptr = rec->data;
	KeyLinkPair * klptr;
	int length;

    if ( rec->length < 0 ) {
        node->leaf = 1;
        length = - rec->length;
	} else {
		node->leaf = 0;
		length = rec->length;
	}

	i = 0;
	while ( ptr - rec->data < length - sizeof(unsigned) && i < BUCKET_SIZE ) {
		klptr = (KeyLinkPair *)ptr;
		node->link[i] = klptr->pos;
		strncpy(node->key[i], klptr->key, klptr->length + 1);

		ptr += sizeof(klptr->pos) + sizeof(klptr->length) + klptr->length;
		++i;
	}
    node->length = i;
    node->link[i] = *(unsigned *)ptr;
	return ptr - rec->data == length - sizeof(unsigned);
}

static int getCacheOffset(BTIndex * index, unsigned where, int forcePresence) {
	int i;
	int leastFrequent = 0;
	unsigned min = 0xffffffff;
	PackedRecord prec;

	for ( i = 0; i < index->cacheSize && index->cache[i].pos != where; ++i )
	    if ( index->cache[i].frequency <= min ) {
			min = index->cache[i].frequency;
			leastFrequent = i;
		}

	if ( i == index->cacheSize ) {
		if ( index->cacheSize < 8 )
		    leastFrequent = index->cacheSize;
		if ( forcePresence || ! readDirect(index->file, &prec, where) ||
		     ! unpackRecord(&(index->cache[leastFrequent].rec, &prec) )
		    return -1;
		index->cache[leastFrequent].frequency = 1;
		index->cache[leastFrequent].pos = where;
		if ( index->cacheSize < 8 )
		    index->cacheSize++;
	} else {
		index->cache[i].frequency++;
		leastFrequent = i;
	}
	return leastFrequent;
}

static int readNode(BTIndex * index, BTNode * rec, unsigned where) {
	int i = getCacheOffset(index, where, 1);

	if ( i < 0 )
	    return 0;

    memcpy(rec, &(index->cache[i].rec), sizeof(BTNode));
	return 1;
}

static int writeNode(BTIndex * index, BTNode * rec, unsigned where) {
	PackedRecord prec;

	if ( packRecord(&prec, rec) && writeDirect(index->file, &prec, where) ) {
	    int i = getCacheOffset(index, where, 0);
	    if ( i >= 0 ) /* update value in cache */
	        memcpy(&(index->cache[i].rec), rec, sizeof(BTNode));
	    return 1;
	} else
	    return 0;
}

static void split(BTNode * oldRec, BTNode * newRec, char * separator) {
	int middle, pos;

    middle = findMiddle(oldRec);
    /* minimal separator */
    for ( pos = 0; oldRec->key[middle][pos] != '\0' && oldRec->key[middle][pos] == oldRec->key[middle-1][pos];
          ++pos )
        ;
	strncpy(separator, oldRec->key[middle], pos+1);

	if ( oldRec->leaf )
    	pos = middle;
    else
        pos = middle + 1;

	newRec->leaf = oldRec->leaf;
	newRec->length = 0;
	while ( pos < oldRec->length ) {
		strcpy(newRec->key[newRec->length], oldRec->key[pos]);
		newRec->link[newRec->length] = oldRec->link[pos];
		newRec->length++;
		pos++;
	}
	newRec->link[newRec->length] = oldRec->link[pos];
    oldRec->length = middle;
}

BTIndex * createBTIndex(char * filename) {
	DAFile * file = openDirectFile(filename, "w", sizeof(PackedRecord), sizeof(unsigned));
	if ( file != NULL ) {
		BTIndex * result = (BTIndex *)malloc(sizeof(BTIndex));
		BTNode firstRoot;
		PackedRecord rec;

		firstRoot.leaf = 1;
		firstRoot.length = 0;
		firstRoot.link[0] = NO_RECORD;
		packRecord(&rec, &firstRoot);
		result->file = file;
		result->root = 0;
		result->cacheSize = 0;
		result->current = 0;
		if ( writeHeading(file, &(result->root)) && writeDirect(file, &rec, 0) )
		    return result;
		else
		    return NULL;
	} else
	    return NULL;

}

BTIndex * openBTIndex(char * filename) {
	DAFile * file = openDirectFile(filename, "r", sizeof(BTNode), sizeof(unsigned));
	if ( file != NULL ) {
		BTIndex * result = (BTIndex *)malloc(sizeof(BTIndex));

		result->file = file;
		result->cacheSize = 0;
		result->current = 0;
		if ( readHeading(file, &(result->root)) )
		    return result;
		else
		    return NULL;
	else
	    return NULL;
}

static int stack[100];
static int top = 0;

unsigned findBTreeKey(BTIndex * index, char * key) {
    BTNode rec;
    unsigned ptr = index->root;
    int i;
    int keylen = strlen(key);

    stack[0] = ptr;
    top = 1;
    while ( readNode(index, &rec, ptr) && ! rec.leaf ) {
		for ( i = 0; i < rec.length && strcmp(key, rec.key[i]) >= 0; ++i )
		    ;
		ptr = rec.link[i];
		stack[top++] = ptr;
	}

	for ( i = 0; i < rec.length && strncmp(key, rec.key[i], keylen) > 0; ++i )
	    ;
	if ( i == rec.length || strncmp(key, rec.key[i], keylen) < 0 )
	    return NO_RECORD;
	else {
		index->current = i;
	    return rec.link[i];
	}
}

unsigned nextBTreeKey(BTIndex * index, char * key, int maxKeyLength) {
	BTNode rec;

	if ( top == 0 )
	    return NO_RECORD; /* haven't searched, yet */
	readNode(index, &rec, stack[top-1]); /* last read leaf */
	if ( ! rec.leaf || (index->current + 1 == rec.length && rec.link[rec.length] == NO_RECORD) )
	    return NO_RECORD;
	index->current++;
	if ( index->current == rec.length ) {
		index->current = 0;
		readNode(index, &rec, rec.link[rec.length]);
	}
	strncpy(key, rec.key[index->current], maxKeyLength);
	return rec.link[index->current];
}

void addBTreeKey(BTIndex * index, char *key, unsigned pos) {
	BTNode rec;
	unsigned promoteRight = 0;
	char sep1[KEY_SIZE];
	char sep2[KEY_SIZE];
	unsigned ptr;

	findKey(key, index);
	ptr = stack[--top];
	if ( ! readNode(index, &rec, ptr) )
	    return;
	if ( fullRecord(&rec, key) || rec.length == BUCKET_SIZE ) {
		BTNode newRec;
		split(&rec, &newRec, sep2);
		if ( strcmp(key, sep2) < 0 )
		    insertKey(&rec, key, pos);
		else
		    insertKey(&newRec, key, pos);
		rec.link[rec.length] = promoteRight = fileDALength(index->file);
		writeNode(index, &rec, ptr);
		writeNode(index, &newRec, promoteRight);
        while ( promoteRight != 0 && top > 0 ) {
			ptr = stack[--top];
			readNode(index, &rec, ptr);
			if ( fullRecord(&rec, sep2) || rec.length == BUCKET_SIZE ) {
				strcpy(sep1, sep2);
				split(&rec, &newRec, sep2);
				if ( strcmp(sep1, sep2) < 0 )
				    insertKey(&rec, sep1, promoteRight);
				else
				    insertKey(&newRec, sep1, promoteRight);
				rec.link[rec.length] = promoteRight = fileLength(index->file);
				writeNode(index, &rec, ptr);
				writeNode(index, &newRec, promoteRight);
			} else
				promoteRight = 0;
    	}
    	if ( promoteRight != 0 ) { /* increase height */
    	    rec.length = 1;
    	    rec.leaf = 0;
    	    rec.link[0] = index->root;
    	    rec.link[1] = promoteRight;
    	    strcpy(rec.key[0], sep2);
    	    index->root = fileLength(index->file);
    	    writeNode(index, &rec, index->root);
		}
	} else {
		insertKey(&rec, key, pos);
}
