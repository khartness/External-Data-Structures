#include "BTIndex.h"
#include <stdlib.h>
#include <memory.h>

static int getCacheOffset(BTIndex * index, unsigned where, int forcePresence) {
	int i;
	int leastFrequent = 0;
	unsigned min = 0xffffffff;

	for ( i = 0; i < index->cacheSize && index->cache[i].pos != where; ++i )
	    if ( index->cache[i].frequency <= min ) {
			min = index->cache[i].frequency;
			leastFrequent = i;
		}

	if ( i == index->cacheSize ) {
		if ( index->cacheSize < 8 )
		    leastFrequent = index->cacheSize;
		if ( forcePresence || ! readDirect(index->file, &(index->cache[leastFrequent].rec, where) )
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
	if ( writeDirect(index->file, rec, where) ) {
	    int i = getCacheOffset(index, where, 0);
	    if ( i >= 0 ) /* update value in cache */
	        memcpy(&(index->cache[i].rec), rec, sizeof(BTNode));
	    return 1;
	} else
	    return 0;
}

static void split(BTNode * oldRec, BTNode * newRec, char * separator) {
	int middle = oldRec->length / 2 + 1;
	int pos;

	strcpy(separator, oldRec->key[middle]);
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
	if ( oldRec->leaf )
	    oldRec->length = middle + 1;
	else
	    oldRec->length = middle;
}

BTIndex * createBTIndex(char * filename) {
	DAFile * file = openDirectFile(filename, "w", sizeof(BTNode), sizeof(unsigned));
	if ( file != NULL ) {
		BTIndex * result = (BTIndex *)malloc(sizeof(BTIndex));
		BTNode firstRoot;

		firstRoot.leaf = 1;
		firstRoot.length = 0;
		firstRoot.link[0] = 0;
		result->file = file;
		result->root = 0;
		result->cacheSize = 0;
		if ( writeHeading(file, &(result->root)) && writeDirect(file, &firstRoot, 0) )
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
		if ( readHeading(file, &(result->root)) )
		    return result;
		else
		    return NULL;
	else
	    return NULL;
}

static int stack[100];
static int top = 0;

unsigned findKey(char * key, BTIndex * index) {
    BTNode rec;
    unsigned ptr = index->root;
    int i;

    stack[0] = ptr;
    top = 1;
    while ( readNode(index, &rec, ptr) && ! rec.leaf ) {
		for ( i = 0; i < rec.length && strcmp(key, rec.key[i]) > 0; ++i )
		    ;
		ptr = rec.link[i];
		stack[top++] = ptr;
	}

	for ( i = 0; i < rec.length && strcmp(key, rec.key[i]) > 0; ++i )
	    ;
	if ( i == rec.length || strcmp(key, rec.key[i]) < 0 )
	    return 0xffffffff;
	else
	    return rec.link[i];
}

static void insertKey(BTNode * rec, char * key, unsigned pos) {
	int i;

	rec->link[rec->length + 1] = rec->link[rec->length];
	for ( i = rec->length; i > 0 && strcmp(rec->key[i-1], key) > 0; --i ) {
		strcpy(rec->key[i], rec->key[i-1]);
		rec->link[i] = rec->link[i-1];
	}
	strcpy(rec->key[i], key);
	rec->link[i] = pos;
}

void addKey(char *key, unsigned pos, BTIndex * index) {
	BTNode rec;
	unsigned promoteRight = 0;
	char sep1[KEY_SIZE];
	char sep2[KEY_SIZE];
	unsigned ptr;

	findKey(key, index);
	ptr = stack[--top];
	if ( ! readNode(index, &rec, ptr) )
	    return;
	if ( rec.length == BUCKET_SIZE ) {
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
			if ( rec.length == BUCKET_SIZE ) {
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
