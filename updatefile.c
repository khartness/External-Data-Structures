#include <stdio.h>
#include <string.h>
#include "updatefile.h"

/* Prior to C++, it was a trivial matter to create the equivalent of a
   single static object by creating private variables that could only
   be viewed from within a single source file. If you need more than
   one file that works like this, it's possible to put the static
   variables in a struct and figure out how to make it work for
   different size records with different keys, but I thought it would
   be easier for you to cover the technique with a single file (and its
   intermediate file) with a known record format for now. */

/* Definition of alias for easier customization. If packing file records,
   change this to a packed struct similar to that used in rest of program
   and create packing and unpacking functions to use in place of memcpy */
typedef Customer RecordBuffer; /* for simplicity, I am skipping packing */

/* private variables visible only within updatefile.c */
static FILE * sourceFile;
static FILE * destinationFile;
static char mainFilename[512];  /* lots of space for long paths */
static char intFilename[512];
static char * srcFilename = mainFilename;
static char * destFilename = intFilename;
static RecordBuffer rec;
static int atEnd = 0;

/**
 * openUpdateFile(mainFile, intermediateFile)
 *     saves filenames in mainFilename and intFilename, respectively.
 *     Initially, mainFilename contains input records and intFilename
 *     contains output records. After the first pass through the data, the
 *     filenames are switched so that the intermediate file provides the
 *     input and the main file receives the output. The pointer srcFilename
 *     points to the input file and the pointer destFilename points to the
 *     output filename as it is easier to swap pointers than actual
 *     characters.
 *     The first record is read after the files are opened or atEnd is set
 *     to true if the file is empty, initially.
 *     Returns whether or not opening the two files is successful.
 **/
int openUpdateFile(char * filename, char * intermediateFilename) {
    strncpy(mainFilename, filename, 512);
    srcFilename = mainFilename;
    strncpy(intFilename, intermediateFilename, 512);
    destFilename = intFilename;
    sourceFile = fopen(filename, "rb");
    if ( sourceFile == NULL )
        return 0;
    destinationFile = fopen(intermediateFilename, "wb");
    if ( destinationFile == NULL )
        return 0;
    atEnd = ( fread(&rec, sizeof(rec), 1, sourceFile) < 1 );
    return 1;
}

/* changeDirections should only be called at the end of the file.
   It closes the files, swaps the filenames, and reopens the files
   at the beginning of the data, using the most recently updated
   file as input. */
static void changeDirections() {
    char * temp;

    fclose(sourceFile);
    fclose(destinationFile);
    temp = srcFilename;
    srcFilename = destFilename;
    destFilename = temp;
    sourceFile = fopen(srcFilename, "rb");
    destinationFile = fopen(destFilename, "wb");
    atEnd = ( fread(&rec, sizeof(rec), 1, sourceFile) < 1);
}

/* copyRestOfFile ensures that a complete, updated copy exists in
   the destination file before closing files or changing directions. */
static void copyRestOfFile() {
    while ( ! atEnd ) {
        fwrite(&rec, sizeof(rec), 1, destinationFile);
        atEnd = ( fread(&rec, sizeof(rec), 1, sourceFile) < 1);
    }
}

/* findKey(key) copies records until it is either
       at the record containing the matching key,
       at the record that should immediately follow the key, if it were there,
       or at the end of the file if the key is larger than the largest key
          in the file.
   As a result, it is at the perfect spot to get a desired record, update
   a record, delete a record, or insert a new record.
   Returns whether rec contains a matching key. */
static int findKey(KeyType key) {
    if ( atEnd )
        changeDirections();
    else if ( KEY_COMPARE(key, rec.KEY) < 0 ) {
        copyRestOfFile();
        changeDirections();
    }

    while ( ! atEnd && KEY_COMPARE(key, rec.KEY) > 0 ) {
        fwrite(&rec, sizeof(rec), 1, destinationFile);
        atEnd = (fread(&rec, sizeof(rec), 1, sourceFile) < 1);
    }

    return ( ! atEnd && KEY_COMPARE(key, rec.KEY) == 0 );
}

/**
 * getRecord(key, &record) copies the record with matching key into the
 * address indicated by the second argument.
 * Returns whether a matching record exists. If false, specified record
 * is unchanged.
 **/
int getRecord(KeyType key, Customer * result) {
    if ( findKey(key) ) {
        memcpy(result, &rec, sizeof(rec));
        return 1;
    } else
        return 0;
}

/**
 * updateRecord(&record) changes the contents of the matching file record.
 * It is optimized so you can use getRecord to obtain data, change the data,
 * then use updateRecord to save the change. You can update the record
 * multiple times without incurring I/O as long as no access of other records
 * is occurring. This version does allow you to save multiple records and
 * update them in any order, although performance does suffer. updateRecord
 * simply searches to find the record if the one you are updating isn't the
 * current one. This flexibility comes at a price, however, as there is no
 * way to prevent you from updating the key and destroying another record.
 * To update the key value, you should use getRecord to save a local copy,
 * deleteRecord(key) to destroy the old record, change the key, and use
 * insertRecord to save the change as a new record.
 * Returns whether or not the updated record matches an existing record
 * in the file.
 **/
int updateRecord(Customer * r) {
	if ( KEY_COMPARE(r->KEY, rec.KEY) != 0 ) {
	    if ( ! findKey(r->KEY) )
	        return 0;
	}
	memcpy(&rec, r, sizeof(rec)); /* can replace memcpy with packing and unpacking operations, if desired */
	return 1;
}

/**
 * insertRecord(&newRecord) adds the new record to the file unless its key is
 * already there. If you assume that new records will be immediately followed
 * by updates to the same record, then it would be more efficient to maintain
 * two buffers, one for the last read record and one for the current record,
 * so that new records are copied into the current record in memory. Reads
 * from the file would then need to check whether the current and last read
 * records were different, in which case they would just copy the last read
 * record into the current record after presumably saving the new record in
 * current to the destinationFile. By writing the new record directly, I
 * guarantee that a search for it must traverse the entire file before reading
 * it again (assuming no searches for other keys happens in between).
 * Returns whether the key is new and unique and the write was successful.
 **/
int insertRecord(Customer * newRec) {
	if ( findKey(newRec->KEY) )
	    return 0;
	else
	    return ( fwrite(newRec, sizeof(RecordBuffer), 1, destinationFile) == 1 );
}

/**
 * deleteRecord(key) locates the desired record. Normally, all reads are ultimately
 * followed by a write. In this case, the next record is read from the file without
 * saving the current record so that the next pass through the file will not find
 * this record, anymore.
 * Returns whether the record with the key was found (and presumably deleted).
 **/
int deleteRecord(KeyType key) {
	if ( findKey(key) ) { /* replace current record with next record */
		atEnd = (fread(&rec, sizeof(rec), 1, sourceFile) < 1);
		return 1;
	} else
	    return 0;
}

/**
 * closeUpdatefile() ensures that the primary file contains a complete set
 * of records, as recently updated as possible, then closes the primary and
 * intermediate files.
 **/
void closeUpdateFile() {
	copyRestOfFile(); /* make sure we have a complete set of records */
	if ( destFilename != mainFilename ) { /* if latest updates are not in */
		changeDirections();               /* the primary file, copy them */
		copyRestOfFile();                 /* back to the primary file. */
	}
	fclose(sourceFile);
	fclose(destinationFile);
}
