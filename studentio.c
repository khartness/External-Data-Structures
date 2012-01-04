#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include "studentio.h"

/* Declare record format */

/* save current packing factor (usually 8 for 64-bit alignment) */
#pragma packed push
/* set packing factor to 1 (slower to breakdown from RAM but takes less space */
#pragma packed(1)

typedef
struct s_StudentRecord {
    char ID[7];
    char name[20];
    double gpa;
    int hours;
} StudentRecord;

typedef
struct s_HeaderRecord {
	unsigned long length;
	size_t recordSize;
	char signature[sizeof(StudentRecord)-sizeof(long) - sizeof(size_t)];
} HeaderRecord;

/* restore original packing factor */
#pragma packed pop

static void packStudent(StudentRecord * r, Student * s) {
    int i;
    int pastEOS = 0;

    for ( i = 0; i < 7; ++i )
        r->ID[i] = s->ID[i];
    for ( i = 0; i < 20; ++i )
        if ( pastEOS )
            r->name[i] = '\0';
        else {
            r->name[i] = s->name[i];
            pastEOS = (s->name[i] == '\0');
        }
    r->gpa = s->gpa;
    r->hours = s->hours;
}

static void unpackStudent(Student * s, StudentRecord * r) {
    int i;

    for ( i = 0; i < 7; ++i )
        s->ID[i] = r->ID[i];
    s->ID[7] = '\0';
    for ( i = 0; i < 20 && r->name[i] != '\0'; ++i )
        s->name[i] = r->name[i];
    s->name[i] = '\0';
    s->gpa = r->gpa;
    s->hours = r->hours;
}

static struct s_filelength {
	       FILE * fp;
	       int length;
	   } lengths[100];
static int numFiles = 0;

static void logFile(FILE * fileptr, int len) {
	if ( numFiles < 100 ) {
		int pos = 0;
		while ( pos < numFiles && lengths[pos].fp != fileptr )
		    ++pos;
		lengths[pos].fp = fileptr;
		lengths[pos].length = len;
		++numFiles;
	}
}

static int length(FILE * fileptr) {
	int i;
	HeaderRecord hdr;

	for ( i = 0; i < numFiles; ++i )
	    if ( lengths[i].fp == fileptr )
	        return lengths[i].length;
	fseek(fileptr, 0, SEEK_SET);
	fread(&hdr, sizeof(hdr), 1, fileptr);
	return hdr.length;
}

FILE * openStudentFile(char * filename) {
	FILE * fileptr = fopen(filename, "r+");
	HeaderRecord hdr;

	if ( fileptr == NULL ) {
		fileptr = fopen(filename, "w+");
		if ( fileptr != NULL ) {
			/* new file created; add header record */
			hdr.length = 0;
			hdr.recordSize = sizeof(StudentRecord);
			strcpy(hdr.signature, "STUDENT");
			fwrite(&hdr, sizeof(StudentRecord), 1, fileptr);
			logFile(fileptr, 0);
		}
	} else {
		fseek(fileptr, 0, SEEK_SET);
		if ( fread(&hdr, sizeof(hdr), 1, fileptr) == 1 &&
		     hdr.recordSize == sizeof(StudentRecord) &&
		     strncmp(hdr.signature, "STUDENT", 8) == 0 )
		    logFile(fileptr, hdr.length);
		else {
		    fclose(fileptr);
		    fileptr = NULL;
		}
	}
	return fileptr;
}

int readStudent(FILE * filePointer, Student * stud, long pos) {
    int result = 0;
    StudentRecord rec;
    long offset = (pos + 1) * sizeof(StudentRecord);
    fseek(filePointer, offset, SEEK_SET);
    result = fread(&rec, sizeof(StudentRecord), 1, filePointer);
    if ( result == 1 ) {
        unpackStudent(stud, &rec);

    return result;
}

int writeStudent(FILE * filePointer, Student * stud, long pos) {
    StudentRecord rec;
    int result = 0;
    long offset = (pos + 1) * sizeof(StudentRecord);

    fseek(filePointer, offset, SEEK_SET);
    packStudent(&rec, stud);
    result = fwrite(&rec, sizeof(StudentRecord), 1, filePointer);
    return result;
}

/* findKey(file, key) uses binary search to locate record with the specified
   key or at least where it should go. */
static long findKey(FILE * fp, char * key, Student * stud, int * found) {
	long first, last, middle;
	int cmp;

	first = 1;
	last = length(fp);
	cmp = 1;
	while ( first <= last && cmp != 0 ) {
		middle = (first + last) / 2;
		readStudent(fp, stud, middle);
		cmp = strcmp(key, stud->ID);
		if ( cmp < 0 )
		    last = middle - 1;
		else
		    first = middle + 1;
	}
	if ( cmp == 0 ) {
		*found = 1;
	    return middle;
	} else {
		*found = 0;
	    if ( cmp < 0 )
	        return last;
	    else
	        return first;
	}
}

static char lastID[8] = "";
static long recnum = -1;

/**
 * getRecord(file, &record) copies the record with matching key into the
 * address indicated by the second argument.
 * Returns whether a matching record exists. If false, specified record
 * is unchanged.
 **/
int getRecord(FILE * fp, Student * stud) {
	int result;

	if ( strncmp(lastID, stud->ID, 8) == 0 ) {
		return readStudent(fp, stud, recnum);
	else {
		long pos = findKey(fp, stud->ID, stud, &result);
		if ( result ) {
			strncpy(lastID, stud->ID, 8);
			recnum = pos;
		}
		return result;
	}
}

/**
 * updateRecord(file_pointer, &record) changes the contents of the matching
 * file record. It is optimized so you can use getRecord to obtain data,
 * change the data, then use updateRecord to save the change. updateRecord
 * simply searches to find the record if the one you are updating isn't the
 * current one. This flexibility comes at a price, however, as there is no
 * way to prevent you from updating the key and destroying another record.
 * To update the key value, you should use getRecord to save a local copy,
 * deleteRecord(key) to destroy the old record, change the key, and use
 * insertRecord to save the change as a new record.
 * Returns whether or not the updated record matches an existing record
 * in the file.
 **/
int updateRecord(FILE * fp, Student * r) {
	if ( strcmp(r->ID, lastID) != 0 ) {
		Student temp;
		int result;
		long pos = findKey(fp, r->ID, &temp, &result);
	    if ( ! result )
	        return 0;
	    strncpy(lastID, r->ID, 8);
	    recnum = pos;
	}
	writeStudent(fp, r, recnum);
	return 1;
}

/**
 * insertRecord(file_pointer, &newRecord) adds the new record to the file
 * unless its key is already there.
 * Returns whether the key is new and unique and the write was successful.
 **/
int insertRecord(FILE * fp, Student * newRec) {
	HeaderRecord hdr;
	Student temp;
	int result;
	long pos = findKey(fp, newRec->ID, &temp, &result);
	if ( result ) {
		fprintf(stderr, "ID %s in use for %s\n", temp.ID, temp.name);
		return 0;
	}

	/* could back up from end until pos2 == pos, but I wanted to demonstrate
	   the read-before-see principle. */
	long numRecords = length(fp);
	pos = numRecords;
	while ( pos >= 0 &&
	        readStudent(fp, &temp, pos) &&
	        strcmp(temp.ID, newRec->ID) > 0 ) {
	    if ( ! writeStudent(fp, &temp, pos+1) ) {
	        fprintf(stderr, "Unable to extend file!\n");
	        return 0;
		}
		--pos;
	}
	writeStudent(fp, newRec, pos+1);
	++numRecords;
	/* make sure we remember change in length */
	logFile(fp, numRecords);
	fseek(fp, 0, SEEK_SET);
	fread(&hdr, sizeof(hdr), 1, fp);
	hdr.length++;
	fseek(fp, 0, SEEK_SET);
	fwrite(&hdr, sizeof(hdr), 1, fp);
}

/**
 * deleteRecord(fp, key) locates the desired record, then copies larger
 * records over it.
 * Returns whether the record with the key was found (and presumably deleted).
 **/
int deleteRecord(FILE * fp, char * key) {
	HeaderRecord hdr;
	Student temp;
	int result;
	long pos = findKey(fp, key, &temp, &result);

	if ( result ) { /* replace current record with next record, and so on */

		atEnd = (fread(&rec, sizeof(rec), 1, sourceFile) < 1);
		return 1;
	} else
	    return 0;
}
