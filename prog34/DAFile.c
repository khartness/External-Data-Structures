#include "DAFile.h"
#include <stdlib.h>

static int DAerrno = DAFILE_OK;

#define BASE(fp) (sizeof(int)+sizeof(int)+(fp)->headerSize)

DAFile * openDirectAccessFile(char * filename, char * mode, int recsz, int hdrSize) {
    FILE * fp = NULL;
    DAFile * result;
    int headerRecordSize, headerFileSize;

    DAerrno = DAFILE_OK; /* assume everything is okay until something goes wrong */
    if ( *mode == 'r' )
        fp = fopen(filename, "rb+");
    else if ( *mode == 'w' )
        fp = fopen(filename, "wb+");
    if ( fp == NULL ) {
        if ( *mode == 'r' )
            DAerrno = DAFILE_NOFILE;
        else if ( *mode == 'w' )
            DAerrno = DAFILE_NOCREATE;
        else
            DAerrno = DAFILE_BADMODE;

        return NULL;
    }

    if ( fread(&headerRecordSize, sizeof(int), 1, fp) == 0 )
        if ( *mode == 'w' ) {
			headerRecordSize = recsz;
			headerFileSize = 0;
			fseek(fp, 0, SEEK_SET);
			if ( fwrite(&headerRecordSize, sizeof(int), 1, fp) == 0 ||
			     fwrite(&headerFileSize, sizeof(int), 1, fp) == 0 ) {
			    DAerrno = DAFILE_NOHEADER;
			    fclose(fp);
			    return NULL;
			}
		} else {
			fclose(fp);
			DAerrno = DAFILE_NOHEADER;
			return NULL;
		}
	else if ( fread(&headerFileSize, sizeof(int), 1, fp) == 0 || headerRecordSize != recsz || headerFileSize < 0 ) {
		fclose(fp);
		DAerrno = DAFILE_BADHEADER;
		return NULL;
	}

	result = (DAFile *)malloc(sizeof(DAFile));
	result->file = fp;
	result->recordSize = recsz;
	result->fileSize = headerFileSize;
	result->headerSize = hdrSize;
	return result;
}

int readHeader(void * hdr, DAFile * fp) {
	DAerrno = DAFILE_OK;
	if ( hdr != NULL && fp->headerSize == 0 ) {
		DAerrno = DAFILE_BADHEADER;
		return 0;
	} else if ( hdr == NULL ) { /* check to see if another process updated file length */
	    if ( fseek(fp->file, sizeof(int), SEEK_SET) != 0 ||
	         fread(&(fp->fileSize), sizeof(int), 1, fp->file) != 1 ) {
			DAerrno = DAFILE_READFAILURE;
			return 0;
		} else
    		return 1;
	} else {
		if ( fseek(fp->file, 2*sizeof(int), SEEK_SET) != 0 ||
		     fread(hdr, fp->headerSize, 1, fp->file) != 1 ) {
			DAerrno = DAFILE_READFAILURE;
			return 0;
		} else
			return 1;
	}
}

int writeHeader(void * hdr, DAFile * fp) {
	DAerrno = DAFILE_OK;
	if ( hdr != NULL && fp->headerSize == 0 ) {
		DAerrno = DAFILE_BADHEADER;
		return 0;
	} else if ( hdr == NULL ) { /* update file length */
	    if ( fseek(fp->file, sizeof(int), SEEK_SET) != 0 ||
	         fwrite(&(fp->fileSize), sizeof(int), 1, fp->file) != 1 ) {
			DAerrno = DAFILE_WRITEFAILURE;
			return 0;
		} else
    		return 1;
	} else {
		if ( fseek(fp->file, 2*sizeof(int), SEEK_SET) != 0 ||
		     fwrite(hdr, fp->headerSize, 1, fp->file) != 1 ) {
			DAerrno = DAFILE_WRITEFAILURE;
			return 0;
		} else
			return 1;
	}
}

int recordContains(char * rec, int size, int byte) {
	while ( size > 0 ) {
		if ( *rec != byte )
		    return 0;
		--size;
		++rec;
	}
	return 1;
}

int readDirect(void * rec, DAFile * fp, int recpos) {
	DAerrno = DAFILE_OK;
	if ( fseek(fp->file, recpos * fp->recordSize + BASE(fp), SEEK_SET) == 0 &&
	     fread(rec, fp->recordSize, 1, fp->file) == 1 ) {
		if ( recordContains(rec, fp->recordSize, 0) ) {
		    DAerrno = DAFILE_EMPTYRECORD;
		    return 0;
		} else if ( recordContains(rec, 4, 0xff) ) {
			DAerrno = DAFILE_DELETEDRECORD;
			return 0;
		} else
		    return 1;
	} else {
		DAerrno = DAFILE_READFAILURE;
	    return 0;
	}
}

int writeDirect(void * rec, DAFile * fp, int recpos) {
	DAerrno = DAFILE_OK;
	if ( fseek(fp->file, recpos * fp->recordSize + BASE(fp), SEEK_SET) == 0 &&
	     fwrite(rec, fp->recordSize, 1, fp->file) == 1 ) {
		if ( fp->fileSize <= recpos )
		    fp->fileSize = recpos + 1;
	    return 1;
	} else {
		DAerrno = DAFILE_WRITEFAILURE;
		return 0;
	}
}

int deleteDirect(DAFile * fp, int recpos) {
	static char deletedRecord[4] = {0xff, 0xff, 0xff, 0xff};
	static int size = 0;

	DAerrno = DAFILE_OK;
	if ( fseek(fp->file, recpos * fp->recordSize + BASE(fp), SEEK_SET) == 0 &&
	     fwrite(deletedRecord, 4, 1, fp->file) == 1 )
	    return 1;
	else {
		DAerrno = DAFILE_WRITEFAILURE;
		return 0;
	}
}

void closeDirect(DAFile * fp) {
	writeHeader(NULL, fp);
	fclose(fp->file);
	free(fp);
}

int getDALength(DAFile * fp) {
	return fp->fileSize;
}

int getDAFileSize(DAFile * fp) {
	return fp->fileSize * fp->recordSize + BASE(fp);
}

int getDAError() {
    return DAerrno;
}
