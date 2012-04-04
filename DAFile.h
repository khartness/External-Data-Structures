#if ! defined(_DAFILE_H)
#define _DAFILE_H 1

#include <stdio.h>

/* error codes */
#define DAFILE_OK 0
#define DAFILE_NOFILE 1
#define DAFILE_NOCREATE 2
#define DAFILE_NOHEADER 3
#define DAFILE_BADHEADER 4
#define DAFILE_BADMODE 5
#define DAFILE_WRITEFAILURE 6
#define DAFILE_READFAILURE 7
#define DAFILE_DELETEFAILURE 8
#define DAFILE_EMPTYRECORD 16
#define DAFILE_DELETEDRECORD 17

typedef
    struct {
        FILE * file;
        int recordSize;
        int fileSize;
        int headerSize;
    } DAFile;

DAFile * openDirectAccessFile(char *, char *, int, int);
int readHeader(void *, DAFile *);
int writeHeader(void *, DAFile *);
int readDirect(void *, DAFile *, int);
int writeDirect(void *, DAFile *, int);
int deleteDirect(DAFile *, int);
void closeDirect(DAFile *);
int getDAError();
int getDAFileSize(DAFile *);
int getDALength(DAFile *);
#endif
