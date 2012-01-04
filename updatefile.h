#if ! defined(_UPDATEFILE_H)
#define _UPDATEFILE_H 1
/**
 * updatefile.h describes interface to updatefile.c for accessing data in a
 * sequential file.
 * Each of the following function returns true (1) on success and false (0)
 * on failure, except for closeUpdateFile.
 * openUpdateFile(filename, intermediateFilename) prepares the file for use
 *     and creates an intermediate file for storing updates until they can be
 *     copied back to the original file.
 * getRecord(key, &record) finds the record with the matching key value.
 * updateRecord(&record) finds the record with the matching key value, usually
 *     the last record obtained by getRecord, and replaces it.
 * insertRecord(&record) adds the new record in order by its key value, unless
 *     the key is already in use within the file (keys are assumed unique).
 * deleteRecord(key) finds the record with the matching key and removes it
 *     from the file.
 * closeUpdateFile() ensures that all updates are saved under the main file,
 *     then closes the main file and intermediate file.
 **/

#include "customer.h"

/* aliases used to limit changes necessary to make updatefile work with
   another record type */
typedef char * KeyType;
#define KEY ID
#define KEY_COMPARE strcmp

int openUpdateFile(char *, char *);
int getRecord(KeyType, Customer *);
int updateRecord(Customer *);
int insertRecord(Customer *);
int deleteRecord(KeyType);
void closeUpdateFile();
#endif
