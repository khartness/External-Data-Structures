       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           SEARCH-CUSTOMER-PHONE.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BUCKET-INDEX ASSIGN TO W-FILENAME1
           ORGANIZATION IS RELATIVE
           ACCESS IS RANDOM
           RELATIVE KEY IS W-HASH-POS.
           
           SELECT BUCKET-FILE ASSIGN TO W-FILENAME2
           ORGANIZATION IS RELATIVE
           ACCESS IS RANDOM
           RELATIVE KEY IS W-BUCKET.
       
       DATA DIVISION.
       FILE SECTION.
       FD  BUCKET-INDEX EXTERNAL.
       01  BI-BUCKET   PIC 9(9) COMP.

       FD  BUCKET-FILE EXTERNAL.
       01  BUCKET-REC.
           05  BUCKET-LENGTH   PIC 999 COMP.
           05  BUCKET-PAIR     OCCURS 1 TO 292 TIMES
                               DEPENDING ON BUCKET-LENGTH
                               INDEXED BY X-PAIR.
               10  BR-KEY  PIC 9(10).
               10  BR-LINK PIC 9(9) COMP.
       
       WORKING-STORAGE SECTION.
       01  W-HASH-POS IS EXTERNAL  PIC 9(9) COMP.
       01  W-BUCKET IS EXTERNAL    PIC 9(9) COMP.
       01  W-TABLESIZE IS EXTERNAL PIC 9(9) COMP.
       01  W-BUCKET-FILE-SIZE is external     PIC 9(9) COMP.
       01  W-EHT-CACHE IS EXTERNAL.
           05  W-RECORD-POS        PIC 9(9) COMP OCCURS 512 TIMES
                                   VALUE 0.
           05  W-BUCKET-POS        PIC 9(9) COMP OCCURS 512 TIMES
                                   VALUE 0.
       01  W-ENTRY-POS             PIC 999 COMP.
           
       LINKAGE SECTION.
       01  L-POSITION  PIC 9(9) COMP.
       01  L-PHONE     PIC 9(10).
       
       PROCEDURE DIVISION USING L-POSITION, L-PHONE.
       ENTRY-POINT.
           PERFORM HASH.
           PERFORM GET-BUCKET.
           SET X-PAIR TO 1.
           SEARCH BUCKET-PAIR
               AT END
                   MOVE 0 TO L-POSITION
               WHEN BR-KEY(X-PAIR) = L-PHONE
                   MOVE BR-LINK(X-PAIR) TO L-POSITION.
           GOBACK.
       
       HASH.
           CALL 'HASH-PHONE' USING W-HASH-POS, L-PHONE.
           COMPUTE W-HASH-POS = FUNCTION MOD(W-HASH-POS, W-TABLESIZE).
           ADD 2 TO W-HASH-POS.

       GET-BUCKET.
      * Note that the bucket index is kept in memory, up to 512 entries.
      * For tables of more than 512 entries, the last position, mod 512,
      * is kept so repeated searches of the same key are faster.
      * Also, since multiple entries may reference the same bucket, I
      * check to be sure the bucket isn't already read before doing so.
      * This step may be unnecessary, depending on how COBOL and the O.S.
      * buffer file I/O for me.
           IF W-TABLESIZE > 512 THEN
               COMPUTE W-ENTRY-POS = FUNCTION MOD(W-HASH-POS - 2, 512)
                                     + 1
           ELSE
               SUBTRACT 1 FROM W-HASH-POS GIVING W-ENTRY-POS.

           IF W-RECORD-POS(W-ENTRY-POS) = W-HASH-POS THEN
               IF W-BUCKET-POS(W-ENTRY-POS) NOT = W-BUCKET THEN
                   MOVE W-BUCKET-POS(W-ENTRY-POS) TO W-BUCKET
                   READ BUCKET-FILE
               END-IF
           ELSE
               READ BUCKET-INDEX
               MOVE W-HASH-POS TO W-BUCKET-POS(W-ENTRY-POS)
               IF BI-BUCKET NOT = W-BUCKET THEN
                   MOVE BI-BUCKET TO W-BUCKET
                   READ BUCKET-FILE
               END-READ
               MOVE W-BUCKET TO W-BUCKET-POS(W-ENTRY-POS).
