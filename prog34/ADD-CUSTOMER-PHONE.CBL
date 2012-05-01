       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           ADD-CUSTOMER-PHONE.
       
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
       01  W-NEW-HASH-KEY          PIC 9(9) COMP.
       01  W-HASH-KEY              PIC 9(9) COMP.
       01  OLD-BUCKET-REC.
           05  OLD-LENGTH          PIC 999 COMP.
           05  OLD-PAIR            OCCURS 1 TO 292 TIMES
                                   DEPENDING ON OLD-LENGTH
                                   INDEXED BY X-OLDPAIR.
               10  OLD-KEY     PIC 9(10).
               10  OLD-LINK    PIC 9(9) COMP.
       01  NEW-BUCKET-REC.
           05  NEW-LENGTH          PIC 999 COMP.
           05  NEW-PAIR            OCCURS 1 TO 292 TIMES
                                   DEPENDING ON NEW-LENGTH
                                   INDEXED BY X-NEWPAIR.
               10  NEW-KEY     PIC 9(10).
               10  NEW-LINK    PIC 9(9) COMP.
       01  W-NUM-DIFFERENT     PIC 999 COMP VALUE 0.
           
       LINKAGE SECTION.
       01  L-STATUS    PIC 9.
           88  STATUS-OK VALUE 0.
           88  STATUS-DUPLICATE-KEY VALUE 1.
           88  STATUS-OTHER VALUE 2.
       01  L-PHONE     PIC 9(10).
       01  L-POSITION  PIC 9(9) COMP.
       
       PROCEDURE DIVISION USING L-STATUS, L-PHONE, L-POSITION.
       ENTRY-POINT.
           SET STATUS-OK TO TRUE.
           CALL 'SEARCH-CUSTOMER-PHONE' USING W-HASH-KEY, L-PHONE.
           IF W-HASH-KEY NOT = 0 THEN
               SET STATUS-DUPLICATE-KEY TO TRUE
           ELSE IF BUCKET-LENGTH < 292 THEN
               ADD 1 TO BUCKET-LENGTH
               MOVE L-PHONE TO BR-KEY(BUCKET-LENGTH)
               MOVE L-POSITION TO BR-LINK(BUCKET-LENGTH)
               REWRITE BUCKET-REC
                   INVALID KEY
                       SET STATUS-OTHER TO TRUE
               END-REWRITE
           ELSE
               PERFORM SPLIT-BUCKET UNTIL OLD-LENGTH > 0
               REWRITE BUCKET-REC FROM OLD-BUCKET-REC
                   INVALID KEY
                       SET STATUS-OTHER TO TRUE
                       GOBACK
               END-REWRITE
               ADD 1 TO W-BUCKET-FILE-SIZE GIVING W-BUCKET
               ADD 1 TO NEW-LENGTH
               MOVE L-PHONE TO NEW-KEY(NEW-LENGTH)
               MOVE L-POSITION TO NEW-LINK(NEW-LENGTH)
               WRITE BUCKET-REC FROM NEW-BUCKET-REC
                   INVALID KEY
                       SET STATUS-OTHER TO TRUE
                       GOBACK
               END-WRITE
               PERFORM CHANGE-BUCKET-INDEX.
           GOBACK.
       
       SPLIT-BUCKET.
           MOVE 0 TO OLD-LENGTH, NEW-LENGTH, W-NUM-DIFFERENT.
           CALL 'HASH-PHONE' USING W-NEW-HASH-KEY, L-PHONE.
           COMPUTE W-HASH-POS =
               FUNCTION MOD(W-NEW-HASH-KEY, W-TABLESIZE) + 2.
           PERFORM VARYING X-PAIR FROM 1 BY 1
                   UNTIL X-PAIR > BUCKET-LENGTH
               CALL 'HASH-PHONE' USING W-HASH-KEY, BR-KEY(X-PAIR)
               IF W-NEW-HASH-KEY NOT = W-HASH-KEY THEN
                   ADD 1 TO W-NUM-DIFFERENT
               END-IF
               IF W-HASH-POS = FUNCTION MOD(W-HASH-KEY, W-TABLESIZE) + 2
                   ADD 1 TO NEW-LENGTH
                   MOVE BUCKET-PAIR(X-PAIR) TO NEW-PAIR(NEW-LENGTH)
               ELSE
                   ADD 1 TO OLD-LENGTH
                   MOVE BUCKET-PAIR(X-PAIR) TO OLD-PAIR(OLD-LENGTH)
               END-IF
           END-PERFORM.
           IF OLD-LENGTH = 0 THEN
               IF W-NUM-DIFFERENT = 0 THEN
                   DISPLAY "Unable to split bucket!"
                   SET STATUS-OTHER TO TRUE
                   GOBACK
               ELSE
                   PERFORM DOUBLE-BUCKET-INDEX.

       DOUBLE-BUCKET-INDEX.
           IF W-TABLESIZE < 512 THEN
               PERFORM VARYING W-ENTRY-POS FROM 1 BY 1
                       UNTIL W-POS > W-TABLESIZE
                   ADD W-TABLESIZE TO W-RECORD-POS(W-ENTRY-POS)
                       GIVING W-RECORD-POS(W-ENTRY-POS + W-TABLESIZE)
                   MOVE W-BUCKET-POS(W-ENTRY-POS)
                       TO W-BUCKET-POS(W-ENTRY-POS + W-TABLESIZE)
               END-PERFORM.
           MOVE 2 TO W-HASH-POS.
           PERFORM UNTIL W-HASH-POS > W-TABLESIZE + 1
               READ BUCKET-INDEX
               ADD W-TABLESIZE TO W-HASH-POS
               WRITE BI-BUCKET
               SUBTRACT W-TABLESIZE, -1 FROM W-HASH-POS
           END-PERFORM.
           MULTIPLY 2 BY W-TABLESIZE.
           MOVE 1 TO W-HASH-POS.
           REWRITE BI-BUCKET FROM W-TABLESIZE.
       
       CHANGE-BUCKET-INDEX.
           IF W-TABLESIZE > 512 THEN
               COMPUTE W-ENTRY-POS = FUNCTION MOD(W-HASH-POS - 2, 512)
                                     + 1
           ELSE
               SUBTRACT 1 FROM W-HASH-POS GIVING W-ENTRY-POS.
           MOVE W-HASH-POS TO W-RECORD-POS(W-ENTRY-POS).
           MOVE W-BUCKET TO W-BUCKET-POS(W-ENTRY-POS).
           REWRITE BI-BUCKET FROM W-BUCKET
               INVALID KEY
                   SET STATUS-OTHER TO TRUE.
