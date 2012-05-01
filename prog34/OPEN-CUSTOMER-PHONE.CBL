       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           OPEN-CUSTOMER-PHONE.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * I prefer using INVALID KEY when I can, but sometimes you need
      * more control, so these temporary files give me that.
           SELECT TEST-BUCKET-INDEX ASSIGN TO W-FILENAME1
           ORGANIZATION IS RELATIVE
           ACCESS IS SEQUENTIAL
           STATUS IS W-INDEX-STATUS.
           
           SELECT TEST-BUCKET-FILE ASSIGN TO W-FILENAME2
           ORGANIZATION IS RELATIVE
           ACCESS IS SEQUENTIAL
           STATUS IS W-BUCKET-STATUS.
           
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
       FD  TEST-BUCKET-INDEX.
       01  TEST-BUCKET PIC 9(9) COMP.
       
       FD  TEST-BUCKET-FILE.
       01  TEST-BUCKET-REC PIC X(4090).
       
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
       01  W-BUCKET-FILE-SIZE      PIC 9(9) COMP.
       01  W-FILENAME1             PIC X(512).
       01  W-FILENAME2             PIC X(512).
       01  W-PERIOD                PIC 999 COMP.
       01  W-POS                   PIC 999 COMP.
       01  W-BASELEN               PIC 999 COMP.
       01  W-EXTLEN                PIC 999 COMP.
       01  W-INDEX-STATUS          PIC XX.
       01  W-BUCKET-STATUS         PIC XX.

       LINKAGE SECTION.
       01  L-FILENAME  PIC X(512).
       
       PROCEDURE DIVISION USING L-FILENAME.
       PREPARE-FILES.
           MOVE 0 TO W-PERIOD.
           PERFORM VARYING W-POS
                   FROM 1 BY 1
                   UNTIL W-POS = 509 OR L-FILENAME(W-POS:2) = '  '
               IF L-FILENAME(W-POS:1) = '.' THEN
                   MOVE W-POS TO W-PERIOD
               END-IF
           END-PERFORM.
           SUBTRACT 1 FROM W-POS.
           MOVE SPACES TO W-FILENAME1, W-FILENAME2.
           IF W-PERIOD NOT = 0 THEN
               SUBTRACT 1 FROM W-PERIOD GIVING W-BASELEN
               SUBTRACT W-BASELEN FROM W-POS GIVING W-EXTLEN
               STRING L-FILENAME(1:W-PERIOD) DELIMITED BY SIZE
                      "1" DELIMITED BY SIZE
                      L-FILENAME(W-PERIOD:W-EXTLEN) DELIMITED BY SIZE
                      INTO W-FILENAME1
               STRING L-FILENAME(1:W-PERIOD) DELIMITED BY SIZE
                      "2" DELIMITED BY SIZE
                      L-FILENAME(W-PERIOD:W-EXTLEN) DELIMITED BY SIZE
                      INTO W-FILENAME2
           ELSE
               STRING L-FILENAME(1:W-POS) DELIMITED BY SIZE
                      "1.REL" DELIMITED BY SIZE
                      INTO W-FILENAME1
               STRING L-FILENAME(1:W-POS) DELIMITED BY SIZE
                      "2.REL" DELIMITED BY SIZE
                      INTO W-FILENAME2.

           PERFORM CHECK-FILES.
           
           OPEN I-O BUCKET-INDEX.
           OPEN I-O BUCKET-FILE.
           MOVE 1 TO W-HASH-POS.
           READ BUCKET-INDEX INTO W-TABLESIZE.
           MOVE 1 TO W-BUCKET.
           READ BUCKET-FILE.
           MOVE BR-LINK(292) TO W-BUCKET-FILE-SIZE.
           GOBACK.
       
       CHECK-FILES.
           OPEN INPUT TEST-BUCKET-INDEX.
           IF W-INDEX-STATUS NOT = '00' THEN
               CLOSE TEST-BUCKET-INDEX
               PERFORM CREATE-EHT
           ELSE
               OPEN INPUT TEST-BUCKET-FILE
               IF W-BUCKET-STATUS NOT = '00' THEN
                   CLOSE TEST-BUCKET-INDEX, TEST-BUCKET-FILE
                   PERFORM CREATE-EHT
               ELSE
                   CLOSE TEST-BUCKET-INDEX, TEST-BUCKET-FILE.
       
       CREATE-EHT.
           OPEN OUTPUT BUCKET-INDEX.
           OPEN OUTPUT BUCKET-FILE.
           MOVE 128 TO BI-BUCKET.
           MOVE 1 TO W-HASH-POS.
           WRITE BI-BUCKET
               INVALID KEY PERFORM BAD-HASH-TABLE.
           MOVE 2 TO BI-BUCKET.
           PERFORM VARYING W-HASH-POS FROM 2 BY 1 UNTIL W-HASH-POS > 129
               WRITE BI-BUCKET
                   INVALID KEY PERFORM BAD-HASH-TABLE END-WRITE
           END-PERFORM.
           MOVE 1 TO W-BUCKET.
           MOVE "EXTENDABLE HASH TABLE: PHONE INDEX" TO BUCKET-REC.
           MOVE 2 TO BR-LINK(292).
           WRITE BUCKET-REC
               INVALID KEY PERFORM BAD-BUCKET-FILE.
           MOVE 2 TO W-BUCKET.
           MOVE 0 TO BUCKET-LENGTH.
           WRITE BUCKET-REC
               INVALID KEY PERFORM BAD-BUCKET-FILE.
           CLOSE BUCKET-INDEX, BUCKET-FILE.
       
       BAD-HASH-TABLE.
           DISPLAY "Unable to create extendable hash table!"
           STOP RUN.
           
       BAD-BUCKET-FILE.
           DISPLAY "Unable to initialize buckets of ",
                   "extendable hash table!".
           STOP RUN.

