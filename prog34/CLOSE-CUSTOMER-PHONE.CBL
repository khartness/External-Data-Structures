       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           CLOSE-CUSTOMER-PHONE.
       
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
       01  W-BUCKET-FILE-SIZE      PIC 9(9) COMP.
       01  W-EHT-CACHE IS EXTERNAL.
           05  W-RECORD-POS        PIC 9(9) COMP OCCURS 512 TIMES
                                   VALUE 0.
           05  W-BUCKET-POS        PIC 9(9) COMP OCCURS 512 TIMES
                                   VALUE 0.
           
       PROCEDURE DIVISION.
       CLOSE-FILES.
      * I've been using a "write through" approach to ensure every
      * change is written except the bucket file size.
           MOVE 1 TO W-BUCKET.
           READ BUCKET-FILE.
           MOVE W-BUCKET-FILE-SIZE TO BR-LINK(292).
           REWRITE BUCKET-REC.
           CLOSE BUCKET-FILE.
           CLOSE BUCKET-INDEX.
           GOBACK.
