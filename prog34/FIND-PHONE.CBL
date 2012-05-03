       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           FIND-PHONE.
      *AUTHOR.
      *    Ken Hartness. Created 2012/05/01.
      *DESCRIPTION.
      *    Find a phone number and its call record positions in the inverted index.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    To simplify my handling of the two indexes, I have combined them.
      *    The key is a combination of phone and a code (1 for caller,
      *    2 for callee) to create distinct keys out of the same phone number.
           SELECT CALL-INDEX ASSIGN TO W-FILENAME
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS CALL-KEY.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CALL-INDEX EXTERNAL.
       01  CALL-INDEX-REC.
           05  CALL-KEY.
               10  CI-PHONE    PIC 9(10).
               10  CI-CODE     PIC 9.
           05  CI-LENGTH       PIC 99 COMP.
           05  CI-POSITION     PIC 9(9) COMP OCCURS 124 TIMES.

       WORKING-STORAGE SECTION.
       01  W-POS       PIC 999 COMP.

       LINKAGE SECTION.
       01  L-STATUS    PIC 9.
           88  STATUS-OK VALUE 0.
           88  CALLER-PHONE VALUE 1.
           88  CALLEE-PHONE VALUE 2.
       01  list-name.
           02  list-length PIC 999 COMP.
           02  position    PIC 9(9) COMP OCCURS 124 TIMES.
       01  L-PHONE     PIC 9(10).
      
       PROCEDURE DIVISION USING L-STATUS, L-PHONE, L-POSITION.
       FIND-AND-COPY.
           MOVE L-PHONE TO CI-PHONE.
           MOVE L-STATUS TO CI-CODE.
           READ CALL-INDEX
               INVALID KEY
                   MOVE 0 TO LIST-LENGTH
                   GOBACK.
 
           MOVE CI-LENGTH TO LIST-LENGTH.
           PERFORM VARYING W-POS FROM 1 BY 1 UNTIL W-POS > CI-LENGTH
               MOVE CI-POSITION(W-POS) TO POSITION(W-POS)
           END-PERFORM.
           SET STATUS-OK TO TRUE.
           GOBACK.