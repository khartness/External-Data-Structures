       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           ADD-PHONE.
      *AUTHOR.
      *    Ken Hartness. Created 2012/05/01.
      *DESCRIPTION.
      *    Add a phone number and its call record position to the inverted index.
      
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
           05  CI-POSITION     PIC 9(9) COMP OCCURS 125 TIMES.
           
       LINKAGE SECTION.
       01  L-STATUS    PIC 9.
           88  STATUS-OK VALUE 0.
           88  CALLER-PHONE VALUE 1.
           88  CALLEE-PHONE VALUE 2.
       01  L-PHONE     PIC 9(10).
       01  L-POSITION  PIC 9(9) COMP.
       
       PROCEDURE DIVISION USING L-STATUS, L-PHONE, L-POSITION.
       ADD-TO-EXISTING-RECORD.
           MOVE L-PHONE TO CI-PHONE.
           MOVE L-STATUS TO CI-CODE.
           READ CALL-INDEX
               INVALID KEY
                   PERFORM CREATE-NEW-RECORD
                   GOBACK.
           IF CI-LENGTH = 125 THEN
      *        Figure out alternative, say odd codes for caller overflow
      *        and even codes for callee overflow. For now, assuming never
      *        more than 125 duplicates (not reasonable for large file).
               GOBACK.
           
           ADD 1 TO CI-LENGTH.
           MOVE L-POSITION TO CI-POSITION(CI-LENGTH).
           REWRITE CALL-INDEX-REC
               INVALID KEY
                   DISPLAY "Problem saving changes!"
               NOT INVALID KEY
                   SET STATUS-OK TO TRUE.
           GOBACK.
       
       CREATE-NEW-RECORD.
           MOVE 1 TO CI-LENGTH.
           MOVE L-POSITION TO CI-POSITION(CI-LENGTH).
           WRITE CALL-INDEX-REC
               INVALID KEY
                   DISPLAY "Problem creating new record!"
               NOT INVALID KEY
                   SET STATUS-OK TO TRUE.
