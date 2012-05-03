       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           OPEN-CALL-INDEX.
      *AUTHOR.
      *    Ken Hartness. Created 2012/05/01.
      *DESCRIPTION.
      *    Open call index file and prepare for use.
      
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
           
           SELECT SEQ-CALL-INDEX ASSIGN TO W-FILENAME
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS SEQ-CALL-KEY
           STATUS IS W-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  CALL-INDEX EXTERNAL.
       01  CALL-INDEX-REC.
           05  CALL-KEY.
               10  CI-PHONE    PIC 9(10).
               10  CI-CODE     PIC 9.
           05  CI-LENGTH       PIC 99 COMP.
           05  CI-POSITION     PIC 9(9) COMP OCCURS 124 TIMES.

       FD  SEQ-CALL-INDEX.
       01  SEQ-REC.
           05  SEQ-CALL-KEY.
               10  SCI-PHONE   PIC 9(10).
               10  SCI-CODE    PIC 9.
           05  SCI-LENGTH      PIC 99 COMP.
           05  SCI-POSITION    PIC 9(9) COMP OCCURS 124 TIMES.

       WORKING-STORAGE SECTION.
       01  W-FILENAME EXTERNAL PIC X(256).
       01  W-STATUS            PIC XX.

       LINKAGE SECTION.
       01  L-FILENAME          PIC X(256).
      
       PROCEDURE DIVISION USING L-FILENAME.
       PREPARE-FILE.
           MOVE L-FILENAME TO W-FILENAME.
           OPEN INPUT SEQ-CALL-INDEX.
           IF W-STATUS NOT = '00' THEN
               CLOSE SEQ-CALL-INDEX
               OPEN OUTPUT SEQ-CALL-INDEX
               IF W-STATUS = '00' THEN
                   MOVE 0 TO SCI-PHONE, SCI-CODE, SCI-LENGTH
                   WRITE SEQ-REC
                   IF W-STATUS = '00' THEN
                       DELETE SEQ-CALL-INDEX
                   ELSE
                       DISPLAY "Unable to write to call index file: ",
                               L-FILENAME
                   END-IF
                   CLOSE SEQ-CALL-INDEX
               ELSE
                   DISPLAY "Unable to open ", L-FILENAME.
           OPEN I-O CALL-INDEX.
           GOBACK.
