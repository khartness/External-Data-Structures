       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           CLOSE-CALL-INDEX.
      *AUTHOR.
      *    Ken Hartness. Created 2012/05/01.
      *DESCRIPTION.
      *    Close call index file.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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

       PROCEDURE DIVISION.
       entry-point.
           close call-index.
           goback.
