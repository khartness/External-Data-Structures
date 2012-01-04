       IDENTIFICATION DIVISION.
       PROGRAM-ID.  TESTDIRECT.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATA-FILE
           ASSIGN TO "/cygdrive/e/class/cs336/example.rel"
           ORGANIZATION IS RELATIVE
           ACCESS IS RANDOM
           RELATIVE KEY IS W-RECPOS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  DATA-FILE.
       01  DATA-REC.
           05  DF-TEXT PIC X(20).
           05  DF-NUM  PIC 99 COMP.
       
       WORKING-STORAGE SECTION.
       01  W-RECPOS PIC 9(9) COMP.
       01  W-NUM PIC 99.
       
       PROCEDURE DIVISION.
       OPEN OUTPUT DATA-FILE.
       MOVE 1 TO W-RECPOS.
       PERFORM UNTIL W-RECPOS > 7
           DISPLAY "Enter text: " WITH NO ADVANCING
           ACCEPT DF-TEXT
           DISPLAY "Enter 2-digit number: " WITH NO ADVANCING
           ACCEPT W-NUM
           MOVE W-NUM TO DF-NUM
           WRITE DATA-REC
           ADD 2 TO W-RECPOS
       END-PERFORM.
       CLOSE DATA-FILE.
       
       OPEN I-O DATA-FILE.
       MOVE 5 TO W-RECPOS.
       READ DATA-FILE, INVALID KEY DISPLAY "Record 5 does not exist!".
       DELETE DATA-FILE RECORD.
       CLOSE DATA-FILE.
       STOP RUN.
