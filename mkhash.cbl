       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           MKHASH.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HASH-FILE ASSIGN TO
               "/cygdrive/c/class/cs336/TESTHASH.REL"
               ORGANIZATION IS RELATIVE
               ACCESS IS RANDOM
               RELATIVE KEY IS W-RECPOS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  HASH-FILE.
       01  HASH-REC.
           05  HF-ID       PIC X(7).
           05  HF-NAME     PIC X(20).
           05  HF-BALANCE  PIC 9(8)V99 COMP.
       
       WORKING-STORAGE SECTION.
       01  W-HEADER.
           05  W-TABLESIZE PIC 9(9) COMP.
           05  W-RECSIZE   PIC 9(9) COMP.
           05  W-SIGNATURE PIC X(24).
       01  W-RECPOS        PIC 9(9) COMP.
       01  TEST-DATA.
           05  PIC X(37) VALUE "0123456Hartness, Ken       0000050000".
           05  PIC X(37) VALUE "0123457Neudorf, Diane      0000025000".
           05  PIC X(37) VALUE "1234456Williams, Betty     0000030000".
           05  PIC X(37) VALUE "1234457Williams, Bob       0000010000".
           05  PIC X(37) VALUE "2345610Reed, Anne          0000020000".
           05  PIC X(37) VALUE "2345710Prouty, Carol       0000020000".
           05  PIC X(37) VALUE "3456789Arnold, Frank       0000010000".
           05  PIC X(37) VALUE "4567890Cockburn, Mary      0000001000".
           05  PIC X(37) VALUE "5839000Thompson, Pat       0000000500".
           05  PIC X(37) VALUE "6123000Holloway, Gail      0000002500".
           05  PIC X(37) VALUE "8349000Sundman, Dawn       0000010000".
           05  PIC X(37) VALUE "8349001Sundman, Robert     0000025000".
       01  TEST-TABLE REDEFINES TEST-DATA.
           05  W-CUST OCCURS 12 TIMES.
               10  W-ID        PIC X(7).
               10  W-NAME      PIC X(20).
               10  W-BALANCE   PIC 9(8)V99.
       01  W-POS PIC 99 COMP.
       01  W-START PIC 9(9) COMP.
       01  W-STATUS PIC 9.
           88  LOOKING VALUE 0.
           88  FOUND   VALUE 1.
           88  TABLE-FULL VALUE 2.
           88  EMPTY-REC VALUE 3.
       01  w-show-pos pic z9.
       
       PROCEDURE DIVISION.
       MAIN.
           PERFORM CREATE-TABLE.
           STOP RUN.
           
       CREATE-TABLE.
           OPEN OUTPUT HASH-FILE.
           MOVE 23 TO W-TABLESIZE.
           MOVE 32 TO W-RECSIZE.
           MOVE "CUSTHASH" TO W-SIGNATURE.
           MOVE 1 TO W-RECPOS.
           WRITE HASH-REC FROM W-HEADER.
           close hash-file.
