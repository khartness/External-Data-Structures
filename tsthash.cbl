       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           TSTHASH.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL HASH-FILE ASSIGN TO
               "/cygdrive/c/class/cs336/hashfile.rel"
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
           PERFORM ECHO-TABLE.
           STOP RUN.
           
       CREATE-TABLE.
           display "create-table".
           open i-o hash-file.
           move 24 to w-recpos.
           display "get header".
           read hash-file into w-header
               invalid key
                   move 23 to w-tablesize
                   move 32 to w-recsize
                   move "CUSTHASH" to w-signature
                   write hash-rec from w-header
                       invalid key
                           display "Unable to save header!".
           PERFORM ADD-ENTRY VARYING W-POS FROM 1 BY 1 UNTIL W-POS > 12.
           close hash-file.
       
       ADD-ENTRY.
           CALL 'hash-code' USING W-RECPOS, W-ID(W-POS), W-TABLESIZE.
           ADD 1 TO W-RECPOS.
           perform report-looking.
           READ HASH-FILE
               INVALID KEY
                   SET EMPTY-REC TO TRUE
               NOT INVALID KEY
                   SET LOOKING TO TRUE.
           MOVE W-RECPOS TO W-START.
           PERFORM UNTIL NOT LOOKING OR W-ID(W-POS) = HF-ID
               COMPUTE W-RECPOS =
                           FUNCTION MOD(W-RECPOS - 1, W-TABLESIZE) + 2
               IF W-RECPOS = W-START THEN
                   SET TABLE-FULL TO TRUE
               ELSE
                   READ HASH-FILE
                       INVALID KEY
                           SET EMPTY-REC TO TRUE
                   END-READ
                   perform report-looking
               END-IF
           END-PERFORM.
           IF EMPTY-REC THEN
               MOVE W-ID(W-POS) TO HF-ID
               MOVE W-NAME(W-POS) TO HF-NAME
               MOVE W-BALANCE(W-POS) TO HF-BALANCE
               WRITE HASH-REC
                   invalid key
                       display "Unable to save record!"
               end-write
               display "    ", w-id(w-pos), " saved!"
           ELSE IF TABLE-FULL THEN
               DISPLAY "TABLE IS FULL!"
           ELSE
               DISPLAY "DUPLICATE ID!".
               
       ECHO-TABLE.
           DISPLAY " ".
           DISPLAY "HASH FILE".
           OPEN I-O HASH-FILE.
           PERFORM VARYING W-RECPOS FROM 2 BY 1
               UNTIL W-RECPOS > W-TABLESIZE
               READ HASH-FILE
                   INVALID KEY
                       DISPLAY "       <EMPTY RECORD>"
                   NOT INVALID KEY
                       DISPLAY HF-ID, " ", HF-NAME
               END-READ
           END-PERFORM.
           CLOSE HASH-FILE.

       REPORT-LOOKING.
           MOVE W-RECPOS TO W-SHOW-POS.
           DISPLAY W-SHOW-POS, " read".
