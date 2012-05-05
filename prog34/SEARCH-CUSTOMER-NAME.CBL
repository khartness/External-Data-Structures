       IDENTIFICATION DIVISION.
       PROGRAM-ID.  SEARCH-CUSTOMER-NAME.
       AUTHOR. Ken Hartness (translated from algorithm in "File Structures" by
           Michael J. Folk, Bill Zoellick, and Greg Riccardi, published
           by Addison-Wesley, 1998).
      *DESCRIPTION.
      *    Searching a B+ Tree of names.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NAME-INDEX-FILE ASSIGN TO W-FILENAME
               ORGANIZATION IS RELATIVE
               ACCESS IS RANDOM
               RELATIVE KEY IS W-BTREE-POS.

       DATA DIVISION.
       FILE SECTION.
       FD  NAME-INDEX-FILE EXTERNAL.
       01  NAMEINDEX-REC.
           05  IF-PREFIX   PIC 999 COMP.
           05  IF-DATA     PIC X(510).

       WORKING-STORAGE SECTION.
       01  W-BTREE-POS EXTERNAL    PIC 9(9) COMP.
       01  W-INDEX-STUFF EXTERNAL.
           05  W-NAME-INDEX-CHANGED PIC X VALUE 'N'.
           05  W-NAME-INDEX-HEADER.
               10  W-NAME-INDEX-SIZE   PIC 9(9) COMP.
               10  W-ROOT              PIC 9(9) COMP.
       01  W-BTREE-CACHE EXTERNAL.
           02  W-CACHE-ENTRY OCCURS 25 TIMES INDEXED BY X-POS.
               03  W-BTREE-RECORD  PIC 9(9) COMP VALUE 0.
               03  W-FREQUENCY     PIC 9(9) COMP VALUE 0.
               03  W-MODIFIED      PIC X VALUE 'N'.
               03  W-BTREE-REC.
                   05  W-LEAF      PIC X.
                   05  W-NUM-KEYS  PIC 999 COMP.
                   05  W-KEY       PIC X(30) OCCURS 76 TIMES.
                   05  W-LINK      PIC 9(9) COMP OCCURS 77 TIMES.
       01  W-BTREE-STACK EXTERNAL.
           05  W-TOP   PIC 9 COMP VALUE 0.
           05  W-POS   PIC 9(9) COMP OCCURS 7 TIMES.
       01  W-FOUND     PIC X VALUE 'N'.
           88  FOUND   VALUE 'Y'.
           88  NOT-FOUND VALUE 'N'.
       01  X           PIC 99 COMP.
       01  W-LFU       PIC 99 COMP.
       01  W-TEMP      PIC 9(9) COMP.
       
       LINKAGE SECTION.
       01  L-POSITION  PIC 9(9) COMP.
       01  L-NAME      PIC X(30).

       PROCEDURE DIVISION USING L-POSITION, L-NAME.
       BT-SEARCH.
           MOVE 0 TO W-TOP.
           PERFORM BT-FIND-LEAF.
           IF FOUND THEN
               MOVE 1 TO X
               PERFORM UNTIL X > W-NUM-KEYS(X-POS) OR
                             W-KEY(X-POS, X) >= L-NAME
                   ADD 1 TO X
               END-PERFORM
               IF X > W-NUM-KEYS(X-POS) or
                  w-key(X-POS, X) not = l-name THEN
                   MOVE 0 TO L-POSITION
               ELSE
                   MOVE W-LINK(X-POS, X) TO L-POSITION.

       BT-FIND-LEAF.
           SET NOT-FOUND TO TRUE.
           MOVE W-ROOT TO W-BTREE-POS.
           PERFORM UNTIL FOUND
               PERFORM READ-RECORD
               IF W-LEAF(X-POS) = 'Y' THEN
                   SET FOUND TO TRUE
               ELSE
                   ADD 1 TO W-TOP
                   MOVE W-BTREE-POS TO W-POS(W-TOP)
                   MOVE 1 TO X
                   PERFORM UNTIL X > W-NUM-KEYS(X-POS) OR
                                 L-NAME < W-KEY(X-POS, X)
                       ADD 1 TO X
                   END-PERFORM
                   MOVE W-LINK(X-POS, X) TO W-BTREE-POS
               END-IF
           END-PERFORM.

       READ-RECORD.
           MOVE 1 TO W-LFU.
           PERFORM VARYING X-POS FROM 1 BY 1
                 UNTIL X-POS > 25 OR W-BTREE-RECORD(X-POS) = W-BTREE-POS
               IF W-FREQUENCY(X-POS) < W-FREQUENCY(W-LFU) THEN
                   SET W-LFU TO X-POS
               END-IF
           END-PERFORM.
           IF X-POS > 25 THEN
               SET X-POS TO W-LFU
               IF W-MODIFIED(X-POS) = 'Y' THEN
                   MOVE W-BTREE-POS TO W-TEMP
                   MOVE W-BTREE-RECORD(X-POS) TO W-BTREE-POS
                   REWRITE NAMEINDEX-REC FROM W-BTREE-REC(X-POS)
                   MOVE W-TEMP TO W-BTREE-POS
               END-IF
               MOVE 0 TO W-FREQUENCY(X-POS)
               MOVE W-BTREE-POS TO W-BTREE-RECORD(X-POS)
               READ NAME-INDEX-FILE
               CALL 'UNPACK-DATA'
                   USING NAMEINDEX-REC, W-BTREE-REC(X-POS).
           ADD 1 TO W-FREQUENCY(X-POS).
