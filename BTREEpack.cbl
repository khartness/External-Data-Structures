       IDENTIFICATION DIVISION.
       PROGRAM-ID.  B-TREE-PACK.
       AUTHOR. Ken Hartness (translated from algorithm in "File Structures" by
           Michael J. Folk, Bill Zoellick, and Greg Riccardi, published
           by Addison-Wesley, 1998).
      *DESCRIPTION.
      *    Example of B-Tree.  Data file positions are 4-byte integers, so
      *    this implementation is limited to 436GB data files.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATA-FILE ASSIGN TO 'S:\COBOL\DATA.TXT'
               ORGANIZATION IS RELATIVE
               ACCESS IS RANDOM
               RELATIVE KEY IS DATA-POS.
           SELECT INDEX-FILE ASSIGN TO 'S:\COBOL\DATA.IDX'
               ORGANIZATION IS RELATIVE
               ACCESS IS RANDOM
               RELATIVE KEY IS INDEX-POS.

       DATA DIVISION.
       FILE SECTION.
       FD  DATA-FILE.
       01  DATA-REC.
           05  DF-ID   PIC 9(9).
           05  DF-NAME PIC X(20).
           05  DF-INFO PIC X(80).
       01  DF-HEADER.
           05  DF-SIZE PIC X(4) COMP-X.

       FD  INDEX-FILE.
       01  INDEX-REC.
           05  IF-PREFIX   PIC XX COMP-X.
           05  IF-DATA     PIC X(510).
       01  HEADER-REC.
           05  IF-SIZE     PIC X(4) COMP-X.
           05  IF-ROOT     PIC X(4) COMP-X.

       WORKING-STORAGE SECTION.
       01  DATA-POS    PIC X(4) COMP-X.
       01  INDEX-POS   PIC X(4) COMP-X.
       01  W-INDEX-STUFF.
           05  W-INDEX-CHANGED PIC X VALUE 'N'.
           05  W-INDEX-HEADER.
               10  W-FILE-SIZE PIC X(4) COMP-X.
               10  W-ROOT      PIC X(4) COMP-X.
       01  W-INDEX-REC.
           05  W-LEAF      PIC X.
           05  W-NUM-KEYS  PIC 999 COMP.
           05  W-KEY       PIC X(20) OCCURS 84 TIMES.
           05  W-LINK      PIC X(4) COMP-X OCCURS 85 TIMES.

      *Search-related
       01  W-FOUND     PIC X VALUE 'N'.
           88  NOT-FOUND   VALUE 'N'.
           88  FOUND       VALUE 'Y'.
       01  W-SEARCH-NAME   PIC X(20).
      *Insert-related
       01  W-STACK.
           05  W-PARENT    PIC X(4) COMP-X OCCURS 30 TIMES.
           05  W-TOP       PIC 99 COMP VALUE 0.
       01  W-MIDDLE.
           05  W-MID-NAME  PIC X(20).
           05  W-MID-POS   PIC X(4) COMP-X.
               88  NO-SPLIT    VALUE 0.
       01  W-NEW-REC.
           05  W-NEW-NAME  PIC X(20).
           05  W-NEW-POS   PIC X(4) COMP-X.
       01  SPLIT-REC.
           05  SR-LEAF     PIC X.
           05  SR-NUM-KEYS PIC 999 COMP.
           05  SR-KEY      PIC X(20) OCCURS 84 TIMES.
           05  SR-LINK     PIC X(4) COMP-X OCCURS 85 TIMES.
       01  W-POS           PIC X(4) COMP-X.
       01  W-STATUS        PIC 9 COMP.
       01  W-NEXT-LENGTH   PIC 999 COMP.
           88  RECORD-FULL VALUES 507 THROUGH 999.

       PROCEDURE DIVISION.
       B-TREE-HANDLING SECTION.
       BT-OPEN.
           OPEN I-O INDEX-FILE.
           MOVE 1 TO INDEX-POS.
           READ INDEX-FILE INTO W-INDEX-HEADER.

       BT-SEARCH.
           PERFORM BT-FIND-LEAF.
           IF FOUND THEN
               MOVE 1 TO W-POS
               PERFORM UNTIL W-POS > W-NUM-KEYS OR
                             W-KEY(W-POS) >= W-SEARCH-NAME
                   ADD 1 TO W-POS
               END-PERFORM
               IF W-POS > W-NUM-KEYS or w-key(w-pos) not = w-search-name THEN
                   SET NOT-FOUND TO TRUE
               ELSE
                   SET FOUND TO TRUE
                   MOVE W-LINK(W-POS) TO DATA-POS.

       BT-FIND-LEAF.
           SET NOT-FOUND TO TRUE.
           MOVE W-ROOT TO INDEX-POS.
           PERFORM UNTIL INDEX-POS = 0 OR FOUND
               READ INDEX-FILE
               CALL 'UNPACK-RECORD' USING INDEX-REC, W-INDEX-REC
               IF W-LEAF = 'Y' THEN
                   SET FOUND TO TRUE
               ELSE
                   ADD 1 TO W-TOP
                   MOVE INDEX-POS TO W-PARENT(W-TOP)
                   MOVE 1 TO W-POS
                   PERFORM UNTIL W-POS > W-NUM-KEYS OR
                                 W-SEARCH-NAME < W-KEY(W-POS)
                       ADD 1 TO W-POS
                   END-PERFORM
                   MOVE W-LINK(W-POS) TO INDEX-POS
               END-IF
           END-PERFORM.
           
       BT-FIND-NEXT-LEAF.
           IF W-LINK(W-NUM-KEYS + 1) = 0 THEN
               SET NOT-FOUND TO TRUE
           ELSE
               MOVE W-LINK(W-NUM-KEYS + 1) TO INDEX-POS
               READ INDEX-FILE
               CALL 'UNPACK-RECORD' USING INDEX-REC, W-INDEX-REC
               SET FOUND TO TRUE.

       BT-INSERT.
           PERFORM BT-FIND-LEAF.
           PERFORM BT-INSERT-NODE.
           PERFORM UNTIL NO-SPLIT OR W-TOP = 0
               MOVE W-PARENT(W-TOP) TO INDEX-POS
               SUBTRACT 1 FROM W-TOP
               READ INDEX-FILE
               CALL 'UNPACK-RECORD' USING INDEX-REC, W-INDEX-REC
               MOVE W-MIDDLE TO W-NEW-REC
               PERFORM BT-INSERT-NODE
           END-PERFORM.
           IF NOT NO-SPLIT THEN
               ADD 1 TO W-FILE-SIZE
      *        MOVE 'Y' TO W-INDEX-CHANGED
               MOVE 'N' TO W-LEAF
               MOVE 1 TO W-NUM-KEYS
               MOVE W-ROOT TO W-LINK(1)
               MOVE W-MIDDLE-POS TO W-LINK(2)
               MOVE W-MID-NAME TO W-KEY(1)
               CALL 'PACK-RECORD' USING W-STATUS, INDEX-REC, W-INDEX-REC
               MOVE W-FILE-SIZE TO INDEX-POS, W-ROOT
               WRITE INDEX-REC
               MOVE 1 TO INDEX-POS
               REWRITE HEADER-REC FROM W-INDEX-HEADER.

       BT-INSERT-NODE.
           PERFORM CHECK-FOR-INSERT.
           IF RECORD-FULL THEN
               PERFORM SPLIT-NODE
               IF W-NEW-NAME >= W-MID-NAME THEN
                   CALL 'PACK-RECORD' USING W-STATUS, INDEX-REC, W-INDEX-REC
                   REWRITE INDEX-REC
                   MOVE W-MID-POS TO INDEX-POS
                   MOVE SPLIT-REC TO W-INDEX-REC
               ELSE
                   MOVE INDEX-POS TO W-POS
                   MOVE W-MID-POS TO INDEX-POS
                   CALL 'PACK-RECORD' USING W-STATUS, INDEX-REC, SPLIT-REC
                   WRITE INDEX-REC
                   MOVE W-POS TO INDEX-POS
           ELSE
               SET NO-SPLIT TO TRUE.
           ADD 1 TO W-NUM-KEYS.
           MOVE W-LINK(W-NUM-KEYS) TO W-LINK(W-NUM-KEYS + 1).
           PERFORM VARYING W-POS FROM W-NUM-KEYS BY -1
                   UNTIL W-POS = 1 OR W-NEW-NAME > W-KEY(W-POS - 1)
               MOVE W-KEY(W-POS - 1) TO W-KEY(W-POS)
               MOVE W-LINK(W-POS - 1) TO W-LINK(W-POS)
           END-PERFORM.
           MOVE W-NEW-NAME TO W-KEY(W-POS).
           IF W-LEAF = 'Y' THEN
               MOVE W-NEW-POS TO W-LINK(W-POS)
           ELSE
               MOVE W-NEW-POS TO W-LINK(W-POS + 1).
           CALL 'PACK-RECORD' USING W-STATUS, INDEX-REC, W-INDEX-REC
           REWRITE INDEX-REC
               INVALID KEY
                   WRITE INDEX-REC
                       INVALID KEY
                           DISPLAY "Problem writing file!".

       SPLIT-NODE.
           PERFORM GET-MIDDLE-KEY.
      *    MOVE W-KEY(W-POS) TO W-MID-NAME.
           CALL 'MINIMAL-SEPARATOR' USING W-KEY(W-POS - 1), W-KEY(W-POS),
                                          W-MID-NAME.
           ADD 1 TO W-FILE-SIZE.
           MOVE W-FILE-SIZE TO W-MID-POS.
           MOVE 'Y' TO W-INDEX-CHANGED.
           MOVE 0 TO SR-NUM-KEYS.
           MOVE W-LEAF TO SR-LEAF.
           SUBTRACT 1 FROM W-POS GIVING W-NEXT-LENGTH.
           IF W-LEAF = 'N' THEN
               ADD 1 TO W-POS.
           PERFORM UNTIL W-POS > W-NUM-KEYS
               ADD 1 TO SR-NUM-KEYS
               MOVE W-KEY(W-POS) TO SR-KEY(SR-NUM-KEYS)
               MOVE W-LINK(W-POS) TO SR-LINK(SR-NUM-KEYS)
               ADD 1 TO W-POS
           END-PERFORM.
           MOVE W-LINK(W-POS) TO SR-LINK(SR-NUM-KEYS + 1).
           MOVE W-NEXT-LENGTH TO W-NUM-KEYS.
           IF W-LEAF = 'Y' THEN
               MOVE W-FILE-SIZE TO W-LINK(W-POS).

       CHECK-FOR-INSERT.
           MOVE 20 TO W-POS.
           PERFORM UNTIL W-POS = 0 OR W-NEW-NAME(W-POS:1) NOT = ' '
               SUBTRACT 1 FROM W-POS
           END-PERFORM.
           IF IF-PREFIX >= 32768 THEN
               ADD W-POS, -32768 TO IF-PREFIX GIVING W-NEXT-LENGTH
           ELSE
               ADD W-POS TO IF-PREFIX GIVING W-NEXT-LENGTH.
       
       GET-MIDDLE-KEY.
      * this needs to be rewritten to calculate middle in terms of string sizes
      * and to determine the minimal separator.
           DIVIDE W-NUM-KEYS BY 2 GIVING W-POS.
           ADD 1 TO W-POS.
