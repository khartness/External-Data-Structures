       IDENTIFICATION DIVISION.
       PROGRAM-ID.  ADD-CUSTOMER-NAME.
       AUTHOR. Ken Hartness (translated from algorithm in "File Structures" by
           Michael J. Folk, Bill Zoellick, and Greg Riccardi, published
           by Addison-Wesley, 1998).
      *DESCRIPTION.
      *    Adding to a B+ Tree of names.

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
       01  Y           PIC 99 COMP.
       01  W-TRY-POS   PIC 9(9) COMP.
       01  W-MIDDLE.
           05  W-MID-NAME  PIC X(30).
           05  W-MID-POS   PIC 9(9) COMP.
               88  NO-SPLIT    VALUE 0.
       01  W-NEW-REC.
           05  W-NEW-NAME  PIC X(30).
           05  W-NEW-POS   PIC 9(9) COMP.
       01  SPLIT-REC.
           05  SR-LEAF     PIC X.
           05  SR-NUM-KEYS PIC 999 COMP.
           05  SR-KEY      PIC X(30) OCCURS 76 TIMES.
           05  SR-LINK     PIC 9(9) COMP OCCURS 77 TIMES.
      * The number 76 is derived from the following assumptions:
      *     512-byte block (minus leaf/length byte)
      *     26 one-letter separators
      *     add 5 bytes (one for string length, 4 for link) per separator
      *     (511 - 4 - 26*6) / (2 + 5) = 50 two-letter separators
      *     No reason not to make it bigger, say up to 562 keys for a 4K block.
       01  W-STATUS        PIC 9 COMP.
           88  RECORD-FULL VALUE 1.
       01  W-SIZE          PIC 999 COMP.
      
       LINKAGE SECTION.
       01  L-STATUS    PIC 9.
           88  STATUS-OK VALUE 0.
           88  STATUS-DUPLICATE-KEY VALUE 1.
           88  STATUS-OTHER VALUE 2.
       01  L-NAME      PIC X(30).
       01  L-POSITION  PIC 9(9) COMP.

       PROCEDURE DIVISION USING L-STATUS, L-NAME, L-POSITION.
       BT-ADD-NEW.
           CALL 'SEARCH-CUSTOMER-NAME' USING W-TRY-POS, L-NAME
           IF W-TRY-POS NOT = 0 THEN
               SET STATUS-DUPLICATE-KEY TO TRUE
               GOBACK.
           MOVE L-NAME TO W-NEW-NAME.
           MOVE L-POSITION TO W-NEW-POS.
           PERFORM BT-INSERT-NODE.
           PERFORM UNTIL NO-SPLIT OR W-TOP = 0
               MOVE W-POS(W-TOP) TO W-BTREE-POS
               SUBTRACT 1 FROM W-TOP
               PERFORM READ-RECORD
               MOVE W-MIDDLE TO W-NEW-REC
               PERFORM BT-INSERT-NODE
           END-PERFORM.
           IF NOT NO-SPLIT THEN
      *        make a new root
               ADD 1 TO W-NAME-INDEX-SIZE
               MOVE 'N' TO SR-LEAF
               MOVE 1 TO SR-NUM-KEYS
               MOVE W-ROOT TO SR-LINK(1)
               MOVE W-MIDDLE-POS TO SR-LINK(2)
               MOVE W-MID-NAME TO SR-KEY(1)
               CALL 'PACK-RECORD' USING W-STATUS, NAMEINDEX-REC, SPLIT-REC
               MOVE W-NAME-INDEX-SIZE TO W-BTREE-POS, W-ROOT
               WRITE INDEX-REC
                   INVALID KEY
                       SET STATUS-OTHER TO TRUE
                   NOT INVALID KEY
                       MOVE 1 TO W-BTREE-POS
                       REWRITE NAMEINDEX-REC FROM W-NAME-INDEX-HEADER.

       BT-INSERT-NODE.
      *    With compression, easier to add key, attempt compress, then
      *    split if won't fit. Need to adjust a few things here in rare
      *    (once) case where exactly 76 keys, but for now I'll just give
      *    up if that happens. Easiest fix is to make it 77, but I'm in
      *    a hurry.
           IF W-NUM-KEYS(X-POS) = 76 THEN
               set status-other to true
               goback.
           
           ADD 1 TO W-NUM-KEYS(X-POS).
           MOVE W-LINK(W-NUM-KEYS(X-POS)) TO W-LINK(W-NUM-KEYS(X-POS) + 1).
           PERFORM VARYING X FROM W-NUM-KEYS(X-POS) BY -1
                   UNTIL X = 1 OR W-NEW-NAME > W-KEY(X - 1) OF W-BTREE-REC(X-POS)
               MOVE W-KEY(X - 1) OF W-BTREE-REC(X-POS) TO W-KEY(X) OF W-BTREE-REC(X-POS)
               MOVE W-LINK(X - 1) OF W-BTREE-REC(X-POS) TO W-LINK(X) OF W-BTREE-REC(X-POS)
           END-PERFORM.
           MOVE W-NEW-NAME TO W-KEY(X) OF W-BTREE-REC(X-POS).
           IF W-LEAF(X-POS) = 'Y' THEN
               MOVE W-NEW-POS TO W-LINK(X) OF W-BTREE-REC(X-POS)
           ELSE
               MOVE W-NEW-POS TO W-LINK(X + 1) OF W-BTREE-REC(X-POS).
               
           CALL 'PACK-RECORD' USING W-STATUS, NAMEINDEX-REC, W-BTREE-REC(X-POS).
           IF RECORD-FULL THEN
               PERFORM SPLIT-NODE
               CALL 'PACK-RECORD' USING W-STATUS, NAMEINDEX-REC, W-BTREE-REC(X-POS)
               PERFORM REWRITE-RECORD
               MOVE W-MID-POS TO W-BTREE-POS
               CALL 'PACK-RECORD' USING W-STATUS, NAMEINDEX-REC, SPLIT-REC
               WRITE NAMEINDEX-REC
                   INVALID KEY
                       DISPLAY "Unable to write index!"
                       SET STATUS-OTHER TO TRUE
                       goback
               END-WRITE
           ELSE
               PERFORM REWRITE-RECORD
               SET NO-SPLIT TO TRUE.               

       SPLIT-NODE.
           PERFORM GET-MIDDLE-KEY.
           IF W-LEAF(X-POS) = 'Y' THEN
               CALL 'MINIMAL-SEPARATOR' USING W-KEY(X - 1), W-KEY(X),
                                              W-MID-NAME
           ELSE
               MOVE W-KEY(X) TO W-MID-NAME.
               
           ADD 1 TO W-NAME-INDEX-SIZE.
           MOVE W-NAME-INDEX-SIZE TO W-MID-POS.
           MOVE 0 TO SR-NUM-KEYS.
           MOVE W-LEAF TO SR-LEAF.
           SUBTRACT 1 FROM X GIVING W-NEXT-LENGTH.
           IF W-LEAF(X-POS) = 'N' THEN
               ADD 1 TO X.
           PERFORM UNTIL X > W-NUM-KEYS(X-POS)
               ADD 1 TO SR-NUM-KEYS
               MOVE W-KEY(X-POS, X) TO SR-KEY(SR-NUM-KEYS)
               MOVE W-LINK(X-POS, X) TO SR-LINK(SR-NUM-KEYS)
               ADD 1 TO X
           END-PERFORM.
           MOVE W-LINK(X-POS, X) TO SR-LINK(SR-NUM-KEYS + 1).
           MOVE W-NEXT-LENGTH TO W-NUM-KEYS(X-POS).
           IF W-LEAF(X-POS) = 'Y' THEN
               MOVE W-NAME-INDEX-SIZE TO W-LINK(W-NEXT-LENGTH + 1)
                                         OF W-BTREE-REC(X-POS).
       GET-MIDDLE-KEY.
      *    Each key is up to 30 characters, plus a link and a string length.
      *    This adds 35, then reduces the number until the end of the string
      *    is found. When the total lengths exceeds half of the record, the
      *    loop stops with the position of the "middle" key.
           MOVE 0 TO W-SIZE.
           PERFORM VARYING X FROM 1 BY 1
                   UNTIL X > W-NUM-KEYS(X-POS) OR W-SIZE > 253
               ADD 35 TO W-SIZE
               PERFORM VARYING Y FROM 30 BY -1
                       UNTIL Y = 0 OR W-KEY(X-POS, X)(Y:1) NOT = ' '
                   SUBTRACT 1 FROM W-SIZE
               END-PERFORM
           END-PERFORM.
           SUBTRACT 1 FROM X.
           
       REWRITE-RECORD.
           ADD 1 TO W-FREQUENCY(X-POS).
           CALL 'PACK-RECORD'
               USING W-STATUS, NAME-INDEX-REC, W-BTREE-REC(X-POS).
           IF W-STATUS = 0 THEN
               REWRITE NAME-INDEX-REC.

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
               CALL 'UNPACK-DATA' USING NAMEINDEX-REC, W-BTREE-REC(X-POS).
           ADD 1 TO W-FREQUENCY(X-POS).
