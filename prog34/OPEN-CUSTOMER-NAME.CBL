       IDENTIFICATION DIVISION.
       PROGRAM-ID.  OPEN-CUSTOMER-NAME.
       AUTHOR. Ken Hartness.
      *DESCRIPTION.
      *    Opening a B+ Tree of names.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NAME-INDEX-FILE ASSIGN TO W-FILENAME
               ORGANIZATION IS RELATIVE
               ACCESS IS RANDOM
               RELATIVE KEY IS W-BTREE-POS.
           select seq-name-index assign to w-filename
               organization is relative
               access is random
               relative key is w-btree-pos
               status is w-status.

       DATA DIVISION.
       FILE SECTION.
       FD  NAME-INDEX-FILE EXTERNAL.
       01  NAMEINDEX-REC.
           05  IF-PREFIX   PIC 999 COMP.
           05  IF-DATA     PIC X(510).

       FD  SEQ-NAME-INDEX.
       01  SEQ-NAMEINDEX-REC.
           05  SF-PREFIX   PIC 999 COMP.
           05  SF-DATA     PIC X(510).
           
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
       01  w-filename external pic x(512).
       01  W-STATUS PIC XX.
      
       LINKAGE SECTION.
       01  L-filename pic x(512).

       PROCEDURE DIVISION USING L-FILENAME.
       ENTRY-POINT.
           MOVE 'N' TO W-NAME-INDEX-CHANGED.
           INITIALIZE W-BTREE-CACHE.
           OPEN INPUT SEQ-NAME-INDEX.
           if w-status not = '00' then
               close seq-name-index
               open output seq-name-index
               move 2 to w-name-index-size
               move 2 to w-root
               move 1 to w-btree-pos
               write seq-nameindex-rec from w-name-index-header
               move 512 to sf-prefix
               move 2 to w-btree-pos
               write seq-nameindex-rec
               close seq-name-index.
           open i-o name-index-file.
           move 1 to w-btree-pos.
           read name-index-file into w-name-index-header.
           goback.

