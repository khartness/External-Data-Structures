       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PACK-RECORD.
       AUTHOR.      Ken Hartness.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-POS           PIC 999 COMP.
       01  W-REC-LEN       PIC 999 COMP.
       01  W-KEY-FIELD.
           05  W-LINK      PIC 9(9) COMP.
           05  W-KEY-LEN   PIC 99 COMP.
           05  W-KEY       PIC X(30).
       01  W-REMAINING     PIC 999 COMP.
           
       LINKAGE SECTION.
       01  L-STATUS        PIC 9 COMP.
       01  L-RECORD.
           05  L-PREFIX    PIC 999 COMP.
           05  L-DATA      PIC X(510).
       01  L-TABLE.
           05  L-LEAF      PIC X.
           05  L-LENGTH    PIC 999 COMP.
           05  L-KEY       PIC X(30) OCCURS 76 TIMES.
           05  L-LINK      PIC 9(9) COMP OCCURS 77 TIMES.
           
       PROCEDURE DIVISION USING L-STATUS, L-TABLE, L-RECORD.
       PACKING.
           MOVE 1 TO W-POS.
           MOVE 1 TO W-REC-LEN.
           PERFORM COMPACT-KEY
               UNTIL W-POS > L-LENGTH OR W-REC-LEN > 506.
           IF W-REC-LEN > 506 THEN
               MOVE 1 TO L-STATUS
           ELSE
               MOVE 0 TO L-STATUS
               MOVE L-LINK(L-LENGTH + 1) TO W-LINK
               MOVE W-KEY-FIELD TO L-DATA(W-REC-LEN:4)
               ADD 3 TO W-REC-LEN.
           PERFORM CREATE-PREFIX.
           GOBACK.
          
       CREATE-PREFIX.
           IF L-LEAF = 'Y' THEN
               ADD 512 TO W-REC-LEN GIVING L-PREFIX
           ELSE
               MOVE W-REC-LEN TO L-PREFIX.
               
       COMPACT-KEY.
           MOVE L-LINK(W-POS) TO W-LINK.
           MOVE L-KEY(W-POS) TO W-KEY.
           MOVE 30 TO W-KEY-LEN.
           PERFORM UNTIL W-KEY-LEN = 0 OR W-KEY(W-KEY-LEN:1) NOT = ' '
               SUBTRACT 1 FROM W-KEY-LEN
           END-PERFORM.
           add 5, w-key-len giving w-remaining.
           IF W-REC-LEN + W-REMAINING > 506 THEN
               SUBTRACT W-REC-LEN FROM 510 GIVING W-REMAINING.
           MOVE W-KEY-FIELD TO L-DATA(W-REC-LEN:w-remaining).
           ADD 5,W-KEY-LEN TO W-REC-LEN.
           ADD 1 TO W-POS.
