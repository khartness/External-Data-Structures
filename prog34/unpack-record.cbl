       IDENTIFICATION DIVISION.
       PROGRAM-ID.  UNPACK-RECORD.
       AUTHOR.      Ken Hartness.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-POS           PIC 999 COMP.
       01  W-REC-LEN       PIC 999 COMP.
       01  W-KEY-FIELD.
           05  W-LINK      PIC 9(9) COMP.
           05  W-KEY-LEN   PIC 99 COMP.
           05  W-KEY       PIC X(30).
           
       LINKAGE SECTION.
       01  L-RECORD.
           05  L-PREFIX    PIC 999 COMP.
           05  L-DATA      PIC X(510).
       01  L-TABLE.
           05  L-LEAF      PIC X.
           05  L-LENGTH    PIC 999 COMP.
           05  L-KEY       PIC X(30) OCCURS 76 TIMES.
           05  L-LINK      PIC 9(9) COMP OCCURS 77 TIMES.
           
       PROCEDURE DIVISION USING L-RECORD, L-TABLE.
       UNPACKING.
           MOVE 1 TO W-POS.
           PERFORM PROCESS-PREFIX.
           MOVE 0 TO L-LENGTH.
           PERFORM EXTRACT-KEY
               UNTIL W-POS > W-REC-LEN - 4.
           MOVE L-DATA(W-POS:) TO W-KEY-FIELD.
           MOVE W-LINK TO L-LINK(L-LENGTH + 1).
           GOBACK.
           
       PROCESS-PREFIX.
           IF L-PREFIX >= 512 THEN
               MOVE 'Y' TO L-LEAF
               SUBTRACT 512 FROM L-PREFIX GIVING W-REC-LEN
           ELSE
               MOVE 'N' TO L-LEAF
               MOVE L-PREFIX TO W-REC-LEN.
               
       EXTRACT-KEY.
           if w-pos > 511 - 35 then
               subtract w-pos from 511 giving w-remaining
           else
               move 35 to w-remaining.
           MOVE L-DATA(W-POS:w-remaining) TO W-KEY-FIELD.
           ADD 1 TO L-LENGTH.
           MOVE W-LINK TO L-LINK(L-LENGTH).
           MOVE W-KEY(1:W-KEY-LEN) TO L-KEY(L-LENGTH).
           ADD 5, W-KEY-LEN TO W-POS.
