       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           HASH-PHONE.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-PHONE                 PIC 9(10).
       01  W-PIECE                 PIC 999.
       
       LINKAGE SECTION.
       01  L-HASH-KEY  PIC 9(9) COMP.
       01  L-KEY       PIC 9(10).
       
       PROCEDURE DIVISION USING L-HASH-KEY, L-KEY.
       COMPUTE-HASH-CODE.
      * On assumption that last four digits will often be unique, at
      * least for "local" numbers, I am including the bit pattern of
      * those digits into the hash key.
           MOVE 87383 TO L-HASH-KEY.
           MOVE L-KEY TO W-PHONE.
           MOVE 1000 TO W-DIGIT-POS.
           PERFORM UNTIL W-PHONE = 0
               DIVIDE W-PHONE BY 353 GIVING W-PHONE REMAINDER W-PIECE
               COMPUTE L-HASH-KEY =
                   FUNCTION MOD(L-HASH-KEY * 257 + W-PIECE, 429497)
           END-PERFORM.
           COMPUTE L-HASH-KEY =
               L-HASH-KEY + FUNCTION MOD(L-KEY, 10000) * 429497.
           GOBACK.
