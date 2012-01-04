       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           HASH-OFFSET.
      *    Calculates an offset within a table between 1 and
      *    table size - 1. To help randomize things, the digits
      *    are transformed into a mix of numbers and treated like
      *    a radix larger than ten. I started to use all prime numbers,
      *    but it occurred to me that an even number of odd numbers is
      *    an odd number, even after multiplying it by another odd
      *    number, so I decided to mix in some other types of numbers,
      *    as well.
      *    2, 3, 6, 7, 14, 19, 28, 31, 34, 37 with radix 41.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  W-PRIME-DATA.
           05  PIC 99 COMP VALUE 37.
           05  PIC 99 COMP VALUE 2.
           05  PIC 99 COMP VALUE 19.
           05  PIC 99 COMP VALUE 6.
           05  PIC 99 COMP VALUE 31.
           05  PIC 99 COMP VALUE 14.
           05  PIC 99 COMP VALUE 3.
           05  PIC 99 COMP VALUE 28.
           05  PIC 99 COMP VALUE 7.
           05  PIC 99 COMP VALUE 34.
       01  W-PRIME-TABLE REDEFINES W-PRIME-DATA.
           05  W-NUM PIC 99 COMP OCCURS 10 TIMES.
       01  W-DIGIT PIC 9.
       01  W-KEY   PIC 9(4).
       
       LINKAGE SECTION.
       01  L-POS PIC 9(9) COMP.
       01  L-KEY PIC 9(4).
       01  L-TABLESIZE PIC 9(9) COMP.
       
       PROCEDURE DIVISION USING L-POS, L-KEY, L-TABLESIZE.
       ENTRY-POINT.
           MOVE L-KEY TO W-KEY.
           MOVE 0 TO L-POS.
           PERFORM 4 TIMES
               DIVIDE W-KEY BY 10 GIVING W-KEY REMAINDER W-DIGIT
               COMPUTE L-POS = L-POS * 41 + W-NUM(W-DIGIT)
           END-PERFORM.
           COMPUTE L-POS = FUNCTION MOD(L-POS, L-TABLESIZE - 1) + 1.
           GOBACK.
