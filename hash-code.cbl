       identification division.
       program-id.  hash-code.
       data division.
       working-storage section.
       01  w-pos pic 9 comp.
       linkage section.
       01  l-pos pic 9(9) comp.
       01  l-key pic x(7).
       01  l-tablesize pic 9(9) comp.
       
       procedure division using l-pos, l-key, l-tablesize.
       entry-point.
           move 0 to l-pos.
           perform varying w-pos from 1 by 1 until w-pos > 7
               compute l-pos =
                   function mod(127 * l-pos +
                                    function ord(l-key(w-pos:1)),
                                l-tablesize)
           end-perform.
           goback.
