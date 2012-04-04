       identification division.
       program-id.  minimal-separator.

       data division.
       working-storage section.
       01  w-pos   pic 99 comp.
       
       linkage section.
       01  l-key1  pic x(20).
       01  l-key2  pic x(20).
       01  l-sep   pic x(20).

       procedure division using l-key1, l-key2, l-sep.
       find-min-sep.
           move 1 to w-pos.
           perform until l-key1(w-pos:1) not = l-key2(w-pos:1) or
                         w-pos = 20
               add 1 to w-pos
           end-perform.
           move l-key2(1:w-pos) to l-sep.
           goback.
