; This is the case in the original assignment.  I gotta say:  If the darker
; line in the handout was supposed to be the shortest path, it's wrong...
; This *will* test one thing:  I'm using entirely integers in the input,
; so I'll get single-floats except for the constants used in intersect.lsp.

(setf *polygons* (list
                    (list
                       (list 0 4)
                       (list 0 0)
                       (list 4 0)
                       (list 4 4)
                       (list 0 4)
                       (list 0 0))
                    (list
                       (list 4 10)
                       (list 2 8)
                       (list 7 4)
                       (list 9 6)
                       (list 4 10)
                       (list 2 8))))

; Start

(setf *start* (list 0 0))

; Goal

(setf *goal* (list 9 6))
