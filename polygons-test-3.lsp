; An even less trivial single-polygon case:  Turn the bowtie inside out by
; listing the points clockwise.  This makes it a perimiter -- the plane beyond
; it is off limits, and paths have to stay in the interior.  Leave the start
; and goal where they are, so we can see it go straight across.

(setf *polygons* (list
                    (list
                       (list 0.5 0.5)
                       (list 1.0 0.0)
                       (list 0.0 -1.0)
                       (list -0.5 -0.5)
                       (list -1.0 0.0)
                       (list 0.0 1.0)
                       (list 0.5 0.5)
                       (list 1.0 0.0))))

; Start

(setf *start* (list -0.5 -0.5))

; Goal

(setf *goal* (list 0.5 0.5))
