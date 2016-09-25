; Give the search an obstacle inside the bowtie perimiter -- add a box enclosing
; a bit of the x axis surrounding the origin, through which the empty bowtie
; path would run.  Seat the box a bit off to one side, so the search has to
; choose one side over the other.  Remember that the box points have to be
; counterclockwise.

(setf *polygons* (list
                    (list
                       (list -0.5 0.1)
                       (list -0.5 -0.1)
                       (list 0.2 -0.1)
                       (list 0.2 0.1)
                       (list -0.5 0.1)
                       (list -0.5 -0.1))
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
