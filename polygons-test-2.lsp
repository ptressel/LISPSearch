; A non-trivial single-polygon case:  Concave polygon, shaped like a bowtie.
; The start is at one inside vertex, and the goal at the other.  Almost forgot:
; gotta go 'round counterclockwise.

(setf *polygons* (list
                    (list
                       (list 0.0 1.0)
                       (list -1.0 0.0)
                       (list -0.5 -0.5)
                       (list 0.0 -1.0)
                       (list 1.0 0.0)
                       (list 0.5 0.5)
                       (list 0.0 1.0)
                       (list -1.0 0.0))))

; Start

(setf *start* (list -0.5 -0.5))

; Goal

(setf *goal* (list 0.5 0.5))
