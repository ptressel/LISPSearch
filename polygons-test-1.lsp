; A trivial case, to serve as a first test.

; One polygon -- a triangle.  Its first two vertices are repeated at the end,
; for a total of 5 points.

(setf *polygons* (list
                    (list
                       (list -1.0 0.0)
                       (list 1.0 0.0)
                       (list 0.0 1.0)
                       (list -1.0 0.0)
                       (list 1.0 0.0))))

; Start point is below and off to one side.

(setf *start* (list 0.5 -0.5))

; Goal is above and on the same side.

(setf *goal* (list 0.5 1.0))
