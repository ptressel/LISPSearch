; This is the 8 thingy case sent out in e-mail.  Hope I get their points all
; properly in counterclockwise order...  Hey, it looks like they're all
; already counterclockwise -- thanks!

(setf *polygons* (list
                    (list
                       (list 13 -7)
                       (list 14 -6)
                       (list 8 4)
                       (list 7 3)
                       (list 13 -7)
                       (list 14 -6))
                    (list
                       (list 4 -4)
                       (list 6 -4)
                       (list 5 -2)
                       (list 0 -2)
                       (list 4 -4)
                       (list 6 -4))
                    (list
                       (list 11 8)
                       (list 12 8)
                       (list 12 16)
                       (list 11 16)
                       (list 11 8)
                       (list 12 8))
                    (list
                       (list 9 -9)
                       (list 10 -8)
                       (list 8 -3)
                       (list 6 -4)
                       (list 9 -9)
                       (list 10 -8))
                    (list
                       (list 6 -2)
                       (list 8 0)
                       (list 3 8)
                       (list 1 6)
                       (list 6 -2)
                       (list 8 0))
                    (list
                       (list 18 -8)
                       (list 19 -8)
                       (list 15 0)
                       (list 14 -1)
                       (list 18 -8)
                       (list 19 -8))
                    (list
                       (list 6 7)
                       (list 7 8)
                       (list 2 14)
                       (list 1 13)
                       (list 6 7)
                       (list 7 8))
                    (list
                       (list 15 2)
                       (list 21 4)
                       (list 21 5)
                       (list 14 3)
                       (list 15 2)
                       (list 21 4))))

; Start

(setf *start* (list 2 -5))

; Goal

(setf *goal* (list 21 14))
