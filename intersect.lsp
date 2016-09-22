;; Author: Joshua Redstone <redstone@pussypaws.cs.washington.edu>
;;
;;  Here is the lispified code to compute intersection of 2 lines.
;;  the entry function is:
;;
;; (intersect x0 y0 x1 y1 x2 y2 x3 y3)
;;
;; where the lines are (x0,y0) - (x1,y1) 
;;     and (x2,y2) - (x3, y3)
;; intersect returns nil if the lines don't intersect
;; note that the endpoints of lines do count as possible intersection 
;; points.  if the lines intersect, it returns a list of the
;; x and y coordinates of the intersection point.

; Modified:
;
;   - Handles intersection at an endpoint differently, and handle colinear
;     segments differently.
;
;   - Semantics are different:  Inputs have meaning specific to this problem,
;     and are no longer treated symmetrically.  Return value is now T or NIL.
;
;   - Adds assorted little helper functions.
;
;   - Adds the in-segment function, called from polygon.lsp.
;
; This version regards the first point as the current point, the second as the
; candidate point, and the third and fourth as endpoints of a polygon side.
;
; If an endpoint of the candidate path lies on the polygon side, that's ok,
; and we should return NIL (no intersection).  One endpoint is the current
; point -- we always want to allow an intersection there, because any test
; with the current point on some polygon was handled in the "outside" test,
; so we don't want to touch those cases here.  The other endpoint is the
; candidate point -- we want to allow the path to end up on the perimiter of
; a polygon.
;
; But if an endpoint *of the polygon side* lies *in* the candidate path, not at
; one of its endpoints, that's a "real" intersection.  It is an intersection
; *even if the segments are colinear*, because if the candidate path is allowed
; to go *through* a polygon vertex, it might go into the interior.  This is 
; also part of the support for concave polygons, which requires that a path
; that goes through the endpoint of a polygon side *must* stop there, and be
; continued in a separate step, so that it can perform the outside angle check.


;; *********************************************************************
;; * Start of line intersection code
;; *********************************************************************

; Make sure all our constants are long-floats.
(setf *read-default-float-format* 'long-float)

(defun SQR (x) (* x x))
(setf EPSILON 1d-6)     ; Make EPSILON a long-float
(defun DET (a b c d)
  (- (* a d) (* b c)))
(defun DIST (x0 y0 x1 y1)
  (sqrt (+ (SQR (- x0 x1)) (SQR (- y0 y1)))))
(defun fabs (x) (abs x))

(defun equal-within-epsilon (a b) (<= (abs (- a b)) EPSILON))


;   (x0, y0) and (x1, y1) are the end points of the first line segment.
;   (xa, ya) and (xb, yb) are the end points of the second line segment.
;
;   If there's no intersection that we care about, return NIL,
;   else return true.

(defun intersect (x0 y0 x1 y1 xa ya xb yb)
 (let*
  ((x 0.0) (y 0.0)
   (m1 0.0) (m2 0.0)
   ; capture the result here so we can fix up the endpoint handling
   (intersection
    (cond 
     
     ; Avoid divide-by-zero by handling vertical and horizontal separately.

     ;; check if line segment 1 is vertical.
     ((<= (fabs (- x0 x1)) EPSILON)
      ;; check if line segment 2 is also vertical.
      (if (<= (fabs (- xa xb)) EPSILON)
        ; both are vertical -- check for overlap
        (overlap x0 y0 x1 y1 xa ya xb yb)
        ; only segment 1 is vertical
        (intersect_vertical x0 y0 x1 y1 xa ya xb yb)))

     ;; check if line segment 2 (only) is vertical.
     ((<= (fabs (- xa xb)) EPSILON)
      ; We don't need to check if line segment 1 is also vertical, because
      ; that case was caught above.
      (intersect_vertical xa ya xb yb x0 y0 x1 y1))

     ;; check if line segment 1 is horizontal.
     ((<= (fabs (- y0 y1)) EPSILON)
      ;; check if line segment 2 is also horizontal.
      (if (<= (fabs (- ya yb)) EPSILON)
        ; both are horizontal -- check for overlap
        (overlap x0 y0 x1 y1 xa ya xb yb)
        ; only segment 1 is horizontal
	(intersect_horizontal x0 y0 x1 y1 xa ya xb yb)))

     ;; check if line segment 2 (only) is horizontal.
     ((<= (fabs (- ya yb)) EPSILON)
        ; only segment 2 is horizontal
	(intersect_horizontal xa ya xb yb x0 y0 x1 y1))

     ; Neither is horizonal or vertical.

     (T
      (setf m1 (/ (- y1 y0) (- x1 x0)))   ;; slope of line segment 1
      (setf m2 (/ (- ya yb) (- xa xb)))   ;; slope of line segment 2
      (if (< (fabs (- m1 m2)) EPSILON)    ;; check if they are parallel

        ; Yes -- check for overlap and see if it's an allowed type.  Want this
        ; to return T if the candidate path goes right through an endpoint of
        ; the polygon side.  That is, return T if an endpoint of the polygon
        ; side is strictly interior to the candidate path, and NIL in all other
        ; cases.
        (overlap x0 y0 x1 y1 xa ya xb yb)

        ;; not parallel
	(progn 

          ;; Calculate the intersection (x,y).
	  (setf x 
		(/  (- (* (- x1 x0) (DET xa ya xb yb))  (* (- xa xb) 
							   (DET x1 y1 x0 y0)))
		    (DET (- y1 y0) (- x1 x0) (- ya yb) (- xa xb))))
	  (setf y 
		(/  (- (* (- ya yb) (DET x1 y1 x0 y0))  (* (- y1 y0)
							   (DET xa ya xb yb)))
		    (DET (- x1 x0) (- y1 y0) (- xa xb) (- ya yb))))
          (cond

           ;; /* check if the intersect point is on the line segment 1. */
           ((> (- (+ (DIST x y x0 y0) (DIST x y x1 y1)) (DIST x0 y0 x1 y1))
               EPSILON)
            nil)
           ;; /* check if the intersect point is on the line segment 2. */
           ((> (- (+ (DIST x y xa ya) (DIST x y xb yb)) (DIST xa ya xb yb))
               EPSILON)
            nil)
           (T (list x y)))))))))

   ; We're in the body of the let* here, with a value for intersection.
   ; Why is the endpoint check done here?  Because intersect_horizontal and
   ; intersect_vertical get called with the line segments in both orders, so
   ; we'd have no way of telling which was the candidate path and which the
   ; polygon side.

   ; The value of intersection is T or NIL, in which case we return those, or
   ; it's a point (which is a list) in which case we make the endpoint test.
   (if (not (consp intersection))

      ; It's not a point -- just return it.
      intersection

      ; Have a point -- need to do the endpoint check.  Recall from above:
      ; The first line segment is the candidate path; the second is a polygon
      ; side.  If the candidate point (second pair of coords) lies on the
      ; polygon side, that's ok.  Semantics of "intersect" are that we return
      ; non-NIL if there *is* an intersection we care about.  We got here if
      ; we *do* have an intersection, so all we want to do is see if the
      ; candidate point is the intersection, i.e. is on the polygon side, and
      ; if so, return NIL (i.e. this point is ok).

      (let ((x (first intersection)) (y (second intersection)))
         ; Allow either the current point, (x0,y0), or the candidate point,
         ; (x1,y1) to be the intersection, i.e. return NIL in those cases.
         ; For any other intersection point, return T. The points are
         ; calculated, and may have roundoff error, so don't do an exact match.
         (not (or (and (equal-within-epsilon x x0)
                       (equal-within-epsilon y y0))
                  (and (equal-within-epsilon x x1)
                       (equal-within-epsilon y y1))))))))


;   (x1, y1) and (x2, y2) are the end points of vertical line segment.
;   (x3, y3) and (x4, y4) are the end points of line to check.
;   return 0 : no intersection.
;   1 : with intersection.
;   (x, y): the intersected point

(defun intersect_vertical (x1 y1 x2 y2 x3 y3 x4 y4)
  (let ((a 0.0) (b 0.0)
	(x 0.0) (y 0.0))

    ;; y = ax + b for (x3, y3) and (x4, y4)
    (setf a (/ (- y3 y4) (- x3 x4)))
    (setf b (/ (- (* x3 y4) (* x4 y3))
	       (- x3 x4)))
    (setf x x1)
    (setf y (+ (* a x1) b))

    (cond 

     ((> (- (+ (DIST x y x3 y3) (DIST x y x4 y4)) (DIST x3 y3 x4 y4))
	   EPSILON)
	nil) ;; point not on segment 1

     ((> (* (- y y1) (- y y2)) 0)
	  nil) ; point not on segment 2

     (T (list x y)))))


;   (x1, y1) and (x2, y2) are the end points of horizontal line segment.
;   (x3, y3) and (x4, y4) are the end points of line to check.
;   return 0 : no intersection.
;   1 : with intersection.
;   (x, y); the intersected point

(defun intersect_horizontal (x1 y1 x2 y2 x3 y3 x4 y4)
  (let ((a 0.0) (b 0.0) 
	(x 0.0) (y 0.0))
    
    ;; y = ax + b for (x3, y3) and (x4, y4) 
    (setf a (/ (- y3 y4) (- x3 x4)))
    (setf b (/ (- (* x3 y4) (* x4 y3)) (- x3 x4)))
    (setf y y1)
    (setf x (/ (- y1 b) a))

    (cond 

     ((> (- (+ (DIST x y x3 y3) (DIST x y x4 y4)) (DIST x3 y3 x4 y4)) 
	   EPSILON)
	 nil) ;; point not on segment 1

     ((> (* (- x x1) (- x x2)) 0)
         nil) ;; point not on segment 2

     (T (list x y)))))

; Test for overlap of two parallel segments:  See if either endpoint of the
; second line segment, which is the polygon side, lies (strictly) in the
; interior of the first line segment, which is the the candidate path.  If
; it does, then the candidate path could potentially penetrate to the inside
; of the polygon -- disallow it by returning T.  If this test *doesn't* return
; T, then this is a legal path.  Return T in that case, not a point, because
; we don't need to do any endpoint fixup -- we already have a final result.

(defun overlap (x0 y0 x1 y1 xa ya xb yb)
   (or (in-segment xa ya x0 y0 x1 y1)
       (in-segment xb yb x0 y0 x1 y1)))

; Test if a point is on a line segment (interior or endpoint), by finding
; the distance from each endpoint to the test point, and seeing if those sum
; to the total length of the segment.  Args are the x and y coords of the
; test point and the two segment endpoints.  Returns true if the test point
; is on the segment.

(defun on-segment (x y x0 y0 x1 y1)
   (<= (- (+ (DIST x y x0 y0) (DIST x y x1 y1)) (DIST x0 y0 x1 y1)) EPSILON))

; Test if a point is in the interior of a line segment.  Only difference is
; that the test explicitly excludes the endpoints.  Do not rely on a >= test
; alone, for fear of roundoff error.  This function is also called from
; polygons.lsp.

(defun in-segment (x y x0 y0 x1 y1)
   (cond
      ; Exclude a match of the intersection point with either endpoint of
      ; the polygon side.  Don't do an exact equality test.
      ((or (and (equal-within-epsilon x x0) (equal-within-epsilon y y0))
           (and (equal-within-epsilon x x1) (equal-within-epsilon y y1))) NIL) 

      ; Allow anything else in the segment.
      ((on-segment x y x0 y0 x1 y1) T)))


