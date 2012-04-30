;*******************************************************************************
; POLYGONS -- navigation around convex polygons via the shortest path
;*******************************************************************************

; Given a starting and ending position on a 2-dimensional plane, and convex
; polygons that serve as obstacles, find the shortest path from start to end
; that doesn't cross a polygon.  The path is allowed to run along a side of
; a polygon.  It hasn't been stated whether the path is allowed to squeeze
; through if two polygons touch.  I'm assuming that this is allowed.

; Only straight line segments need be considered as parts of the path --
; in free space (i.e. when crossing an area without obstacles) a straight
; line is shortest.  And the obstacles all have straight sides, so a path
; that's hugging the side of an obstacle will still be composed of straight
; line segments.

; The only points that need be considered as stopping points along the path
; are the start and end points and vertices of the polygons -- no path that
; includes points other than these will be shorter.  (Note that this does not
; preclude having points that lie on some polygon's side other than at one
; of its vertices, because it is legal for a vertex of one polygon to touch
; a side of another.)

; This version does not support concave polygons, but trys not to make it hard
; to add such support later.  There are some tricky special cases that would
; need their own checks -- I won't have time to add them.  

; The main requirement for supporting concave polygons is that one must know
; which side is the inside of the polygon.  For this reason, I'm requiring
; the user to list the vertices in counterclockwise order.

; I'm not trying to support overlapping polygons.

; Another extension would be to allow an enclosing polygon -- a perimeter.
; To distinguish between an enclosing polygon and an enclosed polygon, the
; order of the vertex list is reversed.  That is, the vertices are listed
; in counterclockwise with respect to the "center" of the polygon.  For
; an enclosing polygon, the "center" would be at infinity, and the order
; would be counterclockwise when viewed from the enclosed region exterior
; to that polygon.  (We may get this extension for free if support for
; concave polygons were added.)

; Before we go any further, try to make sure we don't get bit by roundoff error,
; by telling LISP to regard all non-integers as long-floats.  Apparently
; long-float is required to be the largest number of bits, and may be larger
; than double-float.  But some LISPs may not have long-floats.  Clisp seems
; to have it, and it *is* bigger than clisp's double float.

(setf *read-default-float-format* 'long-float)

;*******************************************************************************
; REPRESENTATION OF THE WORLD
;*******************************************************************************

; The world is represented by a list of (non-overlapping, convex) polygons,
; plus the start and goal points.  All of these are globals.

(declaim (special *polygons* *start* *goal*))

; Each polygon is represented as a list of vertices.  The vertices must be
; listed in order of a counterclockwise traversal around the edges of the
; polygon, with the first two vertices repeated at the end of the list.
; That way, each vertex of the polygon appears in this list surrounded by
; its two adjacent vertices.  When we use the list, we start at the second
; vertex, and quit at the second to last.  (See the routine on-polygon,
; below, which makes use of this convenience.)  It is not necessary to make
; the user supply their raw polygons this way -- we can tack on the duplicate
; vertices ourselves.  (For now, because time is short, I'll provide the
; vertex lists with the repeats.)

; A vertex is represented as a list of two numbers, of which the first is
; the x coordinate and the second the y coordinate.

;*******************************************************************************
; REPRESENTATION OF A STATE
;*******************************************************************************

; A state is a single point, which is one of: the start point, the goal
; point, or a vertex of a polygon.  The state is represented as a list of two
; numbers, x coordinate first, then y coordinate.  It is not necessary to
; include the world in each state, because the world is static.

; It is not necessary to keep a separate list of legal state points, apart from
; the world information, but it may be convenient to do so.

;*******************************************************************************
; Distinguishing legal successor states
;*******************************************************************************

; How do we know what points are legal successors of the current point?
; (I'll refer to a potential successor point as the candidate point, and
; the line segment from the current point to the candidate point as the
; candidate path.)

; There are two types of test that may need to be made.

; Test 1:

; The first is needed only if the current point is on a polygon (which will
; be true except perhaps for the starting point) and if the candidate point
; is also on that polygon.  Such a path may not be excluded by a check for
; intersection with polygon sides, because the candidate path will run
; unobstructed through the interior of the polygon.

; For a convex polygon, the only legal successors *on the same polygon* are
; points on the sides adjacent to the current point.  (That's assuming one
; protects against the degenerate case of colinear adjacent sides -- then
; more than just the immediately adjacent side might be legal.  Not a real
; problem, since we could remove the unneeded vertices at the outset.)  So
; we'd need to check if the candidate point were on a non-adjacent side or
; vertex, and exclude those.  Checking for that is O(n) in the number of
; vertices of the polygon.

; An alternative that's O(1) is to check the direction of the candidate path
; vs. the directions of the adjacent sides -- we don't care if the candidate
; point is on the same polygon or not:  If the candidate path goes into the
; interior of the polygon, we don't want it.  This test will exclude some
; successors that would also be excluded by intersection tests, but then, if
; we exclude 'em here, we don't have to do that test...

; A picture really is worth a thousand words here...even an ASCII picture.
; This shows a *concave* polygon, with a legal candidate path, i.e. one that
; lies in the exterior of the polygon.

;   candidate point               /
;                   \            /side 1 of polygon
;      candidate path\          /
;                     \        /
;                      \      /
; exterior of polygon   \    /
;                        \  /
;          _______________\/current point
;    side 2 of polygon
;
;                       interior of polygon

; If we start from side 1 (which is the side clockwise from the current point
; in the vertex list of the polygon), and sweep around counterclockwise, then
; if we encounter the candidate path before we encounter side 2 of the polygon,
; the candidate path is in the exterior of the polygon (at least to begin with,
; and for convex polygons, a path that starts in the exterior, stays in the
; exterior).

; If the current point is in the middle of some polygon side rather than at a
; vertex, the only difference is that the side 1 and side 2 segments are from
; the current point to the endpoints of that one side, not the endpoints of
; the two sides adjacent to the current point.

; Gory details of this check are with the routines below.

; Test 2:

; If a candidate path survives that test, we still need to see if it runs
; through some *other* polygon besides the one the current point is on.

; We want to allow path segments that *end up* on a polygon side -- after all,
; that's going to be all of them except possibly the one that goes to the goal.
; *However*, we do not want to allow a path that sneaks through a vertex into
; the interior of some polygon.  One might then exclude paths that don't end
; up at a vertex, but rather continue on beyond it.  Except...it's legal for
; the path to go through and beyond a vertex as long as what's beyond is not
; inside the polygon.  Fortunately, this doesn't matter:  If we exclude a path
; segment that passes through and goes beyond a vertex, then we know that that
; very vertex will also be a candidate successor, and we *can* form a path
; segment that goes to that vertex and stops.  From there, another step in the
; search can extend the path on to whatever the original segment's destination
; was.  The path length will be identical, and that's all we care about -- we
; don't care at all that it took two steps instead of one.

; So the intersection test is:  Disallow a path if it *crosses through* some
; polygon side anywhere on that side, including vertices.  Permit the path
; if it doesn't touch a polygon side, or if it runs along that side, or if its
; endpoint is on that side.

; In summary, the tests are:

; 1: If the current point is on a polygon (or polygons), check if the candidate
; path is interior to that/those polygon(s).  Exclude it if so.

; 2: Then check if the candidate path passes through any polygon sides.  Exclude
; it if so.

;*******************************************************************************
; SUCCESSORS function
;*******************************************************************************

; This is the hard part.  Break it up into tiny pieces.

;*******************************************************************************
; Test 1 -- see if path is inside a polygon (see "legal successor states" above)
;*******************************************************************************

; This helper function that checks if a given point (which will be the current
; state) is on a polygon (either a vertex or a side), and if so, returns a list
; containing (at the front) the endpoint of the polygon side to clockwise of
; the point, the point itself, and the endpoint of the polygon side to
; counterclockwise of the point.  For convex polygons, a point can only be on
; one vertex or side, so we can stop when we get to the first match.

; If the point is a vertex, then we can just return a tail of the polygon list
; that starts with the vertex prior to the matching vertex -- we won't touch
; anything but the first three items in the list, so we don't care what's
; beyond.

; If the point is somewhere in the middle of a side, then we'll need to make
; a fresh list.

; It makes use of a function "in-segment", which takes a point, and endpoints
; of a line segment, and figures out if the point is in that segment, excluding
; the endpoints.  Because this is related to the intersection code, it has been
; put in that file.

(defun on-polygon (point polygon)
   ; We enter with a polygon list, or the tail of one.  See if the point is
   ; equal to the vertex that's *second* in the (partial) polygon list.  If not,
   ; see if it's on the line segment between the second vertex and the third
   ; vertex.  We can quit if we don't have a third vertex, because at that
   ; point, the second is a duplicate, and we've already checked that one.
   ; E.g. say the polygon list contains points (a b c d e a b).  We start by
   ; checking b, then c, d, e, a, and quit there.  At all times, we have
   ; access to the two adjacent vertices.  So, in the following, remember that
   ; the vertex being examined is the "second" of the list, and we're done if
   ; we don't have three vertices, i.e. if there's no "third".

   (cond
      ; If we're at the end, the point isn't on the polygon.
      ((null (third polygon)) NIL)

      ; See if the point matches this vertex.
      ((equalp point (second polygon))
          ; Yes -- return the prior vertex, the point, and the next vertex
          ; (with maybe a bit more tacked on the end).
          polygon)

      ; See if the point is in the side beyond this vertex.  Since in-segment
      ; is in intersect.lsp, it obeys the convention of the intersect function,
      ; which is that the arguments are individual coordinates, not point lists.
      ((in-segment (first point)             ; x coord of "point"
                   (second point)            ; y coord of "point"
                   (first (second polygon))  ; x coord of one side endpoint
                   (second (second polygon)) ; y coord of one side endpoint
                   (first (third polygon))   ; x coord of other side endpoint
                   (second (third polygon))) ; y coord of other side endpoint
          ; It's in this side -- sandwich our point between those vertices.
          (list (second polygon) point (third polygon)))

      ; Else continue with the next vertex.
      (T (on-polygon point (rest polygon)))))

; The next helper function is rather clumsy -- it's the thing that sees if the
; candidate path is in the "interior" or "exterior" of the angle defined by
; the triple produced above.  We have three line segments:
;
;   side1, which extends from the current point to the clockwise vertex
;   side2, which extends from the current point to the counterclockwise vertex
;   path, which extends from the current point to the candidate point

; Attempt to avoid calculations by doing this:
;
;   Translate the points to make the current point the origin (i.e. just
;     subtract the current point from the others), getting just a list of
;     endpoints of the translated segments.
;
;   Find out what quadrants they're all in.  (Let the positive x axis be part
;     of quadrant 1, etc.)
;
;   Find out if side1 is closer, in the counterclockwise direction, to path
;     or to side2.  This is easy if path and side2 are in different quadrants,
;     so do that check first.  Then, if they are in the same quadrant, there
;     are two cases:  If side1 is also in that quadrant, compare the slopes
;     of all three: path is ok if we have side1, path, side2 in clockwise order.
;     If side1 is not in that quadrant, compare the slopes of path and side2:
;     path is ok if its slope is clockwise from side2's.

; It's no better to calculate angles (which would mean taking the arctan of the
; quotient of the coordinates, and which would likely be subject to roundoff
; error), because we'd still have to check which quadrant the points were
; in, to tell how to interpret the sign of the result, and to avoid infinities
; for vertical lines.

; But first, a helper for the helper -- a function that subtracts two points
; by subtracting their x coords and y coords separately.  That is, this finds
; the (vector) offset from point2 to point1.  Or, it translates point1 by
; making point2 the new origin.

(defun offset (point1 point2)
   (list (- (first point1) (first point2)) (- (second point1) (second point2))))

; And another helper -- this one decides what quadrant a point is in.  Recall
; I'm putting each axis with the quadrant that's counterclockwise from it.
; We won't pass (0 0) to this, because we won't be making any null moves, so
; the candidate will never be the current point, nor will we allow polygons
; with zero-length sides.

(defun quadrant (point)
   (let ((x (first point))
         (y (second point)))
      (cond
         ; First quadrant: x > 0, y >= 0
         ((and (> x 0) (>= y 0)) 'Q1)

         ; Second quadrant: x <= 0, y > 0
         ((and (<= x 0) (> y 0)) 'Q2)

         ; Third quadrant: x < 0, y <= 0
         ((and (< x 0) (<= y 0)) 'Q3)

         ; Fourth quadrant: x >= 0, y < 0
         ((and (>= x 0) (< y 0)) 'Q4))))

         ; Let (0 0) fall off the end.

; And one more -- this one takes three quadrants, for side1, path, and side2,
; and, if they are distinct, decides if they are in counterclockwise order.
; It just checks through an enumeration of legal combinations.
; Remember that the required order is side1, then path, then side2...

(declaim (special *legal-quadrants*))

(setf *legal-quadrants*
   (list '(Q1 Q2 Q3)
         '(Q1 Q2 Q4)
         '(Q1 Q3 Q4)
         '(Q2 Q3 Q4)
         '(Q2 Q3 Q1)
         '(Q2 Q4 Q1)
         '(Q3 Q4 Q1)
         '(Q3 Q4 Q2)
         '(Q3 Q1 Q2)
         '(Q4 Q1 Q2)
         '(Q4 Q1 Q3)
         '(Q4 Q2 Q3)))

; Start this off by passing it *legal-quadrants* as its fourth arg.

(defun grossly-counterclockwise (side1-quad path-quad side2-quad legal-quads)
   (let ((a-legal-quad (first legal-quads)))
      (cond
         ; Did we run out of legal cases?
         ((null legal-quads) NIL)

         ; Are we at a legal case?
         ((and (equalp side1-quad (first a-legal-quad))
               (equalp path-quad (second a-legal-quad))
               (equalp side2-quad (third a-legal-quad))) T)

         ; Else continue looking.
         (T (grossly-counterclockwise side1-quad path-quad side2-quad
                                      (rest legal-quads))))))

; Well, just *one* more...  If the above test fails, we know that either not
; all three points are in separate quadrants, or they're not counterclockwise.

; Remaining cases are:
;
;   side1 and path in same quadrant, but side2 in a different quadrant, in
;     which case path must be to counter-clockwise of side1, which we test by
;     looking at their slopes
;
;   path and side2 in same quadrant, but side1 in a different quadrant, in
;     which case side2 must be to counter-clockwise of path
;
;   side1 and side2 in same quadrant, but path in a different quadrant, in
;     which case side1 must be to counter-clockwise of side2
;
;   all three in the same quadrant, in which case path must be to counter-
;     clockwise of side1, and side2 to path

; Yes, yes, yes, I *know* that they're "counterclockwise" even if they're
; opposite to the above, by going all the way round through the other quadrants.
; I mean *within* the quadrant...

; This function takes two points in the same quadrant, along with their
; previously-found quadrant designator, and decides if the first is to
; counterclockwise of the second.  If we're in the second or fourth quadrant,
; which include the y axis, we take look at x/y not y/x.  This avoids divide
; by zero problems.

; We know that it will never be the case that side1 and side2 are colinear, so
; we can pick our behavior if the slopes are equal based on what we want if
; path and side1 or path and side2 are colinear.  Since we want to allow a path
; that runs along the side of a polygon, these cases should return true.  I'm
; going to try using just a straight >= rather than allowing some slop for
; roundoff error.

(defun pairwise-counterclockwise (point1 point2 quad)
   (let ((x1 (first point1))
         (y1 (second point1))
         (x2 (first point2))
         (y2 (second point2)))
      (cond
         ; First quadrant and positive x axis -or - third quadrant and negative
         ; x axis:  If slope y/x of point1 is >= than slope of point2, they're
         ; in the desired order.
         ((or (equalp quad 'Q1) (equalp quad 'Q3))
             (>= (/ y1 x1) (/ y2 x2)))

         ; Second quadrant and positive y axis -or fourth quadrant and negative
         ; y axis:  If the inverse slope (x/y) of point1 is >= in magnitude
         ; than that of point2, they're in the desired order.  Since x/y is
         ; negative in Q2 and Q4, then if we don't take the absolute value,
         ; we want x/y for point1 to be <= that of point2.
         ((or (equalp quad 'Q2) (equalp quad 'Q4))
             (<= (/ x1 y1) (/ x2 y2))))))

; Arguments are the candidate point and the triple of points on the polygon
; (in counterclockwise order) that form the angle we're testing against.
; The second point of the triple is the current point, which is the point from
; which all the line segments we're comparing depend.

(defun outside-this-angle (candidate polygon-piece)
   (let*
      (; Translate the three endpoints to put the current point at the origin.
       (current (second polygon-piece))
       (path (offset candidate current))
       (side1 (offset (first polygon-piece) current))
       (side2 (offset (third polygon-piece) current))

       ; Get their quadrants.
       (path-quad (quadrant path))
       (side1-quad (quadrant side1))
       (side2-quad (quadrant side2)))

      ; Run the points through the various counterclockwise filters one at a
      ; time.  All of these are predicates, so combine them with boolean
      ; functions.
      (or
         ; Start with the all-different-quadrants check.
         (grossly-counterclockwise side1-quad path-quad side2-quad
                                   *legal-quadrants*)

         ; Nope?  See if side1 and path are in the same quadrant.  If they are,
         ; there are two cases: side2 is in there with them, or it's not.  In
         ; either case, we need path to be counterclockwise from side1.
         (and (equalp side1-quad path-quad)
              (pairwise-counterclockwise path side1 path-quad)

              ; In addition, we must either have that side2 is not in the same
              ; quadrant, or if it is, then side2 must be to counterclockwise
              ; of path.  The "or" macro will not evaluate any form past the
              ; first non-NIL.  So we can have the quadrant check first, and
              ; fall through to the ccw check if that fails.
              (or (not (equalp path-quad side2-quad))
                  (pairwise-counterclockwise side2 path path-quad)))

         ; We've dealt with side1 and path alone together, and all three
         ; together -- we won't get here if either of those are true, so we
         ; don't need to be sure all three aren't together in the remaining
         ; tests.  Next check the path and side2 together case:
         (and (equalp path-quad side2-quad)
              (pairwise-counterclockwise side2 path path-quad))

         ; Last case is side1 and side2 (but not path) together.
         (and (equalp side2-quad side1-quad)
              (pairwise-counterclockwise side1 side2 side1-quad)))))

; Combine outside-this-angle and on-polygon to verify that for a given polygon,
; if the current point is on the polygon, then the candidate path is outside.

(defun outside-this-polygon (current candidate polygon)
   ; See if the current point is on the supplied polygon. If it is, get the
   ; triple of points that define the surrounding angle.
   (let ((polygon-piece (on-polygon current polygon)))

      ; The candidate is safe (for the moment) if either the current is not on
      ; this polygon (in which case polygon-piece is NIL), or the candidate
      ; path is outside the polygon (i.e. "outside" returns T).
      (or (null polygon-piece) (outside-this-angle candidate polygon-piece))))

;*******************************************************************************
; Test 2 -- look for intersections
;*******************************************************************************

; If a candidate path survives the "outside" check, test it against all polygon
; sides:  If it crosses (not overlaps or stops on) the polygon side, even at
; an endpoint, then the path is not allowed -- return NIL.  If we get through
; the whole list of polygons without failing, return T.

; The routine that performs the intersection test is in the file intersect.lsp.
; It is a modified version of Josh's code.  Since the test is now asymmetrical,
; we need to be careful which line segment to put first in the argument list.  
; It's the first arg that's treated like the candidate path, and the second
; like a polygon side.  If an endpoint of the candidate path lies on the
; polygon side, that's ok.  But if an endpoint of the polygon side lies in the
; candidate path not at one of its endpoints, that's an intersection.

; Apply the intersection test to one polygon, one side at a time.  Step
; through the vertex list, checking the line segment between the current "first"
; and "second" of the list.  We're done with the polygon when the "third" of the
; list is null, because we replicate two points from the head at the tail, so
; when the third is null, the line segment we're positioned at is identical to
; that at the head of the polygon list, which we've already examined.

(defun doesnt-pierce-polygon (current candidate polygon)

   ; We pass this test if either we're done with the polygon, or the current
   ; side is ok and we survive the rest of the polygon.
   (or 
      ; First check if we're done with this polygon.
      (null (third polygon))

      ; Not done -- we need to survive both this side and the rest of the
      ; polygon.
      (and
         ; Check this segment.  The arguments to "intersect" are the endpoints
         ; of the candidate path, then the endpoints of the polygon side.
         ; Intersect wants all its points flattened out.  I could use the
         ; "flatten" function defined below...but that's more work at
         ; runtime, so I'll just split out all the pieces here.  Intersect
         ; returns non-NIL if there *is* an intersection that we want to
         ; exclude, so negate it to make a boolean check for an *allowed* path.
         (not (intersect
                 ; Coords of the current point
                 (first current)
                 (second current)
                 ; Coords of the candidate point
                 (first candidate)
                 (second candidate)
                 ; Coords of one endpoint of the polygon side 
                 (first (first polygon))
                 (second (first polygon))
                 ; Coords of the other endpoint
                 (first (second polygon))
                 (second (second polygon))))

         ; If we survived, continue for the rest of the sides.
         (doesnt-pierce-polygon current candidate (rest polygon)))))

;*******************************************************************************
; Grand unified successor test
;*******************************************************************************

; Step through a polygon list.  For each polygon, make both the above tests.
; Start this off with *polygons* to check the entire world.

(defun outside-all-polygons (current candidate polygons)

   (or
      ; First see if we're at the end of the list.
      (null polygons)

      ; Not done -- we need to survive all of: the outside check, the intersect
      ; check, and the rest of the polygons.
      (and
         ; The "outside" check:
         (outside-this-polygon current candidate (first polygons))

         ; The "intersect" check:
         (doesnt-pierce-polygon current candidate (first polygons))

         ; And the rest of the polygons:
         (outside-all-polygons current candidate (rest polygons)))))

; Whew!  As the Grand Moff Tarkin said to Darth Vader (shortly before the
; Death Star got vaporized), "This had *better* work..."

;*******************************************************************************
; Generate potential successor list
;*******************************************************************************

; All of that work was just to test *one* candidate successor.  We still have
; to go through all the available points.  The points we need to consider as
; potential successors are the vertices and the goal point -- we never need to
; go back to the start point.  For simplicity, and to avoid re-testing a
; repeated point, generate a list of these points with duplicates removed,
; rather than trying to traverse the polygon list plus the goal.  We must be
; careful to do this non-destructively, so we don't ruin the polygon list...

(declaim (special *potential-successor-pool*))

; Take a list of lists, and union them all together.  This does not go any
; deeper than the second level of list.

(defun flatten (flattened-list list-of-lists)

   (cond
      ; We're done if we don't have anything left to tack on.  Return our
      ; flattened list.
      ((null list-of-lists) flattened-list)

      ; Else munge together our flattened list so far, and the head of the
      ; list of lists, then continue.  Union is non-destructive, and doesn't
      ; include duplicate copies of elements.
      (T (flatten (union flattened-list (first list-of-lists))
                  (rest list-of-lists)))))

; Call this in the initialization code, after the goal point and polygon list 
; are available, like this:
;
; (setf *potential-successor-pool* (flatten (list goal) *polygons*))

;*******************************************************************************
; Generate actual successor list
;*******************************************************************************

; Now that we have a collection of possible successors, run the check on each
; of them, and include it in the successor list if the check returns true.

; I use remove-if-not to exclude the points that don't pass the check.  It
; needs a boolean function of one argument, which must be the candidate point.
; Since the test function, outside-all-polygons, takes three args, the others
; have to be packaged up with it as a closure.  Recall we want to start
; outside-all-polygons off with the whole polygon list.

(defun successors (current)
   (remove-if-not #'(lambda (candidate)
                       (outside-all-polygons current candidate *polygons*))
                  *potential-successor-pool*))

;*******************************************************************************
; GOAL function
;*******************************************************************************

; The goal function takes a point as argument, and compares it against the
; goal.  Since it only takes one argument, the goal must be bundled in as a
; closure.  This function will actually be formed in place in the problem list
; when Astar-search is called.

;*******************************************************************************
; COST and HEURISTIC functions
;*******************************************************************************

; Both of these just compute the length of a line segment.  The cost function
; takes two args -- current and successor points.  The heuristic function takes
; one arg, a candidate point, and calculates the distance to the goal.  So just
; make a "length" function, which will be the cost function.  For the goal
; function, make a closure of the length function with one point being the goal
; point.  There is a function that computes length in intersect.lsp, called
; "dist" but not in a form I can use directly -- it doesn't take "points" but
; rather a flat list of four coordinates: x and y for the first point, then
; x and y for the second.  Use it, and convert the args.

(declaim (inline len))

(defun len (point1 point2)
   (dist (first point1) (second point1) (first point2) (second point2)))

;*******************************************************************************
; SHOW-STATE function
;*******************************************************************************

; Just echo back the state, and let it get printed as a list.  One of these
; days, I'll need to learn how to use macros, so I can dispense with the
; "inline"s.

(declaim (inline show-state))

(defun show-state (point) point)

;*******************************************************************************
; POLYGONS main program
;*******************************************************************************

; I'm not going to provide an elaborate user interface that asks the user to
; type in the polygon points, etc.  No-one would want to do that anyway.
; Instead, the user should put the data in a file, in the form of LISP commands
; to setf the *polygons*, *start*, and *goal* globals.  I *could* ask the user
; for the filename, then load it, but for now, the user should just load it
; themselves before invoking polygons.

; I'm also not going to sanity-check the input for disallowed features, like
; colinear adjacent vertices, or identical adjacent vertices, or overlapping
; polygons.

(defun polygons ()

   ; The only setup we need to do is to construct the potential successor list.
   (setf *potential-successor-pool* (flatten (list goal) *polygons*))

   ; The only argument to Astar-search is the problem list.  Form it in place
   ; inside the call.
   (Astar-search
      ; First element is the initial state.
      *start*
      ; Next is the goal function, which is an equality test of the argument
      ; against *goal*.
      #'(lambda (point) (equalp point *goal*))
      ; Next is the successors function.
      #'successors
      ; And the cost function.
      #'length
      ; And the heuristic function, which is the length from the arg to *goal*.
      #'(lambda (point) (len point *goal*))
      ; Last is the show-state function.
      #'show-state))

