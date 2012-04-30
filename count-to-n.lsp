; This is a problem definition for the trivial problem of using functions that
; add or subtract 1 to get from one integer to another.  To test the "exclude
; just visited state" repeated state handling, throw in a NOOP operation.
; This problem is implemented as a function that reads the desired start and
; end numbers, and a search type, and calls blind-search to find a solution.

; We need a few functions that are required by the blind-search package.  Some,
; like the goal test, need to be constructed after we get the user's input, but
; some can be statically defined.  Since these are all small, if we didn't want
; to define top-level functions, we could always include them as closures inside
; the call to blind-search, as we will with the goal test.

; Define a noop.

(defun noop (num) num)

; Return the list of operations.  This takes a state as arg, but we ignore it
; since our ops are always the same.
(defun count-to-n-ops (num)
   (list #'1+ #'1- #'noop))

; Show a state by echoing the number.
(defun count-to-n-show-state (num)
   num)

; Show an operator.
(defun count-to-n-show-op (op)
   (cond ((equal op #'1+) "ADD 1")
         ((equal op #'1-) "SUB 1")
         ((equal op #'noop) "NOOP")
         (t "UNKNOWN")))

(defun count-to-n ()
   (let (; States are just integers.  Read in starting and ending numbers.
         (start-num (read (format t "~&Starting number? ")))
         (end-num (read (format t "~&Ending number? ")))
         (search-type 
            (progn
               ; These are split up to keep the line length in the source less
               ; than 80 chars.  Wish I knew a way to split a quoted string
               ; across a line boundary.  Perhaps there's function or macro
               ; that would let me do it.  Could always *write* a macro, I
               ; suppose...
               (format t "~&Search type options are:~%")
               (format t " depth-first~% breadth-first~% iterative-deepening~%")
               (format t "Search type? ")
               (read)))
         (excl-type 
            (progn
               (format t "~&Repeated state handling options are:~%")
               (format t " exclude-if-visited~% exclude-if-in-path~%")
               (format t " exclude-same-as-parent~% no-exclusion~%")
               (format t "Repeated state handling option? ")
               (read)))
         (max-depth (read (format t "~&Maximum search depth? "))))

      ; The problem list and goal function arguments to blind-search are
      ; constructed in place.  We don't need to do anything other than call it,
      ; since it'll call our show functions to print the result.
      (blind-search
         ; First arg is the problem list, which contains the initial state, the
         ; goal function, which is a closure that tests against the end-num,
         ; and the above functions that provide the list of operations and show
         ; a state an operation.
         (list
            start-num
            #'(lambda (num) (equal num end-num))
            #'count-to-n-ops
            #'count-to-n-show-state
            #'count-to-n-show-op)
         ; Remaining args are keyword args.
         :search-type search-type
         :repeated-state-handling excl-type
         :maximum-depth max-depth)))
