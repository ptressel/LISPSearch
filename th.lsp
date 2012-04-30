;******************************************************************************
; Four peg, n ring Towers of Hanoi
;******************************************************************************

;******************************************************************************
; State representation
;******************************************************************************

; Represent a state by a list of four lists, where each sub-list represents
; the ring stack on a peg.  The peg lists are in order from peg 1 to peg 4
; within the state list.  A peg list consists of the numbers representing
; rings that are on that peg, from smallest to largest.  Rings are numbered
; 1 to n, with 1 being smallest.

;******************************************************************************
; Initial state
;******************************************************************************

; Initial state constructor -- we can't get away with enumerating the ring
; numbers to form the stack on peg 1, because we don't know how big n is.
; So we'll need a function to construct the initial state after we ask the
; user for n.

; This forms a list of numbers from i to n where i <= n.

(defun TH_form_list_i_to_n (i n)
   (if (equal i (1+ n))
          ; If we're past n, we don't need more numbers.
          NIL
          ; Else form list from i+1 to n, and add current ring number on
          ; the front of it.
          (cons i (TH_form_list_i_to_n (1+ i) n))))

; Get number of rings from the user.  Form the state list by calling the
; above function to make the peg 1 list, with NIL for the other lists.

(defun TH_get_initial_state ()
   (list
      (TH_form_list_i_to_n 1 (read (format t "~%Number of rings: ")))
      NIL
      NIL
      NIL))

;******************************************************************************
; Goal test
;******************************************************************************

; Require the first three pegs to be empty, and the fourth to contain a list
; of numbers from 1 to n.  If we trust our procedure to properly maintain
; legal ring stacks, then we really only need to test that the first three
; pegs are empty.  Since this program does intend to maintain legal stacks,
; it is a bug if they are not legal, which should be fixed.  Therefore assume
; the state is legal, and check only for empty pegs.

(defun TH_goal_test (state)
  (and (null (first state))
       (null (second state))
       (null (third state))))

;******************************************************************************
; Operations
;******************************************************************************

; If allowed, move the top ring from peg_X to peg_Y and return
; the state corresponding to the situation after the move. The
; move is not allowed if the top ring on peg_X is larger than
; the top ring on peg_Y, or if peg_X = peg_Y, or if there is no
; ring on peg_X. If not allowed, return NIL.

; In addition, the operations serve as their own print functions,
; when called with a state argument of -1.  This can't be mistaken
; for a legal state, because a state is a list not a number.

(defun TH_move (peg_X peg_Y state)

 ; First see if we want to show or perform the operation.
 (if (equal state -1)

   ; Here if want to return a description.
   (format NIL "~%Move ring from peg ~A to peg ~A~&" peg_X peg_Y)

   ; Else perform the operation:
   ; Get the numbers of the top rings, for ease of testing.
   ; Since peg_X and peg_Y are numbers, they can be used as
   ; arguments of "nth" to pull out from the state the list
   ; corresponding to those pegs. Only wrinkle is that nth
   ; regards 0 to mean the first element. In a serious program,
   ; the pegs would be numbered 0 through 3 internally, and
   ; only printed as 1 through 4 -- this would get rid of the
   ; (1- peg_N) calls.

   (let ((ring_on_X (first (nth (1- peg_X) state)))
         (ring_on_Y (first (nth (1- peg_Y) state))))

      ; Test for conditions that disallow the move:

      (unless 
         (or (null ring_on_X)               ; No ring on peg_X?
             (equal peg_X peg_Y)            ; Move to same peg?
             (and (not (null ring_on_Y))    ; Have ring on peg_Y
                  (> ring_on_X ring_on_Y))) ; and rings would end
                                            ; up out of order?

         ; Unless returns NIL if its test argument above is true.
         ; If the test is false, i.e. if the move is allowed,
         ; then it returns the second argument, below.

         ; Construct the new state here, by forming a list of
         ; four lists representing the new stacks on each peg.
         ; If the number of pegs were not fixed, it would be
         ; necessary to recurse or loop to construct the state
         ; list one peg at a time. Here, we can get away with
         ; constructing the state in place.

         ; For convenience, form the new stacks on peg_X and
         ; peg_Y first.

         (let (; Source stack has its top ring removed.
               (new_X (rest (nth (1- peg_X) state)))
               ; Destination stack has that ring added to top.
               (new_Y (cons ring_on_X (nth (1- peg_Y) state))))

            ; For each peg element in the state list, check if
            ; that peg is peg_X or peg_Y, in which case use
            ; their pre-constructed lists. If not, use the list
            ; that's in that position now.

            (list (cond ((equal peg_X 1) new_X)  ; peg 1
                        ((equal peg_Y 1) new_Y)
                        (t (first state)))
                  (cond ((equal peg_X 2) new_X)  ; peg 2
                        ((equal peg_Y 2) new_Y)
                        (t (second state)))
                  (cond ((equal peg_X 3) new_X)  ; peg 3
                        ((equal peg_Y 3) new_Y)
                        (t (third state)))
                  (cond ((equal peg_X 4) new_X)  ; peg 4
                        ((equal peg_Y 4) new_Y)
                        (t (fourth state)))))))))


; Return a list of all moves.  Since four is a small number, it's
; simplest to enumerate the possibilities.  Don't include any moves to
; same peg, but otherwise don't weed out any possibilities -- the move
; function itself will handle that.  Each move is a lambda expression
; that packages up the general move with two peg number arguments to
; form a function that takes only the state as an arg.

(defun TH_ops (state)
   (list
      #'(lambda (state) (TH_move 1 2 state))
      #'(lambda (state) (TH_move 1 3 state))
      #'(lambda (state) (TH_move 1 4 state))
      #'(lambda (state) (TH_move 2 1 state))
      #'(lambda (state) (TH_move 2 3 state))
      #'(lambda (state) (TH_move 2 4 state))
      #'(lambda (state) (TH_move 3 1 state))
      #'(lambda (state) (TH_move 3 2 state))
      #'(lambda (state) (TH_move 3 4 state))
      #'(lambda (state) (TH_move 4 1 state))
      #'(lambda (state) (TH_move 4 2 state))
      #'(lambda (state) (TH_move 4 3 state))))

;******************************************************************************
; Format functions
;******************************************************************************

; The operations will return their descriptions when called with an argument
; of -1 which is never a legal state.

(defun TH_show_op (op)
   (funcall op -1))

; Use the default printer format for the state -- it's sufficiently easy
; to read.  It's possible to make "stacks" of numbers representing the
; rings on each peg, with a little effort, but I don't have the time.

(defun TH_show_state (state)
   state)

;******************************************************************************
; Main program
;******************************************************************************

(defun hanoi ()
   (let*
      (; Get search parameters.
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

      ; Call blind-search.
      (blind-search
         ; First arg is the problem list, which contains the initial state, the
         ; goal function, which is a closure that tests against the end-num,
         ; and the above functions that provide the list of operations and show
         ; a state an operation.
         (list
            ; Ask user for number of rings and form initial state.
            (TH_get_initial_state)
            ; Goal function.
            #'TH_goal_test
            ; Function to return the op-list.
            #'TH_ops
            ; Function to format a state (but not much...)
            #'TH_show_state
            ; Function to get the description that goes with an operation.
            #'TH_show_op)
         ; Remaining args are keyword args.
         :search-type search-type
         :repeated-state-handling excl-type
         :maximum-depth max-depth)))

