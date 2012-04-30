;*******************************************************************************
; MONKEY AND BANANAS
;*******************************************************************************

; The universe consists of named positions, each of which can contain a box,
; bananas, and/or a monkey.  The monkey may be holding the box, or may climb
; on the box.  There may not be any more than one of these things in a position
; (e.g. the monkey cannot move to a position with a box if he is holding a box).
; The monkey can move directly from any position to any other.

; Contrary to the original statement of the problem, the monkey is not truly
; "blind", or if he is, he has access to a Braille map of the universe.  This
; must be so because the monkey is allowed to plan his moves.  That is, the
; search algorithm backtracks without cost.  If the monkey were truly unable
; to plan his moves, he'd have to actually *perform* them, making destructive
; changes in the universe, and to get back to a prior state, he'd have to make
; appropriate moves to put things back the way they were.  His path would not
; be just the final direct path with all the backtracking pruned out -- it
; would be *at least* the whole traversal, and possibly more, if some moves
; were not reversible, or if the monkey's search algorithm happened not to
; backtrack directly.

; The monkey can succeed in getting the bananas if 1) the monkey is in a
; position that contains bananas, 2) the monkey is not holding a box, and
; the monkey's height plus -- if there's a box at that position and the monkey
; is standing on the box -- the height of said box, is at least as great as the
; banana height.  It is not required that there be a box at the position, nor
; is it required that the monkey be on a box -- the monkey may be tall enough
; to reach the bananas on his own.

; In order to use the blind-search package, one must provide a definition of a
; state (or, more precisely, various support functions relating to the state),
; an initial state, a goal-testing function, a function that produces a list of
; operations, and print formatting functions for states and operations.

;*******************************************************************************
; STATE
;*******************************************************************************

; A state is a list including fields for both the monkey's state and the state
; of the positions:
;
;   (position-list monkey-position monkey-box-height monkey-on-box)
;
; The monkey's position will be stored as a position name rather than as a
; pointer to the position within the list.  This is because the state is going
; to be changed non-destructively, i.e. when a new state is generated LISP may
; need to replace all or part of the position-list.  So pointers into parts of
; the position-list that had been superceded would see stale data.

; Helper functions retrieve the pieces of the state:

(defun position-list (state) (first state))
(defun monkey-position (state) (second state))
(defun monkey-box-height (state) (third state))
(defun monkey-on-box (state) (fourth state))

; The monkey's height is part of the state, but since it doesn't change it
; doesn't have to take up space in the state list.  I'm going to be lazy, and
; let it be a global.  For documentation purposes, setf it here.

(setf *monkey-height* 0)

; The position-list is a list of lists, where each sublist represents one
; position, and consists of:
;
;   (position-name box-height banana-height)
;
; where position-name is a symbol supplied by the user, box-height is the height
; of a box set down at that position, or 0 if none, and banana-height is the
; height of any bananas at that position, or 0 if none.
 
; Helper functions retrieve the fields in a postion:

(defun position-name (position) (first position))
(defun box-height (position) (second position))
(defun banana-height (position) (third position))

; This function locates and returns the position triple for the specified name.

(defun find-position (name pos-list)
   (find-if
      ; Find-if needs a predicate that will compare the supplied name against
      ; the name in a position.
      #'(lambda (position) (equal (position-name position) name))
      ; Find-if's second arg is the list to run the test on.
      pos-list))

;*******************************************************************************
; INITIAL STATE
;*******************************************************************************

; We let the user enter the initial state, by means of some input routines.

; Read in a triple of position name, box height, and banana height, and return
; them as a list.

(defun getpos ()
   (list 
      (read (format t "~&Position name: "))
      (read (format t "~&Height of box at this position (0 means no box): "))
      (read (format t
         "~&Height of bananas at this position (0 means no bananas): "))))

; We're going to let the user specify the number of positions at the outset.
; Since we'll know in advance how many positions there are, it would be nice
; to have a recursive position reading routine that will stop when it gets to
; that many.  Define getposn, which takes a number as arg, and returns NIL if
; the number is zero, else returns a list composed of the result of calling
; getpos and the result of calling itself with number-1.

(defun getposn (number-remaining)
   (cond ((zerop number-remaining) NIL)
         (t (cons (getpos) (getposn (1- number-remaining))))))

; Now use this to read a user-specified number of positions and heights, as
; well as the monkey's initial position.  We'll assume the monkey is not
; holding a box initially.  Return the information in the form of a state list.

(defun get-initial-state ()
   ; Get the monkey's height and store it in the previously-declared global.
   ; This is not an entirely natural order, because we ask about other monkey
   ; state items below.  But it avoids having to do a let to save the state
   ; list around the setf, since it's the list we want to return.  This setf
   ; is pure side-effect.
   (setf *monkey-height* (read (format t "Monkey's height: ")))
   ; Get the part of the state that's stored in the state list.
   (list
      ; First element in a state is the position list.
      (let ((total-number
               (read (format t "~&Total number of positions: "))))
         (getposn total-number))
      ; Next is the monkey's position, which the user specifies as a position
      ; name.
      (read (format t "~&Monkey's starting position: "))
      ; Next is the height of a box held by the monkey -- we set this to 0.
      0
      ; Last is a flag telling if the monkey's on a box -- always NIL to start.
      NIL))

;*******************************************************************************
; GOAL FUNCTION
;*******************************************************************************

; The goal function tests whether the the sum of the monkey's height and the
; box's height is at least equal to the banana height.  We require that the
; monkey set his box down before he can reach for bananas.  If there is no box
; at the position, the monkey can still reach for the bananas.  Climbing on the
; box is a separate operation, although it could be implicit in trying to get
; the bananas.  But that would leave a smaller variety of operations.

(defun get-bananas (state)
   ; Pull out the monkey's position triple.
   (let ((monkey-pos
            (find-position (monkey-position state) (position-list state))))
      ; Check that all the conditions are met.
      (and 
         ; There must be bananas here.
         (not (zerop (banana-height monkey-pos)))
         ; The monkey must not be holding a box.
         (zerop (monkey-box-height state))
         ; The monkey's height plus the height of any box set down in the
         ; position with the monkey standing on it must be at least as high as
         ; the bananas.
         (>= (+ *monkey-height*
                (if (monkey-on-box state) (box-height monkey-pos) 0))
             (banana-height monkey-pos)))))

;*******************************************************************************
; OPERATIONS
;*******************************************************************************

; There are four types of operations:  Pick up a box, set down a box, climb on
; a box, and move from one position to another.  The first three can be
; defined without knowledge of the available positions.

; All operations return NIL if they fail, or the new state if they succeed.

; Text in the documenation string is what will be returned by show-operation.

;*******************************************************************************
; Helpers for pick-up-box and set-down-box
;*******************************************************************************

; We need a helper function that sets a box height in a specified position in
; the position list.  If I had time to be fancy, I'd try to share the beginning
; of the position list between the old and new lists to save memory.  For now,
; I'll just make a whole new list with mapcar.

; First make a helper for the helper, that makes a new position with the new
; height if the position has the desired name, else just returns the old
; position.

(defun set-height-in-pos (pos-name new-height pos)
   (if (equal pos-name (position-name pos))
      (list pos-name new-height (banana-height pos))
      pos))

(defun set-height (pos-name new-height pos-list)
   (mapcar
      ; Make a closure of set-height-in-pos with the specified position and
      ; new height filled in.
      #'(lambda (pos) (set-height-in-pos pos-name new-height pos))
      ; Apply this to each position in pos-list, producing a list that differs
      ; from the original only in the one position triple that has a changed
      ; box height.
      pos-list))

;*******************************************************************************
; pick-up-box
;*******************************************************************************

; Requirements for success of picking up a box are that 1) the monkey must not
; already be holding a box, 2) there must be a box at the monkey's position,
; and 3) the monkey must not be standing on the box.

; State changes are that monkey-box-height is set to the height of the box at
; the monkey's position, and that latter height is zeroed.

(defun pick-up-box (state)
   (let*
      (; Pull out the monkey's position triple.
       (monkey-pos-triple 
          (find-position (monkey-position state) (position-list state)))
       ; And get the position name.
       (monkey-pos (position-name monkey-pos-triple))
       ; Get the height of any box at that position.
       (box-height-here (box-height monkey-pos-triple)))
      ; Test whether this operation is allowed.
      (if (and (zerop (monkey-box-height state))
               (not (zerop box-height-here))
               (not (monkey-on-box state)))
         ; It's ok -- form the new state.
         (list 
            ; Make a new position list with the box height at the monkey's
            ; positon cleared.
            (set-height monkey-pos 0 (position-list state)) 
            ; The monkey's position remains the same.
            monkey-pos
            ; The height of the box that the monkey's holding is the value we
            ; saved from the box at this position before we cleared it.
            box-height-here
            ; We know the monkey is not on the box.
            NIL)
         ; It's not ok -- fail the operation.
         NIL)))

;*******************************************************************************
; set-down-box
;*******************************************************************************

; Requirements for success of setting down a box are that 1) the monkey must
; be holding a box and 2) there must not be a box at the monkey's position.

; State changes are that the height of the box at the monkey's position is set
; to the monkey-box-height, and that latter height is zeroed.

(defun set-down-box (state)
   (let*
      (; Pull out the monkey's position triple.
       (monkey-pos-triple
          (find-position (monkey-position state) (position-list state)))
       ; And get the position name.
       (monkey-pos (position-name monkey-pos-triple))
       ; Save the height of the monkey's box, if any.  I'm saving this because
       ; I don't know that LISP will evaluate the arguments in a function
       ; (specifically, the (list ...) below) in left to right order.  If it
       ; optimizes, it may well not.  If it doesn't evaluate in order, then
       ; this value might be lost before I use it if I don't save it.
       (saved-box-height (monkey-box-height state)))
      ; Test whether this operation is allowed.
      (if (and (not (zerop saved-box-height))
               (zerop (box-height monkey-pos-triple)))
         ; It's ok -- form the new state.
         (list 
            ; Make a new position list with the box height at the monkey's
            ; positon set to the (former) monkey's box height.
            (set-height monkey-pos saved-box-height (position-list state)) 
            ; The monkey's position remains the same.
            monkey-pos
            ; The height of the box that the monkey's holding is now zero.
            0
            ; We know the monkey is not on the box, because it was just holding
            ; a box, and the two are mutually exclusive.
            NIL)
         ; It's not ok -- fail the operation.
         NIL)))

;*******************************************************************************
; climb-on-box
;*******************************************************************************

; Requirements for success of climbing on a box are that 1) the monkey must
; not be holding a box, 2) there must be a box at the monkey's position,
; and 3) the monkey must not already be on the box.

; State change is that the monkey-on-box flag is set to true.

(defun climb-on-box (state)
   (let*
      (; Pull out the monkey's position triple.
       (monkey-pos-triple
          (find-position (monkey-position state) (position-list state)))
       ; And get the position name.
       (monkey-pos (position-name monkey-pos-triple)))
      ; Test whether this operation is allowed.
      (if (and (zerop (monkey-box-height state))
               (not (zerop (box-height monkey-pos-triple)))
               (not (monkey-on-box state)))
         ; It's ok -- form the new state.
         (list
            ; There's no change to the position list.
            (position-list state)
            ; The monkey's position remains the same.
            monkey-pos
            ; We know the monkey's not holding a box.
            0
            ; The monkey is now on the box.
            T)
         ; It's not ok -- fail the operation.
         NIL)))

;*******************************************************************************
; climb-off-box
;*******************************************************************************

; Requirement for success of climbing off a box are that the monkey must be on
; a box.

; State change is that the monkey-on-box flag is set to NIL.

(defun climb-off-box (state)
   (let*
      (; Pull out the monkey's position triple.
       (monkey-pos-triple
          (find-position (monkey-position state) (position-list state)))
       ; And get the position name.  Yes, I could compact these two lets into
       ; one in this routine, but I'm leaving it the same as in the preceding
       ; three routines for consistency, and also 'cause I cut'n'pasted the
       ; code.  ;-)
       (monkey-pos (position-name monkey-pos-triple)))
      ; Test whether this operation is allowed.
      (if (monkey-on-box state)
         ; It's ok -- form the new state.
         (list
            ; There's no change to the position list.
            (position-list state)
            ; The monkey's position remains the same.
            monkey-pos
            ; We know the monkey's not holding a box.
            0
            ; The monkey is no longer on the box.
            NIL)
         ; It's not ok -- fail the operation.
         NIL)))

;*******************************************************************************
; move template
;*******************************************************************************

; The list of move operations depends on the position list entered by the user,
; so we need to create it after we've read that in.

; This function takes a position name and makes a move function that moves the
; monkey to that position and returns the modified state, or fails and returns
; NIL.

; Requirements for success of a move are 1) the monkey must not already be at
; the specified position, 2) the monkey must not be on a box, and 3) if the
; monkey is holding a box, there must not be a box at the destination.  Note
; that it is *not* a requirement that the desired position exist because we
; know it does -- it will be built into the function based on a value gotten
; from the position list.  So if the position were wrong, it would be a bug
; internal to this program, not a mistake by some caller or user of this
; program.

; State change is that the monkey position is changed to the desired value.

(defun make-move (new-pos)
   ; Our return value is a function that takes the state as an arg, as do
   ; all operation functions.
   #'(lambda (state)
        ; Test whether this operation is allowed.
        (when (and (not (equal new-pos (monkey-position state)))
                 (not (monkey-on-box state))
                 (or (zerop (monkey-box-height state))
                     (zerop (box-height
                               (find-position new-pos (position-list state))))))
           ; It's ok -- form the new state.
           (list
              ; There's no change to the position list.
              (position-list state)
              ; The monkey's position becomes new-pos.
              new-pos
              ; Whether the monkey's holding the box is unchanged.
              (monkey-box-height state)
              ; We know the monkey is not on the box.
              NIL))))

; Package a move made with make-move up with an appropriate text description.
; Argument of this function is a position triple, not a name, so we cam mapcar
; it over the position list.

(defun make-move-and-name (new-pos-triple)
   ; Get the position name.
   (let ((new-pos (position-name new-pos-triple)))
      ; Make a pair consisting of the move function and a text description.
      (list (make-move new-pos) (format NIL "Move to ~A" new-pos))))

;*******************************************************************************
; operator list generator
;*******************************************************************************

; This function takes a state and makes a list of operation functions.

; Once defined, the list of operators is static -- it does not depend on any
; changes in the state, because the available positions do not change. 
; So, we can make one copy of the list at the outset and store it.

(defun make-op-and-name-list (state)
   ; First list the ops that don't depend on the state.
   ; *NOTE*:  Monkey's behavior is dependent on the order of these
   ; operations, *especially* under a depth-first search, where it may not
   ; get a chance to find a shorter path.  It might be interesting to allow
   ; the user to rearrange them without editing this program.
   ; (Why, you may be asking yourself, have I got all these conses instead of
   ; making a list of the fixed functions and another of the variable functions
   ; and appending them?  I'm not because that would leave a little orphan list
   ; of the fixed functions.  LISP would presumably garbage collect it, but
   ; this is a "free" LISP, and it likes to core if it runs out of memory, so
   ; I don't want to trust it for too much.
   (cons (list #'pick-up-box "Pick up box")
      (cons (list #'set-down-box "Set down box")
         (cons (list #'climb-on-box "Climb on box")
            (cons (list #'climb-off-box "Climb off box")
               ; Now add on entries for the move operations.
               (mapcar #'make-move-and-name (position-list state)))))))

;*******************************************************************************
; SHOW-STATE and SHOW-OPERATION
;*******************************************************************************

; Given how much time I spent on the search routines, and how little time there
; is left, I'm not going to get fancy here.  For instance, I'm going to let
; LISP print out the state however it pleases.  So I'll just return the raw
; state instead of formatting it with pretty (but paper-wasting) verbiage.
; It's simple enough to read anyway.

(defun show-mb-state (state) state)

; A list containing two-element lists consisting of an operation and its text
; description will be constructed in the main program below.  Show-operation
; will then be made as a closure using that list.  Here, we define a helper
; function that matches the operation in such a list and returns the text.

(defun second-of-pair (first-of-pair pair-list)
   (second (find-if
              #'(lambda (pair) (equal first-of-pair (first pair)))
              pair-list)))

;*******************************************************************************
; MONKEY AND BANANAS main program
;*******************************************************************************

; Get input from the user (search type, etc., positions, etc.) and run the
; search.

(defun monkey-and-bananas ()
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
       (max-depth (read (format t "~&Maximum search depth? ")))
       ; Get the initial state, and along the way, get the monkey height and
       ; put it in *monkey-height*.
       (initial-state (get-initial-state))
       ; Make a list of pairs of operations and their text descriptions.
       ; %&$*@^! LISP wouldn't let me put the text in the documentation strings.
       (op-and-name-list (make-op-and-name-list initial-state))
       ; Make a list with just the ops.
       (op-list (mapcar #'first op-and-name-list)))

      ; Call blind-search.
      (blind-search
         ; First arg is the problem list, which contains the initial state, the
         ; goal function, which is a closure that tests against the end-num,
         ; and the above functions that provide the list of operations and show
         ; a state an operation.
         (list
            ; Guess what this is?
            initial-state
            ; Goal function.
            #'get-bananas
            ; Function to return the op-list.  Must take state as an arg, but
            ; ignores it.
            #'(lambda (state) op-list)
            ; Format a state (but not much...)
            #'show-mb-state
            ; Function to get the text string that goes with an operation.
            #'(lambda (op)
                 (second-of-pair op op-and-name-list)))
         ; Remaining args are keyword args.
         :search-type search-type
         :repeated-state-handling excl-type
         :maximum-depth max-depth)))

