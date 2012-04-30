;*******************************************************************************
; tictactoe -- plays tic-tac-toe
;*******************************************************************************

; Function tictactoe uses minimax with alpha-beta pruning to play tic-tac-toe.

;*******************************************************************************
; USAGE
;*******************************************************************************

; Call tictactoe with no arguments:
;
;   (tictactoe)
;
; Tictactoe will first display the board layout, showing the numbers that
; designate the squares (see below).
;
; It will then prompt for two or three values:
;
; First is whether we should do alpha-beta pruning -- answer T or NIL.
;
; Next is the marker that tictactoe will play -- either "X", if tictactoe is
; supposed to play first, or "O", if it is the second player.
;
; If tictactoe is to play first, then it will ask if it should make a
; particular initial move -- enter either a square number, or NIL to let
; tictactoe make its own initial move.
;
; Thereafter, tictactoe will prompt for moves by its opponent, and display the
; board after its own moves.  Moves are specified (as for the optional initial
; move) by giving the square number of the desired square.

;*******************************************************************************
; BOARD REPRESENTATION
;*******************************************************************************

; A state is a particular layout of Xs and Os on a 3x3 board.  The state is
; represented by two integers, one for X and one for O, with bits on in
; positions that represent squares.  With bits numbered from the low end of
; the integer, map bits to squares as follows:
;
;     1 | 2 | 3
;     ---------
;     4 | 5 | 6
;     ---------
;     7 | 8 | 9
;
; We don't number a square zero, because it looks like an O.  This has the
; annoying side-effect of making square number one greater than the
; corresponding bit position.  But it's kinder to the opponent this way --
; it allows displaying the board with Xs and Os shown, and numbers for the
; empty squares, to make it easier to see what the move numbers are.

(defun bitpos (square) (- square 1))

; We need 9 bits in each of these integers.  (It's not necessary to inform
; Lisp of this, but giving it a fixed size equal to the machine's wordsize
; could save a bit of runtime.  On the other hand, if we tell it exactly 9
; bits, it could sign-extend, thinking that it has a negative number, when
; we use the "ash" function to shift bits to the right, as we'll do while
; checking for equivalent rotated or reflected board layouts.)
;
; A blank board is a zero:

(declaim (special *empty-board*))
(setf *empty-board* #b000000000)

;*******************************************************************************
; STATES
;*******************************************************************************

; For purposes of documenting our moves, we'll want to keep the move we made
; to get to that state.  So in addition, we'll keep two values, one for a
; square played by X and one for O -- one of these will be NIL, meaning that
; side didn't play this turn, and the other will be the square number just
; played.
;
; In order to evaluate states, we need to know who is "us" and who is "them".
; We will always list our own board and move first.
;
; So a state will consist of:
;
;   ( ourboard theirboard ourmove theirmove ourmarker )
;
; where *move are for documentation and to tell more easily who moved last
; (though this could also be gotten by counting the bits that are on in each
; *board) -- they are not used in checking whether two states are equivalent
; (for the repeated-states check).  "ourmarker" is our symbol -- the X or O
; supplied in the tictactoe call.

(defun ourboard   (state) (first state))
(defun theirboard (state) (second state))
(defun ourmove    (state) (third state))
(defun theirmove  (state) (fourth state))
(defun ourmarker  (state) (fifth state))

; The initial state, if we're X and are about to make a move, is:
;
;   ( *empty-board* *empty-board* NIL NIL 'X )

; To add a move, given by a square number, to a board, OR in a bit in the
; position given by that number.

(defun addmove (board square)
   (logior board (ash 1 (bitpos square))))

; Test a move for legality (i.e. be sure square isn't occupied).

(defun notallowed-p (state square)
   (logbitp (bitpos square) (logior (ourboard state) (theirboard state))))

;*******************************************************************************
; DISPLAY FUNCTIONS
;*******************************************************************************

; A function to print an X, an O, or a space, depending on which of two boards
; (if either) has a one in a given location:

(defun XorO (xboard oboard square)
   (cond ((logbitp (bitpos square) xboard) 'X)
         ((logbitp (bitpos square) oboard) 'O)
         (T square)))

; A function to print the current board contents, given a state:

(defun printstate (state)
   (let*
      (; Get the X and O boards -- if we're X then the X board is first in the
       ; state; if we're O, then the O board is first.
       (boards
          (if (equal (ourmarker state) 'X)
             (list (ourboard state) (theirboard state))
             (list (theirboard state) (ourboard state))))
       (xboard (first boards))
       (oboard (second boards)))

      ; Print either X or O in each square, or the square number, if the square
      ; is empty, with appropriate decoration.
      (format t "~%   ~A | ~A | ~A~%   ---------~%   ~A | ~A | ~A~%   ---------~%   ~A | ~A | ~A~%"
         (XorO xboard oboard 1)
         (XorO xboard oboard 2)
         (XorO xboard oboard 3)
         (XorO xboard oboard 4)
         (XorO xboard oboard 5)
         (XorO xboard oboard 6)
         (XorO xboard oboard 7)
         (XorO xboard oboard 8)
         (XorO xboard oboard 9))))
      
;*******************************************************************************
; SUCCESSORS FUNCTION
;*******************************************************************************

; We need a function to generate legal successors to a state.  A legal
; successor is a move by the side that didn't move last, into an unoccupied
; square.  We find the occupied squares by bitwise ORing the two boards in
; the parent state, then take the bit complement to find unoccupied squares,
; and insert each of these bits into the current side's board in turn to make
; the possible next states.  (To start off the game, if we play first, we'll
; fool our "successors" function by setting X's move to NIL and O's to T.)

; Define some helper functions to handle unpacking and repacking states
; depending on whether the side to move is Us or Them.

; Given a board for the side making a move, and the move itself, and the other
; side's board, make a state.  There's one of these for Us and one for Them.

(defun makeourstate (moveboard square otherboard marker)
   (list
      (addmove moveboard square)	; New ourboard
      otherboard			; Same old theirboard
      square				; We make this move,
      NIL				; not them.
      marker))

(defun maketheirstate (moveboard square otherboard marker)
   (list
      otherboard			; Same old ourboard
      (addmove moveboard square)	; New theirboard
      NIL				; We don't make this move,
      square				; they do.
      marker))

; These are for the convenience of the top-level function -- unpack the
; supplied state, then call the above functions.

(defun makeourstatefromstate (state square)
   (makeourstate (ourboard state) square (theirboard state) (ourmarker state)))

(defun maketheirstatefromstate (state square)
   (maketheirstate
      (theirboard state) square (ourboard state) (ourmarker state)))

; This function does the actual work, given the appropriate unpacking and
; repacking functions.

(defun anysuccessors (state moveboard-fn otherboard-fn makestate-fn)
   (let (; Get the free squares.  (Lisp is using more than the 9 bits, so it
         ; complements the rest of the word -- clip off the unwanted part.
         ; Shouldn't affect operation, but looks strange...
         (free (logand #b111111111
                       (lognot (logior (ourboard state) (theirboard state)))))
         ; Pick out the board of the side making the move.
         (moveboard (funcall moveboard-fn state))
         ; Get the other board and our marker, which we'll use in making states.
         (otherboard (funcall otherboard-fn state))
         (marker (ourmarker state)))

      (do (; Step through possible moves.
            (square 1 (+ square 1))
            ; If this square is free, fill it in, make a state list, and tack
            ; it on to a list of successors.  (Recall first form is initial
            ; value, and second is how the variable is altered on each pass.)
            (successor-state-list '()
               (if (logbitp (bitpos square) free)  ; Is square available?
                  ; Yes, add a state with this square filled in.  We want a
                  ; list of lists, so we have to put on a layer of list for
                  ; append to take off.
                  (append successor-state-list
                     (list (funcall makestate-fn moveboard square
                                    otherboard marker)))
                  ; No, stick with the current list.
                  successor-state-list)))
           ; Loop ends after square 9 -- return the list.
           ((= square 10) successor-state-list))))

; Finally, the actual successors function -- it checks who's moving and
; produces the right call.

(defun successors (state)
   (if (null (ourmove state))
      ; If we didn't move in the parent state, it's our turn now.
      (anysuccessors state #'ourboard #'theirboard #'makeourstate)
      ; Else they're moving this turn.
      (anysuccessors state #'theirboard #'ourboard #'maketheirstate)))

;*******************************************************************************
; STATE EQUIVALENCE
;*******************************************************************************

; Equivalence checking is postponed, in favor of finishing the assignment.
; Some notes are included.

; Tic-tac-toe has eight-fold symmetry: reflection and four rotations (0, 90,
; 180, 270 degrees).  Reflection about any of the reflection symmetry axes
; (vertical, horizontal, either diagonal) can be used -- all are equivalent
; when combined with an appropriate rotation.  So only reflection about the
; vertical axis will be used.

; Board layouts that are the same up to reflection and one of these rotations
; have the same value, so once we find a value for one of the (up to) 8
; equivalent layouts, we can use that value for all.  "Up to" because some
; transformations may lead to identical layouts, e.g. a board with one X in
; a corner has only four equivalents.  So the reduction in nodes expanded
; isn't a full factor of eight.

; It is possible to check equivalence with bitwise operations -- no loops are
; needed except over the eight transformations.

; Vertical reflection consists of swapping bits in the first and third column
; of the board, i.e. 0 and 2, 3 and 5, 6 and 8.  The middle row (1, 4, 7)
; is left alone.

(defun reflect (board)

   ; Get the bits for the three columns alone.
   (let ((bits036 (logand board #b001001001))  ; Select out bits 0, 3, 6
         (bits147 (logand board #b010010010))  ;         "       1, 4, 7
         (bits258 (logand board #b100100100))) ;         "       2, 5, 8

      ; Shift the 036 bits up and the 258 bits down then recombine all of them.
      ; (Lisp "ash" sign-extends on a right shift.  Since the size of the
      ; integer is up to Lisp, and I don't know where it'll consider the high
      ; bit to be, I clear the bits it might have set if it sign-extends from
      ; bit 8.)
      (logior (ash bits036 2)
              bits147 
              (logand #b001001001 (ash bits258 -2)))))

; This is just an example of how the bit representation can be manipulated.
; If there's time, I'll put in the rest.

;*******************************************************************************
; COMBINED TERMINATION AND EVALUATION FUNCTION
;*******************************************************************************

; State values are:
;
;   Win by us: 1
;   Win by opponent: -1
;   Draw, or not a terminal state: 0
;
; If the game is over, i.e. if the state contains either a tic-tac-toe or a
; draw, the termination/evaluation function returns T, and also returns the
; value of the state, else it returns NIL and the value 0.  It must know
; who is the "player" and who is the "opponent" -- this is why the boards are
; ordered in the state with the player first.

; A tic-tac-toe is three Xs or three Os in ; a line, either vertical,
; horizontal, or diagonal.  With the bitwise board representation, three in
; line is any of these bit combinations:

(declaim (special *lines*))
(setf *lines* (list #b000000111    ; bits 0, 1, 2
                    #b000111000    ;      3, 4, 5
                    #b111000000    ;      6, 7, 8
                    #b001001001    ;      0, 3, 6
                    #b010010010    ;      1, 4, 7
                    #b100100100    ;      2, 5, 8
                    #b100010001    ;      0, 4, 8
                    #b001010100))  ;      2, 4, 6

; Determine whether the given board contains three bits on in a line by
; selecting out from the board each of the above bit sets in turn, and
; counting whether there are three bits on in any set.

(defun win (board)
   (reduce
      ; Test one set of bits; if it's a win, quit and return T.  Note that
      ; "result" is always NIL -- if we ever see a win, we exit the whole
      ; function immediately.  If we fall out the end of the reduce with no
      ; win, we return NIL.
      #'(lambda (result line)
           (if (= 3 (logcount (logand line board)))
              (return-from win T)  ; Short-circuit the test if there's a win.
              NIL))                ; Else continue looking.

      ; The bits for all lines.
      *lines*

      ; Assume no win.
      :initial-value NIL))

; Make functions that determine a win separately for us and them.

(defun ourwin (state)
   (win (ourboard state)))

(defun theirwin (state)
   (win (theirboard state)))

; A draw occurs if it's no longer possible for either side to get three in a
; line.  In a given line, three in a line isn't possible if X and O each have
; at least one bit on.  So go through each of the lines for both our and their
; boards, and see if all have bits on in both.  (Note: we could allow play to
; continue after a draw til all squares were filled, at which point it would
; be simple to detect a draw (board full and no win) but what if our opponent
; didn't want to go on playing when there was no point?  We'd have to detect
; when they wanted to quit, and check for a draw anyway.)

(defun draw (state)
   (let ((oneboard (ourboard state))
         (otherboard (theirboard state)))

      ; We start with value T, then go through all lines.  If we find any
      ; that are still winnable, we quit and return NIL.  If we fall out of
      ; the reduce without having found any winnable lines, we return T,
      ; meaning the game is a draw.
      (reduce

         ; Get the same line of bits out of both boards, then see if both have
         ; bits on.  If they don't, we quit and return NIL.
         #'(lambda (result line)
              (if (and (logtest line oneboard) (logtest line otherboard))
                 T   ;Continue if this line has both and X and an O.
                 (return-from draw NIL)))   ; Short-circuit if not a draw.

         ; Do this for all "lines" in the board.
         *lines*

         ; Assume T.
         :initial-value T)))

; If either side wins, or if there's a draw, we're done.

(defun term-eval (state)
   (cond ((ourwin state)   (list T 1))     ; Yay!
         ((theirwin state) (list T -1))    ; Pooh!
         ((draw state)     (list T 0))     ; Hmm...
         (T                (list NIL 0)))) ; Not over yet.

;*******************************************************************************
; GAME DRIVER
;*******************************************************************************

; This is the top-level function.  First, it asks for "our" marker, then if
; we're X, it asks for our initial move.  It then alternately asks the opponent
; for a move, and chooses its own move.  It prints the board after each move.
; It does not go on searching after making its own move, while waiting for the
; opponent to move...

(defun tictactoe ()

   ; Print the square numbering.
   (format T "~%Welcome to Tic-Tac-Toe.  Moves are given as square numbers:")
   (format T "~%~%   1 | 2 | 3")
   (format T "~%   ---------")
   (format T "~%   4 | 5 | 6")
   (format T "~%   ---------")
   (format T "~%   7 | 8 | 9~%")
   (format T "~%Before your moves, I'll show you the board, with X or O in")
   (format T "~%occupied squares, and numbers in the squares that are")
   (format T "~%available for your move.  You'll enter moves by typing")
   (format T "~%the number of an available square, followed by return.~%")

   ; Find out what user wants to do.
   (let*
      (; Ask whether we should do alpha-beta pruning.
       (ab-flag
          (read (format T "~%Should I do alpha-beta pruning? (T or NIL): ")))

       ; Ask for our marker.
       (marker (read (format T "~%Should I play X or O? (X or O): ")))

       ; If we're X, ask for our first move.
       (firstmove
          (if (equal marker 'X)
             (read (format T "~%My first move? (square number, or NIL to let me make the move): "))
             NIL))

       ; If we're X, and weren't told a move, get our first move, else set
       ; up initial state as specified.
       (state
          (if (equal marker 'X)
             (if (null firstmove)
                ; Here, we're X but get to make our own first move.  Since
                ; alphabeta returns a list of a value and selected state,
                ; we need to pull out the state.
                (second
                   (alphabeta 
                      ( list *empty-board* *empty-board* NIL NIL 'X )
                      #'term-eval #'successors #'successors NIL NIL
                      -2 2 ab-flag))
                ; Here, we've been given a first move.
                (list (addmove *empty-board* firstmove)
                      *empty-board* firstmove NIL 'X ))
             ; Here, we're O, so we just set up for their first move.
             (list *empty-board* *empty-board* NIL NIL 'O))))

      ; Here we're set up so that the opponent's move comes next.
      ; Alternately ask for moves and play til we reach a terminal state.
      ; (Yes, this is procedural.  Yes, it uses setf.  I didn't have time to
      ; be fancy here.)
      (loop

         ; Show them the board:
         (printstate state)

         ; Get their move:
         (let
            ((usersmove
                (read (format t
                   "~%Your move? (type an available square number): "))))

            ; Check for legality.
            (if (notallowed-p state usersmove)

               ; Ask them to choose again.  From here, we go back to the top
               ; of the loop, without having changed the state.
               (format t "~%That square is occupied; please choose another.")

               ; Their move is legal -- update the state and see if they won.
               (progn
                  (setf state (maketheirstatefromstate state usersmove))

                  (cond
                     ((theirwin state)
                         (return (format T "~%~%You won!!~%")))
                     ((draw state)
                         (return (format T "~%~%That was a draw.~%")))
                     (T
                        ; Still playing -- make our move and check for a win.
                        (progn
                           (setf state
                              (second (alphabeta state #'term-eval
                                                #'successors #'successors
                                                NIL NIL -2 2 ab-flag)))
                           (cond
                              ((ourwin state)
                                  (return
                                     (format T "~%~%Sorry, I won.~%")))
                              ((draw state)
                                  (return
                                     (format T
                                        "~%~%That was a draw.~%")))))))))))))
