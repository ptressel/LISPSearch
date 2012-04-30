;*******************************************************************************
; alphabeta2 -- minimax with alpha-beta pruning
;*******************************************************************************

; Function alphabeta is a tool for finding good moves in two-player games.
; It implements minimax search with alpha-beta pruning.  In many games, the
; same situation can be reached by multiple paths, so alphabeta also caches
; previously-seen states and their values.
;
; This version accepts functions to replace the <= and >= tests done in the
; max and min steps, so that the caller can change their sense.
;
; Note that checking for repeated states, and short-circuiting evaluation,
; is the business of the caller.

;*******************************************************************************
; USAGE
;*******************************************************************************

; The caller passes to alphabeta an initial state, a termination (goal or
; cutoff) test, a state evaluation function and the range of possible values,
; a successors function, a "max" test, and a "min" test.
;
; "alphabeta2" returns a list containing the highest potential value and the
; successor that leads to it.
;
;   (alphabeta2 initial-state term-eval-fn
;               player-successors-fn opponent-successors-fn
;               equivalence-fn max-fn min-fn
;               low-value high-value)
;
; State internals are known only to the caller.

; "term-eval-fn" should take a state and a search depth as arguments and
; return a list whose first element is T if the search should end on this
; state, which would be the case if it were a goal state, but also if the
; caller knew this path didn't need to be followed further, or wanted to quit
; due to depth.  The caller might save a sequence of states where depth
; increases by one, and do some sort of quiescence check.  (That's not very
; convenient, and if time permits, some sort of quiescence callback might be
; implemented, perhaps by saving the current search path and passing it to a
; quiescence checking function.)  The function should return NIL in the first
; element for non-terminal states.  The second element should be the value,
; if the state is terminal.  It's ignored for non-terminals.  The value should
; be strictly within the range (low-value, high-value).  (Note: the evaluation
; and termination functions are combined for the convenience of the caller,
; who may need to repeat the same work if they were separate.)

; "player-successors-fn" should take a state as argument and return a list of
; states that can follow the given state by means of a move made by the player.
; "opponent-successors-fn" is the same for moves by the opponent.  These two
; are split so that the caller can curry their generic successors function with
; the identity of the side making the move.  If it's more convenient to package
; the side-to-move with the state, and feed that to the generic function, just
; supply the generic function in both places.

;*******************************************************************************
; IMPLEMENTATION NOTES
;*******************************************************************************

; Alphabeta does a modified depth-first search, using recursion rather than an
; explicit queue.  After initialization, functions max-search and min-search
; call each other alternately.  "max-search" quits if it encounters a possible
; "worse" score for the initial player (i.e. a lower score) along its current
; search path, than its "best" score over previous completed paths.
; "min-search" does the opposite -- it quits if it finds a "worse" score for
; the *opponent*, i.e. a higher score for the initial player, than the
; previously found "best" score for the opponent.

; Note that max-search and min-search could be combined, by passing in the
; appropriate value test functions "max" or "min" and the appropriate alpha-
; beta cutoff test "<=" or ">=", and by swapping alpha and beta on each
; nested call.  I'm not going to change this now...

;*******************************************************************************
; GLOBAL PARAMETERS AND VARIABLES
;*******************************************************************************

; The following are global to avoid cluttering up argument lists with things
; that really are global...  So sue me.

; Appease fussy LISPs by declaiming all globals to be special.  Note that these
; declaims are at top level, so they apply to anything loaded after this file.
; Could declaim them anywhere at top level, but this file is convenient because
; it will be loaded with any game-specific file.

; SEARCH STATISTICS ************************************************************

; Global variables are used to count the total number of nodes visited and the
; cpu time.  We may need wall-clock time later, for games with wall-clock time
; limits, like chess.  But issues of fairness arise, if the machine we're
; running on carries other load, and if, in particular, our opponent is running
; on the same machine, and continues to run while it's our turn!

(declaim (special *total-nodes-evaluated* *cpu-time* ))

; Count the nodes as they are visited -- this is a measure of search efficiency.
; The appropriate place to do this is in the search routines at the point where
; they process the node.  The caller should clear this variable whenever they
; want to start over with counting nodes, as follows.
;
;   (setf *total-nodes-evaluated* 0)

; Collect the cpu time at the outset and again at the end -- subtract to get the
; total time.  Since we'll use our process's runtime, this is a fair measure
; even if we're playing an opponent program that is running on the same machine,
; and even if there are pauses while someone else makes a move.  The caller
; should clear this variable whenever they want to start over with counting
; time, as follows.
;
;   (setf *cpu-time* 0)

;*******************************************************************************
; REPEATED-STATES EXCLUSION
;*******************************************************************************

; This shows how a hashtable can be used to store states for the repeated-states
; check.  A value or list of values is stored against the state, which is the
; key.  These use the equivalence-fn supplied by the caller.

(defun exclude-if-seen (state equiv-fn)
   ; If we don't have a hash table, make one.
   (or (hash-table-p *already-seen*)
       (setf *already-seen* (make-hash-table :test equiv-fn)))
   ; If entry with this state is present, return its values, else return NIL.
   (gethash state *already-seen*))

(defun store-if-not-seen (state equiv-fn values)
   ; If we don't have a hash table, make one.
   (or (hash-table-p *already-seen*)
       (setf *already-seen* (make-hash-table :test equiv-fn)))
   ; If entry with this state is present, return NIL, else store the new values
   ; and return T.
   (cond ((not (gethash state *already-seen*)) NIL)
         ; The setf returns T, so this acts as an "else" clause.
         ((setf (gethash state *already-seen*) T) values)))

;*******************************************************************************
; SEARCH FUNCTIONS
;*******************************************************************************

; "max-search" and "min-search" are called with the same arguments
; as "alphabeta".  "max-search" returns a pair -- the value of the best
; choice for the player, and the corresponding state.  "min-search" only
; returns the "best" value for the opponent -- we don't need the opponent's
; actual choice.  "max-search" only returns the state so we can report it at
; the top level.  An alternative would be to have "max-search" return only
; the value, and perform the top-level successor expansion in "alphabeta"
; itself.  But then we'd be duplicating the "max-search" code in "alphabeta",
; which is bad for maintenance.  Maintaining the state in "max-search" isn't
; too much overhead.  On the other hand, time is critical, and this *is*
; Lisp -- no speed demon.  Not a big deal -- this function can just be turned
; into the top-level "alphabeta", and a stripped down "max-search" added later.

; How does the alpha-beta cutoff work?  At any time, alpha is the best score
; the player has found in previous completed paths.  Beta is the "best" the
; opponent can do (i.e. the worst score they can force for the player).  (The
; opponent's play doesn't extend to the top level, else alpha and beta would
; be the same...)  Say we're at a max node below the top level, and say we've
; already done a bit of exploring, so we have real alpha and beta values.
; If, while examining successors, we encounter a score that's >= the "best"
; (lowest) score we know the opponent can guarantee, then we know that the
; min node above this max node will never choose this path, so we can quit
; working on it and ditch the entire subtree depending from this max node.
; Now say we're at a min node (these always have parents since a max node is
; always at the top).  If we encounter a score that's <= the best (highest)
; the player has found so far, we know the parent max node will never choose
; this path, so, again, we can quit now.  Ow, my head hurts.

(defun max-search (state term-eval-fn player-fn opp-fn max-fn min-fn
                   alpha beta)

   ; Count any node that gets a search function called on it.
   (incf *total-nodes-evaluated*)

   ; Check if this state is terminal, and get its value.
   (let ((term-eval (funcall term-eval-fn state)))
      (if (first term-eval)

         ; This is a terminal state, so return its value and the state itself.
         (list (second term-eval) state)

         ; Otherwise, we need to expand this state:
         ; Step through successors, and get the "best" (highest) value and the
         ; successor that yields it, or stick with the original alpha.  Note:
         ; when we start out, alpha will be set to the low end of the value
         ; range, and so will be replaced when we hit a terminal on the very
         ; first path followed.  When it backs up and tries alternate paths
         ; below the top level, we don't care about recording the best
         ; successors, but we do want to know if there were better values
         ; possible.  When we get back up to the top level, the first successor
         ; will be recorded, along with; the best value in this whole subtree.
         ; In later subtrees, we don't want to replace the previous best
         ; successor at the top level unless its subtree yielded a better value.
         ;  Again, we don't bother returning anything for the successor unless
         ; we found a better one.
         (reduce

            ; Function that selects the better of the previous best
            ; (value successor) pair and the next successor.  Here, "better"
            ; is the state with the *higher* value, because it's "our"
            ; player's turn.  We start out the reduce with a "fake" state whose
            ; value is the supplied alpha.  At the top level, where we actually
            ; want to know which successor had the highest value, alpha will be
            ; set initially to the low end of he value range, and so will be
            ; replaced by a real value, and the fake state by a real one.
            #'(lambda (current-pair next-successor)
                 (let* ((current-value (first current-pair))
                        (next-value
                           (min-search next-successor term-eval-fn
                                       player-fn opp-fn equiv-fn
                                       current-value beta ab-flag)))
                    (if (funcall max-fn current-value next-value)
                       ; If we're sticking with the old value, we don't need to
                       ; test it against beta, nor do we alter alpha.
                       current-pair
                       ; If we have a new
                       ; value, check whether our parent min node will ever
                       ; consider this subtree.  Since the test for doing
                       ; pruning or not is performed every time, one might want
                       ; instead to split this into two functions, if one is
                       ; very concerned about performance.
                       (if (funcall max-fn next-value beta)
                          (return-from max-search (list beta NIL))
                          (list next-value next-successor)))))

            ; List of successors after a move by "our" player.
            (funcall player-fn state)

            ; Start with the supplied alpha.  State is ignored unless we're at
            ; the top level, where it'll never return NIL (see above).
            :initial-value (list alpha NIL)))))

; I was having some arithmetic problems, and so, being unsure that the "reduce"
; was doing what I wanted, I tried the following in its place.  It's straight
; procedural code, very non-Lisp.  Fortunately, the reduce wasn't the problem,
; so I didn't have to keep this.  Left it in for the shudder effect...
;
;        (progn
;           (setf successor-list (funcall opp-fn state))
;           (setf next-successor NIL)
;           (loop
;              (setf curr-successor next-successor)
;              (setf next-successor (car successor-list))
;              (cond ((null next-successor)
;                        (return-from min-search (list alpha curr-successor))))
;              (setf successor-list (cadr successor-list))
;              (setf alpha (min-search next-successor term-eval-fn
;                                      player-fn opp-fn max-fn min-fn
;                                      alpha beta))
;              (cond ((>= alpha beta)
;                        (return-from min-search (list beta NIL)))))))))

; "min-search" is the flip-side of "max-search" -- it's from the opponent's
; point of view, so we're looking for the smallest value, and we abandon the
; subtree if we see a value <= alpha.  "min-search" doesn't need to remember
; which successor yields the "best" value.

(defun min-search (state term-eval-fn player-fn opp-fn max-fn min-fn
                   alpha beta)

   ; Count any node that gets a search function called on it.
   (incf *total-nodes-evaluated*)

   ; Check if this state is terminal, and get its value.
   (let ((term-eval (funcall term-eval-fn state)))
      (if (first term-eval)

         ; This is a terminal state, so return its value.
         (second term-eval)

         ; Not terminal, so we need to expand this state:
         ; Step through successors, and get the "best" (lowest) value, or stick
         ; with the original beta.  When we start out, beta will be set to the
         ; high end of the value range, and so will be replaced as soon as we
         ; get a real value.

         (reduce

            ; Function that selects the better value, which, here, is the lower
            ; value, because it's the opponent's turn.  We start out the reduce
            ; with the supplied beta.
            #'(lambda (current-value next-successor)
                 (let ((next-value
                          (first (max-search next-successor term-eval-fn
                                             player-fn opp-fn equiv-fn
                                             alpha current-value ab-flag))))
                    (if (funcall min-fn current-value next-value)
                       ; If we're sticking with the old value, we don't need to
                       ; test it against alpha, nor do we alter beta.
                       current-value
                       ; If we have a new value, check whether our parent max
                       ; node will ever consider this subtree.
                       (if (funcall min-fn next-value alpha)
                          (return-from min-search alpha)
                          next-value))))

            ; List of successors after a move by opponent.
            (funcall opp-fn state)

            ; Start with the supplied beta.
            :initial-value beta))))

;        (progn
;           (setf successor-list (funcall opp-fn state))
;           (loop
;              (setf next-successor (car successor-list))
;              (cond ((null next-successor) (return-from min-search beta)))
;              (setf successor-list (cadr successor-list))
;              (setf beta (first (max-search next-successor term-eval-fn
;                                            player-fn opp-fn max-fn min-fn
;                                            alpha beta)))
;              (cond ((<= beta alpha) (return-from min-search alpha))))))))

;*******************************************************************************
; TOP LEVEL
;*******************************************************************************

; "alphabeta" is the starting point.  After initializing the statistics,
; it calls max-search with alpha and beta set to their extremes.

(defun alphabeta (init-state term-eval-fn player-fn opp-fn
                  max-fn min-fn low-value high-value)
   ; Clear counts.
   (setf *total-nodes-evaluated* 0)
   (setf *cpu-time* 0)

   ; Get and save the result.
   (setf retval
      (max-search init-state term-eval-fn player-fn opp-fn
                  max-fn min-fn low-value high-value))

   ; Report counts -- commented out so as not to confuse xboard.  Counts are
   ; available anyway at the read-eval-print prompt.
   ;
   ; (format t "~&Total nodes evaluated: ~A~%Cpu time: ~A~%"
   ;    *total-nodes-evaluated*
   ;    (/ (- (get-internal-run-time) *cpu-time*)
   ;       internal-time-units-per-second))

   ; Return the result.
   (return-from alphabeta retval))

