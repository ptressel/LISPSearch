; The expand function takes a node and list of operations as arguments, and
; produces a list of successor nodes.  (Note that expand is "perpendicular"
; to mapcar, which applies one function to a list of variables).  Expand
; does not weed out duplicate successor states (see next paragraph).

(defun expand (node ops)
   (cond ((null ops) NIL)
         (t (progn
               ; Count the new node we're about to create.
               (incf *total-nodes-created*)
               ; Use the first operation in the list to make a new state, and,
               ; if it leads to a valid state, create a node containing it and
               ; tack it onto whatever nodes get made using the rest of the ops.
               (let ((successor (successor-node node (first ops))))
                  (if successor 
                     (cons successor
                           (expand node (rest ops)))
                     (expand node (rest ops))))))))

; An earlier version of this routine removed duplicate successor states.
; But this is a waste of time:  The user is going to ask for some exclusion
; method.  The only one that would require taking out duplicate successors
; is the full check against any previously visited state.  But that will be
; handled with much less overhead by the hash lookup, so it's unnecessary to
; check here.  For the other exclusion methods, this is beyond what the user
; has requested.  Presumably they have a reason for their choice, and the
; equivalence test is expensive, so don't sneak it in.
;
;(defun expand (node ops)
;   (cond ((null ops) NIL)
;         (t (let (first-successor (successor-node node (first ops)))
;                 (rest-of-successors (expand node (rest ops)))
;               (cond ((member first-successor rest-of-successors)
;                         rest-of-successors)
;                     (t (cons first-successor rest-of-successors)))))))

