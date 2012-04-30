;*******************************************************************************
; A* SEARCH PACKAGE, astar-search.lsp
;*******************************************************************************

; This collection of functions provides A* search, using user-supplied
; cost and heuristic functions.

;*******************************************************************************
; USAGE
;*******************************************************************************

; To define a "problem", the user must provide several pieces of information
; and several functions, packaged into a list:
;
; (initial-state goal-function successors-function cost-function 
; heuristic-function show-state)
;
; "initial-state" is the state at which the search should start, in whatever
; representation the user chooses for states.
;
; "goal-function" takes a state as argument and returns NIL iff the state is
; not a goal state.
;
; "successors-function" takes a state as argument and returns a list of states
; that can be reached from the given state.  NOTE:  It would be possible to
; implement this by having the user supply a function that returned a list of
; operations, just as the blind search was done.  Here, the operations would
; be transitions from the current state to any other state in the state space.
; The operation would just return NIL if the transition were not possible.
; But the statement of problem 4.13 says we should use a successors function
; instead.  That's why this is a separate file, and not part of the same file
; with all the other searches.
;
; "cost-function" takes two states as arguments, and returns the cost of going
; from the first state to the second.  This is what will be used to update the
; path costs.
;
; "heuristic-function" takes a state as argument and returns the estimated cost
; of the path from the given state to the goal.
;
; "show-state" takes a state as argument and makes a string, formatted for
; printing, that contains whatever information the user wants, to identify that
; state.  The string should conclude with a newline.  Unless the output is very
; short, it should start with a newline as well.
;
; (Note:  It may be useful to have the user supply an equality function for
; states.  Currently, equalp is used to compare states.  But experience with
; the behavior of floating-point numbers and roundoff error indicates that an
; exact match may not be what the user wants.  For instance, all the number
; comparisons in the polygon avoidance program allow a bit of slop.  This search
; will work with a fussier equality than the user might want, but it will
; generate and expand more states than it might have to, if there are some that
; the user's package treats as equivalent.)
;
; The user calls Astar-search, supplying the above items in a problem definition
; list, and receives the goal state as a return value.  As a side effect, the
; goal state, search statistics, and path are printed.
;
; Example of a call:
;
; (Astar-search
;    (start-point 'is-goal 'successors 'cost 'heuristic 'showstate))
;
; Before calling Astar-search, the user must load this package,
; astar-search.lsp, using whatever means are provided by their LISP environment.

;*******************************************************************************
; Global variables
;*******************************************************************************

; Appease fussy LISPs by declaring all globals to be special.
(declaim (special *trace-messages* *total-nodes-created* *total-nodes-visited*
                  *total-nodes-touched* *all-nodes* *open-queue*))

;*******************************************************************************
; Trace messages
;*******************************************************************************

; To enable tracing messages, which will show each step in the execution of the
; algorithm, set *trace-messages* to something non-NIL after loading the
; package.

(setf *trace-messages* NIL)

;*******************************************************************************
; PROBLEM EXTRACTION HELPER FUNCTIONS
;*******************************************************************************

; A problem list contains:
; (initial-state goal-function successors-function cost-function 
; heuristic-function show-state)

; Helper functions extract each portion of the problem.  Inline all the tiny
; routines.

(declaim (inline initial-state goal-fn successors-fn cost-fn heuristic-fn
                 show-state))

(defun initial-state (problem)
   (first problem))

(defun goal-fn (problem)
   (second problem))

(defun successors-fn (problem)
   (third problem))

(defun cost-fn (problem)
   (fourth problem))

(defun heuristic-fn (problem)
   (fifth problem))

(defun show-state (problem)
   (sixth problem))

;*******************************************************************************
; SEARCH TREE
;*******************************************************************************

; What sort of interconnections will we need between nodes?
;
;   - There will only be one minimal *cost* to get to a given point: there
;     may be multiple *paths* with that cost, but as far as we're concerned,
;     they're all equally good, and we can just pick one.  So there is only
;     one path to any node that we need to keep, so we only need storage for
;     one parent pointer.  We can chain back through the parent pointers to
;     see the path -- we do not need to store the path in each node.  In fact,
;     storing the path would be a bad idea, because the path can change.
;
;   - For each visited node, we'll need to know what its successors are.  This
;     is because, if we find a better path to that node, we'll need to traverse
;     the successors (and their successors, etc.) and update all their path
;     costs, reducing them by the difference between the old and new costs to
;     the given node.

; There are three cost-related values that we might need for a given node:
;
;   - the best cost we've found so far to get from the initial state to this
;     state
;   - the heuristic
;   - their sum, f'
;
; Since we can compute the sum when we need it, we might leave that out.  But
; f' is what will be used in ordering the open queue, so leaving it out will
; add a little to the sort time.  On the other hand, the time lost to performing
; an addition is trivial compared to the time lost through doing a sort instead
; of maintaining a heap.  For now, don't store f'.
;
; Adding two numbers is cheap.  But we don't know how costly it is to compute
; the cost from one state to another.  So we *do* store that.  Need to beware
; aliasing -- we update this value, and must be sure to have the newest copy
; of the node in which it's stored.  This is a mis-feature of LISP, as far as
; database-type operations are concerned, where an update to a value is
; *supposed* to be seen by all accessors.

; So, a node in the search tree is represented by a list containing:
;
;   - the parent state
;   - the current state
;   - a list of successors
;   - the path cost so far
;   - the cost from our immediate parent
;   - the heuristic value

; Functions to extract each piece of a node, and print the path represented
; by the node.  The show-state and show-operation functions are supplied as
; part of the problem.  Better would be to make the state and operation objects
; with print methods!

(declaim (inline parent-node state successor-list path-cost cost-from-parent
                 heuristic-value))

(defun parent (node)
   (first node))

(defun state (node)
   (second node))

(defun successor-list (node)
   (third node))

(defun cost-from-parent (node)
   (fourth node))

(defun path-cost (node)
   (fifth node))

(defun heuristic-value (node)
   (sixth node))

; Need to get at the parent *node* of a node through the hash table (see below).
(defun parent-node (node)
   (retrieve-node (parent node)))

; Print the states along the path, with heuristic and costs.
(defun print-path (node problem)
   ; First follow the parents until we reach the root of the tree.
   (if (parent node) (print-path (parent-node node) problem))
   ; As we fall out of the recursion, print the node info.  If we're at the
   ; root node, use different verbiage.
   (if (null (parent node))
      (format t "~&Start: ~A~1,8@THeuristic: ~A~1,8@TPath cost: ~A~%"
        (funcall (show-state problem) (state node))
        (heuristic-value node)
        (path-cost node))
      (format t
        "~&Next:  ~A~1,8@THeuristic: ~A~1,8@TPath cost: ~A~1,8@TMove cost: ~A~%"
        (funcall (show-state problem) (state node))
        (heuristic-value node)
        (path-cost node)
        (cost-from-parent node))))

; Make a new node given all the pieces, and put it in the hash table.
(defun make-new-node (parent state successor-list cost-from-parent path-cost
                      heuristic-value)
   (let ((node (list parent state successor-list cost-from-parent path-cost
                     heuristic-value)))
      ; Count the new or changed node.
      (incf *total-nodes-touched*)
      (when *trace-messages* (format t "~&~A~%" node))
      ; Store-node puts the node in the hash table, indexed by the state (see
      ; below).
      (store-node node)))

; A function to construct a node given a state, its parent node, and the
; problem definition list, then insert the node in the hash table.  (This
; function does *not* update the parent's successor list.  At the point where
; this is called, that has already been done.  Besides, burying a change in
; here would be dangerous -- the caller might go on using the obsolete copy
; of the parent node.  Alternative would be to make destructive changes to
; nodes or never pass nodes, only states, and always look up the node afresh.)
(defun make-new-node-from-parent (state parent-node problem)
   (let* (; Get some values we'll use more than once in this routine:
          ; Parent state
          (parent-state (state parent-node))
          ; Cost of path segment from parent state to this state.
          (cost-from-parent
             (funcall (cost-fn problem) parent-state state)))
      ; This is a new node, not a one that's being modified, so count it.
      (incf *total-nodes-created*)
      (when *trace-messages* (format t "~&Creating new node:~%"))
      ; Construct it and put it in the hash table.
      (make-new-node
         ; Parent state (not parent node)
         parent-state
         ; State
         state
         ; Successor list, currently empty
         NIL
         ; Cost of path segment from parent
         cost-from-parent
         ; Path cost from initial state through parent to this state is sum
         ; of cost to parent, plus cost from parent to this state.
         (+ (path-cost parent-node) cost-from-parent)
         ; Heuristic value
         (funcall (heuristic-fn problem) state))))

; Given a node and a list of states to be its successors, make a new node with
; those successors included in the successor list.  Exclude duplicates.
(defun make-new-successors (node more-successors)
   (when *trace-messages* (format t "~&Updating node with new successors:~%"))
   (make-new-node
      ; Everything except the successor list is the same.
      (parent node)
      (state node)
      ; The successor list is the combined old and new successor lists with
      ; duplicates removed.  It would be fine to do this destructively.
      (remove-duplicates (append (successor-list node) more-successors))
      (cost-from-parent node)
      (path-cost node)
      (heuristic-value node)))

; Make a new node with a changed path cost, due to a change in the parent's
; path cost.  Parent, and cost to parent, remain the same.
(defun make-new-cost (node parent-node)
   (let (; Pull out the cost to parent, because it'll be used twice.
          (current-cost-from-parent (cost-from-parent node)))
      (when *trace-messages*
         (format t "~&Updating node due to change in parent's path cost.~%"))
      (make-new-node
         ; Parent state, state, successor list, cost from parent are the same.
         (parent node)
         (state node)
         (successor-list node)
         current-cost-from-parent
         ; Path cost is new.
         (+ (path-cost parent-node) current-cost-from-parent)
         ; Heuristic is same.
         (heuristic-value node))))

; Make a new node with a different parent, cost to parent, and path cost.
; When we call this, we happen to have already calculated both the cost to
; the new parent and the new path cost through that parent.
(defun make-new-parent (node new-parent new-cost new-cost-from-parent)
   (when *trace-messages*
      (format t "~&Updating node due to change of parent.~%"))
   (make-new-node
      ; Parent state is new.
      (state new-parent)
      ; State, successor list, are the same.
      (state node)
      (successor-list node)
      ; Cost from parent is new.
      new-cost-from-parent
      ; Path cost is new.
      new-cost
      ; Heuristic is same.
      (heuristic-value node)))

;*******************************************************************************
; SEARCH STATISTICS
;*******************************************************************************

; Global variables are used to count various possibly interesting things.

; I'm initializing these variables here using setf, just to announce their
; presence.  LispWorks issues style warnings -- it wants them defvar'd.
; Clisp doesn't care.  Both make the right (and only) assumption that these
; are "special" variables.  I don't yet trust defvar -- I want to know what
; it's *really* doing, and what the difference is between a defvar'd thing
; and whatever I get when I just setf something at the top level.  In
; particular, I want to know if the fact that defvar makes dynamic variables
; causes any difference in behavior.

; The other thing I need to find out is how to specify, in the functions that
; use these variables, that I want to use the global variable, and not any
; dynamic variable of the same name that the user might happen to define.  As
; an imperfect means of avoiding aliasing, I could use long, obscure names for
; the variables, e.g. I should append a prefix like search-package- or SP- or
; some such thing to all of the globals.  (Actually, the same problem applies
; to all the *function* names in the package as well.)

; Count the nodes as they are created -- this is a measure of memory usage.
; The appropriate place to do this is in the enqueue function.
(setf *total-nodes-created* 0)

; Count the nodes as they are visited -- this is a measure of search efficiency.
; The appropriate place to do this is in the search routines at the point where
; they test a node to see if it's a goal node.
(setf *total-nodes-visited* 0)

; Count nodes as they are constructed or reconstructed.  Difference between
; this and *total-nodes-created* is the number of times nodes are modified.
(setf *total-nodes-touched* 0)

;*******************************************************************************
; QUEUEING and STORAGE MAINTENANCE FUNCTIONS
;*******************************************************************************

; The Rich & Knight definition of A* makes use of two places to store nodes,
; "open" and "closed".  Only "open" is a queue in the sense of our other search
; methods, i.e. a place to store generated but unvisited nodes.  "Closed" is a
; holding-pen for visited nodes, in case we happen to reach them again, and
; have found a shorter path to them.
 
; However, A* must locate nodes in both "open" and "closed" that have a given
; state.  Therefore, it will be convenient to store both types of nodes
; together in a hash table.  Since we will never keep more than one node
; containing any given state, then we can index the nodes by their states.
; In order to avoid the risk of aliasing, we should not store nodes in the
; "open" queue.  Instead, we should store only the indices, i.e. the states,
; and look up the nodes in the hash table to get any required node information.
; (The one piece of information we'll most often need is the f' value, because
; that's what we sort on.)  We still need to store the state in the node,
; because we can't get from the node to the index under which its hashed, and
; we will need to get from the node to the state in order to print the path.

; In the following code and comments, since a node is indexed by its state,
; references to "node" will be made in regard to variables that contain state
; information, for instance, the open queue.

;*******************************************************************************
; Hash table & its operations
;*******************************************************************************

; Declare a global variable that will be set to a hash table when we call
; Astar-search.  I suppose I should use defvar for this...

(setf *all-nodes* NIL)

; Find a node in the hash table given its state.  This is also how we test
; whether a node is in the hash table, because gethash with no default
; returns NIL if the state isn't found, but if the state is there, it will
; have a non-NIL value -- the node -- returned.  So there's no need for a
; separate predicate.

(declaim (inline retrieve-node))

(defun retrieve-node (state)
   (gethash state *all-nodes*))

; Put a node in the hash table.  (I don't know if I can use retrieve-node
; inside the setf.  Leave the bare gethash in there for now.)  Although the
; main purpose of this function is its side-effect of updating the hash table,
; it also returns the node, and that value does get used.

(defun store-node (node)
   (setf (gethash (state node) *all-nodes*) node))

; Given a state, look up its node and compute f'.

(defun fprime (state)
   (let ((node (retrieve-node state)))
      (+ (path-cost node) (heuristic-value node))))

;*******************************************************************************
; Queue operations
;*******************************************************************************

; The "open" queue is a priority queue, but due to lack of time, the queue will
; be implemented as a list, and the LISP sort function will be used to get the
; state with the lowest f' value.  A better implementation would use some
; appropriate structure such as a heap.  One advantage to using a list with
; sort is that we can still clip the head off the list with impunity -- we
; couldn't do that with a heap.

; Because we're going to perform destructive operations on the queue anyway,
; and because we never need to preserve any old version of the queue, make it a
; global.

(setf *open-queue* NIL)

; Unlike the previous search package, we'll sort the queue when we need to get
; an element off it, not when we queue up elements.  That runs the sort only
; when we need it.  It may be that there wouldn't be any difference in the
; number of sort calls, but the number of calls won't be *greater* this way.

(declaim (inline sort-open))

(defun sort-open ()
  (sort *open-queue* #'< :key #'fprime))

(defun get-best ()
   ; Yes, I know, eeeeeeuw, yuk, gag, bletch.  Sort the queue, get the head,
   ; behead the sorted queue, stuff it back into the global, and return the
   ; former head.
   (let* ((sorted-open (sort-open))
          (best (first sorted-open)))
      (when *trace-messages*
         (format t "~&Sorted open queue:~%~1,8TState: ~3,8TF':")
         (mapcar
            #'(lambda (state)
                 (format t "~&~1,8T~A ~3,8T~A~%" state (fprime state)))
            sorted-open)
         (format t "~&Get-best selects: ~A~%" best))
      (setf *open-queue* (rest sorted-open))
      best))
     
; Unlike the previous searches, we only queue up one state at a time, so batch
; queueing functions aren't needed -- we can just cons new nodes.  Or perhaps
; they should be appended...don't know which would be better for the sort.
; The new state will likely have a worse f'.

(defun insert-open (state)
   (setf *open-queue* (cons state *open-queue*))
   (when *trace-messages*
      (format t "~&Inserting ~A in open queue.  New queue is:~%~A~&"
              state *open-queue*)))

;*******************************************************************************
; A* SEARCH
;*******************************************************************************

; Based on the A* algorithm in Rich & Knight, p 76, with some simplifications.

;*******************************************************************************
; Loop invariants
;*******************************************************************************

; The information we keep per node that is manipulated by the search algorithm
; is:
;   - parent pointer (which yields the path)
;   - path-cost from initial-state
;   - cost of the step from the parent to the current state
;
; In order to determine if the algorithm or implementation is correct, we need
; to decide what statements about this data must remain true at the end of any
; cycle through the search code.  Here is what we must maintain:
;
; Multiple nodes can have the same node as a successor -- that is, there can be
; more than one path to a node.  The costs of these paths can, and likely will,
; differ.  Because our purpose is finding the least cost path, then we must be
; sure that:
;
;      If multiple paths feed into a node, the node's parent will be on the
;      path among these that has the lowest cost.
;
; The relation between costs of a node and its parent must be:
;
;      The cost-from-parent of a node, plus the path-cost of its parent, must
;      equal the path-cost of the node.
;
; Due to the use of a hash table to store node data, which makes the hash table
; the authoritative source for node data, there is a restriction on storing
; node data in variables, and especially on passing that data to a function.
; The safe choice would be to never store the pointer to a node in a variable,
; but rather to store the state (which is the hash index), and look up the
; information in the hash table whenever it's needed.  However, this may lead
; to poorer performance than if we can temporarily cache the data.  The
; practical restriction imposed by this is:
;
;      Do not keep a pointer to a node across code that manipulates the
;      node in the hash table, or else be certain that no changed data will
;      be used.

; Contrary to my usual habit, I'll write the search functions from the top down.

;*******************************************************************************
; Top-level search
;*******************************************************************************

; At the top level, we take the best node off the open queue.  If it's empty,
; we quit and report failure.  If we have a candidate, we see if it's a goal.
; If so, we quit and report success.  Otherwise, we get its successors.  For
; each, we do a fair amount of preliminary fussing around, before we decide
; whether to queue it up.  This fussing around takes the place of the simple
; queueing functions in the previous search package.

; This is a modified version of single-search from the previous search package.
; This one's *really* tail-recursive.  LISP had *better* loop-ize it...

(defun single-search (problem)
   (let ((state (get-best)))  ; Get state to visit next.
      (cond
         ; If queue is empty, we didn't find a goal state: return NIL == fail.
         ((null state)
             (when *trace-messages*
                (format t "~&Open queue exhausted without reaching goal.~%"))
             NIL)
         (t ; Get the node corresponding to this state.  (If it was on the
            ; queue, it's got a node in the hash table.)
            (let ((node (retrieve-node state)))
               (when *trace-messages*
                  (format t "~&Selected node is:~%~A~&" node))

               ; Check whether it's a goal state.
               (if (funcall (goal-fn problem) state)

                  ; It is, so return the node.
                  (progn
                     (when *trace-messages*
                        (format t "~&Found goal state.~%"))
                     node)

                  ; Else process its successors.  (Note that we don't have to
                  ; put the node in any "closed" list, because it's already in
                  ; the hash table, and we took it out of the open queue --
                  ; that's the same as being "closed".
                  (let ((successors (funcall (successors-fn problem) state)))
                     (when *trace-messages*
                        (format t "~&Not a goal state.  Successors are:~%~A~&"
                                successors))

                     ; We don't do simple repeated-state exclusion.  What
                     ; exclusion we do occurs before we insert a node in the
                     ; queue.  And we do visit nodes more than once, whereas we
                     ; only create a given node once. So count anything that
                     ; gets here as a visit.
                     (incf *total-nodes-visited*)

                     ; Add the successors in to this node's successor list.
                     ; An examination of the A* algorithm shows that we want
                     ; to add the successors to this node's successor list no
                     ; matter what, so we may as well do it in a batch.  Since
                     ; we are using states, not nodes, in the successor list,
                     ; then we don't need to have nodes already made and put
                     ; in the hash table.  That will happen when the successor
                     ; state is dealt with during successor processing.  We do
                     ; need to be sure that we can handle a successor state
                     ; without a node -- these should just be ignored because
                     ; in the A* algorithm, these are equivalent to states that
                     ; haven't yet been added to the successor list of any node.
                     (make-new-successors node successors)

                     ; Do whatever needs doing to the successors.  This is the
                     ; parallel to running the queue-fn in the previous version.
                     ; This could probably be put in the same form as queue-fn,
                     ; i.e. something that took a list of successors, and
                     ; returned a new queue...but not quite -- these successors
                     ; are states, not nodes.
                     (process-successors successors node problem)

                     ; Now that the queue is updated, call ourselves again.
                     (single-search problem))))))))

;*******************************************************************************
; Successor processing
;*******************************************************************************

; There are two sets of successor-handling routines with similar names.  The
; first set, process-successor(s), is the outer level of A* successor
; manipuation.  It's the part that examines newly-acquired successors.
; The update-successor(s) routines, on the other hand, deal with fixing up
; the path costs if we find a better path.

; To process the successors, we'll need:
;   - a recursive function to run through the successor list
;   - code to examine one successor, which may call...
;   - a traversal procedure to update successors if we discover a lower-cost
;     path.

(defun process-successors (successors parent-node problem)
   ; Get a successor.
   (let ((successor (first successors)))
      ; If there are no more, we're through.
      (if (null successor)
         (progn
            (when *trace-messages*
               (format t "~&No more successors to process.~%"))
            NIL) ; We don't need to return anything.  This routine is pure
                 ; side-effect.  If it gets rewritten to look like a 
                 ; queue-fn, that will change.

         ; Here if we have a successor.  Look it up in the hash table.
         (let ((old-node (retrieve-node successor)))
            (when *trace-messages*
               (format t "~&Processing successor: ~A~%" successor))

            ; There are two cases: 1) we've seen this state before, so it's got
            ; a parent already, 2) it's a new state, so it has no parent and no
            ; path. 
            (cond

               ; Have we seen this state?
               (old-node
                  (when *trace-messages*
                    (format t "~&State previously seen.  Node is:~%~A~&"
                            old-node))

                  ; Yes -- do whatever it needs.  Yes, successor is the state
                  ; in old-node as well.  It's only passed in so we don't have
                  ; to grub it out of a node again.
                  (process-successor successor old-node parent-node problem))

               ; Else it's a new state.
               (t (when *trace-messages*
                    (format t "~&State is new.~%"))

                  ; Make a node out of it and insert it in the hash table.
                  ; Making the node includes calling the heuristic function and
                  ; calculating the path cost.
                  (make-new-node-from-parent successor parent-node problem)
                  ; Insert the successor state into the open queue.
                  (insert-open successor)))

            ; Process the rest of the successors.
            (process-successors (rest successors) parent-node problem)))))

; Call this if the state is one we've already seen.  It handles updating the
; path cost and parent if the new parent provides a lower-cost path.

(defun process-successor (successor old-node new-parent-node problem)
   ; Calculate the path cost to the successor via the new parent.  This is
   ; the new parent's path cost plus the cost to get from the new parent to
   ; the successor.  We don't need to do the calculation for the old parent,
   ; because that's stored in the node we found in the hash table.
   (let* ((new-cost-from-parent
             (funcall (cost-fn problem) (state new-parent-node) successor))
          (new-cost
             (+ (path-cost new-parent-node)
                new-cost-from-parent))
          (old-cost (path-cost old-node)))

      (when *trace-messages*
         (format t "~&Path cost through old parent is:~%~A~&" old-cost)
         (format t "~&Path cost through new parent is:~%~A~&" new-cost))

      ; Check which of the two possible paths had the lesser cost.  We don't
      ; want to make any changes unless the new parent is strictly less.
      (when (< new-cost old-cost)
         (when *trace-messages* (format t "~&Switching to new parent.~%"))

         ; Here if the new parent's path cost was lower.
         (let (; We want to switch to the new parent, so construct a new node
               ; with the new cost and parent, and put it in the hash table in
               ; place of what's there now.
               (new-successor-node
                  (make-new-parent old-node 
                                   new-parent-node
                                   new-cost
                                   new-cost-from-parent)))
            ; Fix up the costs and parents in the successor list.
            (update-successors (successor-list new-successor-node)
                               new-successor-node
                               problem)))))

; Go through all the successors and update their path costs and parents if
; appropriate.  In the ordinary case, when the successor is the child of the
; node through which we get to it, we only update the path cost.  But if the
; successor participates in a different path, which was, at one time, a better
; path, then we need to see which is better now.

; The traversal path is: If we're at the end of the successor list, we're done.
; If not, update the head of the successor list as needed, and call ourselves
; on that node's successor list.  Upon return, call ourselves on the tail our
; successor list.  This recursion indicates that the argument to this function
; should be a successor list.  We will also need the parent node.

(defun update-successors (successors passed-parent-node problem)
   (cond
      ; Quit if no more successors.  Return value isn't used; return NIL anyway.
      ((null successors)
          (when *trace-messages*
             (format t "~&No more successors to update.~%"))
          NIL)

      ; Not done yet.
      (t ; Update the head of the successor list.  This includes a recursive
         ; call to update-successors on this node's successor list.
         (update-successor (first successors) passed-parent-node problem)

         ; Update the rest of the successor list.
         (update-successors (rest successors) passed-parent-node problem))))

; Update one successor and its successor list.  Successor is assumed to be
; non-NIL.

(defun update-successor (successor passed-parent-node problem)
   ; Get the node for this successor state.
   (let ((successor-node (retrieve-node successor)))
      ; If the successor doesn't have a node in the hash table (and therefore
      ; no parent), it's one we just added and haven't processed yet --
      ; ignore it.  I'm not sure this case is possible.
      (when *trace-messages*
         (format t "~&Updating successor: ~A~%" successor)
         (unless successor-node
            (format t "~&No node exists for successor ~A~%" successor)))

      (when successor-node
         ; Check if the state in parent-node is this successor's parent.
         ; Parents are stored as states, so we want to compare the
         ; successor's parent with the passed-parent-node's state.
         (cond 
            ((equalp (parent successor-node) (state passed-parent-node))
              ; This is the normal case:  We just want to update the path cost
              ; then update the successor list.
               (when *trace-messages*
                  (format t "~&Parent still same.))

               (let (; The updated cost to the parent node is in passed-parent.
                     ; The cost from the parent to this node is still the same,
                     ; and it's saved in our node (just for this purpose).  So
                     ; add them to get the new path cost.  Make a new node with
                     ; this cost, and replace the one in the hash table.
                     (new-successor-node
                        (make-new-cost successor-node passed-parent-node)))

                  ; Call update-successors to handle the successor's successor
                  ; list.  But note we cannot use the old successor-node --
                  ; it's no longer current.  I should probably re-do all the
                  ; calls to take only states, and always look up the node.
                  (update-successors (successor-list new-successor-node)
                                     new-successor-node
                                     problem)))

            (t ; Here if the parent is different.  Compare the cost that's
               ; already there with the cost through parent-node.
               (when *trace-messages*
                  (format t "~&Parent is different.))

               (let* (; The cost through passed-parent is the path cost in
                      ; passed-parent plus the cost from there to the successor,
                      ; which latter is not what's stored in the successor-node,
                      ; because that's the cost to a different parent.
                      (cost-from-passed-parent
                         (funcall (cost-fn problem) (state passed-parent-node)
                                                    successor))
                      (cost-through-passed-parent
                         (+ (path-cost passed-parent-node)
                            cost-from-passed-parent))
                      (current-path-cost (path-cost successor-node)))

                  (when *trace-messages*
                     (format t "~&Path cost through old parent is:~%~A~&"
                        current-path-cost)
                     (format t "~&Path cost through new parent is:~%~A~&"
                        cost-through-passed-parent))

                  ; Compare that cost with the cost already stored in
                  ; successor-node.  We only want to make a change if the
                  ; new cost is strictly less.  Otherwise, we leave the
                  ; successor alone -- we neither change its cost nor visit
                  ; its successor list, because they are on a different path.
                  (when (< cost-through-passed-parent current-path-cost)
                     (when *trace-messages*
                        (format t "~&New cost is less.~%~"))

                     ; Here if the cost through passed-parent is less.  We need
                     ; to replace the parent and the cost-from-parent.
                     (let ((new-successor-node
                              (make-new-parent successor-node
                                               passed-parent-node
                                               cost-through-passed-parent
                                               cost-from-passed-parent)))
                        ; And update the successor list.
                        (update-successors (successor-list new-successor-node)
                                           new-successor-node
                                           problem)))))))))

;*******************************************************************************
; USER INTERFACE
;*******************************************************************************

; This is what the user calls directly.  It's simpler than the blind-search
; version, because there are no options.

(defun Astar-search (problem)
   ; Get the initial state because we'll use it twice.
   (let ((i-state (initial-state problem)))

      ; Initialize the statistics variables.  The initial node is made "by
      ; hand" just below, rather than by use of make-new-node-from-parent,
      ; which is used for all other brand-new nodes, and which is where they
      ; get counted.  So count the initial node here.  We don't count nodes
      ; as created if they're only being rebuilt because their innards are
      ; getting updated.
      (setf *total-nodes-created* 1)
      (setf *total-nodes-visited* 0)
      (setf *total-nodes-touched* 1)

      ; Make the hash table.
      (setf *all-nodes* (make-hash-table :test 'equalp))
      ; Clear out the open queue.
      (setf *open-queue* NIL)

      ; The initial node contains the initial state but no parent or cost.
      ; Make it and insert it in the hash table.
      (when *trace-messages* (format t "~&Initial node:~%"))
      (make-new-node
         ; Initial node has no parent.
         NIL
         ; Initial state is supplied by user.
         i-state
         ; No successors yet.
         NIL
         ; Cost to non-existent parent is 0.
         0
         ; No path cost.
         0
         ; Request the heuristic value for the initial state.
         (funcall (heuristic-fn problem) i-state))

      ; Insert the initial state into the open queue.
      (insert-open i-state)
      (when *trace-messages* (format t "~&Initial queue: ~A" *open-queue*))

      ; Capture the result of the search, which is NIL or a goal node, in a
      ; variable.
      (let* ((result (single-search problem))
             (result-state (state result)))

         ; We're now in the body of the let, with the goal state, if any, in
         ; "result".  Tell the user what happened, then return result.
         (if result
            (progn
               (format t "~&Search succeeded.~%")
               (format t "~&Goal state is: ~A~%Path cost is: ~A~%Path is:~%"
                  (funcall (show-state problem) result-state)
                  (path-cost result))
               (print-path result problem))
            (format t "~&Search failed.~%"))
         (format t "~&Total nodes created: ~A~%Total nodes visited: ~A~%"
            *total-nodes-created* *total-nodes-visited*)
         (format t "~&Total nodes updated: ~A~%"
            (- *total-nodes-touched* *total-nodes-created*))
         ; Just in case anyone wants it, return the goal state.
         result-state)))
