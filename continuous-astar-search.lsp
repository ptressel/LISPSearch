;*******************************************************************************
; Continuous A* search, continuous-astar-search.lsp
;*******************************************************************************

; This collection of functions provides a version of A* search that disallows
; backtracking beyond a user-specified depth, by pruning its search tree.

;*******************************************************************************
; USAGE
;*******************************************************************************

; To use cAstar-search, the user must define a "problem", which consists of
; providing several pieces of information and several functions, packaged into
; a list:
;
; (initial-state goal-function successors-function cost-function 
; heuristic-function show-state at-depth)
;
; "initial-state" is the state at which the search should start, in whatever
; representation the user chooses for states.
;
; "goal-function" takes a state as argument and returns NIL iff the state is
; not a goal state.
;
; "successors-function" takes a state as argument and returns a list of states
; that can be reached from the given state.
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
; will work while using a fussier equality than the user might want, but it will
; generate and expand more states than it might have to, if there are some that
; the user's code treats as equivalent.)
;
; The user calls cAstar-search, supplying the above items in a list, and
; receives the goal state as a return value.  As a side effect, the goal state,
; search statistics, and path are printed.
;
; A call might look like this:
;
; (cAstar-search
;    (list start-point #'is-goal #'successors #'cost #'heuristic #'showstate))

; In this version, in which depth maintenance is done by the caller, there is
; also a callback, cAstar-cleanup, by which the caller informs the search that
; a depth boundary has been passed, and it's time to prune the search tree.
; The caller supplies the state that's on the path whose subtree is to be
; preserved, and a predicate that tells if a state is at the desired depth to
; prune.  A call might look something like:
;
; (cAstar-cleanup current-state
;                 #'(lambda (state)
;                      (at-depth-to-prune-p state depth-to-prune)))

;*******************************************************************************
; Global variables
;*******************************************************************************

; Appease fussy LISPs by declaring all globals to be special.
(declaim (special *cAstar-trace-messages*
                  *cAstar-total-nodes-created*
                  *cAstar-total-nodes-visited*
                  *cAstar-total-nodes-touched*
                  *cAstar-all-nodes*
                  *cAstar-open-queue*))

;*******************************************************************************
; Trace messages
;*******************************************************************************

; To enable tracing messages, which will show each step in the execution of the
; algorithm, set *cAstar-trace-messages* to something non-NIL after loading the
; package.

(setf *cAstar-trace-messages* NIL)

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

; The next two routines are new, added in support of tree pruning.

; Follow the path back from a node, performing a supplied predicate on each.
; Return a list containing the path starting from the first node for which the
; predicate is true, if any, or return NIL if we get to the end of the path
; without success.  (To do for later:  Make this function take only the node
; and predicate as args, and define a helper with "labels" that takes all three
; args, so that the caller doesn't have to put something in for the path tail.
; For now, the caller should supply NIL.)  To get the entire path back to a
; node with a null parent, supply a predicate that checks if the parent is NIL.
; The print-path function below could be rewritten to use this.

(defun find-if-on-path (node path-tail predicate)
   (cond
      ; Quit if the node is NIL, which means we've run off the end of the path.
      ((null node) NIL)
      ; Perform the test and return the node and child if the test succeeds.
      ((funcall predicate node) (cons node path-tail))
      ; Else continue back up the path.
      (t (find-if-on-path (parent-node node) (cons node path-tail) predicate))))

; Determine if the given node is a decendent of the given subtree root node.
; This predicate is used to decide which nodes to keep during tree pruning.
; It's brute-force -- just looks back along the path from the node to see if
; root is in there.  (I don't call find-if-on-path because I don't want the
; overhead of consing up the path nodes.)

(defun decendent-p (node root)
   (cond
      ; Quit if the node is NIL, which means we've run off the end of the path
      ; without finding root.
      ((null node) NIL)
      ; See if we're at root -- if so, return true.  We can't compare the
      ; entire nodes, because this is used during tree pruning, and the path
      ; cost of the root node may get updated during that process.  If our
      ; caller has a stale copy of the root node, it won't match as a whole.
      ; Instead, just compare the states.  That's more correct anyway, because
      ; the states are also the keys in the hash table.
      ((equalp (state node) (state root)) T)
      ; Else continue back up the path.
      ((decendent-p (parent-node node) root))))

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
      (incf *cAstar-total-nodes-touched*)
      (when *cAstar-trace-messages* (format t "~&~A~%" node))
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
      (incf *cAstar-total-nodes-created*)
      (when *cAstar-trace-messages* (format t "~&Creating new node:~%"))
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
   (when *cAstar-trace-messages*
      (format t "~&Updating node with new successors:~%"))
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

; Make a new node with a changed path cost.  All else remains the same.
(defun make-new-cost (node new-cost)
   (when *cAstar-trace-messages*
      (format t "~&Updating node with new path cost.~%")
      (format t "~&Old cost: ~A   New cost: ~A~%" (path-cost node) new-cost))
   (make-new-node
      ; Get everything but the cost out of the supplied node.
      (parent node)
      (state node)
      (successor-list node)
      (cost-from-parent node)
      ; Path cost is new.
      new-cost
      (heuristic-value node)))

; Make a new node with a different parent, cost to parent, and path cost.
; When we call this, we happen to have already calculated both the cost to
; the new parent and the new path cost through that parent.
(defun make-new-parent (node new-parent new-cost new-cost-from-parent)
   (when *cAstar-trace-messages*
      (format t "~&Updating node due to change of parent.~%"))
   (make-new-node
      ; Parent state is new.  (This works even if new-parent is NIL.)
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
; TREE PRUNING
;*******************************************************************************

; Here's the intent:

; The user specifies a maximum depth of backtracking to allow (call this
; max-depth).  A variable is maintained that denotes the deepest depth we've
; reached so far.  Whenever successors are created, a check is made to see if
; they're at a depth greater than the deepest so far.  If so, all nodes at a
; depth preceeding the deepest so far by the max backtracking depth are
; discarded.  In addition, any of their children are discarded unless they are
; "relatives" of the state whose successors are being created.  That is, we
; keep only nodes in the subtree containing the current node being visited,
; and whose depth is max-depth.  For any nodes we keep, we remove any memory
; of the ancestor we just cut off, by subtracting its path cost from all of
; their path costs.

; How do we discover if a given node belongs to this subtree?  First we find
; the ancestor by starting from the current node and going back up the path
; for max-depth steps.  To test another node for subtree membership, we go up
; its path for no more than max-depth steps, and see if the same ancestor is
; present.  If not, the node is discarded.

; Here's the actuality:

; That's not quite what's actually done, or at least, it's not all done here...
; Partly through shortage of time, which led to unwillingness to change the
; search code significantly, all the knowledge of depth was built into
; the "problem" code that calls this search, and depths are stored in the
; problem's states rather than the search tree nodes.  So instead of this code
; maintaining depths directly, it calls helper functions provided by the
; problem to determine if a state is at the right depth for pruning.

; There is a point at which the problem code *must* know it's advanced to a new
; depth level, because it outputs a partial result and reads in new data there.
; In fact, the search would need to provide the result -- it's in the state
; that's max-depth back along whatever path the search is following when it
; advances to a new depth level.  So if the depth maintenance were pulled
; inside the search, there would need to be a callback to provide this result
; and allow the problem to take any needed actions on a depth boundary.

; The pruning is identical in both cases -- the only difference is who does the
; work of maintaining the depth information.

; If there's time, I'll move the depth handling into the search to get a cleaner
; division of labor.  For now, there's only one routine that deals with tree
; pruning, which is called by the problem code when it detects an advance to
; a new depth level.

(defun cAstar-cleanup (state at-depth-to-prune-p)
   (when *cAstar-trace-messages*
      (format t "~&Cleanup requested at state:~%~A~&With predicate:~%~A~&"
         state at-depth-to-prune-p))
   (let*
      (; Find the ancestors of the given state that bracket the maximum depth to
       ; keep.  These are the ancestor that contains the finalized result state
       ; that we'll return to the caller, and its child along the path which is
       ; the root of the subtree to preserve.  To find them, use the path
       ; searching routine defined above, with a predicate that tests the state
       ; in a node to see if it's at the desired depth.

       (ancestors-at-depth-boundary
          (find-if-on-path
             ; Get the node corresponding to the supplied state.
             (retrieve-node state)
             ; Give the routine its path-so-far arg.
             NIL
             ; Make a predicate that will test a *node* using the supplied
             ; predicate that tests a *state*.
             #'(lambda (node)
                  (funcall at-depth-to-prune-p (state node)))))

       ; Here we have a list containing first the ancestor with the desired
       ; result, and second, the root of the subtree to keep.  Pull out
       ; the result state here, because we're about to nuke its node out of
       ; the hash table.

       (result-state-at-max-depth (state (first ancestors-at-depth-boundary)))

       ; Separate out the node at the root of the subtree.

       (root-of-subtree-to-keep (second ancestors-at-depth-boundary))

       ; Get its parent cost -- we're about to forget about anything that
       ; preceded this node, so we want to disregard any path cost prior to
       ; this node  If we are correctly maintaining the tree, then this amount
       ; should just be this node's cost-from-parent, and this should also
       ; equal this node's path-cost, because this node should have no ancestor
       ; before its parent -- they've all been pruned in previous cleanups.
       ; We'll subtract this from the path costs of all its ancestors.  Why
       ; do we bother with this, since all costs are relative?  It's because
       ; if we don't, the cost can grow without bound, which ruins our goal
       ; of O(1) performance in storage.

       (cost-to-subtract (path-cost root-of-subtree-to-keep)))

      ; At this point we have the data to return to the caller, and the root
      ; of the subtree to keep.

      (when *cAstar-trace-messages*
         (format t "~&Root of subtree to keep:~%~A~&" root-of-subtree-to-keep))

      ; If the tree isn't deep enough yet, don't bother with the cleanup --
      ; just return NIL.

      (unless (null ancestors-at-depth-boundary)

         ; First look at all the nodes belonging to states in the open queue to
         ; see if we want to get rid of them.  Do this first before looking at
         ; the hash table, because we don't want to nuke a node in the hash
         ; table without taking its state out of the queue.  If we only went
         ; through the hash table, we'd have to search the queue on each node.
         ; This way we may, at the worst, examine each state in the queue twice,
         ; but that's better than traversing the queue for each node in the hash
         ; table...

         (setf *cAstar-open-queue*
               (remove-if-not
                  ; Supply a test function to remove-if-not that checks if a
                  ; queue entry, which is a state, is in the subtree -- return
                  ; true if so, else remove the state's node from the hash table
                  ; and return NIL so remove-if-not takes it out of the queue
                  ; list.
                  #'(lambda (state)
                       (cond
                          ; Is the state in the subtree?  If so, we want to keep
                          ; it, so return true (just return the result of
                          ; decendent-p in that case).
                          ((decendent-p (retrieve-node state)
                                        root-of-subtree-to-keep))

                          ; Otherwise remove it from the hash table, if it's in
                          ; there, and return NIL.
                          (t (when *cAstar-trace-messages*
                                (format t "~&Discarding (via open queue):~%~A~&"
                                   (gethash state *cAstar-all-nodes*)))
                             (remhash state *cAstar-all-nodes*)
                             NIL)))

                  *cAstar-open-queue*))

         ; Now look at all the nodes in the hash table and see if we want to
         ; prune or keep them.  If we want to prune them we remove them from the
         ; hash table.  If we want to keep them, we fix up their path costs.
         (maphash
            ; Maphash calls the supplied function with the key and value (which
            ; are the state and node here) for each entry in the hash table.  In
            ; maphash, we're allowed to use remhash, and to (setf (gethash ...))
            ; a new value.
            #'(lambda (state node)
                 (cond
                    ; Is this node in the subtree?  If so, we want to keep it --
                    ; fix its path cost.  Return value doesn't matter.
                    ((decendent-p node root-of-subtree-to-keep)
                        ; It's a keeper: make a new node with the modified path
                        ; cost, and replace the current node in the hash table.
                        (setf (gethash state *cAstar-all-nodes*)
                              (make-new-cost
                                 node (- (path-cost node) cost-to-subtract))))

                    ; It's not in the subtree -- get rid of it.
                    (t (when *cAstar-trace-messages*
                          (format t "~&Discarding (via hash table):~%~A~&"
                             (gethash state *cAstar-all-nodes*)))
                       (remhash state *cAstar-all-nodes*))))
            *cAstar-all-nodes*)

         ; Clear the parent pointer of the root of the remaining subtree by
         ; making a new node without it, and putting that back in the hash
         ; table.  The cost-from-parent is now 0.  We leave the path cost alone
         ; because it got fixed just above.
         (setf (gethash (state root-of-subtree-to-keep) *cAstar-all-nodes*)
               (make-new-parent root-of-subtree-to-keep
                                NIL     ; new parent
                                (path-cost root-of-subtree-to-keep)
                                0 ))   ; new cost-from-parent

         ; Finally, we return the state that was the ancestor of the supplied
         ; state that we just purged from the tree.
         (when *cAstar-trace-messages*
            (format t "~&Returning ancestor state:~%~A~&"
               result-state-at-max-depth))
         result-state-at-max-depth)))

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
(setf *cAstar-total-nodes-created* 0)

; Count the nodes as they are visited -- this is a measure of search efficiency.
; The appropriate place to do this is in the search routines at the point where
; they test a node to see if it's a goal node.
(setf *cAstar-total-nodes-visited* 0)

; Count nodes as they are constructed or reconstructed.  Difference between
; this and *cAstar-total-nodes-created* is the number of times nodes are
; modified.
(setf *cAstar-total-nodes-touched* 0)

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

(setf *cAstar-all-nodes* NIL)

; Find a node in the hash table given its state.  This is also how we test
; whether a node is in the hash table, because gethash with no default
; returns NIL if the state isn't found, but if the state is there, it will
; have a non-NIL value -- the node -- returned.  So there's no need for a
; separate predicate.

(declaim (inline retrieve-node))

(defun retrieve-node (state)
   (gethash state *cAstar-all-nodes*))

; Put a node in the hash table.  (I don't know if I can use retrieve-node
; inside the setf.  Leave the bare gethash in there for now.)  Although the
; main purpose of this function is its side-effect of updating the hash table,
; it also returns the node, and that value does get used.

(defun store-node (node)
   (setf (gethash (state node) *cAstar-all-nodes*) node))

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

(setf *cAstar-open-queue* NIL)

; Unlike the previous search package, we'll sort the queue when we need to get
; an element off it, not when we queue up elements.  That runs the sort only
; when we need it.  It may be that there wouldn't be any difference in the
; number of sort calls, but the number of calls won't be *greater* this way.

(declaim (inline sort-open))

(defun sort-open ()
  (sort *cAstar-open-queue* #'< :key #'fprime))

(defun get-best ()
   ; Yes, I know, eeeeeeuw, yuk, gag, bletch.  Sort the queue, get the head,
   ; behead the sorted queue, stuff it back into the global, and return the
   ; former head.
   (let* ((sorted-open (sort-open))
          (best (first sorted-open)))
      (when *cAstar-trace-messages*
         (format t "~&Sorted open queue:~%~1,8TState: ~3,8TF':")
         (mapcar
            #'(lambda (state)
                 (format t "~&~1,8T~A ~3,8T~A~%" state (fprime state)))
            sorted-open)
         (format t "~&Get-best selects: ~A~%" best))
      (setf *cAstar-open-queue* (rest sorted-open))
      best))
     
; Unlike the previous searches, we only queue up one state at a time, so batch
; queueing functions aren't needed -- we can just cons new nodes.  Or perhaps
; they should be appended...don't know which would be better for the sort.
; The new state will likely have a worse f'.

(defun insert-open (state)
   (setf *cAstar-open-queue* (cons state *cAstar-open-queue*))
   (when *cAstar-trace-messages*
      (format t "~&Inserting ~A in open queue.  New queue is:~%~A~&"
              state *cAstar-open-queue*)))

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
; This one's *really* tail-recursive.  Lisp had *better* loop-ize it...

; Well, no, don't trust Lisp -- explicitly loop-ize it.

(defun single-search (problem)
   (do
      ; Loop variable:
      ; Get state to visit next.  (First (get-best) is initial value, second
      ; is update on each loop pass.)
      ((state (get-best) (get-best)))

      ; Loop termination test:
      ; If queue is empty, we didn't find a goal state: return NIL == fail.
      ((null state)
          (when *cAstar-trace-messages*
             (format t "~&Open queue exhausted without reaching goal.~%"))
          (return-from single-search NIL))

      ; Loop body:
      ; Get the node corresponding to this state.  (If it was on the
      ; queue, it's got a node in the hash table.)
      (let ((node (retrieve-node state)))
         (when *cAstar-trace-messages*
            (format t "~&Selected node is:~%~A~&" node))

         ; Check whether it's a goal state.
         (if (funcall (goal-fn problem) state)

            ; It is, so return the node.
            (progn
               (when *cAstar-trace-messages* (format t "~&Found goal state.~%"))
               (return-from single-search node))

            ; Else process its successors.  (Note that we don't have to
            ; put the node in any "closed" list, because it's already in
            ; the hash table, and we took it out of the open queue --
            ; that's the same as being "closed".
            (let ((successors (funcall (successors-fn problem) state)))
               (when *cAstar-trace-messages*
                  (format t "~&Not a goal state.  Successors are:~%~A~&"
                     successors))

               ; We don't do simple repeated-state exclusion.  What
               ; exclusion we do occurs before we insert a node in the
               ; queue.  And we do visit nodes more than once, whereas we
               ; only create a given node once. So count anything that
               ; gets here as a visit.
               (incf *cAstar-total-nodes-visited*)

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
               (process-successors successors node problem))))))

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
            (when *cAstar-trace-messages*
               (format t "~&No more successors to process.~%"))
            NIL) ; We don't need to return anything.  This routine is pure
                 ; side-effect.  If it gets rewritten to look like a 
                 ; queue-fn, that will change.

         ; Here if we have a successor.  Look it up in the hash table.
         (let ((old-node (retrieve-node successor)))
            (when *cAstar-trace-messages*
               (format t "~&Processing successor: ~A~%" successor))

            ; There are two cases: 1) we've seen this state before, so it's got
            ; a parent already, 2) it's a new state, so it has no parent and no
            ; path. 
            (cond

               ; Have we seen this state?
               (old-node
                  (when *cAstar-trace-messages*
                    (format t "~&State previously seen.  Node is:~%~A~&"
                            old-node))

                  ; Yes -- do whatever it needs.  Yes, successor is the state
                  ; in old-node as well.  It's only passed in so we don't have
                  ; to grub it out of a node again.
                  (process-successor successor old-node parent-node problem))

               ; Else it's a new state.
               (t (when *cAstar-trace-messages*
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

      (when *cAstar-trace-messages*
         (format t "~&Path cost through old parent is:~%~A~&" old-cost)
         (format t "~&Path cost through new parent is:~%~A~&" new-cost))

      ; Check which of the two possible paths had the lesser cost.  We don't
      ; want to make any changes unless the new parent is strictly less.
      (when (< new-cost old-cost)
         (when *cAstar-trace-messages*
            (format t "~&Switching to new parent.~%"))

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
          (when *cAstar-trace-messages*
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
      (when *cAstar-trace-messages*
         (format t "~&Updating successor: ~A~%" successor)
         (unless successor-node
            (format t "~&No node exists for successor: ~A~%" successor)))

      (when successor-node
         ; Check if the state in parent-node is this successor's parent.
         ; Parents are stored as states, so we want to compare the
         ; successor's parent with the passed-parent-node's state.
         (cond 
            ((equalp (parent successor-node) (state passed-parent-node))
              ; This is the normal case:  We just want to update the path cost
              ; then update the successor list.
               (when *cAstar-trace-messages*
                  (format t "~&Parent still same.~%"))

               (let (; The updated cost to the parent node is in passed-parent.
                     ; The cost from the parent to this node is still the same,
                     ; and it's saved in our node (just for this purpose).  So
                     ; add them to get the new path cost.  Make a new node with
                     ; this cost, and replace the one in the hash table.
                     (new-successor-node
                        (make-new-cost
                           successor-node
                             (+ (cost-from-parent successor-node)
                                (path-cost passed-parent-node)))))

                  ; Call update-successors to handle the successor's successor
                  ; list.  But note we cannot use the old successor-node --
                  ; it's no longer current.  I should probably re-do all the
                  ; calls to take only states, and always look up the node.
                  (update-successors (successor-list new-successor-node)
                                     new-successor-node
                                     problem)))

            (t ; Here if the parent is different.  Compare the cost that's
               ; already there with the cost through parent-node.
               (when *cAstar-trace-messages*
                  (format t "~&Parent is different.~%"))

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

                  (when *cAstar-trace-messages*
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
                     (when *cAstar-trace-messages*
                        (format t "~&New cost is less.~%"))

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

; This is what the user calls directly.  See the "Usage" section for a
; description of the argument.

(defun cAstar-search (problem)
   ; Get the initial state because we'll use it twice.
   (let ((i-state (initial-state problem)))

      ; Initialize the statistics variables.  The initial node is made "by
      ; hand" just below, rather than by use of make-new-node-from-parent,
      ; which is used for all other brand-new nodes, and which is where they
      ; get counted.  So count the initial node here.  We don't count nodes
      ; as created if they're only being rebuilt because their innards are
      ; getting updated.
      (setf *cAstar-total-nodes-created* 1)
      (setf *cAstar-total-nodes-visited* 0)
      (setf *cAstar-total-nodes-touched* 1)

      ; Make the hash table.
      (setf *cAstar-all-nodes* (make-hash-table :test 'equalp))
      ; Clear out the open queue.
      (setf *cAstar-open-queue* NIL)

      ; The initial node contains the initial state but no parent or cost.
      ; Make it and insert it in the hash table.
      (when *cAstar-trace-messages* (format t "~&Initial node:~%"))
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
      (when *cAstar-trace-messages*
         (format t "~&Initial queue: ~A" *cAstar-open-queue*))

      ; Get the result of the search, which is NIL or a goal node.
      ; Unlike the prior version, we don't print the result, or path, or
      ; statistics, because the caller is taking care of output.  One can
      ; still see the statistics by examining the globals they're stored in.
      ; (Also unlike the previous version, it's currently mandatory to return
      ; the goal state because the caller is using it to print the final
      ; result.  This will not be the case when the search is converted to
      ; do the depth tracking -- then it'll call an output function provided
      ; by the caller.)

      (state (single-search problem))))
