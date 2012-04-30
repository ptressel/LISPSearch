;*******************************************************************************
; BLIND SEARCH PACKAGE, blind-search.lsp
;*******************************************************************************

; This collection of functions provides various types of blind search.
; It includes four search schemes: breadth first, depth first, depth limited
; depth first, and iterative deepening depth first.  It includes three
; methods of excluding repeated states:  exclude identical successor state,
; exclude if identical state on current path, exclude if identical state
; previously encountered.

;*******************************************************************************
; USAGE
;*******************************************************************************

; The user must provide several pieces of information and several functions,
; packaged into a problem definition, which is a list:
;
; (initial-state goal-function operations-function print-state print-operation)
;
; "initial-state" is the state at which the search should start, in whatever
; representation the user chooses for states.
;
; "goal-function" takes a state as argument and returns NIL iff the state is
; not a goal state.
;
; "operations-function" takes a state as argument and returns a list of
; operation functions that can be applied to that state.
;
; "show-state" takes a state as argument and makes a string, formatted for
; printing, that contains whatever information the user wants, to identify that
; state.  The output should conclude with a newline.  Unless the output is very
; short, it should start with a newline as well.
;
; "show-operation" takes an operation function as argument and returns a string
; that contains whatever information the user wants, to identify that operation.
; Unless the output is short, it should start and conclude with a newline.

; (There is a bit of difficulty about printing the function name.  LISP does not
; seem to provide anything that, given a function, returns its name.  And if the
; function is a closure, it doesn't *have* a name...  An alternative is to store
; a descriptive name in the function's documentation string, but the LISP spec
; says an implementation is free to discard the documentation string.  This is
; what I'll use if I can.  Another method:  The user might have a cond that
; compares the arg against all the problem's functions with eql, and returns an
; appropriate quoted string for each.  This won't be useful if the functions are
; generated on the fly for each state...  To be completely general, I could
; have the user provide, instead of bare functions, lists containing the
; function and a name string.)
;
; The user selects a search method, a repeated-state exclusion method, and a
; maximum depth, and passes these, along with the problem, to the blind-search
; function.  Any search type can take a maximum depth:  The search will not
; examine nodes at depths greater than that value.
;
; The blind-search function takes a problem definition list, as above, as its
; first argument, and can take three additional keyword arguments.  The keywords
; are listed below, followed by their possible values, with the default value
; first.
;
;    :search-type
;        'breadth-first
;        'depth-first
;        'iterative-deepening
;    :repeated-state-handling
;        'exclude-if-visited
;        'exclude-if-in-path
;        'exclude-same-as-parent
;        'no-exclusion
;    :maximum-depth
;        -1
;        <integer>
;
; If maximum-depth is set to a negative number, the search depth is not limited.
; This is the default.
;
; Example:  This call requests an iterative-deepening search, limited to a
; maximum depth of 12, that excludes identical states along the current path:
;
; (blind-search
;    ((...) 'is-goal 'ops 'printstate 'printop)
;    :search-type 'iterative-deepening
;    :maximum-depth 12
;    :repeated-state-handling 'exclude-if-in-path)
;
; Before calling blind-search, the user must load this package,
; blind-search.lsp, using whatever means are provided by their LISP environment.

;*******************************************************************************
; PROBLEM EXTRACTION HELPER FUNCTIONS
;*******************************************************************************

; A problem consists of an initial state, a goal function that is used to test
; whether a given state is a goal state, an operations function that, for a
; given state, returns a list of operations that can be performed on that state,
; a function to print a state, and a function to print an operation.  (An
; alternative to supplying a function to produce a list of operations would be
; to supply a fixed list of operations, but there are some problems for which a
; fixed list is inconvenient.)

;Helper functions extract each portion of the problem.

(defun initial-state (problem)
   (first problem))

(defun goal-fn (problem)
   (second problem))

(defun ops-fn (problem)
   (third problem))

(defun show-state (problem)
   (fourth problem))

(defun show-op (problem)
   (fifth problem))

;*******************************************************************************
; SEARCH TREE
;*******************************************************************************

; A node in the search tree is represented by a list containing:
;
;   - a pointer to the parent node
;   - the current state
;   - the operator by which we arrived here from the parent state
;       (this is used only to display the path)
;   - the depth (i.e. how many operations have been applied since the initial
;       state).
;
; If it turns out to be simple to dispense with the parent node and/or depth,
; relying instead on the call stack produced by recursion, then these fields
; will be dropped.  (A more sophisticated search would include the path cost
; to this point.)

; Helper functions extract each piece of a node, and print the path represented
; by the node.  The show-state and show-operation functions are supplied as
; part of the problem.  Better would be to make the state and operation objects
; with print methods!

(defun parent-node (node)
   (first node))

(defun state (node)
   (second node))

(defun path-operation (node)
   (third node))

(defun depth (node)
   (fourth node))

(defun print-path (node problem)
   ; First follow the parent-node pointers until we reach the root of the tree.
   (if (parent-node node) (print-path (parent-node node) problem))
   ; As we fall out of the recursion, print the node info.  If we're at the
   ; root node, don't try to print the operation, and use different verbiage.
   (if (null (parent-node node))
      (format t "~&Starting from: ~A~%"
         (funcall (show-state problem) (state node)))
      (format t "~&By means of: ~A arrived at: ~A~%"
         (funcall (show-op problem) (path-operation node))
         (funcall (show-state problem) (state node)))))

;*******************************************************************************
; SEARCH STATISTICS
;*******************************************************************************

; Global variables are used to count the total number of nodes created and the
; total number visited (these are separate because the search may generate and
; queue up many nodes that it never reaches).  I'm initializing these variables
; here using setf, just to announce their presence.  I don't know if there's a
; more formal way to declare a global variable.  The other thing I need to find
; out is how to specify, in the functions that use these variables, that I want
; to use the global variable, and not any dynamic variable of the same name that
; the user might happen to define.  As an imperfect means of avoiding aliasing,
; I could use long, obscure names for the variables, e.g. I should append a
; prefix like blind-search-package- or BSP- or some such thing to all of the
; globals.  (Actually, the same problem applies to all the *function* names in
; the package as well.)

; Count the nodes as they are created -- this is a measure of memory usage.
; The appropriate place to do this is in the enqueue function.
(setf *total-nodes-created* 0)

; Count the nodes as they are visited -- this is a measure of search efficiency.
; The appropriate place to do this is in the search routines at the point where
; they test a node to see if it's a goal node.
(setf *total-nodes-visited* 0)

;*******************************************************************************
; QUEUEING FUNCTIONS
;*******************************************************************************

; For search methods other than iterative deepening, the search method is
; entirely determined by the queueing function that inserts newly generated
; successor states into the queue of states yet to be visited.  The iterative
; deepening method uses only depth limited depth first queueing.

; The queueing functions are currently all required to order their queues so
; that the head of the queue is the next node to visit.  The search will never
; dequeue two nodes in a row without calling the queueing function, so it'll
; always get a chance to reorder the queue after we dequeue a node.  So no
; special requirements are placed on the *rest* of the queue.  But ripping the
; first node off the queue might make it difficult for the queueing function
; to maintain the queue, so a fancier scheme would have a pair of functions for
; each method, one to enqueue a list of nodes, and one to dequeue the next node
; to visit.  (Beheading the queue is not a problem for any of the included
; queueing methods, but it might be for a priority queue stored as a heap,
; which might be a tree rather than a list.  Snipping off the tree's root node
; might be a bad thing...)
 
; In a breadth first search, nodes at greater depths are visited after those
; at shallower depths, so new nodes, which are child nodes to a node already
; on the queue, and so are at greater depths, are added to the tail of the
; queue.

(defun breadth-first (queue new-nodes)
   (append queue new-nodes))

; A depth limited breadth first search quits when it exceeds a specified depth.
; This function implements this by simply returning the original queue if the
; new nodes are at too great a depth.  Then since the queue is no longer being
; extended, it will eventually be exhausted, and the search will quit.

; All nodes in new-nodes are at the same depth, so the depth is gotten from
; the first node in new-nodes.  (If we want to make the depth a variable
; maintained by the recursion rather than stored in nodes, we could make it
; a global or dynamically-scoped variable, which would be set inside the search
; function.  If that's too ugly, we could add the depth as an argument to all
; the queueing functions -- since only the package actually calls these
; functions, they are hidden, so the unused arg wouldn't annoy the user.  But I
; ain't a'gonna change it now.)

; We don't need to test for a max-depth that's < 0 because in that case the
; top-level routine that generates the search call, blind-search, sets up to
; call just breadth-first.

; The top-level routine will package up the *depth-limited function with the
; appropriate maximum depth as a closure, so that all the queueing functions
; can be called with the same args: queue and a list of new nodes.

(defun breadth-first-depth-limited (queue new-nodes max-depth)
   (cond ((<= (depth (first new-nodes)) max-depth)
             (breadth-first queue new-nodes))
         (t queue)))

; In a depth-first search, child nodes are visited before going on to peer
; nodes, so child nodes are inserted at the head of the queue.  Note that the
; search function dequeues the current node before enqueueing its child nodes,
; so this does not place the child nodes in front of the current node.

(defun depth-first (queue new-nodes)
   (append new-nodes queue))

(defun depth-first-depth-limited (queue new-nodes max-depth)
   (cond ((<= (depth (first new-nodes)) max-depth)
             (depth-first queue new-nodes))
         (t queue)))

;*******************************************************************************
; REPEATED-STATE EXCLUSION FUNCTIONS
;*******************************************************************************

; We can test for repeated states during the search:  Just as we're about to
; visit a state, we perform one of the checks for repeated states -- no check,
; check parent only, check current path, check all previously-visited states.
;
; For the less-exhaustive checks, we can just chain back through the parent
; nodes.  But if we're checking against the entire collection of previously-
; visited states, we should do something to avoid searching the entire tree,
; e.g. use a hash table to store the already-visited nodes.  We want to avoid
; having the user do any setup relating to the hash table.  It would be nice
; to avoid using a global symbol for the table, as the user might happen to
; choose to use that symbol too.  But alternatives to a global are awkward,
; e.g.:  The hash table might be dynamically scoped and defined in the outer
; function that calls the search.  I don't know enough LISP to see a good
; alternative to a global.  (I believe this is a case where we're justified in
; destructively modifying something -- we don't want multiple copies of a huge
; table...)
;
; Note that it's the states we compare against, not the nodes -- the nodes
; contain the depth, which would prevent two nodes at different depths from
; matching, even if their states were the same.  Also, comparing just the
; states makes testing for equality faster.

; The exclusion functions are predicates -- they return NIL if we don't want
; to use the node.  If we do want to use the node, they return the node.

; Don't exclude any states:

(defun no-exclusion (node)
   node)

; Check whether the selected state is the same as its parent:

(defun exclude-same-as-parent (node)
   (cond ((equal (state node) (state (parent-node node))) NIL)
         (t node)))

; Check if the selected state appeared in the path to this state.  Uses a
; helper function to do the recursive search so that the arguments to all three
; exclusion functions can be the same -- just the selected node.  Second arg to
; this helper function is the node we're checking in the path.

(defun not-in-path (node path-node)
   (cond ((null path-node) node)	; if at end of path, keep this node
         ((equal (state node) (state path-node)) NIL)	; if match, exclude
         (t (not-in-path node (parent-node path-node)))))	; else continue

(defun exclude-if-in-path (node)
   (not-in-path node (parent-node node)))

; Here's the hard one.  First we initialize to NIL a global variable that will
; be set to a hash table the first time we call this function.  It may be
; possible to do without this setf, by an appropriate test inside the function
; for whether the global variable has been set or not.  If I find a predicate
; that tests that, I'll use that instead.

(setf *visited-states* NIL)

; First we check for instantiation of the hash table -- if not present, make it.
; Then see if supplied state is in it (not worth a special case for the first
; time through) -- if so, return NIL, else insert the state in the hash table
; and return the node.  We insert the state in the hash table here because we
; don't want to know about the hash table outside this routine.  We aren't using
; the hash table to store values associated with the nodes -- we just want to
; know if the nodes are in there.  So the node is inserted as the key, and we
; don't care what the value is.  If we make its value something true, we can
; use the return value of setf in a cond as an always-true value.  (Yes, I could
; have used the node itself as the value, and so just returned the result of the
; setf, but then the hash table would be storing large items.)

; After much puzzling over the documentation for gethash and setf, here's how I
; think they work.  Gethash returns "multiple values", not a list of values.
; The first return value is NIL if the key does not exist in the hash table,
; or, if the key does exist, it's the value associated with the key.  So in
; order to test if a key is in the table, be sure to associate a non-NIL value
; with it when inserting it in the table.  Then one only has to test the "first"
; return value from gethash.  Apparently the "first" return value is accessed as
; usual.  I haven't looked into the means of getting at the other return values.

; But (setf (gethash...) newvalue) supposedly stores newvalue with the entry
; found by gethash.  At first glance, this looks like the first arg to the setf
; would be the value returned by gethash, which would be pretty useless:  The
; first arg to setf is supposed to be a "place" (a.k.a. l-value), not some
; arbitrary value which can't possibly point to anything...  The trick seems to
; be that setf is a macro, not a function, so LISP isn't evaluating that
; gethash call.  Presumably the setf macro is grubbing around inside the gethash
; form to get the place of the entry rather than executing it to get its value.
; For this reason, I won't even try using a "let" to capture the result of
; gethash into a variable, even though I have the same gethash form twice in
; the following code.  Looks like one has to know whether something that *looks*
; like a function is actually a macro, because the behavior could be quite
; different and unexpected.  Don't know that I like that...

; I haven't tested this option sufficiently to know if the matching is working
; correctly.  The counts of visited states, with the count-to-n problem, are
; no different from the counts for exclude-if-in-path -- I don't know if that's
; correct for this problem or not.

(defun exclude-if-visited (node)
   ; If we don't have a hash table, make one.
   (or (hash-table-p *visited-states*)
       (setf *visited-states* (make-hash-table :test 'equal)))
   ; If entry with this state is present, return NIL, else store this state
   ; in the table and return the node.
   (cond ((gethash (state node) *visited-states*) NIL)
         ; Yes, the setf returns t, so this is an "else" clause.
         ((setf (gethash (state node) *visited-states*) t) node)))

;*******************************************************************************
; SEARCH
;*******************************************************************************

; The search process (though not the implementation) is almost verbatim out of
; Fig 3.10 in Russell and Norvig except that this search provides exclusion of
; repeated states.  This implementation is recursive instead of iterative.

;*******************************************************************************
; SEARCH HELPER FUNCTIONS
;*******************************************************************************

; The successor-node function creates a child node of the supplied node whose
; state comes from applying the supplied operation to the supplied node's state.
; If the operation fails, return NIL.

(defun successor-node (node operation)
   (let ((successor-state (funcall operation (state node))))
      (if successor-state
         (list node successor-state operation (1+ (depth node)))
         NIL)))

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

;*******************************************************************************
; SINGLE-ITERATION SEARCH
;*******************************************************************************

; The search function takes as arguments a queueing function, a repeated-state
; exclusion function, a problem, and a queue.  The first three provide general
; information, while the queue provides the current search state.  So the first
; three args remain the same in our recursive calls.

; The search function itself has no knowledge of the internals of a state,
; or anything else specific to the problem.  Neither the queueing function nor
; the exclusion function are part of the problem, because, presumably, one might
; want to apply different search strategies to the same problem.

(defun single-search (queue-fn excl-fn problem queue)
   (let ((node (first queue)))  ; Node to visit is at head of queue.
      (cond
         ; If queue is empty, we didn't find a goal state: return NIL == fail.
         ((null node) NIL)
         ; We're just about to visit a node -- first see if we should
         ; exclude it as a repeated state.  We want to do this before we
         ; count the node as visited.  If it were not for counting visited
         ; nodes, this could could be re-ordered to make this function tail-
         ; recursive, with only a single call to single-search, and the choice
         ; of whether to expand this node or not based on whether to exclude it
         ; could be done right inside the argument list for single-search.
         ; Grumph. 
         (t (cond
               ((funcall excl-fn node)
                   ; This isn't a repeated state, so we should visit the node.
                   (progn
                      ; Count it as a visited node.
                      (incf *total-nodes-visited*)
                      ; Check whether it contains a goal state.
                      (if (funcall (goal-fn problem) (state node))
                         ; It does, so return the node.
                         node
                         ; Else continue the search, including this node's
                         ; successors.
                         (single-search queue-fn excl-fn problem
                            ; Generate the new queue, which consists of the
                            ; remainder of the current queue minus this node,
                            ; and the successors of this node, queued according
                            ; to the specified queueing method.
                            (funcall queue-fn (rest queue)
                               (expand node
                                  (funcall (ops-fn problem) (state node))))))))
               ; This is a repeated state, so we neither test it for being a
               ; goal state nor do we queue up its successors.  Instead, we just
               ; skip it and go on with the rest of the queue.
               (t (single-search queue-fn excl-fn problem (rest queue))))))))

;*******************************************************************************
; ITERATIVE-DEEPENING SEARCH
;*******************************************************************************

; An iterative-deepening search layers on top of the above search function.
; It calls the above search, with the depth limited depth first queueing
; function, in a loop with increasing values of the maximum depth.  (I'm going
; to use an actual loop here, because I haven't done that in LISP yet, and
; because it is the more natural iteration in this case.)

; If max-depth is < 0, the depth is not limited.  Note that a depth 0 depth-
; limited search checks just the initial node, which will have its depth set to
; 0 by the top-level search function.

(defun iterative-deepening-search (excl-fn problem queue max-depth)
   (do 
      ; Start at depth 0 and increase it by one on each pass.
      ((depth 0 (1+ depth)))
      ; The end test will never be true if max-depth is negative, otherwise
      ; it'll become true when the loop counter passes max-depth.  The return
      ; value, NIL, will only be used if we don't execute the (return node)
      ; in the body of the loop.
      ((and (>= max-depth 0) (> depth max-depth)) NIL)
      ; Get the result of one run of single-search.
      (let ((node (single-search
                     ; The first arg of single-search is the queueing function.
                     ; Pass it a closure of depth-first-depth-limited with the
                     ; current loop count, depth, as the maximum depth.
                     #'(lambda (q nn)
                          (depth-first-depth-limited q nn depth))
                     excl-fn problem queue)))
         ; The body of the let just checks if we got a success, and
         ; if so, returns the goal node to terminate the loop.
         (if node (return node)))))

;*******************************************************************************
; BLIND SEARCH
;*******************************************************************************

; Finally, the top-level function...  This is the only function the user
; calls directly.  Based on the options they select, blind-search produces an
; appropriate call to either single-search or iterative-deepening-search, and
; fills in the selected queueing and exclusion functions.
;
; Only the first argument has a fixed meaning -- it's the problem.  The others
; are all keywords.
;
; A note on quoting:  Although the symbols used to tell which type of
; search/exclusion to use are identical to the function names, they are not
; functions when they appear in the blind-search arg list -- they are only
; symbols, and are quoted with ' not #'.  That was a choice based on
; esthetics -- why should the user even know that there are functions by those
; names?  They shouldn't have to know anything about this package other than
; the usage info.)

(defun blind-search (problem &key (search-type 'breadth-first)
                                  (repeated-state-handling 'exclude-if-visited)
                                  (maximum-depth -1))
   (let*
      (; Get the desired queueing function.  Any type can be depth limited, if
       ; the user specifies a non-negative maximum-depth.
       (queue-fn
          (cond ((equal search-type 'breadth-first)
                    (cond ((< maximum-depth 0) #'breadth-first)
                          ; For the depth limited variant, we form a closure
                          ; with the desired maximum depth.
                          (t #'(lambda (q nn)
                                  (breadth-first-depth-limited
                                     q nn maximum-depth)))))
                ((equal search-type 'depth-first)
                    (cond ((< maximum-depth 0) #'depth-first)
                          (t #'(lambda (q nn)
                                  (depth-first-depth-limited
                                     q nn maximum-depth)))))
                ; Since iterative deepening has a fixed queueing function, we
                ; don't use a queue-fn in that case.
                ((equal search-type 'iterative-deepening) NIL)
                ; If user supplied an unknown search-type, complain and quit,
                ; returning NIL.
                (t (return-from blind-search
                      (format t "~&Unknown search type ~A~%" search-type)))))

       ; Get the desired exclusion function.
       (excl-fn
          (cond ((equal repeated-state-handling 'exclude-if-visited)
                    #'exclude-if-visited)
                ((equal repeated-state-handling 'exclude-if-in-path)
                    #'exclude-if-in-path)
                ((equal repeated-state-handling 'exclude-same-as-parent)
                    #'exclude-same-as-parent)
                ((equal repeated-state-handling 'no-exclusion)
                    #'no-exclusion)
                (t (return-from blind-search
                      (format t "~&Unknown repeated state handling ~A~%"
                                 repeated-state-handling)))))

       ; The initial queue is a list containing a single node, which is itself
       ; a list, and whose state is the initial state, with no parent or
       ; operation, and depth 0.
       (queue (list (list NIL (initial-state problem) NIL 0)))

       ; Capture the result of the search, which is NIL or a goal node, in a
       ; variable.
       (result
          (progn
             ; Initialize the statistics variables.
             (setf *total-nodes-created* 1) ; We always make the initial node.
             (setf *total-nodes-visited* 0)
             ; Clear out the hash table, in case we're going to use it.
             (setf *visited-states* NIL)
             ; There are two cases, based on the search type.
             (cond ((equal search-type 'iterative-deepening)
                       (iterative-deepening-search
                          excl-fn problem queue maximum-depth))
                   (t (single-search queue-fn excl-fn problem queue))))))

      ; We're now in the body of the let*, with the goal node, if any, in
      ; "result".  Tell the user what happened, then return result.
      (if result
         (progn
            (format t "~&Search succeeded.  Goal state is: ~A~%Path is:~%"
               (funcall (show-state problem) (state result)))
            (print-path result problem))
         (format t "~&Search failed.~%"))
      (format t "~&Total nodes created: ~A~%Total nodes visited: ~A~%"
         *total-nodes-created* *total-nodes-visited*)
      ; Just in case anyone wants it, return the goal state.
      (state result)))

