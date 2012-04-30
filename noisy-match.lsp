;******************************************************************************
; Matching in the presence of noise
;******************************************************************************

; Say we have a graph whose nodes represent the state of some system, and
; whose directed edges represent allowed next states.  Say also that we
; collect data on successive states of a real system that purports to obey
; this allowed-successor graph, but that our measurements are not precise:
; any given measurement has some chance of being in error.

; One possible problem (out of many imaginable) is:
;
; Given a string of measured states, where some measurements may be in error,
; construct a corrected string by replacing some measured values.  Minimize
; the number of replacements.  Avoid making false "corrections" when the
; measurements really are garbage.

; (For speech recognition, we want to keep the same number of measurements
; as are in the original string, i.e. we want to replace, not drop or add,
; measurements.  This is because the elements in the string represent time-
; sliced frequency spectra -- all the time slices really are present in the
; string, even if their interpretation may be inaccurate.  On the other hand,
; for interpreting an image, we would want to allow dropping or adding string
; elements, because the image may contain irrelevant artifacts that seem to
; be additional features, or some real features may not be visible.)

; There is no recognizable goal -- this is an optimization problem, and there
; is no way to know what's optimal without examining the entire (exponential)
; search space.  So we may need to cut off the search when we get to a "good
; enough" point, and we also need to be sure it will terminate at all.

; There are two main scenarios:  Either we have the entire string to work on,
; so that it *is* (technically if not practically) possible to backtrack as
; far as we need -OR- we are processing the string in realtime.  That is, we
; may need to produce a corrected value within some fixed time after receiving
; the original input value.  In the latter case, backtracking depth is limited
; to what can be processed in the interval.  We would want to discard from the
; queue of possible states anything that we can't reach in the time available.
; This is similar to dropping anything that has a higher cost than the minimum
; solution so far, but fact that the criterion is nothing to do with the
; quality of the result has this bad consequence:  Dropping possibilities
; because we don't have time to examine them means we cannot guarantee an
; optimal result.  In fact, kluging the cost function to *effectively* drop
; some possibilities that would require backtracking (e.g. by reducing the
; cost of replacements further back in the string), is equivalent to actually
; dropping them -- the kluged cost function is not the real cost, so again we
; can't assure an optimal result.

; What I'm going to do first is operate with full backtracking, and a non-
; kluged cost function.  Then I'll see if I can adjust the cost function to
; reduce the value of backtracking.  After that, I'll check into purging from
; the queue any states that won't get used.

; Some assumptions about noise:
;
; *Correctible* errors are considered to be either random or very short in
; duration.  The probablility of a false measurement due to random noise is
; taken to be a fixed value, p, per measurement, so that the probability of
; n false measurements in a row is p^n.  Then if p is small, we don't expect
; long strings of false measurements just due to random error.  Random or
; short duration noise is what there's some hope of correcting given the
; successor graph.  Since we don't expect more than a small number of errors
; in a row (that we can correct), we may want to cut off a search path if it
; requires changing more than some small number of successive measurements.
;
; *Uncorrectible* noise is persistent -- it affects some number of adjacent
; measurements.  This might be static or interference -- sustained noise, like
; yadayadayaSSSSSSyadayada.  Or it might be drop out or dead air, like
; blahblahbl      blahblah.  In these cases we do not want to "fill in" the
; gap with our algorithm's imaginings -- we want to call out the uncorrectible
; parts.  (In this program, symbols that are considered to be uncorrectible
; error are replaced by #.)

;******************************************************************************
; Details, details...
;******************************************************************************

; Unfortunately, the words node and state have two meanings in this program.
; The first meanings are those above, i.e. the meanings that are relevant to
; these problems: nodes in the hypothetical system's allowed successor graph,
; and states of that system.  But since these problems will be handled using
; a search, then "node" might also refer to a node in the search tree, or in
; a priority queue.  And "state" will be a state of the formal representation
; of the problem's search space, namely, variant forms of the sequence of
; measurements or of the measured successor graphs.  For that reason, I'm
; going to use the word "value" to mean the state of the system being
; measured, and reserve "state" for the formal search space.  I don't need to
; refer explicitly to nodes of the search tree, since that exists only as a
; trace of the program execution.  For the...things...in the priority queue,
; I'll use the word "elements".

; All functions in this program will be prefixed with NM- (for "noisy match")
; to keep them from colliding with other names.  I should make this a LISP
: "package", but will defer that since it's irrelevant to the course.

;******************************************************************************
; Data structures
;******************************************************************************

; There are two main structures: a graph of values and allowed successors,
; and a state representing a partially corrected data stream.  

; Since entire strings will be included in the states in the search, we're
; going to be using a *lot* of storage...  It will be nice if states can be
; made to share their strings as much as possible.

;******************************************************************************
; Representation of a state
;******************************************************************************

; Required information for a state is:
;
;   -- the adjusted data string, up to the last value processed
;   -- the remaining unprocessed data

; In order to save space, we might keep a single list representing the raw
; data, and indicate "the remaining data" by a pointer into it.  A list isn't
; very compact, and if there's time, I may try to squish it into strings or
; arrays.

; Also, we might keep only the last processed value in the state, and have
; a pointer to a parent state that would allow us to recover the previous
; values.  This is rather like recovering a path by chaining back through
; parent states.  It does require that we *keep* the parent states around,
; so we must be careful that pruning of non-viable states doesn't get any
; ancestors we'll need.  On the other hand, we'll only be pruning if we're
; imposing a requirement to finish processing within some time limit, and in
; that case, we'll be *outputting* the result as we go along, so we won't
; *need* the ancestors back beyond where we've outputted them...

; The search packages we've written so far all provide handling for costs
; and parents, and themselves recover the path.  I'm going to try to force
; this into a form compatible with one of the already-written searches,
; either the original best-first or A*.  For A*, the heuristic could be 0,
; because the future cost could well be zero, unless we do some looking
; ahead, in which case we might check a few upcoming values to see if any
; are not legal successors.

; Can we get away with having the search package handle the parents?  That
; is, do we need the path -- the previously processed values -- to figure
; out possible successors or calculate the cost?  We don't need the path to
; find successors, because using the successor graph only depends on the
; *current* value.  And for this first version, without a kluged cost function,
; we don't need access to the parent's cost to calculate ours -- we can still
; get away with supplying an incremental cost to the search, and letting it
; maintain the total cost as a simple sum with the parent's cost.  Later,
; we'll need to extend the search to use a total cost function that we supply,
; that will take the parent cost as an argument supplied by the search package.

; After all that, what we need to keep per state is:
;
;   -- the value we just selected
;   -- a pointer to the remaining unprocessed data
;
; Just bundle these into a list.  Since the remaining data is itself a list,
; all we need to do is cons our current value onto it.  This has no effect on
; other states or on the original raw data list.

; Helper functions for the state:
;
;   -- Create a new state with a given value and pointer.
;   -- Given a state, get the value.
;   -- Given a state, get the remaining data.
;   -- Given a state, get the next value to process.
;   -- Given a state, get the remaining data after that next value (which is
;      what will go into this state's successors).
 
; Make a state by prepending the new value onto the rest of the raw values.

(defun NM-make-state (value rest-of-data)
   (cons value rest-of-data))

; The current value is just the first of the state list.

(defun NM-current-value (state)
   (first state))

; The remaining data is just the rest of the state list.

(defun NM-rest-of-data (state)
   (rest state))

; The next value to process is the first value of the remaining data, i.e.
; the second of the state list.

(defun NM-next-value-to-process (state)
   (second state))

; The remaining data after that next value is the rest of the remaining data.
; That's the rest of the rest of the state list, i.e. the cddr.

(defun NM-remaining-data-after-next (state)
   (cddr state))

;******************************************************************************
; Representation of the successor graph
;******************************************************************************

; Represent the graph as a collection of nodes whose contents are:
;
;   -- the value represented by the node
;   -- an optional frequency or probability of encountering this value
;   -- a collection of successors:
;        -- value of the successor
;        -- an optional frequency or probability of encountering this successor
 
; This version doesn't make use of the probabilities.  So a node consists of
; a value and a collection of successor values -- make this a list of two
; elements.

; The collection of successors for each node could be a list of values (if
; it's small) or could itself be a hash table.  For now, since our test cases
; will have no more than half a dozen successors per node, use a list.

; (Why did I prepend the current value to the rest of the values for a state,
; but didn't do that with the node value and successors here?  I could, but...
; The current value and the rest of the values in a state are *peers* -- along
; with the path so far, they form one continuous stream, representing one
; possible processed string.  But in the successor graph, the successor values
; really represent directed edges, not just other nodes -- I'm emphasizing this
; distinction by not lumping them all together.)

; For efficiency in looking up nodes, store the nodes in a hash table indexed
; by node values.  (I got chewed on for making the hash table I used in A*
; search a global rather than passing it as an argument.  So I won't do that
; here.  But in this case, I *can't* pass it as an argument -- it's part of
; the *problem*, not part of the search.  My search code won't know anything
; about the internals of the problem.  So when I provide routines to the
; search code that encapsulate problem features (principally the successors
; function), I'll have to make them closures that incorporate the successor
; graph.)

; Accessor functions for an individual node:

;   -- Given a node, pull out the value.
;   -- Given a node, pull out the successor list.
;   -- Given a node, check if a value is present in the successor list.

(declaim (inline NM-value NM-successors))

; Get the value of a node.  Since the value is the hash key, this is also the
; key function for use with make-hash-table.

(defun NM-value (node)
   (first node))

; Get the successors.

(defun NM-successors (node)
   (second node))

; Check for a value in the successors list.  I'm using equalp because I don't
; want this to break if the values aren't trivial objects.  This means that
; equalp should be used as the hash table test as well.

(defun NM-in-successor-list (value node)
   (member value (NM-successors node) :test #'equalp))

; Accessor functions for the successor graph:

;   -- Retrieve a node given its value.
;   -- Given a value, return the successor list.
;   -- Check if a given value and successor pair is in the graph.

; Retrieve a node given its value.

(defun NM-get-node (value graph)
   (gethash value graph))

;******************************************************************************
; Noisy string cleanup given allowed successors
;******************************************************************************

