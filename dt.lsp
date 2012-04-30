; DT-LEARN, DT-PERFORM ********************************************************
;
; DT-LEARN implements a decision tree learner based on ID3 with a few
; extensions from those parts of C4.5 relevant to categorical data.
; It generates code for a function that runs the tree, and writes it to
; a file.  User can then load the file and use the tree by calling the
; function DT-PERFORM with one feature vector:
;
;   (dt-perform vector)
;
; Call to dt-learn is:
; 
;   (dt-learn (list "file1" "file2" ...) excluded-words)
;
; Arguments "file1", "file2", etc., are paths, in the form of quoted strings,
; of files containing the dataset to be used.
;
; The dataset consists of one file per class, with the class name on the
; first line, followed by examples in that class, one per line. 
;
; Examples are assumed to be in the form of a sequence of words -- a sentence --
; where the words are separated by a space.
;
; Excluded-words is a list of words that should be ignored.
;
; After the excluded words list, several keyword arguments can be included
;
; :min-samples <integer>
;   If this many or fewer samples are available at a node, stop and make this
;   a leaf.  Default is 5.  NOTE: May want to change this such that if some
;   branches at a node contain less than min-samples samples, even though the
;   node as a whole has > min-samples, the underpopulated branches are grouped.
;   Make groups of no more than necessary to get above min-samples, and choose
;   combinations that have similar main categories.  (There will be few enough
;   that grouping them will not be too time-consuming).  If class-if-below-min
;   is not 'vote, this grouping is moot.
;
; :max-depth <integer or NIL>
;   If not NIL, quit at this many features in a conjunct (i.e. paths are
;   limited to this many decision nodes.  Default is NIL (i.e. don't constrain
;   depth.
;
; :class-if-below-min <class or 'vote>
;   If a subtree is truncated due to having fewer than the :min-samples cutoff
;   or reaching the :max-depth cutoff, substitute this class.  If the special
;   name 'vote is used, take the majority class of the samples remaining at
;   this node.  Default is 'vote.
;
; :output-file <string>
;   Name of the file to which the dt-perform code should be written.
;   Default is "dt-perform.lsp".
;
; For instance, a call to dt-learn might look like:
;
; (dt-learn (list "target1.data" "target2.data" "target3.data")
;           (list "a" "an" "the") :min-samples 0 :max-depth NIL)
;
; There is no return value.  Instead, dt-learn produces code for function
; DT-PERFORM, and writes it to a file.  Load this file, then call dt-perform
; with a single vector whose form is same as in the original data but without
; the assigned class (i.e. there is one fewer item in the comma-separated list).
; To run dt-perform:
;
; (load "dt-perform.lsp")
; 
; (dt-perform <sentence>)
;
; where <sentence> is a list of words, e.g. (list "this" "is" "a" "sentence").
;
; Return value is a class.

; INPUT and associated TEXT MANIPULATION **************************************

; Input and text manipulation functions are in text.lsp -- get it.

(load "text.lsp");

; STRING HELPERS for OUTPUT ***************************************************
;
; Since we're producing code as output, we do a fair amount of string
; manipulation.  In particular, we often need to make quoted strings.

; Make a quoted string out of the supplied text (itself a string).

(defun make-quoted-string (text)
  (concatenate 'string
    (princ-to-string #\")
    text
    (princ-to-string #\")))
  
; Helper for making strings:  Given a list of strings, make a text string that's
; a Lisp-style list containing those strings.

(defun make-string-of-list (list-of-strings)
  (concatenate 'string
    ; Start the list.
    "(list "
    ; Insert strings, quoted and space-separated, as list contents.
    (reduce
      ; Join new string to what we have so far.
      #'(lambda (prev-string new-string)
          (concatenate 'string
            prev-string
            " "
            (make-quoted-string new-string)))
      ; Get strings out of the supplied list.
      list-of-strings
      ; Start with an empty string so the first item gets processed as the
      ; "new-string" argument to the above function, and gets quoted.
      :initial-value "")
    ; Close the list.
    ")"))

; Write out a string without escaped characters.  This avoids getting strings
; with \" instead of ".  Use "~A" not "~S" -- "~A" uses princ, which does not
; add escapes, whereas "~S" uses prin1, which does.

(defun write-without-escaped-chars (text output-file)
  (with-open-file (output-stream output-file :direction :output)
    (format output-stream "~A" text)))

; BIT OPERATIONS **************************************************************
;
; Because features are binary (presence or absence of words and word pairs),
; feature vectors can be represented compactly as integers.  A few convenience
; functions will be useful.

; Make an integer with a one bit at the specified position.

(defun makebit (index)
  (byte 1 index)) ; or could use (ash 1 index)

; Set the bit at the given index in the given number.  To change the bit in
; place (i.e. destructively), setf this back into the original place.

(defun setbit (index number)
  (logior number (byte 1 index))) ; or could do (dpb 1 (byte 1 index) number)
                                  ; but logior may be faster

; Turn off a bit.

(defun clrbit (index number)
  (logandc2 number (byte 1 index)))

; To test whether a particular bit is on, do (logbitp index number).

; Compare a particular bit in a number to a supplied bit value.

(defun cmpbit (index number value)
  (= value (ldb (byte 1 index) number)))

; Make sure a value is represented by 1 and 0 -- if needed, convert T and NIL
; to 1 and 0.

(defun bitvalue (value)
  (if (or (null value) (equal value 0)) 0 1))

; DECISION TREE GENERATION ****************************************************
;
; Once we have the file read in, we fall into a tree traversal -- DFS, because
; the intent is to produce Lisp code for the conditional expression
; representing the tree, and will have nested expressions, which is natural with
; DFS.  This conditional can then be wrapped in a defun and saved in a file for
; future use.  I'll store it as a string, not a list, so I can write it out as a
; file without having to fight with the Lisp printer.
;
; Remember, in the following, that we are *writing code* to process *one* (real)
; sentence (not training data).  (We process the training data right here.)
;
; We don't start with feature vectors in the traditional sense -- not a list of
; feature values -- so our first step is to create them.  Even though there may
; be roughly a thousand features (e.g. for a lexicon of size 30 there are 930),
; it is still less work to test for features up front, because otherwise, when
; filtering down through the levels while generating the tree, we'd be doing a
; test for all but the previously-selected features at each tree level on every
; training sentence.
;
; On the other hand, we will not extract a feature vector for sentences
; *processed by* the tree while running, because we'll only be testing each
; feature once, and only a small fraction of the possible features.
;
; For our word or word pair features, we only have "present" or "absent" as
; values, i.e. all are binary.  We use true to represent presence and NIL for
; absence -- this is what the feature predicate, contains-in-order, returns.
;
; At this point, we have for each class a collection of binary feature vectors.
;
; At any node, we test one feature, and split into branches for each of its
; values (there are no missing features, else we'd have a branch for that).
; This corresponds to an "if" statement.
;
; When we follow a branch, we select out only those sentences that have the
; feature value corresponding to that branch -- others are dropped.  So at any
; node, we have data in exactly the same form as at the root -- just less of it.
;
; When we're creating the code for any node, our caller will have two string
; fragments that belong before and after our code.  We'll just generate a
; string for this node as though it were the root, and pass it back.  The
; caller will combine the results.
;
; The code to make one node does this:  First we decide if it's an end node
; (i.e. if all remaining vectors belong to a single class, or we've dropped
; below min-samples, or the level has reached max-depth.  If it is, we return a
; string containing the appropriate class, which depends on the user's
; settings -- it's either the most common class, or the one they specify.
;
; If it's not an end node, we calculate the change in information for each
; (remaining) feature.
;
; (Note re. information gain:  ID3 compares the information gain, which is the
; current node's information minus that of all successors when split on an
; feature.  However, there is no need to calculate the information in the
; current node, because that's the same for all features -- just need to
; remember that minus sign in front of the sum of information over the values.)
;
; Pick the best feature and make a test for it.  This yields two branches, so
; make it an "if" statement.
;
; In the place where the result value of each "if" branch goes, make a
; recursive call with the subset of vectors belonging to this branch, and with
; the current feature filtered out.  Insert the result into the "if" and
; close off this "if" option.
;
; Continue for the remaining branches, finish off the "if", and write it out.

; HELPERS for DECISION TREE GENERATION ****************************************

; Count the total number of vectors in vector-lists.

(defun total-vectors (vector-lists)
  (reduce #'+ (mapcar #'length vector-lists)))

; Helper function that makes a list of integers from 0 to k-1 given k.
; (Numerical indices into the features are used freqently.  This list assists
; in running mapcar over them.)

(defun make-index-list (num)
  ; Count backwards from k-1 and cons the numbers onto the front of a list.
  (do
    ((index (1- num) (1- index))
     (index-list NIL (cons index index-list)))
    ((= index -1) index-list)))

; Helper that locates the class with the largest number of examples, and
; gets the corresponding class name.

(defun find-biggest-class (classes vector-lists)
  (let (; Step through vector lists; get length; compare with those already
        ; examined -- if bigger, get the index of the current list, else keep
        ; the old index.  (A triple of values is used as an iterator: index of
        ; biggest class so far, its size, and current class index.  This could
        ; be done with mapc and side effects, as in pick-best below.  Which is
        ; uglier?  Dunno, but at least this one has no setfs...  Maybe I'll
        ; rewrite it with do*.)
        (index
          (first (reduce
                   ; Compare current vector list with previous biggest.  If
                   ; same size, stick with the previous one.
                   #'(lambda (bestindex-bestsize-curindex vector-list)
                       (let ((bestindex (first bestindex-bestsize-curindex))
                             (bestsize (second bestindex-bestsize-curindex))
                             (curindex (third bestindex-bestsize-curindex))
                             (cursize (length vector-list)))
                         (if (> cursize bestsize)
                           ; Found a bigger one.  Make a new triple with its
                           ; index and size, and increment the current index.
                           (list curindex cursize (1+ curindex))
                           ; Keep the previous best, but bump the current index.
                           (list bestindex bestsize (1+ curindex)))))

                   ; Vector lists
                   vector-lists

                   ; Start with best size = -1 so it'll be replaced no matter
                   ; what.  Start with curindex = 0 because on each pass,
                   ; curindex is the index of the vector list we're examining.
                   :initial-value (list -1 -1 0)))))

    ; Get the class name corresponding to the biggest vector list.
    ; Wrap it in quotes.
    (make-quoted-string (nth index classes))))

; Given a vector, check if it has the specified value for the specified
; feature.  (Recall feature values are bits in integers, with 1 representing
; true, and 0 representing NIL.)  Return the vector if the value matches,
; else NIL.

(defun feature-has-value (vector index value)
  (when (cmpbit index vector (bitvalue value)) vector))

; Given a vector list, apply feature-has-value to produce a list of vectors or
; NILs, where the vectors are those that had the right value (it's been
; stripped), and the NILs are in the places where there were vectors without
; the right value.  Then remove the NILs.

(defun filter-one-list (vector-list index value)
  (remove-if #'null
    (mapcar #'(lambda (vector) (feature-has-value vector index value))
            vector-list)))

; Given a feature index and a value, along with vector-lists, filter out
; vectors that don't have that value for the specified feature.  Return a
; reduced vector-lists.

(defun filter-on-value (vector-lists index value)
  ; Remove vectors that don't have this value.
  (mapcar
    #'(lambda (vector-list) (filter-one-list vector-list index value))
    vector-lists))

; For each value (T or NIL) of the given feature (denoted by its index), make
; a new vector-lists containing only the vectors with that value.  Return the
; pair of vector-lists in a list with the T vector-lists first.
;
; If the feature is selected, these vector-lists will be used by the node
; generator to make nodes for each value.

(defun split-on-feature (vector-lists index)
  ; Split up vector-lists by starting with the whole thing for each value,
  ; then filtering out vectors that don't have that value.  Resulting split
  ; vector-lists are in order with the list for value T first, then for NIL.
  (mapcar #'(lambda (value)
              (filter-on-value vector-lists index value)) '(T NIL)))

; Helper for use when we're in an end case, and need to decide whether to vote
; or use the supplied class-if-below-min value.

(defun vote-or-use-supplied-class (classes vector-lists class-if-below-min)
  ; If the user wants to vote, find the most populous class, else use their
  ; supplied class.
  (if (equal class-if-below-min 'vote)
    ; Here if we want to vote.
    (find-biggest-class classes vector-lists)
    ; Here to use the fixed class -- make a quoted string.
    (make-quoted-string class-if-below-min)))

; Test for end case and return NIL if not an end case, or a string with the
; chosen class.  We are at an end case if: 1) there are <= min_samples
; vectors total, 2) all samples have the same class, or 3) the tree level is
; deeper than max-depth.

(defun end-case (classes vector-lists
                 min-samples class-if-below-min max-depth depth)

  ; See if we've fallen below min-samples.
  (if (< (total-vectors vector-lists) min-samples)

    ; Here, there are too few vectors to continue.  Depending on
    ;class-if-below-min, take a vote or use the supplied class.
    (vote-or-use-supplied-class classes vector-lists class-if-below-min)

    ; Here, we have enough vectors.  Find out if they're all one class, and
    ; if so, return that class.
    (if (= 1 (length (remove-if #'null vector-lists)))

      ; Here, there's just one class surviving.  Get its index and pull out
      ; that class name.  Wrap it in quotes.
      (make-quoted-string (nth (position-if-not #'null vector-lists) classes))

      ; Here, we have more than enough vectors to proceed, and they're not
      ; all the same class.  See if we want to truncate the tree due to depth,
      ; which we do if the user has supplied a non-NIL max-depth, and if we're
      ; deeper than that.
      (if (and (not (null max-depth)) (> depth max-depth))

        ; Here, we've gone one level deeper than the cutoff.  Use the same
        ; vote-or-fixed-class choice as above.
        (vote-or-use-supplied-class classes vector-lists class-if-below-min)

        ; Here, we survived all the end-case checks -- say we're not done.
        NIL))))

; Compute -(p log2 p) given number of vectors with one value, and total vectors.
; Define p log2 p to be 0 if p is 0.

(defun mplogp (num total)
  (if (= 0 num)
    0
    (let ((fraction (/ num total)))
      (- (* fraction (log fraction 2))))))

; Compute the entropy in a vector-lists.  Recall it's split into classes.
; Sum -(p log2 p) over classes, where p = number of vectors in class / total
; number of vectors.  !!! Find Quinlan's paper in which he discusses having more
; than two classes, to make sure there are no hidden gotchas.  Since we've
; counted all the vectors, and we'll need that when we average over values,
; return that as well as the entropy.

(defun entropy (vector-lists)
  (let* (; Get counts of vectors in each class.
         (num-per-class (mapcar #'length vector-lists))
         ; Could call total-vectors to get this, but we've done half its work.
         (total-vectors (reduce #'+ num-per-class))
         ; Form a list of -(p log2 p).
         (mplogp-list (mapcar #'(lambda (num) (mplogp num total-vectors))
                              num-per-class)))
    (list total-vectors (reduce #'+ mplogp-list))))

; Compute the information change for the specified feature.  If we were to use
; this to compute the gain, we'd subtract it from the entropy at the current
; node.  Since we're not bothering with the entropy at the current node, we just
; negate this.  We call split-on-feature here, which makes the split vector
; lists we'll need if we pick this feature.  So we return them too.

(defun info-change (vector-lists index)
  (let* (; Split vectors on this feature.
         (split-vector-lists (split-on-feature vector-lists index))

         ; Compute the entropy for each value, along with the number of vectors
         ; having that value.  Each vector-lists represents a separate value.
         (entropy-per-value (mapcar #'entropy split-vector-lists))

         ; Total number of vectors.
         (total (total-vectors vector-lists))

         ; To average over the values, we include a portion of entropy for each
         ; value, weighted by the fraction of those values among the total.
         ; Recall our list of entropies has pairs with the count of vectors
         ; having a value, then the entropy for that value.
         (fraction-of-entropy
           (mapcar #'(lambda (e) (* (/ (first e) total) (second e)))
                   entropy-per-value))

         ; Sum the fractions to form the average entropy for this feature.
         ; Negate it so we can test for the highest value of info-change.
         (info-change-value (- (reduce #'+ fraction-of-entropy))))

    ; Package up the info-change-value with the split lists.
    (list info-change-value split-vector-lists)))

; Helper for make-node:  Get the info and split vector lists for each feature.
; We have a list of index numbers for the remaining unused features, which
; we can mapcar through to examine the remaining features.

(defun get-info-and-split-vector-lists-list (features classes
                                             feature-indices vector-lists)
  ; For each remaining feature, get the info and split vector lists.  Return
  ; them all in a list.
  (mapcar #'(lambda (index) (info-change vector-lists index))
          feature-indices))

; Helper for make-node:  Given a list of pairs of info and split vector lists
; for each feature, pick the best.  Return its index and split vectors.

(defun pick-best (feature-indices info-and-split-vector-lists-list)
  (let* (; Start with the first remaining feature as the "best so far".
         (best-index (first feature-indices))
         (best-info-and-split-vector-lists
           (first info-and-split-vector-lists-list)))

    ; Step through other features, selecting the better of the current best
    ; and the next feature.
    (mapc #'(lambda (next-index next-info-and-split-vector-lists)
              ; Compare current info-change with previous best.
              ; If better, replace it.  (Info-change is the first item in
              ; info-and-split-vector-lists.)
              (when (> (first next-info-and-split-vector-lists)
                       (first best-info-and-split-vector-lists))
                (setf best-index next-index)  ; Sometimes ya just gotta setf...
                (setf best-info-and-split-vector-lists
                      next-info-and-split-vector-lists)))

          ; Tails of the supplied lists.
          (rest feature-indices)
          (rest info-and-split-vector-lists-list))

    ; Return the index of the best feature and its split vectors (remove the
    ; info value, because we don't need it once we've picked the best).
    (list best-index (second best-info-and-split-vector-lists))))

; MAKE-NODE and MAKE-IF-STATEMENT *********************************************
;
; This pair constitutes the recursive operation that constructs the tree --
; they make a "node" and call themelves to make child nodes.
;
; Recall that we are constructing code, as a string.  This has to work for any
; node, including the root.  We have the feature list, class names, and the
; user's parameters, which are all static.  What changes at each node are the
; lists of vectors in each class -- we have only the ones with the feature
; values along the path to the current node.  In addition there are two
; bookkeeping values -- the depth (for checking against max-depth) and an
; indent string for pretty-printing.
;
; First, we check if we're at an end node.  If we are, we return the class as
; a string -- our caller (us, since we're called recursively) will take care
; of whitespace if needed.  Note that a bare value is fine as the body of a
; function -- if we're at the root, and there's just one class, then that's
; what we want to return when the run-time tree is used.
;
; If we're not at an end node, we extract the information we need to select
; the best feature, then make an "if" statement to test the value of that
; feature.  In the "if" branches, we call ourself recursively.
;
; Arguments in common between make-node and make-if-statement:
;
; min-samples, class-if-below-min, max-depth -- see keyword args to dt-learn
;                                               at top of this file
; depth           = depth in decision tree of this node; root is 0
; indent-string   = blanks to prepend to code, so it's pretty-printed
; feature-indices = indices of the remaining features -- those we have
;                   available to pick from at this node
;
; Other arguments to make-node:
;
; vector-lists = the entire vector-lists at this node, i.e.:
;   (list vectors-in-class-1 vectors-in-class-2 ...)
;
; Other arguments to make-if-statement:
;
; feature = the selected feature to test at this node
; split-vector-lists = a list of vector-lists, one for each value of the given
;                      feature.  (These are packaged in a list because in the
;                      general case, the feature might have an arbitrary number
;                      of values.)

; Form the "if" statement derived from the given feature, with each branch's
; contents generated by recursive calls to make node on each of a pair of
; vector-lists, one for each value of the feature, T or NIL.

(defun make-if-statement (features classes feature-indices
                          feature split-vector-lists
                          min-samples class-if-below-min max-depth
                          depth indent-string)

  ; Wrap an "if" statement around recursive calls to make-node.  Note we
  ; increment the depth, decrement the number of features, and add some spaces
  ; to the indent.  The "if" test uses contains-in-order to check if the
  ; supplied feature is in the sentence.  The first if branch, for value T,
  ; uses the first split vector-lists, and the second branch gets the second
  ; vector-lists.

  (concatenate 'string 

    ; Start the "if" statement and its test, contains-in-order. First argument
    ; to contains-in-order is the sentence variable name, "sentence".
    (princ-to-string #\newline)
    indent-string
    "(if (contains-in-order sentence "

    ; Second argument to contains-in-order is the feature.  We have a list of
    ; words, where the words are strings.  We need a text string of a list
    ; of those words, where the words are quoted.
    (make-string-of-list feature)

    ; Close off the test.
    ")"

    ; Add the first (T) branch.
    (princ-to-string #\newline)
    (make-node features classes feature-indices (first split-vector-lists)
               min-samples class-if-below-min max-depth
               depth indent-string)

    ; Add the second (NIL) branch.
    (princ-to-string #\newline)
    (make-node features classes feature-indices (second split-vector-lists)
               min-samples class-if-below-min max-depth
               depth indent-string)

    ; Close off the if.
    ")"))

(defun make-node (features classes feature-indices vector-lists
                  min-samples class-if-below-min max-depth
                  depth indent-string)

  (let (; Get the class if this should be an end node, or NIL if not.
        (end-class (end-case classes vector-lists
                             min-samples class-if-below-min max-depth depth))
        ; We'll need the deeper indent for both if branches, so make it here.
        (next-indent-string (concatenate 'string "  " indent-string)))

    (if end-class
      ; Here if we're at an end node -- just return the class string.
      (concatenate 'string next-indent-string end-class)

      ; Here if we have more work to do...  We want to pick the best feature
      ; to split on.
      (let* (; For each feature, calculate the info-gain and get the
             ; split lists.
             (info-and-split-vector-lists-list
               (get-info-and-split-vector-lists-list
                 features classes feature-indices vector-lists))

             ; Select the feature with the highest info-gain.
             (best-index-and-split-vector-lists
               (pick-best feature-indices info-and-split-vector-lists-list))
             (best-index (first best-index-and-split-vector-lists))
             (split-vector-lists (second best-index-and-split-vector-lists))
             ; Extract the best feature out of the features list.
             (feature (nth best-index features))
             ; Remove the chosen feature from the feature index list.
             ; (Warning!  Do not attempt to setf this to save space.  Each
             ; path gets its own variant depending on the features previously
             ; selected along that path.)
             (reduced-feature-indices (remove best-index feature-indices)))

        ; Make the "if" statement for this feature.  (Since we removed the index
        ; for the chosen feature from feature-indices, we may as well update
        ; the other values that will need to be adjusted for the new tree level
        ; (depth and indent-string) here, rather than leaving some for
        ; make-if-statement to do.)
        (make-if-statement features classes reduced-feature-indices 
                           feature split-vector-lists
                           min-samples class-if-below-min max-depth
                           (1+ depth)
                           next-indent-string)))))

; DT-LEARN ********************************************************************
;
; This is the top-level call.  It does a sanity-check on the arguments; makes
; the initial data-lists (consisting of the feature list, class name list, and
; vector lists for each class) by calling read-dataset and make-data-lists;
; calls make-node to get the decision tree code string as a big "if" statement;
; adds a "defun" wrapper and a "compile" on the end, and writes the whole mess
; out.

(defun dt-learn (dataset-files excluded-words
                 &key (min-samples 5) (class-if-below-min 'vote)
                      (max-depth NIL) (output-file "dt-perform.lsp"))

  (let* (
         ; Sanity checks:  We can't allow class-if-below-min to be NIL, nor
         ; can we allow min-samples to be zero, nor max-depth to be anything
         ; but a positive integer or NIL, nor training-fraction (if we were
         ; using it) to be outside of (0,1), not inclusive.
         (safe-min-samples (if (= 0 min-samples) 1 min-samples))
         (safe-class-if-below-min (if (null class-if-below-min)
                                    'vote class-if-below-min))
         (safe-max-depth (when (or (null max-depth)
                                   (and (typep max-depth 'number)
                                        (plusp max-depth)))
                           max-depth))

         ; Read in the data.  For now, the user should split off the training
         ; data and pass just that to us.
         (data-lists (make-data-lists (read-dataset dataset-files)
                                      excluded-words))

         ; Extract features, classes, vectors lists from the data-lists.
         (features     (get-features      data-lists))
         (classes      (get-classes       data-lists))
         (vector-lists (get-vector-lists  data-lists))

         ; Find out how many features are there are by looking at the size of
         ; the feature list.  Then make a list of indices for all features.
         ; As we use a feature, we'll remove its index from the list.
         (feature-indices (make-index-list (length features)))

         ; Generate a string of code.  Concatenate pieces that are produced
         ; separately.
         (code
           (concatenate 'string

             ; Start off with a defun.
             (princ-to-string #\newline)
             "(defun dt-perform (sentence)"
             (princ-to-string #\newline)

             ; Generate the big conditional representing the tree.  It returns
             ; a string with the name of the class.
             (make-node features classes feature-indices vector-lists
                        safe-min-samples safe-class-if-below-min
                        safe-max-depth 0 "")

             ; Close off the defun and add a compile.
             ")"
             (princ-to-string #\newline)
             (princ-to-string #\newline)
             "(compile 'dt-perform)"
             (princ-to-string #\newline))))

      ; Done with the code string (concatenate was closed just above).
      ; Write it out.  Use "~A" not "~S" so that quotes don't get escaped.
      (with-open-file (output-stream output-file :direction :output)
        (format output-stream "~A" code))))

; TESTING *********************************************************************

; Provide a function that runs dt-perform on a set of test examples and reports
; how many were right out of what total.  We expect that dt-perform has
; already been loaded.  For now, caller must separate test from training
; examples, and pass in only test examples.

(defun dt-test (dataset-files)
  (let* (; Read in the test data.  Pull out the class list and example lists.
         ; (Note we stick with sentences here -- no conversion to vectors.)
         (data (read-dataset dataset-files))
         (classes (first data))
         (sentence-lists (rest data))

         ; For each class, run the sentences through dt-perform.  Collect a
         ; list of classes.
         (result-lists (mapcar #'(lambda (sentence-list)
                                    (mapcar #'dt-perform sentence-list))
                               sentence-lists))

         ; Filter out the ones that don't match their true classes.
         (correct-lists (mapcar #'(lambda (result-list class)
                                    (remove-if-not #'(lambda (result)
                                                       (equal result class))
                                                   result-list))
                                result-lists
                                classes))

         ; Count the ones that match their true classes.  At the end, sum them.
         (correct-counts (mapcar #'length correct-lists))
         (correct-total (reduce #'+ correct-counts))

         ; Total sentences in the test set.
         (num-vectors (total-vectors sentence-lists)))

    ; Show results.
    (format t "~%Number correct ~D out of ~D~%" correct-total num-vectors)))

