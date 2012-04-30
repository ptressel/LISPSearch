; *****************************************************************************
; Text processing functions for preparing data for the decision tree generator
; *****************************************************************************
;
; Input to the decision tree consists of strings of words -- we'll need a way
; to read these in and extract their feature values.  Features are presence or
; absence of words from a lexicon, or ordered pairs of words.
;
; Training data is supplied in files of strings of words, one string per line.
; Each file represents a separate goal, and the name of the goal should be
; put on the first line.
;
; From the training data, we need to generate a lexicon -- a list of all words
; that appear, possibly excluding words that are in a separate words-to-exclude
; list.  For the prototype, the lexicon is generated while processing the
; expressions that are expanded into "sentences", and is available as a global
; *lexicon*, which is a hashtable -- if we have this, we'll convert it to a
; list.  For real sentences, the lexicon will be extracted from the sentences.
;
; Ordinarily, one would present the tree a feature vector.  There will be a very
; large number of features, though, and the feature values can be deduced from
; the sentence itself, so we could use the sentence as input instead.  However,
; the tree generator is simpler if we stick to a feature vector.  Features are
; binary, so we can use an integer to hold the bits.
;
; So during tree generation, we'll rely on the ordering of the lexicon to map
; word and word pair features to bits in the bit vector:  single words will come
; first, then pairs, with the second word being the more rapidly varying index.
; Once the tree is generated, the node tests will all contain the actual words,
; so the lexicon is no longer needed.
;
; To generate the vector for each sentence, we map over the list of "features"
; (words and word pairs) with a function that tests for the presence of a list
; of words in order.

; Input functions *************************************************************

; Get one token.  Reads char by char.  If not a delimiter or end of line, chars
; are accumulated into a string.  User can specify a list of delimiters as
; characters (not strings); if unspecified, default is comma (the standard for
; the UCI test datasets).  User can also ask to have non-alphanumeric characters
; filtered out.  (We don't make the user supply a list of chars to filter, as
; non-alphanumeric is somewhat fuzzy.  But we might add another option to filter
; a supplied list.)  Finally, the user can ask to have the string made all
; lowercase (default is to do so).
;
; Some of the options provided are not needed for our sentence data -- it will
; probably already be lowercase and without extraneous chars.  But speed isn't
; really important during tree generation.

(defun read-token (&key (input-stream *standard-input*)
                        (delimiters (list #\,))
                        (filter-non-alphanum T)
                        (make-lowercase T))
  (let* ((rawchar (read-char input-stream NIL #\page))
         (char (if make-lowercase (char-downcase rawchar) rawchar)))
    (cond
      ; A token is terminated by a delimiter, end of file, or newline.
      ; I'm using the page-feed char as a marker for end-of-file, so don't
      ; put any page-feeds in your data files...
      ((member char delimiters) NIL)
      ((equal char #\page) NIL)
      ; Put back the newline because read-vector needs to see it.
      ((equal char #\newline) (unread-char char input-stream) NIL)
      ; If it's alphanumeric, or if we're not filtering, we want to keep the
      ; char.
      ((or (null filter-non-alphanum) (alphanumericp char))
         (concatenate 'string (make-string 1 :initial-element char)
                              (read-token :input-stream input-stream
                                :delimiters delimiters
                                :filter-non-alphanum filter-non-alphanum
                                :make-lowercase make-lowercase)))
      ; Here, we're filtering and the char isn't alphanumeric -- skip it.
      (T (read-token :input-stream input-stream
                     :delimiters delimiters
                     :filter-non-alphanum filter-non-alphanum
                     :make-lowercase make-lowercase)))))

; Read tokens and accumulate them into a list.

(defun read-vector (&key (input-stream *standard-input*)
                         (delimiters (list #\,))
                         (filter-non-alphanum T)
                         (make-lowercase T))
  (let ((char (peek-char NIL input-stream NIL #\page)))
    (cond
      ((equal char #\page) NIL)
      ; Eat the newline so we don't report NIL next time.
      ((equal char #\newline) (read-char input-stream) NIL)
      (T (let ((token (read-token :input-stream input-stream
                                  :delimiters delimiters
                                  :filter-non-alphanum filter-non-alphanum
                                  :make-lowercase make-lowercase)))
           (if (null token)
             NIL
             (cons token (read-vector :input-stream input-stream
                                      :delimiters delimiters
                                      :filter-non-alphanum filter-non-alphanum
                                      :make-lowercase make-lowercase))))))))

; Read a sentence from a file, separate the words, strip punctuation, switch to
; lowercase, and put the words in a list.  Assumes words are space-separated and
; the sentence ends at the newline.

(defun read-sentence (&optional (input-stream *standard-input*))
  (read-vector :input-stream input-stream
               :delimiters (list #\space)
               :filter-non-alphanum T
               :make-lowercase T))

; Input functions for training data *******************************************
;
; Training data is presented in files, one per class, containing the class name
; on the first line, then the representative sentences, one per line.

; Read a file in the above format.  Process the sentences with read-sentence,
; and make a list containing first the class, then a list of sentence lists.

(defun read-sentence-file (file)
  (with-open-file (data-stream file :direction :input :if-does-not-exist NIL)
    (when (streamp data-stream)

      (let* (; First get the class.
             (class (read-line data-stream))
             ; Then get the sentences.
             (sentences (read-sentences data-stream)))

        ; Make a list of the class and the list of sentences.
        (list class sentences)))))

(defun read-sentences (&optional (data-stream *standard-input*))
  ; Read one sentence, then call ourselves for the rest.  Quit when no more.
  (let ((sentence (read-sentence data-stream)))
    (when sentence (cons sentence (read-sentences data-stream)))))

; We'll need to read one file per class.  Assume the file names have been read
; into a list, which we're given.  We read the files and collect the class
; names into a list, followed by the lists of their representative sentences.
; (Note we expect the first line of each file to contain the name of the class.)
; This is approaching the format dt-learn wants for its input.  For the rest,
; see make-data-lists below.

(defun read-dataset (files)
  (let (; Read in all the files.
        (file-data-list (mapcar #'read-sentence-file files)))
    (if (member NIL file-data-list)
      ; Here, we didn't find all the files -- report the missing ones.
      (mapc #'(lambda (file result)
                (unless result (format t "File not found: ~S~&" file)))
            files file-data-list)
      ; Here, we found all the files.  Rearrange as needed by dt-learn.
      (let ((classes (mapcar #'first file-data-list))
            (sentence-lists (mapcar #'second file-data-list)))
        ; We want the classes to stay in their list, but want to tack the
        ; individual lists of sentences on behind that.
        (cons classes sentence-lists)))))

; Set functions with equal ****************************************************

; These are only ever used with equal, and I keep forgetting to put it
; in...grrr!

(defun union-equal (list1 list2)
  (union list1 list2 :test #'equal))

(defun member-equal (item a-list)
  (member item a-list :test #'equal))

(defun set-difference-equal (list1 list2)
  (set-difference list1 list2 :test #'equal))

; Lexicon preparation *********************************************************

; Given a list of lists of word lists (sentences), collect a list of all words
; contained therein, minus words in an "excluded words" list.  (This form of
; lexicon generation is not used by the prototype -- see input.lsp.)

(defun make-lexicon (list-of-sentence-lists excluded-words)
  ; Collapse sentence lists into one list, dropping duplicates.  Note there are
  ; two levels of list to collapse.  Remove excluded words.
  (set-difference-equal
    (reduce #'union-equal (reduce #'union-equal list-of-sentence-lists))
    excluded-words))

; Feature list ****************************************************************

; Given a lexicon, make a feature list.  Here, we form words and word pairs.
; If we ever consider using triples, we should not use this method (there would
; just be too many triples), so no argument is provided for altering the type of
; features.  If we do need triples (or higher order combinations), suggest that
; we extract pairs, and see if there are pairs of the form a..b and b..c.  Only
; if so, *then* look for a..b..c.

(defun make-pairs-with-one-word (word lexicon)
  ; Make a list of pairs '(word word2) where word2 is each word in the lexicon.
  (mapcar #'(lambda (word2) (list word word2)) lexicon))

(defun make-all-pairs (lexicon)
  ; Make a list of pairs of words from the supplied lexicon.  For each word in
  ; the lexicon, form a list of pairs with every word in the lexicon.  Append
  ; all these lists together.
  (reduce #'append (mapcar #'(lambda (word)
                               (make-pairs-with-one-word word lexicon))
                           lexicon)))

(defun listify (lexicon)
  ; Make a list with each item in the supplied list packaged in its own list.
  (mapcar #'list lexicon))

(defun make-features (lexicon)
  ; Make word pairs, then prepend the words in the lexicon, each made into a
  ; list.
  (append (listify lexicon) (make-all-pairs lexicon)))

; Feature extraction **********************************************************

; During production, we aren't given a feature vector, so we need a means to
; tell if what we do get (words in the input sentence) contains a particular
; feature (words in some order, not necessarily adjacent).  This function will
; be inserted in the decision tree code's cond statements to test features.  It
; will also be used during decision tree generation to make the training data
; into feature vectors.

(defun contains-in-order (list-to-check items-to-check-for)
  ; Look for first item, then call ourselves recursively on remainder of list.
  ; If items-to-check-for is non-empty, and we don't find the first item in
  ; the list, return NIL.  If we exhaust the items to check for, return T.
  (if (null items-to-check-for) T
    (let (; This will be NIL if the first item to check for isn't in the list,
          ; else it will contain the tail of the list beginning with the item.
          (remainder (member-equal (first items-to-check-for) list-to-check)))
      (when remainder
        ; Take off the item we just found from both the list to check and the
        ; items to look for, then continue on to look for the rest.
        (contains-in-order (rest remainder) (rest items-to-check-for))))))

; Convert a sentence into a feature vector.  (Note this is *only* done for the
; training vectors -- it would be a waste of effort for a sentence being
; classified by the tree, as only a small number of features will be checked.) 

(defun sentence-to-vector (sentence features)
  (do* (; Get a bit corresponding to the position of the current feature in the
        ; vector.
        (bit-position 1 (ash bit-position 1))
        ; Clip features off the feature list as we process them.
        (remaining-features features (rest remaining-features))
        ; Start with an empty vectorl
        (vector 0))

       ; Quit when we run out of features.  Return the vector.
       ((null remaining-features) vector)

       ; If the first of the remaining features is true, set its bit.
       (when (contains-in-order sentence (first remaining-features))
         (setf vector (logior vector bit-position)))))

; Convert all sentences in the sentence lists to vectors.  Caller should pass us
; just the sentence lists.

(defun sentences-to-vectors (sentence-lists features)
  ; Step through lists of sentences.  Step though each list and convert
  ; its sentences.
  (mapcar #'(lambda (sentence-list)
              (mapcar #'(lambda (sentence)
                          (sentence-to-vector sentence features))
                      sentence-list))
          sentence-lists))

; Accessors for return values *************************************************
;
; The "make-data-lists" functions below package up their data in a list of the
; form:
;   (features classes class-1-vectors class-2-vectors ... )
; where "features" is a list of lists of words or word pairs, classes is a list
; of the class names, from the first lines of the dataset files, and the
; remaining elements are lists of the feature vectors belonging to each class,
; in the same order as the classes list.  The values in the feature vectors are
; in the same order as the features in the features list.

; The first element is a list of features.

(defun get-features (data-lists)
  (first data-lists))

; The second element is a list of classes.

(defun get-classes (data-lists)
  (second data-lists))

; The tail of the list after classes is the collection of feature vectors.

(defun get-vector-lists (data-lists)
  (cddr data-lists))

; Make a data-lists out of features, classes, and vectors.

(defun assemble-data-lists (features classes vector-lists)
  (cons features (cons classes vector-lists)))

; Top-level functions *********************************************************
;
; These functions read in the data and prepare it for dt-train.

; Starting with a list of classes followed by lists of sentences in each class
; (produced by read-dataset):  Generate the lexicon and feature list.
; Convert all sentences into feature vectors.  Add the feature list on the
; front.
;
; At the end, we have a list containing the feature list, the class list, and
; lists of feature vectors per class, in the same order as the class list.
; This will be the input to dt-learn's decision tree generator, make-node.
;
; Call this with the result of read-dataset, and with the list of classes:
; (make-data-lists (read-dataset '("file1" "file2" "file3" ... )))

; This version expects the lexicon to be present in a hashtable *lexicon*.

(defun make-data-lists-for-prototype (raw-dataset excluded-words)
  (let* (; Expect that we have a hashtable from running the expression
         ; parser -- convert that to a list.  This will get an error if there's
         ; no *lexicon*.
         (lexicon-end NIL)
         (lexicon-all (maphash #'(lambda (key value)
                                   (declare (ignore value))
                                   (push key lexicon-end)) *lexicon*))
         (lexicon (set-difference lexicon-all excluded-words :test #'equal)))

    ; Helper (below) will do the rest of the work.
    (make-data-lists-given-lexicon raw-dataset lexicon)))

; This version generates the lexicon from the input sentences.

(defun make-data-lists (raw-dataset excluded-words)
  (let* (; Extract the lexicon from the sentences.  (The dataset has the list
         ; of classes first -- take that off to get just the sentence lists.)
         (lexicon (make-lexicon (rest raw-dataset) excluded-words)))

    (make-data-lists-given-lexicon raw-dataset lexicon)))

; This continues the work for either of the above functions.

(defun make-data-lists-given-lexicon (raw-dataset lexicon)
  (let* (; Make the feature list.
         (features (make-features lexicon))

         ; Get classes.
         (classes (first raw-dataset))

         ; Convert sentences to feature vectors.  Pass in sentence lists, which
         ; follow class list.
         (vector-data (sentences-to-vectors (rest raw-dataset) features)))

    ; Combine these into a data-lists.  The function assemble-data-lists is an
    ; accessor for data-lists, and is found in dt.lsp, so that must be loaded
    ; before this is run.
    (assemble-data-lists features classes vector-data)))
