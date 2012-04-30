; This is used to generate test data for dt-learn.  See dt.lsp.
;
; To save typing, input data for the text prototype goal recognizer can be
; supplied in the form of simplified regular expressions.  These include the
; usual:
;
;   (   )  for grouping
;   [   ]  surrounding parts that can be omitted
;     |    for separating items in a list of options, any one of which is to
;          be included
;
; None of the indefinite repetition (pumping) operations are included...
;
; One can also define named sub-expressions that can be substituted in others:
;
;   %<name> = <expression>
;
; E.g. one might say:
;
;   %account = ( my | our ) (([ checking | savings | bank ] account ) | IRA )
;   Have I got ( any money | anything ) in %account [please]?
;
; Phrases separated only by whitespace (as "any money" above) are treated as
; a unit (the expression "( any money | anything )" expands to "any money"
; and "anything", not "any money" and "any anything").
;
; Caution:
;
; A variable substitution is just a direct text insertion -- it does not have
; an implied () around it.  E.g. the expressions:
;
;   %x = a | b
;   c %x d
;
; become:
;
;   c a | b d
;
; which expands to two strings:
;
;   c a
;   b d
;
; This is not the same as:
;
;   %x = ( a | b )
;   c %x d
;
; which expands to:
;
;   c a d
;   c b d
;
; Note that:
;
; Operations can only add, never remove, variants.  
;
; A section of text without any operators can only produce one variant.
;
; An expression surrounded by ( ) may yield any number of variants.
; Each such variant can be inserted in every enclosing variant.
;
; An expression surrounded by [ ] yields all the variants that would be
; produced if it were ( ) plus the unmodified enclosing variants, i.e. to
; the substrings produced by ( ), the null string is added.
;
; Expressions joined by | yield each of the substrings yielded by any of the
; expressions.

; Expressions will be read in from a file.  There should be one expression
; per line -- don't break them across lines.  Likewise, the left-hand-side
; of a named sub-expression assignment should be on the same line as the
; expression.  Put named sub-expressions before the expressions that use them.
;
; Usage:  At a DOS prompt, start clisp by running clisp.bat.  On using,
; type clisp.  If there's not already an "input.fas" file, compile input.lsp:
;   (compile-file "input.lsp")
; Then load the compiled file:
;   (load "input.fas")
; If you aren't saving an old lexicon across calls, initialize it:
;   (setf *lexicon* NIL)
; Then execute:
;   (process-and-randomize-exp-file "class-name" "exp-file" "sentences-file")
; where class-name is a (short) string describing the category, exp-file is the
; path of the input file, containing expressions, and sentences-file is the
; path where the output file, containing the resulting sentences, should go.
; This will return the number of lines produced.

; First we need to read in the string and make a list of tokens, which are
; just the unpunctuated text strings and operator punctuation, each as its
; own string.  (We wouldn't need to split at whitespace between non-operator
; chars, except that we need to know when a %xxx name ends.)

; Operator and whitespace chars.

(declaim (special *operators* *whitespace* *op-or-wspace*
                  *op-wspace-id-angle* *id-char* *left-angle*))
(setf *operators* '( #\( #\) #\[ #\] #\| #\= ))
(setf *whitespace* '( #\Space #\Tab ))
(setf *op-or-wspace* (append *operators* *whitespace*))
(setf *op-wspace-id-angle* (append *op-or-wspace* '( #\< #\> #\% )))
(setf *id-char* #\% )    ; Note char not list.
(setf *left-angle* #\< )

; Test for a string containing only whitespace.

(defun whitespace-p (str)
  (every #'(lambda (char) (member char *whitespace*)) str))

; Trim whitespace-only strings out of a list.

(defun purge-whitespace (sequence)
  (remove-if #'(lambda (element)
                 (and (stringp element) (whitespace-p element)))
             sequence))

; Trim whitespace off either a string or list.  (For strings, only surrounding
; whitespace is trimmed, but that's fine because we break tokens at whitespace.)

(defun trim-whitespace (sequence)
  (if (stringp sequence)
    (string-trim *whitespace* sequence)
    (purge-whitespace sequence)))

; Split off words not containing operators.  Return words and remainder of line.
; This works on either a string (e.g. the input line) or a tokenized expression.

(defun get-words (line)
  (if (null line)
    ; Done if line is empty.
    '(NIL NIL)
    ; Find position of next operator or whitespace, then get everything up to
    ; that point.
    (let ((next-op-index
            (position-if #'(lambda (char) (member char *op-or-wspace*)) line)))
      ; If there are no ops in the line, return the whole thing as the token
      ; and NIL for the remainder, else split the line starting at the op.
      (cond
        ((null next-op-index) (values (trim-whitespace line) NIL))
        ((= next-op-index 0) (values NIL (trim-whitespace line)))
        (T (values (trim-whitespace (subseq line 0 next-op-index))
                 (trim-whitespace (subseq line next-op-index))))))))
      
; Split off one token, whether operator or words.  Return token and remainder
; of line.

(defun get-token (line)
  (if (null line)
    ; Done if line is empty.
    (values NIL NIL)
    ; Get first char.  If op, return it and remainder of line, else call
    ; get-words to split off initial words.
    (let* ((char (elt line 0))
           (rest-of-line-as-string (subseq line 1))
           (rest-of-line (if (equal rest-of-line-as-string "")
                           NIL
                           rest-of-line-as-string)))
      (if (member char *operators*)
        ; An operator char is a token by itself.  If remainder of line is
        ; empty, return NIL not "".
        (values char rest-of-line)
        ; Here we have a non-op.  Lump it together with other contiguous non-op
        ; chars.  If we got just whitespace (e.g. between two operators) call
        ; ourselves again.
        (multiple-value-bind (result-prefix result-remainder) (get-words line)
          (if (or (whitespace-p result-prefix) (null result-prefix))
            ; All whitespace -- try again on the remainder.
            (get-token result-remainder)
            ; Not just whitespace -- return it.
            (values result-prefix result-remainder)))))))

; Strip unwanted punctuation and make everyting lowercase.  Need to keep ops,
; % for identifiers, and < and > for placeholders.  The ops and identifiers
; will not appear in the final sentences, but <> can.  (If we decide to strip
; numerals as well, replace "alphanumericp" by "alpha-char-p".)  Apply this to
; the line before calling tokenize.  (One could tidy the resulting sentences,
; but that would be a lot more work.  Also, this lets us capture the lexicon
; from the expressions rather than the sentences -- also a time savings.)

(defun tidy-string (a-string)
  (remove-if-not
    #'(lambda (char) (or (alphanumericp char)
                         (member char *op-wspace-id-angle*)))
    (string-downcase (string-trim *whitespace* a-string))))

; Convert one line to tokens.

(defun tokenize (line)
  (multiple-value-bind (first-token rest-of-line) (get-token line)
    (if (null rest-of-line)
      ; If there's only one token, return it in a list by itself.
      (list first-token)
      ; Else get the rest an put the first token on the front.
      (cons first-token (tokenize rest-of-line)))))

; Helper that decides if a token is a name by seeing if it starts with %.
; If it is, it returns the trimmed name, else returns NIL.

(defun name (token)
  (when (stringp token)  ; If it's not a string, it's not a name.
    (let ((trimmed-token (string-trim *whitespace* token)))
      (when (equal (char trimmed-token 0) *id-char*)
        trimmed-token))))

; Replace names of named sub-expressions by their values from the
; *sub-expressions* hashtable.

(defun rep-named-exps (exp)
  (reduce #'append
    (mapcar #'(lambda (token)
                (let ((exp-name (name token)))
                  (if (null exp-name)
                    ; Here, it's not a name -- just return the token (wrapped
                    ; in a list so it's like the named sub-expressions).
                    (list token)
                    ; Here, it's a name -- return its value.
                    (gethash exp-name *sub-expressions*))))
            exp)))

; In between tokenizing the line and substituting named expressions, we can
; collect the lexicon.  We ignore % names and ops -- everything else gets
; put in a set.  A hashtable is fastest for this, although we have only keys
; and no values.  The hashtable is *not* cleared between runs, so it can
; accumulate the entire lexicon across all goals, if desired.  If the
; hashtable does not already exist, it will be set up on first access.
; So before starting to process a whole dataset, do (setf *lexicon* NIL).

(declaim (special *lexicon*))

; We want to exclude from the lexicon, besides operators, names of named
; expressions and placeholders.  Here, tokens have already been tidied,
; so we don't need to trim them, and we can get by with a bit less work than
; what "name" (above) does.

(defun word-p (token)
  (and (stringp token)  ; Words must be strings -- this excludes ops.
       (let ((first-char (char token 0)))
         (not (or (equal first-char *id-char*)
                  (equal first-char *left-angle*))))))

(defun store-word-in-lexicon (token)
  ; Ignore ops, expression names, placeholders.
  (when (word-p token)
    ; If we don't have a hash table, make one.
    (unless (hash-table-p *lexicon*)
      (setf *lexicon* (make-hash-table :test #'equal)))
    ; Insert the word -- we don't care if we replace an existing entry.
    ; Doesn't matter what the value is either -- we only need the key.
    (setf (gethash token *lexicon*) T)))

(defun store-exp-in-lexicon (exp)
  (mapcar #'store-word-in-lexicon exp)
  exp)  ; Return the exp so we can nest this call.

; Working from the head of the input, we can accumulate a list of sentence
; fragments -- at the outset, this will be NIL.
;
; Since we're processing input sequentially, we won't know if there's a "|"
; later on, that will add more options to the set of sentence fragments we're
; collecting.  We'll need to finish processing the entire set of "|"-separated
; pieces of input...at our level of nesting in brackets...before we have the
; complete set.  (There is an effective set of brackets surrounding the entire
; input expression.)
;
; Within one sub-expression delimited by "|", we'll have sections that are
; either text or nested expressions surrounded by brackets.  Bracketed
; expressions can contain "|" themselves, or can be [] brackets, which yield
; either NIL or the contents of the [] as options, so these may generate a
; set of sentence fragments.
;
; When we encounter a ( or [, we make a recursive call, beginning beyond the
; ( or [.  We want the call to process everything up to the closing ) or ],
; which, on return, we consume.  If all recursive calls do the same, then the
; only ) or ] we see will be the one that closes our call -- we return at that
; point.  In the outer call, we verify that the suffix of the input starts with
; the matching bracket, and remove the bracket.  If the brackets were [], we
; add a NIL to the expanded bracketed expression.
;
; Since the recursive call is going to be taking tokens off the input, it
; should also return the tail of the input.  We could return two values,
; a pair (two element list), an object with two fields, or we could have a
; global pointer into the input and setf it to point to the remainder.  This
; last is simplest and is legitimate in this situation -- we'll never need to
; back up.  (The only excuse for returning the tail pointer instead of
; setf'ing it is to be "Lisp-like".  But making the code longer and harder
; to understand is also un-Lisp-like -- see tokenizer above, which does the
; return-a-pair ritual.)
;
; On return from a recursive call, we'll have a list of prefixes, a new set of
; fragments representing the bracketed expression, and the remaining input.
; We join the prefix list and the list for the bracketed expression by forming
; all combinations.
;
; If we encounter another section of plain text, we append it, up to the next
; operator, onto all of our prefixes.  Since text and bracketed expressions
; can alternate, we can use a loop to process them.
;
; When we find a "|", the expression after that is independent of what we've
; accumulated.  The list of fragments that is the result of processing the
; expression after the "|" will be added into our *list* of already-accumulated
; fragments.  We can use a recursive call to handle the expression after the
; "|".  If it does the same, when it returns, we'll have processed the rest of
; the "|"-separated expressions at our level, and the next input char we see
; should be a closing bracket or end-of-line.
;
; On a format error (e.g. mismatched brackets), we signal -- again, to avoid the
; complication of testing for an error return value, and because we don't want
; to continue from an error -- the user should fix their input.  For politeness
; sake, we maintain a global line number, and include it in the signalled
; message.  We don't need to handle the errors -- the line number is sufficient
; info for the user, and if they want, they can examine the current line in
; the debugger.

; Globals

; The current (un-tokenized) input line and line number, for the convenience
; of the user in case a format error is signalled.
(declaim (special *current-line* *current-line-number*))
(setf *current-line* NIL)
(setf *current-line-number* NIL)

; A hashtable containing named sub-expressions.
(declaim (special *sub-expressions*))
(defun init-sub-expressions ()
  (setf *sub-expressions*
    (if (and (boundp '*sub-expressions*) (hash-table-p *sub-expressions*))
      ; If it's already a hashtable, keep it, so can accumulate sub-expressions
      ; across files.
      *sub-expressions*
      ; If not, make one.
      (make-hash-table :test 'equal))))

; Function "expand" takes a tokenized input line as argument. It returns a list
; of all the word combinations represented by the portion of the input it
; decides to handle, and the tail of the input -- the part it did not process.
; It returns on end-of-line, or on a closing bracket for which it did not see
; the matching opening bracket (in which case it's been called recursively to
; process a nested expression).  On a format error, it signals.

(defun expand (exp)

  ; We're done if the input is empty.
  (unless (null exp)

    ; At the outset, we're at the start of a sub-expression -- it must have the
    ; same form as an entire expression, except that it may be terminated by an
    ; unmatched ) or ].  So it must start with text or an opening bracket -- an
    ; |, ), or ] is an error here.
    (let ((first-token (first exp)))   ; Get the first token.

      (when (or (equal first-token #\| )
                (equal first-token #\) )
                (equal first-token #\] ))
        (error "~&Improperly-placed |, ), or ] in line ~A~%"
               *current-line-number*))

      ; Call a helper to process the first item (which may be a nested
      ; expression).  It returns the sentence fragments resulting from the
      ; first item and the remainder of the expression.
      (multiple-value-bind (initial-fragments first-remainder) (expand-one exp)

        ; Here, we're done handling the current section of the input (text or
        ; bracketed sub-expression).  We have a list of fragments resulting
        ; from that section.  The helper took care of pruning the input, so
        ; it's ready for us to proceed -- split off the next token.  (We're
        ; only checking for an operator at this point, so we don't have to
        ; collect together any non-operator tokens at the start of the
        ; expression.)
        (let ((next-token     (first first-remainder))
              (next-remainder (rest  first-remainder)))

          ; What we do next depends on the next token.  If we're at the end of
          ; a sub-expression (i.e. if we're at end-of-line, ")" or "]"), we
          ; want to return.  If it's |, we clip it off and also do a recursive
          ; call, but at the end of that call, we *insert* the returned items
          ; into our fragments list, rather than appending them onto each
          ; fragment.  Otherwise (if it's text or another bracketed expression)
          ; we'll want to do an ordinary join on the result of a recursive call
          ; on the tail of the input.

          (cond
            ((equal next-token #\|)
               ; Here we have "|" -- remove it and append result of recursive
               ; call onto our current list of fragments.  Return the new
               ; fragments and the tail of the expression.
               (multiple-value-bind
                 (fragments new-remainder) (expand next-remainder)
                 (values
                   (append initial-fragments fragments) ; New fragments
                   new-remainder)))                     ; New remainder of exp

            ((or (equal next-token NIL )
                 (equal next-token #\) )
                 (equal next-token #\] ))
               ; Here we're at the end of a sub-expression.  Return what we
               ; have.  Our caller needs the bracket (if any) left on the
               ; expression to so it can be matched with the opening bracket,
               ; so we return first-remainder not next-remainder.
               (values initial-fragments first-remainder))

            (T ; Here we have anything else -- join the result to our current
               ; fragments.
               (multiple-value-bind
                 (fragments new-remainder) (expand first-remainder)
                 (values
                   (join initial-fragments fragments) ; New fragments
                   new-remainder)))))))))             ; New remainder of exp

; Helper for expand that processes the lead item of the input -- text or
; bracketed expression.  In any of these cases, the fragments we return should
; be *joined* to the caller's fragment collection.

(defun expand-one (exp)

  ; We do not get here if exp is empty, or if it starts with
  ; |, ), or ].

  (let ((first-token (first exp))      ; Get the first token.
        (first-remainder (rest exp)))  ; Advance input ptr.

    (cond
      ; If the first token is ( or [, we recurse.  The helper expand-and-
      ; check-bracket will verify that the appropriate closing bracket is
      ; present.  (We tell it which one because we already know.  We also
      ; give it the opening bracket for its error message.)   If we had
      ; square brackets, we add NIL to the returned list, because their
      ; contents are optional.

      ; "("?
      ((equal first-token #\( )
         (expand-and-check-bracket first-remainder #\( #\) ))
         
      ; "["? -- here we add NIL as an option.
      ((equal first-token #\[ )
         (multiple-value-bind
              (initial-fragments new-remainder)
              (expand-and-check-bracket first-remainder #\[ #\] )
           (values (cons NIL initial-fragments) new-remainder)))

      ; Here, it's not an operator.  For our "fragments", we split off any
      ; adjacent non-operator words and wrap them in a list (to be compatible
      ; with the other options, which all return a list of lists) and return
      ; them along with the remainder of the expression.
      (T (multiple-value-bind (initial-words new-remainder) (get-words exp)
           (values (list initial-words) new-remainder))))))

; Helper that processes one bracketed expression, with a check for a matched
; bracket.  Our caller already knows what sort of bracket we need to check for,
; so let them tell us.  If we return, the bracket matched.

(defun expand-and-check-bracket (exp opening-bracket closing-bracket)

  ; Process nested expression.
  (multiple-value-bind (initial-fragments first-remainder) (expand exp)

    ; Token after it should be the matching bracket.
    (let ((token-after-nested-exp (first first-remainder)))
      (unless (equal token-after-nested-exp closing-bracket)
        (error "~&Unmatched ~A in line ~A~%"
               opening-bracket *current-line-number*))

      ; Here, we survived the bracket check.  Return the list of fragments from
      ; the sub-expression, and the remainder with the closing bracket snipped
      ; off.
      (values initial-fragments (rest first-remainder)))))

; Helper that "joins" two lists by forming all combinations of an element from
; each, leaving them in the same order as that in which the lists were passed.
; Note that the elements in these lists are each lists -- we append them.
; (This is approximately a "full outer join" in DB-speak.  However, we append
; or postpend NIL if the "other" list has no elements, since we have amorphous
; lists, not records.)

(defun join (list1 list2)
  (cond
    ; If we have nothing to mapcar over, use the other list.
    ((null list1) list2)
    ((null list2) list1)
    (T (reduce #'append
         (mapcar #'(lambda (element1) (one-sided-join element1 list2))
                 list1)))))

; Helper for the above helper that does a one-sided join -- appends a single
; element onto each element in a list.  (Note that the "elements" in the lists
; are themselves lists.)

(defun one-sided-join (element1 list2)
    ; (We checked for a null list above.)
    (mapcar #'(lambda (element2) (append element1 element2)) list2))

; When expand is done, we have a list of lists of strings.  Strings may have
; leading or trailing whitespace, or not.  We want to reassemble them into a
; single string, with a single space between each word.  So we trim any spaces
; that may be present, and combine the trimmed strings with a space between.

(defun recombine (list-of-strings)
  (unless (null list-of-strings)
    (reduce
      #'(lambda (result-string next-string)
          (concatenate 'string result-string
                               " "
                               (string-trim *whitespace* next-string)))
      (rest list-of-strings)
      :initial-value (string-trim *whitespace* (first list-of-strings)))))

(defun recombine-all (list-of-lists-of-strings)
  (mapcar #'recombine list-of-lists-of-strings))

; Read and process a file of expressions.  For each expression:  Tokenize the
; expression.  If the expression is an assignment, store the name and sub-
; expression in a hashtable.  Otherwise, call expand on the token list to get
; a list of lists of sentence fragment strings.  Call recombine-all on the list
; of lists to get a list of sentence strings.  Call write-line on each non-nil
; result.  (In real data, there aren't likely to be any NILs, but my test cases
; do include expressions where NIL is a valid sentence.)
;
; (Note that the sub-expression processing could perhaps be speeded up by
; storing the expanded sub-expression.  But that would mean making expand more
; complicated.  For a non-production function, it's not worth the complication.)

(declaim (special *num-output-lines*))

(defun process-exp-file (classname infile outfile)
  (with-open-file (in infile :direction :input :if-does-not-exist NIL)
    (with-open-file (out outfile :direction :output)

      ; Did we get both files?
      (when (and (streamp in) (streamp out))

        ; Write out the class name verbatim as the first line of the file.
        ; We don't count this in the number of output lines -- that's the
        ; number of sentences.
        (write-line classname out)

        ; Set up the hashtable for named sub-expressions.
        (init-sub-expressions)

        ; Count output lines -- clear here, count in the loop below.
        (setf *num-output-lines* 0)

        (do* (; Read one line.
              (*current-line* (read-line in NIL NIL) (read-line in NIL NIL))
              ; Keep track of the line number.
              (*current-line-number* 1 (1+ *current-line-number*)))
             ; Quit when no more.
             ((null *current-line*))

          ; Skip blank lines.
          (unless (whitespace-p *current-line*)

            (let* (; Tokenize the line.
                   (exp (store-exp-in-lexicon
                          (tokenize (tidy-string *current-line*))))
                   (first-token (name (first exp)))
                   (second-token (second exp)))

              ; Find out if this is a named sub-expression assignment (must 
              ; have %name =).
              (if (and (not (null first-token)) (equal second-token #\=))

                ; It is -- get name and exp and store them in table (skip over
                ; the "=").
                (setf (gethash first-token *sub-expressions*)
                      (rep-named-exps (rest (rest exp))))

                ; It's not -- process the expression (replace named sub-
                ; expressions, expand, make token lists into strings) and
                ; write it out.
                (mapcar #'(lambda (a-string)
                            (unless (null a-string)
                              (setf *num-output-lines* (1+ *num-output-lines*))
                              (write-line a-string out)))
                        (recombine-all (expand (rep-named-exps exp))))))))

        ; If we processed a file, report how many lines we wrote.  This will
        ; be useful in automating training and test procedures, which will
        ; need to randomize the lines and split them into training and test
        ; sets.
        *num-output-lines*))))

; That's all well and good, but we're going to want to randomize the order of
; the sentences before we divide them into training and test sets.  So provide
; another function that, instead of writing the output to a file, stores it in
; an array.  It returns the array and the number of sentences.

(declaim (special *initial-array-size* *array-size-increment*))
(setf *initial-array-size* 1000)
(setf *array-size-increment* 1000)

(defun make-array-from-exp-file (infile)
  (with-open-file (in infile :direction :input :if-does-not-exist NIL)

    ; Did we get the file?
    (when (streamp in)

      ; Make an array.
      (let ((sentence-array (make-array *initial-array-size*
                                        :element-type 'string
                                        :adjustable T)))

        ; Set up the hashtable for named sub-expressions.
        (init-sub-expressions)

        ; Count output lines -- clear here, count in the loop below.  We'll
        ; also use this as our array index.
        (setf *num-output-lines* 0)

        (do* (; Read one line.
              (*current-line* (read-line in NIL NIL) (read-line in NIL NIL))
              ; Keep track of the line number.
              (*current-line-number* 1 (1+ *current-line-number*)))
             ; Quit when no more.
             ((null *current-line*))

          ; Skip blank lines.
          (unless (whitespace-p *current-line*)

            (let* (; Tokenize the line.
                   (exp (store-exp-in-lexicon
                          (tokenize (tidy-string *current-line*))))
                   (first-token (name (first exp)))
                   (second-token (second exp)))

              ; Find out if this is a named sub-expression assignment (must 
              ; have %name =).
              (if (and (not (null first-token)) (equal second-token #\=))

                ; It is -- get name and exp, expand named sub-exps in
                ; exp, and store them in table (skip over the "=").
                (setf (gethash first-token *sub-expressions*)
                      (rep-named-exps (rest (rest exp))))

                ; It's not -- process the expression (replace named sub-
                ; expressions, expand, make token lists into strings) and
                ; insert them in the array.
                (mapcar #'(lambda (a-string)
                            (unless (null a-string)
                              ; If there isn't any more room in the array,
                              ; make it larger.
                              (unless (array-in-bounds-p sentence-array
                                                         *num-output-lines*)
                                (setf sentence-array
                                      (adjust-array sentence-array
                                        (+ *num-output-lines*
                                           *array-size-increment*))))
                              ; The array's big enough now.
                              (setf (aref sentence-array *num-output-lines*)
                                    a-string)
                              (setf *num-output-lines*
                                    (1+ *num-output-lines*))))
                        (recombine-all (expand (rep-named-exps exp))))))))

        ; If we processed a file, return the array and the number of lines
        ; we wrote.  This will be useful in automating training and test
        ; procedures, which will need to randomize the lines and split them
        ; into training and test sets.
        (values sentence-array *num-output-lines*)))))

; Now that we've got an array, randomize it.  We can do this individually to
; the file for each goal's expressions, then take the same fraction off the
; top of each array to be the training set, and use the rest for testing.
; We'll probably want to write out the randomized arrays one-by-one rather
; than doing all this in memory for all goals.  We can split the files using
; Unix split, or provide a function here.
;
; Randomize the array by selecting a random element and swapping it with the
; first in the array.  Repeat for all suffixes of the array.
;
; The size is an argument because the number of elements in use may be smaller
; than the array length.

(defun randomize-array (array size)

  (do (; Loop counter is array index.
       (index 0 (1+ index)))
      ; Quit at end of data and return array and size.  Note when we get to the
      ; last element, we don't need to do anything, because the only element
      ; left to swap it with is itself.
      ((>= index (- size 1)) (values array size))
    (let* (; Get a random number between index and size-1.  We'll swap this
           ; with what's at index.  Yes, it can = index, in which case this
           ; won't change anything.  (Note random returns a number only up
           ; to, not including, its argument, so we use size not size-1.)
           (swap-index (+ index (random (- size index))))
           ; Get the values at index and swap-index.
           (index-element (aref array index))
           (swap-index-element (aref array swap-index)))
      ; Put them in their new locations.
      (setf (aref array index) swap-index-element)
      (setf (aref array swap-index) index-element))))

; To use that, we need to unpack the values returned by make-array-... and
; provide them as args, for which we use multiple-value-call.  For example:
;
;   (multiple-value-call #'randomize-array (make-array-from-exp-file "input"))
;
; where input is the path of the expressions file.  Then we need to write it
; back out.

(defun process-and-randomize-exp-file (classname infile outfile)

  (multiple-value-bind (sentence-array size)
    (multiple-value-call #'randomize-array (make-array-from-exp-file infile))

    ; Here we have the array and its size -- write it out.
    (with-open-file (out outfile :direction :output)
      ; Did we get the output file?
      (when (streamp out)

        ; Write out the class name verbatim as the first line of the file.
        (write-line classname out)

        (do (; Array index.
             (index 0 (1+ index)))
            ; Quit at end of array.  Return its size.
            ((>= index size) size)

          (write-line (aref sentence-array index) out))))))
