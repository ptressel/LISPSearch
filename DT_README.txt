Files dt.lsp, text.lsp, and input.lsp are parts of a decision-tree-based classifier
for sentences.  Although this might be used for general text classification (e.g.
into topics), the intended use was goal recognition from unconstrained speech.
The intended input is questions or statements by a person trying to convey a request
to an automated dialog system.  Requests are expected to fall into a relatively
small number of main categories, then within each main category, there can be
sub-categories.  The main categories represent the overall domain the user is
talking about, and the sub-categories represent what the user wants to accomplish,
i.e. their goal.  The idea (believed to be novel at the time this was written) is
that one should have one classifier for the main categories, then additional
classifiers per category to identify the sub-category.  A sentence would first be
run through the main classifier, then would be re-run through the sub-category
classifier for the identified main category.

dt.lsp contains the main functions.  text.lsp contains helpers for text handling.
input.lsp is used for generating "fake" test data if one cannot obtain sufficient
samples of requests from actual people.

Following is an example of running dt-learn, dt-perform, and dt-test.

Start clisp (or whichever), however it's appropriate for the OS.

Load dt.lsp (and text.lsp, which is included by dt.lsp):

[36]> (load "dt.lsp")
;; Loading file dt.lsp ...
;;  Loading file text.lsp ...
;;  Loading of file text.lsp is finished.
;; Loading of file dt.lsp is finished.
T

Run dt-learn on some tiny data sets.  The second argument is a list of words to
exclude from the lexicon, which I'm not using in this case.

[37]> (dt-learn (list "data1-train" "data2-train") NIL :min-samples 0 :max-depth NIL)
NIL

That writes out dt-perform.lsp, if you don't tell it another name with the
:output-file option.  (No matter what name the file has, the function is still
dt-perform.)

Load input.lsp (which is the expression exploder), because it has a tokenizer
that converts strings to lists of words (each word as a string), and
dt-perform expects its input already listified.

[40]> (load "input.lsp")
;; Loading file input.lsp ...
;; Loading of file input.lsp is finished.
T

Load dt-perform.lsp.

[41]> (load "dt-perform.lsp")
;; Loading file dt-perform.lsp ...
;; Loading of file dt-perform.lsp is finished.
T

This is how to run it on individual sentences.

[42]> (dt-perform (tokenize "ef ij qr"))
"data1"

[43]> (dt-perform (tokenize "ef op st"))
"data2"

There's a convenience function that will run dt-perform on test sets and
report results.

[44]> (dt-test (list "data1-test" "data2-test"))

Number correct 12 out of 12
NIL