This is a collection of LISP programs that (mainly) implement various forms
of search (with a decision tree example thrown in).  (These were mostly
written for an undergrad AI course at the University of Washington, rather
some while back...)  It is mainly posted here for benefit of folks implementing
A* search and 2D route planning with obstacles.  The main files to look at for
that purpose are astar-search.lsp and polygons.lsp.  Two cautions:
First, polygons.lsp does not support concave polygons.  Second, it uses a
function that computes the intersection of two line segments which is not
included, since it was written by the TA for the course.  The line segment
computation can benefit from some performance tricks, so before reimplementing
it, one should look at discussions of appropriate methods.