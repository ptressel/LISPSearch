This is a collection of LISP programs that (mainly) implement various forms
of search (with a decision tree example thrown in).  (These were mostly
written for an undergrad AI course at the University of Washington, rather
some while back...)  It is mainly posted here for benefit of folks implementing
A* search and 2D route planning with obstacles.  The main files to look at for
that purpose are astar-search.lsp and polygons.lsp.

The search functions are in separate files from the examples that use them.
Load the required search file first with the load command, e.g.
(load "name-of-file.lsp"), before executing the example.

Two cautions:

First, polygons.lsp does not properly support concave polygons.  There is no reason
for a path to visit a vertex in a concavity.  The fix is to compute the convex hull
of each polygon, and use only those vertices as targets.  Caveat is that is must
still be possible for the goal point to be within a concavity.  The only place this
comes in is if a test is added for a "legal" goal point that is not within a polygon.
In that case, the test must be against the original polygons, not their convex hulls.

Second, polygons.lsp uses a function that computes the intersection of two line
segments which is only provisionally included here, since it was written by the TA
for the course, Joshua Redstone.  No license to incorporate this in commercial code
is implied by posting this file, intersect.lsp, here.  One should contact Dr.
Redstone for permission.  An alternative is to do a cleanroom reimplimentation of
line intersection.  The line segment computation can benefit from some performance
tricks, so before reimplementing it, one should look at discussions of appropriate
methods.