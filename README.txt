This is a collection of LISP programs that (mainly) implement various forms
of search (with a decision tree example thrown in).  (These were mostly
written for an undergrad AI course at the University of Washington, rather
some while back...)  It is mainly posted here for benefit of folks implementing
A* search and 2D route planning with obstacles.  The main files to look at for
that purpose are astar-search.lsp and polygons.lsp.  Two cautions:
First, polygons.lsp does not support concave polygons.  There is no reason for
a path to visit a vertex in a concavity.  The fix is to compute the convex hull
of each polygon, and use only those vertices as targets.  Caveat is that is must
still be possible for the goal point to be within a concavity.  Second, it uses a
function that computes the intersection of two line segments which is only
provisionally included, since it was written by the TA for the course, Joshua
Redstone.  No license to incorporate this in commercial code is implied by posting
this file, intersect.lsp, here.  An alternative is to do a cleanroom reimplimentation
of line intersection.  The line segment computation can benefit from some performance
tricks, so before reimplementing it, one should look at discussions of appropriate
methods.