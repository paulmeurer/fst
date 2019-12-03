# FST

Common Lisp API for the Xerox Finite State Tool (fst) library.

When using the API in Clozure CL:

There is a problem with function name clash between CCL and fst.

Make sure there is no --export-dynamic in the Makefile of CCL:

instead of this:

lisp-kernel/linuxx8664/Makefile:        $(CC)  -m64 $(CDEBUG)  -Wl,--export-dynamic $(HASH_STYLE) 

this:

lisp-kernel/linuxx8664/Makefile:        $(CC)  -m64 $(CDEBUG) $(HASH_STYLE)

Then rebuild with make.

This does work for Linux, but at present there is no solution for Darwin.