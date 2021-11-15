Super Star Trek README

This is a Common Lisp port of the Dave Matuszek et al Fortran version of Super
Star Trek by way of the Tom Almy C version, including enhancements by Stas
Sergev, Eric Raymond, and others. There are some BSD Trek features in here
too, depending on who borrowed what from which sources.

Two motivations for the port are a desire to learn Common Lisp and happy
memories of playing an earlier version on a PDP-11 when I should have been
working. Since this is a learning project feedback is welcome. This is a
faithless port, that is, I've added, removed, or changed features when it
makes sense to me, hopefully while keeping the feel of the game intact.

Some of the changes include:
  - no option selection, all gameplay features are enabled
  - debug code has been removed
  - no save and restore, games in progress are checkpointed automatically
  - player help information is integrated into the source code and
    and accessed as part of game play
  - The concept of "flight status" has been introduced and separated from
    "condition".
  - Sound output, which seems to have been added around the same time as
    curses support, has not been implemented  because I couldn't find a
    portable way to do it.

The source contains comments prefixed with TODO. These indicate bugs to fix,
work to be done, code optimizations to explore, and new features to implement.
In most cases I retained comments from the C source to assist with comparing
the Lisp and C versions but they will probably be removed at some point.

The C source shows both Fortran and C influence on variable names. Lisp
conventions are used if I understood them, for example by appending p to
booleans, using *earmuffs* with globals, and +pluses+ with contstants. Names
that focus on brevity by removing vowels and shortening names are usually
expanded to standard spelling.

Over time my goal is to take advantage of Lisp features to improve the code.
For example, many uses of arrays in the C source could be expressed as Lisp
lists, and object-oriented techniques that didn't exist at the time the Fortran
version was written can now be used.

This port uses SBCL's version of Common Lisp along with cl-charms for ncurses
support and cl-utilities for some sequence handling. The ncurses features are
functional under FreeBSD, and probably other Unix-alikes but I continue to
encounter problems under Windows. If ncurses doesn't work the fallback is the
classic line-by-lne mode of the original game. Although a work in progress,
the initial code commit compiles cleanly and the game is fully functional.
