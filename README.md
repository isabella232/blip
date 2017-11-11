Blip: Boring Lisp Project
=========================

Blip is a boring lisp project that started as a personal exploration into the
nature of parsing. Originally a C project that parsed inputs by iteratively
transforming a graph, it was rewritten from the ground up in Common Lisp to
parse by iteratively transforming s-expressions (which are used to represent
trees). I call this (interchangeably) "iterative grouping" and "iterative
transformation". It has the interesting characteristic that it will consume
inputs that are totally non-compliant with a language grammar (for example, we
can extract javascript code from markdown files without a hitch; and parse most
C files without pre-processing them). This makes it useful for crawling
code-bases, but not necessarily useful for eval'ing the stuff it's parsing.
Also, because transformation is woven into the fabric of parsing, it is very
easy to write your own post-processing passes that do neat things like rename
functions/variables, and insert print/logging statements for debugging. I have
not yet found prior work that that implements parsers in this way, but I would
be very interested in hearing about it --- this method seems absurdley obvious,
so either a) prior work must exist under some obscure hard-to-google name or b)
was deemed (understandably) beneath the publishability threshold.

The code is not very polished, mostly because I used the project as a
launch-pad to learn lisp. As far as style goes, the general rule is that
anything goes, as long as it doesn't go over 80 columns, and you indent with
spaces instead of tabs. At some point we should be able to program blip render
its own code in any style that you specify -- like it does for JS and C already
--  so that styling doesn't become fodder for flame-wars.

It has grown beyond simple parsing, and is capable of fetching and crawling
repositories from github and gerrit. It can parse JavaScript, JSON, and C (aside
from some pre-processor edge-cases that throw off our paren/brace matching (I am
looking at you, postmaster.c)). It can build indexes of words, function calls,
and function definitions, all of which can be addressed using a path-like
notation.

For example:

	namedNetwork{}/parallel()/byName{}/listNetworks()/

The above path represents a path to a call to a function called listNetworks.
Which is present in a definition called byName, which is passed as an argument
to a function called parallel, which is called from the definition of
namedNetwork.

And much more, that is yet to be documented.

The goal of the project is to help developers build and re-build mental maps of
large code-bases written/modified by many other people over a long period of
time. To that end, the project has a promiscuous and unbounded scope,
assimilating languages as the need arises. Pretty soon, it'll assimilate Go,
since we/Joyent run some Go components in production. Ironically, it does not
consume lisp code yet, but that's planned. The project is not shackled to
parsing via iterative transformations --- I'll happily add more formal parsers,
CFGs, etc, if they are needed. Same thing goes for VCS's: we currently only
support git, but will happily add support for bitkeeper, svn, etc, if the need
arises.

See INSTALL.md for installation instructions. If you can get it running, you
start using blip via the `blip` and `blip help` commands. See DOCKER-897 for
examples on how to use it. If you want to ask questions that blip can't answer
you can always write an `xform` (experimental form, extra form, external form,
trans-formation, etc) and execute it via `blip xform`. This is useful for
prototyping code without recompiling all of blip. Don't trust xforms that you
didn't write yourself.

More information and documentation to come.

Note: So far this has only been tested on SmartOS, but there doesn't appear to
be a reason this code shouldn't work other Unix systems.
