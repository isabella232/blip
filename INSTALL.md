Blip is not a standalone command like other unix commands (such as, say, git).
It depends on having a directory hierarchy all to itself, and needs to store
its code (source and binary) in this hierarchy.

So, you'll want to do a few things. You'll want to designate a directory as the
`blip-root`. Something like `/home/my_username/blip/`. I will refer to this
from now on a 'BLIP_ROOT'. Create this BLIP_ROOT directory.


Now create the directory `BLIP_ROOT/stor/repos/`. Clone blip into
`BLIP_ROOT/stor/repos/blip/`. Now, open `blip.lisp` and find the line with the
variable `blip-root`. You'll see a default value, and you should change it to
whatever your `BLIP_ROOT` is.

Now create `BLIP_ROOT/meta/bin`.

From the blip dir (`BLIP_ROOT/stor/repos/blip/`) run, `sbcl --script
install.lisp`. This will install a blip binary under `BLIP_ROOT/meta/bin`.

Now, you can `cd` into `BLIP_ROOT/meta/bin/`, and run `./blip help` for a list
of available commands.

Anyway, this should probably be automated by a packaging system or at least a
script, someday.
