Handwriting Tools
-----------------

Presently this is a commandline rewrite of the Joiner.jar application
from [briem.net]. The end goal is to create an online tool for making
custom handwriting exercise sheets.

Font Joiner
===========

This tool inserts joining glyphs between letters, to create cursive
writing when viewed with the supported font, Briem Handwriting.
See [briem.net].

### Installing

Unix and Apple computers come with the program needed to run this
script.
Windows users will probably need to [install `Perl`|strawberryperl.com].

Save the `bin/font-joiner.pl` script, then open a command terminal
(`CMD.EXE` on Windows) and type `perl font-joiner.pl --manual` to show
the documentation.

### Examples

```
perl font-joiner.pl < Examples/Beowulf.txt > Beowulf-joined.txt
perl font-joiner.pl --encode UTF8 Examples/Beowulf.txt
perl font-joiner.pl --force Examples/Beowulf.txt
```

The input text can be in most any character set. Only the Latin script
is supported by the Briem Handwriting font, though, so most files will
be saved as UTF-8, UTF-16 or Latin-1.

The output will use the same encoding as the input if possible, or
UTF-8 as a default.
