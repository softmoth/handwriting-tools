Handwriting Tools
=================

Presently this is a commandline rewrite of the `joiner.jar` application
from http://briem.net. The end goal is to create an online tool for making
custom handwriting exercise sheets.

Font Joiner
-----------

This tool inserts joining glyphs between letters, to create cursive
writing when viewed with the supported font, Briem Handwriting.
See http://briem.net.

### Installing

Unix and Apple computers come with the program needed to run this
script.
Windows users will probably need to [install *Perl*](https://www.perl.org/get.html#win32).

Save the `bin/font-joiner.pl` script, then open a command terminal
(`CMD.EXE` on Windows) and type `perl font-joiner.pl --manual` to show
the documentation.

### Examples

```
perl font-joiner.pl < Examples/Beowulf.txt > Beowulf-joined.txt
perl font-joiner.pl --encode utf8 Examples/Beowulf.txt
perl font-joiner.pl --force Examples/Beowulf.txt
```

The input text can be in most any encoding. Only the Latin script
is supported by the Briem Handwriting font, though, so most files will
be saved as UTF-8, UTF-16 or Latin-1. Use `--decode=iso-8859-2` if
you need to specify another encoding.

The output will use the same encoding as the input if possible, or
UTF-8 as a default.
