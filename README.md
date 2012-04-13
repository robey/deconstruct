
deconstruct
===========

deconstruct is a small scala library for parsing java ".class" files and extracting signature data
from them.

if you save the signatures for class files from a previous compile, and compare them to signatures
for class files from a new compile, deconstruct will tell you if the classes are API-compatible.

one use for this is avoiding recompiling when your dependencies change. if you rebuild a dependent
jar, deconstruct can tell you if the API really changed. if it didn't, you don't need to recompile.

that's really all it does.
