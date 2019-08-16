# m2hpf
### HPF - High Performance File Format high speed decoder for Modula2 programming language
-- aka. a library to deal with Delsys EMGworks *.hpf files

This is a library to deal with Delsys EMGworks *.hpf files, the HPF file format specs are described in a document found on internet titled:
```
DT High Performance File Format Specification
by
Document Number: 22760, Rev A
Copyright © 2007 Data Translation, Inc. All rights reserved.
_______________________________
Data Translation, Inc.
100 Locke Drive
Marlborough, MA 01752-1192, USA
Telephone (508) 481-3700
Home Page http://www.datatranslation.com/
```

The `m2hpf` library implements a slightly modified version of the format described in the document above,
it has beed written to deal with [Delsys EMGworks](https://www.delsys.com) EMG sensors signals.

This library is made to provide quick data access, to be used in real-time signal processing project, so for example; using this library to implement a *HPF to CSV* file converter will be much more efficient than using the *Delsys File Utility*.

The library has been written in Modula2 programming language for an internal use. Now it is published under LGPL-3.0.
