# m2hpf
### *HPF - DT High Performance File Format* high speed decoding library (for Modula-2 programming language)

The current version support **only reading** from *.hpf files. So this library can't be used to write files in HPF format.

This is a library to deal with Delsys EMGworks *.hpf files, the HPF file format specs are described in a document found [some where on the internet](https://forums.ni.com/ni/attachments/ni/170/813238/1/high_performance_file_format_spec%5B1%5D.pdf) titled:
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

[Abdelhak Bougouffa](https://abougouffa.github.io) 2019
