# m2hpf
### *HPF - DT High Performance File Format* high speed decoding library (for Modula-2 programming language)

The current version of `m2hpf` supports **only reading** *.hpf files. So `m2hpf` can't be used to write files in HPF format.

This implementation is based on the DT HPF file format, which is described in a document I found [some where on the internet](https://forums.ni.com/ni/attachments/ni/170/813238/1/high_performance_file_format_spec%5B1%5D.pdf) titled:
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
it has beed written to support the actual format of [Delsys EMGworks](https://www.delsys.com) signals.

This library is made to provide quick data access, to be used in real-time signal processing project, so for example; using this library to implement a *HPF to CSV* file converter will be much more efficient than using the *Delsys File Utility*.

The library has been written in Modula2 programming language for an internal use. Now it is published under LGPL-3.0.

[Abdelhak Bougouffa](https://abougouffa.github.io) 2019
