fvar
====

API draft for a simple persistence layer for haskell values.

The basic idea is that the library writes values to disk using existing serialization libraries 
while taking care of concurrently accessing processes through file locks (using flock). 
The API is inpired by MVars.

An FVar is basically just a FilePath pointing to the file containing the corresponding data.
Data in an FVar can contain other FVars and thereby reference other values stored on disk.
The FilePaths inside these FVar references should be relative paths from the directory containing
the file of the referencing data to the file containing the referenced data. (We probably should
have an example for this.) Can we implement this transparently?

Status
------

This is a draft. The implementations (where given) are not fully functioning. File locking is still missing.

Solved questions / problems
---------------------------

  - An FVar can always point to a file that doesn't exist. How do we deal with that?
    - We provide 'existsFVar'.
  - Is it possible to lock multiple files atomically?
    - We have a root directory and we can store a central locking file there. The file itself is synchronized with flock. With that, we can atomically lock multiple files.
  - How can you use two distinct FVar-stores in the same program? 'setFVarRoot' makes this difficult.
    - We should provide a more basic interface where you have to supply the root directory to all operations that need it. The interface with 'setFVarRoot' should be implemented on top of that as a convenience layer.
  - How do we deal with exceptions? Atomicity of a call to 'modifyFVar' should be guaranteed.
    - We should have a backup file of the modified file that can be copied back in case of exceptions. (This is a problem for exceptionally large files, but the idea with FVars is to have rather small files anyway.)

Open questions / problems
-------------------------

  - In case of multiple files, do we want blocking or optimistic behavior? (Can we offer both?)
  - It seems that we need some form of type representation in the written files. This is needed to implement 'openFVar' (at least in the way it is documented now). It is also desirable as it makes the files more self-contained. How do we do this?
  - How do we guarantee atomicity in case of modification of multiple files?
