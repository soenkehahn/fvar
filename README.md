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

Open questions / problems
-------------------------

  - Is it possible to lock multiple files atomically? If not, how do we prevent deadlocks?
  - It seems that we need some form of type representation in the written files. This is needed to implement 'getFromDisk' (at least in the way it is documented now). It is also desirable as it makes the files more self-contained. How do we do this? If we use safecopy, we can't just add the TypeRep, because multiple types can be used as multiple versions of the same data.
  - How do we deal with exceptions? Atomicity of a call to 'modifyFVar' should be guaranteed.
    - We should have a backup file of the modified file that can be copied back in case of exceptions. (This is a problem for exceptionally large files, but the idea with FVars is to have rather small files anyway.)
  - An FVar can always point to a file that doesn't exist. How do we deal with that?
