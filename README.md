fvar
====

API draft for a simple persistence layer for haskell values.

The basic idea is that the library writes values to disk using existing serialization libraries 
while taking care of concurrently accessing processes through file locks (using flock). 
The API is inpired by MVars.

Status
------

This is a draft. The implementations (where given) are not fully functioning. File locking is still missing.

Open questions / problems
-------------------------

  - Is it possible to lock multiple files atomically? If not, how do we prevent deadlocks?
  - It seems that we need some form of type representation in the written files. This is needed to implement 'getFromDisk' (at least in the way it is documented now). It is also desirable as it makes the files more self-contained. How do we do this? If we use safecopy, we can't just add the TypeRep, because multiple types can be used as multiple versions of the same data.
  - How do we deal with exceptions? Atomicity of a call to 'modifyFVar' should be guaranteed.
