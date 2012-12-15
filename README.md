# fvar

API draft for a simple persistence layer for haskell values.

The basic idea is that the library writes values to disk using existing serialization libraries 
while taking care of concurrently accessing processes.

The API is inpired by MVars.

An FVar is basically just a FilePath pointing to the file containing the corresponding data.
Data in an FVar can contain other FVars and thereby reference other values stored on disk.
All FilePaths inside FVars should be relative paths from a given root directory.


## Status

This is a draft. The implementations (where given) are not fully functioning. File locking is still missing.


## Solved questions / problems

  - An FVar can always point to a file that doesn't exist. How do we deal with that?
    - We provide 'existsFVar'.
  - Is it possible to lock multiple files atomically?
    - We have a root directory and we can store a central locking file there. The file itself is synchronized with flock. With that, we can atomically lock multiple files.
  - How can you use two distinct FVar-stores in the same program? 'setFVarRoot' makes this difficult.
    - We should provide a more basic interface where you have to supply the root directory to all operations that need it. The interface with 'setFVarRoot' should be implemented on top of that as a convenience layer.
  - How do we deal with exceptions? Atomicity of a call to 'modifyFVar' should be guaranteed.
    - We should have a backup file of the modified file that can be copied back in case of exceptions. (This is a problem for exceptionally large files, but the idea with FVars is to have rather small files anyway.)
  - do we want blocking or optimistic behavior? (Can we offer both?)
    - I think, we should have this: A function similar to 'modifyFVar', except for multiple files. If you use it normally, you will get blocking behavior. If you nest a call to 'modifyFVars' inside another, you get optimistic behavior, i.e. the inner call might throw a FVarDeadlockException.


## Open questions / problems

### Type Representation in files

It seems that we need some form of type representation in the written files. This is needed to implement 'openFVar' (at least in the way it is documented now). It is also desirable as it makes the files more self-contained. How do we do this?

We want to use safecopy to store files and make data migration easy. With safecopy, you can upgrade a datatype to a new version while still being able to read serealized values in older versions. Mostlikely the new version of the type will have the same TypeRep than older versions. But sometimes the TypeRep will change. This means, we cannot use TypeReps for this.

Solution 1:
We could use TypeReps if we would have the additional constraint, that you're not allowed to make TypeReps vary. (I dislike this because it's a rather subtle constraint that is not apparent from the API. And it differs from safecopy.)

Solution 2:
Maybe we should have a type class that would enforce a unique id for every type (in all of its versions).


### atomicity

This is a bit tricky, at least when modifying multiple files. I don't think there is something like atomic moving of multiple files offered by OSes, is there?
