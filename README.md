A STM-based implementation of concurrent priority queues for Haskell
====================================================================

Introduction
------------
In order to choose heap implementation, append `-DHEAP_VERSION=<version>` to `CPPFLAGS` when compiling the code.
The `<version>` should be `FineHeap` or `CoarseHeap`.
For details please see attached slides and comments in the code.

Copyright (c) Mateusz Machalica 2014
