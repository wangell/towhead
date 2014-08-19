towhead
=======

WARNING: Currently, the removeDirectory/clearStructFolder functions are really unsafe

Graph/tag-based file system organization using links. (Right now uses SQLite as backend, plans to use Graph database in the future potentially...)

Impetus:  I needed to tag images/songs/etc. with multiple tags and still have it work with existing software.


Installation
------------

Clone the repo and execute `cabal install`

Usage
-----

First, initialize a workspace in an empty directory:

`mkdir towhead_workspace; cd towhead_workspace`

`towhead init`

Next, scan a directory:

`towhead scan ~/images/`

Bring up the default workspace (just a clone of the scanned directories):

`towhead space`

Begin tagging the files:

`towhead tag blah file1`

Now you can bring up workspaces based on the specified tags:

`towhead space blah`


Commands
--------

`towhead scan [dir]` scans the given directory for files, or will use the "struct/" folder (for now)

`towhead tag tag1,tag2,tag3 file1 file2 file3...` tags the given files

`towhead space tag1,tag2` clears the default workspace, creates links/folders to all files tagged with tag1/tag2


Future
------

Plans for the future:

-Telescoping folder structures based on input patterns: (blah, blah2 -> (blah3, blah4)) produces a folder structure with blah1/blah2 located in the root directory, and blah3/4 as sub-directories of blah2.

-Saving of space patterns (aliases for tree-structures/tags)
