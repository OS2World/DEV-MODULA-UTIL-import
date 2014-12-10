                              Imports version 1.0
                                  Peter Moylan
                                16 January 2000


WHAT IS IT?

The purpose of this program is to list the names of all of the
source files in a Modula-2 program.  It takes a single argument which
is the name of the main module.  The output is a list of file names,
one file per line.


NO CHARGE, NO WARRANTY

This is free software.  Do what you like with it.  All I ask is that
you give appropriate acknowledgement to the original author when
copying it or creating derived works.

In return, I offer no support and no guarantees.  I do not promise
that it will work for you.  You are responsible for any consequences
of its use.


INSTALLATION

That depends on your operating system and compiler.  This package
contains only source code.  Do whatever you normally do to compile
a bunch of Modula-2 source code.  You might wish (as I usually do)
to set up separate directories for different parts of the source.

The program should compile "as is" if you're using XDS Modula-2
for OS/2.  If you're using a different compiler or a different
operating system then you'll probably have to modify some of the
source code.  See the section below called "System dependencies".
The required modifications are usually minor, but they do require
you to understand Modula-2 programming.

One thing the program needs is a specification of which directories
have to be searched for source files.  In the program as I wrote it,
a user-supplied file called XC.RED provides this information.  This
is a file containing lines like
       *.def = .;.\def;d:\mylib\def
       *.mod = .;.\src;d:\mylib\src
and so on.  That is, each line specifies a search path for one
category of files.  Users of the XDS and TopSpeed compilers will
be used to doing it this way.  If you prefer some different system,
you'll have to make the appropriate modifications to the code
in FILES.MOD.


CREATING AN ARCHIVE

The main reason I wrote this program was so that I could keep
snapshots of different versions of my software.  For example, if a
user reports a problem with version 0.73 of my mail server, then
I'd like to be able to recreate version 0.73 so that I can look at it.

The way I personally do this is to pipe the output of Imports.EXE into
a zip utility, with a command like

        Imports MainModuleName | zip -j result.zip -@

Depending on your own system configuration, you might need to do this
a different way, but in any case it shouldn't take more than a few
lines of commands in whatever you use as a command scripting language.


OTHER THINGS YOU CAN DO WITH THE PROGRAM

For my own purposes, all I wanted was a list of file names, so the
program doesn't bother keeping track of what depends on what.
However, the same basic technique can be used for things like

  - detecting circular dependencies in the import structure;

  - working out the order in which the module initialisation
    code will be executed;

  - displaying the dependencies in some sort of tree-like structure.

For these and similar jobs, all you need to do is add a few things
to the main data structure in the program, and add a bit of code
to IMPORTS.MOD.  OK, there's some work involved, but it should be
a job well within the grasp of anyone who's completed an
undergraduate data structures subject.


SYSTEM DEPENDENCIES

I would have liked to make this program completely portable, but
it's fundamentally impossible to do that.  The method of mapping
module names to file names varies from one system to another.  The
best I could do was to confine all the non-portable code to the
source file called FILES.MOD.  To port this program to another
system, you'll have to modify that file.  (In principle I could have
controlled some of the options by command-line parameters, but I
didn't want to end up with a Unix-like monstrosity with a
hundred different parameters.  If you prefer that method, feel free
to modify the parameter-handling in IMPORTS.MOD.)

The details that have to be changed are as follows:

 1. In deriving a file name from a module name, some systems take
    the module name as it is, and others truncate the name to its
    first N characters, where N varies from one implementation to
    another.  In FILES.MOD, this behaviour is controlled by a
    global constant called NameSize.

 2. In creating the file name, we usually tack on an extension
    such as ".DEF" or ".MOD".  The assumptions I made are:

      - For a definition module, try the extension ".DEF".
        If that fails, try ".SYM", but assume that the result
        is a compiled binary file rather than a source file.

      - For an implementation module, try the extension ".MOD".
        If that fails, try ".OBJ", but assume that the result
        is a compiled binary file rather than a source file.

    (In all cases, if no file at all is found then we assume
    that the module is not to be listed in the results, on the
    grounds that its implementation is probably inside some sort
    of system library.)  If your compiler has different rules,
    you should modify procedure Files.LocateModule.

 3. Which directories should we search in order to find the files?
    The assumptions in my implementation are

      - the paths to search are defined in a user-supplied
        file called XC.RED;

      - the directory lookup can be done by a call to the
        operating system library routine OS2.DosSearchPath.

    Again, you should modify the code in FILES.MOD to user
    whatever rules are appropriate for your own system.


AUTHOR DETAILS

The author of this software is
      Peter J Moylan
      Department of Electrical and Computer Engineering
      The University of Newcastle
      NSW 2308, Australia.

E-mail address:   peter@ee.newcastle.edu.au
Web site:         http://www.ee.newcastle.edu.au/users/staff/peter/Moylan.html

You can find other software that I've written by looking at
http://eepjm.newcastle.edu.au and the pages to which it points, or by
looking at the ftp server at ftp://eepjm.newcastle.edu.au/software.


