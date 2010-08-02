*nix means Unix or Linux platforms. These platforms do not have the dfport library to run the system commands in the MUtilsLib_system.f90 file. A new master module was created in the MUtilsLib_nix.f90 file to chop out this dependency.

There are several free compilers for Linux: ifort, g95, gfortran. g95 is used here. g95 does not have some functions which are custom to the Intel family, example INT_PTR_KIND, so there was a minor change to the MUtilsLib_miniDMSL.f90 file. The makefile should work under Windows as well if g95 is installed using the mingw environment. 

The makefile system under windows does not like folders called "Debug" or "Release", so "Dbg" and "Rls" are used instead. If you don't like makefiles files, run the install file (or .bat file on win) instead.