


---


## Why can't the compiler find "dfport"? ##

If you are linking Fortran to MATLAB to produce .m files, you are required to add the following directory to:

`Project Properties | Fortran | Additional Include Directories |` _`[MATLABInstallDir]`_`\extern\lib\win32\microsoft`

In IVF11.0.074 this results in the compiler **not** being able to find the module `dfport` in file `dfport.f90` located in directory:

> _`[IFortInstallDir]`_`fortran\include`

Removing the directory for MATLAB is a quick-fix to this problem, but does not solve the reason for the conflict.

It has been noted as an issue for investigation in the future



