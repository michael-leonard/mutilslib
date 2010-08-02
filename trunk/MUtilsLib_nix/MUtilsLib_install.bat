::
:: Notes:
:: 1. Object directories Debug and Release need not pre-exist
:: 2. Deleting Debug and/or Release files also removes directory
::
::--------------------------------------------------------------
@echo off
set MAKE_NAME="MUtilsLib_makefile"
set DEBUG_PATH="./Dbg/"
set RELEASE_PATH="./Rls/"


::--------------------------------------------------------------
:while


@echo "MUtilsLib installation utility"

@echo "Please select an option:"
@echo "1. Install Debug Mode"
@echo "2. Install Release Mode"
@echo "3. Install Debug/Release"
@echo "4. Delete Debug Files"
@echo "5. Delete Release Files"
@echo "6. Delete Debug/Release"
@echo "7. Exit"



  set /p x=
    if %x% NEQ 1 (goto endif1)
       @echo "compiling Debug ..."
       mkdir %DEBUG_PATH%
       make debug -f %MAKE_NAME%
       goto eof
    :endif1
    if %x% NEQ 2 (goto endif2)
       @echo "compiling Release ..."
       mkdir %RELEASE_PATH%
       make release -f %MAKE_NAME%
       goto eof
    :endif2
    if %x% NEQ 3 (goto endif3)
       @echo "compiling Debug/Release ..."
       mkdir %DEBUG_PATH%
       mkdir %RELEASE_PATH%
       make all -f %MAKE_NAME%
       goto eof
    :endif3
    if %x% NEQ 4 (goto endif4)
       @echo "deleting Debug ..."
       rm -r %DEBUG_PATH%
       @echo "done"
       goto eof
    :endif4
    if %x% NEQ 5 (goto endif5)
       @echo "deleting Release ..."
       rm -r %RELEASE_PATH%
       @echo "done"
       goto eof
    :endif5
    if %x% NEQ 6 (goto endif6)
       @echo "deleting Debug/Release ..."
       rm -r %DEBUG_PATH%
       rm -r %RELEASE_PATH%
       @echo "done"
       goto eof
    :endif6
    if %x% NEQ 7 (goto endif7)
       @echo "exiting ..."
       goto eof
    :endif7
goto while

:eof