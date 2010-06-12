module MUtilsLib_System
    ! Generic system (windows) utilities
    ! Any subroutine that uses IVF/WINDOWS SPECIFIC routines
    ! e.g. 
    ! - calls to if/dfport module
    ! - IVF extensions to Fortran standard
    ! No other modules should have calls to the dos interface
    use kinds_dmsl_kit
    use MUtilsLib_MessageLog
    implicit none
    
    private ! Keep hidden
    
    public :: taskkill, &   ! Terminates ALL copies of a current process based on its image name
              expand, &    ! expand a dos environment variable
              getEnvVar, & ! Wrapper for expand, with better error handling
              findcurrentdir, &   ! determines the location of the current working directory
              processExist, &     ! Determine if a given process exists
              Generate_FileList,& ! Generates a list of files with a given extension in a given path 
              Generate_SystemList,& ! Generates a list from the system of either files or directories
              OSCall,&              ! OS Command line interface utility with immediate return or wait specified in milliseconds
              systemDateTime,&      ! Returns the system clock time and date as a text string
              viewTxtFile           ! Windows call to launch notepad and pass it a file                  
  contains
!*************************************************************************************************************   
   function Mutils_loc(imageName) result(ok)
     ! Terminates ALL copies of a current process based on its image name
     ! uses command prompt + dos syntax
     ! writes dos output to a temp file and then deletes
     ! requisite dos functions: tasklist
     use dfport,only: system
     implicit none
     character(len=*) :: imageName ! the name of the task to be killed
     integer :: ok ! error flag

     ok = system('taskkill /F /FI "IMAGENAME eq ' // trim(imageName) // '" >silent.txt' ) ! direct output to file rather than screen
     ok = system('del silent.txt')

   end function
!************************************************************************************************************* 
!*************************************************************************************************************   
   function taskkill(imageName) result(ok)
     ! Terminates ALL copies of a current process based on its image name
     ! uses command prompt + dos syntax
     ! writes dos output to a temp file and then deletes
     ! requisite dos functions: tasklist
     use dfport,only: system
     implicit none
     character(len=*) :: imageName ! the name of the task to be killed
     integer :: ok ! error flag

     ok = system('taskkill /F /FI "IMAGENAME eq ' // trim(imageName) // '" >silent.txt' ) ! direct output to file rather than screen
     ok = system('del silent.txt')

   end function
!*************************************************************************************************************   
   function expand(ev) result(str)
     ! expand a dos environment variable
     ! uses command prompt + dos syntax
     ! writes dos output to a temp file and then deletes
     ! requisite dos functions: ECHO, ev path variable must already 
     ! be declared in the system and implicit rule for expanding: %EnvVar%
     use dfport,only: system
     implicit none
     character(len=*) :: ev ! environment variable, specified as per a .bat file - must have %% symbols , e.g. %PATH%
     character(len = len_vLongStr) :: str ! the expanded string
     integer :: ok ! error flag

     ok = system('@ECHO "'//trim(ev) //'" >tempEV.txt')
     open(unit = 7698,file = "tempEV.txt")
       read(7698,*) str
     close(7698,status='delete')

   end function
!*************************************************************************************************************   
   function getEnvVar(ev,val) result(ok) ! Wrapper for expand - with a more suitable name, env variable
     
     character(len=*) :: ev ! environment variable, must have %% symbols , e.g. %PATH%
     character(len = len_vLongStr) :: val ! the value of the environmental variable
     integer(mik) :: ok
     ok=0
     val=expand(ev=ev)
     if (trim(val)==trim(ev)) then
      ok=-1;
      call message(log_error,"Environment Variable: "//trim(ev)//" not found")
      val=undefCH
     end if
   end function
!*************************************************************************************************************   
   function findcurrentdir(noslash) result(dir)
      ! determines the location of the current working directory
      ! uses command prompt + dos syntax
      ! writes dos output to a temp file and then deletes
      ! requisite dos functions: chdir
      use dfport, only : system
      use stringfuncs, only: fwdslash
      implicit none
      ! Dummies
      character(len=len_vLongStr) :: dir
      integer(mik) :: ok 
      logical, intent(in),optional :: noslash
      ! Locals
      ok = system("chdir >temp.txt")     ! issue a dos command
      open(unit=123,file="temp.txt")
      read(123,'(A512)') dir             ! get the filepath of the current directory
      close(123,status="delete")
      !dir = fwdslash(dir) ! convert to forward slashes 
      if(present(noslash)) then
        if(.not.noslash) dir=trim(dir)//"\"
      else
        dir=trim(dir)//"\"
      end if
    end function findcurrentdir
!*************************************************************************************************************   
   function processExist(imageName) result(test)
      ! Determine if a given process exists
      ! uses command prompt + dos syntax
      ! writes dos output to a temp file and then deletes
      ! requisite dos functions: tasklist
      use dfport,only: system
      use stringfuncs, only: lcase
      implicit none
      ! Dummies
      character(len=*),intent(in) :: imageName
      character(len=len_trim(imageName)) :: lcase_imageName
      logical :: test 
      ! Locals
      integer(mik) :: ok,status
      character(len=len_vLongStr) :: dummy

      ! This method uses a filter - but if the process does not exist then you get a message on the console
      ! which cannot be removed. A similar method is below that avoids a message being sent to the output

      !ok = system('tasklist /fi "ImageName eq '//trim(imagename)//'" >out.txt') ! issue a dos command
      !open(unit=123,file="out.txt")
      !i=0
      !do 
      !  read(123,*,iostat=status) dummy
      !  select case(status) 
      !  case(-1); exit
      !  case(0); i=i+1
      !  end select
      !end do
      !if (i>0) then; test=.true.; else; test=.false.; end if
      !close(123,status="delete")
      
      lcase_imagename = lcase(trim(imagename))
      test=.false. ! default
      ok = system('tasklist /fo "table" /nh >out.txt') ! issue a dos command
      open(unit=123,file="out.txt")
      do 
        read(123,*,iostat=status) dummy
        select case(status) 
        case(-1); 
            exit
        case(0); 
            if(lcase(trim(dummy))==lcase_imagename) test=.true. ! compare as lower case
        end select
      end do
      close(123,status="delete")


    end function processExist
!*************************************************************************************************************   
    function Generate_FileList(path, ext, filelist) result (ok)
     ! Generates a list of files with a given extension in a given path 
     ! uses command prompt + dos syntax
     ! writes dos output to a temp file and then deletes
     ! requisite dos functions: dir
     ! Author:
     ! Originally Written by Michael Leonard as Generate_RscriptList
     ! Generalised to Generate_FileList by Mark Thyer, May 2009
     use MUtilsLib_messagelog
     implicit none
     ! Dummies - Inputs
     character(len=*) :: path ! the path to the files are located
     character(len=*) :: ext ! the extension of the files that need to be returned, 
                             ! for files with one letter extentions (e.g, *.r), 
                             ! it is important to use a space, e.g. ext="*.r " to distinguish from .r** extensions
     ! Dummies - Outputs
     character(len=*),pointer :: filelist(:) ! Output: the list of Rscripts
     ! Function Results
     integer(mik) :: ok

     !Locals          
     ok=0
     ok=Generate_SystemList(path=path,ext=ext,list=filelist,listtype="FILE") 
     
    end function Generate_FileList
!*************************************************************************************************************   
    function Generate_SystemList(path, ext, list,listtype) result (ok)
     ! Generates a list of files with a given extension in a given path 
     ! uses command prompt + dos syntax
     ! writes dos output to a temp file and then deletes
     ! requisite dos functions: dir
     ! Author:
     ! Originally Written by Michael Leonard as Generate_RscriptList
     ! Generalised to Generate_FileList by Mark Thyer, May 2009
     use MUtilsLib_messagelog
     use dfport,only: system
     implicit none
     ! Dummies - Inputs
     character(len=*) :: path ! the path to the files are located
     character(len=*) :: listtype ! the type of list needed, options include
                             ! "FILE"    - list of files
                             ! "DIRECTORY" - list of directories
     character(len=*),optional :: ext ! the extension of the files that need to be returned, 
                             ! for files with one letter extentions (e.g, *.r), 
                             ! it is important to use a space, e.g. ext="*.r " to distinguish from .r** extensions
                             
                             
     ! Dummies - Outputs
     character(len=len_vLongStr),pointer :: list(:) ! the output list
     ! Function Results
     integer(mik) :: ok

     !Locals          
     integer(mik) :: err ! iostat error when reached the end of the file
     character(len = len_vLongStr) :: temp,errmsg ! used to read in in list 
     integer(mik) :: count ! the length of list
          integer(mik) :: i
     character :: slash
     
     ok=0

     i = len_trim(path)
     slash="\"
     if(path(i:i)=="/".or.path(i:i)=="\") slash=" "

     ! generate the list in the directory
     select case(listtype)
     case("DIRECTORY")
      ok=system('dir /A:D /B  "' // trim(path) // trim(slash) //'" >List.txt') 
      errmsg="Unable to locate any directories in path: "//trim(path)
     CASE("FILE")
      ok=system('dir /B/L "' // trim(path) // trim(slash) //'*.*" >List.txt') ! use 
               ! *.* to avoid an error message to the console when there are no .r files
      if (ok/=0) then
        errmsg="Unable to locate files with extension "//trim(ext)//" in path: "//trim(path)
        return
      end if
     END SELECT     
     
     open(unit = 9538,file="list.txt")
     ! count the number of scripts
     count = 0
     do
       read(9538,*,iostat=err) temp
       if(err/=0) exit
       if(index(temp,ext)/=0) count = count + 1 
     end do
     
     if (count==0) then ! Make sure there are some files with that extension in path
      call message(log_warn,errmsg)
      ok=1
      close(9538,status="delete")
      return
     end if
     
     ! retrieve file list
     if (associated(list)) deallocate(list)
     allocate(list(count))
     rewind(9538)
     count = 0
     do
       read(9538,*,iostat=err) temp
       if(err/=0) exit
       if(index(temp,ext)/=0) then ! the space is important to distinguish from .r** extensions
         count = count + 1
         list(count) = trim(temp)
       end if
     end do
     close(9538,status="delete")
   end function Generate_SystemList
 !*************************************************************************************************************   
    ! Copyright (C) 2001 by Fortran Library
    !
    ! This source may be freely copied, modified, or distributed so long as the original
    ! copyright statement remains intact.
    !
    ! Suggestions for improvment to the original posted version are welcome. Comments
    ! should be sent to mailto:webmaster@fortranlib.com
    !
    ! Version:  2.0, 4 August 2001, 21:20:00
    !
    ! Purpose:  OS Command line interface utility with immediate return or wait specified
    !           in milliseconds (routine automatically quotes the command string)
    !
    ! System Requirements:  Written for Digital/Compaq/Intel) Visual Fortran (x86)
    !
    ! Routine Name:  OSCall
    !
    ! iWaitMS:  default 32-bit (unsigned) integer wait value in milliseconds
    !           0 = do not wait for completion
    !          >0 = Number of milliseconds to wait for completion of initiated process
    !          -1 = Infinite wait (wait for process completion)
    !
    ! Command:  character command string
    !
    ! Args:     optional character argument list string
    !
    ! iRet:     default integer return code
    !          -1 = Unable to initiate process
    !           0 = Successful process initiate (if iWait = 0)

    subroutine OSCall(iWaitMS,Command,Args,iRet)

      ! use iflib
       use dfwin, only: T_STARTUPINFO,T_PROCESS_INFORMATION,NULL,STARTF_USESHOWWINDOW,SW_HIDE,& 
                        NULL_CHARACTER,NULL_SECURITY_ATTRIBUTES,CREATEPROCESS,WAITFORSINGLEOBJECT,CLOSEHANDLE
       !USE DFWINTY !!use ifwin, only:
       
       implicit none

       character(*), intent(in)     :: Command        !Command portion of the command line (i.e. the program name)
       character(*), intent(in)     :: Args           !Argument portion of the command line
       character(256)               :: CmdLine        !Work area for the command line
       integer, intent(in)          :: iWaitMS        !Process completion wait value in milliseconds
       integer, intent(out)         :: iRet           !Main return code
       integer                      :: iCRC           !Return code for CreateProcess

       type (T_StartupInfo)         :: StartInfo      !CreatProcess parms
       type (T_Process_Information) :: ProcInfo       !CreatProcess parms (created process info)
        
    !
    ! Initialize return code
    !
       iRet = 0
       !NULL_CHARACTER=>NULL(0)
    !
    ! Insure console window is suppressed
    !
       StartInfo%cb               = 68
       StartInfo%lpReserved       = 0
       StartInfo%lpDesktop        = NULL
       StartInfo%lpTitle          = NULL
       StartInfo%dwX              = 0
       StartInfo%dwY              = 0
       StartInfo%dwXSize          = 0
       StartInfo%dwYSize          = 0
       StartInfo%dwXCountChars    = 0
       StartInfo%dwYCountChars    = 0
       StartInfo%dwFillAttribute  = 0
       StartInfo%dwFlags          = StartF_UseShowWindow
       StartInfo%wShowWindow      = SW_HIDE
       StartInfo%cbReserved2      = 0
       StartInfo%lpReserved2      = NULL
    !
    ! Prepare the command line string and arguments
    !
       cmdLine = '"' // trim(command) // '" ' // trim(args) // char(0)
    !
    ! Initiate process
    !
!  This version will not work with IVF10.1.3440.2008 if compiler option (/check:pointer) is set
!    via Project Properties|Fortran|Run-time|Check for null pointers and allocatable array references
!   iCRC = CreateProcess(NULL_CHARACTER, &
!              cmdLine, &
!              null_Security_Attributes, &
!              null_Security_Attributes, &
!              .false., &
!              Null, &
!              Null, &
!              Null_Character, &
!              StartInfo, &
!              ProcInfo)

!! This version may not work with CVF6.6a, 
! but works with IVF Project Properties|Fortran|Run-time|Check for null pointers and allocatable array references set
!
       iCRC = CreateProcess(null, &
              cmdLine, &
              null, &
              null, &
              .false., &
              Null, &
              Null, &
              Null, &
              StartInfo, &
              ProcInfo)

    !
    ! Check return code from CreateProcess
    !
       if (iCRC .eq. 0) then !Nonzero means success (i.e. the process id)
          iRet = -1
          return
       end if
    !
    ! If user specified to wait
    !
       if (iWaitMS .ne. 0) then
          iRet = WaitForSingleObject(ProcInfo%hProcess,iWaitMS) !Wait for completion
       end if
     
       return

    end subroutine

!*************************************************************************************************************
FUNCTION systemDateTime() RESULT(dateTimeString)
!*                                                                                                                        
!*  FUNCTION:      curntDateTime
!*  DESCRIPTION:   Returns the current system time and date as a text string                                                  
!*  DEVELOPED BY:  Matthew Hardy
!*  VERSION:       1.0                                                                                                                                                                                   
!*  Last Modified: 08/06/2010 MJH 
!
USE kinds_dmsl_kit
IMPLICIT NONE
INTEGER(MIK),DIMENSION(8)::vals
CHARACTER(LEN=16)::dateTimeString
!---
!
dateTimeString=""

CALL DATE_AND_TIME(VALUES=vals)

SELECT CASE(vals(5))
  CASE(0:9);WRITE(dateTimeString(1:3),'(a1,i1,a1)')'0',vals(5),':'
  CASE(10:);WRITE(dateTimeString(1:3),'(i2,a1)')vals(5),':'
END SELECT

SELECT CASE(vals(6))
  CASE(0:9);WRITE(dateTimeString(4:6),'(a1,i1,a1)')'0',vals(6),'-'
  CASE(10:);WRITE(dateTimeString(4:6),'(i2,a1)')vals(6),'-'
END SELECT

SELECT CASE(vals(3))
  CASE(0:9);WRITE(dateTimeString(7:9),'(a1,i1,a1)')'0',vals(3),'/'
  CASE(10:);WRITE(dateTimeString(7:9),'(i2,a1)')vals(3),'/'
END SELECT

SELECT CASE(vals(2))
  CASE(0:9);WRITE(dateTimeString(10:12),'(a1,i1,a1)')'0',vals(2),'/'
  CASE(10:);WRITE(dateTimeString(10:12),'(i2,a1)')vals(2),'/'
END SELECT

WRITE(dateTimeString(13:16),'(i4)')vals(1)

END FUNCTION systemDateTime
!*************************************************************************************************************
SUBROUTINE viewTxtFile(fileToOpen)
!*                                                                                                                        
!*  FUNCTION:      curntDateTime
!*  DESCRIPTION:   Returns the current system time and date as a text string                                                  
!*  DEVELOPED BY:  Matthew Hardy
!*  VERSION:       1.0                                                                                                                                                                                   
!*  Last Modified: 08/06/2010 MJH 
USE dfwin,ONLY:GetSystemDirectory,null0 => null       
USE dflib,only: RUNQQ,SYSTEMQQ
IMPLICIT NONE
!
! Dummy variables
CHARACTER(LEN=*),INTENT(IN)::fileToOpen      
!
! Local variables
INTEGER::sysDirOk,indx_i,indx_ii      
CHARACTER(LEN=240)::sysDir,notePadExePath
CHARACTER(LEN=360)::command
LOGICAL::fileExist,launchOk     
!---
!   
! Check output file is present
INQUIRE(FILE=fileToOpen(1:LEN_TRIM(fileToOpen)),EXIST=fileExist)
IF(.NOT.fileExist)THEN;CALL message('Can not find output: '//fileToOpen(1:LEN_TRIM(fileToOpen)));RETURN;END IF
!
! Check notepad.exe can be found    
sysDirOk = GetSystemDirectory(sysDir,240)      
notePadExePath=sysDir(1:sysDirOk)//'\notepad.exe'

INQUIRE(FILE=notePadExePath(1:LEN_TRIM(notePadExePath)),EXIST=fileExist)

SELECT CASE(fileExist)
   CASE(.FALSE.)
         CALL message('Can not find the notepad text editor.& Output file created: '//fileToOpen(1:LEN_TRIM(fileToOpen)));RETURN
   CASE(.TRUE.)
      command=''
      indx_i =1 
      indx_ii=indx_i
      WRITE(command(indx_i:indx_ii),'(a)',ERR=100)'"'

      indx_i =indx_ii+1 
      indx_ii=indx_i+LEN_TRIM(fileToOpen)
      WRITE(command(indx_i:indx_ii),'(a)',ERR=100)fileToOpen(1:LEN_TRIM(fileToOpen))

      indx_i =indx_ii+1 
      indx_ii=indx_i
      WRITE(command(indx_i:indx_ii),'(a)',ERR=100)'"'
      command=command(1:LEN_TRIM(command))//char(0)
      
      launchOk = RUNQQ(notePadExePath,command)
END SELECT

RETURN
!
! Error Handling
100 CALL message('Error writing output file name string to command line call& Output file created: '//fileToOpen(1:LEN_TRIM(fileToOpen)))
RETURN

END SUBROUTINE viewTxtFile  

 end module MUtilsLib_System
!*****************************************************************************
! For backwards compatibility
module utils_system
use MUtilsLib_System
end module utils_system
!*****************************************************************************

 
