!<license>

! Notes: FileIO cannot contain any dependencies onto other modules, apart from kinds_dmsl_kit
module MUtilsLib_fileIO
use kinds_dmsl_kit
use MUtilsLib_System,ONLY:findcurrentdir
use MUtilsLib_StringFuncs

implicit none
contains
function findEOF(filepath,err,msg) result(nLines)
! Returns number of lines at EOF 
  implicit none
  ! Dummies
  character(len=*),intent(in) :: filepath ! Full filename including path
  integer(mik),intent(out):: err
  character(len=*),intent(out) :: msg
  integer(mik) :: nLines
  ! Locals
  integer(mik) i,status
  character(len=1) text
  err=0;msg="Ok"
  ! Open File
  open(unit=10,file=trim(filepath),status="old",iostat=status)
  if (status/=0) then
    err=-1
    msg="Unable to open file:"//trim(filepath)
    nLines=0
    return 
  end if
  ! Read File
  i=0
  do 
    read(unit=10,fmt='(a)',iostat=status) text
    select case(status)
    case(0); 
        i=i+1; continue
    case(-1); 
        close(unit=10); nLines=i; exit
    case default; 
        err=-2;msg="Error reading file:"//trim(filepath)
        nLines=i
        return
    end select
  end do
  ! Close File
  close(unit=10)

end function findEOF

function countlines(unit) result(nLines)
  ! Returns number of lines at EOF 
  ! Without opening/closing files (slow!)
  implicit none
  ! Dummies
  integer,intent(in) :: unit ! unit no. of open file
  integer(mik) :: nLines
  ! Locals
  integer(mik) status
    
  nLines=0
  do 
    read(unit,*,iostat=status)
    if(status==0) then
      nLines=nLines+1
    else
      exit
    end if
  end do
  rewind(unit)
end function countLines

    subroutine skiplines(unit,n)
      ! description:  skips n lines in file given by unit
        
      implicit none
      integer, intent(in)  ::   unit ! unit of file to read
      integer, intent(in)  ::   n    ! no. of lines to skip
      integer              ::   i    ! index

      do i = 1,n
        read(unit,*)
      end do ! i 

    end subroutine skiplines

function fileExist(fileToOpen,msg)
!*************************************************
!use mutilslib_messagelog, only : message 

character(len=*),intent(in):: fileToOpen             ! full path of file name to test
character(len=*), intent(inout), optional :: msg ! suffix to add to log message, if not present no log message is written
logical :: fileExist

INQUIRE(FILE=trim(fileToOpen),EXIST=fileExist)

if (present(msg)) then
 if (.not.FileExist) then; msg='Could not find file: '//trim(fileToOpen); RETURN;END IF
end if

end function fileExist
!_____________________________________________________________________________________________
!
!> Creates a full file name and path string [relative  or absolute] from and input file name
!! and or path.
FUNCTION fullPath(fileName,filePath) RESULT(fullPathStr)
!
IMPLICIT NONE
CHARACTER(LEN=*),INTENT(IN),OPTIONAL::fileName   !> File name including extension eg testData.txt
CHARACTER(LEN=*),INTENT(IN),OPTIONAL::filePath   !> File path [relative or absolute] eg C:\devel\
CHARACTER(LEN=LEN(filePath))::fullPathStr        !> File name and path [relative or absolute] eg C:\devel\testData.txt
!
! Local variables
CHARACTER(LEN=360)::currentDir,name,path
 !---
 !
 ! DEFINE FULL NAME AND PATH STRING
 IF((PRESENT(fileName)).AND.(.NOT.PRESENT(filePath)))THEN
    name=remove_startslash(fileName)
    currentDir=findcurrentdir()
    fullPathStr=currentDir(1:LEN_TRIM(currentDir))//name(1:LEN_TRIM(name))
    
 ELSE IF((PRESENT(fileName)).AND.(PRESENT(filePath)))THEN
    name=remove_startslash(fileName)
    path=add_endslash(filePath)
    fullPathStr=path(1:LEN_TRIM(path))//name(1:LEN_TRIM(name))
 END IF

END FUNCTION




end module MUtilsLib_fileIO
!**************************
! For backwards compatibility
module fileio

use MUtilsLib_fileIO

end module fileio


