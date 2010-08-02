!<license>
module MUtilsLib_fileIO
use kinds_dmsl_kit
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


end module MUtilsLib_fileIO
!**************************
! For backwards compatibility
module fileio

use MUtilsLib_fileIO

end module fileio


