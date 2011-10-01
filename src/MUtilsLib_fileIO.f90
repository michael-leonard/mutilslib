!<license>

!> Notes: FileIO cannot contain any dependencies onto other modules, apart from kinds_dmsl_kit
module MUtilsLib_fileIO

  use kinds_dmsl_kit

  contains

    !> Returns number of lines at EOF 
    function findEOF(filepath,err,msg) result(nLines)

      implicit none
      ! Dummies
      character(len=*),intent(in) :: filepath !< Full filename including path
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

    !> Returns number of lines at EOF 
    !> Without opening/closing files (for speed)
    function countlines(unit,stripBlank) result(nLines)
      implicit none
      ! Dummies
      integer,intent(in) :: unit !< unit no. of open file
      integer(mik) :: nLines     !< no. lines in file
      logical, intent(in), optional :: stripBlank !< ignore blank lines?
      ! Locals
      integer(mik) status
      character(20) :: ch ! test for non-blank lines, assume any character exists in first 20 cols
      logical :: cutBlank ! local copy of optional stripBlank
      integer :: temp
      
      cutBlank=.false.
      if(present(stripBlank)) cutBlank=stripBlank
      nLines=0
      do 
        read(unit,*,iostat=status) ch
        if(status==0) then
          if(cutBlank) then
            if(len_trim(ch)>0) nLines=nLines+1
          else
            nLines=nLines+1
          end if
        else
          exit
        end if
      end do
      rewind(unit)
    end function countLines

    !>  skips n lines in file given by unit
    subroutine skiplines(unit,n)
      
        
      implicit none
      integer, intent(in)  ::   unit !< unit of file to read
      integer, intent(in)  ::   n    !< no. of lines to skip
      integer              ::   i    ! index

      do i = 1,n
        read(unit,*)
      end do ! i 

    end subroutine skiplines

    !> Check if a file exists
    function fileExist(fileToOpen,msg)
      character(len=*),intent(in):: fileToOpen         !< full path of file name to test
      character(len=*), intent(inout), optional :: msg !< suffix to add to log message, if not present no log message is written
      logical :: fileExist

      INQUIRE(FILE=trim(fileToOpen),EXIST=fileExist)

      if (present(msg)) then
       if (.not.FileExist) then; msg='Could not find file: '//trim(fileToOpen); RETURN;END IF
      end if

    end function fileExist

end module MUtilsLib_fileIO

!> For backwards compatibility
module fileio
  use MUtilsLib_fileIO
end module fileio


