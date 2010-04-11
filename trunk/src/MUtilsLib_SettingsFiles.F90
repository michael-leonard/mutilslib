module MUtilsLib_SettingsFiles
!~**********************************************************************
!~* Purpose: General Procedures for Settings Files
!~**********************************************************************
!~* Programmer: Mark Thyer , University of Newcastle
!~**********************************************************************
!~* Last modified:
!~**********************************************************************
!~* Comments:
!~**********************************************************************
!~* References:
!~**********************************************************************
!~* 2Do List:
!~**********************************************************************
!~* Quick description of public procedures:
!~*		1.
!~*		2.
!~*		3.
!~**********************************************************************
use kinds_dmsl_kit ! numeric kind definitions from DMSL

implicit none
Private


public :: ReadSetting,OpenSettingsFile

interface ReadSetting
   module procedure ReadSetting_0d_str
   module procedure ReadSetting_0d_i_mik
   module procedure ReadSetting_0d_r_mrk
   module procedure ReadSetting_0d_l_mlk
   module procedure ReadSetting_1d_str
   module procedure ReadSetting_1d_r_mrk
   module procedure ReadSetting_2d_r_mrk
end interface

contains
!#**********************************************************************
function openSettingsFile(file,unit,checkVersion) result (ok)
!#**********************************************************************
!#* Purpose: Opens a Settings File and checks version number is consistent
!#**********************************************************************
!#* Programmer: Mark Thyer, University of Newcastle
!#**********************************************************************
use kinds_dmsl_kit ! numeric kind definitions from DMSL
use MutilsLib_System, only : findCurrentDir
use MutilsLib_StringFuncs, only : relPathtoAbsPath,operator(//)
use MutilsLib_MessageLog, only : message,log_error
implicit none

! Dummies - Inputs
integer(mik), intent(in) ::  unit                     ! Unit no for settings file
character(len=len_vLongStr), intent(inout) ::  file   ! Settings file with path - can be relative 
                                                      ! or absolute file paths, on exit it is the absolute filepath
character(len=*), intent(in) :: checkVersion          ! Used to check whether setttings file has the same version number 
                                                      ! - ensures compatibility
! Function - Outputs
integer(mik) :: ok                                    ! Error Flag (0 = No Errors, >0 Error condition)
! Locals
integer(mik) :: status                                ! 
character(len=len_vLongStr) :: version                !
ok=0  
File=relPathtoAbsPath(file,findCurrentDir())
do 
  open(unit=unit,file=trim(File),iostat=status,status="OLD")
  select case(status)
  case(0) ! ok
     exit
  case(29) ! File not Found
     call message(log_error,"Settings File: "//trim(File)//" not found ")
     ok=status
     return
  case default
     call message(log_error,"Error No:"//ok//" when opening settings file: "//trim(File))
     ok=status
     return
  end select
end do

ok=readSetting(file=trim(file),unit=unit,keyword="[Version]",val=version)

if (ok/=0) then
  call message(log_error,"Unable to read [Version] of settings file: "//trim(File)); return
end if

if (trim(version)/=trim(checkVersion)) then
  call message(log_error,"Incompatible version of settings file: "//trim(File)&
                //", looking for version:"//trim(checkVersion)//" found version:"//trim(version))
  ok=1; return
end if

end function openSettingsFile
!#**********************************************************************
function findKeyWord(file,unit,keyword,nrow,ncol,rewindIn,msg_statusIn) result(ok)
!#**********************************************************************
!#* Purpose: Find Key Word in Settings File
!#**********************************************************************
!#* Programmer: Mark Thyer, University of Newcastle
!#**********************************************************************
use kinds_dmsl_kit ! numeric kind definitions from DMSL
use MUtilsLib_Messagelog
use MUtilsLib_StringFuncs, only : operator(//)
use MUtilsLib_VarFuncs, only: checkPresent
implicit none

! Dummies - Inputs
character(len=*), intent(in) :: file                ! Keyword
integer(mik), intent(in) :: unit                        ! Unit of file
character(len=*), intent(in) :: keyword                 ! Keyword
logical(mlk), intent(in), optional :: RewindIn          ! Rewind the file 
integer(mik), intent(in),optional :: msg_statusIn       ! Either log_error or log_warn    

! Dummies - Outputs
integer(mik), intent(out), optional :: nrow,ncol
! Function - Outputs
integer(mik) :: ok                                       ! Error Flag (0 = No Errors, >0 Error condition)
! Locals
integer(mik) :: status                                    ! 
character(len=len_trim(keyword)) :: readLine              ! 
logical(mlk) :: RewindLc
integer(mik) :: msg_status

! Initialise
ok=0
RewindLc=checkPresent(RewindIn,.true.) ! Default option is to rewind the file
msg_status=checkPresent(msg_statusIn,log_error) ! Default is if cannot find setting it is an error

If (rewindLc) REWIND(UNIT=unit)

do
  read(unit,*,iostat=status) readLine
  select case(status)
  case(0)
    if (readLine(1:1)=="!") cycle ! Skip comments
    if (trim(readline)==trim(keyword)) then
      if (.not.present(nrow) .and. .not.present(ncol)) then ! Only looking for a scalar 
        exit
      else 
        backspace(unit=unit) ! Looking for vector, go back read in 
        if (present(nrow) .and. .not.present(ncol)) then
          read(unit,'(a,",",i12)',iostat=status) readLine,nrow
          if (status/=0) then
            call message(msg_status,"IO Error No: "//status//" and unable to read nrow for keyword:"//&
                        trim(keyword)//" in settings file: "//trim(file))
            ok=status;return
          end if
          return
        else if (.not.present(nrow) .and. present(ncol)) then
          read(unit,'(a,",",i12)',iostat=status) readLine,ncol
          if (status/=0) then
            call message(msg_status,"IO Error No: "//status//" and unable to read ncol for keyword:"//&
                         trim(keyword)//" in settings file: "//trim(file))
            ok=status;return
          end if
          return
        else
         read(unit,'(a,",",2i12)',iostat=status) readLine,nrow,ncol
         if (status/=0) then
            call message(msg_status,"IO Error No: "//status//" and unable to read nrow and ncol for keyword:"//&
                        trim(keyword)//" in settings file: "//trim(file))
            ok=status;return
          end if
          return
        end if
      end if
    end if  
  case(-1) 
    call message(msg_status,"Reached end of file before finding keyword:"//&
                trim(keyword)//" in settings file: "//trim(file))
    ok=status
    return
  case default
    call message(msg_status,"Error No of "//status//" while searching for keyword:"//&
                trim(keyword)//" in settings file: "//trim(file))
    ok=status
    return
  end select
end do  
end function findKeyWord
!#**********************************************************************
function ReadSetting_0d_str(file,unit,keyword,val,rewindIn,msg_statusIn) result(ok)
!#**********************************************************************
!#* Purpose: Read Settings for Str Value
!#**********************************************************************
!#* Programmer: Mark Thyer, University of Newcastle
!#**********************************************************************
use kinds_dmsl_kit ! numeric kind definitions from DMSL
use MUtilsLib_Messagelog
use MUtilsLib_StringFuncs, only : operator(//)
use MUtilsLib_VarFuncs, only: checkPresent
implicit none

! Dummies - Inputs
character(len=*), intent(in) :: file                ! file
integer(mik), intent(in) :: unit                        ! Unit of file
character(len=*), intent(in) :: keyword                 ! Keyword
logical(mlk), intent(in), optional :: RewindIn          ! Rewind file
integer(mik), intent(in),optional :: msg_statusIn       ! Either log_error or log_warn    
! Dummies - Outputs
character(len=*), intent(out) :: val              ! Value of Keyword
! Function - Outputs
integer(mik) :: ok                                       ! Error Flag (0 = No Errors, >0 Error condition)
! Locals
integer(mik) :: status                                    ! 
character(len=len_vLongStr) :: readLine                       ! 
integer(mik) :: msg_status
ok=0
msg_status=checkPresent(msg_statusIn,log_error) ! Default is if cannot find setting it is an error
! Search file for keyword
ok=findkeyword(file,unit,keyword,rewindIn=rewindIn,msg_statusIn=msg_statusIn)
if (ok/=0) return

! Once found keyword then read value
do
  read(unit,*,iostat=status) readLine
  select case(status)
  case(0)
    if (readLine(1:1)=="!") cycle ! Skip comments
    val=TRIM(readLine)
    exit
  case(-1) 
    call message(msg_status,"Reached end of file before finding value of keyword:"//&
                    trim(keyword)//" in settings file: "//trim(file))
    ok=status
    return
  case default
    call message(msg_status,"Error No of "//status//" while searching for value of keyword:"//&
                    trim(keyword)//" in settings file: "//trim(file))
    ok=status
   return
  end select
end do  


end function ReadSetting_0d_str
!#**********************************************************************
function ReadSetting_0d_i_mik(file,unit,keyword,val,rewindIn,msg_statusIn) result(ok)
!#**********************************************************************
!#* Purpose: Read Settings for Integer(4) value
!#**********************************************************************
!#* Programmer: Mark Thyer, University of Newcastle
!#**********************************************************************
use kinds_dmsl_kit ! numeric kind definitions from DMSL
use MUtilsLib_Messagelog
use MUtilsLib_StringFuncs, only : operator(//),int
use MUtilsLib_VarFuncs, only: checkPresent
implicit none

! Dummies - Inputs
character(len=*), intent(in) :: file                ! file
integer(mik), intent(in) :: unit                        ! Unit of file
character(len=*), intent(in) :: keyword                 ! Keyword
logical(mlk), intent(in), optional :: RewindIn          ! Rewind file
integer(mik), intent(in),optional :: msg_statusIn       ! Either log_error or log_warn    
! Dummies - Outputs
integer(mik), intent(out) :: val              ! Value of Keyword
! Function - Outputs
integer(mik) :: ok                                       ! Error Flag (0 = No Errors, >0 Error condition)
! Locals
integer(mik) :: status                                    ! 
character(len=len_vLongStr) :: readLine                       ! 
integer(mik) :: msg_status

ok=0
msg_status=checkPresent(msg_statusIn,log_error) ! Default is if cannot find setting it is an error
! Search file for keyword
ok=findkeyword(file,unit,keyword,RewindIn=RewindIn,msg_StatusIn=msg_StatusIn)
if (ok/=0) return
! Once found keyword then read value
do
  read(unit,*,iostat=status) readLine
  select case(status)
  case(0)
    if (readLine(1:1)=="!") cycle ! Skip comments
    val=int(readLine)
    exit
  case(-1) 
    call message(msg_status,"Reached end of file before finding value of keyword:"//&
                            trim(keyword)//" in settings file: "//trim(file))
    ok=status
    return
  case default
    call message(msg_status,"Error No of "//status//" while searching for value of keyword:"//&
                            trim(keyword)//" in settings file: "//trim(file))
    ok=status
   return
  end select
end do  


end function ReadSetting_0d_i_mik
!#**********************************************************************
  function ReadSetting_0d_r_mrk(file,unit,keyword,val,rewindIn,msg_statusIn) result(ok)
!#**********************************************************************
!#* Purpose: Read Settings for real(8) values
!#**********************************************************************
!#* Programmer: Mark Thyer, University of Newcastle
!#**********************************************************************
use kinds_dmsl_kit ! numeric kind definitions from DMSL
use MUtilsLib_Messagelog
use MUtilsLib_StringFuncs, only : operator(//),real
use MUtilsLib_VarFuncs, only: checkPresent
implicit none

! Dummies - Inputs
character(len=*), intent(in) :: file                ! file
integer(mik), intent(in) :: unit                        ! Unit of file
character(len=*), intent(in) :: keyword                 ! Keyword
logical(mlk), intent(in),optional :: RewindIn                    ! Rewind file?
integer(mik), intent(in),optional :: msg_statusIn       ! Either log_error or log_warn    

! Dummies - Outputs
real(mrk), intent(out) :: val              ! Value of Keyword
! Function - Outputs
integer(mik) :: ok                                       ! Error Flag (0 = No Errors, >0 Error condition)
! Locals
integer(mik) :: status                                    ! 
character(len=len_vLongStr) :: readLine                       ! 
integer(mik) :: msg_status

ok=0
msg_status=checkPresent(msg_statusIn,log_error) ! Default is if cannot find setting it is an error
! Search file for keyword
ok=findkeyword(file,unit,keyword,RewindIn=RewindIn,msg_StatusIn=msg_StatusIn)
if (ok/=0) return
! Once found keyword then read value
do
  read(unit,*,iostat=status) readLine
  select case(status)
  case(0)
    if (readLine(1:1)=="!") cycle ! Skip comments
    val=real(readLine)
    exit
  case(-1) 
    call message(msg_status,"Reached end of file before finding value of keyword:"//&
                trim(keyword)//" in settings file: "//trim(file))
    ok=status
    return
  case default
    call message(msg_status,"Error No of "//status//" while searching for value of keyword:"//&
                trim(keyword)//" in settings file: "//trim(file))
    ok=status
   return
  end select
end do  


end function ReadSetting_0d_r_mrk
!#**********************************************************************
function ReadSetting_0d_l_mlk(file,unit,keyword,val,rewindIn,msg_statusIn) result(ok)
!#**********************************************************************
!#* Purpose: Read Settings for logical values
!#**********************************************************************
!#* Programmer: Mark Thyer, University of Newcastle
!#**********************************************************************
use kinds_dmsl_kit ! numeric kind definitions from DMSL
use MUtilsLib_Messagelog
use MUtilsLib_StringFuncs, only : operator(//),real
use MUtilsLib_VarFuncs, only: checkPresent
implicit none

! Dummies - Inputs
character(len=*), intent(in) :: file                ! file
integer(mik), intent(in) :: unit                        ! Unit of file
character(len=*), intent(in) :: keyword                 ! Keyword
logical(mlk), intent(in),optional :: RewindIn                    ! Rewind file?
integer(mik), intent(in),optional :: msg_statusIn       ! Either log_error or log_warn    
! Dummies - Outputs
logical(mlk), intent(out) :: val              ! Value of Keyword
! Function - Outputs
integer(mik) :: ok                                       ! Error Flag (0 = No Errors, >0 Error condition)
! Locals
integer(mik) :: status                                    ! 
character(len=len_vLongStr) :: readLine                       ! 
integer(mik) :: msg_status

ok=0
msg_status=checkPresent(msg_statusIn,log_error) ! Default is if cannot find setting it is an error
! Search file for keyword
ok=findkeyword(file,unit,keyword,RewindIn=RewindIn,msg_StatusIn=msg_StatusIn)
if (ok/=0) return
! Once found keyword then read value
do
  read(unit,*,iostat=status) readLine
  select case(status)
  case(0)
    if (readLine(1:1)=="!") cycle ! Skip comments
    select case(TRIM(readline))
    case("T","t","TRUE","true")
        val=.true.
    case("F","f","FALSE","false")
        val=.false.    
    end select
    exit
  case(-1) 
    call message(msg_status,"Reached end of file before finding value of keyword:"//&
                trim(keyword)//" in settings file: "//trim(file))
    ok=status
    return
  case default
    call message(msg_status,"Error No of "//status//" while searching for value of keyword:"//&
                 trim(keyword)//" in settings file: "//trim(file))
    ok=status
   return
  end select
end do  


end function ReadSetting_0d_l_mlk
!#**********************************************************************
function ReadSetting_1d_str(file,unit,keyword,val,rewindIn,msg_statusIn) result(ok)
!#**********************************************************************
!#* Purpose: Read Settings for Str Value
!#**********************************************************************
!#* Programmer: Mark Thyer, University of Newcastle
!#**********************************************************************
use kinds_dmsl_kit ! numeric kind definitions from DMSL
use MUtilsLib_Messagelog
use MUtilsLib_StringFuncs, only : operator(//)
use MUtilsLib_VarFuncs, only: checkPresent
implicit none

! Dummies - Inputs
character(len=*), intent(in) :: file                ! file
integer(mik), intent(in) :: unit                        ! Unit of file
character(len=*), intent(in) :: keyword                 ! Keyword
logical(mlk), intent(in), optional :: RewindIn          ! Rewind file
integer(mik), intent(in),optional :: msg_statusIn       ! Either log_error or log_warn    
! Dummies - Outputs
character(len=*), allocatable, intent(out) :: val(:)    ! Value of Keyword
! Function - Outputs
integer(mik) :: ok                                       ! Error Flag (0 = No Errors, >0 Error condition)
! Locals
integer(mik) :: status,ncol
integer(mik), allocatable :: len_val(:)                  ! 
character(len=len_vLongStr) :: readLine                  ! 
integer(mik) :: msg_status

ok=0
msg_status=checkPresent(msg_statusIn,log_error) ! Default is if cannot find setting it is an error
! Search file for keyword
ok=findkeyword(file,unit,keyword,RewindIn=RewindIn,msg_StatusIn=msg_StatusIn,ncol=ncol)
if (ok/=0) return

! Once found keyword then read value
if (allocated(val)) deallocate(val); allocate(val(ncol),len_val(ncol))
val=undefCH
do
  read(unit,*,iostat=status) readLine
  select case(status)
  case(0)
    if (readLine(1:1)=="!") cycle ! Skip comments
    backspace(unit)
    read(unit,"(<ncol>g)",iostat=status) val
    if (status/=0) then
      call message(msg_status,"IO Error No: "//status//" and unable to read value of keyword: "//&
                    trim(keyword)//" in settings file: "//trim(file)); ok=status;return
    end if
    len_val=len_trim(val)
    if(any(val==undefCH) .or. any(len_trim(val)==0)) then
      call message(msg_status,"Size mismatch reading in values for:"//&
                    trim(keyword)//" in settings file: "//trim(file)); ok=-1;return
    end if
    exit
  case(-1) 
    call message(msg_status,"Reached end of file before finding value of keyword:"//&
                    trim(keyword)//" in settings file: "//trim(file))
    ok=status
    return
  case default
    call message(msg_status,"Error No of "//status//" while searching for value of keyword:"//&
                    trim(keyword)//" in settings file: "//trim(file))
    ok=status
   return
  end select
end do  


end function ReadSetting_1d_str
!#**********************************************************************
function ReadSetting_1d_r_mrk(file,unit,keyword,val,rewindIn,msg_statusIn) result(ok)
!#**********************************************************************
!#* Purpose: Read Settings for real(8) values
!#**********************************************************************
!#* Programmer: Mark Thyer, University of Newcastle
!#**********************************************************************
use kinds_dmsl_kit ! numeric kind definitions from DMSL
use MUtilsLib_Messagelog
use MUtilsLib_StringFuncs, only : operator(//),real
use MUtilsLib_VarFuncs, only: checkPresent
implicit none

! Dummies - Inputs
character(len=*), intent(in) :: file                ! Keyword
integer(mik), intent(in) :: unit                        ! Unit of file
character(len=*), intent(in) :: keyword                 ! Keyword
logical(mlk), intent(in),optional :: RewindIn                    ! Rewind file?
integer(mik), intent(in),optional :: msg_statusIn       ! Either log_error or log_warn    
! Dummies - Outputs
real(mrk), intent(out),allocatable :: val(:)              ! Value of Keyword
! Function - Outputs
integer(mik) :: ok                                       ! Error Flag (0 = No Errors, >0 Error condition)
! Locals
integer(mik) :: status,ncol                                   ! 
character(len=len_vLongStr) :: readLine                       ! 
integer(mik) :: msg_status

ok=0
msg_status=checkPresent(msg_statusIn,log_error) ! Default is if cannot find setting it is an error
! Search file for keyword
ok=findkeyword(file,unit,keyword,RewindIn=RewindIn,msg_StatusIn=msg_StatusIn,ncol=ncol)
if (ok/=0) return
If(allocated(val)) deallocate(val)
allocate(val(ncol))
val=undefRN
! Once found keyword then read value
do
  read(unit,*,iostat=status) readLine
  select case(status)
  case(0)
    if (readLine(1:1)=="!") cycle ! Skip comments
    backspace(unit)
    read(unit,"(<ncol>f)",iostat=status) val
    if (status/=0) then
      call message(msg_status,"IO Error No: "//status//" and unable to read value of keyword: "//&
                    trim(keyword)//" in settings file: "//trim(file)); ok=status;return
    end if
    if(any(val==undefRN)) then
      call message(msg_status,"Size mismatch reading in values for:"//&
                    trim(keyword)//" in settings file: "//trim(file)); ok=status;return
    end if
    exit
  case(-1) 
    call message(msg_status,"Reached end of file before finding value of keyword:"//&
                    trim(keyword)//" in settings file: "//trim(file)); ok=status;return
  case default
    call message(msg_status,"Error No of "//status//" while searching for value of keyword:"//&
                    trim(keyword)//" in settings file: "//trim(file)); ok=status;return
  end select
end do  


end function ReadSetting_1d_r_mrk
!#**********************************************************************
function ReadSetting_2d_r_mrk(file,unit,keyword,val,rewindIn,msg_statusIn) result(ok)
!#**********************************************************************
!#* Purpose: Read Settings for real(8) values
!#**********************************************************************
!#* Programmer: Mark Thyer, University of Newcastle
!#**********************************************************************
use kinds_dmsl_kit ! numeric kind definitions from DMSL
use MUtilsLib_Messagelog
use MUtilsLib_StringFuncs, only : operator(//),real
use MUtilsLib_VarFuncs, only: checkPresent
implicit none

! Dummies - Inputs
character(len=*), intent(in) :: file                ! Keyword
integer(mik), intent(in) :: unit                        ! Unit of file
character(len=*), intent(in) :: keyword                 ! Keyword
logical(mlk), intent(in),optional :: RewindIn                    ! Rewind file?
integer(mik), intent(in),optional :: msg_statusIn       ! Either log_error or log_warn    
! Dummies - Outputs
real(mrk), intent(out),allocatable :: val(:,:)              ! Value of Keyword
! Function - Outputs
integer(mik) :: ok                                       ! Error Flag (0 = No Errors, >0 Error condition)
! Locals
integer(mik) :: status,nrow,ncol,i                             ! 
character(len=len_vLongStr) :: readLine                       ! 
integer(mik) :: msg_status

ok=0
msg_status=checkPresent(msg_statusIn,log_error) ! Default is if cannot find setting it is an error
! Search file for keyword
ok=findkeyword(file,unit,keyword,RewindIn=RewindIn,msg_StatusIn=msg_StatusIn,ncol=ncol,nrow=nrow)
if (ok/=0) return

If(allocated(val)) deallocate(val)
allocate(val(nrow,ncol))
! Once found keyword then read value
do
  read(unit,*,iostat=status) readLine
  select case(status)
  case(0)
    if (readLine(1:1)=="!") cycle ! Skip comments
    ! Once finished comments then start reading array
    backspace(unit)
    do i=1,nrow
      read(unit,"(<ncol>f12.0)",iostat=status) val(i,:)
      if (status/=0) then
        call message(msg_status,"IO Error No: "//status//" and unable to read nrow:"//i//" value of keyword: "//&
                      trim(keyword)//" in settings file: "//trim(file)); ok=status;return
      end if
    end do
    ! Once finished reading array then exit
    exit
  case(-1) 
    call message(msg_status,"Reached end of file before finding value of keyword:"//&
                 trim(keyword)//" in settings file: "//trim(file))
    ok=status
    return
  case default
    call message(msg_status,"Error No of "//status//" while searching for value of keyword:"//&
                    trim(keyword)//" in settings file: "//trim(file))
    ok=status
   return
  end select
end do  


end function ReadSetting_2d_r_mrk
!#**********************************************************************
end module MUtilsLib_SettingsFiles