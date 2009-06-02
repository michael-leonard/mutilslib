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


public :: ReadSetting

interface ReadSetting
   module procedure ReadSetting_str
   module procedure ReadSetting_i4
   module procedure ReadSetting_r8
end interface
      
   

Contains

!#**********************************************************************
function findKeyWord(filename,unit,keyword) result(ok)
!#**********************************************************************
!#* Purpose: Find Key Word in Settings File
!#**********************************************************************
!#* Programmer: Mark Thyer, University of Newcastle
!#**********************************************************************
use kinds_dmsl_kit ! numeric kind definitions from DMSL
use MUtilsLib_Messagelog
use MUtilsLib_StringFuncs, only : operator(//)
implicit none

! Dummies - Inputs
character(len=*), intent(in) :: filename                ! Keyword
integer(mik), intent(in) :: unit                        ! Unit of file
character(len=*), intent(in) :: keyword                 ! Keyword
! Function - Outputs
integer(mik) :: ok                                       ! Error Flag (0 = No Errors, >0 Error condition)
! Locals
integer(mik) :: status                                    ! 
character(len=len_trim(keyword)) :: readLine                       ! 
ok=0
REWIND(UNIT=unit)
do
  read(unit,*,iostat=status) readLine
  select case(status)
  case(0)
    if (readLine(1:1)=="!") cycle ! Skip comments
    if (trim(readline)==trim(keyword)) exit
  case(-1) 
    call message($ERROR,"Reached end of file before finding keyword:"//trim(keyword)//" in settings file:"//trim(filename))
    ok=status
    return
  case default
    call message($ERROR,"Error No of "//status//" while searching for keyword:"//trim(keyword)//" in settings file:"//trim(filename))
    ok=status
    return
  end select
end do  
end function findKeyWord
!#**********************************************************************
function ReadSetting_str(filename,unit,keyword,val) result(ok)
!#**********************************************************************
!#* Purpose: Read Settings for Str Value
!#**********************************************************************
!#* Programmer: Mark Thyer, University of Newcastle
!#**********************************************************************
use kinds_dmsl_kit ! numeric kind definitions from DMSL
use MUtilsLib_Messagelog
use MUtilsLib_StringFuncs, only : operator(//)
implicit none

! Dummies - Inputs
character(len=*), intent(in) :: filename                ! Keyword
integer(mik), intent(in) :: unit                        ! Unit of file
character(len=*), intent(in) :: keyword                 ! Keyword
! Dummies - Outputs
character(len=*), intent(out) :: val              ! Value of Keyword
! Function - Outputs
integer(mik) :: ok                                       ! Error Flag (0 = No Errors, >0 Error condition)
! Locals
integer(mik) :: status                                    ! 
character(len=len_vLongStr) :: readLine                       ! 
ok=0
! Search file for keyword
ok=findkeyword(filename,unit,keyword)
! Once found keyword then read value
do
  read(unit,*,iostat=status) readLine
  select case(status)
  case(0)
    if (readLine(1:1)=="!") cycle ! Skip comments
    val=TRIM(readLine)
    exit
  case(-1) 
    call message($ERROR,"Reached end of file before finding value of keyword:"//trim(keyword)//" in settings file:"//trim(filename))
    ok=status
    return
  case default
    call message($ERROR,"Error No of "//status//" while searching for value of keyword:"//trim(keyword)//" in settings file:"//trim(filename))
    ok=status
   return
  end select
end do  


end function ReadSetting_str
!#**********************************************************************
function ReadSetting_i4(filename,unit,keyword,val) result(ok)
!#**********************************************************************
!#* Purpose: Read Settings for Integer(4) value
!#**********************************************************************
!#* Programmer: Mark Thyer, University of Newcastle
!#**********************************************************************
use kinds_dmsl_kit ! numeric kind definitions from DMSL
use MUtilsLib_Messagelog
use MUtilsLib_StringFuncs, only : operator(//),int
implicit none

! Dummies - Inputs
character(len=*), intent(in) :: filename                ! Keyword
integer(mik), intent(in) :: unit                        ! Unit of file
character(len=*), intent(in) :: keyword                 ! Keyword
! Dummies - Outputs
integer(4), intent(out) :: val              ! Value of Keyword
! Function - Outputs
integer(mik) :: ok                                       ! Error Flag (0 = No Errors, >0 Error condition)
! Locals
integer(mik) :: status                                    ! 
character(len=len_vLongStr) :: readLine                       ! 
ok=0
! Search file for keyword
ok=findkeyword(filename,unit,keyword)
! Once found keyword then read value
do
  read(unit,*,iostat=status) readLine
  select case(status)
  case(0)
    if (readLine(1:1)=="!") cycle ! Skip comments
    val=int(readLine)
    exit
  case(-1) 
    call message($ERROR,"Reached end of file before finding value of keyword:"//trim(keyword)//" in settings file:"//trim(filename))
    ok=status
    return
  case default
    call message($ERROR,"Error No of "//status//" while searching for value of keyword:"//trim(keyword)//" in settings file:"//trim(filename))
    ok=status
   return
  end select
end do  


end function ReadSetting_i4
!#**********************************************************************
function ReadSetting_r8(filename,unit,keyword,val) result(ok)
!#**********************************************************************
!#* Purpose: Read Settings for real(8) values
!#**********************************************************************
!#* Programmer: Mark Thyer, University of Newcastle
!#**********************************************************************
use kinds_dmsl_kit ! numeric kind definitions from DMSL
use MUtilsLib_Messagelog
use MUtilsLib_StringFuncs, only : operator(//),real
implicit none

! Dummies - Inputs
character(len=*), intent(in) :: filename                ! Keyword
integer(mik), intent(in) :: unit                        ! Unit of file
character(len=*), intent(in) :: keyword                 ! Keyword
! Dummies - Outputs
real(8), intent(out) :: val              ! Value of Keyword
! Function - Outputs
integer(mik) :: ok                                       ! Error Flag (0 = No Errors, >0 Error condition)
! Locals
integer(mik) :: status                                    ! 
character(len=len_vLongStr) :: readLine                       ! 
ok=0
! Search file for keyword
ok=findkeyword(filename,unit,keyword)
! Once found keyword then read value
do
  read(unit,*,iostat=status) readLine
  select case(status)
  case(0)
    if (readLine(1:1)=="!") cycle ! Skip comments
    val=real(readLine)
    exit
  case(-1) 
    call message($ERROR,"Reached end of file before finding value of keyword:"//trim(keyword)//" in settings file:"//trim(filename))
    ok=status
    return
  case default
    call message($ERROR,"Error No of "//status//" while searching for value of keyword:"//trim(keyword)//" in settings file:"//trim(filename))
    ok=status
   return
  end select
end do  


end function ReadSetting_r8

end module MUtilsLib_SettingsFiles