!<license>
module MUtilsLib_varFuncs

use kinds_dmsl_kit
implicit none

interface checkPresent
  module procedure checkPresent_i4
!  module procedure checkPresent_r4
!  module procedure checkPresent_r8
  module procedure checkPresent_bool
  module procedure checkPresent_str
end interface
 
contains
!*****************************************************************************
function checkPresent_str(var,valueIfNotPresent) result (checkPresent)
    use MUtilsLib_MessageLog
    use MUtilsLib_StringFuncs, only: operator(//)
    implicit none

    character(*), intent(in), optional :: var
    character(*), intent(in) :: valueIfNotPresent
    character(len=len_vLongStr) :: checkPresent     
    
    if (present(var)) then
      If (len_trim(var)>len_vLongStr) call message(log_warn,& 
           "length of string var is greater than len_vLongStr:"//len_vLongStr//" - need to increase")
        checkPresent=trim(var)
     else
      If (len_trim(valueifNotPresent)>len_vLongStr) call message(log_warn,&
           "length of string valueifNotPresent is greater than len_vLongStr:"//len_vLongStr//" - need to increase")
      checkPresent=trim(valueIfNotPresent)
     end if
 
end function checkPresent_str
!*****************************************************************************
function checkPresent_i4(var,valueIfNotPresent) result (checkPresent) 
    implicit none

    integer(4), intent(in), optional :: var
    integer(4), intent(in) :: valueIfNotPresent
    integer(4) :: checkPresent     
    
    if (present(var)) then
      checkPresent=var
     else
      checkPresent=valueIfNotPresent
     end if
 
end function checkPresent_i4
!*****************************************************************************
function checkPresent_bool(var,valueIfNotPresent) result (checkPresent) 
    implicit none

    logical, intent(in), optional :: var
    logical, intent(in) :: valueIfNotPresent
    logical :: checkPresent     
    
    if (present(var)) then
      checkPresent=var
     else
      checkPresent=valueIfNotPresent
     end if
 
end function checkPresent_bool
!*****************************************************************************

end module MUtilsLib_varfuncs
!*****************************************************************************
! For backwards compatibility
module varfuncs
use MUtilsLib_varfuncs
end module varfuncs
!*******************************