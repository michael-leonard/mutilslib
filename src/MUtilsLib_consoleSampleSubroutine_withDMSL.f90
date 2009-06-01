subroutine myApplication(runMode)
! dummy routine to make DMSL happy :-)
use kinds_dmsl_kit
implicit none
! dummies
integer(4),intent(in)::runMode
! locals
! Start procedure here
write(*,*) "HUGE ERROR!!! YOU SHOULD NOT BE HERE WHEN RUNNING UNDER CONSOLE!!!"
read(*,*)
! End procedure here
endsubroutine myApplication
!*******************************************************************
subroutine myFinish(finishMode,messageIN,statOut,messageOut)
! Purpose: shutdown routine.
! * message is used for any strongly abnormal finish that could not
!   be handled otherwise. Be prepared to crash/hangup in many such cases.
!   Report to author such cases for investigation.
!use kinds_dmsl_kit
!use ioTools_dmsl_mod,only:normalFinish !,alertDialog
implicit none
! dummies
integer(4),intent(in),optional::finishMode
character(*),intent(in),optional :: messageIN
integer(4),intent(inout),optional::statOUT
character(*),intent(out),optional::messageOUT

! Start procedure here
!if(present(message))call alertdialog(trim(message))
! * do anything else here - close data files, etc.
! * but do not stop (to allow freeing any resources by DMSL)
! End procedure here
endsubroutine myFinish
!*******************************************************************
 