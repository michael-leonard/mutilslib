PROGRAM TestPlatform
USE MUtilsLib
USE moduleOne_unitTests
IMPLICIT NONE
!---
!
CALL myGlobalTestPlatformLog(action="open")

CALL unitTests_modOne()
CALL viewTxtFile("..\moduleOne_unitTests\Results\results_summary.txt")

!CALL myGlobalTestPlatformLog(action="close")
pause

END PROGRAM TestPlatform

