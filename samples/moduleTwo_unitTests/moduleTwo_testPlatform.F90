PROGRAM TestPlatform
USE MUtilsLib
USE moduleTwo_unitTests
IMPLICIT NONE
!---
!
CALL myGlobalTestPlatformLog(action="open")

CALL unitTests_modTwo()
CALL viewTxtFile("..\moduleTwo_unitTests\Results\results_summary.txt")
!CALL myGlobalTestPlatformLog(action="close")

END PROGRAM TestPlatform

