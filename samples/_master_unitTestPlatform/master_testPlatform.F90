PROGRAM TestPlatform
USE MUtilsLib
USE moduleOne_unitTests
USE moduleTwo_unitTests

IMPLICIT NONE
LOGICAL::unitTestModOk
!---
!
CALL myGlobalTestPlatformLog(action="open")

CALL unitTests_modOne(allTests=.TRUE.,unitTestModOk=unitTestModOk)
CALL unitTests_modTwo(allTests=.TRUE.,unitTestModOk=unitTestModOk)

CALL viewTxtFile("..\moduleOne_unitTests\Results\results_summary.txt")
CALL viewTxtFile("..\moduleTwo_unitTests\Results\results_summary.txt")

!CALL myGlobalTestPlatformLog(action="close")

END PROGRAM TestPlatform

