PROGRAM TestPlatform
USE MUtilsLib, only : init_log
USE moduleOne_unitTests, only : unitTests_modOne
USE MUtilsLib_unitTestPlatform, only : unitTest_SummarizeResults
USE MUtilsLib_system, only : viewTxtFile
USE kinds_dmsl_kit

IMPLICIT NONE
!---
!
logical(mlk) :: allTestsPass

CALL init_log(file="message.log",append_always=.TRUE.)
	
CALL unitTests_modOne()

call unitTest_SummarizeResults(allTestsPass,failmessage=" Please see message.log does to help resolve test failure")
	
CALL viewTxtFile("..\moduleOne_unitTests\Results\results_summary.txt")
if (.not.allTestsPass) CALL viewTxtFile("message.log")

END PROGRAM TestPlatform

