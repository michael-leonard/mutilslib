MODULE moduleOne_unitTests

USE kinds_dmsl_kit
USE MUtilsLib_unitTestPlatform
!
IMPLICIT NONE

PUBLIC::unitTests_modOne
!
! io variables
INTEGER(MIK),PARAMETER::SUMMARYFILEUNIT=500,RESULTSFILEUNIT=501,TESTFILEUNIT=502,STNDFILEUNIT=503
CHARACTER(LEN=180)::inputFilePath,standardsPath,resultsPath
CHARACTER(LEN=180),DIMENSION(4)::inputFiles
CHARACTER(LEN=180),DIMENSION(4)::standardsFiles
CHARACTER(LEN=180),DIMENSION(4)::resultsFiles
!
CONTAINS
!____________
!
SUBROUTINE unitTests_modOne(allTests,unitTestModOk)
!
! Optional master test platform subroutine variables
LOGICAL,INTENT(IN),OPTIONAL::allTests
LOGICAL,INTENT(OUT),OPTIONAL::unitTestModOk
!
! Local results variables
integer::ok,testResult
logical :: testVal
!---
!
! Initalise optional master test platform subroutine variables
IF(PRESENT(unitTestModOk))unitTestModOk=.FALSE.
! 
! Initialise Unit Test Module Information
SELECT CASE(PRESENT(allTests))
   CASE(.TRUE.)
      inputFilePath = "..\moduleOne_unitTests\InputFiles\"
      standardsPath = "..\moduleOne_unitTests\Standards\"
      resultsPath   = "..\moduleOne_unitTests\Results\"
   CASE(.FALSE.)
      inputFilePath = "InputFiles\"
      standardsPath = "Standards\"
      resultsPath   = "Results\"
END SELECT

! Initialise unitTest files
!standardsFiles(1) = trim(standardsPath)//"moduleOne_standard.txt"
!resultsFiles(1)   = trim(resultsPath)//"moduleOne_results.txt"

!standardsFiles(2) = trim(standardsPath)//"moduleOne_standard.txt"
!resultsFiles(2)   = trim(resultsPath)//"moduleOne_results.txt"

!standardsFiles(3) = trim(standardsPath)"moduleOne_standard.txt"
!resultsFiles(3)   = trim(resultsPath)//"moduleOne_results_wrong.txt"

!standardsFiles(4) = trim(standardsPath)"moduleOne_standard.txt"
!resultsFiles(4)   = trim(resultsPath)//"moduleOne_results_wrong.txt"

call unitTest_init(name="Module One - Unit Test Module Results", &
                   nTests=6, &
                   nInputFiles=0, &
                   nRsltsFiles=0, &
                   nStndsFiles=0, &
                   err=ok)
if (ok/=0) then ;call message("Unable to initialise UnitTests"); return; end if
   
!***TEST - Example Test One - Compare procedure result to a known value = test correct
CALL dummyUnitTest_ArgResult(testresult,getCorrectResult=.true.)
testVal=(testresult==10)
CALL unitTest_chkResult(testVal=testVal,testName="Example test 1 - rslts from a proc. arg - correct",err=ok)

!***TEST - Example Test Two - Compare procedure result to a known value - test incorrect
CALL dummyUnitTest_ArgResult(testresult,getCorrectResult=.false.)
testVal=(testresult==10)
CALL unitTest_chkResult(testVal=testVal,testName="Example test 2 - rslts from a proc. arg - incorrect",err=ok,message="This test should fail")

!***TEST - Example Test Three - Compare procedure results output in file to a standard file - results correct
! This first procedure is a dummy example procedure that produces a test result
! In real example this would be replaced by a call to the procedure to be tested
CALL dummyUnitTest_FileResult(resultsFile=trim(resultsPath)//"moduleOne_results.txt",getCorrectResult=.true.,err=ok)
! This first procedure checks the results of the unit test
CALL unitTest_chkResult(fileOne=trim(standardsPath)//"moduleOne_standard.txt",fileTwo=trim(resultsPath)//"moduleOne_results.txt",& 
                        testName="Example test 3 - rslts from proc. output file-correct",message="This test should pass")

!***TEST - Example Test Four - Compare procedure results output in file to a standard file - results incorrect, line number comparison
CALL dummyUnitTest_FileResult(resultsFile=trim(resultsPath)//"moduleOne_results_wrong.txt",getCorrectResult=.false.,err=ok)
CALL unitTest_chkResult(fileOne=trim(standardsPath)//"moduleOne_standard.txt",fileTwo=trim(resultsPath)//"moduleOne_results_wrong.txt",compareLevel=1, &
                         testName="Example test 4 - rslts from proc. output file-incorrect (linecount comp)",message="This test should fail, but does not because only no of lines comparison is undertaken")

!***TEST - Example Test Five - Compare procedure results output in file to a standard file - results incorrect, line by line comparison
CALL dummyUnitTest_FileResult(resultsFile=trim(resultsPath)//"moduleOne_results_wrong.txt",getCorrectResult=.false.,err=ok)
CALL unitTest_chkResult(fileOne=trim(standardsPath)//"moduleOne_standard.txt",fileTwo=trim(resultsPath)//"moduleOne_results_wrong.txt",compareLevel=2, &
                        testName="Example test 5 - rslts from a proc output file-incorrect (line by line comp)",message="This test should fail")

!***TEST - Example Test Six - Compare procedure results output in file to a standard file, results incorrect, file comparison
CALL dummyUnitTest_FileResult(resultsFile=trim(resultsPath)//"moduleOne_results_wrong.txt",getCorrectResult=.false.,err=ok)
CALL unitTest_chkResult(fileOne=trim(standardsPath)//"moduleOne_standard.txt",fileTwo=trim(resultsPath)//"moduleOne_results_wrong.txt",compareLevel=3, &
                        testName="Example test 6 - rslts from a proc. output file-incorrect (external file comp)",message="This test should fail")

IF(PRESENT(unitTestModOk))THEN;IF(ALL(unitTest%ok))unitTestModOk=.TRUE.;END IF

CONTAINS
   !______________
   !
   SUBROUTINE dummyUnitTest_ArgResult(testResult,getCorrectResult)
      IMPLICIT NONE 
      INTEGER,INTENT(OUT)::testResult
      logical,intent(in) :: getCorrectResult
      !---
      
      if (getCorrectResult) then
        testResult=10
      else
        testResult=20
      end if
      
   END SUBROUTINE dummyUnitTest_ArgResult
   !______________
   !
   SUBROUTINE dummyUnitTest_FileResult(resultsFile,getCorrectResult,err)
      IMPLICIT NONE 
      character(len=*) :: resultsFile
      logical,intent(in) :: getCorrectResult
      integer :: err
      !---
      open(unit=10,file=resultsFile,iostat=err)
      if (err/=0) return
      
      write(10,'(a)') "Module One Example Output File"
      if (getCorrectResult) then
        write(10,'(a)') "Module One example test"
      else
        write(10,'(a)') "Module One example test - wrong"
      end if
      
      close(unit=10)
      if (err/=0) return
      
   END SUBROUTINE dummyUnitTest_FileResult
   
END SUBROUTINE unitTests_modOne

END MODULE moduleOne_unitTests
