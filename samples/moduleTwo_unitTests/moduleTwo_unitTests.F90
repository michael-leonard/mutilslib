MODULE moduleTwo_unitTests
USE kinds_dmsl_kit
USE MUtilsLib_unitTestPlatform
!
IMPLICIT NONE

PUBLIC::unitTests_modTwo
!
! io variables
INTEGER(MIK),PARAMETER::SUMMARYFILEUNIT=500,RESULTSFILEUNIT=501,TESTFILEUNIT=502,STNDFILEUNIT=503
CHARACTER(LEN=180)::inputFilePath,standardsPath,resultsPath
CHARACTER(LEN=180),DIMENSION(1)::inputFiles
CHARACTER(LEN=180),DIMENSION(1)::standardsFiles
CHARACTER(LEN=180),DIMENSION(1)::resultsFiles
!
CONTAINS
!____________
!
SUBROUTINE unitTests_modTwo(allTests,unitTestModOk)
!
! Optional master test platform subroutine variables
LOGICAL,INTENT(IN),OPTIONAL::allTests
LOGICAL,INTENT(OUT),OPTIONAL::unitTestModOk
!
! Unit test module information
TYPE(unitTestModuleData)::unitTestMod
TYPE(unitTestResultsData),ALLOCATABLE,DIMENSION(:)::unitTest
!
! Local results variables
LOGICAL::ok
!
! General variables
INTEGER(MIK)::i
!---
!
! Initalise optional master test platform subroutine variables
IF(PRESENT(unitTestModOk))unitTestModOk=.FALSE.
! 
! Initialise Unit Test Module Information
unitTestMod%numTests = 2
unitTestMod%name = "Module Two - Unit Test Module Results"
IF(ALLOCATED(unitTest))DEALLOCATE(unitTest);ALLOCATE(unitTest(unitTestMod%numTests))
!
! Initialise unitTest paths
SELECT CASE(PRESENT(allTests))
   CASE(.TRUE.)
      inputFilePath = "..\moduleTwo_unitTests\InputFiles\"
      standardsPath = "..\moduleTwo_unitTests\Standards\"
      resultsPath   = "..\moduleTwo_unitTests\Results\"
   CASE(.FALSE.)
      inputFilePath = "InputFiles\"
      standardsPath = "Standards\"
      resultsPath   = "Results\"
END SELECT
!
! Initialise unitTest files
inputFiles(1)     = "moduleTwo_Input.txt"
standardsFiles(1) = "moduleTwo_standard.txt"
resultsFiles(1)   = "moduleTwo_results.txt"
!
! Initialise unitTest info
unitTest%ok=.FALSE.
unitTest%metaDataTag="" ! currently not used

unitTest(1)%name="Example test two"
unitTest(2)%name="Example test two"

! Check that standards and input files are available
DO i=1,SIZE(inputFiles);CALL myFileInquire(myResult=ok,fileName=inputFiles(i),filePath=inputFilePath);IF(.NOT.ok)RETURN;END DO
DO i=1,SIZE(standardsFiles);CALL myFileInquire(myResult=ok,fileName=standardsFiles(i),filePath=standardsPath);IF(.NOT.ok)RETURN;END DO
!
! Open unit test module summary file and write header
CALL myFileOpen(unitID=SUMMARYFILEUNIT,myResult=ok,fileName="results_summary.txt",filePath=resultsPath,status="UNKNOWN");IF(.NOT.ok)RETURN
CALL myWriteHeader(unitID=SUMMARYFILEUNIT,myResult=ok,name=unitTestMod%name,inputFiles=inputFiles,&
                   standardsFiles=standardsFiles,resultsFiles=resultsFiles);IF(.NOT.ok)RETURN
!
!***TEST - Example Test Two
CALL dummyUnitTest_two(testTwoResult=unitTest(1)%result_i,message=unitTest(1)%message)

CALL testMyResult(testValue=unitTest(1)%result_i,value_true=0,myTestResult=unitTest(1)%ok)
CALL myWriteTestResult(testName=unitTest(1)%name,testResult=unitTest(1)%ok,failMessage=unitTest(1)%message,unitID=SUMMARYFILEUNIT)
!
!***TEST - Example Test Two
CALL myFileOpen(unitID=STNDFILEUNIT,myResult=ok,fileName=standardsFiles(1),filePath=standardsPath,status='OLD');IF(.NOT.ok)RETURN
CALL myFileOpen(unitID=TESTFILEUNIT,myResult=ok,fileName=resultsFiles(1),filePath=resultsPath,status='UNKNOWN');IF(.NOT.ok)RETURN

CALL myFileCompare(unitOne=TESTFILEUNIT,unitTwo=STNDFILEUNIT,myTestResult=unitTest(2)%ok)
CALL myWriteTestResult(testName=unitTest(2)%name,testResult=unitTest(2)%ok,failMessage=unitTest(2)%message,unitID=SUMMARYFILEUNIT)

IF(PRESENT(unitTestModOk))THEN;IF(ALL(unitTest%ok))unitTestModOk=.TRUE.;END IF

CONTAINS
   !______________
   !
   SUBROUTINE dummyUnitTest_two(testTwoResult,message)
      IMPLICIT NONE 
      INTEGER,INTENT(OUT)::testTwoResult
      CHARACTER(LEN=120),INTENT(OUT)::message
      !
      ! Local variables
      LOGICAL::openOk
      CHARACTER(LEN=23)::dummyUnitTestString
      !---
      !
      ! Note myFileOpen is used for convienence only. MyFileInquire has already ensured that files are present
      CALL myFileOpen(unitID=STNDFILEUNIT,myResult=openOk,fileName=standardsFiles(1),filePath=standardsPath,status='OLD')
      REWIND(UNIT=STNDFILEUNIT)
      READ(STNDFILEUNIT,'(a23)')dummyUnitTestString
      CLOSE(UNIT=STNDFILEUNIT)
      
      
      CALL myFileOpen(unitID=TESTFILEUNIT,myResult=openOk,fileName=resultsFiles(1),filePath=resultsPath,status='UNKNOWN')      
      REWIND(UNIT=TESTFILEUNIT)
      WRITE(TESTFILEUNIT,'(a23)')dummyUnitTestString     
      CLOSE(UNIT=TESTFILEUNIT)
      
      testTwoResult=0
      RETURN
      
100   CONTINUE
      testTwoResult=-1
      message=''
      message="Error running dummyUnitTest_two"
      
   END SUBROUTINE dummyUnitTest_two
   !______________
   !
END SUBROUTINE unitTests_modTwo

END MODULE moduleTwo_unitTests
