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
unitTestMod%numTests = 4
unitTestMod%name = "Module One - Unit Test Module Results"
IF(ALLOCATED(unitTest))DEALLOCATE(unitTest);ALLOCATE(unitTest(unitTestMod%numTests))
!
! Initialise unitTest paths
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
!
! Initialise unitTest files
inputFiles(1)     = "moduleOne_Input.txt"
standardsFiles(1) = "moduleOne_standard.txt"
resultsFiles(1)   = "moduleOne_results.txt"

inputFiles(2)     = "moduleOne_Input.txt"
standardsFiles(2) = "moduleOne_standard.txt"
resultsFiles(2)   = "moduleOne_results.txt"

!
inputFiles(3)     = "moduleOne_Input.txt"
standardsFiles(3) = "moduleOne_standard.txt"
resultsFiles(3)   = "moduleOne_results_wrong.txt"

inputFiles(4)     = "moduleOne_Input.txt"
standardsFiles(4) = "moduleOne_standard.txt"
resultsFiles(4)   = "moduleOne_results_wrong.txt"

! Initialise unitTest info
unitTest%ok=.FALSE.
unitTest%metaDataTag="" ! currently not used

unitTest(1)%name="Example test one"
unitTest%message=""
unitTest(2)%name="Example test two"
unitTest(3)%name="Example test three"
unitTest(4)%name="Example test four"


! Check that standards and input files are available
DO i=1,SIZE(inputFiles);CALL myFileInquire(myResult=ok,fileName=inputFiles(i),filePath=inputFilePath);IF(.NOT.ok)RETURN;END DO
DO i=1,SIZE(standardsFiles);CALL myFileInquire(myResult=ok,fileName=standardsFiles(i),filePath=standardsPath);IF(.NOT.ok)RETURN;END DO
!
! Open unit test module summary file and write header
CALL myFileOpen(unitID=SUMMARYFILEUNIT,myResult=ok,fileName="results_summary.txt",filePath=resultsPath,status="UNKNOWN");IF(.NOT.ok)RETURN
CALL myWriteHeader(unitID=SUMMARYFILEUNIT,myResult=ok,name=unitTestMod%name,inputFiles=inputFiles,&
                   standardsFiles=standardsFiles,resultsFiles=resultsFiles);IF(.NOT.ok)RETURN
!
!***TEST - Example Test One
CALL dummyUnitTest_one(testOneResult=unitTest(1)%result_i,message=unitTest(1)%message)

CALL testMyResult(testValue=unitTest(1)%result_i,value_true=0,myTestResult=unitTest(1)%ok)
CALL myWriteTestResult(testName=unitTest(1)%name,testResult=unitTest(1)%ok,failMessage=unitTest(1)%message,unitID=SUMMARYFILEUNIT)
!
!***TEST - Example Test Two
CALL myFileOpen(unitID=STNDFILEUNIT,myResult=ok,fileName=standardsFiles(1),filePath=standardsPath,status='OLD');IF(.NOT.ok)RETURN
CALL myFileOpen(unitID=TESTFILEUNIT,myResult=ok,fileName=resultsFiles(1),filePath=resultsPath,status='UNKNOWN');IF(.NOT.ok)RETURN

CALL myFileCompare(unitOne=TESTFILEUNIT,unitTwo=STNDFILEUNIT,myTestResult=unitTest(2)%ok)
CALL myWriteTestResult(testName=unitTest(2)%name,testResult=unitTest(2)%ok,failMessage=unitTest(2)%message,unitID=SUMMARYFILEUNIT)

!***TEST - Example Test Three
CALL myFileCompare(fileOne=trim(standardsPath)//trim(standardsFiles(3)), &
                   fileTwo=trim(resultsPath)//trim(resultsFiles(3)),myTestResult=unitTest(3)%ok)
CALL myWriteTestResult(testName=unitTest(3)%name,testResult=unitTest(3)%ok,failMessage=unitTest(3)%message,unitID=SUMMARYFILEUNIT)

!***TEST - Example Test Four - same as three but with external file comparison
CALL myFileCompare(fileOne=trim(standardsPath)//trim(standardsFiles(3)), &
                   fileTwo=trim(resultsPath)//trim(resultsFiles(3)),myTestResult=unitTest(3)%ok,Comparelevel=3)
CALL myWriteTestResult(testName=unitTest(3)%name,testResult=unitTest(3)%ok,failMessage=unitTest(3)%message,unitID=SUMMARYFILEUNIT)


IF(PRESENT(unitTestModOk))THEN;IF(ALL(unitTest%ok))unitTestModOk=.TRUE.;END IF

CONTAINS
   !______________
   !
   SUBROUTINE dummyUnitTest_one(testOneResult,message)
      IMPLICIT NONE 
      INTEGER,INTENT(OUT)::testOneResult
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
      
      testOneResult=0
      RETURN
      
100   CONTINUE
      testOneResult=-1
      message=''
      message="Error running dummyUnitTest_one"
      
   END SUBROUTINE dummyUnitTest_one
   !______________
   !
END SUBROUTINE unitTests_modOne

END MODULE moduleOne_unitTests
