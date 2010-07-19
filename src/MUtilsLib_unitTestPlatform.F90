!*************************************************************************************!
!*                                                                                   *!
!*  MODULE:       MUtilsLib_unitTestPlatform                                         *!
!*  DEVELOPED BY: Matthew Hardy                                                      *!
!*  DESCRIPTION:  The unit test platform provides a set of moudule procedures to 
!*                support the fast and efficent creation of a unit testing framework
!*                for fortran procedures.  A variety of unit test framework can be 
!8                created depending on the users requirments                         *!
!*                                                                                   *!                                             
!*  VERSION:      1.0                                                                *!
!*  CONTAINS:     myFileInquire,myFileOpen,myWriteHeader,myIntegerTestResult         *!
!*                myWriteTestResult                                                  *!
!*                                                                                   *!
!*  LAST MODIFIED: 08/06/2010 MJH                                                    *!
!*                                                                                   *!
!*************************************************************************************! 
MODULE MUtilsLib_unitTestPlatform
USE kinds_dmsl_kit
USE MUtilsLib_System
USE MUtilsLib_MessageLog
IMPLICIT NONE
SAVE

TYPE unitTestModuleData
CHARACTER(LEN=180)::name
INTEGER(MIK)::numTests
END TYPE unitTestModuleData

TYPE unitTestResultsData
   LOGICAL::ok,result_l
   INTEGER(MIK)::result_i
   CHARACTER(LEN=75)::name
   CHARACTER(LEN=180)::metaDataTag
   CHARACTER(LEN=360)::message
END TYPE unitTestResultsData

PUBLIC::myFileInquire,myFileOpen,myWriteHeader,testMyResult,myWriteTestResult,myFileCompare
!---
!
INTERFACE testMyResult
MODULE PROCEDURE testMyResult_Integer,testMyResult_Logical
END INTERFACE

CONTAINS
!___________________________________________________________________________________________________________________
!
SUBROUTINE myFileInquire(myResult,fileName,filePath,fileNameAndPath)
   IMPLICIT NONE
   ! Subroutine
   LOGICAL,INTENT(OUT)::myResult
   CHARACTER(LEN=*),INTENT(IN),OPTIONAL::fileName,filePath
   CHARACTER(LEN=*),INTENT(IN),OPTIONAL::fileNameAndPath   
   ! Local
   INTEGER::err
   LOGICAL::fileExists  
   CHARACTER(LEN=360)::test_currentPath,test_fileNameAndPath
   !---
   !
   myResult=.TRUE.;test_fileNameAndPath=''
   ! DEFINE FULL NAME AND PATH STRING
   IF((PRESENT(fileName)).AND.(.NOT.PRESENT(filePath)))THEN
      test_currentPath=findcurrentdir()
      test_fileNameAndPath=test_currentPath(1:LEN_TRIM(test_currentPath))//fileName(1:LEN_TRIM(fileName))
   ELSE IF((PRESENT(fileName)).AND.(PRESENT(filePath)))THEN
      test_fileNameAndPath=filePath(1:LEN_TRIM(filePath))//fileName(1:LEN_TRIM(fileName))
   ELSE IF(PRESENT(fileNameAndPath))THEN
      test_fileNameAndPath=fileNameAndPath
   END IF
   !    
   ! CHECK FILE
   fileExists=.FALSE.
   INQUIRE(FILE=test_fileNameAndPath,EXIST=fileExists,IOSTAT=err)
   
   IF((.NOT.fileExists).OR.(err/=0))THEN
      myResult=.FALSE.
      CALL message(log_error,"E-Could not find the file "//test_fileNameAndPath(1:LEN_TRIM(test_fileNameAndPath)))
   END IF   

END SUBROUTINE myFileInquire
!___________________________________________________________________________________________________________________
!
SUBROUTINE myFileOpen(unitID,myResult,fileName,filePath,fileNameAndPath,status)
   IMPLICIT NONE
   ! Subroutine
   INTEGER(MIK),INTENT(IN)::unitID
   LOGICAL,INTENT(OUT)::myResult   
   CHARACTER(LEN=*),INTENT(IN),OPTIONAL::fileName,filePath,fileNameAndPath,status
   ! 
   ! Local
   INTEGER(MIK)::err
   CHARACTER(LEN=360)::open_currentPath,open_fileNameAndPath
   CHARACTER(LEN=7)fileStatus
   !---
   !
   open_fileNameAndPath=''
   IF(PRESENT(status))THEN;fileStatus=status;ELSE;fileStatus='UNKNOWN';END IF
   
   ! DEFINE FULL NAME AND PATH STRING
   IF((PRESENT(fileName)).AND.(.NOT.PRESENT(filePath)))THEN
      open_currentPath=findcurrentdir()
      open_fileNameAndPath=open_currentPath(1:LEN_TRIM(open_currentPath))//fileName(1:LEN_TRIM(fileName))
   ELSE IF((PRESENT(fileName)).AND.(PRESENT(filePath)))THEN
      open_fileNameAndPath=filePath(1:LEN_TRIM(filePath))//fileName(1:LEN_TRIM(fileName))
   ELSE IF(PRESENT(fileNameAndPath))THEN
      open_fileNameAndPath=fileNameAndPath
   END IF
   !    
   ! CHECK FILE
   OPEN(UNIT=unitID,FILE=open_fileNameAndPath(1:LEN_TRIM(open_fileNameAndPath)),IOSTAT=err,STATUS=fileStatus)
   
   IF(err==0)THEN
      myResult=.TRUE.
   ELSE   
      myResult=.FALSE.
      CALL message(log_error,"Could not open the file "//open_fileNameAndPath(1:LEN_TRIM(open_fileNameAndPath)))
   END IF   

END SUBROUTINE myFileOpen
!___________________________________________________________________________________________________________________
!
SUBROUTINE myWriteHeader(unitID,myResult,name,inputFiles,standardsFiles,resultsFiles)
   IMPLICIT NONE
   ! Subroutine
   INTEGER(MIK),INTENT(IN)::unitID
   LOGICAL,INTENT(OUT)::myResult     
   CHARACTER(LEN=*),INTENT(IN)::name
   CHARACTER(LEN=*),DIMENSION(:),INTENT(IN)::inputFiles,standardsFiles,resultsFiles
   ! Local
   INTEGER(MIK)::i,err
   !---
   !
   err=0;REWIND(unitID)
   !
   WRITE(unitID,*,err=100)""
   WRITE(unitID,*,err=100)name(1:LEN_TRIM(name))
   WRITE(unitID,*,err=100)""
   
   IF(SIZE(inputFiles))THEN
      DO i=1,SIZE(inputFiles)
         SELECT CASE(i)
            CASE(1); WRITE(unitID,*,err=100)"Input files:          "//inputFiles(i)(1:LEN_TRIM(inputFiles(i)))
            CASE(2:);WRITE(unitID,*,err=100)"                      "//inputFiles(i)(1:LEN_TRIM(inputFiles(i)))
         END SELECT   
      END DO
   END IF
   
   IF(SIZE(standardsFiles))THEN
      WRITE(unitID,*,err=100)""
      DO i=1,SIZE(standardsFiles)
         SELECT CASE(i)
            CASE(1); WRITE(unitID,*,err=100)"Standard files:       "//standardsFiles(i)(1:LEN_TRIM(standardsFiles(i)))
            CASE(2:);WRITE(unitID,*,err=100)"                      "//standardsFiles(i)(1:LEN_TRIM(standardsFiles(i)))
         END SELECT
      END DO
   END IF

   IF(SIZE(resultsFiles))THEN
      WRITE(unitID,*,err=100)""
      DO i=1,SIZE(resultsFiles)
         SELECT CASE(i)
            CASE(1); WRITE(unitID,*,err=100)"Results files created: "//resultsFiles(i)(1:LEN_TRIM(resultsFiles(i)))
            CASE(2:);WRITE(unitID,*,err=100)"                       "//resultsFiles(i)(1:LEN_TRIM(resultsFiles(i)))
         END SELECT      
      END DO
   END IF
   
   WRITE(unitID,*,err=100)""
   WRITE(unitID,*,err=100)"Unit Test perfomed at: "//systemDateTime()
   WRITE(unitID,*,err=100)""

   RETURN
   
100 CONTINUE

   myResult=.FALSE.
   CALL message(log_error,"Error writing header for "//name(1:LEN_TRIM(name)))
 
END SUBROUTINE myWriteHeader
!___________________________________________________________________________________________________________________
!
SUBROUTINE myWriteMessage(unitID,myMessage,blankLine)
   IMPLICIT NONE
   ! Subroutine
   INTEGER(MIK),INTENT(IN)::unitID   
   CHARACTER(LEN=*),INTENT(IN)::myMessage
   LOGICAL,OPTIONAL::blankLine
   !---
   !
   WRITE(unitID,"(a)",err=101)myMessage(1:LEN_TRIM(myMessage))
   IF(PRESENT(blankLine))WRITE(unitID,*,err=101)""
   RETURN
   
101 CONTINUE
   CALL message(log_error,"Error writing message to unit test module summary file")
 
END SUBROUTINE myWriteMessage
!__________________________________________________________________________________________________________________
!
SUBROUTINE myFileCompare(unitOne,unitTwo,skip,myTestResult)
   !
   ! Compare two file
   IMPLICIT NONE
   !
   ! Subroutine variables
   INTEGER(MIK),INTENT(IN),OPTIONAL::unitOne,unitTwo,skip
   LOGICAL,INTENT(OUT)::myTestResult

   !
   ! General variables
   INTEGER(MIK)::i,fileOneLineCount,fileTwoLineCount,endOfFile_i,endOfFile_ii
   CHARACTER(LEN=1)::dummy 
   CHARACTER(LEN=240)::standard,test
   !---
   !
   INQUIRE(UNIT=unitOne,EXIST=myTestResult);IF(.not.myTestResult)RETURN
   INQUIRE(UNIT=unitTwo,EXIST=myTestResult);IF(.not.myTestResult)RETURN
  
   REWIND(UNIT=unitOne);REWIND(UNIT=unitTwo)
   fileOneLineCount=0_MIK;fileTwoLineCount=0_MIK

   DO
      READ(unitOne,*,IOSTAT=endOfFile_i)dummy
      IF(endOfFile_i/=0)THEN;EXIT;ELSE;fileOneLineCount=fileOneLineCount+1;END IF
   END DO

   DO
      READ(unitTwo,*,IOSTAT=endOfFile_ii)dummy
      IF(endOfFile_ii/=0)THEN;EXIT;ELSE;fileTwoLineCount=fileTwoLineCount+1;END IF
   END DO
   
   IF((fileOneLineCount-fileTwoLineCount)/=0)THEN;myTestResult=.FALSE.;RETURN;ELSE;myTestResult=.TRUE.;END IF

   REWIND(UNIT=unitOne);REWIND(UNIT=unitTwo)
   
   IF(PRESENT(skip))THEN
      DO i=1,skip;READ(unitOne,"(a)",IOSTAT=endOfFile_i)standard;READ(unitTwo,"(a)",IOSTAT=endOfFile_ii)test;END DO
   END IF
   
   DO
   standard='';test=''
   READ(unitOne,"(a)",IOSTAT=endOfFile_i)standard;READ(unitTwo,"(a)",IOSTAT=endOfFile_ii)test
   IF((endOfFile_i/=0).and.(endOfFile_ii/=0))THEN;myTestResult=.TRUE.;EXIT;ELSE;myTestResult=.FALSE.;END IF
   IF(test(1:LEN_TRIM(test))/=standard(1:LEN_TRIM(standard)))THEN
   myTestResult=.FALSE.
   EXIT
   END IF
   END DO
   
   REWIND(UNIT=unitOne);REWIND(UNIT=unitTwo)
   
END SUBROUTINE myFileCompare
!__________________________________________________________________________________________________________________
!
SUBROUTINE testMyResult_Integer(testValue,value_true,value_false,myTestResult)
   IMPLICIT NONE
   ! Subroutine
   INTEGER(MIK),INTENT(IN)::testValue
   INTEGER(MIK),INTENT(IN),OPTIONAL::value_true,value_false
   LOGICAL,INTENT(OUT)::myTestResult
   !---
   !
   IF(PRESENT(value_true))THEN
     IF(testValue==value_true)THEN;myTestResult=.TRUE.;ELSE;myTestResult=.FALSE.;END IF
   ELSE IF(PRESENT(value_false))THEN 
     IF(testValue==value_false)THEN;myTestResult=.FALSE.;ELSE;myTestResult=.TRUE.;END IF   
   END IF

END SUBROUTINE testMyResult_Integer
!__________________________________________________________________________________________________________________
!
SUBROUTINE testMyResult_Logical(testValue,value_true,value_false,myTestResult)
   IMPLICIT NONE
   ! Subroutine
   LOGICAL,INTENT(IN)::testValue
   LOGICAL,INTENT(IN),OPTIONAL::value_true,value_false
   LOGICAL,INTENT(OUT)::myTestResult
   !---
   !
   IF(PRESENT(value_true))THEN
     IF(testValue==value_true)THEN;myTestResult=.TRUE.;ELSE;myTestResult=.FALSE.;END IF
   ELSE IF(PRESENT(value_false))THEN 
     IF(testValue==value_false)THEN;myTestResult=.FALSE.;ELSE;myTestResult=.TRUE.;END IF  
   ELSE IF(testValue)THEN
      myTestResult=.TRUE.
   ELSE
      myTestResult=.FALSE.
   END IF

END SUBROUTINE testMyResult_Logical
!___________________________________________________________________________________________________________________
!
SUBROUTINE myWriteTestResult(testName,testResult,failMessage,unitID)
   IMPLICIT NONE
   ! Subroutine
   INTEGER(MIK),INTENT(IN)::unitID
   LOGICAL,INTENT(IN)::testResult
   CHARACTER(LEN=*),INTENT(IN)::testName
   CHARACTER(LEN=*),INTENT(IN)::failMessage
   ! Local
   CHARACTER(LEN=360)::outputString   
   !---
   !
   outputString=""

   SELECT CASE(testResult)
       CASE(.TRUE.) ; WRITE(outputString,*,err=102)testName(1:LEN_TRIM(testName))
                      WRITE(outputString(80:84),"(a)",err=102)"PASS"
                      WRITE(unitID,'(a)',err=102)outputString(1:LEN_TRIM(outputString))
       
       CASE(.FALSE.); WRITE(outputString,*,err=102)testName(1:LEN_TRIM(testName))
                      WRITE(outputString(80:84),"(a)",err=102)"FAIL"
                      WRITE(unitID,'(a)',err=102)outputString(1:LEN_TRIM(outputString))
                      WRITE(unitID,'(a)',err=102)"                     "//failMessage(1:LEN_TRIM(failMessage))    
   END SELECT

   RETURN
   
102 CONTINUE

   CALL message(log_error,"Error writing test results for "//testName(1:LEN_TRIM(testName)))
 
 END SUBROUTINE myWriteTestResult
!___________________________________________________________________________________________________________________
!
SUBROUTINE myGlobalTestPlatformLog(action)
   IMPLICIT NONE
   ! Subroutine
   CHARACTER(LEN=*),INTENT(IN)::action
   ! Local
   LOGICAL::fileOpen
   !---
   !
   SELECT CASE(action(1:LEN_TRIM(action)))
      CASE("open","OPEN")
         CALL init_log(close=.false.)
      CASE("close","CLOSE")
         CALL close_log()
   END SELECT
 
END SUBROUTINE myGlobalTestPlatformLog
!___________________________________________________________________________________________________________________
!

END MODULE MUtilsLib_unitTestPlatform

