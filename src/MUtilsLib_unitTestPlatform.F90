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
   LOGICAL::ok=.false.
   LOGICAL::result_l=.false.
   INTEGER(MIK)::result_i=undefIN
   CHARACTER(LEN=75)::name=undefCH
   CHARACTER(LEN=180)::metaDataTag=undefCH
   CHARACTER(LEN=360)::message=undefCH
   CHARACTER(LEN=4)::resultString=undefCH
   
END TYPE unitTestResultsData

PUBLIC::myFileInquire,myFileOpen,myWriteHeader,testMyResult,myWriteTestResult,myFileCompare
!---
!
INTERFACE testMyResult
MODULE PROCEDURE testMyResult_Integer,testMyResult_Logical,testMyResult_Real,&
                 testMyResult_Integer_NewAndImproved,testMyResult_Logical_NewAndImproved
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
!> Compares files and evalutes whether they are the same
!! Comparison can be undertaken at various levels of detail, see compareLevel arg
SUBROUTINE myFileCompare(unitOne,unitTwo,fileOne,fileTwo,CompareLevel,skip,myTestResult)
   use mUtilsLib_System, only : fileCompare_External
   use mUtilsLib_fileio, only : fileExist,findEOF
   use mUtilsLib_StringFuncs, only: operator(//)
   use MUtilsLib_VarFuncs, only : CheckPresent
   
   ! Compare two files
   IMPLICIT NONE
   ! Subroutine variables
   INTEGER(MIK),INTENT(IN),OPTIONAL::unitOne,UnitTwo !> Units for files - now deprecated
   character(len=*), optional :: fileOne, fileTwo    !> Full path names for files to be compared
   integer(mik), intent(in),optional :: CompareLevel !> Provides different levels of comparison
                                                     !! 1 = LineCount
                                                     !! 2 = 1 + Line by Line Comparison
                                                     !! 3 = 2 + External File Compare(e.g. using tortoisemerge)
   INTEGER(MIK),INTENT(IN),OPTIONAL:: skip           !> Number of lines to skip if doing line by line comparison
   LOGICAL,INTENT(OUT)::myTestResult                 !> Result of file comparison   

      ! General variables
   INTEGER(MIK)::i,fileOneLineCount,fileTwoLineCount,endOfFile_i,endOfFile_ii,unitOneLc,unitTwoLc,ok
   CHARACTER(LEN=1)::dummy 
   CHARACTER(LEN=len_vLongStr)::textone,texttwo
   character(len=len_vLongStr) :: msg
   integer(mik) :: CompareLevelLc
   !---
   !
   CompareLevelLc=checkPresent(Comparelevel,2) 
   if (present(fileOne) .and. present(fileTwo)) then
    
     ! Check if files exist
     if(.not.fileExist(fileToOpen=fileOne,msg=msg)) then; call message(trim(msg)//" in F: myFileCompare"); myTestResult=.false.; return; end if
     if(.not.fileExist(fileToOpen=fileTwo,msg=msg)) then; call message(trim(msg)//" in F: myFileCompare"); myTestResult=.false.; return; end if
     
     ! Undertake line count
     fileOneLineCount=findEof(filepath=fileOne,err=ok,msg=msg)
     if (ok/=0) then; call message(trim(msg)//" in F: myFileCompare"); myTestResult=.false.; return; end if
     fileTwoLineCount=findEof(filepath=fileTwo,err=ok,msg=msg)
     if (ok/=0) then; call message(trim(msg)//" in F: myFileCompare"); myTestResult=.false.; return; end if
          
     ! Open files to unitOne and UnitTwo
     unitOneLc=98765
     unitTwoLc=45678
     call myFileOpen(unitID=unitOneLc,fileNameandPath=fileOne,myResult=myTestResult)
     if(.not.myTestResult) then
        call message("Unable to open file:"//trim(fileOne)//" in F: myFileCompare"); return
     end if
     call myFileOpen(unitID=unitTwoLc,fileNameandPath=fileTwo,myResult=myTestResult)
     if(.not.myTestResult) then
        call message("Unable to open file:"//trim(fileTwo)//" in F: myFileCompare"); return
     end if
     
     
  else if (present(unitOne) .and. present(unitTwo)) then
     ! Deprecated code section
     call message(log_warn,"UnitOne and UnitTwo are deprecated arguments in F:myFileCompare, &
                            please use FileOne and FileTwo instead and will be deleted by Dec. 2010")
     INQUIRE(UNIT=unitOne,EXIST=myTestResult);
     IF(.not.myTestResult) then; call message("Unable to find unit "//unitOne//" in myFileCompare"); myTestresult=.false.; return; endif
     INQUIRE(UNIT=unitTwo,EXIST=myTestResult)
     IF(.not.myTestResult) then; call message("Unable to find unit "//unitTwo//" in myFileCompare"); myTestresult=.false. ;return; endif
     unitOneLc=UnitOne
     unitTwoLc=UnitTwo
     
     REWIND(UNIT=unitOneLc);REWIND(UNIT=unitTwolc)
     fileOneLineCount=0_MIK;fileTwoLineCount=0_MIK

     DO
      READ(unitOneLc,*,IOSTAT=endOfFile_i)dummy
      IF(endOfFile_i/=0)THEN;EXIT;ELSE;fileOneLineCount=fileOneLineCount+1;END IF
     END DO

     DO
      READ(unitTwolc,*,IOSTAT=endOfFile_ii)dummy
      IF(endOfFile_ii/=0)THEN;EXIT;ELSE;fileTwoLineCount=fileTwoLineCount+1;END IF
     END DO
  else
     call message("Incorrect combination of optional arguments: unitOne, UnitTwo, FileOne, FileTwo in myFileCompare") 
     myTestResult=.false.
     return
  end if
    
  ! Compare file count
  if (ComparelevelLc>0) then
   REWIND(UNIT=unitOneLc);REWIND(UNIT=unitTwolc)
   IF((fileOneLineCount-fileTwoLineCount)/=0)THEN;
    myTestResult=.FALSE.
    CLOSE(UNIT=unitOneLc);CLOSE(UNIT=unitTwolc)
    if(compareLevelLc>2) call call_FileCompare_External()
    RETURN
   ELSE; myTestResult=.TRUE.;END IF
  end if 

  
  ! Compare line-by-line
  if (CompareLevelLc>1) then 
   REWIND(UNIT=unitOneLc);REWIND(UNIT=unitTwolc)
   IF(PRESENT(skip))THEN
      DO i=1,skip;READ(unitOneLc,"(a)",IOSTAT=endOfFile_i) textone;READ(unitTwolc,"(a)",IOSTAT=endOfFile_ii)texttwo;END DO
   END IF
   
   DO
      textone='';texttwo=''
      READ(unitOneLc,"(a)",IOSTAT=endOfFile_i)textone;READ(unitTwolc,"(a)",IOSTAT=endOfFile_ii)texttwo
      IF((endOfFile_i==(-1)).and.(endOfFile_ii==(-1)))THEN;
       myTestResult=.TRUE.;EXIT;
      ELSE IF ((endOfFile_i==0) .and. (endOfFile_ii==0)) then
       IF(textone(1:LEN_TRIM(texttwo))/=texttwo(1:LEN_TRIM(texttwo)))THEN
        myTestResult=.FALSE.
        CLOSE(UNIT=unitOneLc);CLOSE(UNIT=unitTwolc)
        if(compareLevelLc>2) call call_FileCompare_External()
        return    
       end if
      END IF
   END DO
  end if
   
   
  CLOSE(UNIT=unitOneLc);CLOSE(UNIT=unitTwoLc)
   
contains 

 subroutine Call_FileCompare_External
     
    if (present(fileOne) .and. present(fileTwo)) then 
       close(unitOneLc,iostat=ok)
       close(unitTwoLc,iostat=ok)
       ok=FileCompare_External(fileOne=fileOne,fileTwo=FileTwo)
       if (ok/=0) call message("Unable to compare files using external viewer:"//trim(fileone)//" and"//trim(filetwo)//" in f:myFileCompare")
       return
    end if
 
end subroutine Call_FileCompare_External
   
END SUBROUTINE myFileCompare
!__________________________________________________________________________________________________________________
!
SUBROUTINE testMyResult_Real(testValue,trueValue,tolerance,myTestResult,resultString)
   IMPLICIT NONE
   ! Subroutine
   REAL(MRK),INTENT(IN)::testValue,trueValue,tolerance
   LOGICAL,INTENT(OUT)::myTestResult
   CHARACTER(LEN=*),OPTIONAL::resultString
   !---
   !
   IF(ABS(testValue)-ABS(trueValue)<=tolerance)THEN
      myTestResult=.TRUE.
      IF(PRESENT(resultString))resultString="PASS"
   ELSE
      myTestResult=.FALSE.
      IF(PRESENT(resultString))resultString="FAIL"
   END IF

END SUBROUTINE testMyResult_Real
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
SUBROUTINE testMyResult_Integer_NewAndImproved(test,value_true,value_false,outputUnit)
   IMPLICIT NONE
   ! Subroutine
   TYPE(unitTestResultsData),INTENT(INOUT)::test
   INTEGER(MIK),INTENT(IN),OPTIONAL::value_true,value_false,outputUnit
   !---
   !
   IF(PRESENT(value_true))THEN
     IF(test%result_i==value_true)THEN;test%ok=.TRUE.;ELSE;test%ok=.FALSE.;END IF
   ELSE IF(PRESENT(value_false))THEN 
     IF(test%result_i==value_false)THEN;test%ok=.FALSE.;ELSE;test%ok=.TRUE.;END IF   
   END IF

   IF(PRESENT(outputUnit))CALL myWriteTestResult(testName=test%name,testResult=test%ok,failMessage=test%message,unitID=outputUnit)

END SUBROUTINE testMyResult_Integer_NewAndImproved
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
!__________________________________________________________________________________________________________________
!
SUBROUTINE testMyResult_Logical_NewAndImproved(test,value_true,value_false,outputUnit)
   IMPLICIT NONE
   ! Subroutine
   TYPE(unitTestResultsData),INTENT(INOUT)::test
   LOGICAL,INTENT(IN),OPTIONAL::value_true,value_false
   INTEGER(MIK),INTENT(IN),OPTIONAL::outputUnit
   !---
   !
   IF(PRESENT(value_true))THEN
     IF(test%result_l==value_true)THEN;test%ok=.TRUE.;ELSE;test%ok=.FALSE.;END IF
   ELSE IF(PRESENT(value_false))THEN 
     IF(test%result_l==value_false)THEN;test%ok=.FALSE.;ELSE;test%ok=.TRUE.;END IF  
   ELSE IF(test%result_l)THEN
      test%ok=.TRUE.
   ELSE
      test%ok=.FALSE.
   END IF

   IF(PRESENT(outputUnit))CALL myWriteTestResult(testName=test%name,testResult=test%ok,failMessage=test%message,unitID=outputUnit)

END SUBROUTINE testMyResult_Logical_NewAndImproved
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

