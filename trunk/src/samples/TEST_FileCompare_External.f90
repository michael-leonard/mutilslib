  subroutine sample_subroutine
    ! Example to test messageslog
    
    
    use MUtilsLib_FileIO
    use MUtilsLib_MessageLog 
    use Mutilslib_System, only : fileCompare_External
    use kinds_dmsl_kit
       

    Implicit none
    ! Locals
    integer(mik) :: ok
    character(len= len_longStr) :: fileOne,fileTwo
        
    call init_log() ! Initialise the log
    
    ! Compare two files using external - this should work
    fileOne="message_orig.log"
    fileTwo="message_change.log"
    
    ok=fileCompare_External(fileOne=fileOne,fileTwo=fileTwo)
    if (ok/=0) then
        call message("Problem with fileCompare_External that should work")
     else
        call message(log_debug,"Woo Hoo! fileCompare_External worked")
    end if    
    
    ! Compare two files using external - this should not work
    fileOne="message_orig2.log"
    fileTwo="message_change.log"
    
    ok=fileCompare_External(fileOne=fileOne,fileTwo=fileTwo)
    if (ok/=0) then
        call message(log_debug,"An error should occur with this call of fileCompare_External")
    end if
       
    pause
        
  end subroutine sample_subroutine
