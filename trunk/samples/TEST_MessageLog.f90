  subroutine sample_subroutine
    ! Example to test messageslog
    
    
    use MUtilsLib_messagelog

    Implicit none
    ! Locals
    integer(4) err
    character(len= msg_len + tag_len), dimension(:),allocatable :: messages
    character(len= msg_len + tag_len):: lastmessage
    character(len=100) msg
    
    call init_log() ! Initialise the log
    
    ! Generate some error messages
    call message("TEST MESSAGE: Default messages are errors")
    call message(log_warn,"TEST MESSAGE:  This is a test-warning message")
    call message(log_error,"TEST MESSAGE: This is a test-error message ")
    call message(log_blank,"TEST MESSAGE: This is a non-tagged message ")
    call message(log_comment,"TEST MESSAGE: This is a generic test message ")
    ! Retrieve Messages - only last
    call get_messages(lastmessage=lastmessage)
    ! Retrieve Messages - all
    call get_messages(allmessages=messages)
    
    ! Generate more error messages
    call message(log_comment,"TEST MESSAGE: This is the last comment message")
    
    ! Retrieve Messages - All again
    call get_messages(allmessages=messages)
    
    call message(log_blank, "Press any key to finish program")
    read(*,*)

  end subroutine sample_subroutine
