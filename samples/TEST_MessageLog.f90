  subroutine sample_subroutine
    ! Example to test messageslog
    
    
    use MUtilsLib_messagelog

    Implicit none
    ! Locals
    integer(4) err
    character(len= msg_len + tag_len), dimension(:),pointer :: messages
    character(len= msg_len + tag_len):: lastmessage
    character(len=100) msg
    integer :: i
    
    call init_log(db_id="Testdb",msgdb_file="..\..\samples\TestMsg_db.csv") ! Initialise the log
    call init_log(db_id="Testdb2",msgdb_file="..\..\samples\TestMsg_db2.csv") ! Initialise the log
    
    ! Generate some error messages
    call message("TEST MESSAGE: Default messages are errors")
    call message(log_warn,"TEST MESSAGE:  This is a test-warning message")
    
    ! Generate some error messages using msg_db
    do i=1,3
       call message(log_error,"Test for the message db",msg_id=i,db_id="Testdb")
    end do
    call message(log_error,"Test for the message db with wrong msg_id ",msg_id=4,db_id="Testdb")
    call message(log_error,"This is a test-error message for the message db with wrong db_id",msg_id=4,db_id="Testdb_wrong")

    do i=1,3
       call message(log_error,"Test for the message db",msg_id=i,db_id="Testdb2")
    end do
    
    ! Retrieve Messages - only last
    call get_messages(lastmessage=lastmessage)
    ! Retrieve Messages - all
    call get_messages(allmessages=messages)
    
    do i=1,3
       call message(log_error,"Test for the message db",msg_id=i,db_id="Testdb2")
    end do
    
    ! Retrieve Messages - all (again)
    call get_messages(allmessages=messages)
    
    pause
        
  end subroutine sample_subroutine
