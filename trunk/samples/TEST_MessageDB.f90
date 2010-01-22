  subroutine sample_subroutine
    ! Example to test messageslog
    
    
    use MUtilsLib_messagelog

    Implicit none
    ! Locals
    integer(4) err
    character(len= msg_len + tag_len), dimension(:),allocatable :: messages
    character(len= msg_len + tag_len):: lastmessage
    character(len=100) msg
    integer :: i
    
    call init_log(db_id="Testdb",msg_file="..\..\samples\TestMsg_db.csv") ! Initialise the log
    call init_log(db_id="Testdb2",msg_file="..\..\samples\TestMsg_db2.csv") ! Initialise the log
    
    ! Generate some error messages
    call message("TEST MESSAGE: Default messages are errors")
    call message($warn,"TEST MESSAGE:  This is a test-warning message")
    do i=1,3
       call message($error,"Test for the message db",msg_id=i,db_id="Testdb")
    end do
    call message($error,"Test for the message db with wrong msg_id ",msg_id=4,db_id="Testdb")
    call message($error,"This is a test-error message for the message db with wrong db_id",msg_id=4,db_id="Testdb_wrong")

    do i=1,3
       call message($error,"Test for the message db",msg_id=i,db_id="Testdb2")
    end do
    
    pause
        
  end subroutine sample_subroutine
