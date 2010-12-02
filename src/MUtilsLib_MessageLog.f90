!<license>

!> message_log - an error/message logging interface
!> description:  logs (error/warning/other) messages during runtime and can flush the msg_log to the screen, to a file or both
!
! List of Routines
! ----------------
! Below provides a list of routines used in this module. The user is recommended to inspect the actual
! interfaces of each routine in order to determine how to use

!  INIT_LOG(unit,close,append,active,echo,file,auto_flush) ! set custom parameters for the msg_log file (otherwise use default)
!  message(message,msg_id,db_id)    ! add message to the msg_log object, defaults to ERROR tag,
!                                                   select tag from predefined list using tagtype,
!  message(msg_type,message,msg,db_id)  ! add tag and message to the msg_log object,
!                                                   select tag from predefined list using tagtype
!           see 'Message Database Functionality' below for info on msg_id and db_id
!  flush_messages()           ! write the entire msg_log to file (unit =6, specifies screen)
!  get_messages()             ! reads the msg_log file and returns last message or entire msg_log
!  WARNING()                  ! logical function to check whether any warnings have occurred
!  ERROR()                    ! logical function to check whether any errors have occurred

!> Notes:
!> (1) This module does not support multiple logs - only one per program.
!>     That is, only one instance of this object is declared as a "global" msg_log in this module.
!> (2) A subroutine that calls one of the routines in this module cannot also be pure or elemental. This is because
!>     the msg_log object reads and writes to screen/file.
!> (3) This method of logging errors and warnings was selected so that the interface for each function
!>     did not get polluted. Also, it is quite easy to remove the references to the msg_log object at a later
!>     date, whereas it is messy to change the interface of a routine at a later date.
!> (4) A tag must be specified for every message that gets entered into the msg_log. Tags can be chosen from a
!>     predefined set for which a global enum is provided.
!> (5) msg_log is kept private, so as to avoid incorrect use and so that logging facilities can be easily removed
!>     from a project
!> (6) The msg_log class has several different options for outputting the data and for tracking errors and
!>     warnings. The warning() and error() routines can be used to provide error handling and check
!>     if a subroutine executed okay.
!>
!> Message Database Functionality
!>  - Users can supply a msg_id (and db_id) to the message subroutine and that is used to locate message
!>    in msg_db (given by db_id) which is appended to the message
!>
!> Notes:
!> (1) msg_db is read-in using init_msg_db which is called from init_log - need to supply db_id and msgdb_file to init_log
!> (2) msgdb_file needs to be in csv format, where first line is header and there are 4 columns (integer and 3 character), as follows
!>       ID,short description,long description,remedy
!>       1,"short db message id 1","long db message id 1","remedy id 1"
!>       2,"short db message id 2","long db message id 2","remedy id 2"
!>       -3,"short db message id -3","long db message id -3","remedy id -3"
!>    see samples\TestMsg_db.csv for more details
!>    Tips: A simple trick to get Excel to put quotes around text is to put a comma in the cell
!>
!>  (3) Multiplie message data-bases are supported, just need to call init_log again (with append=.true.)
!>      to ensure messages are added to the same log file
!>
module MUtilsLib_messagelog
  use kinds_dmsl_kit
  implicit none
  save

  private ! All components are private unless declared otherwise
  integer, parameter, public   ::  tag_len  = len_stdStrB   !< tag length
  integer, parameter, public   ::  msg_len = len_vLongStr   !< some messages can contain strings of deep file directories
  integer, parameter, public   ::  msg_tag_len = msg_len+tag_len
  character(len=1)  :: comchar = " "    !< a comment character

  type obj_msg_log
    private
    character(len = tag_len), pointer, dimension(:) ::  tag  => null()    !< A tag to specify the type of message
    character(len = msg_len), pointer, dimension(:) ::  message => null() !< Any descriptive text
    integer  ::   err  = 0           !< Cumulative count of system errors
    integer  ::   warn = 0           !< Cumulative count of system warnings
    integer  ::   unit = 151         !< file ID unit, screen = 6, file = other
    logical  ::   close  = .true.    !< determines if the msg_log file is to be opened and closed for each flush
    logical  ::   append = .false.   !< Whether a new msg_log should be appended on to existing msg_log
    logical  ::   active = .true.    !< Allows msg_log to be activated / deactivated
    logical  ::   echo   = .true.    !< Whether msg_log should be written to screen and to file
    logical  ::   ignore_warn  = .false.           !< Whether warnings should be ignored
    logical  ::   ignore_error = .false.           !< Whether errors should be ignored
    logical  ::   auto_flush   = .true.            !< Automatically flush each entry after it is made
    logical  ::   debug   = .true.                 !< Should debug messages be written to the msg_log
    logical  ::   time_stamp   = .false.           !< Prefix the message with a timestamp
    character(len = len_vLongStr)  ::   file = 'message.log' ! file name if msg_log is written to file
  end type obj_msg_log


  !> Enumeration for different tag types (public)
  integer, parameter, public  ::   log_error   = 1, &
                                   log_warn    = 2, &
                                   log_write   = 4, &
                                   log_calc    = 5, &
                                   log_name    = 6, &
                                   log_path    = 7, &
                                   log_title   = 8, &
                                   log_comment = 9, &
                                   log_debug   = 10, &
                                   log_blank   = 11, &
                                   log_fatal   = 12


  character(len = tag_len), parameter :: tag(1:12) = (/"*ERROR    :", &
                                                       " Warning  :", &
                                                       " Read     :", &
                                                       " Write    :", &
                                                       " Calculate:", &
                                                       " Filename :", &
                                                       " Filepath :", &
                                                       " Title    :", &
                                                       " Comment  :", &
                                                       " Debug    :", &
                                                       "           ", &
                                                       "FatalError:" /)

  !> Type for messages in message database
  type msg_db_msg_type                                 
    integer(mik):: id                                   !< id for messages in message database
    character(len=len_LongStr) :: desc                  !< message description
    character(len=len_vLongStr) :: remedy               !< possible remedies if message is for an error
  end type msg_db_msg_type

  !> Type for message database
  type msg_db_type                                      
    character(len=len_stdStrD) :: id                    !< id message database
    integer(mik) ::n_msg                                !< number of message in message database
    type(msg_db_msg_type),allocatable :: msg(:)         !< message in message database
  end type

  type (msg_db_type), allocatable :: msg_db(:)          !< Message database(s)

  type(obj_msg_log)  :: msg_log                         !< a private system message log

  ! public interface
  public :: init_log, message, flush_messages, warning, error,get_messages,close_log

  interface message
    module procedure add_log_no_tag
    module procedure add_log_msg_tag
  end interface message
!************************************************************************************************
  contains
!************************************************************************************************
    !> Set the parameters for the log file. Not necessary to be called if you are happy with defaults.
    subroutine init_log(unit,close,append,active,echo,file, ignore_warn, ignore_error,auto_flush,debug,db_id,msgdb_file,append_always,time_stamp)
      
      implicit none
      integer, intent(IN), optional  ::   unit          !< file ID unit, screen = 6, file = other
      logical, intent(IN), optional  ::   close         !< determines if the log file is to be opened and closed for each flush
      logical, intent(IN), optional  ::   append        !< Whether log should be appended on to existing log
      logical, intent(IN), optional  ::   append_always !< Whether all log messages in program should be appended to the log_file that is to be opened
      logical, intent(IN), optional  ::   active        !< Allows log to be activated / deactivated
      logical, intent(IN), optional  ::   echo          !< Whether log should be written to screen and to file
      logical, intent(IN), optional  ::   ignore_warn   !< Whether warnings should be ignored
      logical, intent(IN), optional  ::   ignore_error  !< Whether errors should be ignored
      logical, intent(IN), optional  ::   auto_flush    !< Whether the log should be automatically flushed each time
      logical, intent(IN), optional  ::   debug         !< Whether debug comments should be ignored
      logical, intent(IN), optional  ::   time_stamp    !< Whether to prefix the message with a timestamp

      character(len = *), intent(IN), optional :: file  !< file name if log is written to file
      character(len = *), intent(IN), optional :: db_id !< ID of the message database
      character(len = *), intent(IN), optional :: msgdb_file !< file name of the msg file for message database
      integer :: ok

      ! Transfer input variables into log object
      if (present(unit))   msg_log%unit   = unit
      if (present(active)) msg_log%active = active
      if (present(append)) msg_log%append = append
      if (present(file) .and. .NOT. msg_log%append)   msg_log%file   = trim(file) ! If msg_log is being appended then do not overwrite file ** quick-fix-needs to better thought through
      if (present(close))  msg_log%close  = close
      if (present(echo))   msg_log%echo   = echo
      if (present(ignore_warn))  msg_log%ignore_warn    = ignore_warn
      if (present(ignore_error)) msg_log%ignore_error   = ignore_error
      if (present(auto_flush))   msg_log%auto_flush     = auto_flush
      if (present(debug))   msg_log%debug     = debug


      ! Check for consistency/logic of log parameters
      if (msg_log%echo .AND. msg_log%unit == 6)   msg_log%unit   = 111     ! When echo-ing must have file ID other than 6
      if (msg_log%append .AND. msg_log%unit == 6) msg_log%append = .false. ! file appending is not needed
                                                                           ! when  writing to screen only

      ! if necessary open the log file
      if (msg_log%active) then
        if (msg_log%unit /=6) then
          if (msg_log%append) then ! keep the existing msg_log file (if there is one)
            open(unit = msg_log%unit, file = msg_log%file, status = 'unknown', position = 'append')
          else ! A new msg_log file
            open(unit = msg_log%unit, file = msg_log%file, status = 'replace')
            call message(log_debug,"All messages being written to log file (if no path &
                                   &- written to current executable directory): "//trim(msg_log%file))
          end if
          if (msg_log%close) close(msg_log%unit) ! close the log file
        end if
      end if

      if (present(db_id) .and. present(msgdb_file)) then
        ok=init_msg_db(db_id=db_id,msgdb_file=msgdb_file)
        if (ok/=0) call message(log_error,"Unable to initalise msg_db for "//db_id//" with file "//msgdb_file)
        call message(log_debug,"Using msg_db: "//trim(db_id)//", located at "//trim(msgdb_file))
      end if

      ! Do this upon exit so that if it is called again it it will append to the one that is already opn
      if (present(append_always)) msg_log%append = append_always


    end subroutine init_log
!************************************************************************************************
    !> A wrapper to simplify recording error messages -default tag is log_error
    subroutine add_log_no_tag(message,msg_id,db_id)
      
      implicit none
      character(len = *), intent(IN) ::  message  !< Any descriptive message
      character(len = *), intent(IN),optional ::  db_id
      integer(mik),intent(in), optional :: msg_id

      call add_log_msg_tag(log_error,message,msg_id,db_id)

    end subroutine
!************************************************************************************************
   !> Adds a single message to the msg_log object
   !> All arguments non-optional
    subroutine add_log_msg_tag(msg_type,message,msg_id,db_id)

      use MUtilsLib_StringFuncs, only : insertString,operator(//)
      implicit none
      integer, intent(IN)  ::  msg_type                 !< Index of type of tag
      character(len = *), intent(IN) ::  message        !< Any descriptive message
      character(len = *), intent(IN),optional ::  db_id !< id of the message db
      integer(mik),intent(in), optional :: msg_id       !< message id
      character(8) :: dt
      character(10) :: tm

      character(len=len_vLongStr):: messageLc

      character(len = tag_len), pointer, dimension(:) ::  Tcopy =>null() ! Temporary storage
      character(len = msg_len), pointer, dimension(:) ::  Ncopy =>null() ! Temporary storage
      integer  ::   s ! size of message array

      if (present(db_id) .and. present(msg_id)) then
        messageLc="["//db_id//"] ID:"//msg_id//" "//trim(message)//trim(get_msg_from_db(msg_id,db_id))
      else if (present(db_id)) then
        messageLc="["//db_id//"] "//message
      else 
        messageLc=message
      end if
      !messageLc = insertString(messageLc)
      s = 0
      if(associated(msg_log%message)) s = size(msg_log%message)
      if(s/=0) then
        if(msg_log%message(s)==messageLc.AND.tag(msg_type)==msg_log%tag(s)) then
          ! Avoid multiple identical concurrent entries
          ! I.e. The most recent entry was identical to the one being added, so there is no need to add it in
          ! It is however possible to have multiple entries ... provided they are not concurrent
          ! This feature was added because of issues generating errors in the middle of a do-loop
          return;
        end if
      end if

      if (msg_log%active) then
        select case(msg_type)
          case(log_error)
            msg_log%err = msg_log%err + 1   ! increment the error count
          case(log_warn)
            msg_log%warn = msg_log%warn + 1 ! increment the warning count
          case(log_debug)
            if(.NOT.msg_log%debug) return ! ignore any debug comments made to the msg_log
        end select

        if (associated(msg_log%message)) then
          ! This algorithm is the quickest method for resizing the msg_log to have an extra entry
          s = size(msg_log%message)
          Tcopy => msg_log%tag
          Ncopy => msg_log%message
          nullify(msg_log%tag)
          nullify(msg_log%message)
          allocate(msg_log%tag(s+1))
          allocate(msg_log%message(s+1))
          msg_log%tag(1:s) = Tcopy(1:s)
          msg_log%message(1:s) = Ncopy(1:s)
          msg_log%tag(s+1) = tag(msg_type)   ! append the new tag onto the end
          msg_log%message(s+1) = messageLc ! append the new message onto the end
          deallocate(Tcopy); nullify(Tcopy)
          deallocate(Ncopy); nullify(Ncopy)
        else
          ! add a single message into the msg_log
          allocate(msg_log%message(1))
          allocate(msg_log%tag(1))
          msg_log%message(1) = messageLc   ! insert the message
          msg_log%tag(1) = tag(msg_type)  ! insert the tag
        end if
      end if

      if(msg_type==log_fatal) then
        call add_log_no_tag("Fatal error. Please check "//TRIM(msg_log%file)// &
                        ". Program not terminated, though results may be abnormal.")
        call flush_messages()
      end if

      ! flush the msg_log straight away if necessary
      if(msg_log%auto_flush) call flush_messages()

    end subroutine add_log_msg_tag
!************************************************************************************************
    !> writes out the msg_log object
    !> and deletes all entries
    subroutine flush_messages()
      implicit none
       ! Locals
      integer :: i ! loop counter

      ! Clear out the msg_log object if necessary
      if (associated(msg_log%message)) then
        ! write only if msg_log is activated
        if (msg_log%active) then
          if (msg_log%unit ==6) then ! write to screen only
            ! write all msg_log entries to screen
            do i = 1, size(msg_log%message)
              if (msg_log%ignore_warn .AND. msg_log%tag(i) == tag(log_warn)) cycle ! ignore warning messages
              if (msg_log%ignore_error .AND. msg_log%tag(i) == tag(log_error)) cycle ! ignore error messages
                if(msg_log%tag(i)==tag(log_blank)) then
                    write(*,'(A)') trim(comchar) // trim(msg_log%tag(i)) // trim(msg_log%message(i))
                else
                    write(*,'(A)') trim(comchar) // trim(msg_log%tag(i)) //" "// trim(msg_log%message(i))
                end if
            end do
          else ! a file write is needed
            ! open the file if not already open
            if (msg_log%close) open(unit = msg_log%unit, file = msg_log%file, status = 'old', position = 'append')

              ! write all msg_log entries to file
              do i = 1, size(msg_log%message)
                if (msg_log%ignore_warn .AND. msg_log%tag(i) == tag(log_warn)) cycle ! ignore warning messages
                if (msg_log%ignore_error .AND. msg_log%tag(i) == tag(log_error)) cycle ! ignore error messages
                if(msg_log%tag(i)==tag(log_blank)) then
                    write(msg_log%unit,'(A)') trim(comchar) // trim(msg_log%tag(i)) // trim(msg_log%message(i))
                else
                    write(msg_log%unit,'(A)') trim(comchar) // trim(msg_log%tag(i)) //" "// trim(msg_log%message(i))
                end if
              end do

              ! the message needs to be echoed to screen
              if (msg_log%echo) then
                do i = 1, size(msg_log%message)
                  if (msg_log%ignore_warn .AND. msg_log%tag(i) == tag(log_warn)) cycle ! ignore warning messages
                  if (msg_log%ignore_error .AND. msg_log%tag(i) == tag(log_error)) cycle ! ignore error messages
                  if(msg_log%tag(i)==tag(log_blank)) then
                    write(*,'(A)') trim(comchar) // trim(msg_log%tag(i)) // trim(msg_log%message(i))
                  else
                    write(*,'(A)') trim(comchar) // trim(msg_log%tag(i)) //" "// trim(msg_log%message(i))
                  end if
                end do
              end if

            ! close the file again if specified
            !if (msg_log%close) open(unit = msg_log%unit, file = msg_log%file, &
            !  status = 'old', position = 'append') ML's old line
            if (msg_log%close) close(unit=msg_log%unit) ! MT's new line

          end if

        ! Flush the msg_log entries
          deallocate(msg_log%message); nullify(msg_log%message)
          deallocate(msg_log%tag);  nullify(msg_log%tag)

        ! Reset the error and warning counters
          msg_log%warn = 0; msg_log%err = 0
        end if
      end if

    end subroutine flush_messages
!************************************************************************************************
    !> reads the msg_log file and returns the lastmessage or allmessages depending which is present
    subroutine get_messages(allmessages,lastmessage)

      use MUtilsLib_fileIO, only : findEOF
      implicit none
      ! Outputs
      character(len = *), pointer, dimension(:),optional ::  allmessages ! Descriptive text
                                                                                                   ! of tags and messages
      character(len = *), intent(out),optional ::  lastmessage ! Descriptive text of tags and messages

      integer :: err
      character(len=len_vLongStr) :: msg
      ! Locals
      integer :: i,nMess,test_len ! loop counter
      character(len = msg_len+tag_len) ::  dummy ! dummy text
      character(len=len_vLongStr) :: fmt
      character(len=len_vLongStr) :: testLenCh

      ! Initialisation
      call flush_messages ! Ensures all messages are sent to msg_log file before retrieval
      test_len=(msg_len+tag_len)
      write(testLenCh,*) test_len
      fmt = "(A"//trim(adjustl(testLenCh))//")"
      if (present(lastmessage)) then ! If looking for last-only message
        nMess=findEOF(filepath=(msg_log%file),err=err,msg=msg)
        if (err/=0) then
          lastmessage="Unable to extract last message from file:"//trim(msg_log%file)//" because "//trim(msg)
          return
        end if
        open(unit = msg_log%unit, file = msg_log%file, status = 'old')
        do i=1,(nMess-1)
!        read(msg_log%unit,'(a<test_len>)') dummy ! deprecated<> usage
          read(msg_log%unit,fmt) dummy
        end do
!        read(msg_log%unit,'(a<test_len>)') lastmessage ! deprecated<> usage
        read(msg_log%unit,fmt) lastmessage
        close(msg_log%unit)
      end if
      if (present(allmessages)) then ! If looking to return all error messages
        if (ASSOCIATED(allmessages)) deallocate(allmessages) ! Ensures messages is clear before retrival
        nMess=findEOF(filepath=(msg_log%file),err=err,msg=msg)
        if (err/=0) then
         allocate(allmessages(1))
         allmessages(1)="Unable to extract all messages from file:"//trim(msg_log%file)//" because "//trim(msg)
         return
        end if
        allocate(allmessages(nMess))
        open(unit = msg_log%unit, file = msg_log%file, status = 'old')
        do i=1,(nMess)
!          read(msg_log%unit,'(a<test_len>)') allmessages(i) ! deprecated<> usage
          read(msg_log%unit,fmt) allmessages(i) ! deprecated<> usage
        end do
        close(msg_log%unit)
      end if

    end subroutine get_messages
!************************************************************************************************
    !> detects if warnings have occurred
    elemental function warning() result(bool)
      ! description:
      implicit none
      logical  ::   bool !

      if (msg_log%warn>0) then
        bool = .true.
      else
        bool = .false.
      end if
    end function warning
!************************************************************************************************
    !> detects if errors have occurred
    elemental function error() result(bool)
      
      implicit none
      logical  ::   bool !

      if (msg_log%err>0) then
        bool = .true.
      else
        bool = .false.
      end if
    end function error
!************************************************************************************************
    !> update the comment character
    subroutine change_comchar(ch)
       
       implicit none
       character(1), intent(in) :: ch
       comchar = ch
     end subroutine
!************************************************************************************************
    !> Initiliases a message file db and read's in a msg file into the database
    function init_msg_db(db_id,msgdb_file) result(ok)
     
      use MUtilsLib_fileIO, only : findEof
      use MUtilslib_stringfuncs, only : operator(//)
      implicit none
      character(len = *), intent(IN) :: msgdb_file ! file name of msg file
      character(len = *), intent(IN) :: db_id ! id of the message db

      type (msg_db_type),allocatable ::new_msg_db(:)
      integer(mik) :: ok,i
      integer(mik) :: msg_db_num
      character(len=len_vlongStr):: msg
      ok=0
       if (.not.allocated(msg_db)) then
        allocate(msg_db(1))
        msg_db_num=1
      else
        msg_db_num=SIZE(msg_db)
        do i=1,msg_db_num
            if(msg_db(i)%id==db_id) then
                ! Already have same msg_db - no need to add it again!
                return
            end if
        end do
        allocate(new_msg_db(msg_db_num+1))
        new_msg_db(1:msg_db_num)=msg_db
        deallocate(msg_db)
        allocate(msg_db(msg_db_num+1))
        msg_db=new_msg_db
        deallocate(new_msg_db)
        !call move_alloc(new_msg_db,msg_db)
        msg_db_num=msg_db_num+1
      end if

      ! assign db_id
      msg_db(msg_db_num)%id=db_id

      ! Read-in msg file
      msg_db(msg_db_num)%n_msg=findEof(filepath=msgdb_file,err=ok,msg=msg)-1
      if (ok/=0) then; call message(log_error,msg); return; end if
      allocate(msg_db(msg_db_num)%msg(msg_db(msg_db_num)%n_msg))

      open(unit=10,file=msgdb_file,status="old",iostat=ok)
      if (ok/=0)then; call message(log_error,"Unable to open "//msgdb_file//"in init_msg_db"); return; end if
      read(10,*) ! Skip header
      do i=1,msg_db(msg_db_num)%n_msg
        read(10,*,iostat=ok) msg_db(msg_db_num)%msg(i)
        if (ok/=0) then;
          call message(log_error,"Unable to read msg "//i//" in file: "//msgdb_file//" in init_msg_db");
        end if
        ! Remove comma's on end
        msg_db(msg_db_num)%msg(i)%desc=msg_db(msg_db_num)%msg(i)%desc(1:(len_trim(msg_db(msg_db_num)%msg(i)%desc)-1))
        msg_db(msg_db_num)%msg(i)%remedy=msg_db(msg_db_num)%msg(i)%remedy(1:(len_trim(msg_db(msg_db_num)%msg(i)%remedy)-1))
      end do

      close(unit=10)

  end function init_msg_db
!************************************************************************************************
    !> Gets a message from the message db
    function get_msg_from_db(msg_id,db_id) result(msg)
     
      use MUtilslib_stringfuncs, only : operator(//)
      implicit none
      character(len = *), intent(IN) :: db_id !< id of the message db
      integer(mik), intent(IN) :: msg_id      !< message id

      integer(mik) :: i
      integer(mik) :: msg_db_num,msg_num
      character(len=len_vlongStr):: msg

      ! First find db
      do i=1,size(msg_db)
        if (msg_db(i)%id==db_id) then; msg_db_num=i;exit;end if
      end do

      if (i>size(msg_db)) then
           msg=" (Message db: "//db_id// " not found)"
           return
      end if

      ! Find and return msg in db
      do i=1,msg_db(msg_db_num)%n_msg
        if (msg_db(msg_db_num)%msg(i)%id==msg_id) then; msg_num=i;exit;end if
      end do

      if (i>msg_db(msg_db_num)%n_msg) then
           msg=" (Msg id: "//msg_id//" not found in "//db_id//" db)"
      else
        msg=" ("//trim(msg_db(msg_db_num)%msg(msg_num)%desc)//")"
        if (msg_db(msg_db_num)%msg(msg_num)%remedy/=" ") then
          msg=trim(msg)//" Remedy: "//trim(msg_db(msg_db_num)%msg(msg_num)%remedy)
        end if
      end if

  end function get_msg_from_db
!************************************************************************************************
!> Tidies up message log and msg_db
subroutine close_log()

integer(mik) :: i

! Deallocate msg_log
if (.not.msg_log%auto_flush) deallocate(msg_log%tag,msg_log%message)

! Deallocate msg_db
do i=1,size(msg_db); deallocate(msg_db(i)%msg); end do
deallocate(msg_db)

end subroutine close_log
!************************************************************************************************
end module MUtilsLib_messagelog
!************************************************************************************************

!> For backwards compaitbility
module message_log
  use MUtilsLib_messagelog
end module message_log
!************************************************************************************************

!> Purpose: Prints error messages
!> Provides a wrapper for message msg_log
module errorMod
  implicit none
  contains
  
  !> Write error message to screen then stops
  subroutine fatal_error(mess)
    use message_log
  
    implicit none
    ! Dummy Arguments
    character(len=*), intent(in) :: mess
    
    ! Wrapper Routine to print to GK's alert dialog
    !    CALL alertDialog("ERROR: "//mess)
    ! Wrapper Routine to print to screen
    !PRINT *,"ERROR: "//mess; READ (*,*)
    call message(log_FATAL,trim(mess))
    ! Wrapper Routine to ML's message logging system
    !  call message(log_fatal,msg)
  end subroutine fatal_error
end module errorMod
