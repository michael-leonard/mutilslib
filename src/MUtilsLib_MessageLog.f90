!<license>
module MUtilsLib_messagelog
! message_log - an error/message logging interface
! description:  logs (error/warning/other) messages during runtime and can flush the msg_log to the screen, to a file or both
!
! List of Routines
! ----------------
! Below provides a list of routines used in this module. The user is recommended to inspect the actual 
! interfaces of each routine in order to determine how to use

!  INIT_LOG(unit,close,append,active,echo,file,auto_flush) ! set custom parameters for the msg_log file (otherwise use default)
!  message(message)           ! add message to the msg_log object, defaults to ERROR tag, select tag from predefined list using tagtype
!  message(msg_type,message)  ! add tag and message to the msg_log object, select tag from predefined list using tagtype
!  flush_messages()           ! write the entire msg_log to file (unit =6, specifies screen)
!  get_messages()             ! reads the msg_log file and returns last message or entire msg_log
!  WARNING()                  ! logical function to check whether any warnings have occurred
!  ERROR()                    ! logical function to check whether any errors have occurred


! Notes:
! (1) This module does not support multiple logs - only one per program.
!     That is, only one instance of this object is declared as a "global" msg_log in this module.
! (2) A subroutine that calls one of the routines in this module cannot also be pure or elemental. This is because
!     the msg_log object reads and writes to screen/file.
! (3) This method of logging errors and warnings was selected so that the interface for each function
!     did not get polluted. Also, it is quite easy to remove the references to the msg_log object at a later
!     date, whereas it is messy to change the interface of a routine at a later date.
!	(4) A tag must be specified for every message that gets entered into the msg_log. Tags can be chosen from a
!     predefined set for which a global enum is provided.
!	(5) msg_log is kept private, so as to avoid incorrect use and so that logging facilities can be easily removed
!     from a project
!	(6) The msg_log class has several different options for outputting the data and for tracking errors and
!     warnings. The warning() and error() routines can be used to provide error handling and check 
!     if a subroutine executed okay.
  use kinds_dmsl_kit
  implicit none
  
  private ! All components are private unless declared otherwise
  integer, parameter, public   ::  tag_len  = len_stdStrB   ! tag length
  integer, parameter, public   ::  msg_len = len_vLongStr   ! some messages can contain strings of deep file directories, therefore recommended min = 255
  integer, parameter, public   ::  msg_tag_len = msg_len+tag_len
  character(len=1)  :: comchar = " "    ! a comment character

  type obj_msg_log
    private
    character(len = tag_len), pointer, dimension(:) ::  tag  => null() ! A tag to specify the type of message
    character(len = msg_len), pointer, dimension(:) ::  message => null() ! Any descriptive text
    integer  ::   err  = 0           ! Cumulative count of system errors
    integer  ::   warn = 0           ! Cumulative count of system warnings
    integer  ::   unit = 151         ! file ID unit, screen = 6, file = other
    logical  ::   close  = .true.    ! determines if the msg_log file is to be opened and closed for each flush
    logical  ::   append = .false.   ! Whether a new msg_log should be appeneded on to existing msg_log
    logical  ::   active = .true.    ! Allows msg_log to be activated / deactivated
    logical  ::   echo   = .true.   ! Whether msg_log should be written to screen and to file
    logical  ::   ignore_warn  = .false.           ! Whether warnings should be ignored
    logical  ::   ignore_error = .false.           ! Whether errors should be ignored
    logical  ::   auto_flush   = .true.            ! Automatically flush each entry after it is made
    logical  ::   debug   = .true.                 ! Should debug messages be written to the msg_log
    character(len = 100)  ::   file = 'message.log' ! file name if msg_log is written to file
  end type obj_msg_log

  ! Enumeration for different tag types (public)
  integer, parameter, public  ::   $error   = 1, &
                                   $warn    = 2, &
                                   $read    = 3, &
                                   $write   = 4, &
                                   $calc    = 5, &
                                   $name    = 6, &
                                   $path    = 7, &
                                   $title   = 8, &
                                   $comment = 9, &
                                   $debug   = 10, &
                                   $blank   = 11, &
                                   $fatal   = 12

  character(len = tag_len), parameter :: tag(1:12) = (/"Error:       ", &
                                                       "Warning:     ", &
                                                       "Read:        ", &
                                                       "Write:       ", &
                                                       "Calculate:   ", &
                                                       "File-Name:   ", &
                                                       "File-Path:   ", &
                                                       "Title:       ", &
                                                       "Comment:     ", &
                                                       "Debug:       ", &
                                                       "             ", &
                                                       "Fatal Error: " /)

  type(obj_msg_log)  :: msg_log                           ! a private system message log

  ! public interface
  public :: init_log, message, flush_messages, warning, error,get_messages

  interface message
    module procedure add_log_no_tag
    module procedure add_log_msg_tag
  end interface message
!************************************************************************************************
  contains
!************************************************************************************************
    subroutine init_log(unit,close,append,active,echo,file, ignore_warn, ignore_error,auto_flush,debug)
      ! description:  Set the parameters for the log file. Not necessary to be called if you are happy with defaults.
      implicit none
      integer, intent(IN), optional  ::   unit          ! file ID unit, screen = 6, file = other
      logical, intent(IN), optional  ::   close         ! determines if the log file is to be opened and closed for each flush
      logical, intent(IN), optional  ::   append        ! Whether log should be appeneded on to existing log
      logical, intent(IN), optional  ::   active        ! Allows log to be activated / deactivated
      logical, intent(IN), optional  ::   echo          ! Whether log should be written to screen and to file
      logical, intent(IN), optional  ::   ignore_warn   ! Whether warnings should be ignored
      logical, intent(IN), optional  ::   ignore_error  ! Whether errors should be ignored
      logical, intent(IN), optional  ::   auto_flush    ! Whether the log should be automatically flushed each time
      logical, intent(IN), optional  ::   debug         ! Whether debug comments should be ignored
      character(len = *), intent(IN), optional :: file ! file name if log is written to file

      ! Transfer input variables into log object
      if (present(unit))   msg_log%unit   = unit
      if (present(file))   msg_log%file   = trim(file)
      if (present(close))  msg_log%close  = close
      if (present(append)) msg_log%append = append
      if (present(active)) msg_log%active = active
      if (present(echo))   msg_log%echo   = echo
      if (present(ignore_warn))  msg_log%ignore_warn    = ignore_warn
      if (present(ignore_error)) msg_log%ignore_error   = ignore_error
      if (present(auto_flush))   msg_log%auto_flush     = auto_flush
      if (present(debug))   msg_log%debug     = debug

      ! Check for consistency/logic of log parameters
      if (msg_log%echo .AND. msg_log%unit == 6)   msg_log%unit   = 111     ! When echo-ing must have file ID other than 6
      if (msg_log%append .AND. msg_log%unit == 6) msg_log%append = .false. ! file appending is not needed when  writing to screen only

      ! if necessary open the log file
      if (msg_log%active) then
        if (msg_log%unit /=6) then
          if (msg_log%append) then ! keep the existing msg_log file (if there is one)
            open(unit = msg_log%unit, file = trim(msg_log%file), status = 'unknown', position = 'append')
          else ! A new msg_log file
            open(unit = msg_log%unit, file = trim(msg_log%file), status = 'replace')
          end if
          if (msg_log%close) close(msg_log%unit) ! close the log file
        end if
      end if

    end subroutine init_log
!************************************************************************************************
    subroutine add_log_no_tag(message)
      ! A wrapper to simplify recording error messages -default tag is $error
      implicit none
      character(len = *), intent(IN) ::  message  ! Any descriptive message
      
      call add_log_msg_tag($error,message)

    end subroutine
!************************************************************************************************
    subroutine add_log_msg_tag(msg_type,message)
     ! Adds a single message to the msg_log object
     ! All arguments non-optional

      implicit none
      integer, intent(IN)  ::  msg_type         ! Index of type of tag
      character(len = *), intent(IN) ::  message  ! Any descriptive message
      character(len = tag_len), pointer, dimension(:) ::   Tcopy =>null() ! Temporary storage
      character(len = msg_len), pointer, dimension(:) ::  Ncopy =>null() ! Temporary storage
      integer  ::   s ! size of message array

      s = 0
      if(associated(msg_log%message)) s = size(msg_log%message)
      if(s/=0) then
        if((trim(msg_log%message(s))==trim(message)).AND.(trim(tag(msg_type))==trim(msg_log%tag(s)))) then
          ! Avoid multiple identical concurrent entries
          ! I.e. The most recent entry was identical to the one being added, so there is no need to add it in
          ! It is however possible to have multiple entries ... provided they are not concurrent
          ! This feature was added because of issues generating errors in the middle of a do-loop
          return;
        end if
      end if

      if (msg_log%active) then
        select case(msg_type)
          case($error)
            msg_log%err = msg_log%err + 1   ! increment the error count
          case($warn)
            msg_log%warn = msg_log%warn + 1 ! increment the warning count
          case($debug)
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
          msg_log%message(s+1) = trim(message) ! append the new message onto the end
          deallocate(Tcopy); nullify(Tcopy)
          deallocate(Ncopy); nullify(Ncopy)
        else
          ! add a single message into the msg_log
          allocate(msg_log%message(1))
          allocate(msg_log%tag(1))
          msg_log%message(1) = trim(message)   ! insert the message
          msg_log%tag(1) = tag(msg_type)  ! insert the tag
        end if
      end if

      if(msg_type==$fatal) then
        call add_log_no_tag("A fatal error has occurred. Please check "//TRIM(msg_log%file)//" for details. Program was not terminated, though results may be abnormal.")
        call flush_messages()
      end if

      ! flush the msg_log straight away if necessary
      if(msg_log%auto_flush) call flush_messages()

    end subroutine add_log_msg_tag
!************************************************************************************************
    subroutine flush_messages()
      ! writes out the msg_log object
      ! and deletes all entries

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
              if (msg_log%ignore_warn .AND. msg_log%tag(i) == tag($warn)) cycle ! ignore warning messages
              if (msg_log%ignore_error .AND. msg_log%tag(i) == tag($error)) cycle ! ignore error messages
                if(msg_log%tag(i)==tag($blank)) then
                    write(*,'(A)') trim(comchar) // trim(msg_log%tag(i)) // trim(msg_log%message(i))
                else
                    write(*,'(A)') trim(comchar) // trim(msg_log%tag(i)) //" "// trim(msg_log%message(i))
                end if
            end do
          else ! a file write is needed
            ! open the file if not already open
            if (msg_log%close) open(unit = msg_log%unit, file = trim(msg_log%file), status = 'old', position = 'append')

              ! write all msg_log entries to file
              do i = 1, size(msg_log%message)
                if (msg_log%ignore_warn .AND. msg_log%tag(i) == tag($warn)) cycle ! ignore warning messages
                if (msg_log%ignore_error .AND. msg_log%tag(i) == tag($error)) cycle ! ignore error messages
                if(msg_log%tag(i)==tag($blank)) then
                    write(msg_log%unit,'(A)') trim(comchar) // trim(msg_log%tag(i)) // trim(msg_log%message(i))
                else
                    write(msg_log%unit,'(A)') trim(comchar) // trim(msg_log%tag(i)) //" "// trim(msg_log%message(i))
                end if
              end do

              ! the message needs to be echoed to screen
              if (msg_log%echo) then
                do i = 1, size(msg_log%message)
                  if (msg_log%ignore_warn .AND. msg_log%tag(i) == tag($warn)) cycle ! ignore warning messages
                  if (msg_log%ignore_error .AND. msg_log%tag(i) == tag($error)) cycle ! ignore error messages
                  if(msg_log%tag(i)==tag($blank)) then
                    write(*,'(A)') trim(comchar) // trim(msg_log%tag(i)) // trim(msg_log%message(i))
                  else
                    write(*,'(A)') trim(comchar) // trim(msg_log%tag(i)) //" "// trim(msg_log%message(i))
                  end if
                end do
              end if

            ! close the file again if specified
            !if (msg_log%close) open(unit = msg_log%unit, file = trim(msg_log%file), status = 'old', position = 'append') ML's old like
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
    subroutine get_messages(allmessages,lastmessage)
      ! reads the msg_log file and returns the lastmessage or allmessages depending which is present
      use MUtilsLib_fileIO, only : findEOF
      implicit none
      ! Outputs      
      character(len = msg_tag_len), allocatable, dimension(:),intent(out),optional ::  allmessages ! Descriptive text of tags and messages
      character(len = msg_tag_len), intent(out),optional ::  lastmessage ! Descriptive text of tags and messages
      
      integer :: err
      character(len=len_vLongStr) :: msg
      
       ! Locals
      integer :: i,nMess,test_len ! loop counter
      character(len = msg_len+tag_len) ::  dummy ! dummy text

      ! Initialisation
      call flush_messages ! Ensures all messages are sent to msg_log file before retrieval
	  test_len=(msg_len+tag_len)
      
      if (present(lastmessage)) then ! If looking for last-only message
        nMess=findEOF(filepath=(msg_log%file),err=err,msg=msg)
        if (err/=0) then
          lastmessage="Unable to extract last message from file:"//trim(msg_log%file)//" because "//trim(msg)
          return
        end if
        open(unit = msg_log%unit, file = trim(msg_log%file), status = 'old')
        do i=1,(nMess-1)
	      read(msg_log%unit,'(a<test_len>)') dummy
        end do
        read(msg_log%unit,'(a<test_len>)') lastmessage
        close(msg_log%unit)
      end if
      if (present(allmessages)) then ! If looking to return all error messages
        if (allocated(allmessages)) deallocate(allmessages) ! Ensures messages is clear before retrival
        nMess=findEOF(filepath=(msg_log%file),err=err,msg=msg)
        if (err/=0) then
         allocate(allmessages(1))
         allmessages(1)="Unable to extract all messages from file:"//trim(msg_log%file)//" because "//trim(msg)
         return
        end if
        allocate(allmessages(nMess))
        open(unit = msg_log%unit, file = trim(msg_log%file), status = 'old')
        do i=1,(nMess)
          read(msg_log%unit,'(a<test_len>)') allmessages(i)
        end do
        close(msg_log%unit)
      end if

    end subroutine get_messages
!************************************************************************************************
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
    elemental function error() result(bool)
      ! description:  
      implicit none
      logical  ::   bool ! 

      if (msg_log%err>0) then 
        bool = .true.
      else
        bool = .false.
      end if
    end function error
!************************************************************************************************
    subroutine change_comchar(ch)
       ! update the comment character
       implicit none
       character(1), intent(in) :: ch
       comchar = ch
     end subroutine
!************************************************************************************************
end module MUtilsLib_messagelog
!************************************************************************************************
module message_log
! For backwards compaitbility
use MUtilsLib_messagelog
end module message_log
!************************************************************************************************
MODULE errorMOD
! Purpose: Prints error messages
! Provides a wrapper for message msg_log
IMPLICIT NONE
CONTAINS
!***************************************************************************************
SUBROUTINE fatal_error(mess)
USE MESSAGE_LOG
! Purpose: Write error message to screen then stops
IMPLICIT NONE

   ! Dummy Arguments
    CHARACTER(LEN=*), INTENT(IN) :: mess
! Wrapper Routine to print to GK's alert dialog
!    CALL alertDialog("ERROR: "//mess)
! Wrapper Routine to print to screen
    !PRINT *,"ERROR: "//mess; READ (*,*)
    call message($FATAL,trim(mess))
! Wrapper Routine to ML's message logging system
!  call message($fatal,msg)
END SUBROUTINE fatal_error
end module errorMOD
