!<license>
module MUtilsLib_MessageLog

!*****************************************************************************************
! MessageLog
!*****************************************************************************************
! MessageLog


  use kinds_dmsl_kit
  implicit none
  save
  
  private ! All components are private unless declared otherwise
  integer, parameter, public   ::  tag_len  = len_stdStrB   ! tag length
  integer, parameter, public   ::  msg_len = len_vLongStr   ! some messages can contain strings of deep file directories
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
                                                       
  type msg_db_msg_type                                 ! Type for messages in message database
    integer(mik):: id                                   ! id for messages in message database                    
    character(len=len_LongStr) :: desc                  ! message description 
    character(len=len_vLongStr) :: remedy               ! possible remedies if message is for an error
  end type msg_db_msg_type
        
  type msg_db_type                                      ! Type for message database
    character(len=len_stdStrD) :: id                    ! id message database
    integer(mik) ::n_msg                                ! number of message in message database
    type(msg_db_msg_type),allocatable :: msg(:)         ! message in message database
  end type
  
  type (msg_db_type), allocatable :: msg_db(:)          ! Message database(s)    
    
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
    subroutine init_log(unit,close,append,active,echo,file, ignore_warn, ignore_error,auto_flush,debug,db_id,msg_file)
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
      character(len = *), intent(IN), optional :: db_id ! ID of the message database
      character(len = *), intent(IN), optional :: msg_file ! file name of the msg file for message database
      integer :: ok
      OK=0;
    
    end subroutine init_log
!************************************************************************************************
    subroutine add_log_no_tag(message,msg_id,db_id)
      ! A wrapper to simplify recording error messages -default tag is log_error
      implicit none
      character(len = *), intent(IN) ::  message  ! Any descriptive message
      character(len = *), intent(IN),optional ::  db_id
      integer(mik),intent(in), optional :: msg_id

    end subroutine
!************************************************************************************************
    subroutine add_log_msg_tag(msg_type,message,msg_id,db_id)
     ! Adds a single message to the msg_log object
     ! All arguments non-optional
      implicit none
      integer, intent(IN)  ::  msg_type         ! Index of type of tag
      character(len = *), intent(IN) ::  message  ! Any descriptive message
      character(len = *), intent(IN),optional ::  db_id ! id of the message db
      integer(mik),intent(in), optional :: msg_id ! message id
      
      character(len=len_vLongStr):: messageLc
      
      character(len = tag_len), pointer, dimension(:) ::   Tcopy =>null() ! Temporary storage
      character(len = msg_len), pointer, dimension(:) ::  Ncopy =>null() ! Temporary storage
      integer  ::   s ! size of message array

   end subroutine add_log_msg_tag
!************************************************************************************************
    subroutine flush_messages()
      ! writes out the msg_log object
      ! and deletes all entries

      implicit none
       ! Locals
      integer :: i ! loop counter

    end subroutine flush_messages
!************************************************************************************************
    subroutine get_messages(allmessages,lastmessage)
      ! reads the msg_log file and returns the lastmessage or allmessages depending which is present
      implicit none
      ! Outputs      
      character(len = msg_tag_len), allocatable, dimension(:),intent(out),optional ::  allmessages ! Descriptive text 
                                                                                                   ! of tags and messages
      character(len = msg_tag_len), intent(out),optional ::  lastmessage ! Descriptive text of tags and messages
      
      integer :: err
      character(len=len_vLongStr) :: msg
      
       ! Locals
      integer :: i,nMess,test_len ! loop counter
      character(len = msg_len+tag_len) ::  dummy ! dummy text
      if (present(lastmessage)) lastmessage=""

    end subroutine get_messages
!************************************************************************************************
    elemental function warning() result(bool)
      ! description:  
      implicit none
      logical  ::   bool ! 
      bool=.false.

    end function warning
!************************************************************************************************
    elemental function error() result(bool)
      ! description:  
      implicit none
      logical  ::   bool ! 
      bool=.false.
    end function error
!************************************************************************************************
end module MUtilsLib_messageLog
!************************************************************************************************
module message_log
! For backwards compaitbility
use MUtilsLib
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
    call message(log_FATAL,trim(mess))
! Wrapper Routine to ML's message logging system
!  call message(log_fatal,msg)
END SUBROUTINE fatal_error
end module errorMOD
!***************************************************************************************
! MUtilsLib_System
!***************************************************************************************
module MUtilsLib_System
    ! Generic system (windows) utilities
    ! Any subroutine that uses IVF/WINDOWS SPECIFIC routines
    ! e.g. 
    ! - calls to if/dfport module
    ! - IVF extensions to Fortran standard
    ! No other modules should have calls to the dos interface
    use kinds_dmsl_kit
    use MUtilsLib_MessageLog
    implicit none
    
    private ! Keep hidden
    
    public :: taskkill, &   ! Terminates ALL copies of a current process based on its image name
              expand, &    ! expand a dos environment variable
              getEnvVar, & ! Wrapper for expand, with better error handling
              findcurrentdir, &   ! determines the location of the current working directory
              processExist, &     ! Determine if a given process exists
              Generate_FileList,& ! Generates a list of files with a given extension in a given path 
              Generate_SystemList,& ! Generates a list from the system of either files or directories
              OSCall              ! OS Command line interface utility with immediate return or wait specified in milliseconds 
              
  contains
!*************************************************************************************************************   
   function taskkill(imageName) result(ok)
     implicit none
     character(len=*) :: imageName ! the name of the task to be killed
     integer :: ok ! error flag
     ok=0;   
   end function
!*************************************************************************************************************   
   function expand(ev) result(str)
     implicit none
     character(len=*) :: ev ! environment variable, specified as per a .bat file - must have %% symbols , e.g. %PATH%
     character(len = len_vLongStr) :: str ! the expanded string
     str=""   
   end function
!*************************************************************************************************************   
   function getEnvVar(ev,val) result(ok) ! Wrapper for expand - with a more suitable name, env variable
     
     character(len=*) :: ev ! environment variable, must have %% symbols , e.g. %PATH%
     character(len = len_vLongStr) :: val ! the value of the environmental variable
     integer(mik) :: ok
     ok=0
   end function
!*************************************************************************************************************   
   function findcurrentdir(noslash) result(dir)
      implicit none
      ! Dummies
      character(len=len_vLongStr) :: dir
      logical, intent(in),optional :: noslash
      ! Locals
      dir=""
    end function findcurrentdir
!*************************************************************************************************************   
   function processExist(imageName) result(test)
      implicit none
      ! Dummies
      character(len=*),intent(in) :: imageName
      logical :: test 
      test=.False.

    end function processExist
!*************************************************************************************************************   
    function Generate_FileList(path, ext, filelist) result (ok)
     implicit none
     ! Dummies - Inputs
     character(len=*) :: path ! the path to the files are located
     character(len=*) :: ext ! the extension of the files that need to be returned, 
                             ! for files with one letter extentions (e.g, *.r), 
                             ! it is important to use a space, e.g. ext="*.r " to distinguish from .r** extensions
     ! Dummies - Outputs
     character(len=len_vLongStr),pointer :: filelist(:) ! Output: the list of Rscripts
     ! Function Results
     integer(mik) :: ok
     ok=0;
     filelist=>NULL()
    end function Generate_FileList
!*************************************************************************************************************   
    function Generate_SystemList(path, ext, list,listtype) result (ok)
     implicit none
     ! Dummies - Inputs
     character(len=*) :: path ! the path to the files are located
     character(len=*) :: listtype ! the type of list needed, options include
                             ! "FILE"    - list of files
                             ! "DIRECTORY" - list of directories
     character(len=*),optional :: ext ! the extension of the files that need to be returned, 
                             ! for files with one letter extentions (e.g, *.r), 
                             ! it is important to use a space, e.g. ext="*.r " to distinguish from .r** extensions
                             
                             
     ! Dummies - Outputs
     character(len=len_vLongStr),pointer :: list(:) ! the output list
     ! Function Results
     integer(mik) :: ok
     ok=0
   end function Generate_SystemList
 !*************************************************************************************************************   
    ! Copyright (C) 2001 by Fortran Library
    !
    ! This source may be freely copied, modified, or distributed so long as the original
    ! copyright statement remains intact.
    !
    ! Suggestions for improvment to the original posted version are welcome. Comments
    ! should be sent to mailto:webmaster@fortranlib.com
    !
    ! Version:  2.0, 4 August 2001, 21:20:00
    !
    ! Purpose:  OS Command line interface utility with immediate return or wait specified
    !           in milliseconds (routine automatically quotes the command string)
    !
    ! System Requirements:  Written for Digital/Compaq/Intel) Visual Fortran (x86)
    !
    ! Routine Name:  OSCall
    !
    ! iWaitMS:  default 32-bit (unsigned) integer wait value in milliseconds
    !           0 = do not wait for completion
    !          >0 = Number of milliseconds to wait for completion of initiated process
    !          -1 = Infinite wait (wait for process completion)
    !
    ! Command:  character command string
    !
    ! Args:     optional character argument list string
    !
    ! iRet:     default integer return code
    !          -1 = Unable to initiate process
    !           0 = Successful process initiate (if iWait = 0)

    subroutine OSCall(iWaitMS,Command,Args,iRet)

       implicit none

       character(*), intent(in)     :: Command        !Command portion of the command line (i.e. the program name)
       character(*), intent(in)     :: Args           !Argument portion of the command line
       character(256)               :: CmdLine        !Work area for the command line
       integer, intent(in)          :: iWaitMS        !Process completion wait value in milliseconds
       integer, intent(out)         :: iRet           !Main return code
       integer                      :: iCRC           !Return code for CreateProcess
       
       iRet=0

    end subroutine

 end module MUtilsLib_System
!*****************************************************************************
! For backwards compatibility
module utils_system
use MUtilsLib_System
end module utils_system
!*****************************************************************************
!MUtilsLib_stringfuncs
!*****************************************************************************
module MUtilsLib_stringfuncs
  ! Provides an overloaded interface for the // operator to mix numbers or logicals with strings
  ! Whereas // uses several defaults, str() function provides control over the number converstion
  ! The .pad. operator is for all those times you want to concatenate but have a space (or comma or something) between
  ! c() function is for generating an R-syntax array easily

  ! Note 1
  ! Consider the syntax: "string" // 4 // 5.1
  ! This will first call str_i_concat() to give "string4" and then call str_r8_concat() to give "string45.1"
  ! That is, the function i_r8_concat() is not ever called to concatenate the integer and the real.
  ! How often is it that you want to concatenate numbers without putting a space or a comma or some other string between them.
  ! In otherwords, the concat functions that do not include atleast one string argument are likely to go unused and are
  ! therefore bloatware that could be deleted.
  implicit none
  character(len = 1) :: pad_ch = " "   ! Private padding character

  private ! keep hidden unless declared public
  public :: str,    &              ! this is just to make it easier to build up string expressions for passing numbers into R
            operator(//), &        ! overloaded to allow concatenation of strings
            operator(.pad.), &     ! same as above but allows one padding space between concatenation
            c,            &        ! converts arrays to strings
            set_pad, &             ! change the padding character when using the .pad. operator
            fwdslash,backslash, &  ! convert a <filepath> string with back/fwd slashes to having forward/back slashes
            FolderUp, &            ! convert a <filepath> string be removing trailing folders
            Lcase, Ucase,&         ! convert string to lower/upper case (important for string comparisons)
            int,&                  ! Convert a string to integer
            real,&                 ! COnvert string to real 
            insertString_end, &    ! Insert a string on the end
            removeChar, &          ! Remove Character from string
            relPathtoAbsPath, &    ! Convert a string with a relative path to an absolute path use for Rsetwd command
            stripBlanks,&          ! simple string function to remove blank spaces
            changeChar,&           ! string function to change a character in string to another 
                                   ! (useful for changing spaces to underscores)
            index, &                  ! finds the index of a character vector that corresponds to a string input                
            insertString
  interface index
    module procedure index_1D
  end interface 
    
  ! This routine is a generic utility for making it easier to interface using strings between and others Fortran
  interface int
    module procedure i4_str
  end interface int
  
  interface real
   module procedure real8_str
  end interface

  ! This routine is a generic utility for making it easier to interface using string commands between R and Fortran
  interface str
    module procedure str_i
    module procedure str_r4
    module procedure str_r8
  end interface str

  ! This overloads the concatenate operator to make it even easier to build up strings mixed in with variable expressions
  interface operator(//)
    module procedure str_l_concat
    module procedure str_i_concat
    module procedure str_r4_concat
    module procedure str_r8_concat
    module procedure l_str_concat
    module procedure i_str_concat
    module procedure r4_str_concat
    module procedure r8_str_concat
    module procedure i_i_concat     ! useful for concatenating filenames and dates
    ! The routines below are potentially never used - see note 1
    module procedure l_l_concat
    module procedure r4_r4_concat
    module procedure r8_r8_concat
    module procedure i_r8_concat
    module procedure i_r4_concat
    module procedure i_l_concat
    module procedure r4_r8_concat
    module procedure r4_l_concat
    module procedure l_r8_concat
    module procedure r8_i_concat
    module procedure r4_i_concat
    module procedure l_i_concat
    module procedure r8_r4_concat
    module procedure l_r4_concat
    module procedure r8_l_concat
  end interface

  ! same as above but allows for a pading character in between
  interface operator(.pad.)
    module procedure str_pad_str_concat
    module procedure l_pad_l_concat
    module procedure i_pad_i_concat
    module procedure r4_pad_r4_concat
    module procedure r8_pad_r8_concat
    module procedure str_pad_l_concat
    module procedure str_pad_i_concat
    module procedure str_pad_r4_concat
    module procedure str_pad_r8_concat
    module procedure l_pad_str_concat
    module procedure i_pad_str_concat
    module procedure r4_pad_str_concat
    module procedure r8_pad_str_concat
    module procedure i_pad_r8_concat
    module procedure i_pad_r4_concat
    module procedure i_pad_l_concat
    module procedure r4_pad_r8_concat
    module procedure r4_pad_l_concat
    module procedure l_pad_r8_concat
    module procedure r8_pad_i_concat
    module procedure r4_pad_i_concat
    module procedure l_pad_i_concat
    module procedure r8_pad_r4_concat
    module procedure l_pad_r4_concat
    module procedure r8_pad_l_concat
  end interface

  interface c
    module procedure i4_array_concat
    module procedure r4_array_concat
    module procedure r8_array_concat
    module procedure l_array_concat
    module procedure str_array_concat
  end interface

  ! Private routine ! determine string length of real number conversion
  interface real_len
    module procedure r4_len
    module procedure r8_len
  end interface

  contains
!!!!!!!!!!! Number conversion / string handling conveniences for passing arguments into R
!!!!!!!!!!! Converts String to Others
  function i4_str(str)
    implicit none
    character(len=*),intent(in) :: str
    integer(4) :: i4_str
    str=""
  end function i4_str
!!!!!!!!!!! Converts String to Others
  function real8_str(str)
    implicit none
    character(len=*),intent(in) :: str
    real(8) :: real8_str
    str=""
  end function real8_str
!!!!!!!!!!! Number conversion / string handling conveniences for passing arguments into R
!!!!!!!!!!! Convert Others to String

  function str_i(i,n,pad) result(ch)
    ! Converts an integer to a string
    implicit none
    integer, intent(in) :: i     ! the integer variable
    integer, intent(in) :: n     ! n the length of the return string
    character(len = 1), optional, intent(in) :: pad ! should the leading fields be padded with zeroes, 
                                                    ! return string is padded with this character default blank
    ! Locals
    character(len = n) :: ch     ! the string to be returned
    character(len = 20) :: temp  ! temporary variable
    integer :: j                 ! loop counter

    write(temp,*) i ! default formatting requires atleast 12 spaces
    ch = trim(adjustl(temp))

    if(present(pad)) then
      forall(j = 1:n,ch(j:j)==" ")
        ch(j:j) = pad
      end forall
    end if

  end function

  function str_r4(r,n,pad) result(ch)
    ! Converts an real(4) to a string
    implicit none
    real(4), intent(in) :: r     ! the real variable
    integer, intent(in) :: n     ! n the length of the return string (number of significant digits)
    character(len = 1), optional, intent(in) :: pad ! should the leading fields be padded with zeroes, 
                                                    ! return string is padded with this character default blank
    ! Locals
    character(len = n) :: ch     ! the string to be returned
    character(len = 20) :: temp  ! temporary variable
    integer :: j                 ! loop counter

    write(temp,*) r ! default formatting
    ch = trim(adjustl(temp))

    if(present(pad)) then
      forall(j = 1:n,ch(j:j)==" ")
        ch(j:j) = pad
      end forall
    end if

  end function

  function str_r8(r,n,pad) result(ch)
    ! Converts an real(8) to a string
    implicit none
    real(8), intent(in) :: r     ! the real variable
    integer, intent(in) :: n     ! n the length of the return string (number of significant digits)
    character(len = 1), optional, intent(in) :: pad ! should the leading fields be padded with zeroes, 
                                                    ! return string is padded with this character default blank
    ! Locals
    character(len = n) :: ch     ! the string to be returned
    character(len = 30) :: temp  ! temporary variable (using CVF string needs to be atleast len = 24 to avoid crash)
    integer :: j                 ! loop counter

    write(temp,*) r ! default formatting
    ch = trim(adjustl(temp))

    if(present(pad)) then
      forall(j = 1:n,ch(j:j)==" ")
        ch(j:j) = pad
      end forall
    end if

  end function

  function l_str_concat(l,s) result(ch)
    ! Concatenate an logical and a string
    ! exact length
    implicit none
    logical(4), intent(in) :: l             ! the input logical variable
    character(len=*), intent(in) :: s    ! the input string
    character(len = len(s)+1) :: ch     ! the string to be returned 
    if(l) then
     ch = "T" // s
    else
     ch = "F" // s
    end if
  end function

  function str_l_concat(s,l) result(ch)
    ! Concatenate an logical and a string
    ! exact length
    implicit none
    logical(4), intent(in) :: l             ! the input logical variable
    character(len=*), intent(in) :: s    ! the input string
    character(len = len(s)+1) :: ch     ! the string to be returned
    if(l) then
         ch =  s // "T"
    else
        ch =  s // "F"
    end if
  end function

  function l_l_concat(l1,l2) result(ch)
    ! Concatenate an logical and a logical
    implicit none
    logical(4), intent(in) :: l1,l2             ! the input logical variables
    character(len = 2) :: ch     ! the string to be returned 
    if(l1.AND.l2) then
        ch =  "TT"
    elseif(l1.AND..NOT.l2) then
        ch =  "TF"
    elseif(.NOT.l1.AND.l2) then
        ch =  "FT"
    else
        ch =  "FF"
    end if
  end function

  function r4_str_concat(r,s) result(ch)
    ! Concatenate a real and a string
    implicit none
    real(4), intent(in) :: r             ! the input real variable
    character(len=*), intent(in) :: s    ! the input string
    character(len = len(s)+real_len(r)) :: ch     ! the string to be returned (upto 16 places for real)
    ch = str(r,real_len(r)) // s
  end function

  function str_r4_concat(s,r) result(ch)
    ! Concatenate an real and a string
    implicit none
    real(4), intent(in) :: r             ! the input real variable
    character(len=*), intent(in) :: s    ! the input string
    character(len = len(s)+real_len(r)) :: ch     ! the string to be returned (upto 16 places for real)
    ch = s // str(r,real_len(r))
  end function

  function r4_r4_concat(r1,r2) result(ch)
    ! Concatenate two reals
    implicit none
    real(4), intent(in) :: r1,r2         ! the input real variables
    character(len = real_len(r1)+real_len(r2)) :: ch     ! the string to be returned (upto 16 places for real)
    ch = str(r1,real_len(r1)) // str(r2,real_len(r2))
  end function

  function r8_str_concat(r,s) result(ch)
    ! Concatenate an real and a string
    implicit none
    real(8), intent(in) :: r             ! the input real variable
    character(len=*), intent(in) :: s    ! the input string
    character(len = len(s)+real_len(r)) :: ch     ! the string to be returned (upto 16 places for real)
    ch = str(r,real_len(r)) // s
  end function

  function str_r8_concat(s,r) result(ch)
    ! Concatenate an real and a string
    implicit none
    real(8), intent(in) :: r             ! the input real variable
    character(len=*), intent(in) :: s    ! the input string
    character(len = len(s)+real_len(r)) :: ch     ! the string to be returned (upto 16 places for real)
    ch = s // str(r,real_len(r))
  end function

  function r8_r8_concat(r1,r2) result(ch)
    ! Concatenate two reals
    ! Result will be exact length (no trimming or padding)
    implicit none
    real(8), intent(in) :: r1,r2         ! the input real variables
    character(len = real_len(r1)+real_len(r2)) :: ch     ! the string to be returned (upto 16 places for real)
    ch = str(r1,real_len(r1)) // str(r2,real_len(r2))
  end function

  function i_str_concat(i,s) result(ch)
    ! Concatenate an integer and a string
    ! computes exact length of resultant string, no padding
    implicit none
    integer(4), intent(in) :: i     ! the input integer variable
    character(len=*), intent(in) :: s     ! the input string
    character(len = int_len(i)+len(s)) :: ch     ! the string to be returned (exact size is calculated)
    ch = str(i,int_len(i)) // s
  end function

  function str_i_concat(s,i) result(ch)
    ! Concatenate a string and an integer
    ! computes exact length of resultant string, no padding
    implicit none
    integer(4), intent(in) :: i     ! the input integer variable
    character(len=*), intent(in) :: s     ! the input string
    character(len = int_len(i)+len(s)) :: ch     ! the string to be returned (exact size is calculated)
    ch = s // str(i,int_len(i))
  end function

  function i_i_concat(i,j) result(ch)
    ! Concatenate an integer and an integer
    ! computes exact length of resultant string, no padding
    implicit none
    integer(4), intent(in) :: i     ! the input integer variable
    integer(4), intent(in) :: j     ! the input integer variable
    character(len = int_len(i)+int_len(j)) :: ch     ! the string to be returned (exact size is calculated)
    ch =  str(i,int_len(i)) //  str(j,int_len(j))
  end function

  function i_r8_concat(i,r) result(ch)
    ! Concatenate an integer and a double precision
    ! computes exact length of resultant string, no padding
    implicit none
    integer(4), intent(in) :: i     ! the input integer variable
    real(8), intent(in) :: r        ! the input real variable
    character(len = int_len(i) + real_len(r)) :: ch     ! the string to be returned (exact size is calculated)
    ch =  str(i,int_len(i)) //  str(r,real_len(r))
  end function

  function r8_i_concat(r,i) result(ch)
    ! Concatenate an integer and a double precision
    ! computes exact length of resultant string, no padding
    implicit none
    integer(4), intent(in) :: i     ! the input integer variable
    real(8), intent(in) :: r        ! the input real variable
    character(len = int_len(i) + real_len(r)) :: ch     ! the string to be returned (exact size is calculated)
    ch =  str(i,int_len(i)) //  str(r,real_len(r))
  end function

  function i_r4_concat(i,r) result(ch)
    ! Concatenate an integer and a double precision
    ! computes exact length of resultant string, no padding
    implicit none
    integer(4), intent(in) :: i     ! the input integer variable
    real(4), intent(in) :: r        ! the input real variable
    character(len = int_len(i) + real_len(r)) :: ch     ! the string to be returned (exact size is calculated)
    ch =  str(i,int_len(i)) //  str(r,real_len(r))
  end function

  function r4_i_concat(r,i) result(ch)
    ! Concatenate an integer and a double precision
    ! computes exact length of resultant string, no padding
    implicit none
    integer(4), intent(in) :: i     ! the input integer variable
    real(4), intent(in) :: r        ! the input real variable
    character(len = int_len(i) + real_len(r)) :: ch     ! the string to be returned (exact size is calculated)
    ch =  str(i,int_len(i)) //   str(r,real_len(r))
  end function

  function r4_r8_concat(r4,r8) result(ch)
    ! Concatenate an two reals
    ! Result will be exact length (no trimming or padding)
    implicit none
    real(4), intent(in) :: r4  ! the input real variables
    real(8), intent(in) :: r8
    character(len = real_len(r4)+real_len(r8)) :: ch     ! the string to be returned (upto 16 places for real)
    ch =  str(r4,real_len(r4)) //   str(r8,real_len(r8))
  end function

  function r8_r4_concat(r8,r4) result(ch)
    ! Concatenate two reals
    ! Result will be exact length (no trimming or padding)
    implicit none
    real(4), intent(in) :: r4  ! the input real variables
    real(8), intent(in) :: r8
    character(len = real_len(r4)+real_len(r8)) :: ch     ! the string to be returned (upto 16 places for real)
    ch =  str(r8,real_len(r8)) //  str(r4,real_len(r4))
  end function

  function l_i_concat(l,i) result(ch)
    ! Concatenate an logical and an integer
    ! exact length
    implicit none
    logical(4), intent(in) :: l             ! the input logical variable
    integer(4), intent(in) :: i     ! the input integer variable
    character(len = int_len(i) +1) :: ch     ! the string to be returned 
    if(l) then 
        ch = "T" // str(i,int_len(i))
    else
        ch = "F" // str(i,int_len(i))
    end if
  end function

  function i_l_concat(i,l) result(ch)
    ! Concatenate an logical and an integer
    ! exact length
    implicit none
    logical(4), intent(in) :: l             ! the input logical variable
    integer(4), intent(in) :: i     ! the input integer variable
    character(len = int_len(i) +1) :: ch     ! the string to be returned 
    if(l) then 
        ch =   str(i,int_len(i)) // "T"
    else
       ch =   str(i,int_len(i)) // "F"
    end if
  end function

  function l_r4_concat(l,r4) result(ch)
    ! Concatenate an logical and a real
    ! exact length
    implicit none
    logical(4), intent(in) :: l             ! the input logical variable
    real(4), intent(in) :: r4  ! the input real variables
    character(len = real_len(r4) +1) :: ch     ! the string to be returned 
    if(l) then
      ch = "T" //  str(r4,real_len(r4))
    else
      ch = "F" //  str(r4,real_len(r4))
    end if
  end function

  function r4_l_concat(r4,l) result(ch)
    ! Concatenate an logical and a real
    ! exact length
    implicit none
    logical(4), intent(in) :: l             ! the input logical variable
    real(4), intent(in) :: r4  ! the input real variables
    character(len = real_len(r4) +1) :: ch     ! the string to be returned 
    if(l) then
        ch =   str(r4,real_len(r4)) // "T"
    else
        ch =   str(r4,real_len(r4)) // "F"
    end if
  end function

  function l_r8_concat(l,r8) result(ch)
    ! Concatenate an logical and a real
    ! exact length
    implicit none
    logical(4), intent(in) :: l             ! the input logical variable
    real(8), intent(in) :: r8  ! the input real variables
    character(len = real_len(r8) +1) :: ch     ! the string to be returned 
    if(l) then
      ch = "T" // str(r8,real_len(r8))
    else
      ch = "F" // str(r8,real_len(r8))
    end if
  end function

  function r8_l_concat(r8,l) result(ch)
    ! Concatenate an logical and a real
    ! exact length
    implicit none
    logical(4), intent(in) :: l             ! the input logical variable
    real(8), intent(in) :: r8  ! the input real variables
    character(len = real_len(r8) +1) :: ch     ! the string to be returned 
    if(l) then
     ch =   str(r8,real_len(r8)) // "T"
    else
     ch =   str(r8,real_len(r8)) // "F"
    end if
  end function

  function l_array_concat(l) result(ch)
    ! Convert the array into a string of numbers that produces a command that will be interpreted as a vector in R
    ! length of string is 2*array_size + 2 ... this includes commas and close brackets
    implicit none
    logical(4), intent(in) :: l(:)             ! the input logical variable
    character(len = 2*size(l) +2) :: ch     ! the string to be returned 
    integer :: i ! loop counter

    ch = "c(" ! openbracket
    do i = 1,size(l)
      if(l(i)) then
        ch(2*i+1:2*i+1) = 'T' ! array value
      else
        ch(2*i+1:2*i+1) = 'F'
      end if
      ch(2*i+2:2*i+2) = ',' ! separator
    end do
    ch(2*size(l)+2:2*size(l)+2) = ')' ! close bracket
  end function

  function r4_array_concat(r) result(ch)
    ! Convert the array into a string of numbers that produces a command that will be interpreted as a vector in R
    ! length of string is sum(real string length) + 
    ! (number of ints -1 (for commas)) + 2 brackets + (1 for the leading 'c')...
    !  this includes commas and close brackets
    implicit none
    real(4), intent(in) :: r(:)  ! the input real variables
    character(len = sum(real_len(r)) + size(r) +2) :: ch     ! the string to be returned 
    integer :: i ! loop counter
    
    ch = "c(" ! openbracket
    do i = 1,size(r)
      ch = trim(ch) // str(r(i),real_len(r(i))) // ","
    end do
    ch(len_trim(ch):len_trim(ch)) = ')' ! close bracket
  end function

  function r8_array_concat(r) result(ch)
    ! Convert the array into a string of numbers that produces a command that will be interpreted as a vector in R
    ! length of string is sum(real string length) + 
    ! (number of ints -1 (for commas)) + 2 brackets + (1 for the leading 'c')... this includes commas and close brackets
    implicit none
    real(8), intent(in) :: r(:)  ! the input real variables
    character(len =  sum(real_len(r)) + size(r) +2) :: ch     ! the string to be returned 
    integer :: i ! loop counter
    ch = "c(" ! openbracket
    do i = 1,size(r)
      ch = trim(ch) // str(r(i),real_len(r(i))) // ","
    end do
    ch(len_trim(ch):len_trim(ch)) = ')' ! close bracket
  end function

  function i4_array_concat(i4) result(ch)
     ! Convert the array into a string of numbers that produces a command that will be interpreted as a vector in R
     ! length of string is sum(integer string length) 
     ! + (number of ints -1 (for commas)) + 2 brackets + (1 for the leading 'c')... this includes commas and close brackets
     implicit none
     integer(4), intent(in) :: i4(:)  ! the input integer variables
     character(len = sum(int_len(i4))+size(i4)+2) :: ch     ! the string to be returned 
     integer :: i ! loop counter
 
     ch = "c(" ! openbracket
     do i = 1,size(i4)
       ch = trim(ch) // trim(str(i4(i),int_len(i4(i)))) // ","
     end do
     ch(len_trim(ch):len_trim(ch)) = ')' ! close bracket
   end function
   
  function str_array_concat(str) result(ch)
    ! Convert the string array and produces a command that will be interpreted as a vector in R
    ! length of string is (len(str)+1*array_size + 2 ... this includes commas and close brackets
    implicit none
    character(len=*), dimension(:), intent(in) :: str ! the input variable to be concatenated
    character(len = (len(str)+3)*size(str) +3) :: ch     ! the string to be returned 
    integer :: i ! loop counter

    ch = "c(" ! openbracket
    do i = 1,size(str)
      ch=trim(ch)//"'"//trim(str(i))//"',"
    end do
    ch(len_trim(ch):len_trim(ch)) = ')' ! close bracket
    ch=trim(ch)
  end function
!!! PADDING - COPIED FROM ABOVE + 1 EXTRA SPACE FOR A PAD IN THE MIDDLE

  function str_pad_str_concat(s1,s2) result(ch)
    ! Concatenate a string and a string
    ! exact length + 1 padding in the middle
    implicit none
    character(len=*), intent(in) :: s1    ! the input string
    character(len=*), intent(in) :: s2    ! the input string
    character(len = len(s1)+len(s2)+1) :: ch     ! the string to be returned 
    ch = s1 // pad_ch // s2
  end function

  function l_pad_str_concat(l,s) result(ch)
    ! Concatenate an logical and a string
    ! exact length + 1 padding in the middle
    implicit none
    logical(4), intent(in) :: l             ! the input logical variable
    character(len=*), intent(in) :: s    ! the input string
    character(len = len(s)+1+1) :: ch     ! the string to be returned 
    if(l) then
        ch = "T" // pad_ch // s
    else
        ch = "F" // pad_ch // s
    end if
  end function

  function str_pad_l_concat(s,l) result(ch)
    ! Concatenate an logical and a string
    ! exact length + 1 padding in the middle
    implicit none
    logical(4), intent(in) :: l             ! the input logical variable
    character(len=*), intent(in) :: s    ! the input string
    character(len = len(s)+1+1) :: ch     ! the string to be returned
    if(l) then
      ch =  s // pad_ch // "T"
    else
      ch =  s // pad_ch // "F"
    end if
  end function

  function l_pad_l_concat(l1,l2) result(ch)
    ! Concatenate an logical and a logical
    implicit none
    logical(4), intent(in) :: l1,l2             ! the input logical variables
    character(len = 2+1) :: ch     ! the string to be returned 
    if(l1.AND.l2) then
       ch =  "T"  // pad_ch // "T"
    elseif(l1.AND..NOT.l2) then
       ch =  "T"  // pad_ch // "F"
    elseif(.NOT.l1.AND.l2) then
       ch =  "F"  // pad_ch // "T"
    else
       ch =  "F"  // pad_ch // "F"
    end if
  end function

  function r4_pad_str_concat(r,s) result(ch)
    ! Concatenate a real and a string
    implicit none
    real(4), intent(in) :: r             ! the input real variable
    character(len=*), intent(in) :: s    ! the input string
    character(len = len(s)+real_len(r)+1) :: ch     ! the string to be returned (upto 16 places for real)
    ch = str(r,real_len(r)) // pad_ch // s
  end function

  function str_pad_r4_concat(s,r) result(ch)
    ! Concatenate an real and a string
    implicit none
    real(4), intent(in) :: r             ! the input real variable
    character(len=*), intent(in) :: s    ! the input string
    character(len = len(s)+real_len(r)+1) :: ch     ! the string to be returned (upto 16 places for real)
    ch = s // pad_ch // str(r,real_len(r))
  end function

  function r4_pad_r4_concat(r1,r2) result(ch)
    ! Concatenate two reals
    implicit none
    real(4), intent(in) :: r1,r2         ! the input real variables
    character(len = real_len(r1)+real_len(r2)+1) :: ch     ! the string to be returned (upto 16 places for real)
    ch = str(r1,real_len(r1)) // pad_ch // str(r2,real_len(r2))
  end function

  function r8_pad_str_concat(r,s) result(ch)
    ! Concatenate an real and a string
    implicit none
    real(8), intent(in) :: r             ! the input real variable
    character(len=*), intent(in) :: s    ! the input string
    character(len = len(s)+real_len(r)+1) :: ch     ! the string to be returned (upto 16 places for real)
    ch = str(r,real_len(r)) // pad_ch // s
  end function

  function str_pad_r8_concat(s,r) result(ch)
    ! Concatenate an real and a string
    implicit none
    real(8), intent(in) :: r             ! the input real variable
    character(len=*), intent(in) :: s    ! the input string
    character(len = len(s)+real_len(r)+1) :: ch     ! the string to be returned (upto 16 places for real)
    ch = s // pad_ch // str(r,real_len(r))
  end function

  function r8_pad_r8_concat(r1,r2) result(ch)
    ! Concatenate two reals
    ! Result will be exact length + 1 extra padding
    implicit none
    real(8), intent(in) :: r1,r2         ! the input real variables
    character(len = real_len(r1)+real_len(r2)+1) :: ch     ! the string to be returned (upto 16 places for real)
    ch = str(r1,real_len(r1)) // pad_ch // str(r2,real_len(r2))
  end function

  function i_pad_str_concat(i,s) result(ch)
    ! Concatenate an integer and a string
    ! computes exact length of resultant string, 1 extra padding
    implicit none
    integer(4), intent(in) :: i     ! the input integer variable
    character(len=*), intent(in) :: s     ! the input string
    character(len = int_len(i)+len(s)+1) :: ch     ! the string to be returned (exact size is calculated)
    ch = str(i,int_len(i)) // pad_ch // s
  end function

  function str_pad_i_concat(s,i) result(ch)
    ! Concatenate a string and an integer
    ! computes exact length of resultant string, 1 extra padding
    implicit none
    integer(4), intent(in) :: i     ! the input integer variable
    character(len=*), intent(in) :: s     ! the input string
    character(len = int_len(i)+len(s)+1) :: ch     ! the string to be returned (exact size is calculated)
    ch = s // pad_ch // str(i,int_len(i))
  end function

  function i_pad_i_concat(i,j) result(ch)
    ! Concatenate an integer and an integer
    ! computes exact length of resultant string, 1 extra padding
    implicit none
    integer(4), intent(in) :: i     ! the input integer variable
    integer(4), intent(in) :: j     ! the input integer variable
    character(len = int_len(i)+int_len(j)+1) :: ch     ! the string to be returned (exact size is calculated)
    ch =  str(i,int_len(i)) // pad_ch //  str(j,int_len(j))
  end function

  function i_pad_r8_concat(i,r) result(ch)
    ! Concatenate an integer and a double precision
    ! computes exact length of resultant string, 1 extra padding
    implicit none
    integer(4), intent(in) :: i     ! the input integer variable
    real(8), intent(in) :: r        ! the input real variable
    character(len = int_len(i) + real_len(r)+1) :: ch     ! the string to be returned (exact size is calculated)
    ch =  str(i,int_len(i)) // pad_ch //  str(r,real_len(r))
  end function

  function r8_pad_i_concat(r,i) result(ch)
    ! Concatenate an integer and a double precision
    ! computes exact length of resultant string, 1 extra padding
    implicit none
    integer(4), intent(in) :: i     ! the input integer variable
    real(8), intent(in) :: r        ! the input real variable
    character(len = int_len(i) + real_len(r)+1) :: ch     ! the string to be returned (exact size is calculated)
    ch =  str(i,int_len(i)) // pad_ch //  str(r,real_len(r))
  end function

  function i_pad_r4_concat(i,r) result(ch)
    ! Concatenate an integer and a double precision
    ! computes exact length of resultant string, 1 extra padding
    implicit none
    integer(4), intent(in) :: i     ! the input integer variable
    real(4), intent(in) :: r        ! the input real variable
    character(len = int_len(i) + real_len(r)+1) :: ch     ! the string to be returned (exact size is calculated)
    ch =  str(i,int_len(i)) // pad_ch //  str(r,real_len(r))
  end function

  function r4_pad_i_concat(r,i) result(ch)
    ! Concatenate an integer and a double precision
    ! computes exact length of resultant string, 1 extra padding
    implicit none
    integer(4), intent(in) :: i     ! the input integer variable
    real(4), intent(in) :: r        ! the input real variable
    character(len = int_len(i) + real_len(r)+1) :: ch     ! the string to be returned (exact size is calculated)
    ch =  str(i,int_len(i)) // pad_ch //   str(r,real_len(r))
  end function

  function r4_pad_r8_concat(r4,r8) result(ch)
    ! Concatenate an two reals
    ! Result will be exact length + 1 extra padding
    implicit none
    real(4), intent(in) :: r4  ! the input real variables
    real(8), intent(in) :: r8
    character(len = real_len(r4)+real_len(r8)+1) :: ch     ! the string to be returned (upto 16 places for real)
    ch =  str(r4,real_len(r4)) // pad_ch //   str(r8,real_len(r8))
  end function

  function r8_pad_r4_concat(r8,r4) result(ch)
    ! Concatenate two reals
    ! Result will be exact length + 1 extra padding
    implicit none
    real(4), intent(in) :: r4  ! the input real variables
    real(8), intent(in) :: r8
    character(len = real_len(r4)+real_len(r8)+1) :: ch     ! the string to be returned (upto 16 places for real)
    ch =  str(r8,real_len(r8)) // pad_ch //  str(r4,real_len(r4))
  end function

  function l_pad_i_concat(l,i) result(ch)
    ! Concatenate an logical and an integer
    ! exact length + 1 padding in the middle
    implicit none
    logical(4), intent(in) :: l             ! the input logical variable
    integer(4), intent(in) :: i     ! the input integer variable
    character(len = int_len(i) +1+1) :: ch     ! the string to be returned 
    if(l) then
     ch = "T" // pad_ch // str(i,int_len(i))
    else
     ch = "F" // pad_ch // str(i,int_len(i))
    end if
  end function

  function i_pad_l_concat(i,l) result(ch)
    ! Concatenate an logical and an integer
    ! exact length + 1 padding in the middle
    implicit none
    logical(4), intent(in) :: l             ! the input logical variable
    integer(4), intent(in) :: i     ! the input integer variable
    character(len = int_len(i) +1+1) :: ch     ! the string to be returned 
    if(l) then
     ch =   str(i,int_len(i)) // pad_ch // "T"
    else
     ch =   str(i,int_len(i)) // pad_ch // "F"
    end if
  end function

  function l_pad_r4_concat(l,r4) result(ch)
    ! Concatenate an logical and a real
    ! exact length + 1 padding in the middle
    implicit none
    logical(4), intent(in) :: l             ! the input logical variable
    real(4), intent(in) :: r4  ! the input real variables
    character(len = real_len(r4) +1+1) :: ch     ! the string to be returned 
    if(l) then
      ch = "T" // pad_ch //  str(r4,real_len(r4))
    else
      ch = "F" // pad_ch //  str(r4,real_len(r4))
    end if
  end function

  function r4_pad_l_concat(r4,l) result(ch)
    ! Concatenate an logical and a real
    ! exact length + 1 padding in the middle
    implicit none
    logical(4), intent(in) :: l             ! the input logical variable
    real(4), intent(in) :: r4  ! the input real variables
    character(len = real_len(r4) +1+1) :: ch     ! the string to be returned 
    if(l) then
      ch =   str(r4,real_len(r4)) // pad_ch // "T"
    else
      ch =   str(r4,real_len(r4)) // pad_ch // "F"
    end if
  end function

  function l_pad_r8_concat(l,r8) result(ch)
    ! Concatenate an logical and a real
    ! exact length + 1 padding in the middle
    implicit none
    logical(4), intent(in) :: l             ! the input logical variable
    real(8), intent(in) :: r8  ! the input real variables
    character(len = real_len(r8) +1+1) :: ch     ! the string to be returned 
    if(l) then
     ch = "T" // pad_ch // str(r8,real_len(r8))
    else
     ch = "F" // pad_ch // str(r8,real_len(r8))
    end if
  end function

  function r8_pad_l_concat(r8,l) result(ch)
    ! Concatenate an logical and a real
    ! exact length + 1 padding in the middle
    implicit none
    logical(4), intent(in) :: l             ! the input logical variable
    real(8), intent(in) :: r8  ! the input real variables
    character(len = real_len(r8) +1+1) :: ch     ! the string to be returned 
    if(l) then
     ch =   str(r8,real_len(r8)) // pad_ch // "T"
    else
     ch =   str(r8,real_len(r8)) // pad_ch // "F"
    end if
  end function

  elemental function int_len(i) result(len)
    ! determines the exact length of integer to string conversions
    implicit none
    integer, intent(in) :: i ! input integer
    integer :: len           ! string length

    if(i==0) then ! a zero causes the algorithm below to crash
      len = 1 ! a single digit
    else
      len = int(log10(abs(real(i,4)))) + 1 ! the number of digits in the string
    end if

    if(i<0) then
      len = len + 1  ! allow for negative sign
    end if
  end function
  
  elemental function r8_len(r) result(len)
    ! determines the exact length of integer to string conversions
    implicit none
    real(8), intent(in) :: r ! input integer
    integer :: len           ! string length
    character(len = 30) :: temp  ! temporary variable (using CVF string needs to be atleast len = 24 to avoid crash)

    write(temp,*) r               ! convert the number
    len = len_trim(adjustl(temp)) ! determine the size of the string

  end function

  elemental function r4_len(r) result(len)
    ! determines the exact length of integer to string conversions
    implicit none
    real(4), intent(in) :: r ! input integer
    integer :: len           ! string length
    character(len = 30) :: temp  ! temporary variable (using CVF string needs to be atleast len = 24 to avoid crash)

    write(temp,*) r               ! convert the number
    len = len_trim(adjustl(temp)) ! determine the size of the string

  end function

  subroutine set_pad(pad)
    ! changes the padding character used in padded concatenation
    implicit none
    character(len=1), intent(in) :: pad
    pad_ch = pad
  end subroutine

  function fwdslash(strIn) result(strOut)
      ! converts backslashes to forward slashes because the R interpreter doesn't like backslashes
      implicit none
      character(len = *), intent(in) :: strIn
      character(len = len(strIn)) :: strOut
      integer :: i

      strOut = strIn
      do i = 1,len_trim(strOut)
        if(strOut(i:i)=="\") strOut(i:i)="/"
      end do
   end function

  function backslash(strIn) result(strOut)
      ! converts forwardslashes to back slashes because system function doesn't like fwdslashes
      implicit none
      character(len = *), intent(in) :: strIn
      character(len = len(strIn)) :: strOut
      integer :: i

      strOut = strIn
      do i = 1,len_trim(strOut)
        if(strOut(i:i)=="/") strOut(i:i)="\"
      end do
   end function

  function FolderUp(strIn,n) result(strOut)
      ! For a filepath string, move up 'n' folder levels
      implicit none
      character(len = *), intent(in) :: strIn   ! filepath
      integer, intent(in) :: n                  ! number of folder levels to move up
      character(len = len(strIn)) :: strOut
      integer :: i,k
      character(len=1)  :: slash

      strOut = StrIn
      k = len_trim(strOut)
      slash="/" ! presume string has forward slashes
      if(index(strOut(1:k),slash)==0) slash="\" ! there are no forward slashes so switch to backslashes

      do i = 1,n
        if(strOut(k:k)==slash) k = k-1
        k = index(strOut(1:k),slash,back=.true.)
        strOut = strOut(1:k)
      end do
 
   end function
   
  function relPathtoAbsPath(relPath,currentDir) result(absPath)
      ! Change a relative path to an absolute path, using current directory location
      implicit none
      character(len = *), intent(in) :: relPath    ! relative filepath
      character(len = *), intent(in) :: currentDir ! current Directory Location
      character(len = (len_trim(relPath)+len_trim(currentdir))) :: absPath   ! filepath
      !Locals
      integer :: i
      character(len = len(relpath)) :: CopyRelPath
      CopyRelPath=relPath
      i=0
      DO WHILE (copyRelPath(1:2)=="..")
        i=i+1 
        copyRelPath=copyRelPath(4:len_trim(copyRelPath))
     end do
     if (i>0) then
        abspath=folderup(CurrentDir,n=i)
        abspath=trim(abspath)//trim(copyrelPath)
     else
      abspath=relpath
     end if
   end function
  
  function Lcase(strIn) result(strOut)
      ! converts to lower case
      implicit none
      character(len = *), intent(in) :: strIn
      character(len = len(strIn)) :: strOut
      integer :: k,lb,ub
      integer :: i
      lb = ichar('A') ! lower and upper bounds on character chart
      ub = ichar('Z')
      strOut = strIn
      do i = 1,len_trim(strOut)
      k = ichar(strOut(i:i))
        if(k>=lb.and.k<=ub) strOut(i:i) = achar(k + 32) ! convert to lower case
      end do
   end function

  function Ucase(strIn) result(strOut)
      ! converts to upper case
      implicit none
      character(len = *), intent(in) :: strIn
      character(len = len(strIn)) :: strOut
      integer :: k,lb,ub
      integer :: i
      lb = ichar('a') ! lower and upper bounds on character chart
      ub = ichar('z')
      strOut = strIn
      do i = 1,len_trim(strOut)
      k = ichar(strOut(i:i))
        if(k>=lb.and.k<=ub) strOut(i:i) = achar(k - 32) ! convert to upper case
      end do
   end function
   
  function insertString_end(strIn,insert) result(strOut)
    ! Inserts a string on the end of string - don't concatenate
    implicit none
    character(len=*):: strIn,insert
    character(len=len(strIn)):: strOut
    integer :: lenStr,lenInsert
    strOut=strIn
    lenStr=len(strIn)
    lenInsert=len(insert)
    strOut((lenStr-lenInsert):lenStr)=insert
   end function
!-------------------------------------------------------------------------
function removeChar(strIn,char) result (strOut)
! Removes given character from the input string

    IMPLICIT NONE

    ! Dummy Arguments
    CHARACTER(LEN=*), INTENT(IN) :: strIn
    CHARACTER(LEN=1), INTENT(IN) :: char
    
     character(len=len(strIn)):: strOut

    ! Local Variables
    INTEGER :: i,k
    
    ! Remove Char    
    k=0
    DO i=1,LEN_TRIM(strIn)
        IF(strIn(i:i)/=char) THEN
            k=k+1
            strOut(k:k)=strIn(i:i)  
        END IF
    END DO

    ! Place Spaces at end of string
    DO i=k+1,LEN_TRIM(strOut)
        strOut(i:i)=' '
    END DO

END function removeChar
!_____________________________________________________________________________________________
!
CHARACTER FUNCTION stripBlanks(inString)
  IMPLICIT NONE
  CHARACTER(LEN=*),INTENT(IN)::inString      
  INTEGER::i,indx
  !---
  !
  indx=0
  stripBlanks=''
  DO i=1,LEN_TRIM(inString)
     IF(inString(i:i)==' ')CYCLE
     indx=indx+1
     stripBlanks(indx:indx)=inString(i:i)
  END DO

END FUNCTION stripBlanks
!_____________________________________________________________________________________________
!
function changeChar(strIn,charIn,CharOut) result (strOut)
! Changes all occurences of CharIn in StrIn to CharOut

    IMPLICIT NONE

    ! Dummy Arguments
    CHARACTER(LEN=*), INTENT(IN) :: strIn
    CHARACTER(LEN=1), INTENT(IN) :: charIn,CharOut
        
    character(len=len_trim(strIn)):: strOut

    ! Local Variables
    INTEGER :: i
    
    ! Remove Char    
    DO i=1,LEN_TRIM(strIn)
        IF(strIn(i:i)==charIn) THEN
          strOut(i:i)=charOut
        Else
          strOut(i:i)=strIn(i:i)
        eND IF
    END DO

END function ChangeChar
!_____________________________________________________________________________________________
function index_1D(strVec,str)
    
    use kinds_dmsl_kit

    IMPLICIT NONE

    ! Dummy Arguments
    CHARACTER(LEN=*), INTENT(IN) :: strVec(:),Str
        
    ! Local Variables
    INTEGER(mik) :: i,testVec(size(strVec)),test(1),n
    
    ! Function Definition
    integer(mik) :: index_1D
    n=SIZE(strVec)
        
    If (all(StrVec/=Str)) then
        index_1D=0
        return
    end if
    
    testVec=(/(i,i=1,n)/)
    test=PACK(testVec,StrVec==Str)
    
    index_1D=test(1)      

end function index_1D
!_____________________________________________________________________________________________
function insertString(strIn) result(outString)
    !use MUtilsLib_varFuncs,only :checkPresent
    use kinds_dmsl_kit

    IMPLICIT NONE

    ! Dummy Arguments
    CHARACTER(LEN=*), INTENT(IN) :: strIn
    !CHARACTER(LEN=*), INTENT(IN), optional :: insertStr
    !integer(mik),intent(in), optional :: strlen
        
    ! Local Variables
    INTEGER(mik) :: i,lenInsert,j,k
    character(len=len_shortStr) :: insertStrLc
    integer(mik) :: strlenLc
    
    ! Function Definition
    CHARACTER(LEN=len_vLongStr) :: outString 
            
    insertStrlc="\n"C
    strlenLc=72
    outString=""
    lenInsert=len_trim(insertStrlc)
    j=1
    i=1
    do while (i<=len_trim(strIn))
        if (mod(i,strlenLc)==0) then
            k=0
            do while (strIn(i-k:i-k)/=" "); 
                k=k+1
            end do
            outString(j-k:j-k+lenInsert)=trim(insertStrlc)
            j=j-k+lenInsert
            outString(j:j+k)=strIn(i-k:i)
            j=j+k
        end if
        outString(j:j)=StrIn(i:i)
        j=j+1
        i=i+1
    end do
end function insertString

end module MUtilsLib_stringfuncs
!-------------------------------------------------------------------------
!*****************************************************************************
! For backwards compatibility
module stringfuncs
use MUtilsLib_stringfuncs, only :str,    &              ! this is just to make it easier
                                                        ! to build up string expressions for passing numbers into R
            operator(//), &        ! overloaded to allow concatenation of strings
            operator(.pad.), &     ! same as above but allows one padding space between concatenation
            c,            &        ! converts arrays to strings
            set_pad, &             ! change the padding character when using the .pad. operator
            fwdslash,backslash, &  ! convert a <filepath> string with back/fwd slashes to having forward/back slashes
            FolderUp, &            ! convert a <filepath> string be removing trailing folders
            Lcase, Ucase,&         ! convert string to lower/upper case (important for string comparisons)
            int,&                  ! Convert a string to integer
            insertString_end, &    ! Insert a string on the end
            removeChar, &          ! Remove Character from string
            relPathtoAbsPath, &    ! Convert a string with a relative path to an absolute path use for Rsetwd command
            stripBlanks,&          ! simple string function to remove blank spaces
            changeChar             ! string function to changes a character in string to another 
                                   ! (useful for changing spaces to underscores)

end module stringfuncs
!**********************************************

 

module MUtilsLib
    use MUtilsLib_Messagelog
    use MUtilsLib_System
end module