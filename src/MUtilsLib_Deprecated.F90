  module MUtilsLib_Deprecated
  ! Contains deprecated features of MUtilsLiB
  implicit none
  
  
  ! Message Log
  ! Enumeration for different tag types (public) - retained for backwards compatibility,
  ! but obselete becomes $ prefix is not standard F95
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
                                   
  end module MUtilsLib_Deprecated
  