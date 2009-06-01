!<BEGIN SOURCE FILE HEADER>
!<END SOURCE FILE HEADER>
!***************************************************************************************************
! miniDMSL 
! - Developed by Mark Thyer
! A VERY cut-down version of DMSL library developed by Dmitri Kavetski.
! (C) Copyright 2000,2001,2002,2003,2004,2005,2006,2007,2008,2009. Dmitri Kavetski. All rights reserved.
!
! Based on Version ?? of DMSL  (need to add to remain consistent)
!
! Enables 1. Compilation independent of DMSL
!         2. Consistency when compiling when using DMSL
!
module kinds_dmsl_kit
! Variable kind parameters 
  implicit none
  integer,     parameter::mrk=selected_real_kind(p=8)   ! real kind (4=single,8=double,16=quad)
  integer,     parameter::mik=selected_int_kind(r=8)    ! integer kind
  integer,     parameter::mck=kind((1._mrk,1._mrk))     ! complex kind
  integer,     parameter::mlk=kind(.true.)

  real(mrk),   parameter::protoRe=1._mrk                ! prototype of real(mrk) number
  real(mrk),   parameter::hugeRe=huge(protoRe)          ! largest real on machine
  
  ! (5) Special DMSL values, conventionally used to flag un-initialised variables
  real(mrk),   parameter::undefRN=-999999999._mrk      ! flag for undefined real numbers
  integer(mik),parameter::undefIN=-999999999           ! flag for undefined integer numbers
  logical(mlk),parameter::undefLG=.false.              ! flag for undefined logicals
  ! complex(mck),parameter::undefCZ=(undefRN,undefRN)    ! flag for undefined complex numbers
  complex(mck),parameter::undefCZ=(-999999999._mrk,-999999999._mrk) ! flag for undefined complex numbers
  character(*),parameter::undefCH="undefined"          ! flag for undefined character strings
  
 ! (7) DMSL-wide registered string lengths
  integer(mik),parameter::len_vShortStr=3   ! usually for quick settings via single args
  integer(mik),parameter::len_shortStr=8    ! shortish names
  integer(mik),parameter::len_stdStr=16     ! typical-sized descriptive names
  integer(mik),parameter::len_longStr=128   ! typical line-long string
  integer(mik),parameter::len_vLongStr=1024 ! usually for file paths or very long messages
  
end module kinds_dmsl_kit
!---------------------------------------
module utilities_dmsl_kit
use kinds_dmsl_kit
! 0a. FREQUENTLY USED MATHEMATICAL CONSTANTS (with 50-digit Maple precision)
!--common numbers
real(mrk),parameter::zero=0._mrk,one=1._mrk,two=2._mrk,three=3._mrk,four=4._mrk,&
  five=5._mrk,six=6._mrk,seven=7._mrk,eight=8._mrk,nine=9._mrk,ten=10._mrk
real(mrk),parameter::half=0.5_mrk,oneThird=one/three,twoThirds=two/three,oneQuarter=one/four
complex(mck),parameter::zeroC=(0._mrk,0._mrk)

contains
!---------------------------------------
pure function average(nv1)
! Purpose: returns average of vector
! Source: Copied from DMSL  Version: 3.17 - 23 September 2004 ad
!         File: utilities_dmsl_kit.f90, average_rn1
! Due to Error: LNK2019: unresolved external symbol _UTILITIES_DMSL_KIT_mp_AVERAGE_RN1@4 referenced in function _probLIMMOD_mp_CALCprobLIMARRAY
implicit none
real(mrk),intent(in)::nv1(:)
real(mrk)::average
! locals
integer(mik)::n
! Start procedure here
n=size(nv1)
average=sum(nv1)/real(n,mrk)
! End procedure here
endfunction average
!----------------------------------------------------
end module utilities_dmsl_kit
!---------------------------------------
module numerix_dmsl_kit
use kinds_dmsl_kit

interface getmeanvar
  module procedure getmeanvar_s
endinterface getmeanvar

contains
!----------------------------------------------------------------------
pure subroutine getmeanvar_s(x,mean,var,method,err,message)
! Purpose: Computes the mean and variance of a sample of a scalar random variable.
! IN:  vector x(1:ns), method="f"-fast scheme,"c"-classic,else 2-pass
! OUT: mean "mean" and variance "var"
! Comments:
! 1. When size(x) is very large, it is possible to overflow the
!    stack for intermediate computations of deviations, since on-the-fly
!    arrays go to the stack. In this case, need to introduce allocatable
!    arrays to store intermediate results on the heap
implicit none
! Dummies
real(mrk),intent(in)::x(:)
real(mrk),intent(out)::mean
real(mrk),intent(out),optional::var
character(*),intent(in)::method
integer(mik),intent(out)::err
character(*),intent(out)::message
! Locals
real(mrk),allocatable::s(:)
integer(mik)::ns
real(mrk)::nsr
! Start procedure here
err=0; message="getmeanvar_s/ok"; ns=size(x); nsr=real(ns,mrk)
mean=sum(x(:))/nsr ! get approximate expectation of x first
if(present(var))then ! get approximate variance if requested
  if(ns==1)then
    err=-100; var=hugeRe; message="w-getmeanvar_s/singleSample"; return
  endif
  selectcase(method)
  case("f")     ! fast scheme:    var[x]=E[x2]-E[x].E[x]
    var=sum(x(:)**2)/real(ns-1,mrk)-mean**2
  case("c")     ! classic scheme: var[x]=E[(x-mean)^2]
    var=sum((x(:)-mean)**2)/real(ns-1,mrk)
  case default  ! 2-pass formula to minimise round-off errors
    allocate(s(ns),stat=err)
    if(err/=0)then
      err=10;message="f-getmeanvar_s/allocError";return
    endif
    s(:)=x(:)-mean ! compute deviations for each point
    var=(sum(s(:)**2)-sum(s(:))**2/nsr)/real(ns-1,mrk)
    deallocate(s,stat=err)
    if(err/=0)then
      err=20;message="f-getmeanvar_s/deAllocError";return
    endif
  endselect
endif
! End procedure here
endsubroutine getmeanvar_s
!----------------------------------------------------
end module numerix_dmsl_kit
!******************
module iotools_dmsl_mod

integer(4),parameter::normalFinish=0

end module iotools_dmsl_mod
