!***************************************************************************************************
! miniDMSL 
! - Developed by Mark Thyer
! A VERY cut-down version of DMSL library developed by Dmitri Kavetski.
! (C) Copyright 2000,2001,2002,2003,2004,2005,2006,2007,2008,2009. Dmitri Kavetski. All rights reserved.
!
! Based on Revision 33 of DMSL  (need to add to remain consistent)
!
! Enables 1. Compilation independent of DMSL
!         2. Consistency when compiling when using DMSL
!
!******************************************************************
! (C) Copyright 2000-2009  ---  Dmitri Kavetski  ---  All rights reserved
!******************************************************************
module kinds_dmsl_kit
! Purpose: a. Defines global numeric kinds for DMSL;
!          b. Contains machine precision information;
!          c. Contains global information for DMSL library support.
! This module is typically made globally available.
! ---
! Programmer: Dmitri Kavetski.
! 2000 - 2004
! Civil, Environmental Engineering and Surveying
! University of Newcastle, Callaghan, NSW 2308, Australia.
! 2004 - 2007
! Department of Civil and Environmental Engineering
! Princeton University, Princeton, NJ 08544, USA.
! 2007 - current
! Civil, Environmental Engineering and Surveying
! University of Newcastle, Callaghan, NSW 2308, Australia.
! ---
! Comments:
! 1. The log[] function (2b) may not compile on some compilers.
! 2. The complex constants may not compile on some compilers.
! 3. If the compiler prevents the direct definitions below, hardcode them
!    with at least 40 significant digits of precision.
implicit none
public
! ---
! (1) Parameterised numeric data types
! (a) Available precision
!     CVF/IVF reals: 4=single, 8=double, [IVF only 16=quad]; CVF/IVF integers: 4=short, 8=long
integer,     parameter::srk=selected_real_kind(p=4)   ! single precision
integer,     parameter::drk=selected_real_kind(p=8)   ! double precision
integer,     parameter::qrk=selected_real_kind(p=16)  ! quadruple precision
integer,     parameter::sik=selected_int_kind(r=4)    ! short integer
integer,     parameter::lik=selected_int_kind(r=8)    ! long integer (NB: NR-90 uses r=9)
! (b) Selected global precision in all DMSL units
integer,     parameter::mrk=drk                       ! global real kind
integer,     parameter::mik=lik                       ! global integer kind
real(mrk),   parameter::protoRe=1._mrk                ! prototype of real(mrk) number
integer(mik),parameter::protoInt=1_mik                ! prototype of integer(mik) number
integer,     parameter::mck=kind((1._mrk,1._mrk))     ! global complex kind
integer,     parameter::mlk=kind(.true.)              ! global logical kind
complex(mck),parameter::protoCmx=((1._mrk,1._mrk))    ! prototype of complex(mck) number
logical(mlk),parameter::protoLog=.true.               ! prototype of logical(mlk) number
character(1),parameter::protoCH="d"                   ! prototype of character
! (c) Compiler-specific info [best kept up to date, I guess ...]
integer(mik),parameter::mrkBy=mrk                     ! number of bytes to store protoRe
integer(mik),parameter::mikBy=mik                     ! number of bytes to store protoInt
integer(mik),parameter::mckBy=2*mrk                   ! number of bytes to store protoCmx
integer(mik),parameter::mlkBy=4                       ! number of bytes to store protoLog
! NB:
! On CVF compiler: mrk and mik also denote the number of bytes used to store the value,
!                  mlk requires 4 bytes storage
! single precision      =  32-bit   (4 bytes)
! double precision      =  64-bit   (8 bytes)
! quadruple precisison  = 128-bits (16 bytes)
! ---
! (2) Machine precision information
! (a) Intrinsix
real(mrk),   parameter::tinyRe=tiny(protoRe)          ! smallest real on machine
real(mrk),   parameter::epsRe= epsilon(protoRe)       ! normalised machine accuracy
real(mrk),   parameter::hugeRe=huge(protoRe)          ! largest real on machine
integer(mik),parameter::hugeInt= huge(protoInt)       ! largest integer on machine
real(mrk),   parameter::hugeIntR=real(hugeInt,mrk)    ! largest integer (real format)
! real(mrk),   parameter::hugeIntR=2.14748364700000E+009_mrk      ! Salford Software FTN95
! complex(mck),parameter::tinyC=(tinyRe,tinyRe)         ! smallest complex on machine
! complex(mck),parameter::epsC= (epsRe,epsRe)           ! complex machine precision
! complex(mck),parameter::hugeC=(hugeRe,hugeRe)         ! largest complex on machine
! (b) Functions of machine precision
integer(mik),parameter::minExpRei=minexponent(protoRe)  ! min exponent (int)  in machine base (usually radix=2)
real(mrk),   parameter::minExpRer=real(minExpRei,mrk)   ! min exponent (real) in machine base (usually radix=2)
! real(mrk),   parameter::minExpRer=-1.02100000000000E+003_mrk    ! Salford Software FTN95
integer(mik),parameter::maxExpRei=maxexponent(protoRe)  ! max exponent (int)  in machine base (usually radix=2)
real(mrk),   parameter::maxExpRer=real(maxExpRei,mrk)   ! max exponent (real) in machine base (usually radix=2)
! real(mrk),   parameter::maxExpRer=+1.02400000000000E+003_mrk    ! Salford Software FTN95
real(mrk),   parameter::radixRer=real(radix(protoRe),mrk)       ! radix expressed as real
! real(mrk),   parameter::radixRer=2.00000000000000E+000_mrk      ! Salford Software FTN95
! real(mrk),   parameter::nDecDigitsRe=-log10(epsRe)            ! number of decimal digits
real(mrk),   parameter::nDecDigitsRe=-log(epsRe)/log(10._mrk)   ! number of decimal digits
! real(mrk),   parameter::nDecDigitsRe=1.56535597745270E+001_mrk  ! Salford Software FTN95
real(mrk),   parameter::lnEpsRe=log(epsRe)                      ! ln[] of machine precision
! real(mrk),   parameter::lnEpsRe=3.60436533891172E+001_mrk       ! Salford Software FTN95
real(mrk),   parameter::lunflw=minExpRer*log(radixRer)                ! =log(tinyRe)  ! ln[] of smallest real
! real(mrk),   parameter::lunflw=-7.07703271351704E+002_mrk       ! Salford Software FTN95
real(mrk),   parameter::lovflw=(1._mrk-epsRe)*maxExpRer*log(radixRer) ! =log(hugeRe)  ! ln[] of largest real, safe to exponentiate
! real(mrk),   parameter::lovflw=+7.09782712893384E+002_mrk     ! Salford Software FTN95
! ---
! (3) Parameterised machine settings
integer(mik),parameter::keyboardUnit=5                ! keyboard unit (default input)
integer(mik),parameter::screenUnit=6                  ! screen unit   (default output)
! ---
! (4) Library support features
integer(mik),parameter::DMSL_vernum=424
character(*),parameter::DMSL_authorName="Dmitri Kavetski"
character(*),parameter::DMSL_authorEmail="dmitri.kavetski@gmail.com"
! ---
! (5) Special DMSL values, conventionally used to flag un-initialised variables
real(mrk),   parameter::undefRN=-999999999._mrk       ! flag for undefined real numbers
real(mrk),   parameter::undefRNSV(1)=(/undefRN/)      ! undefined real scalar vector
real(mrk),   parameter::undefRNH=-0.5_mrk*hugeRe      ! huge flag for undefined real numbers
integer(mik),parameter::undefIN=-999999999            ! flag for undefined integer numbers
integer(mik),parameter::undefINSV(1)=(/undefIN/)      ! undefined real scalar vector
integer(mik),parameter::undefINH=-hugeInt/2           ! huge flag for undefined integer numbers
logical(mlk),parameter::undefLG=.false.               ! flag for undefined logicals
! complex(mck),parameter::undefCZ=(undefRN,undefRN)     ! flag for undefined complex numbers
complex(mck),parameter::undefCZ=(-999999999._mrk,-999999999._mrk) ! flag for undefined complex numbers
! complex(mck),parameter::undefCZH=(undefRNH,undefRNH)  ! huge flag for undefined complex numbers
! complex(mck),parameter::undefCZH=cmplx(undefRNH,undefRNH,kind=mck)  ! huge flag for undefined complex numbers
character(*),parameter::undefCH="undefined"           ! flag for undefined character strings
character(*),parameter::undefCHSV(1)=(/undefCH/)      ! flag for undefined character strings
! ---
! (6) DMSL-wide registered settings
integer(mik),parameter::iyes=1,ino=0 ! integer flags for true/false
! ---
! (7) DMSL-wide registered string lengths
integer(mik),parameter::len_iShortStr=1     ! infrashort string
integer(mik),parameter::len_vShortStr=4     ! 2^2 : usually for quick settings via single args
integer(mik),parameter::len_shortStr=8      ! 2^3 : shortish names
integer(mik),parameter::len_stdStrB=16      ! 2^4 : brief descriptive names
integer(mik),parameter::len_stdStrD=32      ! 2^5 : detailed descriptive name
integer(mik),parameter::len_longStr=128     ! 2^7 : typical line-long string
integer(mik),parameter::len_vLongStr=1024   ! 2^10: usually for file paths or very long messages
integer(mik),parameter::len_hLongStr=65536  ! 2^15: hyperlong string (use with care)
! ---
! (8) DMSL-wide registered string length purposes
integer(mik),parameter::len_pathDMSL=len_vLongStr ! maximum length of filepaths
! ---
! (9) DMSL-wide software and hardware information
! (a) Software aspects
integer(mik),parameter::&   ! Types of applications
  CONSOLE_APPTYPE=0,QUICKWIN_APPTYPE =1,WINDOWS_APPTYPE  =2,&
  DLL_APPTYPE    =3,STATICLIB_APPTYPE=4,COMSERVER_APPTYPE=5
integer(mik),parameter::&   ! Compiler types
  FPS_COMPILER=-3,DVF_COMPILER=-2,CVF_COMPILER=-1,IVF_COMPILER=0
integer(mik),parameter::&   ! Application configuration types
  DEBUG_CONFIG=0,RELEASE_CONFIG=1
integer(mik),parameter::&   ! Operating system configuration types
  MAC_OS=-2,LINUX_OS=-1,WIN2K_OS=0,WINXP_OS=1,VISTA_OS=2
! (b) Hardware aspects
integer(mik),parameter::MY_INT_PTR_KIND=INT_PTR_KIND()
! 4 on 32-but bricks, 8 on 64-bit monsters, 16 on 128-bit beasts
! ---
endmodule kinds_dmsl_kit!---------------------------------------
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
!******************************************************************
! (C) Copyright 2009-2010  ---  Dmitri Kavetski  ---  All rights reserved
!******************************************************************
! From   v5.011.000 of DMSL
module intercom_dmsl_kit
! Purpose: Program/language intercommunication:
! (A) Defines global filepaths for
!     (1) DMSL components'
!     (2) RFortran and R scripts;
!     (3) RGui (associated with RFortran).
! (B) Provides language interoperability (COM, etc)
! --
! Programmer: Dmitri Kavetski & Mark Thyer, 2009
! Civil, Environmental Engineering and Surveying
! University of Newcastle, Callaghan, NSW 2308, Australia.
! --
! Comments:
! 1. The default values can be overwritten in calling apps.
! --
use kinds_dmsl_kit
implicit none
private;save
! 1. PATHS - deleted to save confusion!
! 2. PROCEDURE STATUS
public::myProcID,openedCOM
!----------------------------------------------------
integer(MY_INT_PTR_KIND)::myProcID=undefIN  ! procID of the current process
logical(mlk)::openedCOM=.false.             ! true when the COM server is already initialized
!----------------------------------------------------
!----------------------------------------------------
!----------------------------------------------------
endmodule intercom_dmsl_kit
!******************************************************************
