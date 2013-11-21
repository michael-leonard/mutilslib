!***************************************************************************************************
!> miniDMSL
!> Developed by Mark Thyer
!> A VERY cut-down version of DMSL library developed by Dmitri Kavetski.
!> Based on r402 of DMSL
!> Enables 1. Compilation independent of DMSL
!>         2. Consistency when compiling when using DMSL
!>******************************************************************
!> (C) Copyright 2000-2009  ---  Dmitri Kavetski  ---  All rights reserved
!>******************************************************************
!> Purpose: a. Defines global numeric kinds for DMSL;
!>          b. Contains machine precision information;
!>          c. Contains global information for DMSL library support.
!> This module is typically made globally available.
!> ---
!> Programmer: Dmitri Kavetski.
!> 2000 - 2004
!> Civil, Environmental Engineering and Surveying
!> University of Newcastle, Callaghan, NSW 2308, Australia.
!> 2004 - 2007
!> Department of Civil and Environmental Engineering
!> Princeton University, Princeton, NJ 08544, USA.
!> 2007 - current
!> Civil, Environmental Engineering and Surveying
!> University of Newcastle, Callaghan, NSW 2308, Australia.
!> ---
!> Comments:
!> 1. The log[] function (2b) may not compile on some compilers.
!> 2. The complex constants may not compile on some compilers.
!> 3. If the compiler prevents the direct definitions below, hardcode them
!>    with at least 40 significant digits of precision.
!******************************************************************
! (C) Copyright 2000-2020  ---  Dmitri Kavetski  ---  All rights reserved
!******************************************************************
module kinds_dmsl_kit
! Purpose: a. Defines global numeric kinds for DMSL;
!          b. Contains machine precision information;
!          c. Contains global information for DMSL library support.
! Comments:
!          1. This module is typically made globally available.
!          2. It may contain compiler-specific directives, etc.
! ---
! Programmer: Dmitri Kavetski.
! 2000 - 2004
! Civil, Environmental Engineering and Surveying
! University of Newcastle, Callaghan, NSW 2308, Australia.
! 2004 - 2007
! Department of Civil and Environmental Engineering
! Princeton University, Princeton, NJ 08544, USA.
! 2007 - current
! Environmental Engineering Grupo D
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
!     CVF/IVF reals:    4=single, 8=double, [IVF only: 16=quad]
!     CVF/IVF integers: 1=byte,   2=short,  4=long, 8=long-long
! -- available reals
integer,     parameter::srk=selected_real_kind(p=4)   ! srk=4:  single precision
integer,     parameter::drk=selected_real_kind(p=8)   ! drk=8:  double precision
integer,     parameter::qrk=selected_real_kind(p=16)  ! qrk=16: quadruple precision
! -- available integers
integer,     parameter::bik=selected_int_kind(r=2)    ! bik=1: byte integer: -128->127
integer,     parameter::sik=selected_int_kind(r=4)    ! sik=2: short integer: -32768->32767
integer,     parameter::lik=selected_int_kind(r=8)    ! lik=4: long (BOOL) integer: 2,147,483,647
integer,     parameter::hik=selected_int_kind(r=10)   ! hik=8: long-long integer (9x10^18)
! -- available logicals
! integer,     parameter::blk=1                         ! "Boolean" logical kind. NB: C BOOL=INTEGER(4)
integer,     parameter::slk=kind(.true.)              ! standard logical kind
! (b) Selected global precision in all DMSL units
integer,     parameter::mrk=drk                       ! global real kind
integer,     parameter::mik=lik                       ! global integer kind
real(mrk),   parameter::protoRe=1._mrk                ! prototype of real(mrk) number
integer(mik),parameter::protoInt=1_mik                ! prototype of integer(mik) number
integer,     parameter::mck=kind((1._mrk,1._mrk))     ! global complex kind
integer,     parameter::mlk=slk                       ! global logical kind
complex(mck),parameter::protoCmx=((1._mrk,1._mrk))    ! prototype of complex(mck) number
logical(mlk),parameter::protoLog=.true._mlk           ! prototype of logical(mlk) number
character(1),parameter::protoCH="d"                   ! prototype of character
! (c) Storage requirements
integer(mik),parameter::mrkBy=SIZEOF(protoRe)         ! number of bytes to store protoRe
integer(mik),parameter::mikBy=SIZEOF(protoInt)        ! number of bytes to store protoInt
integer(mik),parameter::mckBy=SIZEOF(protoCmx)        ! number of bytes to store protoCmx
integer(mik),parameter::mlkBy=SIZEOF(protoLog)        ! number of bytes to store protoLog
integer(mik),parameter::chaBy=SIZEOF(protoCH)         ! number of bytes to store protoCH
! NB: On CVF/IVF compiler:
!   _rk and _ik denote the number of bytes used to store the value,
!   slk requires 4 bytes storage
! single precision      =  32-bit   (4 bytes)
! double precision      =  64-bit   (8 bytes)
! quadruple precisison  = 128-bits (16 bytes)
! (c) DMSL-wide ID for data types (mimics DEC)
integer(mik),parameter::& !DK: also used in DEC array descriptors
  int1_iDtype=1,int2_iDtype=2,int4_iDtype=3,int8_iDtype=4,& ! integers (byte,short,long,longlong)
  log1_iDtype=5,log2_iDtype=6,log4_iDtype=7,log8_iDtype=8,& ! logicals
  real4_iDtype=9,real8_iDtype=10,real16_iDtype=11,&         ! reals (single,double,quadruple)
  cmx4_iDtype=12,cmx8_iDtype=13,&                           ! complex (single,double)
  char_iDtype=14,&                                          ! character
  rec_iDtype=15,&                                           ! record
  unk_iDtype=-666                                           ! unknown
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
! (4) Special DMSL values, conventionally used to flag un-initialised variables
real(mrk),   parameter::undefRN=-999999999._mrk       ! flag for undefined real numbers
real(mrk),   parameter::undefRNSV(1)=(/undefRN/)      ! undefined real scalar vector
real(mrk),   parameter::undefRNH=-0.5_mrk*hugeRe      ! huge flag for undefined real numbers
real(mrk),   parameter::undefRNHS=undefRNH/1.e9       ! 'safely' huge real number (good flag but unlikely to crush)
integer(mik),parameter::undefIN=-999999999            ! flag for undefined integer numbers
integer(mik),parameter::undefINS=-9                   ! flag for small undefined integer numbers
integer(mik),parameter::undefINSV(1)=(/undefIN/)      ! undefined real scalar vector
integer(mik),parameter::undefINH=-hugeInt/2           ! huge flag for undefined integer numbers
integer(bik),parameter::undefBIN=-8                   ! undefined flag for byte integers
logical(mlk),parameter::undefLG=.false.               ! flag for undefined logicals
! complex(mck),parameter::undefCZ=(undefRN,undefRN)     ! flag for undefined complex numbers
complex(mck),parameter::undefCZ=(-999999999._mrk,-999999999._mrk) ! flag for undefined complex numbers
! complex(mck),parameter::undefCZH=(undefRNH,undefRNH)  ! huge flag for undefined complex numbers
! complex(mck),parameter::undefCZH=cmplx(undefRNH,undefRNH,kind=mck)  ! huge flag for undefined complex numbers
character(*),parameter::undefCH="undefined"           ! flag for undefined character strings
character(*),parameter::undefCHSV(1)=(/undefCH/)      ! flag for undefined character strings
! ---
! (5) DMSL-wide registered settings
integer(mik),parameter::iyes=1,ino=0        ! yes/no integer values
integer(mik),parameter::iTRUE1=1,iFALSE0=0  ! true/false integer values
integer(lik),parameter::BTRUE1=1,BFALSE0=0  ! true/false Boolean integer values
integer(mik),parameter::NULL_IOBJ=0         ! NULL integer object
integer(mik),parameter::QC_PRESENT=0        ! quality code for present data
integer(mik),parameter::QC_MISSVAL=-6666    ! quality code for missing data
! ---
! (6) DMSL-wide registered string lengths
integer(mik),parameter::len_iShortStr=1     ! infrashort string
integer(mik),parameter::len_vShortStr=4     ! 2^2 : usually for quick settings via single args
integer(mik),parameter::len_shortStr=8      ! 2^3 : shortish names
integer(mik),parameter::len_stdStrB=16      ! 2^4 : brief descriptive names
integer(mik),parameter::len_stdStrD=32      ! 2^5 : detailed descriptive name
integer(mik),parameter::len_stdStrL=128     ! 2^7 : typical line-long string
integer(mik),parameter::len_longStr=256     ! 2^8 : long string
integer(mik),parameter::len_vLongStr=1024   ! 2^10: verylong string, usually for file paths or very long messages
integer(mik),parameter::len_uLongStr=8192   ! 2^13: ultralong string
integer(mik),parameter::len_hLongStr=65536  ! 2^15: hyperlong string (use with care)
! ---
! (7) DMSL-wide registered string length purposes
integer(mik),parameter::len_DMSLappName=len_stdStrL   ! max length of window name
integer(mik),parameter::len_DMSLpath=len_vLongStr     ! length of all DMSL filepaths
integer(mik),parameter::len_DMSLmessage=len_vLongStr  ! length of legitimate messages
integer(mik),parameter::len_DMSLjmsg=len_stdStrD      ! length of junky messages
integer(mik),parameter::len_DMSLwinArg=32768          ! max length of window arguments
!                                                       (http://msdn.microsoft.com/en-us/library/ms682425(VS.85).aspx)
! ---
! (8) DMSL-wide software and hardware information
! (a) Software aspects
integer(mik),parameter::EXIT_SUCCESS=0,EXIT_FAILURE=1
integer(mik),parameter::&   ! Types of applications
  CONSOLE_APPTYPE=0,QUICKWIN_APPTYPE =1,WINDOWS_APPTYPE  =2,&
  DLL_APPTYPE    =3,STATICLIB_APPTYPE=4,COMSERVER_APPTYPE=5
integer(mik),parameter::&   ! Compiler types
  FPS_COMPILER=-3,DVF_COMPILER=-2,CVF_COMPILER=-1,IVF_COMPILER=0
integer(mik),parameter::&   ! Application configuration types
  DEBUG_CONFIG=0,RELEASE_CONFIG=1
integer(mik),parameter::&   ! Operating system configuration types
  MAC_OS=-2,LINUX_OS=-1,WIN2K_OS=0,WINXP_OS=1,VISTA_OS=2
! (b) Platform-specific aspects
character(*),parameter::dirSepChar_win="\"      ! directory name separator
character(*),parameter::dirSepChar_lix="/"
character(*),parameter::cmdLineArgSymb_win="/"  ! command-line argument symbol
character(*),parameter::cmdLineArgSymb_lix="-"
!DEC$ IF DEFINED (_WIN32)   ! choose appropriate OS-specific parameters
  integer(mik),parameter::OS_build=WINXP_OS
  character(*),parameter::dirSepChar=dirSepChar_win
  character(*),parameter::cmdLineArgSymb=cmdLineArgSymb_win
!DEC$ ELSE
  integer(mik),parameter::OS_build=LINUX_OS
  character(*),parameter::dirSepChar=dirSepChar_lix
  character(*),parameter::cmdLineArgSymb=cmdLineArgSymb_lix
!DEC$ ENDIF
! (c) Runtime environment aspects (closely related to 'types of applications')
! integer(mik),parameter::&
!   CONSOLE_APPENVIRO=0,&     ! - Console         (windows=supported, linux=supported)
!   VISUALF_APPENVIRO=1,&     ! - Visual-enabled  (windows=supported, linux=not supported)
!   UNKNOWN_APPENVIRO=-666    ! - Unknown environment
! Visual support environment
integer(mik),parameter::ENABLED_visualDMSL=1,UNAVAIL_visualDMSL=0
! (d) Hardware aspects
integer(mik),parameter::MY_INT_PTR_KIND=INT_PTR_KIND()
! 4 on 32-bit bricks, 8 on 64-bit monsters, 16 on 128-bit non-existent (2009AD) beasts
! ---
! (9) Generic action parameters
integer(mik),parameter::dont_action=-1,askk_action=0,dooo_action=+1
! ---
! (10) Library support features
integer(mik),parameter::DMSL_vernum=5034000       ! X.YYY.ZZZ format; X=super-version, Y=major version, Z=micro-version
character(*),parameter::DMSL_vernumCH="5.034.000" ! as string
character(*),parameter::DMSL_subverRevCH=undefCH  ! Subversion revision as string
character(*),parameter::DMSL_authorName="Dmitri Kavetski"
character(*),parameter::DMSL_authorFName=DMSL_authorName//"and contributors"
character(*),parameter::DMSL_verstrCH="DMSL - Version "//DMSL_vernumCH
character(*),parameter::DMSL_yearegCH=" (C) 2000-2011"
character(*),parameter::DMSL_authorEmail="dmitri.kavetski@gmail.com"
! ---
endmodule kinds_dmsl_kit
!> Cut down version of utilities dmsl
module utilities_dmsl_kit
use kinds_dmsl_kit
private

public:: getSpareUnit
public:: splitPathFile
public:: parseStringIntoVector
public:: quickIf,quickConcat 
public:: average


!> FREQUENTLY USED MATHEMATICAL CONSTANTS (with 50-digit Maple precision)
real(mrk),parameter::zero=0._mrk,one=1._mrk,two=2._mrk,three=3._mrk,four=4._mrk,&
  five=5._mrk,six=6._mrk,seven=7._mrk,eight=8._mrk,nine=9._mrk,ten=10._mrk
real(mrk),parameter::half=0.5_mrk,oneThird=one/three,twoThirds=two/three,oneQuarter=one/four
complex(mck),parameter::zeroC=(0._mrk,0._mrk)


!--common chars
character(*),parameter::blankCH=" ",enullCH=""


interface getSpareUnit
  module procedure getSpareUnit_v,getSpareUnit_1,getSpareUnit_2,&
    getSpareUnit_3,getSpareUnit_4,getSpareUnit_5
endinterface getSpareUnit

!--
interface parseStringIntoVector
  module procedure parseStringIntoVector_delS,parseStringIntoVector_delSp
endinterface parseStringIntoVector
!--
interface quickIf
  module procedure quickIf_r1,quickIf_i1,quickIf_z1,quickIf_log1,&
!                   quickIf_r2,quickIf_i2,quickIf_z2,quickIf_log2,& ! fast versions
!                   quickIf_ch1,&
                   quickIf_ch1c,&
                   quickIf_ch1b
endinterface quickIf
!--
interface quickifV
  module procedure quickif_r1_va,quickif_i1_va,quickif_z1_va,quickif_log1_va,quickif_ch1_va
endinterface quickifV

!--
interface quickConcat
  module procedure quickConcat_2s
endinterface quickConcat
!--

contains
!---------------------------------------
!> Purpose: returns average of vector
!> Source: Copied from DMSL  Version: 3.17 - 23 September 2004 ad
!>         File: utilities_dmsl_kit.f90, average_rn1
pure function average(nv1)

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
subroutine getSpareUnit_v(unt,err,message)
! Purpose: returns vector of unused units for file data transfer.
! Unit will be in the range 7->2.2billion (see comment below).
! Comment:
! * Can not be pure as it contains the inquire function,tough life...
! * In Fortran-95,available units range from 0 to 2,147,483,640.
!   Preconnected units 5 (keyboard) and 6 (screen) can be re-connected
!   to a different logical device but to avoid silly errors this is avoided
!   in this procedure.
implicit none
! dummies
integer(mik),dimension(:),intent(out)::unt
integer(mik),intent(out)::err
character(*),intent(out)::message
! locals
integer(mik)::i,j,n
logical(mlk)::opened,xist
integer(mik),parameter::minUnits=7,maxUnits=2147483639
! Start procedure here
n=size(unt); j=1
do i=minUnits,maxUnits
  inquire(unit=i,opened=opened,exist=xist) ! check unit status
  if(.not.opened.and.xist)then ! un-opened existing unit found
    unt(j)=i; err=0; message="getSpareUnit/ok"
    j=j+1
    if(j>n)exit
  endif
enddo
if(i>maxUnits)then  ! all units in use
  unt=-1; err=-10; message="getSpareUnit/allUnitsInUse&"//&
      "&(all 2.2billion-u've goda b jokin')"
endif
! End procedure here
endsubroutine getSpareUnit_v
!---------------------------------------
subroutine getSpareUnit_1(unt,err,message)
! Purpose: overloaded for 1 unit
implicit none
! dummies
integer(mik),intent(out)::unt
integer(mik),intent(out)::err
character(*),intent(out)::message
! locals
integer(mik)::untv(1)
! Start procedure here
call getSpareUnit_v(untv,err,message)
unt=untv(1)
! End procedure here
endsubroutine getSpareUnit_1
!---------------------------------------
subroutine getSpareUnit_2(unt1,unt2,err,message)
! Purpose: overloaded for 2 units
implicit none
! dummies
integer(mik),intent(out)::unt1,unt2
integer(mik),intent(out)::err
character(*),intent(out)::message
! locals
integer(mik)::untv(2)
! Start procedure here
call getSpareUnit_v(untv,err,message)
unt1=untv(1); unt2=untv(2)
! End procedure here
endsubroutine getSpareUnit_2
!---------------------------------------
subroutine getSpareUnit_3(unt1,unt2,unt3,err,message)
! Purpose: overloaded for 3 units
implicit none
! dummies
integer(mik),intent(out)::unt1,unt2,unt3
integer(mik),intent(out)::err
character(*),intent(out)::message
! locals
integer(mik)::untv(3)
! Start procedure here
call getSpareUnit_v(untv,err,message)
unt1=untv(1); unt2=untv(2); unt3=untv(3)
! End procedure here
endsubroutine getSpareUnit_3
!---------------------------------------
subroutine getSpareUnit_4(unt1,unt2,unt3,unt4,err,message)
! Purpose: overloaded for 4 units
implicit none
! dummies
integer(mik),intent(out)::unt1,unt2,unt3,unt4
integer(mik),intent(out)::err
character(*),intent(out)::message
! locals
integer(mik)::untv(4)
! Start procedure here
call getSpareUnit_v(untv,err,message)
unt1=untv(1); unt2=untv(2); unt3=untv(3); unt4=untv(4)
! End procedure here
endsubroutine getSpareUnit_4
!---------------------------------------
subroutine getSpareUnit_5(unt1,unt2,unt3,unt4,unt5,err,message)
! Purpose: overloaded for 5 units
implicit none
! dummies
integer(mik),intent(out)::unt1,unt2,unt3,unt4,unt5
integer(mik),intent(out)::err
character(*),intent(out)::message
! locals
integer(mik)::untv(5)
! Start procedure here
call getSpareUnit_v(untv,err,message)
unt1=untv(1);unt2=untv(2);unt3=untv(3);unt4=untv(4);unt5=untv(5)
! End procedure here
endsubroutine getSpareUnit_5
!***************************************************************************************
pure subroutine parseStringIntoVector_delS(string,delim,array,narr,narrSeek,err,message)
! Purpose: Parses trim(string) into array, according to delimitor delim
! Comments:
! 1. The entire delim is used [not trim(delim)], to allow parsing by blanks.
! 2. However, it is trim(string) [not the entire string] that is parsed.
! 3. Uses serial do loop
! 4. If len(delim)==0 then parses each character into elements of array.
! 5. If len_trim(string)=0 returns
! 6. If string starts with delim, inserts blank as first array element
! 7. If string does not end with delim, the last chars form the last element
implicit none
! dummies
character(*),intent(in)::string
character(*),intent(in)::delim
character(*),intent(out),optional::array(:)
integer(mik),intent(out)::narr              ! number of used elements in arr
integer(mik),intent(in),optional::narrSeek  ! stop after narrSeek delims are found
integer(mik),intent(out)::err
character(*),intent(out)::message
! locals
character(*),parameter::procnam="parseStringIntoVector_delS"
integer(mik)::lenDelim,lenString,sizeArr,startScan,pos,s0
logical(mlk)::presArr
! Start procedure here
lenDelim=len(delim);lenString=len_trim(string); presArr=present(array)
if(presArr)then
  sizeArr=size(array)
else
  sizeArr=hugeInt
endif
narr=0; err=0; message=trim(procnam)//"/ok"
if(lenString==0)then    ! nothing to parse
  err=-10; message="f-"//trim(procnam)//"/len_trim(string)==0"
  return
elseif(lenDelim==0)then ! special case: parse everything
  if(sizeArr<lenString)then
    err=-20; message="f-"//trim(procnam)//"/size(array)tooSmall"
  endif
  narr=min(lenString,sizeArr)
  if(presArr)forall(pos=1:narr)array(pos)=string(pos:pos)
  return
endif
startScan=1
do  ! scan for the provided delimiter
  pos=index(string(startScan:lenString),delim)
  if(pos==0)then  ! no other occurences
    exit
  else ! found delimiter
    narr=narr+1
    if(narr>sizeArr)then
      err=-30; message="f-"//trim(procnam)//"/size(array)tooSmall"
      return
    else
      s0=startScan+pos-2
      if(presArr)array(narr)=string(startScan:s0) ! add to array
      startScan=s0+lenDelim+1
    endif
  endif
  if(present(narrSeek))then  ! check if found enuf
    if(narr==narrSeek)exit
  endif
enddo
if(narr>=0.and.startScan<=lenString)then  ! finish up array if ...
  narr=narr+1 ! ... the master string did not end with a delimiter
  if(narr>sizeArr)then
    err=-40; message="f-"//trim(procnam)//"/size(array)tooSmall"
  elseif(presArr)then
    array(narr)=string(startScan:lenString)
  endif
endif
! End procedure here
endsubroutine parseStringIntoVector_delS
!---------------------------------------
pure subroutine parseStringIntoVector_delSp(string,delim,array,narrSeek,err,message)
! Purpose: Overloaded for auto-allocation to rite size as a pointer.
! Comments:
! 1. Does not deallocate the pointer if an error occurs to allow diagnosis.
implicit none
! dummies
character(*),intent(in)::string
character(*),intent(in)::delim
character(*),pointer::array(:)
integer(mik),intent(in),optional::narrSeek ! stop after narrSeek delims are found
integer(mik),intent(out)::err
character(*),intent(out)::message
! locals
character(*),parameter::procnam="parseStringIntoVector_delSp"
integer(mik)::narr,narrChk,lenCHarr
! Start procedure here
err=0; message=trim(procnam)//"/ok"; lenCHarr=len(array)
call parseStringIntoVector_delS(string=string,delim=delim,&
  narr=narr,narrSeek=narrSeek,err=err,message=message)
if(err/=0)then
  message="f-"//trim(procnam)//"/nline/&"//trim(message)
  err=10; return
endif
allocate(array(narr),stat=err)
if(err/=0)then
  write(message,'(a,i0,a,i0,a)')"f-"//trim(procnam)//"/&
    &allocError[array][narr=",narr,"][lenCHarr=",lenCHarr,"]"
  err=10; return
endif
call parseStringIntoVector_delS(string=string,delim=delim,&
  array=array,narrSeek=narrSeek,narr=narrChk,err=err,message=message)
if(err/=0)then
  message="f-"//trim(procnam)//"/parse/&"//trim(message)
  err=20; return
elseif(narrChk/=narr)then
  write(message,'(a,i0,a,i0,a)')"f-"//trim(procnam)//"/&
    &allocError[array][narr=",narr,"][narrChk=",narrChk,"]"
  err=20; return
endif
! End procedure here
endsubroutine parseStringIntoVector_delSp
!---------------------------------------
elemental function quickIf_r1(tsource,fsource,mask)
! Purpose: Strict version of merge using IF statements (real version)
! Reasons for existence: 1. allows r1=quickIf(optArg,default,present(optArg).and.mask)
implicit none
! dummies
real(mrk),intent(in),optional::tsource
real(mrk),intent(in)::fsource
logical(mlk),intent(in),optional::mask
real(mrk)::quickIf_r1
! locals
logical(mlk)::mask0,presT
logical(mlk),parameter::maskDef=.true.
! Start procedure here
presT=present(tsource)
if(present(mask))then;mask0=mask
else;                 mask0=maskDef;endif
if(mask0.and.presT)then
  quickIf_r1=tsource
else
  quickIf_r1=fsource
endif
! End procedure here
endfunction quickIf_r1
!----------------------------------------------------
elemental function quickIf_r2(tsource,fsource)
! Purpose: compact version with no mask
implicit none
real(mrk),intent(in),optional::tsource
real(mrk),intent(in)::fsource
real(mrk)::quickIf_r2
! Start procedure here
if(present(tsource))then
  quickIf_r2=tsource
else
  quickIf_r2=fsource
endif
! End procedure here
endfunction quickIf_r2
!----------------------------------------------------
elemental function quickIf_i1(tsource,fsource,mask)
! Purpose: integer version
implicit none
integer(mik),intent(in),optional::tsource
integer(mik),intent(in)::fsource
logical(mlk),intent(in),optional::mask
integer(mik)::quickIf_i1
! locals
logical(mlk)::mask0,presT
logical(mlk),parameter::maskDef=.true.
! Start procedure here
presT=present(tsource)
if(present(mask))then;mask0=mask
else;                 mask0=maskDef;endif
if(mask0.and.presT)then
  quickIf_i1=tsource
else
  quickIf_i1=fsource
endif
! End procedure here
endfunction quickIf_i1
!----------------------------------------------------
elemental function quickIf_i2(tsource,fsource)
! Purpose: compact integer version
implicit none
integer(mik),intent(in),optional::tsource
integer(mik),intent(in)::fsource
integer(mik)::quickIf_i2
! Start procedure here
if(present(tsource))then
  quickIf_i2=tsource
else
  quickIf_i2=fsource
endif
! End procedure here
endfunction quickIf_i2
!----------------------------------------------------
elemental function quickIf_z1(tsource,fsource,mask)
! Purpose: integer version
implicit none
complex(mck),intent(in),optional::tsource
complex(mck),intent(in)::fsource
logical(mlk),intent(in),optional::mask
complex(mck)::quickIf_z1
! locals
logical(mlk)::mask0,presT
logical(mlk),parameter::maskDef=.true.
! Start procedure here
presT=present(tsource)
if(present(mask))then;mask0=mask
else;                 mask0=maskDef;endif
if(mask0.and.presT)then
  quickIf_z1=tsource
else
  quickIf_z1=fsource
endif
! End procedure here
endfunction quickIf_z1
!----------------------------------------------------
elemental function quickIf_z2(tsource,fsource)
! Purpose: compact integer version
implicit none
complex(mck),intent(in),optional::tsource
complex(mck),intent(in)::fsource
complex(mck)::quickIf_z2
! Start procedure here
if(present(tsource))then
  quickIf_z2=tsource
else
  quickIf_z2=fsource
endif
! End procedure here
endfunction quickIf_z2
!----------------------------------------------------
elemental function quickIf_log1(tsource,fsource,mask)
! Purpose: logical version.
implicit none
logical(mlk),intent(in),optional::tsource
logical(mlk),intent(in)::fsource
logical(mlk),intent(in),optional::mask
logical(mlk)::quickIf_log1
! locals
logical(mlk)::mask0,presT
logical(mlk),parameter::maskDef=.true.
! Start procedure here
presT=present(tsource)
if(present(mask))then;mask0=mask
else;                 mask0=maskDef;endif
if(mask0.and.presT)then
  quickIf_log1=tsource
else
  quickIf_log1=fsource
endif
! End procedure here
endfunction quickIf_log1
!----------------------------------------------------
elemental function quickIf_log2(tsource,fsource)
! Purpose: compact logical version.
implicit none
logical(mlk),optional,intent(in)::tsource
logical(mlk),intent(in)::fsource
logical(mlk)::quickIf_log2
! Start procedure here
if(present(tsource))then
  quickIf_log2=tsource
else
  quickIf_log2=fsource
endif
! End procedure here
endfunction quickIf_log2
!----------------------------------------------------
! elemental function quickif_ch1(tsource,fsource,mask)
! ! Purpose: Character quickif function. Does not quite satisfy Fortran standard for
! ! specification expressions - it is illegal to use an optional argument
! ! (tsource) in the specification expression function (len2).
! ! silly restriction if you ask me... considering CVF66b and IVF81 handle it fine.
! ! But linux IVF dies with segmentation fault whereas NAG identifies the standard violation.
! ! Programmer: Dmitri Kavetski, Princeton University, 15 April 2005.
! implicit none
! ! dummies
! character(*),intent(in),optional::tsource
! character(*),intent(in)::fsource
! logical(mlk),intent(in),optional::mask
! character(len2(tsource,fsource,mask))::quickif_ch1
! ! locals
! logical(mlk)::mask0,presT
! logical(mlk),parameter::maskDef=.true.
! ! Start procedure here
! presT=present(tsource)
! if(present(mask))then;mask0=mask
! else;                 mask0=maskDef;endif
! if(mask0.and.presT)then
!   quickif_ch1=tsource
! else
!   quickif_ch1=fsource
! endif
! ! End procedure here
! endfunction quickif_ch1
!----------------------------------------------------
elemental function quickIf_ch1b(tsource,fsource,mask)
! Purpose: Character version, with some inevitable Fortran-induced quirks
! * Allows len(tsource)/=len(fsource) (and thus better than the merge intrinsic), BUT
! * Does not allow non-present tsource (due to Fortran limitations with specification expressions)
! * Length of result string is dumb (due to Fortran limitations with specification expressions)
! Usage:
! 1. The benefit of this routine is replacing merge(str1,str2,mask) where str1/str2 are
!    unequal-length strings and thus must be padded.
! 2. Routine can't be used in the original DMSL quickif fashion as
!    quickif(str1,str2) or even quickif(str1,str2,present(str1)),
!    since str1 is not an optional. For this, use quickIf_ch1c below, with some quirks.
implicit none
character(*),intent(in)::tsource,fsource
logical(mlk),intent(in),optional::mask
character(len=max(len(tsource),len(fsource)))::quickIf_ch1b
! locals
logical(mlk)::mask0,presT
logical(mlk),parameter::maskDef=.true.
! Start procedure here
presT=.true.
if(present(mask))then;mask0=mask
else;                 mask0=maskDef;endif
if(mask0.and.presT)then
  quickIf_ch1b=tsource
else
  quickIf_ch1b=fsource
endif
! End procedure here
endfunction quickIf_ch1b
!----------------------------------------------------
pure function quickIf_ch1c(tsource,fsource,lens)
! Purpose: Compact character version, with some inevitable quirks.
! * Allow non-present tsource, BUT
! * Resultant string length given by lens (not automatic).
! * The routine is not elemental due to Fortran constraints.
! Usage:
! 1. Provided care is taken with string lengths, this routine
!    can be used for compact on-the-fly quickif, eg,
!    a) call sub(str=quickif(strOpt,strDef,len2(strOpt,strDef)))
!    b) str=quickif(strOpt,strDef,len(str))
!    Use trim(quickif(tsource,fsource,lens)) otherwise.
implicit none
character(*),optional,intent(in)::tsource
character(*),intent(in)::fsource
integer(mik),intent(in)::lens
! character(len=max(lens,len_trim(fsource)))::quickIf_ch1c
character(len=lens)::quickIf_ch1c
! Start procedure here
if(present(tsource))then
  quickIf_ch1c=tsource
else
  quickIf_ch1c=fsource
endif
! End procedure here
endfunction quickIf_ch1c
!----------------------------------------------------
pure function quickif_r1_va(tsource,fsource,mask)
! Purpose: Compact quickif for vectors on-the-fly.
implicit none
! dummies
real(mrk),intent(in)::tsource(:),fsource(:)
logical(mlk),intent(in)::mask
real(mrk)::quickif_r1_va(merge(size(tsource),size(fsource),mask))
! Start procedure here
if(mask)then; quickif_r1_va=tsource
else;         quickif_r1_va=fsource; endif
! End procedure here
endfunction quickif_r1_va
!----------------------------------------------------
pure function quickif_i1_va(tsource,fsource,mask)
! Purpose: Overlorded for integers
implicit none
! dummies
integer(mik),intent(in)::tsource(:),fsource(:)
logical(mlk),intent(in)::mask
integer(mik)::quickif_i1_va(merge(size(tsource),size(fsource),mask))
! Start procedure here
if(mask)then; quickif_i1_va=tsource
else;         quickif_i1_va=fsource; endif
! End procedure here
endfunction quickif_i1_va
!----------------------------------------------------
pure function quickif_z1_va(tsource,fsource,mask)
! Purpose: Overlorded for integers
implicit none
! dummies
complex(mck),intent(in)::tsource(:),fsource(:)
logical(mlk),intent(in)::mask
complex(mck)::quickif_z1_va(merge(size(tsource),size(fsource),mask))
! Start procedure here
if(mask)then; quickif_z1_va=tsource
else;         quickif_z1_va=fsource; endif
! End procedure here
endfunction quickif_z1_va
!----------------------------------------------------
pure function quickif_log1_va(tsource,fsource,mask)
! Purpose: Overlorded for integers
implicit none
! dummies
logical(mlk),intent(in)::tsource(:),fsource(:)
logical(mlk),intent(in)::mask
logical(mlk)::quickif_log1_va(merge(size(tsource),size(fsource),mask))
! Start procedure here
if(mask)then; quickif_log1_va=tsource
else;         quickif_log1_va=fsource; endif
! End procedure here
endfunction quickif_log1_va
!----------------------------------------------------
pure function quickif_ch1_va(tsource,fsource,lens,mask)
! Purpose: Overlorded for character strings
implicit none
! dummies
character(*),intent(in)::tsource(:),fsource(:)
integer(mik),intent(in)::lens
logical(mlk),intent(in)::mask
character(lens)::quickif_ch1_va(merge(size(tsource),size(fsource),mask))
! Start procedure here
if(mask)then; quickif_ch1_va=tsource
else;         quickif_ch1_va=fsource; endif
! End procedure here
endfunction quickif_ch1_va
!----------------------------------------------------
elemental function len2(string1,string2,mask,trims)
! Purpose: Enhanced len_trim() for use within character quickifs.
! Comments:
! 1.Currently this function is not used in quickif due to
!   Fortran syntax constraints preventing the use of optional
!   arguments in variable specifications.
implicit none
! dummies
character(*),intent(in),optional::string1
character(*),intent(in)::string2
logical(mlk),intent(in),optional::mask
logical(mlk),intent(in),optional::trims
integer(mik)::len2
! locals
logical(mlk),parameter::trimsDef=.true.
logical(mlk)::trims0
! Start procedure here
trims0=quickif_log1(trims,trimsDef)
if(trims0)then
  if(present(string1))then
    len2=quickif_i1(len_trim(string1),len_trim(string2),mask)
  else
    len2=len_trim(string2)
  endif
else
  if(present(string1))then
    len2=quickif_i1(len(string1),len(string2),mask)
  else
    len2=len(string2)
  endif
endif
! End procedure here
endfunction len2
!----------------------------------------------------
elemental function lenOpt(str,trims)
! Purpose: returns LEN or LEN_TRIM of optional string.
! If it is not present then returns "-1"
implicit none
! dummies
character(*),intent(in),optional::str
logical(mlk),intent(in),optional::trims
integer(mik)::lenOpt
! locals
logical(mlk)::trims0
logical(mlk),parameter::trimsDef=.true.
! Start procedure here
if(present(str))then  ! str present
  trims0=quickif(trims,trimsDef)
  if(trims)then;lenOpt=len_trim(str)
  else;         lenOpt=len(str);endif
else
  lenOpt=-1
endif
! End procedure here
endfunction lenOpt
!----------------------------------------------------
pure function len_trimV_1(string)result(res)
! Purpose: Extended len_trim() function for arrays of strings
! Programmer: Dmitri Kavetski
! Last modified: 5 Jan 2010 AD, Wigramistan
! Performance
! IN:
! OUT:
! Comments:
! 1. The native len_trim(), annoyingly, doesnt work for arrays.
implicit none
! dummies
character(*),intent(in)::string(:)
integer(mik)::res
! locals
integer(mik)::len_trimI(size(string))
! Start procedure here
len_trimI=lenOpt(str=string,trims=.true.)
res=maxval(len_trimI)
! End procedure here
endfunction len_trimV_1
!----------------------------------------------------
elemental subroutine quickifCHsub(strsrc,strdef,str,err,message)
! Purpose: Quickif subroutine for string copying.
implicit none
! dummies
character(*),intent(in),optional::strsrc,strdef
character(*),intent(inout)::str
integer(mik),intent(out)::err
character(*),intent(out)::message
! locals
logical(mlk)::haveSrc,haveDef
integer(mik)::lenSrc,lenDef,lenHave
! start procedure here
err=0;haveSrc=present(strsrc);haveDef=present(strdef)
if(haveSrc)then
  lenSrc=len_trim(strsrc);lenHave=len(str)
  if(lenSrc>lenHave)then
    write(message,'(a,i0,a,i0,a)')"f-quickifCHsub/exceeded[lenSrc>lenHave][",&
      lenSrc,">",lenHave,"]/["//trim(strsrc)//"]"
    err=10; return
  else
    str=strsrc
  endif
elseif(haveDef)then
  lenDef=len_trim(strdef);lenHave=len(str)
  if(lenDef>lenHave)then
    write(message,'(a,i0,a,i0,a)')"f-quickifCHsub/exceeded[lenDef>lenHave][",&
      lenDef,">",lenHave,"]/["//trim(strdef)//"]"
    err=10; return
  else
    str=strdef
  endif
endif
! end procedure here
endsubroutine quickifCHsub

!---------------------------------------
pure function quickConcat_2s(string,sep,trimString,innerOnly)result(res)
! Purpose: Jacket for string concatenation: takes
! in array of strings and returns single string composed
! of the individual elements. by default will trim elements
implicit none
! dummies
character(*),intent(in)::sep
!INCLUDE 'utilities_quickConcat_2X_INC.f90'
character(*),intent(in)::string(:)
logical(mlk),intent(in),optional::trimString
logical(mlk),intent(in),optional::innerOnly
character(size(string)*(len(string)+len(sep)))::res
! locals
integer(mik)::lenres,i
! Start procedure here
res=blankCH
do i=1,size(string)
res=trim(res)//","//string(i)
enddo
lenres=len_trim(res)
res(lenres:lenres)=blankCH
! End procedure here
endfunction quickConcat_2s
!---------------------------------------
pure subroutine splitPathFile(pathFileName,path,fileName,delim,err)
! Purpose: Splits a path+name into separate path and name.
! ---
! Programmer: Dmitri Kavetski
! Last modified: 7 Dec 2009, Nuke -> Sydney
! ---
! Performance
! IN:
!   pathFileName    = complete file specification
!   delim           = delimiter to be used ("\" if omitted)
! OUT:
!   path            = file path
!   fileName        = file name
!   err             = error status
! ---
! Comments:
! 1. See also 'splitFilePathKoreExt' for extra details.
! ---
implicit none
! dummies
character(*),intent(in)::pathFileName
character(*),intent(out),optional::path,fileName
character(*),intent(in),optional::delim
integer(mik),intent(out)::err
! locals
integer(mik)::iposDelim
character(*),parameter::delimDef="\"
character(1)::delim0
! Start procedure here
err=0; delim0=quickif(delim,delimDef,lens=1)
iposDelim=scan(pathFileName,delim0,back=.true.)
selectcase(iposDelim)
case(0)
  if(present(path))path=""
  if(present(fileName))fileName=pathFileName
!  err=10; message="f-splitPathFile/noDelim['"//delim0//"']Found"
case default
  if(present(path))path=pathFileName(1:iposDelim)
  if(present(fileName))fileName=pathFileName(iposDelim+1:)
endselect
! End procedure here
endsubroutine splitPathFile
!---------------------------------------

end module utilities_dmsl_kit

!---------------------------------------
!> Cut down version of numerix dmsl
module numerix_dmsl_kit
use kinds_dmsl_kit

interface getmeanvar
  module procedure getmeanvar_s
endinterface getmeanvar

contains
!----------------------------------------------------------------------
!> Purpose: Computes the mean and variance of a sample of a scalar random variable.
!> IN:  vector x(1:ns), method="f"-fast scheme,"c"-classic,else 2-pass
!> OUT: mean "mean" and variance "var"
!> Comments:
!> 1. When size(x) is very large, it is possible to overflow the
!>    stack for intermediate computations of deviations, since on-the-fly
!>    arrays go to the stack. In this case, need to introduce allocatable
!>    arrays to store intermediate results on the heap
pure subroutine getmeanvar_s(x,mean,var,method,err,message)

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
!> Cut down version of DMSL IOTools
module iotools_dmsl_mod

integer(4),parameter::normalFinish=0

end module iotools_dmsl_mod
!******************************************************************
! (C) Copyright 2009-2010  ---  Dmitri Kavetski  ---  All rights reserved
!******************************************************************
!> From   v5.011.000 of DMSL
!> Cut down version of intercom DMSL 
!> Purpose: Program/language intercommunication:
!> (A) Defines global filepaths for
!>     (1) DMSL components'
!>     (2) RFortran and R scripts;
!>     (3) RGui (associated with RFortran).
!> (B) Provides language interoperability (COM, etc)
!> --
!> Programmer: Dmitri Kavetski & Mark Thyer, 2009
!> Civil, Environmental Engineering and Surveying
!> University of Newcastle, Callaghan, NSW 2308, Australia.
!> --
!> Comments:
!> 1. The default values can be overwritten in calling apps.
!> --
module intercom_dmsl_kit
use kinds_dmsl_kit
implicit none
private;save
!> 1. PATHS - deleted to save confusion!
public::RGui_path,RGui_name
public::RFortran_path,Rscript_path
!> 2. PROCEDURE STATUS
public::myProcID,openedCOM
!----------------------------------------------------
integer(MY_INT_PTR_KIND)::myProcID=undefIN  !< procID of the current process
logical(mlk)::openedCOM=.false.             !< true when the COM server is already initialized
!----------------------------------------------------
!> 2. Path to RFortran scripts
character(len_DMSLpath)::RFortran_path="C:\DMITRI\My_Documents\FortranLibs"
character(len_DMSLpath)::Rscript_path="C:\DMITRI\My_Documents\FortranLibs\RFortran\Rscripts"
!> 3. Path to R console
character(len_DMSLpath)::RGui_path="C:\Program Files\R\R-2.8.0\bin"
character(len_vLongStr)::RGui_name="Rgui.exe"
!----------------------------------------------------
!----------------------------------------------------
endmodule intercom_dmsl_kit
!******************************************************************
