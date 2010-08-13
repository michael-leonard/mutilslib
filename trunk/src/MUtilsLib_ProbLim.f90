!<license>
MODULE MUtilsLib_Problim
!************************************************************************************************
!  MODULE: Probability Limits
!  PURPOSE:  Calc Probability limits from a set of unsorted data (point or array)
! 
! Based on code developed by Andrew Frost, Martin Lambert and George Kuczera
!
! Usage:
! sub CalcProbLim(inputdata,problimIN,probLimPct); 
!     inputdata  - vector (nPoints) or 2D array (nPoints,nReps) of data that requires probability limits    
!     problimIN  - vector of probabilities for the probability limits, e.g. (0.05,0.5,0.95)
!     probLimPct - vector (ith element is ith problim) or 2D array (ith column is ith problim) 
!    of the probability limits of inputdata 
! 
!
!*************************************************************************************************
use kinds_dmsl_kit
use errorMOD
IMPLICIT NONE

! Prob Limit Variable - for v1 maintained for backwards compatibility
type ProblimType
    real(mrk) :: Avg,per5th,per50th,per95th
end type
! Local Variables
integer(mik) :: nproblim
real(mrk), dimension(:), allocatable :: probLim     ! Probability Limits 
real(mrk), dimension(:), allocatable :: percentile  ! Percentiles corresponding to the probablity limits 

! Procedural Variables
interface calcProblim
  module procedure calcProblimPoint1,calcProblimArray1 ! Maintained for Backwards Compatibility 
  module procedure calcProblimPoint2,calcProblimArray2   
end interface calcProblim

! Procedural Availability
private
public :: problimType,CalcProblim,hpsort,stanResidualsProb,pValResidualsProb
public :: hpsortInd, isSameR4, isSameR8
contains
!****************************************************************************
function prob (rank, nData)
! Purpose: Get % prob given rank and number of data
! Option to uses different plotting positions
! Source:
implicit none
! Dummies
integer(mik), intent(in) :: rank, nData
real(mrk) :: prob
! Start Procedure Here
! Weibull Plotting Position
!prob = dble(rank)/(dble(nData+1))
! Cunnane Plotting Position
prob = (dble(rank)-0.4_mrk)/(dble(nData+0.2_mrk))
! End Procedure Here
end function prob
!****************************************************************************
function LineInterpolate(x,x1,y1,x2,y2)
! Purpose: Interpolates a line between two points (x1,y1) and (x2,y2)
!          and provides an estimate of y for a given x
implicit none
! Dummies
real(mrk) :: LineInterpolate
real(mrk), intent(in) :: x,x1,y1,x2,y2
! Start Procedure Here
LineInterpolate=y1+((y2-y1)/(x2-x1)*(x-x1))
! End Procedure Here
end function LineInterpolate
!****************************************************************************
subroutine calcPercentile(inputData,nData)
! Purpose: Finds a percentile for a set of Data, inputData
! Input: inputData should be sorted in ascending order
implicit none
! Dummies
integer(mik) :: nData
real(mrk), dimension(:) :: inputData  
! Locals
integer(mik) :: i,problimCount
! Start Procedure Here
problimCount=1
do while (problimCount<=nProblim)
  if (i==nData .and. prob(i,nData)<problim(problimCount)) then
    problim(problimCount)=undefRN
    !call fatal_error("Percentile greater than range of data")
    exit 
  end if
  ! Find good start Point
  ! Find Closet Integer
  i=INT(problim(problimCount)*nData)
  if (i==0) then ! Check bounds
    i=1; !call fatal_error("Percentile less than range of data")
    percentile(problimCount)=undefRN
  else
    ! Calculate Percentile
    Percentile(problimCount)=LineInterpolate(problim(problimCount),prob(i,nData),inputData(i),prob(i+1,nData),inputData(i+1))
  end if    
  problimCount=problimCount+1
end do
! End Procedure Here
end subroutine calcPercentile
!****************************************************************************
subroutine CalcProblimPoint1(inputdata,problimPct)
!  PURPOSE: Calculate 5th, 50th, 95th Confidence limits from a input array of data
!  Inputs 
use utilities_dmsl_kit, only : average
IMPLICIT NONE
! Dummy Arguments
real(mrk), dimension(:), intent(in) :: inputdata
type (problimType), intent(out) :: problimPct
! Local Variables
integer(mik) :: nData,status
real(mrk), dimension(:), allocatable :: sortedData
! Initialise
nData=SIZE(inputData); nproblim=3
allocate(sortedData(nData),problim(nproblim),percentile(nproblim),stat=status); 
if(status/=0) call fatal_error("Unable to allocate sortedData,problim,percentile")
problim = (/  .05d0,  .50d0,  .95d0 /)
! Sort Data
sortedData=InputData
CALL hpsort(nData,sortedData)
! Determine problim
call calcPercentile(sortedData,nData)
problimPct%Per5th=Percentile(1)
problimPct%Per50th=Percentile(2)
problimPct%Per95th=Percentile(3)
problimPct%Avg=average(sortedData)
deallocate(sortedData,problim,percentile)
end subroutine calcproblimPoint1
!****************************************************************************
subroutine CalcproblimPoint2_s(inputdata,problimIN,problimPct)
!  PURPOSE: Wrapper for CalcProbLim2 for scalar input
IMPLICIT NONE
! Dummy Arguments
real(mrk), dimension(:), intent(in)  :: inputdata
real(mrk), intent(in)  :: problimIN        ! Values for probability limits, e.g 0.05, 0.5, 0.95
real(mrk), intent(out) :: problimPct       ! Probability Limits Output: 
                                           ! percentiles with each element i corresponding to the probability 

! Locals
real(mrk), dimension(1):: probLimLc,probLimPctLc        ! Values for probability limits, e.g 0.05, 0.5, 0.95

problimLc(1)=probLimIN
call CalcproblimPoint2(inputdata=inputdata,problimIN=probLimLc,problimPct=probLimPctLc)
probLimPct=probLimPctLc(1)

end subroutine CalcproblimPoint2_s
!****************************************************************************
subroutine CalcproblimPoint2(inputdata,problimIN,problimPct)
!  PURPOSE: More general version of calcproblimPoint1  
!           Calculate percentiles given in problimIN for a vector of data
IMPLICIT NONE
! Dummy Arguments
real(mrk), dimension(:), intent(in)  :: inputdata
real(mrk), dimension(:), intent(in)  :: problimIN        ! Values for probability limits, e.g 0.05, 0.5, 0.95
real(mrk), dimension(:), intent(out) :: problimPct       ! Probability Limits Output: 
                                                         ! percentiles with each element i corresponding 
                                                         ! to the probability limit problimIN(i)
! Local Variables
integer(mik) :: nData,status
real(mrk), dimension(:), allocatable :: sortedData
! Initialise
nData=SIZE(inputData); nproblim=SIZE(problimIN)
allocate(sortedData(nData),problim(nproblim),percentile(nproblim),stat=status); 
if(status/=0) call fatal_error("Unable to allocate sortedData,problim,percentile")
problim = problimIN
! Sort Data
sortedData=InputData
CALL hpsort(nData,sortedData)
! Determine problim
call calcPercentile(sortedData,nData)
problimPct=Percentile
deallocate(sortedData,problim,percentile)
end subroutine calcproblimPoint2
!****************************************************************************
subroutine calcproblimArray1(inputdata,nPoints,nReps,problimPct)
!  PURPOSE: Calculate 5th, 50th, 95th probidence limits from a time series of data
!           with nPOints number of data points and nReps as number of replicates
use utilities_dmsl_kit, only : average
IMPLICIT NONE
! Dummy Arguments
integer(mik), intent(in) :: nPoints,nReps
real(mrk), dimension(nPoints,nReps),intent(in) :: inputdata
type(problimType), dimension(nPoints),intent(out) :: problimPct
! Local Variables
integer(mik) :: i,status
real(mrk), dimension(:,:), allocatable :: sortedData
! Initialise
nproblim=3 
allocate(sortedData(nPoints,nReps),problim(nproblim),percentile(nproblim),stat=status); 
if (status/=0) call fatal_error("Unable to allocate sorted data,problim,percentile")
sortedData=inputData; problim = (/  .05d0,  .50d0,  .95d0 /);
! First rank individual runs
do i = 1, nreps
  call hpsort(nPoints,sortedData(1:nPoints,i))
end do
! Then sort in terms of frequency
do i = 1, nPoints
  call hpsort(nreps,sortedData(i,1:nreps))
  call calcPercentile(sortedData(i,1:nreps),nReps)
  problimPct(i)%Per5th=Percentile(1)
  problimPct(i)%Per50th=Percentile(2)
  problimPct(i)%Per95th=Percentile(3)
  problimPct(i)%Avg=average(sortedData(i,1:nReps))
end do
deallocate(sortedData,problim,percentile)
end subroutine calcproblimArray1
!****************************************************************************
subroutine calcproblimArray2(inputdata,problimIN,problimPct,probLimType)
!  PURPOSE: More general version of calcproblimArray1  
!           Calculate percentiles given in problimProb for a time series of data
!           with nPoints number of data points and nReps as number of replicates
IMPLICIT NONE
! Dummy Arguments
real(mrk), dimension(:,:),intent(in) :: inputdata        ! Used to determine probability limits: array (nPoints,nReps) 
                                                         ! where nPoints is time series length with nReps Replicates
real(mrk), dimension(:),intent(in) ::   problimIN        ! Values for probability limits, e.g 0.05, 0.5, 0.95
character(len=*),intent(in),optional ::   probLimType    ! Specifies problimits on the distributib( (DISTRIB) or time series (TS)
real(mrk), dimension(:,:),intent(out) ::problimPct       ! Probability Limits Output: Percentiles with each column i 
                                                         ! corresponding to the probability limit problimIN(i)
                                                         ! with nPoints rows    
! Local Variables
integer(mik) :: i,j,nPoints,nReps,status 
real(mrk), dimension(:,:), allocatable :: sortedData
! Initialise
nPoints=size(inputData,1); nReps=size(inputData,2); nproblim=SIZE(problimIN); 
allocate(sortedData(nPoints,nReps),percentile(nproblim),problim(nproblim),stat=status);
if(status/=0) call fatal_error("Unable to allocate problim,sortedData")
sortedData=inputData; problim = problimIN
! Perform Checks
if(SIZE(problimPct,1)/=nPoints) call fatal_error("Rank 1 of problim Argument is not equal to nPoints in F:calproblimArrary2")
if(SIZE(problimPct,2)/=nproblim) call fatal_error&  
    ("Rank 2 of problim Argument is not equal to SIZE(problimProb) in F:calproblimArrary2")
! Rank or not
if (present(problimType)) then
  select case(problimType)
  case("DISTRIB") ! Rank individual points to form a distribution
    do i = 1, nreps 
        call hpsort(nPoints,sortedData(1:nPoints,i)) 
    end do
  case("TS") 
   continue ! Don't rank if specify time series
  case default ! Unknown problim Type - assume a distribution
      open(10); write(10,*) "XXX",problimtype,"XXX"; close(10)
      !do i = 1, nreps; call hpsort(nPoints,sortedData(1:nPoints,i)); end do
  end select
else ! Assume a distribution if problimtype not specified
    do i = 1, nreps
         call hpsort(nPoints,sortedData(1:nPoints,i))
    end do
end if
! Then sort in terms of frequency
do i = 1, nPoints
  call hpsort(nreps,sortedData(i,1:nreps))
  call calcPercentile(sortedData(i,1:nreps),nReps)
  forall(j=1:nproblim) 
    problimPct(i,j)=Percentile(j)
  end forall
end do
deallocate(sortedData,percentile,problim)
end subroutine calcproblimArray2
!****************************************************************************
function calcProbArray(x,y,n)
!  PURPOSE: Calculates the proportion of array y that is less than x: P(y[1:n]<x) 
!  INPUTS: x : value to be tested
!          y : array of data, unsorted
!          n : length of array y
IMPLICIT NONE
! Dummy Arguments
integer(mik), intent(in) :: n
real(mrk), intent(in) :: x
real(mrk), dimension(n),intent(in) :: y
! Local Variables
integer(mik) :: botloc,halfloc,it,toploc
real(mrk) :: top,bot,half,diff
real(mrk), dimension(n) :: sortedData
! Func Declarations
real(mrk) :: calcProbArray
! Initialise and Sort Data
sortedData=y
call hpsort(n,sortedData(1:n))
! First find if x is above or below bounds of y
if (x>sortedData(n)) then
  calcProbArray=1.0_mrk; return
else if (x<sortedData(1)) then
  calcProbArray=0.0_mrk; return
end if
! Use Bisection Method to find Data
top=sortedData(n)
bot=sortedData(1)
toploc=n
botloc=1
it=1
do 
  ! Use Bisection Method to search for Probability
  diff=(toploc+botloc)/2
  halfloc=NINT(diff)
  half=sortedData(halfloc)
  if (x<half) then 
    toploc=halfloc 
  else 
    botloc=halfloc; 
  end if
  if ((toploc-botloc)==1) then
    calcProbArray=LineInterpolate(x,sortedData(botloc),prob(botloc,n),sortedData(toploc),prob(toploc,n))
    exit
  end if
  it=it+1; if (it>n) call fatal_error("Bisection method not working in f:calcProbArray") 
end do
end function calcProbArray
!*****************************************************************************************
subroutine stanResidualsProb(obsdata,simInputdata,stanResiduals,mv,centreIn)
!  PURPOSE: Calculate the residuals, standardized by the total uncertainty in the simulated data
!  Inputs : obsdata - vector of observed data
!         : simdata - 2D array (nPoints,nReps) of simulated multi-replicate data used to determine the total uncertainty
use numerix_dmsl_kit, only : getmeanvar
IMPLICIT NONE
! Dummy Arguments
real(mrk), dimension(:,:), intent(in) :: simInputdata
real(mrk), dimension(:), intent(in) :: obsData
real(mrk), dimension(:), intent(out) :: stanResiduals
!real(mrk), intent(in) :: mv  ! Missing Values
logical(mlk), intent(in), optional :: mv(:) 
character(len=*),intent(in),optional :: centreIn

! Local Variables
real(mrk) :: mean,median,var
integer(mik) :: i,nPoints,err
character(len=len_vLongStr) :: message,centre
! Initialise
nPoints=SIZE(obsdata)
if(nPoints/=SIZE(simInputdata,1)) call fatal_error("obsdata and simInputdata have different lengths in stanResiduals Prob")
if(present(centreIn)) then; 
    centre=centreIn; 
else
    centre="MEDIAN"
end if
do i=1,nPoints
  call getmeanvar(x=simInputData(i,:),mean=mean,var=var,method="f",err=err,message=message)
  call CalcproblimPoint2_s(inputdata=simInputData(i,:),problimIN=0.5_mrk,problimPct=median)
  if (err/=0) call fatal_error(trim(message)//" in stanResidualsProb")

  if (present(mv)) then
    if (mv(i)) then
        stanResiduals(i)=undefRN
        cycle
    end if
  end if

    select case(trim(centre))
    case("MEAN")
      stanResiduals(i)=(obsdata(i)-mean)/sqrt(var)
    case("MEDIAN")
      stanResiduals(i)=(obsdata(i)-median)/sqrt(var)
    end select

end do
end subroutine stanResidualsProb
!-----------------------------------------------------------------------
subroutine pValResidualsProb(obsdata,simdata,pValResiduals,mv)
!  PURPOSE: Calculate the p-value (cumulative probability) of the residuals using total uncertainty in the simulated data
!  Inputs : obsdata - vector of observed data
!         : simdata - 2D array (nPoints,nReps) of simulated multi-replicate data used to determine the total uncertainty
IMPLICIT NONE
! Dummy Arguments
real(mrk), dimension(:,:), intent(in) :: simData
real(mrk), dimension(:), intent(in) :: obsData
real(mrk), dimension(:), intent(out) :: pValResiduals
real(mrk), intent(in) :: mv  ! Missing Values

! Local Variables
integer(mik) :: i,nPoints,nReps
! Initialise
nPoints=SIZE(obsdata); nReps=SIZE(simData,2)
if(nPoints/=SIZE(simData,1)) call fatal_error("obsdata and simInputdata have different lengths in stanResiduals Prob")
! Determine cumulative Probability for each obsdata.
do i=1,nPoints
  if (obsdata(i)==mv) then
    pValResiduals(i)=mv
  else
    pValResiduals(i)=calcProbArray(x=obsdata(i),y=simData(i,:),n=nReps)
  end if
end do
end subroutine pValResidualsProb

    subroutine hpsortInd(n,ra,ind)
      ! Inelegant ripoff of hpsort(). Uses a copied array to protect order = slow.
      ! heapsort - sorts an array using the heapsort method
      ! - sorts an array ra of length n into ascending numerical order
      !   using the heapsort algorithm
      ! - n is input; ra is replaced on output by its sorted rearrangement
      implicit none
      integer      :: n         ! length of array
      real(mrk)    :: ra(n)     ! array
      real(mrk)    :: copy(n)   ! array
      integer(mik) :: ind(n)    ! index array
      integer(mik) :: i,ir,j,l  ! index counters
      real(mrk)    :: rra       ! swap?
      integer(mik) :: temp      ! swap?

      if (n<2) return

      l=n/2+1
      ir=n
      do i = 1,n
        ind(i) = i
        copy(i) = ra(i)
      end do

      do
        if(l>1)then
          l=l-1
          rra=copy(l)
          temp = ind(l)
        else
          rra=copy(ir)
          copy(ir)=copy(1)
          temp=ind(ir)
          ind(ir)=ind(1)
          ir=ir-1
          if(ir==1)then
            copy(1)=rra
            ind(1)=temp
            return
          endif
        endif
        i=l
        j=l+l
        do
          if(j<=ir)then
            if(j<ir)then
              if(copy(j)<copy(j+1))then
                j=j+1
              end if
            endif
            if(rra<copy(j))then
              copy(i)=copy(j)
              ind(i)=ind(j)
              i=j
              j=j+j
            else
              j=ir+1
            endif
          else
            exit
          endif
        end do
        copy(i)=rra
        ind(i)=temp
      end do
    end subroutine hpsortInd

    subroutine hpsort(n,ra)
      ! heapsort - sorts an array using the heapsort method
      ! - sorts an array ra of length n into ascending numerical order
      !   using the heapsort algorithm
      ! - n is input; ra is replaced on output by its sorted rearrangement
      implicit none
      integer      :: n         ! length of array
      real(mrk)    :: ra(n)     ! array
      integer(mik) :: i,ir,j,l  ! index counters
      real(mrk)    :: rra       ! ?

      if (n<2) return

      l=n/2+1
      ir=n

      do
        if(l>1)then
          l=l-1
          rra=ra(l)
        else
          rra=ra(ir)
          ra(ir)=ra(1)
          ir=ir-1
          if(ir==1)then
            ra(1)=rra
            return
          endif
        endif
        i=l
        j=l+l
        do
          if(j<=ir)then
            if(j<ir)then
              if(ra(j)<ra(j+1))then
                j=j+1
              end if
            endif
            if(rra<ra(j))then
              ra(i)=ra(j)
              i=j
              j=j+j
            else
              j=ir+1
            endif
          else
            exit
          endif
        end do
        ra(i)=rra
      end do
    end subroutine hpsort

end module mutilslib_Problim
!*********************************************************************************************
! For backwards compatibility
module problimMOd

use MUtilsLib_Problim

end module problimMOd