subroutine sample_subroutine()

use ML_utilities_problim,only : calcproblim
use kinds_dmsl_kit
implicit none

integer(mik) :: i,ndata,nrep,status
real(mrk), allocatable :: reps(:,:),ARI(:),problim(:,:)

ndata=992; nrep=100; allocate(reps(ndata,nrep),ARI(ndata),problim(ndata,3))
! Read-in Data
open(unit=10,file="..\..\data\multireplicateoutput.csv",status='old',iostat=status)
if (status/=0) write(*,*) "Unable to open file, IO error:",status
do i=1,ndata
  read(10,*) ARI(i),reps(i,:)
end do
close(unit=10)

! Calculate Probability Limits
call calcproblim(inputdata=reps,problimIN=(/0.05_mrk,0.5_mrk,0.95_mrk/),problimPct=problim)
! Nots
! 1. You can specify any set of probabilities to calculate the limits in probLimIN, just need to make sure that variable passed via dummy argument problimPct has the same number of columns as the number of probabilitiy limits
! 2. Calcproblim sorts each replicate of inputdata into ascending order, and therefore each column of probLimPct is in ascending order

! Write out Results
open(unit=10,file="problim.txt",status='replace',iostat=status)
if (status/=0) write(*,*) "Unable to open file, IO error:",status
do i=1,ndata
  write(10,'(4e13.6)') ARI(ndata-i+1),problim(i,:) ! Using ARI(ndata-i) as problim is sorted into ascending order - refer to Note 2 above
end do
close(unit=10)

end subroutine







