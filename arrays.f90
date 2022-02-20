program arrays

implicit none

integer :: ii, jj, kk, matrix_size
integer, allocatable, dimension(:,:) :: dynamic_matrix

ii = 0
jj = 0
kk = 0

do
  write(6,*) "Give the matrix size: "
  read(*,*) matrix_size

  if (matrix_size<=0) exit ! termination or convergence criteria

  allocate(dynamic_matrix(0:matrix_size, 0:matrix_size))

  kk = 0
  do ii = 0, size(dynamic_matrix,1) - 1     ! CHECK THIS WARNING
    do jj = 0, size(dynamic_matrix,2) - 1
      kk = kk + 1
      dynamic_matrix(ii,jj) = kk
    enddo
  enddo

  write(6,*) "Printing matrix.."
  do ii = 0, size(dynamic_matrix,1) - 1
      write(6,*) (dynamic_matrix(ii,jj), jj=0, size(dynamic_matrix,2) - 1)
  enddo

  deallocate(dynamic_matrix)
  write(6,*) "starting new loop"
  write(6,*)
enddo

write(6,*) "Exiting code.."
end program arrays
