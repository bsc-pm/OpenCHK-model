PROGRAM TEST_ARRAY 
   include 'mpif.h'
   integer rank, size, ierror, tag, comm, status(MPI_STATUS_SIZE)
   integer restored_i = 0, restored = 0, inject_error = -1, i
   integer, dimension(1024*1024) :: actual_data
   CHARACTER(len=32) :: arg
   
   
   call MPI_INIT(ierror)
   comm = MPI_COMM_WORLD

   if (iargc() == 1) then
       call getarg(1, arg)
       read (arg,'(I10)') inject_error
       print *, 'Inject error at step ', inject_error, '.'
   endif

!$chk init comm(comm)
!$chk load(restored_i, actual_data)
   if (restored_i .ne. 0) then
       print *, 'Restored data from iteration ', restored_i , '. data[i] = ', actual_data(restored_i), '.'
       restored = 1
   end if

   do i = restored_i, 1024*1024
       actual_data(i) = i
!$chk store(i, actual_data) kind(0) id(i) level(mod(i,4)+1) if(mod(i,(1024*128)) .eq. 1)
       if (i .eq. inject_error .AND. restored .eq. 0) then
           print *, 'Injected error'
           call EXIT(-1)
       end if
   end do
!$chk shutdown

   do i = 1, 1024*1024
      if (actual_data(i) .ne. i) then
          call EXIT(-1)
      end if
   end do

   call MPI_FINALIZE(ierror)
END PROGRAM
