PROGRAM T1 
   include 'mpif.h'
   integer rank, size, ierror, tag, comm, status(MPI_STATUS_SIZE)
   integer actual_data = 0, restored_i = 0, restored = 0, inject_error = -1, i
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
       print *, 'Restored data from iteration ', restored_i , '. data = ', actual_data, '.'
       restored = 1
   end if

   do i = restored_i, 10
       actual_data = i
!$chk store(i, actual_data) kind(0) id(i) level(mod(i,4)+1) if(i .ge. 0)
       if (i .eq. inject_error .AND. restored .eq. 0) then
           print *, 'Injected error'
           call EXIT(-1)
       end if
       print *, 'Completed step ', i
   end do
!$chk shutdown

   call MPI_FINALIZE(ierror)
END PROGRAM
