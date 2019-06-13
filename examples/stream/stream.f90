! Program: STREAM
! Programmer: John D. McCalpin, P. Vezolle
!
!-----------------------------------------------------------------------
! Copyright 1991-2003: John D. McCalpin
!-----------------------------------------------------------------------
! License:
!  1. You are free to use this program and/or to redistribute
!     this program.
!  2. You are free to modify this program for your own use,
!     including commercial use, subject to the publication
!     restrictions in item 3.
!  3. You are free to publish results obtained from running this
!     program, or from works that you derive from this program,
!     with the following limitations:
!     3a. In order to be referred to as "STREAM benchmark results",
!         published results must be in conformance to the STREAM
!         Run Rules, (briefly reviewed below) published at
!         http://www.cs.virginia.edu/stream/ref.html
!         and incorporated herein by reference.
!         As the copyright holder, John McCalpin retains the
!         right to determine conformity with the Run Rules.
!     3b. Results based on modified source code or on runs not in
!         accordance with the STREAM Run Rules must be clearly
!         labelled whenever they are published.  Examples of
!         proper labelling include:
!         "tuned STREAM benchmark results"
!         "based on a variant of the STREAM benchmark code"
!         Other comparable, clear and reasonable labelling is
!         acceptable.
!     3c. Submission of results to the STREAM benchmark web site
!         is encouraged, but not required.
!  4. Use of this program or creation of derived works based on this
!     program constitutes acceptance of these licensing restrictions.
!  5. Absolutely no warranty is expressed or implied.
!-----------------------------------------------------------------------
!                            MPI VERSION
!
!  Latest Modification: February 14, 2014 (John D. McCalpin)
!     Minor update to output formatting to prevent overflows.
!  Latest Modification: May 17, 2002 (P. Vezolle)
!  Based on STREAM version 5.0, July 30, 2000
!
! This version has been shown to work under the MPI environment
! of IBM's AIX 5.1 --- comments or suggestions on how to improve
! portability are welcome!    mailto:john@mccalpin.com
!
! This program measures memory transfer rates in MB/s for simple
! computational kernels coded in Fortran.
! The intent is to demonstrate the extent to which ordinary user
! code can exploit the main memory bandwidth of the system under
! test.
!=========================================================================
! The STREAM web page is at:
!          http://www.streambench.org
!
! Most of the content is currently hosted at:
!          http://www.cs.virginia.edu/stream/
!
! BRIEF INSTRUCTIONS:
!       0) See http://www.cs.virginia.edu/stream/ref.html for details
!       1) STREAM requires a timing function called mysecond().
!          Several examples are provided in this directory.
!          "CPU" timers are only allowed for uniprocessor runs.
!          "Wall-clock" timers are required for all multiprocessor runs.
!       2) The STREAM array sizes must be set to size the test.
!          The value "N" must be chosen so that each of the three
!          arrays is at least 4x larger than the sum of all the last-
!          level caches used in the run, or 1 million elements, which-
!          ever is larger.
!          ------------------------------------------------------------
!          Note that you are free to use any array length and offset
!          that makes each array 4x larger than the last-level cache.
!          The intent is to determine the *best* sustainable bandwidth
!          available with this simple coding.  Of course, lower values
!          are usually fairly easy to obtain on cached machines, but
!          by keeping the test to the *best* results, the answers are
!          easier to interpret.
!          You may put the arrays in common or not, at your discretion.
!          There is a commented-out COMMON statement below.
!          Fortran90 "allocatable" arrays are fine, too.
!          ------------------------------------------------------------
!       3) Compile the code with full optimization.  Many compilers
!          generate unreasonably bad code before the optimizer tightens
!          things up.  If the results are unreasonably good, on the
!          other hand, the optimizer might be too smart for me
!          Please let me know if this happens.
!       4) Mail the results to mccalpin@cs.virginia.edu
!          Be sure to include:
!               a) computer hardware model number and software revision
!               b) the compiler flags
!               c) all of the output from the test case.
!          Please let me know if you do not want your name posted along
!          with the submitted results.
!       5) See the web page for more comments about the run rules and
!          about interpretation of the results
!
! Thanks,
!   Dr. Bandwidth
!=========================================================================
!
!=========================================================================
! Customized version by BSC using OpenCHK model 
! See LICENSE
!=========================================================================


PROGRAM stream_mpi

    IMPLICIT NONE
    include 'mpif.h'

    INTERFACE
        FUNCTION reallocate_2d(array, sz1, newsz2)
            IMPLICIT NONE
            DOUBLE PRECISION, POINTER :: reallocate_2d(:,:)
            DOUBLE PRECISION, POINTER :: array(:,:)
            INTEGER :: sz1, newsz2
        END FUNCTION reallocate_2d
    END INTERFACE

    ! .. External Functions ..
    INTEGER :: omp_get_num_threads
    INTEGER :: realsize
    DOUBLE PRECISION :: mysecond
    EXTERNAL mysecond,realsize

    ! .. Intrinsic Functions ..
    INTRINSIC :: dble, max, min, nint, sqrt
    INTEGER :: iargc

    ! .. Parameters ..
    INTEGER(8) :: total_size, size_units
    INTEGER :: n, offset, ndim, ntimes, mtimes

    ! .. Auxiliar variables ..
    INTEGER :: i, val
    DOUBLE PRECISION :: rval
    CHARACTER(len=32) :: arg
    LOGICAL :: keep_running

    ! .. MPI Variables ..
    INTEGER :: numtask, rank, rc
    DOUBLE PRECISION :: rc1

    ! .. Local Scalars ..
    DOUBLE PRECISION :: scalar, t
    INTEGER :: j, k, nbpw, kk

    ! .. Local Arrays ..
    INTEGER :: bytes(4)
    CHARACTER*11 :: label(4)

    ! .. Arrays ..
    DOUBLE PRECISION, ALLOCATABLE :: a(:), b(:), c(:)

    ! .. Data statements ..
    DATA label/'Copy:      ','Scale:     ','Add:       ','Triad:     '/
    DATA bytes/2,2,3,3/

    INTEGER :: fault, ierr, comm

    ! Possible arguments are:
    !   $ PROGRAM [total_size] [ntimes] [mtimes] [chekpoint]
    !           total_size = total problem size in MiB, GiB or TiB
    !           ntimes = number of iterations
    !           mtimes = sub-section repetition
    !           fault = iteration number where injecting an error 
    ! Default values:
    total_size = 100*1024*1024  ! 100 MiB
    ntimes = 100
    mtimes = 1
    offset = 0
    fault = 0
    DO i = 1, iargc()
        CALL getarg(i, arg)
        SELECT CASE (i)
            ! First argument, total size, e.g.: 1.5G
            CASE (1)
                size_units = 0
                DO j=1, 32
                    SELECT CASE (arg(j:j))
                        CASE('M')
                            size_units = 1024_8**2
                            EXIT
                        CASE('G')
                            size_units = 1024_8**3
                            EXIT
                        CASE('T')
                            size_units = 1024_8**4
                            EXIT
                    END SELECT
                ENDDO
                IF ( size_units .EQ. 0 ) THEN
                    print *, "Bad Format"
                    STOP
                ENDIF
                READ (arg(1:j-1),*) rval
                total_size = rval * size_units
            ! Second argument, number of iterations
            CASE(2)
                READ (arg,'(I10)') ntimes
            ! Third argument, Sub-section repetition
            CASE(3)
                READ (arg,'(I10)') mtimes
            ! Fourth argument, fault injection 
            CASE(4)
                READ (arg,'(I10)') fault 
        END SELECT
    ENDDO


    ! --- SETUP --- determine precision and check timing ---

    ! .. MPI Initialization ..

    call MPI_INIT ( rc )
    if ( rc .ne. 0 ) then
        WRITE(*,*) ' MPI Initialization problem, error code: ',rc
        stop
    endif
    comm = MPI_COMM_WORLD
    !$CHK INIT COMM(comm)
    call MPI_COMM_RANK ( MPI_COMM_WORLD, rank, rc )
    call MPI_COMM_SIZE ( MPI_COMM_WORLD, numtask, rc )

    !!! Compute N considering numtask and allocate the arrays
    nbpw = realsize( rank == 0 )
    n = total_size / (3.0*nbpw*numtask)
    ndim = n + offset
    allocate( a(ndim) )
    allocate( b(ndim) )
    allocate( c(ndim) )

    if ( rank .eq. 0 ) then
        WRITE (*,*)        'Number of processors = ', numtask
        WRITE (*,FMT=9010) 'Array size = ',n
        WRITE (*,FMT=9010) 'Offset     = ',offset
        WRITE (*,FMT=9060) 'The total memory requirement is ',      &
                            3.0*nbpw*n*numtask/(1024*1024),' MiB',  &
                            ' (',3.0*nbpw*n/(1024*1024),'MiB/task)'
        WRITE (*,*) 'You are running each test ',ntimes,' times'
        WRITE (*,*) 'repeating each sub-section ',mtimes,' times'
        WRITE (*,FMT=9030) '--'
        WRITE (*,FMT=9030) 'The *best* time for each test is used'
        WRITE (*,FMT=9030) '*EXCLUDING* the first and last iterations'

        !$OMP PARALLEL
        !$OMP MASTER
            PRINT *,'----------------------------------------------'
            PRINT *,'Number of Threads = ',OMP_GET_NUM_THREADS()
        !$OMP END MASTER
        !$OMP END PARALLEL

        !$OMP PARALLEL
            PRINT *,'Printing one line per active thread....'
        !$OMP END PARALLEL

        !$OMP PARALLEL
        !$OMP MASTER
            PRINT *,'----------------------------------------------'
        !$OMP END MASTER
        !$OMP END PARALLEL
    endif

    !$OMP PARALLEL DO SCHEDULE(dynamic, n/64)
    DO j = 1,n
        a(j) = 2.0d0
        b(j) = 0.5D0
        c(j) = 0.0D0
    ENDDO

    !$OMP PARALLEL DO SCHEDULE(dynamic, n/64)
    DO j = 1,n
        a(j) = 0.5d0*a(j)
    ENDDO

    call MPI_BARRIER(MPI_COMM_WORLD, rc)

    ! --- MAIN LOOP --- repeat test cases NTIMES times ---
    keep_running = .true.
    scalar = 0.5d0*a(1)
    k = 1

    !!! Recover checkpoint if necessary
    !$CHK LOAD(k, a, b, c)
    DO WHILE ( (k <= ntimes) .AND. (keep_running == .TRUE.) )
        !$CHK STORE(k, a, b, c) ID(k) LEVEL(4) KIND(0) IF(mod(k,(ntimes/10)) == 0 .AND. k > 1)
        IF ((mod(k,(ntimes/10))==0) .AND. (k > 1)) THEN
            IF (rank .eq. 0) THEN
                PRINT *, "Checkpoint completed at step ", k
            ENDIF
        ENDIF

        IF ( k .eq. fault ) THEN
            PRINT *, "CRASHING!"
            call MPI_ABORT( MPI_COMM_WORLD, -1, ierr )
        ENDIF

        !********** COPY

        call MPI_BARRIER(MPI_COMM_WORLD, rc)
        DO kk = 1,mtimes
            !$OMP PARALLEL DO SCHEDULE(dynamic, n/64)
            DO j = 1,n
                c(j) = a(j)
           ENDDO
        ENDDO
        call MPI_BARRIER(MPI_COMM_WORLD, rc)

        !********** SCALE

        call MPI_BARRIER(MPI_COMM_WORLD, rc)
        DO kk = 1,mtimes
            !$OMP PARALLEL DO SCHEDULE(dynamic, n/64)
            DO j = 1,n
                b(j) = scalar*c(j)
            ENDDO
        ENDDO
        call MPI_BARRIER(MPI_COMM_WORLD, rc)

        !********** ADD

        call MPI_BARRIER(MPI_COMM_WORLD, rc)
        DO kk = 1,mtimes
            !$OMP PARALLEL DO SCHEDULE(dynamic, n/64)
            DO j = 1,n
                c(j) = a(j) + b(j)
            ENDDO
        ENDDO
        call MPI_BARRIER(MPI_COMM_WORLD, rc)

        !********** TRIAD

        call MPI_BARRIER(MPI_COMM_WORLD, rc)
        DO kk = 1,mtimes
            !$OMP PARALLEL DO SCHEDULE(dynamic, n/64)
            DO j = 1,n
                a(j) = b(j) + scalar*c(j)
            ENDDO
        ENDDO
        call MPI_BARRIER(MPI_COMM_WORLD, rc)

        IF (rank .eq. 0 ) THEN
            WRITE(6, *) 'End of step ', k 
        ENDIF
    
        k = k + 1
    ENDDO

    ntimes = min(ntimes, k)


    ! --- SUMMARY ---
    CALL checksums (a,b,c,n,ntimes,rank,numtask)


    call MPI_BARRIER(MPI_COMM_WORLD, rc)
    deallocate(a)
    deallocate(b)
    deallocate(c)
    !$CHK SHUTDOWN
    call MPI_FINALIZE ( rc )
    stop

 9000 FORMAT (1x,a,i6,a)
 9010 FORMAT (1x,a,i10)
 9020 FORMAT (1x,a,i4,a)
 9030 FORMAT (1x,a,i3,a,a)
 9040 FORMAT ('Function',5x,'Rate (MiB/s)  Avg time   Min time  Max time')
 9050 FORMAT (a,f12.1,3 (f11.6,2x))
 9060 FORMAT (1x,a,f9.1,a,a,f9.1,a)

END PROGRAM stream_mpi

!-------------------------------------
! INTEGER FUNCTION realsize()
!
! A semi-portable way to determine the precision of DOUBLE PRECISION
! in Fortran.
! Here used to guess how many bytes of storage a DOUBLE PRECISION
! number occupies.
!
INTEGER FUNCTION realsize( verbose )

    IMPLICIT NONE
    LOGICAL, INTENT(IN) :: verbose

    ! .. Local Scalars ..
    DOUBLE PRECISION result,test
    INTEGER j,ndigits

    ! .. Local Arrays ..
    DOUBLE PRECISION ref(30)

    ! .. External Subroutines ..
    EXTERNAL confuse

    ! .. Intrinsic Functions ..
    INTRINSIC abs,acos,log10,sqrt

    DO j = 1,30
        ref(j) = 1.0d0 + 10.0d0** (-j)
    ENDDO

    DO j = 1,30
        test = ref(j)
        ndigits = j
        CALL confuse(test,result)
        IF (test.EQ.1.0D0) THEN
            IF (ndigits.LE.8) THEN
                realsize = 4
            ELSE
                realsize = 8
            END IF
            IF (verbose ) THEN
                WRITE (*,FMT='(a)') '----------------------------------------------'
                WRITE (*,FMT='(1x,a,i2,a)') 'Double precision appears to have ', ndigits,' digits of accuracy'
                WRITE (*,FMT='(1x,a,i1,a)') 'Assuming ',realsize,' bytes per DOUBLE PRECISION word'
                WRITE (*,FMT='(a)')  '----------------------------------------------'
            END IF
            RETURN
        END IF
    ENDDO

    PRINT *,'Hmmmm.  I am unable to determine the size.'
    PRINT *,'Please enter the number of Bytes per DOUBLE PRECISION number : '
    READ (*,FMT=*) realsize
    IF (realsize.NE.4 .AND. realsize.NE.8) THEN
        PRINT *,'Your answer ',realsize,' does not make sense.'
        PRINT *,'Try again.'
        PRINT *,'Please enter the number of Bytes per DOUBLE PRECISION number : '
        READ (*,FMT=*) realsize
    END IF
    PRINT *,'You have manually entered a size of ',realsize,' bytes per DOUBLE PRECISION number'
    WRITE (*,FMT='(a)') '----------------------------------------------'

END FUNCTION realsize


SUBROUTINE confuse(q,r)

    IMPLICIT NONE

    ! .. Scalar Arguments ..
    DOUBLE PRECISION q,r

    ! .. Intrinsic Functions ..
    INTRINSIC cos

    r = cos(q)
    RETURN

END SUBROUTINE confuse

SUBROUTINE checksums(a,b,c,n,ntimes,rank,numtask)

    IMPLICIT NONE
    include 'mpif.h'

    ! .. Arguments ..
    DOUBLE PRECISION a(*),b(*),c(*)
    INTEGER n,ntimes
    INTEGER rank,numtask,rc, Stat(MPI_STATUS_SIZE)
    DOUBLE PRECISION Status(2), ival

    ! .. Local Scalars ..
    DOUBLE PRECISION aa,bb,cc,scalar,suma,sumb,sumc,epsilon
    INTEGER i,j,k

    ival = 0

    ! Repeat the main loop, but with scalars only.
    ! This is done to check the sum & make sure all
    ! iterations have been executed correctly.

    aa = 2.0D0
    bb = 0.5D0
    cc = 0.0D0
    aa = 0.5D0*aa
    scalar = 0.5d0*aa
    DO k = 1,ntimes
        cc = aa
        bb = scalar*cc
        cc = aa + bb
        aa = bb + scalar*cc
    END DO
    aa = aa*DBLE(n-2)
    bb = bb*DBLE(n-2)
    cc = cc*DBLE(n-2)

    ! Now sum up the arrays, excluding the first and last
    ! elements, which are modified using the timing results
    ! to confuse aggressive optimizers.

    suma = 0.0d0
    sumb = 0.0d0
    sumc = 0.0d0

    !$OMP PARALLEL DO REDUCTION(+:suma,sumb,sumc) SCHEDULE(dynamic, n/64)
    DO j = 2,n-1
        suma = suma + a(j)
        sumb = sumb + b(j)
        sumc = sumc + c(j)
    ENDDO

    epsilon = 1.D-6

    ! .. Gather results by process  0
    IF (ABS(suma-aa)/suma .GT. epsilon) THEN
        Status(1) = aa
        Status(2) = suma
    ELSE
        Status(1) = 0
    ENDIF

    if ( rank .ne. 0 ) then
        call MPI_SEND(Status, 2, MPI_DOUBLE_PRECISION, 0, rank, MPI_COMM_WORLD, rc)
    else
        ival = ival + Status(1)
        if ( Status(1) .ne. 0 ) then
            PRINT *,'Failed Validation on array a(), Process ',rank
            PRINT *,'Target   Sum of a is = ',Status(1)
            PRINT *,'Computed Sum of a is = ',Status(2)
        endif
        do i=1,numtask-1
            call MPI_RECV(Status, 2, MPI_DOUBLE_PRECISION, i, i, MPI_COMM_WORLD, Stat, rc);
            ival = ival + Status(1)
            if ( Status(1) .ne. 0 ) then
                PRINT *,'Failed Validation on array a(), Process ',i
                PRINT *,'Target   Sum of a is = ',Status(1)
                PRINT *,'Computed Sum of a is = ',Status(2)
            endif
        enddo
    endif

    IF (ABS(sumb-bb)/sumb .GT. epsilon) THEN
        Status(1) = bb
        Status(2) = sumb
    ELSE
        Status(1) = 0
    ENDIF

    if ( rank .ne. 0 ) then
        call MPI_SEND(Status, 2, MPI_DOUBLE_PRECISION, 0, 2*rank, MPI_COMM_WORLD, rc)
    else
        ival = ival + Status(1)
        if ( Status(1) .ne. 0 ) then
            PRINT *,'Failed Validation on array b(), Process ',rank
            PRINT *,'Target   Sum of b is = ',Status(1)
            PRINT *,'Computed Sum of b is = ',Status(2)
        endif
        do i=1,numtask-1
            call MPI_RECV(Status, 2, MPI_DOUBLE_PRECISION, i, 2*i, MPI_COMM_WORLD, Stat, rc);
            ival = ival + Status(1)
            if ( Status(1) .ne. 0 ) then
                PRINT *,'Failed Validation on array b(), Process ',i
                PRINT *,'Target   Sum of b is = ',Status(1)
                PRINT *,'Computed Sum of b is = ',Status(2)
            endif
        enddo
    endif

    IF (ABS(sumc-cc)/sumc .GT. epsilon) THEN
        Status(1) = cc
        Status(2) = sumc
    ELSE
        Status(1) = 0
    ENDIF

    if ( rank .ne. 0 ) then
        call MPI_SEND(Status, 2, MPI_DOUBLE_PRECISION, 0, 3*rank, MPI_COMM_WORLD, rc)
    else
        ival = ival + Status(1)
        if ( Status(1) .ne. 0 ) then
            PRINT *,'Failed Validation on array c(), Process ',rank
            PRINT *,'Target   Sum of c is = ',Status(1)
            PRINT *,'Computed Sum of c is = ',Status(2)
        endif
        do i=1,numtask-1
            call MPI_RECV(Status, 2, MPI_DOUBLE_PRECISION, i, 3*i, MPI_COMM_WORLD, Stat, rc);
            ival = ival + Status(1)
            if ( Status(1) .ne. 0 ) then
                PRINT *,'Failed Validation on array c(), Process ',i
                PRINT *,'Target   Sum of c is = ',Status(1)
                PRINT *,'Computed Sum of c is = ',Status(2)
            endif
        enddo

        if ( ival .eq. 0. ) then
            PRINT *,'-----------------------------------------------'
            PRINT*,'Solution Validates!'
            PRINT *,'-----------------------------------------------'
        endif
    endif

END SUBROUTINE checksums

FUNCTION reallocate_2d(array, sz1, newsz2)

    IMPLICIT NONE
    DOUBLE PRECISION, POINTER :: reallocate_2d(:,:)
    DOUBLE PRECISION, POINTER :: array(:,:)
    INTEGER :: sz1, newsz2

    ALLOCATE(reallocate_2d(1:sz1, 1:newsz2))
    reallocate_2d(:, 1:ubound(array,2)) = array(:,:)
    reallocate_2d(:, ubound(array,2)+1:newsz2) = 0
    DEALLOCATE(array)

END FUNCTION reallocate_2d
