module mod_slstm_omp_evol
    USE OMP_LIB
    implicit none
    public slstm_new_lbl, init_random_seed, scramble, Slstm_forward_0NOB, father
    TYPE slstm_type
        INTEGER :: Dhid,Dinp,Dout
        INTEGER :: dpthmax, l
        REAL(8) :: dpth,mix_pc, noise_lev_ass, noise_step_ass, noise_lev_man, noise_step_man
        REAL(8), DIMENSION(:,:,:), ALLOCATABLE :: W, V ! ,goutAR
        REAL(8), DIMENSION(:,:), ALLOCATABLE :: Wout, B, state0 ! ,goutAR
        INTEGER, DIMENSION(:), ALLOCATABLE :: Bout, perm
    END TYPE slstm_type
contains
!  W,V,B,Wout,Bout,dpth,dpthmax,state0,goutAR,perm 

!    Converts a square matrix a to sparse storage format as sa.Only elements of a with magnitude
! ≥thresh are retained.
!>  label
!! aaaa
    SUBROUTINE slstm_new_lbl(Dhid,Dinp,Dout,perm,slstm,lbl)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: Dhid,Dinp,Dout,lbl
        INTEGER, DIMENSION(:), INTENT(IN) :: perm
        TYPE(slstm_type), INTENT(INOUT) :: slstm
        INTEGER :: Z
        INTEGER :: i,j,k

        slstm%Dhid = Dhid
        slstm%Dinp = Dinp
        slstm%Dout = Dout

        slstm%l = lbl

        slstm%mix_pc = 0.5d0

        Z = Dhid + Dinp
        slstm%dpth = (Dhid*Dout+4*Dhid**2+4*Dinp*Dhid)/Dinp !changed not to include B's
        slstm%dpthmax = int(slstm%dpth)

        slstm%noise_lev_ass = -1.d0
        slstm%noise_step_ass = -1.d0
        slstm%noise_lev_man = -1.d0
        slstm%noise_step_man = -1.d0
        
        !write(*,*) 'dpth', slstm%dpth

        allocate(slstm%W(Dhid, Dhid, 4), slstm%V(Dinp,Dhid, 4), slstm%B(Dhid,4))
        allocate(slstm%Wout(Dhid,Dout), slstm%Bout(Dout), slstm%state0(Dhid,2))
        allocate(slstm%perm(Dhid*Dout+4*Dhid**2+4*Dinp*Dhid))

        do i=1,4
            do j=1,Dhid
                do k=1,Dhid
                    !slstm%W(k,j,i) =  random_stdnormal()
                    call random_stdnormal(slstm%W(k,j,i))
                enddo
            enddo
        enddo
        slstm%W = slstm%W / SQRT(Z/2.d0)

        do i=1,4
            do j=1,Dhid
                do k=1,Dinp
                    !slstm%V(k,j,i) =  random_stdnormal()
                    call random_stdnormal(slstm%V(k,j,i))
                enddo
            enddo
        enddo
        slstm%V = slstm%V / SQRT(Z/2.d0)

        slstm%B   = 0.d0

        do i=1,Dout
            do j=1,Dhid
                !slstm%Wout(j,i) =  random_stdnormal()
                call random_stdnormal(slstm%Wout(j,i))
            enddo
        enddo
        slstm%Wout = slstm%Wout / SQRT(Dinp/2.d0)

        slstm%Bout = 0.d0

        slstm%state0 = 0.d0

        slstm%perm = perm

        ! do i=1,Dhid*Dout+4*Dhid**2+4*Dinp*Dhid
        !     slstm%perm(i) = perm(i)
        ! enddo

    END SUBROUTINE slstm_new_lbl


subroutine init_random_seed()


      INTEGER :: i, n, clock
      INTEGER, DIMENSION(:), ALLOCATABLE :: seed

      CALL RANDOM_SEED(size = n)
      ALLOCATE(seed(n))

      CALL SYSTEM_CLOCK(COUNT=clock)

      seed = clock + 37 * (/ (i - 1, i = 1, n) /)
      !seed = 13663671
      CALL RANDOM_SEED(PUT = seed)

      !WRITE(*,*) seed

      DEALLOCATE(seed)
end

integer function father(gain)
    implicit none
    real(8),dimension(100),intent(in) :: gain
    real(8) :: r
    call random_number(r)
    father = 1
    do while (gain(father)<r)
        father = father + 1
    enddo
    !father = findloc(gain>r,.true.,DIM=1)
end function father
 
subroutine random_stduniform(u)
   implicit none
   real(8),intent(out) :: u
   real(8) :: r
   call random_number(r)
   u = 1 - r
end subroutine random_stduniform

! assuming a<b
subroutine random_uniform(a,b,x)
   implicit none
   real(8),intent(in) :: a,b
   real(8),intent(out) :: x
   real(8) :: u
   call random_stduniform(u)
   x = (b-a)*u + a
end subroutine random_uniform

subroutine random_stdnormal(x)
   implicit none
   real(8), intent(out) :: x
   real(8),parameter :: pi=3.14159265
   real(8) :: u1,u2
    call random_stduniform(u1)
    call random_stduniform(u2)
    x = sqrt(-2*log(u1))*cos(2*pi*u2)
end subroutine random_stdnormal

function scramble( number_of_values ) result(array)

!@(#) M_random::scramble(3f): return integer array of random values 1 to N.
      integer,intent(in)    :: number_of_values
      integer,allocatable   :: array(:)
      integer               :: i, j, k, m, n
      integer               :: temp
      real                  :: u

      array=[(i,i=1,number_of_values)]

! The intrinsic RANDOM_NUMBER(3f) returns a real number (or an array
! of such) from the uniform distribution over the interval [0,1). (ie.
! it includes 0 but not 1.).
!
! To have a discrete uniform distribution on
! the integers {n, n+1, ..., m-1, m} carve the continuous distribution
! up into m+1-n equal sized chunks, mapping each chunk to an integer.
!
! One way is:
!   call random_number(u)
!   j = n + FLOOR((m+1-n)*u)  ! choose one from m-n+1 integers

      n=1
      m=number_of_values
      do k=1,2
         do i=1,m
            call random_number(u)
            j = n + FLOOR((m+1-n)*u)
            ! switch values
            temp=array(j)
            array(j)=array(i)
            array(i)=temp
         enddo
      enddo

end function scramble

real(8) function Slstm_forward_0NOB(inp,slstm,depth)
    TYPE(slstm_type), INTENT(IN) :: slstm
    real(8), dimension(:,:), INTENT(IN) :: inp
    real(8), INTENT(IN) :: depth
    real(8), dimension(slstm%Dhid) :: H,C
    real(8), dimension(slstm%Dhid,4) :: HF
    real(8), dimension(slstm%Dout) :: y
    ! real(8) :: yyy
    INTEGER :: i,j

    H = slstm%state0(1:slstm%Dhid,1)
    C = slstm%state0(1:slstm%Dhid,2)

    !write(*,*)  shape(slstm%W)


    !make elemental function

    !do j=1,int(slstm%dpth)
    do j=1,nint(depth)
        do i=1,4
            HF(1:slstm%Dhid,i) = matmul(transpose(slstm%W(1:slstm%Dhid,1:slstm%Dhid,i)),H(1:slstm%Dhid)) + &
             matmul(transpose(slstm%V(1:slstm%Dinp,1:slstm%Dhid,i)),inp(1:slstm%Dinp,j))
        enddo    

        ! yyy = maxval(HF(:,1:3))
        ! yyy = minval(HF(:,1:3))

        HF(:,1:3) = sigmoidCP(HF(:,1:3))
        
        HF(:,4) = tanh(HF(:,4))

        C = HF(:,1)*C+HF(:,2)*HF(:,4)
        H = HF(:,3)*tanh(C)
    enddo

    y = matmul(transpose(slstm%Wout(1:slstm%Dhid,1:slstm%Dout)),H)


    !softmax
    ! write(*,*) 'y', y
    ! write(*,*) 'macval y', maxval(y)
    y = exp(y-maxval(y))
    y = y/sum(y)

    Slstm_forward_0NOB = y(1)
end function Slstm_forward_0NOB


! subroutine Slstm_A2B(slstmA,slstmB)
!     TYPE(slstm_type), INTENT(IN) :: slstmA
!     TYPE(slstm_type), INTENT(INOUT) :: slstmB

!     slstmB%Dhid = slstmA%Dhid
!     slstmB%Dinp = slstmA%Dinp
!     slstmB%Dout = slstmA%Dout
!     slstmB%dpth = slstmA%dpth
!     slstmB%dpthmax = slstmA%dpthmax
!     slstmB%W = slstmA%W
!     slstmB%V = slstmA%V
!     slstmB%Wout = slstmA%Wout
!     !slstmB%B = slstmA%B
!     slstmB%state0 = slstmA%state0
!     !slstmB%Bout = slstmA%Bout
!     slstmB%perm = slstmA%perm


! end subroutine Slstm_A2B

elemental real(8) function sigmoidCP(x)
    real(8), intent(in) ::  x
    if(x >= 0) then
        sigmoidCP = 1.d0 / (1.d0 + exp(-x))
    else
        sigmoidCP = exp(x) / (1.d0 + exp(x))
    endif
end function sigmoidCP


subroutine MutateSLSTMNOBmemdiflbl(slstm,Wstrength)

    ! Mutate single player\\
    ! Wstrenth >0 < 1 size of mutation step\\
    ! Depth strength - size of depth step
    ! changes model

    TYPE(slstm_type), INTENT(INOUT) :: slstm
    real(8), INTENT(IN) :: Wstrength
    integer :: Dhid, Dinp, Dout
    real(8) :: u
    integer :: i, j, k

    Dhid = slstm%Dhid
    Dinp = slstm%Dinp
    Dout = slstm%Dout

    !$OMP PARALLEL PRIVATE(u)
    !$OMP DO COLLAPSE(3)
    do i=1,4
        do j=1,Dhid
            do k=1,Dhid
                !slstm%W(k,j,i) =  random_stdnormal()
                call random_stdnormal(u)
                    slstm%W(k,j,i) = slstm%W(k,j,i) + Wstrength*u
            enddo
        enddo
    enddo
    !$OMP END DO

    !$OMP DO COLLAPSE(3)
    do i=1,4
        do j=1,Dhid
            do k=1,Dinp
                !slstm%V(k,j,i) =  random_stdnormal()
                call random_stdnormal(u)
                    slstm%V(k,j,i) = slstm%V(k,j,i) +  Wstrength*u

            enddo
        enddo
    enddo
    !$OMP END DO

    !$OMP DO COLLAPSE(2)
    do i=1,Dout
        do j=1,Dhid
            !slstm%Wout(j,i) =  random_stdnormal()
            call random_stdnormal(u)

                slstm%Wout(j,i) = slstm%Wout(j,i) +  Wstrength*u

        enddo
    enddo
    !$OMP END DO
    !$OMP END PARALLEL

end subroutine MutateSLSTMNOBmemdiflbl


end module mod_slstm_omp_evol