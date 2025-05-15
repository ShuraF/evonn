module mod_evonn
    USE OMP_LIB
    implicit none
    public slstm_new_lbl, init_random_seed, Slstm_forward_0NOB, father
    TYPE slstm_type
        INTEGER :: Dhid,Dinp,Dout
        INTEGER :: dpthmax, l
        REAL(8) :: dpth,mix_pc, noise_lev_ass, noise_step_ass, noise_lev_man, noise_step_man
        REAL(8), DIMENSION(:,:,:), ALLOCATABLE :: W, V ! ,goutAR
        REAL(8), DIMENSION(:,:), ALLOCATABLE :: Wout, B, state0 ! ,goutAR
        INTEGER, DIMENSION(:), ALLOCATABLE :: Bout, perm
    END TYPE slstm_type
contains

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
                call random_stdnormal(slstm%Wout(j,i))
            enddo
        enddo
        slstm%Wout = slstm%Wout / SQRT(Dinp/2.d0)

        slstm%Bout = 0.d0

        slstm%state0 = 0.d0

        slstm%perm = perm


    END SUBROUTINE slstm_new_lbl


subroutine init_random_seed()


      INTEGER :: i, n, clock
      INTEGER, DIMENSION(:), ALLOCATABLE :: seed

      CALL RANDOM_SEED(size = n)
      ALLOCATE(seed(n))

      CALL SYSTEM_CLOCK(COUNT=clock)

      seed = clock + 37 * (/ (i - 1, i = 1, n) /)
      CALL RANDOM_SEED(PUT = seed)

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

end function father
 
subroutine random_stduniform(u)
   implicit none
   real(8),intent(out) :: u
   real(8) :: r
   call random_number(r)
   u = 1 - r
end subroutine random_stduniform

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

    do j=1,nint(depth)
        do i=1,4
            HF(1:slstm%Dhid,i) = matmul(transpose(slstm%W(1:slstm%Dhid,1:slstm%Dhid,i)),H(1:slstm%Dhid)) + &
             matmul(transpose(slstm%V(1:slstm%Dinp,1:slstm%Dhid,i)),inp(1:slstm%Dinp,j))
        enddo    


        HF(:,1:3) = sigmoidCP(HF(:,1:3))
        
        HF(:,4) = tanh(HF(:,4))

        C = HF(:,1)*C+HF(:,2)*HF(:,4)
        H = HF(:,3)*tanh(C)
    enddo

    y = matmul(transpose(slstm%Wout(1:slstm%Dhid,1:slstm%Dout)),H)

    y = exp(y-maxval(y))
    y = y/sum(y)

    Slstm_forward_0NOB = y(1)
end function Slstm_forward_0NOB

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


end module mod_evonn