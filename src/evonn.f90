PROGRAM evonn

    USE HDF5
    USE mod_evonn
    USE mod_rw_hdf5
    USE OMP_LIB

    IMPLICIT NONE

    CHARACTER(LEN=124) :: str1, filenameout
    real(8), DIMENSION(:,:), ALLOCATABLE :: Xinp_i, Xinp_j, resps
    INTEGER :: error, mix_flag
    INTEGER :: Dhid, Dinp, Dout, Dhid_start
    INTEGER :: depth, maxdepth, Ninteract
    INTEGER :: N, Runs
    INTEGER, DIMENSION(:), ALLOCATABLE :: perm
    real(8), dimension(:), ALLOCATABLE :: inp_st1, inp_st2
    INTEGER :: i, j, k, m
    TYPE(slstm_type), dimension(:), pointer :: pop, popnext
    TYPE(slstm_type), dimension(:), allocatable, target :: popA, popB
    real(8), dimension(:), allocatable :: gain
    integer, dimension(:), allocatable :: fathers
    real(8) :: resp_i, resp_j, CC, CD, DC
    real(8) :: Wstrength, mu_rate
    real(8) :: Clev
    real(8) :: rand8, max_val, sum_exp
    CHARACTER(len=124) :: arg

    if (iargc().ne.10) then
        CC = 1.0
        CD = -0.2
        DC = CC - CD
        Wstrength = 0.01
        N = 100
        Runs = 1000000
        mu_rate = 0.8
        Dhid_start = 5
        mix_flag = 0
        filenameout = "run.out"
    else
        CALL getarg(1, arg)
        read(arg, *) CC

        CALL getarg(2, arg)
        read(arg, *) CD

        CALL getarg(3, arg)
        read(arg, *) DC

        CALL getarg(4, arg)
        read(arg, *) Wstrength

        CALL getarg(5, arg)
        read(arg, *) N    

        CALL getarg(6, arg)
        read(arg, *) Runs    

        CALL getarg(7, arg)
        read(arg, *) mu_rate 

        CALL getarg(8, arg)
        read(arg, *) Dhid_start        

        CALL getarg(9, arg)
        read(arg, *) filenameout  

        CALL getarg(10, arg)
        read(arg, *) mix_flag        
    endif

    call init_random_seed()

    Dhid = Dhid_start
    Dout = 2
    Dinp = 4*Dhid + Dout
    depth = 5*Dhid
    maxdepth = depth

    allocate(perm(Dhid*Dout + 4*Dhid**2 + 4*Dinp*Dhid))
    allocate(inp_st1(Dhid*Dout + 4*Dhid**2 + 4*Dinp*Dhid), inp_st2(Dhid*Dout + 4*Dhid**2 + 4*Dinp*Dhid))
    allocate(popA(N), popB(N), gain(N), fathers(N), resps(N, N))

    do i=1, Dhid*Dout + 4*Dhid**2 + 4*Dinp*Dhid
        perm(i) = i
    enddo

    pop => popA
    popnext => popB
    do i=1, N
        call slstm_new_lbl(Dhid, Dinp, Dout, perm, pop(i), i)
        call slstm_new_lbl(Dhid, Dinp, Dout, perm, popnext(i), i)
    enddo

    do k=1, Runs
        Clev = 0.d0
        gain = 0.d0

        if (mix_flag.eq.0) then
            !$OMP PARALLEL PRIVATE(inp_st1, inp_st2, Xinp_i, Xinp_j), &
            !$OMP& PRIVATE(resp_i, resp_j, i, j), &
            !$OMP& SHARED(gain, pop, Dinp, maxdepth, CC, CD, DC, resps),  &
            !$OMP& REDUCTION(+:Clev)
            !$OMP DO
            do i=1, N
                Ninteract = N-1

                inp_st1 = [pop(i)%W, pop(i)%V, pop(i)%Wout]
                do m=1, size(perm)
                    inp_st2(m) = inp_st1(perm(m))
                enddo

                Xinp_i = reshape(inp_st2, (/Dinp, maxdepth/))

                resp_i = Slstm_forward_0NOB(Xinp_i, pop(i), pop(i)%dpth)
                resps(i, i) = resp_i

                do j=i+1, N
                    inp_st1 = [pop(j)%W, pop(j)%V, pop(j)%Wout]
                    do m=1, size(perm)
                        inp_st2(m) = inp_st1(perm(m))
                    enddo

                    Xinp_j = reshape(inp_st2, (/Dinp, maxdepth/))  

                    resp_i = Slstm_forward_0NOB(Xinp_j, pop(i), pop(j)%dpth)
                    resp_j = Slstm_forward_0NOB(Xinp_i, pop(j), pop(i)%dpth)

                    resps(i, j) = resp_i
                    resps(j, i) = resp_j

                    Clev = Clev + resp_i + resp_j
                    gain(i) = gain(i) + (CC*resp_i*resp_j + CD*resp_i*(1.d0-resp_j) + DC*(1.d0-resp_i)*resp_j) / Ninteract
                    !$OMP CRITICAL
                    gain(j) = gain(j) + (CC*resp_i*resp_j + DC*resp_i*(1.d0-resp_j) + CD*(1.d0-resp_i)*resp_j) / Ninteract
                    !$OMP END CRITICAL
                enddo
            enddo
            !$OMP END DO
            !$OMP END PARALLEL
        else
            !$OMP PARALLEL PRIVATE(resp_i, resp_j, i, j) SHARED(gain, pop, Dinp, maxdepth, CC, CD, DC) REDUCTION(+:Clev)
            !$OMP DO
            do i=1, N
                Ninteract = N-1
                do j=i+1, N                  
                    resp_i = pop(i)%mix_pc
                    resp_i = min(resp_i, 1.d0)
                    resp_i = max(resp_i, 0.d0)
                    resp_j = pop(j)%mix_pc
                    resp_j = min(resp_j, 1.d0)
                    resp_j = max(resp_j, 0.d0)  
                    resps(i, j) = resp_i
                    resps(j, i) = resp_j                                    

                    Clev = Clev + resp_i + resp_j
                    gain(i) = gain(i) + (CC*resp_i*resp_j + CD*resp_i*(1.d0-resp_j) + DC*(1.d0-resp_i)*resp_j) / Ninteract
                    !$OMP CRITICAL
                    gain(j) = gain(j) + (CC*resp_i*resp_j + DC*resp_i*(1.d0-resp_j) + CD*(1.d0-resp_i)*resp_j) / Ninteract
                    !$OMP END CRITICAL
                enddo
            enddo      
            !$OMP END DO
            !$OMP END PARALLEL      
        endif

        if ((k > 25000) .and. (k < 60000)) then
            write(str1, '(I15)')  k
            call arrayR_hdf5(trim(ADJUSTL(filenameout))//trim(ADJUSTL(str1))//trim(".hdf5"), trim("resps"), resps, error)       
            call arrayR_hdf5(trim(ADJUSTL(filenameout))//trim(ADJUSTL(str1))//trim(".hdf5"), trim("gain"), gain, error)                            
        endif

        max_val = MAXVAL(gain)
        sum_exp = 0.0
        DO i = 1, SIZE(gain)
            sum_exp = sum_exp + EXP(gain(i) - max_val)
        END DO

        DO i = 1, SIZE(gain)
            gain(i) = EXP(gain(i) - max_val) / sum_exp
        END DO        

        do i=2, N
            gain(i) = gain(i) + gain(i-1)
        enddo
        gain = gain / gain(N)

        !$OMP PARALLEL  PRIVATE(i) SHARED(fathers, gain, popnext, pop)
        !$OMP DO
        do i=1, N
            fathers(i) = father(gain)
        enddo
        !$OMP END DO

        !$OMP DO
        do i=1, N
            popnext(i) = pop(fathers(i))
        enddo
        !$OMP END DO

        !$OMP END PARALLEL   

        if (mix_flag.eq.0) then  
            do i=1, N
                call random_number(rand8)
                if(rand8 .le. mu_rate) then
                    call MutateSLSTMNOBmemdiflbl(popnext(i), Wstrength)
                endif             
            enddo
        else
            do i=1, N
                call random_number(rand8)
                if(rand8 .le. mu_rate) then
                    call random_stdnormal(rand8)
                    popnext(i)%mix_pc = popnext(i)%mix_pc + Wstrength*rand8
                    popnext(i)%mix_pc = min(popnext(i)%mix_pc, 1.d0)
                    popnext(i)%mix_pc = max(popnext(i)%mix_pc, 0.d0)
                endif
            enddo
        endif

        if(mod(k, 2) .EQ. 1) then
            pop => popB
            popnext => popA
        else
            pop => popA
            popnext => popB
        end if

        WRITE(*,*)  k, Clev/(Ninteract+1)/Ninteract       
    enddo

END PROGRAM evonn