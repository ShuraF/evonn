# slstm_article

to compile:

h5fc   -O3 slstm_evol_mix_noise.f90 mod_slstm_omp_evol.f90 mod_rw_hdf5.f90  -fopenmp -o c_slstmp_evol_mix_noise.out

to run:

survPD0325.py

change sbatch script accordingly:

#!/bin/sh
#
#SBATCH --partition=MY_PARTITION
#SBATCH --nodes=1
#SBATCH --exclusive

$1 $3 $4 $5 $6 $7 $8 $9 ${10} ${11} ${12} ${13} ${14} ${15} ${16} ${17} ${18} ${19} ${20} ${21} ${22} ${23} ${24} ${25} ${26} ${27} ${28} > $2