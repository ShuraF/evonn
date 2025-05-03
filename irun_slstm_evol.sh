#!/bin/sh
#

## SBATCH --job-name=
## SBATCH --output="parjob"{$1}".out"
## SBATCH --error=parjob.err
##SBATCH --partition=socket
#SBATCH --partition=socket
#SBATCH --nodes=1
#SBATCH --exclusive

## com = f'sbatch  --job-name="{jobname}" --output="{jobname}.out" muljobWoAoldCmdArg {RunName} {Nsteps} {M} {Wstrength} {Dstrength}'
#sbatch --job-name="AAA" --output="AAA.out" irun_sas.sh ./a_omp.out a_omp1.dat
##python3 $1."py" $2 $3 $4 $5
$1 $3 $4 $5 $6 $7 $8 $9 ${10} ${11} ${12} ${13} ${14} ${15} ${16} ${17} ${18} ${19} ${20} ${21} ${22} ${23} ${24} ${25} ${26} ${27} ${28} > $2
