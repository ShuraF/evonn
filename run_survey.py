import numpy as np
import matplotlib.pyplot as plt
import pickle
import time
import sys
import h5py
import os
import multiprocessing

# Define constants for the simulation
Wstrength = 0.01
N = 100
Runs = 1000000
Dhid_start = 5
outnamesave = 'RunName1'
mix = 0

# Loop over different parameter values to create and submit SLURM jobs
for Aval in [1.0]:
    CC = 1.0 * Aval
    for CD in np.linspace(-0.20, -0.80, 2) * Aval:
        DC = CC - CD
        for mu_rate in [0.8]:
            for M in range(2):
                # Construct job and output names based on current parameters
                jobname = f"RunName_{DC}_{M}_{mu_rate}_{Aval}"
                outname = f'RunName_{DC}_{M}_{mu_rate}_{Aval}'
                
                # Construct the SLURM command to submit the job
                com = f'sbatch  --job-name="{jobname}" --output="{jobname}.out" ../irun_slstm_evol.sh  ../build/slstm_article {outname} {CC} {CD} {DC} {Wstrength} \
                    {N}  {Runs} {mu_rate}  {Dhid_start} {outnamesave}_{DC}_{M}_{mu_rate} {mix}'
                
                # Print and execute the command
                print('com = ', com)
                os.popen(com)
# # ######################################################
