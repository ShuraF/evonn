import numpy as np
import matplotlib.pyplot as plt
import pickle
import time
import sys
import h5py
import os
import multiprocessing

# Removed unused variables
Wstrength = 0.01
N = 100
Runs = 1000000
Dhid_start = 5
outnamesave = 'RunName1'
mix = 0

# for mu_rate in [0.01,0.09]: #[np.linspace(0.01,0.09,9):
if os.path.isdir(f'/home/sasha/try/slstm_article/'+outnamesave):
    os.chdir(f'/home/sasha/try/slstm_article/'+outnamesave)
else:
    os.mkdir(f'/home/sasha/try/slstm_article/'+outnamesave)
    os.chdir(f'/home/sasha/try/slstm_article/'+outnamesave)
for Aval in [1.0]:
    CC = 1.0*Aval
    for CD in np.linspace(-0.20, -0.80, 2)*Aval:
        DC = CC - CD
        # for N in [10, 100]:  # [np.linspace(0.01,0.09,9):
        for mu_rate in [0.8]:  # [np.linspace(0.01,0.09,9):
            for M in range(2):
                jobname = f"RunName_{DC}_{M}_{mu_rate}_{Aval}"
                outname = f'RunName_{DC}_{M}_{mu_rate}_{Aval}'
                com = f'sbatch  --job-name="{jobname}" --output="{jobname}.out" ../irun_slstm_evol.sh  ../build/slstm_article {outname} {CC} {CD} {DC} {Wstrength} \
                    {N}  {Runs} {mu_rate}  {Dhid_start} {outnamesave}_{DC}_{M}_{mu_rate} {mix}'
                print('com = ', com)
                os.popen(com)
# # ######################################################
