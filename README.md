**File: /home/sasha/try/evonn/README.md**
```markdown
This project compiles a Fortran program using `gfortran`, with support for OpenMP and HDF5. It includes the following source files:

* `evonn.f90`
* `mod_evonn.f90`
* `mod_rw_hdf5.f90`

## Requirements

Ensure the following are installed on your system:

* [CMake](https://cmake.org/) (version ≥ 3.10)
* `gfortran` (GNU Fortran compiler)
* OpenMP (usually included with `gfortran`)
* HDF5 (with Fortran support enabled)

## Compilation and Execution

Follow these steps to build and run the program:

### 1. Create a Build Directory

Open a terminal in the project root directory and run:

```bash
mkdir build
cd build
```

### 2. Configure with CMake

If HDF5 is correctly installed and discoverable, run:

```bash
cmake ..
```

If CMake can't find HDF5 automatically, specify its location:

```bash
cmake -DHDF5_ROOT=/path/to/hdf5 ..
```

Replace `/path/to/hdf5` with the actual path to your HDF5 installation (it must support Fortran).

### 3. Compile the Program

Once configuration is complete, build the executable with:

```bash
make
```

If successful, this will create an executable named:

```bash
evonn
```

### 4. Run the Program

You can now execute the program from the build directory:

```bash
./evonn
```

## Notes

* OpenMP enables parallel computation; ensure your compiler and system support it.
* HDF5 must be compiled with Fortran support.
* If you modify source files, simply re-run `make` to rebuild.

## Troubleshooting

* **HDF5 not found?** Use `cmake -DHDF5_ROOT=...` and ensure the Fortran module files (`.mod`) and libraries are included.
* **Compiler errors?** Verify that your HDF5 and Fortran versions are compatible (e.g., both built with the same compiler).

## Run

The simulation can be executed using `./build/evonn` with default parameters:

- CC = 1.0
- CD = -0.2
- DC = CC - CD
- Wstrength = 0.01
- N = 100
- Runs = 1000000
- mu_rate = 0.8
- Dhid_start = 5
- mix_flag = 0
- filenameout = "run.out"

./build/evonn 1.0 -0.2 1.2 0.01 100 1000000 0.8 5 outhdf5 0 > outtxt

### Using SLURM Batch

To submit a job using SLURM, use the following command:

```bash
sbatch  --job-name="RunName" --output="RunName.out" ../irun_evonn.sh  ../build/evonn outtxt 1.0 -0.2 1.2 0.01 100  1000000 0.8  5 outhdf5 0
```

Modify the SLURM script according to your local partition name:

```bash
#!/bin/sh
#
#SBATCH --partition=MY_PARTITION
#SBATCH --nodes=1
#SBATCH --exclusive

$1 $3 $4 $5 $6 $7 $8 $9 ${10} ${11} ${12} ${13} ${14} > $2
```

To run:

```bash
sbatch --job-name="{jobname}" --output="{jobname}.out" ../irun_evonn.sh ../build/evonn {outname} {CC} {CD} {DC} {Wstrength} {N} {Runs} {mu_rate} {Dhid_start} {outnamesave}_{DC}_{M}_{mu_rate} {mix}
```

Alternatively, use the `run_survey.py` script from the running directory.
```