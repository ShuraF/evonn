
This project compiles a Fortran program using `gfortran`, with support for OpenMP and HDF5. It includes the following source files:

* `slstm_evol_mix_noise.f90`
* `mod_slstm_omp_evol.f90`
* `mod_rw_hdf5.f90`

## Requirements

Make sure the following are installed on your system:

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

If HDF5 is correctly installed and discoverable:

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
c_slstmp_evol_mix_noise
```

### 4. Run the Program

You can now execute the program from the build directory:

```bash
./c_slstmp_evol_mix_noise
```

## Notes

* OpenMP enables parallel computation; ensure your compiler and system support it.
* HDF5 must be compiled with Fortran support.
* If you modify source files, just re-run `make` to rebuild.

## Troubleshooting

* **HDF5 not found?** Use `cmake -DHDF5_ROOT=...` and make sure the Fortran module files (`.mod`) and libraries are included.
* **Compiler errors?** Check that your HDF5 and Fortran versions are compatible (e.g., both built with the same compiler).




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