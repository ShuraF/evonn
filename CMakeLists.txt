cmake_minimum_required(VERSION 3.10)
project(slstm_evol_mix_noise Fortran)

# Use gfortran as the compiler
set(CMAKE_Fortran_COMPILER gfortran)

# Enable OpenMP
find_package(OpenMP REQUIRED)

# Find HDF5 Fortran components
find_package(HDF5 REQUIRED COMPONENTS Fortran)

# Set compiler flags
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 -fopenmp ")

# Collect all Fortran source files in src/
file(GLOB SRC_FILES "${CMAKE_SOURCE_DIR}/src/*.f90")

# Define the executable from source files
add_executable(slstm_article ${SRC_FILES})

# Link OpenMP and HDF5
target_link_libraries(slstm_article
    PRIVATE
    OpenMP::OpenMP_Fortran
    ${HDF5_Fortran_LIBRARIES}
)

# Include HDF5 Fortran modules
target_include_directories(slstm_article
    PRIVATE
    ${HDF5_Fortran_INCLUDE_DIRS}
)
