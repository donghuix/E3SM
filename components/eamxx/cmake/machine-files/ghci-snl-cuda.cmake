# Common settings for our ghci images
include(${CMAKE_CURRENT_LIST_DIR}/ghci-snl.cmake)

# Set SCREAM_MACHINE
set(SCREAM_MACHINE ghci-snl-cuda CACHE STRING "")

# Enable CUDA in kokkos
set (EKAT_MACH_FILES_PATH ${CMAKE_CURRENT_LIST_DIR}/../../../../externals/ekat/cmake/machine-files)
include (${EKAT_MACH_FILES_PATH}/kokkos/cuda.cmake)

set(EKAT_MPI_NP_FLAG "-n" CACHE STRING "The mpirun flag for designating the total number of ranks")

# TODO: rebuild cuda image with cuda-aware MPI, so we can set this to ON
option(SCREAM_MPI_ON_DEVICE "Whether to use device pointers for MPI calls" OFF)

# Currently, we have 2 GPUs/node on Blake, and we run a SINGLE build per node, so we can fit 2 ranks there
set(SCREAM_TEST_MAX_RANKS 2 CACHE STRING "Upper limit on ranks for mpi tests")
