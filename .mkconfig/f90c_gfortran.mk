# compile command
F90C = gfortran

# compile only
FFLAG_C = -c

# fortran preprocessor
FFLAG_FPP = -cpp

# set optimize level 3
FFLAG_OPT3 = -O3

# full link time optimization
FFLAG_FLTO = -flto

# generate asembly
FFLAG_ASM = -S

# warnings
FFLAG_WARN = -g -Wall

# trace back
FFLAG_TB = -fbacktrace

# strict checking
FFLAG_CHECK = -fbounds-check -O -Wuninitialized "-ffpe-trap=invalid,zero,overflow"

# list compiler info
FFLAG_LIST =

# intrinsic module path
FFLAG_OPT_IMP = -J${OPT_DEPS} -fintrinsic-modules-path ${OPT_DEPS}
FFLAG_DEV_IMP = -J${DEV_DEPS} -fintrinsic-modules-path ${DEV_DEPS}

# open Multi Processing
FFLAG_OMP = -fopenmp

# unit test
FFLAG_TEST = -D_UNIT_TEST
