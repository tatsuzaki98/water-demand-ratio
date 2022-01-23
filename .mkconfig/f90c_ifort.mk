# compile command
F90C = ifort

# compile only
FFLAG_C = -c

# fortran preprocessor
FFLAG_FPP = -fpp

# optimize level
FFLAG_OPT3 = -O3

# full link time optimization
FFLAG_FLTO = -flto

# generate asembly
FFLAG_ASM = -S

# warnings
FFLAG_WARN = -g -warn all

# traceback
FFLAG_TB = -traceback

# strict checking
FFLAG_CHECK = -check all

# list compiler info
FFLAG_LIST = -list./info

# intrinsic module path
FFLAG_DEV_IMP = -I${DEV_DEPS} -module ${DEV_DEPS}
FFLAG_OPT_IMP = -I${OPT_DEPS} -module ${OPT_DEPS}

# open Multi Processing
FFLAG_OMP = -qopenmp

# unit test
FFLAG_TEST = -D_UNIT_TEST

# record length
FFLAG_RECL = -assume byterecl
