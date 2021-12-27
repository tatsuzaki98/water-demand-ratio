######################################################################
# Settings for optimized Compile
#
# with these options, compiler generates optimized code.
#

# optimized compile
F90_OPT_COMP = ${F90C} ${FFLAG_C} ${FFLAG_FPP} ${FFLAG_OPT_IMP} ${FFLAG_OPT3} ${FFLAG_OMP} ${FFLAG_RECL}

# optimized link
F90_OPT_LINK = ${F90C} ${FFLAG_FLTO} ${FFLAG_OMP} ${FFLAG_RECL}

# generate assembly code
F90_OPT_ASM = ${F90C} ${FFLAG_FPP} ${FFLAG_OPT_IMP} ${FFLAG_OPT3} ${FFLAG_OMP} ${FFLAG_ASM}



######################################################################
# Settings for Development Compile
#
# with these options, compiler generates object codes which includes
# full debug info.
#

# Fortran90 development compile
F90_DEV_COMP = ${F90C} ${FFLAG_C} ${FFLAG_FPP} ${FFLAG_LIST}\
  ${FFLAG_DEV_IMP} ${FFLAG_CHECK} ${FFLAG_WARN} ${FFLAG_OMP} ${FFLAG_RECL}

# Fortran 90 development link
F90_DEV_LINK = ${F90C} ${FFLAG_OMP} ${FFLAG_RECL}

# generate assembly code
F90_DEV_ASM = ${F90C} ${FFLAG_FPP} ${FFLAG_DEV_IMP} ${FFLAG_ASM}

# Fortran90 unit test
F90_UNIT_TEST = ${F90C} ${FFLAG_FPP} ${FFLAG_DEV_IMP} ${FFLAG_CHECK} ${FFLAG_WARN}\
  ${FFLAG_OMP} ${FFLAG_TEST} ${FFLAG_RECL}