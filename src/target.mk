############################################################
# Executable Targets
# List up source files which contains `program` section.
#

OPT_TARGETS = ${OPT_BUILD}/main
DEV_TARGETS = ${DEV_BUILD}/main


############################################################
# Tests
#

# TESTS = ${DEV_BUILD}/params.test ${DEV_BUILD}/utils.test


############################################################
# Dependencies
#

# Optimized
${OPT_DEPS}/utils.o: ${OPT_DEPS}/params.o
${OPT_DEPS}/main.o: ${OPT_DEPS}/params.o ${OPT_DEPS}/utils.o
${OPT_BUILD}/main: ${OPT_DEPS}/main.o ${OPT_DEPS}/params.o ${OPT_DEPS}/utils.o

${DEV_DEPS}/utils.o: ${DEV_DEPS}/params.o
${DEV_DEPS}/main.o: ${DEV_DEPS}/params.o ${DEV_DEPS}/utils.o
${DEV_BUILD}/main: ${DEV_DEPS}/main.o ${DEV_DEPS}/params.o ${DEV_DEPS}/utils.o
