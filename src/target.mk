############################################################
# Executable Targets
#
# List up source files which contains `program` section.
#
OPT_TARGETS = ${OPT_BUILD}/main ${OPT_BUILD}/flip_ud
DEV_TARGETS = ${DEV_BUILD}/main ${DEV_BUILD}/flip_ud


############################################################
# Tests
#
TESTS = ${DEV_BUILD}/parameters.test ${DEV_BUILD}/utils.test


############################################################
# Dependencies
#

# Optimized
${OPT_DEPS}/utils.o: ${OPT_DEPS}/parameters.o
${OPT_DEPS}/main.o: ${OPT_DEPS}/parameters.o ${OPT_DEPS}/utils.o
${OPT_DEPS}/flip_ud.o: ${OPT_DEPS}/parameters.o ${OPT_DEPS}/utils.o
${OPT_BUILD}/main: ${OPT_DEPS}/main.o ${OPT_DEPS}/parameters.o ${OPT_DEPS}/utils.o
${OPT_BUILD}/flip_ud: ${OPT_DEPS}/flip_ud.o ${OPT_DEPS}/parameters.o ${OPT_DEPS}/utils.o


# Development
${DEV_DEPS}/utils.o: ${DEV_DEPS}/parameters.o
${DEV_DEPS}/main.o: ${DEV_DEPS}/parameters.o ${DEV_DEPS}/utils.o
${DEV_DEPS}/flip_ud.o: ${DEV_DEPS}/parameters.o ${DEV_DEPS}/utils.o
${DEV_BUILD}/main: ${DEV_DEPS}/main.o ${DEV_DEPS}/parameters.o ${DEV_DEPS}/utils.o
${DEV_BUILD}/flip_ud: ${DEV_DEPS}/flip_ud.o ${DEV_DEPS}/parameters.o ${DEV_DEPS}/utils.o
