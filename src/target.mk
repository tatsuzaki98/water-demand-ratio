############################################################
# Executable Targets
# List up source files which contains `program` section.
#

OPT_TARGETS = ${OPT_BUILD}/main # ${OPT_BUILD}/prog_river_rank
DEV_TARGETS = ${DEV_BUILD}/main # ${DEV_BUILD}/prog_river_rank


############################################################
# Tests
#


############################################################
# Dependencies
#

# Optimized
${OPT_DEPS}/utils.o: ${OPT_DEPS}/params.o
${OPT_DEPS}/main.o: ${OPT_DEPS}/params.o ${OPT_DEPS}/utils.o
${OPT_BUILD}/main: ${OPT_DEPS}/main.o ${OPT_DEPS}/params.o ${OPT_DEPS}/utils.o
# ${OPT_BUILD}/prog_river_rank: ${OPT_DEPS}/prog_river_rank.o

${DEV_DEPS}/utils.o: ${DEV_DEPS}/params.o
${DEV_DEPS}/main.o: ${DEV_DEPS}/params.o ${DEV_DEPS}/utils.o
${DEV_BUILD}/main: ${DEV_DEPS}/main.o ${DEV_DEPS}/params.o ${DEV_DEPS}/utils.o
# ${DEV_BUILD}/prog_river_rank: ${DEV_DEPS}/prog_river_rank.o
