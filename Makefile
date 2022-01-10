.PHONY: _
_: info


############################################################
# File Path Definition
#
SRC_DIR := ./src
SRC_FILES := ${wildcard ./src/*.f90}
DEV_ASM_FILES := ${foreach file, ${SRC_FILES}, ./target/develop/info/${notdir ${file:.f90=.s}}}
OPT_ASM_FILES := ${foreach file, ${SRC_FILES}, ./target/optimized/info/${notdir ${file:.f90=.s}}}

DEV_BUILD := ./target/develop/build
DEV_DEPS := ./target/develop/deps

OPT_BUILD := ./target/optimized/build
OPT_DEPS := ./target/optimized/deps



############################################################
# Import Settings
#
include ./config/f90c_ifort.mk
include ./config/f90c.mk
include ./src/target.mk



############################################################
# Init
#
${shell mkdir -p ./${DEV_BUILD} ./${DEV_DEPS} ./target/develop/info}
${shell mkdir -p ./${OPT_BUILD} ./${OPT_DEPS} ./target/optimized/info}



############################################################
# Recipes
#

# object codes
./${DEV_DEPS}/%.o: ${SRC_DIR}/%.f90
	${F90_DEV_COMP} $< -o $@
./${OPT_DEPS}/%.o: ${SRC_DIR}/%.f90
	${F90_OPT_COMP} $< -o $@

# assembly
./target/develop/info/%.s: ${SRC_DIR}/%.f90 ./${DEV_DEPS}/%.o
	${F90_DEV_ASM} $< -o $@
./target/optimized/info/%.s: ${SRC_DIR}/%.f90 ./${OPT_DEPS}/%.o
	${F90_OPT_ASM} $< -o $@

# link
./${DEV_BUILD}/%:
	${F90_DEV_LINK} $^ -o $@
./${OPT_BUILD}/%:
	${F90_OPT_LINK} $^ -o $@

# tests
./${DEV_BUILD}/%.test: ${SRC_DIR}/%.f90
	${F90_UNIT_TEST} $< -o $@

# scripts
./target/run.sh: ${OPT_TARGETS}
	echo "#!/bin/bash" > $@
	for target in $^; do echo ./$$target >> $@; done
	chmod 744 $@
./target/develop.sh: ${DEV_TARGETS}
	echo "#!/bin/bash" > $@
	for target in $^; do echo ./$$target >> $@; done
	chmod 744 $@
./target/test.sh: ${TESTS}
	echo "#!/bin/bash" > $@
	for target in $^; do echo ./$$target >> $@; done
	chmod 744 $@



############################################################
# Tasks
#
.PHONY: run
run: ./target/run.sh build
	@echo "+--------------------------------------------------------+"
	@echo "|  You are running codes in the OPTIMIZED mode.          |"
	@echo "|  This mode will not generate any debug info.           |"
	@echo "|  Make sure that you have already checked your codes in |"
	@echo "| debug mode with this command:                          |"
	@echo "|   $$ make dev                                           |"
	@echo "+------------------------------------------------------98+"
	bash $<

.PHONY: dev
dev: ./target/develop.sh build
	@echo "+-------------------------------------------------------+"
	@echo "|  You are running codes in the DEVELOPMENT mode.       |"
	@echo "|  Make sure that this mode will makes your codes       |"
	@echo "| extremely slow.                                       |"
	@echo "+-----------------------------------------------------98+"
	bash $<

.PHONY: build
build: ${OPT_ASM_FILES} ${DEV_ASM_FILES} ${OPT_TARGETS} ${DEV_TARGETS}
	@echo \# successfully built.

.PHONY: test
test: ./target/test.sh
	bash $<

.PHONY: clean
clean:
	rm -r ./target

.PHONY: info
info:
	@echo "+-------------------------------------------------------+"
	@echo "| usage                                                 |"
	@echo "|  - $$ make: show this page                             |"
	@echo "|  - $$ make build: compile only                         |"
	@echo "|  - $$ make dev: build and run with debug info          |"
	@echo "|  - $$ make run: build and run in optimized mode        |"
	@echo "|  - $$ make clean: clear compiled codes and caches      |"
	@echo "+-----------------------------------------------------98+"
