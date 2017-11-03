# ------------------------------------------------
# CITY Makefile
# ------------------------------------------------
# Legend:
#  D= directory. absolute path for the project file
# 	P= project_name or test_name (without extension) //e.g. test
#	F= filename  (without extension)//e.g. b0
#  dS= service_name (for docker)//e.g. backend
#  dM= mode (for docker)//e.g. d for dev mode

# compiler
COMPILER  = gnatmake
BUILDER = $(CITY_ROOT)/sbin/be-build.sh

# dirs
SRCDIR = "src"
OBJDIR = "obj"
BINDIR = "bin"

# flags
FLAGS   			= -x -P
P_EXT				= .gpr
F_EXT  	   	= .json
PROJ_FILE 		:= $(D)$(P)$(P_EXT)
BATCH_FILE 		:= $(F)$(F_EXT)
TEMPLATE 		= "test.template"

# commands
find 	   		= find $(D) -type f -name
remove_bin 		= find $(BINDIR) -type f -not -name "\.*" -delete
generate_test 	= python generate_test.py


ifndef CITY_COMPILE
# local ENV variables
CITY_CONFIG	= $(CITY_ROOT)/config
# export the correct NODE_ID in the local scope
ARRAY_IDS=$(shell python $(CITY_ROOT)/lib/dockerAPI.py backend $(shell ip addr show | grep -A1 "link/ether" | grep inet | cut -dt -f2 | cut -d/ -f1))
C_NODE_ID=$(shell echo "${ARRAY_IDS}" | cut -d_ -f1)
C_DISTRICT_ID=$(shell echo "${ARRAY_IDS}" | cut -d_ -f2)
# append in bashrc for dev mode
$(shell echo "export CITY_NODE_ID="$(C_NODE_ID) >> "$$HOME"/.bashrc)
$(shell echo "export CITY_DISTRICT_ID="$(C_DISTRICT_ID) >> "$$HOME"/.bashrc)
# export in this local env
export CITY_NODE_ID=$(C_NODE_ID)
export CITY_DISTRICT_ID=$(C_DISTRICT_ID)
endif


all: run

build: $(BUILDER)
	$(BUILDER) $(SRCDIR) $(OBJDIR) $(BINDIR) $(P)
# $(DBG)

run: build
	@$(find) $(P) | sh

.PHONY: test
test:
	$(generate_test) $(TEMPLATE) $(shell $(find) $(BATCH_FILE))
	$(COMPILER) $(FLAGS) $(PROJ_FILE)

.PHONY: run_test
run_test:
	$(find) $(P) | sh

.PHONY: run_all_tests
run_all_tests:
	@for t in `ls bin/test`; do \
		./"bin/test/"$$t; done \

docker: $(CITY_CONFIG)/$(dS)
	$(CITY_CONFIG)/build.sh -s $(dS) -m $(dM)
	$(CITY_CONFIG)/run.sh -s $(dS)

.PHONY: help
help:
ifeq ($(H),test)
	@echo "Command : test\n"\
	"\tCompiles a specific test suite. Each set of test suites is specified "\
	"in the configuration files in test/batches. Each .json file contains a "\
	"a list of suites.\n\t For example, assuming that simple.json contains a "\
	"list of suites, we can compile it in the following way:\n" \
	"\n\t make test D=. P=test F=simple \n\n" \
	"\tD consider all the files from the current directory\n" \
	"\tP use the test.gpr project file\n" \
	"\tF specify the file which contains the list of test suites to compile\n"
else
	@echo "Usage: make [commands] [options]\n"\
	"Options:\n"\
	"\t D=<directory>\t\t\t  Absolute path for the project file\n"\
	"\t P=<name>\t\t\t\t  Project or test name (without extension, e.g. P=test).\n"\
	"\t F=<filename>\t\t\t  Filename which refers the corresponding .json file of tests to be run (without extension, e.g. b0).\n"\
	"\t\t  		 			  use with commands  : { test }\n"\
	"\t dS=<docker_service>\t  Specify the docker service to build (image) and run (container).\n"\
	"\t\t\t\t\t 		  use with commands : { docker }\n"\
	"\t dM=<docker_mode>\t\t  Specify the mode for docker service { dev, test, prod }\n"\
	"\t\t\t\t\t 		  use with commands : { docker }\n"\
	"\t H=<command>\t\t\t  Provides usage info about a command (e.g. make help H=test)\n"\
	"\t\t\t\t\t 		  use with commands : { help }\n"\
	"\n"\
	"Commands:\n" \
	"\t build\t\t\t builds the city-simulator project (specify the entrypoint of the project)\n"\
	"\t run\t\t\t runs the city-simulator -> depends on { build }\n"\
	"\t test\t\t\t compiles the specified tests\n"\
	"\t run_test\t\t runs the specified tests\n"\
	"\t run_all_tests\t runs all the tests in the ${BINDIR} directory\n"\
	"\t docker\t\t\t manage the docker service (i.e. virtual nodes of the project)\n"\
	"\t clean\t\t\t remove all the objects, binary and temporary generated files\n"\
	"\t remove\t\t\t remove a specified file\n"\
	"\t help\t\t\t provides info about a specified command\n"
endif
.PHONY: clean
clean:
	@$(find) "*.o" -delete
	@$(find) "*.ali" -delete
	@$(find) "*b~*" -delete
	@$(find) "*.TMP" -delete
	@$(find) "*.cgpr" -delete
	@$(find) "*.bexch" -delete
	@$(remove_bin)
	@$(remove_bin_test)

.PHONY: remove
remove:
	@$(find) $(P) -delete
