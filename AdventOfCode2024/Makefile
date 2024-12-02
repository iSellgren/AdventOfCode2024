# Define the Fortran compiler (using ifx)
FC = ifx

# Define the flags for the Fortran compiler for different build types
OPTFLAGS = -O3 -march=native -funroll-loops -flto  # Optimization flags for release
DEBUGFLAGS = -g -check all -fpe0 -traceback -fstack-protector  # Debugging and safety flags
LINTFLAGS = -Wunused -Wuninitialized  # Flags to catch unused and uninitialized variables
WARNFLAGS = -warn all  # General warning flags

# Define build types
RELEASE_FLAGS = $(OPTFLAGS) $(WARNFLAGS) $(LINTFLAGS) -I$(BUILD_DIR) -J$(BUILD_DIR) -std=f2008
DEBUG_FLAGS = $(DEBUGFLAGS) $(WARNFLAGS) $(LINTFLAGS) -I$(BUILD_DIR) -J$(BUILD_DIR) -std=f2008

# Define the output executable name and build directory
OUTPUT = program.out
BUILD_DIR = build

# Module source files
MODULE_SOURCES = array_Utils.f90 file_Utils.f90 Day1_Part1.f90 Day1_Part2.f90 Day2_Part1.f90 Day2_Part2.f90

# All test and main source files (files that depend on modules)
SOURCES = Day1_tests.f90 Day2_tests.f90 main.f90

# Create object files for each source file in build directory
MODULE_OBJS = $(patsubst %.f90,$(BUILD_DIR)/%.o,$(MODULE_SOURCES))
OBJECTS = $(patsubst %.f90,$(BUILD_DIR)/%.o,$(SOURCES))
ALL_OBJECTS = $(MODULE_OBJS) $(OBJECTS)

# Default target to build the release executable
all: release

# Release build
release: FCFLAGS = $(RELEASE_FLAGS)
release: $(BUILD_DIR)/$(OUTPUT)

# Debug build
debug: FCFLAGS = $(DEBUG_FLAGS)
debug: $(BUILD_DIR)/$(OUTPUT)

# Create the build directory if it doesn't exist
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Rule to build the final executable (compile modules first, then main and tests)
$(BUILD_DIR)/$(OUTPUT): $(ALL_OBJECTS) | $(BUILD_DIR)
	$(FC) $(FCFLAGS) -o $@ $(ALL_OBJECTS)
	# Create a symbolic link in the root directory for easy execution
	ln -sf $(BUILD_DIR)/$(OUTPUT) program.out

# Pattern rule to compile source files into object files in build directory
$(BUILD_DIR)/%.o: %.f90 | $(BUILD_DIR)
	$(FC) $(FCFLAGS) -c $< -o $@
	# Move .mod files to build directory if generated outside
	mv -f *.mod $(BUILD_DIR) 2>/dev/null || true

# Run target to build and then run the program
.PHONY: run
run: release
	./program.out

# Clean up generated files
.PHONY: clean
clean:
	rm -f $(BUILD_DIR)/*.o $(BUILD_DIR)/*.mod $(BUILD_DIR)/$(OUTPUT) program.out
