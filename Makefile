FORTRAN=gfortran

SRC_DIR=src
BUILD_DIR=build

F90_FILES=$(wildcard $(SRC_DIR)/*.f90)
OBJ_FILES=$(F90_FILES:$(SRC_DIR)/%.f90=$(BUILD_DIR)/%_f90.o)
TARGET=advent_of_code

all: $(TARGET)

$(TARGET): $(OBJ_FILES)
	$(FORTRAN) $(OBJ_FILES) -o $(TARGET)

$(BUILD_DIR)/%_f90.o: $(SRC_DIR)/%.f90
	mkdir -p $(@D)
	$(FORTRAN) -c $< -o $@

clean:
	rm -rf $(TARGET) $(BUILD_DIR)
