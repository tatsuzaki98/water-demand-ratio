##### Build Settings
# FC := nvfortran
# FFLAGS := -mp -O3

FC      := ifort
FFLAGS := -debug all -convert little_endian -assume nobyterecl -O3 -xHOST -list

# -g -check bounds
# FFLAGS  := -debug all -g -traceback -check -mcmodel:=large -shared-intel -convert little_endian -assume nobyterecl -O0 -xHOST -qopenmp
