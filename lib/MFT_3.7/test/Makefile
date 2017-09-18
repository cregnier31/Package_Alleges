# Makefile for Fortran program Test_MCAL.FC
# Makefile pour n'importe quel px
# ( make.macro is a link that points to the file macro.xxx where 
#   xxx is representative of your machine )
# C.REGNIER Septembre 2012  : Tests for MFT installation
include ../src/make_intel.macro
#include ../make_intel_medium.macro

#--------------------------------
EXEDIR= ./bin/
INC=$(INSTALL_DIR)/include
FFLAGS_TEST=-O3 -convert big_endian -I${INC} -I${NETCDF_INC} 
LD_FLAGS=-O3 -convert big_endian
LIBRARY=-L$(INSTALL_DIR)/lib -lmft -L$(NETCDF_LIB) -lnetcdff -lnetcdf  -L${HDF5_LIB} #-lhdf5_hl -lhdf5

.SUFFIXES:.F90 .F .mod .f90

.f90.o:
	$(FC) $(FFLAGS_TEST) -c $< -o $@ 
.F90.o:
	$(FC) $(FFLAGS_TEST) -c $< -o $@ 

EXECS=  Test.exe Test_attributs.exe Test_CHAR_MFT.exe Test_compute_short.exe
OBJ = 

all : $(EXECS)

Test.exe: Test_Read_Write_R4_R8_INT_Sh_By_MFT.o
	$(FC) -o $@  $(LD_FLAGS) $? $(LIBRARY)
	mv $@  $(EXEDIR)
Test_attributs.exe: Test_attributs.o
	$(FC) -o $@  $(LD_FLAGS) $? $(LIBRARY) 
	mv $@  $(EXEDIR)
Test_CHAR_MFT.exe:Test_CHAR_MFT.o
	$(FC) -o $@  $(LD_FLAGS) $? $(LIBRARY) 
	mv $@  $(EXEDIR)
Test_compute_short.exe: Test_compute_short.o
	$(FC) -o $@  $(LD_FLAGS) $? $(LIBRARY) 
	mv $@  $(EXEDIR)
Test_MFT_analytic.exe: Test_analytique_ORCA_MFT.o
	$(FC) -o $@  $(LD_FLAGS) $? $(LIBRARY) 
	mv $@  $(EXEDIR)
## Utilities
cleanexe : 
	rm *.exe
clean :
	rm -rf ${SRCDIR}/*.o
	rm -rf ${SRCDIR}/*.mod
	rm -rf ${ROOT}/*.o
	rm -rf ${ROOT}/*.mod
	rm -rf ${EXEDIR}/*.exe
