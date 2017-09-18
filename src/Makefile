# Makefile for Fortran program
# Makefile pour n'importe quel px
# ( make.macro is a link that points to the file macro.xxx where 
#   xxx is representative of your machine )
## CREGNIER AOUT 2017
include ../macro/make_intel_px.macro
#-------------------------------
EXEDIR=../bin/
SRCDIR=.
#--------------------------------

.SUFFIXES:.F90 .F .mod .f90

.f90.o:
	$(F90) $(INCLUDE) $(FFLAGS) -c $< -o $@

.F90.o:
	$(F90) $(INCLUDE) $(FFLAGS) -c $< -o $@ 

EXECS= read_write_lightout.exe 
OBJ = 
all : $(EXECS)

read_write_lightout.exe: $(OBJ) read_write_lightout.o
	$(F90) -o $@  $(LD_FLAGS) $? $(LIBRARY) 
	mv $@  $(EXEDIR)
## Utilities
cleanexe : 
	rm *.exe
clean :
	rm -rf ${SRCDIR}/*.o
	rm -rf ${SRCDIR}/*.mod
