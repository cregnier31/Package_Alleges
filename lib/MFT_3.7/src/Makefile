#############################
## Make Octobre 2009  
# C.REGNIER en collaboartion avec M. Chekki
# C.REGNIER    Mai 2010  : Version 3.3
# C.REGNIER    Septembre 2017  : MAJ

# Compilation intel 
#module load intel/intel-15
#module load netcdf/netcdf-4.3_intel15_hdf5
include ../../../macro/make_intel_px.macro

# rules
.SUFFIXES:.F90 .F .mod

.F90.o:
	$(F90) $(INCLUDE_NC) $(FFLAGS) -c $< -o $@ $(CPP_KEYS)

all: create_namelist lib install 

create_namelist:
	@echo ""
	@echo "-------- CREATE NAMELIST AND MIOL_PARAM --------"
	@echo "&miolParameterPaths" > ${TMP_NAMELIST}
	@echo "cl_equivalencesPath='${PARAM_DIR_INSTALL}/equivalences/'" >> ${TMP_NAMELIST}
	@echo "cl_globalAttributesPath='${PARAM_DIR_INSTALL}/attributes/global/'">> ${TMP_NAMELIST}
	@echo "cl_variableAttributesPath='${PARAM_DIR_INSTALL}/attributes/variable/'" >> ${TMP_NAMELIST}
	@echo "cl_dimensionsPath='${PARAM_DIR_INSTALL}/dimensions/'" >> ${TMP_NAMELIST}
	@echo "/" >> ${TMP_NAMELIST}
	@echo " NAMELIST DONE ... MODIFY MIOL_PARAM---------"
	@perl -pi -e "s|${param1}.*=.*|${param1}='${PARAM_DIR_INSTALL}'|g" ${FICHIER}
	@perl -pi -e "s|${param2}.*=.*|${param2}='${NAMELIST}'|g" ${FICHIER}
	@perl -pi -e "s|open\(10,file=TRIM(.*)|open\(10,file=TRIM('${INSTALL_DIR}')//TRIM('/param/statics/gsw_data_v3_0.dat'),status='old',err=1)|g" ${FICHIER2}
	@echo "-------- NAMELIST AND MIOL_PARAM DONE --------"
	@echo ""

OBJECTS = MFT_error.o \
	MIOL_param.o \
	MIOL_util.o \
	MFT_Inf_NaN_detection.o \
	INT_ATTRIBUTS.o \
	INT_ATTRIBUTSTYPE.o \
	INT_read_write_R8.o \
	INT_read_write_R4.o \
	INT_read_write_S.o \
	INT_read_write_INTEGER.o \
	INT_read_write_INTEGER8.o \
	INT_read_write_C.o \
	INT_read_write_B.o \
	INT_read_write_ATTLIST.o \
	INT_INQUIRE.o \
	src_ATTRIBUTS.o \
	src_Read_R4.o \
	src_Read_R8.o \
	src_Read_Short.o \
	src_Read_Integer.o \
	src_Read_Integer8.o \
	src_Read_C.o \
	src_Read_B.o \
	src_Read_ATTLIST.o \
	src_Read_ATTRIBUTS.o \
	src_Write_R4.o \
	src_Write_R8.o \
	src_Write_Short.o \
	src_Write_Char.o \
	src_Write_B.o \
	src_Write_Integer.o \
	src_Write_Integer8.o \
	src_Write_ATTLIST.o \
	src_Write_ATTRIBUTS.o \
	MIOL_OPEN_CLOSE.o \
	MIOL.o \
	MCAL.o \
	su_date_var.o \
	su_ajust_typ.o \
	su_glob_typ.o \
	su_gmes_typ.o \
	MCAL_SU_DATE.o \
	MCAL_SU_AJUST.o \
	MCAL_trait_sig_stats.o \
	MCAL_date_and_time.o \
	MCAL_gsw_oceanographic_toolbox.o \
	MCAL_SHFO.o \
	MCAL_read_data.o 

lib: $(OBJECTS)
	@echo ""
	@echo "-------- CREATE MFT LIB --------"
	$(AR) $(LIB_DIR)/$(LIB_MFT) \
	MFT_error.o \
	MIOL_util.o \
	INT_read_write_R8.o \
	INT_read_write_R4.o \
	INT_read_write_S.o \
	INT_read_write_INTEGER.o \
	INT_read_write_INTEGER8.o \
	INT_read_write_C.o \
	INT_read_write_B.o \
	INT_read_write_ATTLIST.o \
	INT_INQUIRE.o \
	INT_ATTRIBUTS.o \
	INT_ATTRIBUTSTYPE.o \
	src_Read_R4.o \
	src_Read_R8.o \
	src_Read_Short.o \
	src_Read_Integer.o \
	src_Read_Integer8.o \
	src_Read_C.o \
	src_Read_B.o \
	src_Read_ATTLIST.o \
	src_Read_ATTRIBUTS.o \
	src_Write_R4.o \
	src_Write_R8.o \
	src_Write_Short.o \
	src_Write_Char.o \
	src_Write_B.o \
	src_Write_Integer.o \
	src_Write_Integer8.o \
	src_Write_ATTLIST.o \
	src_Write_ATTRIBUTS.o \
	src_ATTRIBUTS.o \
	MIOL_OPEN_CLOSE.o \
	MIOL_param.o \
	MFT_Inf_NaN_detection.o \
	MIOL.o \
	MCAL.o \
	su_date_var.o \
	MCAL_SU_DATE.o \
	su_ajust_typ.o \
	su_glob_typ.o \
	su_gmes_typ.o \
	MCAL_SU_AJUST.o \
	MCAL_date_and_time.o \
	MCAL_trait_sig_stats.o \
	MCAL_gsw_oceanographic_toolbox.o \
	MCAL_SHFO.o \
	MCAL_read_data.o 
	mv *.mod $(INC_DIR)

test_all: 
	cd $(MFT_DIR)/test ; \
        make clean && make all ; \
	./bin/Test.exe ; ./bin/Test_attributs.exe ; \
	./bin/Test_CHAR_MFT.exe ; ./bin/Test_compute_short.exe ; \
	 #/bin/rm tmp/Short_test.nc ; /bin/rm tmp/Test_att.nc ; /bin/rm tmp/test_write.nc
install: 
	mkdir -p $(INSTALL_DIR)
	cp -rf $(INC_DIR) $(INSTALL_DIR)
	cp -rf $(LIB_DIR) $(INSTALL_DIR)
	cp -rf $(MFT_DIR)/param $(INSTALL_DIR)
	cp -rf $(TMP_NAMELIST) $(NAMELIST)
###cp -rf $(PARAM_DIR) $(INSTALL_DIR)

clean:
	rm -f  *.o *.mod $(INC_DIR)/* $(LIB_DIR)/$(LIB_MFT) 
