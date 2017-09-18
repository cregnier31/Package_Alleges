!MODULE src_read_ATTLIST
!**-----------------------------------------
!** Module for read an attribute list in NCDF format
!** C.REGNIER Janvier 2013 V3.5 Miol 
!**-------------------------------------------
  ! ---   interface MIOL_read_attributeslist_NC
  ! --- MIOL_readf_attributeslist_NC (cd_filename,cd_varname,id_nbatt,cdpa_attname,cdpa_attvalue,rdpa_attvalue,idpa_typevalue)
  ! --- MIOL_readu_attributeslist_NC (id_file_id,cd_varname,id_nbatt,cdpa_attname,cdpa_attvalue,rdpa_attvalue,idpa_typevalue)
!-------------------------------------------
!> \todo which contain subroutine for Read attributes list in NETCDF format 
!! \n  Total 2 functions
!! \author F.MESSAL first version
!! \date 11/2006
!! \version 1.1
!! \author C.REGNIER Miol V3.5
!! \date 01/2013
!! \version 3.5
!<
  !******************************************************************************
 
 
          !!=====================================================================
          !> \brief
          !! Description: This function gets values of variable or global attributes.
          !!              This function uses pointers, so you don’t have to know
          !!              the correct dimensions of the data arrays. If you use
          !!              also the netcdf library, you can compare values of
          !!              the “typevalue” array to the set of predefined netCDF
          !!              external data types: NF90_BYTE, NF90_CHAR, NF90_SHORT,
          !!              NF90_INT, NF90_FLOAT, NF90_DOUBLE.
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        The name of the variable to be read, or 'global'
          !!                     for global attributes.
          !! @param id_nbatt          Returned the number of attributes.
          !! @param cdpa_attname      Returned vector of with the attribute names.
          !! @param cdpa_attcvalue    Returned vector of NF90_CHAR values if the attribute
          !!                     type is NF90_CHAR else the value is ''.
          !! @param rdpa_attrvalue    Returned vector of NF90_FLOAT values if the attribute
          !!                     type is NF90_FLOAT else the value is 0.
          !! @param idpa_typevalue    Returned vector of type values (2 = NF90_CHAR ;
          !!                     5 = NF90_FLOAT)
          !!
          !! \n History :
          !!          \n 11/2006  (F. Messal)
          !!          \n 01/2013  (C.REGNIER) SVN V3.5.0
          !<
          !!=====================================================================

       SUBROUTINE MIOL_readf_attributeslist_NC (cd_filename, &
                                            cd_varname, &
                                            id_nbatt, &
                                            cdpa_attname, &
                                            cdpa_attvalue, &
                                            rdpa_attvalue, &
                                            idpa_typevalue)
 
          USE netcdf
          USE MFT_error
          IMPLICIT NONE
 
          !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
          INTEGER,                          INTENT(OUT) :: id_nbatt
          CHARACTER(LEN=255), DIMENSION(:), POINTER:: cdpa_attname
          CHARACTER(LEN=255), DIMENSION(:), POINTER :: cdpa_attvalue
          REAL(KIND=4), DIMENSION(:),       POINTER,OPTIONAL :: rdpa_attvalue
          INTEGER, DIMENSION(:),            POINTER, OPTIONAL :: idpa_typevalue
 
          INTEGER :: il_file_id, il_var_id, il_type, il_nbdim, il_nbatt, &
                     il_ji, il_status, il_nbvar
          INTEGER, DIMENSION(:), ALLOCATABLE :: ila_typevalue
          CHARACTER(LEN=60) :: cl_tmp,cl_fonction
          INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimids
 
          cl_fonction="MIOL_readf_attributeslist_NC"
 
          !----------------------------------------------------------------------
          ! Open file
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_NOWRITE, &
                                           il_file_id),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Read variables
          IF ((TRIM(cd_varname) .NE. 'global') .AND. &
              (TRIM(cd_varname) .NE. 'GLOBAL')) THEN
 
             il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                   TRIM(cd_varname), &
                                                   il_var_id),cl_fonction)
 
             il_status = fi_ncError(NF90_INQUIRE_VARIABLE(il_file_id, &
                                                          il_var_id, &
                                                          cl_tmp, &
                                                          il_type, &
                                                          il_nbdim),cl_fonction)
 
             ALLOCATE(ila_dimids(il_nbdim), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' ila_dimids',cl_fonction)
 
             il_status = fi_ncError(NF90_INQUIRE_VARIABLE(il_file_id, &
                                                          il_var_id, &
                                                          cl_tmp, &
                                                          il_type, &
                                                          il_nbdim, &
                                                          ila_dimids, &
                                                          il_nbatt),cl_fonction)
 
          ELSE
 
             il_status = fi_ncError(NF90_INQUIRE(il_file_id, &
                                                 il_nbdim, &
                                                 il_nbvar, &
                                                 il_nbatt),cl_fonction)
 
          ENDIF
 
 
          !----------------------------------------------------------------------
          ! Memory allocation
 
          id_nbatt = il_nbatt
          ALLOCATE(cdpa_attname(il_nbatt), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' cdpa_attname',cl_fonction)
 
          ALLOCATE(cdpa_attvalue(il_nbatt), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' cdpa_attvalue',cl_fonction)
 
          ALLOCATE(ila_typevalue(il_nbatt), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' ila_typevalue',cl_fonction)
 
 
          IF (PRESENT(rdpa_attvalue)) THEN
             ALLOCATE(rdpa_attvalue(il_nbatt), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_attvalue',cl_fonction)
          ENDIF
 
 
          IF (PRESENT(idpa_typevalue)) THEN
             ALLOCATE(idpa_typevalue(il_nbatt), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' idpa_typevalue',cl_fonction)
          ENDIF
 
 
          !----------------------------------------------------------------------
          ! Read attributes
 
          ! Global attributes
          IF ((TRIM(cd_varname) .NE. 'global') .AND. &
              (TRIM(cd_varname) .NE. 'GLOBAL')) THEN
 
             DO il_ji = 1, il_nbatt
 
                il_status = fi_ncError(NF90_INQ_ATTNAME(il_file_id, &
                                                        il_var_id, &
                                                        il_ji, &
                                                        cdpa_attname(il_ji)),cl_fonction)
 
                il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                              il_var_id, &
                                                              cdpa_attname(il_ji), &
                                                              ila_typevalue(il_ji)),cl_fonction)
 
                IF (ila_typevalue(il_ji) .EQ. NF90_CHAR) THEN
 
                   il_status = fi_ncError(NF90_GET_ATT(il_file_id, &
                                                       il_var_id, &
                                                       cdpa_attname(il_ji), &
                                                       cdpa_attvalue(il_ji)),cl_fonction)
                ELSE
                   cdpa_attvalue(il_ji) = ''
                ENDIF
 
                IF (PRESENT(rdpa_attvalue)) THEN
 
                   IF (ila_typevalue(il_ji) .EQ. NF90_FLOAT) THEN
 
                      il_status = fi_ncError(NF90_GET_ATT(il_file_id, &
                                                          il_var_id, &
                                                          cdpa_attname(il_ji), &
                                                          rdpa_attvalue(il_ji)),cl_fonction)
 
                   ELSE
                      rdpa_attvalue(il_ji) = 0
                   ENDIF
 
                ENDIF
 
                IF (PRESENT(idpa_typevalue)) THEN
                   idpa_typevalue(il_ji) = ila_typevalue(il_ji)
                ENDIF
 
             ENDDO
 
          ELSE
 
             ! variable attributes
             DO il_ji = 1, il_nbatt
 
                il_status = fi_ncError(NF90_INQ_ATTNAME(il_file_id, &
                                                        NF90_GLOBAL, &
                                                        il_ji, &
                                                        cdpa_attname(il_ji)),cl_fonction)
 
                il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                              NF90_GLOBAL, &
                                                              cdpa_attname(il_ji), &
                                                              ila_typevalue(il_ji)),cl_fonction)
 
                IF (ila_typevalue(il_ji) .EQ. NF90_CHAR) THEN
 
                   il_status = fi_ncError(NF90_GET_ATT(il_file_id, &
                                                       NF90_GLOBAL, &
                                                       cdpa_attname(il_ji), &
                                                       cdpa_attvalue(il_ji)),cl_fonction)
 
               ELSE
                   cdpa_attvalue(il_ji) = ''
                ENDIF
 
                IF (PRESENT(rdpa_attvalue)) THEN
                   IF (ila_typevalue(il_ji) .EQ. NF90_FLOAT) THEN
 
                      il_status = fi_ncError(NF90_GET_ATT(il_file_id, &
                                                          NF90_GLOBAL, &
                                                          cdpa_attname(il_ji), &
                                                          rdpa_attvalue(il_ji)),cl_fonction)
 
                   ELSE
                      rdpa_attvalue(il_ji) = 0
                   ENDIF
                ENDIF
 
                IF (PRESENT(idpa_typevalue)) THEN
                   idpa_typevalue(il_ji) = ila_typevalue(il_ji)
                ENDIF
 
             ENDDO
 
          ENDIF
 
 
          !----------------------------------------------------------------------
          ! Close file
 
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
 
       END SUBROUTINE MIOL_readf_attributeslist_NC
 
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
  
          !!=====================================================================
          !> \brief
          !! Description: This function gets values of variable or global attributes.
          !!              This function uses pointers, so you don’t have to know
          !!              the correct dimensions of the data arrays. If you use
          !!              also the netcdf library, you can compare values of
          !!              the “typevalue” array to the set of predefined netCDF
          !!              external data types: NF90_BYTE, NF90_CHAR, NF90_SHORT,
          !!              NF90_INT, NF90_FLOAT, NF90_DOUBLE.
          !!
          !! @param id_file_id        A NetCDF file Id.
          !! @param cd_varname        The name of the variable to be read, or 'global'
          !!                     for global attributes.
          !! @param id_nbatt          Returned the number of attributes.
          !! @param cdpa_attname      Returned vector of with the attribute names.
          !! @param cdpa_attcvalue    Returned vector of NF90_CHAR values if the attribute
          !!                     type is NF90_CHAR else the value is ''.
          !! @param rdpa_attrvalue    Returned vector of NF90_FLOAT values if the attribute
          !!                     type is NF90_FLOAT else the value is 0.
          !! @param idpa_typevalue    Returned vector of type values (2 = NF90_CHAR ;
          !!                     5 = NF90_FLOAT)
          !!
          !! \n History :
          !!          \n 07/2007  (F. Messal)
          !!          \n 01/2013  (C.REGNIER) SVN V3.5.0
          !>
          !!=====================================================================
 
       SUBROUTINE MIOL_readu_attributeslist_NC (id_file_id, &
                                            cd_varname, &
                                            id_nbatt, &
                                            cdpa_attname, &
                                            cdpa_attvalue, &
                                            rdpa_attvalue, &
                                            idpa_typevalue)
 
          USE netcdf
          USE MFT_error
          IMPLICIT NONE
 
 
         
          !----------------------------------------------------------------------
 
          INTEGER,                          INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
          INTEGER,                          INTENT(OUT) :: id_nbatt
          CHARACTER(LEN=255), DIMENSION(:), POINTER :: cdpa_attname
          CHARACTER(LEN=255), DIMENSION(:), POINTER  :: cdpa_attvalue
          REAL(KIND=4), DIMENSION(:),       POINTER,OPTIONAL :: rdpa_attvalue
          INTEGER, DIMENSION(:),            POINTER,OPTIONAL :: idpa_typevalue
 
          INTEGER :: il_var_id, il_type, il_nbdim, il_nbatt, &
                     il_ji, il_status, il_nbvar
          INTEGER, DIMENSION(:), ALLOCATABLE :: ila_typevalue
          CHARACTER(LEN=60) :: cl_tmp,cl_fonction
          INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimids
 
          cl_fonction="MIOL_readu_attributeslist_NC"

          !----------------------------------------------------------------------
          ! Read variables
          IF ((TRIM(cd_varname) .NE. 'global') .AND. &
              (TRIM(cd_varname) .NE. 'GLOBAL')) THEN
 
             il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                   TRIM(cd_varname), &
                                                   il_var_id),cl_fonction)
 
             il_status = fi_ncError(NF90_INQUIRE_VARIABLE(id_file_id, &
                                                          il_var_id, &
                                                          cl_tmp, &
                                                          il_type, &
                                                          il_nbdim),cl_fonction)
 
             ALLOCATE(ila_dimids(il_nbdim), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' ila_dimids',cl_fonction)
 
             il_status = fi_ncError(NF90_INQUIRE_VARIABLE(id_file_id, &
                                                          il_var_id, &
                                                          cl_tmp, &
                                                          il_type, &
                                                          il_nbdim, &
                                                          ila_dimids, &
                                                          il_nbatt),cl_fonction)
 
          ELSE
 
             il_status = fi_ncError(NF90_INQUIRE(id_file_id, &
                                                 il_nbdim, &
                                                 il_nbvar, &
                                                 il_nbatt),cl_fonction)
 
          ENDIF
 
 
          !----------------------------------------------------------------------
          ! Memory allocation
 
          id_nbatt = il_nbatt
          ALLOCATE(cdpa_attname(il_nbatt), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' cdpa_attname',cl_fonction)
 
          ALLOCATE(cdpa_attvalue(il_nbatt), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' cdpa_attvalue',cl_fonction)
 
          ALLOCATE(ila_typevalue(il_nbatt), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' ila_typevalue',cl_fonction)
 
 
          IF (PRESENT(rdpa_attvalue)) THEN
             ALLOCATE(rdpa_attvalue(il_nbatt), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_attvalue',cl_fonction)
          ENDIF
 
 
          IF (PRESENT(idpa_typevalue)) THEN
             ALLOCATE(idpa_typevalue(il_nbatt), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' idpa_typevalue',cl_fonction)
          ENDIF
 
 
          !----------------------------------------------------------------------
          ! Read attributes
 
          ! Global attributes
          IF ((TRIM(cd_varname) .NE. 'global') .AND. &
              (TRIM(cd_varname) .NE. 'GLOBAL')) THEN
 
             DO il_ji = 1, il_nbatt
 
                il_status = fi_ncError(NF90_INQ_ATTNAME(id_file_id, &
                                                        il_var_id, &
                                                        il_ji, &
                                                        cdpa_attname(il_ji)),cl_fonction)
 
                il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                              il_var_id, &
                                                              cdpa_attname(il_ji), &
                                                              ila_typevalue(il_ji)),cl_fonction)
 
                IF (ila_typevalue(il_ji) .EQ. NF90_CHAR) THEN
 
                   il_status = fi_ncError(NF90_GET_ATT(id_file_id, &
                                                       il_var_id, &
                                                       cdpa_attname(il_ji), &
                                                       cdpa_attvalue(il_ji)),cl_fonction)
                ELSE
                   cdpa_attvalue(il_ji) = ''
                ENDIF
 
                IF (PRESENT(rdpa_attvalue)) THEN
 
                   IF (ila_typevalue(il_ji) .EQ. NF90_FLOAT) THEN
 
                      il_status = fi_ncError(NF90_GET_ATT(id_file_id, &
                                                          il_var_id, &
                                                          cdpa_attname(il_ji), &
                                                          rdpa_attvalue(il_ji)),cl_fonction)
 
                   ELSE
                      rdpa_attvalue(il_ji) = 0
                   ENDIF
 
                ENDIF
 
                IF (PRESENT(idpa_typevalue)) THEN
                   idpa_typevalue(il_ji) = ila_typevalue(il_ji)
                ENDIF
 
             ENDDO
 
          ELSE
 
             ! variable attributes
             DO il_ji = 1, il_nbatt
 
                il_status = fi_ncError(NF90_INQ_ATTNAME(id_file_id, &
                                                        NF90_GLOBAL, &
                                                        il_ji, &
                                                        cdpa_attname(il_ji)),cl_fonction)
 
                il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                              NF90_GLOBAL, &
                                                              cdpa_attname(il_ji), &
                                                              ila_typevalue(il_ji)),cl_fonction)
 
                IF (ila_typevalue(il_ji) .EQ. NF90_CHAR) THEN
 
                   il_status = fi_ncError(NF90_GET_ATT(id_file_id, &
                                                       NF90_GLOBAL, &
                                                       cdpa_attname(il_ji), &
                                                       cdpa_attvalue(il_ji)),cl_fonction)
 
               ELSE
                   cdpa_attvalue(il_ji) = ''
                ENDIF
 
                IF (PRESENT(rdpa_attvalue)) THEN
                   IF (ila_typevalue(il_ji) .EQ. NF90_FLOAT) THEN
 
                      il_status = fi_ncError(NF90_GET_ATT(id_file_id, &
                                                          NF90_GLOBAL, &
                                                          cdpa_attname(il_ji), &
                                                          rdpa_attvalue(il_ji)),cl_fonction)
 
                   ELSE
                      rdpa_attvalue(il_ji) = 0
                   ENDIF
                ENDIF
 
                IF (PRESENT(idpa_typevalue)) THEN
                   idpa_typevalue(il_ji) = ila_typevalue(il_ji)
                ENDIF
 
             ENDDO
 
          ENDIF
 
 
        END SUBROUTINE MIOL_readu_attributeslist_NC
 
 
 
  !******************************************************************************
  !******************************************************************************
