! Source src_read_Integer for read Short values in NETCDF format
!**-----------------------------------------
!** C.REGNIER Janvier 2013 V3.5 Miol 
!**-----------------------------------------
! ---  MIOL_readf_field_I4_4D_NC(cd_filename,cd_varname,idpa_value,ida_dimsize)
! ---  MIOL_readf_field_I4_3D_NC(cd_filename,cd_varname,idpa_value,ida_dimsize)
! ---  MIOL_readf_field_I4_2D_NC(cd_filename,cd_varname,idpa_value,ida_dimsize)
! ---  MIOL_readf_field_I4_1D_NC(cd_filename,cd_varname,idpa_value,ida_dimsize)
! ---  MIOL_readu_field_I4_4D_NC (id_file_id,cd_varname,idpa_value,ida_dimsize)
! ---  MIOL_readu_field_I4_3D_NC (id_file_id,cd_varname,idpa_value,ida_dimsize)
! ---  MIOL_readu_field_I4_2D_NC (id_file_id,cd_varname,idpa_value,ida_dimsize)
! ---  MIOL_readu_field_I4_1D_NC (id_file_id,cd_varname,idpa_value,ida_dimsize)
!**-----------------------------------------
!> \brief File which contain subroutine for Read Integer values in NETCDF format
!! \n  Total 8 functions
!! \author F.MESSAL first version
!! \date 11/2006
!!  \version 1.1
!! \author C.REGNIER Miol V3.5
!! \date 07/2008
!!  \version 3.5
!< 

  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
         !!=====================================================================
          !> \brief
          !! Description: This function gets data values from the variable defined
          !!              in argument. You can also know the length of the variable
          !!              dimensions.
          !!              This functions allows you to get array of data values
          !!              without knowing the correct lenght of each dimension.
          !!              You just have to know the type and the number of
          !!              dimensions. This function uses pointers, so you don’t
          !!              have to know the correct dimensions of the data arrays.
          !!
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:,:) :: idpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !!
          !!\n  History :
          !!         \n   06/2006  (F. Messal) F90
          !!         \n   11/2006  (F. Messal) CVS V1.0
          !!         \n   10/2008  (C.REGNIER) SVN V3.1.0
          !!         \n   09/2012  (C.REGNIER) SVN V3.4
          !!         \n   01/2013  (C.REGNIER) SVN V3.5
          !<
          !!=====================================================================
  
 
       SUBROUTINE MIOL_readf_field_I4_4D_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
 
         USE MFT_error
          
          USE MIOL_param
          USE netcdf
          IMPLICIT NONE
 
     
          !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=4), DIMENSION(:,:,:,:), POINTER :: idpa_value
          INTEGER(KIND=4), DIMENSION(4), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 
          CHARACTER(LEN=255), DIMENSION(4) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp,cl_fonction
          INTEGER, DIMENSION(4) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_file_id, il_var_id, il_status, il_ji
 
 
          !----------------------------------------------------------------------
          ! Open file
          cl_fonction="MIOL_readf_field_I4_3D_NC"
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_NOWRITE, &
                                           il_file_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Inquire variable
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_VARIABLE(il_file_id, &
                                                       il_var_id, &
                                                       cl_tmp, &
                                                       il_type, &
                                                       il_nbdim, &
                                                       ila_dimids),cl_fonction)
 
          IF (il_nbdim.NE.4) THEN
             WRITE(0,*) "MIOL_read_field_I4_4D_NC: array dimension error. "
             CALL flush(0)
             STOP
          ENDIF
 
          !----------------------------------------------------------------------
          ! Read lenght of dimensions
 
          DO il_ji = 1, il_nbdim
             il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                           ila_dimids(il_ji), &
                                                           cla_dimname(il_ji), &
                                                           ila_dimsize(il_ji)),cl_fonction)
          ENDDO
 
 
          !----------------------------------------------------------------------
          ! Memory allocation
 
          ALLOCATE(idpa_value(ila_dimsize(1), &
                              ila_dimsize(2), &
                              ila_dimsize(3),&
                              ila_dimsize(4)), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
 
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_var_id, &
                                              idpa_value),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
          
          !----------------------------------------------------------------------
          ! Close file
 
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
        END SUBROUTINE MIOL_readf_field_I4_4D_NC

  !******************************************************************************
  !******************************************************************************
  !******************************************************************************

          !!=====================================================================
          !> \brief
          !! Description: This function gets data values from the variable defined
          !!              in argument. You can also know the length of the variable
          !!              dimensions.
          !!              This functions allows you to get array of data values
          !!              without knowing the correct lenght of each dimension.
          !!              You just have to know the type and the number of
          !!              dimensions. This function uses pointers, so you don’t
          !!              have to know the correct dimensions of the data arrays.
          !!
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:,:) :: idpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !!
          !!\n  History :
          !!         \n   06/2006  (F. Messal) F90
          !!         \n   11/2006  (F. Messal) CVS V1.0
          !!         \n   10/2008  (C.REGNIER) SVN V3.1.0
          !!         \n   09/2012  (C.REGNIER) SVN V3.4
          !!         \n   01/2013  (C.REGNIER) SVN V3.5                                       
          !<
          !!=====================================================================
  
 
       SUBROUTINE MIOL_readf_field_I4_3D_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
 
         USE MFT_error
          
          USE MIOL_param
          USE netcdf
          IMPLICIT NONE
 
     
          !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=4), DIMENSION(:,:,:), POINTER :: idpa_value
          INTEGER(KIND=4), DIMENSION(3), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 
          CHARACTER(LEN=255), DIMENSION(3) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp,cl_fonction
          INTEGER, DIMENSION(3) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_file_id, il_var_id, il_status, il_ji
 
 
          !----------------------------------------------------------------------
          ! Open file
          cl_fonction="MIOL_readf_field_I4_3D_NC"
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_NOWRITE, &
                                           il_file_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Inquire variable
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_VARIABLE(il_file_id, &
                                                       il_var_id, &
                                                       cl_tmp, &
                                                       il_type, &
                                                       il_nbdim, &
                                                       ila_dimids),cl_fonction)
 
          IF (il_nbdim.NE.3) THEN
             WRITE(0,*) "MIOL_read_field_I4_4D_NC: array dimension error. "
             CALL flush(0)
             STOP
          ENDIF
 
          !----------------------------------------------------------------------
          ! Read lenght of dimensions
 
          DO il_ji = 1, il_nbdim
             il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                           ila_dimids(il_ji), &
                                                           cla_dimname(il_ji), &
                                                           ila_dimsize(il_ji)),cl_fonction)
          ENDDO
 
 
          !----------------------------------------------------------------------
          ! Memory allocation
 
          ALLOCATE(idpa_value(ila_dimsize(1), &
                              ila_dimsize(2), &
                              ila_dimsize(3)), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
 
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_var_id, &
                                              idpa_value),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
          
          !----------------------------------------------------------------------
          ! Close file
 
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
        END SUBROUTINE MIOL_readf_field_I4_3D_NC
 
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
          
          !!=====================================================================
          !> \brief
          !! Description: This function gets data values from the variable defined
          !!              in argument. You can also know the length of the variable
          !!              dimensions.
          !!              This functions allows you to get array of data values
          !!              without knowing the correct lenght of each dimension.
          !!              You just have to know the type and the number of
          !!              dimensions. This function uses pointers, so you don’t
          !!              have to know the correct dimensions of the data arrays.
          !!
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:) :: idpa_value
          !!
          !! @param filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !!
          !!\n  History :
          !!         \n   06/2006  (F. Messal) F90
          !!         \n   11/2006  (F. Messal) CVS V1.0
          !!         \n   01/2013  (C.REGNIER) SVN V3.5.0
          !<
          !!=====================================================================
  
       SUBROUTINE MIOL_readf_field_I4_2D_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
 
         USE MFT_error
         
         USE MIOL_param
         USE netcdf
         IMPLICIT NONE
         
          !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=4), DIMENSION(:,:), POINTER :: idpa_value
          INTEGER(KIND=4), DIMENSION(2), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 
          CHARACTER(LEN=255), DIMENSION(2) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp,cl_fonction
          INTEGER, DIMENSION(2) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_file_id, il_var_id, il_status, il_ji
 
          cl_fonction="MIOL_readf_field_I4_2D_NC"
          !----------------------------------------------------------------------
          ! Open file
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_NOWRITE, &
                                           il_file_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Inquire variable
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_VARIABLE(il_file_id, &
                                                       il_var_id, &
                                                       cl_tmp, &
                                                       il_type, &
                                                       il_nbdim, &
                                                       ila_dimids),cl_fonction)
 
          IF (il_nbdim.NE.2) THEN
             WRITE(0,*) "MIOL_read_field_I4_2D_NC: array dimension error. "
             CALL flush(0)
             STOP
          ENDIF
 
          !----------------------------------------------------------------------
          ! Read lenght of dimensions
 
          DO il_ji = 1, il_nbdim
             il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                           ila_dimids(il_ji), &
                                                           cla_dimname(il_ji), &
                                                           ila_dimsize(il_ji)),cl_fonction)
          ENDDO
 
 
          !----------------------------------------------------------------------
          ! Memory allocation
 
          ALLOCATE(idpa_value(ila_dimsize(1), &
                              ila_dimsize(2)), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
 
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_var_id, &
                                              idpa_value),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
 
          !----------------------------------------------------------------------
          ! Close file
 
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)

        END SUBROUTINE MIOL_readf_field_I4_2D_NC
 
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
         
          !===================================================================
          !> \brief
          !! Description: This function gets data values from the variable defined
          !!              in argument. You can also know the length of the variable
          !!              dimensions.
          !!              This functions allows you to get array of data values
          !!              without knowing the correct lenght of each dimension.
          !!              You just have to know the type and the number of
          !!              dimensions. This function uses pointers, so you don’t
          !!              have to know the correct dimensions of the data arrays.
          !!
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:) :: idpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !!\n  History :
          !!         \n   06/2006  (F. Messal) F90
          !!         \n   11/2006  (F. Messal) CVS V1.0
          !!         \n   01/2013  (C.REGNIER) SVN V3.5.0
          !<
          !!=====================================================================
 
 
       SUBROUTINE MIOL_readf_field_I4_1D_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
 
         USE MFT_error
         
         USE MIOL_param
         USE netcdf
         IMPLICIT NONE
        
          !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=4), DIMENSION(:), POINTER  :: idpa_value
          INTEGER(KIND=4), DIMENSION(1), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 
          CHARACTER(LEN=255), DIMENSION(1) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp,cl_fonction
          INTEGER, DIMENSION(1) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_file_id, il_var_id, il_status, il_ji
 
          cl_fonction="MIOL_readf_field_I4_1D_NC"
          !----------------------------------------------------------------------
          ! Open file
 
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_NOWRITE, &
                                           il_file_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Inquire variable
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_VARIABLE(il_file_id, &
                                                       il_var_id, &
                                                       cl_tmp, &
                                                       il_type, &
                                                       il_nbdim, &
                                                       ila_dimids),cl_fonction)
 
          IF (il_nbdim.NE.1) THEN
             WRITE(0,*) "MIOL_read_field_I4_1D_NC: array dimension error. "
             CALL flush(0)
             STOP
          ENDIF
 
          !----------------------------------------------------------------------
          ! Read lenght of dimensions
 
          DO il_ji = 1, il_nbdim
             il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                           ila_dimids(il_ji), &
                                                           cla_dimname(il_ji), &
                                                           ila_dimsize(il_ji)),cl_fonction)
          ENDDO
 
 
          !----------------------------------------------------------------------
          ! Memory allocation
 
          ALLOCATE(idpa_value(ila_dimsize(1)), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
 
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_var_id, &
                                              idpa_value),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
 
          !----------------------------------------------------------------------
          ! Close file
 
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)

        END SUBROUTINE MIOL_readf_field_I4_1D_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
  
          !!=====================================================================
          !> \brief
          !! Description: This function gets data values from the variable defined
          !!              in argument. You can also know the length of the variable
          !!              dimensions.
          !!              This functions allows you to get array of data values
          !!              without knowing the correct lenght of each dimension.
          !!              You just have to know the type and the number of
          !!              dimensions. This function uses pointers, so you don’t
          !!              have to know the correct dimensions of the data arrays.
          !!
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:,:) :: idpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !!\n  History :
          !!         \n   06/2006  (F. Messal) F90
          !!         \n   11/2006  (F. Messal) CVS V1.0
          !!         \n   01/2013  (C.REGNIER) SVN V3.5.0
          !<!
          !!=====================================================================
  
       SUBROUTINE MIOL_readu_field_I4_4D_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
 
           USE MFT_error
           
           USE MIOL_param
           USE netcdf
           IMPLICIT NONE
 
           !----------------------------------------------------------------------
 
          INTEGER,                           INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=4), DIMENSION(:,:,:,:), POINTER :: idpa_value
          INTEGER(KIND=4), DIMENSION(4), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 
          CHARACTER(LEN=255), DIMENSION(4) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp,cl_fonction
          INTEGER, DIMENSION(4) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji
 
          cl_fonction="MIOL_readu_field_I4_4D_NC"
          !----------------------------------------------------------------------
          ! Inquire variable
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_VARIABLE(id_file_id, &
                                                       il_var_id, &
                                                       cl_tmp, &
                                                       il_type, &
                                                       il_nbdim, &
                                                       ila_dimids),cl_fonction)
 
          IF (il_nbdim.NE.4) THEN
             WRITE(0,*) "MIOL_read_field_I4_4D_NC: array dimension error. "
             CALL flush(0)
             STOP
          ENDIF
 
          !----------------------------------------------------------------------
          ! Read lenght of dimensions
 
          DO il_ji = 1, il_nbdim
             il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id, &
                                                           ila_dimids(il_ji), &
                                                           cla_dimname(il_ji), &
                                                           ila_dimsize(il_ji)),cl_fonction)
          ENDDO
 
 
          !----------------------------------------------------------------------
          ! Memory allocation
 
          ALLOCATE(idpa_value(ila_dimsize(1), &
                              ila_dimsize(2), &
                              ila_dimsize(3),&
                              ila_dimsize(4)), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Read variable
 
          il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                              il_var_id, &
                                              idpa_value),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF

        END SUBROUTINE MIOL_readu_field_I4_4D_NC



 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
  
          !!=====================================================================
          !> \brief
          !! Description: This function gets data values from the variable defined
          !!              in argument. You can also know the length of the variable
          !!              dimensions.
          !!              This functions allows you to get array of data values
          !!              without knowing the correct lenght of each dimension.
          !!              You just have to know the type and the number of
          !!              dimensions. This function uses pointers, so you don’t
          !!              have to know the correct dimensions of the data arrays.
          !!
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:,:) :: idpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !!\n  History :
          !!         \n   06/2006  (F. Messal) F90
          !!         \n   11/2006  (F. Messal) CVS V1.0
          !!         \n   01/2013  (C.REGNIER) SVN V3.5.0
          !<!
          !!=====================================================================
  
       SUBROUTINE MIOL_readu_field_I4_3D_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
 
           USE MFT_error
           
           USE MIOL_param
           USE netcdf
           IMPLICIT NONE
 
           !----------------------------------------------------------------------
 
          INTEGER,                           INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=4), DIMENSION(:,:,:), POINTER :: idpa_value
          INTEGER(KIND=4), DIMENSION(3), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 
          CHARACTER(LEN=255), DIMENSION(3) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp,cl_fonction
          INTEGER, DIMENSION(3) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji
 
          cl_fonction="MIOL_readu_field_I4_3D_NC"
          !----------------------------------------------------------------------
          ! Inquire variable
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_VARIABLE(id_file_id, &
                                                       il_var_id, &
                                                       cl_tmp, &
                                                       il_type, &
                                                       il_nbdim, &
                                                       ila_dimids),cl_fonction)
 
          IF (il_nbdim.NE.3) THEN
             WRITE(0,*) "MIOL_read_field_I4_4D_NC: array dimension error. "
             CALL flush(0)
             STOP
          ENDIF
 
          !----------------------------------------------------------------------
          ! Read lenght of dimensions
 
          DO il_ji = 1, il_nbdim
             il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id, &
                                                           ila_dimids(il_ji), &
                                                           cla_dimname(il_ji), &
                                                           ila_dimsize(il_ji)),cl_fonction)
          ENDDO
 
 
          !----------------------------------------------------------------------
          ! Memory allocation
 
          ALLOCATE(idpa_value(ila_dimsize(1), &
                              ila_dimsize(2), &
                              ila_dimsize(3)), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
 
          il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                              il_var_id, &
                                              idpa_value),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
 

        END SUBROUTINE MIOL_readu_field_I4_3D_NC
 
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
          !!=====================================================================
          !> \brief
          !! Description: This function gets data values from the variable defined
          !!              in argument. You can also know the length of the variable
          !!              dimensions.
          !!              This functions allows you to get array of data values
          !!              without knowing the correct lenght of each dimension.
          !!              You just have to know the type and the number of
          !!              dimensions. This function uses pointers, so you don’t
          !!              have to know the correct dimensions of the data arrays.
          !!
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:) :: idpa_value
          !!
          !! @param id_file_id        A NetCDF file Id.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !!\n  History :
          !!         \n   06/2006  (F. Messal) F90
          !!         \n   11/2006  (F. Messal) CVS V1.0
          !!         \n   01/2013  (C.REGNIER) SVN V3.5.0
          !<
          !!=====================================================================
  
       SUBROUTINE MIOL_readu_field_I4_2D_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
 
         USE netcdf
         USE MFT_error
         
         USE MIOL_param
         IMPLICIT NONE
         
          !----------------------------------------------------------------------
 
          INTEGER,                           INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=4), DIMENSION(:,:), POINTER :: idpa_value
          INTEGER(KIND=4), DIMENSION(2), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 
          CHARACTER(LEN=255), DIMENSION(2) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp,cl_fonction
          INTEGER, DIMENSION(2) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji
 
          cl_fonction="MIOL_readu_field_I4_2D_NC"
          !----------------------------------------------------------------------
          ! Inquire variable
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_VARIABLE(id_file_id, &
                                                       il_var_id, &
                                                       cl_tmp, &
                                                       il_type, &
                                                       il_nbdim, &
                                                       ila_dimids),cl_fonction)
 
          IF (il_nbdim.NE.2) THEN
             WRITE(0,*) "MIOL_read_field_I4_2D_NC: array dimension error. "
             CALL flush(0)
             STOP
          ENDIF
 
          !----------------------------------------------------------------------
          ! Read lenght of dimensions
 
          DO il_ji = 1, il_nbdim
             il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id, &
                                                           ila_dimids(il_ji), &
                                                           cla_dimname(il_ji), &
                                                           ila_dimsize(il_ji)),cl_fonction)
          ENDDO
 
 
          !----------------------------------------------------------------------
          ! Memory allocation
 
          ALLOCATE(idpa_value(ila_dimsize(1), &
                              ila_dimsize(2)), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
 
          il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                              il_var_id, &
                                              idpa_value),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
 

        END SUBROUTINE MIOL_readu_field_I4_2D_NC
 
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************

          !!=====================================================================
          !> \brief
          !! Description: This function gets data values from the variable defined
          !!              in argument. You can also know the length of the variable
          !!              dimensions.
          !!              This functions allows you to get array of data values
          !!              without knowing the correct lenght of each dimension.
          !!              You just have to know the type and the number of
          !!              dimensions. This function uses pointers, so you don’t
          !!              have to know the correct dimensions of the data arrays.
          !!
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:) :: idpa_value
          !!
          !! @param id_file_id        A NetCDF file Id.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !!
          !!\n  History :
          !!         \n   06/2006  (F. Messal) F90
          !!         \n   11/2006  (F. Messal) CVS V1.0
          !!         \n   01/2013  (C.REGNIER) SVN V3.5.0
          !<
          !!===================================================================== 
 
       SUBROUTINE MIOL_readu_field_I4_1D_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
 
         USE netcdf
         USE MFT_error
         
         USE MIOL_param
         IMPLICIT NONE
 
           !----------------------------------------------------------------------
 
          INTEGER,                           INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=4), DIMENSION(:), POINTER :: idpa_value
          INTEGER(KIND=4), DIMENSION(1), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 
          CHARACTER(LEN=255), DIMENSION(1) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp,cl_fonction
          INTEGER, DIMENSION(1) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji
 
          cl_fonction="MIOL_readu_field_I4_1D_NC"
          !----------------------------------------------------------------------
          ! Inquire variable
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_VARIABLE(id_file_id, &
                                                       il_var_id, &
                                                       cl_tmp, &
                                                       il_type, &
                                                       il_nbdim, &
                                                       ila_dimids),cl_fonction)
 
          IF (il_nbdim.NE.1) THEN
             WRITE(0,*) "MIOL_read_field_I4_1D_NC: array dimension error. "
             CALL flush(0)
             STOP
          ENDIF
 
          !----------------------------------------------------------------------
          ! Read lenght of dimensions
 
          DO il_ji = 1, il_nbdim
             il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id, &
                                                           ila_dimids(il_ji), &
                                                           cla_dimname(il_ji), &
                                                           ila_dimsize(il_ji)),cl_fonction)
          ENDDO
 
 
          !----------------------------------------------------------------------
          ! Memory allocation
 
          ALLOCATE(idpa_value(ila_dimsize(1)), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
 
          il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                              il_var_id, &
                                              idpa_value),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
 
 
        END SUBROUTINE MIOL_readu_field_I4_1D_NC
 
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
  
 
