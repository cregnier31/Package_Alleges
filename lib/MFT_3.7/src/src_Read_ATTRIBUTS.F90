 !**-----------------------------------------
!** Module for read attributs values in NETCDF format
!** C.REGNIER Janvier 2013 V3.5 Miol 
!**-----------------------------------------------------------------------------------------
! --- SUBROUTINE MIOL_readf_attribute_R8_NC (cd_filename,cd_varname,cd_attname,rd_attvalue)
! --- SUBROUTINE MIOL_readf_attribute_R4_NC (cd_filename,cd_varname,cd_attname,rd_attvalue)
! --- SUBROUTINE MIOL_readf_attribute_I_NC (cd_filename,cd_varname,cd_attname,id_attvalue)
! --- SUBROUTINE MIOL_readf_attribute_S_NC (cd_filename,cd_varname,cd_attname,id_attvalue)
! --- SUBROUTINE MIOL_readf_attribute_B_NC (cd_filename,cd_varname,cd_attname,id_attvalue)
! --- SUBROUTINE MIOL_readf_attribute_C_NC (cd_filename,cd_varname,cd_attname,cd_attvalue)

! --- SUBROUTINE MIOL_readu_attribute_R8_NC (id_file_id,cd_varname,cd_attname,rd_attvalue)
! --- SUBROUTINE MIOL_readu_attribute_R4_NC (id_file_id,cd_varname,cd_attname,rd_attvalue)
! --- SUBROUTINE MIOL_readu_attribute_I_NC (id_file_id,cd_varname,cd_attname,id_attvalue)
! --- SUBROUTINE MIOL_readu_attribute_S_NC (id_file_id,cd_varname,cd_attname,id_attvalue)
! --- SUBROUTINE MIOL_readu_attribute_B_NC (id_file_id,cd_varname,cd_attname,id_attvalue)!
! --- SUBROUTINE MIOL_readu_attribute_C_NC (id_file_id,cd_varname,cd_attname,cd_attvalue)

!**------------------------------------------------------------------------------------------
!> \brief File which contain subroutine for Read of types of Attributes values in NETCDF format (R8,R4,C,INT,Short,B)
!!  Total 12 functions
!! \author C.REGNIER Miol V3.5
!! \date 01/2013
!!  \version 3.5
!<
  
         !!======================================================================
         !> \brief
         !! Description: This function reads a variable attribute or a global
         !!              attribute.
         !!
         !! @param cd_filename       A NetCDF filename. You must specify the complete path.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param rd_attvalue       The value of the attribute.
         !!
         !!! \n History :
         !!        \n 07/2007  (F. Messal) Creation
         !!        \n 01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!
         !!======================================================================
 

       SUBROUTINE MIOL_readf_attribute_R8_NC (cd_filename, &
                                              cd_varname, &
                                              cd_attname, &
                                              rd_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
        
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),           INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
         CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
         REAL(KIND=8),               INTENT(OUT) :: rd_attvalue
 
         INTEGER :: il_input_id, il_variable_id, il_status, il_type
         CHARACTER(LEN=255)                      :: cl_fonction

         cl_fonction="MIOL_readf_attribute_R8_NC"

         !------------------------------------------------------------------------
         ! open the Netcdf file
 
         il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                          NF90_NOWRITE, &
                                          il_input_id),cl_fonction)
 
         !------------------------------------------------------------------------
         ! read variable attribute
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(il_input_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_input_id, &
                                                          il_variable_id, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_DOUBLE) THEN
               il_status = fi_ncError(NF90_GET_ATT(il_input_id, &
                                                   il_variable_id, &
                                                   TRIM(cd_attname), &
                                                   rd_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readf_attribute_R8_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
         ELSE
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_input_id, &
                                                          NF90_GLOBAL, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_DOUBLE) THEN
               il_status = fi_ncError(NF90_GET_ATT(il_input_id, &
                                                   NF90_GLOBAL, &
                                                   TRIM(cd_attname), &
                                                   rd_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readf_attribute_R8_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
 
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Close file
 
         il_status = fi_ncError(NF90_CLOSE(il_input_id),cl_fonction)
 
 
         END SUBROUTINE MIOL_readf_attribute_R8_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
         !!======================================================================
         !> \brief
         !! Description: This function reads a variable attribute or a global
         !!              attribute.
         !!
         !! @param cd_filename       A NetCDF filename. You must specify the complete path.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param rd_attvalue       The value of the attribute.
         !!
         !! \n History :
         !!        \n 07/2007  (F. Messal) Creation
         !!        \n 01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
       
         SUBROUTINE MIOL_readf_attribute_R4_NC (cd_filename, &
                                              cd_varname, &
                                              cd_attname, &
                                              rd_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
     
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),           INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
         CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
         REAL(KIND=4),               INTENT(OUT) :: rd_attvalue
 
         CHARACTER(LEN=255)                      :: cl_fonction
         INTEGER :: il_input_id, il_variable_id, il_status, il_type
 
          cl_fonction="MIOL_readf_attribute_R4_NC"

         !------------------------------------------------------------------------
         ! open the Netcdf file
 
         il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                          NF90_NOWRITE, &
                                          il_input_id),cl_fonction)
 
         !------------------------------------------------------------------------
         ! read variable attribute
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(il_input_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_input_id, &
                                                          il_variable_id, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_FLOAT) THEN
               il_status = fi_ncError(NF90_GET_ATT(il_input_id, &
                                                   il_variable_id, &
                                                   TRIM(cd_attname), &
                                                   rd_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readf_attribute_R4_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
         ELSE
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_input_id, &
                                                          NF90_GLOBAL, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_FLOAT) THEN
               il_status = fi_ncError(NF90_GET_ATT(il_input_id, &
                                                   NF90_GLOBAL, &
                                                   TRIM(cd_attname), &
                                                   rd_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readf_attribute_R4_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
 
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Close file
 
         il_status = fi_ncError(NF90_CLOSE(il_input_id),cl_fonction)
 
       
        END SUBROUTINE MIOL_readf_attribute_R4_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
         
         !!======================================================================
         !> \brief
         !! Description: This function reads a variable attribute or a global
         !!              attribute.
         !!
         !! @param cd_filename       A NetCDF filename. You must specify the complete path.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param id_attvalue       The value of the attribute.
         !!
         !!       \n History :
         !!       \n 07/2007  (F. Messal) Creation
         !!       \n 01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_readf_attribute_I_NC (cd_filename, &
                                              cd_varname, &
                                              cd_attname, &
                                              id_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
          
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),           INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
         CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
         INTEGER,                    INTENT(OUT) :: id_attvalue
 
         CHARACTER(LEN=255)                      :: cl_fonction
         INTEGER :: il_input_id, il_variable_id, il_status, il_type
 
         cl_fonction="MIOL_readf_attribute_I_NC"
         
         !------------------------------------------------------------------------
         ! open the Netcdf file
 
         il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                          NF90_NOWRITE, &
                                          il_input_id),cl_fonction)
 
         !------------------------------------------------------------------------
         ! read variable attribute
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(il_input_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_input_id, &
                                                          il_variable_id, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_INT) THEN
               il_status = fi_ncError(NF90_GET_ATT(il_input_id, &
                                                   il_variable_id, &
                                                   TRIM(cd_attname), &
                                                   id_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readf_attribute_I_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
         ELSE
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_input_id, &
                                                          NF90_GLOBAL, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_INT) THEN
               il_status = fi_ncError(NF90_GET_ATT(il_input_id, &
                                                   NF90_GLOBAL, &
                                                   TRIM(cd_attname), &
                                                   id_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readf_attribute_I_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
 
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Close file
 
         il_status = fi_ncError(NF90_CLOSE(il_input_id),cl_fonction)
 
       
        END SUBROUTINE MIOL_readf_attribute_I_NC

  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
        
         !!======================================================================
         !> \brief
         !! Description: This function reads a variable attribute or a global
         !!              attribute.
         !!
         !! @param cd_filename       A NetCDF filename. You must specify the complete path.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param id_attvalue       The value of the attribute.
         !!
         !!       \n History :
         !!       \n 07/2007  (F. Messal) Creation
         !!       \n 01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!====================================================================== 
      SUBROUTINE MIOL_readf_attribute_S_NC (cd_filename, &
                                              cd_varname, &
                                              cd_attname, &
                                              id_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
        
        !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),           INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
         CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
         INTEGER(KIND=2),                    INTENT(OUT) :: id_attvalue
 
         CHARACTER(LEN=255)                      :: cl_fonction
         INTEGER :: il_input_id, il_variable_id, il_status, il_type
 
         cl_fonction="MIOL_readf_attribute_S_NC"

         !------------------------------------------------------------------------
         ! open the Netcdf file
 
         il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                          NF90_NOWRITE, &
                                          il_input_id),cl_fonction)
 
         !------------------------------------------------------------------------
         ! read variable attribute
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(il_input_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_input_id, &
                                                          il_variable_id, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_SHORT) THEN
               il_status = fi_ncError(NF90_GET_ATT(il_input_id, &
                                                   il_variable_id, &
                                                   TRIM(cd_attname), &
                                                   id_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readf_attribute_S_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
         ELSE
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_input_id, &
                                                          NF90_GLOBAL, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_SHORT) THEN
               il_status = fi_ncError(NF90_GET_ATT(il_input_id, &
                                                   NF90_GLOBAL, &
                                                   TRIM(cd_attname), &
                                                   id_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readf_attribute_S_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
 
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Close file
 
         il_status = fi_ncError(NF90_CLOSE(il_input_id),cl_fonction)
 
 
       END SUBROUTINE MIOL_readf_attribute_S_NC

  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
         
         !!======================================================================
         !> \brief 
         !! Description: This function reads a variable attribute or a global
         !!              attribute.
         !!
         !! @param cd_filename       A NetCDF filename. You must specify the complete path.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param id_attvalue       The value of the attribute.
         !!
         !!       \n History :
         !!       \n 07/2007  (F. Messal) Creation
         !!       \n 01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
#if defined Key_Byte 

       SUBROUTINE MIOL_readf_attribute_B_NC (cd_filename, &
                                              cd_varname, &
                                              cd_attname, &
                                              id_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
        
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),           INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
         CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
         INTEGER(KIND=1),                    INTENT(OUT) :: id_attvalue
 
         CHARACTER(LEN=255)                      :: cl_fonction
         INTEGER :: il_input_id, il_variable_id, il_status, il_type
 
         cl_fonction="MIOL_readf_attribute_B_NC"

         !------------------------------------------------------------------------
         ! open the Netcdf file
 
         il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                          NF90_NOWRITE, &
                                          il_input_id),cl_fonction)
 
         !------------------------------------------------------------------------
         ! read variable attribute
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(il_input_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_input_id, &
                                                          il_variable_id, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_BYTE) THEN
               il_status = fi_ncError(NF90_GET_ATT(il_input_id, &
                                                   il_variable_id, &
                                                   TRIM(cd_attname), &
                                                   id_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readf_attribute_B_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
         ELSE
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_input_id, &
                                                          NF90_GLOBAL, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_BYTE) THEN
               il_status = fi_ncError(NF90_GET_ATT(il_input_id, &
                                                   NF90_GLOBAL, &
                                                   TRIM(cd_attname), &
                                                   id_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readf_attribute_B_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
 
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Close file
 
         il_status = fi_ncError(NF90_CLOSE(il_input_id),cl_fonction)
 
  
       END SUBROUTINE MIOL_readf_attribute_B_NC
#endif 

  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
         !!======================================================================
         !> \brief 
         !! Description: This function reads a variable attribute or a global
         !!              attribute.
         !!
         !! @param cd_filename       A NetCDF filename. You must specify the complete path.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param cd_attvalue       The value of the attribute.
         !!
         !!       \n History :
         !!       \n 07/2007  (F. Messal) Creation
         !!       \n 01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_readf_attribute_C_NC (cd_filename, &
                                              cd_varname, &
                                              cd_attname, &
                                              cd_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
        !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),           INTENT( IN) :: cd_filename
         CHARACTER(LEN=*),           INTENT( IN) :: cd_varname
         CHARACTER(LEN=*),           INTENT( IN) :: cd_attname
         CHARACTER(LEN=255),         INTENT(OUT) :: cd_attvalue
 
         CHARACTER(LEN=255)                      :: cl_fonction
         INTEGER :: il_input_id, il_variable_id, il_status, il_type
 
         cl_fonction="MIOL_readf_attribute_C_NC" 

         !------------------------------------------------------------------------
         ! open the Netcdf file
 
         il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                          NF90_NOWRITE, &
                                          il_input_id),cl_fonction)
 
         !------------------------------------------------------------------------
         ! read variable attribute
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(il_input_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_input_id, &
                                                          il_variable_id, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_CHAR) THEN
               il_status = fi_ncError(NF90_GET_ATT(il_input_id, &
                                                   il_variable_id, &
                                                   TRIM(cd_attname), &
                                                   cd_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readf_attribute_C_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
         ELSE
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_input_id, &
                                                          NF90_GLOBAL, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_CHAR) THEN
               il_status = fi_ncError(NF90_GET_ATT(il_input_id, &
                                                   NF90_GLOBAL, &
                                                   TRIM(cd_attname), &
                                                   cd_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readf_attribute_C_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
 
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Close file
 
         il_status = fi_ncError(NF90_CLOSE(il_input_id),cl_fonction)
 
       END SUBROUTINE MIOL_readf_attribute_C_NC
 
  !******************************************************************************
 !******************************************************************************
 !******************************************************************************
 !******************************************************************************
  
         !!======================================================================
         !> \brief 
         !! Description: This function reads a variable attribute or a global
         !!              attribute.
         !!
         !! @param cd_filename       A NetCDF filename. You must specify the complete path.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param rd_attvalue       The value of the attribute.
         !!
         !!       \n History :
         !!       \n 07/2007  (F. Messal) Creation
         !!       \n 01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_readu_attribute_R8_NC (id_file_id, &
                                              cd_varname, &
                                              cd_attname, &
                                              rd_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,                    INTENT(IN) :: id_file_id
         CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
         CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
         REAL(KIND=8),               INTENT(OUT) :: rd_attvalue
 
         INTEGER :: il_variable_id, il_status, il_type
         CHARACTER(LEN=255)                      :: cl_fonction

         cl_fonction  = "MIOL_readu_attribute_R8_NC"

         !------------------------------------------------------------------------
         ! read variable attribute
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_variable_id, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_DOUBLE) THEN
               il_status = fi_ncError(NF90_GET_ATT(id_file_id, &
                                                   il_variable_id, &
                                                   TRIM(cd_attname), &
                                                   rd_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readu_attribute_R8_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
         ELSE
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          NF90_GLOBAL, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_DOUBLE) THEN
               il_status = fi_ncError(NF90_GET_ATT(id_file_id, &
                                                   NF90_GLOBAL, &
                                                   TRIM(cd_attname), &
                                                   rd_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readu_attribute_R8_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
 
         ENDIF
 
       END SUBROUTINE MIOL_readu_attribute_R8_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
         !!======================================================================
         !> \brief
         !! Description: This function reads a variable attribute or a global
         !!              attribute.
         !!
         !! @param id_file_id        A NetCDF file Id.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param rd_attvalue       The value of the attribute.
         !!
         !!       \n History :
         !!       \n 07/2007  (F. Messal) Creation
         !!       \n 01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_readu_attribute_R4_NC (id_file_id, &
                                              cd_varname, &
                                              cd_attname, &
                                              rd_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,                    INTENT(IN) :: id_file_id
         CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
         CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
         REAL(KIND=4),               INTENT(OUT) :: rd_attvalue
 
         INTEGER :: il_variable_id, il_status, il_type
          CHARACTER(LEN=255)                      :: cl_fonction
          
         cl_fonction="MIOL_readu_attribute_R4_NC"
          
         !------------------------------------------------------------------------
         ! read variable attribute
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_variable_id, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_FLOAT) THEN
               il_status = fi_ncError(NF90_GET_ATT(id_file_id, &
                                                   il_variable_id, &
                                                   TRIM(cd_attname), &
                                                   rd_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readu_attribute_R4_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
         ELSE
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          NF90_GLOBAL, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_FLOAT) THEN
               il_status = fi_ncError(NF90_GET_ATT(id_file_id, &
                                                   NF90_GLOBAL, &
                                                   TRIM(cd_attname), &
                                                   rd_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readu_attribute_R4_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
 
         ENDIF
 
  
        END SUBROUTINE MIOL_readu_attribute_R4_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
     
         !!======================================================================
         !> \brief
         !! Description: This function reads a variable attribute or a global
         !!              attribute.
         !!
         !! @param id_file_id        A NetCDF file Id.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param id_attvalue       The value of the attribute.
         !!
         !!       \n History :
         !!       \n 07/2007  (F. Messal) Creation
         !!       \n 01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_readu_attribute_I_NC (id_file_id, &
                                              cd_varname, &
                                              cd_attname, &
                                              id_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,                    INTENT(IN) :: id_file_id
         CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
         CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
         INTEGER,                    INTENT(OUT) :: id_attvalue
 
         INTEGER :: il_variable_id, il_status, il_type
         CHARACTER(LEN=255)                      :: cl_fonction

         cl_fonction="MIOL_readu_attribute_I_NC"

         !------------------------------------------------------------------------
         ! read variable attribute
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_variable_id, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_INT) THEN
               il_status = fi_ncError(NF90_GET_ATT(id_file_id, &
                                                   il_variable_id, &
                                                   TRIM(cd_attname), &
                                                   id_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readu_attribute_I_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
         ELSE
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          NF90_GLOBAL, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_INT) THEN
               il_status = fi_ncError(NF90_GET_ATT(id_file_id, &
                                                   NF90_GLOBAL, &
                                                   TRIM(cd_attname), &
                                                   id_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readu_attribute_I_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
 
         ENDIF
 
       END SUBROUTINE MIOL_readu_attribute_I_NC

  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
         
         !!======================================================================
         !> \brief
         !! Description: This function reads a variable attribute or a global
         !!              attribute.
         !!
         !! @param id_file_id        A NetCDF file Id.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param id_attvalue       The value of the attribute.
         !!
         !!       \n History :
         !!       \n 07/2007  (F. Messal) Creation
         !!       \n 01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_readu_attribute_S_NC (id_file_id, &
                                              cd_varname, &
                                              cd_attname, &
                                              id_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,                    INTENT(IN) :: id_file_id
         CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
         CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
         INTEGER(KIND=2),                    INTENT(OUT) :: id_attvalue
 
         INTEGER :: il_variable_id, il_status, il_type
         CHARACTER(LEN=255)                      :: cl_fonction
          
         cl_fonction="MIOL_readu_attribute_S_NC"
         
         !------------------------------------------------------------------------
         ! read variable attribute
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_variable_id, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_SHORT) THEN
               il_status = fi_ncError(NF90_GET_ATT(id_file_id, &
                                                   il_variable_id, &
                                                   TRIM(cd_attname), &
                                                   id_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readu_attribute_S_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
         ELSE
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          NF90_GLOBAL, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_SHORT) THEN
               il_status = fi_ncError(NF90_GET_ATT(id_file_id, &
                                                   NF90_GLOBAL, &
                                                   TRIM(cd_attname), &
                                                   id_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readu_attribute_S_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
 
         ENDIF
 
  
        END SUBROUTINE MIOL_readu_attribute_S_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
        
         !!======================================================================
         !> \brief
         !! Description: This function reads a variable attribute or a global
         !!              attribute.
         !!
         !! @param id_file_id        A NetCDF file Id.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param id_attvalue       The value of the attribute.
         !!
         !!       \n History :
         !!       \n 07/2007  (F. Messal) Creation
         !!       \n 01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
#if defined Key_Byte 
       SUBROUTINE MIOL_readu_attribute_B_NC (id_file_id, &
                                              cd_varname, &
                                              cd_attname, &
                                              id_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,                    INTENT(IN) :: id_file_id
         CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
         CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
         INTEGER(KIND=1),                    INTENT(OUT) :: id_attvalue
 
         INTEGER :: il_variable_id, il_status, il_type
         CHARACTER(LEN=255)                      :: cl_fonction
         
         cl_fonction="MIOL_readu_attribute_B_NC"
         
         !------------------------------------------------------------------------
         ! read variable attribute
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_variable_id, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_BYTE) THEN
               il_status = fi_ncError(NF90_GET_ATT(id_file_id, &
                                                   il_variable_id, &
                                                   TRIM(cd_attname), &
                                                   id_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readu_attribute_B_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
         ELSE
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          NF90_GLOBAL, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_BYTE) THEN
               il_status = fi_ncError(NF90_GET_ATT(id_file_id, &
                                                   NF90_GLOBAL, &
                                                   TRIM(cd_attname), &
                                                   id_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readu_attribute_B_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
 
         ENDIF
 
 
       END SUBROUTINE MIOL_readu_attribute_B_NC
 
#endif  

  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
       
         !!======================================================================
         !> \brief 
         !! Description: This function reads a variable attribute or a global
         !!              attribute.
         !!
         !! @param id_file_id        A NetCDF file Id.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param cd_attvalue       The value of the attribute.
         !!
         !!       \n History :
         !!       \n 07/2007  (F. Messal) Creation
         !!       \n 01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_readu_attribute_C_NC (id_file_id, &
                                              cd_varname, &
                                              cd_attname, &
                                              cd_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,                    INTENT( IN) :: id_file_id
         CHARACTER(LEN=*),           INTENT( IN) :: cd_varname
         CHARACTER(LEN=*),           INTENT( IN) :: cd_attname
         CHARACTER(LEN=255),         INTENT(OUT) :: cd_attvalue
 
         INTEGER :: il_variable_id, il_status, il_type
         CHARACTER(LEN=255)                      :: cl_fonction

         cl_fonction="MIOL_readu_attribute_C_NC"
 
         !------------------------------------------------------------------------
         ! read variable attribute
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_variable_id, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_CHAR) THEN
               il_status = fi_ncError(NF90_GET_ATT(id_file_id, &
                                                   il_variable_id, &
                                                   TRIM(cd_attname), &
                                                   cd_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readu_attribute_C_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
         ELSE
            il_status = fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          NF90_GLOBAL, &
                                                          TRIM(cd_attname), &
                                                          il_type),cl_fonction)
            IF (il_type .EQ. NF90_CHAR) THEN
               il_status = fi_ncError(NF90_GET_ATT(id_file_id, &
                                                   NF90_GLOBAL, &
                                                   TRIM(cd_attname), &
                                                   cd_attvalue),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_readu_attribute_C_NC : wrong type value.'
               CALL flush(0)
               STOP
            ENDIF
 
         ENDIF
 
 
       END SUBROUTINE MIOL_readu_attribute_C_NC
 
  !******************************************************************************
