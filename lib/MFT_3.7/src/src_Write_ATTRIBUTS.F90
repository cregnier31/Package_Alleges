!> \brief File which contain subroutine for Read of types of Attributes values in NETCDF format (R8,R4,C,INT,Short,B)
!!  Total 12 functions
!! \author C.REGNIER Miol V3.5
!! \date 01/2013
!!  \version 3.5
!<
 !**-----------------------------------------
!** Module for write attributs values in NETCDF format
!** C.REGNIER Janvier 2013 V3.5 Miol 
!**-----------------------------------------------------------------------------------------
! ---   SUBROUTINE MIOL_writef_attribute_R8_NC (cd_ncfilename,cd_varname,cd_attname,rd_attvalue)
! ---   SUBROUTINE MIOL_writef_attribute_R4_NC (cd_ncfilename,cd_varname,cd_attname,rd_attvalue)
! ---   SUBROUTINE MIOL_writef_attribute_I_NC (cd_ncfilename,cd_varname,cd_attname,id_attvalue)
! ---   SUBROUTINE MIOL_writef_attribute_S_NC (cd_ncfilename,cd_varname,cd_attname,id_attvalue)
! ---   SUBROUTINE MIOL_writef_attribute_B_NC (cd_ncfilename,cd_varname,cd_attname,id_attvalue)
! ---   SUBROUTINE MIOL_writef_attribute_C_NC (cd_filename,cd_varname,cd_attname,cd_attvalue)
! ---   SUBROUTINE MIOL_writeu_attribute_R8_NC (id_file_id,cd_varname,cd_attname,rd_attvalue)
! ---   SUBROUTINE MIOL_writeu_attribute_R4_NC (id_file_id,cd_varname,cd_attname,rd_attvalue)
! ---   SUBROUTINE MIOL_writeu_attribute_I_NC (id_file_id,cd_varname,cd_attname,id_attvalue)
! ---   SUBROUTINE MIOL_writeu_attribute_S_NC (id_file_id,cd_varname,cd_attname, id_attvalue) 
! ---   SUBROUTINE MIOL_writeu_attribute_B_NC (id_file_id,cd_varname,cd_attname,id_attvalue)
! ---   SUBROUTINE MIOL_writeu_attribute_C_NC (id_file_id,cd_varname,cd_attname,cd_filename,cd_attvalue)
!**-----------------------------------------------------------------------------------------
 

         !!======================================================================
         !> \brief
         !! Description: This function writes a variable attribute or a global
         !!              attribute.
         !!
         !! @param cd_filename       A NetCDF filename. You must specify the complete path.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param  cd_attvalue       The value of the attribute.
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version V1.0
         !!        \n  01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
  

       SUBROUTINE MIOL_writef_attribute_R8_NC (cd_ncfilename, &
                                               cd_varname, &
                                               cd_attname, &
                                               rd_attvalue)
 
         USE MFT_error
         USE netcdf
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*), INTENT(IN) :: cd_ncfilename
         CHARACTER(LEN=*), INTENT(IN) :: cd_varname
         CHARACTER(LEN=*), INTENT(IN) :: cd_attname
         REAL(KIND=8),       INTENT(IN) :: rd_attvalue
 
         INTEGER :: il_input_id, il_variable_id, il_status
         CHARACTER(LEN=255) :: cl_fonction 
      
         cl_fonction="MIOL_writef_attribute_R8_NC"
         !------------------------------------------------------------------------
         ! Open the Netcdf file
 
         il_status = fi_ncError(NF90_OPEN(cd_ncfilename, &
                                          NF90_WRITE, &
                                          il_input_id),cl_fonction)
 
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(il_input_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Define mode
 
         il_status = fi_ncError(NF90_REDEF(il_input_id),cl_fonction)
 
 
         !------------------------------------------------------------------------
         ! Write variable attribute
 
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                il_variable_id, &
                                                TRIM(cd_attname), &
                                                rd_attvalue),cl_fonction)
 
         ELSE
            il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                NF90_GLOBAL, &
                                                TRIM(cd_attname), &
                                                rd_attvalue),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(il_input_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Close ncfile
 
         il_status = fi_ncError(NF90_CLOSE(il_input_id),cl_fonction)
 
       END SUBROUTINE MIOL_writef_attribute_R8_NC
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
 
       SUBROUTINE MIOL_writef_attribute_R4_NC (cd_ncfilename, &
                                               cd_varname, &
                                               cd_attname, &
                                               rd_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !!======================================================================
         !> \brief
         !! Description: This function writes a variable attribute or a global
         !!              attribute.
         !!
         !! @param cd_filename       A NetCDF filename. You must specify the complete path.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param cd_attvalue     The value of the attribute.
         !!
         !! History :
         !!        \n 06/2006  (F. Messal) Creation
         !!        \n 11/2006  (F. Messal) CVS version V1.0
         !!        \n 01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
 
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*), INTENT(IN) :: cd_ncfilename
         CHARACTER(LEN=*), INTENT(IN) :: cd_varname
         CHARACTER(LEN=*), INTENT(IN) :: cd_attname
         REAL(KIND=4),       INTENT(IN) :: rd_attvalue
 
         INTEGER :: il_input_id, il_variable_id, il_status
         CHARACTER(LEN=255) :: cl_fonction 
      
         cl_fonction="MIOL_writef_attribute_R4_NC"

         !------------------------------------------------------------------------
         ! Open the Netcdf file
 
         il_status = fi_ncError(NF90_OPEN(cd_ncfilename, &
                                          NF90_WRITE, &
                                          il_input_id),cl_fonction)
 
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(il_input_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Define mode
 
         il_status = fi_ncError(NF90_REDEF(il_input_id),cl_fonction)
 
 
         !------------------------------------------------------------------------
         ! Write variable attribute
 
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                il_variable_id, &
                                                TRIM(cd_attname), &
                                                rd_attvalue),cl_fonction)
 
         ELSE
            il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                NF90_GLOBAL, &
                                                TRIM(cd_attname), &
                                                rd_attvalue),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(il_input_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Close ncfile
 
         il_status = fi_ncError(NF90_CLOSE(il_input_id),cl_fonction)
 
       END SUBROUTINE MIOL_writef_attribute_R4_NC
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
         !!======================================================================
         !> \brief
         !! Description: This function writes a variable attribute or a global
         !!              attribute.
         !!
         !! @param cd_filename       A NetCDF filename. You must specify the complete path.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param cd_attvalue       The value of the attribute.
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version V1.0
         !!        \n  01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_writef_attribute_I_NC (cd_ncfilename, &
                                               cd_varname, &
                                               cd_attname, &
                                               id_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
        
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=255), INTENT(IN) :: cd_ncfilename
         CHARACTER(LEN=255), INTENT(IN) :: cd_varname
         CHARACTER(LEN=255), INTENT(IN) :: cd_attname
         INTEGER,            INTENT(IN) :: id_attvalue
 
         INTEGER :: il_input_id, il_variable_id, il_status
         CHARACTER(LEN=255) :: cl_fonction 
      
         cl_fonction="MIOL_writef_attribute_I_NC"

         !------------------------------------------------------------------------
         ! Open the Netcdf file
 
         il_status = fi_ncError(NF90_OPEN(cd_ncfilename, &
                                          NF90_WRITE, &
                                          il_input_id),cl_fonction)
 
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(il_input_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Define mode
 
         il_status = fi_ncError(NF90_REDEF(il_input_id),cl_fonction)
 
 
         !------------------------------------------------------------------------
         ! Write variable attribute
 
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                il_variable_id, &
                                                TRIM(cd_attname), &
                                                id_attvalue),cl_fonction)
 
         ELSE
            il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                NF90_GLOBAL, &
                                                TRIM(cd_attname), &
                                                id_attvalue),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(il_input_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Close ncfile
 
         il_status = fi_ncError(NF90_CLOSE(il_input_id),cl_fonction)
  
 
       END SUBROUTINE MIOL_writef_attribute_I_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
         !!======================================================================
         !> \brief
         !! Description: This function writes a variable attribute or a global
         !!              attribute.
         !!
         !! @param cd_filename       A NetCDF filename. You must specify the complete path.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param cd_attvalue       The value of the attribute.
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version V1.0
         !!        \n  01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
       SUBROUTINE MIOL_writef_attribute_S_NC (cd_ncfilename, &
                                               cd_varname, &
                                               cd_attname, &
                                               id_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
     
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=255), INTENT(IN) :: cd_ncfilename
         CHARACTER(LEN=255), INTENT(IN) :: cd_varname
         CHARACTER(LEN=255), INTENT(IN) :: cd_attname
         INTEGER(KIND=2),    INTENT(IN) :: id_attvalue
 
         INTEGER :: il_input_id, il_variable_id, il_status
         CHARACTER(LEN=255) :: cl_fonction 
      
         cl_fonction="MIOL_writef_attribute_S_NC"

         !------------------------------------------------------------------------
         ! Open the Netcdf file
 
         il_status = fi_ncError(NF90_OPEN(cd_ncfilename, &
                                          NF90_WRITE, &
                                          il_input_id),cl_fonction)
 
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(il_input_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Define mode
 
         il_status = fi_ncError(NF90_REDEF(il_input_id),cl_fonction)
 
 
         !------------------------------------------------------------------------
         ! Write variable attribute
 
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                il_variable_id, &
                                                TRIM(cd_attname), &
                                                id_attvalue),cl_fonction)
 
         ELSE
            il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                NF90_GLOBAL, &
                                                TRIM(cd_attname), &
                                                id_attvalue),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(il_input_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Close ncfile
 
         il_status = fi_ncError(NF90_CLOSE(il_input_id),cl_fonction)
 
 
       END SUBROUTINE MIOL_writef_attribute_S_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
         !!======================================================================
         !> \brief
         !! Description: This function writes a variable attribute or a global
         !!              attribute.
         !!
         !! @param cd_filename       A NetCDF filename. You must specify the complete path.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param cd_attvalue       The value of the attribute.
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version V1.0
         !!        \n  01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================

#if defined Key_Byte 

       SUBROUTINE MIOL_writef_attribute_B_NC (cd_ncfilename, &
                                               cd_varname, &
                                               cd_attname, &
                                               id_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
         
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=255), INTENT(IN) :: cd_ncfilename
         CHARACTER(LEN=255), INTENT(IN) :: cd_varname
         CHARACTER(LEN=255), INTENT(IN) :: cd_attname
         INTEGER(KIND=1),    INTENT(IN) :: id_attvalue
 
         INTEGER :: il_input_id, il_variable_id, il_status
         CHARACTER(LEN=255) :: cl_fonction 
      
         cl_fonction="MIOL_writef_attribute_B_NC"

         !------------------------------------------------------------------------
         ! Open the Netcdf file
 
         il_status = fi_ncError(NF90_OPEN(cd_ncfilename, &
                                          NF90_WRITE, &
                                          il_input_id),cl_fonction)
 
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(il_input_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Define mode
 
         il_status = fi_ncError(NF90_REDEF(il_input_id),cl_fonction)
 
 
         !------------------------------------------------------------------------
         ! Write variable attribute
 
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                il_variable_id, &
                                                TRIM(cd_attname), &
                                                id_attvalue),cl_fonction)
 
         ELSE
            il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                NF90_GLOBAL, &
                                                TRIM(cd_attname), &
                                                id_attvalue),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(il_input_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Close ncfile
 
         il_status = fi_ncError(NF90_CLOSE(il_input_id),cl_fonction)
 
          
        END SUBROUTINE MIOL_writef_attribute_B_NC
#endif  

  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
         !!======================================================================
         !> \brief
         !! Description: This function writes a variable attribute or a global
         !!              attribute.
         !!
         !! @param cd_filename       A NetCDF filename. You must specify the complete path.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param cd_attvalue     The value of the attribute.
         !!
         !! You can use "key" to write default attribute value. The default
         !! attribute names and values are:
         !!  - file_name : the Netcdf filename
         !!  - institution : "SC MERCATOR OCEAN"
         !!  - references : "http://www.mercator-ocean.fr"
         !!  - history : "2006/07/12 18:53:02 MERCATOR OCEAN Netcdf creation"
         !!
         !! The "key" names attribute are:
         !!         name     :    key      :       value
         !!     file_name    : filename    : the Netcdf filename
         !!     institution  : institution : "SC MERCATOR OCEAN"
         !!     references   : references  : http://www.mercator-ocean.fr
         !!     history      : history     : the date and hour of the file creation +
         !!                  :             :    "MERCATOR OCEAN Netcdf creation"
         !!     date         : date        : the date and hour of the file creation
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version V1.0
         !!        \n  01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_writef_attribute_C_NC (cd_filename, &
                                               cd_varname, &
                                               cd_attname, &
                                               cd_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=255),           INTENT(IN) :: cd_filename
         CHARACTER(LEN=255),           INTENT(IN) :: cd_varname
         CHARACTER(LEN=255),           INTENT(IN) :: cd_attname
         CHARACTER(LEN=255), OPTIONAL, INTENT(IN) :: cd_attvalue
 
         INTEGER :: il_input_id, il_variable_id, il_status
 
         CHARACTER(LEN=255) :: cl_history, &
                               cl_creationdate, &
                               cl_institution, &
                               cl_references, &
                               cl_filename
         CHARACTER(LEN=8) :: cl_date
         CHARACTER(LEN=10) :: cl_time
         CHARACTER(LEN=255) :: cl_fonction 
      
         cl_fonction="MIOL_writef_attribute_C_NC"

         !-----------------------------------------------------------------------
 
         CALL DATE_AND_TIME(cl_date, cl_time)
         cl_institution = 'SC MERCATOR OCEAN'
         cl_references = 'http://www.mercator-ocean.fr'
         cl_filename = cd_filename(INDEX(cd_filename, "/", .true.):255)
         cl_creationdate = cl_date(1:4)//'/'//cl_date(5:6)//'/'//cl_date(7:8)// &
            ' '//cl_time(1:2)//':'//cl_time(3:4)//':'//cl_time(5:6)
         cl_history = cl_creationdate//' MERCATOR OCEAN Netcdf creation '
 
         !------------------------------------------------------------------------
         ! open the Netcdf file
 
         il_status = fi_ncError(NF90_OPEN(cd_filename, &
                                          NF90_WRITE, &
                                          il_input_id),cl_fonction)
 
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(il_input_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! define mode
 
         il_status = fi_ncError(NF90_REDEF(il_input_id),cl_fonction)
 
 
         !------------------------------------------------------------------------
         ! write variable attribute
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
 
            IF (PRESENT(cd_attvalue)) THEN
               il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                   il_variable_id, &
                                                   TRIM(cd_attname), &
                                                   TRIM(cd_attvalue)),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_write_attribute_C_NC: value attribute not defined'
            ENDIF
 
         ELSE
            IF (PRESENT(cd_attvalue)) THEN
               il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                   NF90_GLOBAL, &
                                                   TRIM(cd_attname), &
                                                   TRIM(cd_attvalue)),cl_fonction)
 
            ELSE
 
               SELECT CASE (TRIM(cd_attname))
 
                  CASE ('filename')
                     il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                         NF90_GLOBAL, &
                                                         cd_attname, &
                                                         TRIM(cl_filename)),cl_fonction)
 
                  CASE ('date')
                     il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                         NF90_GLOBAL, &
                                                         cd_attname, &
                                                         TRIM(cl_creationdate)),cl_fonction)
 
                  CASE ('history')
                     il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                         NF90_GLOBAL, &
                                                         cd_attname, &
                                                         TRIM(cl_history)),cl_fonction)
 
                  CASE ('institution')
                     il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                         NF90_GLOBAL, &
                                                         cd_attname, &
                                                         TRIM(cl_institution)),cl_fonction)
 
                  CASE ('references')
                     il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                         NF90_GLOBAL, &
                                                         cd_attname, &
                                                         TRIM(cl_references)),cl_fonction)
 
                  CASE DEFAULT
                     WRITE(0,*) ' MIOL_write_attribute_C_NC: attribute ', &
                                TRIM(cd_attname), ' : value not defined'
                     il_status = fi_ncError(NF90_PUT_ATT(il_input_id, &
                                                         NF90_GLOBAL, &
                                                         cd_attname, &
                                                         ' '),cl_fonction)
 
 
               ENDSELECT
 
            ENDIF
 
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(il_input_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Close file
 
         il_status = fi_ncError(NF90_CLOSE(il_input_id),cl_fonction)
 
       END SUBROUTINE MIOL_writef_attribute_C_NC
 
 
  !******************************************************************************
  !******************************************************************************
    
         !!======================================================================
         !> \brief
         !! Description: This function writes a variable attribute or a global
         !!              attribute.
         !!
         !! @param id_file_id        A NetCDF file Id.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param cd_attvalue       The value of the attribute.
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version V1.0
         !!        \n  01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
   
       SUBROUTINE MIOL_writeu_attribute_R8_NC (id_file_id, &
                                               cd_varname, &
                                               cd_attname, &
                                               rd_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
     
         !-----------------------------------------------------------------------
 
         INTEGER,            INTENT(IN) :: id_file_id
         CHARACTER(LEN=*), INTENT(IN) :: cd_varname
         CHARACTER(LEN=*), INTENT(IN) :: cd_attname
         REAL(KIND=8),       INTENT(IN) :: rd_attvalue
 
         INTEGER :: il_variable_id, il_status
         CHARACTER(LEN=255) :: cl_fonction 
      
         cl_fonction="MIOL_writeu_attribute_R8_NC"
     
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Define mode
 
         il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)
 
 
         !------------------------------------------------------------------------
         ! Write variable attribute
 
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                il_variable_id, &
                                                TRIM(cd_attname), &
                                                rd_attvalue),cl_fonction)
 
         ELSE
            il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                NF90_GLOBAL, &
                                                TRIM(cd_attname), &
                                                rd_attvalue),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(id_file_id),cl_fonction)
 
 
       END SUBROUTINE MIOL_writeu_attribute_R8_NC
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
         !!======================================================================
         !> \brief
         !! Description: This function writes a variable attribute or a global
         !!              attribute.
         !!
         !! @param id_file_id        A NetCDF file Id.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param cd_attvalue     The value of the attribute.
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version V1.0
         !!        \n  01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
  
       SUBROUTINE MIOL_writeu_attribute_R4_NC (id_file_id, &
                                                cd_varname, &
                                                cd_attname, &
                                                rd_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,            INTENT(IN) :: id_file_id
         CHARACTER(LEN=*), INTENT(IN) :: cd_varname
         CHARACTER(LEN=*), INTENT(IN) :: cd_attname
         REAL(KIND=4),       INTENT(IN) :: rd_attvalue
 
         INTEGER :: il_variable_id, il_status
         CHARACTER(LEN=255) :: cl_fonction 
      
         cl_fonction="MIOL_writeu_attribute_R4_NC"

         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Define mode
 
         il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)
 
 
         !------------------------------------------------------------------------
         ! Write variable attribute
 
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                il_variable_id, &
                                                TRIM(cd_attname), &
                                                rd_attvalue),cl_fonction)
 
         ELSE
           il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                NF90_GLOBAL, &
                                                TRIM(cd_attname), &
                                                rd_attvalue),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(id_file_id),cl_fonction)
 
 
       END SUBROUTINE MIOL_writeu_attribute_R4_NC
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
         !!======================================================================
         !> \brief
         !! Description: This function writes a variable attribute or a global
         !!              attribute.
         !!
         !! @param id_file_id        A NetCDF file Id.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param cd_attvalue       The value of the attribute.
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version V1.0
         !!        \n  01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_writeu_attribute_I_NC (id_file_id, &
                                               cd_varname, &
                                               cd_attname, &
                                               id_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,            INTENT(IN) :: id_file_id
         CHARACTER(LEN=255), INTENT(IN) :: cd_varname
         CHARACTER(LEN=255), INTENT(IN) :: cd_attname
         INTEGER,            INTENT(IN) :: id_attvalue
 
         INTEGER :: il_variable_id, il_status
         CHARACTER(LEN=255) :: cl_fonction 
      
         cl_fonction="MIOL_writeu_attribute_I_NC"
         
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Define mode
 
         il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)
 
 
         !------------------------------------------------------------------------
         ! Write variable attribute
 
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                il_variable_id, &
                                                TRIM(cd_attname), &
                                                id_attvalue),cl_fonction)
 
         ELSE
            il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                NF90_GLOBAL, &
                                                TRIM(cd_attname), &
                                                id_attvalue),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(id_file_id),cl_fonction)
 
 
       END SUBROUTINE MIOL_writeu_attribute_I_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
       
         !!======================================================================
         !> \brief
         !! Description: This function writes a variable attribute or a global
         !!              attribute.
         !!
         !! @param id_file_id        A NetCDF file Id.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param cd_attvalue       The value of the attribute.
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version V1.0
         !!        \n  01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_writeu_attribute_S_NC (id_file_id, &
                                               cd_varname, &
                                               cd_attname, &
                                               id_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
       
          !-----------------------------------------------------------------------
 
         INTEGER,            INTENT(IN) :: id_file_id
         CHARACTER(LEN=255), INTENT(IN) :: cd_varname
         CHARACTER(LEN=255), INTENT(IN) :: cd_attname
         INTEGER(KIND=2),    INTENT(IN) :: id_attvalue
 
         INTEGER :: il_variable_id, il_status
         CHARACTER(LEN=255) :: cl_fonction 
      
         cl_fonction="MIOL_writeu_attribute_S_NC"
         
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Define mode
 
         il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)
 
 
         !------------------------------------------------------------------------
         ! Write variable attribute
 
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                il_variable_id, &
                                                TRIM(cd_attname), &
                                                id_attvalue),cl_fonction)
 
         ELSE
            il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                NF90_GLOBAL, &
                                                TRIM(cd_attname), &
                                                id_attvalue),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(id_file_id),cl_fonction)
 
         
        END SUBROUTINE MIOL_writeu_attribute_S_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
         !!======================================================================
         !> \brief
         !! Description: This function writes a variable attribute or a global
         !!              attribute.
         !!
         !! @param id_file_id        A NetCDF file Id.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param cd_attvalue       The value of the attribute.
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version V1.0
         !!        \n  01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
#if defined Key_Byte 

       SUBROUTINE MIOL_writeu_attribute_B_NC (id_file_id, &
                                               cd_varname, &
                                               cd_attname, &
                                               id_attvalue)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,            INTENT(IN) :: id_file_id
         CHARACTER(LEN=255), INTENT(IN) :: cd_varname
         CHARACTER(LEN=255), INTENT(IN) :: cd_attname
         INTEGER(KIND=1),    INTENT(IN) :: id_attvalue
 
         INTEGER :: il_variable_id, il_status
         CHARACTER(LEN=255) :: cl_fonction 
      
         cl_fonction="MIOL_writeu_attribute_B_NC"
         
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Define mode
 
         il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)
 
 
         !------------------------------------------------------------------------
         ! Write variable attribute
 
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                il_variable_id, &
                                                TRIM(cd_attname), &
                                                id_attvalue),cl_fonction)
 
         ELSE
            il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                NF90_GLOBAL, &
                                                TRIM(cd_attname), &
                                                id_attvalue),cl_fonction)
 
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(id_file_id),cl_fonction)
 
 
       END SUBROUTINE MIOL_writeu_attribute_B_NC

#endif  

  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
         !!======================================================================
         !> \brief
         !! Description: This function writes a variable attribute or a global
         !!              attribute.
         !!
         !! @param id_file_id       A NetCDF file Id.
         !! @param cd_varname        The variable name or 'global' for global attribute.
         !! @param cd_attname        The attribute name.
         !! @param cd_attvalue     The value of the attribute.
         !!
         !! You can use "key" to write default attribute value. The default
         !! attribute names and values are:
         !!  - file_name : the Netcdf filename
         !!  - institution : "SC MERCATOR OCEAN"
         !!  - references : "http://www.mercator-ocean.fr"
         !!  - history : "2006/07/12 18:53:02 MERCATOR OCEAN Netcdf creation"
         !!
         !! The "key" names attribute are:
         !!         name     :    key      :       value
         !!     file_name    : filename    : the Netcdf filename
         !!     institution  : institution : "SC MERCATOR OCEAN"
         !!     references   : references  : http://www.mercator-ocean.fr
         !!     history      : history     : the date and hour of the file creation +
         !!                  :             :    "MERCATOR OCEAN Netcdf creation"
         !!     date         : date        : the date and hour of the file creation
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version V1.0
         !!        \n  01/2013  (C.Regnier) SVN V3.5.0 add error function
         !<
         !!======================================================================
  
       SUBROUTINE MIOL_writeu_attribute_C_NC (id_file_id, &
                                               cd_varname, &
                                               cd_attname, &
                                               cd_attvalue)
                                             !  cd_filename, &
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,                      INTENT(IN) :: id_file_id
         CHARACTER(LEN=255),           INTENT(IN) :: cd_varname
         CHARACTER(LEN=255),           INTENT(IN) :: cd_attname
         !CHARACTER(LEN=255),           INTENT(IN) :: cd_filename
         CHARACTER(LEN=255), OPTIONAL, INTENT(IN) :: cd_attvalue
 
         INTEGER :: il_variable_id, il_status
 
         CHARACTER(LEN=255) :: cl_history, &
                               cl_creationdate, &
                               cl_institution, &
                               cl_references, &
                               cl_filename
         CHARACTER(LEN=8) :: cl_date
         CHARACTER(LEN=10) :: cl_time
         CHARACTER(LEN=255) :: cl_fonction 
      
         cl_fonction="MIOL_writeu_attribute_C_NC"
         !-----------------------------------------------------------------------
 
         CALL  DATE_AND_TIME(cl_date, cl_time)
         cl_institution = 'SC MERCATOR OCEAN'
         cl_references = 'http://www.mercator-ocean.fr'
         cl_filename ='' ! cd_filename(INDEX(cd_filename, "/", .true.):255)
         cl_creationdate = cl_date(1:4)//'/'//cl_date(5:6)//'/'//cl_date(7:8)// &
            ' '//cl_time(1:2)//':'//cl_time(3:4)//':'//cl_time(5:6)
         cl_history = cl_creationdate//' MERCATOR OCEAN Netcdf creation '
 
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
            il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                  TRIM(cd_varname), &
                                                  il_variable_id),cl_fonction)
         ENDIF
 
 
         !-----------------------------------------------------------------------
         ! define mode
 
         il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)
 
 
         !------------------------------------------------------------------------
         ! write variable attribute
         IF ((TRIM(cd_varname) .NE. 'global') .AND. &
             (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
 
            IF (PRESENT(cd_attvalue)) THEN
               il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                   il_variable_id, &
                                                   TRIM(cd_attname), &
                                                   TRIM(cd_attvalue)),cl_fonction)
            ELSE
               WRITE(0,*) ' MIOL_write_attribute_C_NC: value attribute not defined'
            ENDIF
 
         ELSE
            IF (PRESENT(cd_attvalue)) THEN
               il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                   NF90_GLOBAL, &
                                                   TRIM(cd_attname), &
                                                   TRIM(cd_attvalue)),cl_fonction)
 
            ELSE
 
               SELECT CASE (TRIM(cd_attname))
 
                  CASE ('filename')
                     il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                         NF90_GLOBAL, &
                                                         cd_attname, &
                                                         TRIM(cl_filename)),cl_fonction)
 
                  CASE ('date')
                     il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                         NF90_GLOBAL, &
                                                         cd_attname, &
                                                         TRIM(cl_creationdate)),cl_fonction)
 
                  CASE ('history')
                     il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                         NF90_GLOBAL, &
                                                         cd_attname, &
                                                         TRIM(cl_history)),cl_fonction)
 
                  CASE ('institution')
                     il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                         NF90_GLOBAL, &
                                                         cd_attname, &
                                                         TRIM(cl_institution)),cl_fonction)
 
                  CASE ('references')
                     il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                         NF90_GLOBAL, &
                                                         cd_attname, &
                                                         TRIM(cl_references)),cl_fonction)
 
                  CASE DEFAULT
                     WRITE(0,*) ' MIOL_write_attribute_C_NC: attribute ', &
                                TRIM(cd_attname), ' : value not defined'
                     il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                         NF90_GLOBAL, &
                                                         cd_attname, &
                                                         ' '),cl_fonction)
 
 
               ENDSELECT
 
            ENDIF
 
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(id_file_id),cl_fonction)
 
 
      END SUBROUTINE MIOL_writeu_attribute_C_NC
 
  !******************************************************************************
  !******************************************************************************
!******************************************************************************
