!MODULE src_write_ATTLIST
!**-----------------------------------------
!** Module for write an attribute list in NCDF format
!** C.REGNIER Janvier 2013 V3.5 Miol 
!**-------------------------------------------
  ! ---   interface MIOL_write_attributeslist_NC
  ! ---  SUBROUTINE MIOL_writef_attributeslist_NC (cd_filename,cd_varname,id_nbatt,cda_attname,cda_attvalue,rda_attvalue,ida_typevalue)
  ! ---  SUBROUTINE MIOL_writeu_attributeslist_NC (id_file_id,cd_varname,id_nbatt,cda_attname,cda_attvalue,rda_attvalue,ida_typevalue)
  !----------------------------------------------------------------
  !******************************************************************************
 !-------------------------------------------
!> \brief File which contain subroutine for write attributes list in NETCDF format 
!! \n  Total 2 functions
!! \author F.MESSAL first version
!! \date 11/2006
!! \version 1.1
!! \author C.REGNIER Miol V3.5
!! \date 01/2013
!! \version 3.5
!<
 
          !!=====================================================================
          !> \brief
          !! Description: This function puts values of variable or global attributes.
          !!              This function uses pointers, so you don’t have to know
          !!              the correct dimensions of the data arrays. If you use
          !!              also the netcdf library, you can compare values of
          !!              the “typevalue” array to the set of predefined netCDF
          !!              external data types: NF90_BYTE, NF90_CHAR, NF90_SHORT,
          !!              NF90_INT, NF90_FLOAT, NF90_DOUBLE.
          !!
          !!  @param id_file_id        A NetCDF fileId.
          !!  @param cd_varname        The name of the variable to be read, or 'global'
          !!                     for global attributes.
          !! @param id_nbatt          The number of attributes.
          !! @param cda_attname      Vector of with the attribute names.
          !! @param cda_attcvalue    Vector of NF90_CHAR values if the attribute
          !!                     type is NF90_CHAR else the value is ''.
          !! @param rda_attrvalue    Vector of NF90_FLOAT values if the attribute
          !!                     type is NF90_FLOAT else the value is 0.
          !! @param ida_typevalue    Vector of type values (2 = NF90_CHAR ;
          !!                     5 = NF90_FLOAT)
          !!
          !! History :
          !!        \n  07/2007  (F. Messal)
          !!        \n  01/2013  (C.REGNIER) SVN V3.5.0
          !<
          !!=====================================================================
      
        SUBROUTINE MIOL_writeu_attributeslist_NC (id_file_id, &
                                                  cd_varname, &
                                                  id_nbatt, &
                                                  cda_attname, &
                                                  cda_attvalue, &
                                                  rda_attvalue, &
                                                  ida_typevalue)
 
          USE MFT_error
          USE netcdf
          IMPLICIT NONE

  
         !----------------------------------------------------------------------
          INTEGER,                          INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
          INTEGER,                          INTENT(IN) :: id_nbatt
          CHARACTER(LEN=255), DIMENSION(:) ,INTENT(IN)            :: cda_attname
          CHARACTER(LEN=255), DIMENSION(:) ,INTENT(IN)             :: cda_attvalue
 
          REAL(KIND=4), DIMENSION(:),         OPTIONAL,INTENT(IN)  :: rda_attvalue
          INTEGER, DIMENSION(:),              OPTIONAL,INTENT(IN)  :: ida_typevalue
 
          INTEGER :: il_var_id, &
                     il_ji, il_status
          CHARACTER(LEN=60) :: cl_fonction
 
          cl_fonction="MIOL_writeu_attributeslist_NC"

          il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)

          !----------------------------------------------------------------------
          ! Writes attributes
          IF ((TRIM(cd_varname) .NE. 'global') .AND. &
              (TRIM(cd_varname) .NE. 'GLOBAL')) THEN
             il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                   TRIM(cd_varname), &
                                                   il_var_id),cl_fonction)
 
             IF (PRESENT(ida_typevalue) .AND. &
                 PRESENT(rda_attvalue)) THEN
 
                DO il_ji=1, id_nbatt
 
                   SELECT CASE (ida_typevalue(il_ji))
 
                      CASE (2)
                         il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                             il_var_id, &
                                                             cda_attname(il_ji), &
                                                             cda_attvalue(il_ji)),cl_fonction)
 
                      CASE (5)
                         il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                             il_var_id, &
                                                             cda_attname(il_ji), &
                                                             rda_attvalue(il_ji)),cl_fonction)
 
                      CASE DEFAULT
                         WRITE(0,*) ' MIOL_writeu_attributeslist_NC : undefined type.'
                         CALL flush(0)
                         STOP
 
 
                   END SELECT
 
                ENDDO
 
             ELSE
 
                DO il_ji=1, id_nbatt
 
                   il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                       il_var_id, &
                                                       cda_attname(il_ji), &
                                                       cda_attvalue(il_ji)),cl_fonction)
 
                ENDDO
 
             ENDIF
 
          ELSE
 
             IF (PRESENT(ida_typevalue) .AND. &
                 PRESENT(rda_attvalue)) THEN
 
                DO il_ji=1, id_nbatt
 
                   SELECT CASE (ida_typevalue(il_ji))
 
                      CASE (2)
                         il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                             NF90_GLOBAL, &
                                                             cda_attname(il_ji), &
                                                             cda_attvalue(il_ji)),cl_fonction)
 
                      CASE (5)
                         il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                             NF90_GLOBAL, &
                                                             cda_attname(il_ji), &
                                                             rda_attvalue(il_ji)),cl_fonction)
 
                      CASE DEFAULT
                         WRITE(0,*) ' MIOL_writeu_attributeslist_NC : undefined type.'
                         CALL flush(0)
                         STOP
 
 
                   END SELECT
 
                ENDDO
 
             ELSE
 
                DO il_ji=1, id_nbatt
 
                   il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                       NF90_GLOBAL, &
                                                       cda_attname(il_ji), &
                                                       cda_attvalue(il_ji)),cl_fonction)
 
                ENDDO
 
             ENDIF
 
          ENDIF
          
          !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(id_file_id),cl_fonction)

        END SUBROUTINE MIOL_writeu_attributeslist_NC
 
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
          !!=====================================================================
          !> \brief
          !! Description: This function puts values of variable or global attributes.
          !!              This function uses pointers, so you don’t have to know
          !!              the correct dimensions of the data arrays. If you use
          !!              also the netcdf library, you can compare values of
          !!              the “typevalue” array to the set of predefined netCDF
          !!              external data types: NF90_BYTE, NF90_CHAR, NF90_SHORT,
          !!              NF90_INT, NF90_FLOAT, NF90_DOUBLE.
          !!
          !!  @param cd_filename       A NetCDF filename. You must specify the complete path.
          !!  @param cd_varname        The name of the variable to be read, or 'global'
          !!                     for global attributes.
          !!  @param id_nbatt          The number of attributes.
          !!  @param cda_attname      Vector of with the attribute names.
          !!  @param cda_attcvalue    Vector of NF90_CHAR values if the attribute
          !!                     type is NF90_CHAR else the value is ''.
          !!  @param rda_attrvalue    Vector of NF90_FLOAT values if the attribute
          !!                     type is NF90_FLOAT else the value is 0.
          !!  @param ida_typevalue    Vector of type values (2 = NF90_CHAR ;
          !!                     5 = NF90_FLOAT)
          !!
          !! History :
          !!        \n  07/2007  (F. Messal)
          !!        \n  01/2013  (C.REGNIER) SVN V3.5.0
          !<
          !!=====================================================================
 
          SUBROUTINE MIOL_writef_attributeslist_NC (cd_filename, &
                                                  cd_varname, &
                                                  id_nbatt, &
                                                  cda_attname, &
                                                  cda_attvalue, &
                                                  rda_attvalue, &
                                                  ida_typevalue)
 
          USE netcdf
          USE MFT_error
          IMPLICIT NONE
 
    
          !----------------------------------------------------------------------
          CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
          INTEGER,                          INTENT(IN) :: id_nbatt
          CHARACTER(LEN=255), DIMENSION(:)             :: cda_attname
          CHARACTER(LEN=255), DIMENSION(:)             :: cda_attvalue
          REAL(KIND=4), DIMENSION(:),         OPTIONAL :: rda_attvalue
          INTEGER, DIMENSION(:),              OPTIONAL :: ida_typevalue
 
          INTEGER :: il_file_id, il_var_id, &
                     il_ji, il_status
          CHARACTER(LEN=60) :: cl_fonction

           cl_fonction="MIOL_writef_attributeslist_NC"

          !----------------------------------------------------------------------
          ! Open file
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_WRITE, &
                                           il_file_id),cl_fonction)
 
          il_status = fi_ncError(NF90_REDEF(il_file_id),cl_fonction)
          !----------------------------------------------------------------------
          ! Writes attributes
          IF ((TRIM(cd_varname) .NE. 'global') .AND. &
              (TRIM(cd_varname) .NE. 'GLOBAL')) THEN
 
             il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                   TRIM(cd_varname), &
                                                   il_var_id),cl_fonction)
 
             IF (PRESENT(ida_typevalue) .AND. &
                 PRESENT(rda_attvalue)) THEN
 
                DO il_ji=1, id_nbatt
 
                   SELECT CASE (ida_typevalue(il_ji))
 
                      CASE (2)
                         il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                             il_var_id, &
                                                             cda_attname(il_ji), &
                                                             cda_attvalue(il_ji)),cl_fonction)
 
                      CASE (5)
                         il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                             il_var_id, &
                                                             cda_attname(il_ji), &
                                                             rda_attvalue(il_ji)),cl_fonction)
 
                      CASE DEFAULT
                         WRITE(0,*) ' MIOL_writef_attributeslist_NC : undefined type.'
                         CALL flush(0)
                         STOP
 
 
                   END SELECT
 
                ENDDO
 
             ELSE
 
                DO il_ji=1, id_nbatt
 
                   il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                       il_var_id, &
                                                       cda_attname(il_ji), &
                                                       cda_attvalue(il_ji)),cl_fonction)
 
                ENDDO
 
             ENDIF
 
          ELSE
 
             IF (PRESENT(ida_typevalue) .AND. &
                 PRESENT(rda_attvalue)) THEN
 
                DO il_ji=1, id_nbatt
 
                   SELECT CASE (ida_typevalue(il_ji))
 
                      CASE (2)
                         il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                             NF90_GLOBAL, &
                                                             cda_attname(il_ji), &
                                                             cda_attvalue(il_ji)),cl_fonction)
 
                      CASE (5)
                         il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                             NF90_GLOBAL, &
                                                             cda_attname(il_ji), &
                                                             rda_attvalue(il_ji)),cl_fonction)
 
                      CASE DEFAULT
                         WRITE(0,*) ' MIOL_writef_attributeslist_NC : undefined type.'
                         CALL flush(0)
                         STOP
 
 
                   END SELECT
 
                ENDDO
 
             ELSE
 
                DO il_ji=1, id_nbatt
 
                   il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                       NF90_GLOBAL, &
                                                       cda_attname(il_ji), &
                                                       cda_attvalue(il_ji)),cl_fonction)
 
                ENDDO
 
             ENDIF
 
          ENDIF
 
          !-----------------------------------------------------------------------
          ! Out of define mode
 
          il_status = fi_ncError(NF90_ENDDEF(il_file_id),cl_fonction) 
         
          !----------------------------------------------------------------------
          ! Close file
 
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
 
 
           END SUBROUTINE MIOL_writef_attributeslist_NC
 
   !******************************************************************************
  !******************************************************************************
  !******************************************************************************
