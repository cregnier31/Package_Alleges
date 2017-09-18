!MODULE src_ATTRIBUTS
!**-----------------------------------------
!** SUBROUTINE pour lire les addoffsets et scale factor
!   History
!** C.REGNIER Septembre 2008 V3 Miol  !**
!** C.REGNIER Mars      2009 Modification to read att in r4 Aand r8  !**
! --- General interface MIOL_read_att_scale_offset_NCC
!---  SUBROUTINE MIOL_readf_att_scale_offset_NC_R8(cd_filename,cd_varname,cda_attname,rda_offsetvalue)
!---  SUBROUTINE MIOL_readf_att_scale_offset_NC_R4(cd_filename,cd_varname,cda_attname,rda_offsetvalue)
!---  SUBROUTINE MIOL_readu_att_scale_offset_NC_R8(id_filename,cd_varname,cda_attname,rda_offsetvalue)
!---  SUBROUTINE MIOL_readu_att_scale_offset_NC_R4(id_filename,cd_varname,cda_attname,rda_offsetvalue)
! -----------------------------------------------------------------

SUBROUTINE MIOL_readf_att_sc_off_R8_NC(cd_filename,&
     cd_varname,&
     cda_attname,&
     rda_offsetvalue)

  USE MFT_error
  USE netcdf
  USE INT_ATTRIBUTSTYPE 
  IMPLICIT NONE

  !!======================================================================
  !!
  !! Description: This function reads the scale factor and the offset variable
  !!              in a file with id_file    
  !!
  !! cd_filename       A NetCDF filename. You must specify the complete path.
  !! cd_varname        The variable name 
  !! cda_attname       The attribute name 'scale_factor' and 'add_offset'
  !! rda_attvalue      The value of the attribute, add_offset and scale
  !!
  !! History :
  !!          06/2008  C.REGNIER    :: creation
  !!          11/2008  F.HERNANDEZ  :: read separetaly add_offset
  !!                                   and scale_factor depending
  !!                                   on R4/R8 type
  !!         03/2009  C.REGNIER     :: Modifs C.REGNIER
  !!
  !!======================================================================
  INTEGER                                      :: il_filename,il_type,il_status,il_variable_id
  CHARACTER(LEN=*),           INTENT( IN)      :: cd_filename
  CHARACTER(LEN=*),           INTENT( IN)      :: cd_varname
  CHARACTER(LEN=255), DIMENSION(2),INTENT( IN) :: cda_attname
  CHARACTER(LEN=255)                           :: cl_attname1,cl_attname2,cl_fonction
  REAL(KIND=8), DIMENSION(2)                   :: rda_offsetvalue
  ! FHZ rajout de rl_attvalue2_R8
  REAL(KIND=8)                                 :: rl_attvalue1_R8,rl_attvalue2_R8
  !
  !*---------------------------------------------------------------------------
  ! 
  cl_attname1=cda_attname(1)
  cl_attname2=cda_attname(2)
  cl_fonction='MIOL_readf_att_scale_offset_NC'
  IF (TRIM(cl_attname1) /= 'scale_factor' .AND. TRIM(cl_attname1) /= 'add_offset') THEN
     WRITE(0,*) 'Les attributs ne sont pas scale_factor et add_offset'
  ENDIF

  !** Test sur la valeur de l'attribut
  il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
       NF90_NOWRITE, &
       il_filename),cl_fonction)

  il_status= fi_ncError(NF90_INQ_VARID(il_filename, &
       TRIM(cd_varname),&
       il_variable_id),cl_fonction)

  ! FHZ: on gere separement le type de scale_factor et add_offset
  il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_filename, &
       il_variable_id, &
       TRIM(cl_attname1), &
       il_type),cl_fonction)
  SELECT CASE (il_type)
  CASE (NF90_DOUBLE)
     CALL MIOL_read_attribute_NC(il_filename, &
          cd_varname, &
          TRIM(cl_attname1), &
          rl_attvalue1_R8)
     rda_offsetvalue(1)=rl_attvalue1_R8
!!$  CASE (NF90_FLOAT)
!!$     CALL MIOL_read_attribute_NC(il_filename, &
!!$                                cd_varname, &
!!$                                TRIM(cl_attname1), &
!!$                                rl_attvalue1_R4)
!!$     rda_offsetvalue(1)=rl_attvalue1_R4
  CASE DEFAULT
     WRITE(0,*) ' Type de l attribut non trouvé'
     CALL flush(0)
     STOP
  ENDSELECT

  il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_filename, &
       il_variable_id, &
       TRIM(cl_attname2), &
       il_type),cl_fonction)
  SELECT CASE (il_type)
  CASE (NF90_DOUBLE)
     CALL MIOL_read_attribute_NC(il_filename, &
          cd_varname, &
          TRIM(cl_attname2), &
          rl_attvalue2_R8)
     rda_offsetvalue(2)=rl_attvalue2_R8
!!$  CASE (NF90_FLOAT)
!!$     CALL MIOL_read_attribute_NC(il_filename, &
!!$          cd_varname, &
!!$          TRIM(cl_attname2), &
!!$          rl_attvalue2_R4)
!!$     rda_offsetvalue(2)=rl_attvalue2_R4

  CASE DEFAULT
     WRITE(0,*) ' Type de l attribut non trouvé'
     CALL flush(0)
     STOP
  ENDSELECT

! FHZ: rajout du close qui manquait
  il_status = fi_ncError(NF90_CLOSE(il_filename),cl_fonction)

END SUBROUTINE MIOL_readf_att_sc_off_R8_NC

!******************************************************************************

SUBROUTINE MIOL_readf_att_sc_off_R4_NC(cd_filename,&
     cd_varname,&
     cda_attname,&
     rda_offsetvalue)

  USE MFT_error
  USE netcdf
  USE INT_ATTRIBUTSTYPE   
  IMPLICIT NONE

  !!======================================================================
  !!
  !! Description: This function reads the scale factor and the offset variable
  !!              in a file with id_file    
  !!
  !! cd_filename       A NetCDF filename. You must specify the complete path.
  !! cd_varname        The variable name 
  !! cda_attname       The attribute name 'scale_factor' and 'add_offset'
  !! rda_attvalue      The value of the attribute, add_offset and scale
  !!
  !! History :
  !!          06/2008  C.REGNIER    :: creation
  !!          11/2008  F.HERNANDEZ  :: read separetaly add_offset
  !!                                   and scale_factor depending
  !!                                   on R4/R8 type
  !!         03/2009  C.REGNIER     :: Modifs C.REGNIER
  !!======================================================================
  INTEGER                                      :: il_filename,il_type,il_status,il_variable_id
  CHARACTER(LEN=*),           INTENT( IN)      :: cd_filename
  CHARACTER(LEN=*),           INTENT( IN)      :: cd_varname
  CHARACTER(LEN=255), DIMENSION(2),INTENT( IN) :: cda_attname
  CHARACTER(LEN=255)                           :: cl_attname1,cl_attname2,cl_fonction
  REAL(KIND=4), DIMENSION(2) ,INTENT(OUT)      :: rda_offsetvalue
  ! FHZ rajout de rl_attvalue2_R8
  REAL(KIND=4)                                 :: rl_attvalue1_R4,rl_attvalue2_R4
  !
  !*---------------------------------------------------------------------------
  ! 
  cl_attname1=cda_attname(1)
  cl_attname2=cda_attname(2)
  cl_fonction='MIOL_readf_att_scale_offset_NC'
  IF (TRIM(cl_attname1) /= 'scale_factor' .AND. TRIM(cl_attname1) /= 'add_offset') THEN
     WRITE(0,*) 'Les attributs ne sont pas scale_factor et add_offset'
  ENDIF

  !** Test sur la valeur de l'attribut
  il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
       NF90_NOWRITE, &
       il_filename),cl_fonction)

  il_status= fi_ncError(NF90_INQ_VARID(il_filename, &
       TRIM(cd_varname),&
       il_variable_id),cl_fonction)

  ! FHZ: on gere separement le type de scale_factor et add_offset
  il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_filename, &
       il_variable_id, &
       TRIM(cl_attname1), &
       il_type),cl_fonction)
  SELECT CASE (il_type)
  CASE (NF90_FLOAT)
     CALL MIOL_read_attribute_NC(il_filename, &
          cd_varname, &
          TRIM(cl_attname1), &
          rl_attvalue1_R4)
     rda_offsetvalue(1)=rl_attvalue1_R4
!!$  CASE (NF90_DOUBLE)
!!$     CALL MIOL_read_attribute_NC(il_filename, &
!!$          cd_varname, &
!!$          TRIM(cl_attname1), &
!!$          rl_attvalue1_R8)
!!$     rda_offsetvalue(1)=rl_attvalue1_R8
  CASE DEFAULT
     WRITE(0,*) ' Type de l attribut non trouvé'
     CALL flush(0)
     STOP
  ENDSELECT

  il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_filename, &
       il_variable_id, &
       TRIM(cl_attname2), &
       il_type),cl_fonction)
  SELECT CASE (il_type)
  CASE (NF90_FLOAT)
     CALL MIOL_read_attribute_NC(il_filename, &
          cd_varname, &
          TRIM(cl_attname2), &
          rl_attvalue2_R4)
      rda_offsetvalue(2)=rl_attvalue2_R4
!!$   CASE (NF90_DOUBLE)
!!$      CALL MIOL_read_attribute_NC(il_filename, &
!!$           cd_varname, &
!!$           TRIM(cl_attname2), &
!!$          rl_attvalue2_R8)
!!$      rda_offsetvalue(2)=rl_attvalue2_R8
   CASE DEFAULT
     WRITE(0,*) ' Type de l attribut non trouvé'
     CALL flush(0)
     STOP
  ENDSELECT

! FHZ: rajout du close qui manquait
  il_status = fi_ncError(NF90_CLOSE(il_filename),cl_fonction)

END SUBROUTINE MIOL_readf_att_sc_off_R4_NC
!
!******************************************************************************
!******************************************************************************


SUBROUTINE MIOL_readu_att_sc_off_R8_NC(id_filename,&
     cd_varname,&
     cda_attname,&
     rda_offsetvalue)


  USE MFT_error
  USE netcdf
  USE INT_ATTRIBUTSTYPE 
  IMPLICIT NONE

  !!======================================================================
  !!
  !! Description: This function reads the scale factor and the offset variable
  !!              in a file    
  !!
  !! id_filename       A NetCDF filename. You must specify the complete path.
  !! cda_varname       The variable name 
  !! cd_attname        The attribute name 'scale_factor' and 'add_offset'
  !! rda_attvalue      The value of the attribute, add_offset and scale
  !!
  !! History :
  !!          06/2008  C.REGNIER    :: creation
  !!          11/2008  F.HERNANDEZ  :: read separetaly add_offset
  !!                                   and scale_factor depending
  !!                                   on R4/R8 type
  !!         03/2009  C.REGNIER     :: Modifs C.REGNIER
  !!
  !!======================================================================

  INTEGER,           INTENT( IN)               :: id_filename
  CHARACTER(LEN=*),           INTENT( IN)      :: cd_varname
  CHARACTER(LEN=255), DIMENSION(2),INTENT(IN)  :: cda_attname
  REAL(KIND=8), DIMENSION(2)                   :: rda_offsetvalue
  CHARACTER(LEN=255)                           :: cl_attname1,cl_attname2,cl_fonction
  INTEGER                                      :: il_variable_id,il_type,il_status
  ! FHZ rajout de rl_attvalue2_R8
  REAL(KIND=8)                                 :: rl_attvalue1_R8,rl_attvalue2_R8
  !
  !*---------------------------------------------------------------------------
  ! 
  cl_attname1=cda_attname(1)
  cl_attname2=cda_attname(2)
  cl_fonction='MIOL_readu_att_scale_offset_NC'
  !PRINT *,'cl_fonction ::',cl_fonction
  IF (TRIM(cl_attname1) /= 'scale_factor' .AND. TRIM(cl_attname1) /= 'add_offset') THEN
     WRITE(0,*) 'Les attributs ne sont pas scale_factor et add_offset'
  ENDIF

  !** Test sur la valeur de l'attribut
  il_status= fi_ncError(NF90_INQ_VARID(id_filename, &
                        TRIM(cd_varname),&
                        il_variable_id),cl_fonction)
 ! FHZ: on gere separement le type de scale_factor et add_offset
  il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_filename, &
                                              il_variable_id, &
                                              TRIM(cl_attname1), &
                                              il_type),cl_fonction)
 ! PRINT *,'il_type :: ',il_type,cl_attname1
  SELECT CASE (il_type)
  CASE (NF90_DOUBLE)
  !   PRINT *,'DOUBLE'
     il_status = fi_ncError(NF90_GET_ATT(id_filename, &
                                         il_variable_id, &
                                         TRIM(cl_attname1), &
                                         rl_attvalue1_R8),cl_fonction)

     ! CALL MIOL_read_attribute_NC(id_filename, &
        !  cd_varname, &
        !  TRIM(cl_attname1), &
        !  rl_attvalue1_R8)
!     PRINT *,'rda_offsetvalue(1) ::',rl_attvalue1_R8
     rda_offsetvalue(1)=rl_attvalue1_R8
!!$  CASE (NF90_FLOAT)
!!$     CALL MIOL_read_attribute_NC(id_filename, &
!!$          cd_varname, &
!!$          TRIM(cl_attname1), &
!!$          rl_attvalue1_R4)
!!$     rda_offsetvalue(1)=rl_attvalue1_R4
  CASE DEFAULT
     WRITE(0,*) ' Type de l attribut non trouvé'
     CALL flush(0)
     STOP
  ENDSELECT

  il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_filename, &
       il_variable_id, &
       TRIM(cl_attname2), &
       il_type),cl_fonction)
!  PRINT *,'il_type :: ',il_type,cl_attname2
  SELECT CASE (il_type)
  CASE (NF90_DOUBLE)
!     PRINT *,'DOUBLE'
     il_status = fi_ncError(NF90_GET_ATT(id_filename, &
                                         il_variable_id, &
                                         TRIM(cl_attname2), &
                                        rl_attvalue2_R8),cl_fonction)
     

!!$  CALL MIOL_read_attribute_NC(id_filename, &
!!$          cd_varname, &
!!$          TRIM(cl_attname2), &
!!$          rl_attvalue2_R8)
!        PRINT *,'rda_offsetvalue(2) ::',rl_attvalue2_R8
        rda_offsetvalue(2)=rl_attvalue2_R8
 !       print *,'Affectation ok'
!!$  CASE (NF90_FLOAT)
!!$     CALL MIOL_read_attribute_NC(id_filename, &
!!$          cd_varname, &
!!$          TRIM(cl_attname2), &
!!$          rl_attvalue2_R4)
!!$     rda_offsetvalue(2)=rl_attvalue2_R4
  CASE DEFAULT
     WRITE(0,*) ' Type de l attribut non trouvé'
     CALL flush(0)
     STOP
  ENDSELECT

END SUBROUTINE MIOL_readu_att_sc_off_R8_NC

!******************************************************************************
!******************************************************************************


SUBROUTINE MIOL_readu_att_sc_off_R4_NC(id_filename,&
     cd_varname,&
     cda_attname,&
     rda_offsetvalue)


  USE MFT_error
  USE netcdf
  USE INT_ATTRIBUTSTYPE 
  IMPLICIT NONE

  !!======================================================================
  !!
  !! Description: This function reads the scale factor and the offset variable
  !!              in a file    
  !!
  !! id_filename       A NetCDF filename. You must specify the complete path.
  !! cda_varname       The variable name 
  !! cd_attname        The attribute name 'scale_factor' and 'add_offset'
  !! rda_attvalue      The value of the attribute, add_offset and scale
  !!
  !! History :
  !!          06/2008  C.REGNIER    :: creation
  !!          11/2008  F.HERNANDEZ  :: read separetaly add_offset
  !!                                   and scale_factor depending
  !!                                   on R4/R8 type
  !!         03/2009  C.REGNIER     :: Modifs C.REGNIER
  !!
  !!======================================================================

  INTEGER,           INTENT( IN)               :: id_filename
  CHARACTER(LEN=*),           INTENT( IN)      :: cd_varname
  CHARACTER(LEN=255), DIMENSION(2),INTENT(IN)  :: cda_attname
  REAL(KIND=4), DIMENSION(2) ,INTENT(OUT)      :: rda_offsetvalue
  CHARACTER(LEN=255)                           :: cl_attname1,cl_attname2,cl_fonction
  INTEGER                                      :: il_variable_id,il_type,il_status
  ! FHZ rajout de rl_attvalue2_R8
  REAL(KIND=4)                                 :: rl_attvalue1_R4,rl_attvalue2_R4

  !
  !*---------------------------------------------------------------------------
  ! 
  cl_attname1=cda_attname(1)
  cl_attname2=cda_attname(2)
  cl_fonction='MIOL_readu_att_scale_offset_NC'
  IF (TRIM(cl_attname1) /= 'scale_factor' .AND. TRIM(cl_attname1) /= 'add_offset') THEN
     WRITE(0,*) 'Les attributs ne sont pas scale_factor et add_offset'
  ENDIF
  !** Test sur la valeur de l'attribut
   il_status= fi_ncError(NF90_INQ_VARID(id_filename, &
                        TRIM(cd_varname),&
                        il_variable_id),cl_fonction)

 ! FHZ: on gere separement le type de scale_factor et add_offset
  il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_filename, &
       il_variable_id, &
       TRIM(cl_attname1), &
       il_type),cl_fonction)

  SELECT CASE (il_type)
  CASE (NF90_FLOAT)
     CALL MIOL_read_attribute_NC(id_filename, &
          cd_varname, &
          TRIM(cl_attname1), &
          rl_attvalue1_R4)
     rda_offsetvalue(1)=rl_attvalue1_R4
!!$  CASE (NF90_DOUBLE)
!!$   CALL MIOL_read_attribute_NC(id_filename, &
!!$        cd_varname, &
!!$        TRIM(cl_attname1), &
!!$        rl_attvalue1_R8)
!!$     rda_offsetvalue(1)=rl_attvalue1_R8
  CASE DEFAULT
     WRITE(0,*) ' Type de l attribut non trouvé'
     CALL flush(0)
     STOP
  ENDSELECT
  CALL flush(0) 
   il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_filename, &
       il_variable_id, &
       TRIM(cl_attname2), &
       il_type),cl_fonction)

  SELECT CASE (il_type)
  CASE (NF90_FLOAT)
     CALL MIOL_read_attribute_NC(id_filename, &
          cd_varname, &
          TRIM(cl_attname2), &
          rl_attvalue2_R4)
     rda_offsetvalue(2)=rl_attvalue2_R4
!!$  CASE (NF90_DOUBLE)
!!$     CALL MIOL_read_attribute_NC(id_filename, &
!!$          cd_varname, &
!!$          TRIM(cl_attname2), &
!!$          rl_attvalue2_R8)
!!$     rda_offsetvalue(2)=rl_attvalue2_R8
  CASE DEFAULT
     WRITE(0,*) ' Type de l attribut non trouvé'
     CALL flush(0)
     STOP
  ENDSELECT

END SUBROUTINE MIOL_readu_att_sc_off_R4_NC


