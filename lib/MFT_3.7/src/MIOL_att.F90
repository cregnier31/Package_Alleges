!> \brief Module which contain Interfaces for READ attributes scale offset values in NETCDF format
!! \author C.REGNIER Miol V3.5
!! \date September 2008 
!!  \version 3.5
!<
MODULE MIOL_att 
  
  INTERFACE MIOL_read_att_scale_offset_NC
     MODULE PROCEDURE MIOL_readf_att_scale_offset_NC,&
          MIOL_readu_att_scale_offset_NC
  END INTERFACE
  
  
CONTAINS
  SUBROUTINE MIOL_readf_att_scale_offset_NC(cd_filename,&
                                            cd_varname,&
                                            cda_attname,&
                                            rda_offsetvalue)


    USE netcdf
    IMPLICIT NONE

    !!======================================================================
    !!
    !! Description: This function reads the scale factor and the offset variable
    !!              in a file with id_file    
    !!
    !! cd_filename       A NetCDF filename. You must specify the complete path.
    !! cd_varname       The variable name 
    !! cda_attname        The attribute name 'scale_factor' and 'add_offset'
    !! rda_attvalue      The value of the attribute, add_offset and scale
    !!
    !! History :
    !!          06/2008  C.REGNIER    :: creation
    !!
    !!======================================================================
    
    CHARACTER(LEN=*),           INTENT( IN)      :: cd_filename
    CHARACTER(LEN=*),           INTENT( IN)      :: cd_varname
    CHARACTER(LEN=255), DIMENSION(2),INTENT( IN) :: cda_attname
    CHARACTER(LEN=255)                           :: cl_attname1,cl_attname2
    REAL(KIND=8), DIMENSION(2) ,INTENT(OUT)      :: rda_offsetvalue
    !
    !*---------------------------------------------------------------------------
    ! 
    cl_attname1=cda_attname(1)
    cl_attname2=cda_attname(2)
    IF (TRIM(cl_attname1) /= 'scale_factor' .AND. TRIM(cl_attname1) /= 'add_offset') THEN
       WRITE(0,*) 'Les attributs ne sont pas scale_factor et add_offset'
    ENDIF

    !** Test sur la valeur de l'attribut
    CALL hdlerr(NF90_INQ_VARID(cd_filename, &
         TRIM(clpa_varname(il_ji)),&
         il_variable_id))
    CALL hdlerr(NF90_INQUIRE_ATTRIBUTE(cd_filename, &
         il_variable_id, &
         TRIM(cl_attname1), &
         il_type))

    SELECT CASE (il_type)
    CASE (NF90_FLOAT)
       print *,'NF90_FLOAT'
       CALL MIOL_read_attribute_NC(cd_filename, &
            clpa_varname(il_ji), &
            TRIM(cl_attname1), &
            rl_attvalue1_R4)
       CALL MIOL_read_attribute_NC(cd_filename, &
            clpa_varname(il_ji), &
            TRIM(cl_attname2), &
            rl_attvalue2_R4)
       rda_offsetvalue(1)=rl_attvalue1_R4
       rda_offsetvalue(2)=rl_attvalue2_R4
    CASE (NF90_DOUBLE)
       print *,'NF90_DOUBLE'
       CALL MIOL_read_attribute_NC(cd_filename, &
            clpa_varname(il_ji), &
            TRIM(cl_attname1), &
            rl_attvalue1_R8)
       CALL MIOL_read_attribute_NC(cd_filename, &
            clpa_varname(il_ji), &
            TRIM(cl_attname2), &
            rl_attvalue2_R4)
       rda_offsetvalue(1)=rl_attvalue1_R8
       rda_offsetvalue(2)=rl_attvalue2_R4
    CASE DEFAULT
       WRITE(0,*) ' Type de l attribut non trouvé'
       CALL flush(0)
       STOP
    ENDSELECT



  CONTAINS
    SUBROUTINE hdlerr(id_status)
      !
      USE netcdf
      IMPLICIT NONE
      !
      !---------------------------------------------------------------------
      ! Description: handle error function.
      !
      ! Use:
      !
      !    INTEGER, INTENT(IN) :: id_status
      !    
      !    id_status   The integer representing the status.
      !
      ! History: (11/2006) | F.Messal | Creation
      !---------------------------------------------------------------------
      !
      INTEGER, INTENT(IN) :: id_status
      !
      IF( id_status .NE. NF90_NOERR ) THEN
         WRITE(0,*) '  MIOL_att (HDLERR error):', NF90_STRERROR(id_status)
         CALL flush(0)
         STOP
      ENDIF
      !
      RETURN
      !
    END SUBROUTINE hdlerr

  END SUBROUTINE MIOL_readf_att_scale_offset_NC
!
!*----------------------------------------------------------------------------------------------------
!

 SUBROUTINE MIOL_readu_att_scale_offset_NC(id_filename,&
       cd_varname,&
       cda_attname,&
       rda_offsetvalue)


    USE netcdf
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
    !!
    !!======================================================================

    CHARACTER(LEN=*),           INTENT( IN)      :: id_filename
    CHARACTER(LEN=*),           INTENT( IN)      :: cd_varname
    CHARACTER(LEN=255), DIMENSION(2),INTENT( IN) :: cda_attname
    CHARACTER(LEN=255)                     :: cl_attname1,cl_attname2
    REAL(KIND=8), DIMENSION(2) ,INTENT(OUT)      :: rda_offsetvalue
    !
    !*---------------------------------------------------------------------------
    ! 
    cl_attname1=cda_attname(1)
    cl_attname2=cda_attname(2)
    IF (TRIM(cl_attname1) /= 'scale_factor' .AND. TRIM(cl_attname1) /= 'add_offset') THEN
       WRITE(0,*) 'Les attributs ne sont pas scale_factor et add_offset'
    ENDIF
    !** Test sur la valeur de l'attribut
    CALL hdlerr(NF90_INQ_VARID(id_filename, &
         TRIM(clpa_varname(il_ji)),&
         il_variable_id))
    CALL hdlerr(NF90_INQUIRE_ATTRIBUTE(id_filename, &
         il_variable_id, &
         TRIM(cl_attname1), &
         il_type))
    SELECT CASE (il_type)
    CASE (NF90_FLOAT)
       print *,'NF90_FLOAT'
       CALL MIOL_read_attribute_NC(id_filename, &
            clpa_varname(il_ji), &
            TRIM(cl_attname1), &
            rl_attvalue1_R4)
       CALL MIOL_read_attribute_NC(id_filename, &
            clpa_varname(il_ji), &
            TRIM(cl_attname2), &
            rl_attvalue2_R4)
       rda_offsetvalue(1)=rl_attvalue1_R4
       rda_offsetvalue(2)=rl_attvalue2_R4
    CASE (NF90_DOUBLE)
       print *,'NF90_DOUBLE'
       CALL MIOL_read_attribute_NC(id_filename, &
            clpa_varname(il_ji), &
            TRIM(cl_attname1), &
            rl_attvalue1_R8)
       CALL MIOL_read_attribute_NC(id_filename, &
            clpa_varname(il_ji), &
            TRIM(cl_attname2), &
            rl_attvalue2_R4)
       rda_offsetvalue(1)=rl_attvalue1_R8
       rda_offsetvalue(2)=rl_attvalue2_R4

    CASE DEFAULT
       WRITE(0,*) ' Type de l attribut non trouvé'
       CALL flush(0)
       STOP
    ENDSELECT

  CONTAINS
    SUBROUTINE hdlerr(id_status)
      !
      USE netcdf
      IMPLICIT NONE
      !
      !---------------------------------------------------------------------
      ! Description: handle error function.
      !
      ! Use:
      !
      !    INTEGER, INTENT(IN) :: id_status
      !    
      !    id_status   The integer representing the status.
      !
      ! History: (11/2006) | F.Messal | Creation
      !---------------------------------------------------------------------
      !
      INTEGER, INTENT(IN) :: id_status
      !
      IF( id_status .NE. NF90_NOERR ) THEN
         WRITE(0,*) '  MIOL_att (HDLERR error):', NF90_STRERROR(id_status)
         CALL flush(0)
         STOP
      ENDIF
      !
      RETURN
      !
    END SUBROUTINE hdlerr

  END SUBROUTINE MIOL_readu_att_scale_offset_NC

END MODULE MIOL_att 
!
