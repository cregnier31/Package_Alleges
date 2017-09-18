!> \brief Module which contain subroutines for READ R8 values in NETCDF format
!!
!! \author F.MESSAL first version
!! \date 11/2006
!!  \version 1.1 
!! \author C.REGNIER Miol V3.1
!! \date 10/2008
!!  \version 3.2  
!! \author C.REGNIER Miol V3.2
!! \date 01/2010
!!  \version 3.2
!! \author C.REGNIER Miol V3.5
!! \date 01/2013
!!  \version 3.5 
!!  \n History :
!!        \n  06/2006  (F. Messal) F90
!!        \n 11/2006  (F. Messal) CVS V1.0
!!        \n  09/2008   CREGNIER V3.5 MIOL
!!        \n  06/2009   C.REGNIER : gestion des attributs en double ou float
!!        \n  10/2009   C.REGNIER : gestion la lecture des missing_values en double ou float
!!        \n  01/2010   C.REGNIER : correction gestion des interfaces 
!!        \n  01/2013   C.REGNIER : rajout lecture 5D
!<
!******************************************************************************
!******************************************************************************
!*** Subroutine READ unit +file
! ---                                                           ---
! --- SUBROUTINE MIOL_readf_field_R8_4D_NC(cd_filename,cd_varname,rdpa_value,ida_dimsize)   
! --- SUBROUTINE MIOL_readf_field_R8_3D_NC(cd_filename,cd_varname,rdpa_value,ida_dimsize)   
! --- SUBROUTINE MIOL_readf_field_R8_2D_NC(cd_filename,cd_varname,rdpa_value,ida_dimsize)   
! --- SUBROUTINE MIOL_readf_field_R8_1D_NC(cd_filename,cd_varname,rdpa_value,ida_dimsize)   
! --- SUBROUTINE MIOL_readu_field_R8_4D_NC(cd_filename,cd_varname,rdpa_value,ida_dimsize)   
! --- SUBROUTINE MIOL_readu_field_R8_3D_NC(cd_filename,cd_varname,rdpa_value,ida_dimsize)   
! --- SUBROUTINE MIOL_readu_field_R8_2D_NC(cd_filename,cd_varname,rdpa_value,ida_dimsize)   
! --- SUBROUTINE MIOL_readu_field_R8_1D_NC(cd_filename,cd_varname,rdpa_value,ida_dimsize)   
! --- 
!******************************************************************************

!MODULE src_Read_R8
!**-----------------------------------------
!** Module for READ R8 values in NETCDF format
!** C.REGNIER Janvier 2013 V3.5 Miol 
!**-------------------------------------------
!CONTAINS

          !=====================================================================
          !
          !> \brief Description : This function gets data values from the variable defined
          !!              in argument. You can also know the length of the variable
          !!              dimensions.
          !!              This functions allows you to get array of data values
          !!              without knowing the correct lenght of each dimension.
          !!              You just have to know the type and the number of
          !!              dimensions. This function uses pointers, so you don’t
          !!              have to know the correct dimensions of the data arrays.
          !!
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: REAL(KIND=8), POINTER, DIMENSION(:,:,:,:) :: rdpa_value
          !!
          !!  @param cd_filename A NetCDF filename. You must specify the complete path. \n
          !!  @param cd_varname Variable name. \n
          !!  @param rdpa_values The data values to be read. \n
          !!  @param ida_dimsize Returned vector of values corresponding to the
          !!  @param length of each dimension. \n
          !!
          !! History :
          !!         \n  06/2006  (F. Messal) F90
          !!         \n  11/2006  (F. Messal) CVS V1.0
          !!         \n  01/2013  (C.Regnier) SVN V3.5.0
          !<
           !!=====================================================================

         SUBROUTINE MIOL_readf_field_R8_4D_NC (cd_filename, &
                                            cd_varname, &
                                            rdpa_value, &
                                            ida_dimsize,&
                                            ida_indx,&
                                            ida_indy,&
                                            ida_indz,&
                                            ida_indt)
 
        
       	USE INT_ATTRIBUTS
        USE INT_ATTRIBUTSTYPE
        USE MFT_error
        USE netcdf
        USE MIOL_param 
        IMPLICIT NONE
 
       
 
          !----------------------------------------------------------------------
 
	  CHARACTER(LEN=*),               INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
          REAL(KIND=8), DIMENSION(:,:,:,:), POINTER :: rdpa_value
          INTEGER, DIMENSION(4), OPTIONAL,  INTENT(OUT) :: ida_dimsize
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx,ida_indy,ida_indz,ida_indt

          CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
          INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsize
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER :: il_nbdim, il_type
          INTEGER, DIMENSION(4) :: ila_dimids
          INTEGER :: il_file_id, il_var_id, il_status, il_ji
          
          INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2)                    :: rla_offsetvalue
          REAL(KIND=4), DIMENSION(2)                    :: rla_offsetvalue_R4
          REAL(KIND=4) :: rl_fillvaluetmp 
          REAL(KIND=8) :: rl_fillvaluetmp_R8
          LOGICAL    :: ll_missing_value,ll_fill_value  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset 
          !
          !*---------------------------------------------
          cl_fonction='MIOL_readf_field_R8_4D_NC'
          
          !----------------------------------------------------------------------
          ! Initialization
          
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fill_value=.FALSE. 
 
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
                                                       ila_dimids,&
                                                       il_nbatt),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Memory allocation
 
          ALLOCATE(cla_dimname(il_nbdim), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
          ALLOCATE(ila_dimsize(il_nbdim), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' ila_dimsize',cl_fonction)
 
 
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
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz).AND. PRESENT(ida_indt)) THEN
             ALLOCATE(rdpa_value(size(ida_indx(:)),size(ida_indy(:)),size(ida_indz(:)),size(ida_indt(:)) ), &
                  stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ELSE
          ALLOCATE(rdpa_value(ila_dimsize(1), &
                        ila_dimsize(2), &
                        ila_dimsize(3), &
                        ila_dimsize(4)), &
                        stat=il_status)
          il_status = fi_memError(il_status, 'rdpa_value',cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Real variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz)) THEN
             il_status =fi_ncError(NF90_GET_VAR(il_file_id, &
                                        il_var_id,&
                                        rdpa_value,&
                                        start = (/ida_indx(:),ida_indy(:),ida_indz(:),ida_indt(:)/)),cl_fonction)
                          
          ELSE
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_var_id, &
                                              rdpa_value),cl_fonction)
          
          ENDIF
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(rdpa_value)
          ENDIF
          	
	  !------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          DO il_jj = 1, il_nbatt
          il_status = fi_ncError(NF90_INQ_ATTNAME(il_file_id, &
                                                  il_var_id, &
                                                  il_jj, &
                                                  cl_tmpattname),cl_fonction)
             SELECT CASE(TRIM(cl_tmpattname))
             CASE('scale_factor')
                ll_scale_factor = .TRUE.
             CASE('add_offset')
                ll_offset = .TRUE.
             CASE('missing_value')  ! FHZ: MRI files
                ll_missing_value = .TRUE.
             CASE('_FillValue') ! FHZ verification de l adequation avec rl_fillvalue
                ll_fill_value = .TRUE.
             ENDSELECT
          ENDDO

          !**  Test sur fillvalue et missing value
          IF ( ll_fill_value.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fill_value .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fill_value ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
         !** Inquire type Attribut 
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)
           SELECT CASE (il_type)         
           CASE (NF90_FLOAT)
              !  teste les missing values en FLOAT !
              CALL MIOL_read_attribute_NC(il_file_id, &
                                          cd_varname, &
                                          TRIM(cl_tmpattname), &
                                          rl_fillvaluetmp)
              
              where(rdpa_value(:,:,:,:) == rl_fillvaluetmp)
                 rdpa_value(:,:,:,:)=rg_fillvalue_R8
              endwhere
           CASE (NF90_DOUBLE)
              !  teste les missing values en DOUBLE !
              CALL MIOL_read_attribute_NC(il_file_id, &
                   cd_varname, &
                   TRIM(cl_tmpattname), &
                   rl_fillvaluetmp_R8)
              where(rdpa_value(:,:,:,:) == rl_fillvaluetmp_R8)
                 rdpa_value(:,:,:,:)=rg_fillvalue_R8
              endwhere
           CASE DEFAULT
              WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
              CALL EXIT(3)
           ENDSELECT
        ENDIF
  
          IF (ll_scale_factor .AND. ll_offset) THEN
             !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
              CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                                                   TRIM(cd_varname),&
                                                   cla_attname,&
                                                   rla_offsetvalue_R4)
                WHERE(rdpa_value(:,:,:,:) < rg_fillvalue_R8)
                   rdpa_value(:,:,:,:)=(rdpa_value(:,:,:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:,:,:)=rg_fillvalue_R8
                ENDWHERE
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                                                   TRIM(cd_varname),&
                                                   cla_attname,&
                                                   rla_offsetvalue)
                WHERE(rdpa_value(:,:,:,:) < rg_fillvalue_R8)
                   rdpa_value(:,:,:,:)=(rdpa_value(:,:,:,:)*rla_offsetvalue(1))+rla_offsetvalue(2)
                ELSEWHERE
                   rdpa_value(:,:,:,:)=rg_fillvalue_R8
                ENDWHERE
           ENDSELECT
          ENDIF
          
          !----------------------------------------------------------------------
          ! Close file
 
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
         
 
     END SUBROUTINE MIOL_readf_field_R8_4D_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
 
          !!=====================================================================
          !> \brief
          !! Description : This function gets data values from the variable defined
          !!              in argument. You can also know the length of the variable
          !!              dimensions.
          !!              This functions allows you to get array of data values
          !!              without knowing the correct lenght of each dimension.
          !!              You just have to know the type and the number of
          !!              dimensions. This function uses pointers, so you don’t
          !!              have to know the correct dimensions of the data arrays.
          !!
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: REAL(KIND=8), POINTER, DIMENSION(:,:,:) :: rdpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param rdpa_values       The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !!
          !! History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  09/2008  (C.Regnier) SVN V3.5.0
          !<
          !!=====================================================================
     
           SUBROUTINE MIOL_readf_field_R8_3D_NC (cd_filename, &
                                          cd_varname, &
                                          rdpa_value, &
                                          ida_dimsize,&
                                          ida_indx,&
                                          ida_indy,&
                                          ida_indz)
          
	  USE INT_ATTRIBUTS
          USE INT_ATTRIBUTSTYPE 
          USE MFT_error
          USE netcdf
          USE MIOL_param	
	  IMPLICIT NONE
 
 
          !----------------------------------------------------------------------
          CHARACTER(LEN=*),               INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
          REAL(KIND=8), DIMENSION(:,:,:), POINTER :: rdpa_value
          INTEGER, DIMENSION(3), OPTIONAL,  INTENT(OUT) :: ida_dimsize
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx,ida_indy,ida_indz
 
          CHARACTER(LEN=255), DIMENSION(3) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(3) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_file_id, il_var_id, il_status, il_ji
 	  INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2)                    :: rla_offsetvalue
          REAL(KIND=4), DIMENSION(2)                    :: rla_offsetvalue_R4
          REAL(KIND=4) :: rl_fillvaluetmp 
          REAL(KIND=8) :: rl_fillvaluetmp_R8
          LOGICAL    :: ll_missing_value,ll_fill_value  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset
          !
          !*---------------------------------------------
          cl_fonction='MIOL_readf_field_R8_3D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fill_value=.FALSE.
         
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
                                                       ila_dimids,&
                                                       il_nbatt),cl_fonction)
 
          IF (il_nbdim.NE.3) THEN
             WRITE(0,*) "MIOL_read_field_R8_3D_NC: array dimension error. "
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
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz)) THEN
             ALLOCATE(rdpa_value(size(ida_indx(:)),&
                                 size(ida_indy(:)),&
                                 size(ida_indz(:))),&
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ELSE 
             ALLOCATE(rdpa_value(ila_dimsize(1), &
                                 ila_dimsize(2), &
                                 ila_dimsize(3)), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF 
 
          !----------------------------------------------------------------------
          ! Real variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz)) THEN
             il_status =fi_ncError(NF90_GET_VAR(il_file_id, &
                                                il_var_id,&
                                                rdpa_value,&
                                                start = (/ida_indx(1),ida_indy(1),ida_indz(1)/),&
                                                count = (/size(ida_indx(:)), size(ida_indy(:)), size(ida_indz(:))/)),cl_fonction)
             
          ELSE 
            il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_var_id, &
                                              rdpa_value),cl_fonction)
 
          ENDIF
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(rdpa_value)
          ENDIF
          !------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          DO il_jj = 1, il_nbatt
          il_status = fi_ncError(NF90_INQ_ATTNAME(il_file_id, &
                                                  il_var_id, &
                                                  il_jj, &
                                                  cl_tmpattname),cl_fonction)
          SELECT CASE(TRIM(cl_tmpattname))
          CASE('scale_factor')
             ll_scale_factor = .TRUE.
          CASE('add_offset')
             ll_offset = .TRUE.
          CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          CASE('_FillValue') ! FHZ verification de l adequation avec rg_fillvalue_R8
             ll_fill_value = .TRUE.
          ENDSELECT
          ENDDO
	  !**  Test sur fillvalue et missing value
          IF ( ll_fill_value.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fill_value .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fill_value ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
         !** Inquire type Attribut 
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)
           SELECT CASE (il_type)         
           CASE (NF90_FLOAT)
              !  teste les missing values en FLOAT !
              CALL MIOL_read_attribute_NC(il_file_id, &
                                          cd_varname, &
                                          TRIM(cl_tmpattname), &
                                          rl_fillvaluetmp)
              
              where(rdpa_value(:,:,:) == rl_fillvaluetmp)
                 rdpa_value(:,:,:)=rg_fillvalue_R8
              endwhere
           CASE (NF90_DOUBLE)
              !  teste les missing values en DOUBLE !
              CALL MIOL_read_attribute_NC(il_file_id, &
                   cd_varname, &
                   TRIM(cl_tmpattname), &
                   rl_fillvaluetmp_R8)
              where(rdpa_value(:,:,:) == rl_fillvaluetmp_R8)
                 rdpa_value(:,:,:)=rg_fillvalue_R8
              endwhere
           CASE DEFAULT
              WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
              CALL EXIT(3)
           ENDSELECT
        ENDIF
          
          IF (ll_scale_factor .AND. ll_offset) THEN
             !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
              CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                                                   TRIM(cd_varname),&
                                                   cla_attname,&
                                                   rla_offsetvalue_R4)
                WHERE(rdpa_value(:,:,:) < rg_fillvalue_R8)
                   rdpa_value(:,:,:)=(rdpa_value(:,:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:,:)=rg_fillvalue_R8
                ENDWHERE
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                                                cd_varname,&
                                                cla_attname,&
                                                rla_offsetvalue)
                WHERE(rdpa_value(:,:,:) < rg_fillvalue_R8)
                   rdpa_value(:,:,:)=(rdpa_value(:,:,:)*rla_offsetvalue(1))+rla_offsetvalue(2)
                ELSEWHERE
                   rdpa_value(:,:,:)=rg_fillvalue_R8
                ENDWHERE
             ENDSELECT
          ENDIF
         
          !----------------------------------------------------------------------
          ! Close file
 
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
     END SUBROUTINE MIOL_readf_field_R8_3D_NC
 
  !******************************************************************************
  !******************************************************************************
 !******************************************************************************
 
         !!=====================================================================
          !> \brief
          !! Description : This function gets data values from the variable defined
          !!              in argument. You can also know the length of the variable
          !!              dimensions.
          !!              This functions allows you to get array of data values
          !!              without knowing the correct lenght of each dimension.
          !!              You just have to know the type and the number of
          !!              dimensions. This function uses pointers, so you don’t
          !!              have to know the correct dimensions of the data arrays.
          !!
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: REAL(KIND=8), POINTER, DIMENSION(:,:) :: rdpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param rdpa_values       The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !!
          !! History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  09/2008  (C.Regnier) SVN V3.5.0
          !<
          !!=====================================================================
 
         SUBROUTINE MIOL_readf_field_R8_2D_NC (cd_filename, &
                                          cd_varname, &
                                          rdpa_value, &
                                          ida_dimsize,&
                                          ida_indx,&
                                          ida_indy)

           
          USE MFT_error
          USE INT_ATTRIBUTSTYPE 
          USE INT_ATTRIBUTS
          USE netcdf
          USE MIOL_param
    	  IMPLICIT NONE
 
       
          !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),               INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
          REAL(KIND=8), DIMENSION(:,:), POINTER :: rdpa_value
          INTEGER, DIMENSION(2), OPTIONAL,  INTENT(OUT) :: ida_dimsize
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx,ida_indy

          CHARACTER(LEN=255), DIMENSION(2) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(2) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_file_id, il_var_id, il_status, il_ji
     	  INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2)                    :: rla_offsetvalue
          REAL(KIND=4), DIMENSION(2)                    :: rla_offsetvalue_R4
          REAL(KIND=4) :: rl_fillvaluetmp 
          REAL(KIND=8) :: rl_fillvaluetmp_R8
 
          LOGICAL    :: ll_missing_value,ll_fill_value  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset
          !
          !*---------------------------------------------
          cl_fonction='MIOL_readf_field_R8_2D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fill_value=.FALSE.

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
                                                       ila_dimids,&
                                                       il_nbatt),cl_fonction)
 
          IF (il_nbdim.NE.2) THEN
             WRITE(0,*) "MIOL_read_field_R8_2D_NC: array dimension error. "
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
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy)) THEN
             ALLOCATE(rdpa_value(size(ida_indx(:)),&
                                 size(ida_indy(:))),&
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ELSE 
             ALLOCATE(rdpa_value(ila_dimsize(1), &
                                 ila_dimsize(2)), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF 
 
          !----------------------------------------------------------------------
          ! Real variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy)) THEN
             il_status =fi_ncError(NF90_GET_VAR(il_file_id, &
                                                il_var_id,&
                                                rdpa_value,&
                                                start = (/ida_indx(1),ida_indy(1)/),&
                                                count = (/size(ida_indx(:)), size(ida_indy(:))/)),cl_fonction)
             
          ELSE 
             il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                 il_var_id, &
                                                 rdpa_value),cl_fonction)
          ENDIF
 
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(rdpa_value)
          ENDIF
 	
	  !------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          DO il_jj = 1, il_nbatt
          il_status = fi_ncError(NF90_INQ_ATTNAME(il_file_id, &
                                                  il_var_id, &
                                                  il_jj, &
                                                  cl_tmpattname),cl_fonction)
             SELECT CASE(TRIM(cl_tmpattname))
             CASE('scale_factor')
                ll_scale_factor = .TRUE.
             CASE('add_offset')
                ll_offset = .TRUE.
               CASE('missing_value')  ! FHZ: MRI files
                  ll_missing_value = .TRUE.
               CASE('_FillValue') ! FHZ verification de l adequation avec rl_fillvalue
                  ll_fill_value = .TRUE.
               ENDSELECT
            ENDDO
	 
 !**  Test sur fillvalue et missing value
         IF ( ll_fill_value.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fill_value .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fill_value ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
         !** Inquire type Attribut 
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)
        SELECT CASE (il_type)         
          CASE (NF90_FLOAT)
           !  teste les missing values en FLOAT !
              CALL MIOL_read_attribute_NC(il_file_id, &
                                         cd_varname, &
                                         TRIM(cl_tmpattname), &
                                         rl_fillvaluetmp)
                     
          where(rdpa_value(:,:) == rl_fillvaluetmp)
            rdpa_value(:,:)=rg_fillvalue_R8
         endwhere
        CASE (NF90_DOUBLE)
        !  teste les missing values en DOUBLE !
           CALL MIOL_read_attribute_NC(il_file_id, &
                                       cd_varname, &
                                       TRIM(cl_tmpattname), &
                                       rl_fillvaluetmp_R8)
         where(rdpa_value(:,:) == rl_fillvaluetmp_R8)
            rdpa_value(:,:)=rg_fillvalue_R8
         endwhere
        CASE DEFAULT
         WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
        CALL EXIT(3)
       ENDSELECT
      ENDIF
   
             IF (ll_scale_factor .AND. ll_offset) THEN
             !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
              CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                                                   TRIM(cd_varname),&
                                                   cla_attname,&
                                                   rla_offsetvalue_R4)
                WHERE(rdpa_value(:,:) < rg_fillvalue_R8)
                   rdpa_value(:,:)=(rdpa_value(:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:)=rg_fillvalue_R8
                ENDWHERE
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                                                   cd_varname,&
                                                   cla_attname,&
                                                   rla_offsetvalue)
                WHERE(rdpa_value(:,:) < rg_fillvalue_R8)
                   rdpa_value(:,:)=(rdpa_value(:,:)*rla_offsetvalue(1))+rla_offsetvalue(2)
                ELSEWHERE
                   rdpa_value(:,:)=rg_fillvalue_R8
             ENDWHERE
          ENDSELECT
       ENDIF

          !----------------------------------------------------------------------
          ! Close file
 
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
  

     END SUBROUTINE MIOL_readf_field_R8_2D_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
          
          !!=====================================================================
          !> \brief
          !! Description : This function gets data values from the variable defined
          !!              in argument. You can also know the length of the variable
          !!              dimensions.
          !!              This functions allows you to get array of data values
          !!              without knowing the correct lenght of each dimension.
          !!              You just have to know the type and the number of
          !!              dimensions. This function uses pointers, so you don’t
          !!              have to know the correct dimensions of the data arrays.
          !!
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: REAL(KIND=8), POINTER, DIMENSION(:) :: rdpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param rdpa_values       The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !!
          !! History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  09/2008  (C.Regnier) SVN V3.5.0
          !<
          !!=====================================================================
 
 
       SUBROUTINE MIOL_readf_field_R8_1D_NC (cd_filename, &
                                          cd_varname, &
                                          rdpa_value, &
                                          ida_dimsize,&
                                          ida_indx)
          
         USE INT_ATTRIBUTS
         USE INT_ATTRIBUTSTYPE 
         USE MFT_error
         USE netcdf
         USE MIOL_param
	 IMPLICIT NONE
 
          !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),               INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
          REAL(KIND=8), DIMENSION(:), POINTER :: rdpa_value
          INTEGER, DIMENSION(1), OPTIONAL,  INTENT(OUT) :: ida_dimsize
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx
 
          CHARACTER(LEN=255), DIMENSION(1) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(1) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_file_id, il_var_id, il_status, il_ji
 
           INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2)                    :: rla_offsetvalue
          REAL(KIND=4), DIMENSION(2)                    :: rla_offsetvalue_R4
          REAL(KIND=4) :: rl_fillvaluetmp 
          REAL(KIND=8) :: rl_fillvaluetmp_R8
          LOGICAL    :: ll_missing_value,ll_fill_value  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset
          !
          !*---------------------------------------------
          cl_fonction='MIOL_readf_field_R8_1D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fill_value=.FALSE.
  
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
                                                       ila_dimids,&
                                                       il_nbatt),cl_fonction)
 
          IF (il_nbdim.NE.1) THEN
             WRITE(0,*) "MIOL_read_field_R8_1D_NC: array dimension error. "
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
          IF (PRESENT(ida_indx)) THEN
             ALLOCATE(rdpa_value(size(ida_indx(:))),&                        
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ELSE 
             ALLOCATE(rdpa_value(ila_dimsize(1)), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF 
 
          !----------------------------------------------------------------------
          ! Real variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx)) THEN
             il_status =fi_ncError(NF90_GET_VAR(il_file_id, &
                                                il_var_id,&
                                                rdpa_value,&
                                                start = (/ida_indx(1)/),&
                                                count = (/size(ida_indx(:))/)),cl_fonction)
          ELSE 
             il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                 il_var_id, &
                                                 rdpa_value),cl_fonction)
          ENDIF
        
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(rdpa_value)
          ENDIF
          !------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          DO il_jj = 1, il_nbatt
          il_status = fi_ncError(NF90_INQ_ATTNAME(il_file_id, &
                                                  il_var_id, &
                                                  il_jj, &
                                                  cl_tmpattname),cl_fonction)
          SELECT CASE(TRIM(cl_tmpattname))
          CASE('scale_factor')
             ll_scale_factor = .TRUE.
          CASE('add_offset')
             ll_offset = .TRUE.
          CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          CASE('_FillValue') ! FHZ verification de l adequation avec rl_fillvalue
             ll_fill_value = .TRUE.
          ENDSELECT
          ENDDO
          !**  Test sur fillvalue et missing value
          IF ( ll_fill_value.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fill_value .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fill_value ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
         !** Inquire type Attribut 
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)
           SELECT CASE (il_type)         
           CASE (NF90_FLOAT)
              !  teste les missing values en FLOAT !
              CALL MIOL_read_attribute_NC(il_file_id, &
                                          cd_varname, &
                                          TRIM(cl_tmpattname), &
                                          rl_fillvaluetmp)
              
              where(rdpa_value(:) == rl_fillvaluetmp)
                 rdpa_value(:)=rg_fillvalue_R8
              endwhere
           CASE (NF90_DOUBLE)
              !  teste les missing values en DOUBLE !
              CALL MIOL_read_attribute_NC(il_file_id, &
                   cd_varname, &
                   TRIM(cl_tmpattname), &
                   rl_fillvaluetmp_R8)
              where(rdpa_value(:) == rl_fillvaluetmp_R8)
                 rdpa_value(:)=rg_fillvalue_R8
              endwhere
           CASE DEFAULT
              WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
              CALL EXIT(3)
           ENDSELECT
        ENDIF
          
          IF (ll_scale_factor .AND. ll_offset) THEN
             !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                                                   TRIM(cd_varname),&
                                                   cla_attname,&
                                                   rla_offsetvalue_R4)
                WHERE(rdpa_value(:) < rg_fillvalue_R8)
                   rdpa_value(:)=(rdpa_value(:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:)=rg_fillvalue_R8
                ENDWHERE
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                                                   cd_varname,&
                                                   cla_attname,&
                                                   rla_offsetvalue)
                WHERE(rdpa_value(:) < rg_fillvalue_R8)
                   rdpa_value(:)=(rdpa_value(:)*rla_offsetvalue(1))+rla_offsetvalue(2)
                ELSEWHERE
                   rdpa_value(:)=rg_fillvalue_R8
                ENDWHERE
             ENDSELECT
          ENDIF
  
          !----------------------------------------------------------------------
          ! Close file
 
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)

     END SUBROUTINE MIOL_readf_field_R8_1D_NC
 
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
          !! as a pointer: REAL(KIND=8), POINTER, DIMENSION(:,:,:,:) :: rdpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param rdpa_values       The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          ! !                     length of each dimension.
          !!
          !! History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  09/2008  (C.Regnier) SVN V3.5.0
          !<
          !!=====================================================================
 
       SUBROUTINE MIOL_readu_field_R8_4D_NC (id_file_id, &
                                          cd_varname, &
                                          rdpa_value, &
                                          ida_dimsize,&
                                          ida_indx,&
                                          ida_indy,&
                                          ida_indz,&
                                          ida_indt)
         USE INT_ATTRIBUTSTYPE 
         USE INT_ATTRIBUTS
         USE MFT_error
         USE netcdf
         USE MIOL_param 
	 IMPLICIT NONE
           
          !----------------------------------------------------------------------
 
          INTEGER,                        INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
          REAL(KIND=8), DIMENSION(:,:,:,:), POINTER :: rdpa_value
          INTEGER, DIMENSION(4), OPTIONAL,  INTENT(OUT) :: ida_dimsize
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx,ida_indy,ida_indz,ida_indt

          CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
          INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsize
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER :: il_nbdim, il_type
          INTEGER, DIMENSION(4) :: ila_dimids
          INTEGER :: il_var_id, il_status, il_ji
 
          INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2)                    :: rla_offsetvalue
          REAL(KIND=4), DIMENSION(2)                    :: rla_offsetvalue_R4
          REAL(KIND=4) :: rl_fillvaluetmp
          REAL(KIND=8) :: rl_fillvaluetmp_R8 
          LOGICAL    :: ll_missing_value,ll_fill_value  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset
          !
          !*--------------------------------------------------------
          cl_fonction='MIOL_readu_field_R8_4D_NC'
          ll_fill_value=.FALSE.
          ll_missing_value=.FALSE.
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          !----------------------------------------------------------------------
          ! Initialization
 
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fill_value=.FALSE.
 
 
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
                                                       ila_dimids,&
                                                       il_nbatt),cl_fonction)

          !----------------------------------------------------------------------
          ! Memory allocation
 
          ALLOCATE(cla_dimname(il_nbdim), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
          ALLOCATE(ila_dimsize(il_nbdim), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' ila_dimsize',cl_fonction)
 
 
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
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz).AND. PRESENT(ida_indt)) THEN
             ALLOCATE(rdpa_value(size(ida_indx(:)),size(ida_indy(:)),size(ida_indz(:)),size(ida_indt(:)) ), &
                  stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ELSE
             ALLOCATE(rdpa_value(ila_dimsize(1), &
                                ila_dimsize(2), &
                                ila_dimsize(3), &
                                ila_dimsize(4)), &
                                stat=il_status)
             il_status = fi_memError(il_status, 'rdpa_value ',cl_fonction)
          ENDIF
 
          !----------------------------------------------------------------------
          ! Real variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz)) THEN
             il_status =fi_ncError(NF90_GET_VAR(id_file_id, &
                                                il_var_id,&
                                                rdpa_value,&
                                                start = (/ida_indx(:),ida_indy(:),ida_indz(:),ida_indt(:)/)),cl_fonction)
          ELSE
             il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                                 il_var_id, &
                                                 rdpa_value),cl_fonction)
          ENDIF

          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(rdpa_value)
          ENDIF
          !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          DO il_jj = 1, il_nbatt
          il_status = fi_ncError(NF90_INQ_ATTNAME(id_file_id, &
                                                  il_var_id, &
                                                  il_jj, &
                                                  cl_tmpattname),cl_fonction)
          SELECT CASE(TRIM(cl_tmpattname))
          CASE('scale_factor')
             ll_scale_factor = .TRUE.
          CASE('add_offset')
             ll_offset = .TRUE.
           CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          CASE('_FillValue') ! FHZ verification de l adequation avec rl_fillvalue
             ll_fill_value = .TRUE.
          ENDSELECT
          ENDDO
	  
          	  ! Test sur fillvalue et missing value
         IF ( ll_fill_value.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fill_value .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fill_value ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut 
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)
        SELECT CASE (il_type)         
          CASE (NF90_FLOAT)
           !  teste les missing values en FLOAT !
    	     CALL MIOL_read_attribute_NC(id_file_id, &
                                         cd_varname, &
                                         TRIM(cl_tmpattname), &
                                         rl_fillvaluetmp)
					 
          where(rdpa_value(:,:,:,:) == rl_fillvaluetmp)
	      	rdpa_value(:,:,:,:)=rg_fillvalue_R8
	     endwhere
        CASE (NF90_DOUBLE)
        !  teste les missing values en DOUBLE !
           CALL MIOL_read_attribute_NC(id_file_id, &
                                       cd_varname, &
                                       TRIM(cl_tmpattname), &
                                       rl_fillvaluetmp_R8)
         where(rdpa_value(:,:,:,:) == rl_fillvaluetmp_R8)
            rdpa_value(:,:,:,:)=rg_fillvalue_R8
         endwhere
        CASE DEFAULT
         WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
        CALL EXIT(3)
       ENDSELECT
    ENDIF

          IF (ll_scale_factor .AND. ll_offset) THEN
             !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                                                   TRIM(cd_varname),&
                                                   cla_attname,&
                                                   rla_offsetvalue_R4)
                WHERE(rdpa_value(:,:,:,:) < rg_fillvalue_R8)
                   rdpa_value(:,:,:,:)=(rdpa_value(:,:,:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:,:,:)=rg_fillvalue_R8
                ENDWHERE
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                                                   TRIM(cd_varname),&
                                                   cla_attname,&
                                                   rla_offsetvalue)
                WHERE(rdpa_value(:,:,:,:) < rg_fillvalue_R8)
                   rdpa_value(:,:,:,:)=(rdpa_value(:,:,:,:)*rla_offsetvalue(1))+rla_offsetvalue(2)
                ELSEWHERE
                   rdpa_value(:,:,:,:)=rg_fillvalue_R8
                ENDWHERE
             ENDSELECT
          ENDIF
             
          
     END SUBROUTINE MIOL_readu_field_R8_4D_NC
 
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
          !! as a pointer: REAL(KIND=8), POINTER, DIMENSION(:,:,:) :: rdpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param rdpa_values       The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !!
          !! History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  09/2008  (C.Regnier) SVN V3.5.0
          !<  
          !!=====================================================================
 
 
       SUBROUTINE MIOL_readu_field_R8_3D_NC (id_file_id, &
                                          cd_varname, &
                                          rdpa_value, &
                                          ida_dimsize,&
                                          ida_indx,&
                                          ida_indy,&
                                          ida_indz)
          
         USE INT_ATTRIBUTS
         USE INT_ATTRIBUTSTYPE 
         USE MFT_error
         USE netcdf
         USE MIOL_param
          IMPLICIT NONE
 
          !----------------------------------------------------------------------
 
          INTEGER,                        INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
          REAL(KIND=8), DIMENSION(:,:,:), POINTER :: rdpa_value
          INTEGER, DIMENSION(3), OPTIONAL,  INTENT(OUT) :: ida_dimsize
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL :: ida_indx,ida_indy,ida_indz

          CHARACTER(LEN=255), DIMENSION(3) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(3) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji
          INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2)                    :: rla_offsetvalue
          REAL(KIND=4), DIMENSION(2)                    :: rla_offsetvalue_R4
          REAL(KIND=4) :: rl_fillvaluetmp
          REAL(KIND=8) :: rl_fillvaluetmp_R8 
          LOGICAL                                       :: ll_scale_factor,ll_offset
          LOGICAL    :: ll_missing_value,ll_fill_value  ! FHZ test sur les valeurs manquantes
          !
          !*--------------------------------------------------------
          cl_fonction='MIOL_readu_field_R8_3D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fill_value=.FALSE.


          !------------------------------------------------------------
          ! Inquire variable
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_VARIABLE(id_file_id, &
                                                       il_var_id, &
                                                       cl_tmp, &
                                                       il_type, &
                                                       il_nbdim, &
                                                       ila_dimids,&
                                                       il_nbatt),cl_fonction)
 
          IF (il_nbdim.NE.3) THEN
             WRITE(0,*) "MIOL_read_field_R8_3D_NC: array dimension error. "
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
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz)) THEN
             ALLOCATE(rdpa_value(size(ida_indx(:)),&
                                 size(ida_indy(:)),&
                                 size(ida_indz(:))),&
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ELSE 
             ALLOCATE(rdpa_value(ila_dimsize(1), &
                                 ila_dimsize(2), &
                                 ila_dimsize(3)), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF
 
          !----------------------------------------------------------------------
          ! Real variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz)) THEN
             il_status =fi_ncError(NF90_GET_VAR(id_file_id, &
                                                il_var_id,&
                                                rdpa_value,&
                                                start = (/ida_indx(1),ida_indy(1),ida_indz(1)/),&
                                                count = (/size(ida_indx(:)), size(ida_indy(:)), size(ida_indz(:))/)),cl_fonction)
             
          ELSE 
             il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                                 il_var_id, &
                                                 rdpa_value),cl_fonction)
          ENDIF 
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(rdpa_value)
          ENDIF
          
          !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          DO il_jj = 1, il_nbatt
          il_status = fi_ncError(NF90_INQ_ATTNAME(id_file_id, &
                                                  il_var_id, &
                                                  il_jj, &
                                                  cl_tmpattname),cl_fonction)
          SELECT CASE(TRIM(cl_tmpattname))
          CASE('scale_factor')
             ll_scale_factor = .TRUE.
          CASE('add_offset')
             ll_offset = .TRUE.
           CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          CASE('_FillValue') ! FHZ verification de l adequation avec rl_fillvalue
             ll_fill_value = .TRUE.
          ENDSELECT
          ENDDO

          ! Test sur fillvalue et missing value
          IF ( ll_fill_value.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fill_value .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fill_value ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut 
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)
        SELECT CASE (il_type)         
          CASE (NF90_FLOAT)
           !  teste les missing values en FLOAT !
    	     CALL MIOL_read_attribute_NC(id_file_id, &
                                         cd_varname, &
                                         TRIM(cl_tmpattname), &
                                         rl_fillvaluetmp)
					 
          where(rdpa_value(:,:,:) == rl_fillvaluetmp)
	      	rdpa_value(:,:,:)=rg_fillvalue_R8
	     endwhere
        CASE (NF90_DOUBLE)
        !  teste les missing values en DOUBLE !
           CALL MIOL_read_attribute_NC(id_file_id, &


                                         cd_varname, &
                                         TRIM(cl_tmpattname), &
                                         rl_fillvaluetmp_R8)
         where(rdpa_value(:,:,:) == rl_fillvaluetmp_R8)
            rdpa_value(:,:,:)=rg_fillvalue_R8
         endwhere
        CASE DEFAULT
         WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
        CALL EXIT(3)
       ENDSELECT
      ENDIF

          IF (ll_scale_factor .AND. ll_offset) THEN
              !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                                                   TRIM(cd_varname),&
                                                   cla_attname,&
                                                   rla_offsetvalue_R4)
                WHERE(rdpa_value(:,:,:) < rg_fillvalue_R8)
                   rdpa_value(:,:,:)=(rdpa_value(:,:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:,:)=rg_fillvalue_R8
                ENDWHERE
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                                                   cd_varname,&
                                                   cla_attname,&
                                                   rla_offsetvalue)
                WHERE(rdpa_value(:,:,:) < rg_fillvalue_R8)
                   rdpa_value(:,:,:)=(rdpa_value(:,:,:)*rla_offsetvalue(1))+rla_offsetvalue(2)
                ELSEWHERE
                   rdpa_value(:,:,:)=rg_fillvalue_R8
                ENDWHERE
             ENDSELECT
          ENDIF
 
     END SUBROUTINE MIOL_readu_field_R8_3D_NC
 
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
          !! as a pointer: REAL(KIND=8), POINTER, DIMENSION(:,:) :: rdpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param rdpa_values       The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !!
          !! History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  09/2008  (C.Regnier) SVN V3.5.0
          !<  
          !!=====================================================================
 
 
       SUBROUTINE MIOL_readu_field_R8_2D_NC (id_file_id, &
                                          cd_varname, &
                                          rdpa_value, &
                                          ida_dimsize,&
                                          ida_indx,&
                                          ida_indy)

          
         USE INT_ATTRIBUTS
         USE INT_ATTRIBUTSTYPE 
         USE MFT_error
         USE netcdf
         USE MIOL_param 
	 IMPLICIT NONE
 
          
          !----------------------------------------------------------------------
 
          INTEGER,                        INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
          REAL(KIND=8), DIMENSION(:,:), POINTER:: rdpa_value
          INTEGER, DIMENSION(2), OPTIONAL,  INTENT(OUT) :: ida_dimsize
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx,ida_indy
 
          CHARACTER(LEN=255), DIMENSION(2) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(2) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji
          INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2)                    :: rla_offsetvalue
          REAL(KIND=4), DIMENSION(2)                    :: rla_offsetvalue_R4
          REAL(KIND=4) :: rl_fillvaluetmp
          REAL(KIND=8) :: rl_fillvaluetmp_R8 
  
          LOGICAL                                       :: ll_scale_factor,ll_offset
          LOGICAL    :: ll_missing_value,ll_fill_value  ! FHZ test sur les valeurs manquantes
          !
          !*---------------------------------------------
          cl_fonction='MIOL_readu_field_R8_2D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fill_value=.FALSE.


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
                                                       ila_dimids,&
                                                       il_nbatt),cl_fonction)
 
          IF (il_nbdim.NE.2) THEN
             WRITE(0,*) "MIOL_read_field_R8_2D_NC: array dimension error. "
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
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy)) THEN
             ALLOCATE(rdpa_value(size(ida_indx(:)),&
                                      size(ida_indy(:))),&
                                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ELSE 
             ALLOCATE(rdpa_value(ila_dimsize(1), &
                                 ila_dimsize(2)), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF 
 
          !----------------------------------------------------------------------
          ! Real variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy)) THEN
             il_status =fi_ncError(NF90_GET_VAR(id_file_id, &
                                                il_var_id,&
                                                rdpa_value,&
                                                start = (/ida_indx(1),ida_indy(1)/),&
                                                count = (/size(ida_indx(:)), size(ida_indy(:))/)),cl_fonction)
             
          ELSE 
             il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                                 il_var_id, &
                                                 rdpa_value),cl_fonction)
          ENDIF
        
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(rdpa_value)
          ENDIF
 
          !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          DO il_jj = 1, il_nbatt
          il_status = fi_ncError(NF90_INQ_ATTNAME(id_file_id, &
                                                  il_var_id, &
                                                  il_jj, &
                                                  cl_tmpattname),cl_fonction)
          SELECT CASE(TRIM(cl_tmpattname))
          CASE('scale_factor')
             ll_scale_factor = .TRUE.
          CASE('add_offset')
             ll_offset = .TRUE.
          CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          CASE('_FillValue') ! FHZ verification de l adequation avec rl_fillvalue
             ll_fill_value = .TRUE.
          ENDSELECT
          ENDDO
	  
	  ! Test sur fillvalue et missing value
         IF ( ll_fill_value.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fill_value .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fill_value ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut 
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)
        SELECT CASE (il_type)         
          CASE (NF90_FLOAT)
           !  teste les missing values en FLOAT !
    	     CALL MIOL_read_attribute_NC(id_file_id, &
                                         cd_varname, &
                                         TRIM(cl_tmpattname), &
                                         rl_fillvaluetmp)
					 
          where(rdpa_value(:,:) == rl_fillvaluetmp)
	      	rdpa_value(:,:)=rg_fillvalue_R8
	     endwhere
        CASE (NF90_DOUBLE)
        !  teste les missing values en DOUBLE !
           CALL MIOL_read_attribute_NC(id_file_id, &
                                         cd_varname, &
                                         TRIM(cl_tmpattname), &
                                         rl_fillvaluetmp_R8)
         where(rdpa_value(:,:) == rl_fillvaluetmp_R8)
            rdpa_value(:,:)=rg_fillvalue_R8
         endwhere
        CASE DEFAULT
         WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
        CALL EXIT(3)
       ENDSELECT
      ENDIF

          IF (ll_scale_factor .AND. ll_offset) THEN
              !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                                                   TRIM(cd_varname),&
                                                   cla_attname,&
                                                   rla_offsetvalue_R4)
                WHERE(rdpa_value(:,:) < rg_fillvalue_R8)
                   rdpa_value(:,:)=(rdpa_value(:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:)=rg_fillvalue_R8
                ENDWHERE
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                                                cd_varname,&
                                                cla_attname,&
                                                rla_offsetvalue)
             WHERE(rdpa_value(:,:) < rg_fillvalue_R8)
                rdpa_value(:,:)=(rdpa_value(:,:)*rla_offsetvalue(1))+rla_offsetvalue(2)
             ELSEWHERE
                rdpa_value(:,:)=rg_fillvalue_R8
             ENDWHERE
          ENDSELECT
       ENDIF
 
     END SUBROUTINE MIOL_readu_field_R8_2D_NC
 
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
          !! as a pointer: REAL(KIND=8), POINTER, DIMENSION(:) :: rdpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param rdpa_values       The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !!
          !! History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  09/2008  (C.Regnier) SVN V3.5.0
          !<  
          !!=====================================================================
 
       SUBROUTINE MIOL_readu_field_R8_1D_NC (id_file_id, &
                                          cd_varname, &
                                          rdpa_value, &
                                          ida_dimsize,&
                                          ida_indx)

 
          
         USE INT_ATTRIBUTS
         USE INT_ATTRIBUTSTYPE 
         USE MFT_error
         USE netcdf
         USE MIOL_param
          IMPLICIT NONE
 
          !----------------------------------------------------------------------
 
          INTEGER,                        INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
          REAL(KIND=8), DIMENSION(:), POINTER:: rdpa_value
          INTEGER, DIMENSION(1), OPTIONAL,  INTENT(OUT) :: ida_dimsize
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx
 
          CHARACTER(LEN=255), DIMENSION(1) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(1) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji
          INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2)                    :: rla_offsetvalue
          REAL(KIND=4), DIMENSION(2)                    :: rla_offsetvalue_R4
          REAL(KIND=4) :: rl_fillvaluetmp
          REAL(KIND=8) :: rl_fillvaluetmp_R8 
          LOGICAL                                       :: ll_scale_factor,ll_offset
          LOGICAL    :: ll_missing_value,ll_fill_value  ! FHZ test sur les valeurs manquantes
          !
          !*---------------------------------------------
          cl_fonction='MIOL_readu_field_R8_1D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fill_value=.FALSE.
 
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
                                                       ila_dimids,&
                                                       il_nbatt),cl_fonction)
 
          IF (il_nbdim.NE.1) THEN
             WRITE(0,*) "MIOL_read_field_R8_1D_NC: array dimension error. "
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
          IF (PRESENT(ida_indx)) THEN
             ALLOCATE(rdpa_value(size(ida_indx(:))),&                        
                                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ELSE 
             ALLOCATE(rdpa_value(ila_dimsize(1)), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF 
 
          !----------------------------------------------------------------------
          ! Real variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx)) THEN
             il_status =fi_ncError(NF90_GET_VAR(id_file_id, &
                                                il_var_id,&
                                                rdpa_value,&
                                                start = (/ida_indx(1)/),&
                                                count = (/size(ida_indx(:))/)),cl_fonction)
          ELSE 
             il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                                 il_var_id, &
                                                 rdpa_value),cl_fonction)
          ENDIF
        
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(rdpa_value)
          ENDIF
 
          !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          DO il_jj = 1, il_nbatt
          il_status = fi_ncError(NF90_INQ_ATTNAME(id_file_id, &
                                                  il_var_id, &
                                                  il_jj, &
                                                  cl_tmpattname),cl_fonction)
          SELECT CASE(TRIM(cl_tmpattname))
          CASE('scale_factor')
             ll_scale_factor = .TRUE.
          CASE('add_offset')
             ll_offset = .TRUE.
          CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          CASE('_FillValue') ! FHZ verification de l adequation avec rl_fillvalue
             ll_fill_value = .TRUE.
          ENDSELECT
          ENDDO
	  
          	  ! Test sur fillvalue et missing value
         IF ( ll_fill_value.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fill_value .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fill_value ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut 
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)
        SELECT CASE (il_type)         
          CASE (NF90_FLOAT)
           !  teste les missing values en FLOAT !
    	     CALL MIOL_read_attribute_NC(id_file_id, &
                                         cd_varname, &
                                         TRIM(cl_tmpattname), &
                                         rl_fillvaluetmp)
					 
          where(rdpa_value(:) == rl_fillvaluetmp)
	      	rdpa_value(:)=rg_fillvalue_R8
	     endwhere
        CASE (NF90_DOUBLE)
        !  teste les missing values en DOUBLE !
           CALL MIOL_read_attribute_NC(id_file_id, &


                                         cd_varname, &
                                         TRIM(cl_tmpattname), &
                                         rl_fillvaluetmp_R8)
         where(rdpa_value(:) == rl_fillvaluetmp_R8)
            rdpa_value(:)=rg_fillvalue_R8
         endwhere
        CASE DEFAULT
         WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
        CALL EXIT(3)
       ENDSELECT
      ENDIF

          IF (ll_scale_factor .AND. ll_offset) THEN
              !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                                                   TRIM(cd_varname),&
                                                   cla_attname,&
                                                   rla_offsetvalue_R4)
                WHERE(rdpa_value(:) < rg_fillvalue_R8)
                   rdpa_value(:)=(rdpa_value(:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:)=rg_fillvalue_R8
                ENDWHERE
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                                                cd_varname,&
                                                cla_attname,&
                                                rla_offsetvalue)
             WHERE(rdpa_value(:) < rg_fillvalue_R8)
                rdpa_value(:)=(rdpa_value(:)*rla_offsetvalue(1))+rla_offsetvalue(2)
             ELSEWHERE
                rdpa_value(:)=rg_fillvalue_R8
             ENDWHERE
          ENDSELECT
       ENDIF
 
     END SUBROUTINE MIOL_readu_field_R8_1D_NC
 
  !******************************************************************************
  !******************************************************************************
! END MODULE src_Read_R8 
