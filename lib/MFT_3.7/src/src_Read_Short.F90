!**-----------------------------------------
!** Module for READ short values in NETCDF format
!** C.REGNIER Janvier 2013 V3.5 Miol 
!**-------------------------------------------
!*** Subroutine READ unit +file
! --- SUBROUTINE MIOL_readf_field_S_4D_R4_NC(cd_filename,cd_varname,idpa_value,ida_dimsize,rdpa_value) 
! --- SUBROUTINE MIOL_readf_field_S_4D_R8_NC(cd_filename,cd_varname,idpa_value,ida_dimsize,rdpa_value)
! --- SUBROUTINE MIOL_readf_field_S_3D_R4_NC(cd_filename,cd_varname,idpa_value,ida_dimsize,rdpa_value)
! --- SUBROUTINE MIOL_readf_field_S_3D_R8_NC(cd_filename,cd_varname,idpa_value,ida_dimsize,rdpa_value)
! --- SUBROUTINE MIOL_readf_field_S_2D_R4_NC(cd_filename,cd_varname,idpa_value,ida_dimsize,rdpa_value)
! --- SUBROUTINE MIOL_readf_field_S_2D_R8_NC(cd_filename,cd_varname,idpa_value,ida_dimsize,rdpa_value)
! --- SUBROUTINE MIOL_readf_field_S_1D_R4_NC(cd_filename,cd_varname,idpa_value,ida_dimsize,rdpa_value)
! --- SUBROUTINE MIOL_readf_field_S_1D_R8_NC(cd_filename,cd_varname,idpa_value,ida_dimsize,rdpa_value)

! --- SUBROUTINE MIOL_readu_field_S_4D_R4_NC(cd_filename,cd_varname,idpa_value,ida_dimsize,rdpa_value) 
! --- SUBROUTINE MIOL_readu_field_S_4D_R8_NC(cd_filename,cd_varname,idpa_value,ida_dimsize,rdpa_value)
! --- SUBROUTINE MIOL_readu_field_S_3D_R4_NC(cd_filename,cd_varname,idpa_value,ida_dimsize,rdpa_value)
! --- SUBROUTINE MIOL_readu_field_S_3D_R8_NC(cd_filename,cd_varname,idpa_value,ida_dimsize,rdpa_value)
! --- SUBROUTINE MIOL_readu_field_S_2D_R4_NC(cd_filename,cd_varname,idpa_value,ida_dimsize,rdpa_value)
! --- SUBROUTINE MIOL_readu_field_S_2D_R8_NC(cd_filename,cd_varname,idpa_value,ida_dimsize,rdpa_value)
! --- SUBROUTINE MIOL_readu_field_S_1D_R4_NC(cd_filename,cd_varname,idpa_value,ida_dimsize,rdpa_value)
! --- SUBROUTINE MIOL_readu_field_S_1D_R8_NC(cd_filename,cd_varname,idpa_value,ida_dimsize,rdpa_value)
!**-------------------------------------------
!> \brief File which contain subroutine for Read Short values in NETCDF format and convert the same data in R4 or R8
!!  Total 16 functions
!! \author C.REGNIER Miol V3.1
!! \date 07/2008
!! \version 3.1
!! \author C.REGNIER Miol V3.2
!! \date 01/2010
!! \version 3.2
!! \author C.REGNIER Miol V3.5
!! \date 01/2013
!! \version 3.3
!<

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
          !!              The output data are in R4 format
          !!
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:,:,:) :: idpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value       The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !! @param rdpa_value : The 4D data values read and convert in R4 format
          !!
          !! \memberof INT_read_write_S
          !!
          !! \n History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  07/2008  (C.Regnier) Add read scale and offset factor
          !!        \n  07/2009  (C.Regnier) On enleve le select case
          !!        \n  01/2013  (C.Regnier) V3.5 MIOL
          !<
          !!=====================================================================
 
       SUBROUTINE MIOL_readf_field_S_4D_R4_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize,&
                                         rdpa_value,&
                                         rda_offsetvalue,&
                                         ida_indx,&
                                         ida_indy,&
                                         ida_indz,&
                                         ida_indt)
 
         Use MFT_error  
         USE INT_ATTRIBUTSTYPE       
         Use INT_ATTRIBUTS
         Use netcdf
         Use MIOL_param
         IMPLICIT NONE
 
 
         
          !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=2), DIMENSION(:,:,:,:), POINTER :: idpa_value
          INTEGER, DIMENSION(4), OPTIONAL,     INTENT(OUT) :: ida_dimsize
          REAL(KIND=4),  DIMENSION(:,:,:,:),POINTER,OPTIONAL :: rdpa_value
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx,ida_indy,ida_indz,ida_indt

          CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
          INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsize
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER :: il_nbdim, il_type
          INTEGER, DIMENSION(4) :: ila_dimids
          INTEGER :: il_file_id, il_var_id, il_status, il_ji
          INTEGER :: il_nx, il_ny, il_nz, il_nt
 
          INTEGER                                         :: il_jj,il_nbatt
          CHARACTER(LEN=255)                              :: cl_fonction,cl_fillval
          CHARACTER(LEN=255),DIMENSION(2)                 :: cla_attname
          CHARACTER(LEN=100)                              :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2),OPTIONAL,INTENT(OUT) :: rda_offsetvalue
          REAL(KIND=4), DIMENSION(2)                     :: rla_offsetvalue_R4
          REAL(KIND=8), DIMENSION(2)                     :: rla_offsetvalue_R8
          INTEGER(KIND=2)                               :: il_fillvaluetmp_I2
          INTEGER(KIND=4)                               :: il_fillvaluetmp
          REAL(KIND=4)                                  :: rl_fillvaluetmp 
          LOGICAL    :: ll_missing_value,ll_fillval  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset

          !
          !*----------------------------------------------------------------------
          !
          cl_fonction='MIOL_readf_field_S_4D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fillval=.FALSE.
	  il_fillvaluetmp_I2=-(((2**16-2))/2)
                
          !----------------------------------------------------------------------
          ! Initialization
 
          il_nx = 1
          il_ny = 1
          il_nz = 1
          il_nt = 1
 
 
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
             ALLOCATE(idpa_value(size(ida_indx(:)),&
                                 size(ida_indy(:)),&
                                 size(ida_indz(:)),&
                                 size(ida_indt(:))),&
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
          ELSE
             ALLOCATE(idpa_value(ila_dimsize(1), &
                                 ila_dimsize(2), &
                                 ila_dimsize(3), &
                                 ila_dimsize(4)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' ila_4D_array',cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz) .AND. PRESENT(ida_indt)) THEN
             il_status =fi_ncError(NF90_GET_VAR(il_file_id, &
                                                il_var_id,&
                                                idpa_value,&
                                                start = (/ida_indx(1),ida_indy(1),ida_indz(1),ida_indt(1)/),&
                                                count= (/size(ida_indx(:)),size(ida_indy(:)),size(ida_indz(:)),&
                                                         size(ida_indt(:))/)),cl_fonction) 
                          
          ELSE
             il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                 il_var_id, &
                                                 idpa_value),cl_fonction)
          ENDIF
           
          !----------------------------------------------------------------------
          ! Memory allocation
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz).AND. PRESENT(ida_indt)) THEN
             ALLOCATE(rdpa_value(size(ida_indx(:)),&
                                 size(ida_indy(:)),&
                                 size(ida_indz(:)),&
                                 size(ida_indt(:))),&
                                 stat=il_status)
             il_status = fi_memError(il_status, 'rdpa_value',cl_fonction)
          ELSE
             ALLOCATE(rdpa_value(ila_dimsize(1), &
                                 ila_dimsize(2), &
                                 ila_dimsize(3), &
                                 ila_dimsize(4)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
          !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          cl_fillval='_FillValue'

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
             CASE('_FillValue')
                ll_fillval = .TRUE.  
             CASE('missing_value')  ! FHZ: MRI files
                ll_missing_value = .TRUE.
             ENDSELECT
          ENDDO
          ! Test sur fillvalue et missing value
          IF ( ll_fillval.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fillval .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fillval ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)

               SELECT CASE (il_type)
                 CASE (NF90_INT)
                 !  teste les missing values en INT !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp)
                   il_fillvaluetmp_I2=INT(il_fillvaluetmp,2)
                 CASE (NF90_SHORT)
                 !  teste les missing values en short !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp_I2)
                 CASE (NF90_FLOAT)
                 !  teste les missing values en FLOAT !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             rl_fillvaluetmp)
                  il_fillvaluetmp_I2=INT(rl_fillvaluetmp,2)
                CASE DEFAULT
                  WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
                 CALL EXIT(3)
                ENDSELECT
            endif ! fin test 
   
          IF (ll_scale_factor .AND. ll_offset) THEN
             !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                     cd_varname,&
                     cla_attname,&
                     rla_offsetvalue_R4)
                WHERE(idpa_value(:,:,:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:,:,:) /= -il_fillvaluetmp_I2)
                   rdpa_value(:,:,:,:)=(idpa_value(:,:,:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:,:,:)=rg_fillvalue
                ENDWHERE
     		IF (PRESENT(rda_offsetvalue))  rda_offsetvalue(:)=REAL(rla_offsetvalue_R4(:),8)
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                                                   cd_varname,&
                                                   cla_attname,&
                                                   rla_offsetvalue_R8)
                WHERE(idpa_value(:,:,:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:,:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:,:,:)=(idpa_value(:,:,:,:)*rla_offsetvalue_R8(1))+rla_offsetvalue_R8(2)
                ELSEWHERE
                   rdpa_value(:,:,:,:)=rg_fillvalue
                ENDWHERE
     		IF (PRESENT(rda_offsetvalue))  rda_offsetvalue(:)=rla_offsetvalue_R8(:)
             ENDSELECT
          ENDIF
          
          !** CLOSE FILE
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
                           
  
     END SUBROUTINE MIOL_readf_field_S_4D_R4_NC
 
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
          !!              The output data are in R8 format
          !!
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:,:,:) :: idpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value       The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !! @param rdpa_value : The 4D data values read and convert in R8 format
          !!
          !! \implements INT_read_write_S
          !!
          !! \n History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  07/2008  (C.Regnier) Add read scale and offset factor
          !!        \n  01/2013  (C.Regnier) V3.5 MIOL
          !<
          !!=====================================================================

       SUBROUTINE MIOL_readf_field_S_4D_R8_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize,&
                                         rdpa_value,&
                                         rda_offsetvalue,&
                                         ida_indx,&
                                         ida_indy,&
                                         ida_indz,&
                                         ida_indt)
 
         Use MFT_error
         USE INT_ATTRIBUTSTYPE
         Use INT_ATTRIBUTS
         Use MIOL_param
         Use netcdf
         IMPLICIT NONE
 
 
         !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=2), DIMENSION(:,:,:,:), POINTER:: idpa_value
          INTEGER, DIMENSION(4), OPTIONAL,     INTENT(OUT) :: ida_dimsize
          REAL(KIND=8),  DIMENSION(:,:,:,:),POINTER,OPTIONAL :: rdpa_value
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx,ida_indy,ida_indz,ida_indt

          CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
          INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsize
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER :: il_nbdim, il_type
          INTEGER, DIMENSION(4) :: ila_dimids
          INTEGER :: il_file_id, il_var_id, il_status, il_ji
          INTEGER :: il_nx, il_ny, il_nz, il_nt
 
          INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction,cl_fillval
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2) ,OPTIONAL,INTENT(OUT) :: rda_offsetvalue
          REAL(KIND=4), DIMENSION(2)                     :: rla_offsetvalue_R4
          REAL(KIND=8), DIMENSION(2)                     :: rla_offsetvalue_R8
          INTEGER(KIND=2)                               :: il_fillvaluetmp_I2
          INTEGER(KIND=4)                               :: il_fillvaluetmp
          REAL(KIND=4)                                  :: rl_fillvaluetmp
          LOGICAL    :: ll_missing_value,ll_fillval  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset

          !
          !*----------------------------------------------------------------------
          !
          cl_fonction='MIOL_readf_field_S_4D_NC'
          !----------------------------------------------------------------------
          ! Initialization
 
          il_nx = 1
          il_ny = 1
          il_nz = 1
          il_nt = 1
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fillval=.FALSE.
	  il_fillvaluetmp_I2=-(((2**16-2))/2)
 
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
              ALLOCATE(idpa_value(size(ida_indx(:)),&
                                  size(ida_indy(:)),&
                                  size(ida_indz(:)),&
                                  size(ida_indt(:)) ),&
                                  stat=il_status)
              il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
           ELSE
              ALLOCATE(idpa_value(ila_dimsize(1), &
                                  ila_dimsize(2), &
                                  ila_dimsize(3), &
                                  ila_dimsize(4)), &
                                  stat=il_status)
              il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
           ENDIF
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz).AND. PRESENT(ida_indt)) THEN
              il_status =fi_ncError(NF90_GET_VAR(il_file_id, &
                                                 il_var_id,&
                                                 idpa_value,&
                                                 start = (/ida_indx(1),ida_indy(1),ida_indz(1),ida_indt(1)/),&
                                                 count= (/size(ida_indx(:)),size(ida_indy(:)),size(ida_indz(:)),&
                                                         size(ida_indt(:))/)),cl_fonction) 
           ELSE
              il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                  il_var_id, &
                                                  idpa_value),cl_fonction)
           ENDIF
          !----------------------------------------------------------------------
          ! Memory allocation
           IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz).AND. PRESENT(ida_indt)) THEN
              ALLOCATE(rdpa_value(size(ida_indx(:)),&
                                  size(ida_indy(:)),&
                                  size(ida_indz(:)),&
                                  size(ida_indt(:)) ),&
                                  stat=il_status)
              il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
           ELSE
              ALLOCATE(rdpa_value(ila_dimsize(1), &
                                  ila_dimsize(2), &
                                  ila_dimsize(3), &
                                  ila_dimsize(4)), &
                                  stat=il_status)
              il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
           ENDIF
           !----------------------------------------------------------------------
           ! Complete the correct lenght of dimensions
           
           IF (PRESENT(ida_dimsize)) THEN
              ida_dimsize = SHAPE(idpa_value)
           ENDIF
          !----------------------------------------------------------------------
          !* If scale factor compute

          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'

          cl_fillval='_FillValue'

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
          CASE('_FillValue')
             ll_fillval = .TRUE.  
          CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          ENDSELECT
          ENDDO
         
          ! Test sur fillvalue et missing value
          IF ( ll_fillval.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fillval .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fillval ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)

               SELECT CASE (il_type)
                 CASE (NF90_INT)
                 !  teste les missing values en INT !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp)
                   il_fillvaluetmp_I2=INT(il_fillvaluetmp,2)
                 CASE (NF90_SHORT)
                 !  teste les missing values en short !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp_I2)
                 CASE (NF90_FLOAT)
                 !  teste les missing values en FLOAT !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             rl_fillvaluetmp)
                  il_fillvaluetmp_I2=INT(rl_fillvaluetmp,2)
                CASE DEFAULT
                  WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
                 CALL EXIT(3)
                ENDSELECT
          endif ! fin test 
   
          IF (ll_scale_factor .AND. ll_offset) THEN
              !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                                                   cd_varname,&
                                                   cla_attname,&
                                                   rla_offsetvalue_R4)
                WHERE(idpa_value(:,:,:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:,:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:,:,:)=(idpa_value(:,:,:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:,:,:)=rg_fillvalue_R8
                ENDWHERE
                IF (PRESENT(rda_offsetvalue))  rda_offsetvalue(:)=REAL(rla_offsetvalue_R4(:),8) 
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                                                   cd_varname,&
                                                   cla_attname,&
                                                    rla_offsetvalue_R8)
                WHERE(idpa_value(:,:,:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:,:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:,:,:)=(idpa_value(:,:,:,:)*rla_offsetvalue_R8(1))+rla_offsetvalue_R8(2)
                ELSEWHERE
                   rdpa_value(:,:,:,:)=rg_fillvalue_R8
                ENDWHERE
                IF (PRESENT(rda_offsetvalue))  rda_offsetvalue(:)=rla_offsetvalue_R8(:) 
          ENDSELECT
          ENDIF

          !** CLOSE FILE
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
  
     END SUBROUTINE MIOL_readf_field_S_4D_R8_NC
 
 
 
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
          !!              The output data are in R4 format
          !!
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:,:) :: idpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !! @param rdpa_value : The 3D data values read and convert in R4 format
          !!
          !! \implements INT_read_write_S
          !!
          !!  \n History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  07/2008  (C.Regnier) Add read scale and offset factor
          !!        \n  01/2013  (C.Regnier) V3.5 MIOL
          !<
          !!=====================================================================
 
       SUBROUTINE MIOL_readf_field_S_3D_R4_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize,&
                                         rdpa_value,&
                                         rda_offsetvalue,&
                                         ida_indx,&
                                         ida_indy,&
                                         ida_indz)
 
         Use MFT_error
         USE INT_ATTRIBUTSTYPE      
         Use MFT_Inf_NaN_Detection
         Use INT_ATTRIBUTS
         Use MIOL_param
         Use netcdf
         IMPLICIT NONE
 
 
         !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=2), DIMENSION(:,:,:), POINTER:: idpa_value
          INTEGER, DIMENSION(3), OPTIONAL,     INTENT(OUT) :: ida_dimsize
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx,ida_indy,ida_indz
          REAL(KIND=4),  DIMENSION(:,:,:),POINTER,OPTIONAL :: rdpa_value

          CHARACTER(LEN=255), DIMENSION(3) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(3) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_file_id, il_var_id, il_status, il_ji
          INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction,cl_fillval
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2),OPTIONAL,INTENT(OUT)  :: rda_offsetvalue
          REAL(KIND=4), DIMENSION(2)                     :: rla_offsetvalue_R4
          REAL(KIND=8), DIMENSION(2)                     :: rla_offsetvalue_R8
          INTEGER(KIND=2)                               :: il_fillvaluetmp_I2  
          INTEGER(KIND=4)                               :: il_fillvaluetmp  
          REAL(KIND=4)                                  :: rl_fillvaluetmp
          LOGICAL    :: ll_missing_value,ll_fillval  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset
          !*----------------------------------------------------------------------
          !
          cl_fonction ='MIOL_readf_field_S_3D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fillval=.FALSE.
	  il_fillvaluetmp_I2=-(((2**16-2))/2)

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
             WRITE(0,*) "MIOL_read_field_S_4D_NC: array dimension error. "
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
             ALLOCATE(idpa_value(size(ida_indx(:)),&
                                 size(ida_indy(:)),&
                                 size(ida_indz(:))),&
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
             
          ELSE
             ALLOCATE(idpa_value(ila_dimsize(1), &
                                 ila_dimsize(2), &
                                 ila_dimsize(3)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
             
             ALLOCATE(rdpa_value(ila_dimsize(1), &
                                 ila_dimsize(2), &
                                 ila_dimsize(3)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz)) THEN
             il_status =fi_ncError(NF90_GET_VAR(il_file_id, &
                                                il_var_id,&
                                                idpa_value,&
                                                start = (/ida_indx(1),ida_indy(1),ida_indz(1)/),&
                                                count= (/size(ida_indx(:)),size(ida_indy(:)),size(ida_indz(:))/)),cl_fonction) 
          ELSE
             il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                 il_var_id, &
                                                 idpa_value),cl_fonction)
             
          ENDIF
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
          
          !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          cl_fillval='_FillValue'
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
          CASE('_FillValue')
             ll_fillval = .TRUE.  
          CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          ENDSELECT
          ENDDO
          ! Test sur fillvalue et missing value
          IF ( ll_fillval.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fillval .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fillval ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)

               SELECT CASE (il_type)
                 CASE (NF90_INT)
                 !  teste les missing values en INT !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp)
                   il_fillvaluetmp_I2=INT(il_fillvaluetmp,2)
                 CASE (NF90_SHORT)
                 !  teste les missing values en short !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp_I2)
                 CASE (NF90_FLOAT)
                 !  teste les missing values en FLOAT !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             rl_fillvaluetmp)
                  il_fillvaluetmp_I2=INT(rl_fillvaluetmp,2)
                CASE DEFAULT
                  WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
                 CALL EXIT(3)
                ENDSELECT
          endif ! fin test 
          IF (ll_scale_factor .AND. ll_offset) THEN
             !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                     cd_varname,&
                     cla_attname,&
                     rla_offsetvalue_R4)
                WHERE(idpa_value(:,:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:,:)=(idpa_value(:,:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:,:)=rg_fillvalue
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=REAL(rla_offsetvalue_R4(:),8)
              CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                                                cd_varname,&
                                                cla_attname,&
                                                rla_offsetvalue_R8)
                WHERE(idpa_value(:,:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:,:)=(idpa_value(:,:,:)*rla_offsetvalue_R8(1))+rla_offsetvalue_R8(2)
                ELSEWHERE
                   rdpa_value(:,:,:)=rg_fillvalue
                ENDWHERE
	        IF (PRESENT(rda_offsetvalue))  rda_offsetvalue(:)=rla_offsetvalue_R8(:)
          ENDSELECT
       ENDIF

          !** CLOSE FILE
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
          
        END SUBROUTINE MIOL_readf_field_S_3D_R4_NC
 
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
          !!              The output data are in R8 format
          !!
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:,:) :: idpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !! @param rdpa_value : The 3D data values read and convert in R8 format
          !!
          !! \implements INT_read_write_S
          !!
          !! \n History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  07/2008  (C.Regnier) Add read scale and offset factor
          !!        \n  01/2013  (C.Regnier) V3.5 MIOL
          !<
          !!=====================================================================
 
 
        SUBROUTINE MIOL_readf_field_S_3D_R8_NC (cd_filename, &
                                                cd_varname, &
           				        idpa_value, &
           					ida_dimsize,&
          				        rdpa_value,&
                                                rda_offsetvalue,&
                                                ida_indx,&
                                                ida_indy,&
                                                ida_indz)

          Use MFT_error
          USE INT_ATTRIBUTSTYPE
          Use MIOL_param
          Use INT_ATTRIBUTS
          Use netcdf
          IMPLICIT NONE

          !----------------------------------------------------------------------

          CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=2), DIMENSION(:,:,:), POINTER :: idpa_value
          INTEGER, DIMENSION(3), OPTIONAL,     INTENT(OUT) :: ida_dimsize
          REAL(KIND=8),  DIMENSION(:,:,:),POINTER,OPTIONAL :: rdpa_value
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx,ida_indy,ida_indz
          
          CHARACTER(LEN=255), DIMENSION(3) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(3) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_file_id, il_var_id, il_status, il_ji
          INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction,cl_fillval
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2),OPTIONAL,INTENT(OUT) :: rda_offsetvalue
          REAL(KIND=4), DIMENSION(2)                     :: rla_offsetvalue_R4
          REAL(KIND=8), DIMENSION(2)                     :: rla_offsetvalue_R8
          INTEGER(KIND=2)                               :: il_fillvaluetmp_I2
          INTEGER(KIND=4)                               :: il_fillvaluetmp
          REAL(KIND=4)                                  :: rl_fillvaluetmp  
          LOGICAL    :: ll_missing_value,ll_fillval  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset
          !*----------------------------------------------------------------------
          !
	  cl_fonction ='MIOL_readf_field_S_3D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fillval=.FALSE.
	  il_fillvaluetmp_I2=-(((2**16-2))/2)

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
             WRITE(0,*) "MIOL_read_field_S_4D_NC: array dimension error. "
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
             ALLOCATE(idpa_value(size(ida_indx(:)),&
                                 size(ida_indy(:)),&
                                 size(ida_indz(:))),&
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
          ELSE
             ALLOCATE(idpa_value(ila_dimsize(1), &
                                 ila_dimsize(2), &
                                 ila_dimsize(3)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
             
             ALLOCATE(rdpa_value(ila_dimsize(1), &
                                 ila_dimsize(2), &
                                 ila_dimsize(3)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Read variable Id

          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
               cd_varname, &
               il_var_id),cl_fonction)

          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz)) THEN
             il_status =fi_ncError(NF90_GET_VAR(il_file_id, &
                                                il_var_id,&
                                                idpa_value,&
                                                start = (/ida_indx(1),ida_indy(1),ida_indz(1)/),&
                                                count= (/size(ida_indx(:)),size(ida_indy(:)),size(ida_indz(:))/)),cl_fonction) 
             
          ELSE
             il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                 il_var_id, &
                                                 idpa_value),cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions

          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF

          !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          cl_fillval='_FillValue'
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
             CASE('_FillValue')
                ll_fillval = .TRUE.  
             CASE('missing_value')  ! FHZ: MRI files
                ll_missing_value = .TRUE.
             ENDSELECT
          ENDDO
          ! Test sur fillvalue et missing value
          IF ( ll_fillval.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fillval .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fillval ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)

               SELECT CASE (il_type)
                 CASE (NF90_INT)
                 !  teste les missing values en INT !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp)
                   il_fillvaluetmp_I2=INT(il_fillvaluetmp,2)
                 CASE (NF90_SHORT)
                 !  teste les missing values en short !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp_I2)
                 CASE (NF90_FLOAT)
                 !  teste les missing values en FLOAT !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             rl_fillvaluetmp)
                  il_fillvaluetmp_I2=INT(rl_fillvaluetmp,2)
                CASE DEFAULT
                  WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
                 CALL EXIT(3)
                ENDSELECT
          endif ! fin test 
          IF (ll_scale_factor .AND. ll_offset) THEN
             !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                  il_var_id, &
                  TRIM(cla_attname(1)), &
                  il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                     cd_varname,&
                     cla_attname,&
                     rla_offsetvalue_R4)
                WHERE(idpa_value(:,:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:,:)=(idpa_value(:,:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:,:)=rg_fillvalue_R8
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=REAL(rla_offsetvalue_R4(:),8)                                                                                                
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                     cd_varname,&
                     cla_attname,&
                     rla_offsetvalue_R8)
                WHERE(idpa_value(:,:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:,:)=(idpa_value(:,:,:)*rla_offsetvalue_R8(1))+rla_offsetvalue_R8(2)
                ELSEWHERE
                   rdpa_value(:,:,:)=rg_fillvalue_R8
                ENDWHERE
		IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=rla_offsetvalue_R8(:)
             CASE DEFAULT
                WRITE(0,*) ' Type de l attribut non trouv�'
                CALL flush(0)
                STOP
             ENDSELECT

          ENDIF

          !** CLOSE FILE
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)

        END SUBROUTINE MIOL_readf_field_S_3D_R8_NC
 
 
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
          !!              The output data are in R4 format
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:) :: idpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !! @param rdpa_value : The 2D data values read and convert in R4 format
          !!
          !! \implements INT_read_write_S
          !!
          !! \n History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  07/2008  (C.Regnier) Add read scale and offset factor
          !!        \n  01/2013  (C.Regnier) V3.5 MIOL
          !<
          !!===================================================================== 
 
       SUBROUTINE MIOL_readf_field_S_2D_R4_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize,&
                                         rdpa_value,&
                                         rda_offsetvalue,&
                                         ida_indx,&
                                         ida_indy)
 
         USE MFT_error
         USE INT_ATTRIBUTSTYPE
         USE MIOL_param
         USE INT_ATTRIBUTS
         USE netcdf
         IMPLICIT NONE
 
          !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=2), DIMENSION(:,:), POINTER      :: idpa_value
          REAL(KIND=4),  DIMENSION(:,:),POINTER,OPTIONAL :: rdpa_value
          INTEGER, DIMENSION(2), OPTIONAL,     INTENT(OUT) :: ida_dimsize
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx,ida_indy
           
          CHARACTER(LEN=255), DIMENSION(2) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(2) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_file_id, il_var_id, il_status, il_ji,il_nbatt,il_jj
          CHARACTER(LEN=255)                            :: cl_fonction,cl_fillval
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2),OPTIONAL,INTENT(OUT) :: rda_offsetvalue
          REAL(KIND=4), DIMENSION(2)                      :: rla_offsetvalue_R4
          REAL(KIND=8), DIMENSION(2)                      :: rla_offsetvalue_R8
          REAL(KIND=4)                                    :: rl_fillvaluetmp
          INTEGER(KIND=4)                               :: il_fillvaluetmp  
          INTEGER(KIND=2)                               :: il_fillvaluetmp_I2  
          LOGICAL    :: ll_missing_value,ll_fillval  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset
          !
          !*----------------------------------------------------------------------
          !
	  !il_fillvalue=-(((2**16-2))/2)
    	  cl_fonction ='MIOL_readf_field_S_2D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fillval=.FALSE.
          il_fillvaluetmp_I2=-(((2**16-2))/2)
 
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
             WRITE(0,*) "MIOL_read_field_S_2D_NC: array dimension error. "
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
             ALLOCATE(idpa_value(size(ida_indx(:)),&
                                 size(ida_indy(:))),&
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
          ELSE
             ALLOCATE(idpa_value(ila_dimsize(1), &
                                 ila_dimsize(2)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
             
             ALLOCATE(rdpa_value(ila_dimsize(1), &
                                 ila_dimsize(2)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy)) THEN
             il_status =fi_ncError(NF90_GET_VAR(il_file_id, &
                                                il_var_id,&
                                                idpa_value,&
                                                start = (/ida_indx(1),ida_indy(1)/),&
                                                count= (/size(ida_indx(:)),size(ida_indy(:))/)),cl_fonction) 
             
          ELSE
             il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                 il_var_id, &
                                                 idpa_value),cl_fonction)
             
          ENDIF
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
          
          !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          cl_fillval='_FillValue'
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
          CASE('_FillValue')
             ll_fillval = .TRUE.  
          CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          ENDSELECT
          ENDDO
          ! Test sur fillvalue et missing value
          IF ( ll_fillval.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fillval .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fillval ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)

               SELECT CASE (il_type)
                 CASE (NF90_INT)
                 !  teste les missing values en INT !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp)
                   il_fillvaluetmp_I2=INT(il_fillvaluetmp,2)
                 CASE (NF90_SHORT)
                 !  teste les missing values en short !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp_I2)
                 CASE (NF90_FLOAT)
                 !  teste les missing values en FLOAT !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             rl_fillvaluetmp)
                  il_fillvaluetmp_I2=INT(rl_fillvaluetmp,2)
                CASE DEFAULT
                  WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
                 CALL EXIT(3)
                ENDSELECT
          endif ! fin test 
          IF (ll_scale_factor .AND. ll_offset) THEN
             !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                     cd_varname,&
                     cla_attname,&
                     rla_offsetvalue_R4)
                WHERE(idpa_value(:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:)=(idpa_value(:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:)=rg_fillvalue
                ENDWHERE
                IF (PRESENT(rda_offsetvalue))  rda_offsetvalue(:)=REAL(rla_offsetvalue_R4(:),8)    
            CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                                                cd_varname,&
                                                cla_attname,&
                                                rla_offsetvalue_R8 )
                WHERE(idpa_value(:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:)=(idpa_value(:,:)*rla_offsetvalue_R8(1))+rla_offsetvalue_R8(2)
                ELSEWHERE
                   rdpa_value(:,:)=rg_fillvalue
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=rla_offsetvalue_R8(:)
          ENDSELECT
          ENDIF      
          !** CLOSE FILE
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
          
        END SUBROUTINE MIOL_readf_field_S_2D_R4_NC
 
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
          !!              The output data are in R8 format
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:) :: idpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !! @param rdpa_value : The 2D data values read and convert in R8 format
          !!
          !! \implements INT_read_write_S
          !!
          !! \n History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  07/2008  (C.Regnier) Add read scale and offset factor
          !!        \n  01/2013  (C.Regnier) V3.5 MIOL
          !<
          !!=====================================================================
 
 
       SUBROUTINE MIOL_readf_field_S_2D_R8_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize,&
                                         rdpa_value,&
                                         rda_offsetvalue,&
                                         ida_indx,&
                                         ida_indy)
                                         
 
         USE MFT_error
         USE MIOL_param
         USE INT_ATTRIBUTSTYPE
         USE INT_ATTRIBUTS
         USE netcdf
         IMPLICIT NONE
 
 
           !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=2), DIMENSION(:,:), POINTER :: idpa_value
          INTEGER, DIMENSION(2), OPTIONAL,     INTENT(OUT) :: ida_dimsize
          REAL(KIND=8),  DIMENSION(:,:),POINTER,OPTIONAL :: rdpa_value
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx,ida_indy
                     
          CHARACTER(LEN=255), DIMENSION(2) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(2) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_file_id, il_var_id, il_status, il_ji,il_nbatt,il_jj
          CHARACTER(LEN=255)                            :: cl_fonction,cl_fillval
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2),OPTIONAL,INTENT(OUT) :: rda_offsetvalue
          REAL(KIND=4), DIMENSION(2)                      :: rla_offsetvalue_R4
          REAL(KIND=8), DIMENSION(2)                     :: rla_offsetvalue_R8
          REAL(KIND=4)                                    :: rl_fillvaluetmp
          INTEGER(KIND=2)                                 :: il_fillvaluetmp_I2
          INTEGER(KIND=4)                               :: il_fillvaluetmp  
          LOGICAL    :: ll_missing_value,ll_fillval  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset
          !
          !*----------------------------------------------------------------------
          !
	  il_fillvaluetmp_I2=-(((2**16-2))/2)
      	  cl_fonction ='MIOL_readf_field_S_2D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fillval=.FALSE.
          il_fillvaluetmp_I2=-(((2**16-2))/2)

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
             WRITE(0,*) "MIOL_read_field_S_2D_NC: array dimension error. "
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
              ALLOCATE(idpa_value(size(ida_indx(:)),&
                                  size(ida_indy(:))),&
                                  stat=il_status)
              il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
           ELSE
              ALLOCATE(idpa_value(ila_dimsize(1), &
                                  ila_dimsize(2)), &
                                  stat=il_status)
              il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
              ALLOCATE(rdpa_value(ila_dimsize(1), &
                                  ila_dimsize(2)), &
                                  stat=il_status)
              il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
           ENDIF
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy)) THEN
             il_status =fi_ncError(NF90_GET_VAR(il_file_id, &
                                                il_var_id,&
                                                idpa_value,&
                                                start = (/ida_indx(1),ida_indy(1)/),&
                                                count= (/size(ida_indx(:)),size(ida_indy(:))/)),cl_fonction) 
          ELSE
             il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                 il_var_id, &
                                                 idpa_value),cl_fonction)
             
          ENDIF
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
          
          !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          cl_fillval='_FillValue'
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
          CASE('_FillValue')
             ll_fillval = .TRUE.  
          CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          ENDSELECT
          ENDDO
          ! Test sur fillvalue et missing value
          IF ( ll_fillval.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fillval .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fillval ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)

               SELECT CASE (il_type)
         	 CASE (NF90_INT)
            	 !  teste les missing values en INT !
        	 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp)
      	 	   il_fillvaluetmp_I2=INT(il_fillvaluetmp,2)
       	         CASE (NF90_SHORT)
                 !  teste les missing values en short !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp_I2)
     		 CASE (NF90_FLOAT)
  	         !  teste les missing values en FLOAT !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             rl_fillvaluetmp)
                  il_fillvaluetmp_I2=INT(rl_fillvaluetmp,2)
                CASE DEFAULT
                  WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
                 CALL EXIT(3)
                ENDSELECT
          endif ! fin test 

          IF (ll_scale_factor .AND. ll_offset) THEN
             !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                     cd_varname,&
                     cla_attname,&
                     rla_offsetvalue_R4)
                WHERE(idpa_value(:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:)=(idpa_value(:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:)=rg_fillvalue_R8
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=REAL(rla_offsetvalue_R4(:),8) 
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                                                cd_varname,&
                                                cla_attname,&
                                                rla_offsetvalue_R8)
                WHERE(idpa_value(:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:)=(idpa_value(:,:)*rla_offsetvalue_R8(1))+rla_offsetvalue_R8(2)
                ELSEWHERE
                   rdpa_value(:,:)=rg_fillvalue_R8
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=rla_offsetvalue_R8(:)
          ENDSELECT
         ENDIF

          !** CLOSE FILE
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)

        END SUBROUTINE MIOL_readf_field_S_2D_R8_NC
   
  
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
          !!              The output data are in R4 format
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:) :: idpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !! @param rdpa_value : The 1D data values read and convert in R4 format
          !!
          !! \implements INT_read_write_S
          !!
          !! \n History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  07/2008  (C.Regnier) Add read scale and offset factor
          !!        \n  01/2013  (C.Regnier) V3.5 MIOL
          !<
          !!=====================================================================
  
          SUBROUTINE MIOL_readf_field_S_1D_R4_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize,&
                                         rdpa_value,&
                                         rda_offsetvalue,&
                                         ida_indx)

 
         USE MFT_error
         USE INT_ATTRIBUTSTYPE
         USE MIOL_param
         USE INT_ATTRIBUTS
         USE netcdf
         IMPLICIT NONE
 
          !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=2), DIMENSION(:), POINTER :: idpa_value
          INTEGER, DIMENSION(1), OPTIONAL,     INTENT(OUT) :: ida_dimsize
          REAL(KIND=4),  DIMENSION(:),POINTER,OPTIONAL :: rdpa_value
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx

          CHARACTER(LEN=255), DIMENSION(1) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(1) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_file_id, il_var_id, il_status, il_ji
          INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction,cl_fillval
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2),OPTIONAL,INTENT(OUT) :: rda_offsetvalue
          REAL(KIND=4), DIMENSION(2)                    :: rla_offsetvalue_R4
          REAL(KIND=8), DIMENSION(2)                    :: rla_offsetvalue_R8
          REAL(KIND=4)                                  :: rl_fillvaluetmp
          INTEGER(KIND=2)                               :: il_fillvaluetmp_I2
          INTEGER(KIND=4)                               :: il_fillvaluetmp  
          LOGICAL    :: ll_missing_value,ll_fillval  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset
          !
          !*----------------------------------------------------------------------
          !
          cl_fonction = 'MIOL_readf_field_S_1D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fillval=.FALSE.
          il_fillvaluetmp_I2=-(((2**16-2))/2)

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
             WRITE(0,*) "MIOL_read_field_S_1D_NC: array dimension error. "
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
             ALLOCATE(idpa_value(size(ida_indx(:))),&                        
                                      stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
          ELSE
             ALLOCATE(idpa_value(ila_dimsize(1)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
             ALLOCATE(rdpa_value(ila_dimsize(1)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx)) THEN
             il_status =fi_ncError(NF90_GET_VAR(il_file_id, &
                                                il_var_id,&
                                                idpa_value,&
                                                start = (/ida_indx(1)/),&
                                                count= (/size(ida_indx(:))/)),cl_fonction) 
          ELSE
             il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                 il_var_id, &
                                                 idpa_value),cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
          
          !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          cl_fillval='_FillValue'
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
          CASE('_FillValue')
             ll_fillval = .TRUE.  
          CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          ENDSELECT
          ENDDO
          ! Test sur fillvalue et missing value
          IF ( ll_fillval.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fillval .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fillval ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)

               SELECT CASE (il_type)
                 CASE (NF90_INT)
                 !  teste les missing values en INT !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp)
                   il_fillvaluetmp_I2=INT(il_fillvaluetmp,2)
                 CASE (NF90_SHORT)
                 !  teste les missing values en short !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp_I2)
                 CASE (NF90_FLOAT)
                 !  teste les missing values en FLOAT !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             rl_fillvaluetmp)
                  il_fillvaluetmp_I2=INT(rl_fillvaluetmp,2)
                CASE DEFAULT
                  WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
                 CALL EXIT(3)
                ENDSELECT
          endif ! fin test 
          IF (ll_scale_factor .AND. ll_offset) THEN
              !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                     cd_varname,&
                     cla_attname,&
                     rla_offsetvalue_R4)
                WHERE(idpa_value(:) /= il_fillvaluetmp_I2 .AND. idpa_value(:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:)=(idpa_value(:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:)=rg_fillvalue
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=REAL(rla_offsetvalue_R4(:),8)
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                                                cd_varname,&
                                                cla_attname,&
                                                rla_offsetvalue_R8)
                WHERE(idpa_value(:) /= il_fillvaluetmp_I2 .AND. idpa_value(:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:)=(idpa_value(:)*rla_offsetvalue_R8(1))+rla_offsetvalue_R8(2)
                ELSEWHERE
                   rdpa_value(:)=rg_fillvalue
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=rla_offsetvalue_R8
          ENDSELECT
          ENDIF

          !** CLOSE FILE
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
  
 
        END SUBROUTINE MIOL_readf_field_S_1D_R4_NC

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
          !!              The output data are in R8 format
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:) :: idpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !!
          !! @param rdpa_value : The 1D data values read and convert in R8 format
          !!
          !! \implements INT_read_write_S
          !!
          !! \n History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  07/2008  (C.Regnier) Add read scale and offset factor
          !!        \n  01/2013  (C.Regnier) V3.5 MIOL
          !<
          !!=====================================================================
 
       SUBROUTINE MIOL_readf_field_S_1D_R8_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize,&
                                         rdpa_value,&
                                         rda_offsetvalue,&
                                          ida_indx)
 
         USE MFT_error
         USE MIOL_param
         USE INT_ATTRIBUTSTYPE
         USE INT_ATTRIBUTS
         USE netcdf
         IMPLICIT NONE
 
          !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=2), DIMENSION(:), POINTER :: idpa_value
          INTEGER, DIMENSION(1), OPTIONAL,     INTENT(OUT) :: ida_dimsize
          REAL(KIND=8),  DIMENSION(:),POINTER,OPTIONAL :: rdpa_value
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx

          CHARACTER(LEN=255), DIMENSION(1) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(1) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_file_id, il_var_id, il_status, il_ji
          INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction,cl_fillval
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2),OPTIONAL,INTENT(OUT) :: rda_offsetvalue
          REAL(KIND=4), DIMENSION(2)                      :: rla_offsetvalue_R4
          REAL(KIND=8), DIMENSION(2)                      :: rla_offsetvalue_R8
          REAL(KIND=4)                                  :: rl_fillvaluetmp
          INTEGER(KIND=2)                               :: il_fillvaluetmp_I2
          INTEGER(KIND=4)                               :: il_fillvaluetmp  

          LOGICAL    :: ll_missing_value,ll_fillval  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset
          !
          !*----------------------------------------------------------------------
          !
          cl_fonction = 'MIOL_readf_field_S_1D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fillval=.FALSE.
          il_fillvaluetmp_I2=-(((2**16-2))/2)

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
             WRITE(0,*) "MIOL_read_field_S_1D_NC: array dimension error. "
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
             ALLOCATE(idpa_value(size(ida_indx(:))),&                        
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
          ELSE
             ALLOCATE(idpa_value(ila_dimsize(1)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
             ALLOCATE(rdpa_value(ila_dimsize(1)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx)) THEN
             il_status =fi_ncError(NF90_GET_VAR(il_file_id, &
                                                il_var_id,&
                                                idpa_value,&
                                                start = (/ida_indx(1)/),&
                                                count= (/size(ida_indx(:))/)),cl_fonction) 
          ELSE
             il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                 il_var_id, &
                                                 idpa_value),cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
          
          !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          cl_fillval='_FillValue'
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
          CASE('_FillValue')
             ll_fillval = .TRUE.  
          CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          ENDSELECT
          ENDDO
          ! Test sur fillvalue et missing value
          IF ( ll_fillval.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fillval .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fillval ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)

               SELECT CASE (il_type)
                 CASE (NF90_INT)
                 !  teste les missing values en INT !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp)
                   il_fillvaluetmp_I2=INT(il_fillvaluetmp,2)
                 CASE (NF90_SHORT)
                 !  teste les missing values en short !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp_I2)
                 CASE (NF90_FLOAT)
                 !  teste les missing values en FLOAT !
                 CALL MIOL_read_attribute_NC(il_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             rl_fillvaluetmp)
                  il_fillvaluetmp_I2=INT(rl_fillvaluetmp,2)
                CASE DEFAULT
                  WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
                 CALL EXIT(3)
                ENDSELECT
          endif ! fin test 
          IF (ll_scale_factor .AND. ll_offset) THEN
              !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                     cd_varname,&
                     cla_attname,&
                     rla_offsetvalue_R4)
                WHERE(idpa_value(:) /= il_fillvaluetmp_I2 .AND. idpa_value(:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:)=(idpa_value(:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:)=rg_fillvalue_R8
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=REAL(rla_offsetvalue_R4(:),8)
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(il_file_id,&
                                                cd_varname,&
                                                cla_attname,&
                                                rla_offsetvalue_R8)
                WHERE(idpa_value(:) /= il_fillvaluetmp_I2 .AND. idpa_value(:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:)=(idpa_value(:)*rla_offsetvalue_R8(1))+rla_offsetvalue_R8(2)
                ELSEWHERE
                   rdpa_value(:)=rg_fillvalue_R8
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=rla_offsetvalue_R8
         ENDSELECT
       ENDIF


          !** CLOSE FILE
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
        END SUBROUTINE MIOL_readf_field_S_1D_R8_NC
 
 
   !******************************************************************************
  !****************************************************************************
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
          !!              The output data are in R4 format
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:,:,:) :: idpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value       The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !! @param rdpa_value : The 4D data values read and convert in R4 format
          !!
          !! \implements INT_read_write_S
          !!
          !! \n History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  07/2008  (C.Regnier) Add read scale and offset factor
          !!        \n  07/2009  (C.Regnier) On enleve le select case
          !!        \n  01/2013  (C.Regnier) V3.5 MIOL
          !<
          !!===================================================================== 
 
       SUBROUTINE MIOL_readu_field_S_4D_R4_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize,&
                                         rdpa_value,&
                                         rda_offsetvalue,&
                                         ida_indx,&
                                         ida_indy,&
                                         ida_indz,&
                                         ida_indt)
 
         USE MFT_error
         USE MIOL_param
         USE netcdf
         USE INT_ATTRIBUTSTYPE
         Use MFT_Inf_NaN_Detection
         USE MIOL_param
         USE INT_ATTRIBUTS
         IMPLICIT NONE
 
          !----------------------------------------------------------------------
 
          INTEGER,                           INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=2), DIMENSION(:,:,:,:), POINTER :: idpa_value
          INTEGER, DIMENSION(4), OPTIONAL,     INTENT(OUT) :: ida_dimsize
          REAL(KIND=4), DIMENSION(:,:,:,:),POINTER         :: rdpa_value
          REAL(KIND=8), DIMENSION(2),OPTIONAL,INTENT(OUT) :: rda_offsetvalue
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx,ida_indy,ida_indz,ida_indt

          CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
          INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsize
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER :: il_nbdim, il_type
          INTEGER, DIMENSION(4) :: ila_dimids
          INTEGER :: il_var_id, il_status, il_ji
          INTEGER :: il_nx, il_ny, il_nz, il_nt
 
          INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction,cl_fillval
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=4), DIMENSION(2)                     :: rla_offsetvalue_R4
          REAL(KIND=8), DIMENSION(2)                     :: rla_offsetvalue_R8
          REAL(KIND=4)                                  :: rl_fillvaluetmp
          INTEGER(KIND=2)                               :: il_fillvaluetmp_I2
          INTEGER(KIND=4)                               :: il_fillvaluetmp  
          LOGICAL    :: ll_missing_value,ll_fillval  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset
          !*----------------------------------------------------------------------
          !
          cl_fonction ='MIOL_readu_field_S_4D_NC'
          !----------------------------------------------------------------------
          ! Initialization
 
          il_nx = 1
          il_ny = 1
          il_nz = 1
          il_nt = 1
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fillval=.FALSE.
          il_fillvaluetmp_I2=-(((2**16-2))/2)
  
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
              ALLOCATE(idpa_value(size(ida_indx(:)),&
                                  size(ida_indy(:)),&
                                  size(ida_indz(:)),&
                                  size(ida_indt(:)) ),&
                                  stat=il_status)
              il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
           ELSE
              ALLOCATE(idpa_value(ila_dimsize(1), &
                                  ila_dimsize(2), &
                                  ila_dimsize(3), &
                                  ila_dimsize(4)),stat=il_status)
              il_status = fi_memError(il_status, 'idpa_value',cl_fonction)
           ENDIF
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz).AND. PRESENT(ida_indt)) THEN
              il_status =fi_ncError(NF90_GET_VAR(id_file_id, &
                                                 il_var_id,&
                                                 idpa_value,&
                                                 start = (/ida_indx(1),ida_indy(1),ida_indz(1),ida_indt(1)/),&
                                                 count= (/size(ida_indx(:)),size(ida_indy(:)),size(ida_indz(:)),&
                                                         size(ida_indt(:))/)),cl_fonction) 
           ELSE
              il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                                  il_var_id, &
                                                  idpa_value),cl_fonction)
           ENDIF
          !----------------------------------------------------------------------
          ! Memory allocation
            IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz).AND. PRESENT(ida_indt)) THEN
              ALLOCATE(rdpa_value(size(ida_indx(:)),&
                                  size(ida_indy(:)),&
                                  size(ida_indz(:)),&
                                  size(ida_indt(:)) ),&
                                  stat=il_status)
              il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
           ELSE
              ALLOCATE(rdpa_value(ila_dimsize(1), &
                                  ila_dimsize(2), &
                                  ila_dimsize(3), &
                                  ila_dimsize(4)),stat=il_status)
              il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
           ENDIF
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
          
           !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          cl_fillval='_FillValue'
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
          CASE('_FillValue')
             ll_fillval = .TRUE.  
          CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          ENDSELECT
          ENDDO
           ! Test sur fillvalue et missing value
          IF ( ll_fillval.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fillval .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fillval ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)

               SELECT CASE (il_type)
                 CASE (NF90_INT)
                 !  teste les missing values en INT !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp)
                   il_fillvaluetmp_I2=INT(il_fillvaluetmp,2)
                 CASE (NF90_SHORT)
                 !  teste les missing values en short !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp_I2)
                 CASE (NF90_FLOAT)
                 !  teste les missing values en FLOAT !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             rl_fillvaluetmp)
                  il_fillvaluetmp_I2=INT(rl_fillvaluetmp,2)
                CASE DEFAULT
                  WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
                 CALL EXIT(3)
                ENDSELECT
          endif ! fin test 
          IF (ll_scale_factor .AND. ll_offset) THEN
              !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
               CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                     cd_varname,&
                     cla_attname,&
                     rla_offsetvalue_R4)
                WHERE(idpa_value(:,:,:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:,:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:,:,:)=(idpa_value(:,:,:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:,:,:)=rg_fillvalue
                ENDWHERE 
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=REAL(rla_offsetvalue_R4(:),8)
                !** Remove the NaN values
                !WHERE( isnan(rdpa_value) ) rdpa_value(:,:,:,:)=rg_fillvalue
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                                                cd_varname,&
                                                cla_attname,&
                                                rla_offsetvalue_R8)
                WHERE(idpa_value(:,:,:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:,:,:) /= -il_fillvaluetmp_I2 ) 
                  rdpa_value(:,:,:,:)=(idpa_value(:,:,:,:)*rla_offsetvalue_R8(1))+rla_offsetvalue_R8(2)
                ELSEWHERE
                  rdpa_value(:,:,:,:)=rg_fillvalue
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=rla_offsetvalue_R8(:)
                !WHERE( isnan(rdpa_value) ) rdpa_value(:,:,:,:)=rg_fillvalue
                !WHERE(INT(idpa_value(:,:,:,:),Double) == dNaN) rdpa_value(:,:,:,:)=rg_fillvalue
             ENDSELECT
         ENDIF
          
     END SUBROUTINE MIOL_readu_field_S_4D_R4_NC

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
          !!              The output data are in R8 format
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:,:,:) :: idpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value       The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !! @param rdpa_value : The 4D data values read and convert in R8 format
          !!
          !! \implements INT_read_write_S
          !!
          !! \n History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  07/2008  (C.Regnier) Add read scale and offset factor
          !!        \n  01/2013  (C.Regnier) V3.5 MIOL
          !<
          !!=====================================================================
 
        SUBROUTINE MIOL_readu_field_S_4D_R8_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize,&
                                         rdpa_value,&
                                         rda_offsetvalue,&
                                         ida_indx,&
                                         ida_indy,&
                                         ida_indz,&
                                         ida_indt)
 
         USE MIOL_param
         USE INT_ATTRIBUTSTYPE
         USE INT_ATTRIBUTS
         USE MFT_error
         USE netcdf
         IMPLICIT NONE
        
          !----------------------------------------------------------------------
 
          INTEGER,                           INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=2), DIMENSION(:,:,:,:), POINTER :: idpa_value
          INTEGER, DIMENSION(4), OPTIONAL,     INTENT(OUT) :: ida_dimsize
          REAL(KIND=8), DIMENSION(:,:,:,:),POINTER,OPTIONAL :: rdpa_value
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx,ida_indy,ida_indz,ida_indt
  
          CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
          INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsize
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER :: il_nbdim, il_type
          INTEGER, DIMENSION(4) :: ila_dimids
          INTEGER :: il_var_id, il_status, il_ji
          INTEGER :: il_nx, il_ny, il_nz, il_nt
 
          INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction,cl_fillval
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2),OPTIONAL,INTENT(OUT) :: rda_offsetvalue
          REAL(KIND=4), DIMENSION(2)                     :: rla_offsetvalue_R4
          REAL(KIND=8), DIMENSION(2)                     :: rla_offsetvalue_R8
          REAL(KIND=4)                                  :: rl_fillvaluetmp
          INTEGER(KIND=2)                               :: il_fillvaluetmp_I2  
          INTEGER(KIND=4)                               :: il_fillvaluetmp
          LOGICAL    :: ll_missing_value,ll_fillval  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset
          !*----------------------------------------------------------------------
          !
          cl_fonction ='MIOL_readu_field_S_4D_NC'
          !----------------------------------------------------------------------
          ! Initialization
 
          il_nx = 1
          il_ny = 1
          il_nz = 1
          il_nt = 1
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fillval=.FALSE.
          il_fillvaluetmp_I2=-(((2**16-2))/2)
 
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
             ALLOCATE(idpa_value(size(ida_indx(:)),&
                                 size(ida_indy(:)),&
                                 size(ida_indz(:)),&
                                 size(ida_indt(:))),&
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
          ELSE
             ALLOCATE(idpa_value(ila_dimsize(1), &
                                 ila_dimsize(2), &
                                 ila_dimsize(3), &
                                 ila_dimsize(4)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, 'idpa_value',cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
           IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz).AND. PRESENT(ida_indt)) THEN
              il_status =fi_ncError(NF90_GET_VAR(id_file_id, &
                                                 il_var_id,&
                                                 idpa_value,&
                                                 start = (/ida_indx(1),ida_indy(1),ida_indz(1),ida_indt(1)/),&
                                                 count= (/size(ida_indx(:)),size(ida_indy(:)),size(ida_indz(:)),&
                                                         size(ida_indt(:))/)),cl_fonction) 
           ELSE
              il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                                  il_var_id, &
                                                  idpa_value),cl_fonction)
           ENDIF
          !----------------------------------------------------------------------
          ! Memory allocation
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz).AND. PRESENT(ida_indt)) THEN
             ALLOCATE(rdpa_value(size(ida_indx(:)),&
                                 size(ida_indy(:)),&
                                 size(ida_indz(:)),&
                                 size(ida_indt(:)) ),&
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ELSE
             ALLOCATE(rdpa_value(ila_dimsize(1), &
                                 ila_dimsize(2), &
                                 ila_dimsize(3), &
                                 ila_dimsize(4)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
          
           !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          cl_fillval='_FillValue'
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
          CASE('_FillValue')
             ll_fillval = .TRUE.  
          CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          ENDSELECT
          ENDDO
          ! Test sur fillvalue et missing value
          IF ( ll_fillval.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fillval .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fillval ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)

               SELECT CASE (il_type)
                 CASE (NF90_INT)
                 !  teste les missing values en INT !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp)
                   il_fillvaluetmp_I2=INT(il_fillvaluetmp,2)
                 CASE (NF90_SHORT)
                 !  teste les missing values en short !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp_I2)
                 CASE (NF90_FLOAT)
                 !  teste les missing values en FLOAT !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             rl_fillvaluetmp)
                  il_fillvaluetmp_I2=INT(rl_fillvaluetmp,2)
                CASE DEFAULT
                  WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
                 CALL EXIT(3)
                ENDSELECT
          endif ! fin test 
          IF (ll_scale_factor .AND. ll_offset) THEN
              !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                     cd_varname,&
                     cla_attname,&
                     rla_offsetvalue_R4)
                WHERE(idpa_value(:,:,:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:,:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:,:,:)=(idpa_value(:,:,:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:,:,:)=rg_fillvalue_R8
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=REAL(rla_offsetvalue_R4(:),8)
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                                                cd_varname,&
                                                cla_attname,&
                                                rla_offsetvalue_R8)
                WHERE(idpa_value(:,:,:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:,:,:) /= -il_fillvaluetmp_I2 )
             	   rdpa_value(:,:,:,:)=(idpa_value(:,:,:,:)*rla_offsetvalue_R8(1))+rla_offsetvalue_R8(2)
                ELSEWHERE
                   rdpa_value(:,:,:,:)=rg_fillvalue
         	ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=rla_offsetvalue_R8
            ENDSELECT
       ENDIF
          
     END SUBROUTINE MIOL_readu_field_S_4D_R8_NC
 
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
          !!=====================================================================
          !>\brief
          !! Description: This function gets data values from the variable defined
          !!              in argument. You can also know the length of the variable
          !!              dimensions.
          !!              This functions allows you to get array of data values
          !!              without knowing the correct lenght of each dimension.
          !!              You just have to know the type and the number of
          !!              dimensions. This function uses pointers, so you don’t
          !!              have to know the correct dimensions of the data arrays.
          !!              The output data are in R4 format
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:,:) :: idpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !! @param rdpa_value : The 3D data values read and convert in R4 format
          !!
          !! \implements INT_read_write_S
          !!
          !! \n History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  07/2008  (C.Regnier) Add read scale and offset factor
          !!        \n  01/2013  (C.Regnier) V3.5 MIOL
          !<
          !!=====================================================================
  
       SUBROUTINE MIOL_readu_field_S_3D_R4_NC(id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize,&
                                         rdpa_value,&
                                         rda_offsetvalue,&
                                         ida_indx,&
                                         ida_indy,&
                                         ida_indz)
 
         USE MFT_error
         USE MIOL_param
         USE INT_ATTRIBUTSTYPE
         Use MFT_Inf_NaN_Detection
         USE INT_ATTRIBUTS
         USE netcdf
         IMPLICIT NONE
 
          !----------------------------------------------------------------------
 
          INTEGER,                           INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=2), DIMENSION(:,:,:), POINTER :: idpa_value
          REAL(KIND=4), DIMENSION(:,:,:),POINTER     :: rdpa_value
          REAL(KIND=8), DIMENSION(2),OPTIONAL,INTENT(OUT) :: rda_offsetvalue
          INTEGER, DIMENSION(3), OPTIONAL,     INTENT(OUT) :: ida_dimsize
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL :: ida_indx,ida_indy,ida_indz

          CHARACTER(LEN=255), DIMENSION(3) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(3) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji
          INTEGER                                       :: il_jj,il_nbatt,il_i,il_j
          CHARACTER(LEN=255)                            :: cl_fonction,cl_fillval
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=4), DIMENSION(2)                     :: rla_offsetvalue_R4
          REAL(KIND=8), DIMENSION(2)                     :: rla_offsetvalue_R8
          INTEGER(KIND=2)                               :: il_fillvaluetmp_I2
          INTEGER(KIND=4)                               :: il_fillvaluetmp
          REAL(KIND=4)                                  :: rl_fillvaluetmp  
          LOGICAL    :: ll_missing_value,ll_fillval  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset
          !
          !*----------------------------------------------------------------------
          !
          
          cl_fonction ='MIOL_readu_field_S_3D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fillval=.FALSE.
          il_fillvaluetmp_I2=-(((2**16-2))/2)

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
 
          IF (il_nbdim.NE.3) THEN
            WRITE(0,*) "MIOL_read_field_S_4D_NC: array dimension error."
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
             ALLOCATE(idpa_value(size(ida_indx(:)),&
                                 size(ida_indy(:)),&
                                 size(ida_indz(:))),&
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
          ELSE
             ALLOCATE(idpa_value(ila_dimsize(1), &
                                 ila_dimsize(2), &
                                 ila_dimsize(3)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
             ALLOCATE(rdpa_value(ila_dimsize(1), &
                                 ila_dimsize(2), &
                                 ila_dimsize(3)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz)) THEN
             il_status =fi_ncError(NF90_GET_VAR(id_file_id, &
                                                il_var_id,&
                                                idpa_value,&
                                                 start = (/ida_indx(1),ida_indy(1),ida_indz(1)/),&
                                                 count= (/size(ida_indx(:)),size(ida_indy(:)),size(ida_indz(:))/)),cl_fonction)
             
          ELSE
             il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                                 il_var_id, &
                                                 idpa_value),cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
 

          !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          cl_fillval='_FillValue'
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
          CASE('_FillValue')
             ll_fillval = .TRUE.  
          CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          ENDSELECT
          ENDDO
          ! Test sur fillvalue et missing value
          IF ( ll_fillval.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fillval .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fillval ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)

               SELECT CASE (il_type)
                 CASE (NF90_INT)
                 !  teste les missing values en INT !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp)
                   il_fillvaluetmp_I2=INT(il_fillvaluetmp,2)
                 CASE (NF90_SHORT)
                 !  teste les missing values en short !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp_I2)
                 CASE (NF90_FLOAT)
                 !  teste les missing values en FLOAT !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             rl_fillvaluetmp)
                  il_fillvaluetmp_I2=INT(rl_fillvaluetmp,2)
                CASE DEFAULT
                  WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
                 CALL EXIT(3)
                ENDSELECT
          endif !" fin test 
          IF (ll_scale_factor .AND. ll_offset) THEN
              !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                                                   cd_varname,&
                   				   cla_attname,&
                     				   rla_offsetvalue_R4)
                WHERE(idpa_value(:,:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:,:) =(idpa_value(:,:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:,:)=rg_fillvalue
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=REAL(rla_offsetvalue_R4(:),8)
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                                                   cd_varname,&
                                                   cla_attname,&
                                                   rla_offsetvalue_R8)
                WHERE(idpa_value(:,:,:) /=  il_fillvaluetmp_I2 .AND. idpa_value(:,:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:,:)=(idpa_value(:,:,:)*rla_offsetvalue_R8(1))+rla_offsetvalue_R8(2)
                ELSEWHERE
                   rdpa_value(:,:,:)=rg_fillvalue
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=rla_offsetvalue_R8
          ENDSELECT
       ENDIF
          
        END SUBROUTINE MIOL_readu_field_S_3D_R4_NC
 
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
          !!              The output data are in R8 format
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:,:) :: idpa_value
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !! @param rdpa_value : The 3D data values read and convert in R8 format
          !!
          !! \implements INT_read_write_S
          !!
          !! \n History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  07/2008  (C.Regnier) Add read scale and offset factor
          !!        \n  01/2013  (C.Regnier) V3.5 MIOL
          !<
          !!=====================================================================
  
       SUBROUTINE MIOL_readu_field_S_3D_R8_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize,&
                                         rdpa_value,&
                                         rda_offsetvalue,&
                                         ida_indx,&
                                         ida_indy,&
                                         ida_indz)
 
         USE MFT_error
         USE MIOL_param
         USE INT_ATTRIBUTSTYPE       
         USE INT_ATTRIBUTS
         USE netcdf
         IMPLICIT NONE
 
          !----------------------------------------------------------------------
 
          INTEGER,                           INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=2), DIMENSION(:,:,:), POINTER :: idpa_value
          INTEGER, DIMENSION(3), OPTIONAL,     INTENT(OUT) :: ida_dimsize
          REAL(KIND=8), DIMENSION(:,:,:),POINTER,OPTIONAL :: rdpa_value
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL :: ida_indx,ida_indy,ida_indz

          CHARACTER(LEN=255), DIMENSION(3) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(3) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji
          INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction,cl_fillval
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2) ,OPTIONAL,INTENT(OUT) :: rda_offsetvalue
          REAL(KIND=4), DIMENSION(2)                     :: rla_offsetvalue_R4
          REAL(KIND=8), DIMENSION(2)                     :: rla_offsetvalue_R8
          REAL(KIND=4)                                   :: rl_fillvaluetmp
          INTEGER(KIND=2)                               :: il_fillvaluetmp_I2  
          INTEGER(KIND=4)                               :: il_fillvaluetmp
          LOGICAL    :: ll_missing_value,ll_fillval  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset
          !
          !*----------------------------------------------------------------------
          !
          cl_fonction ='MIOL_readu_field_S_3D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fillval=.FALSE.
          il_fillvaluetmp_I2=-(((2**16-2))/2)


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
 
          IF (il_nbdim.NE.3) THEN
             WRITE(0,*) "MIOL_read_field_S_4D_NC: array dimension error. "
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
             ALLOCATE(idpa_value(size(ida_indx(:)),&
                                 size(ida_indy(:)),&
                                 size(ida_indz(:))),&
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
          ELSE
             ALLOCATE(idpa_value(ila_dimsize(1), &
                                 ila_dimsize(2), &
                                 ila_dimsize(3)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
             ALLOCATE(rdpa_value(ila_dimsize(1), &
                                 ila_dimsize(2), &
                                 ila_dimsize(3)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy).AND. PRESENT(ida_indz)) THEN
             il_status =fi_ncError(NF90_GET_VAR(id_file_id, &
                                                il_var_id,&
                                                idpa_value,&
                                                start = (/ida_indx(1),ida_indy(1),ida_indz(1)/),&
                                                count= (/size(ida_indx(:)),size(ida_indy(:)),size(ida_indz(:))/)),cl_fonction)
          ELSE
             il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                                 il_var_id, &
                                                 idpa_value),cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
 

          !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          cl_fillval='_FillValue'
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
          CASE('_FillValue')
             ll_fillval = .TRUE.  
          CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          ENDSELECT
          ENDDO
          ! Test sur fillvalue et missing value
          IF ( ll_fillval.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fillval .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fillval ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)

               SELECT CASE (il_type)
                 CASE (NF90_INT)
                 !  teste les missing values en INT !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp)
                   il_fillvaluetmp_I2=INT(il_fillvaluetmp,2)
                 CASE (NF90_SHORT)
                 !  teste les missing values en short !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp_I2)
                 CASE (NF90_FLOAT)
                 !  teste les missing values en FLOAT !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             rl_fillvaluetmp)
                  il_fillvaluetmp_I2=INT(rl_fillvaluetmp,2)
                CASE DEFAULT
                  WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
                 CALL EXIT(3)
                ENDSELECT
          endif ! fin test 
          IF (ll_scale_factor .AND. ll_offset) THEN
              !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                     cd_varname,&
                     cla_attname,&
                     rla_offsetvalue_R4)
                WHERE(idpa_value(:,:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:,:) /= -il_fillvaluetmp_I2)
                   rdpa_value(:,:,:)=(idpa_value(:,:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:,:)=rg_fillvalue_R8
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=REAL(rla_offsetvalue_R4(:),8)
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                                                   cd_varname,&
                                                   cla_attname,&
                                                   rla_offsetvalue_R8)
             WHERE(idpa_value(:,:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:,:) /= -il_fillvaluetmp_I2)
                rdpa_value(:,:,:)=(idpa_value(:,:,:)*rla_offsetvalue_R8(1))+rla_offsetvalue_R8(2)
             ELSEWHERE
                rdpa_value(:,:,:)=rg_fillvalue_R8
             ENDWHERE
             IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=rla_offsetvalue_R8
       ENDSELECT
       ENDIF
 
        END SUBROUTINE MIOL_readu_field_S_3D_R8_NC
 
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
          !!              The output data are in R4 format
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:) :: idpa_value
          !!
          !! @param id_file_id        A NetCDF file Id.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !! @param rdpa_value : The 2D data values read and convert in R4 format
          !!
          !! \implements INT_read_write_S
          !!
          !! \n History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  07/2008  (C.Regnier) Add read scale and offset factor
          !!        \n  01/2013  (C.Regnier) V3.5 MIOL
          !<
          !!=====================================================================
  
       SUBROUTINE MIOL_readu_field_S_2D_R4_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize,&
                                         rdpa_value,&
                                         rda_offsetvalue,&
                                         ida_indx,&
                                         ida_indy)
 
         USE MFT_error
         USE MIOL_param
         USE INT_ATTRIBUTSTYPE
         USE INT_ATTRIBUTS
         USE netcdf
         IMPLICIT NONE
 
 
          !----------------------------------------------------------------------
 
          INTEGER,                           INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=2), DIMENSION(:,:), POINTER :: idpa_value
          INTEGER, DIMENSION(2), OPTIONAL,     INTENT(OUT) :: ida_dimsize
          REAL(KIND=4),  DIMENSION(:,:),POINTER,OPTIONAL :: rdpa_value
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx,ida_indy

          CHARACTER(LEN=255), DIMENSION(2) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(2) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji
          INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction,cl_fillval
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2),OPTIONAL,INTENT(OUT) :: rda_offsetvalue
          REAL(KIND=4), DIMENSION(2)                     :: rla_offsetvalue_R4
          REAL(KIND=8), DIMENSION(2)                     :: rla_offsetvalue_R8
          REAL(KIND=4)                                   :: rl_fillvaluetmp
          INTEGER(KIND=2)                               :: il_fillvaluetmp_I2  
          INTEGER(KIND=4)                               :: il_fillvaluetmp
          LOGICAL    :: ll_missing_value,ll_fillval  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset
          !
          !*----------------------------------------------------------------------
          !
          cl_fonction ='MIOL_readu_field_S_2D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fillval=.FALSE.
          il_fillvaluetmp_I2=-(((2**16-2))/2)

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
             WRITE(0,*) "MIOL_read_field_S_2D_NC: array dimension error. "
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
             ALLOCATE(idpa_value(size(ida_indx(:)),&
                                 size(ida_indy(:))),&
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
          ELSE
             ALLOCATE(idpa_value(ila_dimsize(1), &
                                 ila_dimsize(2)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
             
             ALLOCATE(rdpa_value(ila_dimsize(1), &
                                 ila_dimsize(2)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy)) THEN
             il_status =fi_ncError(NF90_GET_VAR(id_file_id, &
                                                il_var_id,&
                                                idpa_value,&
                                                start = (/ida_indx(1),ida_indy(1)/),&
                                                count= (/size(ida_indx(:)),size(ida_indy(:))/)),cl_fonction)
          ELSE
             il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                                 il_var_id, &
                                                 idpa_value),cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
 
 
          !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          cl_fillval='_FillValue'
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
             CASE('_FillValue')
                ll_fillval = .TRUE.  
             CASE('missing_value')  ! FHZ: MRI files
                ll_missing_value = .TRUE.
             ENDSELECT
          ENDDO
          ! Test sur fillvalue et missing value
          IF ( ll_fillval.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fillval .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fillval ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)

               SELECT CASE (il_type)
                 CASE (NF90_INT)
                 !  teste les missing values en INT !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp)
                   il_fillvaluetmp_I2=INT(il_fillvaluetmp,2)
                 CASE (NF90_SHORT)
                 !  teste les missing values en short !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp_I2)
                 CASE (NF90_FLOAT)
                 !  teste les missing values en FLOAT !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             rl_fillvaluetmp)
                  il_fillvaluetmp_I2=INT(rl_fillvaluetmp,2)
                CASE DEFAULT
                  WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
                 CALL EXIT(3)
                ENDSELECT
          endif ! fin test 
          IF (ll_scale_factor .AND. ll_offset) THEN
              !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                     cd_varname,&
                     cla_attname,&
                     rla_offsetvalue_R4)
                WHERE(idpa_value(:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:)=(idpa_value(:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:)=rg_fillvalue
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=REAL(rla_offsetvalue_R4(:),8)
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                                                cd_varname,&
                                                cla_attname,&
                                                rla_offsetvalue_R8)
                WHERE(idpa_value(:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:)=(REAL(idpa_value(:,:))*rla_offsetvalue_R8(1))+rla_offsetvalue_R8(2)
                ELSEWHERE
                   rdpa_value(:,:)=rg_fillvalue
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=rla_offsetvalue_R8
             ENDSELECT
          ENDIF

        END SUBROUTINE MIOL_readu_field_S_2D_R4_NC
 
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
          !!              The output data are in R8 format
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:,:) :: idpa_value
          !!
          !! @param id_file_id        A NetCDF file Id.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !! @param rdpa_value : The 2D data values read and convert in R8 format
          !!
          !! \implements INT_read_write_S
          !!
          !! \n History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  07/2008  (C.Regnier) Add read scale and offset factor
          !!        \n  01/2013  (C.Regnier) V3.5 MIOL
          !<
          !!=====================================================================
  
       SUBROUTINE MIOL_readu_field_S_2D_R8_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize,&
                                         rdpa_value,&
                                         rda_offsetvalue,&
                                         ida_indx,&
                                         ida_indy)

 
         USE MFT_error
         USE MIOL_param
         USE INT_ATTRIBUTSTYPE
         USE INT_ATTRIBUTS
         USE netcdf
         IMPLICIT NONE
         
          !----------------------------------------------------------------------
 
          INTEGER,                           INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=2), DIMENSION(:,:), POINTER :: idpa_value
          INTEGER, DIMENSION(2), OPTIONAL,     INTENT(OUT) :: ida_dimsize
          REAL(KIND=8),  DIMENSION(:,:),POINTER,OPTIONAL :: rdpa_value
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx,ida_indy


          CHARACTER(LEN=255), DIMENSION(2) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(2) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji
          INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction,cl_fillval
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2),OPTIONAL,INTENT(OUT) :: rda_offsetvalue
          REAL(KIND=4), DIMENSION(2)                     :: rla_offsetvalue_R4
          REAL(KIND=8), DIMENSION(2)                     :: rla_offsetvalue_R8
          REAL(KIND=4)                                  :: rl_fillvaluetmp
          INTEGER(KIND=4)                               :: il_fillvaluetmp
          INTEGER(KIND=2)                               :: il_fillvaluetmp_I2  
          LOGICAL    :: ll_missing_value,ll_fillval  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset

          !*----------------------------------------------------------------------
          !
          cl_fonction ='MIOL_readu_field_S_2D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fillval=.FALSE.
          il_fillvaluetmp_I2=-(((2**16-2))/2)

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
             WRITE(0,*) "MIOL_read_field_S_2D_NC: array dimension error. "
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
             ALLOCATE(idpa_value(size(ida_indx(:)),&
                                 size(ida_indy(:))),&
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
          ELSE
             ALLOCATE(idpa_value(ila_dimsize(1), &
                                 ila_dimsize(2)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
             ALLOCATE(rdpa_value(ila_dimsize(1), &
                                 ila_dimsize(2)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx).AND. PRESENT(ida_indy)) THEN
             il_status =fi_ncError(NF90_GET_VAR(id_file_id, &
                                                il_var_id,&
                                                idpa_value,&
                                                start = (/ida_indx(1),ida_indy(1)/),&
                                                count= (/size(ida_indx(:)),size(ida_indy(:))/)),cl_fonction)
          ELSE
             il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                                 il_var_id, &
                                                 idpa_value),cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
 
 
          !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          cl_fillval='_FillValue'
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
             CASE('_FillValue')
                ll_fillval = .TRUE.  
             CASE('missing_value')  ! FHZ: MRI files
                ll_missing_value = .TRUE.
             ENDSELECT
          ENDDO
          ! Test sur fillvalue et missing value
          IF ( ll_fillval.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fillval .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fillval ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)

               SELECT CASE (il_type)
                 CASE (NF90_INT)
                 !  teste les missing values en INT !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp)
                   il_fillvaluetmp_I2=INT(il_fillvaluetmp,2)
                 CASE (NF90_SHORT)
                 !  teste les missing values en short !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp_I2)
                 CASE (NF90_FLOAT)
                 !  teste les missing values en FLOAT !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             rl_fillvaluetmp)
                  il_fillvaluetmp_I2=INT(rl_fillvaluetmp,2)
                CASE DEFAULT
                  WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
                 CALL EXIT(3)
                ENDSELECT
          endif ! fin test 
          IF (ll_scale_factor .AND. ll_offset) THEN
              !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                     cd_varname,&
                     cla_attname,&
                     rla_offsetvalue_R4)
                WHERE(idpa_value(:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:) /= -il_fillvaluetmp_I2  )
                   rdpa_value(:,:)=(idpa_value(:,:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:,:)=rg_fillvalue_R8
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=REAL(rla_offsetvalue_R4(:),8)
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                                                cd_varname,&
                                                cla_attname,&
                                                rla_offsetvalue_R8)
                WHERE(idpa_value(:,:) /= il_fillvaluetmp_I2 .AND. idpa_value(:,:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:,:)=(REAL(idpa_value(:,:))*rla_offsetvalue_R8(1))+rla_offsetvalue_R8(2)
                ELSEWHERE
                   rdpa_value(:,:)=rg_fillvalue_R8
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=rla_offsetvalue_R8
             ENDSELECT
          ENDIF
          
        END SUBROUTINE MIOL_readu_field_S_2D_R8_NC
 
 
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
          !!              The output data are in R4 format
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:) :: idpa_value
          !!
          !! @param id_file_id        A NetCDF file Id.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !! @param rdpa_value : The 1D data values read and convert in R4 format
          !!
          !! \implements INT_read_write_S
          !!
          !! \n History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  07/2008  (C.Regnier) Add read scale and offset factor
          !!        \n  01/2013  (C.Regnier) V3.5 MIOL
          !<
          !!=====================================================================
  
        SUBROUTINE MIOL_readu_field_S_1D_R4_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize,&
                                         rdpa_value,&
                                         rda_offsetvalue,&
                                         ida_indx)

         
         USE MFT_error
         USE INT_ATTRIBUTSTYPE
         USE MIOL_param
         USE INT_ATTRIBUTS
         USE netcdf
         IMPLICIT NONE
 
          !----------------------------------------------------------------------
 
          INTEGER,                           INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=2), DIMENSION(:), POINTER :: idpa_value
          INTEGER, DIMENSION(1), OPTIONAL,     INTENT(OUT) :: ida_dimsize
          REAL(KIND=4),  DIMENSION(:),POINTER,OPTIONAL :: rdpa_value
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx

          CHARACTER(LEN=255), DIMENSION(1) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(1) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji
          INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction,cl_fillval
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2),OPTIONAL,INTENT(OUT) :: rda_offsetvalue
          REAL(KIND=4), DIMENSION(2)                     :: rla_offsetvalue_R4
          REAL(KIND=8), DIMENSION(2)                     :: rla_offsetvalue_R8
          REAL(KIND=4)                                  :: rl_fillvaluetmp
          INTEGER(KIND=4)                               :: il_fillvaluetmp  
          INTEGER(KIND=2)                               :: il_fillvaluetmp_I2
          LOGICAL    :: ll_missing_value,ll_fillval  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset
          !
          !*----------------------------------------------------------------------
          !
          cl_fonction ='MIOL_readu_field_S_1D_NC'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fillval=.FALSE.
          il_fillvaluetmp_I2=-(((2**16-2))/2)

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
             WRITE(0,*) "MIOL_read_field_I_1D_NC: array dimension error. "
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
             ALLOCATE(idpa_value(size(ida_indx(:))),&                        
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
          ELSE
             ALLOCATE(idpa_value(ila_dimsize(1)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
             ALLOCATE(rdpa_value(ila_dimsize(1)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx)) THEN
             il_status =fi_ncError(NF90_GET_VAR(id_file_id, &
                                                il_var_id,&
                                                idpa_value,&
                                                start = (/ida_indx(1)/),&
                                                count= (/size(ida_indx(:))/)),cl_fonction)
          ELSE
             il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                                 il_var_id, &
                                                 idpa_value),cl_fonction)
             
          ENDIF
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
          
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
          
          !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          cl_fillval='_FillValue'
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
          CASE('_FillValue')
             ll_fillval = .TRUE.  
          CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          ENDSELECT
          ENDDO
          ! Test sur fillvalue et missing value
          IF ( ll_fillval.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fillval .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fillval ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)

               SELECT CASE (il_type)
                 CASE (NF90_INT)
                 !  teste les missing values en INT !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp)
                   il_fillvaluetmp_I2=INT(il_fillvaluetmp,2)
                 CASE (NF90_SHORT)
                 !  teste les missing values en short !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp_I2)
                 CASE (NF90_FLOAT)
                 !  teste les missing values en FLOAT !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             rl_fillvaluetmp)
                  il_fillvaluetmp_I2=INT(rl_fillvaluetmp,2)
                CASE DEFAULT
                  WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
                 CALL EXIT(3)
                ENDSELECT
          endif ! fin test 
          IF (ll_scale_factor .AND. ll_offset) THEN
              !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                     cd_varname,&
                     cla_attname,&
                     rla_offsetvalue_R4)
                WHERE(idpa_value(:) /= il_fillvaluetmp_I2 .AND. idpa_value(:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:)=(idpa_value(:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:)=rg_fillvalue
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=REAL(rla_offsetvalue_R4(:),8)
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                                                cd_varname,&
                                                cla_attname,&
                                                rla_offsetvalue_R8)
                WHERE(idpa_value(:) /= il_fillvaluetmp_I2 .AND. idpa_value(:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:)=(idpa_value(:)*rla_offsetvalue_R8(1))+rla_offsetvalue_R8(2)
                ELSEWHERE
                   rdpa_value(:)=rg_fillvalue
                ENDWHERE
		IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=rla_offsetvalue_R8
             ENDSELECT
         ENDIF

        END SUBROUTINE MIOL_readu_field_S_1D_R4_NC
 
 
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
          !!              The output data are in R8 format
          !! Use: You have to declarate the array rdpa_value in the main program
          !! as a pointer: INTEGER, POINTER, DIMENSION(:) :: idpa_value
          !!
          !! @param id_file_id        A NetCDF file Id.
          !! @param cd_varname        Variable name.
          !! @param idpa_value        The data values to be read.
          !! @param ida_dimsize       Returned vector of values corresponding to the
          !!                     length of each dimension.
          !! @param rdpa_value : The 1D data values read and convert in R8 format
          !!
          !! \implements INT_read_write_S
          !!
          !! \n History :
          !!        \n  06/2006  (F. Messal) F90
          !!        \n  11/2006  (F. Messal) CVS V1.0
          !!        \n  07/2008  (C.Regnier) Add read scale and offset factor
          !!        \n  01/2013  (C.Regnier) V3.5 MIOL
          !<
          !!=====================================================================
  
       SUBROUTINE MIOL_readu_field_S_1D_R8_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize,&
                                         rdpa_value,&
                                         rda_offsetvalue,&
                                         ida_indx)

         
         USE MFT_error
         USE INT_ATTRIBUTSTYPE
         USE MIOL_param
         USE netcdf
         USE INT_ATTRIBUTS
         IMPLICIT NONE
         
          !----------------------------------------------------------------------
 
          INTEGER,                           INTENT(IN) :: id_file_id
          CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
          INTEGER(KIND=2), DIMENSION(:), POINTER :: idpa_value
          INTEGER, DIMENSION(1), OPTIONAL,     INTENT(OUT) :: ida_dimsize
          REAL(KIND=8),  DIMENSION(:),POINTER,OPTIONAL :: rdpa_value
          INTEGER, DIMENSION(:), INTENT(IN),  OPTIONAL  :: ida_indx


          CHARACTER(LEN=255), DIMENSION(1) :: cla_dimname
          CHARACTER(LEN=100) :: cl_tmp
          INTEGER, DIMENSION(1) :: ila_dimsize, ila_dimids
          INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji
          INTEGER                                       :: il_jj,il_nbatt
          CHARACTER(LEN=255)                            :: cl_fonction,cl_fillval
          CHARACTER(LEN=255),DIMENSION(2)               :: cla_attname
          CHARACTER(LEN=100)                            :: cl_tmpattname
          REAL(KIND=8), DIMENSION(2),OPTIONAL,INTENT(OUT) :: rda_offsetvalue
          REAL(KIND=4), DIMENSION(2)                     :: rla_offsetvalue_R4
          REAL(KIND=8), DIMENSION(2)                     :: rla_offsetvalue_R8
          REAL(KIND=4)                                  :: rl_fillvaluetmp
          INTEGER(KIND=4)                               :: il_fillvaluetmp  
          INTEGER(KIND=2)                               :: il_fillvaluetmp_I2
          LOGICAL    :: ll_missing_value,ll_fillval  ! FHZ test sur les valeurs manquantes
          LOGICAL                                       :: ll_scale_factor,ll_offset
          !
          !*----------------------------------------------------------------------
          !
          cl_fonction ='MIOL_readu_field_S_1D_NC_R8'
          ll_scale_factor=.FALSE.
          ll_offset=.FALSE.
          ll_missing_value=.FALSE.
          ll_fillval=.FALSE.
          il_fillvaluetmp_I2=-(((2**16-2))/2)


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
             WRITE(0,*) "MIOL_read_field_I_1D_NC: array dimension error. "
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
             ALLOCATE(idpa_value(size(ida_indx(:))),&                        
                                      stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
          ELSE
             ALLOCATE(idpa_value(ila_dimsize(1)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' idpa_value',cl_fonction)
             ALLOCATE(rdpa_value(ila_dimsize(1)), &
                                 stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_value',cl_fonction)
          ENDIF
          !----------------------------------------------------------------------
          ! Read variable Id
 
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          IF (PRESENT(ida_indx)) THEN
             il_status =fi_ncError(NF90_GET_VAR(id_file_id, &
                                                il_var_id,&
                                                idpa_value,&
                                                start = (/ida_indx(1)/),&
                                                count= (/size(ida_indx(:))/)),cl_fonction)
          ELSE
             il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                                 il_var_id, &
                                                 idpa_value),cl_fonction)
             
          ENDIF
          !----------------------------------------------------------------------
          ! Complete the correct lenght of dimensions
 
          IF (PRESENT(ida_dimsize)) THEN
             ida_dimsize = SHAPE(idpa_value)
          ENDIF
          
          !----------------------------------------------------------------------
          !* If scale factor compute
          cla_attname(1)='scale_factor'
          cla_attname(2)='add_offset'
          cl_fillval='_FillValue'

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
          CASE('_FillValue')
             ll_fillval = .TRUE.  
          CASE('missing_value')  ! FHZ: MRI files
             ll_missing_value = .TRUE.
          ENDSELECT
          ENDDO
          ! Test sur fillvalue et missing value
          IF ( ll_fillval.AND.ll_missing_value) ll_missing_value=.FALSE. !! on prend seulement la fillvalue
          if ( ll_fillval .OR. ll_missing_value) then
          !** Fillvalue
          if ( ll_fillval ) cl_tmpattname='_FillValue'
          !** missing_value
          if ( ll_missing_value ) cl_tmpattname='missing_value'
          !** Inquire type Attribut
           il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                     il_var_id, &
                                                     TRIM(cl_tmpattname), &
                                                     il_type),cl_fonction)

               SELECT CASE (il_type)
                 CASE (NF90_INT)
                 !  teste les missing values en INT !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp)
                   il_fillvaluetmp_I2=INT(il_fillvaluetmp,2)
                 CASE (NF90_SHORT)
                 !  teste les missing values en short !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             il_fillvaluetmp_I2)
                 CASE (NF90_FLOAT)
                 !  teste les missing values en FLOAT !
                 CALL MIOL_read_attribute_NC(id_file_id, &
                                             cd_varname, &
                                             TRIM(cl_tmpattname), &
                                             rl_fillvaluetmp)
                  il_fillvaluetmp_I2=INT(rl_fillvaluetmp,2)
                CASE DEFAULT
                  WRITE (0,*) "Pb with type argument not a int or a short",il_type,TRIM(cl_tmpattname)
                 CALL EXIT(3)
                ENDSELECT
          endif ! fin test 
          IF (ll_scale_factor .AND. ll_offset) THEN
              !** Inquire type Attribut
             il_status= fi_ncError(NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                                          il_var_id, &
                                                          TRIM(cla_attname(1)), &
                                                          il_type),cl_fonction)
             SELECT CASE (il_type)
             CASE (NF90_FLOAT)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                     cd_varname,&
                     cla_attname,&
                     rla_offsetvalue_R4)
                WHERE(idpa_value(:) /= il_fillvaluetmp_I2 .AND. idpa_value(:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:)=(idpa_value(:)*rla_offsetvalue_R4(1))+rla_offsetvalue_R4(2)
                ELSEWHERE
                   rdpa_value(:)=rg_fillvalue_R8
                ENDWHERE
                IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=REAL(rla_offsetvalue_R4(:),8)
             CASE (NF90_DOUBLE)
                CALL MIOL_read_att_scale_offset_NC(id_file_id,&
                                                cd_varname,&
                                                cla_attname,&
                                                rla_offsetvalue_R8)
                WHERE(idpa_value(:) /= il_fillvaluetmp_I2 .AND. idpa_value(:) /= -il_fillvaluetmp_I2 )
                   rdpa_value(:)=(idpa_value(:)*rla_offsetvalue_R8(1))+rla_offsetvalue_R8(2)
                ELSEWHERE
                   rdpa_value(:)=rg_fillvalue_R8
                ENDWHERE
		IF (PRESENT(rda_offsetvalue)) rda_offsetvalue(:)=rla_offsetvalue_R8
             ENDSELECT
       ENDIF

        END SUBROUTINE MIOL_readu_field_S_1D_R8_NC
 
 
  !******************************************************************************
  !******************************************************************************
!END MODULE src_Read_Short
