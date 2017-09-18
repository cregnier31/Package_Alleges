!> \brief Module which contain utilitary subroutines
!! \author C.REGNIER Miol V3.5
!! \date September 2008 
!!  \version 3.5
!<
 MODULE MIOL_UTIL
   implicit none
  PUBLIC :: MIOL_getFreeLU,MIOL_read_maskorca_NC,MIOL_write_maskorca_NC,MIOL_read_maskcls_NC,&
           MIOL_read_coordorca_NC,MIOL_read_bathyorca_NC,MIOL_read_bathycls_NC,&
           MIOL_copy_dimensions_NC,MIOL_add_dimension_NC,MIOL_create_file_NC,MIOL_getNLines
 CONTAINS

   !!=====================================================================
   !>\brief
   !! Description: This function get free Fortran logical unit
   !! @param il_LU = free logical unit in integer
   !! \n History : P De Mey  in ??
   !!  \n         C.REGNIER MIOL V3
   !<
   !!=====================================================================
    
   SUBROUTINE MIOL_getFreeLU ( il_LU )
 
     IMPLICIT NONE
   INTEGER,intent(out) :: il_LU
   integer :: il_testLU
   LOGICAL :: ll_isOpened

! we have to try everything since we don't know when a il_LU is freed

   DO il_testLU = 100,200
     INQUIRE ( unit=il_testLU, opened=ll_isOpened )
     IF ( .NOT. ll_isOpened ) THEN
         il_LU = il_testLU
         RETURN
     END IF
   END DO

! bypass event()

   WRITE ( 0, '(a)') '-FATAL- no free logical units available'
   STOP '-FATAL- no free logical units available'
   
    END SUBROUTINE MIOL_getFreeLU
   !!=====================================================================
   !>\brief
   !! Description: This function return the number of lines of a file 
   !! @param il_LU = free logical unit in integer
   !! \n History : 14/01/13 L. ZAWADZKI : First contribution in MIOL !  
   !!  \n         
   !<
   !!=======================================================================
   SUBROUTINE MIOL_getNLines (il_LU, il_nlines)

   IMPLICIT NONE
   INTEGER,INTENT(IN)             :: il_LU
   INTEGER,INTENT(OUT)            :: il_nlines
   INTEGER                        :: istatus
   CHARACTER(LEN=255)             :: charead
  ! Reading number of lines in the file
  il_nlines  = 0
  istatus = 0
  DO WHILE (istatus == 0 )
     READ(il_LU,fmt=*,iostat=istatus),&
        charead
     IF (istatus==0) THEN
        il_nlines = il_nlines+1
     ENDIF
  ENDDO
  CLOSE(il_LU)


   END SUBROUTINE MIOL_getNLines 
   !!=====================================================================

  !******************************************************************************
  !******************************************************************************
  !******************************************************************************

          !!=====================================================================
          !> \brief
          !! Description: This function reads the data values from an ORCA mask
          !!              file created by using the writing function
          !!              MIOL_write_maskorca_NC.
          !!              For example, this kind of mask files are used by SCRIP.
          !!              This function uses pointers, so you don’t have to know
          !!              the correct dimensions of the data arrays.
          !!
          !! @param cd_filename       The NetCDF ORCA bathymetry filename.
          !!                     You must specify the complete path.
          !! @param idpa_mask         The data values to be read.
          !! @param rdpa_lon          Returned the longitude values.
          !! @param rdpa_lat          Returned the latitude values.
          !! @param rdpa_depth        Returned the depth values.
          !! @param rdpa_cos          Returned the cosinus values in case of a 'U' or
          !!                     'V' grid mask file (array of xdim*ydim dimensions).
          !! @param rdpa_sin          Returned the sinus values in case of a 'U' or
          !!                     'V' grid mask file (array of xdim*ydim dimensions).
          !!
          !! History :
          !!        \n  11/2006  (F. Messal)
          !!
          !!=====================================================================
 
        SUBROUTINE MIOL_read_maskorca_NC (cd_filename, &
                                          idpa_mask, &
                                          rdpa_lon, &
                                          rdpa_lat, &
                                          rdpa_depth, &
                                          rdpa_cos, &
                                          rdpa_sin)
 
          USE netcdf
          USE MFT_error
          IMPLICIT NONE
 
        
          !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),                       INTENT( IN) :: cd_filename
          REAL(KIND=4), DIMENSION(:,:),           POINTER :: rdpa_lon
          REAL(KIND=4), DIMENSION(:,:),           POINTER :: rdpa_lat
          REAL(KIND=4), DIMENSION(:),             POINTER :: rdpa_depth
          INTEGER, DIMENSION(:,:,:),              POINTER :: idpa_mask
          REAL(KIND=4), DIMENSION(:,:), OPTIONAL, POINTER :: rdpa_cos
          REAL(KIND=4), DIMENSION(:,:), OPTIONAL, POINTER :: rdpa_sin
 
          INTEGER :: il_file_id
          INTEGER :: il_var_id
          INTEGER :: il_dim_id
          CHARACTER(LEN=255), DIMENSION(3) :: cla_dimname
          CHARACTER(LEN=255)               :: cl_fonction
          INTEGER, DIMENSION(3) :: ila_dimsize
          INTEGER :: il_status
          !
          !*--------------------------------------------------------------------
          !
          cl_fonction='MIOL_read_maskorca_NC'
          !----------------------------------------------------------------------
          ! Open file
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_NOWRITE, &
                                           il_file_id),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Inquire dimensions
          il_status = fi_ncError(NF90_INQ_DIMID(il_file_id, &
                                                'x', &
                                                il_dim_id),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                        il_dim_id, &
                                                        cla_dimname(1), &
                                                        ila_dimsize(1)),cl_fonction)
 
          il_status = fi_ncError(NF90_INQ_DIMID(il_file_id, &
                                                'y', &
                                                il_dim_id),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                        il_dim_id, &
                                                        cla_dimname(2), &
                                                        ila_dimsize(2)),cl_fonction)
 
          il_status = fi_ncError(NF90_INQ_DIMID(il_file_id, &
                                                'depth', &
                                                il_dim_id),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                        il_dim_id, &
                                                        cla_dimname(3), &
                                                        ila_dimsize(3)),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Memory allocation
          ALLOCATE(rdpa_lon(ila_dimsize(1), &
                            ila_dimsize(2)), &
                   stat = il_status)
          il_status = fi_memError(il_status, ' rdpa_lon',cl_fonction)
 
          ALLOCATE(rdpa_lat(ila_dimsize(1), &
                            ila_dimsize(2)), &
                   stat = il_status)
          il_status = fi_memError(il_status, ' rdpa_lat',cl_fonction)
 
          ALLOCATE(rdpa_depth(ila_dimsize(3)), &
                   stat = il_status)
          il_status = fi_memError(il_status, ' rdpa_depth',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variables
 
          ! Read longitude values
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                'longitude', &
                                                il_var_id),cl_fonction)
 
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_var_id, &
                                              rdpa_lon),cl_fonction)
 
          ! Read latitude values
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                'latitude', &
                                                il_var_id),cl_fonction)
 
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_var_id, &
                                              rdpa_lat),cl_fonction)
 
          ! Read depth values
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                'depth', &
                                                il_var_id),cl_fonction)
 
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_var_id, &
                                              rdpa_depth),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read 'mask' variable
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                'mask', &
                                                il_var_id),cl_fonction)
 
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_var_id, &
                                              idpa_mask),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read 'grid_cos' and 'grid_sin' variables
 
          IF (PRESENT(rdpa_cos).AND.PRESENT(rdpa_sin)) THEN
 
             ALLOCATE(rdpa_cos(ila_dimsize(1), &
                               ila_dimsize(2)), &
                      stat = il_status)
             il_status = fi_memError(il_status, ' rdpa_cos',cl_fonction)
 
             il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                   'grid_cos', &
                                                   il_var_id),cl_fonction)
 
             il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                 il_var_id, &
                                                 rdpa_cos),cl_fonction)
 
             ALLOCATE(rdpa_sin(ila_dimsize(1), &
                               ila_dimsize(2)), &
                      stat = il_status)
             il_status = fi_memError(il_status, ' rdpa_sin',cl_fonction)
 
             il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                   'grid_sin', &
                                                   il_var_id),cl_fonction)
 
             il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                 il_var_id, &
                                                 rdpa_sin),cl_fonction)
 
          ENDIF
 
 
          !-----------------------------------------------------------------------
          ! Close file
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
 
        END SUBROUTINE MIOL_read_maskorca_NC

 !******************************************************************************
 
 
       SUBROUTINE MIOL_write_maskorca_NC (cd_filename, &
                                           cd_title, &
                                           rda_lon, &
                                           rda_lat, &
                                           rda_depth, &
                                           ida_mask, &
                                           id_nx, &
                                           id_ny, &
                                           id_nz, &
                                           rda_cos, &
                                           rda_sin)
 
          USE MFT_error
          USE netcdf
          IMPLICIT NONE
 
          !!=====================================================================
          !!
          !! Description: This function creates a complete NetCDF file with
          !!              characteristics of an ORCA mask file (just longitude,
          !!              latitude, depth and mask variables with x, y, depth
          !!              as dimensions).
          !!
          !! cd_filename     A NetCDF filename. You must specify the complete path.
          !! cd_title        The NetCDF filename specified in the global attribute
          !!                   'title'.
          !! rda_lon         Values of the longitude variable.
          !! rda_lat         Values of the latitude variable.
          !! rda_depth       Values of the depth variable.
          !! ida_mask        Values of the 3D mask.
          !! id_nx           Length of the x dimension.
          !! id_ny           Length of the y dimension.
          !! id_nz           Length of the z dimension.
          !! rda_cos         Values of the cosinus variable in case of U or V grid
          !!                   mask (see MCAL_compute_angle).
          !! rda_sin         Values of the sinus variable in case of U or V grid
          !!                   mask (see MCAL_compute_angle).
          !!
          !! History :
          !!        !  06/2006  (F. Messal) Creation
          !!        !  11/2006  (F. Messal) CVS version 1.0
          !!        !  06/2009   C.REGNIER : add to library V3.5
          !!=====================================================================
 
          !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),                             INTENT(IN) :: cd_filename
          CHARACTER(LEN=*),                             INTENT(IN) :: cd_title
          INTEGER,                                        INTENT(IN) :: id_nx,id_ny,id_nz
          REAL(KIND=4), DIMENSION(id_nx,id_ny),            INTENT(IN) :: rda_lon
          REAL(KIND=4), DIMENSION(id_nx,id_ny),            INTENT(IN) :: rda_lat
          REAL(KIND=4), DIMENSION(id_nx,id_ny),OPTIONAL, INTENT(IN) :: rda_cos
          REAL(KIND=4), DIMENSION(id_nx,id_ny),OPTIONAL, INTENT(IN) :: rda_sin
          REAL(KIND=4), DIMENSION(id_nz),                INTENT(IN) :: rda_depth
          INTEGER, DIMENSION(id_nx,id_ny,id_nz),         INTENT(IN) :: ida_mask
 
          INTEGER :: il_output_id                      ! NetCDF file ID
          INTEGER :: il_lonout_id, &
                     il_latout_id, &
                     il_depthout_id, &
                     il_maskout_id, &
                     il_sinout_id, &
                     il_cosout_id
          INTEGER :: il_status
          INTEGER, DIMENSION(3) :: il_dimsout_id
          CHARACTER(LEN=30) :: cl_fonction

          cl_fonction="MIOL_write_maskorca_NC"
          !----------------------------------------------------------------------
          ! Create file
          il_status = fi_ncError(NF90_CREATE(TRIM(cd_filename), &
                                             NF90_CLOBBER, &
                                             il_output_id),cl_fonction)
         
 
          !----------------------------------------------------------------------
          ! Write title attribute
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              NF90_GLOBAL, &
                                              'title', &
                                              TRIM(cd_title)),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Write dimensions
          il_status = fi_ncError(NF90_DEF_DIM(il_output_id, &
                                              'x', &
                                              id_nx, &
                                              il_dimsout_id(1)),cl_fonction)
 
          il_status = fi_ncError(NF90_DEF_DIM(il_output_id, &
                                              'y', &
                                              id_ny, &
                                              il_dimsout_id(2)),cl_fonction)
 
          il_status = fi_ncError(NF90_DEF_DIM(il_output_id, &
                                              'depth', &
                                              id_nz, &
                                              il_dimsout_id(3)),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Define longitude variable and attributes
          il_status = fi_ncError(NF90_DEF_VAR(il_output_id, &
                                              'longitude', &
                                              NF90_FLOAT, &
                                              il_dimsout_id(1:2), &
                                              il_lonout_id),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_lonout_id, &
                                              'long_name', &
                                              'longitude'),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_lonout_id, &
                                              'standard_name', &
                                              'longitude'),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_lonout_id, &
                                              'units', &
                                              'degrees_east'),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_lonout_id, &
                                              'unit_long', &
                                              'Degrees East'),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_lonout_id, &
                                              'axis', &
                                              'X'),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Define latitude variable and attributes
          il_status = fi_ncError(NF90_DEF_VAR(il_output_id, &
                                              'latitude', &
                                              NF90_FLOAT, &
                                              il_dimsout_id(1:2), &
                                              il_latout_id),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_latout_id, &
                                              'long_name', &
                                              'latitude'),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_latout_id, &
                                              'standard_name', &
                                              'latitude'),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_latout_id, &
                                              'units', &
                                              'degrees_north'),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_latout_id, &
                                              'unit_long', &
                                              'Degrees North'),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_latout_id, &
                                              'axis', &
                                              'Y'),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Define depth variable and attributes
          il_status = fi_ncError(NF90_DEF_VAR(il_output_id, &
                                              'depth', &
                                              NF90_FLOAT, &
                                              il_dimsout_id(3), &
                                              il_depthout_id),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_depthout_id, &
                                              'long_name', &
                                              'depth'),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_depthout_id, &
                                              'standard_name', &
                                              'depth'),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_depthout_id, &
                                              'units', &
                                              'Meters'),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_depthout_id, &
                                              'positive', &
                                              'down'),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_depthout_id, &
                                              'unit_long', &
                                              'm'),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_depthout_id, &
                                              'axis', &
                                              'Z'),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Define cosinus variable and attributes
          IF (PRESENT(rda_cos)) THEN
             il_status = fi_ncError(NF90_DEF_VAR(il_output_id, &
                                                 'grid_cos', &
                                                 NF90_FLOAT, &
                                                 il_dimsout_id(1:2), &
                                                 il_cosout_id),cl_fonction)
 
             il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                                 il_cosout_id, &
                                                 'long_name', &
                                                 'grid_cos'),cl_fonction)
 
          ENDIF
 
 
          !----------------------------------------------------------------------
          ! Define sinus variable and attributes
          IF (PRESENT(rda_sin)) THEN
             il_status = fi_ncError(NF90_DEF_VAR(il_output_id, &
                                                 'grid_sin', &
                                                 NF90_FLOAT, &
                                                 il_dimsout_id(1:2), &
                                                 il_sinout_id),cl_fonction)
 
             il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                                 il_sinout_id, &
                                                 'long_name', &
                                                 'grid_sin'),cl_fonction)
          ENDIF
 
 
          !----------------------------------------------------------------------
          ! Define mask variable and attributes
          il_status = fi_ncError(NF90_DEF_VAR(il_output_id, &
                                              'mask', &
                                              NF90_INT, &
                                              il_dimsout_id(1:3), &
                                              il_maskout_id),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_maskout_id, &
                                              'long_name', &
                                              'mask'),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_ATT(il_output_id, &
                                              il_maskout_id, &
                                              'units', &
                                              'unitless'),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! End of define mode
          il_status = fi_ncError(NF90_ENDDEF(il_output_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Write arrays:
          !
          il_status = fi_ncError(NF90_PUT_VAR(il_output_id, &
                                              il_depthout_id, &
                                              rda_depth),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_VAR(il_output_id, &
                                              il_lonout_id, &
                                              rda_lon),cl_fonction)
 
          il_status = fi_ncError(NF90_PUT_VAR(il_output_id, &
                                              il_latout_id, &
                                              rda_lat),cl_fonction)
 
          IF (PRESENT(rda_cos)) THEN
             il_status = fi_ncError(NF90_PUT_VAR(il_output_id, &
                                                 il_cosout_id, &
                                                 rda_cos),cl_fonction)
          ENDIF
 
          IF (PRESENT(rda_sin)) THEN
             il_status = fi_ncError(NF90_PUT_VAR(il_output_id, &
                                                 il_sinout_id, &
                                                 rda_sin),cl_fonction)
          ENDIF
 
         il_status = fi_ncError(NF90_PUT_VAR(il_output_id, &
                                             il_maskout_id, &
                                             ida_mask),cl_fonction)
         !-----------------------------------------------------------------------
         ! Close file
         !
         il_status = fi_ncError(NF90_CLOSE(il_output_id),cl_fonction)
 
        END SUBROUTINE MIOL_write_maskorca_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************

          !!=====================================================================
          !> \brief
          !! Description: This function reads the data values from a CLS mask file.
          !!              This function uses pointers, so you don’t have to know
          !!              the correct dimensions of the data arrays.
          !!
          !!              For the longitude type :
          !!                 - 180 means [-180;180] range for the longitude
          !!                 - 360 means [0;360] range
          !!
          !! @param cd_filename       The NetCDF CLS bathymetry filename.
          !!                     You must specify the complete path.
          !! @param id_lontype        Value specifying the longitude bounds:
          !!                     180 for [-180:180] or 360 for [0:360].
          !! @param idpa_mask              The data values to be read.
          !! @param rdpa_lonmask      Returned the longitude values.
          !! @param rdpa_latmask      Returned the latitude values.
          !! @param id_nblon          Returned the length of the longitude dimension.
          !! @param id_nblat          Returned the length of the latitude dimension.
          !!
          !! History :
          !!       \n    Original    : 09/2005 (G. Vinay)
          !!       \n    F90         : 08/2006 (F. Messal)
          !!
          !!=====================================================================
 
 
       SUBROUTINE MIOL_read_maskcls_NC(cd_filename, &
                                        id_lontype, &
                                        idpa_mask, &
                                        rdpa_lonmask, &
                                        rdpa_latmask, &
                                        id_nblon, &
                                        id_nblat)
          USE MFT_error
          USE netcdf
          IMPLICIT NONE
        
          CHARACTER(LEN=*),           INTENT( IN) :: cd_filename
          INTEGER,                    INTENT( IN) :: id_lontype
          INTEGER, DIMENSION(:,:),    POINTER :: idpa_mask
          REAL(KIND=8), DIMENSION(:), POINTER :: rdpa_lonmask, rdpa_latmask
          INTEGER,                    INTENT(OUT), OPTIONAL :: id_nblon, &
                                                               id_nblat
 
          CHARACTER(LEN=30) :: cl_tmp
          CHARACTER(LEN=255) :: cl_fonction

          INTEGER :: il_file_id, il_ji, il_jj, il_jk, &
                     il_indice, il_status
          INTEGER, DIMENSION(:,:), ALLOCATABLE :: ila_grid
          INTEGER, DIMENSION(:,:), ALLOCATABLE :: ila_tmp
          REAL(KIND=4) :: rl_lonmin, rl_latmin, rl_lonstep, rl_latstep, rl_shift
          REAL(KIND=8), DIMENSION(2) :: rla_latlonmin, rla_latlonstep
 
          !
          !*--------------------------------------------------------------------
          !
          cl_fonction='MIOL_read_maskcls_NC'
          !----------------------------------------------------------------------
          ! Open file
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_NOWRITE, &
                                           il_file_id),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Inquire dimensions
          il_status = fi_ncError(NF90_INQ_DIMID(il_file_id, &
                                                'NbLongitudes', &
                                                il_ji),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                        il_ji, &
                                                        cl_tmp, &
                                                        id_nblon),cl_fonction)
 
          il_status = fi_ncError(NF90_INQ_DIMID(il_file_id, &
                                                'NbLatitudes', &
                                                il_ji),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                        il_ji, &
                                                        cl_tmp, &
                                                        id_nblat),cl_fonction)
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                'LatLonMin', &
                                                il_ji),cl_fonction)
 
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_ji, &
                                              rla_latlonmin),cl_fonction)
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                'LatLonStep', &
                                                il_ji),cl_fonction)
 
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_ji, &
                                              rla_latlonstep),cl_fonction)
 
          rl_latmin=REAL(rla_latlonmin(1))
          rl_lonmin=REAL(rla_latlonmin(2))
 
          rl_latstep=REAL(rla_latlonstep(1))
          rl_lonstep=REAL(rla_latlonstep(2))
 
          !----------------------------------------------------------------------
          ! Memory allocation
          ALLOCATE(ila_grid(id_nblat, &
                            id_nblon), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' ila_grid',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read variable
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                'Grid_0001', &
                                                il_ji),cl_fonction)
 
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_ji, &
                                              ila_grid),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Close file
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Memory allocation
          ALLOCATE(idpa_mask(id_nblon, &
                             id_nblat), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' idpa_mask',cl_fonction)
 
          ALLOCATE(rdpa_lonmask(id_nblon), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' rdpa_lonmask',cl_fonction)
 
          ALLOCATE(rdpa_latmask(id_nblat), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' rdpa_latmask',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Transpose bathy array
 
          SELECT CASE (id_lontype)
             CASE (360)
                idpa_mask = TRANSPOSE(ila_grid)
 
                rl_shift=0.
 
             CASE (180)
                ALLOCATE(ila_tmp(id_nblon, &
                                 id_nblat), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' ila_tmp',cl_fonction)
 
                ila_tmp(:,:) = TRANSPOSE(ila_grid)
 
                il_indice = INT(id_nblon/2)+1
 
                DO il_ji=1,il_indice-1
                   il_jk=(il_indice+il_ji-1)
 
                   DO il_jj=1,id_nblat
                      idpa_mask(il_ji,il_jj)=ila_tmp(il_jk,il_jj)
                   ENDDO
                ENDDO
 
                DO il_ji=il_indice,id_nblon
                   il_jk=(il_ji-il_indice+1)
 
                   DO il_jj=1,id_nblat
                      idpa_mask(il_ji,il_jj)=ila_tmp(il_jk,il_jj)
                   ENDDO
                ENDDO
 
                rl_shift=-180.
 
             CASE DEFAULT
                WRITE(0,*) ' MIOL_read_maskcls_NC : cd_lontype value error. '
                CALL flush(0)
                STOP
 
          ENDSELECT
 
 
          !----------------------------------------------------------------------
          ! Compute longitude and latitude
 
          DO il_ji=1, id_nblon
             rdpa_lonmask(il_ji) = (rl_lonmin+rl_shift) + (il_ji-1)*rl_lonstep
          ENDDO
 
          DO il_ji=1, id_nblat
             rdpa_latmask(il_ji) = (rl_latmin) + (il_ji-1)*rl_latstep
          ENDDO
 
           
     END SUBROUTINE MIOL_read_maskcls_NC
 
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************

          !!=====================================================================
          !> \brief
          !! Description: This function reads the data values from coordinates
          !!              data files. You just have to choose the “grid type”.
          !!              This function uses pointers, so you don’t have to know
          !!              the correct dimensions of the data arrays.
          !!
          !!
          !!  @param cd_filename       The NetCDF ORCA coordinates data filename.
          !!                      You must specify the complete path.
          !!  @param cd_grid           The grid type between 'T' (or 'W'), 'U', 'V' or 'F'.
          !!  @param rdpa_glam         The longitude data values to be read.
          !!  @param rdpa_gphi         The latitude data values to be read.
          !!  @param id_xdim           Returned the length of the x dimension. (Optional)
          !!  @param id_ydim           Returned the length of the y dimension. (Optional)
          !!
          !! History :
          !!	    \n  09/2005  (G. Vinay)
          !!        \n  06/2006  (F. Messal) F77 to F90
          !!        \n  11/2006  (F. Messal) New modifcations
          !<
          !!=====================================================================
  

       SUBROUTINE MIOL_read_coordorca_NC (cd_filename, &
                                           cd_grid, &
                                           rdpa_glam, &
                                           rdpa_gphi, &
                                           id_xdim, &
                                           id_ydim)
 
          USE netcdf
          USE MFT_error
          IMPLICIT NONE
 
           !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),             INTENT(IN) :: cd_filename
          CHARACTER(LEN=1),             INTENT(IN) :: cd_grid
          REAL(KIND=4), DIMENSION(:,:), POINTER :: rdpa_gphi
          REAL(KIND=4), DIMENSION(:,:), POINTER :: rdpa_glam
          INTEGER, OPTIONAL,            INTENT(OUT) :: id_xdim, id_ydim
 
          CHARACTER(LEN=255) :: cl_lam_varname, cl_phi_varname,cl_fonction
          CHARACTER(LEN=255), DIMENSION(3) :: cla_dimname
          INTEGER, DIMENSION(3) :: ila_dimsize
          INTEGER :: il_file_id, il_var_id, il_dim_id, il_status
          REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:,:,:) :: rla_R8_4D
 
          cl_fonction="MIOL_read_coordorca_NC"
          !----------------------------------------------------------------------
          ! Open file
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                 NF90_NOWRITE, &
                                 il_file_id),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Read dimensions length
 
          ! Read the 'x' dimension Id
          il_status = fi_ncError(NF90_INQ_DIMID(il_file_id, &
                                                'x', &
                                                il_dim_id),cl_fonction)
 
          ! Read the 'x' dimension length
          il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                        il_dim_id, &
                                                        cla_dimname(1), &
                                                        ila_dimsize(1)),cl_fonction)
 
          ! Read the 'y' dimension Id
          il_status = fi_ncError(NF90_INQ_DIMID(il_file_id, &
                                                'y', &
                                                il_dim_id),cl_fonction)
 
          ! Read the 'y' dimension length
          il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                        il_dim_id, &
                                                        cla_dimname(2), &
                                                        ila_dimsize(2)),cl_fonction)
 
          ! Read the 'z' dimension Id
          il_status = fi_ncError(NF90_INQ_DIMID(il_file_id, &
                                                'z', &
                                                il_dim_id),cl_fonction)
 
          ! Read the 'z' dimension length
          il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                        il_dim_id, &
                                                        cla_dimname(3), &
                                                        ila_dimsize(3)),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Memory allocation
 
          ALLOCATE(rla_R8_4D(ila_dimsize(1), &
                             ila_dimsize(2), &
                             ila_dimsize(3), &
                             1), &
                   stat=il_status)
          il_status = fi_memError(il_status, 'rla_R8_4D',cl_fonction)
 
          ALLOCATE(rdpa_glam(ila_dimsize(1), &
                             ila_dimsize(2)), &
                   stat=il_status)
          il_status = fi_memError(il_status, 'rdpa_glam',cl_fonction)
 
          ALLOCATE(rdpa_gphi(ila_dimsize(1), &
                             ila_dimsize(2)), &
                   stat=il_status)
          il_status = fi_memError(il_status, 'rdpa_gphi',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Initialization
 
          rla_R8_4D(1:ila_dimsize(1),1:ila_dimsize(2),1:ila_dimsize(3),1:1)=0.
          rdpa_glam(1:ila_dimsize(1),1:ila_dimsize(2))=0.
          rdpa_gphi(1:ila_dimsize(1),1:ila_dimsize(2))=0.
 
 
          !----------------------------------------------------------------------
          ! Read variable
 
          ! Find the correct variable names
          SELECT CASE(cd_grid)
 
             CASE('t','w','T','W')
                cl_lam_varname='glamt'
                cl_phi_varname='gphit'
 
             CASE('u','U')
                cl_lam_varname='glamu'
                cl_phi_varname='gphiu'
 
             CASE('v','V')
                cl_lam_varname='glamv'
                cl_phi_varname='gphiv'
 
             CASE('f','F')
                cl_lam_varname='glamf'
                cl_phi_varname='gphif'
 
             CASE default
                WRITE(0,*) ' MIOL_read_coordorca_NC : grid type unknown ', cd_grid
                CALL flush(0)
                STOP
 
          ENDSELECT
 
          ! Read the longitude variable Id
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cl_lam_varname, &
                                                il_var_id),cl_fonction)
 
          ! Read the longitude values
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_var_id, &
                                              rla_R8_4D),cl_fonction)
 
          rdpa_glam(:,:) = REAL(rla_R8_4D(:,:,1,1),4)
 
          ! Initialize the temporary array
          rla_R8_4D(1:ila_dimsize(1),1:ila_dimsize(2),1:ila_dimsize(3),1:1)=0.
 
          ! Read the latitude variable Id
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cl_phi_varname, &
                                                il_var_id),cl_fonction)
 
          ! Read the latitude values
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_var_id, &
                                              rla_R8_4D),cl_fonction)
 
          rdpa_gphi(:,:) = REAL(rla_R8_4D(:,:,1,1),4)
 
 
          !----------------------------------------------------------------------
          ! Close file
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Return the correct dimensions
          IF (PRESENT(id_xdim)) THEN
             id_xdim = ila_dimsize(1)
          ENDIF
 
          IF (PRESENT(id_ydim)) THEN
             id_ydim = ila_dimsize(2)
          ENDIF
 
 
          !----------------------------------------------------------------------
          ! Memory deallocation
          IF (ALLOCATED(rla_R8_4D)) DEALLOCATE(rla_R8_4D,stat=il_status)
          il_status = fi_memError(il_status, 'rla_R8_4D',cl_fonction)
 
        END SUBROUTINE MIOL_read_coordorca_NC
 
  !******************************************************************************
  !******************************************************************************
!******************************************************************************
 
          !!=====================================================================
          !> \brief
          !! Description: This function reads the data values from an ORCA
          !!              bathymetry file. This function uses pointers, so you
          !!              don’t have to know the correct dimensions of the data
          !!              arrays.Read ORCA bathymetry NetCDF file
          !!
          !! @param cd_filename   The NetCDF ORCA bathymetry filename.
          !!                 You must specify the complete path.
          !! @param idpa_bathy    The bathymetry data values to be read.
          !! @param ld_ismeter    Returned true value if the bathymetry data unit is meter.
          !!
          !! History :
          !!        \n  11/2006  (F. Messal)
          !<
          !!=====================================================================
     
        SUBROUTINE MIOL_read_bathyorca_NC (cd_filename, &
                                           idpa_bathy, &
                                           ld_ismeter)
 
          USE netcdf
          USE MFT_error
          IMPLICIT NONE
 
        
 
          !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),        INTENT(IN) :: cd_filename
          INTEGER, DIMENSION(:,:), POINTER :: idpa_bathy
          LOGICAL, OPTIONAL,       INTENT(OUT) :: ld_ismeter

          CHARACTER(LEN=255), DIMENSION(2) :: cla_dimname 
          CHARACTER(LEN=255) :: cl_fonction
          INTEGER, DIMENSION(2) :: ila_dimsize
          INTEGER :: il_file_id
          INTEGER :: il_var_id
          INTEGER :: il_dim_id
          INTEGER :: il_status
          REAL(KIND=4), ALLOCATABLE, DIMENSION(:,:,:,:) :: rla_R4_4D
 
          cl_fonction="MIOL_read_bathyorca_NC"
          
          !----------------------------------------------------------------------
          ! Open file
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_NOWRITE, &
                                           il_file_id),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Read dimensions length
 
          ! Read the 'x' dimension Id
          il_status = fi_ncError(NF90_INQ_DIMID(il_file_id, &
                                                'x', &
                                                il_dim_id),cl_fonction)
 
          ! Read the 'x' dimension length
          il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                        il_dim_id, &
                                                        cla_dimname(1), &
                                                        ila_dimsize(1)),cl_fonction)
 
          ! Read the 'y' dimension Id
          il_status = fi_ncError(NF90_INQ_DIMID(il_file_id, &
                                                'y', &
                                                il_dim_id),cl_fonction)
 
          ! Read the 'y' dimension length
          il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                        il_dim_id, &
                                                        cla_dimname(2), &
                                                        ila_dimsize(2)),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Memory allocation
 
          ALLOCATE(rla_R4_4D(ila_dimsize(1), &
                             ila_dimsize(2), &
                             1, &
                             1), &
                   stat=il_status)
          il_status = fi_memError(il_status, 'rla_R4_4D',cl_fonction)
 
          ALLOCATE(idpa_bathy(ila_dimsize(1), &
                              ila_dimsize(2)), &
                   stat=il_status)
          il_status = fi_memError(il_status, 'idpa_bathy',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Initialization
 
          rla_R4_4D(1:ila_dimsize(1),1:ila_dimsize(2),1:1,1:1)=0.
          idpa_bathy(1:ila_dimsize(1),1:ila_dimsize(2))=0.
 
 
          !----------------------------------------------------------------------
          ! find variable
          IF (NF90_INQ_VARID(il_file_id, 'Bathymetry', il_var_id).EQ.NF90_NOERR) THEN
             ld_ismeter = .TRUE.
          ELSE IF (NF90_INQ_VARID(il_file_id, 'Bathy_level', il_var_id).EQ.NF90_NOERR) THEN
             ld_ismeter = .FALSE.
          ELSE
             WRITE(0,*) " MIOL_read_bathyorca_NC : bathymetry variable not found. "
             CALL flush(0)
             STOP
          ENDIF
 
 
          !----------------------------------------------------------------------
          ! Read variable
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_var_id, &
                                              rla_R4_4D),cl_fonction)
 
          idpa_bathy(:,:) = ANINT(rla_R4_4D(:,:,1,1))
 
 
          !----------------------------------------------------------------------
          ! Close file
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Memory deallocation
 
          IF (ALLOCATED(rla_R4_4D)) DEALLOCATE(rla_R4_4D,stat=il_status)
          il_status = fi_memError(il_status, 'rla_R4_4D',cl_fonction)
 
          
     END SUBROUTINE MIOL_read_bathyorca_NC
 
  !******************************************************************************
  !******************************************************************************
 
          !!=====================================================================
          !> \brief
          !! Description: This function reads the data values from a CLS bathymetry
          !!              file. This function uses pointers, so you don’t have to
          !!              know the correct dimensions of the data arrays.
          !!
          !! @param cd_filename       The NetCDF ORCA bathymetry filename.
          !!                     You must specify the complete path.
          !! @param id_lontype        Value specifying the longitude bounds: 180 for [-180:180]
          !!                     or 360 for [0:360].
          !! @param idpa_bathy        The bathymetry data values to be read.
          !! @param rdpa_lonbathy     Returned the longitude values.
          !! @param rdpa_latbathy     Returned the latitude values.
          !! @param id_nblon          Returned the length of the longitude dimension.
          !! @param id_nblat          Returned the length of the latitude dimension.
          !!
          !! History :
          !!       \n    Original    : 09/2005 (G. Vinay)
          !!       \n    F90         : 08/2006 (F. Messal)
          !<
          !!=====================================================================

       SUBROUTINE MIOL_read_bathycls_NC(cd_filename, &
                                         id_lontype, &
                                         idpa_bathy, &
                                         rdpa_lonbathy, &
                                         rdpa_latbathy, &
                                         id_nblon, &
                                         id_nblat)
 
          USE netcdf
          USE MFT_error
          IMPLICIT NONE
          
 
          CHARACTER(LEN=*),           INTENT(IN) :: cd_filename
          INTEGER,                    INTENT(IN) :: id_lontype
          INTEGER(KIND=4), DIMENSION(:,:),    POINTER :: idpa_bathy
          REAL(KIND=8), DIMENSION(:), POINTER :: rdpa_lonbathy, &
                                                 rdpa_latbathy
          INTEGER,                    INTENT(OUT), OPTIONAL :: id_nblon, &
                                                               id_nblat
 
          CHARACTER(LEN=30) :: cl_tmp,cl_fonction
          INTEGER :: il_file_id, il_ji, il_jj, il_jk, &
                     il_indice, il_status
          INTEGER(KIND=2), DIMENSION(:,:), ALLOCATABLE :: ila_grid
          INTEGER, DIMENSION(:,:), ALLOCATABLE :: ila_tmp
          REAL(KIND=4) :: rl_lonmin, rl_latmin, rl_lonstep, rl_latstep, rl_shift
          REAL(KIND=8), DIMENSION(2) :: rla_latlonmin, rla_latlonstep
 
          cl_fonction="MIOL_read_bathycls_NC"
          
          !----------------------------------------------------------------------
          ! Open file
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                 NF90_NOWRITE, &
                                 il_file_id),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Inquire dimensions and variables
          il_status = fi_ncError(NF90_INQ_DIMID(il_file_id, &
                                                'NbLongitudes', &
                                                il_ji),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                        il_ji, &
                                                        cl_tmp, &
                                                        id_nblon),cl_fonction)
 
          il_status = fi_ncError(NF90_INQ_DIMID(il_file_id, &
                                                'NbLatitudes', &
                                                il_ji),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                        il_ji, &
                                                        cl_tmp, &
                                                        id_nblat),cl_fonction)

          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                'LatLonMin', &
                                                il_ji),cl_fonction)
 
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_ji, &
                                              rla_latlonmin),cl_fonction)

          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                'LatLonStep', &
                                                il_ji),cl_fonction)
 
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_ji, &
                                              rla_latlonstep),cl_fonction)
 
          rl_latmin=REAL(rla_latlonmin(1))
          rl_lonmin=REAL(rla_latlonmin(2))
 
          rl_latstep=REAL(rla_latlonstep(1))
          rl_lonstep=REAL(rla_latlonstep(2))
          ALLOCATE(ila_grid(id_nblat, &
                            id_nblon), &
                   stat=il_status)
          il_status = fi_memError(il_status, 'ila_grid',cl_fonction)
 
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                'Grid_0001', &
                                                il_ji),cl_fonction)
 
          il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                              il_ji, &
                                              ila_grid),cl_fonction)

          !----------------------------------------------------------------------
          ! Close file
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Memory allocation
          ALLOCATE(idpa_bathy(id_nblon, &
                              id_nblat), &
                   stat=il_status)
          il_status = fi_memError(il_status, 'idpa_bathy',cl_fonction)
 
          ALLOCATE(rdpa_lonbathy(id_nblon), &
                   stat=il_status)
          il_status = fi_memError(il_status, 'rdpa_lonbathy',cl_fonction)
 
          ALLOCATE(rdpa_latbathy(id_nblat), &
                   stat=il_status)
          il_status = fi_memError(il_status, 'rdpa_latbathy',cl_fonction)
 
          !----------------------------------------------------------------------
          ! Transpose bathy array
 
          SELECT CASE (id_lontype)
 
             CASE (360)
                idpa_bathy = TRANSPOSE(ila_grid)
                rl_shift=0.
 
             CASE (180)
                ALLOCATE(ila_tmp(id_nblon, &
                                 id_nblat), &
                         stat=il_status)
                il_status = fi_memError(il_status, 'ila_tmp',cl_fonction)
 
                ila_tmp = TRANSPOSE(ila_grid)
 
                il_indice = INT(id_nblon/2)+1
 
                DO il_ji=1,il_indice-1
                   il_jk=(il_indice+il_ji-1)
 
                   DO il_jj=1,id_nblat
                      idpa_bathy(il_ji,il_jj)=ila_tmp(il_jk,il_jj)
                   ENDDO
                ENDDO
                DO il_ji=il_indice,id_nblon
                   il_jk=(il_ji-il_indice+1)
 
                   DO il_jj=1,id_nblat
                      idpa_bathy(il_ji,il_jj)=ila_tmp(il_jk,il_jj)
                   ENDDO
                ENDDO
 
                rl_shift=-180.
             CASE DEFAULT
                WRITE(0,*) ' MIOL_read_bathycls_NC : cd_lontype value error.'
                CALL flush(0)
                STOP
 
 
          ENDSELECT
 
 
          !----------------------------------------------------------------------
          ! Compute longitude and latitude
 
          DO il_ji=1, id_nblon
             rdpa_lonbathy(il_ji) = (rl_lonmin+rl_shift) + (il_ji-1)*rl_lonstep
          ENDDO
          DO il_ji=1, id_nblat
             rdpa_latbathy(il_ji) = (rl_latmin) + (il_ji-1)*rl_latstep
          ENDDO
     END SUBROUTINE MIOL_read_bathycls_NC
 
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
         !!=====================================================================
         !> \brief
         !! Description: This function creates and defines the dimensions of a
         !!              new NetCDF file.
         !!
         !! @param cd_filename       A NetCDF filename. You must specify the complete
         !!                     path.
         !! @param id_nbdim          The number of dimensions.
         !! @param cda_dimname       Vector of characters specifying the name of each
         !!                     dimension.
         !! @param ida_dimlen        Vector of integers specifying the length of each
         !!                     dimension.
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version 1.0
         !<
         !!=====================================================================
 
       SUBROUTINE MIOL_create_file_NC (cd_filename, &
                                        id_nbdim, &
                                        cda_dimname, &
                                        ida_dimlen)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),                        INTENT(IN) :: cd_filename
         INTEGER,                                 INTENT(IN) :: id_nbdim
         CHARACTER(LEN=*), DIMENSION(id_nbdim),   INTENT(IN) :: cda_dimname
         INTEGER, DIMENSION(id_nbdim),            INTENT(IN) :: ida_dimlen

         INTEGER :: il_ji, il_output_id, il_status
         INTEGER, DIMENSION(id_nbdim) :: ila_dimsout_id
         CHARACTER(LEN=255), DIMENSION(id_nbdim) :: cla_dimname
         CHARACTER(LEN=255) :: cl_fonction
         
         !-----------------------------------------------------------------------
  
         cl_fonction="MIOL_create_file_NC" 

         DO il_ji=1, id_nbdim
            cla_dimname(il_ji) = TRIM(cda_dimname(il_ji))
         ENDDO
 
         !------------------------------------------------------------------------
         ! Create the Netcdf file
         il_status = fi_ncError(NF90_CREATE(TRIM(cd_filename), &
                                            or(NF90_CLOBBER, &
                                            NF90_64bit_offset),&
                                            il_output_id),cl_fonction)
 
 
         !------------------------------------------------------------------------
         ! Write dimensions
 
         DO il_ji = 1, id_nbdim
            il_status = fi_ncError(NF90_DEF_DIM(il_output_id, &
                                                cda_dimname(il_ji), &
                                                ida_dimlen(il_ji), &
                                                ila_dimsout_id(il_ji)),cl_fonction)

         ENDDO
         
         il_status = fi_ncError(NF90_ENDDEF(il_output_id),cl_fonction)
 
         !------------------------------------------------------------------------
         ! Close file
 
         il_status = fi_ncError(NF90_CLOSE(il_output_id),cl_fonction)
 
 
       END SUBROUTINE MIOL_create_file_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
       
         !!======================================================================
         !!
         !! Description: Create the target file with the same dimensions of the
         !!              source file.
         !!   @param  cl_sourcefile  ::A NetCDF input filename. You must specify the complete path
         !!   @param  cl_targetfile  ::NetCDF output filename. You must specify the complete path
         !!  \n History :
         !!        \n  10/2006  (F. Messal)
         !!
         !!======================================================================
 
       SUBROUTINE MIOL_copy_dimensions_NC(cl_sourcefile, &
                                    cl_targetfile)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
       
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*), INTENT(IN) :: cl_sourcefile
         CHARACTER(LEN=*), INTENT(IN) :: cl_targetfile
 
         INTEGER :: il_sourcefileId, il_targetfileId, il_nbdim, il_nbvar, il_nbatt, &
                    il_ji, il_status, il_jj
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimlen
         CHARACTER(LEN=50), DIMENSION(:), ALLOCATABLE :: cla_dimname, cla_attname
         CHARACTER(LEN=50) :: cl_fonction
 
         !-----------------------------------------------------------------------
         cl_fonction="MIOL_copy_dimensions_NC"
         
         il_status = NF90_OPEN(TRIM(cl_targetfile), NF90_WRITE, il_targetfileId)
 
         IF (il_status .NE. NF90_NOERR) THEN
 
            il_status = fi_ncError(NF90_OPEN(TRIM(cl_sourcefile), &
                                   NF90_NOWRITE, &
                                   il_sourcefileId),cl_fonction)
 
            il_status = fi_ncError(NF90_CREATE(TRIM(cl_targetfile), &
                                   NF90_WRITE, &
                                   il_targetfileId),cl_fonction)
 
            il_status = fi_ncError(NF90_INQUIRE(il_sourcefileId, &
                                   il_nbdim, &
                                   il_nbvar, &
                                   il_nbatt),cl_fonction)
 
            ALLOCATE(cla_dimname(il_nbdim), stat = il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
            ALLOCATE(ila_dimlen(il_nbdim), stat = il_status)
            il_status = fi_memError(il_status, ' ila_dimlen',cl_fonction)
            ALLOCATE(cla_attname(il_nbatt), stat = il_status)
            il_status = fi_memError(il_status, ' cla_attname',cl_fonction)
 
 
            DO il_ji=1, il_nbdim
               il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_sourcefileId, &
                                                             il_ji, &
                                                             cla_dimname(il_ji), &
                                                             ila_dimlen(il_ji)),cl_fonction)
 
               il_status = fi_ncError(NF90_DEF_DIM(il_targetfileId, &
                                                   TRIM(cla_dimname(il_ji)), &
                                                   ila_dimlen(il_ji), &
                                                   il_jj),cl_fonction)
            ENDDO
 
            DO il_ji=1, il_nbatt
               il_status = fi_ncError(NF90_INQ_ATTNAME(il_sourcefileId, &
                                                       NF90_GLOBAL, &
                                                       il_ji, &
                                                       cla_attname(il_ji)),cl_fonction)
 
               il_status = fi_ncError(NF90_COPY_ATT(il_sourcefileId, &
                                                    NF90_GLOBAL, &
                                                    TRIM(cla_attname(il_ji)), &
                                                    il_targetfileId, &
                                                    NF90_GLOBAL),cl_fonction)
            ENDDO
 
            il_status = fi_ncError(NF90_CLOSE(il_sourcefileId),cl_fonction)
            il_status = fi_ncError(NF90_CLOSE(il_targetfileId),cl_fonction)
 
            DEALLOCATE(cla_dimname, stat = il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
            DEALLOCATE(ila_dimlen, stat = il_status)
            il_status = fi_memError(il_status, ' ila_dimlen',cl_fonction)
            DEALLOCATE(cla_attname, stat = il_status)
            il_status = fi_memError(il_status, ' cla_attname',cl_fonction)
 
 
         ELSE
            WRITE(0,*) ' MIOL_copy_dim_NC: the file allready exists. '
            CALL flush(0)
            STOP
 
        ENDIF
 
        END SUBROUTINE MIOL_copy_dimensions_NC
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
        
         !!======================================================================
         !> \brief
         !! Description: This function defines a new dimension in a NetCDF file.
         !!
         !! @param cd_filename       A NetCDF filename. You must specify the complete path.
         !! @param cd_dimname        The dimension name.
         !! @param cd_len            The dimension length.
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version 1.0
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_add_dimension_NC(cd_filename, &
                                         cd_dimname, &
                                         id_len)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),   INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),   INTENT(IN) :: cd_dimname
         INTEGER,            INTENT(IN) :: id_len
 
         INTEGER :: il_input_id, il_dimid, il_status
         CHARACTER(LEN=30) :: cl_fonction
 
 
         cl_fonction ="MIOL_add_dimension_NC"
        
         !------------------------------------------------------------------------
         ! Open the Netcdf file
 
         il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                          NF90_WRITE, &
                                          il_input_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Add the new dimension
 
         il_status = fi_ncError(NF90_REDEF(il_input_id),cl_fonction)
 
         il_status = fi_ncError(NF90_DEF_DIM(il_input_id, &
                                             TRIM(cd_dimname), &
                                             id_len, &
                                             il_dimid),cl_fonction)
 
         il_status = fi_ncError(NF90_ENDDEF(il_input_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Close file
 
         il_status = fi_ncError(NF90_CLOSE(il_input_id),cl_fonction)
 
 
       END SUBROUTINE MIOL_add_dimension_NC
 
 
  !******************************************************************************
  !******************************************************************************

END MODULE MIOL_UTIL
