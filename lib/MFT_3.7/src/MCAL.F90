!> \brief Module which contain interfaces and subroutines for calculation
!! \author C.REGNIER Miol 3.5
!! \date Mai 2013 
!!  \version 3.5
!< 
MODULE MCAL
  USE MIOL_PARAM 
  INTERFACE MCAL_interp1D_cubicspline
     MODULE PROCEDURE MCAL_interp1D_cubicspline_R4, &
          MCAL_interp1D_cubicspline_R8
  END INTERFACE
  
  INTERFACE MCAL_generate_grid
     MODULE PROCEDURE MCAL_gen_reggrid_gtype1D, &
                         MCAL_gen_reggrid_gtype2D, &
                         MCAL_gen_strgrid_gtype2D
  END INTERFACE
 
  INTERFACE MCAL_interp2D_4NNmean
     MODULE PROCEDURE MCAL_int2D_gtype1D_4NNmean_I, &
                      MCAL_int2D_gtype1D_4NNmean_R4, &
                      MCAL_int2D_gtype1D_4NNmean_R8, &
                      MCAL_int2D_gtype2D_4NNmean_I, &
                      MCAL_int2D_gtype2D_4NNmean_R4, &
                      MCAL_int2D_gtype2D_4NNmean_R8
  END INTERFACE
 
  INTERFACE MCAL_generate_depthvalues
     MODULE PROCEDURE MCAL_generate_depthvalues_R4, &
                      MCAL_generate_depthvalues_R8
  END INTERFACE
 
  INTERFACE MCAL_compute_gridangle
     MODULE PROCEDURE MCAL_compute_gridangle_R4, &
                      MCAL_compute_gridangle_R8
  END INTERFACE
       
  INTERFACE MCAL_generate_canevas
     MODULE PROCEDURE MCAL_generate_canevas_1Ddim, &
                      MCAL_generate_canevas_2Ddim
  END INTERFACE
  
  INTERFACE MCAL_compute_short
     MODULE PROCEDURE  MCAL_compute_short_R4_2D,&
                       MCAL_compute_short_R4_3D,&
                       MCAL_compute_short_R4_4D,&
                       MCAL_compute_short_R8_2D,&
                       MCAL_compute_short_R8_3D,&
                       MCAL_compute_short_R8_4D
  END INTERFACE

  INTERFACE MCAL_transform_char_to_int
     MODULE PROCEDURE transform_char_to_int
  END INTERFACE
  INTERFACE MCAL_read_lightout
     MODULE PROCEDURE read_lightout_nc
  END INTERFACE
  
  INTERFACE MCAL_my_ichar
     MODULE PROCEDURE my_ichar
  END INTERFACE
  
  INTERFACE MCAL_IDW
     MODULE PROCEDURE MCAL_IDW
  END INTERFACE
  INTERFACE MCAL_generate_maskvalues
     MODULE PROCEDURE MCAL_generate_maskvalues
  END INTERFACE
  
  PUBLIC :: MCAL_conversion_STU50_CAL,MCAL_compute_distance
  
 CONTAINS
 

!*=================================================================================== !
!> \brief
!!**** MCAL_IDW \n
!!
!!     Purpose : computes the inverse-distance weights for a
!!     nearest-neighbor interpolation \n
!!
!!     Input ::  \n
!!      @param rd_lonprof : longitude of in situ data
!!      @param rd_latprof : latitude of in situ data
!!      @param rda_longrid : longitudes of the grid
!!      @param rda_latgrid : latitudes of the grid
!!
!!     Output : \n
!!      @param rda_poids : weight of 4 points  \n
!!
!!***  Method: \n
!!     ------- \n
!!     Classical IDW method \n
!!
!!     Externals: \n
!!     ---------- \n
!!     MCAL_compute_distance \n
!!
!!     History: \n
!!     -------- \n
!!   \n Version    Programmer      Date            Description
!!   \n -------    ----------      ----            -----------
!!   \n  1.0        C. REGNIER       23/01/2007      Implementation for class4 
!!   \n  1.1        C. REGNIER       25/06/2012      Ajout corrections clément sur MCAL_IDW
!<
!
!*=================================================================================== !

      SUBROUTINE MCAL_IDW(rd_lonprof, &
                         rd_latprof, &
                         rda_longrid, &
                         rda_latgrid, &
                         rda_poids,&
                         id_nx, &
                         id_ny, &
                         ida_xindex, &
                         ida_yindex,&
                         lda_nbr_mask,&
                         rda_dist)

!      
!** + DECLARATIONS
!
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: id_nx, &
                             id_ny
      REAL(KIND=4), INTENT(IN) :: rd_lonprof, &
                                  rd_latprof
      REAL(KIND=4), DIMENSION(id_nx, id_ny), INTENT(IN) :: rda_longrid, &
                                                           rda_latgrid
      INTEGER, parameter :: il_num_neighbors=4  ! num nearest neighbors to interpolate from
  
      INTEGER, DIMENSION(il_num_neighbors),INTENT(IN)   :: ida_xindex, &
                                            ida_yindex
!
      LOGICAL,DIMENSION(il_num_neighbors),INTENT(IN)                    :: lda_nbr_mask
      REAL(KIND=4), DIMENSION(il_num_neighbors), INTENT(OUT) :: rda_poids
      REAL(KIND=4), DIMENSION(il_num_neighbors), INTENT(OUT),OPTIONAL :: rda_dist

      REAL(KIND=4), PARAMETER :: rl_ra = 6371.0
      REAL(KIND=8), DIMENSION(il_num_neighbors)              :: rla_dist
      REAL(KIND=8) ::rl_dist_tot,rl_zlon,rl_zlat
      INTEGER      :: il_xind, il_yind
      INTEGER :: il_n,il_i,il_miss,il_miss2
      LOGICAL,DIMENSION(4)      :: ll_ref
!   
!
!***********************************************************************************
!
      !* Initialisation

      rl_dist_tot=0.0
      rla_dist(:)=0.0
      il_miss=9999.
      il_miss2=0.
        
      !*check if on of the 4 points is the same as reference point
      ll_ref(1:4)=.FALSE.
      DO  il_i=1,il_num_neighbors
         IF ( ((ida_xindex(il_i) == il_miss) .AND. (ida_yindex(il_i) == il_miss)) .OR. &
            ((ida_xindex(il_i) == il_miss2) .AND. (ida_yindex(il_i) == il_miss2)) ) THEN
             ll_ref(il_i)=.FALSE.
          ELSE
             il_xind = ida_xindex(il_i)
             il_yind = ida_yindex(il_i)
             rl_zlon = rda_longrid(il_xind,il_yind)
             rl_zlat = rda_latgrid(il_xind,il_yind)
             IF( rl_zlon==rd_lonprof .AND. rl_zlat==rd_latprof ) ll_ref(il_i)=.TRUE.
          ENDIF
      ENDDO

      !* Compute the different distances
      IF( COUNT(ll_ref) .NE. 0 )THEN
         DO  il_i=1,il_num_neighbors
            rda_poids(il_i) = COUNT((/ll_ref(il_i)/))
         ENDDO
      ELSE
         DO  il_i=1,il_num_neighbors
            IF ( ((ida_xindex(il_i) == il_miss) .AND. (ida_yindex(il_i) == il_miss)) .OR. &
               ((ida_xindex(il_i) == il_miss2) .AND. (ida_yindex(il_i) == il_miss2)) ) THEN
                  rla_dist(il_i)=0.0
                  ll_ref(il_i)=.FALSE.
            ELSE
                 ll_ref(il_i)=.TRUE.
                 CALL MCAL_compute_distance(rd_lonprof, & ! point A coordinates (lon)
                                            rd_latprof, &            ! point A coordinates (lat)
                                            rda_longrid(ida_xindex(il_i),ida_yindex(il_i)), &            ! point B coordinates (lon)
                                            rda_latgrid(ida_xindex(il_i),ida_yindex(il_i)), &            ! point B coordinates (lat)
                                            rla_dist(il_i))
            ENDIF	
            IF (lda_nbr_mask(il_i)) THEN
              rla_dist(il_i) = 1/rla_dist(il_i)
              rl_dist_tot = rl_dist_tot + rla_dist(il_i)
            ENDIF
         ENDDO
         !** Calcul des poids pour chaque points
         DO il_n=1,il_num_neighbors
            IF (lda_nbr_mask(il_n)) THEN
               rda_poids(il_n) = rla_dist(il_n)/rl_dist_tot
            ELSE
               rda_poids(il_n)=0.0
            ENDIF
         ENDDO
      ENDIF
      rda_poids(:)=REAL(rda_poids(:),KIND=4)
      rda_dist(:)=REAL(1/rla_dist(:),KIND=4)*rl_ra  
   ! PRINT *,'rda_poids',rda_poids(:)

!
!*===================================================================================
!
ENDSUBROUTINE  MCAL_IDW
!
!*===================================================================================
!*===================================================================================
!
        !!=======================================================================
        !> \brief
        !! Description: This function creates the mask on the chosen grid
        !!              ('t', 'u', 'v', 'f', 'w') from an original mask based
        !!              on the 'T' grid. \n
        !!
        !!   @param cd_grid         The type of the chosen grid: 't', 'u', 'v', 'w', 'f'
        !!   @param ida_mask_3D     The original mask on the 't' grid in, the new mask out.
        !!   @param id_nx           The length of the x dimension.
        !!   @param id_ny           The length of the y dimension.
        !!   @param id_nz           The length of the z dimension.
        !!
        !! History : 
        !!        \n  07/2006  (F. Messal)
        !<
        !!=======================================================================
  
      SUBROUTINE MCAL_generate_maskvalues(cd_grid, &
                                            ida_mask_3D, &
                                            id_nx, &
                                            id_ny, &
                                            id_nz)
 
        IMPLICIT NONE
        
        !------------------------------------------------------------------------
 
        CHARACTER(LEN=1), INTENT(IN) :: cd_grid
        INTEGER :: id_nx, id_ny, id_nz
        INTEGER, DIMENSION(id_nx, id_ny, id_nz), INTENT(OUT) :: ida_mask_3D
        !INTEGER, DIMENSION(:,:,:),POINTER :: ida_mask_3D

 
       ! INTEGER, DIMENSION(id_nx, id_ny, id_nz) :: ila_mask
        INTEGER, DIMENSION(:,:,:),ALLOCATABLE    :: ila_mask
        INTEGER :: il_ji, il_jj, il_jk
 
        !------------------------------------------------------------------------
        ALLOCATE(ila_mask(id_nx, id_ny, id_nz)) 
        IF(cd_grid=='t' .OR. cd_grid=='T' .OR. cd_grid=='w' .OR. cd_grid=='W') THEN
           ila_mask(:,:,:) = ida_mask_3D(:,:,:)
        ENDIF

        IF(cd_grid=='f' .OR. cd_grid=='F') THEN
           ila_mask(:,:,:)= 0
           DO il_ji=1,id_nx-1
              DO il_jj=1, id_ny-1
                 DO il_jk=1, id_nz
                    ila_mask(il_ji,il_jj,il_jk) = ida_mask_3D(il_ji,il_jj,il_jk)* &
                                                  ida_mask_3D(il_ji+1,il_jj,il_jk)* &
                                                  ida_mask_3D(il_ji,il_jj+1,il_jk)* &
                                                  ida_mask_3D(il_ji+1,il_jj+1,il_jk)
                 END DO
              END DO
           END DO
        ENDIF
 
        IF(cd_grid=='u' .OR. cd_grid=='U') THEN
           ila_mask(:,:,:)= 0
           DO il_ji=1,id_nx-1
              DO il_jj=1, id_ny
                 DO il_jk=1, id_nz
                    ila_mask(il_ji,il_jj,il_jk) = ida_mask_3D(il_ji,il_jj,il_jk)* &
                                                  ida_mask_3D(il_ji+1,il_jj,il_jk)
                 END DO
              END DO
           END DO
        ENDIF
 
        IF(cd_grid=='v' .OR. cd_grid=='V') THEN
           ila_mask(:,:,:)= 0
           DO il_ji=1,id_nx
              DO il_jj=1, id_ny-1
                 DO il_jk=1, id_nz
                    ila_mask(il_ji,il_jj,il_jk) = ida_mask_3D(il_ji,il_jj,il_jk)* &
                                                  ida_mask_3D(il_ji,il_jj+1,il_jk)
                 END DO
              END DO
           END DO
        ENDIF
        ida_mask_3D(1:id_nx,1:id_ny,1:id_nz) = ila_mask(1:id_nx,1:id_ny,1:id_nz)
 
        ENDSUBROUTINE MCAL_generate_maskvalues
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
          !!=====================================================================
          !> \brief
          !! Description: Generate the model levels from an analytical function.
          !!
          !!  @param id_nblevels       The number of depth levels to be generated.
          !!  @param rda_tdepth         Vector of depth values on a 'T' type grid.
          !!  @param rda_wdepth         Vector of depth values on a 'W' type grid.(Optional)
          !!
          !! History :
          !!   \n      Original    : 03/1988 (G. Madec)
          !!   \n      Additions   : 11/1991 (G. Madec)
          !!   \n      Corrections : 06/1992 Doctor norme (M. Imbard)
          !!   \n      Additions   : 03/1993 Analytic function (G. MadeC, M. Imbard)
          !!   \n      Additions   : 12/1995 Generalized vertical coordinate (G. Madec)
          !!   \n      Evolutions  : 06/2006 Fortran90 (F. Messal)
          !<
          !!=====================================================================

        SUBROUTINE MCAL_generate_depthvalues_R8 (id_nlevels, &
                                                 rda_tdepth, &
                                                 rda_wdepth)
 
          USE netcdf
          IMPLICIT NONE
         
           !----------------------------------------------------------------------
 
          INTEGER, INTENT(IN) :: id_nlevels
          REAL(KIND=8), DIMENSION(id_nlevels), INTENT(OUT) :: rda_tdepth
          REAL(KIND=8), DIMENSION(id_nlevels), INTENT(OUT), OPTIONAL :: rda_wdepth
 
          INTEGER :: il_jk
          REAL(8) :: rl_za0,rl_zsur,rl_za1,rl_zacr,rl_zkth,rl_tz,rl_wz
 
          !----------------------------------------------------------------------
 
          rl_zsur = -2155.7278
          rl_za0 = 128.1196
          rl_za1 = 123.7533
          rl_zkth = 23.5630
          rl_zacr = 9.
 
          !----------------------------------------------------------------------
 
          ! calculate
          DO il_jk=1,id_nlevels
             rl_wz = REAL((il_jk),8)
             rl_tz = REAL((il_jk)+0.5,8)
             rda_tdepth(il_jk) = (rl_za0 * rl_tz) + rl_zsur + (rl_za1 * rl_zacr * LOG( COSH((rl_tz-rl_zkth)/rl_zacr )))
             IF (PRESENT(rda_wdepth)) THEN
                rda_wdepth(il_jk) = (rl_za0 * rl_wz) + rl_zsur + (rl_za1 * rl_zacr * LOG( COSH((rl_wz-rl_zkth)/rl_zacr )))
             ENDIF
 
          END DO
 
 
        ENDSUBROUTINE MCAL_generate_depthvalues_R8
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
          !!=====================================================================
          !> \brief
          !! Description: Generate the model levels from an analytical function.
          !!
          !!  @param id_nblevels       The number of depth levels to be generated.
          !!  @param rda_tdepth         Vector of depth values on a 'T' type grid.
          !!  @param rda_wdepth         Vector of depth values on a 'W' type grid.(Optional)
          !!
          !! History :
          !!   \n        Original    : 03/1988 (G. Madec)
          !!   \n        Additions   : 11/1991 (G. Madec)
          !!   \n        Corrections : 06/1992 Doctor norme (M. Imbard)
          !!   \n        Additions   : 03/1993 Analytic function (G. MadeC, M. Imbard)
          !!   \n        Additions   : 12/1995 Generalized vertical coordinate (G. Madec)
          !!   \n        Evolutions  : 06/2006 Fortran90 (F. Messal)
          !<
          !!=====================================================================
        
        SUBROUTINE MCAL_generate_depthvalues_R4 (id_nlevels, &
                                                 rda_tdepth, &
                                                 rda_wdepth)
 
          USE netcdf
          IMPLICIT NONE
 
         
 
          !----------------------------------------------------------------------
 
          INTEGER, INTENT(IN) :: id_nlevels
          REAL(KIND=4), DIMENSION(id_nlevels), INTENT(OUT) :: rda_tdepth
          REAL(KIND=4), DIMENSION(id_nlevels), INTENT(OUT), OPTIONAL :: rda_wdepth
 
          INTEGER :: il_jk
          REAL(4) :: rl_za0,rl_zsur,rl_za1,rl_zacr,rl_zkth,rl_tz,rl_wz
 
          !----------------------------------------------------------------------
 
          rl_zsur = -2155.7278
          rl_za0 = 128.1196
          rl_za1 = 123.7533
          rl_zkth = 23.5630
          rl_zacr = 9.
 
          !----------------------------------------------------------------------
 
          ! calculate
          DO il_jk=1,id_nlevels
             rl_wz = REAL((il_jk),4)
             rl_tz = REAL((il_jk)+0.5,4)
             rda_tdepth(il_jk) = (rl_za0 * rl_tz) + rl_zsur + (rl_za1 * rl_zacr * LOG( COSH((rl_tz-rl_zkth)/rl_zacr )))
             IF (PRESENT(rda_wdepth)) THEN
                rda_wdepth(il_jk) = (rl_za0 * rl_wz) + rl_zsur + (rl_za1 * rl_zacr * LOG( COSH((rl_wz-rl_zkth)/rl_zacr )))
             ENDIF
 
          END DO
 
 
        ENDSUBROUTINE MCAL_generate_depthvalues_R4
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
          !!=====================================================================
          !> \brief
          !! Description: this routine computes extrapolation along the shore by
          !!     the following method :
          !!     1 research of among land points those which have, at least, a sea
          !!       point as neighbour
          !!     2 compute for these land points an extrapolate field value from
          !!       field values of its no-masked neighbour by inverse rd_distance
          !!       weighted interpolation method
          !!     3 And begin again, as many time as id_num_iter value
          !!
          !!     To compute extrapolation coefficient, we use the following method :
          !!     Among points which need extrapolation calculus, we chearch if among
          !!     their 8 neighbours of each point, if there are sea points on the
          !!     initial grid1 mask.
          !!     If there are sea points, we compute coefficient extrapolation by
          !!     inverse rd_distance from these points and we modify mask of
          !!     extrapolated point (false -> true)
          !!     If not, we store these points for the next extrapolation iteration.
          !!
          !!     Then at the next iteration, we use the new mask just modified to
          !!     apply again, the extrapolation method.
          !!
          !!     Maximum number of iteration is 5
          !!
          !!  @param rda_value      3D array of values to be extrapolated.
          !!  @param ida_mask_3D    3D array of the mask values.
          !!  @param rda_lon        Vector of the original longitude values.
          !!  @param rda_lat        Vector of the original latitude values.
          !!  @param id_nx          Length of the x dimension.
          !!  @param id_ny          Length of the y dimension.
          !!  @param id_nz          Length of the z dimension.
          !!  @param iter           Number of iterations.
          !!
          !! History :
          !!   \n        05/2006 (S. Besnard) Creation
          !!   \n        11/2006 (F. Messal) CVS version 1.0
          !!   \n        10/2008 (C. REGNIER) Add MFT_error MIOL 3.1
          !<
          !!=====================================================================

        SUBROUTINE MCAL_extrap3D_distweight (rda_value,&
                                             ida_mask_3D,&
                                             rda_lon, &
                                             rda_lat, &
                                             id_nx, &
                                             id_ny, &
                                             id_nz, &
                                             id_num_iter,&
                                             idpa_mask_3D_ex)
 
          USE MFT_error
          IMPLICIT NONE
 
          !----------------------------------------------------------------------
          INTEGER,                                    INTENT(IN) :: id_nx,id_ny,id_nz
          REAL(KIND=4), DIMENSION(id_nx,id_ny,id_nz), INTENT(INOUT) :: rda_value
          INTEGER, DIMENSION(id_nx,id_ny,id_nz),      INTENT(IN) :: ida_mask_3D
          REAL(KIND=4), DIMENSION(id_nx,id_ny,id_nz), INTENT(IN) :: rda_lon, rda_lat
          INTEGER, INTENT(IN) :: id_num_iter
          INTEGER(KIND=4), DIMENSION(:,:,:),POINTER               :: idpa_mask_3D_ex
         

          INTEGER :: il_ji, il_jj, il_jk, il_jim1, il_jjm1, il_jip1, il_jjp1, il_jn, il_iter, il_status
          REAL(KIND=8) :: rl_dist, rl_invdist, rl_sum_inv_dist
          INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: ila_maskin, ila_maskout
          REAL(KIND=4), DIMENSION(:,:,:), ALLOCATABLE :: rla_datain, rla_dataout
          INTEGER, DIMENSION(8,2) :: il_neigh  ! addresses on source grid
          REAL(KIND=4) :: rl_lon_a, rl_lat_a, rl_lon_b, rl_lat_b
          REAL(KIND=8) :: rl_coslat_a, &    ! cos(lat) of source grid point
                          rl_coslon_a, &    ! cos(lon) of source grid point
                          rl_sinlat_a, &    ! sin(lat) of source grid point
                          rl_sinlon_a, &    ! sin(lon) of source grid point
                          rl_coslat_b, &    ! cos(lat) of destination grid point
                          rl_coslon_b, &    ! cos(lon) of destination grid point
                          rl_sinlat_b, &    ! sin(lat) of destination grid point
                          rl_sinlon_b       ! sin(lon) of destination grid point
          REAL(KIND=8), PARAMETER :: rl_pi = 3.14159265359
          REAL(KIND=8), PARAMETER :: rl_deg2rad = rl_pi/180.   ! conversion for deg to rads
          CHARACTER(LEN=255)      :: cl_fonction
          cl_fonction='MCAL_extrap3D_distweight'
          !----------------------------------------------------------------------
          ! Test array
          il_status = fi_arrError(MINVAL(rda_value), &
                                  MAXVAL(rda_value), &
                                  'rda_value',cl_fonction)
 
          il_status = fi_arrError(REAL(MINVAL(ida_mask_3D),4), &
                                  REAL(MAXVAL(ida_mask_3D),4), &
                                  'ida_mask_3D',cl_fonction)
 
          il_status = fi_arrError(MINVAL(rda_lon), &
                                  MAXVAL(rda_lon), &
                                  'rda_lon',cl_fonction)
 
          il_status = fi_arrError(MINVAL(rda_lat), &
                                  MAXVAL(rda_lat), &
                                  'rda_lat',cl_fonction)
 
          !----------------------------------------------------------------------
          ! Data and mask temporary arrays initialization
 
          ALLOCATE(ila_maskout(id_nx, &
                               id_ny, &
                               id_nz), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' ila_maskout',cl_fonction)
 
          ALLOCATE(ila_maskin(id_nx, &
                              id_ny, &
                              id_nz), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' ila_maskin',cl_fonction)
 
          ALLOCATE(rla_datain(id_nx, &
                              id_ny, &
                              id_nz), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' rla_datain',cl_fonction)
 
          ALLOCATE(rla_dataout(id_nx, &
                               id_ny, &
                               id_nz), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' rla_dataout',cl_fonction)
 
          rla_datain(1:id_nx,1:id_ny,1:id_nz) = rda_value(1:id_nx,1:id_ny,1:id_nz)
          ila_maskin(1:id_nx,1:id_ny,1:id_nz) = ida_mask_3D(1:id_nx,1:id_ny,1:id_nz)
          rla_dataout(1:id_nx,1:id_ny,1:id_nz) = rla_datain(1:id_nx,1:id_ny,1:id_nz)
          ila_maskout(1:id_nx,1:id_ny,1:id_nz) = ila_maskin(1:id_nx,1:id_ny,1:id_nz)
 
          !----------------------------------------------------------------------
          ! Iteration on extrapolation method
 
          DO il_iter = 1, id_num_iter
          !WRITE(0,*) 'Iteration n°', il_iter, 'for extrapolation method'
          CALL FLUSH(0)
 
          DO il_jk=1, id_nz
             CALL FLUSH(0)
 
             DO il_jj=2,id_ny-1
 
                DO il_ji=2,id_nx-1
 
                   il_jip1 = il_ji+1
                   il_jim1 = il_ji-1
                   il_jjp1 = il_jj+1
                   il_jjm1 = il_jj-1
                   il_neigh(1,1) = il_jim1
                   il_neigh(1,2) = il_jjm1
                   il_neigh(2,1) = il_ji
                   il_neigh(2,2) = il_jjm1
                   il_neigh(3,1) = il_jip1
                   il_neigh(3,2) = il_jjm1
                   il_neigh(4,1) = il_jim1
                   il_neigh(4,2) = il_jj
                   il_neigh(5,1) = il_jip1
                   il_neigh(5,2) = il_jj
                   il_neigh(6,1) = il_jim1
                   il_neigh(6,2) = il_jjp1
                   il_neigh(7,1) = il_ji
                   il_neigh(7,2) = il_jjp1
                   il_neigh(8,1) = il_jip1
                   il_neigh(8,2) = il_jjp1
 
                   IF ( ( ila_maskin(il_ji,il_jj,il_jk) == 0        ) .AND.&
                     ( (ila_maskin(il_neigh(1,1),il_neigh(1,2),il_jk)==1)   .OR.&
                       (ila_maskin(il_neigh(2,1),il_neigh(2,2),il_jk)==1)   .OR.&
                       (ila_maskin(il_neigh(3,1),il_neigh(3,2),il_jk)==1)   .OR.&
                       (ila_maskin(il_neigh(4,1),il_neigh(4,2),il_jk)==1)   .OR.&
                       (ila_maskin(il_neigh(5,1),il_neigh(5,2),il_jk)==1)   .OR.&
                       (ila_maskin(il_neigh(6,1),il_neigh(6,2),il_jk)==1)   .OR.&
                       (ila_maskin(il_neigh(7,1),il_neigh(7,2),il_jk)==1)   .OR.&
                       (ila_maskin(il_neigh(8,1),il_neigh(8,2),il_jk)==1) )  )  &
                   THEN
 
                      ila_maskout(il_ji,il_jj,il_jk) = 1
 
                      ! Compute total inverse rd_distance to norme weight
                      rl_sum_inv_dist = 0
                      DO il_jn=1,8
 
                         IF (ila_maskin(il_neigh(il_jn,1), il_neigh(il_jn,2), il_jk) == 1) THEN
 
                            ! compute distance
 
                            rl_lon_a = rda_lon(il_ji,il_jj,il_jk)
                            rl_lat_a = rda_lat(il_ji,il_jj,il_jk)
                            rl_lon_b = rda_lon(il_neigh(il_jn,1),il_neigh(il_jn,2),il_jk)
                            rl_lat_b = rda_lat(il_neigh(il_jn,1),il_neigh(il_jn,2),il_jk)
 
                            rl_coslat_a = COS(rl_lat_a*rl_deg2rad)
                            rl_coslon_a = COS(rl_lon_a*rl_deg2rad)
                            rl_sinlat_a = SIN(rl_lat_a*rl_deg2rad)
                            rl_sinlon_a = SIN(rl_lon_a*rl_deg2rad)
 
                            rl_coslat_b = COS(rl_lat_b*rl_deg2rad)
                            rl_coslon_b = COS(rl_lon_b*rl_deg2rad)
                            rl_sinlat_b = SIN(rl_lat_b*rl_deg2rad)
                            rl_sinlon_b = SIN(rl_lon_b*rl_deg2rad)
 
                            rl_dist = ACOS( rl_sinlat_a*rl_sinlat_b + &
                                            rl_coslat_a*rl_coslat_b*  &
                                          ( rl_coslon_a*rl_coslon_b + &
                                            rl_sinlon_a*rl_sinlon_b) )
 
                            rl_invdist = 1 / rl_dist
                            rl_sum_inv_dist = rl_sum_inv_dist + rl_invdist
 
                         ENDIF
 
                      ENDDO
 
                      ! Compute weight
                      rla_dataout(il_ji,il_jj,il_jk) = 0.0
 
                      DO il_jn=1,8
 
                         IF (ila_maskin(il_neigh(il_jn,1), il_neigh(il_jn,2), il_jk) == 1) THEN
 
                            ! Compute distance
                            rl_lon_a = rda_lon(il_ji,il_jj,il_jk)
                            rl_lat_a = rda_lat(il_ji,il_jj,il_jk)
                            rl_lon_b = rda_lon(il_neigh(il_jn,1),il_neigh(il_jn,2),il_jk)
                            rl_lat_b = rda_lat(il_neigh(il_jn,1),il_neigh(il_jn,2),il_jk)
 
                            rl_coslat_a = COS(rl_lat_a*rl_deg2rad)
                            rl_coslon_a = COS(rl_lon_a*rl_deg2rad)
                            rl_sinlat_a = SIN(rl_lat_a*rl_deg2rad)
                            rl_sinlon_a = SIN(rl_lon_a*rl_deg2rad)
 
                            rl_coslat_b = COS(rl_lat_b*rl_deg2rad)
                            rl_coslon_b = COS(rl_lon_b*rl_deg2rad)
                            rl_sinlat_b = SIN(rl_lat_b*rl_deg2rad)
                            rl_sinlon_b = SIN(rl_lon_b*rl_deg2rad)
 
                            rl_dist = ACOS( rl_sinlat_a*rl_sinlat_b + &
                                            rl_coslat_a*rl_coslat_b*  &
                                          ( rl_coslon_a*rl_coslon_b + &
                                            rl_sinlon_a*rl_sinlon_b) )
 
                            IF((rl_sum_inv_dist .EQ. 0) .OR. &
                               (rl_dist .EQ. 0)) THEN
                               WRITE(0,*) 'division par zéro'
                            ENDIF
 
                            rl_invdist = (1 / rl_dist) / rl_sum_inv_dist
                            rla_dataout(il_ji,il_jj,il_jk) = rla_dataout(il_ji,il_jj,il_jk) + &
                                    REAL((rl_invdist *  &
                                     rla_datain(il_neigh(il_jn,1), il_neigh(il_jn,2), il_jk)),4)
                         ENDIF
 
                      ENDDO
 
                   ENDIF
 
                ENDDO
 
             ENDDO
 
          ENDDO
 
          ila_maskin = ila_maskout
          rla_datain = rla_dataout
 
          ENDDO
 
          IF (ALLOCATED(ila_maskin)) DEALLOCATE(ila_maskin, stat = il_status)
             il_status = fi_memError(il_status, ' ila_maskin',cl_fonction)
 
          IF (ALLOCATED(rla_datain)) DEALLOCATE(rla_datain, stat = il_status)
             il_status = fi_memError(il_status, ' rla_datain',cl_fonction)
 
          !----------------------------------------------------------------------
          ! Output arrays filling
 
          DO il_jk=1, id_nz
             DO il_jj=1, id_ny
                DO il_ji=1, id_nx
                   IF (ila_maskout(il_ji, il_jj, il_jk) .EQ. 1) THEN
                      rda_value(il_ji, il_jj, il_jk) = rla_dataout(il_ji, il_jj, il_jk)
                   ENDIF
                ENDDO
             ENDDO
          ENDDO
          !** Extrap mask       
           ALLOCATE(idpa_mask_3D_ex(id_nx, &
                                id_ny, &
                                id_nz), &
                    stat=il_status)
           il_status = fi_memError(il_status, 'idpa_mask_3D_ex',cl_fonction)
           idpa_mask_3D_ex(:,:,:)=ila_maskout(:,:,:)
 
          IF (ALLOCATED(rla_dataout)) DEALLOCATE (rla_dataout, stat = il_status)
             il_status = fi_memError(il_status, ' rla_dataout',cl_fonction)
 
          IF (ALLOCATED(ila_maskout)) DEALLOCATE (ila_maskout, stat = il_status)
             il_status = fi_memError(il_status, ' ila_maskout',cl_fonction)
 
 
           ENDSUBROUTINE MCAL_extrap3D_distweight
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
 
          !!=====================================================================
          !> \brief
          !! Description: This function computes the cosinus and the sinus of
          !!              the angle existing between the 'U' or 'V' vectors with
          !!              the North direction.
          !!
          !!  @param rda_glam     2D array of longitude values from the 'U' or 'V' type grid.
          !!  @param rda_gphi     2D array of latitude values from the 'U' or 'V' type grid.
          !!  @param rda_glamf    2D array of longitude values from the 'F' type grid.
          !!  @param rda_gphif    2D array of latitude values from the 'F' type grid.
          !!  @param id_jpi       The length of the x dimension.
          !!  @param id_jpj       The length of the x dimension.
          !!  @param cd_kgrid     The type of grid: 'U' or 'V'
          !!  @param rda_gcos     2D array of cosinus values.
          !!  @param rda_gsin     2D array of sinus values.
          !!
          !!\n History :
          !!        \n  06/2006  (F. Messal)
          !!        \n  10/2008 (C. REGNIER) Add MFT_error MIOL 3.1
          !!        \n  10/2010 (M. CHEKKI)  Correction après pb sur yuki (division par 0)
          !<
          !!=====================================================================
 
        SUBROUTINE MCAL_compute_gridangle_R8 (rda_glam, &
                                              rda_gphi, &
                                              rda_glamf, &
                                              rda_gphif, &
                                              id_jpi, &
                                              id_jpj, &
                                              cd_kgrid, &
                                              rda_gcos, &
                                              rda_gsin)
 
          USE netcdf
          USE MFT_error
          IMPLICIT NONE
 
          !----------------------------------------------------------------------
 
          CHARACTER(1) :: cd_kgrid               ! 'u' or 'v'
          CHARACTER(LEN=255) :: cl_fonction
          INTEGER, INTENT(in) :: id_jpi,id_jpj
          INTEGER :: il_ji,il_jj
          REAL(KIND=8), DIMENSION(id_jpi,id_jpj), INTENT(in) :: &
                rda_glam, rda_gphi, rda_glamf, rda_gphif
 
          REAL(KIND=8), DIMENSION(id_jpi,id_jpj), INTENT(OUT) :: &
                rda_gsin, rda_gcos
 
          REAL(KIND=8), DIMENSION(id_jpi,id_jpj) :: &
                rda_R8_glam, rda_R8_gphi, rda_R8_glamf, rda_R8_gphif
 
          REAL(KIND=8) :: rl_zxnp, rl_zynp, rl_znnp, &
                          rl_zxff, rl_zyff, rl_zmnpf
          INTEGER :: il_status
          
           !----------------------------------------------------------------------
          cl_fonction='MCAL_compute_gridangle_R8'
          
          rda_R8_glam(:,:) = REAL(rda_glam(:,:),8)
          rda_R8_gphi(:,:) = REAL(rda_gphi(:,:),8)
          rda_R8_glamf(:,:) = REAL(rda_glamf(:,:),8)
          rda_R8_gphif(:,:) = REAL(rda_gphif(:,:),8)
 
          !----------------------------------------------------------------------
          ! Test
          il_status = fi_arrError_R8(MINVAL(rda_glam), &
                                  MAXVAL(rda_glam), &
                                  ' rda_glam',cl_fonction)
 
          il_status = fi_arrError_R8(MINVAL(rda_gphi), &
                                  MAXVAL(rda_gphi), &
                                  ' rda_gphi',cl_fonction)
 
          il_status = fi_arrError_R8(MINVAL(rda_glamf), &
                                  MAXVAL(rda_glamf), &
                                  ' rda_glamf',cl_fonction)
 
          il_status = fi_arrError_R8(MINVAL(rda_gphif), &
                                  MAXVAL(rda_gphif), &
                                  ' rda_gphif',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Compute the cosinus and sinus
          ! (computation done on the north stereographic polar plan)
          SELECT CASE(cd_kgrid)
 
            CASE('u','U')
 
             DO il_jj = 2, id_jpj
                DO il_ji = 1, id_jpi
 
                  ! north pole direction & modulous (at u-point)
                  rl_zxnp = 0. - rl_fsxspp( rda_R8_glam(il_ji,il_jj), &
                                            rda_R8_gphi(il_ji,il_jj) )
                  rl_zynp = 0. - rl_fsyspp( rda_R8_glam(il_ji,il_jj), &
                                            rda_R8_gphi(il_ji,il_jj) )
                  rl_znnp = rl_zxnp*rl_zxnp + rl_zynp*rl_zynp
 
                  ! j-direction: f-point segment direction (u-point)
                  if (il_jj .eq. 1) then
                     rl_zxff=  rl_fsxspp( rda_R8_glamf(il_ji, il_jj), &
                                          rda_R8_gphif(il_ji,il_jj) ) &
                        - rl_fsxspp( rda_R8_glamf(il_ji,id_jpj), &
                                     rda_R8_gphif(il_ji,id_jpj) )
                     rl_zyff=  rl_fsyspp( rda_R8_glamf(il_ji,il_jj  ), &
                                          rda_R8_gphif(il_ji,il_jj  ) ) &
                        - rl_fsyspp( rda_R8_glamf(il_ji,id_jpj), &
                                     rda_R8_gphif(il_ji,id_jpj) )
                  else
                     rl_zxff=  rl_fsxspp( rda_R8_glamf(il_ji,il_jj), &
                                          rda_R8_gphif(il_ji,il_jj) ) &
                        - rl_fsxspp( rda_R8_glamf(il_ji,il_jj-1), &
                                     rda_R8_gphif(il_ji,il_jj-1) )
                     rl_zyff=  rl_fsyspp( rda_R8_glamf(il_ji,il_jj), &
                                          rda_R8_gphif(il_ji,il_jj) ) &
                        - rl_fsyspp( rda_R8_glamf(il_ji,il_jj-1), &
                                     rda_R8_gphif(il_ji,il_jj-1) )
                  endif
                  rl_zmnpf= sqrt ( rl_znnp * ( rl_zxff*rl_zxff + rl_zyff*rl_zyff )  )

                  ! cosinus and sinus using scalar and vectorial products
                  if ( rl_zmnpf .eq. 0 ) then
                     rda_gsin(il_ji,il_jj) = 0.
                     rda_gcos(il_ji,il_jj) = 0.
                  else 
                     rda_gsin(il_ji,il_jj) = ( rl_zxnp*rl_zyff - rl_zynp*rl_zxff ) / rl_zmnpf
                     rda_gcos(il_ji,il_jj) = ( rl_zxnp*rl_zxff + rl_zynp*rl_zyff ) / rl_zmnpf
                  end if 
 
                ENDDO
             ENDDO
 
             rda_gsin(1:id_jpi,1) = rda_gsin(1:id_jpi,id_jpj)
             rda_gcos(1:id_jpi,1) = rda_gcos(1:id_jpi,id_jpj)
 
            CASE('v','V')
 
             DO il_jj = 1, id_jpj
                DO il_ji = 2, id_jpi
 
                  ! north pole direction & modulous (at v-point)
                  rl_zxnp = 0. - rl_fsxspp( rda_R8_glam(il_ji,il_jj), &
                                            rda_R8_gphi(il_ji,il_jj) )
                  rl_zynp = 0. - rl_fsyspp( rda_R8_glam(il_ji,il_jj), &
                                            rda_R8_gphi(il_ji,il_jj) )
                  rl_znnp = rl_zxnp*rl_zxnp + rl_zynp*rl_zynp
 
                  ! i-direction: f-point segment direction (v-point)
                  if (il_ji .eq. 1) then
                     rl_zxff=  rl_fsxspp( rda_R8_glamf(il_ji  ,il_jj), &
                                          rda_R8_gphif(il_ji  ,il_jj) ) &
                        - rl_fsxspp( rda_R8_glamf(id_jpi,il_jj), &
                                     rda_R8_gphif(id_jpi,il_jj) )
                     rl_zyff=  rl_fsyspp( rda_R8_glamf(il_ji  ,il_jj), &
                                          rda_R8_gphif(il_ji  ,il_jj) ) &
                        - rl_fsyspp( rda_R8_glamf(id_jpi,il_jj), &
                                     rda_R8_gphif(id_jpi,il_jj) )
                  else
                     rl_zxff=  rl_fsxspp( rda_R8_glamf(il_ji  ,il_jj), &
                                          rda_R8_gphif(il_ji  ,il_jj) ) &
                        - rl_fsxspp( rda_R8_glamf(il_ji-1,il_jj), &
                                     rda_R8_gphif(il_ji-1,il_jj) )
                     rl_zyff=  rl_fsyspp( rda_R8_glamf(il_ji  ,il_jj), &
                                          rda_R8_gphif(il_ji  ,il_jj) ) &
                        - rl_fsyspp( rda_R8_glamf(il_ji-1,il_jj), &
                                     rda_R8_gphif(il_ji-1,il_jj) )
                  endif
                  rl_zmnpf= sqrt ( rl_znnp * ( rl_zxff*rl_zxff + rl_zyff*rl_zyff )  )
 
                  ! cosinus and sinus using scalar and vectorial products
                  ! (caution, rotation of 90 degres)
                  if ( rl_zmnpf .eq. 0 ) then
                     rda_gsin(il_ji,il_jj) = 0.
                     rda_gcos(il_ji,il_jj) = 0.
                  else     
                     rda_gcos(il_ji,il_jj) = -( rl_zxnp*rl_zxff + rl_zynp*rl_zyff ) / rl_zmnpf
                     rda_gsin(il_ji,il_jj) = -( rl_zxnp*rl_zyff - rl_zynp*rl_zxff ) / rl_zmnpf
                  end if 
 
                END DO
             ENDDO
 
             rda_gsin(1,1:id_jpj) = rda_gsin(id_jpi,1:id_jpj)
             rda_gcos(1,1:id_jpj) = rda_gcos(id_jpi,1:id_jpj)
 
            CASE DEFAULT
                WRITE(0,*) ' MCAL_compute_gridangle_R8 : grid key error.'
                CALL flush(0)
 
          END SELECT
 
 
          CONTAINS
 
          FUNCTION rl_fsxspp( rl_plam, rl_pphi)
 
             REAL (KIND=8) :: rl_fsxspp
             REAL (KIND=8) :: rl_plam, rl_pphi, &
                              rl_rad, rl_rpi
 
             rl_rpi=2.*asin( 1. )
             rl_rad=rl_rpi/180.

             rl_fsxspp = 2. * cos( rl_rad*rl_plam ) * tan( rl_rpi/4. - rl_rad*rl_pphi/2. )
 
          END FUNCTION rl_fsxspp
 
 
          FUNCTION rl_fsyspp( rl_plam, rl_pphi)
 
             REAL (KIND=8) :: rl_fsyspp
             REAL (KIND=8) :: rl_plam, rl_pphi, &
                              rl_rad, rl_rpi
 
             rl_rpi=2.d0*asin( 1.d0 )
             rl_rad=rl_rpi/180.d0
 
             rl_fsyspp = 2.d0 * sin( rl_rad*rl_plam ) * tan( rl_rpi/4.d0 - rl_rad*rl_pphi/2.d0 )
 
          END FUNCTION rl_fsyspp
 
        ENDSUBROUTINE MCAL_compute_gridangle_R8
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
          !!=====================================================================
          !> \brief
          !! Description: This function computes the cosinus and the sinus of
          !!              the angle existing between the 'U' or 'V' vectors with
          !!              the North direction.
          !!
          !!  @param rda_glam     2D array of longitude values from the 'U' or 'V' type grid.
          !!  @param rda_gphi     2D array of latitude values from the 'U' or 'V' type grid.
          !!  @param rda_glamf    2D array of longitude values from the 'F' type grid.
          !!  @param rda_gphif    2D array of latitude values from the 'F' type grid.
          !!  @param id_jpi       The length of the x dimension.
          !!  @param id_jpj       The length of the x dimension.
          !!  @param cd_kgrid     The type of grid: 'U' or 'V'
          !!  @param rda_gcos     2D array of cosinus values.
          !!  @param rda_gsin     2D array of sinus values.
          !!
          !! History :
          !!        \n  06/2006  (F. Messal)
          !!        \n  10/2008 (C. REGNIER) Add MFT_error MIOL 3.1
          !!        \n  10/2010 (M. CHEKKI)  Correction après pb sur yuki (division par 0)
          !<
          !!=====================================================================
 
        SUBROUTINE MCAL_compute_gridangle_R4 (rda_glam, &
                                              rda_gphi, &
                                              rda_glamf, &
                                              rda_gphif, &
                                              id_jpi, &
                                              id_jpj, &
                                              cd_kgrid, &
                                              rda_gcos, &
                                              rda_gsin)
 
          USE netcdf
          USE MFT_error
          IMPLICIT NONE
 
          !----------------------------------------------------------------------
 
          CHARACTER(1) :: cd_kgrid               ! 'u' or 'v'
          CHARACTER(len=255) :: cl_fonction
          INTEGER, INTENT(in) :: id_jpi,id_jpj
          INTEGER :: il_ji,il_jj
          REAL(KIND=4), DIMENSION(id_jpi,id_jpj), INTENT(in) :: &
                rda_glam, rda_gphi, rda_glamf, rda_gphif
 
          REAL(KIND=4), DIMENSION(id_jpi,id_jpj), INTENT(OUT) :: &
                rda_gsin, rda_gcos
 
          REAL(KIND=8), DIMENSION(id_jpi,id_jpj) :: &
                rda_R8_glam, rda_R8_gphi, rda_R8_glamf, rda_R8_gphif
 
          REAL(KIND=8) :: rl_zxnp, rl_zynp, rl_znnp, &
                          rl_zxff, rl_zyff, rl_zmnpf
          INTEGER :: il_status
 
          !--------------------------------------------------------------------------
          cl_fonction='MCAL_compute_gridangle_R4'
          
          rda_R8_glam(:,:) = REAL(rda_glam(:,:),8)
          rda_R8_gphi(:,:) = REAL(rda_gphi(:,:),8)
          rda_R8_glamf(:,:) = REAL(rda_glamf(:,:),8)
          rda_R8_gphif(:,:) = REAL(rda_gphif(:,:),8)
 
          !----------------------------------------------------------------------
          ! Test
          il_status = fi_arrError(MINVAL(rda_glam), &
                                  MAXVAL(rda_glam), &
                                  ' rda_glam',cl_fonction)
 
          il_status = fi_arrError(MINVAL(rda_gphi), &
                                  MAXVAL(rda_gphi), &
                                  ' rda_gphi',cl_fonction)
 
          il_status = fi_arrError(MINVAL(rda_glamf), &
                                  MAXVAL(rda_glamf), &
                                  ' rda_glamf',cl_fonction)
 
          il_status = fi_arrError(MINVAL(rda_gphif), &
                                  MAXVAL(rda_gphif), &
                                  ' rda_gphif',cl_fonction)
 
          !----------------------------------------------------------------------
          ! Compute the cosinus and sinus
          ! (computation done on the north stereographic polar plan)
          SELECT CASE(cd_kgrid)
 
            CASE('u','U')
 
             DO il_jj = 1, id_jpj
                DO il_ji = 1, id_jpi
 
                  ! north pole direction & modulous (at u-point)
                  rl_zxnp = 0. - rl_fsxspp( rda_R8_glam(il_ji,il_jj), &
                                            rda_R8_gphi(il_ji,il_jj) )
                  rl_zynp = 0. - rl_fsyspp( rda_R8_glam(il_ji,il_jj), &
                                            rda_R8_gphi(il_ji,il_jj) )
                  rl_znnp = rl_zxnp*rl_zxnp + rl_zynp*rl_zynp
 
                  ! j-direction: f-point segment direction (u-point)
                  if (il_jj .eq. 1) then
                     rl_zxff=  rl_fsxspp( rda_R8_glamf(il_ji, il_jj), &
                                          rda_R8_gphif(il_ji,il_jj) ) &
                        - rl_fsxspp( rda_R8_glamf(il_ji,id_jpj), &
                                     rda_R8_gphif(il_ji,id_jpj) )
                     rl_zyff=  rl_fsyspp( rda_R8_glamf(il_ji,il_jj  ), &
                                          rda_R8_gphif(il_ji,il_jj  ) ) &
                        - rl_fsyspp( rda_R8_glamf(il_ji,id_jpj), &
                                     rda_R8_gphif(il_ji,id_jpj) )
                  else
                     rl_zxff=  rl_fsxspp( rda_R8_glamf(il_ji,il_jj), &
                                          rda_R8_gphif(il_ji,il_jj) ) &
                        - rl_fsxspp( rda_R8_glamf(il_ji,il_jj-1), &
                                     rda_R8_gphif(il_ji,il_jj-1) )
                     rl_zyff=  rl_fsyspp( rda_R8_glamf(il_ji,il_jj), &
                                          rda_R8_gphif(il_ji,il_jj) ) &
                        - rl_fsyspp( rda_R8_glamf(il_ji,il_jj-1), &
                                     rda_R8_gphif(il_ji,il_jj-1) )
                  endif
                  rl_zmnpf= sqrt ( rl_znnp * ( rl_zxff*rl_zxff + rl_zyff*rl_zyff )  )

                  ! cosinus and sinus using scalar and vectorial products

                  ! MODIFS M. Chekki
                  if ( rl_zmnpf .eq. 0 ) then
                     rda_gsin(il_ji,il_jj) = 0.
                     rda_gcos(il_ji,il_jj) = 0.
                  else
                    rda_gsin(il_ji,il_jj) = REAL(( rl_zxnp*rl_zyff - rl_zynp*rl_zxff ) / rl_zmnpf, 4)
                    rda_gcos(il_ji,il_jj) = REAL(( rl_zxnp*rl_zxff + rl_zynp*rl_zyff ) / rl_zmnpf, 4)
                  endif 
 
                ENDDO
             ENDDO
 
 
            CASE('v','V')
 
             DO il_jj = 1, id_jpj
                DO il_ji = 1, id_jpi
 
                  ! north pole direction & modulous (at v-point)
                  rl_zxnp = 0. - rl_fsxspp( rda_R8_glam(il_ji,il_jj), &
                                            rda_R8_gphi(il_ji,il_jj) )
                  rl_zynp = 0. - rl_fsyspp( rda_R8_glam(il_ji,il_jj), &
                                            rda_R8_gphi(il_ji,il_jj) )
                  rl_znnp = rl_zxnp*rl_zxnp + rl_zynp*rl_zynp
 
                  ! i-direction: f-point segment direction (v-point)
                  if (il_ji .eq. 1) then
                     rl_zxff=  rl_fsxspp( rda_R8_glamf(il_ji  ,il_jj), &
                                          rda_R8_gphif(il_ji  ,il_jj) ) &
                        - rl_fsxspp( rda_R8_glamf(id_jpi,il_jj), &
                                     rda_R8_gphif(id_jpi,il_jj) )
                     rl_zyff=  rl_fsyspp( rda_R8_glamf(il_ji  ,il_jj), &
                                          rda_R8_gphif(il_ji  ,il_jj) ) &
                        - rl_fsyspp( rda_R8_glamf(id_jpi,il_jj), &
                                     rda_R8_gphif(id_jpi,il_jj) )
                  else
                     rl_zxff=  rl_fsxspp( rda_R8_glamf(il_ji  ,il_jj), &
                                          rda_R8_gphif(il_ji  ,il_jj) ) &
                        - rl_fsxspp( rda_R8_glamf(il_ji-1,il_jj), &
                                     rda_R8_gphif(il_ji-1,il_jj) )
                     rl_zyff=  rl_fsyspp( rda_R8_glamf(il_ji  ,il_jj), &
                                          rda_R8_gphif(il_ji  ,il_jj) ) &
                        - rl_fsyspp( rda_R8_glamf(il_ji-1,il_jj), &
                                     rda_R8_gphif(il_ji-1,il_jj) )
                  endif
                  rl_zmnpf= sqrt ( rl_znnp * ( rl_zxff*rl_zxff + rl_zyff*rl_zyff )  )
 
                  ! cosinus and sinus using scalar and vectorial products
                  ! (caution, rotation of 90 degres)

                  ! MODIFS M. Chekki
                  if ( rl_zmnpf .eq. 0 ) then  
                     rda_gsin(il_ji,il_jj) = 0.
                     rda_gcos(il_ji,il_jj) = 0.
                  else
                    rda_gcos(il_ji,il_jj) = REAL((-( rl_zxnp*rl_zxff + rl_zynp*rl_zyff ) / rl_zmnpf), 4)
                    rda_gsin(il_ji,il_jj) = REAL((-( rl_zxnp*rl_zyff - rl_zynp*rl_zxff ) / rl_zmnpf), 4)
                  end if 
 
                END DO
             ENDDO
 
            CASE DEFAULT
                WRITE(0,*) ' MCAL_compute_gridangle_R4 : grid key error.'
                CALL flush(0)
 
          END SELECT
 
 
          CONTAINS
 
          FUNCTION rl_fsxspp( rl_plam, rl_pphi)
 
             REAL (KIND=8) :: rl_fsxspp
             REAL (KIND=8) :: rl_plam, rl_pphi, &
                              rl_rad, rl_rpi
 
             rl_rpi=2.*asin( 1. )
             rl_rad=rl_rpi/180.
 
             rl_fsxspp = 2. * cos( rl_rad*rl_plam ) * tan( rl_rpi/4. - rl_rad*rl_pphi/2. )
 
          END FUNCTION rl_fsxspp
 
 
          FUNCTION rl_fsyspp( rl_plam, rl_pphi)
 
             REAL (KIND=8) :: rl_fsyspp
             REAL (KIND=8) :: rl_plam, rl_pphi, &
                              rl_rad, rl_rpi
 
             rl_rpi=2.*asin( 1. )
             rl_rad=rl_rpi/180.
 
             rl_fsyspp = 2. * sin( rl_rad*rl_plam ) * tan( rl_rpi/4. - rl_rad*rl_pphi/2. )
 
          END FUNCTION rl_fsyspp
 
        ENDSUBROUTINE MCAL_compute_gridangle_R4
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
          !!=====================================================================
          !> \brief
          !! Description: this routine compute the rd_distance between two points
          !!     on a sphere of radius = 1.
          !!
          !!  @param rd_lon_a        The longitude value of the point A.
          !!  @param rd_lat_a        The latitude value of the point A.
          !!  @param rd_lon_b        The longitude value of the point B.
          !!  @param rd_lat_b        The latitude value of the point B.
          !!  @param distance        The distance between A and B of a sphere of radius = 1.
          !!
          !! History :
          !!           Original    : 05/2006 (S. Besnard)
          !!                     \n  10/2008 (C. REGNIER) Add MFT_error MIOL 3.1
          !<
          !!=====================================================================

        SUBROUTINE MCAL_compute_distance (rd_lon_a, &
                                          rd_lat_a, &
                                          rd_lon_b, &
                                          rd_lat_b, &
                                          rd_distance)
 
         
          !----------------------------------------------------------------------
 
          IMPLICIT NONE
 
          REAL(KIND=4), INTENT(IN) :: rd_lon_a, rd_lat_a, rd_lon_b, rd_lat_b
          REAL(KIND=8), INTENT(OUT) :: rd_distance ! rd_distance between a et b on the globe
 
          REAL(KIND=8), PARAMETER :: pi = 3.14159265359
          REAL(KIND=8) :: coslat_a, &    ! cos(lat) of source grid point
                          coslon_a, &    ! cos(lon) of source grid point
                          sinlat_a, &    ! sin(lat) of source grid point
                          sinlon_a, &    ! sin(lon) of source grid point
                          coslat_b, &    ! cos(lat) of destination grid point
                          coslon_b, &    ! cos(lon) of destination grid point
                          sinlat_b, &    ! sin(lat) of destination grid point
                          sinlon_b       ! sin(lon) of destination grid point
          REAL(KIND=8), PARAMETER :: rd_deg2rad = pi/180.   ! conversion for deg to rads
 
          !----------------------------------------------------------------------
 
          coslat_a = COS(rd_lat_a*rd_deg2rad)
          coslon_a = COS(rd_lon_a*rd_deg2rad)
          sinlat_a = SIN(rd_lat_a*rd_deg2rad)
          sinlon_a = SIN(rd_lon_a*rd_deg2rad)
 
          coslat_b = COS(rd_lat_b*rd_deg2rad)
          coslon_b = COS(rd_lon_b*rd_deg2rad)
          sinlat_b = SIN(rd_lat_b*rd_deg2rad)
          sinlon_b = SIN(rd_lon_b*rd_deg2rad)
 
          rd_distance = ACOS( sinlat_a*sinlat_b + &
                              coslat_a*coslat_b*  &
                            ( coslon_a*coslon_b + &
                              sinlon_a*sinlon_b) )
 
 
        ENDSUBROUTINE MCAL_compute_distance
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
        !!=======================================================================
        !> \brief
        !! Description:This function transforms a depth level value 2D mask in a
        !!             [0,1] value 3D mask.
        !!
        !!   @param ida_mask_2D       The original 2D mask.
        !!   @param ida_mask_3D       The output 3D mask.
        !!   @param id_nx             The length of the x dimension.
        !!   @param id_ny             The length of the x dimension.
        !!   @param id_nz             The length of the x dimension.
        !!
        !! History :
        !!        \n  06/2006  (F. Messal)
        !!        \n  10/2008  (C. REGNIER) Add MFT_error MIOL 3.1
        !<
        !!=======================================================================
 
 
        SUBROUTINE MCAL_transform_mask2Dto3D(ida_mask_2D, &
                                             ida_mask_3D, &
                                             id_nx, &
                                             id_ny, &
                                             id_nz, &
                                             rda_depth)
 
        IMPLICIT NONE
 
       !------------------------------------------------------------------------
 
        INTEGER, INTENT(IN) :: id_nx, id_ny, id_nz
        INTEGER, INTENT(IN), DIMENSION(id_nx, id_ny) :: ida_mask_2D
        INTEGER, INTENT(OUT), DIMENSION(id_nx, id_ny, id_nz) :: ida_mask_3D
        REAL(KIND=4), DIMENSION(id_nz), OPTIONAL :: rda_depth
        INTEGER :: il_ji, il_jj, il_jk
 
        !------------------------------------------------------------------------
        ! Initialization
 
        ida_mask_3D(1:il_ji,1:il_jj,1:il_jk)=0
 
 
        !------------------------------------------------------------------------
        ! Compute
 
        DO il_ji=1, id_nx
          DO il_jj=1, id_ny
            DO il_jk=1, id_nz
               IF (PRESENT(rda_depth)) THEN
                  IF (ida_mask_2D(il_ji, il_jj) <  REAL(rda_depth(il_jk)*(-1))) THEN
                     ida_mask_3D(il_ji, il_jj, il_jk) = 1
                  ENDIF
               ELSE
                  IF (ida_mask_2D(il_ji, il_jj) < (il_jk)) THEN
                     ida_mask_3D(il_ji, il_jj, il_jk) = 1
                  ENDIF
               ENDIF
            ENDDO
          ENDDO
        ENDDO
 
        RETURN
 
 
        ENDSUBROUTINE MCAL_transform_mask2Dto3D
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
          !!=====================================================================
          !> \brief
          !! Description: this routine computes an erosion by researching of
          !!     among land points those which have, at least, a sea point as
          !!     neighbour and put the value at 1 and begin again, as many
          !!     time as id_num_iter value.
          !!
          !!  @param ila_mask_3D       3D array mask values to be eroded.
          !!  @param id_nx             The x dimension of the 3D mask.
          !!  @param id_ny             The y dimension of the 3D mask.
          !!  @param id_nz             The z dimension of the 3D mask.
          !!  @param id_num_iter       The number of iterations.
          !!
          !! History :
          !!       \n    Original    : 10/2006 (S. Besnard - F. Messal)
          !!       \n  10/2008  (C. REGNIER) Add MFT_error MIOL 3.1
          !<
          !!=====================================================================
 
        SUBROUTINE MCAL_morpho3D_erosionmask (ida_mask_3D, &
                                              id_nx, &
                                              id_ny, &
                                              id_nz, &
                                              id_num_iter)
          USE MFT_error
          IMPLICIT NONE
 
          
 
          INTEGER,                             INTENT(IN) :: id_nx,id_ny,id_nz
          INTEGER, DIMENSION(id_nx,id_ny,id_nz) :: ida_mask_3D
          INTEGER, INTENT(IN) :: id_num_iter
 
          INTEGER :: il_ji, il_jj, il_jk, il_jim1, il_jjm1, il_jip1, il_jjp1, il_iter, il_status
          INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: ila_maskin, ila_maskout
          INTEGER, DIMENSION(8,2) :: il_neigh  ! addresses on source grid
          CHARACTER(LEN=255)      :: cl_fonction
 
          cl_fonction="MCAL_morpho3D_erosionmask"
          !----------------------------------------------------------------------
          ! Test array
          il_status = fi_arrError(REAL(MINVAL(ida_mask_3D),4), &
                                  REAL(MAXVAL(ida_mask_3D),4), &
                                  'ida_mask_3D',cl_fonction)
 
          !----------------------------------------------------------------------
          ! Data and mask temporary arrays initialization
 
          ALLOCATE(ila_maskout(id_nx, &
                               id_ny, &
                               id_nz), &
                   stat=il_status)
          il_status = fi_memError(il_status,'ila_maskout',cl_fonction)
 
          ALLOCATE(ila_maskin(id_nx, &
                              id_ny, &
                              id_nz), &
                   stat=il_status)
          il_status = fi_memError(il_status,'ila_maskin',cl_fonction)
 
          ila_maskin(1:id_nx,1:id_ny,1:id_nz) = ida_mask_3D(1:id_nx,1:id_ny,1:id_nz)
          ila_maskout(1:id_nx,1:id_ny,1:id_nz) = ila_maskin(1:id_nx,1:id_ny,1:id_nz)
 
          !----------------------------------------------------------------------
          ! Iteration on extrapolation method
 
          DO il_iter = 1, id_num_iter
          !WRITE(0,*) 'Iteration n°', il_iter, 'for extrapolation method'
          CALL FLUSH(0)
 
          DO il_jk=1, id_nz
             CALL FLUSH(0)
 
             DO il_jj=2,id_ny-1
 
                DO il_ji=2,id_nx-1
 
                   il_jip1 = il_ji+1
                   il_jim1 = il_ji-1
                   il_jjp1 = il_jj+1
                   il_jjm1 = il_jj-1
                   il_neigh(1,1) = il_jim1
                   il_neigh(1,2) = il_jjm1
                   il_neigh(2,1) = il_ji
                   il_neigh(2,2) = il_jjm1
                   il_neigh(3,1) = il_jip1
                   il_neigh(3,2) = il_jjm1
                   il_neigh(4,1) = il_jim1
                   il_neigh(4,2) = il_jj
                   il_neigh(5,1) = il_jip1
                   il_neigh(5,2) = il_jj
                   il_neigh(6,1) = il_jim1
                   il_neigh(6,2) = il_jjp1
                   il_neigh(7,1) = il_ji
                   il_neigh(7,2) = il_jjp1
                   il_neigh(8,1) = il_jip1
                   il_neigh(8,2) = il_jjp1
 
                   IF ( ( ila_maskin(il_ji,il_jj,il_jk) == 0        ) .AND.&
                     ( (ila_maskin(il_neigh(1,1),il_neigh(1,2),il_jk)==1)   .OR.&
                       (ila_maskin(il_neigh(2,1),il_neigh(2,2),il_jk)==1)   .OR.&
                       (ila_maskin(il_neigh(3,1),il_neigh(3,2),il_jk)==1)   .OR.&
                       (ila_maskin(il_neigh(4,1),il_neigh(4,2),il_jk)==1)   .OR.&
                       (ila_maskin(il_neigh(5,1),il_neigh(5,2),il_jk)==1)   .OR.&
                       (ila_maskin(il_neigh(6,1),il_neigh(6,2),il_jk)==1)   .OR.&
                       (ila_maskin(il_neigh(7,1),il_neigh(7,2),il_jk)==1)   .OR.&
                       (ila_maskin(il_neigh(8,1),il_neigh(8,2),il_jk)==1) )  )  &
                   THEN
 
                      ila_maskout(il_ji,il_jj,il_jk) = 1
 
                   ENDIF
 
                ENDDO
 
             ENDDO
 
          ENDDO
 
          ila_maskin = ila_maskout
 
          ENDDO
 
          DEALLOCATE(ila_maskin)
 
          !----------------------------------------------------------------------
          ! Output arrays filling
 
          ida_mask_3D = ila_maskout
 
          DEALLOCATE(ila_maskout)
 
        
        ENDSUBROUTINE MCAL_morpho3D_erosionmask
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
          !!=====================================================================
          !> \brief
          !! Description: This function interpolates 1D data values on an
          !!              interpolated abscissa. The interpolation method is
          !!              a cubic spline.
          !!
          !!  @param rda_data            Vector of the values to be interpolated.
          !!  @param rda_abscissa        Vector of original abcsissa.
          !!  @param id_nbdata           Length of the data values array.
          !!  @param rda_interdata       Vector of values interpolated
          !!  @param rda_interabscissa   Vector of new abscissa.
          !!  @param id_nbinterdata      Length of the interpolated data values array.
          !!  @param rd_emptyvalue       Specify the empty value of the original data array.
          !!
          !! History :
          !!      \n     1985 (E. Dombrowsky) Original
          !!      \n     1993 (P. de Mey) Improved
          !!      \n     09/2006 (F. Messal) F90
          !!      \n     11/2006 (F. Messal) CVS version 1.0
          !<
          !!=====================================================================
 
 
        SUBROUTINE MCAL_interp1D_cubicspline_R8 (rda_data, &
                                                 rda_abscissa, &
                                                 id_nbdata, &
                                                 rda_interdata, &
                                                 rda_interabscissa, &
                                                 id_nbinterdata, &
                                                 rd_emptyvalue)
 
 
        
          IMPLICIT NONE
 
          INTEGER,                                 INTENT(IN) :: id_nbdata, &
                                                                 id_nbinterdata
          REAL(KIND=8), DIMENSION(id_nbdata),      INTENT(IN) :: rda_data, &
                                                                 rda_abscissa
          REAL(KIND=8), DIMENSION(id_nbinterdata), INTENT(IN) :: rda_interabscissa
          REAL(KIND=8), DIMENSION(id_nbinterdata), INTENT(OUT) :: rda_interdata
          REAL(KIND=8),                            INTENT(IN) :: rd_emptyvalue
 
          REAL(KIND=8), DIMENSION(id_nbdata) :: rla_normdata, rla_normabscissa, &
                                                rla_derivate
          REAL(KIND=8), DIMENSION(id_nbdata-1) :: rla_Acoeff, rla_Bcoeff, rla_Ccoeff, &
                                                  rla_Dcoeff
          REAL(KIND=8), DIMENSION(id_nbinterdata) :: rla_norminterabscissa
          REAL(KIND=8) :: rl_dataminvalue, rl_datamaxvalue, rl_abscissaminvalue, &
                          rl_abscissamaxvalue, rl_datalar, rl_abscissalar, &
                          rl_dataver, rl_abscissaver, &
                          rl_result
          REAL(KIND=8) :: rl_a1, rl_b1, rl_c1, rl_a2, rl_b2, &
                          rl_c2, rl_deter, rl_test
          INTEGER :: il_ji, il_nok, il_jj
 
 
          !----------------------------------------------------------------------
          ! Calculate range, check dimensions
 
          rl_dataminvalue = 1.E+35
          rl_abscissaminvalue = 1.E+35
          rl_datamaxvalue = 1.E-35
          rl_abscissamaxvalue = 1.E-35
          il_nok = 0
 
          DO il_ji=1, id_nbdata
             IF (     (rda_data(il_ji).NE.rd_emptyvalue) &
                 .AND. &
                      (rda_abscissa(il_ji).NE.rd_emptyvalue) &
                ) THEN
                IF (rda_data(il_ji).LT.rl_dataminvalue) THEN
                   rl_dataminvalue = rda_data(il_ji)
                ENDIF
                IF (rda_data(il_ji).GT.rl_datamaxvalue) THEN
                   rl_datamaxvalue = rda_data(il_ji)
                ENDIF
                IF (rda_abscissa(il_ji).LT.rl_abscissaminvalue) THEN
                   rl_abscissaminvalue = rda_abscissa(il_ji)
                ENDIF
                IF (rda_abscissa(il_ji).GT.rl_abscissamaxvalue) THEN
                   rl_abscissamaxvalue = rda_abscissa(il_ji)
                ENDIF
             ELSE
             ENDIF
          ENDDO
 
 
          rl_abscissalar = (rl_abscissamaxvalue - rl_abscissaminvalue)
          IF (rl_abscissalar.EQ.0.0) THEN
             rl_abscissalar = 1.0
          ENDIF
 
          rl_datalar = (rl_datamaxvalue - rl_dataminvalue)
          IF (rl_datalar.EQ.0.0) THEN
             rl_datalar = 1.0
          ENDIF
 
          rl_abscissaver = (rl_abscissamaxvalue + rl_abscissaminvalue)/2.0
          rl_dataver = (rl_datamaxvalue + rl_dataminvalue)/2.0
 
 
          !----------------------------------------------------------------------
          ! Normalize
 
          DO il_ji=1, id_nbdata
             IF ((rda_data(il_ji).NE.rd_emptyvalue).AND. &
                 (rda_abscissa(il_ji).NE.rd_emptyvalue)) THEN
                il_nok = il_nok + 1
                rla_normabscissa(il_nok) = (rda_abscissa(il_ji) - rl_abscissaver) /&
                   rl_abscissalar
                rla_normdata(il_nok) = (rda_data(il_ji) - rl_dataver) / &
                   rl_datalar
 
             ENDIF
          ENDDO
 
 
          DO il_ji=1, id_nbinterdata
             IF (rda_interabscissa(il_ji).NE.rd_emptyvalue) THEN
                rla_norminterabscissa(il_ji) = (rda_interabscissa(il_ji) - rl_abscissaver) /&
                   rl_abscissalar
             ENDIF
          ENDDO
 
 
          IF (il_nok.GE.2) THEN
 
             !----------------------------------------------------------------------
             ! Calculate derivative
 
             rla_derivate(1) = (rla_normdata(2)-rla_normdata(1)) /&
                               (rla_normabscissa(2)-rla_normabscissa(1))
             DO il_ji = 2, il_nok-1
                rla_derivate(il_ji) = (rla_normdata(il_ji+1)-rla_normdata(il_ji-1)) / &
                              (rla_normabscissa(il_ji+1)-rla_normabscissa(il_ji-1))
             ENDDO
             rla_derivate(il_nok) = (rla_normdata(il_nok)-rla_normdata(il_nok-1)) / &
                              (rla_normabscissa(il_nok)-rla_normabscissa(il_nok-1))
 
 
             !----------------------------------------------------------------------
             ! Calculate rank 2 linear system coefficients
 
             DO il_ji = 1, il_nok-1
                rl_a1 = 2*rla_normabscissa(il_ji)*rla_normabscissa(il_ji)*rla_normabscissa(il_ji) - &
                        3*rla_normabscissa(il_ji+1)*rla_normabscissa(il_ji)*rla_normabscissa(il_ji) + &
                        rla_normabscissa(il_ji+1)*rla_normabscissa(il_ji+1)*rla_normabscissa(il_ji+1)
                rl_b1 = (rla_normabscissa(il_ji+1)-rla_normabscissa(il_ji)) * &
                        (rla_normabscissa(il_ji+1)-rla_normabscissa(il_ji))
                rl_c1 = rla_normdata(il_ji+1)-rla_normdata(il_ji) - &
                        rla_derivate(il_ji)*(rla_normabscissa(il_ji+1)-rla_normabscissa(il_ji))
                rl_a2 = 3*(rla_normabscissa(il_ji+1)*rla_normabscissa(il_ji+1) - &
                        rla_normabscissa(il_ji)*rla_normabscissa(il_ji))
                rl_b2 = 2*(rla_normabscissa(il_ji+1)-rla_normabscissa(il_ji))
                rl_c2 = rla_derivate(il_ji+1)-rla_derivate(il_ji)
 
                ! solve for a and b
 
                rl_deter = rl_a1*rl_b2-rl_a2*rl_b1
                rla_Acoeff(il_ji) = (rl_c1*rl_b2-rl_c2*rl_b1)/rl_deter
                rla_Bcoeff(il_ji) = (rl_a1*rl_c2-rl_a2*rl_c1)/rl_deter
 
                ! substitute for c and d
 
                rla_Ccoeff(il_ji) = -3*rla_Acoeff(il_ji) * &
                                       rla_normabscissa(il_ji)*rla_normabscissa(il_ji) - &
                                     2*rla_Bcoeff(il_ji) * &
                                    rla_normabscissa(il_ji)+rla_derivate(il_ji)
                rla_Dcoeff(il_ji) = -rla_Acoeff(il_ji) * &
                                       rla_normabscissa(il_ji)*rla_normabscissa(il_ji)* &
                                       rla_normabscissa(il_ji) - &
                                     rla_Bcoeff(il_ji) * &
                                       rla_normabscissa(il_ji)*rla_normabscissa(il_ji) - &
                                     rla_Ccoeff(il_ji) * &
                                       rla_normabscissa(il_ji)+rla_normdata(il_ji)
 
             ENDDO
 
          ENDIF
 
          !----------------------------------------------------------------------
          ! Calculate interpolated values
 
          il_ji = 1
          DO il_jj = 1, id_nbinterdata
 
             IF (il_nok.GE.2) THEN
 
                IF (rda_interabscissa(il_jj).NE.rd_emptyvalue) THEN
 
                   rl_test = rl_abscissaver + rl_abscissalar*rla_normabscissa(il_ji+1)
                   DO WHILE (     (rda_interabscissa(il_jj).GT.rl_test) &
                             .AND. &
                                  (il_ji.LT.il_nok-1))
                      il_ji = il_ji+1
                      rl_test = rl_abscissaver + rl_abscissalar*rla_normabscissa(il_ji+1)
                   ENDDO
 
                   rl_result = rl_dataver + rl_datalar * &
                                       ( rla_Acoeff(il_ji) * &
                                          rla_norminterabscissa(il_jj) * &
                                          rla_norminterabscissa(il_jj) * &
                                          rla_norminterabscissa(il_jj) &
                                       + rla_Bcoeff(il_ji) * &
                                          rla_norminterabscissa(il_jj) * &
                                          rla_norminterabscissa(il_jj) &
                                       + rla_Ccoeff(il_ji) * &
                                          rla_norminterabscissa(il_jj) &
                                       + rla_Dcoeff(il_ji) )
                ELSE
                   rl_result = rd_emptyvalue
                ENDIF
 
             ELSE
 
                rl_result = rd_emptyvalue
 
             ENDIF
 
             rda_interdata(il_jj) = rl_result
 
          ENDDO
 
        ENDSUBROUTINE MCAL_interp1D_cubicspline_R8
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************

          !!=====================================================================
          !> \brief
          !! Description: This function interpolates 1D data values on an
          !!              interpolated abscissa. The interpolation method is
          !!              a cubic spline.
          !!
          !!  @param rda_data            Vector of the values to be interpolated.
          !!  @param rda_abscissa        Vector of original abcsissa.
          !!  @param id_nbdata           Length of the data values array.
          !!  @param rda_interdata       Vector of values interpolated
          !!  @param rda_interabscissa   Vector of new abscissa.
          !!  @param id_nbinterdata      Length of the interpolated data values array.
          !!  @param rd_emptyvalue       Specify the empty value of the original data array.
          !!
          !! History :
          !!    \n       1985 (E. Dombrowsky) Original
          !!    \n        1993 (P. de Mey) Improved
          !!    \n        09/2006 (F. Messal) F90
          !!    \n        11/2006 (F. Messal) CVS version 1.0
          !<
          !!=====================================================================
  

        SUBROUTINE MCAL_interp1D_cubicspline_R4 (rda_data, &
                                                 rda_abscissa, &
                                                 id_nbdata, &
                                                 rda_interdata, &
                                                 rda_interabscissa, &
                                                 id_nbinterdata, &
                                                 rd_emptyvalue)
 
          IMPLICIT NONE
 
          INTEGER,                                 INTENT(IN) :: id_nbdata, &
                                                                 id_nbinterdata
          REAL(KIND=4), DIMENSION(id_nbdata),      INTENT(IN) :: rda_data, &
                                                                 rda_abscissa
          REAL(KIND=4), DIMENSION(id_nbinterdata), INTENT(IN) :: rda_interabscissa
          REAL(KIND=4), DIMENSION(id_nbinterdata), INTENT(OUT) :: rda_interdata
          REAL(KIND=4),                            INTENT(IN) :: rd_emptyvalue
 
          REAL(KIND=8), DIMENSION(id_nbdata) :: rla_normdata, rla_normabscissa, &
                                                rla_derivate
          REAL(KIND=8), DIMENSION(id_nbdata-1) :: rla_Acoeff, rla_Bcoeff, rla_Ccoeff, &
                                                  rla_Dcoeff
          REAL(KIND=8), DIMENSION(id_nbinterdata) :: rla_norminterabscissa
          REAL(KIND=8) :: rl_dataminvalue, rl_datamaxvalue, rl_abscissaminvalue, &
                          rl_abscissamaxvalue, rl_datalar, rl_abscissalar, &
                          rl_dataver, rl_abscissaver, rl_result
          REAL(KIND=8) :: rl_a1, rl_b1, rl_c1, rl_a2, rl_b2, &
                          rl_c2, rl_deter, rl_test
          INTEGER :: il_ji, il_nok, il_jj
 
 
          !----------------------------------------------------------------------
          ! Calculate range, check dimensions
 
          rl_dataminvalue = 1.E+35
          rl_abscissaminvalue = 1.E+35
          rl_datamaxvalue = 1.E-35
          rl_abscissamaxvalue = 1.E-35
          il_nok = 0
 
          DO il_ji=1, id_nbdata
             IF (     (rda_data(il_ji).NE.rd_emptyvalue) &
                 .AND. &
                      (rda_abscissa(il_ji).NE.rd_emptyvalue) &
                ) THEN
                IF (rda_data(il_ji).LT.rl_dataminvalue) THEN
                   rl_dataminvalue = rda_data(il_ji)
                ENDIF
                IF (rda_data(il_ji).GT.rl_datamaxvalue) THEN
                   rl_datamaxvalue = rda_data(il_ji)
                ENDIF
                IF (rda_abscissa(il_ji).LT.rl_abscissaminvalue) THEN
                   rl_abscissaminvalue = rda_abscissa(il_ji)
                ENDIF
                IF (rda_abscissa(il_ji).GT.rl_abscissamaxvalue) THEN
                   rl_abscissamaxvalue = rda_abscissa(il_ji)
                ENDIF
             ELSE
             ENDIF
          ENDDO
 
 
          rl_abscissalar = (rl_abscissamaxvalue - rl_abscissaminvalue)
          IF (rl_abscissalar.EQ.0.0) THEN
             rl_abscissalar = 1.0
          ENDIF
 
          rl_datalar = (rl_datamaxvalue - rl_dataminvalue)
          IF (rl_datalar.EQ.0.0) THEN
             rl_datalar = 1.0
          ENDIF
 
          rl_abscissaver = (rl_abscissamaxvalue + rl_abscissaminvalue)/2.0
          rl_dataver = (rl_datamaxvalue + rl_dataminvalue)/2.0
 
 
          !----------------------------------------------------------------------
          ! Normalize
 
          DO il_ji=1, id_nbdata
             IF ((rda_data(il_ji).NE.rd_emptyvalue).AND. &
                 (rda_abscissa(il_ji).NE.rd_emptyvalue)) THEN
                il_nok = il_nok + 1
                rla_normabscissa(il_nok) = (rda_abscissa(il_ji) - rl_abscissaver) /&
                   rl_abscissalar
                rla_normdata(il_nok) = (rda_data(il_ji) - rl_dataver) / &
                   rl_datalar
 
             ENDIF
          ENDDO
 
 
          DO il_ji=1, id_nbinterdata
             IF (rda_interabscissa(il_ji).NE.rd_emptyvalue) THEN
                rla_norminterabscissa(il_ji) = (rda_interabscissa(il_ji) - rl_abscissaver) /&
                   rl_abscissalar
             ENDIF
          ENDDO
 
 
          IF (il_nok.GE.2) THEN
 
             !----------------------------------------------------------------------
             ! Calculate derivative
 
             rla_derivate(1) = (rla_normdata(2)-rla_normdata(1)) /&
                               (rla_normabscissa(2)-rla_normabscissa(1))
             DO il_ji = 2, il_nok-1
                rla_derivate(il_ji) = (rla_normdata(il_ji+1)-rla_normdata(il_ji-1)) / &
                              (rla_normabscissa(il_ji+1)-rla_normabscissa(il_ji-1))
             ENDDO
             rla_derivate(il_nok) = (rla_normdata(il_nok)-rla_normdata(il_nok-1)) / &
                              (rla_normabscissa(il_nok)-rla_normabscissa(il_nok-1))
 
 
             !----------------------------------------------------------------------
             ! Calculate rank 2 linear system coefficients
 
             DO il_ji = 1, il_nok-1
                rl_a1 = 2*rla_normabscissa(il_ji)*rla_normabscissa(il_ji)*rla_normabscissa(il_ji) - &
                        3*rla_normabscissa(il_ji+1)*rla_normabscissa(il_ji)*rla_normabscissa(il_ji) + &
                        rla_normabscissa(il_ji+1)*rla_normabscissa(il_ji+1)*rla_normabscissa(il_ji+1)
                rl_b1 = (rla_normabscissa(il_ji+1)-rla_normabscissa(il_ji)) * &
                        (rla_normabscissa(il_ji+1)-rla_normabscissa(il_ji))
                rl_c1 = rla_normdata(il_ji+1)-rla_normdata(il_ji) - &
                        rla_derivate(il_ji)*(rla_normabscissa(il_ji+1)-rla_normabscissa(il_ji))
                rl_a2 = 3*(rla_normabscissa(il_ji+1)*rla_normabscissa(il_ji+1) - &
                        rla_normabscissa(il_ji)*rla_normabscissa(il_ji))
                rl_b2 = 2*(rla_normabscissa(il_ji+1)-rla_normabscissa(il_ji))
                rl_c2 = rla_derivate(il_ji+1)-rla_derivate(il_ji)
 
                ! solve for a and b
 
                rl_deter = rl_a1*rl_b2-rl_a2*rl_b1
                rla_Acoeff(il_ji) = (rl_c1*rl_b2-rl_c2*rl_b1)/rl_deter
                rla_Bcoeff(il_ji) = (rl_a1*rl_c2-rl_a2*rl_c1)/rl_deter
 
                ! substitute for c and d
 
                rla_Ccoeff(il_ji) = -3*rla_Acoeff(il_ji) * &
                                       rla_normabscissa(il_ji)*rla_normabscissa(il_ji) - &
                                     2*rla_Bcoeff(il_ji) * &
                                    rla_normabscissa(il_ji)+rla_derivate(il_ji)
                rla_Dcoeff(il_ji) = -rla_Acoeff(il_ji) * &
                                       rla_normabscissa(il_ji)*rla_normabscissa(il_ji)* &
                                       rla_normabscissa(il_ji) - &
                                     rla_Bcoeff(il_ji) * &
                                       rla_normabscissa(il_ji)*rla_normabscissa(il_ji) - &
                                     rla_Ccoeff(il_ji) * &
                                       rla_normabscissa(il_ji)+rla_normdata(il_ji)
 
             ENDDO
 
          ENDIF
 
          !----------------------------------------------------------------------
          ! Calculate interpolated values
 
          il_ji = 1
          DO il_jj = 1, id_nbinterdata
 
             IF (il_nok.GE.2) THEN
 
                IF (rda_interabscissa(il_jj).NE.rd_emptyvalue) THEN
 
                   rl_test = rl_abscissaver + rl_abscissalar*rla_normabscissa(il_ji+1)
                   DO WHILE (     (rda_interabscissa(il_jj).GT.REAL(rl_test, 4)) &
                             .AND. &
                                  (il_ji.LT.il_nok-1))
                      il_ji = il_ji+1
                      rl_test = rl_abscissaver + rl_abscissalar*rla_normabscissa(il_ji+1)
                   ENDDO
 
                   rl_result = rl_dataver + rl_datalar * &
                                       ( rla_Acoeff(il_ji) * &
                                          rla_norminterabscissa(il_jj) * &
                                          rla_norminterabscissa(il_jj) * &
                                          rla_norminterabscissa(il_jj) &
                                       + rla_Bcoeff(il_ji) * &
                                          rla_norminterabscissa(il_jj) * &
                                          rla_norminterabscissa(il_jj) &
                                       + rla_Ccoeff(il_ji) * &
                                          rla_norminterabscissa(il_jj) &
                                       + rla_Dcoeff(il_ji) )
                ELSE
                   rl_result = rd_emptyvalue
                ENDIF
 
             ELSE
 
                rl_result = rd_emptyvalue
 
             ENDIF
 
             rda_interdata(il_jj) = REAL(rl_result, 4)
 
          ENDDO
 
        ENDSUBROUTINE MCAL_interp1D_cubicspline_R4
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
          
          !!=====================================================================
          !> \brief
          !! Description: This function interpolates 2D data values on a 2D grid.
          !!              Grid type 1D: longitude(longitude), latitude (latitude)
          !!              Grid type 2D: longitude(x,y), latitude(x,y)
          !!              The 4 nearest neightbors mean interpolation method is
          !!              used.
          !!
          !!  @param ida_values        2D array of values to be interpolated.
          !!  @param rda_lonvalues     Vector of the original longitude values.
          !!  @param rda_latvalues     Vector of the original latitude values.
          !!  @param id_nblonvalues    Length of the original longitude vector.
          !!  @param id_nblatvalues    Length of the original latitude vector.
          !!  @param rda_longrid       Vector of the interpolated longitude values.
          !!  @param rda_latgrid       Vector of the interpolated latitude values.
          !!  @param id_nblongrid      Length of the interpolated longitude vector.
          !!  @param id_nblatgrid      Length of the interpolated latitude vector.
          !!  @param rda_result        2D array of interpolated values.
          !!  @param ld_ismask         If the data represents a mask ([0,1]), "ismask"
          !!                      value equals true.(Optional)
          !!  @param id_fillvalue      Specify the fille value.(Optional)
          !!
          !! History :
          !!   \n        09/2005 (G. Vinay) Original
          !!   \n        08/2006 (F. Messal) F90
          !!   \n        11/2006 (F. Messal) CVS version 1.0
          !!   \n        10/2008  (C. REGNIER) Add MFT_error MIOL 3.1
          !!   \n        10/2009  (C. REGNIER) Modif Nec changement de
          !!                                   nom de nom de routine trop long (> 31 char)
          !<
          !!=====================================================================
 
        SUBROUTINE MCAL_int2D_gtype1D_4NNmean_R4 (rda_values, &
                                              rda_lonvalues, &
                                              rda_latvalues, &
                                              id_nblonvalues, &
                                              id_nblatvalues, &
                                              rda_longrid, &
                                              rda_latgrid, &
                                              id_nblongrid, &
                                              id_nblatgrid, &
                                              rda_result, &
                                              ld_ismask, &
                                              rd_fillvalue)
 
 
          USE MFT_error
          IMPLICIT NONE
     
          INTEGER,                                               INTENT( IN) :: id_nblonvalues, id_nblatvalues
          REAL(KIND=4), DIMENSION(id_nblonvalues, id_nblatvalues), INTENT( IN) :: rda_values
          REAL(KIND=4), DIMENSION(id_nblonvalues),               INTENT( IN) :: rda_lonvalues
          REAL(KIND=4), DIMENSION(id_nblatvalues),               INTENT( IN) :: rda_latvalues
          INTEGER,                                               INTENT( IN) :: id_nblongrid, id_nblatgrid
          REAL(KIND=4), DIMENSION(id_nblongrid),                 INTENT( IN) :: rda_longrid
          REAL(KIND=4), DIMENSION(id_nblatgrid),                 INTENT( IN) :: rda_latgrid
          REAL(KIND=4), DIMENSION(id_nblongrid, id_nblatgrid),   INTENT(OUT) :: rda_result
          LOGICAL,                                               OPTIONAL :: ld_ismask
          REAL(KIND=4),                                          OPTIONAL :: rd_fillvalue
 
          CHARACTER(LEN=30) :: cl_fonction
          INTEGER :: il_ji, il_jj, il_jk, il_nbval, il_status
          REAL(KIND=4) :: rl_result
          INTEGER, DIMENSION(id_nblongrid, 2) :: ila_lonminmax
          INTEGER, DIMENSION(id_nblatgrid, 2) :: ila_latminmax
          REAL(KIND=4), DIMENSION(4) :: rla_points
          REAL(KIND=4) :: rl_value
          LOGICAL :: ll_flag
 
          cl_fonction="MCAL_interp2D_gtype1D_4NNmean_R4"
          !----------------------------------------------------------------------
          ! Test array
          il_status = fi_arrError(MINVAL(rda_lonvalues), &
                                  MAXVAL(rda_lonvalues), &
                                  'rda_lonvalues',cl_fonction)
 
          il_status = fi_arrError(MINVAL(rda_latvalues), &
                                  MAXVAL(rda_latvalues), &
                                  'rda_latvalues',cl_fonction)
 
          il_status = fi_arrError(MINVAL(rda_longrid), &
                                  MAXVAL(rda_longrid), &
                                  'rda_longrid',cl_fonction)
 
          il_status = fi_arrError(MINVAL(rda_latgrid), &
                                  MAXVAL(rda_latgrid), &
                                  'rda_latgrid',cl_fonction)
 
          il_status = fi_arrError(MINVAL(rda_values), &
                                  MAXVAL(rda_values), &
                                  'rda_values',cl_fonction)
 
          !----------------------------------------------------------------------
          ! find minimum i and j indices for longitudes
 
          DO il_ji=1, id_nblongrid
 
             IF(rda_longrid(il_ji) .GT. 180.0) THEN
                rl_value = rda_longrid(il_ji) - 360.0
             ELSE
                rl_value = rda_longrid(il_ji)
             ENDIF
 
             IF (rl_value.LT.rda_lonvalues(1)) THEN
                ila_lonminmax(il_ji,1)=1
                ila_lonminmax(il_ji,2)=1
 
             ELSE IF (rl_value.GE.rda_lonvalues(id_nblonvalues)) THEN
                   ila_lonminmax(il_ji,1)=id_nblonvalues
                   ila_lonminmax(il_ji,2)=id_nblonvalues
 
             ELSE
 
                il_jj=1
                il_jk=il_jj+1
                ila_lonminmax(il_ji,1)=il_jj
                ila_lonminmax(il_ji,2)=ila_lonminmax(il_ji,1)+1
 
                ll_flag = .TRUE.
                DO WHILE (ll_flag)
                   ila_lonminmax(il_ji,1)=il_jj
                   ila_lonminmax(il_ji,2)=il_jk
 
                   IF (il_jj.EQ.id_nblonvalues) THEN
                      ila_lonminmax(il_ji,2)=il_jj
                   ENDIF
 
                   il_jj=il_jj+1
                   il_jk=il_jj+1
 
                   IF (il_jj.LE.id_nblonvalues) THEN
 
                      IF ((rda_lonvalues(il_jj).LE.rl_value) .AND. &
                          (rda_lonvalues(il_jk).GT.rl_value)) THEN
                         ll_flag = .FALSE.
                      ELSE
                         ll_flag = .TRUE.
                      ENDIF
 
                   ELSE
                      ll_flag = .FALSE.
                   ENDIF
 
                ENDDO
 
             ENDIF
 
          ENDDO
 
          !----------------------------------------------------------------------
          ! find minimum i and j indices for latitudes
 
          DO il_ji=1, id_nblatgrid
             IF (rda_latgrid(il_ji).LT.rda_latvalues(1)) THEN
                ila_latminmax(il_ji,1)=1
                ila_latminmax(il_ji,2)=1
 
             ELSE IF (rda_latgrid(il_ji).GE.rda_latvalues(id_nblatvalues)) THEN
                ila_latminmax(il_ji,1)=id_nblatgrid
                ila_latminmax(il_ji,2)=id_nblatgrid
 
             ELSE
                il_jj=1
                il_jk=il_jj+1
 
                ila_latminmax(il_ji,1)=il_jj
                ila_latminmax(il_ji,2)=ila_latminmax(il_ji,1)+1
 
                ll_flag = .TRUE.
                DO WHILE (ll_flag)
                   ila_latminmax(il_ji,1)=il_jj
                   ila_latminmax(il_ji,2)=il_jk
 
                   IF (il_jj.LE.SIZE(rda_latvalues)) THEN
                      ila_latminmax(il_ji,2)=il_jj
                   ENDIF
 
                   il_jj=il_jj+1
                   il_jk=il_jj+1
 
                   IF (il_jj.LE.SIZE(rda_latvalues)) THEN
                      IF ((rda_latvalues(il_jj).LE.rda_latgrid(il_ji)) .AND. &
                          (rda_latvalues(il_jk).GT.rda_latgrid(il_ji))) THEN
                         ll_flag = .FALSE.
                      ELSE
                         ll_flag = .TRUE.
                      ENDIF
 
                   ELSE
                      ll_flag = .FALSE.
                   ENDIF
 
 
                ENDDO
 
             ENDIF
 
 
          ENDDO
 
          !----------------------------------------------------------------------
          ! compute bathymetry
 
          DO il_ji=1, id_nblongrid
             DO il_jj=1, id_nblatgrid
 
                rla_points(1) = rda_values(ila_lonminmax(il_ji,1), &
                                           ila_latminmax(il_jj,1))
                rla_points(2) = rda_values(ila_lonminmax(il_ji,2), &
                                           ila_latminmax(il_jj,1))
                rla_points(3) = rda_values(ila_lonminmax(il_ji,1), &
                                           ila_latminmax(il_jj,2))
                rla_points(4) = rda_values(ila_lonminmax(il_ji,2), &
                                           ila_latminmax(il_jj,2))
 
                il_nbval=4
                IF (PRESENT(rd_fillvalue)) THEN
                   DO il_jk=1,4
                      IF (rla_points(il_jk).EQ.rd_fillvalue) THEN
                         il_nbval=il_nbval-1
                         rla_points(il_jk)=0
                      ENDIF
                   ENDDO
                ENDIF
 
                rl_result=( (rla_points(1)+rla_points(2)+rla_points(3)+ &
                                      rla_points(4)) / il_nbval )
 
 
                IF (PRESENT(ld_ismask)) THEN
                   IF(ld_ismask) THEN
                      IF(rl_result .GE. 0.5) THEN
                         rl_result = 1.
                      ELSE
                         rl_result = 0.
                      ENDIF
                   ENDIF
                ENDIF
 
                rda_result(il_ji,il_jj) = rl_result
 
             ENDDO
          ENDDO
 
 
        END SUBROUTINE MCAL_int2D_gtype1D_4NNmean_R4
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
           !!=====================================================================
           !> \brief
           !! Description: This function interpolates 2D data values on a 2D grid.
           !!              Grid type 1D: longitude(longitude), latitude (latitude)
           !!              Grid type 2D: longitude(x,y), latitude(x,y)
           !!              The 4 nearest neightbors mean interpolation method is
           !!              used.
           !!
           !!  @param ida_values        2D array of values to be interpolated.
           !!  @param rda_lonvalues     Vector of the original longitude values.
           !!  @param rda_latvalues     Vector of the original latitude values.
           !!  @param id_nblonvalues    Length of the original longitude vector.
           !!  @param id_nblatvalues    Length of the original latitude vector.
           !!  @param rda_longrid       2D array of the interpolated longitude values.
           !!  @param rda_latgrid       2D array of the interpolated latitude values.
           !!  @param id_nblongrid      Length of the interpolated longitude vector.
           !!  @param id_nblatgrid      Length of the interpolated latitude vector.
           !!  @param rda_result        2D array of interpolated values.
           !!  @param ld_ismask         If the data represents a mask ([0,1]), "ismask"
           !!                      value equals true.(Optional)
           !!  @param id_fillvalue      Specify the fille value.(Optional)
           !!
           !! History :
           !!       \n    09/2005 (G. Vinay) Original
           !!       \n    10/2007 (F. Messal) F90
           !!       \n    10/2008  (C. REGNIER) Add MFT_error MIOL 3.1
           !!       \n        10/2009  (C. REGNIER) Modif Nec changement de
           !!                           nom de routine trop long (> 31 char)
           !<
           !!=====================================================================
 
         SUBROUTINE MCAL_int2D_gtype2D_4NNmean_R4 (rda_values, &
                                         rda_lonvalues, &
                                         rda_latvalues, &
                                         id_nblonvalues, &
                                         id_nblatvalues, &
                                         rda_longrid, &
                                         rda_latgrid, &
                                         id_nx, &
                                         id_ny, &
                                         rda_result, &
                                         ld_ismask, &
                                         rd_fillvalue)
 
 
           USE MFT_error
           IMPLICIT NONE
 
           INTEGER,                                               INTENT( IN) :: id_nblonvalues, id_nblatvalues
           REAL(KIND=4), DIMENSION(id_nblonvalues, id_nblatvalues), INTENT( IN) :: rda_values
           REAL(KIND=4), DIMENSION(id_nblonvalues),               INTENT( IN) :: rda_lonvalues
           REAL(KIND=4), DIMENSION(id_nblatvalues),               INTENT( IN) :: rda_latvalues
           INTEGER,                                               INTENT( IN) :: id_nx, id_ny
           REAL(KIND=4), DIMENSION(id_nx, id_ny),                 INTENT( IN) :: rda_longrid, &
                                                                                 rda_latgrid
           REAL(KIND=4), DIMENSION(id_nx, id_ny),                 INTENT(OUT) :: rda_result
           LOGICAL,                                               OPTIONAL :: ld_ismask
           REAL(KIND=4),                                          OPTIONAL :: rd_fillvalue
 
           CHARACTER(LEN=30) :: cl_fonction
           INTEGER :: il_ji, il_jj, il_jk, il_jl, il_nbval, il_status
           REAL(KIND=4) :: rl_result
           INTEGER, DIMENSION(id_nx, id_ny, 2) :: ila_lonminmax
           INTEGER, DIMENSION(id_nx, id_ny, 2) :: ila_latminmax
           REAL(KIND=4), DIMENSION(4) :: rla_points
           REAL(KIND=4) :: rl_value
           LOGICAL :: ll_flag

           cl_fonction="MCAL_interp2D_gtype2D_4NNmean_R4"
 
           !----------------------------------------------------------------------
           ! Test array
           il_status = fi_arrError(MINVAL(rda_lonvalues), &
                                   MAXVAL(rda_lonvalues), &
                                   'rda_lonvalues',cl_fonction)
 
           il_status = fi_arrError(MINVAL(rda_latvalues), &
                                   MAXVAL(rda_latvalues), &
                                   'rda_latvalues',cl_fonction)
 
           il_status = fi_arrError(MINVAL(rda_longrid), &
                                   MAXVAL(rda_longrid), &
                                   'rda_longrid',cl_fonction)
 
           il_status = fi_arrError(MINVAL(rda_latgrid), &
                                   MAXVAL(rda_latgrid), &
                                   'rda_latgrid',cl_fonction)
 
           il_status = fi_arrError(MINVAL(rda_values), &
                                   MAXVAL(rda_values), &
                                   'rda_values',cl_fonction)
 
           !----------------------------------------------------------------------
           ! find minimum i and j indices for longitudes
 
           DO il_ji=1, id_nx
              DO il_jj=1, id_ny
 
                 IF(rda_longrid(il_ji, il_jj) .GT. 180.0) THEN
                    rl_value = rda_longrid(il_ji, il_jj) - 360.0
                 ELSE
                    rl_value = rda_longrid(il_ji, il_jj)
                 ENDIF
 
              IF (rl_value.LT.rda_lonvalues(1)) THEN
                 ila_lonminmax(il_ji, il_jj, 1)=1
                 ila_lonminmax(il_ji, il_jj, 2)=1
 
              ELSE IF (rl_value.GE.rda_lonvalues(id_nblonvalues)) THEN
                    ila_lonminmax(il_ji, il_jj, 1)=id_nblonvalues
                    ila_lonminmax(il_ji, il_jj, 2)=id_nblonvalues
 
              ELSE
 
                 il_jk=1
                 il_jl=il_jk+1
                 ila_lonminmax(il_ji, il_jj, 1)=il_jk
                 ila_lonminmax(il_ji, il_jj, 2)=ila_lonminmax(il_ji, il_jj, 1)+1
 
                 ll_flag = .TRUE.
                 DO WHILE (ll_flag)
                    ila_lonminmax(il_ji, il_jj, 1)=il_jk
                    ila_lonminmax(il_ji, il_jj, 2)=il_jl
 
                    IF (il_jk .EQ. id_nblonvalues) THEN
                       ila_lonminmax(il_ji, il_jj, 2)=il_jk
                    ENDIF
 
                    il_jk=il_jk+1
                    il_jl=il_jk+1
 
                    IF (il_jk .LE. id_nblonvalues) THEN
 
                       IF ((rda_lonvalues(il_jk) .LE. rl_value) .AND. &
                           (rda_lonvalues(il_jl) .GT. rl_value)) THEN
                          ll_flag = .FALSE.
                       ELSE
                          ll_flag = .TRUE.
                       ENDIF
 
                    ELSE
                       ll_flag = .FALSE.
                    ENDIF
 
                 ENDDO
 
              ENDIF
 
              ENDDO
           ENDDO
 
           !----------------------------------------------------------------------
           ! find minimum i and j indices for latitudes
           DO il_ji=1, id_nx
              DO il_jj=1, id_ny
 
              IF (rda_latgrid(il_ji, il_jj) .LT. rda_latvalues(1)) THEN
                 ila_latminmax(il_ji, il_jj, 1)=1
                 ila_latminmax(il_ji, il_jj, 2)=1
 
              ELSE IF (rda_latgrid(il_ji, il_jj) .GE. rda_latvalues(id_nblatvalues)) THEN
                 ila_latminmax(il_ji, il_jj, 1)=id_ny
                 ila_latminmax(il_ji, il_jj, 2)=id_ny
 
              ELSE
                 il_jk=1
                 il_jl=il_jk+1
 
                 ila_latminmax(il_ji, il_jj, 1)=il_jk
                 ila_latminmax(il_ji, il_jj, 2)=ila_latminmax(il_ji, il_jj, 1)+1
 
                 ll_flag = .TRUE.
                 DO WHILE (ll_flag)
                    ila_latminmax(il_ji, il_jj, 1)=il_jk
                    ila_latminmax(il_ji, il_jj, 2)=il_jl
 
                    IF (il_jk.LE.SIZE(rda_latvalues)) THEN
                       ila_latminmax(il_ji, il_jj, 2)=il_jk
                    ENDIF
 
                    il_jk=il_jk+1
                    il_jl=il_jk+1
 
                    IF (il_jk.LE.SIZE(rda_latvalues)) THEN
                       IF ((rda_latvalues(il_jk).LE.rda_latgrid(il_ji, il_jj)) .AND. &
                           (rda_latvalues(il_jl).GT.rda_latgrid(il_ji, il_jj))) THEN
                          ll_flag = .FALSE.
                       ELSE
                          ll_flag = .TRUE.
                       ENDIF
 
                    ELSE
                       ll_flag = .FALSE.
                    ENDIF
 
 
                 ENDDO
 
              ENDIF
 
              ENDDO
           ENDDO
 
           !----------------------------------------------------------------------
           ! compute bathymetry
           DO il_ji=1, id_nx
              DO il_jj=1, id_ny
 
                 rla_points(1) = rda_values(ila_lonminmax(il_ji, il_jj, 1), &
                                            ila_latminmax(il_ji, il_jj, 1))
                 rla_points(2) = rda_values(ila_lonminmax(il_ji, il_jj, 2), &
                                            ila_latminmax(il_ji, il_jj, 1))
                 rla_points(3) = rda_values(ila_lonminmax(il_ji, il_jj, 1), &
                                            ila_latminmax(il_ji, il_jj, 2))
                 rla_points(4) = rda_values(ila_lonminmax(il_ji, il_jj, 2), &
                                            ila_latminmax(il_ji, il_jj, 2))
 
                 il_nbval=4
                 IF (PRESENT(rd_fillvalue)) THEN
                    DO il_jk=1,4
                       IF (rla_points(il_jk).EQ.rd_fillvalue) THEN
                          il_nbval=il_nbval-1
                          rla_points(il_jk)=0
                       ENDIF
                    ENDDO
                 ENDIF
 
                 rl_result=( (rla_points(1)+rla_points(2)+rla_points(3)+ &
                                       rla_points(4)) / il_nbval )
 
                 IF (PRESENT(ld_ismask)) THEN
                    IF(ld_ismask) THEN
                       IF(rl_result .GE. 0.5) THEN
                          rl_result = 1.
                       ELSE
                          rl_result = 0.
                       ENDIF
                    ENDIF
                 ENDIF
 
                 rda_result(il_ji,il_jj) = rl_result
 
              ENDDO
           ENDDO
 
 
         END SUBROUTINE MCAL_int2D_gtype2D_4NNmean_R4
 
 
   !******************************************************************************
   !******************************************************************************
   !******************************************************************************
 
          !!=====================================================================
          !> \brief
          !! Description: This function creates 2D grid and returns longitude an
          !!              latitude values array (1D : longitude(longitude) or
          !!              2D: longitude(x,y)). The user choose the kind of grid:
          !!              regular in degrees or mercator projection.
          !!
          !!  @param rd_lonmin       The minimum longitude value of the grid.
          !!  @param rd_lonmax       The maximum longitude value of the grid.
          !!  @param rd_lonres       The resolution value of the longitude axis.
          !!                    (Ex: for 1/4° resolution, value is 4)
          !!  @param rd_latmin       The minimum latitude value of the grid.
          !!  @param rd_latmax       The maximum latitude value of the grid.
          !!  @param rd_latres       The resolution value of the latitude axis.
          !!                    (Ex: for 1/4° resolution, value is 4)
          !!  @param cd_gridtype     The type of the grid (latitude axis): 'DEG' for
          !!                    regular in degrees latitude axis, 'MER' for
          !!                    mercator projection latitude axis.
          !!  @param rdpa_longitude  Vector or array of longitude values. It depends on
          !!                    the longitude array number of dimension.
          !!  @param rdpa_latitude   Vector or array of latitude values. It depends on
          !!                    the latitude array number of dimension.
          !!  @param id_nblon        Number of longitude values.(Optional)
          !!  @param id_nblat        Number of latitude values.(Optional)
          !!
          !! History :
          !!   \n        Original    : 07/2006 (S. Besnard)
          !!   \n        Modif       : 01/2008 (N. PENE) Mercator grid limit modif.
          !!   \n        Modif       : 06/2008  C.REGNIER Modifs for GODAE 
          !!   \n        Modif       : 10/2008  C.REGNIER add MFT_error 
          !!   \n        Modif       : 10/2009  C.REGNIER Modif Nec changement de
          !!                                    nom de nom de routine trop long (> 31 char)
          !<
          !!=====================================================================
 
        SUBROUTINE MCAL_gen_reggrid_gtype1D (cd_gridtype, &
                                                rd_lonmin, &
                                                rd_lonmax, &
                                                rd_xresolution, &
                                                rd_latmin, &
                                                rd_latmax, &
                                                rd_yresolution, &
                                                rdpa_longitude, &
                                                rdpa_latitude, &
                                                id_nblon, &
                                                id_nblat)
 
          USE MFT_error
          IMPLICIT NONE
 
         !----------------------------------------------------------------------
 
          CHARACTER(LEN=3), INTENT(IN) :: cd_gridtype
          CHARACTER(LEN=255)           :: cl_fonction
          REAL(KIND=4), INTENT(IN) :: rd_lonmin, rd_lonmax, rd_latmin, rd_latmax
          REAL(KIND=8), INTENT(IN) :: rd_xresolution, rd_yresolution
          REAL(KIND=8), POINTER, DIMENSION(:) :: rdpa_longitude, rdpa_latitude
          INTEGER, OPTIONAL :: id_nblon, id_nblat
 
          INTEGER :: il_nbx, il_nby, il_ji, il_nbsouth, il_nbnorth, il_status, &
                     il_jmin, il_jmax
          REAL(KIND=8) :: rl_dx, rl_dy
          REAL(KIND=8), DIMENSION(50000) :: rla_northlat, rla_southlat
          REAL(KIND=8), PARAMETER :: rl_northlimit=89., rl_southlimit=-89.
          REAL(KIND=8) :: rl_pi
          
          cl_fonction="MCAL_generate_regulgrid_gtype1D"
          
          !----------------------------------------------------------------------
          print *,'MCAL_generate_regulgrid_gtype1D'
          rl_pi = 4.*ATAN(1.)
 
          !----------------------------------------------------------------------
          ! Verify
          il_status = fi_arrError(rd_latmin, rd_latmax, ' bounds.',cl_fonction)
          il_status = fi_arrError(rd_lonmin, rd_lonmax, ' bounds.',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Resolutions
          rl_dx = 1./rd_xresolution
          rl_dy = 1./rd_yresolution
          !----------------------------------------------------------------------
          ! Compute values
          IF ((cd_gridtype .EQ. 'deg') .OR. (cd_gridtype .EQ. 'DEG')) THEN
             il_nbx = NINT((rd_lonmax-rd_lonmin)/rl_dx) + 1
             il_nby = NINT((rd_latmax-rd_latmin)/rl_dy) + 1
             ALLOCATE(rdpa_longitude(il_nbx), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_longitude',cl_fonction)
             
             ALLOCATE(rdpa_latitude(il_nby), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_latitude',cl_fonction)
  
             DO il_ji=1,il_nbx
                rdpa_longitude(il_ji) = rd_lonmin + (il_ji-1)*rl_dx
             ENDDO
             DO il_ji=1,il_nby
                rdpa_latitude(il_ji)  = rd_latmin + (il_ji-1)*rl_dy
             ENDDO
 
             id_nblon = il_nbx
             id_nblat = il_nby
 
          ELSE IF ((cd_gridtype .EQ. 'mer') .OR. (cd_gridtype .EQ. 'MER')) THEN

           !  il_nbx = NINT(rd_lonmax-rd_lonmin)/rl_dx + 1
             il_nbx = NINT((rd_lonmax-rd_lonmin)/rl_dx) + 1

            ALLOCATE(rdpa_longitude(il_nbx), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_longitude',cl_fonction)

             !all grid points latitude calculation
             il_ji=1
             rla_northlat(1)=0.
             DO WHILE (rla_northlat(il_ji) < rl_northlimit)
                il_ji=il_ji+1
                rla_northlat(il_ji) = ASIN(TANH((0. + (il_ji-1)*rl_dy)*rl_pi/180.))* &
                                     180./rl_pi
                il_nbnorth=il_ji-1
             ENDDO
             il_ji=1
             rla_southlat(1)=0.
             DO WHILE (rla_southlat(il_ji) > rl_southlimit)
                il_ji=il_ji+1
                rla_southlat(il_ji) = ASIN(TANH((0. - (il_ji-1)*rl_dy)*rl_pi/180.))* &
                                    180./rl_pi
                il_nbsouth=il_ji-1
             ENDDO

 
             !North pole area
             IF (rd_latmin >= 0.) THEN
 
                ! mod b11
                ! look for min indices of rla_northlat
                il_ji = 1
                DO WHILE (il_ji < il_nbnorth .AND. rla_northlat(il_ji) < rd_latmin )
                   il_ji=il_ji+1
                END DO
                il_jmin=il_ji
                ! look for max indices of rla_northlat
                IF ( rd_latmax >= rla_northlat(il_nbnorth) ) THEN
                   il_jmax=il_nbnorth
                ELSE
                   il_ji=il_jmin
                   DO WHILE (il_ji <= il_nbnorth .AND. rla_northlat(il_ji) < rd_latmax )
                      il_ji=il_ji+1
                   END DO
                   il_jmax=il_ji-1
                ENDIF
                ! end mod b11

                !* Define latitude array
                 il_nby=il_jmax-il_jmin+1

              ALLOCATE(rdpa_latitude(il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_latitude',cl_fonction)

                DO il_ji=1,il_nbx
                   rdpa_longitude(il_ji) = REAL(rd_lonmin + (il_ji-1)*rl_dx,8)
                ENDDO
 
                rdpa_latitude(1:il_nby) = REAL(rla_northlat(il_jmin:il_jmax),8)
 
 
             ! area on both south and north poles
             ELSE IF ((rd_latmin < 0.) .AND. (rd_latmax > 0.)) THEN

                !* look for min indices of rla_southlat     
                DO il_ji=1,il_nbsouth
                   IF (rla_southlat(il_ji) >= rd_latmin) il_jmin=il_ji
                ENDDO
                !* look for max indices of rla_northlat 
                DO il_ji=1,il_nbnorth
                   IF (rla_northlat(il_ji) <= rd_latmax) il_jmax=il_ji
                ENDDO
                
                !* Define latitude array
                il_nby=il_jmin+il_jmax-1
                ALLOCATE(rdpa_latitude(il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_latitude',cl_fonction)
 
                DO il_ji=1,il_nbx
                   rdpa_longitude(il_ji) = REAL(rd_lonmin + (il_ji-1)*rl_dx,8)
                ENDDO
                DO il_ji=1,il_jmin-1
                   rdpa_latitude(il_ji) = REAL(rla_southlat(il_jmin-il_ji+1),8)
                ENDDO
                DO il_ji=il_jmin,il_nby
                   rdpa_latitude(il_ji) = REAL(rla_northlat(il_ji-il_jmin+1),8)
                ENDDO

             ! south pole area
             ELSE IF ((rd_latmin < 0.) .AND. (rd_latmax <=0.)) THEN
 
                ! mod b11
                il_ji = 1
                DO WHILE (il_ji < il_nbsouth .AND. rla_southlat(il_ji) > rd_latmax )
                   il_ji=il_ji+1
                END DO
                il_jmax=il_ji
                ! look for min indices of rla_southlat
                IF ( rd_latmin <= rla_southlat(il_nbsouth) ) THEN
                   il_jmin=il_nbsouth
                ELSE
                   il_ji=il_jmax
                   DO WHILE (il_ji <= il_nbsouth .AND. rla_southlat(il_ji) > rd_latmin )
                      il_ji=il_ji+1
                   END DO
                   il_jmin=il_ji-1
                ENDIF
                ! end mod b11

                !* Define latitude array
                il_nby=ABS(il_jmax-il_jmin)+1
                ALLOCATE(rdpa_latitude(il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_latitude',cl_fonction)

                DO il_ji=1,il_nbx
                   rdpa_longitude(il_ji) = REAL(rd_lonmin + (il_ji-1)*rl_dx,8)
                ENDDO
                DO il_ji=1,il_nby
                   rdpa_latitude(il_ji) = REAL(rla_southlat(il_jmin-il_ji+1),8)
                ENDDO
 
             ENDIF

             id_nblon = il_nbx
             id_nblat = il_nby
 
          ENDIF
 
        ENDSUBROUTINE MCAL_gen_reggrid_gtype1D
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
          !!=====================================================================
          !> \brief
          !! Description: This function interpolates 2D data values on a 2D grid.
          !!              Grid type 1D: longitude(longitude), latitude (latitude)
          !!              Grid type 2D: longitude(x,y), latitude(x,y)
          !!              The 4 nearest neightbors mean interpolation method is
          !!              used.
          !!
          !!  @param ida_values        2D array of values to be interpolated.
          !!  @param rda_lonvalues     Vector of the original longitude values.
          !!  @param rda_latvalues     Vector of the original latitude values.
          !!  @param id_nblonvalues    Length of the original longitude vector.
          !!  @param id_nblatvalues    Length of the original latitude vector.
          !!  @param rda_longrid       Vector of the interpolated longitude values.
          !!  @param rda_latgrid       Vector of the interpolated latitude values.
          !!  @param id_nblongrid      Length of the interpolated longitude vector.
          !!  @param id_nblatgrid      Length of the interpolated latitude vector.
          !!  @param rda_result        2D array of interpolated values.
          !!  @param ld_ismask         If the data represents a mask ([0,1]), "ismask"
          !!                      value equals true.(Optional)
          !!  @param id_fillvalue      Specify the fille value.(Optional)
          !!
          !! History :
          !!       \n    09/2005 (G. Vinay) Original
          !!       \n    08/2006 (F. Messal) F90
          !!       \n    11/2006 (F. Messal) CVS version 1.0
          !!       \n    10/2008 (C.REGNIER) add MFT_error 
          !!       \n    10/2009 (C.REGNIER) Modif Nec changement de
          !!                               nom de routine trop long (> 31 char)
          !<
          !!=====================================================================
        
        SUBROUTINE MCAL_int2D_gtype1D_4NNmean_R8 (rda_values, &
                                              rda_lonvalues, &
                                              rda_latvalues, &
                                              id_nblonvalues, &
                                              id_nblatvalues, &
                                              rda_longrid, &
                                              rda_latgrid, &
                                              id_nblongrid, &
                                              id_nblatgrid, &
                                              rda_result, &
                                              ld_ismask, &
                                              rd_fillvalue)
 
 
          USE MFT_error
          IMPLICIT NONE
  
          INTEGER,                                               INTENT( IN) :: id_nblonvalues, id_nblatvalues
          REAL(KIND=8), DIMENSION(id_nblonvalues, id_nblatvalues), INTENT( IN) :: rda_values
          REAL(KIND=4), DIMENSION(id_nblonvalues),               INTENT( IN) :: rda_lonvalues
          REAL(KIND=4), DIMENSION(id_nblatvalues),               INTENT( IN) :: rda_latvalues
          INTEGER,                                               INTENT( IN) :: id_nblongrid, id_nblatgrid
          REAL(KIND=4), DIMENSION(id_nblongrid),                 INTENT( IN) :: rda_longrid
          REAL(KIND=4), DIMENSION(id_nblatgrid),                 INTENT( IN) :: rda_latgrid
          REAL(KIND=8), DIMENSION(id_nblongrid, id_nblatgrid),   INTENT(OUT) :: rda_result
          LOGICAL,                                               OPTIONAL :: ld_ismask
          REAL(KIND=8),                                          OPTIONAL :: rd_fillvalue
 
          CHARACTER(LEN=30) :: cl_fonction
          INTEGER :: il_ji, il_jj, il_jk, il_nbval, il_status
          REAL(KIND=8) :: rl_result
          INTEGER, DIMENSION(id_nblongrid, 2) :: ila_lonminmax
          INTEGER, DIMENSION(id_nblatgrid, 2) :: ila_latminmax
          REAL(KIND=8), DIMENSION(4) :: rla_points
          LOGICAL :: ll_flag
 
          cl_fonction="MCAL_interp2D_gtype1D_4NNmean_R8"
          !----------------------------------------------------------------------
          ! Test array
          il_status = fi_arrError(MINVAL(rda_lonvalues), &
                                  MAXVAL(rda_lonvalues), &
                                  'rda_lonvalues',cl_fonction)
 
          il_status = fi_arrError(MINVAL(rda_latvalues), &
                                  MAXVAL(rda_latvalues), &
                                  'rda_latvalues',cl_fonction)
 
          il_status = fi_arrError(MINVAL(rda_longrid), &
                                  MAXVAL(rda_longrid), &
                                  'rda_longrid',cl_fonction)
 
          il_status = fi_arrError(MINVAL(rda_latgrid), &
                                  MAXVAL(rda_latgrid), &
                                  'rda_latgrid',cl_fonction)
 
          il_status = fi_arrError(REAL(MINVAL(rda_values),4), &
                                  REAL(MAXVAL(rda_values),4), &
                                  'rda_values',cl_fonction)
 
          !----------------------------------------------------------------------
          ! find minimum i and j indices for longitudes
 
          DO il_ji=1, id_nblongrid
             IF (rda_longrid(il_ji).LT.rda_lonvalues(1)) THEN
                ila_lonminmax(il_ji,1)=1
                ila_lonminmax(il_ji,2)=1
 
             ELSE IF (rda_longrid(il_ji).GE.rda_lonvalues(id_nblonvalues)) THEN
                ila_lonminmax(il_ji,1)=id_nblonvalues
                ila_lonminmax(il_ji,2)=id_nblonvalues
 
             ELSE
 
             il_jj=1
             il_jk=il_jj+1
             ila_lonminmax(il_ji,1)=il_jj
             ila_lonminmax(il_ji,2)=ila_lonminmax(il_ji,1)+1
 
             ll_flag = .TRUE.
             DO WHILE (ll_flag)
                ila_lonminmax(il_ji,1)=il_jj
                ila_lonminmax(il_ji,2)=il_jk
 
                IF (il_jj.EQ.id_nblonvalues) THEN
                   ila_lonminmax(il_ji,2)=il_jj
                ENDIF
 
                il_jj=il_jj+1
                il_jk=il_jj+1
 
                IF (il_jj.LE.id_nblonvalues) THEN
 
                   IF ((rda_lonvalues(il_jj).LE.rda_longrid(il_ji)) .AND. &
                       (rda_lonvalues(il_jk).GT.rda_longrid(il_ji))) THEN
                      ll_flag = .FALSE.
                   ELSE
                      ll_flag = .TRUE.
                   ENDIF
 
                ELSE
                   ll_flag = .FALSE.
                ENDIF
 
             ENDDO
 
             ENDIF
 
          ENDDO
 
          !----------------------------------------------------------------------
          ! find minimum i and j indices for latitudes
 
          DO il_ji=1, id_nblatgrid
             IF (rda_latgrid(il_ji).LT.rda_latvalues(1)) THEN
                ila_latminmax(il_ji,1)=1
                ila_latminmax(il_ji,2)=1
 
             ELSE IF (rda_latgrid(il_ji).GE.rda_latvalues(id_nblatvalues)) THEN
                ila_latminmax(il_ji,1)=id_nblatgrid
                ila_latminmax(il_ji,2)=id_nblatgrid
 
             ELSE
                il_jj=1
                il_jk=il_jj+1
 
                ila_latminmax(il_ji,1)=il_jj
                ila_latminmax(il_ji,2)=ila_latminmax(il_ji,1)+1
 
                ll_flag = .TRUE.
                DO WHILE (ll_flag)
                   ila_latminmax(il_ji,1)=il_jj
                   ila_latminmax(il_ji,2)=il_jk
 
                   IF (il_jj.LE.SIZE(rda_latvalues)) THEN
                      ila_latminmax(il_ji,2)=il_jj
                   ENDIF
 
                   il_jj=il_jj+1
                   il_jk=il_jj+1
 
                   IF (il_jj.LE.SIZE(rda_latvalues)) THEN
                      IF ((rda_latvalues(il_jj).LE.rda_latgrid(il_ji)) .AND. &
                          (rda_latvalues(il_jk).GT.rda_latgrid(il_ji))) THEN
                         ll_flag = .FALSE.
                      ELSE
                         ll_flag = .TRUE.
                      ENDIF
 
                   ELSE
                      ll_flag = .FALSE.
                   ENDIF
 
 
                ENDDO
 
             ENDIF
 
 
          ENDDO
 
          !----------------------------------------------------------------------
          ! compute bathymetry
 
          DO il_ji=1, id_nblongrid
             DO il_jj=1, id_nblatgrid
 
                rla_points(1) = rda_values(ila_lonminmax(il_ji,1), &
                                           ila_latminmax(il_jj,1))
                rla_points(2) = rda_values(ila_lonminmax(il_ji,2), &
                                           ila_latminmax(il_jj,1))
                rla_points(3) = rda_values(ila_lonminmax(il_ji,1), &
                                           ila_latminmax(il_jj,2))
                rla_points(4) = rda_values(ila_lonminmax(il_ji,2), &
                                           ila_latminmax(il_jj,2))
 
                il_nbval=4
                IF (PRESENT(rd_fillvalue)) THEN
                   DO il_jk=1,4
                      IF (rla_points(il_jk).EQ.rd_fillvalue) THEN
                         il_nbval=il_nbval-1
                         rla_points(il_jk)=0
                      ENDIF
                   ENDDO
                ENDIF
 
                rl_result=( (rla_points(1)+rla_points(2)+rla_points(3)+ &
                                      rla_points(4)) / il_nbval )
 
 
                IF (PRESENT(ld_ismask)) THEN
                   IF(ld_ismask) THEN
                      IF(rl_result .GE. 0.5) THEN
                         rl_result = 1.
                      ELSE
                         rl_result = 0.
                      ENDIF
                   ENDIF
                ENDIF
 
                rda_result(il_ji,il_jj) = rl_result
 
             ENDDO
          ENDDO
 
        END SUBROUTINE MCAL_int2D_gtype1D_4NNmean_R8
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
          !!=====================================================================
          !> \brief 
          !! Description: This function creates 2D grid and returns longitude an
          !!              latitude values array (1D : longitude(longitude) or
          !!              2D: longitude(x,y)). The user choose the kind of grid:
          !!              regular or mercator projection.
          !!
          !!  @param rd_lonmin       The minimum longitude value of the grid.
          !!  @param rd_lonmax       The maximum longitude value of the grid.
          !!  @param rd_lonres       The resolution value of the longitude axis.
          !!                    (Ex: for 1/4° resolution, value is 4)
          !!  @param rd_latmin       The minimum latitude value of the grid.
          !!  @param rd_latmax       The maximum latitude value of the grid.
          !!  @param rd_latres       The resolution value of the latitude axis.
          !!                    (Ex: for 1/4° resolution, value is 4)
          !!  @param cd_gridtype     The type of the grid (latitude axis): 'DEG' for
          !!                    regular in degrees latitude axis, 'MER' for
          !!                    mercator projection latitude axis.
          !!  @param rdpa_longitude  Vector or array of longitude values. It depends on
          !!                    the longitude array number of dimension.
          !!  @param rdpa_latitude   Vector or array of latitude values. It depends on
          !!                    the latitude array number of dimension.
          !!  @param id_nblon        Number of longitude values.(Optional)
          !!  @param id_nblat        Number of latitude values.(Optional)
          !!
          !! History :
          !!     \n      Original    : 07/2006 (S. Besnard)
          !!     \n      Modif       : 01/2008 (N. PENE) Mercator grid limit modif.
          !!     \n      Modif       : 10/2008 C.REGNIER add MFT_error 
          !!     \n      Modif       : 10/2009 C.REGNIER Modif Nec changement de
          !!                            nom de nom de routine trop long (> 31 char)  
          !<
          !!=====================================================================
 
        SUBROUTINE MCAL_gen_reggrid_gtype2D (cd_gridtype,&
                                                  rd_lonmin, &
                                                rd_lonmax, &
                                                rd_xresolution, &
                                                rd_latmin, &
                                                rd_latmax, &
                                                rd_yresolution, &
                                                rdpa_longitude, &
                                                rdpa_latitude, &
                                               id_nblon, &
                                               id_nblat)
 
          USE MFT_error
          IMPLICIT NONE
 
         !----------------------------------------------------------------------
 
          CHARACTER(LEN=3), INTENT(IN) :: cd_gridtype
          REAL(KIND=4), INTENT(IN) :: rd_lonmin, rd_lonmax, rd_latmin, rd_latmax
          REAL(KIND=8), INTENT(IN) :: rd_xresolution, rd_yresolution
          REAL(KIND=4), POINTER, DIMENSION(:,:) :: rdpa_longitude, rdpa_latitude
          INTEGER, OPTIONAL :: id_nblon, id_nblat
 
          INTEGER :: il_nbx, il_nby, il_ji, il_jj, il_nbsouth, il_nbnorth, il_status, &
                     il_jmin, il_jmax
          REAL(KIND=4) :: rl_dx, rl_dy
          REAL(KIND=4), DIMENSION(3000) :: rla_northlat, rla_southlat
          REAL(KIND=4), PARAMETER :: rl_northlimit=89., rl_southlimit=-89.
          REAL(KIND=8) :: rl_pi
          CHARACTER(LEN=255) :: cl_fonction

          cl_fonction='MCAL_generate_regulgrid_gtype2D'
          !----------------------------------------------------------------------
          rl_pi = 4.*ATAN(1.)
 
          !----------------------------------------------------------------------
          ! verify
          il_status = fi_arrError(rd_latmin, rd_latmax, ' bounds.',cl_fonction)
          il_status = fi_arrError(rd_lonmin, rd_lonmax, ' bounds.',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! resolutions
          rl_dx = 1./rd_xresolution
          rl_dy = 1./rd_yresolution
 
 
          !----------------------------------------------------------------------
          ! compute values
          IF ((cd_gridtype .EQ. 'deg') .OR. (cd_gridtype .EQ. 'DEG')) THEN
 
             il_nbx = (rd_lonmax-rd_lonmin)/rl_dx + 1
             il_nby = (rd_latmax-rd_latmin)/rl_dy + 1
 
             ALLOCATE(rdpa_longitude(il_nbx, il_nby), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_longitude',cl_fonction)
 
             ALLOCATE(rdpa_latitude(il_nbx, il_nby), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_latitude',cl_fonction)
 
             DO il_ji=1, il_nbx
                DO il_jj=1, il_nby
                   rdpa_longitude(il_ji, il_jj) = rd_lonmin + (il_ji-1)*rl_dx
                   rdpa_latitude(il_ji, il_jj)  = rd_latmin + (il_jj-1)*rl_dy
                ENDDO
             ENDDO
 
             id_nblon = il_nbx
             id_nblat = il_nby
 
          ELSE IF ((cd_gridtype .EQ. 'mer') .OR. (cd_gridtype .EQ. 'MER')) THEN
 
             il_nbx = (rd_lonmax-rd_lonmin)/rl_dx + 1
 
 
             !all grid points latitude calculation
             il_ji=1
             rla_northlat(1)=0.
             DO WHILE (rla_northlat(il_ji) < rl_northlimit)
                il_ji=il_ji+1
                rla_northlat(il_ji) = ASIN(TANH((0. + (il_ji-1)*rl_dy)*rl_pi/180.))* &
                                     180./rl_pi
                il_nbnorth=il_ji-1
             ENDDO
 
             il_ji=1
             rla_southlat(1)=0.
             DO WHILE (rla_southlat(il_ji) > rl_southlimit)
                il_ji=il_ji+1
                rla_southlat(il_ji) = ASIN(TANH((0. - (il_ji-1)*rl_dy)*rl_pi/180.))* &
                                    180./rl_pi
                il_nbsouth=il_ji-1
             ENDDO
 
 
             !North pole area
             IF (rd_latmin >= 0.) THEN
 
                ! mod b11
                ! look for min indices of rla_northlat
                il_ji = 1
                DO WHILE (il_ji < il_nbnorth .AND. rla_northlat(il_ji) < rd_latmin )
                   il_ji=il_ji+1
                END DO
                il_jmin=il_ji
                ! look for max indices of rla_northlat
                IF ( rd_latmax >= rla_northlat(il_nbnorth) ) THEN
                   il_jmax=il_nbnorth
                ELSE
                   il_ji=il_jmin
                   DO WHILE (il_ji <= il_nbnorth .AND. rla_northlat(il_ji) < rd_latmax )
                      il_ji=il_ji+1
                   END DO
                   il_jmax=il_ji-1
                ENDIF
                ! end mod b11
 
                il_nby=il_jmax-il_jmin+1
 
 
                ALLOCATE(rdpa_longitude(il_nbx, il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_longitude',cl_fonction)
 
                ALLOCATE(rdpa_latitude(il_nbx, il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_latitude',cl_fonction)
 
                DO il_ji=1,il_nbx
                   rdpa_longitude(il_ji, 1:il_nby) = rd_lonmin + (il_ji-1)*rl_dx
                   rdpa_latitude(il_ji, 1:il_nby) = rla_northlat(il_jmin:il_jmax)
                ENDDO
 
 
             ! area on both south and north poles
             ELSE IF ((rd_latmin < 0.) .AND. (rd_latmax > 0.)) THEN
 
                DO il_ji=1,il_nbsouth
                   IF (rla_southlat(il_ji) >= rd_latmin) il_jmin=il_ji
                ENDDO
                DO il_ji=1,il_nbnorth
                   IF (rla_northlat(il_ji) <= rd_latmax) il_jmax=il_ji
                ENDDO
 
                il_nby=il_jmin+il_jmax-1
 
                ALLOCATE(rdpa_longitude(il_nbx, il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_longitude',cl_fonction)
 
                ALLOCATE(rdpa_latitude(il_nbx, il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_latitude',cl_fonction)
 
 
                DO il_ji=1,il_nbx
                   DO il_jj=1,il_jmin-1
                      rdpa_latitude(il_ji, il_jj) = rla_southlat(il_jmin-il_jj+1)
                   ENDDO
                   DO il_jj=il_jmin,il_nby
                      rdpa_latitude(il_ji, il_jj) = rla_northlat(il_jj-il_jmin+1)
                   ENDDO
                   rdpa_longitude(il_ji, 1:il_nby) = rd_lonmin + (il_ji-1)*rl_dx
                ENDDO
 
             ! south pole area
             ELSE IF ((rd_latmin < 0.) .AND. (rd_latmax <=0.)) THEN
 
                ! mod b11
                il_ji = 1
                DO WHILE (il_ji < il_nbsouth .AND. rla_southlat(il_ji) > rd_latmax )
                   il_ji=il_ji+1
                END DO
                il_jmax=il_ji
                ! look for min indices of rla_southlat
                IF ( rd_latmin <= rla_southlat(il_nbsouth) ) THEN
                   il_jmin=il_nbsouth
                ELSE
                   il_ji=il_jmax
                   DO WHILE (il_ji <= il_nbsouth .AND. rla_southlat(il_ji) > rd_latmin )
                      il_ji=il_ji+1
                   END DO
                   il_jmin=il_ji-1
                ENDIF
                ! end mod b11
 
                il_nby=ABS(il_jmax-il_jmin)+1
 
                ALLOCATE(rdpa_longitude(il_nbx, il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_longitude',cl_fonction)
 
                ALLOCATE(rdpa_latitude(il_nbx, il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_latitude',cl_fonction)
 
                DO il_ji=1,il_nbx
                   rdpa_longitude(il_ji, 1:il_nby) = rd_lonmin + (il_ji-1)*rl_dx
                   DO il_jj=1,il_nby
                      rdpa_latitude(il_ji,il_ji) = rla_southlat(il_jmin-il_jj+1)
                   ENDDO
                ENDDO
 
             ENDIF
 
             id_nblon = il_nbx
             id_nblat = il_nby
 
          ENDIF
 
        ENDSUBROUTINE MCAL_gen_reggrid_gtype2D
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
 
           !!=====================================================================
           !> \brief
           !! Description: This function interpolates 2D data values on a 2D grid.
           !!              Grid type 1D: longitude(longitude), latitude (latitude)
           !!              Grid type 2D: longitude(x,y), latitude(x,y)
           !!              The 4 nearest neightbors mean interpolation method is
           !!              used.
           !!
           !!  @param ida_values        2D array of values to be interpolated.
           !!  @param rda_lonvalues     Vector of the original longitude values.
           !!  @param rda_latvalues     Vector of the original latitude values.
           !!  @param id_nblonvalues    Length of the original longitude vector.
           !!  @param id_nblatvalues    Length of the original latitude vector.
           !!  @param rda_longrid       2D array of the interpolated longitude values.
           !!  @param rda_latgrid       2D array of the interpolated latitude values.
           !!  @param id_nblongrid      Length of the interpolated longitude vector.
           !!  @param id_nblatgrid      Length of the interpolated latitude vector.
           !!  @param rda_result        2D array of interpolated values.
           !!  @param ld_ismask         If the data represents a mask ([0,1]), "ismask"
           !!                      value equals true.(Optional)
           !!  @param id_fillvalue      Specify the fille value.(Optional)
           !!
           !! History :
           !!    \n       09/2005 (G. Vinay) Original
           !!    \n       10/2007 (F. Messal) F90
           !!    \n       10/2008 (C.REGNIER) add MFT_error 
           !!    \n       10/2009 (C.REGNIER) Modif Nec changement de
           !!                                 nom de routine trop long (> 31 char)
           !<
           !!=====================================================================

         SUBROUTINE MCAL_int2D_gtype2D_4NNmean_R8 (rda_values, &
                                         rda_lonvalues, &
                                         rda_latvalues, &
                                         id_nblonvalues, &
                                         id_nblatvalues, &
                                         rda_longrid, &
                                         rda_latgrid, &
                                         id_nx, &
                                         id_ny, &
                                         rda_result, &
                                         ld_ismask, &
                                         rd_fillvalue)
 
           USE MFT_error
           IMPLICIT NONE
  
           INTEGER,                                               INTENT( IN) :: id_nblonvalues, id_nblatvalues
           REAL(KIND=8), DIMENSION(id_nblonvalues, id_nblatvalues), INTENT( IN) :: rda_values
           REAL(KIND=4), DIMENSION(id_nblonvalues),               INTENT( IN) :: rda_lonvalues
           REAL(KIND=4), DIMENSION(id_nblatvalues),               INTENT( IN) :: rda_latvalues
           INTEGER,                                               INTENT( IN) :: id_nx, id_ny
           REAL(KIND=4), DIMENSION(id_nx, id_ny),                 INTENT( IN) :: rda_longrid, &
                                                                                 rda_latgrid
           REAL(KIND=8), DIMENSION(id_nx, id_ny),                 INTENT(OUT) :: rda_result
           LOGICAL,                                               OPTIONAL :: ld_ismask
           REAL(KIND=8),                                          OPTIONAL :: rd_fillvalue
 
           CHARACTER(LEN=30) :: cl_fonction
           INTEGER :: il_ji, il_jj, il_jk, il_jl, il_nbval, il_status
           REAL(KIND=8) :: rl_result
           INTEGER, DIMENSION(id_nx, id_ny, 2) :: ila_lonminmax
           INTEGER, DIMENSION(id_nx, id_ny, 2) :: ila_latminmax
           REAL(KIND=8), DIMENSION(4) :: rla_points
           REAL(KIND=4) :: rl_value
           LOGICAL :: ll_flag

           cl_fonction="MCAL_interp2D_gtype2D_4NNmean_R8"
 
           !----------------------------------------------------------------------
           ! Test array
           il_status = fi_arrError(MINVAL(rda_lonvalues), &
                                   MAXVAL(rda_lonvalues), &
                                   'rda_lonvalues',cl_fonction)
 
           il_status = fi_arrError(MINVAL(rda_latvalues), &
                                   MAXVAL(rda_latvalues), &
                                   'rda_latvalues',cl_fonction)
 
           il_status = fi_arrError(MINVAL(rda_longrid), &
                                   MAXVAL(rda_longrid), &
                                   'rda_longrid',cl_fonction)
 
           il_status = fi_arrError(MINVAL(rda_latgrid), &
                                   MAXVAL(rda_latgrid), &
                                   'rda_latgrid',cl_fonction)
 
           !----------------------------------------------------------------------
           ! find minimum i and j indices for longitudes
 
           DO il_ji=1, id_nx
              DO il_jj=1, id_ny
 
                 IF(rda_longrid(il_ji, il_jj) .GT. 180.0) THEN
                    rl_value = rda_longrid(il_ji, il_jj) - 360.0
                 ELSE
                    rl_value = rda_longrid(il_ji, il_jj)
                 ENDIF
 
              IF (rl_value.LT.rda_lonvalues(1)) THEN
                 ila_lonminmax(il_ji, il_jj, 1)=1
                 ila_lonminmax(il_ji, il_jj, 2)=1
 
              ELSE IF (rl_value.GE.rda_lonvalues(id_nblonvalues)) THEN
                    ila_lonminmax(il_ji, il_jj, 1)=id_nblonvalues
                    ila_lonminmax(il_ji, il_jj, 2)=id_nblonvalues
 
              ELSE
 
                 il_jk=1
                 il_jl=il_jk+1
                 ila_lonminmax(il_ji, il_jj, 1)=il_jk
                 ila_lonminmax(il_ji, il_jj, 2)=ila_lonminmax(il_ji, il_jj, 1)+1
 
                 ll_flag = .TRUE.
                 DO WHILE (ll_flag)
                    ila_lonminmax(il_ji, il_jj, 1)=il_jk
                    ila_lonminmax(il_ji, il_jj, 2)=il_jl
 
                    IF (il_jk .EQ. id_nblonvalues) THEN
                       ila_lonminmax(il_ji, il_jj, 2)=il_jk
                    ENDIF
 
                    il_jk=il_jk+1
                    il_jl=il_jk+1
 
                    IF (il_jk .LE. id_nblonvalues) THEN
 
                       IF ((rda_lonvalues(il_jk) .LE. rl_value) .AND. &
                           (rda_lonvalues(il_jl) .GT. rl_value)) THEN
                          ll_flag = .FALSE.
                       ELSE
                          ll_flag = .TRUE.
                       ENDIF
 
                    ELSE
                       ll_flag = .FALSE.
                    ENDIF
 
                 ENDDO
 
              ENDIF
 
              ENDDO
           ENDDO
 
           !----------------------------------------------------------------------
           ! find minimum i and j indices for latitudes
           DO il_ji=1, id_nx
              DO il_jj=1, id_ny
 
              IF (rda_latgrid(il_ji, il_jj) .LT. rda_latvalues(1)) THEN
                 ila_latminmax(il_ji, il_jj, 1)=1
                 ila_latminmax(il_ji, il_jj, 2)=1
 
              ELSE IF (rda_latgrid(il_ji, il_jj) .GE. rda_latvalues(id_nblatvalues)) THEN
                 ila_latminmax(il_ji, il_jj, 1)=id_ny
                 ila_latminmax(il_ji, il_jj, 2)=id_ny
 
              ELSE
                 il_jk=1
                 il_jl=il_jk+1
 
                 ila_latminmax(il_ji, il_jj, 1)=il_jk
                 ila_latminmax(il_ji, il_jj, 2)=ila_latminmax(il_ji, il_jj, 1)+1
 
                 ll_flag = .TRUE.
                 DO WHILE (ll_flag)
                    ila_latminmax(il_ji, il_jj, 1)=il_jk
                    ila_latminmax(il_ji, il_jj, 2)=il_jl
 
                    IF (il_jk.LE.SIZE(rda_latvalues)) THEN
                       ila_latminmax(il_ji, il_jj, 2)=il_jk
                    ENDIF
 
                    il_jk=il_jk+1
                    il_jl=il_jk+1
 
                    IF (il_jk.LE.SIZE(rda_latvalues)) THEN
                       IF ((rda_latvalues(il_jk).LE.rda_latgrid(il_ji, il_jj)) .AND. &
                           (rda_latvalues(il_jl).GT.rda_latgrid(il_ji, il_jj))) THEN
                          ll_flag = .FALSE.
                       ELSE
                          ll_flag = .TRUE.
                       ENDIF
 
                    ELSE
                       ll_flag = .FALSE.
                    ENDIF
 
 
                 ENDDO
 
              ENDIF
 
              ENDDO
           ENDDO
 
           !----------------------------------------------------------------------
           ! compute bathymetry
           DO il_ji=1, id_nx
              DO il_jj=1, id_ny
 
                 rla_points(1) = rda_values(ila_lonminmax(il_ji, il_jj, 1), &
                                            ila_latminmax(il_ji, il_jj, 1))
                 rla_points(2) = rda_values(ila_lonminmax(il_ji, il_jj, 2), &
                                            ila_latminmax(il_ji, il_jj, 1))
                 rla_points(3) = rda_values(ila_lonminmax(il_ji, il_jj, 1), &
                                            ila_latminmax(il_ji, il_jj, 2))
                 rla_points(4) = rda_values(ila_lonminmax(il_ji, il_jj, 2), &
                                            ila_latminmax(il_ji, il_jj, 2))
 
                 il_nbval=4
                 IF (PRESENT(rd_fillvalue)) THEN
                    DO il_jk=1,4
                       IF (rla_points(il_jk).EQ.rd_fillvalue) THEN
                          il_nbval=il_nbval-1
                          rla_points(il_jk)=0
                       ENDIF
                    ENDDO
                 ENDIF
 
                 rl_result=( (rla_points(1)+rla_points(2)+rla_points(3)+ &
                                       rla_points(4)) / il_nbval )
 
                 IF (PRESENT(ld_ismask)) THEN
                    IF(ld_ismask) THEN
                       IF(rl_result .GE. 0.5) THEN
                          rl_result = 1.
                       ELSE
                          rl_result = 0.
                       ENDIF
                    ENDIF
                 ENDIF
 
                 rda_result(il_ji,il_jj) = rl_result
 
              ENDDO
           ENDDO
 
 
         END SUBROUTINE MCAL_int2D_gtype2D_4NNmean_R8
 
 
   !******************************************************************************
   !******************************************************************************
   !******************************************************************************
 
           !!=====================================================================
           !> \brief
           !! Description: This function interpolates 2D data values on a 2D grid.
           !!              The 4 nearest neightbors mean interpolation method is
           !!              used.
           !!
           !!  @param ida_values        2D array of values to be interpolated.
           !!  @param rda_lonvalues     Vector of the original longitude values.
           !!  @param rda_latvalues     Vector of the original latitude values.
           !!  @param id_nblonvalues    Length of the original longitude vector.
           !!  @param id_nblatvalues    Length of the original latitude vector.
           !!  @param rda_longrid       Vector of the interpolated longitude values.
           !!  @param rda_latgrid       Vector of the interpolated latitude values.
           !!  @param id_nblongrid      Length of the interpolated longitude vector.
           !!  @param id_nblatgrid      Length of the interpolated latitude vector.
           !!  @param rda_result        2D array of interpolated values.
           !!  @param ld_ismask         If the data represents a mask ([0,1]), "ismask"
           !!                      value equals true.(Optional)
           !!  @param id_fillvalue      Specify the fille value.(Optional)
           !!
           !! History :
           !!   \n        09/2005 (G. Vinay) Original
           !!   \n        08/2006 (F. Messal) F90
           !!   \n        11/2006 (F. Messal) CVS version 1.0
           !!   \n        10/2008 (C.REGNIER) add MFT_error 
           !!   \n        10/2009 (C.REGNIER) Modif Nec changement de
           !!                                 nom de routine trop long (> 31 char)
           !<
           !!=====================================================================
         
           SUBROUTINE MCAL_int2D_gtype2D_4NNmean_I (ida_values, &
                                         rda_lonvalues, &
                                         rda_latvalues, &
                                         id_nblonvalues, &
                                         id_nblatvalues, &
                                         rda_longrid, &
                                         rda_latgrid, &
                                         id_nx, &
                                         id_ny, &
                                         ida_result, &
                                         ld_ismask, &
                                         id_fillvalue)
 
           USE MFT_error
           IMPLICIT NONE
 
           INTEGER,                                               INTENT( IN) :: id_nblonvalues, id_nblatvalues
           INTEGER, DIMENSION(id_nblonvalues, id_nblatvalues), INTENT( IN) :: ida_values
           REAL(KIND=4), DIMENSION(id_nblonvalues),               INTENT( IN) :: rda_lonvalues
           REAL(KIND=4), DIMENSION(id_nblatvalues),               INTENT( IN) :: rda_latvalues
           INTEGER,                                               INTENT( IN) :: id_nx, id_ny
           REAL(KIND=4), DIMENSION(id_nx, id_ny),                 INTENT( IN) :: rda_longrid, &
                                                                                 rda_latgrid
           INTEGER, DIMENSION(id_nx, id_ny),                      INTENT(OUT) :: ida_result
           INTEGER,                                               OPTIONAL :: id_fillvalue
           LOGICAL,                                               OPTIONAL :: ld_ismask
 
           CHARACTER(LEN=30) :: cl_fonction
           INTEGER :: il_ji, il_jj, il_jk, il_jl, il_nbval, il_status
           REAL(KIND=4) :: rl_result
           INTEGER, DIMENSION(id_nx, id_ny, 2) :: ila_lonminmax
           INTEGER, DIMENSION(id_nx, id_ny, 2) :: ila_latminmax
           INTEGER, DIMENSION(4) :: ila_points
           REAL(KIND=4) :: rl_value
           LOGICAL :: ll_flag
 
           cl_fonction="MCAL_interp2D_gtype2D_4NNmean_I"
           !----------------------------------------------------------------------
           ! Test array
           il_status = fi_arrError(MINVAL(rda_lonvalues), &
                                   MAXVAL(rda_lonvalues), &
                                   'rda_lonvalues',cl_fonction)
 
           il_status = fi_arrError(MINVAL(rda_latvalues), &
                                   MAXVAL(rda_latvalues), &
                                   'rda_latvalues',cl_fonction)
 
           il_status = fi_arrError(MINVAL(rda_longrid), &
                                   MAXVAL(rda_longrid), &
                                   'rda_longrid',cl_fonction)
 
           il_status = fi_arrError(MINVAL(rda_latgrid), &
                                   MAXVAL(rda_latgrid), &
                                   'rda_latgrid',cl_fonction)
 
           il_status = fi_arrError(REAL(MINVAL(ida_values),4), &
                                   REAL(MAXVAL(ida_values),4), &
                                   'rda_values',cl_fonction)
 
           !----------------------------------------------------------------------
           ! find minimum i and j indices for longitudes
 
           DO il_ji=1, id_nx
              DO il_jj=1, id_ny
 
                 IF(rda_longrid(il_ji, il_jj) .GT. 180.0) THEN
                    rl_value = rda_longrid(il_ji, il_jj) - 360.0
                 ELSE
                    rl_value = rda_longrid(il_ji, il_jj)
                 ENDIF
 
              IF (rl_value.LT.rda_lonvalues(1)) THEN
                 ila_lonminmax(il_ji, il_jj, 1)=1
                 ila_lonminmax(il_ji, il_jj, 2)=1
 
              ELSE IF (rl_value.GE.rda_lonvalues(id_nblonvalues)) THEN
                    ila_lonminmax(il_ji, il_jj, 1)=id_nblonvalues
                    ila_lonminmax(il_ji, il_jj, 2)=id_nblonvalues
 
              ELSE
 
                 il_jk=1
                 il_jl=il_jk+1
                 ila_lonminmax(il_ji, il_jj, 1)=il_jk
                 ila_lonminmax(il_ji, il_jj, 2)=ila_lonminmax(il_ji, il_jj, 1)+1
 
                 ll_flag = .TRUE.
                 DO WHILE (ll_flag)
                    ila_lonminmax(il_ji, il_jj, 1)=il_jk
                    ila_lonminmax(il_ji, il_jj, 2)=il_jl
 
                    IF (il_jk .EQ. id_nblonvalues) THEN
                       ila_lonminmax(il_ji, il_jj, 2)=il_jk
                    ENDIF
 
                    il_jk=il_jk+1
                    il_jl=il_jk+1
 
                    IF (il_jk .LE. id_nblonvalues) THEN
 
                       IF ((rda_lonvalues(il_jk) .LE. rl_value) .AND. &
                           (rda_lonvalues(il_jl) .GT. rl_value)) THEN
                          ll_flag = .FALSE.
                       ELSE
                          ll_flag = .TRUE.
                       ENDIF
 
                    ELSE
                       ll_flag = .FALSE.
                    ENDIF
 
                 ENDDO
 
              ENDIF
 
              ENDDO
           ENDDO
 
           !----------------------------------------------------------------------
           ! find minimum i and j indices for latitudes
 
           DO il_ji=1, id_nx
              DO il_jj=1, id_ny
 
              IF (rda_latgrid(il_ji, il_jj) .LT. rda_latvalues(1)) THEN
                 ila_latminmax(il_ji, il_jj, 1)=1
                 ila_latminmax(il_ji, il_jj, 2)=1
 
              ELSE IF (rda_latgrid(il_ji, il_jj) .GE. rda_latvalues(id_nblatvalues)) THEN
                 ila_latminmax(il_ji, il_jj, 1)=id_ny
                 ila_latminmax(il_ji, il_jj, 2)=id_ny
 
              ELSE
                 il_jk=1
                 il_jl=il_jk+1
 
                 ila_latminmax(il_ji, il_jj, 1)=il_jk
                 ila_latminmax(il_ji, il_jj, 2)=ila_latminmax(il_ji, il_jj, 1)+1
 
                 ll_flag = .TRUE.
                 DO WHILE (ll_flag)
                    ila_latminmax(il_ji, il_jj, 1)=il_jk
                    ila_latminmax(il_ji, il_jj, 2)=il_jl
 
                    IF (il_jk.LE.SIZE(rda_latvalues)) THEN
                       ila_latminmax(il_ji, il_jj, 2)=il_jk
                    ENDIF
 
                    il_jk=il_jk+1
                    il_jl=il_jk+1
 
                    IF (il_jk.LE.SIZE(rda_latvalues)) THEN
                       IF ((rda_latvalues(il_jk).LE.rda_latgrid(il_ji, il_jj)) .AND. &
                           (rda_latvalues(il_jl).GT.rda_latgrid(il_ji, il_jj))) THEN
                          ll_flag = .FALSE.
                       ELSE
                          ll_flag = .TRUE.
                       ENDIF
 
                    ELSE
                       ll_flag = .FALSE.
                    ENDIF
 
 
                 ENDDO
 
              ENDIF
 
              ENDDO
           ENDDO
 
           !----------------------------------------------------------------------
           ! compute bathymetry
           DO il_ji=1, id_nx
              DO il_jj=1, id_ny
 
                 ila_points(1) = ida_values(ila_lonminmax(il_ji, il_jj, 1), &
                                            ila_latminmax(il_ji, il_jj, 1))
                 ila_points(2) = ida_values(ila_lonminmax(il_ji, il_jj, 2), &
                                            ila_latminmax(il_ji, il_jj, 1))
                 ila_points(3) = ida_values(ila_lonminmax(il_ji, il_jj, 1), &
                                            ila_latminmax(il_ji, il_jj, 2))
                 ila_points(4) = ida_values(ila_lonminmax(il_ji, il_jj, 2), &
                                            ila_latminmax(il_ji, il_jj, 2))
 
 
                 il_nbval=4
                 IF (PRESENT(id_fillvalue)) THEN
                    DO il_jk=1,4
                       IF (ila_points(il_jk).EQ.id_fillvalue) THEN
                          il_nbval=il_nbval-1
                          ila_points(il_jk)=0
                       ENDIF
                    ENDDO
                 ENDIF
 
                 rl_result=( (ila_points(1)+ila_points(2)+ila_points(3)+ &
                                       ila_points(4)) / il_nbval )
 
 
                 ida_result(il_ji,il_jj) = INT(rl_result)
 
              ENDDO
           ENDDO
 
         END SUBROUTINE MCAL_int2D_gtype2D_4NNmean_I
 
 
   !******************************************************************************
   !******************************************************************************
   !******************************************************************************
 
          !!=====================================================================
          !> \brief
          !! Description: This function creates 2D grid and returns longitude an
          !!              latitude values array : longitude(x,y).
          !!
          !!  @param id_nx           The number of longitude points.
          !!  @param id_ny           The number of latitude points.
          !!  @param rd_psclon       The polar stereographic central longitude (-45.0)
          !!  @param rd_psclat       The polar stereographic central latitude (90.0)
          !!  @param rd_proj         The proj_conv_fac (0.08)
          !!  @param rdpa_longitude  2D array of longitude values.
          !!  @param rdpa_latitude   2D array of latitude values.
          !!  @param rdpa_longitude  2D array of longitude in km.
          !!  @param rdpa_latitude   2D array of latitude in km.
          !!
          !! History :
          !!    \n       Original    : 09/2007 (F. Messal)
          !!    \n       Modif       : 06/2008 C.REGNIER Add comments
          !!    \n       Modif       : 10/2008 C.REGNIER add MFT_error 
          !!    \n       Modif       : 10/2009 C.REGNIER Modif Nec changement de
          !!                            nom de nom de routine trop long (> 31 char)         
          !< 
          !!=====================================================================
         
        SUBROUTINE MCAL_gen_strgrid_gtype2D(id_nx, &
                                                    id_ny, &
                                                    rd_psclon, &
                                                    rd_psclat, &
                                                    rd_proj, &
                                                    rdpa_lonValue_2D, &
                                                    rdpa_latValue_2D,&
                                                    rdpa_lonValuekm_2D,&
                                                    rdpa_latValuekm_2D )
 
          USE MFT_error
          IMPLICIT NONE
 
        !----------------------------------------------------------------------
 
        INTEGER :: id_nx, &
                   id_ny
        REAL(KIND=4) :: rd_psclon, & ! polar stereographic central longitude (-45.0)
                        rd_psclat, & ! polar stereographic central latitude (90.0 or -90.0)
                        rd_proj   ! proj_conv_fac (0.08)
 
 
        INTEGER :: il_ji, &
                   il_jj, &
                   id_nxcenter, &
                   id_nycenter, &
                   il_status
        REAL(KIND=4) :: rl_rho, &
                        rl_c
        REAL(KIND=8), DIMENSION(:,:), POINTER :: rdpa_lonValue_2D, &
                                                 rdpa_latValue_2D

           REAL(KIND=4), DIMENSION(:,:), POINTER ::rdpa_lonValuekm_2D,&
                                                   rdpa_latValuekm_2D
        REAL(KIND=4), PARAMETER :: rl_rade = 57.29577951, & ! radian to degree conv factor
                                   rl_re = 6378.273 ! radius of earth
        REAL(KIND=4) :: rl_x, rl_y
        CHARACTER(LEN=255) ::     cl_fonction

        cl_fonction="MCAL_generate_stereogrid_gtype2D"

        IF ( rd_psclat /= 90.0 .AND. rd_psclat /= -90.0 )  THEN
          PRINT * ,'rd_psclat in only equal to 90 or -90' ,rd_psclat
          STOP
        ENDIF
        !----------------------------------------------------------------------
        !* Define latitude array
        ALLOCATE(rdpa_lonValue_2D(id_nx, id_ny), &
                 stat=il_status)
        il_status = fi_memError(il_status, ' rdpa_longitude',cl_fonction)

        ALLOCATE(rdpa_latValue_2D(id_nx, id_ny), &
                 stat=il_status)
        il_status = fi_memError(il_status, ' rdpa_latitude',cl_fonction)
  
        ALLOCATE(rdpa_lonValuekm_2D(id_nx, id_ny), &
                 stat=il_status)
        il_status = fi_memError(il_status, ' rdpa_longitudekm',cl_fonction)

        ALLOCATE(rdpa_latValuekm_2D(id_nx, id_ny), &
                 stat=il_status)
        il_status = fi_memError(il_status, ' rdpa_latitudekm',cl_fonction)
  

        id_nxcenter = id_nx/2
        id_nycenter = id_ny/2
 
        WRITE(0,*) ' Grille Stereopolaire '
 
        DO il_ji=1, id_nx
           DO il_jj=1, id_ny
            
              !* compute the X and Y coordinates on the regular stereopolar grid
              rl_x = (il_ji-id_nxcenter)/rd_proj
              rl_y = (il_jj-id_nycenter)/rd_proj
              rdpa_lonValuekm_2D(il_ji, il_jj)=rl_x
              rdpa_latValuekm_2D(il_ji, il_jj)=rl_y
              !* compute the distance to the center of the grid
              rl_rho =SQRT(REAL(rl_x*rl_x+rl_y*rl_y,4))
              rl_c = 2*ATAN(rl_rho/(2*rl_re))
              !* compute the latitude
              IF ( rd_psclat == 90) THEN
                rdpa_latValue_2D(il_ji, il_jj) = rl_rade*ASIN(COS(rl_c))
              ELSE 
                rdpa_latValue_2D(il_ji, il_jj) = rl_rade*ASIN(-COS(rl_c)) 
              ENDIF
              !* compute the longitude
              IF (il_jj .EQ. id_nycenter) THEN
                 IF (il_ji .LE. id_nxcenter) THEN
                     IF ( rd_psclat == 90) THEN 
                       rdpa_lonValue_2D(il_ji, il_jj) = rd_psclon - 90
                     ELSE
                       rdpa_lonValue_2D(il_ji, il_jj) = rd_psclon + 90 
                     ENDIF
                 ELSE
                    IF ( rd_psclat == 90) THEN
                       rdpa_lonValue_2D(il_ji, il_jj) = rd_psclon + 90
                    ELSE
                       rdpa_lonValue_2D(il_ji, il_jj) = rd_psclon - 90
                    ENDIF
                 ENDIF
              ELSE
                 IF (il_jj .LE. id_nycenter) THEN
                   IF ( rd_psclat == 90) THEN
                    rdpa_lonValue_2D(il_ji, il_jj) = rd_psclon + &
                       rl_rade*ATAN(-(real(il_ji-id_nxcenter,8)) / (real(il_jj-id_nycenter,8)))
                   ELSE
                    rdpa_lonValue_2D(il_ji, il_jj) = rd_psclon + &
                       rl_rade*ATAN((real(il_ji-id_nxcenter,8)) / (real(il_jj-id_nycenter,8)))
                   ENDIF
                 ELSE
                    IF (il_ji .GT. id_nxcenter) THEN
                      IF ( rd_psclat == 90) THEN
                         rdpa_lonValue_2D(il_ji, il_jj) = rd_psclon + &
                         rl_rade*ATAN(-(REAL(il_ji-id_nxcenter,8)) / (REAL(il_jj-id_nycenter,8))) + 180
                      ELSE
                         rdpa_lonValue_2D(il_ji, il_jj) = rd_psclon + &
                         rl_rade*ATAN((REAL(il_ji-id_nxcenter,8)) / (REAL(il_jj-id_nycenter,8))) + 180
                      ENDIF
                    ELSE
                       IF ( rd_psclat == 90) THEN
                          rdpa_lonValue_2D(il_ji, il_jj) = rd_psclon + &
                          rl_rade*ATAN(-(REAL(il_ji-id_nxcenter,8)) / (REAL(il_jj-id_nycenter,8))) - 180
                       ELSE
                          rdpa_lonValue_2D(il_ji, il_jj) = rd_psclon + &
                          rl_rade*ATAN((REAL(il_ji-id_nxcenter,8)) / (REAL(il_jj-id_nycenter,8))) - 180
                       ENDIF 
                       IF (rdpa_lonValue_2D(il_ji, il_jj) .LT. -180.0 ) THEN
                          rdpa_lonValue_2D(il_ji, il_jj) = 360 + rdpa_lonValue_2D(il_ji, il_jj)
                       ENDIF
                    ENDIF
                 ENDIF
              ENDIF
 
 
           ENDDO
        ENDDO
 
      ENDSUBROUTINE MCAL_gen_strgrid_gtype2D
 
 
 !******************************************************************************
 !******************************************************************************
 !******************************************************************************
          
          !!=====================================================================
          !> \brief
          !! Description: This function interpolates 2D data values on a 2D grid.
          !!              Grid type 1D: longitude(longitude), latitude (latitude)
          !!              Grid type 2D: longitude(x,y), latitude(x,y)
          !!              The 4 nearest neightbors mean interpolation method is
          !!              used.
          !!
          !!  @param ida_values        2D array of values to be interpolated.
          !!  @param rda_lonvalues     Vector of the original longitude values.
          !!  @param rda_latvalues     Vector of the original latitude values.
          !!  @param id_nblonvalues    Length of the original longitude vector.
          !!  @param id_nblatvalues    Length of the original latitude vector.
          !!  @param rda_longrid       Vector of the interpolated longitude values.
          !!  @param rda_latgrid       Vector of the interpolated latitude values.
          !!  @param id_nblongrid      Length of the interpolated longitude vector.
          !!  @param id_nblatgrid      Length of the interpolated latitude vector.
          !!  @param ida_result        2D array of interpolated values.
          !!  @param ld_ismask         If the data represents a mask ([0,1]), "ismask"
          !!                      value equals true.(Optional)
          !!  @param id_fillvalue      Specify the fille value.(Optional)
          !!
          !! History :
          !!    \n       09/2005 (G. Vinay) Original
          !!    \n       08/2006 (F. Messal) F90
          !!    \n       11/2006 (F. Messal) CVS version 1.0
          !!    \n       10/2008 (C.REGNIER) add MFT_error 
          !!    \n       10/2009 (C.REGNIER) Modif Nec changement de
          !!                                 nom de routine trop long (> 31 char)
          !<
          !!=====================================================================
  
        SUBROUTINE MCAL_int2D_gtype1D_4NNmean_I (ida_values, &
                                             rda_lonvalues, &
                                             rda_latvalues, &
                                             id_nblonvalues, &
                                             id_nblatvalues, &
                                             rda_longrid, &
                                             rda_latgrid, &
                                             id_nblongrid, &
                                             id_nblatgrid, &
                                             ida_result, &
                                             ld_ismask, &
                                             id_fillvalue)
 
          USE MFT_error
          IMPLICIT NONE
 
          INTEGER,                                            INTENT(IN) :: id_nblonvalues, id_nblatvalues
          INTEGER, DIMENSION(id_nblonvalues, id_nblatvalues), INTENT(IN) :: ida_values
          REAL(KIND=4), DIMENSION(id_nblonvalues),            INTENT(IN) :: rda_lonvalues
          REAL(KIND=4), DIMENSION(id_nblatvalues),            INTENT(IN) :: rda_latvalues
          INTEGER,                                            INTENT(IN) :: id_nblongrid, id_nblatgrid
          REAL(KIND=4), DIMENSION(id_nblongrid),              INTENT(IN) :: rda_longrid
          REAL(KIND=4), DIMENSION(id_nblatgrid),              INTENT(IN) :: rda_latgrid
          INTEGER, DIMENSION(id_nblongrid, id_nblatgrid),     INTENT(OUT):: ida_result
          LOGICAL,                                            OPTIONAL :: ld_ismask
          INTEGER,                                            OPTIONAL :: id_fillvalue
 
          CHARACTER(LEN=30) :: cl_fonction
          INTEGER :: il_ji, il_jj, il_jk, il_nbval, il_status
          REAL(KIND=4) :: rl_result, rl_value
          INTEGER, DIMENSION(id_nblongrid, 2) :: ila_lonminmax
          INTEGER, DIMENSION(id_nblatgrid, 2) :: ila_latminmax
          INTEGER, DIMENSION(4) :: ila_points
          LOGICAL :: ll_flag,ll_fin
          
          cl_fonction="MCAL_interp2D_gtype1D_4NNmean_I"

          !----------------------------------------------------------------------
          ! Test array
          il_status = fi_arrError(MINVAL(rda_lonvalues), &
                                  MAXVAL(rda_lonvalues), &
                                  'rda_lonvalues',cl_fonction)
 
          il_status = fi_arrError(MINVAL(rda_latvalues), &
                                  MAXVAL(rda_latvalues), &
                                  'rda_latvalues',cl_fonction)
 
          il_status = fi_arrError(MINVAL(rda_longrid), &
                                  MAXVAL(rda_longrid), &
                                  'rda_longrid',cl_fonction)
 
          il_status = fi_arrError(MINVAL(rda_latgrid), &
                                  MAXVAL(rda_latgrid), &
                                  'rda_latgrid',cl_fonction)
 
          il_status = fi_arrError(REAL(MINVAL(ida_values),4), &
                                  REAL(MAXVAL(ida_values),4), &
                                  'ida_values',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Find minimum i and j indices for longitudes
 
 
          DO il_ji=1, id_nblongrid
 
             IF(rda_longrid(il_ji) .GT. 180.0) THEN
                rl_value = rda_longrid(il_ji) - 360.0
             ELSE
                rl_value = rda_longrid(il_ji)
             ENDIF
 
             iF (rl_value.LT.rda_lonvalues(1)) THEN
                ila_lonminmax(il_ji,1)=1
                ila_lonminmax(il_ji,2)=1
 
             ELSE IF (rl_value.GE.rda_lonvalues(id_nblonvalues)) THEN
                ila_lonminmax(il_ji,1)=id_nblonvalues
                ila_lonminmax(il_ji,2)=id_nblonvalues
 
             ELSE
 
                il_jj=1
                il_jk=il_jj+1
                ila_lonminmax(il_ji,1)=il_jj
                ila_lonminmax(il_ji,2)=ila_lonminmax(il_ji,1)+1
 
                ll_flag = .TRUE.
                DO WHILE (ll_flag)
                   ila_lonminmax(il_ji,1)=il_jj
                   ila_lonminmax(il_ji,2)=il_jk
 
                   IF (il_jj.EQ.id_nblonvalues) THEN
                      ila_lonminmax(il_ji,2)=il_jj
                   ENDIF
 
                   il_jj=il_jj+1
                   il_jk=il_jj+1
 
                   IF (il_jj.LE.id_nblonvalues) THEN
 
                      IF ((rda_lonvalues(il_jj).LE.rl_value) .AND. &
                          (rda_lonvalues(il_jk).GT.rl_value)) THEN
                         ll_flag = .FALSE.
                      ELSE
                         ll_flag = .TRUE.
                      ENDIF
                   ELSE
                      ll_flag = .FALSE.
                   ENDIF
 
                ENDDO
 
             ENDIF
 
          ENDDO
 
 
          !----------------------------------------------------------------------
          ! Find minimum i and j indices for latitudes
 
          DO il_ji=1, id_nblatgrid
             IF (rda_latgrid(il_ji).LT.rda_latvalues(1)) THEN
                ila_latminmax(il_ji,1)=1
                ila_latminmax(il_ji,2)=1
 
             ELSE IF (rda_latgrid(il_ji).GT.rda_latvalues(id_nblatvalues)) THEN
                ila_latminmax(il_ji,1)=id_nblatgrid
                ila_latminmax(il_ji,2)=id_nblatgrid
 
             ELSE
                il_jj=1
                IF (il_jj.LE.id_nblatvalues-1) THEN
                   DO WHILE (.NOT.((rda_latvalues(il_jj).LE.rda_latgrid(il_ji)) .AND. &
                                   (rda_latvalues(il_jj+1).GT.rda_latgrid(il_ji))))
                      il_jj=il_jj+1
                    IF ( il_jj.EQ.id_nblatvalues) THEN 
                        ll_fin=.TRUE.
                       exit
                    ENDIF
                   ENDDO
                ENDIF
                IF (ll_fin) THEN
                  ila_latminmax(il_ji,1)=il_jj
                  ila_latminmax(il_ji,2)=ila_latminmax(il_ji,1)
                ELSE
                  ila_latminmax(il_ji,1)=il_jj
                  ila_latminmax(il_ji,2)=ila_latminmax(il_ji,1)+1
                ENDIF
             ENDIF
 
          ENDDO
 
 
          !----------------------------------------------------------------------
          ! Compute mean
 
          DO il_ji=1, id_nblongrid
             DO il_jj=1, id_nblatgrid
 
                ila_points(1) = ida_values(ila_lonminmax(il_ji,1), &
                                           ila_latminmax(il_jj,1))
                ila_points(2) = ida_values(ila_lonminmax(il_ji,2), &
                                           ila_latminmax(il_jj,1))
                ila_points(3) = ida_values(ila_lonminmax(il_ji,1), &
                                           ila_latminmax(il_jj,2))
                ila_points(4) = ida_values(ila_lonminmax(il_ji,2), &
                                           ila_latminmax(il_jj,2))
 
                il_nbval=4
                IF (PRESENT(id_fillvalue)) THEN
                   DO il_jk=1,4
                      IF (ila_points(il_jk).EQ.id_fillvalue) THEN
                         il_nbval=il_nbval-1
                         ila_points(il_jk)=0
                      ENDIF
                   ENDDO
                ENDIF
 
                rl_result=( (ila_points(1)+ila_points(2)+ila_points(3)+ &
                                      ila_points(4)) / il_nbval )
 
                IF (PRESENT(ld_ismask)) THEN
                   IF(ld_ismask) THEN
                      IF(rl_result .GE. 0.5) THEN
                         rl_result = 1.
                      ELSE
                         rl_result = 0.
                      ENDIF
                   ENDIF
                ENDIF
 
                ida_result(il_ji,il_jj)= INT(rl_result)
 
             ENDDO
          ENDDO
      
        ENDSUBROUTINE MCAL_int2D_gtype1D_4NNmean_I
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
          !!=====================================================================
          !> \brief
          !! Description: This function creates 2D grid and returns longitude an
          !!              latitude values array (1D : longitude(longitude) or
          !!              2D: longitude(x,y)). The user choose the kind of grid:
          !!              regular or mercator projection.
          !!
          !!  @param rd_lonmin       The minimum longitude value of the grid.
          !!  @param rd_lonmax       The maximum longitude value of the grid.
          !!  @param rd_lonres       The resolution value of the longitude axis.
          !!                    (Ex: for 1/4° resolution, value is 4)
          !!  @param rd_latmin       The minimum latitude value of the grid.
          !!  @param rd_latmax       The maximum latitude value of the grid.
          !!  @param rd_latres       The resolution value of the latitude axis.
          !!                    (Ex: for 1/4° resolution, value is 4)
          !!  @param cd_gridtype     The type of the grid (latitude axis): 'DEG' for
          !!                    regular in degrees latitude axis, 'MER' for
          !!                    mercator projection latitude axis.
          !!  @param rdpa_longitude  Vector or array of longitude values. It depends on
          !!                    the longitude array number of dimension.
          !!  @param rdpa_latitude   Vector or array of latitude values. It depends on
          !!                    the latitude array number of dimension.
          !!  @param id_nblon        Number of longitude values.(Optional)
          !!  @param id_nblat        Number of latitude values.(Optional)
          !!
          !! History :
          !!      \n     Original    : 07/2006 (S. Besnard)
          !!    \n       Modif       : 10/2008 C.REGNIER add MFT_error 
          !<
          !!=====================================================================
  

       SUBROUTINE MCAL_generate_canevas_2Ddim (rd_lonmin, &
                                                rd_lonmax, &
                                                rd_xresolution, &
                                                rd_latmin, &
                                                rd_latmax, &
                                                rd_yresolution, &
                                                cd_gridtype,&
                                                rdpa_longitude, &
                                                rdpa_latitude, &
                                               id_nblon, &
                                               id_nblat)
 
         USE MFT_error
         IMPLICIT NONE
 
         !----------------------------------------------------------------------
 
          CHARACTER(LEN=3), INTENT(IN) :: cd_gridtype
          REAL(KIND=4), INTENT(IN) :: rd_lonmin, rd_lonmax, rd_latmin, rd_latmax, &
                                      rd_xresolution, rd_yresolution
          REAL(KIND=4), POINTER, DIMENSION(:,:) :: rdpa_longitude, rdpa_latitude
          INTEGER, OPTIONAL :: id_nblon, id_nblat
 
          INTEGER :: il_nbx, il_nby, il_ji, il_jj, il_nbsouth, il_nbnorth, il_status, &
                     il_jmin, il_jmax
          REAL(KIND=4) :: rl_dx, rl_dy
          REAL(KIND=4), DIMENSION(3000) :: rla_northlat, rla_southlat
          REAL(KIND=4), PARAMETER :: rl_northlimit=89., rl_southlimit=-89.
          REAL(KIND=8) :: rl_pi
          CHARACTER(LEN=255) :: cl_fonction
 
          cl_fonction="MCAL_generate_canevas_2Ddim"
          !----------------------------------------------------------------------
          rl_pi = 4.*ATAN(1.)
 
          !----------------------------------------------------------------------
          ! verify
          il_status = fi_arrError(rd_latmin, rd_latmax, ' bounds.',cl_fonction)
          il_status = fi_arrError(rd_lonmin, rd_lonmax, ' bounds.',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! resolutions
          rl_dx = 1./rd_xresolution
          rl_dy = 1./rd_yresolution
 
 
          !----------------------------------------------------------------------
          ! compute values
          IF ((cd_gridtype .EQ. 'deg') .OR. (cd_gridtype .EQ. 'DEG')) THEN
 
             il_nbx = (rd_lonmax-rd_lonmin)/rl_dx + 1
             il_nby = (rd_latmax-rd_latmin)/rl_dy + 1
 
             ALLOCATE(rdpa_longitude(il_nbx, il_nby), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_longitude',cl_fonction)
 
             ALLOCATE(rdpa_latitude(il_nbx, il_nby), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_latitude',cl_fonction)
 
             DO il_ji=1, il_nbx
                DO il_jj=1, il_nby
                   rdpa_longitude(il_ji, il_jj) = rd_lonmin + (il_ji-1)*rl_dx
                   rdpa_latitude(il_ji, il_jj)  = rd_latmin + (il_jj-1)*rl_dy
                ENDDO
             ENDDO
 
             id_nblon = il_nbx
             id_nblat = il_nby
 
          ELSE IF ((cd_gridtype .EQ. 'mer') .OR. (cd_gridtype .EQ. 'MER')) THEN
 
             il_nbx = (rd_lonmax-rd_lonmin)/rl_dx + 1
 
 
             !all grid points latitude calculation
             il_ji=1
             rla_northlat(1)=0.
             DO WHILE (rla_northlat(il_ji) < rl_northlimit)
                il_ji=il_ji+1
                rla_northlat(il_ji) = ASIN(TANH((0. + (il_ji-1)*rl_dy)*rl_pi/180.))* &
                                     180./rl_pi
                il_nbnorth=il_ji-1
             ENDDO
 
             il_ji=1
             rla_southlat(1)=0.
             DO WHILE (rla_southlat(il_ji) > rl_southlimit)
                il_ji=il_ji+1
                rla_southlat(il_ji) = ASIN(TANH((0. - (il_ji-1)*rl_dy)*rl_pi/180.))* &
                                    180./rl_pi
                il_nbsouth=il_ji-1
             ENDDO
 
 
             !North pole area
             IF (rd_latmin >= 0.) THEN
 
                DO il_ji=1,il_nbnorth
                   IF (rla_northlat(il_ji) <= rd_latmin) il_jmin=il_ji
                   IF (rla_northlat(il_ji) <= rd_latmax) il_jmax=il_ji
                ENDDO
 
                il_nby=il_jmax-il_jmin+1
 
 
                ALLOCATE(rdpa_longitude(il_nbx, il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_longitude',cl_fonction)
 
                ALLOCATE(rdpa_latitude(il_nbx, il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_latitude',cl_fonction)
 
                DO il_ji=1,il_nbx
                   rdpa_longitude(il_ji, 1:il_nby) = rd_lonmin + (il_ji-1)*rl_dx
                   rdpa_latitude(il_ji, 1:il_nby) = rla_northlat(il_jmin:il_jmax)
                ENDDO
 
 
             ! area on both south and north poles
             ELSE IF ((rd_latmin < 0.) .AND. (rd_latmax > 0.)) THEN
 
                DO il_ji=1,il_nbsouth
                   IF (rla_southlat(il_ji) >= rd_latmin) il_jmin=il_ji
                ENDDO
                DO il_ji=1,il_nbnorth
                   IF (rla_northlat(il_ji) <= rd_latmax) il_jmax=il_ji
                ENDDO
 
                il_nby=il_jmin+il_jmax-1
 
                ALLOCATE(rdpa_longitude(il_nbx, il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_longitude',cl_fonction)
 
                ALLOCATE(rdpa_latitude(il_nbx, il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_latitude',cl_fonction)
 
 
                DO il_ji=1,il_nbx
                   DO il_jj=1,il_jmin-1
                      rdpa_latitude(il_ji, il_jj) = rla_southlat(il_jmin-il_jj+1)
                   ENDDO
                   DO il_jj=il_jmin,il_nby
                      rdpa_latitude(il_ji, il_jj) = rla_northlat(il_jj-il_jmin+1)
                   ENDDO
                   rdpa_longitude(il_ji, 1:il_nby) = rd_lonmin + (il_ji-1)*rl_dx
                ENDDO
 
             ! south pole area
             ELSE IF ((rd_latmin < 0.) .AND. (rd_latmax <=0.)) THEN
 
                DO il_ji=1,il_nbsouth
                   IF (rla_southlat(il_ji) >= rd_latmin) il_jmin=il_ji
                   IF (rla_southlat(il_ji) >= rd_latmax) il_jmax=il_ji
                ENDDO
 
                il_nby=ABS(il_jmax-il_jmin)+1
 
                ALLOCATE(rdpa_longitude(il_nbx, il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_longitude',cl_fonction)
 
                ALLOCATE(rdpa_latitude(il_nbx, il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_latitude',cl_fonction)
 
                DO il_ji=1,il_nbx
                   rdpa_longitude(il_ji, 1:il_nby) = rd_lonmin + (il_ji-1)*rl_dx
                   DO il_jj=1,il_nby
                      rdpa_latitude(il_ji,il_ji) = rla_southlat(il_jmin-il_jj+1)
                   ENDDO
                ENDDO
 
             ENDIF
 
             id_nblon = il_nbx
             id_nblat = il_nby
 
          ENDIF
 
        ENDSUBROUTINE MCAL_generate_canevas_2Ddim
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
          !!=====================================================================
          !> \brief
          !! Description: This function creates 2D grid and returns longitude an
          !!              latitude values array (1D : longitude(longitude) or
          !!              2D: longitude(x,y)). The user choose the kind of grid:
          !!              regular or mercator projection.
          !!
          !!  @param rd_lonmin       The minimum longitude value of the grid.
          !!  @param rd_lonmax       The maximum longitude value of the grid.
          !!  @param rd_lonres       The resolution value of the longitude axis.
          !!                    (Ex: for 1/4° resolution, value is 4)
          !!  @param rd_latmin       The minimum latitude value of the grid.
          !!  @param rd_latmax       The maximum latitude value of the grid.
          !!  @param rd_latres       The resolution value of the latitude axis.
          !!                    (Ex: for 1/4° resolution, value is 4)
          !!  @param cd_gridtype     The type of the grid (latitude axis): 'DEG' for
          !!                    regular in degrees latitude axis, 'MER' for
          !!                    mercator projection latitude axis.
          !!  @param rdpa_longitude  Vector or array of longitude values. It depends on
          !!                    the longitude array number of dimension.
          !!  @param rdpa_latitude   Vector or array of latitude values. It depends on
          !!                    the latitude array number of dimension.
          !!  @param id_nblon        Number of longitude values.(Optional)
          !!  @param id_nblat        Number of latitude values.(Optional)
          !!
          !! History :
          !! \n          Original    : 07/2006 (S. Besnard)
          !!    \n       Modif       : 10/2008 C.REGNIER add MFT_error 
          !<
          !!=====================================================================
 
       SUBROUTINE MCAL_generate_canevas_1Ddim (rd_lonmin, &
                                                rd_lonmax, &
                                                rd_xresolution, &
                                                rd_latmin, &
                                                rd_latmax, &
                                                rd_yresolution, &
                                                cd_gridtype, &
                                                rdpa_longitude, &
                                                rdpa_latitude, &
                                                id_nblon, &
                                                id_nblat)
 
         USE MFT_error
         IMPLICIT NONE
 
         !----------------------------------------------------------------------
 
          CHARACTER(LEN=3), INTENT(IN) :: cd_gridtype
          REAL(KIND=4), INTENT(IN) :: rd_lonmin, rd_lonmax, rd_latmin, rd_latmax, &
                                      rd_xresolution, rd_yresolution
          REAL(KIND=4), POINTER, DIMENSION(:) :: rdpa_longitude, rdpa_latitude
          INTEGER, OPTIONAL :: id_nblon, id_nblat
 
          INTEGER :: il_nbx, il_nby, il_ji, il_nbsouth, il_nbnorth, il_status, &
                     il_jmin, il_jmax
          REAL(KIND=8) :: rl_dx, rl_dy
          REAL(KIND=8), DIMENSION(5000) :: rla_northlat, rla_southlat
          REAL(KIND=8), PARAMETER :: rl_northlimit=89., rl_southlimit=-89.
          REAL(KIND=8) :: rl_pi
          CHARACTER(LEN=255) :: cl_fonction
          
          cl_fonction="MCAL_generate_canevas_1Ddim"

          !----------------------------------------------------------------------
 
          rl_pi = 4.*ATAN(1.)
 
          !----------------------------------------------------------------------
          ! Verify
          il_status = fi_arrError(rd_latmin, rd_latmax, ' bounds.',cl_fonction)
          il_status = fi_arrError(rd_lonmin, rd_lonmax, ' bounds.',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Resolutions
          rl_dx = 1./rd_xresolution
          rl_dy = 1./rd_yresolution
 
          !----------------------------------------------------------------------
          ! Compute values
          IF ((cd_gridtype .EQ. 'deg') .OR. (cd_gridtype .EQ. 'DEG')) THEN
 
             il_nbx = (rd_lonmax-rd_lonmin)/rl_dx + 1
             il_nby = (rd_latmax-rd_latmin)/rl_dy + 1
 
             ALLOCATE(rdpa_longitude(il_nbx), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_longitude',cl_fonction)
 
             ALLOCATE(rdpa_latitude(il_nby), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_latitude',cl_fonction)
 
 
             DO il_ji=1,il_nbx
                rdpa_longitude(il_ji) = rd_lonmin + (il_ji-1)*rl_dx
             ENDDO
             DO il_ji=1,il_nby
                rdpa_latitude(il_ji)  = rd_latmin + (il_ji-1)*rl_dy
             ENDDO
 
             id_nblon = il_nbx
             id_nblat = il_nby
 
          ELSE IF ((cd_gridtype .EQ. 'mer') .OR. (cd_gridtype .EQ. 'MER')) THEN
 
             il_nbx = (rd_lonmax-rd_lonmin)/rl_dx + 1
 
             ALLOCATE(rdpa_longitude(il_nbx), &
                      stat=il_status)
             il_status = fi_memError(il_status, ' rdpa_longitude',cl_fonction)
 
             !all grid points latitude calculation
             il_ji=1
             rla_northlat(1)=0.
             DO WHILE (rla_northlat(il_ji) < rl_northlimit)
                il_ji=il_ji+1
                rla_northlat(il_ji) = ASIN(TANH((0. + (il_ji-1)*rl_dy)*rl_pi/180.))* &
                                     180./rl_pi
                il_nbnorth=il_ji-1
             ENDDO
 
             il_ji=1
             rla_southlat(1)=0.
             DO WHILE (rla_southlat(il_ji) > rl_southlimit)
                il_ji=il_ji+1
                rla_southlat(il_ji) = ASIN(TANH((0. - (il_ji-1)*rl_dy)*rl_pi/180.))* &
                                    180./rl_pi
                il_nbsouth=il_ji-1
             ENDDO
 
 
             !North pole area
             IF (rd_latmin >= 0.) THEN
 
                WRITE(0,*) 'rd_latmin= ',rd_latmin,' rd_latmax= ', rd_latmax
 
                DO il_ji=1,il_nbnorth
                   IF (rla_northlat(il_ji) <= rd_latmin) THEN
                       WRITE(0,*) ' il_ji =',il_ji,' rla_northlat(il_ji)=',rla_northlat(il_ji)
                      il_jmin=il_ji
                   ENDIF
                   IF (rla_northlat(il_ji) <= rd_latmax) il_jmax=il_ji
                ENDDO
 
                il_nby=il_jmax-il_jmin+1
 
                ALLOCATE(rdpa_latitude(il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_latitude',cl_fonction)
 
                DO il_ji=1,il_nbx
                   rdpa_longitude(il_ji) = REAL(rd_lonmin + (il_ji-1)*rl_dx,4)
                ENDDO
 
                rdpa_latitude(1:il_nby) = REAL(rla_northlat(il_jmin:il_jmax),4)
 
 
             ! area on both south and north poles
             ELSE IF ((rd_latmin < 0.) .AND. (rd_latmax > 0.)) THEN
 
                DO il_ji=1,il_nbsouth
                   IF (rla_southlat(il_ji) >= rd_latmin) il_jmin=il_ji
                ENDDO
                DO il_ji=1,il_nbnorth
                   IF (rla_northlat(il_ji) <= rd_latmax) il_jmax=il_ji
                ENDDO
 
                il_nby=il_jmin+il_jmax-1
 
                ALLOCATE(rdpa_latitude(il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_latitude',cl_fonction)
 
                DO il_ji=1,il_nbx
                   rdpa_longitude(il_ji) = REAL(rd_lonmin + (il_ji-1)*rl_dx,4)
                ENDDO
                DO il_ji=1,il_jmin-1
                   rdpa_latitude(il_ji) = REAL(rla_southlat(il_jmin-il_ji+1),4)
                ENDDO
                DO il_ji=il_jmin,il_nby
                   rdpa_latitude(il_ji) = REAL(rla_northlat(il_ji-il_jmin+1),4)
                ENDDO
 
             ! south pole area
             ELSE IF ((rd_latmin < 0.) .AND. (rd_latmax <=0.)) THEN
 
                DO il_ji=1,il_nbsouth
                   IF (rla_southlat(il_ji) >= rd_latmin) il_jmin=il_ji
                   IF (rla_southlat(il_ji) >= rd_latmax) il_jmax=il_ji
                ENDDO
 
                il_nby=ABS(il_jmax-il_jmin)+1
 
                ALLOCATE(rdpa_latitude(il_nby), &
                         stat=il_status)
                il_status = fi_memError(il_status, ' rdpa_latitude',cl_fonction)
 
                DO il_ji=1,il_nbx
                   rdpa_longitude(il_ji) = REAL(rd_lonmin + (il_ji-1)*rl_dx,4)
                ENDDO
                DO il_ji=1,il_nby
                   rdpa_latitude(il_ji) = REAL(rla_southlat(il_jmin-il_ji+1),4)
                ENDDO
 
             ENDIF
 
             id_nblon = il_nbx
             id_nblat = il_nby
 
          ENDIF

 END SUBROUTINE MCAL_generate_canevas_1Ddim

   !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 !
 !*----------------------------------------------------------------
 !
 
          !!=====================================================================
          !> \brief
          !! Description:  Conversion of 2D data into short
          !!
          !! @param rda_2DR4values :: 2D data values to convert
          !! @param ida_2Ddimsize  :: 2D size of rda_2DR4values
          !! @param rd_min         :: ida_2Ddimsize: min range 2D data
          !! @param rd_max         :: max range 2D data
          !! @param idpa_variable_2D:: 2D output data in short
          !! @param rd_scale_fact  :: scale factor in R8
          !! @param rd_offset      :: offset 
          !!
          !! History :  \n
          !!         C.REGNIER 11 Juin 2008
          !<
          !!=====================================================================
          SUBROUTINE MCAL_compute_short_R4_2D(rda_2DR4values,&
                                           ida_2Ddimsize,&
                                           rd_min,&
                                           rd_max,&
                                           idpa_variable_2D,&
                                           rd_scale_fact,&
                                           rd_offset)
 
          USE netcdf
          USE MFT_error
          IMPLICIT NONE
      
          REAL(KIND=4), DIMENSION(:,:), INTENT(IN)                 :: rda_2DR4values
          REAL(KIND=4), INTENT(IN)                                 :: rd_min,rd_max
          INTEGER(KIND=2), DIMENSION(:,:),POINTER                  :: idpa_variable_2D
          REAL(KIND=8), INTENT(OUT)                                :: rd_offset
          REAL(KIND=8),INTENT(OUT)                                 :: rd_scale_fact
          REAL(KIND=8)                                             :: rl_minvalue,rl_maxvalue
          REAL(KIND=4)                                             :: rl_fillvalue,rl_missvalue
          INTEGER(KIND=2)                                          :: il_fillvalue,il_missvalue
          INTEGER(KIND=4)                                          :: il_status
          INTEGER, DIMENSION(2)                                    :: ida_2Ddimsize
          CHARACTER(LEN=255)                                       :: cl_fonction

         cl_fonction="MCAL_compute_short_R4_2D"
         !
         !*----------------------------------------------------------------------------
         ! Init
          il_fillvalue=-(((2**16-2))/2)
          il_missvalue=il_fillvalue 
          rl_fillvalue =rg_fillvalue
          rl_missvalue =rg_fillvalue
          !* Allocation
          ALLOCATE(idpa_variable_2D(ida_2Ddimsize(1), &
                   ida_2Ddimsize(2)), &
                   stat=il_status)
          il_status = fi_memError(il_status, 'ila_variable_2D',cl_fonction)
          ! 
          !* Compute min et max value
          rl_minvalue = MINVAL(rda_2DR4values)
          rl_maxvalue = MAXVAL(array=rda_2DR4values, &
                                mask=rda_2DR4values .NE. rl_fillvalue)               
          !** Min et Max between a range
          IF(rl_minvalue>=rd_min .AND. rl_maxvalue<=rd_max) THEN
             !** Compute scale factor with min and max range
             !** Case 1 unsign short, reserves 0 as a pack form 
             IF (rd_min >= 0 ) THEN
                rd_scale_fact=(rd_max-rd_min)/((2**16)-2)
                rd_offset=rd_min-rd_scale_fact
             ELSE
                !** Case 2 signshort
                rd_scale_fact=(rd_max-rd_min)/((2**16)-2)
                rd_offset=int((rd_max+rd_min)/2) 
             ENDIF
          ELSE
             WRITE (0,*) 'Probleme sur les valeurs min max trop petites',rl_minvalue,rd_min,rl_maxvalue,rd_max
             STOP
          ENDIF
          WHERE (rda_2DR4values(:,:) .NE. rl_fillvalue)
             idpa_variable_2D(:,:)=nint((rda_2DR4values(:,:)-rd_offset)/rd_scale_fact,2)
          ELSEWHERE
             idpa_variable_2D=il_fillvalue
          ENDWHERE
          
        END SUBROUTINE MCAL_compute_short_R4_2D
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
          !!=====================================================================
          !> \brief
          !! Description:  Conversion of 3D data into short
          !! @param rda_3DR4values :: 3D data values to convert
          !! @param ida_3Ddimsize  :: 3D size of rda_3DR4values
          !! @param rd_min         :: min range 3D data
          !! @param rd_max         :: max range 3D data
          !! @param idpa_variable_3D:: 3D output data in short
          !! @param rd_scale_fact  :: scale factor in R8
          !! @param rd_offset      :: offset 
          !!
          !!  History : \n
          !! C.REGNIER 11 Juin 2008
          !<
          !!=====================================================================
 
       SUBROUTINE MCAL_compute_short_R4_3D(rda_3DR4values,&
                                           ida_3Ddimsize,&
                                           rd_min,&
                                           rd_max,&
                                           idpa_variable_3D,&
                                           rd_scale_fact,&
                                           rd_offset)
 
          USE netcdf
          USE MFT_error
         ! IMPLICIT NONE
 
          REAL(KIND=4), DIMENSION(:,:,:), INTENT(IN)               :: rda_3DR4values
          REAL(KIND=4), INTENT(IN)                                 :: rd_min,rd_max
          INTEGER(KIND=2), DIMENSION(:,:,:),POINTER                :: idpa_variable_3D
          REAL(KIND=8), INTENT(OUT)                                :: rd_offset
          REAL(KIND=8),INTENT(OUT)                                 :: rd_scale_fact
          REAL(KIND=8)                                             :: rl_minvalue,rl_maxvalue
          REAL(KIND=4)                                             :: rl_fillvalue,rl_missvalue
          INTEGER(KIND=2)                                          :: il_fillvalue,il_missvalue
          INTEGER(KIND=4)                                          :: il_status
          INTEGER, DIMENSION(3)                                    :: ida_3Ddimsize
          CHARACTER(LEN=255)                                       :: cl_fonction

          cl_fonction="MCAL_compute_short_R4_3D" 
          !
          !*----------------------------------------------------------------------------
          ! Init
          il_fillvalue=-(((2**16-2))/2)
          il_missvalue=il_fillvalue 
          rl_fillvalue =rg_fillvalue
          rl_missvalue =rg_fillvalue
          !* Allocation
          ALLOCATE(idpa_variable_3D(ida_3Ddimsize(1), &
                   ida_3Ddimsize(2),ida_3Ddimsize(3)), &
                   stat=il_status)
               
          il_status = fi_memError(il_status, 'ila_variable_3D',cl_fonction)
          
          ! 
          !* Compute min et max value
          rl_minvalue = MINVAL(rda_3DR4values)
          rl_maxvalue = MAXVAL(array=rda_3DR4values, &
                                mask=rda_3DR4values .NE. rl_fillvalue)               
          WRITE (0,*)  'rl_minvalue,rd_min,rl_maxvalue,rd_max  :: ',rl_minvalue,rd_min,rl_maxvalue,rd_max
          !** Min et Max between a range
          IF(rl_minvalue>=rd_min .AND. rl_maxvalue<=rd_max) THEN
             !** Compute scale factor with min and max range
             !** Case 1 unsign short, reserves 0 as a pack form 
             IF (rd_min >= 0 ) THEN
                WRITE (0,*) 'Cas unsign short'
                rd_scale_fact=(rd_max-rd_min)/((2**16)-2)
                rd_offset=rd_min-rd_scale_fact
             ELSE
                !** Case 2 signshort
                WRITE (0,*) 'Cas sign short'
                rd_scale_fact=(rd_max-rd_min)/((2**16)-2)
                rd_offset=int((rd_max+rd_min)/2) 
             ENDIF
          ELSE
             WRITE (0,*) 'Probleme sur les valeurs min max trop petites',rl_minvalue,rd_min,rl_maxvalue,rd_max
             STOP
          ENDIF
         WHERE (rda_3DR4values(:,:,:) .NE. rl_fillvalue)
            idpa_variable_3D(:,:,:)=nint((rda_3DR4values(:,:,:)-rd_offset)/rd_scale_fact)
         ELSEWHERE
            idpa_variable_3D=il_fillvalue
         ENDWHERE
       
       END SUBROUTINE MCAL_compute_short_R4_3D
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
          !!=====================================================================
          !>\brief
          !! Description:  Conversion of 4D data into short
          !! @param rda_4DR4values :: 4D data values to convert
          !! @param ida_4Ddimsize  :: 4D size of rda_4DR4values
          !! @param rd_min         :: min range 4D data
          !! @param rd_max         :: max range 4D data
          !! @param idpa_variable_4D:: 4D output data in short
          !! @param rd_scale_fact  :: scale factor in R8
          !! @param rd_offset      :: offset 
          !!
          !!  History :
          !! C.REGNIER 11 Juin 2008
          !<
          !!=====================================================================
          
 
       SUBROUTINE MCAL_compute_short_R4_4D(rda_4DR4values,&
                                           ida_4Ddimsize,&
                                           rd_min,&
                                           rd_max,&
                                           idpa_variable_4D,&
                                           rd_scale_fact,&
                                           rd_offset)
 
          USE netcdf
          USE MFT_error
          IMPLICIT NONE
 
          REAL(KIND=4), DIMENSION(:,:,:,:), INTENT(IN)             :: rda_4DR4values
          REAL(KIND=4), INTENT(IN)                                 :: rd_min,rd_max
          INTEGER(KIND=2), DIMENSION(:,:,:,:),POINTER              :: idpa_variable_4D
          REAL(KIND=8), INTENT(OUT)                                :: rd_offset
          REAL(KIND=8),INTENT(OUT)                                 :: rd_scale_fact
          REAL(KIND=8)                                             :: rl_minvalue,rl_maxvalue
          REAL(KIND=4)                                             :: rl_fillvalue,rl_missvalue
          INTEGER(KIND=2)                                          :: il_fillvalue,il_missvalue
          INTEGER(KIND=4)                                          :: il_status
          INTEGER, DIMENSION(4)                                    :: ida_4Ddimsize
          CHARACTER(LEN=255)                                       :: cl_fonction

          cl_fonction="MCAL_compute_short_R4_4D" 
         !!
         !*----------------------------------------------------------------------------
         ! Init
          il_fillvalue=-(((2**16-2))/2)
          il_missvalue=il_fillvalue 
          rl_fillvalue =rg_fillvalue
          rl_missvalue =rg_fillvalue
          !* Allocation
          ALLOCATE(idpa_variable_4D(ida_4Ddimsize(1), &
                   ida_4Ddimsize(2),ida_4Ddimsize(3),&
                   ida_4Ddimsize(4)),&
                   stat=il_status)
               
          il_status = fi_memError(il_status, 'ila_variable_4D',cl_fonction)
          ! 
          !* Compute min et max value
          rl_minvalue = MINVAL(rda_4DR4values)
          rl_maxvalue = MAXVAL(array=rda_4DR4values, &
                                mask=rda_4DR4values .NE. rl_fillvalue)
          WRITE (0,*)  'rl_minvalue,rd_min,rl_maxvalue,rd_max  :: ',rl_minvalue,rd_min,rl_maxvalue,rd_max
          !** Min et Max between a range
          IF(rl_minvalue>=rd_min .AND. rl_maxvalue<=rd_max) THEN
             !** Compute scale factor with min and max range
             !** Case 1 unsign short, reserves 0 as a pack form 
             IF (rd_min >= 0 ) THEN
                WRITE (0,*) 'Cas unsign short'
                rd_scale_fact=(rd_max-rd_min)/((2**16)-2)
                rd_offset=rd_min-rd_scale_fact
             ELSE
                !** Case 2 signshort
                WRITE (0,*) 'Cas sign short',rd_min
                rd_scale_fact=(rd_max-rd_min)/((2**16)-2)
                rd_offset=int((rd_max+rd_min)/2) 
             ENDIF
          ELSE
             WRITE (0,*) 'Probleme sur les valeurs min max trop petites',rl_minvalue,rd_min,rl_maxvalue,rd_max
             STOP
          ENDIF
          WHERE (rda_4DR4values(:,:,:,:) .NE. rl_fillvalue)
             idpa_variable_4D(:,:,:,:)=nint((rda_4DR4values(:,:,:,:)-rd_offset)/rd_scale_fact,2)
          ELSEWHERE
             idpa_variable_4D=il_fillvalue
          ENDWHERE
          
        END SUBROUTINE MCAL_compute_short_R4_4D
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************

          !!=====================================================================
          !> \brief
          !! Description:  Conversion of 2D data into short
          !! @param rda_2DR8values :: 2D data values to convert
          !! @param ida_2Ddimsize  :: 2D size of rda_2DR8values
          !! @param rd_min         :: min range 2D data
          !! @param rd_max         :: max range 2D data
          !! @param idpa_variable_2D:: 2D output data in short
          !! @param rd_scale_fact  :: scale factor in R8
          !! @param rd_offset      :: offset 
          !!
          !! History :
          !! C.REGNIER 11 Juin 2008
          !<
          !!=====================================================================

          SUBROUTINE MCAL_compute_short_R8_2D(rda_2DR8values,&
                                           ida_2Ddimsize,&
                                           rd_min,&
                                           rd_max,&
                                           idpa_variable_2D,&
                                           rd_scale_fact,&
                                           rd_offset)
 
          USE netcdf
          USE MFT_error
          IMPLICIT NONE
          
          REAL(KIND=8), DIMENSION(:,:), INTENT(IN)                 :: rda_2DR8values
          REAL(KIND=4), INTENT(IN)                                 :: rd_min,rd_max
          INTEGER(KIND=2), DIMENSION(:,:),POINTER                  :: idpa_variable_2D
          REAL(KIND=8), INTENT(OUT)                                :: rd_offset
          REAL(KIND=8),INTENT(OUT)                                 :: rd_scale_fact
          REAL(KIND=8)                                             :: rl_minvalue,rl_maxvalue
          REAL(KIND=8)                                             :: rl_fillvalue,rl_missvalue
          INTEGER(KIND=2)                                          :: il_fillvalue,il_missvalue
          INTEGER(KIND=4)                                          :: il_status
          INTEGER, DIMENSION(2)                                    :: ida_2Ddimsize
          CHARACTER(LEN=255)                                       :: cl_fonction
          
          cl_fonction="MCAL_compute_short_R8_2D" 
          !!
         !*----------------------------------------------------------------------------
         ! Init
          il_fillvalue=-(((2**16-2))/2)
          il_missvalue=il_fillvalue 
          rl_fillvalue = rg_fillvalue_R8
          rl_missvalue = rg_fillvalue_R8
          !* Allocation
          ALLOCATE(idpa_variable_2D(ida_2Ddimsize(1), &
                   ida_2Ddimsize(2)), &
                   stat=il_status)
               
          il_status = fi_memError(il_status, 'ila_variable_2D',cl_fonction)
          
          ! 
          !* Compute min et max value
          rl_minvalue = MINVAL(rda_2DR8values)
          rl_maxvalue = MAXVAL(array=rda_2DR8values, &
                                mask=rda_2DR8values .NE. rl_fillvalue)               
          !** Min et Max between a range
          IF(rl_minvalue>=rd_min .AND. rl_maxvalue<=rd_max) THEN
             !** Compute scale factor with min and max range
             !** Case 1 unsign short, reserves 0 as a pack form 
             IF (rd_min >= 0 ) THEN
                rd_scale_fact=(rd_max-rd_min)/((2**16)-2)
                rd_offset=rd_min-rd_scale_fact
             ELSE
                !** Case 2 signshort
                rd_scale_fact=(rd_max-rd_min)/((2**16)-2)
                rd_offset=int((rd_max+rd_min)/2) 
             ENDIF
          ELSE
             WRITE (0,*) 'Probleme sur les valeurs min max trop petites',rl_minvalue,rd_min,rl_maxvalue,rd_max
             STOP
          ENDIF
          WHERE (rda_2DR8values .NE. rl_fillvalue)
             idpa_variable_2D=nint((rda_2DR8values-rd_offset)/rd_scale_fact,2)
          ELSEWHERE
             idpa_variable_2D=il_fillvalue
          ENDWHERE
          
        END SUBROUTINE MCAL_compute_short_R8_2D
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
 
          !!=====================================================================
          !> \brief
          !! Description:  Conversion of 3D data into short
          !! @param rda_3DR8values :: 3D data values to convert
          !! @param ida_3Ddimsize  :: 3D size of rda_3DR8values
          !! @param rd_min         :: min range 3D data
          !! @param rd_max         :: max range 3D data
          !! @param idpa_variable_3D:: 3D output data in short
          !! @param rd_scale_fact  :: scale factor in R8
          !! @param rd_offset      :: offset 
          !!
          !!  History :
          !! C.REGNIER 11 Juin 2008
          !<
          !!=====================================================================
          
       SUBROUTINE MCAL_compute_short_R8_3D(rda_3DR8values,&
                                           ida_3Ddimsize,&
                                           rd_min,&
                                           rd_max,&
                                           idpa_variable_3D,&
                                           rd_scale_fact,&
                                           rd_offset)
 
          USE netcdf
          USE MFT_error
          IMPLICIT NONE
 
          REAL(KIND=8), DIMENSION(:,:,:), INTENT(IN)               :: rda_3DR8values
          REAL(KIND=4), INTENT(IN)                                 :: rd_min,rd_max
          INTEGER(KIND=2), DIMENSION(:,:,:),POINTER                :: idpa_variable_3D
          REAL(KIND=8), INTENT(OUT)                                :: rd_offset
          REAL(KIND=8),INTENT(OUT)                                 :: rd_scale_fact
          REAL(KIND=8)                                             :: rl_minvalue,rl_maxvalue
          REAL(KIND=8)                                             :: rl_fillvalue,rl_missvalue
          INTEGER(KIND=2)                                          :: il_fillvalue,il_missvalue
          INTEGER(KIND=4)                                          :: il_status
          INTEGER, DIMENSION(3)                                    :: ida_3Ddimsize
          CHARACTER(LEN=255)                                       :: cl_fonction
          
          cl_fonction="MCAL_compute_short_R8_3D"
          !
          !*----------------------------------------------------------------------------
          ! Init
          il_fillvalue=-(((2**16-2))/2)
          il_missvalue=il_fillvalue 
          rl_fillvalue = rg_fillvalue_R8
          rl_missvalue = rg_fillvalue_R8
          !* Allocation
          ALLOCATE(idpa_variable_3D(ida_3Ddimsize(1), &
                   ida_3Ddimsize(2),ida_3Ddimsize(3)), &
                   stat=il_status)
               
          il_status = fi_memError(il_status, 'ila_variable_3D',cl_fonction)
          
          ! 
          !* Compute min et max value
          rl_minvalue = MINVAL(rda_3DR8values)
          rl_maxvalue = MAXVAL(array=rda_3DR8values, &
                                mask=rda_3DR8values .NE. rl_fillvalue)                       
          !** Compute scale factor with min and max range
           IF(rl_minvalue>=rd_min .AND. rl_maxvalue<=rd_max) THEN
              !** Compute scale factor with min and max range
              !** Case 1 unsign short, reserves 0 as a pack form  
              IF (rd_min >= 0 ) THEN
                 WRITE (0,*) 'Cas unsign short'
                 rd_scale_fact=(rd_max-rd_min)/((2**16)-2)
                 rd_offset=rd_min-rd_scale_fact
              ELSE
                 !** Case 2 signshort
                 WRITE (0,*) 'Cas sign short'
                 rd_scale_fact=(rd_max-rd_min)/((2**16)-2)
                 rd_offset=int((rd_max+rd_min)/2) 
              ENDIF
           ELSE
              WRITE (0,*) 'Probleme sur les valeurs min max trop petites',rl_minvalue,rd_min,rl_maxvalue,rd_max
              STOP
           ENDIF
          WHERE (rda_3DR8values .NE. rl_fillvalue)
             idpa_variable_3D=nint((rda_3DR8values-rd_offset)/rd_scale_fact,2)
          ELSEWHERE
             idpa_variable_3D=il_fillvalue
          ENDWHERE
          
        END SUBROUTINE MCAL_compute_short_R8_3D
        
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
          !!=====================================================================
          !> \brief
          !! Description:  Conversion of 4D data into short
          !! @param rda_4DR8values :: 4D data values to convert
          !! @param ida_4Ddimsize  :: 4D size of rda_4DR8values
          !! @param rd_min         :: min range 4D data
          !! @param rd_max         :: max range 4D data
          !! @param idpa_variable_4D:: 4D output data in short
          !! @param rd_scale_fact  :: scale factor in R8
          !! @param rd_offset      :: offset 
          !!
          !!  History :
          !! C.REGNIER 11 Juin 2008
          !<
          !!=====================================================================

       SUBROUTINE MCAL_compute_short_R8_4D(rda_4DR8values,&
                                           ida_4Ddimsize,&
                                           rd_min,&
                                           rd_max,&
                                           idpa_variable_4D,&
                                           rd_scale_fact,&
                                           rd_offset)
 
          USE netcdf
          USE MFT_error
          IMPLICIT NONE
 
          REAL(KIND=8), DIMENSION(:,:,:,:), INTENT(IN)             :: rda_4DR8values
          REAL(KIND=4), INTENT(IN)                                 :: rd_min,rd_max
          INTEGER(KIND=2), DIMENSION(:,:,:,:),POINTER              :: idpa_variable_4D
          REAL(KIND=8), INTENT(OUT)                                :: rd_offset
          REAL(KIND=8),INTENT(OUT)                                 :: rd_scale_fact
          REAL(KIND=8)                                             :: rl_minvalue,rl_maxvalue
          REAL(KIND=8)                                             :: rl_fillvalue,rl_missvalue
          INTEGER(KIND=2)                                          :: il_fillvalue,il_missvalue
          INTEGER(KIND=4)                                          :: il_status
          INTEGER, DIMENSION(4)                                    :: ida_4Ddimsize
          CHARACTER(LEN=255)                                       :: cl_fonction
          
          cl_fonction="MCAL_compute_short_R8_4D"
          !
         !*----------------------------------------------------------------------------
         ! Init
          il_fillvalue=-(((2**16-2))/2)
          il_missvalue=il_fillvalue 
          rl_fillvalue = rg_fillvalue_R8
          rl_missvalue = rg_fillvalue_R8
          !* Allocation
          ALLOCATE(idpa_variable_4D(ida_4Ddimsize(1), &
                   ida_4Ddimsize(2),ida_4Ddimsize(3),&
                   ida_4Ddimsize(4)),&
                   stat=il_status)
               
          il_status = fi_memError(il_status, 'ila_variable_4D',cl_fonction)
          
          ! 
          !* Compute min et max value
          rl_minvalue = MINVAL(rda_4DR8values)
          rl_maxvalue = MAXVAL(array=rda_4DR8values, &
                                mask=rda_4DR8values .NE. rl_fillvalue)               
          !** Compute scale factor with min and max range
          IF(rl_minvalue>=rd_min .AND. rl_maxvalue<=rd_max) THEN
             !** Compute scale factor with min and max range
             !** Case 1 unsign short, reserves 0 as a pack form 
             IF (rd_min >= 0 ) THEN
                WRITE (0,*) 'Cas unsign short'
                rd_scale_fact=(rd_max-rd_min)/((2**16)-2)
                rd_offset=rd_min-rd_scale_fact
             ELSE
                !** Case 2 signshort
                WRITE (0,*) 'Cas sign short',rd_min
                rd_scale_fact=(rd_max-rd_min)/((2**16)-2)
                rd_offset=int((rd_max+rd_min)/2) 
             ENDIF
          ELSE
             WRITE (0,*) 'Probleme sur les valeurs min max trop petites',rl_minvalue,rd_min,rl_maxvalue,rd_max
             STOP
          ENDIF
          WHERE (rda_4DR8values(:,:,:,:) .NE. rl_fillvalue)
             idpa_variable_4D(:,:,:,:)=nint((rda_4DR8values(:,:,:,:)-rd_offset)/rd_scale_fact,2)
          ELSEWHERE
             idpa_variable_4D=il_fillvalue
          ENDWHERE
          
        END SUBROUTINE MCAL_compute_short_R8_4D
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
        !!=====================================================================
          !> \brief
          !! Description:  Convert char to int
          !! @param  cd_character  character value
          !! @param id_value  integer value
          !!
          !!  History : \n
          !! C.REGNIER  octobre 2008 V3
          !<
          !!=====================================================================
      SUBROUTINE transform_char_to_int (cd_character, &
                                        id_value)
 
         CHARACTER(LEN=5) :: cd_character
         CHARACTER(LEN=1) :: cl_tmp
         INTEGER :: id_value, il_ji, il_val

         id_value = 0
         DO il_ji=1,5
            cl_tmp = cd_character(il_ji:il_ji)
            !WRITE(0,*) ' cl_tmp = ',cl_tmp
            SELECT CASE (il_ji)

               CASE (1)
                 CALL my_ichar(cl_tmp,il_val)
                 id_value = id_value + (il_val*10000)
 
               CASE (2)
                 CALL my_ichar(cl_tmp, il_val)
                 id_value = id_value + (il_val*1000)
 
               CASE (3)
                 CALL my_ichar(cl_tmp, il_val)
                 id_value = id_value + (il_val*100)
 
               CASE (4)
                 CALL my_ichar(cl_tmp, il_val)
                 id_value = id_value + (il_val*10)
 
               CASE (5)
                 CALL my_ichar(cl_tmp, il_val)
                 id_value = id_value + (il_val*1)
           
               CASE DEFAULT
                 write(0,*) 'case default!'

            ENDSELECT
         
         ENDDO 
        


      ENDSUBROUTINE transform_char_to_int


!******************************************************************************
!******************************************************************************
!******************************************************************************

     SUBROUTINE my_ichar(cd_char, id_value)

        CHARACTER(LEN=1) :: cd_char
        INTEGER :: id_value


        SELECT CASE (cd_char)

           CASE('0')
              id_value = 0
           CASE('1')
              id_value = 1
           CASE('2')
              id_value = 2
           CASE('3')
              id_value = 3
           CASE('4')
              id_value = 4
           CASE('5')
              id_value = 5
           CASE('6')
              id_value = 6
           CASE('7')
              id_value = 7
           CASE('8')
              id_value = 8
           CASE('9')
              id_value = 9
           CASE DEFAULT
              id_value = 0

        ENDSELECT

     ENDSUBROUTINE my_ichar

!******************************************************************************
!******************************************************************************
!******************************************************************************
!
!*======================================================================
!     
!> \brief
!!**** MCAL_conversion_STU50_CAL \n
!!
!!     Purpose:\n
!!     --------\n
!!     Conversion d'une date exprimee en nombre de secondes              
!!     ecoulees depuis le 01-Jan-1950 00:00:00 en une date               
!!     exprimee sous la forme calendaire      
!!     issu de SU_DATE_STU50_CAL & su_date_SU_DATE_STU50_JMAHMSM dans su_date.f    
!!     (CLS) voir F. Hernandez  \n 
!!     Input : \n
!!     ------\n
!!     @param rl_dv_sec50 : Nombre de secondes ecoulees depuis le 01-Jan-1950 00:00:00                             
!!
!!     Output : \n
!!     @param cl_tv_date : date calendaire (15 caracteres)                            
!!                  (jjaaaaMMMjjhhmmss)                
!!
!!***  Method: \n
!!     ------- \n
!!
!!     Externals:\n
!!     ----------\n
!!     History:\n
!!     --------\n
!!      Version    Programmer      Date            Description\n
!!      -------    ----------      ----            -----------\n
!!      1.0        P.Sicard        14 Avril 1995   Machine for CLS Space Oceanography Group \n
!!      1.1        C.REGNIER        Mars 2007      Revision in Fortran 90\n
!<
!*--------------------------------------------------------------------------
!
    SUBROUTINE MCAL_conversion_STU50_CAL(rl_dv_sec50,cl_tv_date)
!
!**   0. DECLARATIONS
!        ------------
!
!**   0.1 Include files and modules
!     
     IMPLICIT NONE 

!
!**   0.2 Local variables
!   
      INTEGER(KIND=2)	              :: il_ev_jour    ! (S)
      INTEGER(KIND=2)	              :: il_ev_mois	 ! (S)
      INTEGER(KIND=4)                 :: il_ev_annee   ! (S)
      INTEGER(KIND=2)	              :: il_ev_heure   ! (S)
      INTEGER(KIND=2)	              :: il_ev_minute  ! (S)
      INTEGER(KIND=2)	              :: il_ev_seconde ! (S)
      INTEGER(KIND=4)	              :: il_jv_mcsec   ! (S)
      INTEGER(KIND=4)                 :: il_jv_ios  ! (S)
      INTEGER(KIND=4)	              :: il_jv_nb_jours	     ! nombre de jours ecoules
      INTEGER(KIND=4)	              :: il_jv_nb_annees	     ! nombre d'annees ecoulees

      INTEGER(KIND=2)	              :: il_ev_nb_annees_bis    ! nombre d'annees bissextiles
      INTEGER(KIND=2)	              :: il_ev_ind_bis          ! indic. annee bissextile
      REAL(KIND=8)                    :: rl_dv_trav	       ! variable de travail
      INTEGER(KIND=2),DIMENSION(12,2) :: ila_ek_su_date_quant

      CHARACTER(LEN=29)              :: cl_tc_su_date_form_cal
      CHARACTER(LEN=29)              :: cl_tc_su_date_form_cal_GODAE

      DATA cl_tc_su_date_form_cal /'(I4.4,A3,I2.2,I2.2,I2.2,I2.2)'/ 
      DATA cl_tc_su_date_form_cal_GODAE /'(I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2)'/
!      DATA cl_tc_su_date_form_cal_GODAE /'I4.4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2'/

!     DATA ila_ek_su_date_quant /'(0,31,60,91,121,152,182,213,244,274,305,335,0,31,59,90,120,151,181,212,243,273,304,334)'/
      DATA ila_ek_su_date_quant / 0,31,60,91,121,152,182,213,244,274,305,335,0,31,59,90,120,151,181,212,243,273,304,334 /
      CHARACTER(LEN=3),DIMENSION(12)     ::	tk_su_date_nom_mois
      DATA tk_su_date_nom_mois /'JAN','FEB','MAR','APR','MAY','JUN',&
     &                          'JUL','AUG','SEP','OCT','NOV','DEC'/
!
!**   0.3 Dummy variables 
!   

    CHARACTER(LEN=19),INTENT(OUT)           :: cl_tv_date    ! (S)
    REAL(KIND=4)       	                   :: rl_dv_sec50   ! (E)
!    
!*--------------------------------------------------------------------------
!* 
! Initialisation 
      cl_tv_date = ''
!
!**    TEST d'entree
      IF (rl_dv_sec50 .lt. 0.d0) stop
! 
!**     CALCUL DU NOMBRE DE JOURS ECOULEES DEPUIS 1950
! 
      il_jv_nb_jours = INT (rl_dv_sec50 / 86400.D0)

!**     CALCUL HEURE
 
      rl_dv_trav  = rl_dv_sec50 - DBLE(il_jv_nb_jours) * 86400.D0
      il_ev_heure = INT(rl_dv_trav / 3600.D0)

!**     CALCUL MINUTE
! 
      rl_dv_trav   = rl_dv_trav - DBLE(il_ev_heure) * 3600.D0
      il_ev_minute = INT(rl_dv_trav / 60.D0)

!**     CALCUL SECONDE
! 
      rl_dv_trav    = rl_dv_trav - DBLE(il_ev_minute) * 60.D0
      il_ev_seconde = INT(rl_dv_trav)

!**     CALCUL MICROSECONDES
! 
      rl_dv_trav  = (rl_dv_trav - DBLE(il_ev_seconde)) * 1.D6
      il_jv_mcsec = NINT(rl_dv_trav)

!**     AJUSTEMENT NB_JOURS HEURE MINUTE SECONDE SI LE NOMBRE
!**     DE MICROSECONDES CALCULEES EST >= 1.D6
! 
      if (il_jv_mcsec .ge. 1000000) then
         il_jv_mcsec = il_jv_mcsec - 1000000
         il_ev_seconde = il_ev_seconde + 1
         if (il_ev_seconde .ge. 60) then
             il_ev_seconde = il_ev_seconde - 60
             il_ev_minute  = il_ev_minute + 1
             if (il_ev_minute .ge. 60) then
                il_ev_minute = il_ev_minute - 60
                il_ev_heure  = il_ev_heure + 1
                if (il_ev_heure .ge. 24) then
                    il_ev_heure = il_ev_heure - 24
                    il_jv_nb_jours = il_jv_nb_jours + 1
                endif
             endif
          endif
      endif

!**     CALCUL NOMBRE D'ANNEES ECOULEES DEPUIS 1950
! 
      rl_dv_trav      = DBLE(il_jv_nb_jours)
      il_jv_nb_annees = INT((rl_dv_trav + 0.5) / 365.25)

!**     CALCUL ANNEE 
 
      il_ev_annee = il_jv_nb_annees + 1950

!**     CALCUL DU NOMBRE D'ANNEES BISSEXTILES PASSEES DEPUIS 1950
! 
      il_ev_nb_annees_bis =  (il_ev_annee-1 -1900) / 4 - 12

!**     Test si annee courante bissextile
!**     ind_bis = 1 -> annee courante bissextile
!**     ind_bis = 2 -> sinon
      il_ev_ind_bis = MIN(MOD(il_ev_annee,4) + 1, 2)

!**     CALCUL DU NOMBRE DE JOURS ECOULES DANS L'ANNEE
! 
      il_jv_nb_jours = il_jv_nb_jours - (il_jv_nb_annees * 365) - il_ev_nb_annees_bis + 1

!**     CALCUL NO DU MOIS DANS L'ANNEE
!
      il_ev_mois = 1

      !do while (il_ev_mois .LE. 12) 
      ! IF (il_jv_nb_jours .GT. ila_ek_su_date_quant(il_ev_mois,il_ev_ind_bis)) il_ev_mois = il_ev_mois + 1
      !enddo
      do while ((il_ev_mois .LE. 12) .and.(il_jv_nb_jours .GT.ila_ek_su_date_quant(il_ev_mois,il_ev_ind_bis)))
            il_ev_mois = il_ev_mois + 1
        IF (il_ev_mois == 13 ) exit 
      enddo
      il_ev_mois = il_ev_mois - 1

!**     CALCUL DU NUMERO DU JOURS DANS LE MOIS
! 
       il_ev_jour = il_jv_nb_jours -  ila_ek_su_date_quant(il_ev_mois,il_ev_ind_bis)

!**     FORMATTAGE SOUS FORME ASCII DE LA DATE
 
        write(cl_tv_date,9001) il_ev_annee,&
             & il_ev_mois,il_ev_jour,il_ev_heure,il_ev_minute,il_ev_seconde
!    
9001 FORMAT(I4.4,'-',I2.2,'-',I2.2,' ',I2.2,':',I2.2,':',I2.2)

!*--------------------------------------------------------------------------
!

  END SUBROUTINE MCAL_conversion_STU50_CAL

!******************************************************************************
!******************************************************************************
!******************************************************************************
FUNCTION search_season(cd_month)

CHARACTER(LEN=3)                   :: cd_month    ! nom du mois courant                                 
INTEGER(KIND=4)                    :: search_season   ! nom du mois courant                                 
SELECTCASE (cd_month)
       CASE ('JAN')
          search_season=1 
       CASE ('FEB')
          search_season=1 
       CASE ('MAR') 
          search_season=1 
       CASE ('APR')
          search_season=2 
       CASE ('MAY') 
          search_season=2 
       CASE ('JUN')
          search_season=2 
       CASE ('JUL')
          search_season=3 
       CASE ('AUG')
          search_season=3 
       CASE ('SEP')
          search_season=3
       CASE ('OCT')
          search_season=4 
       CASE ('NOV')
          search_season=4 
       CASE ('DEC') 
          search_season=4 
       CASE DEFAULT
          WRITE(0,*) 'Month unrecognize !'
          STOP
    RETURN
ENDSELECT
END FUNCTION search_season
!
!******************************************************************************
!
FUNCTION search_month(cd_month)

CHARACTER(LEN=3)                   :: cd_month    ! nom du mois courant                                 
INTEGER(KIND=4)                    :: search_month   ! nom du mois courant                                 
SELECTCASE (cd_month)
       CASE ('JAN')
          search_month=1 
       CASE ('FEB')
          search_month=2 
       CASE ('MAR') 
          search_month=3 
       CASE ('APR')
          search_month=4 
       CASE ('MAY') 
          search_month=5 
       CASE ('JUN')
          search_month=6 
       CASE ('JUL')
          search_month=7 
       CASE ('AUG')
          search_month=8 
       CASE ('SEP')
          search_month=9
       CASE ('OCT')
          search_month=10 
       CASE ('NOV')
          search_month=11 
       CASE ('DEC') 
          search_month=12 
       CASE DEFAULT
          WRITE(0,*) 'Month unrecognize !'
          STOP
    RETURN
ENDSELECT

END FUNCTION search_month
!
    !!=======================================================================
    !> \brief
    !!**** subroutine read_lighout \n
    !!
    !!     Purpose:\n
    !!     --------\n
    !!     Program to read lightoutfile coded in bytes
    !!     Input : \n
    !!     ------\n
    !!     @filename : input filename
    !!     @dimension : input dimension in 2D or 3D
    !!     @variable : name of output variable
    !!     @meshfile :  mesh file for lon lat construction
    !!     @sampling :  sampling of the lightout file (for ex 3 for PSY4 and 2 for PSY3)
    !!     @lon      : 2D pointer for output longitude
    !!     @lat      : 2D pointer for output latitude
    !!     @var      : 3D pointer for output variable
    !!     Output : \n
    !!     file in netcdf with lon/lat/depth
    !!     @lon      : 2D pointer for output longitude
    !!     @lat      : 2D pointer for output latitude
    !!     @var      : 3D pointer for output variable
    !!
    !!***  Method: \n
    !!     ------- \n
    !!     History: \n
    !!     -------- \n
    !!   \n Version    Programmer      Date            Description
    !!   \n -------    ----------      ----            -----------
    !!   \n  1.0        C. REGNIER     1st June 2016        Creation with jmi contribution
    !<
    !!=======================================================================
    !
    SUBROUTINE read_lightout_nc(cd_filename,cd_dim,cd_variable,cd_meshfile,&
                             id_sampling,rdpa_lon,rdpa_lat,rdpa_depth,&
                             rdpa_var,ld_write)
    !
    !
    !**   0. DECLARATIONS
    !        ------------
    !
    !**   0.1 Include files and modules
    !
    USE MIOL
    IMPLICIT NONE
    !
    !
    !**   0.2 Local variables
    !
    !
    CHARACTER(LEN=256),INTENT(IN)                  :: cd_filename,cd_dim,cd_meshfile,cd_variable
    CHARACTER(LEN=256)                             :: cl_outputfile
    INTEGER(KIND=4),INTENT(IN)                     :: id_sampling
    CHARACTER(LEN=256)                             :: cl_lon,cl_lat,cl_depth
    REAL(KIND=4)                                   :: rl_dimiglo_sampling,rl_dimjglo_sampling,rl_dimk
    REAL(KIND=4)                                   :: rl_AUXMIN,rl_AUXMAX,rl_UNDEFO,rl_AUXMID,&
                                                      rl_scale,rl_ATANMAX,rl_ATANMIN
    REAL(KIND=4),DIMENSION(:,:),POINTER            :: rlpa_lon,rlpa_lat,rdpa_lon,rdpa_lat
    REAL(KIND=4),DIMENSION(:,:,:),POINTER          :: rdpa_var
    REAL(KIND=4),DIMENSION(:),POINTER              :: rdpa_depth
    REAL(KIND=4),DIMENSION(:,:,:),ALLOCATABLE      :: rla_tab3D
    REAL(KIND=4),DIMENSION(:,:,:,:),ALLOCATABLE    :: rla_tab4D
    REAL(KIND=4), parameter                        :: rp_pi     = 3.14159265358979323846
    CHARACTER(LEN=255),DIMENSION(:),ALLOCATABLE    :: cla_dimname
    INTEGER(KIND=4)  ,DIMENSION(:),ALLOCATABLE     :: ila_dimlen
    INTEGER(KIND=2)                                :: il_MAXINT,il_RMAXINT
    INTEGER(KIND=2), DIMENSION(:,:,:), ALLOCATABLE :: ila_AUXglo
    INTEGER(KIND=4)                                :: il_ji,il_err,il_dimiglo_sampling,il_dimjglo_sampling,&
                                                      il_nx,il_ny,il_nx_ini,il_ny_ini,il_nz,il_dim,il_dimk,il_unit_lu,il_narg,&
                                                      iargc,il_file_id,il_meshfile
    INTEGER(KIND=4),DIMENSION(2)                   :: ila_lon2D,ila_lat2D
    INTEGER(KIND=4),DIMENSION(2)                   :: ila_depth
    INTEGER(KIND=4),DIMENSION(3)                   :: ila_dim3D
    INTEGER(KIND=4),DIMENSION(4)                   :: ila_dim4D
    LOGICAL                                        :: ld_write
    CALL MIOL_getFreeLU(il_unit_lu)
    OPEN(il_unit_lu,file=TRIM(cd_filename),form='unformatted',access='sequential',action='READ',status='OLD',iostat=il_err)
    IF (il_err > 0) THEN
       WRITE (0,*) "read_lightout: Problem opening file "//TRIM(cd_filename)
       CALL flush(0)
       STOP
    ENDIF
!
    READ(il_unit_lu) rl_dimiglo_sampling
    READ(il_unit_lu) rl_dimjglo_sampling
    READ(il_unit_lu) rl_dimk
    READ(il_unit_lu) rl_AUXMIN,rl_AUXMAX,rl_UNDEFO,il_MAXINT
!
    il_dimiglo_sampling=INT(rl_dimiglo_sampling,4)
    il_dimjglo_sampling=INT(rl_dimjglo_sampling,4)
    il_dimk=INT(rl_dimk,4)
!
    ALLOCATE(ila_AUXglo(il_dimiglo_sampling,il_dimjglo_sampling,il_dimk))
!
    READ(il_unit_lu) ila_AUXglo
!   Convert with offset and scale factor
    WRITE(0,*) 'Read OK : ',shape(ila_AUXglo)
    WRITE(0,*) 'Min : ',minval(ila_AUXglo)
    WRITE(0,*) 'Max : ',maxval(ila_AUXglo)
    WRITE(0,*) 'Dimensions : ',size(shape(ila_AUXglo))
    il_dim=size(shape(ila_AUXglo))
    IF ( cd_dim == '3D')  THEN
        IF ( il_dim /= 3 ) THEN
            WRITE(0,*) cd_filename,' is not a 3D file'
            STOP
        ENDIF
        ila_dim3D=shape(ila_AUXglo)
        ALLOCATE(rdpa_var(ila_dim3D(1),ila_dim3D(2),ila_dim3D(3)))
        rdpa_var(:,:,:)=rg_fillValue
        il_nx=ila_dim3D(1)
        il_ny=ila_dim3D(2)
        il_nz=ila_dim3D(3)
    !ELSEIF ( cd_dim == '4D')  THEN
    !    IF ( il_dim /= 4 ) THEN
    !        WRITE(0,*) cd_filename,' is not a 3D file'
    !        STOP
    !    ENDIF
    !    ila_dim4D=shape(ila_AUXglo)
    !    ALLOCATE(rla_tab4D(ila_dim4D(1),ila_dim4D(2),ila_dim4D(3),ila_dim4D(4)))
    !    rla_tab4D(:,:,:,:)=rg_fillValue
    ELSE
        WRITE(0,*) 'Problem with dimensions : ',cd_dim
        STOP
    ENDIF
    rl_AUXMID = (rl_AUXMIN+rl_AUXMAX)/2.
    rl_scale = rl_AUXMAX-rl_AUXMID
    WRITE(0,*) 'Scale : ',rl_scale
    IF  (rl_scale .ne. 0.) THEN
        rl_scale = 4./rl_scale
    ELSE
        rl_scale = 1.
    ENDIF
    rl_ATANMAX=atan( (rl_AUXMAX - rl_AUXMID )*rl_scale ) / rp_pi
    rl_ATANMIN=ATAN( (rl_AUXMIN - rl_AUXMID )*rl_scale ) / rp_pi
    il_RMAXINT=il_MAXINT-1
    IF ( il_dim == 3 ) THEN
        WHERE (ila_AUXglo .ne. il_MAXINT) rdpa_var=TAN(ila_AUXglo/(il_RMAXINT/rl_ATANMAX)*rp_pi)/rl_scale+rl_AUXMID
  !!  ELSE IF ( il_dim == 4 ) THEN
  !!      WHERE (ila_AUXglo .ne. il_MAXINT) rla_tab4d=TAN(ila_AUXglo/(il_RMAXINT/rl_ATANMAX)*rp_pi)/rl_scale+rl_AUXMID
    ENDIF
    cl_lon="nav_lon"
    cl_lat="nav_lat"
    cl_depth="nav_lev"
   !! open mesh file
    CALL MIOL_openr_file_NC(cd_meshfile,il_meshfile)
    CALL MIOL_read_field_NC(il_meshfile,cl_lon,rlpa_lon,ila_lon2D)
    CALL MIOL_read_field_NC(il_meshfile,cl_lat,rlpa_lat,ila_lat2D)
    CALL MIOL_read_field_NC(il_meshfile,cl_depth,rdpa_depth,ila_depth)
    CALL MIOL_close_file_NC(il_meshfile)
    print * ,shape(rlpa_lon)
    il_nx_ini=ila_lon2D(1)
    il_ny_ini=ila_lon2D(2)
    ALLOCATE(rdpa_lon(il_nx_ini/id_sampling,il_ny_ini/id_sampling))
    ALLOCATE(rdpa_lat(il_nx_ini/id_sampling,il_ny_ini/id_sampling))
    rdpa_lon(:,:)=rlpa_lon(1:il_nx_ini:id_sampling,1:il_ny_ini:id_sampling)
    print *,'Longitude'
    !print *,rla_lon(:,1)
    print *,'------------------'
    rdpa_lat(:,:)=rlpa_lat(1:il_nx_ini:id_sampling,1:il_ny_ini:id_sampling)
    print *,'Latitude'
    !print *,rla_lat
    !print *,rla_tab3d
    IF ( ld_write) THEN
        ALLOCATE(cla_dimname(il_dim))
        ALLOCATE(ila_dimlen(il_dim))
        cla_dimname(1) = 'lon'
        cla_dimname(2)=  'lat'
        cla_dimname(3) = 'depth'
        ila_dimlen(1) = ila_dim3D(1)
        ila_dimlen(2) = ila_dim3D(2)
        ila_dimlen(3) = ila_dim3D(3)
        cl_outputfile=TRIM(cd_filename(1:len(cd_filename)-3))//'.nc'
        call MIOL_create_file_NC(TRIM(cl_outputfile),&
                                  il_dim,&
                                  cla_dimname,&
                                  ila_dimlen)
        call MIOL_openw_file_NC(TRIM(cl_outputfile), il_file_id)
        call MIOL_write_field_NC (il_file_id,&
                                  'longitude',&
                                  'XY',&
                                  rdpa_lon)
        call MIOL_write_field_NC (il_file_id,&
                                  'latitude',&
                                  'XY',&
                                  rdpa_lat)
        IF (ila_dimlen(3) .eq. 1 ) THEN
            call MIOL_write_field_NC (il_file_id,&
                                      'depth',&
                                      'Z',&
                                      rdpa_depth(1))
        ELSE
            call MIOL_write_field_NC (il_file_id,&
                                      'depth',&
                                      'Z',&
                                      rdpa_depth(:))
        ENDIF
        call MIOL_write_field_NC (il_file_id,&
                                  cd_variable,&
                                  'XYZ',&
                                  rdpa_var(:,:,:))
        CALL MIOL_close_file_NC(il_file_id)
        WRITE (0,*) 'MIOL_create_file_NC OK'
    ENDIF
    END SUBROUTINE read_lightout_nc



 END MODULE MCAL
