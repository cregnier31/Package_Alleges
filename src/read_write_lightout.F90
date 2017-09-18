!!=======================================================================
!!     Program to read lightoutfile coded in bytes
!!***  Method: \n
!!     ------- \n
!!     read_lightout
!!   \n Version    Programmer      Date            Description
!!   \n -------    ----------      ----            -----------
!!   \n  1.0        C. REGNIER     May 2016        Creation with jmi contribution
!!=======================================================================

PROGRAM read_lightout_main
    !
    !**   0. DECLARATIONS
    !        ------------
    !
    !**   0.1 Include files and modules
    !
    USE MIOL
    IMPLICIT NONE
    !
    !**   0.2 Local variables
    !
    CHARACTER(LEN=256)                             :: cl_filename,cl_input,cl_dim,cl_meshfile,cl_variable,cl_sampling
    REAL(KIND=4),DIMENSION(:,:,:),POINTER          :: rlpa_var
    REAL(KIND=4),DIMENSION(:,:),POINTER            :: rlpa_lon,rlpa_lat
    REAL(KIND=4),DIMENSION(:),POINTER              :: rlpa_depth
    INTEGER(KIND=4)                                :: il_ji,il_sampling,il_narg,iargc
    INTEGER(KIND=4),DIMENSION(2)                   :: ila_lon2D,ila_lat2D
    INTEGER(KIND=4),DIMENSION(2)                   :: ila_depth
    INTEGER(KIND=4),DIMENSION(3)                   :: ila_dim3D
    INTEGER(KIND=4),DIMENSION(4)                   :: ila_dim4D
    LOGICAL                                        :: ll_write
    !
    il_narg=iargc()
    IF (il_narg.NE.10) THEN
       PRINT *,' '
       PRINT *,'Read lightoutfile usage : '
       PRINT *,' ReadLightout.exe -i inputfile -d dimensions 3D/4D -v variable'
       PRINT *,'Arguments  :: ',il_narg
       STOP
    ELSE
        DO il_ji=1,il_narg
            CALL getarg(il_ji,cl_input)
            IF (cl_input .EQ. '-i') THEN
                CALL getarg(il_ji+1, cl_filename)
            ELSE IF (cl_input .EQ. '-d') THEN
                CALL getarg(il_ji+1, cl_dim)
            ELSE IF (cl_input .EQ. '-v') THEN
                CALL getarg(il_ji+1, cl_variable)
            ELSE IF (cl_input .EQ. '-m') THEN
                CALL getarg(il_ji+1, cl_meshfile)
            ELSE IF (cl_input .EQ. '-s') THEN
                CALL getarg(il_ji+1, cl_sampling)
                READ(cl_sampling,'(I2)') il_sampling
            ENDIF
        ENDDO
    ENDIF
    ll_write=.TRUE.
    CALL read_lightout(cl_filename,cl_dim,cl_variable,cl_meshfile,&
                       il_sampling,rlpa_lon,rlpa_lat,rlpa_depth,rlpa_var,ll_write)

CONTAINS
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
    SUBROUTINE read_lightout(cd_filename,cd_dim,cd_variable,cd_meshfile,&
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
    END SUBROUTINE read_lightout
!
END PROGRAM read_lightout_main
