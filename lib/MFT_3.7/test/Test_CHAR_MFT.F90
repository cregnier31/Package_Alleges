  PROGRAM Test_CHAR_MFT  
  !
  !**** 
  !
  !     Purpose :
  !     Programme de test pour MFT pour tester la lecture des charactères 1,2D,3D,4D
  !
  !***  Method : Test de la lecture d'un fichier CORIOLIS
  !     Externals :
  !     Files :
  !     References:
  !
  !     History
  !     -------
  !      Version    Programmer    Date       Description
  !      ---------------------------------------------
  !       1.0       C.REGNIER    20/03/2008    creation test implentation lecture char dans MIOL
  !       1.1       C.REGNIER    19/09/2012    make test integration 
  !
  !*-------------------------------------------------------------
  !
  !
  !** ++  MODULES used
  USE MIOL
  USE netcdf
  !
  IMPLICIT NONE
  !
  !** ++  Local Declarations 
  !
  INTEGER(KIND=4)                         :: il_file_id,il_filein,il_nprof,il_size,il_dim
 
  CHARACTER(LEN=255)                      :: cl_file,cl_tmp

  !** 4D
  
  CHARACTER(LEN=256), DIMENSION(:,:,:,:), POINTER :: clpa_SCI
  CHARACTER(LEN=16), DIMENSION(:,:,:,:), POINTER  :: clpa_PARAMETER
  CHARACTER(LEN=1), DIMENSION(:,:,:,:), POINTER   :: clpa_calib
  !** 3D
  CHARACTER(LEN=16), DIMENSION(:,:,:), POINTER :: clpa_HISTORY_PARAMETER,clpa_STATION_PARAMETER
  CHARACTER(LEN=4), DIMENSION(:,:,:), POINTER  :: clpa_HISTORY_INSTITUTION
  CHARACTER(LEN=1), DIMENSION(:,:,:), POINTER  :: clpa_HISTORY_DATE

  !** 2D

  CHARACTER(LEN=64), DIMENSION(:,:), POINTER :: clpa_PROJECT_NAME
  CHARACTER(LEN=16), DIMENSION(:,:), POINTER :: clpa_dc_reference
  CHARACTER(LEN=8), DIMENSION(:,:), POINTER  :: clpa_platnum
  CHARACTER(LEN=4), DIMENSION(:,:), POINTER  :: clpa_WMO_INST_TYPE
  CHARACTER(LEN=2), DIMENSION(:,:), POINTER  :: clpa_datacentre
  CHARACTER(LEN=1), DIMENSION(:,:), POINTER  :: clpa_tempqc,clpa_PARAMETERINT

  !** 1D
  CHARACTER(LEN=16), DIMENSION(:), POINTER   :: clpa_datatypevalue
  CHARACTER(LEN=4), DIMENSION(:), POINTER    :: clpa_formatversionvalue
  CHARACTER(LEN=1), DIMENSION(:), POINTER    :: clpa_positionqcvalue


  LOGICAL                                 :: ll_exist
 

  !
  !*------------------------------------------------------------------------------------
  !
  WRITE (0,*) '*********************************************************'
  WRITE (0,*) '               Test Read Char                            '
  WRITE (0,*) '---------------------------------------------------------'

  cl_file='tmp/CO_20120201_180000_PR_PF_A.nc'
  ll_exist=.FALSE.
  
  !** Test Lecture char
  CALL MIOL_openr_file_NC(cl_file, il_filein)

  cl_tmp ='PARAMETERINT'
  il_size=1
  CALL MIOL_read_field_NC(il_filein,cl_tmp,clpa_PARAMETERINT,il_size)
  PRINT *,'Test PARAMETERINT OK'

  !** Test 1D ::
  ! les tableaux 1D sont de plusieurs type ::
  ! Soit les dimensions suivantes 
  ! STRING256 = 256 ;
  ! STRING64 = 64 ;
  ! STRING32 = 32 ;
  ! STRING16 = 16 ;
  ! STRING8 = 8 ;
  ! STRING4 = 4 ;
  ! STRING2 = 2 ;
  ! N_PROF = 5 ;
  ! N_PARAM = 2 ;
  ! N_LEVELS = 24 ;

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !  1D !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !**** POSITION_QC(N_PROF)
  cl_tmp ='POSITION_QC'
  il_size=1
  CALL MIOL_read_field_NC(cl_file,cl_tmp,clpa_positionqcvalue,il_size)
  !WRITE (0,*) 'TEST 1 POSITION_QC ok : ',clpa_positionqcvalue
  !**** DATA_TYPE(STRING16)
  cl_tmp ='DATA_TYPE' 
  il_size=16
  il_dim=1
  CALL MIOL_read_field_NC(cl_file,cl_tmp,clpa_datatypevalue,il_size)
 ! WRITE (0,*) 'TEST 2 DATA_TYPE ok : ',clpa_datatypevalue
 !*** FORMAT_VERSION(STRING4)
  !$ 
  cl_tmp ='FORMAT_VERSION' 
  il_size=4
  il_dim=1
  CALL MIOL_read_field_NC(cl_file,cl_tmp,clpa_formatversionvalue,il_size)
 ! WRITE (0,*) 'TEST 3 FORMAT_VERSION ok : ',clpa_formatversionvalue

PRINT *,'Test Char 1D OK                                        '
!  DATE_CREATION(DATE_TIME)
!  REFERENCE_DATE_TIME(DATE_TIME)
  
!!$  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!$  ! 2D !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  !!$ TEMP_QC(N_PROF, N_LEVELS)
  cl_tmp ='TEMP_QC'
  il_size=1
  CALL MIOL_read_field_NC(cl_file,cl_tmp,clpa_tempqc,il_size)
  !WRITE (0,*) 'TEST 4 TEMP_QC ok : ',clpa_tempqc(:,1),size(clpa_tempqc(:,1))
  
  !!$ PLATFORM_NUMBER(N_PROF, STRING8)
  cl_tmp='PLATFORM_NUMBER'
  il_size=8
  CALL MIOL_read_field_NC(cl_file,cl_tmp,clpa_platnum,il_size)
  !WRITE (0,*) 'TEST 5  PLATFORM_NUMBER ok : ',clpa_platnum 
  cl_tmp='DATA_CENTRE'
  !!$ DATA_CENTRE(N_PROF, STRING2)
  il_size=2
  CALL MIOL_read_field_NC(cl_file,cl_tmp,clpa_datacentre,il_size)
  !WRITE (0,*) 'TEST 6 DATA_CENTRE  ok : ',clpa_datacentre

  cl_tmp='DC_REFERENCE'
  !!$  DC_REFERENCE(N_PROF, STRING32)
  il_size=16
  CALL MIOL_read_field_NC(cl_file,cl_tmp,clpa_dc_reference,il_size)
  ! WRITE (0,*) 'TEST 7 DC_REFERENCE  ok : ',clpa_dc_reference

  cl_tmp='PROJECT_NAME'
  !!$  PROJECT_NAME(N_PROF, STRING64)
  il_size=64
  CALL MIOL_read_field_NC(cl_file,cl_tmp,clpa_PROJECT_NAME,il_size)
  !WRITE (0,*) 'TEST 8 PROJECT_NAME ok : ',clpa_PROJECT_NAME
  cl_tmp='WMO_INST_TYPE'
  !!$  WMO_INST_TYPE(N_PROF, STRING4)
  il_size=4
  CALL MIOL_read_field_NC(cl_file,cl_tmp,clpa_WMO_INST_TYPE,il_size)
  !WRITE (0,*) 'TEST 9 WMO_INST_TYPE ok : ',clpa_WMO_INST_TYPE

PRINT *,'Test Char 2D OK                                        '
!!$
!!$  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!$  ! 3D !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  cl_tmp='HISTORY_DATE'
  !!$ HISTORY_DATE(N_HISTORY, N_PROF, DATE_TIME)
  il_size=1
  CALL MIOL_read_field_NC(cl_file,cl_tmp,clpa_HISTORY_DATE,il_size)
  !WRITE (0,*) 'TEST 10 HISTORY_DATE  ok : ',clpa_HISTORY_DATE
  !!$
  cl_tmp='HISTORY_PARAMETER'
  !!$  HISTORY_PARAMETER(N_HISTORY, N_PROF, STRING16)
  il_size=16
  CALL MIOL_read_field_NC(cl_file,cl_tmp,clpa_HISTORY_PARAMETER,il_size)
  !WRITE (0,*) 'TEST 11 HISTORY_PARAMETER  ok : ',clpa_HISTORY_PARAMETER
  cl_tmp='HISTORY_INSTITUTION'
!!$  HISTORY_INSTITUTION(N_HISTORY, N_PROF, STRING4)
  il_size=4
  CALL MIOL_read_field_NC(cl_file,cl_tmp,clpa_HISTORY_INSTITUTION,il_size)
!  WRITE (0,*) 'TEST 12 HISTORY_INSTITUTION  ok : ',clpa_HISTORY_INSTITUTION

PRINT *,'Test char 3D OK                                        '

!!$  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!!$  ! 4D !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cl_tmp='STATION_PARAMETERS'
!!$ STATION_PARAMETERS(N_PROF, N_PARAM, STRING16)
 il_size=16
 CALL MIOL_read_field_NC(cl_file,cl_tmp,clpa_STATION_PARAMETER,il_size)
! WRITE (0,*) 'TEST 13 STATION PARAMETER ok : ',clpa_STATION_PARAMETER
  cl_tmp='PARAMETER'
!!$  PARAMETER(N_PROF, N_CALIB, N_PARAM, STRING16)
  il_size=16
  CALL MIOL_read_field_NC(cl_file,cl_tmp,clpa_PARAMETER,il_size)
!  WRITE (0,*) 'TEST 13 PARAMETER ok : ',clpa_PARAMETER
  cl_tmp='SCIENTIFIC_CALIB_EQUATION'
!!$  SCIENTIFIC_CALIB_EQUATION(N_PROF, N_CALIB, N_PARAM, STRING256)
  il_size=256
  CALL MIOL_read_field_NC(cl_file,cl_tmp,clpa_SCI,il_size)
!  WRITE (0,*) 'TEST 14  SCIENTIFIC_CALIB_EQUATION ok : ',clpa_SCI
  cl_tmp='CALIBRATION_DATE'
!!$  CALIBRATION_DATE(N_PROF, N_CALIB, N_PARAM, DATE_TIME)
  il_size=1
  CALL MIOL_read_field_NC(cl_file,cl_tmp,clpa_calib,il_size)
!  WRITE (0,*) 'TEST 15  CALIBRATION_DATE ok : ',clpa_calib
  CALL MIOL_close_file_NC(il_filein)

PRINT *,'Test Char 4D OK                                        '
WRITE (0,*) '---------------------------------------------------------'
WRITE (0,*) '               Test Read Char OK                         '
 
END PROGRAM Test_CHAR_MFT
