  PROGRAM Test_MFT_analytique
  !
  !**** 
  !
  !     Purpose :
  !     Programme de test pour MFT
  !
  !***  Method : Test de MIOL avec des fonctions analytiques
  !     Externals :
  !     Files :
  !     References:
  !
  !     History
  !     -------
  !      Version    Programmer    Date       Description
  !      ---------------------------------------------
  !       1.0       C.REGNIER    01/2008    cretion test
  !
  !*-------------------------------------------------------------
  !
  !
  !** ++  MODULES used
  USE MIOL
  USE MCAL
  USE netcdf
  !
  IMPLICIT NONE
  !
  !** ++  Local Declarations 
  !
  INTEGER(KIND=4)                                    :: il_nbdim,il_file_id,il_filein
  INTEGER(KIND=4)                                    :: il_i,il_j,il_k
  INTEGER(KIND=4)  ,DIMENSION(:),ALLOCATABLE         :: ila_dimlen
  INTEGER(KIND=4),DIMENSION(:,:,:),POINTER           :: ilpa_mask  
  INTEGER(KIND=4), DIMENSION(:), POINTER             :: ilpa_dimlen
  INTEGER(KIND=4)                                    :: il_offset,il_status
  INTEGER(KIND=2)                                    :: il_min,il_max
  INTEGER(KIND=2), DIMENSION(:,:,:,:),POINTER        :: ilpa_variable_4D
  INTEGER(KIND=2), DIMENSION(4)                      :: ila_specialvalue
  CHARACTER(LEN=255), DIMENSION(:), POINTER          :: clpa_dimname
  CHARACTER(LEN=255)                                 :: cl_maskfile,cl_file_out
  CHARACTER(LEN=255),DIMENSION(:),ALLOCATABLE        :: cla_dimname
  CHARACTER(LEN=255)                                 :: cl_lon,cl_lat,cl_temp,cl_depth,cl_mask
  REAL(KIND=4),DIMENSION(:,:),POINTER                :: rlpa_LONGITUDE,rlpa_LATITUDE
  REAL(KIND=4),DIMENSION(:),POINTER                  :: rlpa_depth
  REAL(KIND=8), DIMENSION(:,:,:,:), ALLOCATABLE      :: rla_tab,rla_tab2,rla_tab4
  REAL(KIND=8), DIMENSION(2)                         :: rla_att
  !REAL(KIND=4), DIMENSION(:,:,:), ALLOCATABLE        :: rla_tab3
  LOGICAL                                            :: ll_fic
  REAL(KIND=4), PARAMETER                            :: rp_PI=3.14159265359
  REAL(KIND=8)                                       :: rl_scale_fact,rl_offset 
  REAL(KIND=4)                                       :: rl_min,rl_max
  !
  !*-------------------------------------------------------------
  WRITE(0,*) '----------------------------------------------'
  WRITE(0,*) '                                                '
  WRITE(0,*) '  Etape 1 Lecture d un fichier '
  WRITE(0,*) '                                                '
  WRITE(0,*) '----------------------------------------------'
  cl_maskfile='/homelocal/cregnier/DATA_SGPS/static/origin_grids/ORCA025_PSY3V2R2_T.nc'
  !
  !** Lecture d'un  masque
  INQUIRE(FILE=cl_maskfile,EXIST=ll_fic)
  IF ( ll_fic ) THEN 
     WRITE (0,*) 'Masque ok',cl_maskfile
  ELSE
     WRITE(0,*) 'Fichier Mask inexistant @# {"}*!% deju! '
     STOP
  ENDIF

     !** Definition du nom des variables à lire 
     cl_lon='longitude'
     cl_lat='latitude'
     cl_depth='depth'
     cl_temp='temperature'
     cl_mask='mask'
    
     !** lecture des variables
     CALL MIOL_openr_file_NC(cl_maskfile, il_filein)
     CALL MIOL_inquire_dimensions_NC(il_filein,il_nbdim,clpa_dimname,ilpa_dimlen)
     WRITE (0,*) 'Dimensions :: ',ilpa_dimlen(1),ilpa_dimlen(2),ilpa_dimlen(3)
     CALL MIOL_read_field_NC(il_filein,cl_lon, rlpa_LONGITUDE)!,ila_mask_dimlon)
     WRITE (0,*) 'READ lon ok',shape(rlpa_LONGITUDE)
     CALL MIOL_read_field_NC(il_filein,cl_lat, rlpa_LATITUDE)
     WRITE (0,*) 'READ lat ok',shape(rlpa_LATITUDE)
     CALL MIOL_read_field_NC(il_filein,cl_depth, rlpa_depth)
     WRITE (0,*) 'READ depth ok',shape(rlpa_depth)
     !ALLOCATE(ilpa_mask(ilpa_dimlen(1),ilpa_dimlen(2),ilpa_dimlen(3)))
     CALL MIOL_read_field_NC(il_filein,cl_mask,ilpa_mask)
     WRITE (0,*) 'READ Mask ok',shape(ilpa_mask)
     CALL MIOL_close_file_NC(il_filein)

     !*-------------------------------------------------------------------------
     !
     !** Creation du fichier de sortie
     !
     !*-------------------------------------------------------------------------

     print *,' ---------Creation du fichier de sortie-------------------------------'
     
     il_nbdim=4
     ALLOCATE(cla_dimname(il_nbdim))
     ALLOCATE(ila_dimlen(il_nbdim))
     cla_dimname(1) = 'x'
     cla_dimname(2)=  'y'
     cla_dimname(3)=  'deptht'
     cla_dimname(4)=  'time_counter'
    
     ila_dimlen(1) = ilpa_dimlen(3)
     ila_dimlen(2) = ilpa_dimlen(2)
     ila_dimlen(3) = ilpa_dimlen(1)
     ila_dimlen(4) = 1 !NF90_UNLIMITED
     PRINT *,'Dimensions :: ',ilpa_dimlen(1),ilpa_dimlen(2),ilpa_dimlen(3)            
     cl_file_out='ORCA_analytic.nc'
     call MIOL_create_file_NC(TRIM(cl_file_out),&
                              il_nbdim,&
                              cla_dimname,&
                              ila_dimlen)

     WRITE (0,*) 'MIOL_create_file_NC OK'
     call MIOL_openw_file_NC(TRIM(cl_file_out), il_file_id)  
     call MIOL_write_field_NC (TRIM(cl_file_out),&
                               'longitude',&
                               'XY',&
                                rlpa_LONGITUDE)
     WRITE (0,*) ' MIOL_write_field_NC LON OK'
     call MIOL_write_field_NC (TRIM(cl_file_out),&
                               'latitude',&
                               'XY',&
                               rlpa_LATITUDE)
     WRITE (0,*) ' MIOL_write_field_NC LAT OK'
     call MIOL_write_field_NC (TRIM(cl_file_out),&
                               'depth',&
                               'Z',&
                               rlpa_depth)
     WRITE (0,*) ' MIOL_write_field_NC depth OK'

     ALLOCATE(rla_tab(ila_dimlen(1),ila_dimlen(2),ila_dimlen(3),1))
     ALLOCATE(rla_tab2(ila_dimlen(1),ila_dimlen(2),ila_dimlen(3),1))
     ALLOCATE(rla_tab4(ila_dimlen(1),ila_dimlen(2),ila_dimlen(3),1))
     
     rla_tab=0
     rla_tab2=0
     rla_tab4=0
   
  !** rla_tab sera une fonction analytique
    
     DO il_i=1,ila_dimlen(1)
        DO il_j=1,ila_dimlen(2)
           DO il_k=1,ila_dimlen(3)
              !** Fonction analytique 1
              rla_tab(il_i,il_j,il_k,1)=2-cos(rp_PI*(acos(cos(rlpa_LATITUDE(il_i,il_j))*cos(rlpa_LONGITUDE(il_i,il_j)))/1.2*rp_PI))
              !** Fonction analytique 2
              rla_tab2(il_i,il_j,il_k,1)=2+cos(rlpa_LATITUDE(il_i,il_j)**2)*cos(2*rlpa_LONGITUDE(il_i,il_j))          !** Fonction analytique 4
              rla_tab4(il_i,il_j,il_k,1)=cos(rlpa_LATITUDE(il_i,il_j))+((exp(cos(rlpa_LONGITUDE(il_i,il_j))))/(1+sin(rlpa_LATITUDE(il_i,il_j))**2))
           ENDDO
        ENDDO
        
     ENDDO
     rla_tab(:,:,:,1)=rla_tab(:,:,:,1)*ilpa_mask
     rla_tab2(:,:,:,1)=rla_tab2(:,:,:,1)*ilpa_mask
     rla_tab4(:,:,:,1)=rla_tab4(:,:,:,1)*ilpa_mask
     
     rl_min=-1
     rl_max=5
     CALL MCAL_compute_short(rla_tab4,&
                             ila_dimlen,&
                             rl_min,&
                             rl_max,&
                             ilpa_variable_4D,&
                             rl_scale_fact,&
                             rl_offset)

     DEALLOCATE(rla_tab4)
     il_min=MINVAL(ilpa_variable_4D)
     il_max=MAXVAL(ilpa_variable_4D)
     PRINT *,'Min ,Max :: ',il_min,il_max
     
     ! Definition Parametres
     rla_att(1)=rl_scale_fact
     rla_att(2)=REAL(il_offset)
     ila_specialvalue(1)= il_min
     ila_specialvalue(2)= il_max
     ila_specialvalue(3)= il_fillvalue
     ila_specialvalue(4)= il_fillvalue

     call MIOL_write_field_NC (TRIM(cl_file_out),&
                               'analytic_function3',&
                               'XYZT',&
                               ilpa_variable_4D,&
                               rla_att,&
                               ila_specialvalue)
     DEALLOCATE(ilpa_variable_4D, stat=il_status)
     PRINT *,'Fonction analytique ecrite'

     CALL MCAL_compute_short(rla_tab,&
                             ila_dimlen,&
                             rl_min,&
                             rl_max,&
                             ilpa_variable_4D,&
                             rl_scale_fact,&
                             rl_offset)

     DEALLOCATE(rla_tab)
     ! Definition Parametres
     rla_att(1)=rl_scale_fact
     rla_att(2)=REAL(il_offset)
     ila_specialvalue(1)= il_min
     ila_specialvalue(2)= il_max
     ila_specialvalue(3)= il_fillvalue
     ila_specialvalue(4)= il_fillvalue

     call MIOL_write_field_NC (TRIM(cl_file_out),&
                               'analytic_function1',&
                               'XYZT',&
                               ilpa_variable_4D,&
                               rla_att,&
                               ila_specialvalue)
     DEALLOCATE(ilpa_variable_4D, stat=il_status)

     PRINT *,'Fonction analytique ecrite'
      CALL MCAL_compute_short(rla_tab2,&
                             ila_dimlen,&
                             rl_min,&
                             rl_max,&
                             ilpa_variable_4D,&
                             rl_scale_fact,&
                             rl_offset)

      DEALLOCATE(rla_tab2)
     ! Definition Parametres
     rla_att(1)=rl_scale_fact
     rla_att(2)=REAL(il_offset)
     ila_specialvalue(1)= il_min
     ila_specialvalue(2)= il_max
     ila_specialvalue(3)= il_fillvalue
     ila_specialvalue(4)= il_fillvalue

     call MIOL_write_field_NC (TRIM(cl_file_out),&
                               'analytic_function2',&
                               'XYZT',&
                               ilpa_variable_4D,&
                               rla_att,&
                               ila_specialvalue)
     DEALLOCATE(ilpa_variable_4D, stat=il_status)

     PRINT *,'Fonction analytique ecrite'
     PRINT *,'----------------------------------'
     WRITE (0,*)' MIOL_write_field_NC Champs OK'
          
     CALL MIOL_close_file_NC(il_file_id)
     WRITE (0,*) 'Test OK'
     
   END PROGRAM Test_MFT_analytique
