!*======================================================================
!>\brief   ***  MODULE Read_DATA  ***
!!   \n   Ce module réalise la lecture des différents              
!!   \n  fichiers MERCATOR, dimg, fichiers moorings des differents protos correspondant aux class2 MERSEA      
!!   \date Octobre 2008
!!  \version 3.5
!<                                                                           
!======================================================================
!
MODULE MCAL_READ_DATA

   
!!
PRIVATE
PUBLIC :: lec_2Dpsy2,read_mooring_oper
!!
CONTAINS
!> \brief
!!
!!**** lec_2Dpsy2 \n
!!
!!     Purpose:\n
!!     --------\n
!!       Lecture des fichiers DIMG 2D \n
!!
!!     Input : \n
!!     ------ \n
!!      
!!      @param cd_filename : nom du fichier d'entrée
!!      @param rtab2d  : tableau lu
!!      @param il_indice : indice indiquant le nom de variable à lire
!!       @param il_ier   : variable de sortie d'erreur
!!
!!  \n     liste des variables psy2v2
!!  \n     --------------------------
!! \n level 1:  taux(ji,jj) * umask(ji,jj,1) zonal stress in N.m-2
!! \n  level 2:  tauy(ji,jj) * vmask(ji,jj,1) meridional stress in N. m-2
!! \n  level 3:   q   (ji,jj) + qsr(ji,jj)    total heat flux (W/m2)
!! \n  level 4:   emp (ji,jj)                 E-P flux (mm/day)
!! \n  level 5:  tb  (ji,jj,1)                model SST (degree C)
!! \n  level 6:  bsfb(ji,jj)                  streamfunction (m**3/s)
!! \n  level 7:  qsr (ji,jj)                  solar flux (W/m2)
!! \n  level 8:  qrp (ji,jj)                  relax component of T flux.
!! \n  level 9:  erp (ji,jj)                  relax component of S flux
!! \n  level 10: mld(ji,jj)                   turbocline depth
!! \n  level 11: mlp(ji,jj)                   mixed layer depth
!! \n  level 12: freeze (ji,jj)              Ice cover (1. or 0.)
!! \n  level 13: sst(ji,jj)                  the observed SST we relax to.
!! \n  level 14: gps(ji,jj)                  the surface pressure (m).     (ssh(:,:) = gps(:,:)/gravity)    g=9.806649999999999  m.s-2
!! \n .......   peu d'importance
!! \n .......   peu d'importance
!! \n  level 17: hbar(ji,jj)   hauteur barotrope
!! \n  level 18: hdyn(ji,jj)  hauteur barocline
!! \n  level 19: mxlb(ji,jj)  base de la couche mélangée
!!
!!     Output : \n
!!
!!***  Method: \n
!!     ------- \n
!!     moyenne simple par jour \n
!!     Externals: \n
!!     ---------- \n
!!     History: \n
!!     -------- \n
!!      Version    Programmer      Date            Description \n
!!      -------    ----------      ----            -----------  \n 
!!     1.0        M. Benkiran     2006                              \n 
!!     1.1        C.REGNIER        Mars 2007       Validation plateform \n
!<
!*--------------------------------------------------------------------------
!
  SUBROUTINE lec_2Dpsy2(cd_filename,rda_tab2d,il_indice,il_ier)
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
  INTEGER, PARAMETER :: IP_jpkglo=43, IP_jpiglo=1022, IP_jpjglo=1288
  INTEGER(KIND=4)    :: il_ji,il_jj
  INTEGER(KIND=4)    :: il_jk,il_jt,il_iversion
  INTEGER(KIND=4)    :: il_ibloc,il_jpbyt,il_nrecl4,il_irecl
  INTEGER(KIND=4)    :: il_jpi_read,il_jpj_read,il_jpk_read,il_nt_read,il_ndim_read,il_irecl_read
  INTEGER(KIND=4)    :: il_nt
!
  REAL(KIND=4)       :: rl_x1_read,rl_y1_read,rl_dx_read,rl_dy_read,rl_spval_read
  REAL(KIND=4),DIMENSION(IP_jpiglo,IP_jpjglo,IP_jpkglo)       :: rda_dataOUT
  REAL,DIMENSION(43) :: rla_gdept_read
  REAL,DIMENSION(20) :: rla_ztimm_read
  CHARACTER(len=80)  :: cl_text_read
  CHARACTER(len=4)   :: cl_ver_read

!
!**   0.3 Dummy variables
!
  INTEGER(KIND=4),INTENT(IN)                                    :: il_indice,il_ier
  CHARACTER(len=80),INTENT(IN)                                  :: cd_filename
  REAL(KIND=4),DIMENSION(IP_jpiglo,IP_jpjglo), INTENT(OUT)      :: rda_tab2d
 
!
!*-------------------------------------------------------------
!
!
  il_ibloc=4096*4
  il_jpbyt=4
  il_nrecl4=il_ibloc* ( (IP_jpiglo*IP_jpjglo*il_jpbyt-1) / il_ibloc + 1)
  il_irecl=il_nrecl4
  print *,'nrecl:', il_nrecl4
!
  open (801,FILE=cd_filename,form='unformatted',access='direct',recl=il_nrecl4)
  
      read(801,REC=1) cl_ver_read,cl_text_read,il_irecl_read,il_jpi_read,il_jpj_read, &
                       il_jpk_read,il_nt_read,il_ndim_read,rl_x1_read,rl_y1_read,rl_dx_read,rl_dy_read,rl_spval_read, &
                       (rla_gdept_read(il_jk), il_jk=1 , 43), &
                       (rla_ztimm_read(il_jt), il_jt=1 , il_nt) 
   
       print *, 'clver ', cl_ver_read
       if (cl_ver_read(1:2) .eq. '@!') then
          read(cl_ver_read(3:4),*) il_iversion
       else
          il_iversion=0
        print *,'Problème sur le fichier'
       endif
      ! write(0,*) cl_ver_read,cl_text_read,irecl_read,il_jpi_read,il_jpil_j_read, &
       !                il_jpk_read,il_nt_read,il_ndim_read,rl_x1_read,rl_y1_read,rl_dx_read,rl_dy_read,rl_spval_read, &
        !               (rla_gdept_read(il_jk), il_jk=1 , 43), &
        !               (rla_ztimm_read(il_jt), il_jt=1 , il_nt) 

         write(0,*)
         write(0,*)'Nb pas de tps en i =',il_jpi_read
         write(0,*)'NB pas de temps en j =',il_jpj_read
         write(0,*)'Nombre de variables =',il_jpk_read
         write(0,*)
!
   DO il_jk = 1, il_jpk_read
    read(801,REC=il_jk+1) ((rda_dataOUT(il_ji,il_jj,il_jk),il_ji=1,IP_jpiglo),il_jj=1,IP_jpjglo)
    if(il_jk == il_indice) rda_tab2d(1:IP_jpiglo,1:IP_jpjglo) = rda_dataOUT(1:IP_jpiglo,1:IP_jpjglo,il_jk)
!
   END DO
21 continue
  CLOSE (801)
!
!*--------------------------------------------------------------------------
!
  END SUBROUTINE lec_2Dpsy2
!
!*======================================================================
!
!
!>\brief
!!**** read_mooring_oper
!!
!!     Purpose: \n
!!     --------  \n
!!     Lecture des fichiers OPA moorings haute résolution operationnel avec 7 jours de recouvrement
!!
!!     Input : \n
!!     ------  \n
!!      
!!      @param id_nbfile : nombre de fichier en entrée
!!      @param id_indi   : indice de lecture d'entrée du fichier
!!      @param id_indf   : indice de lecture de sortie du fichier
!!      @param id_nbtimestep : Allocation du nombre du pas de tps adequat exemple:3(tout les 8h)*7jrs*2+2jours
!!      @param cd_filename: nom du fichier d'entrée
!!      @param id_nlevel : nombre du niveau du modèle
!!      @param rda_jullim_int : vecteur des jours juliens en sortie
!!      @param rda_lon : vecteur de longitude
!!      @param rda_lat : vecteur de latitude
!!      @param rda_gps_int : vecteur de ssh modele en sortie
!!      @param id_nb_i_array : nombre de valeurs dans le vecteur de sortie
!!      @param rda_temperature_int : vecteur de temperature modele en sortie
!!      @param rda_salinity_int : vecteur de salinité modele en sortie
!!      @param rda_w_int : vecteur de vitesse verticale modele en sortie
!!      @param rda_u_int : vecteur de courant zonal modele en sortie
!!      @param rda_v_int : vecteur de courant méridien modele en sortie
!!      @param rda_avt_int : vecteur de avt modele en sortie
!!
!!     Output : \n
!!     ------ \n
!!      rda_adatrj,rda_jullim_int,id_ii,id_jj,rda_lon,rda_lat,rda_levelsdepth
!!      rda_temperature,rda_salinity,rda_w,rda_u,rda_v,rda_avt,rda_gps_int
!!      id_nb_i_array
!!
!!***  Method: \n
!!     ------- \n
!!     Lectures des fichiers moorings CLASS2 : psy2 et ps3v1 psy3v2
!!     Attention au niveaux de profondeurs pour ces différents modèles
!!     psy2v2 : 43 psy3v1 : 46 psy3v2 : 50 \n
!!     Externals: \n
!!     ---------- \n
!!     MCAL_SU_DATE \n
!!     History: \n
!!     --------  \n
!!      Version    Programmer      Date            Description \n
!!      -------    ----------      ----            ----------- \n
!!     1.0        M. Benkiran     2005                        \n
!!     1.1        G.Vinay          10/2005         Modif            \n
!!     1.1        C.REGNIER        Mars 2007       Validation plateform \n
!<
!*--------------------------------------------------------------------------
!
!
!
!*======================================================================
!
!
  SUBROUTINE read_mooring_oper(id_nbfile,id_indi,id_indf,id_nbtimestep,ID_nbtime,&
				cd_filename,id_nlevel,rda_jullim_int,rda_lon,rda_lat,&
				rda_gps_int,id_nb_i_array,rda_temperature_int,&
				rda_salinity_int,rda_w_int,rda_u_int,rda_v_int,rda_avt_int)
!
!**** read_mooring_oper
!
!     Purpose:
!     --------
!     Lecture des fichiers OPA moorings haute résolution operationnel avec 7 jours de recouvrement
!     ou 14 pour le rtr
!     Input : \\
!     ------
!      \begin{itemize}
!      \item id_nbfile
!      \item cd_filename
!
!     Output : \\
!     ------
!      rda_adatrj,rda_jullim_int,id_ii,id_jj,rda_lon,rda_lat,rda_levelsdepth
!      rda_temperature,rda_salinity,rda_w,rda_u,rda_v,rda_avt,rda_gps_int
!      id_nb_i_array
!
!***  Method:
!     -------
!     Lectures des fichiers moorings CLASS2 : psy2 et ps3v1 psy3v2
!     Attention au niveaux de profondeurs pour ces différents modèles
!     psy2v2 : 43 psy3v1 : 46 psy3v2 : 50
!     Externals:
!     ----------
!     SU_DATE
!     History:
!     --------
!      Version    Programmer      Date            Description
!      -------    ----------      ----            -----------
!     1.0        M. Benkiran     2005 
!     1.1        G.Vinay          10/2005         Modif
!     1.1        C.REGNIER        Mars 2007       Validation plateform
!*--------------------------------------------------------------------------
!
!
!**   0. DECLARATIONS
!        ------------
!
!**   0.1 Include files and modules
!
    USE MCAL_SU_DATE
    USE MIOL_util
    IMPLICIT NONE
!
!**   0.2 Local variables
!
  INTEGER                          :: ID_nbtime    ! Max de pas de tps pour 14jours 
  INTEGER, DIMENSION(ID_nbtime)               :: il_ikt,il_ndastp         
  INTEGER                                     :: il_ific
  INTEGER                                     :: il_jk,il_it,il_ik,il_i,il_ios  
  REAL(KIND=4),DIMENSION(ID_nbtime)           :: rla_rdt,rla_gphit,rla_glamt
  REAL(KIND=4),DIMENSION(ID_nbtime)           :: rla_jullim,rla_gps
  CHARACTER(LEN=255)                          :: cl_filename1
  REAL                                        :: rl_time
!
!**   0.3 Dummy variables
!  
  INTEGER                                      :: id_nbfile,id_nlevel,id_indi,id_indf,id_nbtimestep
  CHARACTER(LEN=255),DIMENSION(id_nbfile), INTENT(IN)              :: cd_filename
!
  INTEGER,INTENT(OUT)                                              :: id_nb_i_array
  INTEGER, DIMENSION(ID_nbtime)                                    :: id_ii,id_jj
  REAL(KIND=4), DIMENSION(:), POINTER                              :: rda_gps_int,rda_adatrj
  REAL(KIND=4), DIMENSION(:),ALLOCATABLE                           :: rda_tmp_gps_int,rda_tmp_adatrj
  REAL(KIND=8), DIMENSION(:), POINTER                              :: rda_jullim_int
  REAL(KIND=8), DIMENSION(:),ALLOCATABLE                           :: rda_tmp_jullim_int

  REAL(KIND=4),DIMENSION(:),ALLOCATABLE                            :: rda_tmp_lon,rda_tmp_lat
  REAL(KIND=4),DIMENSION(:),POINTER                                :: rda_lon,rda_lat
  REAL(KIND=4),DIMENSION(:),POINTER                                :: rda_levelsdepth
  REAL(KIND=4),DIMENSION(:),ALLOCATABLE                            :: rda_tmp_levelsdepth
  REAL(KIND=4),    DIMENSION(:,:),POINTER                          :: rda_salinity_int,rda_w_int,rda_u_int
  REAL(KIND=4),    DIMENSION(:,:),POINTER                          :: rda_temperature_int,rda_v_int,rda_avt_int
  REAL(KIND=4), DIMENSION(:,:),ALLOCATABLE                         :: rda_tmp_temperature,rda_tmp_salinity,rda_tmp_w,&
                                                                      rda_tmp_u,rda_tmp_v,rda_tmp_avt
  REAL(KIND=4),    DIMENSION(:,:),ALLOCATABLE                      :: rda_temperature,rda_salinity,rda_w,rda_u,rda_v,rda_avt

!
!*--------------------------------------------------------------------------
!
!
           ALLOCATE(rda_tmp_lon(id_nbtimestep))
           ALLOCATE(rda_tmp_lat(id_nbtimestep))
           ALLOCATE(rda_tmp_gps_int(id_nbtimestep))
           ALLOCATE(rda_tmp_jullim_int(id_nbtimestep))
           ALLOCATE(rda_tmp_adatrj(id_nbtimestep))
           ALLOCATE(rda_tmp_temperature(ID_nbtime,id_nlevel))
           ALLOCATE(rda_tmp_salinity(ID_nbtime,id_nlevel))
           ALLOCATE(rda_tmp_w(ID_nbtime,id_nlevel))
           ALLOCATE(rda_tmp_u(ID_nbtime,id_nlevel))
           ALLOCATE(rda_tmp_v(ID_nbtime,id_nlevel))
           ALLOCATE(rda_tmp_avt(ID_nbtime,id_nlevel))
           ALLOCATE(rda_tmp_levelsdepth(id_nlevel))
           ALLOCATE(rda_temperature(id_nbtimestep,id_nlevel))
           ALLOCATE(rda_salinity(id_nbtimestep,id_nlevel))
           ALLOCATE(rda_u(id_nbtimestep,id_nlevel))
           ALLOCATE(rda_v(id_nbtimestep,id_nlevel))
           ALLOCATE(rda_w(id_nbtimestep,id_nlevel))
           ALLOCATE(rda_avt(id_nbtimestep,id_nlevel))


           print *,'Nb Fichiers',id_nbfile,id_nbtimestep
           cl_filename1= 'output_file'
           id_nb_i_array=0
           DO il_i=1,id_nbfile
           CALL MIOL_getFreeLu(il_ific)
           OPEN(il_ific,file=cd_filename(il_i),form='UNFORMATTED',ACCESS='SEQUENTIAL', POSITION='rewind',IOSTAT=il_ios) 
                 print *,'Fichier::',cd_filename(il_i)
                  IF (il_ios /= 0) stop "Problème à l'ouverture OPA Mooring"
                  
                  print *,'TAILLE tableau',ID_nbtime
                  
                  DO il_it=1,ID_nbtime
                        ! if first record : load the full header PARAMETERs
                           IF(il_it.eq.1) then
                              READ(il_ific,END=20,IOSTAT=il_ios) rda_tmp_adatrj(il_it)
                              READ(il_ific,END=20,IOSTAT=il_ios) il_ikt(il_it),il_ndastp(il_it),id_ii(il_it),id_jj(il_it) 
                              READ(il_ific,END=20,IOSTAT=il_ios) rla_glamt(il_it)
                              READ(il_ific,END=20,IOSTAT=il_ios) rla_gphit(il_it)
                              READ(il_ific,END=20,IOSTAT=il_ios) rla_rdt(il_it)
                              IF(il_ios/= 0) stop "Problème à l'ouverture :: en tete"
                              PRINT *,'adatrj',rda_tmp_adatrj(il_it)
                              PRINT *,'ikt',il_ikt(il_it)
                              PRINT *,'ndastp',il_ndastp(il_it)
                              PRINT *,'ii',id_ii(il_it)
                              PRINT *,'jj',id_jj(il_it)
                              PRINT *,'glamt',rla_glamt(il_it)
                              PRINT *,'gphit',rla_gphit(il_it)
                                PRINT *,'rdt',rla_rdt(il_it)
                              rda_tmp_lon(il_i)=rla_glamt(il_it)
                              rda_tmp_lat(il_i)=rla_gphit(il_it)
                              ! rda_lon(il_i)=rla_glamt(il_it)
                              ! rda_lat(il_i)=rla_gphit(il_it)
                              ! load levels depth :
                              DO il_ik=1,id_nlevel-1
                                 READ(il_ific,END=20,IOSTAT=il_ios) rda_tmp_levelsdepth(il_ik)
                                 IF(il_ios/= 0) stop "Problème à l'ouverture ::rda_levelsdepth"
                              END DO

                              ! if following records :
                           ELSE 
                              READ(il_ific,END=20,IOSTAT=il_ios) rda_tmp_adatrj(il_it)
                              READ(il_ific,END=20,IOSTAT=il_ios) il_ikt(il_it),il_ndastp(il_it)
                              ! PRINT *,'adatrj',rda_tmp_adatrj(il_it)
                              ! PRINT *,'ikt',il_ikt(il_it)
                              ! PRINT *,'ndastp',il_ndastp(il_it)
                              ! PRINT *,'il_ios,ID_nbtime',il_ios,il_it
                              IF(il_ios/= 0) PRINT *,'Pb a l ouv::rda_adatrj(it),il_ikt(il_it),il_ndastp(il_it)  un seul pas de tps'
                          ENDIF
                           READ(il_ific,END=20,IOSTAT=il_ios)(rda_tmp_u(il_it,il_jk)          ,il_jk=1,id_nlevel-1)
                           READ(il_ific,END=20,IOSTAT=il_ios)(rda_tmp_v(il_it,il_jk)          ,il_jk=1,id_nlevel-1)
                           READ(il_ific,END=20,IOSTAT=il_ios)(rda_tmp_w(il_it,il_jk)          ,il_jk=1,id_nlevel-1)
                           READ(il_ific,END=20,IOSTAT=il_ios)(rda_tmp_temperature(il_it,il_jk),il_jk=1,id_nlevel-1)
                           READ(il_ific,END=20,IOSTAT=il_ios)(rda_tmp_salinity(il_it,il_jk),il_jk=1,id_nlevel-1)
                           READ(il_ific,END=20,IOSTAT=il_ios)(rda_tmp_avt(il_it,il_jk)        ,il_jk=1,id_nlevel-1)
                           READ(il_ific,END=20,IOSTAT=il_ios) rla_gps(il_it)
                           IF(il_ios/= 0) stop "Problème à l'ouverture gps"
                           !PRINT *,'il_ndastp(il_it)',il_ndastp(il_it)
                          ! On ne garde que les 7 premiers jours du fichier (1 mesure/8H*7jours) pour psy3v1
                          ! On ne garde que les 7 premiers jours du fichier (1 mesure/6H*7jours) pour psy3v2
                           IF (il_it.GE.id_indi.AND.il_it.LE.id_indf) THEN
				rl_time=rla_jullim(il_it)  
				CALL SU_DATE_CAL_STU50_2(il_ndastp(il_it),rl_time)
			      id_nb_i_array=id_nb_i_array+1
                              rda_tmp_gps_int(id_nb_i_array)=rla_gps(il_it)
                              rda_tmp_jullim_int(id_nb_i_array)=rla_jullim(il_it)!+rda_tmp_adatrj(il_it)
                           !   print *,'Jour',rda_tmp_jullim_int(id_nb_i_array),il_ndastp(il_it)
                              rda_temperature(id_nb_i_array,:)=rda_tmp_temperature(il_it,:)
                              rda_salinity(id_nb_i_array,:)=rda_tmp_salinity(il_it,:)
                              rda_u(id_nb_i_array,:)=rda_tmp_u(il_it,:)
                              rda_v(id_nb_i_array,:)=rda_tmp_v(il_it,:)
                              rda_w(id_nb_i_array,:)=rda_tmp_w(il_it,:)
                              rda_avt(id_nb_i_array,:)=rda_tmp_avt(il_it,:)
                           ENDIF
                        END DO
                          IF ( ALLOCATED (rda_tmp_temperature)) DEALLOCATE (rda_tmp_temperature)
                          IF ( ALLOCATED (rda_tmp_salinity)) DEALLOCATE (rda_tmp_salinity)
                          IF ( ALLOCATED (rda_tmp_u)) DEALLOCATE (rda_tmp_u)
                          IF ( ALLOCATED (rda_tmp_v)) DEALLOCATE (rda_tmp_v)
                          IF ( ALLOCATED (rda_tmp_w)) DEALLOCATE (rda_tmp_w)
                          IF ( ALLOCATED (rda_tmp_avt)) DEALLOCATE (rda_tmp_avt)
 
                          ALLOCATE(rda_levelsdepth(id_nlevel))
                          rda_levelsdepth=rda_tmp_levelsdepth
                          if (ALLOCATED(rda_tmp_levelsdepth)) DEALLOCATE(rda_tmp_levelsdepth)
                      
20 continue
                      CLOSE(il_ific)
                   ENDDO
                      ALLOCATE(rda_lon(id_nbfile))
                      rda_lon=rda_tmp_lon(1:id_nbfile)
                      IF ( ALLOCATED (rda_tmp_lon)) DEALLOCATE (rda_tmp_lon)     
                      ALLOCATE(rda_lat(id_nbfile))
                      rda_lat= rda_tmp_lat(1:id_nbfile)
                      IF ( ALLOCATED (rda_tmp_lat)) DEALLOCATE (rda_tmp_lat)     
                     
                      ALLOCATE(rda_gps_int(id_nb_i_array))
                      rda_gps_int=rda_tmp_gps_int
                      IF ( ALLOCATED (rda_tmp_gps_int)) DEALLOCATE (rda_tmp_gps_int)     
                      ALLOCATE(rda_jullim_int(id_nb_i_array))
                      rda_jullim_int=rda_tmp_jullim_int
                      IF ( ALLOCATED (rda_tmp_jullim_int)) DEALLOCATE (rda_tmp_jullim_int) 
                      ALLOCATE(rda_adatrj(id_nb_i_array) )
                      rda_adatrj=rda_tmp_adatrj
                      IF ( ALLOCATED (rda_tmp_adatrj)) DEALLOCATE (rda_tmp_adatrj) 
                      
                      ALLOCATE(rda_temperature_int(id_nb_i_array,id_nlevel))
                      rda_temperature_int=rda_temperature(1:id_nb_i_array,:)
                      IF (ALLOCATED(rda_temperature)) DEALLOCATE(rda_tmp_temperature)

                      ALLOCATE(rda_salinity_int(id_nb_i_array,id_nlevel))
                      rda_salinity_int=rda_salinity(1:id_nb_i_array,:)
                      IF ( ALLOCATED(rda_salinity)) DEALLOCATE(rda_salinity)
  
                      ALLOCATE(rda_w_int(id_nb_i_array,id_nlevel))
                      rda_w_int=rda_w(1:id_nb_i_array,:)
                      IF (ALLOCATED(rda_w))  DEALLOCATE(rda_w)
                        
                      ALLOCATE(rda_u_int(id_nb_i_array,id_nlevel))
                      rda_u_int=rda_u(1:id_nb_i_array,:)
                      IF ( ALLOCATEd(rda_u)) DEALLOCATE(rda_u)
                       
                      ALLOCATE(rda_v_int(id_nb_i_array,id_nlevel))
                      rda_v_int=rda_v(1:id_nb_i_array,:)
                      if(ALLOCATED(rda_v))  DEALLOCATE(rda_v)
                         
                      ALLOCATE(rda_avt_int(id_nb_i_array,id_nlevel))
                      rda_avt_int=rda_avt(1:id_nb_i_array,:)
                      if (ALLOCATED(rda_avt)) DEALLOCATE(rda_avt)
                                               
!
!*--------------------------------------------------------------------------
!
END SUBROUTINE read_mooring_oper
!
!
!*--------------------------------------------------------------------------
!*--------------------------------------------------------------------------
!
END MODULE MCAL_READ_DATA
