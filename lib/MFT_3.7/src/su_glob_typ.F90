MODULE SU_GLOB_TYP
IMPLICIT NONE
! *****************************************************************************
! *                                                                           *
! * Fichier : SU_GLOB_TYP.H                                                   *
! *                                                                           *
! *****************************************************************************
! *                                                                           *
! * ROLE   :                                                                  *
! *                                                                           *
! *    Constantes et types communs a tout CALVAL                              *
! *                                                                           *
! * FICHIERS A INCLURE AVANT CELUI-CI :                                       *
! *                                                                           *
! *    <aucun>                                                                *
! *                                                                           *
! *****************************************************************************
! *                                                                           *
! *                                      P.Sicard                             *
! *                                      CLS Space Oceanography Group         *
! *                                      13 Avril 1995                        *
! *                                      C.REGNIER Juin 2013 Conversion F90
! *                                                                           *
! *****************************************************************************

!-----------------------------------------------------------------------------!
!                              C O N S T A N T E S                            !
!-----------------------------------------------------------------------------!


!     Compte rendu operation
!     ----------------------
      INTEGER(KIND=4) ::      jc_su_glob_ok
      parameter     (jc_su_glob_ok  = 0)

      INTEGER(KIND=4) ::      jc_su_glob_nok
      parameter     (jc_su_glob_nok = 1)

!     Constantes Mathematiques
!     ------------------------

!          PI
!          --
      REAL(KIND=8) ::   dc_su_glob_pi
      parameter     (dc_su_glob_pi = 3.141592653589793D0)

!          Constante de Gravite
!          --------------------
      REAL(KIND=8) ::   dc_su_grav
      parameter      (dc_su_grav = 9.807D0)

!        Rotation de la TERRE
!          --------------------
      REAL(KIND=8) ::   dc_su_omega
      parameter      (dc_su_omega = 7.272205D-5)

!        Conversion Degree -> radian
!        ---------------------------
      REAL(KIND=8) ::   dc_su_glob_cdr
      parameter     (dc_su_glob_cdr = dc_su_glob_pi / 180.D+00)

!   Nombre maximum de 1/2 orbites dans un cycle
!   (prevu pour sous cycle 37 jours ERS-1)
!   -------------------------------------------
      INTEGER(KIND=2) ::      ec_su_glob_maxTracePerCyc
      parameter     (ec_su_glob_maxTracePerCyc = 1063)
 
!   Nombre maximum de mesures dans une 1/2 orbite
!   ---------------------------------------------
      INTEGER(KIND=2) ::      ec_su_glob_maxMesPerTrace
      parameter     (ec_su_glob_maxMesPerTrace = 4000)

!   Types d'altimetres reconnus
!   ---------------------------
      INTEGER(KIND=1) ::       yc_su_glob_alt_poseidon
      parameter (yc_su_glob_alt_poseidon = 0)

      INTEGER(KIND=1) ::       yc_su_glob_alt_topex
      parameter (yc_su_glob_alt_topex    = 1)

      INTEGER(KIND=1) ::       yc_su_glob_alt_ers1
      parameter (yc_su_glob_alt_ers1     = 2)

      INTEGER(KIND=1) ::       yc_su_glob_alt_ers2
      parameter (yc_su_glob_alt_ers2     = 3)

      INTEGER(KIND=1) ::       yc_su_glob_alt_geos
      parameter (yc_su_glob_alt_geos     = 4)

      INTEGER(KIND=1) ::       yc_su_glob_alt_qle1
      parameter (yc_su_glob_alt_qle1     = 5)

      INTEGER(KIND=1) ::       yc_su_glob_alt_qle2
      parameter (yc_su_glob_alt_qle2     = 6)

!   Type d'orbites pour les satellites
!   ----------------------------------
      character  ac_su_glob_sat_prograde        ! satellite prograde
      parameter (ac_su_glob_sat_prograde   = 'P')
      character  ac_su_glob_sat_retrograde      ! satellite retrograde
      parameter (ac_su_glob_sat_retrograde = 'R')

!   Nom des produits
!   ----------------
      CHARACTER(LEN=*) ::    tc_su_glob_nom_mixt            ! GDR-MIXT(OLD)
      parameter      ( tc_su_glob_nom_mixt  = 'MIXT')

      CHARACTER(LEN=*) ::    tc_su_glob_nom_nmix            ! GDR-MIXT(NEW)
      parameter      ( tc_su_glob_nom_nmix  = 'NMIX')

      CHARACTER(LEN=*) ::    tc_su_glob_nom_c1e1            ! CERSAT-1 ERS-1
      parameter      ( tc_su_glob_nom_c1e1  = 'C1E1')

      CHARACTER(LEN=*) ::    tc_su_glob_nom_c2e1            ! CERSAT-2 ERS-1
      parameter      ( tc_su_glob_nom_c2e1  = 'C2E1')

      CHARACTER(LEN=*) ::    tc_su_glob_nom_c2e2            ! CERSAT-2 ERS-2
      parameter      ( tc_su_glob_nom_c2e2  = 'C2E2')

      CHARACTER(LEN=*) ::    tc_su_glob_nom_geos            ! GEOSAT
      parameter      ( tc_su_glob_nom_geos  = 'GEOS')

      CHARACTER(LEN=*) ::    tc_su_glob_nom_qle1            ! QLOPR ERS-1
      parameter      ( tc_su_glob_nom_qle1  = 'QLE1')

      CHARACTER(LEN=*) ::    tc_su_glob_nom_qle2            ! QLOPR ERS-2
      parameter      ( tc_su_glob_nom_qle2  = 'QLE2')

      CHARACTER(LEN=*) ::    tc_su_glob_nom_gdrt            ! GDR TOPEX
      parameter      ( tc_su_glob_nom_gdrt  = 'GDRT')


!   périodes et pulsations de base suivant les satellites
!   -----------------------------------------------------

      REAL(KIND=8) ::    dc_su_glob_per_tp   ! période de base de Topex/Poseidon (secondes)
      PARAMETER (dc_su_glob_per_tp = 6745.72)                      
      REAL(KIND=8) ::    dc_su_glob_pul_tp   ! pulsation de base de Topex/Poseidon
      PARAMETER (dc_su_glob_pul_tp = 2*dc_su_glob_pi/dc_su_glob_per_tp)    
               
      REAL(KIND=8) ::    dc_su_glob_per_ers1 ! période de base ERS-1
      PARAMETER (dc_su_glob_per_ers1 = 6029.0)                           
      REAL(KIND=8) ::    dc_su_glob_pul_ers1 ! pulsation de base d'ERS-1
      PARAMETER (dc_su_glob_pul_ers1 = 2*dc_su_glob_pi/dc_su_glob_per_ers1) 
               
      REAL(KIND=8) ::    dc_su_glob_per_ers2 ! période de base ERS-2
      PARAMETER (dc_su_glob_per_ers2 = 6029.0)                                            
      REAL(KIND=8) ::    dc_su_glob_pul_ers2 ! pulsation de base d'ERS-2
      PARAMETER (dc_su_glob_pul_ers2 = 2*dc_su_glob_pi/dc_su_glob_per_ers2) 
               
      REAL(KIND=8) ::    dc_su_glob_per_geos ! période de base GEOSAT
      PARAMETER (dc_su_glob_per_geos = 6037.5)                   
      REAL(KIND=8) ::    dc_su_glob_pul_geos   ! pulsation de base de Geosat
      PARAMETER (dc_su_glob_pul_geos = 2*dc_su_glob_pi/dc_su_glob_per_geos) 
               
      
!   -----------------------------------------------------------------
!   Descriptifs des champs
!   Ce descriptif permet d'etre renseigne sur le contenu d'un champ.
!   Il est present dans les en-tetes des fichiers FMC, croisement, et
!   simplifie (AC) de la chaine CALVAL.
!   Si descriptif <0 - le champ n'est pas valide
!   sinon            - le champ a ete valide par la methode des seuils
!                      de la chaine VL
!   -----------------------------------------------------------------

      INTEGER(KIND=1) ::       yc_su_glob_descr_default          ! Non renseigne
      parameter (yc_su_glob_descr_default   =127)

      INTEGER(KIND=1) ::       yc_su_glob_descr_extr_gdr         ! Extrait GDR
      parameter (yc_su_glob_descr_extr_gdr  = -1)

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_hbaro         ! Algorithme calcul baro-inverse
      parameter (yc_su_glob_descr_al_hbaro  = -2)  

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_fiono         ! Filtrage correction Iono 
      parameter (yc_su_glob_descr_al_fiono  = -3)  ! Bifrequence (TOPEX only)

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_mshum         ! Modele de Shum (maree    
      parameter (yc_su_glob_descr_al_mshum  = -4)  ! ou effet de charge)    )

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_shmss1         ! Mise a jour MSS          
      parameter (yc_su_glob_descr_al_shmss1  = -5)  ! MSS0G1TP00000001.

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_bemc         ! Biais Etat Mer          
      parameter (yc_su_glob_descr_al_bemc = -6)  ! Formule CNES

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_bdalt         ! Biaisage distance        
      parameter (yc_su_glob_descr_al_bdalt  = -7)  ! altimetrique      

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_wind_mcw      ! Module du vent           
      parameter (yc_su_glob_descr_al_wind_mcw=-8)  ! (formule de Chelton)

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_bias_sig0     ! Biaisage sigma0 en fct.  
      parameter (yc_su_glob_descr_al_bias_sig0=-9) ! de l'altimetre et du numero
                                                   ! de cycle

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_bem4          ! Biais Etat Mer          
      parameter (yc_su_glob_descr_al_bem4   = -10) ! Formule BM4 (Gaspar+Ogor)

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_gjgm2         ! Geoide JGM2             
      parameter (yc_su_glob_descr_al_gjgm2  = -11) !                            

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_gosu          ! Geoide OSU              
      parameter (yc_su_glob_descr_al_gosu   = -12) !                            

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_eocls         ! Erreur d'orbite CLS     
      parameter (yc_su_glob_descr_al_eocls  = -13) !                            

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_synchro       ! Calcul par synchronisation
      parameter (yc_su_glob_descr_al_synchro =-14) ! avec le Radiomètre (QLOPR)  

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_jgm2_3        ! remplacement de l'orbite   
      parameter (yc_su_glob_descr_al_jgm2_3 = -15) ! JGM2 par JGM3 (NASA)        

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_odr           ! calcul de l'orbite à partir
      parameter (yc_su_glob_descr_al_odr    = -16) ! de fichier ODR (DELFT)      

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_bq1           ! calcul du biais d'état de mer
      parameter (yc_su_glob_descr_al_bq1    = -17) ! sur les QLOPR ERS-1         

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_bq2           ! calcul du biais d'état de mer
      parameter (yc_su_glob_descr_al_bq2    = -18) ! sur les QLOPR ERS-2         

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_mfes_95_2         ! Modele FES95.2 (maree    
      parameter (yc_su_glob_descr_al_mfes_95_2 = -19)  ! ou effet de charge)    )

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_mcsr_3_0          ! Modele CSR3.0  (maree    
      parameter (yc_su_glob_descr_al_mcsr_3_0  = -20)  ! ou effet de charge)    )

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_eorb_ters         ! Erreur d'orbite  
      parameter (yc_su_glob_descr_al_eorb_ters = -21)  ! ajustee avec TOPEX        

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_ellip_et         ! Ellipsoide d'ERS 
      parameter (yc_su_glob_descr_al_ellip_et = -22)  ! vers TOPEX                

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_tb_ers           ! TB corrigee par  
      parameter (yc_su_glob_descr_al_tb_ers   = -23)  ! algo al_tb_corr_ers       

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_wet_ers          ! correction tropo corrigee
      parameter (yc_su_glob_descr_al_wet_ers  = -24)  ! par algo al_wet_ers       

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_bc2e1           ! calcul du biais d'état de mer
      parameter (yc_su_glob_descr_al_bc2e1    = -25) ! sur les OPR ERS-1 

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_bc2e2           ! calcul du biais d'état de mer
      parameter (yc_su_glob_descr_al_bc2e2    = -26) ! sur les OPR ERS-2 

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_glace_ers       ! calcul du flag glace pour    
      parameter (yc_su_glob_descr_al_glace_ers= -27) ! les OPR ERS       

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_bath12          ! calcul bathy au 1/12eme 
      parameter (yc_su_glob_descr_al_bath12= -28)     

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_shmss_rap       ! Mise a jour MSS          
      parameter (yc_su_glob_descr_al_shmss_rap = -29)! MSS0G1TP00000002.

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_wind_fch        ! Module du vent           
      parameter (yc_su_glob_descr_al_wind_fch = -30) ! (formule de Freilich)

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_gjgm3         ! Geoide JGM3             
      parameter (yc_su_glob_descr_al_gjgm3  = -31) !                            

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_bmss          ! Biaisage surface        
      parameter (yc_su_glob_descr_al_bmss  = -32)  ! moyenne  

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_doppler          ! Distance altimétrique    
      parameter (yc_su_glob_descr_al_doppler  = -33)  ! corrigée de l'effet Doppler

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_hpoint           ! Vitesse radiale calculée 
      parameter (yc_su_glob_descr_al_hpoint   = -34)  !                    

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_bdatorb         ! Orbite corrigée du biais 
      parameter (yc_su_glob_descr_al_bdatorb= -35)   ! de datation 

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_bdatdalt        ! DALT corrigée du biais 
      parameter (yc_su_glob_descr_al_bdatdalt= -36)  ! de datation 

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_dryECMWF        ! Tropo seche calculee a 
      parameter (yc_su_glob_descr_al_dryECMWF= -37)  ! partir du modele centre Europeen

      INTEGER(KIND=1) ::       yc_su_glob_descr_al_bgeos           ! calcul du biais d'état de mer
      parameter (yc_su_glob_descr_al_bgeos   = -38)  ! sur les données GEOSAT 

      INTEGER(KIND=1) ::       yc_su_glob_descr_btrack_pos         ! Biais tracker Poseidon enleve
      parameter (yc_su_glob_descr_btrack_pos = -39)  ! au range et rajoute au bem      

      INTEGER(KIND=1) ::       yc_su_glob_descr_btrack_posret     ! Biais tracker Poseidon retracke
      parameter (yc_su_glob_descr_btrack_posret= -40)! - au range, + au bem       

      INTEGER(KIND=1) ::       yc_su_glob_descr_extrapol_wet       ! extrapolation tropo
      parameter (yc_su_glob_descr_extrapol_wet= -41)    
END MODULE SU_GLOB_TYP
