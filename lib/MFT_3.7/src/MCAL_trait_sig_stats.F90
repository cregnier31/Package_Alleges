!> \brief
!!======================================================================
!!            ***  MODULE Traitement du signal & stats *** \n
!! Ce module realise plusieurs operations necessaires en traitement du signal
!! - filtrage de la serie 1D (ici hanning)
!! - boucher les trous d'une serie par des 9999
!! - interpolation lineaire simple
!! Il realise egalement des moyennes journalieres pour un signal donné
!! Pour ce qui est des stats il effectue les calculs statistiques classiques que sont
!! la moyenne, ecart type, calcul de rms,correlation...
!!                                                   
!! \date C.REGNIER  Fevrier 2008   Creation
!! \date C.REGNIER  Octobre 2008  
!! \version Integration MIOL V3.5
!! \date Mai 2013 
!! \version 3.5
!!======================================================================
!< 

MODULE MCAL_trait_sig_stats

IMPLICIT NONE
!!
PRIVATE :: nrerror
        INTEGER, PARAMETER :: SP = KIND(1.0)
	INTEGER, PARAMETER :: I4B = SELECTED_INT_KIND(9)
PUBLIC :: MCAL_dhanning,MCAL_bouche_serie,MCAL_dbouche_trous,MCAL_mean_day,MCAL_recherche_valeurs_2D,MCAL_tri_ins,MCAL_avevarsdev,&
       MCAL_moment,MCAL_corre,MCAL_autocorre,MCAL_find,MCAL_moyenne,MCAL_rms_2fields,MCAL_rms_1field,MCAL_sort_shell,MCAL_shapiro
!!
CONTAINS
    !> \brief
    !!**** MCAL_dhanning \n
    !!
    !!     Purpose:\n
    !!     --------\n
    !!      Filtre passe bas de hanning\n
    !!
    !!     Input : \n
    !!     ------ \n
    !!      @param rda_y: serie temporelle non filtrée
    !!      @param id_nbpts : nb de points de la serie
    !!      @param id_KMAX : demi largeur du filtre
    !!      
    !!     Output : \n
    !!     ------ \n
    !!      @param rda_yout : serie temporelle filtrée \n
    !!
    !!**  Method: \n
    !!     -------\n
    !!       2*KMAX+1 pts sont utilises pour chaque point ce qui correspond à la largeur du filtre
    !!       filtre. KMAX pts restent inchanges en debut
    !!       et fin de serie. \n
    !!           Gain                 Periode  \n
    !!            90%                 5*KMAX intervalles en y. \n
    !!            50%                 2*KMAX         \n
    !!            10%              1.25*KMAX        \n 
    !!     KMAX points sont retirés en début et fin de serie \n
    !!     Externals: \n
    !!     ---------- \n
    !!     History: \n 
    !!     --------\n
    !!   \n   Version    Programmer      Date            Description
    !!      -------    ----------      ----            ----------- \n
    !!      1.0        C.REGNIER        Mai 2007      issu d'un prog de T. Delcroix \n
    !<
    !*--------------------------------------------------------------------------
    !
    !  
    SUBROUTINE MCAL_dhanning(rda_y,rda_yout,id_nbpts,id_KMAX)
       !**   0. DECLARATIONS
    INTEGER(KIND=4),PARAMETER               ::  IP_NWMAX=5000
    INTEGER(KIND=4)                         ::  il_passe,il_kspy,il_i,il_k
    REAL(KIND=8)                            ::  rl_sumy
    REAL(KIND=8),DIMENSION(0:IP_NWMAX)      ::  rl_weight
    REAL(KIND=8)                            ::  rl_sumw,pi,un,deux
    !
    !**   0.3 Dummy variables
    !
    INTEGER(KIND=4),INTENT(IN)                      :: id_nbpts,id_KMAX
    REAL(KIND=4),DIMENSION(id_nbpts),INTENT(IN)     :: rda_y
    REAL(KIND=8),DIMENSION(id_nbpts),INTENT(OUT)    :: rda_yout(*)

    DATA pi /3.1415926535897932/, un /1/, deux /2/,&
         il_passe /0/   
 
    !*--------------------------------------------------------------------------
    !

!** Calcul des poids du filtre, la 1ere fois seulement.
        IF (il_passe.lt.2.or.id_KMAX.ne.il_kspy) THEN
            rl_sumw = 0.
            DO il_k=0,id_KMAX
                rl_weight(il_k) = un + cos(pi*il_k/id_KMAX)
                rl_sumw = rl_sumw + rl_weight(il_k)
             ENDDO
             rl_sumw = deux*rl_sumw-rl_weight(0)
             DO il_k = 0,id_KMAX
                rl_weight(il_k) = rl_weight(il_k)/rl_sumw
             ENDDO
            il_passe = 2
        ENDIF
         
!** Application du filtre.
        DO il_i = id_KMAX+un , id_nbpts-id_KMAX
            rl_sumy = 0.
            DO il_k = 1,id_KMAX
               rl_sumy = rl_sumy + rl_weight(il_k)*(rda_y(il_i-il_k)+rda_y(il_i+il_k))
            ENDDO
            rda_yout(il_i) = rl_weight(0)*rda_y(il_i) + rl_sumy
         ENDDO
         
         DO il_i = 1,id_KMAX
            rda_yout(il_i) = rda_y(il_i)
         ENDDO
         
         DO  il_i = id_nbpts-id_KMAX+1,id_nbpts
            rda_yout(il_i) = rda_y(il_i)
         ENDDO
         il_kspy=id_KMAX
         

!
!*--------------------------------------------------------------------------
!
 END SUBROUTINE MCAL_dhanning
!
!
!
!*======================================================================
!*======================================================================
! 
  !> \brief
  !!**** MCAL_bouche serie \n 
  !!
  !!     Purpose:  \n
  !!     -------- \n
  !!      bouche les trous d'une serie temporelle par des NaN en tenant compte 
  !!      de la frequence d'echantillonnage du signal \n
  !! 
  !!     Input : \n
  !!     ------ \n
  !!      
  !!      @param rda_time : vecteur de date 
  !!      @param rda_ssh  : vecteur de ssh
  !!      @param id_freq : frequence de sortie
  !!     Output : \n
  !!     ------ \n
  !!     @param rda_time : vecteur de date  \n
  !!     @param rlpa_ssh2 : vecteur de ssh  \n
  !!
  !!***  Method: \n
  !!     ------- \n
  !!     Externals: \n
  !!     ---------- \n
  !!     History: \n
  !!     -------- \n
  !!      Version    Programmer      Date            Description \n
  !!      -------    ----------      ----            ----------- \n
  !!      1.0        C.REGNIER        Juin 2007      Developpement Maregraphe \n
  !<
  !*--------------------------------------------------------------------------
  !
  !

SUBROUTINE MCAL_bouche_serie(rda_time,rlpa_time2,rda_ssh,rlpa_ssh2,id_freq,idpa_mask)
  !
  !**   0. DECLARATIONS
  INTEGER(KIND=4)                        :: il_n,il_nbval,il_values,il_taille,il_f,il_i

  REAL(KIND=4), DIMENSION(:),ALLOCATABLE :: rl_vec
  REAL(KIND=8)                           :: rl_diff,rl_diff2 
  REAL(KIND=4)                           :: seuil=1.e-2
  REAL(KIND=8)                           :: dmiss,dmiss2
  REAL(KIND=4), DIMENSION(:),ALLOCATABLE   :: rla_ssh2_tmp,rla_mask_tmp
  REAL(KIND=4), DIMENSION(:),ALLOCATABLE   :: rla_time2_tmp
  !
  !**   0.3 Dummy variables
  !
  INTEGER(KIND=4)                                 :: id_freq
  REAL(KIND=8), DIMENSION(:)                      :: rda_time
  REAL(KIND=4), DIMENSION(:)                      :: rda_ssh
  REAL(KIND=4), DIMENSION(:),POINTER              :: rlpa_ssh2
  INTEGER(KIND=4), DIMENSION(:),POINTER,OPTIONAL  :: idpa_mask
  REAL(KIND=8), DIMENSION(:),POINTER              :: rlpa_time2
  
  DATA dmiss/1.e35/,dmiss2/1.e30/
  !
  !*--------------------------------------------------------------------------
  !
  il_taille=size(rda_time)
  !** Nombre de valeurs théoriques en tenant compte de la frequence
  il_nbval=((rda_time(il_taille)-rda_time(1))+1)*(24/id_freq)

  ALLOCATE(rla_ssh2_tmp(il_nbval))
  ALLOCATE(rla_time2_tmp(il_nbval))
  ALLOCATE(rla_mask_tmp(il_nbval))
  rla_ssh2_tmp(:)=dmiss
  rla_time2_tmp(:)=dmiss
  rla_mask_tmp(:)=0
  il_n=2
  il_f=2
  rl_diff=rda_time(2)-rda_time(1)
  rla_time2_tmp(1)=rda_time(1)
  rla_ssh2_tmp(1)=rda_ssh(1)
  rla_mask_tmp(1)=1
  DO WHILE(il_n.LE.il_taille)
     rl_diff2=rda_time(il_n)-rda_time(il_n-1)
     IF (abs(rl_diff2-rl_diff).LT.seuil)THEN   
        rla_ssh2_tmp(il_f)=rda_ssh(il_n)
        rla_time2_tmp(il_f)=rda_time(il_n)
        rla_mask_tmp(il_f)=1
        il_f=il_f+1
        il_n=il_n+1
     ELSEIF (abs(rl_diff2-rl_diff).GT.seuil)THEN
        il_values=(int((rl_diff2/rl_diff)+0.1))*id_freq
        rla_ssh2_tmp(il_f:il_f+il_values-1)=dmiss
        rla_ssh2_tmp(il_f+il_values)=rda_ssh(il_n)
        rla_mask_tmp(il_f:il_f+il_values-1)=0
        rla_mask_tmp(il_f+il_values)=1
        ALLOCATE(rl_vec(il_values))
        DO il_i=1,il_values
        rl_vec(il_i)=rda_time(il_n-1)+rl_diff*il_i
        ENDDO
        rla_time2_tmp(il_f:il_f+il_values-1)=rl_vec(:)
        rla_time2_tmp(il_f+il_values)=rda_time(il_n)
        il_f=il_f+il_values
        il_n=il_n+1
        DEALLOCATE(rl_vec)
     ELSEIF (abs(rl_diff2).ge.dmiss2) THEN
        rla_time2_tmp(il_f)=rda_time(il_n)
        rla_ssh2_tmp(il_f)=rda_ssh(il_n)
        rla_mask_tmp(il_f)=1
        il_n=il_taille+1
     ENDIF
  ENDDO
 ALLOCATE(rlpa_time2(il_f-1))
 ALLOCATE(rlpa_ssh2(il_f-1))
 ALLOCATE(idpa_mask(il_f-1))
  rlpa_ssh2(:)=rla_ssh2_tmp(1:il_f-1)
  rlpa_time2(:)=rla_time2_tmp(1:il_f-1)
  idpa_mask(:)=rla_mask_tmp(1:il_f-1)
DEALLOCATE(rla_ssh2_tmp) 
DEALLOCATE(rla_time2_tmp)

!
!*--------------------------------------------------------------------------
!
  END SUBROUTINE MCAL_bouche_serie
!
!*======================================================================
!*======================================================================
!> \brief
 !!****dbouche_trous \n
 !! 
 !!    Purpose: \n
 !!    -------- \n
 !!   Bouchage d'une serie temporelle par interpolation lineaire \n
 !!
 !!     Input : \n
 !!     ------ \n
 !!      
 !!      @param rda_time : vecteur de date 
 !!      @param rda_ssh  : vecteur de ssh
 !!      @param id_freq : frequence de sortie
 !!     Output : \n
 !!     @param rda_time : vecteur de date 
 !!     @param rda_ssh2 : vecteur de ssh  \n
 !!
 !!***  Method: \n
 !!     ------- \n
 !!     Externals:  \n
 !!     ----------  \n
 !!     History: \n
 !!     -------- \n
 !!      Version    Programmer                                        Date            Description \n
 !!      -------    ----------                                       ----            -----------\n 
 !!      1.0        C.REGNIER adapt dev Stage avec T.Delcroix 2005   Juin 2007       Developpement Maregraphe\n
 !<
 !*--------------------------------------------------------------------------

       SUBROUTINE MCAL_dbouche_trous(rda_dZ,rlpa_dZBO,id_ndim,id_kmax)
  !
  !**   0. DECLARATIONS
  INTEGER(KIND=4)                        :: il_ideb ,il_k,il_i
  INTEGER(KIND=4)                        :: il_ipremier,il_nimoy
  REAL(KIND=4)                           :: rl_dmiss
  REAL(KIND=4)                           :: rl_x1,rl_x2,rl_RMOY,rl_y1,rl_y2,rl_a,rl_b
  !
  !**   0.3 Dummy variables
  !
  INTEGER(KIND=4)                               :: id_ndim,id_kmax
  REAL(KIND=4), DIMENSION(id_ndim)              :: rda_dZ
  REAL(KIND=4), DIMENSION(:),POINTER            :: rlpa_dZBO
  DATA rl_dmiss/1.e35/

  !
  !*--------------------------------------------------------------------------
  !
  ALLOCATE(rlpa_dZBO(id_ndim))
        do  il_k=1,id_ndim
           rlpa_dZBO(il_k)=rda_dZ(il_k)
        enddo
        il_ideb=0
        if(rlpa_dZBO(1).eq.rl_dmiss) il_ideb=1
!
!**     Premier a rl_dmiss
!
        do 210 il_i=2,id_ndim
           if(il_ideb.eq.1.and.rlpa_dZBO(il_i).ne.rl_dmiss)then
             il_nimoy=1
             rl_RMOY=rlpa_dZBO(il_i)
	      if(id_kmax.gt.1) then
	       do il_k=il_i+1,il_i+id_kmax-1
                 if(il_k.gt.id_ndim) goto 2151
                 if(rlpa_dZBO(il_k).ne.rl_dmiss) then
                   rl_RMOY = rl_RMOY + rlpa_dZBO(il_k)
                   il_nimoy    = il_nimoy + 1
                 else
                    goto 2151
                 endif
               enddo
	      endif
2151          do  il_k=1,il_i-1
                rlpa_dZBO(il_k) = rl_RMOY/dble(float(il_nimoy))
              enddo
              il_ideb=0
              goto 210
            endif
!**          Trou quelconque

            if(il_ideb.eq.0.and.rlpa_dZBO(il_i).eq.rl_dmiss)then
               il_ideb=2
               il_ipremier=il_i
               goto 210
            endif
            if(il_ideb.eq.2.and.rlpa_dZBO(il_i).ne.rl_dmiss)then
               rl_x1=dble(float(il_ipremier-1))
               rl_x2=dble(float(il_i))
!**           Avant le trou
               il_nimoy=1
               rl_RMOY=rlpa_dZBO(il_ipremier-1)
	        if(id_kmax.gt.1) then
                 do 217 il_k=il_ipremier-2 , il_ipremier-id_kmax ,-1
                   if(il_k.eq.0) goto 2171
                   if(rlpa_dZBO(il_k).ne.rl_dmiss) then
                       rl_RMOY = rl_RMOY + rlpa_dZBO(il_k)
                       il_nimoy    = il_nimoy + 1
                   else
                      goto 2171
                   endif
217               continue
		 endif
2171            rl_y1=rl_RMOY/dble(float(il_nimoy)) 

!                             Apres le trou 
                il_nimoy=1
                rl_RMOY=rlpa_dZBO(il_i)
		 if(id_kmax.gt.1) then
                  do  il_k=il_i+1 , il_i+id_kmax-1
                    if(il_k.gt.id_ndim) goto 2181
                    if(rlpa_dZBO(il_k).ne.rl_dmiss) then
                        rl_RMOY = rl_RMOY + rlpa_dZBO(il_k)
                        il_nimoy    = il_nimoy + 1
                    else
                       goto 2181
                    endif
                  enddo
		 endif
2181            rl_y2=rl_RMOY/dble(float(il_nimoy))

                rl_a=(rl_y2-rl_y1)/(rl_x2-rl_x1)
                rl_b=(rl_y1*rl_x2-rl_y2*rl_x1)/(rl_x2-rl_x1)

                do  il_k=il_ipremier,il_i-1
                     rlpa_dZBO(il_k)=rl_a*dble(float(il_k))+rl_b
                enddo
                il_ideb=0
              endif

!**            Les derniers sont a rl_dmiss
!
              if(il_ideb.eq.2.and.il_i.eq.id_ndim)then
                il_nimoy=1
                rl_RMOY=rlpa_dZBO(il_ipremier-1)
		  if(id_kmax.gt.1) then
                   do 219 il_k=il_ipremier-2 , il_ipremier-id_kmax ,-1
                     if(il_k.le.0) goto 2191
                     if(rlpa_dZBO(il_k).ne.rl_dmiss) then
                       rl_RMOY = rl_RMOY + rlpa_dZBO(il_k)
                       il_nimoy    = il_nimoy + 1
                     else
                       goto 2191
                     endif
219                continue
		  endif
2191            do  il_k=il_ipremier,id_ndim
                   rlpa_dZBO(il_k) = rl_RMOY/dble(float(il_nimoy))
                enddo
             endif

 210     continue
!**                                  Le dernier est a rl_dmiss
         if(rlpa_dZBO(id_ndim).eq.rl_dmiss) rlpa_dZBO(id_ndim)=rlpa_dZBO(id_ndim-1)
!
!
!*--------------------------------------------------------------------------
!
       END SUBROUTINE MCAL_dbouche_trous
!
!*======================================================================
!*======================================================================
!

  !> \brief
  !!**** MCAL_mean_day \n
  !!
  !!     Purpose: \n 
  !!     -------- \n
  !!      Réalisation d'une serie temporelle moyennée chaque jour avec le masque qui lui est associé
  !!      le but est de calculer des moyennes temporelles realistes en fonction
  !!      du nombre de données presentes sur la série pour cela on utilise
  !!      une serie temporelle de mask qui indique si oui ou non la valeur de la
  !!      serie existe ; si oui elle compte dans le calcul de la moyenne sinon elle est enlevée
  !!      Lorsque qu'il y a des trous dans la serie la valeurs moyenne sera la valeur de la 
  !!      moyenne interpolée cependant la serie temporelle journaliere de masque prendra
  !!      en compte cette absence de données en remplissant par un zero. La premiere application de
  !!      cette routine est de calculer une serie temporelle moyennée chaque jour d'une serie temporelle
  !!       marégraphe possedant une donnée toute les heures  en vue de la comparer à une serie temporelle
  !!      modele qui possede, elle,  une seule valeur journaliere. 
  !!
  !!     Input : \n
  !!     ------ \n
  !!      
  !!      @param rda_time : vecteur de date 
  !!      @param rda_ssh  : vecteur de ssh
  !!      @param id_ntime : longueur de la serie temporelle à moyenner
  !!      @param ida_mask : valeur du mask d'entree de la serie temporelle
  !!     Output : \n
  !!     ------ \n
  !!     @param rda_time_daily : vecteur moyenne du jour
  !!     @param rda_ssh_daily  : vecteur moyenne journalière de la ssh
  !!     @param id_n_day       : nombre de jours dans le vecteur moy jour
  !!     @param idpa_mask_daily: valeur du mask de sortie de la serie temporelle
  !!
  !!***  Method: \n
  !!     -------  \n
  !!     moyenne simple par jour
  !!     Externals: \n
  !!     ---------- \n
  !!     History: \n
  !!     -------- \n
  !!      Version    Programmer      Date            Description \n 
  !!      -------    ----------      ----            ----------- \n
  !!      1.0        C.REGNIER        Mars 2007      Plateforme validation \n
  !<
  !*--------------------------------------------------------------------------
  !
  !
SUBROUTINE MCAL_mean_day(rda_time,rda_ssh,id_ntime,rdpa_time_daily,rdpa_ssh_daily,id_n_day,rd_moyenne,ida_mask,idpa_mask_daily)
 
  !**   0. DECLARATIONS
  INTEGER(KIND=4)                              :: il_i,il_j,il_k,il_1,il_2,il_f
  INTEGER(KIND=4)                              :: il_err,il_n_day_moy,il_n_year,il_nb_jour,il_f1
  REAL(KIND=4), DIMENSION(1)                   :: rla_sum
  REAL(KIND=4), DIMENSION(id_ntime)            :: rla_t
  REAL(KIND=4), DIMENSION(:),ALLOCATABLE       :: rla_ssh2
  REAL(KIND=4), DIMENSION(1)                   :: rla_moy,rla_somme,rla_somme2,rla_somme3,rla_moy2
  REAL(KIND=8)                                 :: rl_dmiss
  REAL(KIND=4),TARGET, DIMENSION(id_ntime)     :: rla_ssh_daily
  REAL(KIND=4),TARGET, DIMENSION(id_ntime)     :: rla_mask_tmp
  REAL(KIND=4),TARGET, DIMENSION(id_ntime)     :: rla_time_daily
  LOGICAL                                      :: ll_flag
  !
  !**   0.3 Dummy variables
  !
  REAL(KIND=4), DIMENSION(:),INTENT(IN)                        :: rda_ssh
  REAL(KIND=8), DIMENSION(:),INTENT(IN)                        :: rda_time
  INTEGER(KIND=4),INTENT(IN)                                   :: id_ntime
  INTEGER(KIND=4), DIMENSION(id_ntime),OPTIONAL,INTENT(IN)     :: ida_mask
 !* OUT
  REAL(KIND=4), DIMENSION(:),POINTER                         :: rdpa_ssh_daily, rdpa_time_daily 
  REAL(KIND=4),INTENT(OUT)                                   :: rd_moyenne
  INTEGER(KIND=4), OPTIONAL,INTENT(OUT)                      :: id_n_day
  INTEGER(KIND=4), DIMENSION(:),POINTER,OPTIONAL             :: idpa_mask_daily 
  DATA rl_dmiss/1.e35/
  !
  !*--------------------------------------------------------------------------
  !
  ALLOCATE(rla_ssh2(id_ntime),stat=il_err)
  !  
  if (il_err /= 0) then
     print *,"Erreur d'allocation"; stop 4
  endif
  IF (id_ntime.lt.1) Print *,'Pas assez de valeurs'
  rla_ssh2(:)= rda_ssh(:)
  !*------------------------------------------------------------------
  !**  Moyennes Journalières
  !*-------------------------------------------------------------------
  !     
  rla_t=aint(rda_time)
  il_nb_jour=int(rda_time(id_ntime))-int(rda_time(1))
  rla_somme=0
  rla_somme2=0 
  rla_somme3=0
  il_f=0
  DO il_i=2,id_ntime
     IF(il_i.EQ.2) THEN
        il_k=1
        il_j=1
        il_f=1
        rla_somme=rla_ssh2(il_i-1)
        rla_somme2=rda_time(il_i-1)
        rla_somme3=rla_ssh2(il_i-1)
     ENDIF
     ll_flag = .TRUE.
     DO WHILE (ll_flag)
        !** Dans ce cas la la valeur suivante correspond à un jour différent 
        IF(rla_t(il_i).NE.rla_t(il_i-1)) THEN
           IF(il_j.EQ.1) THEN    ! il_j=1 lorsque le 1er jour est passé
              il_1=1
           ELSE
              il_1=il_2+1 !1er cas il_1=1 et il_2=il_i-1 sinon il_1=il_2+1
           ENDIF
           il_2=il_i-1 ! il_2 
           IF( COUNT ( ida_mask(il_1:il_2) .EQ. 1) > 1) THEN 
              !*si dans toutes les valeurs correspondant au jour concerné il y a au moins 1 valeur valable (au moins 1 valeur mask=1)
              rla_mask_tmp(il_j)=1             
              !* il_f1 correspond au nombre de valeurs valides dans le calcul de la moyenne journalieres (nb valeur mask =1)
              il_f1=COUNT ( ida_mask(il_1:il_2) .EQ. 1)
              rla_moy=rla_somme3/(il_f) ! moyenne des valeurs valides (mask=1) **1
              !* Serie possedant des valeurs de masque positives pour ce jour la
              IF (il_f /= il_f1) THEN 
                 PRINT *,'Pb sur le nombre de valeurs valables dans le calcul de la moyenne journaliere'
                 STOP !** Cas où on trouve un nombre de valeurs differentes du nombres additionné dans le test si dessous
              ENDIF
           ELSEIF ( COUNT ( ida_mask(il_1:il_2) .EQ. 1) < 1) THEN
              !* Serie ne possedant pas de valeurs de masque positive pour ce jour la
              rla_mask_tmp(il_j)=0
              rla_moy=rla_somme/il_k
           ENDIF
           !* Pour calculer le jour moyen on tient compte de tous les jours
           rla_moy2=rla_somme2/il_k
           rla_ssh_daily(il_j)=rla_moy(1) ! calculé plus haut cf **1
           rla_time_daily(il_j)=int(rla_moy2(1)) ! on prend la valeur entiere pour le temps
           !** Reinitialisation des variables pour le jour d'après
           ll_flag = .FALSE.
           il_k=1 
           il_f=1
           !** Reinitialisation de la somme3 pour la ssh
           IF(ida_mask(il_i).EQ.1) THEN 
              rla_somme3=rla_ssh2(il_i)
              il_f=1
           ELSE
              rla_somme3=0
              il_f=0
           ENDIF
           !** Reinitialisation de la somme2 pour le temps
           rla_somme2=rda_time(il_i)
           rla_somme =rla_ssh2(il_i)
           !** On incremente le nombre de jour
           il_j=il_j+1
           !
           !** Dans ce cas la la valeur suivante correspond à un meme jour 
        ELSEIF(rla_t(il_i).EQ.rla_t(il_i-1)) THEN 
           rla_somme=rla_somme+rla_ssh2(il_i)
           rla_somme2=rla_somme2+rda_time(il_i)
           il_k=il_k+1
           IF(ida_mask(il_i).EQ.1) THEN
              rla_somme3=rla_somme3+rla_ssh2(il_i)
              il_f=il_f+1
           ENDIF
        ENDIF
        EXIT
     ENDDO
  ENDDO
  ! ** Ici allocation de deux tableaux qui contiendront les valeurs de rda_ssh et rda_time
  !    le signe = indique que chaque cas de ces tableau va pointer sur la cas du 
  !    tableau target correspondant pas la peine de  mettre => car le tableau est alloué   
  ALLOCATE(rdpa_ssh_daily(il_j-1)) ! petit detail on prend il_j -
  ALLOCATE(rdpa_time_daily(il_j-1))
  ALLOCATE(idpa_mask_daily(il_j-1))
  rdpa_ssh_daily=rla_ssh_daily(1:il_j-1)
  rdpa_time_daily=rla_time_daily(1:il_j-1)
  idpa_mask_daily=rla_mask_tmp(1:il_j-1)
  !*-----------------------------
  !** Mean ssh
  !*-------------------------

  id_n_day=il_j

  il_n_year=int((id_n_day+0.5)/365.25)
  IF (il_n_year.le.0) THEN
     print *,'Moins de 1 an de données'
  ENDIF
  print *,'nb annee',il_n_year,id_n_day
  il_n_day_moy=il_n_year*365 
  rla_sum=0

  DO il_i=1,il_n_day_moy
     rla_sum=rla_sum+rla_ssh_daily(il_i)    
  ENDDO

  rd_moyenne=rla_sum(1)/il_n_day_moy
  IF (il_n_day_moy.EQ.0) rd_moyenne=0
  !
  IF(ALLOCATED(rla_ssh2))  DEALLOCATE(rla_ssh2)
  !
  !*--------------------------------------------------------------------------
  !
END SUBROUTINE MCAL_mean_day
!
!*=============================================================================================
!*=============================================================================================
!	
          !> \brief
          !!**** MCAL_recherche_valeurs  \n
          !! 
          !!    Purpose: \n
          !!    -------- \n
          !!     Pour un profil colocalisé grâce à 4 points de grilles si ces quatre se trouvent dans le masque, cette
          !!     routine recherche le  point de grille valide le plus proche dans un rayon fixé et en tenant compte du
          !!     point ayant le poids le plus grand => cas de recherche d'un point qui se trouve dans le masque pour un champ 2D
          !!     Input : \n
          !!     ------ \n
          !!      
          !!      @param rd_poidsSO : valeur du poids au point de colloc Sud Ouest 
          !!      @param rd_poidsSE : valeur du poids au point de colloc Sud Est 
          !!      @param rd_poidsNO : valeur du poids au point de colloc Nord Ouest 
          !!      @param rd_poidsNE : valeur du poids au point de colloc Nord Est
          !!      @param id_xSO : valeur de l'indice du point de grille en longitude au SO
          !!      @param id_ySO : valeur  de l'indice du point de grille en latitude au SO
          !!      @param  id_indmax : nombre d'iteration pour la recherche (1 iteration= cherche sur une l
          !!                        largeur de 2 mailles modele
          !!     @param rda_pos : valeur du vecteur sur lequel effectuer la recherche
          !!     Output :  \n
          !!     ------ \n
          !!     @param rd_value :   valeur finale
          !!     
          !!***  Method: \n
          !!     ------- \n
          !!     MCAL_tri_ins
          !!     Externals: \n
          !!     ---------- \n
          !!     History: \n
          !!     -------- \n
          !!      Version    Programmer     Date            Description \n 
          !!      -------    ----------    ----            ----------- \n
          !!      1.0        C.REGNIER   Juin 2007         Maregraphe developpement  \n
          !<
          !*--------------------------------------------------------------------------
          !
         SUBROUTINE MCAL_recherche_valeurs_2D(rd_poidsSO,rd_poidsSE,rd_poidsNO,rd_poidsNE,id_xSO,id_ySO,rda_pos,rd_value,id_indmax)
          !
          !****recherche_valeurs
          ! 
          !    Purpose:
          !    --------
          !     Pour un profil colocalisé grâca 4 points de grilles si ces quatrpoints se trouvent dans le masque, cette
          !     routine recherche le point de grille valide le plus proche dans un rayon fixéen tenant compte du
          !     point ayant le poids le plus grand =>  cas de recherche d'un points qui se trouve dans le masque pour un champ 2D
          !     Input : \\
          !     ------
          !      \begin{itemize}
          !      \item 
          !      \item 
          !      \item 
          !     Output : \\
          !     rda_time :
          !     rda_ssh2 : 
          !***  Method:
          !     -------
          !     Externals:
          !     ----------
          !     History:
          !     --------
          !      Version    Programmer                                       Date            Description
          !      -------    ----------                                       ----            -----------
          !      1.0        C.REGNIER   Juin 2007      Plateforme validation 
          !*--------------------------------------------------------------------------
          !
          !
          !**   0. DECLARATIONS

          INTEGER(KIND=4)                                  :: il_indexmax
          INTEGER(KIND=4)                                  :: il_x1,il_y1,il_x2,il_y2,il_x3,il_y3,il_x4,il_y4,il_indice
          INTEGER(KIND=4), DIMENSION(4)                    :: ila_ind,ila_indout
          REAL(KIND=4), DIMENSION(4)                       :: rla_poids,il_x,il_y,rla_poidsout

          !
          !**   0.3 Dummy variables
          !
          INTEGER(KIND=4),INTENT(IN)                    :: id_xSO,id_ySO,id_indmax
          REAL(KIND=4),INTENT(IN)                       :: rd_poidsSO,rd_poidsSE,rd_poidsNO,rd_poidsNE

          REAL(KIND=4), DIMENSION(:,:),INTENT(IN)       :: rda_pos
	  REAL(KIND=4),INTENT(OUT)                      :: rd_value 
          ! 
          !*--------------------------------------------------------------------------
          !
	  !** 
	  il_indexmax=9999
          rla_poids(1)=rd_poidsSO
          rla_poids(2)=rd_poidsSE
          rla_poids(3)=rd_poidsNO
          rla_poids(4)=rd_poidsNE
          ila_ind(1)=1
          ila_ind(2)=2
          ila_ind(3)=3
          ila_ind(4)=4

          !** Tri du vecteur poids pour faire une recherche par poids decroissant pour avoir 
          !   les indices par ordre décroissant
          CALL MCAL_tri_ins(rla_poids,rla_poidsout,ila_ind,ila_indout)
          !PRINT *,'AVANT',rla_poids,ila_ind
          !PRINT *,'APRES',rla_poidsout,ila_indout


          !** Boucle autour de la premiere serie de points ppur trouver le point le plus proche
          !   valable ; on se fixe un rayon de 100km ici il_x = 4
          !               .-------. 
          !               | .---. |
          !               | |N=0| |
          !               | °---° |   
          !               °-------°
          !                 N=1 ...
          il_indice=1

          DO WHILE(il_indice.LE.id_indmax)
             !print *,'il_indice',il_indice
             il_x(1)=id_xSO    - il_indice
             il_y(1)=id_ySO    - il_indice
             il_x(2)=id_xSO+1  + il_indice
             il_y(2)=id_ySO    - il_indice  
             il_x(3)=id_xSO    - il_indice
             il_y(3)=id_ySO+1  + il_indice
             il_x(4)=id_xSO+1  + il_indice
             il_y(4)=id_ySO+1  + il_indice


             il_x1=il_x(ila_indout(4))
             il_y1=il_y(ila_indout(4))
             il_x2=il_x(ila_indout(3))
             il_y2=il_y(ila_indout(3))
             il_x3=il_x(ila_indout(2))
             il_y3=il_y(ila_indout(2))
             il_x4=il_x(ila_indout(1))
             il_y4=il_y(ila_indout(1))

             !PRINT *,'POIDS',rla_poids(ila_indout(4)),il_x1,il_y1
             !** Le poids est fixé à 1 pour la valeur la plus proche
            !print *,'VAL ::',il_x1,il_y1,il_x2,il_y2,il_x3,il_y3,il_x4,il_y4
	
	   IF (il_x1 < il_indexmax .AND. il_y1 < il_indexmax) THEN
            rd_value=rda_pos(il_x1,il_y1)
             IF(rd_value.EQ.0) THEN
            !    PRINT *,'passe au 2eme'
                rd_value=rda_pos(il_x2,il_y2)
                IF (rd_value.EQ.0) THEN
             !      PRINT *,'passe au 3eme'
                   rd_value=rda_pos(il_x3,il_y3)
                   IF (rd_value.EQ.0) THEN
              !        PRINT *,'passe au 4eme'
                      rd_value=rda_pos(il_x4,il_y4)
                      IF (rd_value.EQ.0) THEN
               !          PRINT *,'passe à un rayon plus grand'
                         il_indice=il_indice+1
                      ELSE
                         il_indice=5 
                !         PRINT *,'Quatrieme'
                      ENDIF
                   ELSE
                 !     PRINT *,'Troisième'
                      il_indice=5
                   ENDIF
                ELSE
                 !  PRINT *,'Deuxième'
                   il_indice=5
                ENDIF
             ELSE
                !PRINT *,'Premier OK'
                il_indice=5
             ENDIF
        ELSE
	rd_value=0
	print *,'Valeur index abherentes'
	il_indice=il_indice+1
	ENDIF 
	
	
	 ENDDO
!
!*--------------------------------------------------------------------------
!
      END SUBROUTINE MCAL_recherche_valeurs_2D
!
!=================================================================================
!
        !> \brief                                                              
        !!
        !!**** sort_shell(rla_arr) 
        !!
        !!     Purpose :
        !!     Compute a sort of a large data set with the shell method
        !!        
        !!    References: Numerical Recipies
        !!     History
        !!     -------
        !!      Version    Programmer    Date       Description
        !!      ---------------------------------------------
        !!       1.0       C.REGNIER    23/09/2010   Adaptation
        !<
        !*-------------------------------------------------------------
        SUBROUTINE MCAL_sort_shell(rla_arr)
        !**
        IMPLICIT NONE
        !**
        INTEGER(KIND=4) :: il_i,il_j,il_inc,il_n
        REAL(KIND=4), DIMENSION(:), INTENT(INOUT) :: rla_arr
        REAL(KIND=4) :: rl_v
        !**
        il_n=size(rla_arr)
        il_inc=1
        do
                il_inc=3*il_inc+1
                if (il_inc > il_n) exit
        end do
        do
                il_inc=il_inc/3
                do il_i=il_inc+1,il_n
                        rl_v=rla_arr(il_i)
                        il_j=il_i
                        do
                                if (rla_arr(il_j-il_inc) <= rl_v) exit
                                rla_arr(il_j)=rla_arr(il_j-il_inc)
                                il_j=il_j-il_inc
                                if (il_j <= il_inc) exit
                        end do
                        rla_arr(il_j)=rl_v
                end do
                if (il_inc <= 1) exit
        end do

        END SUBROUTINE MCAL_sort_shell

!
!=================================================================================
!
          !> \brief
          !!****tri_ins \n
          !! 
          !!    Purpose: \n
          !!    -------- \n
          !!    Tri par insertion d'un tableau \n
          !!     Input : \n
          !!     ------ \n
          !!      
          !!      @param rda_tab : tableau d'entrée
          !!
          !!     Output : \n 
          !!     ------ \n
          !!     @param rda_tabout : tableau de sortie
          !!     @param ila_ind    : indices du tableau
          !!
          !!***  Method:  \n 
          !!     ------- \n 
          !!     Externals: \n 
          !!     ---------- \n 
          !!     History: \n 
          !!     -------- \n 
          !!      Version    Programmer   Date            Description \n 
          !!      -------    ----------   ----            ----------- \n 
          !!      1.0        C.REGNIER   Juin 2007      Plateforme validation  \n 
          !<
          !*--------------------------------------------------------------------------
          !
          !
        SUBROUTINE MCAL_tri_ins(rda_tab,rda_tabout,ila_ind,ila_indout)
          !
          !**   0. DECLARATIONS

          IMPLICIT NONE
          integer :: il_i,il_j,il_dim
          ! 
          !**   0.3 Dummy variables
          !
          REAL(KIND=4), DIMENSION(:)    :: rda_tab
          REAL(KIND=4), DIMENSION(:)    :: rda_tabout
          INTEGER(KIND=4), DIMENSION(:) :: ila_ind,ila_indout
          REAL(KIND=4)                  :: rl_aux , rl_aux2       
          !
          !*--------------------------------------------------------------------------
          !
          il_dim=size(rda_tab)
          rda_tabout(:)=rda_tab(:)
          ila_indout=ila_ind(:)
          DO il_i=2,il_dim
             il_j=il_i
             DO WHILE (rda_tabout(il_j-1).GT.rda_tabout(il_j))
                rl_aux = rda_tabout(il_j-1)
                rl_aux2=ila_indout(il_j-1)
                rda_tabout(il_j-1)=rda_tabout(il_j)
                ila_indout(il_j-1)=ila_indout(il_j)
                rda_tabout(il_j)=rl_aux
                ila_indout(il_j)=rl_aux2

                IF (il_j.GT.2) il_j=il_j-1
           
             ENDDO
             
          ENDDO
          !
          !*--------------------------------------------------------------------------
          !
          
        END SUBROUTINE MCAL_tri_ins
        
!
!*=================================================================================
!
! Les routines suivantes servent à calculer des stats
!
!*=================================================================================
!*=================================================================================
    !> \brief
    !!
    !!**** avevarsdev \n 
    !!
    !!     Purpose: \n 
    !!     -------- \n 
    !!       Pour une serie temporelle donnée, cette routine retourne sa moyenne
    !!       son ecart type, sa variance 
    !!     Input : \n 
    !!     ------  \n 
    !!      
    !!      @param il_n : nombre de données 
    !!      @param rda_data : vecteur de données 
    !! 
    !!     Output : \n 
    !!     ------  \n 
    !!      @param rd_ave : moyenne
    !!      @param rd_sdev : ecart type 
    !!      @param rd_var : variance
    !!
    !!***  Method: \n 
    !!     ------- \n 
    !!     Externals: \n  
    !!     ---------- \n 
    !!     History:  \n 
    !!     --------  \n 
    !!      Version    Programmer      Date            Description  \n 
    !!      -------    ----------      ----            -----------  \n 
    !!      1.0        C.REGNIER        Mai 2007      Issu de Numerical Recipies \n 
    !<
    !*--------------------------------------------------------------------------
    !

 SUBROUTINE MCAL_avevarsdev(rda_data,rd_ave,rd_sdev,rd_var)
    !
    !USE nrtype
    IMPLICIT NONE
    !
    !**   0. DECLARATIONS
    INTEGER(KIND=4)                         :: il_n
    REAL(SP),DIMENSION(:),ALLOCATABLE       :: rla_s
    !
    !**   0.3 Dummy variables
    !
    REAL(KIND=4), DIMENSION(:),INTENT(IN)  :: rda_data
    REAL(KIND=4),INTENT(OUT)               :: rd_ave,rd_sdev
    REAL(KIND=4)                           :: rd_var
    !
    !*--------------------------------------------------------------------------
    !
    !Initialisation
    ALLOCATE(rla_s(size(rda_data)))
    rla_s(:)=0.
    rd_var=0.
    rd_sdev=0.
    !
    il_n=size(rda_data)
    IF (il_n <= 1) CALL nrerror('Subroutine avevarstd : n doit être supèrieur à 2 !!')
    !** Premier passage :: obtention de la moyenne
    rd_ave=sum(rda_data(:))/il_n
    !** Deuxieme passage pour obtenir le 2eme troisieme et quatrième moment de
    ! la deviation par rapport à la moyenne
    rla_s(:)=rda_data(:)-rd_ave
    rd_var=dot_product(rla_s,rla_s)
    !** Correction de la formule
    rd_var=(rd_var-sum(rla_s)**2/il_n)/(il_n-1)
    rd_sdev=sqrt(rd_var)
!   
!****************************************************************** 
  END SUBROUTINE MCAL_avevarsdev
!

!
!====================================================================================
!====================================================================================
!
!
    !> \brief
    !!**** moment \n 
    !!
    !!     Purpose: \n 
    !!     -------- \n 
    !!       Pour une serie temporelle donnée, cette routine retourne sa moyenne
    !!       son ecart moyen, son ecart type, sa variance et le skewness et curtosis
    !!
    !!     Input : \n  
    !!     ------  \n 
    !!      
    !!      @param il_n : nombre de données 
    !!      @param rda_data : vecteur de données 
    !! 
    !!     Output : \n  
    !!      @param rd_ave : moyenne
    !!      @param rd_adev : ecart moyen
    !!      @param rd_sdev : ecart type 
    !!      @param rd_var : variance
    !!      @param rd_skew : skewness
    !!      @param rd_kurt : kurtosis
    !!
    !!***  Method: \n 
    !!     ------- \n 
    !!     Externals: \n 
    !!     ---------- \n 
    !!     History: \n 
    !!     -------- \n 
    !!      Version    Programmer      Date            Description \n 
    !!      -------    ----------      ----            ----------- \n 
    !!      1.0        C.REGNIER        Mai 2007      Issu de Numerical Recipies \n 
    !<
    !*--------------------------------------------------------------------------
    !
  SUBROUTINE MCAL_moment(rda_data,rd_ave,rd_adev,rd_sdev,rd_var,rd_skew,rd_kurt)
    !
    !USE nrtype
    IMPLICIT NONE
    !
    !**   0. DECLARATIONS
    INTEGER(KIND=4)                        :: il_n
    REAL(SP),DIMENSION(:),ALLOCATABLE      :: rla_p,rla_s
    REAL(KIND=4)                           :: rl_ep

    !
    !**   0.3 Dummy variables
    !
    REAL(KIND=4), DIMENSION(:),INTENT(IN)  :: rda_data
    REAL(KIND=4),INTENT(OUT)               :: rd_ave,rd_sdev,rd_var,rd_adev
    REAL(KIND=4),OPTIONAL                  :: rd_skew,rd_kurt
    !
    !*--------------------------------------------------------------------------
    !
    ALLOCATE(rla_p(size(rda_data)))
    ALLOCATE(rla_s(size(rda_data)))

    il_n=size(rda_data)
    IF (il_n <= 1) CALL nrerror('Subroutine moment : n doit être supèrieur à 2 !!')
    !** Premier passage :: obtention de la moyenne
    rd_ave=sum(rda_data(:))/il_n
    !** Deuxieme passage pour obtenir le 2eme troisieme et quatrième moment de
    ! la deviation par rapport à la moyenne
    !Initialisation
    rla_s=0.
    rl_ep=0.
    rd_adev=0.
    rd_var=0.
    rd_skew=0.
    rd_kurt=0.
 
    rla_s(:)=rda_data(:)-rd_ave
    rl_ep=sum(rla_s(:))
    rd_adev=sum(abs(rla_s(:)))/il_n
    rla_p(:)=rla_s(:)*rla_s(:)
    rd_var=sum(rla_p(:))
    rla_p(:)=rla_p(:)*rla_s(:)
    rd_skew=sum(rla_p(:))
    rla_p(:)=rla_p(:)*rla_s(:)
    rd_kurt=sum(rla_p(:))
    !** Correction de la formule
    rd_var=(rd_var-rl_ep**2/il_n)/(il_n-1)
    rd_sdev=sqrt(rd_var)
    IF(rd_var /= 0 )THEN
       rd_skew=rd_skew/(il_n*(rd_sdev**3))
       rd_kurt=rd_kurt/(il_n*rd_var**2)-3.0_sp
    ELSE
       CALL nrerror('Subroutine moment : NO SKEW OR KURTOSIS WHEN ZERO VARIANCE IN MOMENT')
    ENDIF
    DEALLOCATE(rla_p)
    DEALLOCATE(rla_s)

    !****************************************************************** 
  END SUBROUTINE MCAL_moment
  !
  !*======================================================================
  !*======================================================================
  !
    !> \brief
    !!**** corre \n  
    !!
    !!     Purpose: \n 
    !!     -------- \n 
    !!       Pour deux series temporelle données d'une même taille cette routine
    !!       calcule leur coefficient de corrélation r 
    !!
    !!     Input : \n 
    !!     ------  \n 
    !!      
    !!      @param x : 1ere serie 
    !!      @param y : 2eme serie 
    !! 
    !!     Output : \n 
    !!      @param r : coefficient de correlation
    !!
    !!***  Method: \n 
    !!     ------- \n 
    !!     Externals: \n 
    !!     ---------- \n 
    !!     History: \n  
    !!     -------- \n 
    !!      Version    Programmer      Date            Description \n 
    !!      -------    ----------      ----            ----------- \n 
    !!      1.0        C.REGNIER        Mai 2007      Issu de Numerical Recipies \n 
    !<
    !*--------------------------------------------------------------------------
    !
    !
  SUBROUTINE MCAL_corre(x,y,r)
    
    !USE nrtype
    IMPLICIT NONE
    !**   0. DECLARATIONS
    REAL(KIND=4), DIMENSION(:),ALLOCATABLE     :: xt,yt
    REAL(KIND=4)                               :: ax,ay,sxx,sxy,syy
    INTEGER(KIND=4)                           :: n 
     
    !
    !**   0.3 Dummy variables
    !
    REAL(KIND=4),INTENT(OUT) :: r
    REAL(KIND=4),DIMENSION(:),INTENT(IN) :: x,y
    !
    !*--------------------------------------------------------------------------
    !
    ALLOCATE(xt(size(x)))
    ALLOCATE(yt(size(x)))

    n=assert_eq2(size(x),size(y),'corre')
    !**Calcul de la moyenne
    ax=sum(x)/n
    ay=sum(y)/n
    !**Calcul des coefficients de correlation
    xt(:)=x(:)-ax
    yt(:)=y(:)-ay
    sxx=dot_product(xt,xt)
    syy=dot_product(yt,yt)
    sxy=dot_product(xt,yt)
    r=sxy/(sqrt(sxx*syy))
    DEALLOCATE(xt)
    DEALLOCATE(yt)
    !      
    !*--------------------------------------------------------------------------
    ! 
  END SUBROUTINE MCAL_corre
  !      
  !*==============================================================================================
  !*==============================================================================================
  !
 !      
  !*==============================================================================================
  !*==============================================================================================
  !
    !> \brief
    !!**** autocorre \n 
    !!
    !!     Purpose: \n 
    !!     -------- \n 
    !!       Pour une serie temporelle données d'une même taille cette routine
    !!       calcule l'autocorrelation 
    !!
    !!     Input : \n 
    !!     ------  \n 
    !!      
    !!      @param x : 1ere serie 
    !!      @param y : 2eme serie 
    !! 
    !!     Output : \n 
    !!      @param r : coefficient de correlation
    !!
    !!***  Method: \n 
    !!     ------- \n 
    !!     Externals: \n 
    !!     ---------- \n 
    !!     History: \n 
    !!     -------- \n 
    !!      Version    Programmer      Date            Description \n 
    !!      -------    ----------      ----            ----------- \n 
    !!      1.0        C.REGNIER        Mai 2007      Issu de Numerical Recipies \n 
    !<
    !*--------------------------------------------------------------------------
    !
 SUBROUTINE MCAL_autocorre(x,xt,il_dtime2)
    !
    !USE nrtype
    IMPLICIT NONE
    !**   0. DECLARATIONS
    REAL(SP), DIMENSION(:),INTENT(OUT)      :: xt
    REAL(SP), DIMENSION(:),ALLOCATABLE      :: c
    REAL(SP)                                :: ax,rl_err
    INTEGER(I4B)                            :: n
    INTEGER(KIND=4)                         :: il_i,il_j
    INTEGER(I4B),DIMENSION(:),ALLOCATABLE   :: il_dtime
  !  INTEGER(I4B),DIMENSION(:),POINTER       :: il_dtime2
     INTEGER(I4B),DIMENSION(:),INTENT(OUT)   :: il_dtime2

    !
    !**   0.3 Dummy variables
    !
    REAL(SP),DIMENSION(:),INTENT(IN) :: x
    !
    !*--------------------------------------------------------------------------
    !
   ! ALLOCATE(xt(size(x)))
    ALLOCATE(c(size(x)))
    ALLOCATE(il_dtime(size(x)))
  !  ALLOCATE(il_dtime2(size(x)))

    n=size(x)
    !**Calcul de la moyenne
    ax=sum(x)/n
    !**Calcul des coefficients de correlation
    xt(:)=x(:)-ax
    !** Calcul correlation function
      
      do il_i=1,n
        c(il_i)=0.d0
        do il_j=1,n+1-il_i
          c(il_i)=c(il_i)+xt(il_j)*xt(il_i+il_j-1)
        enddo
      enddo
!     normalise the correlation function
      xt(1)=c(1)/dble(n)
      do il_i=2,n
        xt(il_i)=(c(il_i)/dble(n+1-il_i))/xt(1)
      enddo
      xt(1)=1.d0
     ! il_dtime=(/0:n-1:1/)
      il_dtime=(/ 0, n-1, 1 /)
     ! il_dtime2=(/1:n:1/)
 	il_dtime2=(/ 1, n, 1 /) 
      !Erreur
       rl_err=sqrt(dot_product(2.d0*il_dtime2,(1-xt))**2/dble(n))
    !      
    !*--------------------------------------------------------------------------
    ! 
  END SUBROUTINE MCAL_autocorre

!
!*======================================================================
!*======================================================================

    !> \brief
    !!**** find \n 
    !!     Purpose : \n 
    !!     Compute the comparison between two times series and find the indices \n 
    !! 
    !!     History \n 
    !!     ------- \n 
    !!      Version    Programmer    Date       Description \n  
    !!      --------------------------------------------- \n 
    !!       1.0       C.REGNIER    06/06/2007    created \n 
    !!       1.1       C.REGNIER    24/03/2009    correction si taille des series egales \n  
    !<
    !*-------------------------------------------------------------
    !
  SUBROUTINE MCAL_find(rda_data1,rda_data2,il_data1_ini,il_data1_fin,il_data2_ini,il_data2_fin,il_size)
    !
    !** ++  Local Declarations 
    !
     USE MIOL_param
     IMPLICIT NONE
    INTEGER,PARAMETER                             :: SP = KIND(1.0)
    INTEGER(KIND=4)                               :: il_size1,il_size2,il_i,il_ind,il_ind2,il_data,il_size
    INTEGER(KIND=4)                               :: il_pas1,il_pas2,il_indi_ind,il_indf_ind,il_indi_ref,il_indf_ref
    INTEGER(KIND=4)                               :: il_data1_ini,il_data1_fin,il_data2_ini,il_data2_fin
    REAL(KIND=4),DIMENSION(:),TARGET,INTENT(IN)   :: rda_data1,rda_data2
    REAL(KIND=4),DIMENSION(:),POINTER             :: rla_data_ref,rla_data_ind
    !
    !*-------------------------------------------------------------
    !
    il_size1=COUNT(rda_data1.NE.rg_fillvalue)
    il_size2=COUNT(rda_data2.NE.rg_fillvalue)
    il_pas1=rda_data1(2)-rda_data1(1)
    il_pas2=rda_data2(2)-rda_data2(1)
    print *,'il',il_size1,il_size2,il_pas1,il_pas2
    !** Test
    IF (il_pas1 /= il_pas2) THEN
       PRINT *,"Les deux series n'ont pas la même discrétisation!!",il_pas1,il_pas2
       STOP
    ENDIF
    !** Recherche de la longeur max
    IF(il_size1.LT.il_size2)THEN
       PRINT *,'data2 plus grand'
       il_data=2
       il_ind=il_size2
       il_ind2=il_size1
       rla_data_ref=>rda_data2(:)
       rla_data_ind=>rda_data1(:)
    ELSEIF(il_size2.LT.il_size1)THEN
       PRINT *,'data1 plus grand'
       il_data=1
       il_ind=il_size1
       il_ind2=il_size2
       rla_data_ref=>rda_data1(:)
       rla_data_ind=>rda_data2(:)
    ELSE
       Print *,'Series egales'
       il_data=1
       il_ind2=il_size2
       il_ind=il_size1  !** Modif 24/03/09
       rla_data_ref=>rda_data1(:)
       rla_data_ind=>rda_data2(:)
    ENDIF
    il_indi_ref=0
    il_indf_ref=0
    il_indi_ind=0
    il_indf_ind=0
    !
    !** On test le recouvrement entre les deux series
    !* Cas 1      rla_data_ref        |---------------------------------------| ou |-----------------------| ou        |---------------------------| 
    !             rla_data_ind              |---------------------------|       ou |-----------------------| ou  |---------------------------------------|
    IF (rla_data_ref(1).LE.rla_data_ind(1).AND.&
         rla_data_ref(il_ind).GE.rla_data_ind(il_ind2))THEN
       il_indi_ind=1
       il_indf_ind=il_ind2
       PRINT *,'RECOUVREMENT entre les deux SERIES cas 1'
       DO il_i=1,il_ind
          IF(int(rla_data_ref(il_i)).EQ.int(rla_data_ind(1))) THEN
             il_indi_ref=il_i
          ELSEIF(int(rla_data_ref(il_i)).EQ.int(rla_data_ind(il_ind2))) THEN
             il_indf_ref=il_i          
          ENDIF
       ENDDO
       !* Cas 2
       !            rla_data_ref                   |---------------------------------------|
       !            rla_data_ind              |---------------------------|
    ELSEIF (rla_data_ref(1).GT.rla_data_ind(1).AND.&
         rla_data_ref(il_ind).GT.rla_data_ind(il_ind2).AND.&
         rla_data_ref(1).LT.rla_data_ind(il_ind2))THEN
       PRINT *,'SERIES decalées la 2eme serie debute et fini avant la 1er cas 2'
       il_indi_ref=1
       il_indf_ind=il_ind2
       DO il_i=1,il_ind2
          IF(int(rla_data_ind(il_i)).EQ.int(rla_data_ref(1))) THEN
             il_indi_ind=il_i
          ENDIF
       ENDDO
       DO il_i=1,il_ind
          IF(int(rla_data_ref(il_i)).EQ.int(rla_data_ind(il_ind2))) THEN
             il_indf_ref=il_i
          ENDIF
       ENDDO
       !* Cas 3
       !            rla_data_ref                   |---------------------------------------|
       !            rla_data_ind                                    |---------------------------|
    ELSEIF (rla_data_ref(1).LT.rla_data_ind(1).AND.&   
         rla_data_ref(il_ind).LT.rla_data_ind(il_ind2))THEN
       PRINT *,'SERIES decalées la 1eme serie debute et fini avant la 2eme cas 3'
       il_indi_ind=1
       il_indf_ref=il_ind
       DO il_i=1,il_ind2
          IF(int(rla_data_ind(il_i)).EQ.int(rla_data_ref(il_ind))) THEN
             il_indf_ind=il_i
          ENDIF
       ENDDO
       DO il_i=1,il_ind
          IF(int(rla_data_ref(il_i)).EQ.int(rla_data_ind(1))) THEN
             il_indi_ref=il_i
          ENDIF
       ENDDO
       !* Cas 4  Pas de recouvrement 
       !            rla_data_ref            |-------------------|
       !            rla_data_ind                                   |----------| 
       !  ou 
       !            rla_data_ref                         |-------------------|
       !            rla_data_ind            |----------| 


    ELSEIF (rla_data_ref(1).LT.rla_data_ind(1).AND.&   
         rla_data_ref(il_ind).LT.rla_data_ind(1).OR. &
        ! rla_data_ref(1).GT.rla_data_ind(il_ind2).AND.&   
        ! rla_data_ref(il_ind).LT.rla_data_ind(il_ind2)) THEN
         rla_data_ref(1).GT.rla_data_ind(1).AND.&
         rla_data_ref(1).GT.rla_data_ind(il_ind2)) THEN
       PRINT *,'Pas de recouvrement cas 4'
       il_indi_ref=0
       il_indf_ref=0
       il_indi_ind=0
       il_indf_ind=0   
    ELSE
       Print *,'Cas impossible :)'
       STOP
    ENDIF

    IF(il_data.EQ.1) THEN
       il_data1_ini=il_indi_ref
       il_data1_fin=il_indf_ref
       il_data2_ini=il_indi_ind
       il_data2_fin=il_indf_ind 
       il_size=size(rla_data_ind(il_indi_ind:il_indf_ind))
       PRINT *,'Resultats :: ',il_data1_ini,il_data1_fin,il_data2_ini,il_data2_fin,il_size

    ELSEIF(il_data.EQ.2) THEN
       il_data1_ini=il_indi_ind
       il_data1_fin=il_indf_ind
       il_data2_ini=il_indi_ref
       il_data2_fin=il_indf_ref
       il_size=size(rla_data_ind(il_indi_ind:il_indf_ind))
       PRINT *,'taille :: ',il_data1_ini,il_data1_fin,il_data2_ini,il_data2_fin,il_size
    ENDIF
    !
    !*-------------------------------------------------------------
    !
  END SUBROUTINE MCAL_find
!
!*======================================================================
!*======================================================================
    !> \brief
    !!**** MCAL_moyenne \n 
    !!
    !!     Purpose : \n  
    !!     -------- \n 
    !!       Pour une serie temporelle donnée, cette routine retourne sa moyenne
    !!       avec un masque pour ne pas compter les valeurs necessaires 
    !!     Input : \n 
    !!     ------  \n 
    !!      
    !!      @param rda_data : vecteur de données 
    !! 
    !!     Output : \n 
    !!      @param rd_ave : moyenne
 
    !!***  Method: \n 
    !!     ------- \n 
    !!     Externals: \n 
    !!     ---------- \n 
    !!     History: \n 
    !!     -------- \n 
    !!      Version    Programmer      Date            Description \n 
    !!      -------    ----------      ----            ----------- \n 
    !!      1.0        C.REGNIER        Mai 2007      Issu de Numerical Recipies \n 
    !<
    !*--------------------------------------------------------------------------
  SUBROUTINE MCAL_moyenne_m(rda_data,rd_ave,ida_mask)
    !
      IMPLICIT NONE
    !
    !**   0. DECLARATIONS
    INTEGER(KIND=4)                         :: il_n,il_i,il_sum
    REAL(KIND=4)                            :: rd_sum
    !
    !**   0.3 Dummy variables
    !
    REAL(KIND=4), DIMENSION(:),INTENT(IN)  :: rda_data
    INTEGER(KIND=4),DIMENSION(:),INTENT(IN) :: ida_mask
    REAL(KIND=4),INTENT(OUT)               :: rd_ave
         
    !
    !*--------------------------------------------------------------------------
    !
     rd_sum=0
     il_sum=0
     il_n=size(rda_data)
     IF (il_n <= 1) CALL nrerror('Subroutine moyenne : n doit être supèrieur à 2 !!')

     DO il_i=1,il_n
      IF (ida_mask(il_i) /= 0 ) THEN
         rd_sum=rd_sum+ rda_data(il_i)
         il_sum=il_sum+1
      ENDIF
   ENDDO
    !** Obtention de la moyenne corrigée
     rd_ave=rd_sum/il_sum

   END SUBROUTINE MCAL_moyenne_m

!*======================================================================

!*======================================================================
    !> \brief
    !!**** MCAL_moyenne \n 
    !!
    !!     Purpose : \n  
    !!     -------- \n 
    !!       Pour une serie temporelle donnée, cette routine retourne sa moyenne \n 
    !!     Input : \n 
    !!     ------  \n 
    !!      
    !!      @param rda_data : vecteur de données 
    !! 
    !!     Output : \n 
    !!      @param rd_ave : moyenne
 
    !!***  Method: \n 
    !!     ------- \n 
    !!     Externals: \n 
    !!     ---------- \n 
    !!     History: \n 
    !!     -------- \n 
    !!      Version    Programmer      Date            Description \n 
    !!      -------    ----------      ----            ----------- \n 
    !!      1.0        C.REGNIER        Mai 2007      Issu de Numerical Recipies \n 
    !<
    !*--------------------------------------------------------------------------
  SUBROUTINE MCAL_moyenne(rda_data,rd_ave)
    !
      IMPLICIT NONE
    !
    !**   0. DECLARATIONS
    INTEGER(KIND=4)                         :: il_n
    !
    !**   0.3 Dummy variables
    !
    REAL(KIND=4), DIMENSION(:),INTENT(IN)  :: rda_data
    REAL(KIND=4),INTENT(OUT)               :: rd_ave
    !
    !*--------------------------------------------------------------------------
    !
     il_n=size(rda_data)

    IF (il_n <= 1) CALL nrerror('Subroutine moyenne : n doit être supèrieur à 2 !!')
    !** Premier passage :: obtention de la moyenne
     rd_ave=sum(rda_data(:))/il_n
   END SUBROUTINE MCAL_moyenne

!*======================================================================
!*======================================================================
   !> \brief
    !!
    !!**** find \n 
    !!     Purpose : \n 
    !!     Compute the rms between two times series  \n 
    !!
    !!     Input : \n 
    !!     ------ \n 
    !!     
    !!     @param rda_data1 : vecteur de données 
    !!     @param rda_data2 : vecteur de données 
    !!     @param rd_rms : valeur de la rms entre les deux series
    !!
    !!     History \n 
    !!     ------- \n 
    !!      Version    Programmer    Date       Description \n 
    !!      ---------------------------------------------   \n 
    !!       1.0       C.REGNIER    06/06/2007    created   \n 
    !<
    !*-------------------------------------------------------------
    !
  SUBROUTINE MCAL_rms_2fields(rda_data1,rda_data2,rd_rms)
  
    !
    !** ++  Local Declarations 
    !

    !    IMPLICIT NONE
    REAL(KIND=4),DIMENSION(:),INTENT(IN)          :: rda_data1,rda_data2
    REAL(KIND=4),DIMENSION(:),ALLOCATABLE         :: rla_diff
    REAL(KIND=4)                                  :: rl_moy,rl_sdev,rl_var
    REAL(KIND=4) ,INTENT(OUT)                     :: rd_rms
    !
    !*--------------------------------------------
    IF (size(rda_data1).NE.size(rda_data2)) THEN
       PRINT *,'Times series with differents length'
        STOP! GOTO 110 ! pas beau le goto mais bon ...
    ENDIF
    ALLOCATE(rla_diff(size(rda_data1)))
    rla_diff=rda_data1-rda_data2
    CALL MCAL_avevarsdev(rla_diff,rl_moy,rl_sdev,rl_var)
    rd_rms=sqrt(rl_moy**2+rl_sdev**2)

END SUBROUTINE MCAL_rms_2fields
!
!*========================================================================================
!*========================================================================================
    ! 
    !> \brief
    !!**** find  \n 
    !!     Purpose : \n 
    !!     Compute the rms of a time serie \n 
    !!  
    !!     Input : \n 
    !!     ------
    !!     @param rda_data1 : vecteur de données 
    !!     @param rda_data2 : vecteur de données 
    !!     @param rd_rms : valeur de la rms entre les deux series
    !! 
    !!     History \n 
    !!     ------- \n 
    !!      Version    Programmer    Date       Description \n 
    !!      --------------------------------------------- \n 
    !!       1.0       C.REGNIER    06/06/2007    created \n 
    !<
    !*-------------------------------------------------------------
    !
 SUBROUTINE MCAL_rms_1field(rda_data1,rd_rms)
    !
    !** ++  Local Declarations 
    !

    !    IMPLICIT NONE
    REAL(KIND=4),DIMENSION(:),INTENT(IN)          :: rda_data1
    REAL(KIND=4)                                  :: rl_moy,rl_sdev,rl_var
    REAL(KIND=4) ,INTENT(OUT)                     :: rd_rms
    !
    !*--------------------------------------------
    CALL MCAL_avevarsdev(rda_data1,rl_moy,rl_sdev,rl_var)
    rd_rms=sqrt(rl_moy**2+rl_sdev**2)
 !
 !*-------------------------------------------------------------
 !    
  END SUBROUTINE MCAL_rms_1field
!
!*======================================================================
!
  SUBROUTINE nrerror (string)
    ! Report a message then die
    CHARACTER(LEN=*),INTENT(IN) :: STRING
    WRITE(*,*) 'nrerror:',string
    PRINT *,'Program terminated by nrerror'
  END SUBROUTINE nrerror
!

!
!*======================================================================
!
  FUNCTION assert_eq2(n1,n2,string)
    CHARACTER(LEN=*),INTENT(IN):: string
    INTEGER,INTENT(IN) :: n1,n2
    INTEGER :: assert_eq2
    IF (n1==n2) THEN
       assert_eq2=n1
    ELSE
       WRITE(*,*) 'nerror : an assert_eq failed with this tag :',&
            string
       STOP 'PROGRAM terminated by assert_eq2'
    ENDIF
  END FUNCTION assert_eq2
!
!
!
!*======================================================================
   !> \brief
    !!
    !!**** find \n 
    !!     Purpose : \n 
    !!     Compute the shapiro filter of a 2D matrix
    !!
    !!     Input : \n 
    !!     ------ \n 
    !!     
    !!     @param rda_tabin : 2D input
    !!     @param rda_tabout :  2D output 
    !!     @param id_npass :  number of shapiro pass
    !!     @param ida_tabmaskin : input mask
    !!     @param jpi : nb longitudes
    !!     @param jpj : nb latitudes
    !!
    !!     History \n 
    !!     ------- \n 
    !!      Version    Programmer    Date       Description \n 
    !!      ---------------------------------------------   \n 
    !!       1.0       C.REGNIER   04/11/2011    created from shapiro_mpp routine   \n 
    !<
    !*-------------------------------------------------------------
    !
  SUBROUTINE MCAL_shapiro(rda_tabin,rda_tabout,id_npass,ida_tabmaskin, jpi,jpj)
!
    implicit none
!
    real(kind=4), dimension(1:jpi,1:jpj), intent(in) :: rda_tabin
    real(kind=4), dimension(1:jpi,1:jpj), intent(out) :: rda_tabout
    integer, intent(in) :: id_npass, jpi,jpj
    integer, dimension(1:jpi,1:jpj), intent(in) :: ida_tabmaskin
!
    real(kind=4), dimension(1:jpi,1:jpj) :: rla_tabmaskin,rla_tabsummask
    real(kind=8), parameter ::   rp_alpha = 1./2.
    integer :: il_jpass,il_jpim1,il_jpjm1,il_jpim2,il_jpjm2
    real(kind=8) :: rl_coef
!
    if (id_npass<1) then
       write(0,*) 'Error shapiro filter: (id_npass<1)',id_npass
       stop
     else
       write(0,*) 'Shapiro filter pass : ',id_npass
    endif
!
    il_jpim1=jpi-1
    il_jpjm1=jpj-1
    il_jpim2=jpi-2
    il_jpjm2=jpj-2
!
    rl_coef=0.25 * rp_alpha
!
    rla_tabmaskin(1:jpi,1:jpj)=real(ida_tabmaskin(1:jpi,1:jpj),4)
!
    where (ida_tabmaskin(2:il_jpim1,2:il_jpjm1) == 1 ) &
         rla_tabsummask(2:il_jpim1,2:il_jpjm1)=( rla_tabmaskin(1:il_jpim2,2:il_jpjm1) + &                                                                     
         rla_tabmaskin(3:jpi  ,2:il_jpjm1) + rla_tabmaskin(2:il_jpim1,1:il_jpjm2) + rla_tabmaskin(2:il_jpim1,3:jpj) )
!
    DO il_jpass = 1,id_npass! nombre de passes du filtre
!
       if (il_jpass/=1) where (ida_tabmaskin(2:il_jpim1,2:il_jpjm1) == 1 ) &
          rda_tabout(2:il_jpim1,2:il_jpjm1)=rda_tabin(2:il_jpim1,2:il_jpjm1)
!
       where (ida_tabmaskin(2:il_jpim1,2:il_jpjm1) == 1 ) &
            rda_tabout(2:il_jpim1,2:il_jpjm1)=rda_tabin(2:il_jpim1,2:il_jpjm1)- &
            rl_coef * rda_tabin(2:il_jpim1,2:il_jpjm1)* rla_tabsummask(2:il_jpim1,2:il_jpjm1) + &
            rl_coef *( rda_tabin(1:il_jpim2,2:il_jpjm1)* rla_tabmaskin(1:il_jpim2,2:il_jpjm1) + &
            rda_tabin(3:jpi  ,2:il_jpjm1)*rla_tabmaskin(3:jpi ,2:il_jpjm1) + &
            rda_tabin(2:il_jpim1,1:il_jpjm2)*rla_tabmaskin(2:il_jpim1,1:il_jpjm2) + &
            rda_tabin(2:il_jpim1,3:jpj)*rla_tabmaskin(2:il_jpim1,3:jpj) )
!
    ENDDO!il_jpass = 1,id_npass
!
  end SUBROUTINE MCAL_shapiro


END MODULE MCAL_trait_sig_stats

