MODULE SU_GMES_TYP
IMPLICIT NONE
! *****************************************************************************
! *                                                                           *
! * Fichier : SU_GMES_TYP.H                                                   *
! *                                                                           *
! *****************************************************************************
! *                                                                           *
! * ROLE   :                                                                  *
! *                                                                           *
! *    Interface machine SU_GMES                                              *
! *                                                                           *
! * FICHIERS A INCLURE AVANT CELUI-CI :                                       *
! *                                                                           *
! *    <aucun>                                                                *
! *                                                                           *
! *****************************************************************************
! *                                                                           *
! *                                      P.Sicard                             *
! *                                      CLS Space Oceanography Group         *
! *                                      12 Avril 1995                        *
! *                                                                           *
! *****************************************************************************

!-----------------------------------------------------------------------------!
!                              C O N S T A N T E S                            !
!-----------------------------------------------------------------------------!

! Longueur maximum (en octets) d'un message d'erreur
! --------------------------------------------------

      INTEGER(KIND=2) ::		 ec_su_gmes_lg_max_mes
      parameter		(ec_su_gmes_lg_max_mes = 160)

! Longueur maximum d'un nom de subroutine (nom emetteur du
! message d'erreur)
! --------------------------------------------------------

      INTEGER(KIND=2) ::		 ec_su_gmes_lg_nom_sub
      parameter		(ec_su_gmes_lg_nom_sub = 30)

! LISTE DES NUMEROS DE MESSAGES D'ERREUR
! --------------------------------------
!
!  Un message d'erreur est identifie par un numero. Il peut comporter des
!  parametres de type entier (I) , flottant (F) ou chaine de caracteres
!  (A). L'emplacement ainsi que la longueur de chaque parametre est repere
!  dans le message entre 2 caracteres '#'
!
!  exemple:
! 
!   ERREUR OUVERTURE FICHIER #A80# STATUS #I4# VALEUR #F8.3#
!
!     ce message comporte trois parametres. Le premier est une chaine de
!     caracteres de 80 octets maximum, le second est un entier qui sera 
!     code sur 4 octets dans le message formatte, le troisime est un nombre
!     flottants (8 caracteres, 3 chiffres apres la virgule)
!
!  Le numero de message ainsi que la valeur de chaque parametre qu'il
!  contient sont stockes dans la structure d'appel a l'operation SU_GERR_MES
!  (cf. description de la structure ci-dessous).
!  Il peut y avoir au maximum 4 parametres entiers, 4 parametres flottants
!  et 4 parametres chaines de caracteres dans un meme message.


! Numero de chaque message d'erreur (Le descriptif de chaque message
! se trouve dans le fichier su_gmes_init.h)
! ------------------------------------------------------------------

      INTEGER(KIND=2) ::	 ec_su_gmes_mgetlun       	! erreur acquisition nul
      parameter	(ec_su_gmes_mgetlun      = 1)

      INTEGER(KIND=2) ::	 ec_su_gmes_mfreelun       	! erreur liberation nul
      parameter	(ec_su_gmes_mfreelun     = 2)

      INTEGER(KIND=2) ::  ec_su_gmes_mfcreat        	! erreur creation fichier
      parameter	(ec_su_gmes_mfcreat      = 3)

      INTEGER(KIND=2) ::  ec_su_gmes_mfopen        	! erreur ouverture fichier
      parameter	(ec_su_gmes_mfopen       = 4)

      INTEGER(KIND=2) ::	 ec_su_gmes_mfdestroy     	! erreur destruction fichier
      parameter	(ec_su_gmes_mfdestroy 	= 5)

      INTEGER(KIND=2) ::	 ec_su_gmes_mfread        	! erreur lecture fichier
      parameter	(ec_su_gmes_mfread 	     = 6)

      INTEGER(KIND=2) ::	 ec_su_gmes_mfwrite       	! erreur ecriture fichier
      parameter	(ec_su_gmes_mfwrite      = 7)

      INTEGER(KIND=2) ::	 ec_su_gmes_mdiv0         	! Division par zero 
      parameter	(ec_su_gmes_mdiv0      	= 8)

      INTEGER(KIND=2) ::	 ec_su_gmes_mlogneg       	! Logarithme d'un nbre negatif 
      parameter	(ec_su_gmes_mlogneg    	= 9)

      INTEGER(KIND=2) ::	 ec_su_gmes_msincos       	! Sin/Cos superieur a 1 
      parameter	(ec_su_gmes_msincos   	=10)

      INTEGER(KIND=2) ::	 ec_su_gmes_msqrtneg      	! Racine d'un nbre negatif
      parameter	(ec_su_gmes_msqrtneg   	=11)

      INTEGER(KIND=2) ::  ec_su_gmes_mdim          	! Table sous-dimensionnee
      parameter (ec_su_gmes_mdim 	     =12)

      INTEGER(KIND=2) ::  ec_su_gmes_mfovf             ! Trop de fichiers a gerer
      parameter (ec_su_gmes_mfovf        =13)

      INTEGER(KIND=2) ::  ec_su_gmes_mfunknown         ! Acces fichier inconnu   
      parameter (ec_su_gmes_mfunknown    =14)

      INTEGER(KIND=2) ::  ec_su_gmes_mdate             ! Erreur conversion date
      parameter (ec_su_gmes_mdate        =15)

      INTEGER(KIND=2) ::  ec_su_gmes_mbadchp           ! Champ mesure inconnu  
      parameter (ec_su_gmes_mbadchp      =16)

      INTEGER(KIND=2) ::  ec_su_gmes_mgetpar           ! Parametres de fct.    
      parameter (ec_su_gmes_mgetpar      =17)

      INTEGER(KIND=2) ::  ec_su_gmes_malgo             ! Erreur detectee dans lib ALGO
      parameter (ec_su_gmes_malgo        =18)

      INTEGER(KIND=2) ::  ec_su_gmes_mbadprod          ! Erreur sur le produit        
      parameter (ec_su_gmes_mbadprod     =19)

      INTEGER(KIND=2) ::  ec_su_gmes_mbadsat           ! Satellite inconnu            
      parameter (ec_su_gmes_mbadsat      =20)

      INTEGER(KIND=2) ::  ec_su_gmes_mbadgrid          ! Parametres grille incorrect  
      parameter (ec_su_gmes_mbadgrid     =21)

      INTEGER(KIND=2) ::  ec_su_gmes_mtrtdup           ! Traitement deja effectue     
      parameter (ec_su_gmes_mtrtdup      =22) ! (exemple biais Dalt)

      INTEGER(KIND=2) ::  ec_su_gmes_mbadalt           ! Altimetre non traite         
      parameter (ec_su_gmes_mbadalt      =23) ! (inconnu ?)
                                                    
      INTEGER(KIND=2) ::  ec_su_gmes_mnoana            ! Aucune analyse valide      
      parameter (ec_su_gmes_mnoana       =24) ! (inconnu ?)
                                                    
      INTEGER(KIND=2) ::  ec_su_gmes_mpvw              ! Communication FOrtran_Wave 
      parameter (ec_su_gmes_mpvw         =25) !           

                                                    
      INTEGER(KIND=2) ::  ec_su_gmes_mchrono           ! ordre chronologique
      parameter (ec_su_gmes_mchrono      =26) ! non respecte                
                                                    
      INTEGER(KIND=2) ::  ec_su_gmes_mfmanq            ! Fichier manquant   
      parameter (ec_su_gmes_mfmanq       =27) !                             

      INTEGER(KIND=2) ::  ec_su_gmes_mincchp           ! numéro de champ incorrect
      parameter (ec_su_gmes_mincchp      =28) !
                                                    
      INTEGER(KIND=2) ::  ec_su_gmes_mbadenr           ! nb enreg. incorrect (conv ftp)
      parameter (ec_su_gmes_mbadenr      =29) !

      INTEGER(KIND=2) ::  ec_su_gmes_mbadparam         ! valeur d'un paramètre incorrecte
      parameter (ec_su_gmes_mbadparam    =30) !
                                                    
      INTEGER(KIND=2) ::  ec_su_gmes_mbadval           ! Algorithme de validation Inconnu
      parameter (ec_su_gmes_mbadval      =31) !
                                                    
      INTEGER(KIND=2) ::  ec_su_gmes_mbadgeo           ! Mauvaise zone geographique       
      parameter (ec_su_gmes_mbadgeo      =32) !
                                                    
      INTEGER(KIND=2) ::  ec_su_gmes_mfvide            ! Fichier vide   
      parameter (ec_su_gmes_mfvide       =33) !                             

      INTEGER(KIND=2) ::  ec_su_gmes_mincgdrt          ! Incohérence détectée pour
      parameter (ec_su_gmes_mincgdrt     =34) ! un fichier GDRT
     
      INTEGER(KIND=2) ::   ec_su_gmes_mprodmat          ! produit matriciel              
      parameter (ec_su_gmes_mprodmat     =35) ! impossible                     
      INTEGER(KIND=2) ::   ec_su_gmes_mnag              ! erreur détectée par Nag        
      parameter (ec_su_gmes_mnag         =36) !                                
      INTEGER(KIND=2) ::  ec_su_gmes_inc_fic           ! incohérence entre fichiers     
      parameter (ec_su_gmes_inc_fic      =37) !                                
!     --------------
!     Erreurs DUACS                                                            
!    ---------------                                                          
       INTEGER(KIND=2) :: ec_su_gmes_fvalcyc           ! meme cycle present dans        
       parameter (ec_su_gmes_fvalcyc = 41)     ! plusieurs fichiers             
       INTEGER(KIND=2) :: ec_su_gmes_fvalsat           ! plusieurs satellite en         
       parameter (ec_su_gmes_fvalsat = 42)     ! entree de residus              

!-----------------------------------------------------------------------------!
!                                   T Y P E S                                 !
!-----------------------------------------------------------------------------!


! Descriptif d'un message d'erreur a afficher
! --------------------------------------------

      TYPE s_su_gmes_err 
           INTEGER(KIND=2)                          ev_num_mes      ! numero message
           CHARACTER(LEN=ec_su_gmes_lg_nom_sub)     tv_nomop	    ! nom emetteur
	   REAL(KIND=8)                             dw_param(4)        ! parametres flottants
	   INTEGER(KIND=4)                          jw_param(4)	    ! parametres entiers
	   CHARACTER(len=ec_su_gmes_lg_max_mes)     tw_param(4)     ! parametres chaines
    END TYPE

END MODULE SU_GMES_TYP
