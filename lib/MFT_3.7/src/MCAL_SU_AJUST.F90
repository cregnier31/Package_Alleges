!> \brief                                                                                                                                                                 
! ****************************************************************************
! *                                                                          *
! * MACHINE : SU_AJUST                                                       *
! *                                                                          *
! ****************************************************************************
! *                                                                          *
! * ROLE                                                                     *
! *                                                                          *
! *   Ajustement de fonctions polynomiales et sinusoïdales                   *
! *                                                                          *
! ****************************************************************************
! *                                                                          *
! * OPERATIONS EXTERNES                                                      *
! *                                                                          *
! *   SU_AJUST_FONCT  : Ajustement d'une fonction polynôme ou sinus          *
! *                                                                          *
! * OPERATIONS INTERNES                                                      *
! *                                                                          *
! *   Calcul matriciel                                                       *
! *   SU_AJUST_TRANS_MAT  : Calcul transposee                                *
! *   SU_AJUST_MULT_MAT   : Calcul produit                                   *
! *   SU_AJUST_INV_MAT    : Calcul inverse                                   *
! *                                                                          *
! * OPERATIONS APPELEES                                                      *
! *                                                                          *
! * MACHINES SUPPORT UTILISEES                                               *
! *                                                                          *
! *                                                                          *
! * ALGORITHMES UTILISES                                                     *
! *                                                                          *
! *   Ajustement polynomial d'une fonction                                   *
! *                                                                          *
! ****************************************************************************
! *                                                                          *
! *                                      P. Mambert                          *
! *                                      Fevrier 1997                        *
! *                                                                          *
! ****************************************************************************
!! \date Mai 2013 
!!  \version 3.5
!< 

MODULE MCAL_SU_AJUST

IMPLICIT NONE
PRIVATE
PUBLIC su_ajust_fonct,su_ajust_trans_mat,su_ajust_mult_mat,su_ajust_inv_mat

CONTAINS



! +--------------------------------------------------------------------+
! !                                                                    !
! ! SU_AJUST_FONCT                                                     !
! !                                                                    !
! +--------------------------------------------------------------------+
! !                                                                    !
! ! ROLE: Ajustement de fonctions sinusoidales et polynomiales         !
! ! ----                                                               !
! !       Cette version prevoit un ajustement d'un polynôme et         !
! !       de fonctions sinusoïdales                                    !
! !                                                                    !
! ! Soit A la matrice des estimations des fonctions aux points         !
! ! considérés;                                                        !
! ! Soit C le vecteur des coefficients                                 !
! ! Soit Y le vecteur des ordonnées (observations)                     !
! ! On doit minimiser !!AC - y!! * !!AC - Y!!, ce qui revient à        !
! ! l'équation At.A.C = At.Y                                           !
! !                                                                    !
! ! PARAMETRES EN ENTREE                                               !
! !                                                                    !
! !    ENTREE:                                                         !
! !      lv_pol      : présence (TRUE) ou absence (FALSE)              !
! !                    d'un polynôme                                   !
! !      jv_nb_deg   : nombre de degrés du polynôme                    !
! !      jv_nb_pul   : nombre de pulsations à ajuster                  !
! !      dw_pul      : valeur des pulsations                           !
! !      jv_nb_mes   : nbre de valeurs dans tableaux suivants          !
! !      dw_x        : tableau des abscisses                           !
! !      dw_y        : tableau des ordonnées                           !
! !                                                                    !
! ! DONNEES EN SORTIE                                                  !
! !                                                                    !
! !      jv_nb_coef  : Nombre de coefficients calculés                 !
! !      dw_coeff    : tableau des coefficients des fonctions          !
! !                    ajustées dans l'ordre suivant:                  !
! !                    - constante si demandée                         !
! !                    - coefficient degré 1 du polynôme (si demandé)  !
! !                    - coefficient degré 2 du polynôme (si demandé)  !
! !                    - ............................................  !
! !                    - coefficient Sinus pulsation 1 (si demandé)    !
! !                    - coefficient Cosinus pulsation 2 (si demandé)  !
! !                    - coefficient Sinus pulsation 2 (si demandé)    !
! !                    - coefficient Cosinus pulsation 2 (si demandé)  !
! !                                                                    !
! !      dw_y_ajust  : tableau des ordonnées apres ajustement          !
! !      jv_status   : Compte rendu operation                          !
! !                                                                    !
! +--------------------------------------------------------------------+
! !                                                                    !
! !                                      P. Mambert                    !
! !                                      Fevrier 1997  	        !
! !                                      (D'apres sources J. Dorandeu)	!
! !                                      C.REGNIER Mai 2013 : Conversion F90
! !                                                                    !
! +--------------------------------------------------------------------+

      subroutine su_ajust_fonct(lv_pol     ,& 
     &                          jv_nb_deg  ,&
     &                          jv_nb_pul  ,&
     &                          dw_pul     ,&
     &                          jv_nb_mes  ,&
     &                          dw_x       ,&
     &                          dw_y       ,&
     &                          jv_nb_coef ,&
     &                          dw_coeff   ,&
     &                          dw_y_ajust ,&
     &                          jv_status  )


!  +--------------------------------------------------------------------------+
!  +                              I N C L U D E S                             +
!  +--------------------------------------------------------------------------+

      USE su_glob_typ
      USE su_gmes_typ
      USE su_ajust_typ
      implicit none

!  +--------------------------------------------------------------------------+
!  +           D E C L A R A T I O N   D E S   P A R A M E T R E S            +
!  +--------------------------------------------------------------------------+

      LOGICAL                                            ::      lv_pol              ! (E)
      INTEGER(KIND=4)                                    ::      jv_nb_deg           ! (E)
      INTEGER(KIND=4)                                    ::      jv_nb_pul           ! (E)
      REAL(KIND=8),DIMENSION(jv_nb_pul)                  ::      dw_pul              ! (E)
      INTEGER(KIND=4)                                    ::      jv_nb_mes           ! (E)
      REAL(KIND=8),DIMENSION(jv_nb_mes)                  ::      dw_x,dw_y           ! (E)
 !     REAL(KIND=8),DIMENSION() ::         dw(*)             ! (E)

      INTEGER(KIND=4)                                    ::      jv_nb_coef          ! (S)
      REAL(KIND=8),DIMENSION(jv_nb_coef)                 ::      dw_coeff      ! (S)
      REAL(KIND=8),DIMENSION(jv_nb_mes)                  ::      dw_y_ajust       ! (S)
      INTEGER(KIND=4)                                    ::      jv_status           ! (S)

!  +--------------------------------------------------------------------------+
!  +                      V A R I A B L E S   L O C A L E S                   +
!  +--------------------------------------------------------------------------+

!     matrice A
      REAL(KIND=8),DIMENSION(ec_su_glob_maxMesPerTrace, jc_su_ajust_nb_maxcoef) :: dw_mat_a
!     matrice At
      REAL(KIND=8),DIMENSION(jc_su_ajust_nb_maxcoef, ec_su_glob_maxMesPerTrace) :: dw_mat_at
!     matrice (At.A)
      REAL(KIND=8),DIMENSION(jc_su_ajust_nb_maxcoef, jc_su_ajust_nb_maxcoef)    :: dw_mat_ata 
!     matrice ((At.A)-1 . At)
      REAL(KIND=8),DIMENSION(jc_su_ajust_nb_maxcoef, ec_su_glob_maxMesPerTrace) :: dw_mat_buff 

      INTEGER(KIND=4) :: jv_x,jv_l
      INTEGER(KIND=4) :: jv_coef
      INTEGER(KIND=4) :: jv_deg
      INTEGER(KIND=4) :: jv_pul

      record /s_su_gmes_err/ sv_mes_err  ! Descriptif Erreur

      CHARACTER(LEN=*) ::  tc_nomop
      PARAMETER (tc_nomop = 'SU_AJUST_FONCT')
 
!  +--------------------------------------------------------------------------+
!  +                                C O D E                                   +
!  +--------------------------------------------------------------------------+


      jv_status = jc_su_glob_ok
      sv_mes_err.tv_nomop = tc_nomop

!     Calcul du nombre de coefficients à déterminer
!     ---------------------------------------------
      jv_nb_coef = 2 * jv_nb_pul	! 2 fois le nombre de frequences
     
!     On rajoute un coefficient pour la constante et autant de coefficients
!     que de degrés du polynôme
      jv_nb_coef = jv_nb_coef + 1

      if (lv_pol) then
        jv_nb_coef = jv_nb_coef + jv_nb_deg
      endif

      print*,jv_nb_coef,jv_nb_mes

!     Controle taille matrice
      if ( (jv_nb_mes .gt. ec_su_glob_maxMesPerTrace) .or. &
     &     (jv_nb_coef .gt. jc_su_ajust_nb_maxcoef) ) then

         sv_mes_err.ev_num_mes  = ec_su_gmes_mdim
         sv_mes_err.tw_param(1) = 'Matrice A'
         sv_mes_err.jw_param(1) = jv_nb_coef
         call su_gmes_err(sv_mes_err)
         jv_status = jc_su_glob_nok
         goto 9999
      endif


!     Calcul de la matrice A des fonctions aux points abscisses
!     ---------------------------------------------------------
      do jv_x = 1 , jv_nb_mes

         print*,dw_x(jv_x),dw_y(jv_x)

         jv_coef = 1

         dw_mat_a(jv_x, jv_coef) = 1.
         jv_coef = jv_coef + 1

         if (lv_pol) then
            do jv_deg = 1 , jv_nb_deg
               dw_mat_a(jv_x, jv_coef) = dw_x(jv_x) ** jv_deg
               jv_coef = jv_coef + 1
            enddo
         endif

         do jv_pul = 1 , jv_nb_pul
            dw_mat_a(jv_x, jv_coef) = sin(dw_pul(jv_pul) * dw_x(jv_x))
            jv_coef = jv_coef + 1            
            dw_mat_a(jv_x, jv_coef) = cos(dw_pul(jv_pul) * dw_x(jv_x))
            jv_coef = jv_coef + 1 
         enddo  

         do jv_l = 1 , jv_nb_coef
            print*,jv_x,jv_l,dw_mat_a(jv_x, jv_l)
            enddo

      enddo


!     Calcul de At . A
!     ----------------
!     calcul de At
      call su_ajust_trans_mat (dw_mat_a   , &                      
     &                         ec_su_glob_maxMesPerTrace , &
     &                         jc_su_ajust_nb_maxcoef, &
     &                         jv_nb_mes  , &
     &                         jv_nb_coef , &
     &                         dw_mat_at  )

      do jv_l = 1 , jv_nb_coef
         do jv_x = 1 , jv_nb_mes
            print*,jv_l,jv_x,dw_mat_at(jv_l,jv_x)
         enddo
      enddo


      print*,'Calcul de At . A'

!     calcul de At . A
      call su_ajust_mult_mat (dw_mat_at  , &
     &                        jc_su_ajust_nb_maxcoef, &
     &                        ec_su_glob_maxMesPerTrace , &
     &                        jv_nb_coef , &
     &                        jv_nb_mes  , &
     &                        dw_mat_a   , &
     &                        ec_su_glob_maxMesPerTrace , &
     &                        jc_su_ajust_nb_maxcoef, &
     &                        jv_nb_mes  , &
     &                        jv_nb_coef , &
     &                        dw_mat_ata , &
     &                        jv_status  ) 
      if (jv_status .ne. jc_su_glob_ok) then
        goto 9999
      endif


!     Inversion de la matrice At.A (calcul de (At.A)-1)
!     -------------------------------------------------
!     dw_mat_ata : At.A devient (At.A)-1
      call su_ajust_inv_mat (dw_mat_ata    , &
     &                       jv_nb_coef    , &
     &                       jc_su_ajust_nb_maxcoef , &
     &                       jv_status     )
      if (jv_status .ne. jc_su_glob_ok) then
        goto 9999
      endif

      print*,'Inversion'


!     Calcul des coefficients (dw_coeff = (At.A)-1 . At . Y)
!     ------------------------------------------------------
!     produit matriciel en 2 etapes via dw_mat_buff
!     calcul de (At.A)-1 . At
      call su_ajust_mult_mat (dw_mat_ata    , &
     &                        jc_su_ajust_nb_maxcoef, &
     &                        jc_su_ajust_nb_maxcoef, &
     &                        jv_nb_coef    , &
     &                        jv_nb_coef    , &
     &                        dw_mat_at     , &
     &                        jc_su_ajust_nb_maxcoef, &
     &                        ec_su_glob_maxMesPerTrace , &
     &                        jv_nb_coef    , &
     &                        jv_nb_mes     , &
     &                        dw_mat_buff   , &
     &                        jv_status     )
      if (jv_status .ne. jc_su_glob_ok) then
        goto 9999
      endif

!     calcul de ((At.A)-1 . At) . Y
      call su_ajust_mult_mat (dw_mat_buff   , & 
     &                        jc_su_ajust_nb_maxcoef, &
     &                        ec_su_glob_maxMesPerTrace , &
     &                        jv_nb_coef    , &
     &                        jv_nb_mes     , &
     &                        dw_y          , &
     &                        ec_su_glob_maxMesPerTrace , &
     &                        1             , &
     &                        jv_nb_mes     , & 
     &                        1             , &
     &                        dw_coeff      , &
     &                        jv_status     )
      if (jv_status .ne. jc_su_glob_ok) then
        goto 9999
      endif


!     Calcul des ordonnees de la fonction ajustee
!     -------------------------------------------
      do jv_x = 1 , jv_nb_mes

         jv_coef = 1

!        constante
         dw_y_ajust(jv_x) = dw_coeff(jv_coef)
         jv_coef = jv_coef + 1

!        ajustement polynomial
         if (lv_pol) then
            do jv_deg = 1 , jv_nb_deg
               dw_y_ajust(jv_x) = dw_y_ajust(jv_x) + &
     &              dw_coeff(jv_coef) * dw_x(jv_x) ** jv_deg
               jv_coef = jv_coef + 1
            enddo
         endif

!        ajustement sinusoidal
         do jv_pul = 1 , jv_nb_pul
            dw_y_ajust(jv_x) = dw_y_ajust(jv_x) + &
     &           dw_coeff(jv_coef) * cos(dw_pul(jv_pul) * dw_x(jv_x)) + &
     &           dw_coeff(jv_coef+1) * sin(dw_pul(jv_pul) * dw_x(jv_x))
            jv_coef = jv_coef + 2
         enddo  

      enddo


 9999 continue
      return
      end subroutine su_ajust_fonct


! +--------------------------------------------------------------------+
! !                                                                    !
! ! SU_AJUST_TRANS_MAT                                                 !
! !                                                                    !
! +--------------------------------------------------------------------+
! !                                                                    !
! ! ROLE: Calcul matriciel : calcule At (transposee de A) a partir de A!
! ! ----                                                               !
! !                                                                    !
! ! PARAMETRES EN ENTREE                                               !
! !                                                                    !
! !    ENTREE:                                                         !
! !      dw_mat_A    : matrice de depart A                             !
! !      jv_dim1     : dimension memoire de la matrice (lignes)        !
! !      jv_dim2     : dimension memoire de la matrice (colonnes)      !
! !      jv_nlig     : nombre de lignes                                !
! !      jv_ncol     : nombre de colonnes                              !
! !                                                                    !
! ! DONNEES EN SORTIE                                                  !
! !                                                                    !
! !      dw_mat_At   : matrice At                                      !
! !                                                                    !
! +--------------------------------------------------------------------+
! !                                                                    !
! !                                      P. Mambert                    !
! !                                      Fevrier 1997  		      !
! +--------------------------------------------------------------------+

      subroutine su_ajust_trans_mat (dw_mat_A ,& 
     &                               jv_dim1  ,&
     &                               jv_dim2  ,&
     &                               jv_nlig  ,&
     &                               jv_ncol  ,&
     &                               dw_mat_At)

      implicit none

!  +--------------------------------------------------------------------------+
!  +                              I N C L U D E S                             +
!  +--------------------------------------------------------------------------+

!  +--------------------------------------------------------------------------+
!  +           D E C L A R A T I O N   D E S   P A R A M E T R E S            +
!  +--------------------------------------------------------------------------+

      INTEGER(KIND=4) ::      jv_dim1           		! (E)
      INTEGER(KIND=4) ::      jv_dim2           		! (E)
      REAL(KIND=8) ::	     dw_mat_A(jv_dim1, jv_dim2)	! (E)
      INTEGER(KIND=4) ::      jv_nlig           		! (E)
      INTEGER(KIND=4) ::      jv_ncol           		! (E)

      REAL(KIND=8) ::	     dw_mat_At(jv_dim2, jv_dim1)! (S)

!  +--------------------------------------------------------------------------+
!  +                      V A R I A B L E S   L O C A L E S                   +
!  +--------------------------------------------------------------------------+

      INTEGER(KIND=4) ::	     jv_i
      INTEGER(KIND=4) ::	     jv_j

!  +--------------------------------------------------------------------------+
!  +                                C O D E                                   +
!  +--------------------------------------------------------------------------+

      do jv_i = 1, jv_nlig
        do jv_j = 1, jv_ncol

            dw_mat_At(jv_j, jv_i) = dw_mat_A(jv_i, jv_j)

        enddo
      enddo

      return
      end subroutine su_ajust_trans_mat
         

! +--------------------------------------------------------------------+
! !                                                                    !
! ! SU_AJUST_MULT_MAT                                                  !
! !                                                                    !
! +--------------------------------------------------------------------+
! !                                                                    !
! ! ROLE: Produit matriciel : calcule A.B a partir de A et B           !
! ! ----                                                               !
! !                                                                    !
! ! PARAMETRES EN ENTREE                                               !
! !                                                                    !
! !    ENTREE:                                                         !
! !      dw_mat_A    : matrice de depart A                             !
! !      jv_dimA1    : dimension memoire de A (lignes)                 !
! !      jv_dimA2    : dimension memoire de A (colonnes)               !
! !      jv_nligA    : nombre de lignes                                !
! !      jv_ncolA    : nombre de colonnes                              !
! !      dw_mat_B    : matrice de depart B                             !
! !      jv_dimB1    : dimension memoire de B (lignes)                 !
! !      jv_dimB2    : dimension memoire de B (colonnes)               !
! !      jv_nligB    : nombre de lignes                                !
! !      jv_ncolB    : nombre de colonnes                              !
! !      jv_status   : compte-rendu d'execution                        !
! !                                                                    !
! ! DONNEES EN SORTIE                                                  !
! !                                                                    !
! !      dw_mat_AB   : matrice AB = A.B                                !
! !                                                                    !
! +--------------------------------------------------------------------+
! !                                                                    !
! !                                      P. Mambert                    !
! !                                      Fevrier 1997  	        !
! +--------------------------------------------------------------------+

      subroutine su_ajust_mult_mat  (dw_mat_A ,&  
     &                               jv_dimA1 ,&
     &                               jv_dimA2 ,&
     &                               jv_nligA ,&
     &                               jv_ncolA ,&
     &                               dw_mat_B ,&
     &                               jv_dimB1 ,&
     &                               jv_dimB2 ,&
     &                               jv_nligB ,&
     &                               jv_ncolB ,&
     &                               dw_mat_AB,&
     &                               jv_status)


!  +--------------------------------------------------------------------------+
!  +                              I N C L U D E S                             +
!  +--------------------------------------------------------------------------+
      USE su_glob_typ
      USE su_gmes_typ
      implicit none
      
!  +--------------------------------------------------------------------------+
!  +           D E C L A R A T I O N   D E S   P A R A M E T R E S            +
!  +--------------------------------------------------------------------------+

      INTEGER(KIND=4) ::      jv_dimA1           ! (E)
      INTEGER(KIND=4) ::      jv_dimA2           ! (E)
      REAL(KIND=8) ::	     dw_mat_A(jv_dimA1, jv_dimA2)	! (E)
      INTEGER(KIND=4) ::      jv_nligA           ! (E)
      INTEGER(KIND=4) ::      jv_ncolA           ! (E)
      INTEGER(KIND=4) ::      jv_dimB1           ! (E)
      INTEGER(KIND=4) ::      jv_dimB2           ! (E)
      REAL(KIND=8) ::	     dw_mat_B(jv_dimB1, jv_dimB2)	! (E)
      INTEGER(KIND=4) ::      jv_nligB           ! (E)
      INTEGER(KIND=4) ::      jv_ncolB           ! (E)

      REAL(KIND=8) ::	     dw_mat_AB(jv_dimA1, jv_dimB2)	! (S)
      INTEGER(KIND=4) ::      jv_status 	        		! (S)

!  +--------------------------------------------------------------------------+
!  +                      V A R I A B L E S   L O C A L E S                   +
!  +--------------------------------------------------------------------------+

      RECORD / S_SU_GMES_ERR / sv_mes_err
      CHARACTER(LEN=*) ::  tc_nomop
      PARAMETER      (tc_nomop = 'SU_AJUST_MULT_MAT')

      INTEGER(KIND=4) ::	     jv_i
      INTEGER(KIND=4) ::	     jv_j
      INTEGER(KIND=4) ::	     jv_k

!  +--------------------------------------------------------------------------+
!  +                                C O D E                                   +
!  +--------------------------------------------------------------------------+

      jv_status = jc_su_glob_ok
      sv_mes_err.tv_nomop = tc_nomop

!     Controle dimensions communes pour produit
      if (jv_ncolA .ne. jv_nligB) then
         sv_mes_err.ev_num_mes = ec_su_gmes_mprodmat
         CALL SU_GMES_ERR(sv_mes_err)
         jv_status = jc_su_glob_nok
         goto 9999
       endif

!     Calcul produit matriciel
      do jv_i = 1, jv_nligA
        do jv_j = 1, jv_ncolB

          dw_mat_AB(jv_i, jv_j) = 0.D0
          do jv_k = 1, jv_ncolA
            dw_mat_AB(jv_i, jv_j) = dw_mat_AB(jv_i, jv_j) + &
     &                              dw_mat_A(jv_i, jv_k) * dw_mat_B(jv_k, jv_j)
          enddo

        enddo
      enddo

 9999 continue
      return
      end subroutine su_ajust_mult_mat
         

! +--------------------------------------------------------------------+
! !                                                                    !
! ! SU_AJUST_INV_MAT                                                   !
! !                                                                    !
! +--------------------------------------------------------------------+
! !                                                                    !
! ! ROLE: Inversion de la matrice A : calcule A-1 a partir de A        !
! ! ----                                                               !
! !                                                                    !
! ! PARAMETRES EN ENTREE                                               !
! !                                                                    !
! !    ENTREE:                                                         !
! !      dw_mat_A    : matrice de depart A                             !
! !      jv_ordre    : ordre de la matrice                             !
! !      jv_dim      : dimension (memoire) de A                        !
! !                                                                    !
! ! DONNEES EN SORTIE                                                  !
! !                                                                    !
! !      dw_mat_A    : matrice inverse A-1                             !
! !      jv_status   : compte-rendu d'execution                        !
! !                                                                    !
! ! Utilisation de la librairie FORTRAN NAG			        !
! ! (routines DPOTRF et DPOTRI)					!
! +--------------------------------------------------------------------+
! !                                                                    !
! !                                      P. Mambert                    !
! !                                      Fevrier 1997  	        !
! +--------------------------------------------------------------------+

      subroutine su_ajust_inv_mat  (dw_mat_A ,& 
     &                              jv_ordre ,&
     &                              jv_dim   ,&
     &                              jv_status)


!  +--------------------------------------------------------------------------+
!  +                              I N C L U D E S                             +
!  +--------------------------------------------------------------------------+
      USE su_glob_typ
      USE su_gmes_typ
      implicit none
      
!  +--------------------------------------------------------------------------+
!  +           D E C L A R A T I O N   D E S   P A R A M E T R E S            +
!  +--------------------------------------------------------------------------+

      INTEGER(KIND=4) ::      jv_dim             ! (E)
      REAL(KIND=8) ::	     dw_mat_A(jv_dim, jv_dim)	! (E/S)
      INTEGER(KIND=4) ::      jv_ordre           ! (E)

      INTEGER(KIND=4) ::      jv_status          ! (S)

!  +--------------------------------------------------------------------------+
!  +                      V A R I A B L E S   L O C A L E S                   +
!  +--------------------------------------------------------------------------+

      RECORD / S_SU_GMES_ERR / sv_mes_err
      CHARACTER(LEN=*) :: tc_nomop
      PARAMETER     (tc_nomop = 'SU_AJUST_INV_MAT')

!     parametres d'appel aux fonctions NAG
      INTEGER(KIND=4) ::      jv_info

      INTEGER(KIND=2) ::      ev_i
      INTEGER(KIND=2) ::      ev_j

!  +--------------------------------------------------------------------------+
!  +                                C O D E                                   +
!  +--------------------------------------------------------------------------+

      jv_status = jc_su_glob_ok
      sv_mes_err.tv_nomop = tc_nomop


!     Factoriser A
!     ------------
      call dpotrf ( 'U'      ,& 
     &              jv_ordre ,&
     &              dw_mat_A ,&
     &              jv_dim   ,&
     &              jv_info  )

      if (jv_info .ne. 0) then
        sv_mes_err.ev_num_mes = ec_su_gmes_mnag
        sv_mes_err.tw_param(1) = 'DPOTRF'
        sv_mes_err.jw_param(1) = jv_info
        CALL SU_GMES_ERR(sv_mes_err)
        jv_status = jc_su_glob_nok
        goto 9999
      endif


!     Inverser A
!     ----------
!     Seule la partie superieure de la matrice est inversee
      call dpotri ( 'U'      ,& 
     &              jv_ordre ,&
     &              dw_mat_A ,&
     &              jv_dim   ,&
     &              jv_info  )

      if (jv_info .ne. 0) then
        sv_mes_err.ev_num_mes = ec_su_gmes_mnag
        sv_mes_err.tw_param(1) = 'DPOTRI'
        sv_mes_err.jw_param(1) = jv_info
        CALL SU_GMES_ERR(sv_mes_err)
        jv_status = jc_su_glob_nok
        goto 9999
      endif

!     Completer la partie inferieure par symetrie
!     -------------------------------------------
      do ev_i = 1, jv_ordre
        do ev_j = 1, ev_i-1
          dw_mat_A(ev_i, ev_j) = dw_mat_A(ev_j, ev_i)
        enddo
      enddo

 9999 continue
      return
      end subroutine su_ajust_inv_mat


END MODULE MCAL_SU_AJUST 
