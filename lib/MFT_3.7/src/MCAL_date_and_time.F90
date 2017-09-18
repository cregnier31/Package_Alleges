MODULE MCAL_date_and_time

  IMPLICIT NONE

   !! * Private variables
   INTEGER, PARAMETER      :: rlg = 8

  CONTAINS

  SUBROUTINE decompdate( date, jj, mm, aaaa, hh, min, sec )

   !&E---------------------------------------------------------------------
   !&E                 ***  ROUTINE decompdate  ***
   !&E
   !&E ** Purpose : decompose une date entree sous forme de chaine de
   !&E              caractere de longueur 19 de la forme 
   !&E              "aaaa-mm-jj hh:mm:ss"
   !&E              en des entiers donnant le jour (jj) le mois (mm)
   !&E              l annee (aaaa), l heure (hh), la minute (min) et la
   !&E              seconde (sec)
   !&E
   !&E ** Description :
   !&E
   !&E ** Called by :
   !&E
   !&E ** External calls :
   !&E
   !&E ** Reference :
   !&E
   !&E ** History :
   !&E       !  2004-08  (F. Dumas, IFREMER)
   !&E       !  2008-11  (B. Levier)
   !&E
   !&E---------------------------------------------------------------------

   !! * Arguments
   CHARACTER(LEN=19), INTENT( IN )   ::  date
   INTEGER,           INTENT( OUT )  ::  jj, mm, aaaa, hh, min, sec  

   !! * Local declarations
   LOGICAL                       :: cont
   CHARACTER(LEN=2)              :: mois
   CHARACTER(LEN=2),DIMENSION(12):: tab_mois=(/'01','02','03','04','05','06',&
                                               '07','08','09','10','11','12'/)
   INTEGER                       :: i

   !!----------------------------------------------------------------------
   !! * Executable part

   READ(date,800) aaaa, mois, jj, hh, min, sec

   cont = .TRUE.
   i = 1
!
   DO WHILE( cont .AND. i <= 12)
     IF( tab_mois(i) == mois ) THEN
       cont = .FALSE.
     ELSE
       i = i +1
     END IF
   END DO
!
   IF(i <= 12) THEN
     mm = i
   ELSE
     PRINT *, 'erreur decompdate : impossible de determiner le mois'
     STOP
   END IF
!
 800 FORMAT(I4,1X,A2,1X,I2,1X,2(I2,1X),I2)
!
   END SUBROUTINE decompdate

  FUNCTION SEC_TO_JUL_DAY( dv_sec, date_ref )
!
!****
!
!     Purpose:
!     --------
!     Conversion d'une date exprimee en nombre de secondes
!     ecoulees depuis une date de reference quelconque en une date
!     exprimee sous la forme de nombre juliens CNES (1950)
!     Input : \\
!     ------
!     \begin{itemize}
!     \item dv_sec   : nombre de secondes ecoulees depuis une date de reference
!     \item date_ref : date de reference du type "2002-12-30 00:00:00"
!
!     Output : \\
!     SEC_TO_JUL_DAY : date en jour juliens
!
!
!***  Method:
!     -------
!
!     Externals:
!     ----------
!     History:
!     --------
!      Version    Programmer      Date            Description
!      -------    ----------      ----            -----------
!      1.0        P.Sicard        14 Avril 1995   Machine for CLS Space Oceanography Group
!      1.1        C.REGNIER        Mars 2007      Revision in Fortran 90 + Creation routine pour psy3v2
!      1.2        B.LEVIER         Nov  2008      D apres routine datosec de F. Dumas (IFREMER)
!*--------------------------------------------------------------------------
!
!
!**   0. DECLARATIONS
!        ------------
!
   CHARACTER(LEN=19), INTENT( IN )   ::  date_ref
   REAL (KIND=rlg),   INTENT( IN )   ::  dv_sec
   REAL (KIND=rlg)                   ::  sec_to_jul_day

   !! * Local declarations
   INTEGER                     :: annee, mois, jour, heure, minute, seconde
   INTEGER, DIMENSION(12), PARAMETER ::         &
           jours_avt_mois=(/0,31,59,90,120,151,181,212,243,273,304,334/)
   REAL(KIND=rlg), PARAMETER   :: secs_in_minute = 60.d0,                   &
                                  secs_in_heure  = 3600.d0,                 &
                                  secs_in_jour   = 86400.d0,                &
                                  secs_in_annee  = secs_in_jour * 365.d0,   &
                                  secs_in_siecle = secs_in_jour * 36524.d0, &
                                  !                1900/01/01    1950/01/01
                                  tref           = 59958230400.d0 + 1577836800.d0
   REAL(KIND=rlg)              :: total_secs

!    !*--------------------------------------------------------------------------
!
!**    TEST d'entree
     IF ( dv_sec < 0.d0 ) STOP

!** Number of seconds between the reference date and origin

   !-- Characters to numerics
   CALL decompdate( date_ref, jour, mois, annee, heure, minute, seconde )

   !-- Add number of seconds for each century since origin
   total_secs = secs_in_siecle * INT( annee / 100 )

   !-- Add one day every 400 years  (bissextil century)
   !-- Remark: 0.9975 = (1 - 1/400) for year 0 which is bissextil
   total_secs = total_secs + secs_in_jour * INT( DBLE(annee)/400.0 + 0.9975 )

   !-- Add each year from the begenning of the last century
   total_secs = total_secs + secs_in_annee * MOD( annee, 100 )

   !-- Add one day for each bissextil year since the last century
   total_secs = total_secs + secs_in_jour*INT( (MOD( annee, 100) - 1) / 4 )

   !-- Add each month since the beginning of the year
   total_secs = total_secs + jours_avt_mois(mois) * secs_in_jour

   !-- Add one day if we are after february and the year is bissextil
   !
   IF( mois > 2 ) THEN
     IF( MOD( annee, 400 ) == 0 ) THEN
       total_secs = total_secs + secs_in_jour
     ELSE
       IF ( (MOD( annee, 4 ) == 0) .AND. ( MOD( annee, 100 ) /= 0 ) ) &
         total_secs = total_secs + secs_in_jour
     END IF
   END IF

   !-- Add days
   total_secs = total_secs + secs_in_jour * (jour-1)

   !-- Add hours
   total_secs = total_secs + secs_in_heure * (heure)

   !-- Add minutes
   total_secs = total_secs + secs_in_minute * (minute)

   !-- Add seconds
   total_secs     = total_secs + seconde

!** Number of seconds between the reference date and 1950/01/01

   sec_to_jul_day = total_secs - tref

!** Number of seconds since 1950/01/01

   sec_to_jul_day = sec_to_jul_day + dv_sec

!** Julian days since 1950/01/01

   sec_to_jul_day = sec_to_jul_day / secs_in_jour
    
END FUNCTION SEC_TO_JUL_DAY

END MODULE MCAL_date_and_time
