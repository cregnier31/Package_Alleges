!> \brief Module which contain Interfaces for READ WRITE Byte values in NETCDF format
!! \author C.REGNIER Miol V3.5
!! \date Janvier 2013
!!  \version 3.5
!<
MODULE INT_read_write_B
  implicit none
  ! -----------------------------------------------------------------
  ! --- General interface
  ! ---  MIOL_read_field_NC interface 
  ! --  MIOL_readf_field_B_4D_NC (cd_filename,cd_varname,idpa_value,ida_dimsize)
  ! --  MIOL_readf_field_B_3D_NC (cd_filename,cd_varname,idpa_value,ida_dimsize)
  ! --  MIOL_readf_field_B_2D_NC (cd_filename,cd_varname,idpa_value,ida_dimsize)
  ! --  MIOL_readf_field_B_1D_NC (cd_filename,cd_varname,idpa_value,ida_dimsize)
  ! --  MIOL_readu_field_B_4D_NC (id_file_id,cd_varname,idpa_value,ida_dimsize) 
  ! --  MIOL_readu_field_B_3D_NC (id_file_id,cd_varname,idpa_value,ida_dimsize) 
  ! --  MIOL_readu_field_B_2D_NC (id_file_id,cd_varname,idpa_value,ida_dimsize) 
  ! --  MIOL_readu_field_B_1D_NC (id_file_id,cd_varname,idpa_value,ida_dimsize) 
  ! ------------------------------------------------------------------------------
  ! ---  MIOL_write_field_NC interface
  ! --  MIOL_writef_field_B_4D_NC (cd_filename,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
  ! --  MIOL_writef_field_B_3D_NC (cd_filename,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
  ! --  MIOL_writef_field_B_2D_NC (cd_filename,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
  ! --  MIOL_writef_field_B_1D_NC (cd_filename,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
  ! --  MIOL_writeu_field_B_4D_NC (id_file_id,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
  ! --  MIOL_writeu_field_B_3D_NC (id_file_id,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
  ! --  MIOL_writeu_field_B_2D_NC (id_file_id,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
  ! --  MIOL_writeu_field_B_1D_NC (id_file_id,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
  ! ------------------------------------------------------------------------------
  ! -- C.REGNIER Janvier   2010 : changement de l'interface (module procedure)   
  ! -- C.REGNIER Avril     2010 : retour à l'interface classique  
  !**

!> An interface for read Byte Values in NETCDF
!!
!<
INTERFACE MIOL_read_field_NC

 SUBROUTINE MIOL_readf_field_B_4D_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
   IMPLICIT NONE
   CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
   CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
   INTEGER(KIND=1), DIMENSION(:,:,:,:), POINTER :: idpa_value
   INTEGER, DIMENSION(4), OPTIONAL,     INTENT(OUT) :: ida_dimsize
END SUBROUTINE MIOL_readf_field_B_4D_NC

SUBROUTINE MIOL_readf_field_B_3D_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
  IMPLICIT NONE
  CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
  CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
  INTEGER(KIND=1), DIMENSION(:,:,:), POINTER :: idpa_value
  INTEGER, DIMENSION(3), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 END SUBROUTINE MIOL_readf_field_B_3D_NC
  SUBROUTINE MIOL_readf_field_B_2D_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
    IMPLICIT NONE
    CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
    CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
    INTEGER(KIND=1), DIMENSION(:,:), POINTER :: idpa_value
    INTEGER, DIMENSION(2), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 END SUBROUTINE MIOL_readf_field_B_2D_NC
  SUBROUTINE MIOL_readf_field_B_1D_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
    IMPLICIT NONE
    CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
    CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
    INTEGER(KIND=1), DIMENSION(:), POINTER:: idpa_value
    INTEGER, DIMENSION(1), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 END SUBROUTINE MIOL_readf_field_B_1D_NC

 SUBROUTINE MIOL_readu_field_B_4D_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
   IMPLICIT NONE
   INTEGER,                           INTENT(IN) :: id_file_id
   CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
   INTEGER(KIND=1), DIMENSION(:,:,:,:), POINTER :: idpa_value
   INTEGER, DIMENSION(4), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 END SUBROUTINE MIOL_readu_field_B_4D_NC

   SUBROUTINE MIOL_readu_field_B_3D_NC (id_file_id, &
                                        cd_varname, &
                                        idpa_value, &
                                        ida_dimsize)
    IMPLICIT NONE
    INTEGER,                           INTENT(IN) :: id_file_id
    CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
    INTEGER(KIND=1), DIMENSION(:,:,:), POINTER :: idpa_value
    INTEGER, DIMENSION(3), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 END SUBROUTINE MIOL_readu_field_B_3D_NC
   SUBROUTINE MIOL_readu_field_B_2D_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
     IMPLICIT NONE
     INTEGER,                           INTENT(IN) :: id_file_id
     CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
     INTEGER(KIND=1), DIMENSION(:,:), POINTER :: idpa_value
     INTEGER, DIMENSION(2), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 END SUBROUTINE MIOL_readu_field_B_2D_NC
 SUBROUTINE MIOL_readu_field_B_1D_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
   IMPLICIT NONE
   INTEGER,                           INTENT(IN) :: id_file_id
   CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
   INTEGER(KIND=1), DIMENSION(:), POINTER :: idpa_value
   INTEGER, DIMENSION(1), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 END SUBROUTINE MIOL_readu_field_B_1D_NC
END INTERFACE MIOL_read_field_NC
!**
!**
!> An interface for write Byte Values in NETCDF
!!
!<
INTERFACE MIOL_write_field_NC

 SUBROUTINE MIOL_writef_field_B_4D_NC (cd_filename, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)
 

   IMPLICIT NONE
   CHARACTER(LEN=*),            INTENT(IN) :: cd_filename
   CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
   CHARACTER(LEN=4),            INTENT(IN) :: cd_key
   INTEGER(KIND=1), DIMENSION(:,:,:,:), INTENT(IN) :: ida_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
   INTEGER(KIND=1), DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
 END SUBROUTINE MIOL_writef_field_B_4D_NC
SUBROUTINE MIOL_writef_field_B_3D_NC (cd_filename, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)

  IMPLICIT NONE
  CHARACTER(LEN=*),            INTENT(IN) :: cd_filename
  CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
  CHARACTER(LEN=3),            INTENT(IN) :: cd_key
  INTEGER(KIND=1), DIMENSION(:,:,:), INTENT(IN) :: ida_varvalue
  REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
  INTEGER(KIND=1), DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
 END SUBROUTINE MIOL_writef_field_B_3D_NC
 SUBROUTINE MIOL_writef_field_B_2D_NC (cd_filename, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)
  IMPLICIT NONE
  CHARACTER(LEN=*),            INTENT(IN) :: cd_filename
  CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
  CHARACTER(LEN=2),            INTENT(IN) :: cd_key
  INTEGER(KIND=1), DIMENSION(:,:), INTENT(IN) :: ida_varvalue
  REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
  INTEGER(KIND=1), DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
END SUBROUTINE MIOL_writef_field_B_2D_NC
 SUBROUTINE MIOL_writef_field_B_1D_NC (cd_filename, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)
   IMPLICIT NONE
   CHARACTER(LEN=*),            INTENT(IN) :: cd_filename
   CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
   CHARACTER(LEN=1),            INTENT(IN) :: cd_key
   INTEGER(KIND=1), DIMENSION(:), INTENT(IN) :: ida_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
   INTEGER(KIND=1), DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
 END SUBROUTINE MIOL_writef_field_B_1D_NC
 SUBROUTINE MIOL_writeu_field_B_4D_NC (id_file_id, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)
    IMPLICIT NONE
    INTEGER,                     INTENT(IN) :: id_file_id
    CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
    CHARACTER(LEN=4),            INTENT(IN) :: cd_key
    INTEGER(KIND=1), DIMENSION(:,:,:,:), INTENT(IN) :: ida_varvalue
    REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
    INTEGER(KIND=1), DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
END SUBROUTINE MIOL_writeu_field_B_4D_NC 
   SUBROUTINE MIOL_writeu_field_B_3D_NC (id_file_id, &
                                         cd_varname, &
                                         cd_key, &
                                         ida_varvalue, &
                                         rda_offsetvalue, &
                                         ida_specialvalue)
     IMPLICIT NONE
     INTEGER,                     INTENT(IN) :: id_file_id
     CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
     CHARACTER(LEN=3),            INTENT(IN) :: cd_key
     INTEGER(KIND=1), DIMENSION(:,:,:), INTENT(IN) :: ida_varvalue
     REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
     INTEGER(KIND=1), DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
   END SUBROUTINE MIOL_writeu_field_B_3D_NC
    SUBROUTINE MIOL_writeu_field_B_2D_NC (id_file_id, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)
      IMPLICIT NONE
      INTEGER,            INTENT(IN) :: id_file_id
      CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
      CHARACTER(LEN=2),            INTENT(IN) :: cd_key
      INTEGER(KIND=1), DIMENSION(:,:), INTENT(IN) :: ida_varvalue
      REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
      INTEGER(KIND=1), DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
 END SUBROUTINE MIOL_writeu_field_B_2D_NC 
 SUBROUTINE MIOL_writeu_field_B_1D_NC (id_file_id, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)
   
   IMPLICIT NONE
   INTEGER,                     INTENT(IN) :: id_file_id
   CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
   CHARACTER(LEN=1),            INTENT(IN) :: cd_key
   INTEGER(KIND=1), DIMENSION(:), INTENT(IN) :: ida_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
   INTEGER(KIND=1), DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
END  SUBROUTINE MIOL_writeu_field_B_1D_NC
END INTERFACE MIOL_write_field_NC
!**
END MODULE INT_read_write_B
