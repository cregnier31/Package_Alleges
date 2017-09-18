!> \brief Module which contain Interfaces for READ WRITE Integer values in NETCDF format
!! \author C.REGNIER Miol V3.5
!! \date September 2008
!!  \version 3.5
!<
MODULE INT_read_write_INTEGER
  implicit none
  ! -----------------------------------------------------------------
  ! --- General interface
  ! ---  MIOL_read_field_NC interface
  ! ---  MIOL_readf_field_I4_4D_NC(cd_filename,cd_varname,idpa_value,ida_dimsize)
  ! ---  MIOL_readf_field_I4_3D_NC(cd_filename,cd_varname,idpa_value,ida_dimsize)
  ! ---  MIOL_readf_field_I4_2D_NC(cd_filename,cd_varname,idpa_value,ida_dimsize)
  ! ---  MIOL_readf_field_I4_1D_NC(cd_filename,cd_varname,idpa_value,ida_dimsize)
  ! ---  MIOL_readu_field_I4_4D_NC (id_file_id,cd_varname,idpa_value,ida_dimsize)
  ! ---  MIOL_readu_field_I4_3D_NC (id_file_id,cd_varname,idpa_value,ida_dimsize)
  ! ---  MIOL_readu_field_I4_2D_NC (id_file_id,cd_varname,idpa_value,ida_dimsize)
  ! ---  MIOL_readu_field_I4_1D_NC (id_file_id,cd_varname,idpa_value,ida_dimsize)
  ! -----------------------------------------------------------------
  ! ---  MIOL_write_field_NC interface
  ! -- SUBROUTINE MIOL_writef_field_I4_4D_NC (cd_filename,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
  ! -- SUBROUTINE MIOL_writef_field_I4_3D_NC (cd_filename,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
  ! -- SUBROUTINE MIOL_writef_field_I4_2D_NC (cd_filename,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
  ! -- SUBROUTINE MIOL_writef_field_I4_1D_NC (cd_filename,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
  ! -- SUBROUTINE MIOL_writeu_field_I4_4D_NC (id_file_id,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
  ! -- SUBROUTINE MIOL_writeu_field_I4_3D_NC (id_file_id,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
  ! -- SUBROUTINE MIOL_writeu_field_I4_2D_NC (id_file_id,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
  ! -- SUBROUTINE MIOL_writeu_field_I4_1D_NC (id_file_id,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
!> An interface for read Integer Values in NETCDF
!!
!<

INTERFACE MIOL_read_field_NC
 SUBROUTINE MIOL_readf_field_I4_4D_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
    IMPLICIT NONE
    CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
    CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
    INTEGER(KIND=4), DIMENSION(:,:,:,:), POINTER :: idpa_value
    INTEGER(KIND=4), DIMENSION(4), OPTIONAL,     INTENT(OUT) :: ida_dimsize
END  SUBROUTINE MIOL_readf_field_I4_4D_NC
   SUBROUTINE MIOL_readf_field_I4_3D_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
    IMPLICIT NONE
    CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
    CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
    INTEGER(KIND=4), DIMENSION(:,:,:), POINTER :: idpa_value
    INTEGER(KIND=4), DIMENSION(3), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 END SUBROUTINE MIOL_readf_field_I4_3D_NC

 SUBROUTINE MIOL_readf_field_I4_2D_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
    IMPLICIT NONE
    CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
    CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
    INTEGER(KIND=4), DIMENSION(:,:), POINTER :: idpa_value
    INTEGER(KIND=4), DIMENSION(2), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 END SUBROUTINE MIOL_readf_field_I4_2D_NC
 
 SUBROUTINE MIOL_readf_field_I4_1D_NC (cd_filename, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
    IMPLICIT NONE
    CHARACTER(LEN=*),                  INTENT(IN) :: cd_filename
    CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
    INTEGER(KIND=4), DIMENSION(:), POINTER  :: idpa_value
    INTEGER(KIND=4), DIMENSION(1), OPTIONAL,     INTENT(OUT) :: ida_dimsize
 END SUBROUTINE MIOL_readf_field_I4_1D_NC
 SUBROUTINE MIOL_readu_field_I4_4D_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)

   
   IMPLICIT NONE
   INTEGER,                           INTENT(IN) :: id_file_id
   CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
   INTEGER(KIND=4), DIMENSION(:,:,:,:), POINTER :: idpa_value
   INTEGER(KIND=4), DIMENSION(4), OPTIONAL,     INTENT(OUT) :: ida_dimsize
END SUBROUTINE MIOL_readu_field_I4_4D_NC
  SUBROUTINE MIOL_readu_field_I4_3D_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
    IMPLICIT NONE
    INTEGER,                           INTENT(IN) :: id_file_id
    CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
    INTEGER(KIND=4), DIMENSION(:,:,:), POINTER:: idpa_value
    INTEGER(KIND=4), DIMENSION(3), OPTIONAL,     INTENT(OUT) :: ida_dimsize
END SUBROUTINE MIOL_readu_field_I4_3D_NC
   SUBROUTINE MIOL_readu_field_I4_2D_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
 
     IMPLICIT NONE
     INTEGER,                           INTENT(IN) :: id_file_id
     CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
     INTEGER(KIND=4), DIMENSION(:,:), POINTER :: idpa_value
     INTEGER(KIND=4), DIMENSION(2), OPTIONAL,     INTENT(OUT) :: ida_dimsize
   END SUBROUTINE MIOL_readu_field_I4_2D_NC
   
SUBROUTINE MIOL_readu_field_I4_1D_NC (id_file_id, &
                                         cd_varname, &
                                         idpa_value, &
                                         ida_dimsize)
     IMPLICIT NONE
     INTEGER,                           INTENT(IN) :: id_file_id
     CHARACTER(LEN=*),                  INTENT(IN) :: cd_varname
     INTEGER(KIND=4), DIMENSION(:), POINTER:: idpa_value
     INTEGER(KIND=4), DIMENSION(1), OPTIONAL,     INTENT(OUT) :: ida_dimsize
   END SUBROUTINE MIOL_readu_field_I4_1D_NC

END INTERFACE !MIOL_read_field
!**
!> An interface for write Integer Values in NETCDF
!!
!<

INTERFACE MIOL_write_field_NC
 SUBROUTINE MIOL_writef_field_I4_4D_NC (cd_filename, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)
     IMPLICIT NONE
     CHARACTER(LEN=*),            INTENT(IN) :: cd_filename
     CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
     CHARACTER(LEN=4),            INTENT(IN) :: cd_key
     INTEGER(KIND=4), DIMENSION(:,:,:,:), INTENT(IN) :: ida_varvalue
     REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
     INTEGER, DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
END SUBROUTINE MIOL_writef_field_I4_4D_NC 
 SUBROUTINE MIOL_writef_field_I4_3D_NC (cd_filename, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)
   IMPLICIT NONE
   CHARACTER(LEN=*),            INTENT(IN) :: cd_filename
   CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
   CHARACTER(LEN=3),            INTENT(IN) :: cd_key
   INTEGER(KIND=4), DIMENSION(:,:,:), INTENT(IN) :: ida_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
   INTEGER, DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
END  SUBROUTINE MIOL_writef_field_I4_3D_NC
 SUBROUTINE MIOL_writef_field_I4_2D_NC (cd_filename, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)
 
   IMPLICIT NONE
   CHARACTER(LEN=*),            INTENT(IN) :: cd_filename
   CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
   CHARACTER(LEN=2),            INTENT(IN) :: cd_key
   INTEGER(KIND=4), DIMENSION(:,:), INTENT(IN) :: ida_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
   INTEGER, DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
   END SUBROUTINE MIOL_writef_field_I4_2D_NC

 SUBROUTINE MIOL_writef_field_I4_1D_NC (cd_filename, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue,&
                                          ida_specialvalue)

   IMPLICIT NONE
   CHARACTER(LEN=*),            INTENT(IN) :: cd_filename
   CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
   CHARACTER(LEN=1),            INTENT(IN) :: cd_key
   INTEGER(KIND=4), DIMENSION(:), INTENT(IN) :: ida_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
   INTEGER, DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
END  SUBROUTINE MIOL_writef_field_I4_1D_NC
 SUBROUTINE MIOL_writef_field_I4_scalar_NC (cd_filename, &
                                          cd_varname, &
                                          id_varvalue, &
                                          ida_specialvalue)
   IMPLICIT NONE
   CHARACTER(LEN=*),            INTENT(IN) :: cd_filename
   CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
   INTEGER, INTENT(IN) :: id_varvalue
   INTEGER, DIMENSION(5),       OPTIONAL, INTENT(IN) :: ida_specialvalue
END SUBROUTINE MIOL_writef_field_I4_scalar_NC

SUBROUTINE MIOL_writeu_field_I4_scalar_NC (id_file_id, &
                                          cd_varname, &
                                          id_varvalue, &
                                          ida_specialvalue)
  IMPLICIT NONE
  INTEGER,            INTENT(IN) :: id_file_id
  CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
  INTEGER(KIND=4), INTENT(IN) :: id_varvalue
  INTEGER, DIMENSION(5),       OPTIONAL, INTENT(IN) :: ida_specialvalue
END SUBROUTINE MIOL_writeu_field_I4_scalar_NC 

 SUBROUTINE MIOL_writeu_field_I4_4D_NC (id_file_id, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)
   IMPLICIT NONE
   INTEGER,                     INTENT(IN) :: id_file_id
   CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
   CHARACTER(LEN=4),            INTENT(IN) :: cd_key
   INTEGER(KIND=4), DIMENSION(:,:,:,:), INTENT(IN) :: ida_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
   INTEGER, DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
END SUBROUTINE MIOL_writeu_field_I4_4D_NC 

 SUBROUTINE MIOL_writeu_field_I4_3D_NC (id_file_id, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)
   IMPLICIT NONE
   INTEGER,                     INTENT(IN) :: id_file_id
   CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
   CHARACTER(LEN=3),            INTENT(IN) :: cd_key
   INTEGER(KIND=4), DIMENSION(:,:,:), INTENT(IN) :: ida_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
   INTEGER, DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
 END SUBROUTINE MIOL_writeu_field_I4_3D_NC 

SUBROUTINE MIOL_writeu_field_I4_2D_NC (id_file_id, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)

   IMPLICIT NONE
   INTEGER,            INTENT(IN) :: id_file_id
   CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
   CHARACTER(LEN=2),            INTENT(IN) :: cd_key
   INTEGER(KIND=4), DIMENSION(:,:), INTENT(IN) :: ida_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
   INTEGER, DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
END SUBROUTINE MIOL_writeu_field_I4_2D_NC
 
 SUBROUTINE MIOL_writeu_field_I4_1D_NC (id_file_id, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)
   IMPLICIT NONE
   INTEGER,                     INTENT(IN) :: id_file_id
   CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
   CHARACTER(LEN=1),            INTENT(IN) :: cd_key
   INTEGER(KIND=4), DIMENSION(:), INTENT(IN) :: ida_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
   INTEGER, DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
END SUBROUTINE MIOL_writeu_field_I4_1D_NC
END INTERFACE !MIOL_write_field_NC
!**
END MODULE INT_read_write_INTEGER
