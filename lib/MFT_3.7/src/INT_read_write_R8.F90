!> \brief Module which contain Interfaces for READ WRITE R8 values in NETCDF format
!! \author C.REGNIER Miol V3.5
!! \date September 2008 
!!  \version 3.5
!<

MODULE INT_read_write_R8
  implicit none
  !
  ! -----------------------------------------------------------------
  ! --- General interface
  ! ---  MIOL_read_field_NC interface
  ! --- 
  ! ---  MIOL_readf_field_R8_4D_NC(cd_filename,cd_varname,rdpa_value,ida_dimsize)
  ! ---  MIOL_readf_field_R8_3D_NC(cd_filename,cd_varname,rdpa_value,ida_dimsize)
  ! ---  MIOL_readf_field_R8_2D_NC(cd_filename,cd_varname,rdpa_value,ida_dimsize)
  ! ---  MIOL_readf_field_R8_1D_NC(cd_filename,cd_varname,rdpa_value,ida_dimsize)
  ! ---  MIOL_readu_field_R8_4D_NC(id_file_id,cd_varname,rdpa_value,ida_dimsize)
  ! ---  MIOL_readu_field_R8_3D_NC(id_file_id,cd_varname,rdpa_value,ida_dimsize)
  ! ---  MIOL_readu_field_R8_2D_NC(id_file_id,cd_varname,rdpa_value,ida_dimsize)
  ! ---  MIOL_readu_field_R8_1D_NC(id_file_id,cd_varname,rdpa_value,ida_dimsize)
  ! -----------------------------------------------------------------
  ! ---  MIOL_write_field_NC interface
  ! --- 
  ! --- SUBROUTINE MIOL_writef_field_R8_4D_NC (cd_filename,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
  ! --- SUBROUTINE MIOL_writef_field_R8_3D_NC (cd_filename,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
  ! --- SUBROUTINE MIOL_writef_field_R8_2D_NC (cd_filename,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
  ! --- SUBROUTINE MIOL_writef_field_R8_1D_NC (cd_filename,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
  ! --- SUBROUTINE MIOL_writef_field_R8_scalar_NC(cd_filename,cd_varname,cd_key,rd_varvalue,rda_offsetvalue,rda_specialvalue)
  ! --- SUBROUTINE MIOL_writeu_field_R8_4D_NC (id_file_id,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
  ! --- SUBROUTINE MIOL_writeu_field_R8_3D_NC (id_file_id,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
  ! --- SUBROUTINE MIOL_writeu_field_R8_2D_NC (id_file_id,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
  ! --- SUBROUTINE MIOL_writeu_field_R8_1D_NC (id_file_id,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
  ! --- SUBROUTINE MIOL_writeu_field_R8_scalar_NC(cd_filename,cd_varname,cd_key,rd_varvalue,rda_offsetvalue,rda_specialvalue)
!> An interface for read R8 NETCDF VALUES
!!
!<



 INTERFACE MIOL_read_field_NC
    SUBROUTINE MIOL_readf_field_R8_4D_NC(cd_filename, & 
                                          cd_varname, & 
                                          rdpa_value, & 
                                          ida_dimsize,&
                                          ida_indx,&
                                          ida_indy,&
                                          ida_indz,&
                                          ida_indt)
    implicit none
    CHARACTER(LEN=*),               INTENT(IN) :: cd_filename
    CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
    REAL(KIND=8), DIMENSION(:,:,:,:), POINTER :: rdpa_value
    INTEGER, DIMENSION(:),  INTENT(IN),OPTIONAL  :: ida_indx,ida_indy,ida_indz,ida_indt
    INTEGER, DIMENSION(4), OPTIONAL,  INTENT(OUT) :: ida_dimsize
  END SUBROUTINE MIOL_readf_field_R8_4D_NC

  SUBROUTINE MIOL_readf_field_R8_3D_NC(cd_filename, &
                                       cd_varname, &
                                       rdpa_value, &
                                       ida_dimsize,&
                                       ida_indx,&
                                       ida_indy,&
                                       ida_indz)
    implicit none
    CHARACTER(LEN=*),               INTENT(IN) :: cd_filename
    CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
    REAL(KIND=8), DIMENSION(:,:,:), POINTER :: rdpa_value
    INTEGER, DIMENSION(:),  INTENT(IN),OPTIONAL  :: ida_indx,ida_indy,ida_indz
    INTEGER, DIMENSION(3), OPTIONAL,  INTENT(OUT) :: ida_dimsize
  END SUBROUTINE MIOL_readf_field_R8_3D_NC

  SUBROUTINE MIOL_readf_field_R8_2D_NC(cd_filename, &
                                       cd_varname, &
                                       rdpa_value, &
                                       ida_dimsize,&
                                       ida_indx,&
                                       ida_indy)
    implicit none
    CHARACTER(LEN=*),               INTENT(IN) :: cd_filename
    CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
    REAL(KIND=8), DIMENSION(:,:), POINTER :: rdpa_value
    INTEGER, DIMENSION(:),  INTENT(IN),OPTIONAL  :: ida_indx,ida_indy
    INTEGER, DIMENSION(2), OPTIONAL,  INTENT(OUT) :: ida_dimsize
  END SUBROUTINE MIOL_readf_field_R8_2D_NC

  SUBROUTINE MIOL_readf_field_R8_1D_NC(cd_filename, &
                                       cd_varname, &
                                       rdpa_value, &
                                       ida_dimsize,&
                                       ida_indx)
    implicit none
    CHARACTER(LEN=*),               INTENT(IN) :: cd_filename
    CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
    REAL(KIND=8), DIMENSION(:), POINTER :: rdpa_value
    INTEGER, DIMENSION(:),  INTENT(IN),OPTIONAL  :: ida_indx
    INTEGER, DIMENSION(1), OPTIONAL,  INTENT(OUT) :: ida_dimsize
  END SUBROUTINE MIOL_readf_field_R8_1D_NC

  SUBROUTINE MIOL_readu_field_R8_4D_NC (id_file_id, &
                                        cd_varname, &
                                        rdpa_value, &
                                        ida_dimsize,&
                                        ida_indx,&
                                        ida_indy,&
                                        ida_indz,&
                                        ida_indt)

    implicit none
    INTEGER,                        INTENT(IN) :: id_file_id
    CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
    REAL(KIND=8), DIMENSION(:,:,:,:), POINTER :: rdpa_value
    INTEGER, DIMENSION(:),  INTENT(IN),OPTIONAL  :: ida_indx,ida_indy,ida_indz,ida_indt
    INTEGER, DIMENSION(4), OPTIONAL,  INTENT(OUT) :: ida_dimsize
  END  SUBROUTINE MIOL_readu_field_R8_4D_NC

  SUBROUTINE MIOL_readu_field_R8_3D_NC (id_file_id, &
                                        cd_varname, &
                                        rdpa_value, &
                                        ida_dimsize,&
                                        ida_indx,&
                                        ida_indy,&
                                        ida_indz)
  implicit none
  INTEGER,                        INTENT(IN) :: id_file_id
  CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
  REAL(KIND=8), DIMENSION(:,:,:), POINTER :: rdpa_value
  INTEGER, DIMENSION(:),  INTENT(IN),OPTIONAL  :: ida_indx,ida_indy,ida_indz
  INTEGER, DIMENSION(3), OPTIONAL,  INTENT(OUT) :: ida_dimsize

  END SUBROUTINE MIOL_readu_field_R8_3D_NC

  SUBROUTINE MIOL_readu_field_R8_2D_NC (id_file_id, &
                                        cd_varname, &
                                        rdpa_value, &
                                        ida_dimsize,&
                                        ida_indx,&
                                        ida_indy)
  implicit none
  INTEGER,                        INTENT(IN) :: id_file_id
  CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
  REAL(KIND=8), DIMENSION(:,:), POINTER:: rdpa_value
  INTEGER, DIMENSION(:),  INTENT(IN),OPTIONAL  :: ida_indx,ida_indy
  INTEGER, DIMENSION(2), OPTIONAL,  INTENT(OUT) :: ida_dimsize
 END SUBROUTINE MIOL_readu_field_R8_2D_NC
 SUBROUTINE MIOL_readu_field_R8_1D_NC (id_file_id, &
                                          cd_varname, &
                                          rdpa_value, &
                                          ida_dimsize,&
                                          ida_indx)
   implicit none
   INTEGER,                        INTENT(IN) :: id_file_id
   CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
   REAL(KIND=8), DIMENSION(:), POINTER:: rdpa_value
   INTEGER, DIMENSION(:),  INTENT(IN),OPTIONAL  :: ida_indx
   INTEGER, DIMENSION(1), OPTIONAL,  INTENT(OUT) :: ida_dimsize
 END SUBROUTINE MIOL_readu_field_R8_1D_NC
END INTERFACE !MIOL_read_field_NC
!> An interface for write R8 NETCDF VALUES
!!
!<
INTERFACE MIOL_write_field_NC
 SUBROUTINE MIOL_writef_field_R8_4D_NC (cd_filename, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
 

   IMPLICIT NONE
   CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
   CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
   CHARACTER(LEN=4),                 INTENT(IN) :: cd_key
   REAL(KIND=8), DIMENSION(:,:,:,:), INTENT(IN) :: rda_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
   REAL(KIND=8), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 END SUBROUTINE MIOL_writef_field_R8_4D_NC
   SUBROUTINE MIOL_writef_field_R8_3D_NC (cd_filename, &
                                          cd_varname, &
                                          cd_key, &
                                          rda_varvalue, &
                                          rda_offsetvalue, &
                                           rda_specialvalue)
     IMPLICIT NONE
     CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
     CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
     CHARACTER(LEN=3),                 INTENT(IN) :: cd_key
     REAL(KIND=8), DIMENSION(:,:,:), INTENT(IN) :: rda_varvalue
     REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
     REAL(KIND=8), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
   END SUBROUTINE MIOL_writef_field_R8_3D_NC
  
SUBROUTINE MIOL_writef_field_R8_2D_NC (cd_filename, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
  IMPLICIT NONE
  CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
  CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
  CHARACTER(LEN=2),                 INTENT(IN) :: cd_key
  REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: rda_varvalue
  REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
  REAL(KIND=8), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 END SUBROUTINE MIOL_writef_field_R8_2D_NC

   SUBROUTINE MIOL_writef_field_R8_1D_NC (cd_filename, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)

     IMPLICIT NONE
     CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
     CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
     CHARACTER(LEN=1),                 INTENT(IN) :: cd_key
     REAL(KIND=8), DIMENSION(:), INTENT(IN) :: rda_varvalue
     REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
     REAL(KIND=8), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
   END SUBROUTINE MIOL_writef_field_R8_1D_NC

   SUBROUTINE MIOL_writef_field_R8_scalar_NC(cd_filename, &
                                           cd_varname, &
                                           cd_key, &
                                           rd_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)

     IMPLICIT NONE
     CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
     CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
     CHARACTER(LEN=1),                 INTENT(IN) :: cd_key
     REAL(KIND=8),INTENT(IN) :: rd_varvalue
     REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
     REAL(KIND=8), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
   END SUBROUTINE MIOL_writef_field_R8_scalar_NC

 SUBROUTINE MIOL_writeu_field_R8_4D_NC (id_file_id, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
 
   IMPLICIT NONE
   INTEGER,                          INTENT(IN) :: id_file_id
   CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
   CHARACTER(LEN=4),                 INTENT(IN) :: cd_key
   REAL(KIND=8), DIMENSION(:,:,:,:), INTENT(IN) :: rda_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
   REAL(KIND=8), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 END SUBROUTINE MIOL_writeu_field_R8_4D_NC

   SUBROUTINE MIOL_writeu_field_R8_3D_NC (id_file_id, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
 
     IMPLICIT NONE
     INTEGER,                          INTENT(IN) :: id_file_id
     CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
     CHARACTER(LEN=3),                 INTENT(IN) :: cd_key
     REAL(KIND=8), DIMENSION(:,:,:), INTENT(IN) :: rda_varvalue
     REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
     REAL(KIND=8), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 END SUBROUTINE MIOL_writeu_field_R8_3D_NC
 
SUBROUTINE MIOL_writeu_field_R8_2D_NC (id_file_id, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
  IMPLICIT NONE
  INTEGER,                          INTENT(IN) :: id_file_id
  CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
  CHARACTER(LEN=2),                 INTENT(IN) :: cd_key
  REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: rda_varvalue
  REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
  REAL(KIND=8), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 END SUBROUTINE MIOL_writeu_field_R8_2D_NC

 SUBROUTINE MIOL_writeu_field_R8_1D_NC (id_file_id, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
   IMPLICIT NONE
   INTEGER,                          INTENT(IN) :: id_file_id
   CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
   CHARACTER(LEN=1),                 INTENT(IN) :: cd_key
   REAL(KIND=8), DIMENSION(:), INTENT(IN) :: rda_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
   REAL(KIND=8), DIMENSION(5),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 END SUBROUTINE MIOL_writeu_field_R8_1D_NC

SUBROUTINE MIOL_writeu_field_R8_scalar_NC (id_file_id, &                                                                              
                                           cd_varname, &
                                           cd_key, &
                                           rl_varvalue, &
                                           id_indice, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
  IMPLICIT NONE
  INTEGER,                          INTENT(IN) :: id_file_id
  CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
  CHARACTER(LEN=1),                 INTENT(IN) :: cd_key
  REAL(KIND=8), INTENT(IN)                     :: rl_varvalue
  INTEGER(KIND=4), OPTIONAL ,           INTENT(IN) :: id_indice
  REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
  REAL(KIND=4), DIMENSION(5),       OPTIONAL, INTENT(IN) :: rda_specialvalue

 END SUBROUTINE MIOL_writeu_field_R8_scalar_NC

END INTERFACE ! MIOL_write_field_NC
END MODULE INT_read_write_R8

