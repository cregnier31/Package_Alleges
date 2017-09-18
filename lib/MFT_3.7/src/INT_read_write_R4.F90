!> \brief Module which contain Interfaces for READ WRITE R4 values in NETCDF format
!! \author C.REGNIER Miol V3.5
!! \date January 2008 
!!  \version 3.5
!<

MODULE INT_Read_Write_R4
   implicit none
  !
  ! -----------------------------------------------------------------
  ! --- General interface
  ! ---  MIOL_read_field_NC interface
  ! ---  MIOL_readf_field_R4_5D_NC(cd_filename,cd_varname,rdpa_value,ida_dimsize),
  ! ---  MIOL_readf_field_R4_4D_NC(cd_filename,cd_varname,rdpa_value,ida_dimsize),
  ! ---  MIOL_readf_field_R4_3D_NC(cd_filename,cd_varname,rdpa_value,ida_dimsize),
  ! ---  MIOL_readf_field_R4_2D_NC(cd_filename,cd_varname,rdpa_value,ida_dimsize),
  ! ---  MIOL_readf_field_R4_1D_NC(cd_filename,cd_varname,rdpa_value,ida_dimsize),
  ! ---  MIOL_readu_field_R4_4D_NC(id_file_id,cd_varname,rdpa_value,ida_dimsize)
  ! ---  MIOL_readu_field_R4_5D_NC(id_file_id,cd_varname,rdpa_value,ida_dimsize)
  ! ---
  !! Name of the file, Name of the variable,Pointer to stock the values,size of the matrix's result
   ! -----------------------------------------------------------------

!> An interface for read R4 NETCDF VALUES
!!
!<
INTERFACE MIOL_read_field_NC
  SUBROUTINE MIOL_readf_field_R4_5D_NC(cd_filename, &                          
                                        cd_varname, & 
                                        rdpa_value, & 
                                        ida_dimsize,&
                                        ida_indx,&
                                        ida_indy,&
                                        ida_indz,&
                                        ida_indt,&
                                        ida_inde)
     implicit none
     CHARACTER(LEN=*),               INTENT(IN) :: cd_filename
     CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
     REAL(KIND=4), DIMENSION(:,:,:,:,:), POINTER :: rdpa_value
     INTEGER, DIMENSION(:),  INTENT(IN),OPTIONAL  :: ida_indx,ida_indy,ida_indz,ida_indt,ida_inde
     INTEGER, DIMENSION(5),  INTENT(OUT), OPTIONAL :: ida_dimsize
   END SUBROUTINE MIOL_readf_field_R4_5D_NC

   SUBROUTINE MIOL_readf_field_R4_4D_NC(cd_filename, &
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
     REAL(KIND=4), DIMENSION(:,:,:,:), POINTER :: rdpa_value
     INTEGER, DIMENSION(:),  INTENT(IN),OPTIONAL  :: ida_indx,ida_indy,ida_indz,ida_indt
     INTEGER, DIMENSION(4),  INTENT(OUT), OPTIONAL :: ida_dimsize
   END SUBROUTINE MIOL_readf_field_R4_4D_NC

SUBROUTINE MIOL_readf_field_R4_3D_NC(cd_filename, &
                                     cd_varname, &
                                     rdpa_value, &
                                     ida_dimsize,&
                                     ida_indx,&
                                     ida_indy,&
                                     ida_indz)

                                                
    implicit none
    CHARACTER(LEN=*),               INTENT(IN) :: cd_filename
    CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
    INTEGER, DIMENSION(:),  INTENT(IN),OPTIONAL  :: ida_indx,ida_indy,ida_indz
    REAL(KIND=4), DIMENSION(:,:,:), POINTER :: rdpa_value
    INTEGER, DIMENSION(3), OPTIONAL,  INTENT(OUT) :: ida_dimsize
END SUBROUTINE MIOL_readf_field_R4_3D_NC

SUBROUTINE MIOL_readf_field_R4_2D_NC(cd_filename, &
                                     cd_varname, &
                                     rdpa_value, &
                                     ida_dimsize,&
                                     ida_indx,&
                                     ida_indy)

    implicit none
    CHARACTER(LEN=*),               INTENT(IN) :: cd_filename
    CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
    INTEGER, DIMENSION(:),  INTENT(IN),OPTIONAL  :: ida_indx,ida_indy
    REAL(KIND=4), DIMENSION(:,:), POINTER :: rdpa_value
    INTEGER, DIMENSION(2), OPTIONAL,  INTENT(OUT) :: ida_dimsize
END SUBROUTINE MIOL_readf_field_R4_2D_NC

SUBROUTINE MIOL_readf_field_R4_1D_NC(cd_filename, &
                                     cd_varname, &
                                     rdpa_value, &
                                     ida_dimsize,&
                                     ida_indx)
    implicit none
    CHARACTER(LEN=*),               INTENT(IN) :: cd_filename
    CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
    INTEGER, DIMENSION(:),  INTENT(IN),OPTIONAL  :: ida_indx
    REAL(KIND=4), DIMENSION(:), POINTER :: rdpa_value
    INTEGER, DIMENSION(1), OPTIONAL,  INTENT(OUT) :: ida_dimsize
END SUBROUTINE MIOL_readf_field_R4_1D_NC

SUBROUTINE MIOL_readf_field_R4_scalar_NC(cd_filename, &
                                          cd_varname, &
                                          rd_value)
   IMPLICIT NONE
   CHARACTER(LEN=*),               INTENT(IN) :: cd_filename,cd_varname
   REAL(KIND=4)                               :: rd_value
END SUBROUTINE MIOL_readf_field_R4_scalar_NC


 SUBROUTINE MIOL_readu_field_R4_5D_NC(id_file_id, &
                                          cd_varname, &
                                          rdpa_value, &
                                          ida_dimsize,&
                                          ida_indx,&
                                          ida_indy,&
                                          ida_indz,&
                                          ida_indt,&
                                          ida_inde)
    implicit none
    INTEGER,                        INTENT(IN) :: id_file_id
    CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
    REAL(KIND=4), DIMENSION(:,:,:,:,:), POINTER :: rdpa_value
    INTEGER, DIMENSION(5),  INTENT(OUT), OPTIONAL :: ida_dimsize
    INTEGER, DIMENSION(:),  INTENT(IN),  OPTIONAL :: ida_indx,ida_indy,ida_indz,ida_indt,ida_inde

  END SUBROUTINE MIOL_readu_field_R4_5D_NC

   SUBROUTINE MIOL_readu_field_R4_4D_NC(id_file_id, &
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
    REAL(KIND=4), DIMENSION(:,:,:,:), POINTER :: rdpa_value
    INTEGER, DIMENSION(4),  INTENT(OUT), OPTIONAL :: ida_dimsize
    INTEGER, DIMENSION(:),  INTENT(IN),  OPTIONAL :: ida_indx,ida_indy,ida_indz,ida_indt

  END SUBROUTINE MIOL_readu_field_R4_4D_NC
SUBROUTINE MIOL_readu_field_R4_3D_NC(id_file_id, &
                                          cd_varname, &
                                          rdpa_value, &
                                          ida_dimsize,&
                                          ida_indx,&
                                          ida_indy,&
                                          ida_indz)

  IMPLICIT NONE
  INTEGER,                        INTENT(IN) :: id_file_id
  CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
  INTEGER, DIMENSION(:),  INTENT(IN),OPTIONAL :: ida_indx,ida_indy,ida_indz
  REAL(KIND=4), DIMENSION(:,:,:), POINTER :: rdpa_value
  INTEGER, DIMENSION(3), OPTIONAL,  INTENT(OUT) :: ida_dimsize
END SUBROUTINE MIOL_readu_field_R4_3D_NC
SUBROUTINE MIOL_readu_field_R4_2D_NC(id_file_id, &
                                          cd_varname, &
                                          rdpa_value, &
                                          ida_dimsize,&
                                          ida_indx,&
                                          ida_indy)

    IMPLICIT NONE
    INTEGER,                        INTENT(IN)    :: id_file_id
    CHARACTER(LEN=*),               INTENT(IN)    :: cd_varname
    REAL(KIND=4), DIMENSION(:,:), POINTER  :: rdpa_value
    INTEGER, DIMENSION(:),  INTENT(IN),OPTIONAL   :: ida_indx,ida_indy
    INTEGER, DIMENSION(2), OPTIONAL,  INTENT(OUT) :: ida_dimsize
END SUBROUTINE MIOL_readu_field_R4_2D_NC
 SUBROUTINE MIOL_readu_field_R4_1D_NC(id_file_id, &
                                          cd_varname, &
                                          rdpa_value, &
                                          ida_dimsize,&
                                          ida_indx)
   IMPLICIT NONE
   INTEGER,                        INTENT(IN) :: id_file_id
   CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
   INTEGER, DIMENSION(:),  INTENT(IN),OPTIONAL   :: ida_indx
   REAL(KIND=4), DIMENSION(:), POINTER :: rdpa_value
   INTEGER, DIMENSION(1), OPTIONAL,  INTENT(OUT) :: ida_dimsize
 END SUBROUTINE MIOL_readu_field_R4_1D_NC

 SUBROUTINE MIOL_readu_field_R4_scalar_NC(id_file_id, &
                                          cd_varname, &
                                          rd_value)
   IMPLICIT NONE
   INTEGER,                        INTENT(IN) :: id_file_id
   CHARACTER(LEN=*),               INTENT(IN) :: cd_varname
   REAL(KIND=4)                               :: rd_value

 END SUBROUTINE MIOL_readu_field_R4_scalar_NC


END INTERFACE !MIOL_read_field_NC
!> An interface for write R4 NETCDF VALUES
!!
!<
INTERFACE MIOL_write_field_NC

SUBROUTINE MIOL_writef_field_R4_5D_NC (cd_filename, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
    IMPLICIT NONE
    CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
    CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
    CHARACTER(LEN=5),                 INTENT(IN) :: cd_key
    REAL(KIND=4), DIMENSION(:,:,:,:,:), INTENT(IN) :: rda_varvalue
    REAL(KIND=8), DIMENSION(2),INTENT(IN),OPTIONAL :: rda_offsetvalue
    REAL(KIND=4), DIMENSION(4),INTENT(IN),OPTIONAL :: rda_specialvalue
END SUBROUTINE MIOL_writef_field_R4_5D_NC

SUBROUTINE MIOL_writef_field_R4_4D_NC (cd_filename, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
    IMPLICIT NONE
    CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
    CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
    CHARACTER(LEN=4),                 INTENT(IN) :: cd_key
    REAL(KIND=4), DIMENSION(:,:,:,:), INTENT(IN) :: rda_varvalue
    REAL(KIND=8), DIMENSION(2),INTENT(IN),OPTIONAL :: rda_offsetvalue
    REAL(KIND=4), DIMENSION(4),INTENT(IN),OPTIONAL :: rda_specialvalue
END SUBROUTINE MIOL_writef_field_R4_4D_NC
 SUBROUTINE MIOL_writef_field_R4_3D_NC (cd_filename, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
   IMPLICIT NONE
   CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
   CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
   CHARACTER(LEN=3),                 INTENT(IN) :: cd_key
   REAL(KIND=4), DIMENSION(:,:,:),   INTENT(IN) :: rda_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
   REAL(KIND=4), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 END SUBROUTINE MIOL_writef_field_R4_3D_NC

 SUBROUTINE MIOL_writef_field_R4_2D_NC (cd_filename, &
                                        cd_varname, &
                                        cd_key, &
                                        rda_varvalue, &
                                        rda_offsetvalue, &
                                        rda_specialvalue)
   IMPLICIT NONE
   CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
   CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
   CHARACTER(LEN=2),                 INTENT(IN) :: cd_key
   REAL(KIND=4), DIMENSION(:,:),     INTENT(IN) :: rda_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
   REAL(KIND=4), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
END SUBROUTINE MIOL_writef_field_R4_2D_NC

 SUBROUTINE MIOL_writef_field_R4_1D_NC (cd_filename, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
   IMPLICIT NONE
   CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
   CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
   CHARACTER(LEN=1),                 INTENT(IN) :: cd_key
   REAL(KIND=4), DIMENSION(:),       INTENT(IN) :: rda_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(OUT) :: rda_offsetvalue
   REAL(KIND=4), DIMENSION(5),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 END SUBROUTINE MIOL_writef_field_R4_1D_NC

 SUBROUTINE MIOL_writef_field_R4_scalar_NC (cd_filename, &
                                           cd_varname, &
                                           cd_key, &
                                           rl_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
   IMPLICIT NONE
   CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
   CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
   CHARACTER(LEN=1),                 INTENT(IN) :: cd_key
   REAL(KIND=4), INTENT(IN)                      :: rl_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(OUT) :: rda_offsetvalue
   REAL(KIND=4), DIMENSION(5),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 END SUBROUTINE MIOL_writef_field_R4_scalar_NC

 SUBROUTINE MIOL_writeu_field_R4_5D_NC (id_file_id, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
     IMPLICIT NONE
     INTEGER,                          INTENT(IN) :: id_file_id
     CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
     CHARACTER(LEN=5),                 INTENT(IN) :: cd_key
     REAL(KIND=4), DIMENSION(:,:,:,:,:), INTENT(IN) :: rda_varvalue
     REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
     REAL(KIND=4), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
   END SUBROUTINE MIOL_writeu_field_R4_5D_NC

 SUBROUTINE MIOL_writeu_field_R4_4D_NC (id_file_id, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
     IMPLICIT NONE
     INTEGER,                          INTENT(IN) :: id_file_id
     CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
     CHARACTER(LEN=4),                 INTENT(IN) :: cd_key
     REAL(KIND=4), DIMENSION(:,:,:,:), INTENT(IN) :: rda_varvalue
     REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
     REAL(KIND=4), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
   END SUBROUTINE MIOL_writeu_field_R4_4D_NC
 SUBROUTINE MIOL_writeu_field_R4_3D_NC (id_file_id, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)

   IMPLICIT NONE
   INTEGER,                          INTENT(IN) :: id_file_id
   CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
   CHARACTER(LEN=3),                 INTENT(IN) :: cd_key
   REAL(KIND=4), DIMENSION(:,:,:),   INTENT(IN) :: rda_varvalue
   REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
   REAL(KIND=4), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue       
 END SUBROUTINE MIOL_writeu_field_R4_3D_NC

   SUBROUTINE MIOL_writeu_field_R4_2D_NC (id_file_id, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)

     IMPLICIT NONE
     INTEGER,                          INTENT(IN) :: id_file_id
     CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
     CHARACTER(LEN=2),                 INTENT(IN) :: cd_key
     REAL(KIND=4), DIMENSION(:,:),     INTENT(IN) :: rda_varvalue
     REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
     REAL(KIND=4), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
   END SUBROUTINE MIOL_writeu_field_R4_2D_NC

SUBROUTINE MIOL_writeu_field_R4_1D_NC (id_file_id, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
  IMPLICIT NONE
  INTEGER,                          INTENT(IN) :: id_file_id
  CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
  CHARACTER(LEN=1),                 INTENT(IN) :: cd_key
  REAL(KIND=4), DIMENSION(:),       INTENT(IN) :: rda_varvalue
  REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
  REAL(KIND=4), DIMENSION(5),       OPTIONAL, INTENT(IN) :: rda_specialvalue
END SUBROUTINE MIOL_writeu_field_R4_1D_NC

SUBROUTINE MIOL_writeu_field_R4_scalar_NC (id_file_id, &
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
  REAL(KIND=4), INTENT(IN)                     :: rl_varvalue
  INTEGER(KIND=4), OPTIONAL ,           INTENT(IN) :: id_indice
  REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
  REAL(KIND=4), DIMENSION(5),       OPTIONAL, INTENT(IN) :: rda_specialvalue
END SUBROUTINE MIOL_writeu_field_R4_scalar_NC

END INTERFACE !MIOL_write_field_NC

INTERFACE MIOL_write_multifield_NC

   SUBROUTINE MIOL_writeu_multifield_R4_1D_NC (id_file_id, &
                                               cda_varname, &
                                               cda_key, &
                                               rda_varvalue, &
                                               ida_dimsout_id,&
                                               rda_offsetvalue, &
                                               rda_specialvalue)
 
     IMPLICIT NONE
     INTEGER,                          INTENT(IN) :: id_file_id
     CHARACTER(LEN=255),DIMENSION(:),     INTENT(IN) :: cda_varname
     CHARACTER(LEN=1),DIMENSION(:),     INTENT(IN) :: cda_key
     REAL(KIND=4), DIMENSION(:,:),       INTENT(IN) :: rda_varvalue
     INTEGER(KIND=4),DIMENSION(:),       INTENT(IN) :: ida_dimsout_id

     REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
     REAL(KIND=4), DIMENSION(5),       OPTIONAL, INTENT(IN) :: rda_specialvalue

END SUBROUTINE MIOL_writeu_multifield_R4_1D_NC
END INTERFACE !MIOL_write_multifield_NC

END MODULE INT_Read_Write_R4


