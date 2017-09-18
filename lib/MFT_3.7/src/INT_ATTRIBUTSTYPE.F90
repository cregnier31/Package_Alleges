!> \brief Module which contain Interfaces for READ and Write attributes in NETCDF format
!! \author C.REGNIER Miol V3.5
!! \date Avril 2010 : add Byte interface
!!  \version 3.3
!!  
!<
MODULE INT_ATTRIBUTSTYPE
  implicit none

  ! -----------------------------------------------------------------
  ! --- General interface
  ! ---  MIOL_read_attribute_NC interface
  ! --- SUBROUTINE MIOL_readf_attribute_I_NC (cd_filename,cd_varname,cd_attname,id_attvalue)
  ! --- SUBROUTINE MIOL_readu_attribute_I_NC (id_file_id,cd_varname,cd_attname,id_attvalue)
  ! -----------------------------------------------------------------
  ! --- MIOL_write_attribute_NC interface
  ! --- MIOL_writef_attribute_I_NC (cd_ncfilename,cd_varname,cd_attname,id_attvalue)
  ! --- MIOL_writeu_attribute_I_NC (id_file_id,cd_varname,cd_attname,id_attvalue)
  ! -----------------------------------------------------------------
  ! ---  MIOL_readf_attribute_C_NC (cd_filename,cd_varname,cd_attname,cd_attvalue)
  ! ---  MIOL_readu_attribute_C_NC (id_file_id,cd_varname,cd_attname, cd_attvalue)
  ! ---  MIOL_writef_attribute_C_NC (cd_filename,cd_varname,cd_attname,cd_attvalue)
  ! ---  MIOL_writeu_attribute_C_NC (id_file_id,cd_varname,cd_attname,cd_filename,cd_attvalue)! --- MIOL_read_attribute_NC interface
  ! --- MIOL_readu_attribute_R4_NC (id_file_id,cd_varname,cd_attname,rd_attvalue)
  ! --- MIOL_readf_attribute_R4_NC (cd_filename,cd_varname,cd_attname,rd_attvalue)
 ! ---
  ! --- MIOL_write_attribute_NC interface
  ! --- MIOL_writef_attribute_R4_NC (cd_ncfilename,cd_varname,cd_attname,rd_attvalue)
  ! --- MIOL_writeu_attribute_R4_NC (id_file_id,cd_varname,cd_attname,rd_attvalue)
  ! -----------------------------------------------------------------
  ! --- MIOL_read_attribute_NC interface
  ! --- MIOL_readu_attribute_R8_NC (id_file_id,cd_varname,cd_attname,rd_attvalue)
  ! --- MIOL_readf_attribute_R8_NC (cd_filename,cd_varname,cd_attname,rd_attvalue)
  ! -----------------------------------------------------------------
  ! --- MIOL_write_attribute_NC interface
  ! --- MIOL_writef_attribute_R8_NC (cd_ncfilename,cd_varname,cd_attname,rd_attvalue)
  ! --- MIOL_writeu_attribute_R8_NC (id_file_id,cd_varname,cd_attname,rd_attvalue)
  ! -----------------------------------------------------------------
  ! ---
  ! --- INTERFACE MIOL_read_attribute_NC
  ! --- SUBROUTINE MIOL_readu_attribute_S_NC (id_file_id,cd_varname,cd_attname,id_attvalue)
  ! --- SUBROUTINE MIOL_readf_attribute_S_NC (cd_filename,cd_varname,cd_attname,id_attvalue)
  ! -----------------------------------------------------------------
  ! --- MIOL_write_attribute_NC interface
  ! --- MIOL_writef_attribute_S_NC (cd_ncfilename,cd_varname,cd_attname,id_attvalue)
  ! --- MIOL_writeu_attribute_S_NC (id_file_id,cd_varname,cd_attname, id_attvalue) 
  ! -----------------------------------------------------------------
  ! --- SUBROUTINE MIOL_readu_attribute_B_NC (id_file_id,cd_varname,cd_attname,id_attvalue)
  ! --- SUBROUTINE MIOL_readf_attribute_B_NC (cd_filename,cd_varname,cd_attname,id_attvalue)
  ! --- SUBROUTINE MIOL_writef_attribute_B_NC (cd_ncfilename,cd_varname,cd_attname,id_attvalue)
  ! --- SUBROUTINE MIOL_writeu_attribute_B_NC (id_file_id,cd_varname,cd_attname, id_attvalue) 

!** 
!> An interface for read  Integer attributes Values in NETCDF
!!
!<
 INTERFACE MIOL_read_attribute_NC
   SUBROUTINE MIOL_readf_attribute_I_NC (cd_filename, &
                                              cd_varname, &
                                              cd_attname, &
                                              id_attvalue)
         IMPLICIT NONE
         CHARACTER(LEN=*),           INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
         CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
         INTEGER,                    INTENT(OUT) :: id_attvalue
END SUBROUTINE MIOL_readf_attribute_I_NC
   SUBROUTINE MIOL_readu_attribute_I_NC (id_file_id, &
                                              cd_varname, &
                                              cd_attname, &
                                              id_attvalue)

     IMPLICIT NONE
     INTEGER,                    INTENT(IN) :: id_file_id
     CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
     CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
     INTEGER,                    INTENT(OUT) :: id_attvalue
 END SUBROUTINE MIOL_readu_attribute_I_NC

END INTERFACE !MIOL_read_attribute_NC
!**
!> An interface for write Integer attributes Values in NETCDF
!!
!<
 INTERFACE MIOL_write_attribute_NC
    SUBROUTINE MIOL_writeu_attribute_I_NC (id_file_id, &
                                               cd_varname, &
                                               cd_attname, &
                                               id_attvalue)
      IMPLICIT NONE
      INTEGER,            INTENT(IN) :: id_file_id
      CHARACTER(LEN=255), INTENT(IN) :: cd_varname
      CHARACTER(LEN=255), INTENT(IN) :: cd_attname
      INTEGER,            INTENT(IN) :: id_attvalue
    END SUBROUTINE MIOL_writeu_attribute_I_NC
 SUBROUTINE MIOL_writef_attribute_I_NC (cd_ncfilename, &
                                               cd_varname, &
                                               cd_attname, &
                                               id_attvalue)
   IMPLICIT NONE
   CHARACTER(LEN=255), INTENT(IN) :: cd_ncfilename
   CHARACTER(LEN=255), INTENT(IN) :: cd_varname
   CHARACTER(LEN=255), INTENT(IN) :: cd_attname
   INTEGER,            INTENT(IN) :: id_attvalue
 END SUBROUTINE MIOL_writef_attribute_I_NC
END INTERFACE !MIOL_write_attribute_NC
!**
!** 
!> An interface for read Character attributes Values in NETCDF
!!
!<
INTERFACE MIOL_read_attribute_NC
  SUBROUTINE MIOL_readf_attribute_C_NC (cd_filename, &
                                              cd_varname, &
                                              cd_attname, &
                                              cd_attvalue)
    IMPLICIT NONE
    CHARACTER(LEN=*),           INTENT( IN) :: cd_filename
    CHARACTER(LEN=*),           INTENT( IN) :: cd_varname
    CHARACTER(LEN=*),           INTENT( IN) :: cd_attname
    CHARACTER(LEN=255),         INTENT(OUT) :: cd_attvalue
END SUBROUTINE MIOL_readf_attribute_C_NC
 SUBROUTINE MIOL_readu_attribute_C_NC (id_file_id, &
                                              cd_varname, &
                                              cd_attname, &
                                              cd_attvalue)
   IMPLICIT NONE
   INTEGER,                    INTENT( IN) :: id_file_id
   CHARACTER(LEN=*),           INTENT( IN) :: cd_varname
   CHARACTER(LEN=*),           INTENT( IN) :: cd_attname
   CHARACTER(LEN=255),         INTENT(OUT) :: cd_attvalue
 END SUBROUTINE MIOL_readu_attribute_C_NC
END INTERFACE !MIOL_read_attribute_NC
!**
!> An interface for write Character attributes Values in NETCDF
!!
!<
INTERFACE MIOL_write_attribute_NC
 SUBROUTINE MIOL_writef_attribute_C_NC (cd_filename, &
                                               cd_varname, &
                                               cd_attname, &
                                               cd_attvalue)
   IMPLICIT NONE
   CHARACTER(LEN=255),           INTENT(IN) :: cd_filename
   CHARACTER(LEN=255),           INTENT(IN) :: cd_varname
   CHARACTER(LEN=255),           INTENT(IN) :: cd_attname
   CHARACTER(LEN=255), OPTIONAL, INTENT(IN) :: cd_attvalue
 END SUBROUTINE MIOL_writef_attribute_C_NC
SUBROUTINE MIOL_writeu_attribute_C_NC (id_file_id, &
                                               cd_varname, &
                                               cd_attname, &
                                         !      cd_filename, &
                                               cd_attvalue)
  IMPLICIT NONE
  INTEGER,                      INTENT(IN) :: id_file_id
  CHARACTER(LEN=255),           INTENT(IN) :: cd_varname
  CHARACTER(LEN=255),           INTENT(IN) :: cd_attname
!  CHARACTER(LEN=255),           INTENT(IN) :: cd_filename
  CHARACTER(LEN=255), OPTIONAL, INTENT(IN) :: cd_attvalue
END SUBROUTINE MIOL_writeu_attribute_C_NC
END INTERFACE
!**
!> An interface for read attributs R4 NETCDF VALUES
!!
!<

INTERFACE MIOL_read_attribute_NC

SUBROUTINE MIOL_readu_attribute_R4_NC (id_file_id, &
                                              cd_varname, &
                                              cd_attname, &
                                              rd_attvalue)
  IMPLICIT NONE
  INTEGER,                    INTENT(IN) :: id_file_id
  CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
  CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
  REAL(KIND=4),               INTENT(OUT) :: rd_attvalue
END  SUBROUTINE MIOL_readu_attribute_R4_NC
  SUBROUTINE MIOL_readf_attribute_R4_NC (cd_filename, &
                                              cd_varname, &
                                              cd_attname, &
                                              rd_attvalue)
    IMPLICIT NONE
    CHARACTER(LEN=*),           INTENT(IN) :: cd_filename
    CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
    CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
    REAL(KIND=4),               INTENT(OUT) :: rd_attvalue
  END SUBROUTINE MIOL_readf_attribute_R4_NC
END INTERFACE !MIOL_read_attribute_NC
!> An interface for write attributs R4 NETCDF VALUES
!!
!<
INTERFACE MIOL_write_attribute_NC
SUBROUTINE MIOL_writef_attribute_R4_NC (cd_ncfilename, &
                                               cd_varname, &
                                               cd_attname, &
                                               rd_attvalue)
     IMPLICIT NONE
     CHARACTER(LEN=*), INTENT(IN) :: cd_ncfilename
     CHARACTER(LEN=*), INTENT(IN) :: cd_varname
     CHARACTER(LEN=*), INTENT(IN) :: cd_attname
     REAL(KIND=4),       INTENT(IN) :: rd_attvalue
   END SUBROUTINE MIOL_writef_attribute_R4_NC
  SUBROUTINE MIOL_writeu_attribute_R4_NC (id_file_id, &
                                                cd_varname, &
                                                cd_attname, &
                                                rd_attvalue)
      IMPLICIT NONE
      INTEGER,            INTENT(IN) :: id_file_id
      CHARACTER(LEN=*), INTENT(IN) :: cd_varname
      CHARACTER(LEN=*), INTENT(IN) :: cd_attname
      REAL(KIND=4),       INTENT(IN) :: rd_attvalue
    END SUBROUTINE MIOL_writeu_attribute_R4_NC
END INTERFACE  !MIOL_write_attribute_NC
!**
!> An interface for read attributs R8 NETCDF VALUES
!!
!<

INTERFACE MIOL_read_attribute_NC

    SUBROUTINE MIOL_readf_attribute_R8_NC (cd_filename, &
                                              cd_varname, &
                                              cd_attname, &
                                              rd_attvalue)

      IMPLICIT NONE
      CHARACTER(LEN=*),           INTENT(IN) :: cd_filename
      CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
      CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
      REAL(KIND=8),               INTENT(OUT) :: rd_attvalue
  END SUBROUTINE MIOL_readf_attribute_R8_NC
 SUBROUTINE MIOL_readu_attribute_R8_NC (id_file_id, &
                                              cd_varname, &
                                              cd_attname, &
                                              rd_attvalue)
   IMPLICIT NONE
   INTEGER,                    INTENT(IN) :: id_file_id
   CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
   CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
   REAL(KIND=8),               INTENT(OUT) :: rd_attvalue
 END SUBROUTINE MIOL_readu_attribute_R8_NC
END INTERFACE !MIOL_read_attribute_NC
!> An interface for write R8 NETCDF VALUES
!!
!<
INTERFACE MIOL_write_attribute_NC

   SUBROUTINE MIOL_writef_attribute_R8_NC (cd_ncfilename, &
                                               cd_varname, &
                                               cd_attname, &
                                               rd_attvalue)
     IMPLICIT NONE
     CHARACTER(LEN=*), INTENT(IN) :: cd_ncfilename
     CHARACTER(LEN=*), INTENT(IN) :: cd_varname
     CHARACTER(LEN=*), INTENT(IN) :: cd_attname
     REAL(KIND=8),       INTENT(IN) :: rd_attvalue
   END SUBROUTINE MIOL_writef_attribute_R8_NC
   SUBROUTINE MIOL_writeu_attribute_R8_NC (id_file_id, &
                                               cd_varname, &
                                               cd_attname, &
                                               rd_attvalue)
     IMPLICIT NONE
     INTEGER,            INTENT(IN) :: id_file_id
     CHARACTER(LEN=*), INTENT(IN) :: cd_varname
     CHARACTER(LEN=*), INTENT(IN) :: cd_attname
     REAL(KIND=8),       INTENT(IN) :: rd_attvalue
   END SUBROUTINE MIOL_writeu_attribute_R8_NC
END INTERFACE !MIOL_write_attribute_NC

!> An interface for read attributes Short (INTEGER 2) NETCDF VALUES
!!
!<
INTERFACE MIOL_read_attribute_NC
SUBROUTINE MIOL_readf_attribute_S_NC (cd_filename, &
                                              cd_varname, &
                                              cd_attname, &
                                              id_attvalue)
  IMPLICIT NONE
  CHARACTER(LEN=*),           INTENT(IN) :: cd_filename
  CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
  CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
  INTEGER(KIND=2),                    INTENT(OUT) :: id_attvalue
END SUBROUTINE MIOL_readf_attribute_S_NC
 SUBROUTINE MIOL_readu_attribute_S_NC (id_file_id, &
                                              cd_varname, &
                                              cd_attname, &
                                              id_attvalue)
   IMPLICIT NONE
   INTEGER,                    INTENT(IN) :: id_file_id
   CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
   CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
   INTEGER(KIND=2),                    INTENT(OUT) :: id_attvalue
 END SUBROUTINE MIOL_readu_attribute_S_NC
END INTERFACE !MIOL_read_attribute_NC

!> An interface for write attributes Short (INTEGER 2) NETCDF VALUES
!!
!<
INTERFACE MIOL_write_attribute_NC
 SUBROUTINE MIOL_writef_attribute_S_NC (cd_ncfilename, &
                                               cd_varname, &
                                               cd_attname, &
                                               id_attvalue)
     IMPLICIT NONE
     CHARACTER(LEN=255), INTENT(IN) :: cd_ncfilename
     CHARACTER(LEN=255), INTENT(IN) :: cd_varname
     CHARACTER(LEN=255), INTENT(IN) :: cd_attname
     INTEGER(KIND=2),    INTENT(IN) :: id_attvalue
   END SUBROUTINE MIOL_writef_attribute_S_NC
 SUBROUTINE MIOL_writeu_attribute_S_NC (id_file_id, &
                                               cd_varname, &
                                               cd_attname, &
                                               id_attvalue)

   IMPLICIT NONE
   INTEGER,            INTENT(IN) :: id_file_id
   CHARACTER(LEN=255), INTENT(IN) :: cd_varname
   CHARACTER(LEN=255), INTENT(IN) :: cd_attname
   INTEGER(KIND=2),    INTENT(IN) :: id_attvalue
 END SUBROUTINE MIOL_writeu_attribute_S_NC

END INTERFACE  !MIOL_write_attribute_NC


#if defined Key_Byte

!> An interface for read attributes Byte (INTEGER 1) NETCDF VALUES
!!
!<
INTERFACE MIOL_read_attribute_NC
SUBROUTINE MIOL_readf_attribute_B_NC (cd_filename, &
                                              cd_varname, &
                                              cd_attname, &
                                              id_attvalue)
  IMPLICIT NONE
  CHARACTER(LEN=*),           INTENT(IN) :: cd_filename
  CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
  CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
  INTEGER(KIND=1),                    INTENT(OUT) :: id_attvalue
END SUBROUTINE MIOL_readf_attribute_B_NC
 SUBROUTINE MIOL_readu_attribute_B_NC (id_file_id, &
                                              cd_varname, &
                                              cd_attname, &
                                              id_attvalue)
   IMPLICIT NONE
   INTEGER,                    INTENT(IN) :: id_file_id
   CHARACTER(LEN=*),           INTENT(IN) :: cd_varname
   CHARACTER(LEN=*),           INTENT(IN) :: cd_attname
   INTEGER(KIND=1),                    INTENT(OUT) :: id_attvalue
 END SUBROUTINE MIOL_readu_attribute_B_NC
END INTERFACE !MIOL_read_attribute_NC

!> An interface for write attributes Byte (INTEGER 1) NETCDF VALUES
!!
!<
INTERFACE MIOL_write_attribute_NC
 SUBROUTINE MIOL_writef_attribute_B_NC (cd_ncfilename, &
                                               cd_varname, &
                                               cd_attname, &
                                               id_attvalue)
     IMPLICIT NONE
     CHARACTER(LEN=255), INTENT(IN) :: cd_ncfilename
     CHARACTER(LEN=255), INTENT(IN) :: cd_varname
     CHARACTER(LEN=255), INTENT(IN) :: cd_attname
     INTEGER(KIND=1),    INTENT(IN) :: id_attvalue
   END SUBROUTINE MIOL_writef_attribute_B_NC
 SUBROUTINE MIOL_writeu_attribute_B_NC (id_file_id, &
                                               cd_varname, &
                                               cd_attname, &
                                               id_attvalue)

   IMPLICIT NONE
   INTEGER,            INTENT(IN) :: id_file_id
   CHARACTER(LEN=255), INTENT(IN) :: cd_varname
   CHARACTER(LEN=255), INTENT(IN) :: cd_attname
   INTEGER(KIND=1),    INTENT(IN) :: id_attvalue
 END SUBROUTINE MIOL_writeu_attribute_B_NC

END INTERFACE  !MIOL_write_attribute_NC

#endif 

END MODULE INT_ATTRIBUTSTYPE
