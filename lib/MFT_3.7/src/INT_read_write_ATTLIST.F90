!> \brief Module which contain Interfaces for READ WRITE attributes list in NETCDF format
!! \author C.REGNIER Miol V3.5
!! \date Janvier 2013
!!  \version 3.5
!<
MODULE INT_Read_Write_ATTLIST

  ! -----------------------------------------------------------------
  ! --- General interface
  ! ---   interface MIOL_read_attributeslist_NC
  ! ---  MIOL_readf_attributeslist_NC (cd_filename,cd_varname,id_nbatt,cdpa_attname,cdpa_attvalue,rdpa_attvalue,idpa_typevalue)
  ! ---  MIOL_readu_attributeslist_NC (id_file_id,cd_varname,id_nbatt,cdpa_attname,cdpa_attvalue,rdpa_attvalue,idpa_typevalue)
  ! ----------------------------------------------------------------
  ! ---   interface MIOL_write_attributeslist_NC
  ! ---  MIOL_writef_attributeslist_NC (cd_filename,cd_varname,id_nbatt,cda_attname,cda_attvalue,rda_attvalue,ida_typevalue)
  ! ---  MIOL_writeu_attributeslist_NC (id_file_id,cd_varname,id_nbatt,cda_attname,cda_attvalue,rda_attvalue,ida_typevalue)
  !----------------------------------------------------------------

!> An interface for read attributes list in NETCDF
!!
!<


INTERFACE MIOL_read_attributeslist_NC

 SUBROUTINE MIOL_readf_attributeslist_NC (cd_filename, &
                                            cd_varname, &
                                            id_nbatt, &
                                            cdpa_attname, &
                                            cdpa_attvalue, &
                                            rdpa_attvalue, &
                                            idpa_typevalue)
   IMPLICIT NONE
   CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
   CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
   INTEGER,                          INTENT(OUT) :: id_nbatt
   CHARACTER(LEN=255), DIMENSION(:), POINTER:: cdpa_attname
   CHARACTER(LEN=255), DIMENSION(:), POINTER :: cdpa_attvalue
   REAL(KIND=4), DIMENSION(:),       POINTER, OPTIONAL   :: rdpa_attvalue
   INTEGER, DIMENSION(:),            POINTER, OPTIONAL :: idpa_typevalue
END SUBROUTINE MIOL_readf_attributeslist_NC
SUBROUTINE MIOL_readu_attributeslist_NC (id_file_id, &
                                            cd_varname, &
                                            id_nbatt, &
                                            cdpa_attname, &
                                            cdpa_attvalue, &
                                            rdpa_attvalue, &
                                            idpa_typevalue)
 
  IMPLICIT NONE
  INTEGER,                          INTENT(IN) :: id_file_id
  CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
  INTEGER,                          INTENT(OUT) :: id_nbatt
  CHARACTER(LEN=255), DIMENSION(:), POINTER :: cdpa_attname
  CHARACTER(LEN=255), DIMENSION(:), POINTER  :: cdpa_attvalue
  REAL(KIND=4), DIMENSION(:),       POINTER, OPTIONAL  :: rdpa_attvalue
  INTEGER, DIMENSION(:),            POINTER ,OPTIONAL  :: idpa_typevalue
END SUBROUTINE MIOL_readu_attributeslist_NC
END INTERFACE !MIOL_read_attributeslist_NC
!**
!> An interface for write attributes list in NETCDF
!!
!<
INTERFACE MIOL_write_attributeslist_NC
  SUBROUTINE MIOL_writeu_attributeslist_NC (id_file_id, &
                                                  cd_varname, &
                                                  id_nbatt, &
                                                  cda_attname, &
                                                  cda_attvalue, &
                                                  rda_attvalue, &
                                                  ida_typevalue)
    IMPLICIT NONE
    INTEGER,                          INTENT(IN) :: id_file_id
    CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
    INTEGER,                          INTENT(IN) :: id_nbatt
    CHARACTER(LEN=255), DIMENSION(:) ,INTENT(IN)            :: cda_attname
    CHARACTER(LEN=255), DIMENSION(:) ,INTENT(IN)             :: cda_attvalue
    REAL(KIND=4), DIMENSION(:),         OPTIONAL,INTENT(IN)  :: rda_attvalue
    INTEGER, DIMENSION(:),              OPTIONAL,INTENT(IN)  :: ida_typevalue
  END SUBROUTINE MIOL_writeu_attributeslist_NC
SUBROUTINE MIOL_writef_attributeslist_NC (cd_filename, &
                                                  cd_varname, &
                                                  id_nbatt, &
                                                  cda_attname, &
                                                  cda_attvalue, &
                                                  rda_attvalue, &
                                                  ida_typevalue)
  IMPLICIT NONE
  CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
  CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
  INTEGER,                          INTENT(IN) :: id_nbatt
  CHARACTER(LEN=255), DIMENSION(:) ,INTENT(IN)            :: cda_attname
  CHARACTER(LEN=255), DIMENSION(:), INTENT(IN)            :: cda_attvalue
  REAL(KIND=4), DIMENSION(:),         OPTIONAL,INTENT(IN) :: rda_attvalue
  INTEGER, DIMENSION(:),              OPTIONAL,INTENT(IN) :: ida_typevalue
END SUBROUTINE MIOL_writef_attributeslist_NC
END INTERFACE !MIOL_write_attributeslist_NC
!**
END MODULE INT_Read_Write_ATTLIST
