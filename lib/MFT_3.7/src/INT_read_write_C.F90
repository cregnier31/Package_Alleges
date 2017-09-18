!> \brief Module which contain Interfaces for READ WRITE Character values in NETCDF format
!! \author C.REGNIER Miol V3.5
!! \date Janvier 2013
!!  \version 3.5
!!  History :
!!   09/2009  (C.REGNIER) Creation
!!   01/2009  (C.REGNIER) Correction sur la lecture des champs, en fonction de
!!                         la taille du charactere il peut lire un champ 1-2-3-4D 
!!   01/2013  (C.REGNIER) Ajout de l'ecriture des characteres 1D et 2D
!<
MODULE INT_read_write_C
  implicit none
  ! -----------------------------------------------------------------
  ! --- General interface  
  ! ---  MIOL_read_field_NC interface 
  ! ---  MIOL_readf_field_C_1D_NC (cd_filename,cd_varname,cdpa_value,ida_dimsize)
  ! ---  MIOL_readf_field_C_2D_NC (cd_filename,cd_varname,cdpa_value,ida_dimsize)
  ! ---  MIOL_readf_field_C_3D_NC (cd_filename,cd_varname,cdpa_value,ida_dimsize)
  ! ---  MIOL_readf_field_C_4D_NC (cd_filename,cd_varname,cdpa_value,ida_dimsize)
  ! ---  MIOL_readu_field_C_1D_NC (id_file_id,cd_varname,cdpa_value,id_stringsize,ida_dimsize) 
  ! ---  MIOL_readu_field_C_2D_NC (id_file_id,cd_varname,cdpa_value,id_stringsize,ida_dimsize) 
  ! ---  MIOL_readu_field_C_3D_NC (id_file_id,cd_varname,cdpa_value,id_stringsize,ida_dimsize) 
  ! ---  MIOL_readu_field_C_4D_NC (id_file_id,cd_varname,cdpa_value,id_stringsize,ida_dimsize) 
  ! ---  MIOL_writef_field_C_1D_NC (cd_filename,cd_varname,cdpa_value,id_stringsize)
  ! ---  MIOL_writef_field_C_2D_NC (cd_filename,cd_varname,cdpa_value,id_stringsize)
  ! ---  MIOL_writeu_field_C_1D_NC (id_file_id,cd_varname,cdpa_value,id_stringsize)
  ! ---  MIOL_writeu_field_C_2D_NC (id_file_id,cd_varname,cdpa_value,id_stringsize)
  ! ---  C.REGNIER Septembre 2008
  !---  C.REGNIER Mars 2009 : Generalisation des routines de lecture et ajout des lectures 3D et 4D
!**
!> An interface for read Character Values in NETCDF
!!
!<

INTERFACE MIOL_read_field_NC
  SUBROUTINE MIOL_readf_field_C_1D_NC (cd_filename, &   ! Name of the NetCDF file
                                       cd_varname, &    ! Variable name
                                       cdpa_value, &
                                       id_stringsize, & ! length of string
                                       ida_dimsize)     ! Length of each dimension
   IMPLICIT NONE
   CHARACTER(LEN=*), INTENT(IN)                          :: cd_filename
   CHARACTER(LEN=*), INTENT(IN)                          :: cd_varname
   CHARACTER(LEN=id_stringsize), DIMENSION(:), POINTER   :: cdpa_value
   INTEGER, INTENT(IN)                                   :: id_stringsize                      
   INTEGER, INTENT(OUT), DIMENSION(1), OPTIONAL          :: ida_dimsize

END SUBROUTINE MIOL_readf_field_C_1D_NC

 SUBROUTINE MIOL_readu_field_C_1D_NC (id_file_id, &    ! Name of the NetCDF file
                                      cd_varname, &    ! Variable name
                                      cdpa_value, &
                                      id_stringsize,&! length of string
                                      ida_dimsize)     ! Length of each dimension
   
   IMPLICIT NONE
   INTEGER, INTENT(IN)                                 :: id_file_id
   CHARACTER(LEN=*), INTENT(IN)                        :: cd_varname
   CHARACTER(LEN=id_stringsize), DIMENSION(:), POINTER :: cdpa_value
   INTEGER, INTENT(IN)                                 :: id_stringsize                      
   INTEGER, INTENT(OUT), DIMENSION(1), OPTIONAL        :: ida_dimsize

 END SUBROUTINE MIOL_readu_field_C_1D_NC

SUBROUTINE MIOL_readf_field_C_2D_NC (cd_filename, &         ! Name of the NetCDF file
                                     cd_varname, &    ! Variable name
                                     cdpa_value, &
                                     id_stringsize,&  ! length of string
                                     ida_dimsize)     ! Length of each dimension
   IMPLICIT NONE
   CHARACTER(LEN=*), INTENT(IN) :: cd_filename
   CHARACTER(LEN=*), INTENT(IN) :: cd_varname
   CHARACTER(LEN=id_stringsize), DIMENSION(:,:), POINTER :: cdpa_value
   INTEGER, INTENT(IN) :: id_stringsize                    
   INTEGER, INTENT(OUT), DIMENSION(2), OPTIONAL :: ida_dimsize
 END SUBROUTINE MIOL_readf_field_C_2D_NC

 SUBROUTINE MIOL_readu_field_C_2D_NC (id_file_id, &   ! Name of the NetCDF file
                                      cd_varname, &   ! Variable name
                                      cdpa_value, &
                                      id_stringsize,& ! length of string
                                      ida_dimsize)    ! Length of each dimension
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: id_file_id
  CHARACTER(LEN=*), INTENT(IN) :: cd_varname
  CHARACTER(LEN=id_stringsize), DIMENSION(:,:), POINTER :: cdpa_value
  INTEGER, INTENT(IN)                                  :: id_stringsize                     
  INTEGER, INTENT(OUT), DIMENSION(2), OPTIONAL :: ida_dimsize
 END SUBROUTINE MIOL_readu_field_C_2D_NC

 SUBROUTINE MIOL_readu_field_C_3D_NC (id_file_id, &   ! Name of the NetCDF file
                                      cd_varname, &    ! Variable name
                                      cdpa_value, &
                                      id_stringsize,&  ! length of string
                                      ida_dimsize)     ! Length of each dimension

  IMPLICIT NONE
  INTEGER, INTENT(IN) :: id_file_id
  CHARACTER(LEN=*), INTENT(IN) :: cd_varname
  CHARACTER(LEN=id_stringsize), DIMENSION(:,:,:), POINTER :: cdpa_value
  INTEGER, INTENT(IN)                                     :: id_stringsize                    
  INTEGER, INTENT(OUT), DIMENSION(3), OPTIONAL            :: ida_dimsize
END SUBROUTINE MIOL_readu_field_C_3D_NC

 SUBROUTINE MIOL_readf_field_C_3D_NC (cd_filename, &   ! Name of the NetCDF file
                                      cd_varname, &    ! Variable name
                                      cdpa_value, &
                                      id_stringsize,&  ! length of string
                                      ida_dimsize)     ! Length of each dimension
   IMPLICIT NONE
   CHARACTER(LEN=*), INTENT(IN) :: cd_varname,cd_filename
   CHARACTER(LEN=id_stringsize), DIMENSION(:,:,:), POINTER :: cdpa_value
   INTEGER, INTENT(IN)                                  :: id_stringsize               
   INTEGER, INTENT(OUT), DIMENSION(3), OPTIONAL :: ida_dimsize
 END SUBROUTINE MIOL_readf_field_C_3D_NC

   SUBROUTINE MIOL_readu_field_C_4D_NC (id_file_id, &   ! Name of the NetCDF file
                                        cd_varname, &    ! Variable name
                                        cdpa_value, &
                                        id_stringsize,&  ! length of string
                                        ida_dimsize)     ! Length of each dimension
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: id_file_id
  CHARACTER(LEN=*), INTENT(IN) :: cd_varname
  CHARACTER(LEN=id_stringsize), DIMENSION(:,:,:,:), POINTER :: cdpa_value
  INTEGER, INTENT(IN)                                       :: id_stringsize                   
  INTEGER, INTENT(OUT), DIMENSION(4), OPTIONAL :: ida_dimsize


END SUBROUTINE MIOL_readu_field_C_4D_NC
SUBROUTINE MIOL_readf_field_C_4D_NC (cd_filename, &   ! Name of the NetCDF file
                                     cd_varname, &    ! Variable name
                                     cdpa_value, &
                                     id_stringsize,&
                                     ida_dimsize)     ! Length of each dimension
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: cd_varname,cd_filename
  CHARACTER(LEN=id_stringsize), DIMENSION(:,:,:,:), POINTER :: cdpa_value
  INTEGER, INTENT(IN)                                       :: id_stringsize                     
  INTEGER, INTENT(OUT), DIMENSION(4), OPTIONAL :: ida_dimsize 
 END SUBROUTINE MIOL_readf_field_C_4D_NC

END INTERFACE !MIOL_read_field_NC
!**
INTERFACE MIOL_write_field_NC
 SUBROUTINE MIOL_writef_field_C_1D_NC (cd_filename, &  ! Name of the NetCDF file
                                       cd_varname, &   ! Variable name
                                       cd_key, &
                                       cda_varvalue, &
                                       id_length)  ! length of char variable
  IMPLICIT NONE
  CHARACTER(LEN=*),            INTENT(IN) :: cd_filename
  CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
  CHARACTER(LEN=2),            INTENT(IN) :: cd_key
  CHARACTER(LEN=id_length),DIMENSION(:),INTENT(IN) :: cda_varvalue
  INTEGER(KIND=4),             INTENT(IN) :: id_length

  END SUBROUTINE MIOL_writef_field_C_1D_NC

SUBROUTINE MIOL_writeu_field_C_1D_NC (id_file_id, &  ! Name of the NetCDF file                                                  
                                      cd_varname, &   ! Variable name
                                      cd_key, &
                                      cda_varvalue, &
                                      id_length)  ! length of char variable
  IMPLICIT NONE
  INTEGER(KIND=4),             INTENT(IN) :: id_file_id 
  CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
  CHARACTER(LEN=2),            INTENT(IN) :: cd_key
  CHARACTER(LEN=id_length),DIMENSION(:),INTENT(IN) :: cda_varvalue
  INTEGER(KIND=4),             INTENT(IN) :: id_length

  END SUBROUTINE MIOL_writeu_field_C_1D_NC

SUBROUTINE MIOL_writeu_field_C_2D_NC (id_file_id, &  ! Name of the NetCDF file                                                  
                                      cd_varname, &   ! Variable name
                                      cd_key, &
                                      cda_varvalue, &
                                      id_length)  ! length of char variable
  IMPLICIT NONE
  INTEGER(KIND=4),             INTENT(IN) :: id_file_id 
  CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
  CHARACTER(LEN=3),            INTENT(IN) :: cd_key
  CHARACTER(LEN=id_length),DIMENSION(:,:),INTENT(IN) :: cda_varvalue
  INTEGER(KIND=4),             INTENT(IN) :: id_length

  END SUBROUTINE MIOL_writeu_field_C_2D_NC


END INTERFACE ! MIOL_write_field_NC
END MODULE INT_read_write_C
