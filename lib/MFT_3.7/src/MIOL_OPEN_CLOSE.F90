!> \brief Module which contain OPEN/CLOSE subroutines for NETCDF files
!! \author C.REGNIER Miol V3.5
!! \date September 2008 
!!  \version 3.5
!<
MODULE MIOL_OPEN_CLOSE


CONTAINS
    SUBROUTINE MIOL_create_file_NOCLUB_NC (cd_filename, &
                                        id_nbdim, &
                                        cda_dimname, &
                                        ida_dimlen,&
                                        ida_dimsout_id)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),                        INTENT(IN) :: cd_filename
         INTEGER,                                 INTENT(IN) :: id_nbdim
         CHARACTER(LEN=*), DIMENSION(id_nbdim),   INTENT(IN) :: cda_dimname
         INTEGER, DIMENSION(id_nbdim),            INTENT(IN) :: ida_dimlen
         INTEGER, DIMENSION(id_nbdim),            INTENT(OUT) :: ida_dimsout_id

         INTEGER :: il_ji, il_output_id, il_status
         CHARACTER(LEN=255), DIMENSION(id_nbdim) :: cla_dimname
         CHARACTER(LEN=255) :: cl_fonction
         
         !-----------------------------------------------------------------------
  
         cl_fonction="MIOL_create_file_NC" 

         DO il_ji=1, id_nbdim
            cla_dimname(il_ji) = TRIM(cda_dimname(il_ji))
         ENDDO
 
         !------------------------------------------------------------------------
         ! Create the Netcdf file
         il_status = fi_ncError(NF90_CREATE(TRIM(cd_filename), &
                                            or(NF90_CLOBBER, &
                                            NF90_64bit_offset),&
                                            il_output_id),cl_fonction)
 
         !------------------------------------------------------------------------
         ! Write dimensions
 
         DO il_ji = 1, id_nbdim
            il_status = fi_ncError(NF90_DEF_DIM(il_output_id, &
                                                cda_dimname(il_ji), &
                                                ida_dimlen(il_ji), &
                                                ida_dimsout_id(il_ji)),cl_fonction)

         ENDDO
         il_status = fi_ncError(NF90_ENDDEF(il_output_id),cl_fonction)
 
         !------------------------------------------------------------------------
         ! Close file
 
      !   il_status = fi_ncError(NF90_CLOSE(il_output_id),cl_fonction)
 
 
       END SUBROUTINE MIOL_create_file_NOCLUB_NC
 
  !*****************************************************************************



  !******************************************************************************
  !******************************************************************************
 
      SUBROUTINE MIOL_openw_file_NC(cd_filename, &
				    id_file_id)
 
  	  USE netcdf
 	  USE MFT_error
	  IMPLICIT NONE
 
          !!=====================================================================
          !!
          !! Description: You can open a file. This function returns the NetCDF Id.
          !!
          !! cd_filename       A NetCDF filename. You must specify the complete path.
          !! id_file_id        The file Id.
          !!
          !! History :
          !!        !  06/2007  (F. Messal) CVS V1.0
          !!
          !!=====================================================================
 
          !----------------------------------------------------------------------
 
          CHARACTER(LEN=*), INTENT(IN) :: cd_filename
          INTEGER,INTENT(OUT) :: id_file_id

          INTEGER  :: il_file_id, il_status
          CHARACTER(LEN=255) :: cl_fonction
          
          cl_fonction="MIOL_openw_file_NC"
         
          !----------------------------------------------------------------------
          ! Open file
 
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_WRITE, &
                                           il_file_id),cl_fonction)
 
          id_file_id = il_file_id
 
       ENDSUBROUTINE MIOL_openw_file_NC
 
 !******************************************************************************
 !******************************************************************************
 !******************************************************************************
 
 
      SUBROUTINE MIOL_openr_file_NC(cd_filename, &
                                     id_file_id)
 
  	  USE netcdf
	  USE MFT_error
          IMPLICIT NONE
 
          !!=====================================================================
          !!
          !! Description: You can open a file. This function returns the NetCDF Id.
          !!
          !! cd_filename       A NetCDF filename. You must specify the complete path.
          !! id_file_id        The file Id.
          !!
          !! History :
          !!        !  06/2007  (F. Messal) CVS V1.0
          !!
          !!=====================================================================
 
          !----------------------------------------------------------------------
 
          CHARACTER(LEN=*), INTENT(IN) :: cd_filename
          INTEGER,INTENT(OUT) :: id_file_id
          
          INTEGER :: il_file_id, il_status
          CHARACTER(LEN=255) :: cl_fonction
          
          cl_fonction="MIOL_openr_file_NC"
          !----------------------------------------------------------------------
          ! Open file
 
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_NOWRITE, &
                                           il_file_id),cl_fonction)
 
          id_file_id = il_file_id
 
        END SUBROUTINE MIOL_openr_file_NC
 
  !******************************************************************************
 
 
      SUBROUTINE MIOL_close_file_NC(id_file_id)
	  USE netcdf
          USE MFT_error
           IMPLICIT NONE
 
          !!=====================================================================
          !!
          !! Description: To close a NetCDF file.
          !!
          !! id_file_id        The file Id.
          !!
          !! History :
          !!        !  06/2007  (F. Messal) CVS V1.0
          !!
          !!=====================================================================
 
          !----------------------------------------------------------------------
 
          INTEGER,INTENT(IN) :: id_file_id
          
          INTEGER  :: il_status
          CHARACTER(LEN=255) :: cl_fonction
          
          cl_fonction="MIOL_close_file_NC"
          !----------------------------------------------------------------------
          ! Open file
 
          il_status = fi_ncError(NF90_CLOSE(id_file_id),cl_fonction)
 
 
        END SUBROUTINE MIOL_close_file_NC
 
 !******************************************************************************
 !*********************************************************
 !******************************************************************************

END MODULE MIOL_OPEN_CLOSE
