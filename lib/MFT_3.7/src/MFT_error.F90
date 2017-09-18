!> \brief Module which contain NETCDF error functions 
!! \author C.REGNIER Miol V3.5
!! \date September 2008 
!!  \version 3.5
!<

MODULE MFT_error
!
! Fonctions d'erreurs en netcdf
!
USE netcdf

CONTAINS

  FUNCTION fi_ncError(id_status,cd_fonction) RESULT (il_result)
    
    INTEGER :: id_status, il_result
    CHARACTER(LEN=*) :: cd_fonction

    IF ( id_status.NE.NF90_NOERR ) THEN
       WRITE(0,*) '(NetCDF Handle error):',TRIM(cd_fonction),NF90_STRERROR(id_status)
       CALL flush(0)
       STOP
    ENDIF

    il_result = id_status

  END FUNCTION fi_ncError

  FUNCTION fi_memError(id_status, cd_variable,cd_fonction) RESULT(il_result)
         
    INTEGER :: id_status, il_result
    CHARACTER(LEN=*) :: cd_variable,cd_fonction

    IF ( id_status .NE. 0 ) THEN
       WRITE(0,*) TRIM(cd_fonction),' (memory allocation or deallocation error) :', TRIM(cd_variable)
       CALL flush(0)
       STOP
    ENDIF
    il_result = id_status

  END FUNCTION fi_memError

  FUNCTION fi_arrError(rd_minval, rd_maxval, cd_array,cd_fonction) RESULT (il_result)
 
            INTEGER :: il_result
            REAL(KIND=4) :: rd_minval, rd_maxval
            CHARACTER(LEN=*) :: cd_array
            CHARACTER(LEN=*) :: cd_fonction

            IF (rd_minval .EQ. rd_maxval) THEN
               WRITE(0,*)  TRIM(cd_fonction),' (array min-max error) : ',TRIM(cd_array)
               CALL flush(0)
            ENDIF
 
            il_result = 1
 
          END FUNCTION fi_arrError

    FUNCTION fi_arrError_R8(rd_minval, rd_maxval, cd_array,cd_fonction) RESULT (il_result)

            INTEGER :: il_result
            REAL(KIND=8) :: rd_minval, rd_maxval
            CHARACTER(LEN=*) :: cd_array
            CHARACTER(LEN=*) :: cd_fonction

            IF (rd_minval .EQ. rd_maxval) THEN
               WRITE(0,*)  TRIM(cd_fonction),' (array min-max error) : ',TRIM(cd_array)
               CALL flush(0)
            ENDIF

            il_result = 1

     END FUNCTION fi_arrError_R8


!******************************************************************************
!******************************************************************************
!******************************************************************************
!
         !---------------------------------------------------------------------
         !> \brief
         !! Description: handle error function.
         !!
         !! Use:
         !!
         !!    INTEGER, INTENT(IN) :: id_status
         !!    
         !!   @param  id_status   The integer representing the status.
         !!   @param  cd_fonction name of the function
         !!
         ! History: (11/2006) | F.Messal | Creation
         !<
         !---------------------------------------------------------------------
! 
     SUBROUTINE hdlerr(id_status,cd_fonction)
!
      USE netcdf
      IMPLICIT NONE
!        
         INTEGER,         INTENT(IN) :: id_status
         CHARACTER(LEN=*),INTENT(IN) :: cd_fonction

         IF( id_status .NE. NF90_NOERR ) THEN
            WRITE(0,*) ' Netcdf Handle error):',TRIM(cd_fonction), NF90_STRERROR(id_status)
            CALL flush(0)
            CALL EXIT(3) 
!!               STOP
         ENDIF
!
         RETURN
!
      END SUBROUTINE hdlerr
!
!
!
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
         !---------------------------------------------------------------------
         !> \brief
         !!Description : Memory allocation or deallocation error function.
         !!
         !! Use:
         !!
         !!    INTEGER,          INTENT(INOUT) :: id_status
         !!    CHARACTER(LEN=*), INTENT(IN) :: cd_variable
         !!
         !!    @param id_status     The integer representing the status.
         !!    @param cd_variable   Name of the array which needs memory.
         !!   @param  cd_fonction name of the function
         !! History : (11/2006) | F.Messal | Creation
         !<
         !---------------------------------------------------------------------
!
      SUBROUTINE memerr(id_status, &
                        cd_variable,cd_fonction)
!
         IMPLICIT NONE
!
         INTEGER,          INTENT(INOUT) :: id_status
         CHARACTER(LEN=*), INTENT(IN)    :: cd_variable
         CHARACTER(LEN=*),INTENT(IN)     :: cd_fonction

         IF ( id_status .NE. 0 ) THEN
              WRITE(0,*)  TRIM(cd_fonction),'Memory allocation or deallocation error:', &
                         TRIM(cd_variable)
              CALL flush(0)
              STOP
         ENDIF
!
         id_status = 1
!
         ENDSUBROUTINE memerr
!
!
!
!******************************************************************************
!************************************************************************
!******************************************************************************

END MODULE MFT_error
