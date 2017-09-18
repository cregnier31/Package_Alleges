!> \brief Module which contain Interfaces for READ WRITE attributes scale factor in NETCDF format
!! \author C.REGNIER Miol V3.5
!! \date September 2008
!!  \version 3.5
!<
MODULE INT_ATTRIBUTS
 !
  ! -----------------------------------------------------------------
  ! --- General interface MIOL_read_att_scale_offset_NCC
  !---  MIOL_readf_att_scale_offset_NC(cd_filename,cd_varname,cda_attname,rda_offsetvalue)
  !---  MIOL_readu_att_scale_offset_NC(id_filename,cd_varname,cda_attname,rda_offsetvalue)
  ! C.REGNIER 09/2008
  ! -----------------------------------------------------------------

!> An interface for read attributes scale factor in NETCDF
!!
!<

 INTERFACE MIOL_read_att_scale_offset_NC
         
   SUBROUTINE MIOL_readf_att_sc_off_R8_NC(cd_filename,&
                                           cd_varname,&
                                           cda_attname,&
                                           rda_offsetvalue)
    implicit none
    CHARACTER(LEN=*),           INTENT( IN)      :: cd_filename
    CHARACTER(LEN=*),           INTENT( IN)      :: cd_varname
    CHARACTER(LEN=255), DIMENSION(2),INTENT( IN) :: cda_attname  
    REAL(KIND=8), DIMENSION(2) ,INTENT(OUT)      :: rda_offsetvalue
 
 END SUBROUTINE MIOL_readf_att_sc_off_R8_NC

  SUBROUTINE MIOL_readf_att_sc_off_R4_NC(cd_filename,&
                                           cd_varname,&
                                           cda_attname,&
                                           rda_offsetvalue)
    implicit none
    CHARACTER(LEN=*),           INTENT( IN)      :: cd_filename
    CHARACTER(LEN=*),           INTENT( IN)      :: cd_varname
    CHARACTER(LEN=255), DIMENSION(2),INTENT( IN) :: cda_attname  
    REAL(KIND=4), DIMENSION(2) ,INTENT(OUT)      :: rda_offsetvalue
  END SUBROUTINE MIOL_readf_att_sc_off_R4_NC


 SUBROUTINE MIOL_readu_att_sc_off_R8_NC(id_filename,&
                                            cd_varname,&
                                            cda_attname,&
                                            rda_offsetvalue)
    implicit none
    INTEGER,           INTENT( IN)               :: id_filename
    CHARACTER(LEN=*),           INTENT( IN)      :: cd_varname
    CHARACTER(LEN=255), DIMENSION(2),INTENT( IN) :: cda_attname
    REAL(KIND=8), DIMENSION(2) ,INTENT(OUT)      :: rda_offsetvalue
  
 END SUBROUTINE MIOL_readu_att_sc_off_R8_NC

 SUBROUTINE MIOL_readu_att_sc_off_R4_NC(id_filename,&
                                            cd_varname,&
                                            cda_attname,&
                                            rda_offsetvalue)
    implicit none
    INTEGER,           INTENT( IN)               :: id_filename
    CHARACTER(LEN=*),           INTENT( IN)      :: cd_varname
    CHARACTER(LEN=255), DIMENSION(2),INTENT( IN) :: cda_attname
    REAL(KIND=4), DIMENSION(2) ,INTENT(OUT)      :: rda_offsetvalue
  
 END SUBROUTINE MIOL_readu_att_sc_off_R4_NC    
 END INTERFACE !MIOL_read_att_scale_offset_NC

END MODULE INT_ATTRIBUTS
