!**-----------------------------------------
!** Module for read Character values in NETCDF format
!** C.REGNIER Septembre 2008 V3 Miol 
!**-------------------------------------------
! --------------------------------------- General interface --------------------------------------------
  ! ---  MIOL_read_field_NC interface 
  ! ---  MIOL_readf_field_C_1D_NC (cd_filename,cd_varname,cdpa_value,id_stringsize,ida_dimsize)
  ! ---  MIOL_readf_field_C_2D_NC (cd_filename,cd_varname,cdpa_value,id_stringsize,ida_dimsize)
  ! ---  MIOL_readf_field_C_3D_NC (cd_filename,cd_varname,cdpa_value,id_stringsize,ida_dimsize)
  ! ---  MIOL_readf_field_C_4D_NC (cd_filename,cd_varname,cdpa_value,id_stringsize,ida_dimsize)
  ! ---  MIOL_readu_field_C_1D_NC (id_file_id,cd_varname,cdpa_value,id_stringsize,ida_dimsize) 
  ! ---  MIOL_readu_field_C_2D_NC (id_file_id,cd_varname,cdpa_value,id_stringsize,ida_dimsize) 
  ! ---  MIOL_readu_field_C_3D_NC (id_file_id,cd_varname,cdpa_value,id_stringsize,ida_dimsize) 
  ! ---  MIOL_readu_field_C_4D_NC (id_file_id,cd_varname,cdpa_value,id_stringsize,ida_dimsize) 
  

!******************************************************************************
 !******************************************************************************
!> \brief File which contain subroutine for Read Character values in NETCDF format 
!! \n  Total 8 functions
!! \author F.MESSAL first version
!! \date 07/2007
!! \version 1.1
!! \author C.REGNIER Miol V3.5
!! \date 01/2013
!! \author C.REGNIER Miol V3.5
!! \date 03/2003
!! \version 3.5
!<
    
         !!===================================================================== 
         !> \brief
         !! Description: Read the 1D data values from a Netcdf file.
         !!
         !! Use: You have to declarate the array rdpa_value in the main program
         !! as a pointer: CHARACTER(LEN=14), POINTER, DIMENSION(:) :: cdpa_value
         !! @param cd_filename  A NetCDF filename. You must specify the complete path.
         !! @param cd_varname Variable name
         !! @param cdpa_value The data values to be read in char format 
         !! @param id_dim dimension of the character value
         !! @param ida_dimsize Returned vector of values corresponding to the length of each dimension     
         !!
         !!   \n History :
         !!        \n  09/2006  (F. Messal) F90 
         !!        \n  01/2013  (C.REGNIER) SVN V3.5.0
         !!        \n  01/2009  (C.REGNIER) Correction sur la lecture des champs
         !!        \n  03/2009  (C.REGNIER) Modifications
         !<
         !!=====================================================================
 
      SUBROUTINE MIOL_readu_field_C_1D_NC (id_file_id, &    ! Name of the NetCDF file
                                           cd_varname, &    ! Variable name
                                           cdpa_value, &
                                           id_stringsize,&
                                           ida_dimsize)     ! Length of each dimension
 
         USE netcdf
         USE MFT_error
         USE MIOL_param
         IMPLICIT NONE
        
         !----------------------------------------------------------------------
 
         INTEGER, INTENT(IN)                                 :: id_file_id
         CHARACTER(LEN=*), INTENT(IN)                        :: cd_varname
         CHARACTER(LEN=id_stringsize), DIMENSION(:), POINTER :: cdpa_value
         INTEGER, INTENT(IN)                                 :: id_stringsize                               
         INTEGER, INTENT(OUT), DIMENSION(1), OPTIONAL        :: ida_dimsize
         CHARACTER(LEN=255), DIMENSION(1) :: cla_dimname
         CHARACTER(LEN=255)               :: cl_fonction
         CHARACTER(LEN=100)            ::  cl_tmp
         INTEGER, DIMENSION(1) :: ila_dimsize, ila_dimids
         INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji
 
          cl_fonction="MIOL_readu_field_C_1D_NC"
          !----------------------------------------------------------------------
          ! Inquire variable
          il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_VARIABLE(id_file_id, &
                                                       il_var_id, &
                                                       cl_tmp, &
                                                       il_type, &
                                                       il_nbdim, &
                                                       ila_dimids),cl_fonction)
 
         IF (il_nbdim.NE.1) THEN
            WRITE(0,*) "MIOL_read_ncvar_field: array dimension error. "
            CALL flush(0)
            STOP
         ENDIF
         !----------------------------------------------------------------------
         !** read lenght of dimensions
         DO il_ji = 1, il_nbdim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id,&
                                                          ila_dimids(il_ji), &
                                                          cla_dimname(il_ji), &
                                                          ila_dimsize(il_ji)),cl_fonction)
         ENDDO
         
         IF (id_stringsize .EQ.1 ) THEN
         !----------------------------------------------------------------------
         ! memory allocation
         ALLOCATE(cdpa_value(ila_dimsize(1)),stat=il_status)
         il_status = fi_memError(il_status, ' cdpa_value',cl_fonction)


            il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                             il_var_id, &
                                             cdpa_value,&
                                             start = (/ 1 /),&
                                             count = (/ ila_dimsize(1) /),&
                                             stride =(/ 1 /)),cl_fonction)         
            
         ELSE
            !----------------------------------------------------------------------
            ! memory allocation
            ALLOCATE(cdpa_value(1),stat=il_status)
            il_status = fi_memError(il_status, ' cdpa_value',cl_fonction)
                     
            il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                             il_var_id, &
                                             cdpa_value),cl_fonction)
         ENDIF
         !----------------------------------------------------------------------
         ! complete the correct lenght of dimensions
         IF (PRESENT(ida_dimsize)) THEN
            ida_dimsize = SHAPE(cdpa_value)
         ENDIF
       
          
       END SUBROUTINE MIOL_readu_field_C_1D_NC
 
 !******************************************************************************
 !******************************************************************************


         !!=====================================================================
         !> \brief
         !! Description: Read the 1D data values from a Netcdf file.
         !!
         !! Use: You have to declarate the array rdpa_value in the main program
         !! as a pointer: CHARACTER(LEN=14), POINTER, DIMENSION(:) :: cdpa_value
         !! @param cd_filename  A NetCDF filename. You must specify the complete path.
         !! @param cd_varname Variable name
         !! @param cdpa_value The data values to be read in char format 
         !! @param id_dim dimension of the character value
         !! @param ida_dimsize Returned vector of values corresponding to the length of each dimension     
         !! History :
         !!        \n  09/2006  (F. Messal) F90
         !!        \n  01/2013  (C.REGNIER) SVN V3.5.0 
         !!        \n  01/2009  (C.REGNIER) Correction sur la lecture des champs
         !!        \n  03/2009  (C.REGNIER) Modifications
         !<
         !!=====================================================================
 
     SUBROUTINE MIOL_readf_field_C_1D_NC (cd_filename, &   ! Name of the NetCDF file
                                               cd_varname, &    ! Variable name
                                               cdpa_value, &
                                               id_stringsize, & 
                                               ida_dimsize)     ! Length of each dimension
 
         USE netcdf
         USE MFT_error
         USE MIOL_param
         IMPLICIT NONE
 
         !----------------------------------------------------------------------
 
         CHARACTER(LEN=*), INTENT(IN) :: cd_filename
         CHARACTER(LEN=*), INTENT(IN) :: cd_varname
         CHARACTER(LEN=id_stringsize), DIMENSION(:), POINTER :: cdpa_value
         INTEGER, INTENT(IN) :: id_stringsize                      
         INTEGER, INTENT(OUT), DIMENSION(1), OPTIONAL :: ida_dimsize
         CHARACTER(LEN=255), DIMENSION(1) :: cla_dimname
         CHARACTER(LEN=255) :: cl_fonction
         CHARACTER(LEN=100) :: cl_tmp
         INTEGER, DIMENSION(1) :: ila_dimsize, ila_dimids
         INTEGER :: il_nbdim, il_type, il_file_id, il_var_id, il_status, il_ji

         cl_fonction="MIOL_readf_field_C_1D_NC"
         
         !----------------------------------------------------------------------
          ! Open file
 
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_NOWRITE, &
                                           il_file_id),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Inquire variable
          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                cd_varname, &
                                                il_var_id),cl_fonction)
 
          il_status = fi_ncError(NF90_INQUIRE_VARIABLE(il_file_id, &
                                                       il_var_id, &
                                                       cl_tmp, &
                                                       il_type, &
                                                       il_nbdim, &
                                                       ila_dimids),cl_fonction)
 
          
          IF (il_nbdim.NE.1) THEN
            WRITE(0,*) "MIOL_read_ncvar_field: array dimension error. "
            CALL flush(0)
            STOP
         ENDIF
         
         !----------------------------------------------------------------------
         !** read lenght of dimensions
         DO il_ji = 1, il_nbdim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id,&
                                                          ila_dimids(il_ji), &
                                                          cla_dimname(il_ji), &
                                                          ila_dimsize(il_ji)),cl_fonction)
         ENDDO
 

         !PRINT *,'id_stringsize :: ',id_stringsize,ila_dimsize(1)
         IF (id_stringsize .EQ.1 ) THEN
         !----------------------------------------------------------------------
         ! memory allocation
         ALLOCATE(cdpa_value(ila_dimsize(1)),stat=il_status)
         il_status = fi_memError(il_status, ' cdpa_value',cl_fonction)
            il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                             il_var_id, &
                                             cdpa_value,&
                                             start = (/ 1 /),&
                                             count = (/ ila_dimsize(1) /),&
                                             stride =(/ 1 /)),cl_fonction)         
         ELSE
            !----------------------------------------------------------------------
            ! memory allocation
            ALLOCATE(cdpa_value(1),stat=il_status)
            il_status = fi_memError(il_status, ' cdpa_value',cl_fonction)
            il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                             il_var_id, &
                                             cdpa_value),cl_fonction)
         ENDIF

         !----------------------------------------------------------------------
         ! complete the correct lenght of dimensions
         IF (PRESENT(ida_dimsize)) THEN
            ida_dimsize = SHAPE(cdpa_value)
         ENDIF
 
         !----------------------------------------------------------------------
         ! close file
         il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
 
       END SUBROUTINE MIOL_readf_field_C_1D_NC

 !******************************************************************************
  !******************************************************************************
  !******************************************************************************

         !!=====================================================================
         !> \brief
         !! Description: Read the 1D data values from a Netcdf file.
         !!
         !! Use: You have to declarate the array rdpa_value in the main program
         !! as a pointer: CHARACTER(LEN=14), POINTER, DIMENSION(:) :: cdpa_value
         !! @param cd_filename  A NetCDF filename. You must specify the complete path.
         !! @param cd_varname Variable name
         !! @param cdpa_value The data values to be read in char format 
         !! @param id_stringsize size of the character value
         !! @param id_dim dimension of the character value
         !! @param ida_dimsize Returned vector of values corresponding to the length of each dimension     
         !! History :
         !!        \n  09/2006  (F. Messal) F90 
         !!        \n  07/2007  Modification (F. Messal)
         !!        \n  01/2013  (C.REGNIER) SVN V3.5.0
         !!        \n  01/2009  (C.REGNIER) Correction sur la lecture des champs
         !!        \n  03/2009  (C.REGNIER) Modifications
         !<
         !!=====================================================================
 
      SUBROUTINE MIOL_readu_field_C_2D_NC (id_file_id, &   ! Name of the NetCDF file
                                                cd_varname, &    ! Variable name
                                                cdpa_value, &
                                                id_stringsize,&
                                                ida_dimsize)     ! Length of each dimension
 
         USE netcdf
         USE MFT_error
         USE MIOL_param
         IMPLICIT NONE
        
         !----------------------------------------------------------------------
 
         INTEGER, INTENT(IN) :: id_file_id
         CHARACTER(LEN=*), INTENT(IN) :: cd_varname
         CHARACTER(LEN=id_stringsize), DIMENSION(:,:), POINTER :: cdpa_value
         INTEGER, INTENT(IN)                                  :: id_stringsize                    
         INTEGER, INTENT(OUT), DIMENSION(2), OPTIONAL :: ida_dimsize
 
         CHARACTER(LEN=id_stringsize), DIMENSION(:,:), POINTER :: cdpa_value2

         CHARACTER(LEN=255), DIMENSION(2) :: cla_dimname 
         CHARACTER(LEN=255)               :: cl_fonction
         CHARACTER(LEN=100) :: cl_tmp
         INTEGER, DIMENSION(2) :: ila_dimsize, ila_dimids
         INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji
 
         cl_fonction="MIOL_readu_field_C_2D_NC"
         
         !----------------------------------------------------------------------
         ! get variable rank
         il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                               cd_varname, &
                                               il_var_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE_VARIABLE(id_file_id, &
                                                      il_var_id, &
                                                      cl_tmp, &
                                                      il_type, &
                                                      il_nbdim, &
                                                      ila_dimids),cl_fonction)
         IF (il_nbdim.NE.2) THEN
            WRITE(0,*) "MIOL_read_ncvar_field: array dimension error. "
            CALL flush(0)
            STOP
         ENDIF
         
         !** read lenght of dimensions
         DO il_ji = 1, il_nbdim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id,&
                                                          ila_dimids(il_ji), &
                                                          cla_dimname(il_ji), &
                                                          ila_dimsize(il_ji)),cl_fonction)
         ENDDO
         
         IF (id_stringsize .EQ.1 ) THEN
         !----------------------------------------------------------------------
         ! memory allocation
            ALLOCATE(cdpa_value2(ila_dimsize(1),ila_dimsize(2)),stat=il_status)
            ALLOCATE(cdpa_value(ila_dimsize(1),ila_dimsize(2)),stat=il_status)
            il_status = fi_memError(il_status, ' cdpa_value',cl_fonction)
            il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                             il_var_id, &
                                             cdpa_value,&
                                             start = (/ 1,1 /),&
                                             count = (/ ila_dimsize(1),ila_dimsize(2) /),&
                                             stride =(/ 1,1 /)),cl_fonction)         
           !DO il_ji=1,ila_dimsize(2)
           ! cdpa_value(il_ji,:)=cdpa_value2(:,il_ji)
           !ENDDO    
         ELSE
            !----------------------------------------------------------------------
            ! memory allocation
            ALLOCATE(cdpa_value(ila_dimsize(2),1),stat=il_status)
            il_status = fi_memError(il_status, ' cdpa_value',cl_fonction)
            il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                             il_var_id, &
                                             cdpa_value),cl_fonction)
         ENDIF

          
         !----------------------------------------------------------------------
         ! complete the correct lenght of dimensions
         IF (PRESENT(ida_dimsize)) THEN
            ida_dimsize = SHAPE(cdpa_value)
         ENDIF
 
       END SUBROUTINE MIOL_readu_field_C_2D_NC
 
!******************************************************************************
!******************************************************************************
!******************************************************************************
 
         !!=====================================================================
         !> \brief
         !! Description: Read the 1D data values from a Netcdf file.
         !!
         !! Use: You have to declarate the array rdpa_value in the main program
         !! as a pointer: CHARACTER(LEN=14), POINTER, DIMENSION(:) :: cdpa_value
         !!
         !! @param cd_filename  A NetCDF filename. You must specify the complete path.
         !! @param cd_varname Variable name
         !! @param cdpa_value The data values to be read in char format 
         !! @param id_stringsize size of the character value
         !! @param id_dim dimension of the character value
         !! @param ida_dimsize Returned vector of values corresponding to the length of each dimension     
         !! History :
         !!        \n  09/2006  (F. Messal) F90
         !!        \n  07/2007  Modification (F. Messal)
         !!        \n  01/2013  (C.REGNIER) SVN V3.5.0
         !!        \n  01/2009  (C.REGNIER) Correction sur la lecture des champs
         !!        \n  03/2009  (C.REGNIER) Modifications
         !<
         !!=====================================================================
 
      SUBROUTINE MIOL_readf_field_C_2D_NC (cd_filename, &   ! Name of the NetCDF file
                                                cd_varname, &    ! Variable name
                                                cdpa_value, &
                                                id_stringsize,&
                                                ida_dimsize)     ! Length of each dimension
 
         USE netcdf
         USE MFT_error
         USE MIOL_param
         IMPLICIT NONE
        
         !----------------------------------------------------------------------
 
         CHARACTER(LEN=*), INTENT(IN) :: cd_filename
         CHARACTER(LEN=*), INTENT(IN) :: cd_varname
         CHARACTER(LEN=id_stringsize), DIMENSION(:,:), POINTER :: cdpa_value
         INTEGER, INTENT(IN) :: id_stringsize                    
         INTEGER, INTENT(OUT), DIMENSION(2), OPTIONAL :: ida_dimsize
 
         CHARACTER(LEN=255), DIMENSION(2) :: cla_dimname
         CHARACTER(LEN=255) :: cl_fonction
         CHARACTER(LEN=100) :: cl_tmp
         INTEGER, DIMENSION(2) :: ila_dimsize, ila_dimids
         INTEGER :: il_nbdim, il_type, il_file_id, il_var_id, il_status, il_ji
 
         cl_fonction="MIOL_readf_field_C_2D_NC"
         !----------------------------------------------------------------------
         ! get variable rank
         il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                          NF90_NOWRITE, &
                                          il_file_id),cl_fonction)
         il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                               cd_varname, &
                                               il_var_id),cl_fonction)
         il_status = fi_ncError(NF90_INQUIRE_VARIABLE(il_file_id, &
                                                      il_var_id, &
                                                      cl_tmp, &
                                                      il_type, &
                                                      il_nbdim, &
                                                      ila_dimids),cl_fonction)
          IF (il_nbdim.NE.2) THEN
            WRITE(0,*) "MIOL_read_ncvar_field: array dimension error. "
            CALL flush(0)
            STOP
         ENDIF
        
         !----------------------------------------------------------------------
         !** read lenght of dimensions
         DO il_ji = 1, il_nbdim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id,&
                                                          ila_dimids(il_ji), &
                                                          cla_dimname(il_ji), &
                                                          ila_dimsize(il_ji)),cl_fonction)
         ENDDO

         IF (id_stringsize .EQ.1 ) THEN
         !----------------------------------------------------------------------
         ! memory allocation
            ALLOCATE(cdpa_value(ila_dimsize(1),ila_dimsize(2)),stat=il_status)
            il_status = fi_memError(il_status, ' cdpa_value',cl_fonction)
            il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                             il_var_id, &
                                             cdpa_value,&
                                             start = (/ 1,1 /),&
                                             count = (/ ila_dimsize(1),ila_dimsize(2) /),&
                                             stride =(/ 1,1 /)),cl_fonction)              
         ELSE
            !----------------------------------------------------------------------
            ! memory allocation
            ALLOCATE(cdpa_value(ila_dimsize(2),1),stat=il_status)
            il_status = fi_memError(il_status, ' cdpa_value',cl_fonction)
            il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                             il_var_id, &
                                             cdpa_value),cl_fonction)
         ENDIF

         !----------------------------------------------------------------------
         ! complete the correct lenght of dimensions
         IF (PRESENT(ida_dimsize)) THEN
            ida_dimsize = SHAPE(cdpa_value)
         ENDIF
 
         !----------------------------------------------------------------------
         ! close file
         il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
       END SUBROUTINE MIOL_readf_field_C_2D_NC


 !******************************************************************************

         !!=====================================================================
         !> \brief
         !! Description: Read the 1D data values from a Netcdf file.
         !!
         !! Use: You have to declarate the array rdpa_value in the main program
         !! as a pointer: CHARACTER(LEN=14), POINTER, DIMENSION(:) :: cdpa_value
         !! @param cd_filename  A NetCDF filename. You must specify the complete path.
         !! @param cd_varname Variable name
         !! @param cdpa_value The data values to be read in char format 
         !! @param id_stringsize size of the character value
         !! @param id_dim dimension of the character value
         !! @param ida_dimsize Returned vector of values corresponding to the length of each dimension     
         !! History :
         !!        \n  09/2006  (F. Messal) F90 
         !!        \n  07/2007  Modification (F. Messal)
         !!        \n  01/2013  (C.REGNIER) SVN V3.5.0
         !!        \n  01/2009  (C.REGNIER) Correction sur la lecture des champs
         !<
         !!=====================================================================
 
      SUBROUTINE MIOL_readu_field_C_3D_NC (id_file_id, &   ! Name of the NetCDF file
                                                cd_varname, &    ! Variable name
                                                cdpa_value, &
                                                id_stringsize,&
                                                ida_dimsize)     ! Length of each dimension
 
         USE netcdf
         USE MFT_error
         USE MIOL_param
         IMPLICIT NONE
        
         !----------------------------------------------------------------------
 
         INTEGER, INTENT(IN) :: id_file_id
         CHARACTER(LEN=*), INTENT(IN) :: cd_varname
         CHARACTER(LEN=id_stringsize), DIMENSION(:,:,:), POINTER :: cdpa_value
         INTEGER, INTENT(IN)                                  :: id_stringsize                    
         INTEGER, INTENT(OUT), DIMENSION(3), OPTIONAL :: ida_dimsize
 
         CHARACTER(LEN=255), DIMENSION(3) :: cla_dimname
         CHARACTER(LEN=255)               :: cl_fonction
         CHARACTER(LEN=100) :: cl_tmp
         INTEGER, DIMENSION(3) :: ila_dimsize, ila_dimids
         INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji
 
         cl_fonction="MIOL_readu_field_C_3D_NC"
         
         !----------------------------------------------------------------------
         ! get variable rank
         il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                               cd_varname, &
                                               il_var_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE_VARIABLE(id_file_id, &
                                                      il_var_id, &
                                                      cl_tmp, &
                                                      il_type, &
                                                      il_nbdim, &
                                                      ila_dimids),cl_fonction)
 
         IF (il_nbdim.NE.3) THEN
            WRITE(0,*) "MIOL_read_ncvar_field: array dimension error. "
            CALL flush(0)
            STOP
         ENDIF
         
         !** read lenght of dimensions
         DO il_ji = 1, il_nbdim
              il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id,&
                                                            ila_dimids(il_ji), &
                                                            cla_dimname(il_ji), &
                                                            ila_dimsize(il_ji)),cl_fonction)
              
         ENDDO

         IF (id_stringsize .EQ.1 ) THEN
         !----------------------------------------------------------------------
         ! memory allocation
            ALLOCATE(cdpa_value(ila_dimsize(1),ila_dimsize(2),ila_dimsize(3)),stat=il_status)
            il_status = fi_memError(il_status, ' cdpa_value',cl_fonction)
            il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                             il_var_id, &
                                             cdpa_value,&
                                             start = (/ 1,1,1 /),&
                                             count = (/ ila_dimsize(1),ila_dimsize(2),ila_dimsize(3) /),&
                                             stride =(/ 1,1,1 /)),cl_fonction)         
            
         ELSE
            !----------------------------------------------------------------------
            ! memory allocation
             PRINT *,'DIMENSIONS ::',ila_dimsize(3),ila_dimsize(2),ila_dimsize(1)
            ALLOCATE(cdpa_value(ila_dimsize(2),ila_dimsize(3),1),stat=il_status)
            il_status = fi_memError(il_status, ' cdpa_value',cl_fonction)
            il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                             il_var_id, &
                                             cdpa_value),cl_fonction)
         ENDIF

         !----------------------------------------------------------------------
         ! complete the correct lenght of dimensions
         IF (PRESENT(ida_dimsize)) THEN
            ida_dimsize = SHAPE(cdpa_value)
         ENDIF
 
       END SUBROUTINE MIOL_readu_field_C_3D_NC
 
 !******************************************************************************
 !******************************************************************************

         !!=====================================================================
         !> \brief
         !! Description: Read the 1D data values from a Netcdf file.
         !!
         !! Use: You have to declarate the array rdpa_value in the main program
         !! as a pointer: CHARACTER(LEN=14), POINTER, DIMENSION(:) :: cdpa_value
         !! @param cd_filename  A NetCDF filename. You must specify the complete path.
         !! @param cd_varname Variable name
         !! @param cdpa_value The data values to be read in char format 
         !! @param id_stringsize size of the character value
         !! @param id_dim dimension of the character value
         !! @param ida_dimsize Returned vector of values corresponding to the length of each dimension     
         !! History :
         !!        \n  09/2006  (F. Messal) F90 
         !!        \n  07/2007  Modification (F. Messal)
         !!        \n  01/2013  (C.REGNIER) SVN V3.5.0
         !!        \n  01/2009  (C.REGNIER) Correction sur la lecture des champs
         !<
         !!=====================================================================
 
      SUBROUTINE MIOL_readf_field_C_3D_NC (cd_filename, &   ! Name of the NetCDF file
                                                cd_varname, &    ! Variable name
                                                cdpa_value, &
                                                id_stringsize,&
                                                ida_dimsize)     ! Length of each dimension
 
         USE netcdf
         USE MFT_error
         USE MIOL_param
         IMPLICIT NONE
        
         !----------------------------------------------------------------------
 
         CHARACTER(LEN=*), INTENT(IN) :: cd_varname,cd_filename
         CHARACTER(LEN=id_stringsize), DIMENSION(:,:,:), POINTER :: cdpa_value
         INTEGER, INTENT(IN)                                  :: id_stringsize               
         INTEGER, INTENT(OUT), DIMENSION(3), OPTIONAL :: ida_dimsize
 
         CHARACTER(LEN=255), DIMENSION(3) :: cla_dimname
         CHARACTER(LEN=255)               :: cl_fonction
         CHARACTER(LEN=100) :: cl_tmp
         INTEGER, DIMENSION(3) :: ila_dimsize, ila_dimids
         INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji, il_file_id
 
         cl_fonction="MIOL_readf_field_C_3D_NC"
         
         !----------------------------------------------------------------------
         ! get variable rank
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_NOWRITE, &
                                           il_file_id),cl_fonction)

          il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                               cd_varname, &
                                               il_var_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE_VARIABLE(il_file_id, &
                                                      il_var_id, &
                                                      cl_tmp, &
                                                      il_type, &
                                                      il_nbdim, &
                                                      ila_dimids),cl_fonction)
 
         IF (il_nbdim.NE.3) THEN
            WRITE(0,*) "MIOL_read_ncvar_field: array dimension error. "
            CALL flush(0)
            STOP
         ENDIF
         
         !** read lenght of dimensions
         DO il_ji = 1, il_nbdim
              il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id,&
                                                            ila_dimids(il_ji), &
                                                            cla_dimname(il_ji), &
                                                            ila_dimsize(il_ji)),cl_fonction)
              
         ENDDO

         
         IF (id_stringsize .EQ.1 ) THEN
         !----------------------------------------------------------------------
         ! memory allocation
            ALLOCATE(cdpa_value(ila_dimsize(1),ila_dimsize(2),ila_dimsize(3)),stat=il_status)
            il_status = fi_memError(il_status, ' cdpa_value',cl_fonction)
            il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                             il_var_id, &
                                             cdpa_value,&
                                             start = (/ 1,1,1 /),&
                                             count = (/ ila_dimsize(1),ila_dimsize(2),ila_dimsize(3) /),&
                                             stride =(/ 1,1,1 /)),cl_fonction)         
            
         ELSE
            !----------------------------------------------------------------------
            ! memory allocation
!            PRINT *,'Taille ::',ila_dimsize(3),ila_dimsize(2),ila_dimsize(1)
            ALLOCATE(cdpa_value(ila_dimsize(2),ila_dimsize(3),1),stat=il_status)
            il_status = fi_memError(il_status, ' cdpa_value',cl_fonction)
            il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                             il_var_id, &
                                             cdpa_value),cl_fonction)
         ENDIF

 
         !----------------------------------------------------------------------
         ! complete the correct lenght of dimensions
         IF (PRESENT(ida_dimsize)) THEN
            ida_dimsize = SHAPE(cdpa_value)
         ENDIF
         
         il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)

       END SUBROUTINE MIOL_readf_field_C_3D_NC
 
 !******************************************************************************

 !******************************************************************************

         !!=====================================================================
         !> \brief
         !! Description: Read the 1D data values from a Netcdf file.
         !!
         !! Use: You have to declarate the array rdpa_value in the main program
         !! as a pointer: CHARACTER(LEN=14), POINTER, DIMENSION(:) :: cdpa_value
         !! @param cd_filename  A NetCDF filename. You must specify the complete path.
         !! @param cd_varname Variable name
         !! @param cdpa_value The data values to be read in char format 
         !! @param id_stringsize size of the character value
         !! @param id_dim dimension of the character value
         !! @param ida_dimsize Returned vector of values corresponding to the length of each dimension     
         !! History :
         !!        \n  09/2006  (F. Messal) F90 
         !!        \n  07/2007  Modification (F. Messal)
         !!        \n  01/2013  (C.REGNIER) SVN V3.5.0
         !!        \n  01/2009  (C.REGNIER) Correction sur la lecture des champs
         !<
         !!=====================================================================
 
      SUBROUTINE MIOL_readu_field_C_4D_NC (id_file_id, &   ! Name of the NetCDF file
                                                cd_varname, &    ! Variable name
                                                cdpa_value, &
                                                id_stringsize,&
                                                ida_dimsize)     ! Length of each dimension
 
         USE netcdf
         USE MFT_error
         USE MIOL_param
         IMPLICIT NONE
        
         !----------------------------------------------------------------------
 
         INTEGER, INTENT(IN) :: id_file_id
         CHARACTER(LEN=*), INTENT(IN) :: cd_varname
         CHARACTER(LEN=id_stringsize), DIMENSION(:,:,:,:), POINTER :: cdpa_value
         INTEGER, INTENT(IN)                                  :: id_stringsize                   
         INTEGER, INTENT(OUT), DIMENSION(4), OPTIONAL :: ida_dimsize
 
         CHARACTER(LEN=255), DIMENSION(4) :: cla_dimname
         CHARACTER(LEN=255)               :: cl_fonction
         CHARACTER(LEN=100) :: cl_tmp
         INTEGER, DIMENSION(4) :: ila_dimsize, ila_dimids
         INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji
 
         cl_fonction="MIOL_readu_field_C_4D_NC"
         
         !----------------------------------------------------------------------
         ! get variable rank
         il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                               cd_varname, &
                                               il_var_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE_VARIABLE(id_file_id, &
                                                      il_var_id, &
                                                      cl_tmp, &
                                                      il_type, &
                                                      il_nbdim, &
                                                      ila_dimids),cl_fonction)
 
         IF (il_nbdim.NE.4) THEN
            WRITE(0,*) "MIOL_read_ncvar_field: array dimension error. "
            CALL flush(0)
            STOP
         ENDIF
         
         !** read lenght of dimensions
         DO il_ji = 1, il_nbdim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id,&
                                                          ila_dimids(il_ji), &
                                                          cla_dimname(il_ji), &
                                                          ila_dimsize(il_ji)),cl_fonction)
              
         ENDDO
         !
         IF (id_stringsize .EQ.1 ) THEN
         !----------------------------------------------------------------------
         ! memory allocation
            ALLOCATE(cdpa_value(ila_dimsize(1),ila_dimsize(2),ila_dimsize(3),ila_dimsize(4)),stat=il_status)
            il_status = fi_memError(il_status, ' cdpa_value',cl_fonction)
            il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                             il_var_id, &
                                             cdpa_value,&
                                             start = (/ 1,1,1,1 /),&
                                             count = (/ ila_dimsize(1),ila_dimsize(2),ila_dimsize(3),ila_dimsize(4) /),&
                                             stride =(/ 1,1,1,1 /)),cl_fonction)         
            
         ELSE
            !----------------------------------------------------------------------
            ! memory allocation
            ALLOCATE (cdpa_value(ila_dimsize(2),ila_dimsize(3),ila_dimsize(4),1),stat=il_status)
            il_status = fi_memError(il_status, ' cdpa_value',cl_fonction)
            il_status = fi_ncError(NF90_GET_VAR(id_file_id, &
                                             il_var_id, &
                                             cdpa_value),cl_fonction)
         ENDIF

         !----------------------------------------------------------------------
         ! complete the correct lenght of dimensions
         IF (PRESENT(ida_dimsize)) THEN
            ida_dimsize = SHAPE(cdpa_value)
         ENDIF
 
       END SUBROUTINE MIOL_readu_field_C_4D_NC
 
!******************************************************************************
!******************************************************************************
!******************************************************************************
!******************************************************************************

         !!=====================================================================
         !> \brief
         !! Description: Read the 1D data values from a Netcdf file.
         !!
         !! Use: You have to declarate the array rdpa_value in the main program
         !! as a pointer: CHARACTER(LEN=14), POINTER, DIMENSION(:) :: cdpa_value
         !! @param cd_filename  A NetCDF filename. You must specify the complete path.
         !! @param cd_varname Variable name
         !! @param cdpa_value The data values to be read in char format 
         !! @param id_stringsize size of the character value
         !! @param id_dim dimension of the character value
         !! @param ida_dimsize Returned vector of values corresponding to the length of each dimension     
         !! History :
         !!        \n  09/2006  (F. Messal) F90 
         !!        \n  07/2007  Modification (F. Messal)
         !!        \n  01/2013  (C.REGNIER) SVN V3.5.0
         !!        \n  01/2009  (C.REGNIER) Correction sur la lecture des champs
         !<
         !!=====================================================================
 
      SUBROUTINE MIOL_readf_field_C_4D_NC (cd_filename, &   ! Name of the NetCDF file
                                                cd_varname, &    ! Variable name
                                                cdpa_value, &
                                                id_stringsize,&
                                                ida_dimsize)     ! Length of each dimension
 
         USE netcdf
         USE MFT_error
         USE MIOL_param
         IMPLICIT NONE
        
         !----------------------------------------------------------------------
 
         CHARACTER(LEN=*), INTENT(IN) :: cd_varname,cd_filename
         CHARACTER(LEN=id_stringsize), DIMENSION(:,:,:,:), POINTER :: cdpa_value
         INTEGER, INTENT(IN)                                  :: id_stringsize                     
         INTEGER, INTENT(OUT), DIMENSION(4), OPTIONAL :: ida_dimsize
         CHARACTER(LEN=255), DIMENSION(4) :: cla_dimname
         CHARACTER(LEN=255)               :: cl_fonction
         CHARACTER(LEN=100) :: cl_tmp
         INTEGER, DIMENSION(4) :: ila_dimsize, ila_dimids
         INTEGER :: il_nbdim, il_type, il_var_id, il_status, il_ji, il_file_id
 
         cl_fonction="MIOL_readf_field_C_4D_NC"
         
         !----------------------------------------------------------------------
         ! get variable rank
         il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                          NF90_NOWRITE, &
                                          il_file_id),cl_fonction)

         il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                               cd_varname, &
                                               il_var_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE_VARIABLE(il_file_id, &
                                                      il_var_id, &
                                                      cl_tmp, &
                                                      il_type, &
                                                      il_nbdim, &
                                                      ila_dimids),cl_fonction)
 
         IF (il_nbdim.NE.4) THEN
            WRITE(0,*) "MIOL_read_ncvar_field: array dimension error. "
            CALL flush(0)
            STOP
         ENDIF
         
         !** read lenght of dimensions
         DO il_ji = 1, il_nbdim
              il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id,&
                                                          ila_dimids(il_ji), &
                                                          cla_dimname(il_ji), &
                                                          ila_dimsize(il_ji)),cl_fonction)
              
         ENDDO
         !
         IF (id_stringsize .EQ.1 ) THEN
         !----------------------------------------------------------------------
         ! memory allocation
            ALLOCATE(cdpa_value(ila_dimsize(1),ila_dimsize(2),ila_dimsize(3),ila_dimsize(4)),stat=il_status)
            il_status = fi_memError(il_status, ' cdpa_value',cl_fonction)
            il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                             il_var_id, &
                                             cdpa_value,&
                                             start = (/ 1,1,1,1 /),&
                                             count = (/ ila_dimsize(1),ila_dimsize(2),ila_dimsize(3),ila_dimsize(4) /),&
                                             stride =(/ 1,1,1,1 /)),cl_fonction)         
            
         ELSE
            !----------------------------------------------------------------------
            ! memory allocation
            ALLOCATE (cdpa_value(ila_dimsize(2),ila_dimsize(3),ila_dimsize(4),1),stat=il_status)
            il_status = fi_memError(il_status, ' cdpa_value',cl_fonction)
            il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                             il_var_id, &
                                             cdpa_value),cl_fonction)
         ENDIF
 
         !----------------------------------------------------------------------
         ! complete the correct lenght of dimensions
         IF (PRESENT(ida_dimsize)) THEN
            ida_dimsize = SHAPE(cdpa_value)
         ENDIF
     
         il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)

       END SUBROUTINE MIOL_readf_field_C_4D_NC
 
 !******************************************************************************
 !******************************************************************************
 !******************************************************************************
 


 
