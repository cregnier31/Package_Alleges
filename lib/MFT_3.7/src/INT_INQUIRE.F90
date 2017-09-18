!> \brief Module which contain Interfaces for inquiring dimensions of values and variables in NETCDF format
!! \author C.REGNIER Miol V3.5
!! \date September 2008
!!  \version 3.5
!<
MODULE INT_INQUIRE
!
  ! -----------------------------------------------------------------
  ! --- General interface MIOL_inquire_type_NC
  !--- MIOL_inquiref_type_NC(cd_filename,cd_varname,id_type,id_nbdim)
  !--- MIOL_inquireu_type_NC(id_filename,cd_varname,id_type,id_nbdim)
  !
  ! -----------------------------------------------------------------
  ! --- General interface MIOL_inquire_dimensions_NC
  !---  MIOL_inquireu_dimensions_NC (id_file_id,id_nbdim,cdpa_dimname,idpa_dimlen)
  !---  MIOL_inquiref_dimensions_NC(cd_filename,id_nbdim,cdpa_dimname,idpa_dimlen)
  ! -------------  ----------------------------------------------------
  ! --- General interface MIOL_inquire_variables_NC
  !---   MIOL_inquiref_variables_NC (cd_filename,id_nbvar,cdpa_varname,idpa_nbdim,idpa_type)
  !---   MIOL_inquireu_variables_NC (id_file_id,id_nbvar,cdpa_varname,idpa_nbdim,idpa_type) 
  ! -----------------------------------------------------------------
  ! --- General interface MIOL_inquire_attribute_NC
  !---   MIOL_inquireu_attribute_NC (id_file_id,cd_varname,cd_attname,ld_exist,id_type)
  !---   MIOL_inquiref_attribute_NC (cd_filename,cd_varname,cd_attname,ld_exist,id_type)
  ! History
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~é
  ! C.REGNIER 09/2008 Creation 
  ! C.REGNIER 03/2009 Ajout de MIOL_inquire_attribute_NC
  ! C.REGNIER 01/2010 Changement de l'interface
  ! -----------------------------------------------------------------

!> An interface for inquire attribute existence in NETCDF
!!
!<
INTERFACE MIOL_inquire_attribute_NC
module procedure MIOL_inquireu_attribute_NC,&
                 MIOL_inquiref_attribute_NC
END INTERFACE 
!> An interface for inquire type of a variable in NETCDF
!!
!<
INTERFACE MIOL_inquire_type_NC
module procedure MIOL_inquiref_type_NC,&
                 MIOL_inquireu_type_NC
END INTERFACE 
!> An interface for inquire dimensions in NETCDF
!!
!<
INTERFACE MIOL_inquire_dimensions_NC
module procedure MIOL_inquireu_dimensions_NC,&
                 MIOL_inquiref_dimensions_NC
END INTERFACE 
!> An interface for inquire dimensions of a variable in NETCDF
!!
!<
INTERFACE MIOL_inquire_dimensions_var_NC
module procedure MIOL_inquireu_dimensions_var_NC,&
                 MIOL_inquiref_dimensions_var_NC
END INTERFACE 
!> An interface for inquire variable in NETCDF
!!
!<
INTERFACE MIOL_inquire_variables_NC
module procedure MIOL_inquiref_variables_NC,&
                 MIOL_inquireu_variables_NC
END INTERFACE 
!**
CONTAINS
!**
          !!==================================================================================
          !> \brief
          !! Description: This function return the existence of an attribut and his type
          !!
          !!@param id_file_id        A NetCDF ID of the filename
          !!@param cd_varname        Name of the variable
          !!@param cd_attname        Name of the attribut
          !!@paral ld_exist          Logical for the existence of the attribut value
          !!
          !! History :
          !!       \n  03/2008  (C.REGNIER) Creation
          !<
          !!============================================================================
 
       SUBROUTINE MIOL_inquireu_attribute_NC (id_file_id, &
                                              cd_varname,&
                                              cd_attname,&
                                              ld_exist,&
                                              id_type)
                                                
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         INTEGER,INTENT(IN)            :: id_file_id
         CHARACTER(LEN=*),INTENT(IN)   :: cd_varname,cd_attname
         lOGICAL,OPTIONAL,INTENT(OUT)  :: ld_exist
         INTEGER,INTENT(OUT),OPTIONAL  :: id_type
         
         INTEGER                       :: il_status,il_variable_id,il_len
         CHARACTER(255)                :: cl_fonction
!
!*------------------------------------------------------------------------------        
!
          cl_fonction="MIOL_inquireu_attribute_NC"
          il_len=0
          ld_exist=.FALSE.

          IF ((TRIM(cd_varname) .NE. 'global') .AND. &
               (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
             il_status = fi_ncError(NF90_INQ_VARID(id_file_id, &
                                                   TRIM(cd_varname), &
                                                   il_variable_id),cl_fonction)
           il_status=NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                    il_variable_id, &
                                    TRIM(cd_attname), &
                                    id_type,il_len)             
          ELSE
            il_status=NF90_INQUIRE_ATTRIBUTE(id_file_id, &
                                    NF90_GLOBAL, &
                                    TRIM(cd_attname), &
                                    id_type,il_len)
          ENDIF
          
          IF ( id_type /= 0 ) THEN
             ld_exist=.TRUE.
          ELSE
             ld_exist=.FALSE.
          ENDIF
 
        END SUBROUTINE MIOL_inquireu_attribute_NC
!
!*-----------------------------------------------------------------------------------
!
          !!==================================================================================
          !> \brief
          !! Description: This function return the existence of an attribut and his type
          !!
          !!@param cd_filename       A NetCDF filename. You must specify the complete path.
          !!@param cd_varname        Name of the variable
          !!@param cd_attname        Name of the attribut
          !!@paral ld_exist          Logical for the existence of the attribut value
          !!
          !! History :
          !!       \n  03/2008  (C.REGNIER) Creation
          !<
          !!============================================================================
 
       SUBROUTINE MIOL_inquiref_attribute_NC (cd_filename, &
                                              cd_varname,&
                                              cd_attname,&
                                              ld_exist,&
                                              id_type)
                                                
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         CHARACTER(LEN=*),           INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),INTENT(IN)   :: cd_varname,cd_attname
         lOGICAL,OPTIONAL,INTENT(OUT)  :: ld_exist
         INTEGER,INTENT(OUT),OPTIONAL  :: id_type
         
         INTEGER                       :: il_file_id,il_status,il_variable_id,il_len
         CHARACTER(255)                :: cl_fonction

         !
         !*------------------------------------------------------------------------------        
         !
          cl_fonction="MIOL_inquireu_attribute_NC"
          il_len=0
          ld_exist=.FALSE.
          !----------------------------------------------------------------------
          ! Open file
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_NOWRITE, &
                                           il_file_id),cl_fonction)
 
          IF ((TRIM(cd_varname) .NE. 'global') .AND. &
               (TRIM(cd_varname) .NE. 'GLOBAL') ) THEN
             il_status = fi_ncError(NF90_INQ_VARID(il_file_id, &
                                                   TRIM(cd_varname), &
                                                   il_variable_id),cl_fonction)
            il_status=NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                    il_variable_id, &
                                    TRIM(cd_attname), &
                                    id_type,il_len)
          ELSE
            il_status=NF90_INQUIRE_ATTRIBUTE(il_file_id, &
                                    NF90_GLOBAL, &
                                    TRIM(cd_attname), &
                                    id_type,il_len)
          ENDIF
           
          IF ( id_type /= 0 ) THEN
             ld_exist=.TRUE.
          ELSE
             ld_exist=.FALSE.
          ENDIF
         !-----------------------------------------------------------------------
         ! Close file
 
         il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)   

        END SUBROUTINE MIOL_inquiref_attribute_NC
!
!******************************************************************************
!
          !> \brief
          !! Description: This function return the dimensions of a netcdf variable
          !!
          !!@param cd_filename       A NetCDF filename. You must specify the complete path.
          !!@param cd_varname        Name of the variable
          !!@param id_nbdim          Returned the dimension of the variable
          !!@param ld_exist          Return the existence of the variable
          !!
          !! History :
          !!       \n  10/2011  (C.REGNIER) Miol V3.5, creation
          !<
          !!=====================================================================
 
       SUBROUTINE MIOL_inquiref_dimensions_var_NC(cd_filename, &
                                                  cd_varname,&
                                                  id_nbdim,&
                                                  ld_exist)
                                                
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
          CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename,cd_varname
          INTEGER,                          INTENT(OUT) :: id_nbdim
          lOGICAL,                          OPTIONAL,INTENT(OUT)  :: ld_exist
          INTEGER, DIMENSION(:),            POINTER :: idpa_dimlen
          CHARACTER(LEN=255), DIMENSION(:), POINTER :: cdpa_dimname
          CHARACTER(LEN=255), DIMENSION(:), POINTER :: clpa_varname
          INTEGER, DIMENSION(:),            POINTER  :: ilpa_nbdim
          INTEGER :: il_file_id, il_nbdim, il_ji, il_status,il_nbVariable,&
                     il_type,il_nbdim2,il_nbatt
          INTEGER, DIMENSION(4)                         :: ila_dimids
          CHARACTER(255) :: cl_fonction,cl_varname
          LOGICAL :: ll_var
          !----------------------------------------------------------------------
          cl_fonction="MIOL_inquireu_dimensions_var_NC"
 
          !----------------------------------------------------------------------
          ! Open file
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_NOWRITE, &
                                           il_file_id),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Inquire the number of dimensions
          il_status = fi_ncError(NF90_INQUIRE(il_file_id, &
                                              il_nbdim),cl_fonction)
          IF ( PRESENT(ld_exist)) ld_exist=.FALSE.
          ll_var=.FALSE.
          !----------------------------------------------------------------------
          ! Memory allocation
          ALLOCATE(idpa_dimlen(il_nbdim), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' idpa_dimlen',cl_fonction)

          ALLOCATE(cdpa_dimname(il_nbdim), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' cdpa_dimname',cl_fonction)
 
          ! Inquire variables
           CALL MIOL_inquire_variables_NC(il_file_id, &
                                          il_nbVariable, &
                                          clpa_varname, &
                                          ilpa_nbdim)
           IF  (ANY (clpa_varname(:)==TRIM(cd_varname)) ) ll_var=.TRUE.

           IF (ll_var) THEN
              DO il_ji = 1, il_nbVariable !il_nbvar
                 il_status = fi_ncError(NF90_INQUIRE_VARIABLE(il_file_id, &
                                                              il_ji, &
                                                              cl_varname, &
                                                              il_type, &
                                                              il_nbdim2, &
                                                              ila_dimids, &
                                                              il_nbatt),cl_fonction)  
                 
                 IF (TRIM(cl_varname) == TRIM(cd_varname) )THEN
		    id_nbdim=il_nbdim2
                    EXIT
                 ENDIF
              ENDDO
              IF ( PRESENT(ld_exist)) THEN
               ld_exist=ll_var
              ENDIF
 
           ELSE 
                 IF ( PRESENT(ld_exist)) THEN
                    ld_exist=.FALSE.
                 ELSE
                    WRITE (0,*) 'cl_varname en entrée ne fait pas partie du fichier',cd_varname
                    STOP
                 ENDIF   
          ENDIF

          !----------------------------------------------------------------------
          ! Close file
           il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)

 
        END SUBROUTINE MIOL_inquiref_dimensions_var_NC

!******************************************************************************
!******************************************************************************

          !!=====================================================================
          !> \brief
          !! Description: This function return the dimensions of a netcdf variable
          !!
          !!
          !!@param id_file_id        Id filename
          !!@param cd_varname        Name of the variable
          !!@param id_nbdim          Returned the dimension of the variable
          !!@param ld_exist          Return the existence of the variable
          !!
          !! History :
          !!       \n  11/2008  (C.REGNIER) Miol V3.5, creation
          !<
          !!=====================================================================
 
       SUBROUTINE MIOL_inquireu_dimensions_var_NC (id_file_id, &
                                                   cd_varname,&
					           id_nbdim,&
                                                   ld_exist)
                                                
          USE netcdf
          USE MFT_error
          IMPLICIT NONE
          INTEGER,                          INTENT(IN) ::id_file_id
          CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
          INTEGER,                         INTENT(OUT) :: id_nbdim
          lOGICAL,                         OPTIONAL,INTENT(OUT) :: ld_exist
          INTEGER, DIMENSION(:),            POINTER :: idpa_dimlen
          CHARACTER(LEN=255), DIMENSION(:), POINTER :: cdpa_dimname
          CHARACTER(LEN=255), DIMENSION(:), POINTER :: clpa_varname
          INTEGER, DIMENSION(:),            POINTER  :: ilpa_nbdim
          INTEGER :: il_nbdim, il_ji, il_status,il_nbVariable,&
                     il_type,il_nbdim2,il_nbatt
          INTEGER, DIMENSION(4)                         :: ila_dimids
          CHARACTER(255) :: cl_fonction,cl_varname
          LOGICAL :: ll_var
          !----------------------------------------------------------------------

          cl_fonction="MIOL_inquireu_type_NC"

          !----------------------------------------------------------------------
          ! Inquire the number of dimensions
          il_status = fi_ncError(NF90_INQUIRE(id_file_id, &
                                              il_nbdim),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Memory allocation
          ALLOCATE(idpa_dimlen(il_nbdim), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' idpa_dimlen',cl_fonction)

          ALLOCATE(cdpa_dimname(il_nbdim), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' cdpa_dimname',cl_fonction)
          ll_var=.FALSE.
          IF ( PRESENT(ld_exist)) ld_exist=.FALSE.

          ! Inquire variables
           CALL MIOL_inquire_variables_NC(id_file_id, &
                                          il_nbVariable, &
                                          clpa_varname, &
                                          ilpa_nbdim)
					  
           IF  (ANY (clpa_varname(:)==TRIM(cd_varname)) ) ll_var=.TRUE.
           IF (ll_var) THEN
              DO il_ji = 1, il_nbVariable !il_nbvar
                 il_status = fi_ncError(NF90_INQUIRE_VARIABLE(id_file_id, &
                                        il_ji, &
                                        cl_varname, &
                                        il_type, &
                                        il_nbdim2, &
                                        ila_dimids, &
                                        il_nbatt),cl_fonction)  
                 
                 IF (TRIM(cl_varname) == TRIM(cd_varname) )THEN
		    id_nbdim=il_nbdim2
                    EXIT
                 ENDIF
              ENDDO
              IF ( PRESENT(ld_exist)) THEN
                ld_exist=ll_var
              ENDIF
          ELSE 
              IF ( PRESENT(ld_exist)) THEN
                  ld_exist=.FALSE.
              ELSE      
                  WRITE (0,*) 'cl_varname en entrée ne fait pas partie du fichier ',cd_varname
                  STOP 
              ENDIF
          ENDIF
 
        END SUBROUTINE MIOL_inquireu_dimensions_var_NC

!
!******************************************************************************
!
         !> \brief
          !! Description: This function return the type of an argument
          !!              in netcdf format
          !!
          !!@param cd_filename       A NetCDF filename. You must specify the complete path.
          !!@param cd_varname        Name of the variable
          !!@param id_type           Type of the variable :: FLOAT,DOUBLE,INT... 
          !!@param id_nbdim          Returned the dimension of the variable
          !!@param ld_exist          Return the existence of the variable
          !!
          !! History :
          !!       \n  11/2008  (C.REGNIER) Miol V3.5, creation
          !<
          !!=====================================================================
 
       SUBROUTINE MIOL_inquiref_type_NC (cd_filename, &
                                         cd_varname,&
                                         id_type,&
					 id_nbdim,&
                                         ld_exist)
                                                
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
          CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename,cd_varname
          INTEGER,                          INTENT(OUT) :: id_type,id_nbdim
          lOGICAL,                          OPTIONAL,INTENT(OUT)  :: ld_exist
          INTEGER, DIMENSION(:),            POINTER :: idpa_dimlen
          CHARACTER(LEN=255), DIMENSION(:), POINTER :: cdpa_dimname
          CHARACTER(LEN=255), DIMENSION(:), POINTER :: clpa_varname
          INTEGER, DIMENSION(:),            POINTER  :: ilpa_nbdim
          INTEGER :: il_file_id, il_nbdim, il_ji, il_status,il_nbVariable,&
                     il_type,il_nbdim2,il_nbatt
          INTEGER, DIMENSION(4)                         :: ila_dimids
          CHARACTER(255) :: cl_fonction,cl_varname
          LOGICAL :: ll_var
          cl_fonction="MIOL_inquiref_type_NC"
 
          !----------------------------------------------------------------------
          ! Open file
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_NOWRITE, &
                                           il_file_id),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Inquire the number of dimensions
          il_status = fi_ncError(NF90_INQUIRE(il_file_id, &
                                              il_nbdim),cl_fonction)
          IF ( PRESENT(ld_exist)) ld_exist=.FALSE.
          ll_var=.FALSE.
          !----------------------------------------------------------------------
          ! Memory allocation
          ALLOCATE(idpa_dimlen(il_nbdim), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' idpa_dimlen',cl_fonction)

          ALLOCATE(cdpa_dimname(il_nbdim), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' cdpa_dimname',cl_fonction)
 
          ! Inquire variables
           CALL MIOL_inquire_variables_NC(il_file_id, &
                                          il_nbVariable, &
                                          clpa_varname, &
                                          ilpa_nbdim)
           IF  (ANY (clpa_varname(:)==TRIM(cd_varname)) ) ll_var=.TRUE.

           IF (ll_var) THEN
              DO il_ji = 1, il_nbVariable !il_nbvar
                 il_status = fi_ncError(NF90_INQUIRE_VARIABLE(il_file_id, &
                                                              il_ji, &
                                                              cl_varname, &
                                                              il_type, &
                                                              il_nbdim2, &
                                                              ila_dimids, &
                                                              il_nbatt),cl_fonction)  
                 
                 IF (TRIM(cl_varname) == TRIM(cd_varname) )THEN
                    id_type=il_type
		    id_nbdim=il_nbdim2
                    EXIT
                 ENDIF
              ENDDO
              IF ( PRESENT(ld_exist)) THEN
               ld_exist=ll_var
              ENDIF
 
           ELSE 
                 IF ( PRESENT(ld_exist)) THEN
                    ld_exist=.FALSE.
                 ELSE
                    WRITE (0,*) 'cl_varname en entrée ne fait pas partie du fichier ',cd_varname
                    STOP
                 ENDIF   
          ENDIF

          !----------------------------------------------------------------------
          ! Close file
           il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)

 
        END SUBROUTINE MIOL_inquiref_type_NC

!******************************************************************************
!******************************************************************************

          !!=====================================================================
          !> \brief
          !! Description: This function return the type of an argument
          !!              in netcdf format
          !!
          !!
          !!@param id_file_id        Id filename
          !!@param cd_varname        Name of the variable
          !!@param id_type           Type of the variable :: FLOAT,DOUBLE,INT... 
          !!@param id_nbdim          Returned the dimension of the variable
          !!@param ld_exist          Return the existence of the variable
          !!
          !! History :
          !!       \n  11/2008  (C.REGNIER) Miol V3.5, creation
          !<
          !!=====================================================================
 
       SUBROUTINE MIOL_inquireu_type_NC (id_file_id, &
                                         cd_varname,&
                                         id_type,&
					 id_nbdim,&
                                         ld_exist)
                                                
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
         INTEGER,                          INTENT(IN) ::id_file_id
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
          INTEGER,                         INTENT(OUT) :: id_type,id_nbdim
          lOGICAL,                         OPTIONAL,INTENT(OUT) :: ld_exist
          INTEGER, DIMENSION(:),            POINTER :: idpa_dimlen
          CHARACTER(LEN=255), DIMENSION(:), POINTER :: cdpa_dimname
          CHARACTER(LEN=255), DIMENSION(:), POINTER :: clpa_varname
          INTEGER, DIMENSION(:),            POINTER  :: ilpa_nbdim
          INTEGER :: il_nbdim, il_ji, il_status,il_nbVariable,&
                     il_type,il_nbdim2,il_nbatt
          INTEGER, DIMENSION(4)                         :: ila_dimids
          CHARACTER(255) :: cl_fonction,cl_varname
          LOGICAL :: ll_var
          
          cl_fonction="MIOL_inquireu_type_NC"

          !----------------------------------------------------------------------
          ! Inquire the number of dimensions
          il_status = fi_ncError(NF90_INQUIRE(id_file_id, &
                                              il_nbdim),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Memory allocation
          ALLOCATE(idpa_dimlen(il_nbdim), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' idpa_dimlen',cl_fonction)

          ALLOCATE(cdpa_dimname(il_nbdim), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' cdpa_dimname',cl_fonction)
          ll_var=.FALSE.
          IF ( PRESENT(ld_exist)) ld_exist=.FALSE.

          ! Inquire variables
           CALL MIOL_inquire_variables_NC(id_file_id, &
                                          il_nbVariable, &
                                          clpa_varname, &
                                          ilpa_nbdim)
					  
           IF  (ANY (clpa_varname(:)==TRIM(cd_varname)) ) ll_var=.TRUE.
           IF (ll_var) THEN
              DO il_ji = 1, il_nbVariable !il_nbvar
                 il_status = fi_ncError(NF90_INQUIRE_VARIABLE(id_file_id, &
                                        il_ji, &
                                        cl_varname, &
                                        il_type, &
                                        il_nbdim2, &
                                        ila_dimids, &
                                        il_nbatt),cl_fonction)  
                 
                 IF (TRIM(cl_varname) == TRIM(cd_varname) )THEN
                    id_type=il_type
		    id_nbdim=il_nbdim2
                    EXIT
                 ENDIF
            ENDDO
                IF ( PRESENT(ld_exist)) THEN
                   ld_exist=ll_var
                ENDIF
          ELSE 
              IF ( PRESENT(ld_exist)) THEN
                  ld_exist=.FALSE.
              ELSE      
                  WRITE (0,*) 'cl_varname en entrée ne fait pas partie du fichier ',cd_varname
                  STOP
              ENDIF
          ENDIF
 
        END SUBROUTINE MIOL_inquireu_type_NC

!******************************************************************************
!******************************************************************************

          !!=====================================================================
          !> \brief
          !! Description: This function reads the number, the name and the length
          !!              of a NetCDF file dimensions. This function uses pointers,
          !!              so you don’t have to know the correct dimensions of the
          !!              data arrays.
          !!
          !!@param cd_filename       A NetCDF filename. You must specify the complete path.
          !!@param il_nbdim          Returned number of dimensions.
          !!@param cdpa_dimname      Returned vector of nbdim names corresponding to
          !!                        the name of the dimensions.
          !!@param idpa_dimlen       Returned vector of nbdim values corresponding to the
          !!                     length of the dimensions.
          !!
          !! History :
          !!       \n  11/2006  (F. Messal)
          !!       \n  01/2013  (C.REGNIER) Miol V3.5, add error function
          !<
          !!=====================================================================
 
       SUBROUTINE MIOL_inquiref_dimensions_NC (cd_filename, &
                                               id_nbdim, &
                                               cdpa_dimname, &
                                               idpa_dimlen)
 
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
          CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
          INTEGER,                          INTENT(OUT) :: id_nbdim
          INTEGER, DIMENSION(:),            POINTER  :: idpa_dimlen
          CHARACTER(LEN=255), DIMENSION(:), POINTER  :: cdpa_dimname
 
          INTEGER :: il_file_id, il_nbdim, il_ji, il_status
          CHARACTER(255) :: cl_fonction
          
          cl_fonction="MIOL_inquiref_dimensions_NC"
 
          !----------------------------------------------------------------------
          ! Open file
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_NOWRITE, &
                                           il_file_id),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Inquire the number of dimensions
          il_status = fi_ncError(NF90_INQUIRE(il_file_id, &
                                              il_nbdim),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Memory allocation
          ALLOCATE(idpa_dimlen(il_nbdim), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' idpa_dimlen',cl_fonction)

          ALLOCATE(cdpa_dimname(il_nbdim), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' cdpa_dimname',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read dimension
          DO il_ji = 1, il_nbdim
             il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                           il_ji, &
                                                           cdpa_dimname(il_ji), &
                                                           idpa_dimlen(il_ji)),cl_fonction)
          ENDDO
          id_nbdim = il_nbdim
 
          !----------------------------------------------------------------------
          ! Close file
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
 
        END SUBROUTINE MIOL_inquiref_dimensions_NC
 

 !******************************************************************************
!******************************************************************************
 
          !!=====================================================================
          !> \brief
          !! Description: This function reads the number, the name and the length
          !!              of a NetCDF file dimensions. This function uses pointers,
          !!              so you don’t have to know the correct dimensions of the
          !!              data arrays.
          !!
          !! @param id_file_id        A NetCDF file Id.
          !! @param il_nbdim          Returned number of dimensions.
          !! @param cdpa_dimname      Returned vector of nbdim names corresponding to
          !!                     the name of the dimensions.
          !! @param idpa_dimlen       Returned vector of nbdim values corresponding to the
          !!                     length of the dimensions.
          !!
          !! History :
          !!        \n  11/2006  (F. Messal)
          !!	    \n  01/2013  (C.REGNIER) MIOL V3.5, add functions error
	  !<
          !!=====================================================================
 
       SUBROUTINE MIOL_inquireu_dimensions_NC (id_file_id, &
                                               id_nbdim, &
                                               cdpa_dimname, &
                                               idpa_dimlen)
 
          USE netcdf
          USE MFT_error
	  IMPLICIT NONE
 
          !----------------------------------------------------------------------
 
          INTEGER,                          INTENT(IN) :: id_file_id
          INTEGER,                          INTENT(OUT) :: id_nbdim
          INTEGER, DIMENSION(:),            POINTER :: idpa_dimlen
          CHARACTER(LEN=255), DIMENSION(:), POINTER :: cdpa_dimname
 
          INTEGER :: il_nbdim, il_ji, il_status
          CHARACTER(255) :: cl_fonction
          
          cl_fonction="MIOL_inquireu_dimensions_NC"
 
          !----------------------------------------------------------------------
          ! Inquire the number of dimensions
          il_status = fi_ncError(NF90_INQUIRE(id_file_id, &
                                              il_nbdim),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Memory allocation
          ALLOCATE(idpa_dimlen(il_nbdim), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' idpa_dimlen',cl_fonction)
 
          ALLOCATE(cdpa_dimname(il_nbdim), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' cdpa_dimname',cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Read dimension
          DO il_ji = 1, il_nbdim
             il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id, &
                                                           il_ji, &
                                                           cdpa_dimname(il_ji), &
                                                           idpa_dimlen(il_ji)),cl_fonction)
          ENDDO
          id_nbdim = il_nbdim
 
        END SUBROUTINE MIOL_inquireu_dimensions_NC



!Source src_INQUIRE_var
 
          !!=====================================================================
          !> \brief
          !! Description: This function reads the number, the name and the length
          !!              of a NetCDF file variables. This function uses pointers,
          !!              so you don’t have to know the correct dimensions of the
          !!              data arrays.
          !!              If you use also the netcdf library, you can compare
          !!              “vartype” to the set of predefined netCDF external data
          !!              types: NF90_BYTE, NF90_CHAR, NF90_SHORT, NF90_INT,
          !!              NF90_FLOAT, NF90_DOUBLE.
          !!
          !! @param cd_filename       A NetCDF filename. You must specify the complete path.
          !! @param id_nbvar          Returned number of variables.
          !! @param cdpa_varname      Returned vector of nbvar names corresponding
          !!                          to the name of each variable.
          !! @param idpa_nbdim        Returned vector of nbvar values corresponding
          !!                          to the number of dimensions (2 for a matrix,
          !!                          1 for a vector, 0 for a scalar...)
          !! @param idpa_type         Returned vector of nbvar values corresponding to
          !!                          the type of each variable: BYTE(1), CHAR(2),
          !!                         SHORT(3), INT(4), FLOAT(5), DOUBLE(6).
          !!
          !! \n History :
          !!        \n  11/2006  (F. Messal)
          !!        \n  01/2013 (C.REGNIER) MIOL V3.5 add functions errors
          !<
	  !!=====================================================================
 
       SUBROUTINE MIOL_inquiref_variables_NC (cd_filename, &
                                              id_nbvar, &
                                              cdpa_varname, &
                                              idpa_nbdim, &
                                              idpa_type)
 
          USE netcdf
          USE MFT_error
          IMPLICIT NONE
 
 
 
          !----------------------------------------------------------------------
 
          CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
          INTEGER,                          INTENT(OUT) :: id_nbvar
          CHARACTER(LEN=255), DIMENSION(:), POINTER :: cdpa_varname
          INTEGER, DIMENSION(:),            POINTER :: idpa_nbdim
          INTEGER, DIMENSION(:),OPTIONAL,   POINTER :: idpa_type
 
          INTEGER :: il_file_id, il_nbvar, il_ji, il_nbdim, il_tmp, il_status
          CHARACTER(LEN=255)  :: cl_fonction

          cl_fonction="MIOL_inquiref_variables_NC"
 
          !----------------------------------------------------------------------
          ! Open file
 
          il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                           NF90_NOWRITE, &
                                           il_file_id),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Inquire dimensions
 
          il_status = fi_ncError(NF90_INQUIRE(il_file_id, &
                                              il_nbdim, &
                                              il_nbvar),cl_fonction)
 
 
          !----------------------------------------------------------------------
          ! Memory allocation
 
          ALLOCATE(cdpa_varname(il_nbvar), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' cdpa_varname',cl_fonction)
 
          ALLOCATE(idpa_nbdim(il_nbvar), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' idpa_nbdim',cl_fonction)
 
          IF (PRESENT(idpa_type)) THEN
             ALLOCATE(idpa_type(il_nbvar), &
                      stat=il_status)
             il_status = fi_memError(il_status, 'idpa_type',cl_fonction)
 
          ENDIF
 
 
          !----------------------------------------------------------------------
          ! Read variable
 
          DO il_ji = 1, il_nbvar
 
             IF (PRESENT(idpa_type)) THEN
                il_status = fi_ncError(NF90_INQUIRE_VARIABLE(il_file_id, &
                                                             il_ji, &
                                                             cdpa_varname(il_ji), &
                                                             idpa_type(il_ji), &
                                                             idpa_nbdim(il_ji)),cl_fonction)
             ELSE
                il_status = fi_ncError(NF90_INQUIRE_VARIABLE(il_file_id, &
                                                             il_ji, &
                                                             cdpa_varname(il_ji), &
                                                             il_tmp, &
                                                             idpa_nbdim(il_ji)),cl_fonction)
             ENDIF
 
          ENDDO
          id_nbvar = il_nbvar
 
          !----------------------------------------------------------------------
          ! Close file
 
          il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
         END SUBROUTINE MIOL_inquiref_variables_NC
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
          !!=====================================================================
          !> \brief
          !! Description: This function reads the number, the name and the length
          !!              of a NetCDF file variables. This function uses pointers,
          !!              so you don’t have to know the correct dimensions of the
          !!              data arrays.
          !!              If you use also the netcdf library, you can compare
          !!              “vartype” to the set of predefined netCDF external data
          !!              types: NF90_BYTE, NF90_CHAR, NF90_SHORT, NF90_INT,
          !!              NF90_FLOAT, NF90_DOUBLE.
          !!
          !! @param id_file_id        A NetCDF file Id.
          !! @param id_nbvar          Returned number of variables.
          !! @param cdpa_varname      Returned vector of nbvar names corresponding
          !!                          to the name of each variable.
          !! @param idpa_nbdim        Returned vector of nbvar values corresponding
          !!                          to the number of dimensions (2 for a matrix,
          !!                           1 for a vector, 0 for a scalar...)
          !! @param idpa_type         Returned vector of nbvar values corresponding to
          !!                   the type of each variable: BYTE(1), CHAR(2),
          !!                   SHORT(3), INT(4), FLOAT(5), DOUBLE(6).
          !!
          !! History :
          !!        \n  11/2006  (F. Messal)
          !!        \n  01/2013  (C.REGNIER) MIOL V3.5 Add functions error
	  !<
          !!=====================================================================
 
       SUBROUTINE MIOL_inquireu_variables_NC (id_file_id, &
                                              id_nbvar, &
                                              cdpa_varname, &
                                              idpa_nbdim, &
                                              idpa_type)
 
          USE netcdf
          USE MFT_error
          IMPLICIT NONE
 
          !----------------------------------------------------------------------
 
          INTEGER,                          INTENT(IN) :: id_file_id
          INTEGER,                          INTENT(OUT) :: id_nbvar
          CHARACTER(LEN=255), DIMENSION(:), POINTER :: cdpa_varname
          INTEGER, DIMENSION(:),            POINTER :: idpa_nbdim
          INTEGER, DIMENSION(:), OPTIONAL,  POINTER :: idpa_type
 
          INTEGER :: il_nbvar, il_ji, il_nbdim, il_tmp, il_status
          CHARACTER(LEN=255)  :: cl_fonction

          cl_fonction="MIOL_inquireu_variables_NC"
 
 
          !----------------------------------------------------------------------
          ! Inquire dimensions
 
          il_status = fi_ncError(NF90_INQUIRE(id_file_id, &
                                              il_nbdim, &
                                              il_nbvar),cl_fonction)
 
          !----------------------------------------------------------------------
          ! Memory allocation
 
          ALLOCATE(cdpa_varname(il_nbvar), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' cdpa_varname',cl_fonction)
 
          ALLOCATE(idpa_nbdim(il_nbvar), &
                   stat=il_status)
          il_status = fi_memError(il_status, ' idpa_nbdim',cl_fonction)
 
          IF (PRESENT(idpa_type)) THEN
             ALLOCATE(idpa_type(il_nbvar), &
                      stat=il_status)
             il_status = fi_memError(il_status, 'idpa_type',cl_fonction)
 
          ENDIF
 
          !----------------------------------------------------------------------
          ! Read variable
 
          DO il_ji = 1, il_nbvar
 
             IF (PRESENT(idpa_type)) THEN
                il_status = fi_ncError(NF90_INQUIRE_VARIABLE(id_file_id, &
                                                             il_ji, &
                                                             cdpa_varname(il_ji), &
                                                             idpa_type(il_ji), &
                                                             idpa_nbdim(il_ji)),cl_fonction)
             ELSE
                il_status = fi_ncError(NF90_INQUIRE_VARIABLE(id_file_id, &
                                                             il_ji, &
                                                             cdpa_varname(il_ji), &
                                                             il_tmp, &
                                                             idpa_nbdim(il_ji)),cl_fonction)
            ENDIF
 
          ENDDO
          id_nbvar = il_nbvar
       
         END SUBROUTINE MIOL_inquireu_variables_NC
 
 
  !******************************************************************************

 
END MODULE INT_INQUIRE
