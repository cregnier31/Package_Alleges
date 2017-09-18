!> \brief Module which contain subroutine for WRITE Byte values in NETCDF format
!! \n  Total 8 functions
!! \author F.MESSAL first version
!! \date 07/2007
!!  \version 1.1 
!! \author C.REGNIER Miol V3.5
!! \date 01/2013
!!  \version 3.5  
!<
!Source src_write_B
!**-----------------------------------------
!** Module for Write Byte values in NETCDF format
!** C.REGNIER Janvier 2013 V3.5 Miol 
!**-------------------------------------------
! --------------------------------------- General interface --------------------------------------------------------------
! -- SUBROUTINE MIOL_writef_field_B_4D_NC (cd_filename,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
! -- SUBROUTINE MIOL_writef_field_B_3D_NC (cd_filename,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
! -- SUBROUTINE MIOL_writef_field_B_2D_NC (cd_filename,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
! -- SUBROUTINE MIOL_writef_field_B_1D_NC (cd_filename,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
! -- SUBROUTINE MIOL_writeu_field_B_4D_NC (id_file_id,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
! -- SUBROUTINE MIOL_writeu_field_B_3D_NC (id_file_id,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
! -- SUBROUTINE MIOL_writeu_field_B_2D_NC (id_file_id,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
! -- SUBROUTINE MIOL_writeu_field_B_1D_NC (id_file_id,cd_varname,cd_key,ida_varvalue,rda_offsetvalue,ida_specialvalue)
!**-------------------------------------------
!******************************************************************************
 
 
         !!======================================================================
         !> \brief
         !!
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param cd_filename         A NetCDF filename. You must specify the complete
         !!                       path.
         !! @param cd_varname          The variable name.
         !! @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param ida_varvalue        The data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values 
         !! @param ida_specialvalue    Vector of special values of the variable:
         !!                                      ida_specialvalue(1) = minvalue
         !!                                      ida_specialvalue(2) = maxvalue
         !!                                      ida_specialvalue(3) = fillvalue
         !!                                      ida_specialvalue(4) = missvalue
         !!
         !! History :
         !!        \n  07/2007  (F. Messal) Creation
         !!        \n  09/2007  (F. Messal) CVS version 1.1
         !!        \n  01/2013  (C.REGNIER) SVn Version 3.5
         !<
         !!======================================================================

       SUBROUTINE MIOL_writef_field_B_4D_NC (cd_filename, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)
 
         USE netcdf
         
         USE MIOL_param
         USE MFT_error
         IMPLICIT NONE
  
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),            INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
         CHARACTER(LEN=4),            INTENT(IN) :: cd_key
         INTEGER(KIND=1), DIMENSION(:,:,:,:), INTENT(IN) :: ida_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         INTEGER(KIND=1), DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_file_id, il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         INTEGER(KIND=1) :: il_fillvalue2, il_missvalue, il_minvalue, il_maxvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue
         CHARACTER(LEN=255) :: cl_attvalue,cl_fonction
         CHARACTER(LEN=18) :: cl_attname
         INTEGER :: il_ji, il_jj, il_jk, il_status, il_kindofdim
         LOGICAL :: ll_tri

 
         !-----------------------------------------------------------------------
         NAMELIST /miolParameterPaths/ cl_equivalencesPath, &
                                         cl_globalAttributesPath, &
                                         cl_variableAttributesPath, &
                                         cl_dimensionsPath
         NAMELIST /miolParameterUserPaths/ cl_equivalencesPath, &
                                             cl_globalAttributesPath, &
                                             cl_variableAttributesPath, &
                                             cl_dimensionsPath
         NAMELIST /nb_dim/il_kindofdim
         NAMELIST /nb_att/ il_nbatt,& 
                           cl_varname
         

         cl_fonction="MIOL_writef_field_B_4D_NC"
         IF (cl_miolUserParameterFile /= '') THEN 
            OPEN(20, file=cl_miolUserParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterUserPaths)
            CLOSE(20)
         ELSE
            OPEN(20, file=cp_miolParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterPaths)
            CLOSE(20)
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Initialization
 
         cl_varfile = TRIM(cl_variableAttributesPath)//TRIM(cd_varname)//'.in'
         il_minvalue = -99
         il_maxvalue = 99
         il_fillvalue2 = 99
         il_missvalue = 99
 
 
         !-----------------------------------------------------------------------
         ! Read dimensions
 
         il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                          NF90_WRITE, &
                                          il_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_REDEF(il_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE(il_file_id, &
                                             il_nbfiledim),cl_fonction)
 
         ALLOCATE(ila_filedimlen(il_nbfiledim), stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         ALLOCATE(cla_filedimname(il_nbfiledim), stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         DO il_ji = 1, il_nbfiledim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                          il_ji, &
                                                          cla_filedimname(il_ji), &
                                                          ila_filedimlen(il_ji)),cl_fonction)
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find dimensions
 
         ALLOCATE(ila_dimsout_id(4), stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
 
                     IF (ila_filedimlen(il_jk).NE.SIZE(ida_varvalue, dim=il_ji)) THEN
                        WRITE(0,*) ' MIOL_write_field_B_4D_NC : dimensions error. '
                        CALL flush(0)
                        STOP
                     ENDIF
 
                  ENDIF
 
               ENDDO
 
            ENDDO
 
            CLOSE(20)
 
            DEALLOCATE(cla_dimname, stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find the offset values

          IF (PRESENT(rda_offsetvalue)) THEN
            rl_scalevalue = rda_offsetvalue(1)
            rl_offsetvalue = rda_offsetvalue(2)
   	  ELSE
            rl_scalevalue  = 1
            rl_offsetvalue = 0
 	  ENDIF 

         !-----------------------------------------------------------------------
         ! Find the minimum and maximum values 
 
         IF (.NOT.(PRESENT(ida_specialvalue))) THEN
            il_minvalue = MINVAL(ida_varvalue)
            il_maxvalue = MAXVAL(array=ida_varvalue, &
                                      mask=ida_varvalue .NE. il_fillvalue2)
         ELSE
            il_minvalue = ida_specialvalue(1)
            il_maxvalue = ida_specialvalue(2)
            il_fillvalue2 = ida_specialvalue(3)
            il_missvalue = ida_specialvalue(4)
         ENDIF
 
 
         !------------------------------------------------------------------------
         ! Write variable attributes
         INQUIRE(FILE=cl_varfile,EXIST=ll_tri)
         !** Si le fichier.in existe on prend la namelist sinon creation d'un fichier par defaut

         IF (ll_tri) THEN
            OPEN(20, file=cl_varfile, status='old', form='formatted')
            READ(20, nb_att)
         !-----------------------------------------------------------------------
         ! Define the variable
 
         il_status = fi_ncError(NF90_DEF_VAR(il_file_id, &
                                             cl_varname, &
                                             NF90_BYTE, &
                                             ila_dimsout_id, &
                                             il_varin_id),cl_fonction)
 
        
 
         DO il_ji = 1, il_nbatt
 
            READ(20, *) cl_attname
            READ(20, '(A100)') cl_attvalue
 
            SELECTCASE (cl_attvalue)
 
               CASE ('minvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_minvalue),cl_fonction)
 
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_maxvalue),cl_fonction)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_fillvalue2),cl_fonction)
 
               
               CASE ('scalevalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rl_scalevalue),cl_fonction) 
 
               CASE ('offsetvalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rl_offsetvalue),cl_fonction) 
 
               CASE ('missvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_missvalue),cl_fonction)
 
               CASE DEFAULT
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      TRIM(cl_attvalue)),cl_fonction)
 
            ENDSELECT
 
         ENDDO
 
         CLOSE(20)
         ELSE
            il_status = fi_ncError(NF90_DEF_VAR(il_file_id, &
                                                cd_varname, &
                                                NF90_BYTE, &
                                                ila_dimsout_id, &
                                                il_varin_id),cl_fonction)
 
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(il_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Put array
 
         il_status = fi_ncError(NF90_PUT_VAR(il_file_id, &
                                             il_varin_id, &
                                             ida_varvalue),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Close file
 
         il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Memory deallocation
 
         IF (ALLOCATED(cla_filedimname)) DEALLOCATE (cla_filedimname, stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         IF (ALLOCATED(ila_filedimlen)) DEALLOCATE (ila_filedimlen, stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         IF (ALLOCATED(ila_dimsout_id)) DEALLOCATE (ila_dimsout_id, stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
 
       END SUBROUTINE MIOL_writef_field_B_4D_NC
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
         !!======================================================================
         !> \brief
         !!
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param cd_filename         A NetCDF filename. You must specify the complete
         !!                       path.
         !! @param cd_varname          The variable name.
         !! @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param ida_varvalue        The data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values 
         !! @param ida_specialvalue    Vector of special values of the variable:
         !!                                      ida_specialvalue(1) = minvalue
         !!                                      ida_specialvalue(2) = maxvalue
         !!                                      ida_specialvalue(3) = fillvalue
         !!                                      ida_specialvalue(4) = missvalue
         !!
         !! History :
         !!        \n  07/2007  (F. Messal) Creation
         !!        \n  09/2007  (F. Messal) CVS version 1.1
         !!        \n  01/2013  (C.REGNIER) SVn Version 3.5
         !<
         !!======================================================================

       SUBROUTINE MIOL_writef_field_B_3D_NC (cd_filename, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)
 
         USE netcdf
         
         USE MIOL_param
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),            INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
         CHARACTER(LEN=3),            INTENT(IN) :: cd_key
         INTEGER(KIND=1), DIMENSION(:,:,:), INTENT(IN) :: ida_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         INTEGER(KIND=1), DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_file_id, il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         INTEGER(KIND=1) :: il_fillvalue2, il_missvalue, il_minvalue, il_maxvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue
         CHARACTER(LEN=255) :: cl_attvalue,cl_fonction
         CHARACTER(LEN=18) :: cl_attname
         INTEGER :: il_ji, il_jj, il_jk, il_status, il_kindofdim
         LOGICAL :: ll_tri


         !-----------------------------------------------------------------------
         NAMELIST /miolParameterPaths/ cl_equivalencesPath, &
                                         cl_globalAttributesPath, &
                                         cl_variableAttributesPath, &
                                         cl_dimensionsPath
         NAMELIST /miolParameterUserPaths/ cl_equivalencesPath, &
                                             cl_globalAttributesPath, &
                                             cl_variableAttributesPath, &
                                             cl_dimensionsPath
         NAMELIST /nb_dim/il_kindofdim
         NAMELIST /nb_att/ il_nbatt,& 
                           cl_varname

         cl_fonction="MIOL_writef_field_B_3D_NC"

         IF (cl_miolUserParameterFile /= '') THEN 
            OPEN(20, file=cl_miolUserParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterUserPaths)
            CLOSE(20)
         ELSE
            OPEN(20, file=cp_miolParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterPaths)
            CLOSE(20)
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Initialization
 
         cl_varfile = TRIM(cl_variableAttributesPath)//TRIM(cd_varname)//'.in'
         il_minvalue = -99
         il_maxvalue = 99
         il_fillvalue2 = 99
         il_missvalue = 99
 
 
         !-----------------------------------------------------------------------
         ! Read dimensions
 
         il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                          NF90_WRITE, &
                                          il_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_REDEF(il_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE(il_file_id, &
                                             il_nbfiledim),cl_fonction)
 
         ALLOCATE(ila_filedimlen(il_nbfiledim), stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         ALLOCATE(cla_filedimname(il_nbfiledim), stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         DO il_ji = 1, il_nbfiledim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                          il_ji, &
                                                          cla_filedimname(il_ji), &
                                                          ila_filedimlen(il_ji)),cl_fonction)
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find dimensions
 
         ALLOCATE(ila_dimsout_id(3), stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
 
                     IF (ila_filedimlen(il_jk).NE.SIZE(ida_varvalue, dim=il_ji)) THEN
                        WRITE(0,*) ' MIOL_write_field_B_3D_NC : dimensions error. '
                        CALL flush(0)
                        STOP
                     ENDIF
 
                  ENDIF
 
               ENDDO
 
            ENDDO
 
            CLOSE(20)
 
            DEALLOCATE(cla_dimname, stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find the offset values

          IF (PRESENT(rda_offsetvalue)) THEN
            rl_scalevalue = rda_offsetvalue(1)
            rl_offsetvalue = rda_offsetvalue(2)
          ELSE
            rl_scalevalue  = 1
            rl_offsetvalue = 0
           ENDIF 

         !-----------------------------------------------------------------------
         ! Find the minimum and maximum values 
 
         IF (.NOT.(PRESENT(ida_specialvalue))) THEN
            il_minvalue = MINVAL(ida_varvalue)
            il_maxvalue = MAXVAL(array=ida_varvalue, &
                                      mask=ida_varvalue .NE. il_fillvalue2)
         ELSE
            il_minvalue = ida_specialvalue(1)
            il_maxvalue = ida_specialvalue(2)
            il_fillvalue2 = ida_specialvalue(3)
            il_missvalue = ida_specialvalue(4)
         ENDIF
 
         !------------------------------------------------------------------------
         ! Write variable attributes
         INQUIRE(FILE=cl_varfile,EXIST=ll_tri)
         !** Si le fichier.in existe on prend la namelist sinon creation d'un fichier par defaut

         IF (ll_tri) THEN
            OPEN(20, file=cl_varfile, status='old', form='formatted')
            READ(20, nb_att)
            
         !-----------------------------------------------------------------------
         ! Define the variable
 
         il_status = fi_ncError(NF90_DEF_VAR(il_file_id, &
                                             cl_varname, &
                                             NF90_BYTE, &
                                             ila_dimsout_id, &
                                             il_varin_id),cl_fonction)
 
         
         DO il_ji = 1, il_nbatt
 
            READ(20, *) cl_attname
            READ(20, '(A100)') cl_attvalue
 
            SELECTCASE (cl_attvalue)
 
               CASE ('minvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_minvalue),cl_fonction)
 
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_maxvalue),cl_fonction)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_fillvalue2),cl_fonction)
 
               
               CASE ('scalevalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rl_scalevalue),cl_fonction) 
 
               CASE ('offsetvalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rl_offsetvalue),cl_fonction) 
 
               CASE ('missvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_missvalue),cl_fonction)
 
               CASE DEFAULT
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      TRIM(cl_attvalue)),cl_fonction)
 
            ENDSELECT
 
         ENDDO
 
         CLOSE(20)
         ELSE
            il_status = fi_ncError(NF90_DEF_VAR(il_file_id, &
                                                cd_varname, &
                                                NF90_BYTE, &
                                                ila_dimsout_id, &
                                                il_varin_id),cl_fonction)
            
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(il_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Put array
 
         il_status = fi_ncError(NF90_PUT_VAR(il_file_id, &
                                             il_varin_id, &
                                             ida_varvalue),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Close file
 
         il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Memory deallocation
 
         IF (ALLOCATED(cla_filedimname)) DEALLOCATE (cla_filedimname, stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         IF (ALLOCATED(ila_filedimlen)) DEALLOCATE (ila_filedimlen, stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         IF (ALLOCATED(ila_dimsout_id)) DEALLOCATE (ila_dimsout_id, stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
  
        END SUBROUTINE MIOL_writef_field_B_3D_NC
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
         !!======================================================================
         !> \brief
         !!
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param cd_filename         A NetCDF filename. You must specify the complete
         !!                       path.
         !! @param cd_varname          The variable name.
         !! @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param ida_varvalue        The data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values 
         !! @param ida_specialvalue    Vector of special values of the variable:
         !!                                      ida_specialvalue(1) = minvalue
         !!                                      ida_specialvalue(2) = maxvalue
         !!                                      ida_specialvalue(3) = fillvalue
         !!                                      ida_specialvalue(4) = missvalue
         !!
         !! History :
         !!        \n  07/2007  (F. Messal) Creation
         !!        \n  09/2007  (F. Messal) CVS version 1.1
         !!        \n  01/2013  (C.REGNIER) SVn Version 3.5
         !<
         !!======================================================================

       SUBROUTINE MIOL_writef_field_B_2D_NC (cd_filename, &
                                             cd_varname, &
                                             cd_key, &
                                             ida_varvalue, &
                                             rda_offsetvalue, &
                                             ida_specialvalue)
 
         USE netcdf
         
         USE MIOL_param
         USE MFT_error
         IMPLICIT NONE
 
              !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),            INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
         CHARACTER(LEN=2),            INTENT(IN) :: cd_key
         INTEGER(KIND=1), DIMENSION(:,:), INTENT(IN) :: ida_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         INTEGER(KIND=1), DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_file_id, il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         INTEGER(KIND=1) :: il_fillvalue2, il_missvalue, il_minvalue, il_maxvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue
         CHARACTER(LEN=255) :: cl_attvalue,cl_fonction
         CHARACTER(LEN=18) :: cl_attname
         INTEGER :: il_ji, il_jj, il_jk, il_status, il_kindofdim
         LOGICAL :: ll_tri

         !-----------------------------------------------------------------------
         
         NAMELIST /miolParameterPaths/ cl_equivalencesPath, &
                                         cl_globalAttributesPath, &
                                         cl_variableAttributesPath, &
                                         cl_dimensionsPath
         NAMELIST /miolParameterUserPaths/ cl_equivalencesPath, &
                                             cl_globalAttributesPath, &
                                             cl_variableAttributesPath, &
                                             cl_dimensionsPath
         NAMELIST /nb_dim/il_kindofdim
         NAMELIST /nb_att/ il_nbatt,& 
                           cl_varname

         cl_fonction="MIOL_writef_field_B_2D_NC"

         IF (cl_miolUserParameterFile /= '') THEN 
            OPEN(20, file=cl_miolUserParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterUserPaths)
            CLOSE(20)
         ELSE
            OPEN(20, file=cp_miolParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterPaths)
            CLOSE(20)
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Initialization
 
         cl_varfile = TRIM(cl_variableAttributesPath)//TRIM(cd_varname)//'.in'
         il_minvalue = -99
         il_maxvalue = 99
         il_fillvalue2 = 99
         il_missvalue = 99
 
 
         !-----------------------------------------------------------------------
         ! Read dimensions
 
         il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                          NF90_WRITE, &
                                          il_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_REDEF(il_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE(il_file_id, &
                                             il_nbfiledim),cl_fonction)
 
         ALLOCATE(ila_filedimlen(il_nbfiledim), stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         ALLOCATE(cla_filedimname(il_nbfiledim), stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         DO il_ji = 1, il_nbfiledim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                          il_ji, &
                                                          cla_filedimname(il_ji), &
                                                          ila_filedimlen(il_ji)),cl_fonction)
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find dimensions
 
         ALLOCATE(ila_dimsout_id(2), stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
 
                     IF (ila_filedimlen(il_jk).NE.SIZE(ida_varvalue, dim=il_ji)) THEN
                        WRITE(0,*) ' MIOL_write_field_I_2D_NC : dimensions error. '
                        CALL flush(0)
                        STOP
                     ENDIF
 
                  ENDIF
 
               ENDDO
 
            ENDDO
 
            CLOSE(20)
 
            DEALLOCATE(cla_dimname, stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find the offset values

          IF (PRESENT(rda_offsetvalue)) THEN
            rl_scalevalue = rda_offsetvalue(1)
            rl_offsetvalue = rda_offsetvalue(2)
          ELSE
            rl_scalevalue  = 1
            rl_offsetvalue = 0
 	  ENDIF 

         !-----------------------------------------------------------------------
         ! Find the minimum and maximum values 
 
         IF (.NOT.(PRESENT(ida_specialvalue))) THEN
            il_minvalue = MINVAL(ida_varvalue)
            il_maxvalue = MAXVAL(array=ida_varvalue, &
                                      mask=ida_varvalue .NE. il_fillvalue2)
         ELSE
            il_minvalue = ida_specialvalue(1)
            il_maxvalue = ida_specialvalue(2)
            il_fillvalue2 = ida_specialvalue(3)
            il_missvalue = ida_specialvalue(4)
         ENDIF
 
         !------------------------------------------------------------------------
         ! Write variable attributes
         INQUIRE(FILE=cl_varfile,EXIST=ll_tri)
         !** Si le fichier.in existe on prend la namelist sinon creation d'un fichier par defaut

         IF (ll_tri) THEN
            OPEN(20, file=cl_varfile, status='old', form='formatted')
            READ(20, nb_att)
 
         !-----------------------------------------------------------------------
         ! Define the variable
 
         il_status = fi_ncError(NF90_DEF_VAR(il_file_id, &
                                             cl_varname, &
                                             NF90_BYTE, &
                                             ila_dimsout_id, &
                                             il_varin_id),cl_fonction)
 
        
         DO il_ji = 1, il_nbatt
 
            READ(20, *) cl_attname
            READ(20, '(A100)') cl_attvalue
 
            SELECTCASE (cl_attvalue)
 
               CASE ('minvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_minvalue),cl_fonction)
 
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_maxvalue),cl_fonction)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_fillvalue2),cl_fonction)
 
               
               CASE ('scalevalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rl_scalevalue),cl_fonction) 
 
               CASE ('offsetvalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rl_offsetvalue),cl_fonction) 
 
               CASE ('missvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_missvalue),cl_fonction)
 
               CASE DEFAULT
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      TRIM(cl_attvalue)),cl_fonction)
 
            ENDSELECT
 
         ENDDO
 
         CLOSE(20)
         ELSE
              il_status = fi_ncError(NF90_DEF_VAR(il_file_id, &
                                                  cd_varname, &
                                                  NF90_BYTE, &
                                                  ila_dimsout_id, &
                                                  il_varin_id),cl_fonction)
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(il_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Put array
 
         il_status = fi_ncError(NF90_PUT_VAR(il_file_id, &
                                             il_varin_id, &
                                             ida_varvalue),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Close file
 
         il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Memory deallocation
 
         IF (ALLOCATED(cla_filedimname)) DEALLOCATE (cla_filedimname, stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         IF (ALLOCATED(ila_filedimlen)) DEALLOCATE (ila_filedimlen, stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         IF (ALLOCATED(ila_dimsout_id)) DEALLOCATE (ila_dimsout_id, stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
  
        END SUBROUTINE MIOL_writef_field_B_2D_NC


 !******************************************************************************
 !******************************************************************************
 !******************************************************************************
 
         !!======================================================================
         !> \brief
         !!
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param cd_filename         A NetCDF filename. You must specify the complete
         !!                       path.
         !! @param cd_varname          The variable name.
         !! @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param ida_varvalue        The data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values 
         !! @param ida_specialvalue    Vector of special values of the variable:
         !!                                      ida_specialvalue(1) = minvalue
         !!                                      ida_specialvalue(2) = maxvalue
         !!                                      ida_specialvalue(3) = fillvalue
         !!                                      ida_specialvalue(4) = missvalue
         !!
         !! History :
         !!        \n  07/2007  (F. Messal) Creation
         !!        \n  09/2007  (F. Messal) CVS version 1.1
         !!        \n  01/2013  (C.REGNIER) SVn Version 3.5
         !<
         !!======================================================================

       SUBROUTINE MIOL_writef_field_B_1D_NC (cd_filename, &
                                             cd_varname, &
                                             cd_key, &
                                             ida_varvalue, &
                                             rda_offsetvalue, &
                                             ida_specialvalue)
 
         USE netcdf
         
         USE MIOL_param
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),            INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
         CHARACTER(LEN=1),            INTENT(IN) :: cd_key
         INTEGER(KIND=1), DIMENSION(:), INTENT(IN) :: ida_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         INTEGER(KIND=1), DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_file_id, il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         INTEGER(KIND=1) :: il_fillvalue2, il_missvalue, il_minvalue, il_maxvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue
         CHARACTER(LEN=255) :: cl_attvalue,cl_fonction
         CHARACTER(LEN=18) :: cl_attname
         INTEGER :: il_ji, il_jj, il_jk, il_status, il_kindofdim
         LOGICAL :: ll_tri

 
         !-----------------------------------------------------------------------
         NAMELIST /miolParameterPaths/ cl_equivalencesPath, &
                                         cl_globalAttributesPath, &
                                         cl_variableAttributesPath, &
                                         cl_dimensionsPath
         NAMELIST /miolParameterUserPaths/ cl_equivalencesPath, &
                                             cl_globalAttributesPath, &
                                             cl_variableAttributesPath, &
                                             cl_dimensionsPath
         NAMELIST /nb_dim/il_kindofdim
         NAMELIST /nb_att/ il_nbatt,& 
                           cl_varname

         cl_fonction="MIOL_writef_field_B_1D_NC"

         IF (cl_miolUserParameterFile /= '') THEN 
            OPEN(20, file=cl_miolUserParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterUserPaths)
            CLOSE(20)
         ELSE
            OPEN(20, file=cp_miolParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterPaths)
            CLOSE(20)
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Initialization
 
         cl_varfile = TRIM(cl_variableAttributesPath)//TRIM(cd_varname)//'.in'
         il_minvalue = -99
         il_maxvalue = 99
         il_fillvalue2 = 99
         il_missvalue = 99
 
 
         !-----------------------------------------------------------------------
         ! Read dimensions
 
         il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                          NF90_WRITE, &
                                          il_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_REDEF(il_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE(il_file_id, &
                                             il_nbfiledim),cl_fonction)
 
         ALLOCATE(ila_filedimlen(il_nbfiledim), stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         ALLOCATE(cla_filedimname(il_nbfiledim), stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         DO il_ji = 1, il_nbfiledim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                          il_ji, &
                                                          cla_filedimname(il_ji), &
                                                          ila_filedimlen(il_ji)),cl_fonction)
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find dimensions
 
         ALLOCATE(ila_dimsout_id(1), stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
 
                     IF (ila_filedimlen(il_jk).NE.SIZE(ida_varvalue, dim=il_ji)) THEN
                        WRITE(0,*) ' MIOL_write_field_B_1D_NC : dimensions error. '
                        CALL flush(0)
                        STOP
                     ENDIF
 
                  ENDIF
 
               ENDDO
 
            ENDDO
 
            CLOSE(20)
 
            DEALLOCATE(cla_dimname, stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find the offset values

          IF (PRESENT(rda_offsetvalue)) THEN
            rl_scalevalue = rda_offsetvalue(1)
            rl_offsetvalue = rda_offsetvalue(2)
          ELSE
            rl_scalevalue  = 1
            rl_offsetvalue = 0
          ENDIF 

         !-----------------------------------------------------------------------
         ! Find the minimum and maximum values 
 
         IF (.NOT.(PRESENT(ida_specialvalue))) THEN
            il_minvalue = MINVAL(ida_varvalue)
            il_maxvalue = MAXVAL(array=ida_varvalue, &
                                      mask=ida_varvalue .NE. il_fillvalue2)
         ELSE
            il_minvalue = ida_specialvalue(1)
            il_maxvalue = ida_specialvalue(2)
            il_fillvalue2 = ida_specialvalue(3)
            il_missvalue = ida_specialvalue(4)
         ENDIF
 
         !------------------------------------------------------------------------
         ! Write variable attributes
         INQUIRE(FILE=cl_varfile,EXIST=ll_tri)
         !** Si le fichier.in existe on prend la namelist sinon creation d'un fichier par defaut

         IF (ll_tri) THEN
            OPEN(20, file=cl_varfile, status='old', form='formatted')
            READ(20, nb_att)

         !-----------------------------------------------------------------------
         ! Define the variable
 
         il_status = fi_ncError(NF90_DEF_VAR(il_file_id, &
                                             cl_varname, &
                                             NF90_BYTE, &
                                             ila_dimsout_id, &
                                             il_varin_id),cl_fonction)
 
        
 
         DO il_ji = 1, il_nbatt
 
            READ(20, *) cl_attname
            READ(20, '(A100)') cl_attvalue
 
            SELECTCASE (cl_attvalue)
 
               CASE ('minvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_minvalue),cl_fonction)
 
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_maxvalue),cl_fonction)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_fillvalue2),cl_fonction)
 
               
               CASE ('scalevalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rl_scalevalue),cl_fonction) 
 
               CASE ('offsetvalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rl_offsetvalue),cl_fonction) 
 
               CASE ('missvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_missvalue),cl_fonction)
 
               CASE DEFAULT
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      TRIM(cl_attvalue)),cl_fonction)
 
            ENDSELECT
 
         ENDDO
 
         CLOSE(20)
      ELSE
         il_status = fi_ncError(NF90_DEF_VAR(il_file_id, &
                                             cd_varname, &
                                             NF90_BYTE, &
                                             ila_dimsout_id, &
                                             il_varin_id),cl_fonction)
 
      ENDIF
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(il_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Put array
 
         il_status = fi_ncError(NF90_PUT_VAR(il_file_id, &
                                             il_varin_id, &
                                             ida_varvalue),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Close file
 
         il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Memory deallocation
 
         IF (ALLOCATED(cla_filedimname)) DEALLOCATE (cla_filedimname, stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         IF (ALLOCATED(ila_filedimlen)) DEALLOCATE (ila_filedimlen, stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         IF (ALLOCATED(ila_dimsout_id)) DEALLOCATE (ila_dimsout_id, stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
 
       END SUBROUTINE MIOL_writef_field_B_1D_NC
 
 

   !******************************************************************************
   !******************************************************************************
   !******************************************************************************
   !******************************************************************************
 
         !!======================================================================
         !> \brief
         !!
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !!  @param id_file_id         A NetCDF file Id.
         !!  @param cd_varname          The variable name.
         !!  @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !!  @param ida_varvalue        The data values to be write.
         !!  @param rda_offsetvalue     optional offset and scale factor values 
         !!  @param ida_specialvalue    Vector of special values of the variable:
         !!                                      ida_specialvalue(1) = minvalue
         !!                                      ida_specialvalue(2) = maxvalue
         !!                                      ida_specialvalue(3) = fillvalue
         !!                                      ida_specialvalue(4) = missvalue
         !!
         !! History :
         !!        \n  07/2007  (F. Messal) Creation
         !!        \n  09/2007  (F. Messal) CVS version 1.1
         !!        \n  01/2013  (C.REGNIER) SVn Version 3.5
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_writeu_field_B_4D_NC (id_file_id, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)
 
         USE netcdf
         
         USE MIOL_param
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,                     INTENT(IN) :: id_file_id
         CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
         CHARACTER(LEN=4),            INTENT(IN) :: cd_key
         INTEGER(KIND=1), DIMENSION(:,:,:,:), INTENT(IN) :: ida_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         INTEGER(KIND=1), DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         INTEGER(KIND=1) :: il_fillvalue2, il_missvalue, il_minvalue, il_maxvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue
         CHARACTER(LEN=255) :: cl_attvalue,cl_fonction
         CHARACTER(LEN=18) :: cl_attname
         INTEGER :: il_ji, il_jj, il_jk, il_status, il_kindofdim
         LOGICAL :: ll_tri

         !-----------------------------------------------------------------------
         NAMELIST /miolParameterPaths/ cl_equivalencesPath, &
                                         cl_globalAttributesPath, &
                                         cl_variableAttributesPath, &
                                         cl_dimensionsPath
         NAMELIST /miolParameterUserPaths/ cl_equivalencesPath, &
                                             cl_globalAttributesPath, &
                                             cl_variableAttributesPath, &
                                             cl_dimensionsPath
         NAMELIST /nb_dim/il_kindofdim
         NAMELIST /nb_att/ il_nbatt,& 
                           cl_varname

         cl_fonction="MIOL_writeu_field_B_4D_NC"

         IF (cl_miolUserParameterFile /= '') THEN 
            OPEN(20, file=cl_miolUserParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterUserPaths)
            CLOSE(20)
         ELSE
            OPEN(20, file=cp_miolParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterPaths)
            CLOSE(20)
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Initialization
 
         cl_varfile = TRIM(cl_variableAttributesPath)//TRIM(cd_varname)//'.in'
         il_minvalue = -99
         il_maxvalue = 99
         il_fillvalue2 = 99
         il_missvalue = 99
 
 
         !-----------------------------------------------------------------------
         ! Read dimensions
 
         il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE(id_file_id, &
                                             il_nbfiledim),cl_fonction)
 
         ALLOCATE(ila_filedimlen(il_nbfiledim), stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         ALLOCATE(cla_filedimname(il_nbfiledim), stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         DO il_ji = 1, il_nbfiledim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id, &
                                                          il_ji, &
                                                          cla_filedimname(il_ji), &
                                                          ila_filedimlen(il_ji)),cl_fonction)
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find dimensions
 
         ALLOCATE(ila_dimsout_id(4), stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
 
                     IF (ila_filedimlen(il_jk).NE.SIZE(ida_varvalue, dim=il_ji)) THEN
                        WRITE(0,*) ' MIOL_write_field_B_4D_NC : dimensions error. '
                        CALL flush(0)
                        STOP
                     ENDIF
 
                  ENDIF
 
               ENDDO
 
            ENDDO
 
            CLOSE(20)
 
            DEALLOCATE(cla_dimname, stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find the offset values

          IF (PRESENT(rda_offsetvalue)) THEN
            rl_scalevalue = rda_offsetvalue(1)
            rl_offsetvalue = rda_offsetvalue(2)
     	  ELSE
            rl_scalevalue  = 1
            rl_offsetvalue = 0
          ENDIF 

         !-----------------------------------------------------------------------
         ! Find the minimum and maximum values 
 
         IF (.NOT.(PRESENT(ida_specialvalue))) THEN
            il_minvalue = MINVAL(ida_varvalue)
            il_maxvalue = MAXVAL(array=ida_varvalue, &
                                      mask=ida_varvalue .NE. il_fillvalue2)
         ELSE
            il_minvalue = ida_specialvalue(1)
            il_maxvalue = ida_specialvalue(2)
            il_fillvalue2 = ida_specialvalue(3)
            il_missvalue = ida_specialvalue(4)
         ENDIF
 
 
         !------------------------------------------------------------------------
         ! Write variable attributes
         
         INQUIRE(FILE=cl_varfile,EXIST=ll_tri)
         !** Si le fichier.in existe on prend la namelist sinon creation d'un fichier par defaut

         IF (ll_tri) THEN
            OPEN(20, file=cl_varfile, status='old', form='formatted')
            READ(20, nb_att)
 
         !-----------------------------------------------------------------------
         ! Define the variable
 
         il_status = fi_ncError(NF90_DEF_VAR(id_file_id, &
                                             cl_varname, &
                                             NF90_BYTE, &
                                             ila_dimsout_id, &
                                             il_varin_id),cl_fonction)
 
         DO il_ji = 1, il_nbatt
 
            READ(20, *) cl_attname
            READ(20, '(A100)') cl_attvalue
 
            SELECTCASE (cl_attvalue)
 
               CASE ('minvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_minvalue),cl_fonction)
 
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_maxvalue),cl_fonction)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_fillvalue2),cl_fonction)
 
               
               CASE ('scalevalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rl_scalevalue),cl_fonction) 
 
               CASE ('offsetvalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rl_offsetvalue),cl_fonction) 
 
               CASE ('missvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_missvalue),cl_fonction)
 
               CASE DEFAULT
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      TRIM(cl_attvalue)),cl_fonction)
 
            ENDSELECT
 
         ENDDO
 
         CLOSE(20)
         
         ELSE
             il_status = fi_ncError(NF90_DEF_VAR(id_file_id, &
                                                 cd_varname, &
                                                 NF90_BYTE, &
                                                 ila_dimsout_id, &
                                                 il_varin_id),cl_fonction)
 
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(id_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Put array
 
         il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
                                             il_varin_id, &
                                             ida_varvalue),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Memory deallocation
 
         IF (ALLOCATED(cla_filedimname)) DEALLOCATE (cla_filedimname, stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         IF (ALLOCATED(ila_filedimlen)) DEALLOCATE (ila_filedimlen, stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         IF (ALLOCATED(ila_dimsout_id)) DEALLOCATE (ila_dimsout_id, stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
 
       END SUBROUTINE MIOL_writeu_field_B_4D_NC
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
       
         !!======================================================================
         !> \brief    
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !!  @param id_file_id          A NetCDF file Id.
         !!  @param cd_varname          The variable name.
         !!  @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !!  @param ida_varvalue        The data values to be write.
         !!  @param rda_offsetvalue     optional offset and scale factor values 
         !!  @param ida_specialvalue    Vector of special values of the variable:
         !!                                      ida_specialvalue(1) = minvalue
         !!                                      ida_specialvalue(2) = maxvalue
         !!                                      ida_specialvalue(3) = fillvalue
         !!                                      ida_specialvalue(4) = missvalue
         !!
         !! History :
         !!        \n  07/2007  (F. Messal) Creation
         !!        \n  09/2007  (F. Messal) CVS version 1.1
         !!        \n  01/2013  (C.REGNIER) SVn Version 3.5
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_writeu_field_B_3D_NC (id_file_id, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)
 
         USE netcdf
         
         USE MIOL_param
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,                     INTENT(IN) :: id_file_id
         CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
         CHARACTER(LEN=3),            INTENT(IN) :: cd_key
         INTEGER(KIND=1), DIMENSION(:,:,:), INTENT(IN) :: ida_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         INTEGER(KIND=1), DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         INTEGER(KIND=1) :: il_fillvalue2, il_missvalue, il_minvalue, il_maxvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue
         CHARACTER(LEN=255) :: cl_attvalue,cl_fonction
         CHARACTER(LEN=18) :: cl_attname
         INTEGER :: il_ji, il_jj, il_jk, il_status, il_kindofdim
         LOGICAL :: ll_tri

         
         !-----------------------------------------------------------------------
         NAMELIST /miolParameterPaths/ cl_equivalencesPath, &
                                         cl_globalAttributesPath, &
                                         cl_variableAttributesPath, &
                                         cl_dimensionsPath
         NAMELIST /miolParameterUserPaths/ cl_equivalencesPath, &
                                             cl_globalAttributesPath, &
                                             cl_variableAttributesPath, &
                                             cl_dimensionsPath
         NAMELIST /nb_dim/il_kindofdim
         NAMELIST /nb_att/ il_nbatt,& 
                           cl_varname

         cl_fonction="MIOL_writeu_field_B_3D_NC"
 
         IF (cl_miolUserParameterFile /= '') THEN 
            OPEN(20, file=cl_miolUserParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterUserPaths)
            CLOSE(20)
         ELSE
            OPEN(20, file=cp_miolParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterPaths)
            CLOSE(20)
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Initialization
 
         cl_varfile = TRIM(cl_variableAttributesPath)//TRIM(cd_varname)//'.in'
         il_minvalue = -99
         il_maxvalue = 99
         il_fillvalue2 = 99
         il_missvalue = 99
 
 
         !-----------------------------------------------------------------------
         ! Read dimensions
 
         il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE(id_file_id, &
                                             il_nbfiledim),cl_fonction)
 
         ALLOCATE(ila_filedimlen(il_nbfiledim), stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         ALLOCATE(cla_filedimname(il_nbfiledim), stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         DO il_ji = 1, il_nbfiledim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id, &
                                                          il_ji, &
                                                          cla_filedimname(il_ji), &
                                                          ila_filedimlen(il_ji)),cl_fonction)
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find dimensions
 
         ALLOCATE(ila_dimsout_id(3), stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
 
                     IF (ila_filedimlen(il_jk).NE.SIZE(ida_varvalue, dim=il_ji)) THEN
                        WRITE(0,*) ' MIOL_write_field_B_3D_NC : dimensions error. '
                        CALL flush(0)
                        STOP
                     ENDIF
 
                  ENDIF
 
               ENDDO
 
            ENDDO
 
            CLOSE(20)
 
            DEALLOCATE(cla_dimname, stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find the offset values

          IF (PRESENT(rda_offsetvalue)) THEN
            rl_scalevalue = rda_offsetvalue(1)
            rl_offsetvalue = rda_offsetvalue(2)
          ELSE
            rl_scalevalue  = 1
            rl_offsetvalue = 0
 	  ENDIF 

         !-----------------------------------------------------------------------
         ! Find the minimum and maximum values 
 
         IF (.NOT.(PRESENT(ida_specialvalue))) THEN
            il_minvalue = MINVAL(ida_varvalue)
            il_maxvalue = MAXVAL(array=ida_varvalue, &
                                      mask=ida_varvalue .NE. il_fillvalue2)
         ELSE
            il_minvalue = ida_specialvalue(1)
            il_maxvalue = ida_specialvalue(2)
            il_fillvalue2 = ida_specialvalue(3)
            il_missvalue = ida_specialvalue(4)
         ENDIF
 
         !------------------------------------------------------------------------
         ! Write variable attributes
 
         INQUIRE(FILE=cl_varfile,EXIST=ll_tri)
         !** Si le fichier.in existe on prend la namelist sinon creation d'un fichier par defaut

         IF (ll_tri) THEN
            OPEN(20, file=cl_varfile, status='old', form='formatted')
            READ(20, nb_att)
 
         !-----------------------------------------------------------------------
         ! Define the variable
 
         il_status = fi_ncError(NF90_DEF_VAR(id_file_id, &
                                             cl_varname, &
                                             NF90_BYTE, &
                                             ila_dimsout_id, &
                                             il_varin_id),cl_fonction)
 
        
         DO il_ji = 1, il_nbatt
 
            READ(20, *) cl_attname
            READ(20, '(A100)') cl_attvalue
 
            SELECTCASE (cl_attvalue)
 
               CASE ('minvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_minvalue),cl_fonction)
 
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_maxvalue),cl_fonction)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_fillvalue2),cl_fonction)
 
               
               CASE ('scalevalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rl_scalevalue),cl_fonction) 
 
               CASE ('offsetvalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rl_offsetvalue),cl_fonction) 
 
               CASE ('missvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_missvalue),cl_fonction)
 
               CASE DEFAULT
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      TRIM(cl_attvalue)),cl_fonction)
 
            ENDSELECT
 
         ENDDO
 
         CLOSE(20)
         
         ELSE
            
         il_status = fi_ncError(NF90_DEF_VAR(id_file_id, &
                                             cd_varname, &
                                             NF90_BYTE, &
                                             ila_dimsout_id, &
                                             il_varin_id),cl_fonction)
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(id_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Put array
 
         il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
                                             il_varin_id, &
                                             ida_varvalue),cl_fonction)
 
         !-----------------------------------------------------------------------
         ! Memory deallocation
 
         IF (ALLOCATED(cla_filedimname)) DEALLOCATE (cla_filedimname, stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         IF (ALLOCATED(ila_filedimlen)) DEALLOCATE (ila_filedimlen, stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         IF (ALLOCATED(ila_dimsout_id)) DEALLOCATE (ila_dimsout_id, stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
 
       END SUBROUTINE MIOL_writeu_field_B_3D_NC
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
         !!======================================================================
         !> \brief    
         !!
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !!  @param  id_file_id          A NetCDF file Id.
         !!  @param  cd_varname          The variable name.
         !!  @param  cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !!  @param  ida_varvalue        The data values to be write.
         !!  @param rda_offsetvalue     optional offset and scale factor values 
         !!  @param  ida_specialvalue    Vector of special values of the variable:
         !!                                      ida_specialvalue(1) = minvalue
         !!                                      ida_specialvalue(2) = maxvalue
         !!                                      ida_specialvalue(3) = fillvalue
         !!                                      ida_specialvalue(4) = missvalue
         !!
         !! History :
         !!        \n  07/2007  (F. Messal) Creation
         !!        \n  09/2007  (F. Messal) CVS version 1.1
         !!        \n  01/2013  (C.REGNIER) SVn Version 3.5
         !<
         !!======================================================================

       SUBROUTINE MIOL_writeu_field_B_2D_NC (id_file_id, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                          ida_specialvalue)
 
         USE netcdf
         
         USE MIOL_param
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,            INTENT(IN) :: id_file_id
         CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
         CHARACTER(LEN=2),            INTENT(IN) :: cd_key
         INTEGER(KIND=1), DIMENSION(:,:), INTENT(IN) :: ida_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         INTEGER(KIND=1), DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         INTEGER(KIND=1) :: il_fillvalue2, il_missvalue, il_minvalue, il_maxvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue
         CHARACTER(LEN=255) :: cl_attvalue,cl_fonction
         CHARACTER(LEN=18) :: cl_attname
         INTEGER :: il_ji, il_jj, il_jk, il_status, il_kindofdim
         LOGICAL :: ll_tri

         !-----------------------------------------------------------------------
         NAMELIST /miolParameterPaths/ cl_equivalencesPath, &
                                         cl_globalAttributesPath, &
                                         cl_variableAttributesPath, &
                                         cl_dimensionsPath
         NAMELIST /miolParameterUserPaths/ cl_equivalencesPath, &
                                             cl_globalAttributesPath, &
                                             cl_variableAttributesPath, &
                                             cl_dimensionsPath
         NAMELIST /nb_dim/il_kindofdim
         NAMELIST /nb_att/ il_nbatt,& 
                           cl_varname

         cl_fonction="MIOL_writeu_field_B_2D_NC"

         IF (cl_miolUserParameterFile /= '') THEN 
            OPEN(20, file=cl_miolUserParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterUserPaths)
            CLOSE(20)
         ELSE
            OPEN(20, file=cp_miolParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterPaths)
            CLOSE(20)
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Initialization
 
         cl_varfile = TRIM(cl_variableAttributesPath)//TRIM(cd_varname)//'.in'
         il_minvalue = -99
         il_maxvalue = 99
         il_fillvalue2 = 99
         il_missvalue = 99
 
 
         !-----------------------------------------------------------------------
         ! Read dimensions
 
         il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE(id_file_id, &
                                             il_nbfiledim),cl_fonction)
 
         ALLOCATE(ila_filedimlen(il_nbfiledim), stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         ALLOCATE(cla_filedimname(il_nbfiledim), stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         DO il_ji = 1, il_nbfiledim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id, &
                                                          il_ji, &
                                                          cla_filedimname(il_ji), &
                                                          ila_filedimlen(il_ji)),cl_fonction)
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find dimensions
 
         ALLOCATE(ila_dimsout_id(2), stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
 
                     IF (ila_filedimlen(il_jk).NE.SIZE(ida_varvalue, dim=il_ji)) THEN
                        WRITE(0,*) ' MIOL_write_field_B_2D_NC : dimensions error. '
                        CALL flush(0)
                        STOP
                     ENDIF
 
                  ENDIF
 
               ENDDO
 
            ENDDO
 
            CLOSE(20)
 
            DEALLOCATE(cla_dimname, stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find the offset values

          IF (PRESENT(rda_offsetvalue)) THEN
            rl_scalevalue = rda_offsetvalue(1)
            rl_offsetvalue = rda_offsetvalue(2)
          ELSE
            rl_scalevalue  = 1
            rl_offsetvalue = 0
 	 ENDIF 

         !-----------------------------------------------------------------------
         ! Find the minimum and maximum values 
 
         IF (.NOT.(PRESENT(ida_specialvalue))) THEN
            il_minvalue = MINVAL(ida_varvalue)
            il_maxvalue = MAXVAL(array=ida_varvalue, &
                                      mask=ida_varvalue .NE. il_fillvalue2)
         ELSE
            il_minvalue = ida_specialvalue(1)
            il_maxvalue = ida_specialvalue(2)
            il_fillvalue2 = ida_specialvalue(3)
            il_missvalue = ida_specialvalue(4)
         ENDIF
 
         !------------------------------------------------------------------------
         ! Write variable attributes
         INQUIRE(FILE=cl_varfile,EXIST=ll_tri)
         !** Si le fichier.in existe on prend la namelist sinon creation d'un fichier par defaut

         IF (ll_tri) THEN
            OPEN(20, file=cl_varfile, status='old', form='formatted')
            READ(20, nb_att)
 
         !-----------------------------------------------------------------------
         ! Define the variable
 
         il_status = fi_ncError(NF90_DEF_VAR(id_file_id, &
                                             cl_varname, &
                                             NF90_BYTE, &
                                             ila_dimsout_id, &
                                             il_varin_id),cl_fonction)
 
       
         DO il_ji = 1, il_nbatt
 
            READ(20, *) cl_attname
            READ(20, '(A100)') cl_attvalue
 
            SELECTCASE (cl_attvalue)
 
               CASE ('minvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_minvalue),cl_fonction)
 
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_maxvalue),cl_fonction)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_fillvalue2),cl_fonction)
 
               
               CASE ('scalevalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rl_scalevalue),cl_fonction) 
 
               CASE ('offsetvalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rl_offsetvalue),cl_fonction) 
 
               CASE ('missvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_missvalue),cl_fonction)
 
               CASE DEFAULT
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      TRIM(cl_attvalue)),cl_fonction)
 
            ENDSELECT
 
         ENDDO
 
         CLOSE(20)
         ELSE
               il_status = fi_ncError(NF90_DEF_VAR(id_file_id, &
                                                   cd_varname, &
                                                   NF90_BYTE, &
                                                   ila_dimsout_id, &
                                                   il_varin_id),cl_fonction)
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(id_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Put array
 
         il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
                                             il_varin_id, &
                                             ida_varvalue),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Memory deallocation
 
         IF (ALLOCATED(cla_filedimname)) DEALLOCATE (cla_filedimname, stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         IF (ALLOCATED(ila_filedimlen)) DEALLOCATE (ila_filedimlen, stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         IF (ALLOCATED(ila_dimsout_id)) DEALLOCATE (ila_dimsout_id, stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
 
        END SUBROUTINE MIOL_writeu_field_B_2D_NC
 
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
         !!======================================================================
         !> \brief    
         !!
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param  id_file_id         A NetCDF file Id.
         !! @param  cd_varname          The variable name.
         !! @param  cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param  ida_varvalue        The data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values 
         !! @param  ida_specialvalue    Vector of special values of the variable:
         !!                                      ida_specialvalue(1) = minvalue
         !!                                      ida_specialvalue(2) = maxvalue
         !!                                      ida_specialvalue(3) = fillvalue
         !!                                      ida_specialvalue(4) = missvalue
         !!
         !! History :
         !!        \n  07/2007  (F. Messal) Creation
         !!        \n  09/2007  (F. Messal) CVS version 1.1
         !!        \n  01/2013  (C.REGNIER) SVn Version 3.5
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_writeu_field_B_1D_NC (id_file_id, &
                                          cd_varname, &
                                          cd_key, &
                                          ida_varvalue, &
                                          rda_offsetvalue, &
                                           ida_specialvalue)
 
         USE netcdf
         USE MIOL_param
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,                     INTENT(IN) :: id_file_id
         CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
         CHARACTER(LEN=1),            INTENT(IN) :: cd_key
         INTEGER(KIND=1), DIMENSION(:), INTENT(IN) :: ida_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         INTEGER(KIND=1), DIMENSION(4),       OPTIONAL, INTENT(IN) :: ida_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         INTEGER(KIND=1) :: il_fillvalue2, il_missvalue, il_minvalue, il_maxvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue
         CHARACTER(LEN=255) :: cl_attvalue,cl_fonction
         CHARACTER(LEN=18) :: cl_attname
         INTEGER :: il_ji, il_jj, il_jk, il_status, il_kindofdim
         LOGICAL :: ll_tri

         !-----------------------------------------------------------------------
         NAMELIST /miolParameterPaths/ cl_equivalencesPath, &
                                         cl_globalAttributesPath, &
                                         cl_variableAttributesPath, &
                                         cl_dimensionsPath
         NAMELIST /miolParameterUserPaths/ cl_equivalencesPath, &
                                             cl_globalAttributesPath, &
                                             cl_variableAttributesPath, &
                                             cl_dimensionsPath
         NAMELIST /nb_dim/il_kindofdim
         NAMELIST /nb_att/ il_nbatt,& 
                           cl_varname

         cl_fonction="MIOL_writeu_field_B_1D_NC"
 
         IF (cl_miolUserParameterFile /= '') THEN 
            OPEN(20, file=cl_miolUserParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterUserPaths)
            CLOSE(20)
         ELSE
            OPEN(20, file=cp_miolParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterPaths)
            CLOSE(20)
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Initialization
 
         cl_varfile = TRIM(cl_variableAttributesPath)//TRIM(cd_varname)//'.in'
         il_minvalue = -99
         il_maxvalue = 99
         il_fillvalue2 = 99
         il_missvalue = 99
 
 
         il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE(id_file_id, &
                                             il_nbfiledim),cl_fonction)
 
         ALLOCATE(ila_filedimlen(il_nbfiledim), stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         ALLOCATE(cla_filedimname(il_nbfiledim), stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         DO il_ji = 1, il_nbfiledim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id, &
                                                          il_ji, &
                                                          cla_filedimname(il_ji), &
                                                          ila_filedimlen(il_ji)),cl_fonction)
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find dimensions
 
         ALLOCATE(ila_dimsout_id(1), stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
 
                     IF (ila_filedimlen(il_jk).NE.SIZE(ida_varvalue, dim=il_ji)) THEN
                        WRITE(0,*) ' MIOL_write_field_B_1D_NC : dimensions error. '
                        CALL flush(0)
                        STOP
                     ENDIF
 
                  ENDIF
 
               ENDDO
 
            ENDDO
 
            CLOSE(20)
 
            DEALLOCATE(cla_dimname, stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find the offset values

          IF (PRESENT(rda_offsetvalue)) THEN
            rl_scalevalue = rda_offsetvalue(1)
            rl_offsetvalue = rda_offsetvalue(2)
	 ELSE
            rl_scalevalue  = 1
            rl_offsetvalue = 0
 	ENDIF 

         !-----------------------------------------------------------------------
         ! Find the minimum and maximum values 
 
         IF (.NOT.(PRESENT(ida_specialvalue))) THEN
            il_minvalue = MINVAL(ida_varvalue)
            il_maxvalue = MAXVAL(array=ida_varvalue, &
                                      mask=ida_varvalue .NE. il_fillvalue2)
         ELSE
            il_minvalue = ida_specialvalue(1)
            il_maxvalue = ida_specialvalue(2)
            il_fillvalue2 = ida_specialvalue(3)
            il_missvalue = ida_specialvalue(4)
         ENDIF
 
         !------------------------------------------------------------------------
         ! Write variable attributes
         INQUIRE(FILE=cl_varfile,EXIST=ll_tri)
         !** Si le fichier.in existe on prend la namelist sinon creation d'un fichier par defaut

         IF (ll_tri) THEN
            OPEN(20, file=cl_varfile, status='old', form='formatted')
            READ(20, nb_att)
 
         !-----------------------------------------------------------------------
         ! Define the variable
 
         il_status = fi_ncError(NF90_DEF_VAR(id_file_id, &
                                             cl_varname, &
                                             NF90_BYTE, &
                                             ila_dimsout_id, &
                                             il_varin_id),cl_fonction)
         
         DO il_ji = 1, il_nbatt
 
            READ(20, *) cl_attname
            READ(20, '(A100)') cl_attvalue
 
            SELECTCASE (cl_attvalue)
 
               CASE ('minvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_minvalue),cl_fonction)
 
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_maxvalue),cl_fonction)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_fillvalue2),cl_fonction)
 
               
               CASE ('scalevalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rl_scalevalue),cl_fonction) 
 
               CASE ('offsetvalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rl_offsetvalue),cl_fonction) 
 
               CASE ('missvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      il_missvalue),cl_fonction)
 
               CASE DEFAULT
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      TRIM(cl_attvalue)),cl_fonction)
 
            ENDSELECT
 
         ENDDO
 
         CLOSE(20)
         ELSE
            il_status = fi_ncError(NF90_DEF_VAR(id_file_id, &
                                                cd_varname, &
                                                NF90_BYTE, &
                                                ila_dimsout_id, &
                                                il_varin_id),cl_fonction)
         ENDIF
 
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(id_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Put array
 
         il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
                                             il_varin_id, &
                                             ida_varvalue),cl_fonction)
 
         !-----------------------------------------------------------------------
         ! Memory deallocation
 
         IF (ALLOCATED(cla_filedimname)) DEALLOCATE (cla_filedimname, stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         IF (ALLOCATED(ila_filedimlen)) DEALLOCATE (ila_filedimlen, stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         IF (ALLOCATED(ila_dimsout_id)) DEALLOCATE (ila_dimsout_id, stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
  
        END SUBROUTINE MIOL_writeu_field_B_1D_NC
 
 

  !******************************************************************************
  !******************************************************************************
