!! \brief Module which contain subroutine for WRITE Char values in NETCDF format
!! \n  Total 4  functions
!! \author C.REGNIER  first version
!! \date 01/2013
!!  \version 3.5  
!<
!Source src_write_character
!**-----------------------------------------
!** Module for Write Char  values in NETCDF format
!**-------------------------------------------
! --------------------------------------- General interface ----------------------------------------------------------------------------
! -- SUBROUTINE MIOL_writef_field_I_1D_NC (cd_filename,cd_varname,cd_key,cda_varvalue,id_length)
! -- SUBROUTINE MIOL_writef_field_I_2D_NC (cd_filename,cd_varname,cd_key,cda_varvalue,id_length)
! -- SUBROUTINE MIOL_writeu_field_I_1D_NC ((id_file_id,cd_varname,cd_key,cda_varvalue,id_length)
! -- SUBROUTINE MIOL_writeu_field_I_2D_NC (id_file_id,cd_varname,cd_key,cda_varvalue,id_length)


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
         !! @param id_length           lenght of the char value
         !!
         !! History :
         !!        \n  01/2013   CREGNIER V3.5 MIOL
         !<
         !!======================================================================

       SUBROUTINE MIOL_writef_field_C_1D_NC (cd_filename, &
                                             cd_varname, &
                                             cd_key, &
                                             cda_varvalue, &
                                             id_length)
 
         
         USE MIOL_param
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),            INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
         CHARACTER(LEN=2),            INTENT(IN) :: cd_key
         CHARACTER(LEN=id_length),DIMENSION(:),INTENT(IN) :: cda_varvalue
         INTEGER(KIND=4),             INTENT(IN) :: id_length
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         CHARACTER(LEN=256)                      :: cl_fonction,cl_varfile,cl_variableAttributesPath,&
                                                    cl_equivalencesPath,cl_globalAttributesPath,&
                                                    cl_dimensionsPath,cl_dimfile,cl_varname
         INTEGER(KIND=4)                         :: il_ji,il_jj,il_jk,il_status,il_file_id,il_nbfiledim,il_kindofdim,&
                                                    il_nbatt,il_RhVarId,il_varin_id
         INTEGER, DIMENSION(:), ALLOCATABLE      :: ila_dimsout_id,ila_filedimlen
         CHARACTER(LEN=255)                      :: cl_attvalue
         CHARACTER(LEN=18)                       :: cl_attname
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
         cl_fonction='MIOL_writef_field_C_1D_NC'
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
         !
         cl_varfile = TRIM(cl_variableAttributesPath)//TRIM(cd_varname)//'.in'
         !-----------------------------------------------------------------------
         ! Read dimensions
         il_status = fi_ncError(NF90_OPEN(TRIM(cd_filename), &
                                          NF90_WRITE, &
                                          il_file_id),cl_fonction)
         il_status = fi_ncError(NF90_REDEF(il_file_id),cl_fonction)
         il_status = fi_ncError(NF90_INQUIRE(il_file_id, &
                                             il_nbfiledim),cl_fonction)
         ALLOCATE(ila_filedimlen(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         ALLOCATE(cla_filedimname(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         DO il_ji = 1, il_nbfiledim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                          il_ji, &
                                                          cla_filedimname(il_ji), &
                                                          ila_filedimlen(il_ji)),cl_fonction)
         ENDDO
 
          !-----------------------------------------------------------------------
         ! Find dimensions
          ALLOCATE(ila_dimsout_id(2), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted') 
            READ(20, nb_dim) !'!
 
            ALLOCATE(cla_dimname(il_kindofdim), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
                       
                  ENDIF
 
               ENDDO
 
            ENDDO
 
            CLOSE(20)
 
            DEALLOCATE(cla_dimname, stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
         ENDDO
         !------------------------------------------------------------------------
         ! Write variable attributes
         INQUIRE(FILE=cl_varfile,EXIST=ll_tri)
         !** Si le fichier.in existe on prend la namelist sinon creation d'un fichier par defaut

         IF (ll_tri) THEN
               OPEN(20, file=cl_varfile, status='old', form='formatted')
               READ(20, nb_att)
               !-----------------------------------------------------------------------
               ! Define the variable
               il_status = NF90_INQ_VARID(il_file_id,cl_varname,il_RhVarId)
               IF(il_status /= NF90_NoErr) THEN      
                  il_status = fi_ncError(NF90_DEF_VAR(il_file_id, &
                                                      cl_varname, &
                                                      NF90_CHAR, &
                                                      ila_dimsout_id, &
                                                      il_varin_id),cl_fonction)
                  DO il_ji = 1, il_nbatt
                    READ(20, *) cl_attname
                    READ(20, '(A100)') cl_attvalue
                     il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                         il_varin_id, &
                                                         cl_attname, &
                                                         TRIM(cl_attvalue)),cl_fonction)
                  ENDDO
               ELSE
                  il_varin_id=il_RhVarId
               ENDIF
               CLOSE(20)
 
        ELSE
               il_status = fi_ncError(NF90_DEF_VAR(il_file_id, &
                                                   cd_varname, &
                                                   NF90_CHAR, &
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
                                             cda_varvalue),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Close file
 
         il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Memory deallocation
 
         IF (ALLOCATED(cla_filedimname)) DEALLOCATE (cla_filedimname, &
                                                     stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         IF (ALLOCATED(ila_filedimlen)) DEALLOCATE (ila_filedimlen, &
                                                    stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         IF (ALLOCATED(ila_dimsout_id)) DEALLOCATE (ila_dimsout_id, &
                                                    stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
 
 
        END SUBROUTINE MIOL_writef_field_C_1D_NC



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
         !! @param id_length           lenght of the char value
         !!
         !! History :
         !!        \n  01/2013   CREGNIER V3.5 MIOL
         !<
         !!======================================================================

       SUBROUTINE MIOL_writeu_field_C_1D_NC (id_filename, &
                                             cd_varname, &
                                             cd_key, &
                                             cda_varvalue, &
                                             id_length)
 
         
         USE MIOL_param
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER(KIND=4),             INTENT(IN) :: id_filename
         CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
         CHARACTER(LEN=2),            INTENT(IN) :: cd_key
         CHARACTER(LEN=id_length),DIMENSION(:),INTENT(IN) :: cda_varvalue
         INTEGER(KIND=4),             INTENT(IN) :: id_length
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         CHARACTER(LEN=256)                      :: cl_fonction,cl_varfile,cl_variableAttributesPath,&
                                                    cl_equivalencesPath,cl_globalAttributesPath,&
                                                    cl_dimensionsPath,cl_dimfile,cl_varname
         INTEGER(KIND=4)                         :: il_ji,il_jj,il_jk,il_status,il_nbfiledim,il_kindofdim,&
                                                    il_nbatt,il_RhVarId,il_varin_id
         CHARACTER(LEN=255)                      :: cl_attvalue
         CHARACTER(LEN=18)                       :: cl_attname
         INTEGER, DIMENSION(:), ALLOCATABLE      :: ila_dimsout_id,ila_filedimlen
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
         cl_fonction='MIOL_writef_field_C_1D_NC'
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
         !
         cl_varfile = TRIM(cl_variableAttributesPath)//TRIM(cd_varname)//'.in'
         !-----------------------------------------------------------------------
         ! Read dimensions
         il_status = fi_ncError(NF90_REDEF(id_filename),cl_fonction)
         il_status = fi_ncError(NF90_INQUIRE(id_filename, &
                                             il_nbfiledim),cl_fonction)
 
         ALLOCATE(ila_filedimlen(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         ALLOCATE(cla_filedimname(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)

         DO il_ji = 1, il_nbfiledim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_filename, &
                                                          il_ji, &
                                                          cla_filedimname(il_ji), &
                                                          ila_filedimlen(il_ji)),cl_fonction)
         ENDDO
          !-----------------------------------------------------------------------
         ! Find dimensions
          ALLOCATE(ila_dimsout_id(2), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted') 
            READ(20, nb_dim) !'!
 
            ALLOCATE(cla_dimname(il_kindofdim), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
                       
                  ENDIF
 
               ENDDO
 
            ENDDO
 
            CLOSE(20)
 
            DEALLOCATE(cla_dimname, stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
         ENDDO
         !------------------------------------------------------------------------
         ! Write variable attributes
         INQUIRE(FILE=cl_varfile,EXIST=ll_tri)
         !** Si le fichier.in existe on prend la namelist sinon creation d'un fichier par defaut

         IF (ll_tri) THEN
               OPEN(20, file=cl_varfile, status='old', form='formatted')
               READ(20, nb_att)
               !-----------------------------------------------------------------------
               ! Define the variable
               il_status = NF90_INQ_VARID(id_filename,cl_varname,il_RhVarId)
               IF(il_status /= NF90_NoErr) THEN      
                  il_status = fi_ncError(NF90_DEF_VAR(id_filename, &
                                                      cl_varname, &
                                                      NF90_CHAR, &
                                                      (/ila_dimsout_id(1),ila_dimsout_id(2)/), &
                                                      il_varin_id),cl_fonction)
                  DO il_ji = 1, il_nbatt
                    READ(20, *) cl_attname
                    READ(20, '(A100)') cl_attvalue
                     il_status = fi_ncError(NF90_PUT_ATT(id_filename, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      TRIM(cl_attvalue)),cl_fonction)
                  ENDDO
               ELSE
                  il_varin_id=il_RhVarId
               ENDIF
               CLOSE(20)
 
        ELSE
               il_status = fi_ncError(NF90_DEF_VAR(id_filename, &
                                                   cd_varname, &
                                                   NF90_CHAR, &
                                                   (/ila_dimsout_id(1),ila_dimsout_id(2)/), &
                                                   il_varin_id),cl_fonction)
         ENDIF
         !-----------------------------------------------------------------------
         ! Out of define mode
         il_status = fi_ncError(NF90_ENDDEF(id_filename),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Put array
         il_status = fi_ncError(NF90_PUT_VAR(id_filename, &
                                             il_varin_id, &
                                             cda_varvalue),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Memory deallocation
 
         IF (ALLOCATED(cla_filedimname)) DEALLOCATE (cla_filedimname, &
                                                     stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         IF (ALLOCATED(ila_filedimlen)) DEALLOCATE (ila_filedimlen, &
                                                    stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         IF (ALLOCATED(ila_dimsout_id)) DEALLOCATE (ila_dimsout_id, &
                                                    stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
 
 
        END SUBROUTINE MIOL_writeu_field_C_1D_NC

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
         !! @param id_length           lenght of the char value
         !!
         !! History :
         !!        \n  01/2013   CREGNIER V3.5 MIOL
         !<
         !!======================================================================

       SUBROUTINE MIOL_writeu_field_C_2D_NC (id_filename, &
                                             cd_varname, &
                                             cd_key, &
                                             cda_varvalue, &
                                             id_length)
 
         
         USE MIOL_param
         USE netcdf
         USE MFT_error
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER(KIND=4),             INTENT(IN) :: id_filename
         CHARACTER(LEN=*),            INTENT(IN) :: cd_varname
         CHARACTER(LEN=3),            INTENT(IN) :: cd_key
         CHARACTER(LEN=id_length),DIMENSION(:,:),INTENT(IN) :: cda_varvalue
         INTEGER(KIND=4),             INTENT(IN) :: id_length
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         CHARACTER(LEN=256)                      :: cl_fonction,cl_varfile,cl_variableAttributesPath,&
                                                    cl_equivalencesPath,cl_globalAttributesPath,&
                                                    cl_dimensionsPath,cl_dimfile,cl_varname
         INTEGER(KIND=4)                         :: il_ji,il_jj,il_jk,il_status,il_nbfiledim,il_kindofdim,&
                                                    il_nbatt,il_RhVarId,il_varin_id
         INTEGER, DIMENSION(:), ALLOCATABLE      :: ila_dimsout_id,ila_filedimlen
         CHARACTER(LEN=255)                      :: cl_attvalue
         CHARACTER(LEN=18)                       :: cl_attname
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
         cl_fonction='MIOL_writef_field_C_1D_NC'
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
         !
         cl_varfile = TRIM(cl_variableAttributesPath)//TRIM(cd_varname)//'.in'
         !-----------------------------------------------------------------------
         ! Read dimensions
         il_status = fi_ncError(NF90_REDEF(id_filename),cl_fonction)
         il_status = fi_ncError(NF90_INQUIRE(id_filename, &
                                             il_nbfiledim),cl_fonction)
 
         ALLOCATE(ila_filedimlen(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         ALLOCATE(cla_filedimname(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)

         DO il_ji = 1, il_nbfiledim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_filename, &
                                                          il_ji, &
                                                          cla_filedimname(il_ji), &
                                                          ila_filedimlen(il_ji)),cl_fonction)
         ENDDO
          !-----------------------------------------------------------------------
         ! Find dimensions
          ALLOCATE(ila_dimsout_id(3), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted') 
            READ(20, nb_dim) !'!
 
            ALLOCATE(cla_dimname(il_kindofdim), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
                       
                  ENDIF
 
               ENDDO
 
            ENDDO
 
            CLOSE(20)
 
            DEALLOCATE(cla_dimname, stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
         ENDDO
         !------------------------------------------------------------------------
         ! Write variable attributes
         INQUIRE(FILE=cl_varfile,EXIST=ll_tri)
         !** Si le fichier.in existe on prend la namelist sinon creation d'un fichier par defaut

         IF (ll_tri) THEN
               OPEN(20, file=cl_varfile, status='old', form='formatted')
               READ(20, nb_att)
               !-----------------------------------------------------------------------
               ! Define the variable
               il_status = NF90_INQ_VARID(id_filename,cl_varname,il_RhVarId)
               IF(il_status /= NF90_NoErr) THEN      
                  il_status = fi_ncError(NF90_DEF_VAR(id_filename, &
                                                      cl_varname, &
                                                      NF90_CHAR, &
                                                      (/ila_dimsout_id(1),ila_dimsout_id(2),ila_dimsout_id(3)/), &
                                                      il_varin_id),cl_fonction)
                  DO il_ji = 1, il_nbatt
                    READ(20, *) cl_attname
                    READ(20, '(A100)') cl_attvalue
                     il_status = fi_ncError(NF90_PUT_ATT(id_filename, &
                                                         il_varin_id, &
                                                         cl_attname, &
                                                         TRIM(cl_attvalue)),cl_fonction)
                  ENDDO
               ELSE
                  il_varin_id=il_RhVarId
               ENDIF
               CLOSE(20)
 
        ELSE
               !print *,'DEF var  :: ',ila_dimsout_id(1),id_length 
               il_status = fi_ncError(NF90_DEF_VAR(id_filename, &
                                                   cd_varname, &
                                                   NF90_CHAR, &
                                                   (/ila_dimsout_id(1),ila_dimsout_id(2),ila_dimsout_id(3)/), &
                                                   il_varin_id),cl_fonction)
         ENDIF
         !-----------------------------------------------------------------------
         ! Out of define mode
         il_status = fi_ncError(NF90_ENDDEF(id_filename),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Put array
         il_status = fi_ncError(NF90_PUT_VAR(id_filename, &
                                             il_varin_id, &
                                             cda_varvalue),cl_fonction)
 
         !-----------------------------------------------------------------------
         ! Memory deallocation
 
         IF (ALLOCATED(cla_filedimname)) DEALLOCATE (cla_filedimname, &
                                                     stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         IF (ALLOCATED(ila_filedimlen)) DEALLOCATE (ila_filedimlen, &
                                                    stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         IF (ALLOCATED(ila_dimsout_id)) DEALLOCATE (ila_dimsout_id, &
                                                    stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
 
 
        END SUBROUTINE MIOL_writeu_field_C_2D_NC

