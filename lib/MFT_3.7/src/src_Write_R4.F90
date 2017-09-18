!> \brief Module which contain subroutine for Write R4 values in NETCDF format
!! \n  Total 10 functions
!! \author F.MESSAL first version
!! \date 11/2006
!!  \version 1.1 
!! \author C.REGNIER Miol V3.5
!! \date 01/2013
!!  \version 3.5
!<
!! \n     
!MODULE src_Write_R4
!**-----------------------------------------
!** Source for Write R4 values in NETCDF format
!** C.REGNIER Janvier 2013 V3.5 Miol
!**-------------------------------------------
! --------------------------------------- General interface ----------------------------------------------------------------------------
! --- SUBROUTINE MIOL_writef_field_R4_5D_NC (cd_filename,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
! --- SUBROUTINE MIOL_writef_field_R4_4D_NC (cd_filename,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
! --- SUBROUTINE MIOL_writef_field_R4_3D_NC (cd_filename,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
! --- SUBROUTINE MIOL_writef_field_R4_2D_NC (cd_filename,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
! --- SUBROUTINE MIOL_writef_field_R4_1D_NC (cd_filename,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
! --- SUBROUTINE MIOL_writeu_field_R4_5D_NC (id_file_id,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
! --- SUBROUTINE MIOL_writeu_field_R4_4D_NC (id_file_id,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
! --- SUBROUTINE MIOL_writeu_field_R4_3D_NC (id_file_id,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
! --- SUBROUTINE MIOL_writeu_field_R4_2D_NC (id_file_id,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
! --- SUBROUTINE MIOL_writeu_field_R4_1D_NC (id_file_id,cd_varname,cd_key,rda_varvalue,rda_offsetvalue,rda_specialvalue)
!-----------------------------------------------------------------------------------------------------------------------------------------
!CONTAINS
         !!======================================================================
         !> \brief 
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param cd_filename         A NetCDF filename. You must specify the complete
         !!                       path.
         !! @param cd_varname          The variable name.
         !! @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param rda_varvalue        The data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values  
         !! @param rda_specialvalues   Vector of special values of the variable:
         !!                                            spvalue(1) = minvalue
         !!                                            spvalue(2) = maxvalue
         !!                                            spvalue(3) = fillvalue
         !!                                            spvalue(4) = missvalue
         !!
         !! History :
         !!        \n  01/2013   CREGNIER V3.5 MIOL
         !<
         !!====================================================================== 

       SUBROUTINE MIOL_writef_field_R4_5D_NC (cd_filename, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
 
         USE MFT_error
         USE MIOL_param
         USE netcdf
         
         USE MFT_Inf_NaN_detection
         IMPLICIT NONE
!         USE INT_ATTRIBUTS
 
          !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
         CHARACTER(LEN=5),                 INTENT(IN) :: cd_key
         REAL(KIND=4), DIMENSION(:,:,:,:,:), INTENT(IN) :: rda_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         REAL(KIND=4), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_file_id, il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         REAL(KIND=4) :: rl_missvalue, rl_minvalue, rl_maxvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue,rl_fillvalue
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
         
         NAMELIST /nb_dim/ il_kindofdim
         NAMELIST /nb_att/ il_nbatt,& 
                           cl_varname
         
         cl_fonction='MIOL_writef_field_R4_4D_NC'

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
         rl_minvalue = -rg_fillvalue
         rl_maxvalue = rg_fillvalue
         rl_missvalue = rg_fillvalue
         rl_fillvalue=rg_fillvalue
 
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
         ALLOCATE(ila_dimsout_id(5), stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
 
                    ! IF (ila_filedimlen(il_jk).NE.SIZE(rda_varvalue, dim=il_ji)) THEN
                    !    WRITE(0,*) ' MIOL_write_field_R4_4D_NC : dimensions error. '
                    !    CALL flush(0)
                    !    STOP
                    ! ENDIF
 
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
 
         IF (.NOT.(PRESENT(rda_specialvalue))) THEN
            rl_minvalue = MINVAL(array=rda_varvalue,mask=rda_varvalue .NE.sNaN.AND.rda_varvalue.NE.rg_flagvalue)
            rl_maxvalue = MAXVAL(array=rda_varvalue, &
                                      mask=rda_varvalue .NE. rl_fillvalue .AND.rda_varvalue .NE.sNaN &
                                      & .AND.rda_varvalue .NE.rg_flagvalue)
          ELSE
            rl_minvalue = rda_specialvalue(1)
            rl_maxvalue = rda_specialvalue(2)
            rl_fillvalue = rda_specialvalue(3)
            rl_missvalue = rda_specialvalue(4)
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
                                             NF90_FLOAT, &
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
                                                      rl_minvalue),cl_fonction)
 
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_maxvalue),cl_fonction)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_fillvalue),cl_fonction)
 
               
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
                                                      rl_missvalue),cl_fonction)
               CASE ('flagvalues')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rg_flagvalue),cl_fonction)
                  
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
                                                NF90_FLOAT, &
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
                                             rda_varvalue),cl_fonction)
 
 
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
 
 
       END SUBROUTINE MIOL_writef_field_R4_5D_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************

         !!======================================================================
         !> \brief 
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param cd_filename         A NetCDF filename. You must specify the complete
         !!                       path.
         !! @param cd_varname          The variable name.
         !! @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param rda_varvalue        The data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values  
         !! @param rda_specialvalues   Vector of special values of the variable:
         !!                                            spvalue(1) = minvalue
         !!                                            spvalue(2) = maxvalue
         !!                                            spvalue(3) = fillvalue
         !!                                            spvalue(4) = missvalue
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version 1.0
         !!        \n  01/2013   CREGNIER V3.5 MIOL
         !<
         !!====================================================================== 

       SUBROUTINE MIOL_writef_field_R4_4D_NC (cd_filename, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
 
         USE MFT_error
         USE MIOL_param
         USE netcdf
         
         USE MFT_Inf_NaN_detection
         IMPLICIT NONE
!         USE INT_ATTRIBUTS
 
          !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
         CHARACTER(LEN=4),                 INTENT(IN) :: cd_key
         REAL(KIND=4), DIMENSION(:,:,:,:), INTENT(IN) :: rda_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         REAL(KIND=4), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_file_id, il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         REAL(KIND=4) :: rl_missvalue, rl_minvalue, rl_maxvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue,rl_fillvalue
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
         
         NAMELIST /nb_dim/ il_kindofdim
         NAMELIST /nb_att/ il_nbatt,& 
                           cl_varname
         
         cl_fonction='MIOL_writef_field_R4_4D_NC'

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
         rl_minvalue = -rg_fillvalue
         rl_maxvalue = rg_fillvalue
         rl_missvalue = rg_fillvalue
         rl_fillvalue=rg_fillvalue
 
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
         ALLOCATE(ila_dimsout_id(4), stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
 
                    ! IF (ila_filedimlen(il_jk).NE.SIZE(rda_varvalue, dim=il_ji)) THEN
                    !    WRITE(0,*) ' MIOL_write_field_R4_4D_NC : dimensions error. '
                    !    CALL flush(0)
                    !    STOP
                    ! ENDIF
 
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
 
         IF (.NOT.(PRESENT(rda_specialvalue))) THEN
            rl_minvalue = MINVAL(array=rda_varvalue,mask=rda_varvalue .NE.sNaN.AND.rda_varvalue.NE.rg_flagvalue)
            rl_maxvalue = MAXVAL(array=rda_varvalue, &
                                      mask=rda_varvalue .NE. rl_fillvalue .AND.rda_varvalue .NE.sNaN &
                                      & .AND.rda_varvalue .NE.rg_flagvalue)
          ELSE
            rl_minvalue = rda_specialvalue(1)
            rl_maxvalue = rda_specialvalue(2)
            rl_fillvalue = rda_specialvalue(3)
            rl_missvalue = rda_specialvalue(4)
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
                                             NF90_FLOAT, &
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
                                                      rl_minvalue),cl_fonction)
 
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_maxvalue),cl_fonction)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_fillvalue),cl_fonction)
 
               
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
                                                      rl_missvalue),cl_fonction)
               CASE ('flagvalues')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rg_flagvalue),cl_fonction)
                  
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
                                                NF90_FLOAT, &
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
                                             rda_varvalue),cl_fonction)
 
 
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
 
 
       END SUBROUTINE MIOL_writef_field_R4_4D_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************

         !!======================================================================
         !> \brief 
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param cd_filename         A NetCDF filename. You must specify the complete
         !!                       path.
         !! @param cd_varname          The variable name.
         !! @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param rda_varvalue        The data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values  
         !! @param rda_specialvalues   Vector of special values of the variable:
         !!                                            spvalue(1) = minvalue
         !!                                            spvalue(2) = maxvalue
         !!                                            spvalue(3) = fillvalue
         !!                                            spvalue(4) = missvalue
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version 1.0
         !!        \n  01/2013   CREGNIER V3.5 MIOL
         !<
         !!======================================================================
  
       SUBROUTINE MIOL_writef_field_R4_3D_NC (cd_filename, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
         
         USE MIOL_param
         USE MFT_error
!         USE INT_ATTRIBUTS
         USE netcdf
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
         CHARACTER(LEN=3),                 INTENT(IN) :: cd_key
         REAL(KIND=4), DIMENSION(:,:,:),   INTENT(IN) :: rda_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         REAL(KIND=4), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_file_id, il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         REAL(KIND=4) :: rl_missvalue, rl_minvalue, rl_maxvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue,rl_fillvalue
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
 
         cl_fonction="MIOL_writef_field_R4_3D_NC"
         !WRITE(0,*) ' file = ',TRIM(cp_miolParameterFile)
         CALL flush(0)
        
         IF (cl_miolUserParameterFile /= '') THEN 
            OPEN(20, file=cl_miolUserParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterUserPaths)
            CLOSE(20)
          !WRITE(0,*) ' file = ',TRIM(cl_miolUserParameterFile)
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
         rl_minvalue = -rg_fillvalue
         rl_maxvalue = rg_fillvalue
         rl_missvalue = rg_fillvalue
         rl_fillvalue=rg_fillvalue
          
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
         ALLOCATE(ila_dimsout_id(LEN(cd_key)), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml' 
            CALL flush(0)
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
 
                    ! IF (ila_filedimlen(il_jk).NE.SIZE(rda_varvalue, dim=il_ji)) THEN
                    !    WRITE(0,*) ' MIOL_write_field_R4_3D_NC : dimensions error. '
                    !    CALL flush(0)
                    !    STOP
                    ! ENDIF
 
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
 
         IF (.NOT.(PRESENT(rda_specialvalue))) THEN
           rl_minvalue = MINVAL(array=rda_varvalue,mask=rda_varvalue .NE.sNaN.AND.rda_varvalue .NE.rg_flagvalue)
           rl_maxvalue = MAXVAL(array=rda_varvalue, &
                                 mask=rda_varvalue .NE. rl_fillvalue .AND.rda_varvalue .NE.sNaN &
                                 & .AND.rda_varvalue .NE.rg_flagvalue)
         ELSE
            rl_minvalue = rda_specialvalue(1)
            rl_maxvalue = rda_specialvalue(2)
          !  rl_fillvalue = rda_specialvalue(3)
            rl_missvalue = rda_specialvalue(4)
         ENDIF
 
 
         !------------------------------------------------------------------------
         ! Write variable attributes
 
 
         CALL flush(0)
         INQUIRE(FILE=cl_varfile,EXIST=ll_tri)
         !** Si le fichier.in existe on prend la namelist sinon creation d'un fichier par defaut

         IF (ll_tri) THEN
            OPEN(20, file=cl_varfile, status='old', form='formatted')
            READ(20, nb_att)

         !-----------------------------------------------------------------------
         ! Define the variable
 
         il_status = fi_ncError(NF90_DEF_VAR(il_file_id, &
                                             cl_varname, &
                                             NF90_FLOAT, &
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
                                                      rl_minvalue),cl_fonction)
 
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_maxvalue),cl_fonction)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_fillvalue),cl_fonction)
 
               
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
                                                      rl_missvalue),cl_fonction)
 
               CASE ('flagvalues')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rg_flagvalue),cl_fonction)

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
                                                NF90_FLOAT, &
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
                                             rda_varvalue),cl_fonction)
 
 
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
 
 
       END SUBROUTINE MIOL_writef_field_R4_3D_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
         !!======================================================================
         !> \brief
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param cd_filename         A NetCDF filename. You must specify the complete
         !!                       path.
         !! @param cd_varname          The variable name.
         !! @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param rda_varvalue        The data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values  
         !! @param rda_specialvalues   Vector of special values of the variable:
         !!                                            spvalue(1) = minvalue
         !!                                            spvalue(2) = maxvalue
         !!                                            spvalue(3) = fillvalue
         !!                                            spvalue(4) = missvalue
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version 1.0
         !!        \n  01/2013   CREGNIER V3.5 MIOL
         !<
         !!======================================================================
       SUBROUTINE MIOL_writef_field_R4_2D_NC (cd_filename, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
 
         
         USE MIOL_param
         USE MFT_error
    !     USE INT_ATTRIBUTS
         USE netcdf
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
         CHARACTER(LEN=2),                 INTENT(IN) :: cd_key
         REAL(KIND=4), DIMENSION(:,:),     INTENT(IN) :: rda_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         REAL(KIND=4), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_file_id, il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         REAL(KIND=4) :: rl_missvalue, rl_minvalue, rl_maxvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue,rl_fillvalue
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
         NAMELIST/nb_dim/il_kindofdim
         NAMELIST /nb_att/ il_nbatt,& 
                           cl_varname
 
         cl_fonction="MIOL_writef_field_R4_2D_NC"

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
         rl_minvalue = -rg_fillvalue
         rl_maxvalue = rg_fillvalue
         rl_missvalue = rg_fillvalue
         rl_fillvalue=rg_fillvalue

 
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
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
 
                     !IF (ila_filedimlen(il_jk).NE.SIZE(rda_varvalue, dim=il_ji)) THEN
                     !   WRITE(0,*) ' MIOL_write_field_R4_2D_NC : dimensions error. '
                     !   CALL flush(0)
                     !   STOP
                     !ENDIF
 
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
            rl_scalevalue=1 
            rl_offsetvalue=0
          ENDIF

         !-----------------------------------------------------------------------
         ! Find the minimum and maximum values 
 
         IF (.NOT.(PRESENT(rda_specialvalue))) THEN
            rl_minvalue = MINVAL(array=rda_varvalue,mask=rda_varvalue .NE.sNaN.AND.rda_varvalue .NE.rg_flagvalue)
            rl_maxvalue = MAXVAL(array=rda_varvalue, &
             mask=rda_varvalue .NE.rl_fillvalue.AND.rda_varvalue.NE.sNaN.AND.rda_varvalue.NE.rg_flagvalue)
         ELSE
            rl_minvalue = rda_specialvalue(1)
            rl_maxvalue = rda_specialvalue(2)
            rl_fillvalue = rda_specialvalue(3)
            rl_missvalue = rda_specialvalue(4)
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
                                             NF90_FLOAT, &
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
                                                      rl_minvalue),cl_fonction)
 
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_maxvalue),cl_fonction)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_fillvalue),cl_fonction)
 
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
                                                      rl_missvalue),cl_fonction)
 
               CASE ('stepvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rda_varvalue(2,2)-rda_varvalue(1,1)),cl_fonction) 

                CASE ('flagvalues')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rg_flagvalue),cl_fonction)
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
                                                NF90_FLOAT, &
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
                                             rda_varvalue),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Close file
 
         il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Memory deallocation
 
         IF (ALLOCATED(cla_filedimname)) DEALLOCATE (cla_filedimname, &
                                                     stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname,cl_fonction',cl_fonction)
 
 
         IF (ALLOCATED(ila_filedimlen)) DEALLOCATE (ila_filedimlen, &
                                                    stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
 
         IF (ALLOCATED(ila_dimsout_id)) DEALLOCATE (ila_dimsout_id, &
                                                    stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout_id',cl_fonction)
 
 
        END SUBROUTINE MIOL_writef_field_R4_2D_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
         !!======================================================================
         !> \brief
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param cd_filename         A NetCDF filename. You must specify the complete
         !!                       path.
         !! @param cd_varname          The variable name.
         !! @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param rda_varvalue        The data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values  
         !! @param rda_specialvalues   Vector of special values of the variable:
         !!                                            spvalue(1) = minvalue
         !!                                            spvalue(2) = maxvalue
         !!                                            spvalue(3) = fillvalue
         !!                                            spvalue(4) = missvalue
         !!                                            spvalue(5) = stepvalue
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version 1.0
         !!        \n  01/2013   CREGNIER V3.5 MIOL
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_writef_field_R4_1D_NC (cd_filename, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
 
         USE netcdf
         USE MIOL_param
         
         USE MFT_error
        ! USE INT_ATTRIBUTS
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
         CHARACTER(LEN=1),                 INTENT(IN) :: cd_key
         REAL(KIND=4), DIMENSION(:),       INTENT(IN) :: rda_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         REAL(KIND=4), DIMENSION(5),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_file_id, il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         REAL(KIND=4) :: rl_missvalue, rl_minvalue, rl_maxvalue, &
                         rl_stepvalue
         REAL(KIND=4) :: rl_scalevalue,rl_offsetvalue,rl_fillvalue
         CHARACTER(LEN=255) :: cl_attvalue,cl_fonction
         CHARACTER(LEN=18) :: cl_attname
         INTEGER :: il_ji, il_jj, il_jk, il_status, il_kindofdim,il_RhVarId
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
 
         cl_fonction="MIOL_writef_field_R4_1D_NC"
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
         rl_minvalue = -rg_fillvalue
         rl_maxvalue = rg_fillvalue
         rl_missvalue = rg_fillvalue
         rl_stepvalue = rg_fillvalue
         rl_fillvalue=rg_fillvalue
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
         ALLOCATE(ila_dimsout_id(1), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
                       
                    !IF (ila_filedimlen(il_jk).NE.SIZE(rda_varvalue, dim=il_ji)) THEN
                    !   WRITE(0,*) ' MIOL_write_field_R4_1D_NC : dimensions error. '
                    !    PRINT *,'ila_filedimlen(il_jk) SIZE(rda_varvalue, dim=il_ji ::',ila_filedimlen(il_jk),SIZE(rda_varvalue, dim=il_ji)
                    !   CALL flush(0)
                    !   STOP
                    !ENDIF
 
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

         IF (.NOT.(PRESENT(rda_specialvalue))) THEN
            rl_minvalue = MINVAL(array=rda_varvalue,mask=rda_varvalue .NE.sNaN.AND.rda_varvalue .NE.rg_flagvalue)
            rl_maxvalue = MAXVAL(array=rda_varvalue, &
                                 mask=rda_varvalue .NE. rl_fillvalue.AND.rda_varvalue .NE.sNaN &
                                 .AND.rda_varvalue .NE.rg_flagvalue)
         ELSE
            rl_minvalue = rda_specialvalue(1)
            rl_maxvalue = rda_specialvalue(2)
            rl_fillvalue = rda_specialvalue(3)
            rl_missvalue = rda_specialvalue(4)
            rl_stepvalue = rda_specialvalue(5)
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
         il_status = NF90_INQ_VARID(il_file_id,cl_varname,il_RhVarId)
         IF(il_status /= NF90_NoErr) THEN      
            il_status = fi_ncError(NF90_DEF_VAR(il_file_id, &
                                                cl_varname, &
                                                NF90_FLOAT, &
                                                ila_dimsout_id, &
                                                il_varin_id),cl_fonction)
            
         ELSE
            il_varin_id=il_RhVarId
         ENDIF
         DO il_ji = 1, il_nbatt
 
            READ(20, *) cl_attname
            READ(20, '(A100)') cl_attvalue
 
            SELECTCASE (cl_attvalue)
 
            CASE ('minvalue')
               il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                   il_varin_id, &
                                                   cl_attname, &
                                                   rl_minvalue),cl_fonction)
               
            CASE ('maxvalue')
               il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                   il_varin_id, &
                                                   cl_attname, &
                                                   rl_maxvalue),cl_fonction)
 
            CASE ('fillvalue')
               il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                   il_varin_id, &
                                                   cl_attname, &
                                                   rl_fillvalue),cl_fonction)
               
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
                                                   rl_missvalue),cl_fonction)
            CASE ('stepvalue')
               il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                   il_varin_id, &
                                                   cl_attname, &
                                                   rl_stepvalue),cl_fonction)
 
            CASE ('flagvalues')
               il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                   il_varin_id, &
                                                   cl_attname, &
                                                   rg_flagvalue),cl_fonction)
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
                                                NF90_FLOAT, &
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
                                             rda_varvalue),cl_fonction)
 
 
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
 
 
         END SUBROUTINE MIOL_writef_field_R4_1D_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 

 !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
         !!======================================================================
         !> \brief
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param cd_filename         A NetCDF filename. You must specify the complete
         !!                       path.
         !! @param cd_varname          The variable name.
         !! @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param rd_varvalue        The SCALAR data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values  
         !! @param rda_specialvalues   Vector of special values of the variable:
         !!                                            spvalue(1) = minvalue
         !!                                            spvalue(2) = maxvalue
         !!                                            spvalue(3) = fillvalue
         !!                                            spvalue(4) = missvalue
         !!                                            spvalue(5) = stepvalue
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version 1.0
         !!        \n  01/2013   CREGNIER V3.5 MIOL
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_writef_field_R4_scalar_NC (cd_filename, &
                                           cd_varname, &
                                           cd_key, &
                                           rd_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
 
         USE netcdf
         USE MIOL_param
         
         USE MFT_error
        ! USE INT_ATTRIBUTS
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_filename
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
         CHARACTER(LEN=1),                 INTENT(IN) :: cd_key
         REAL(KIND=4),                     INTENT(IN) :: rd_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         REAL(KIND=4), DIMENSION(5),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_file_id, il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         REAL(KIND=4) :: rl_missvalue, rl_minvalue, rl_maxvalue, &
                         rl_stepvalue
         REAL(KIND=4) :: rl_scalevalue,rl_offsetvalue,rl_fillvalue
         CHARACTER(LEN=255) :: cl_attvalue,cl_fonction
         CHARACTER(LEN=18) :: cl_attname
         INTEGER :: il_ji, il_jj, il_jk, il_status, il_kindofdim,il_RhVarId
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
         cl_fonction="MIOL_write_field_R4_scalar_NC"
 
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
         rl_minvalue = -rg_fillvalue
         rl_maxvalue = rg_fillvalue
         rl_missvalue = rg_fillvalue
         rl_stepvalue = rg_fillvalue
         rl_fillvalue=rg_fillvalue

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
         ALLOCATE(ila_dimsout_id(1), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
 
                     IF (ila_filedimlen(il_jk).NE.1) THEN
                        WRITE(0,*) ' MIOL_write_field_R4_scalar_NC : dimensions error. '
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

         IF (.NOT.(PRESENT(rda_specialvalue))) THEN
            rl_minvalue = 0
            rl_maxvalue = 0
         ELSE
            rl_minvalue = rda_specialvalue(1)
            rl_maxvalue = rda_specialvalue(2)
            rl_fillvalue = rda_specialvalue(3)
            rl_missvalue = rda_specialvalue(4)
            rl_stepvalue = rda_specialvalue(5)
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
         il_status = NF90_INQ_VARID(il_file_id,cl_varname,il_RhVarId)
         IF(il_status /= NF90_NoErr) THEN      
            WRITE(0,*) 'Variable not define'
            il_status = fi_ncError(NF90_DEF_VAR(il_file_id, &
                                                cl_varname, &
                                                NF90_FLOAT, &
                                                ila_dimsout_id, &
                                                il_varin_id),cl_fonction)
            
         ELSE
            il_varin_id=il_RhVarId
         ENDIF

         DO il_ji = 1, il_nbatt
 
            READ(20, *) cl_attname
            READ(20, '(A100)') cl_attvalue
 
            SELECTCASE (cl_attvalue)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                     rl_fillvalue),cl_fonction)
 
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
               CASE ('minvalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rd_varvalue),cl_fonction) 
               CASE ('maxvalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rd_varvalue),cl_fonction) 
               CASE ('missvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_missvalue),cl_fonction)
               CASE ('stepvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_stepvalue),cl_fonction)
 
               CASE ('flagvalues')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rg_flagvalue),cl_fonction)
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
                                                NF90_FLOAT, &
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
                                             rd_varvalue),cl_fonction)
 
 
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
 
 
         END SUBROUTINE MIOL_writef_field_R4_scalar_NC

 !******************************************************************************
  !******************************************************************************
  !******************************************************************************


         !!======================================================================
         !> \brief
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param id_file_id          A NetCDF file Id.
         !! @param cd_varname          The variable name.
         !! @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param rda_varvalue        The data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values  
         !! @param rda_specialvalues   Vector of special values of the variable:
         !!                                            spvalue(1) = minvalue
         !!                                            spvalue(2) = maxvalue
         !!                                            spvalue(3) = fillvalue
         !!                                            spvalue(4) = missvalue
         !!
         !! History :
         !!        \n  01/2013   CREGNIER V3.5 MIOL
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_writeu_field_R4_5D_NC (id_file_id, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
 
         USE netcdf
         USE MIOL_param
         
         USE MFT_error
        ! USE INT_ATTRIBUTS
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,                          INTENT(IN) :: id_file_id
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
         CHARACTER(LEN=5),                 INTENT(IN) :: cd_key
         REAL(KIND=4), DIMENSION(:,:,:,:,:), INTENT(IN) :: rda_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         REAL(KIND=4), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         REAL(KIND=4) :: rl_missvalue, rl_minvalue, rl_maxvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue,rl_fillvalue
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
         NAMELIST /nb_dim/ il_kindofdim
         NAMELIST /nb_att/ il_nbatt,& 
                           cl_varname
 
         cl_fonction="MIOL_writeu_field_R4_4D_NC"

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
         rl_minvalue = -rg_fillvalue
         rl_maxvalue = rg_fillvalue
         rl_missvalue = rg_fillvalue
         rl_fillvalue=rg_fillvalue
           
         !-----------------------------------------------------------------------
         ! Read dimensions
 
         il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE(id_file_id, &
                                             il_nbfiledim),cl_fonction)
 
         ALLOCATE(ila_filedimlen(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         ALLOCATE(cla_filedimname(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         DO il_ji = 1, il_nbfiledim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id, &
                                                          il_ji, &
                                                          cla_filedimname(il_ji), &
                                                          ila_filedimlen(il_ji)),cl_fonction)
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find dimensions
         ALLOCATE(ila_dimsout_id(5), stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
                     !WRITE(0,*) 'cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk)) :: ',cla_dimname(il_jj),cla_filedimname(il_jk)
                     !IF (ila_filedimlen(il_jk).NE.SIZE(rda_varvalue, dim=il_ji)) THEN
                     !   WRITE(0,*) ' MIOL_write_field_R4_4D_NC : dimensions error. '
                     !   CALL flush(0)
                     !   STOP
                     !ENDIF
 
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
 
         IF (.NOT.(PRESENT(rda_specialvalue))) THEN
            rl_minvalue = MINVAL(array=rda_varvalue,mask=rda_varvalue .NE.sNaN.AND.rda_varvalue .NE.rg_flagvalue)
            rl_maxvalue = MAXVAL(array=rda_varvalue, &
                 mask=rda_varvalue .NE. rl_fillvalue .AND.rda_varvalue .NE.sNaN.AND.rda_varvalue .NE.rg_flagvalue)
         ELSE
            rl_minvalue = rda_specialvalue(1)
            rl_maxvalue = rda_specialvalue(2)
            rl_fillvalue = rda_specialvalue(3)
            rl_missvalue = rda_specialvalue(4)
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
                                             NF90_FLOAT, &
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
                                                      rl_minvalue),cl_fonction)
 
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_maxvalue),cl_fonction)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_fillvalue),cl_fonction)
 
               
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
                                                      rl_missvalue),cl_fonction)
 
                CASE ('flagvalues')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rg_flagvalue),cl_fonction)
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
                                                NF90_FLOAT, &
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
                                             rda_varvalue),cl_fonction)
 
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
 
 
       END SUBROUTINE MIOL_writeu_field_R4_5D_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************


         !!======================================================================
         !> \brief
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param id_file_id          A NetCDF file Id.
         !! @param cd_varname          The variable name.
         !! @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param rda_varvalue        The data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values  
         !! @param rda_specialvalues   Vector of special values of the variable:
         !!                                            spvalue(1) = minvalue
         !!                                            spvalue(2) = maxvalue
         !!                                            spvalue(3) = fillvalue
         !!                                            spvalue(4) = missvalue
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version 1.0
         !!        \n  01/2013   CREGNIER V3.5 MIOL
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_writeu_field_R4_4D_NC (id_file_id, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
 
         USE netcdf
         USE MIOL_param
         
         USE MFT_error
        ! USE INT_ATTRIBUTS
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,                          INTENT(IN) :: id_file_id
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
         CHARACTER(LEN=4),                 INTENT(IN) :: cd_key
         REAL(KIND=4), DIMENSION(:,:,:,:), INTENT(IN) :: rda_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         REAL(KIND=4), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         REAL(KIND=4) :: rl_missvalue, rl_minvalue, rl_maxvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue,rl_fillvalue
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
         NAMELIST /nb_dim/ il_kindofdim
         NAMELIST /nb_att/ il_nbatt,& 
                           cl_varname
 
         cl_fonction="MIOL_writeu_field_R4_4D_NC"

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
         rl_minvalue = -rg_fillvalue
         rl_maxvalue = rg_fillvalue
         rl_missvalue = rg_fillvalue
         rl_fillvalue=rg_fillvalue
           
         !-----------------------------------------------------------------------
         ! Read dimensions
 
         il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE(id_file_id, &
                                             il_nbfiledim),cl_fonction)
 
         ALLOCATE(ila_filedimlen(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         ALLOCATE(cla_filedimname(il_nbfiledim), &
                  stat=il_status)
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
         il_status = fi_memError(il_status, ' ila_dimsout',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
 
                     !IF (ila_filedimlen(il_jk).NE.SIZE(rda_varvalue, dim=il_ji)) THEN
                     !   WRITE(0,*) ' MIOL_write_field_R4_4D_NC : dimensions error. '
                     !   CALL flush(0)
                     !   STOP
                     !ENDIF
 
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
 
         IF (.NOT.(PRESENT(rda_specialvalue))) THEN
            rl_minvalue = MINVAL(array=rda_varvalue,mask=rda_varvalue .NE.sNaN.AND.rda_varvalue .NE.rg_flagvalue)
            rl_maxvalue = MAXVAL(array=rda_varvalue, &
                 mask=rda_varvalue .NE. rl_fillvalue .AND.rda_varvalue .NE.sNaN.AND.rda_varvalue .NE.rg_flagvalue)
         ELSE
            rl_minvalue = rda_specialvalue(1)
            rl_maxvalue = rda_specialvalue(2)
            rl_fillvalue = rda_specialvalue(3)
            rl_missvalue = rda_specialvalue(4)
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
                                             NF90_FLOAT, &
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
                                                      rl_minvalue),cl_fonction)
 
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_maxvalue),cl_fonction)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_fillvalue),cl_fonction)
 
               
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
                                                      rl_missvalue),cl_fonction)
 
                CASE ('flagvalues')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rg_flagvalue),cl_fonction)
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
                                                NF90_FLOAT, &
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
                                             rda_varvalue),cl_fonction)
 
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
 
 
       END SUBROUTINE MIOL_writeu_field_R4_4D_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
         !!======================================================================
         !> \brief
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param id_file_id          A NetCDF file Id.
         !! @param cd_varname          The variable name.
         !! @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param rda_varvalue        The data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values  
         !! @param rda_specialvalues   Vector of special values of the variable:
         !!                                            spvalue(1) = minvalue
         !!                                            spvalue(2) = maxvalue
         !!                                            spvalue(3) = fillvalue
         !!                                            spvalue(4) = missvalue
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version 1.0
         !!        \n  01/2013   CREGNIER V3.5 MIOL
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_writeu_field_R4_3D_NC (id_file_id, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
 
         USE netcdf
         USE MIOL_param
         
         USE MFT_error
       !  USE INT_ATTRIBUTS
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,                          INTENT(IN) :: id_file_id
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
         CHARACTER(LEN=3),                 INTENT(IN) :: cd_key
         REAL(KIND=4), DIMENSION(:,:,:),   INTENT(IN) :: rda_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         REAL(KIND=4), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         REAL(KIND=4) :: rl_missvalue, rl_minvalue, rl_maxvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue,rl_fillvalue
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
         cl_fonction="MIOL_writeu_field_R4_3D_NC"
         CALL flush(0)
         IF (cl_miolUserParameterFile /= '') THEN 
            OPEN(20, file=cl_miolUserParameterFile, &
                 status='old', &
                 form='formatted')
            READ(20, miolParameterUserPaths)
            CLOSE(20)
         WRITE(0,*) ' file = ',TRIM(cl_miolUserParameterFile)
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
         rl_minvalue = -rg_fillvalue
         rl_maxvalue = rg_fillvalue
         rl_missvalue = rg_fillvalue
         rl_fillvalue=rg_fillvalue
         !-----------------------------------------------------------------------
         ! Read dimensions
         !PRINT *,'Read dimensions' 
         il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE(id_file_id, &
                                             il_nbfiledim),cl_fonction)
 
         ALLOCATE(ila_filedimlen(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen,cl_fonction',cl_fonction)
 
         ALLOCATE(cla_filedimname(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         DO il_ji = 1, il_nbfiledim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id, &
                                                          il_ji, &
                                                          cla_filedimname(il_ji), &
                                                          ila_filedimlen(il_ji)),cl_fonction)
         ENDDO

         !-----------------------------------------------------------------------
         ! Find dimensions
         ALLOCATE(ila_dimsout_id(LEN(cd_key)), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
 
            !WRITE(0,*) ' file = ',TRIM(cl_dimfile)
            CALL flush(0)
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
 
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
 
                     !IF (ila_filedimlen(il_jk).NE.SIZE(rda_varvalue, dim=il_ji)) THEN
                     !   WRITE(0,*) ' MIOL_write_field_R4_3D_NC : dimensions error. '
                     !   CALL flush(0)
                     !   STOP
                     !ENDIF
 
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
         !PRINT *,"Find min and max value"
         IF (.NOT.(PRESENT(rda_specialvalue))) THEN
             rl_minvalue = MINVAL(array=rda_varvalue,mask=rda_varvalue.NE.sNaN.AND.rda_varvalue.NE.rg_flagvalue)
             rl_maxvalue = MAXVAL(array=rda_varvalue, &
                 mask=rda_varvalue.NE.rl_fillvalue.AND.rda_varvalue.NE.sNaN.AND.rda_varvalue.NE.rg_flagvalue)
 ! rl_minvalue = MINVAL(array=rda_varvalue,mask=rda_varvalue.NE.sNaN.AND.rda_varvalue .NE.rg_flagvalue)
           ! rl_maxvalue = MAXVAL(array=rda_varvalue, &
           !      mask=rda_varvalue .NE. rl_fillvalue .AND.rda_varvalue.NE.sNaN.AND.rda_varvalue.NE.rg_flagvalue)
        
            
         ELSE
            rl_minvalue = rda_specialvalue(1)
            rl_maxvalue = rda_specialvalue(2)
            rl_fillvalue = rda_specialvalue(3)
            rl_missvalue = rda_specialvalue(4)
         ENDIF
 
 
         !------------------------------------------------------------------------
         ! Write variable attributes
 
 
         !WRITE(0,*) ' file = ',TRIM(cl_varfile)
         CALL flush(0)
         INQUIRE(FILE=cl_varfile,EXIST=ll_tri)
         !** Si le fichier.in existe on prend la namelist sinon creation d'un fichier par defaut

         IF (ll_tri) THEN
            OPEN(20, file=cl_varfile, status='old', form='formatted')
            READ(20, nb_att)

         !-----------------------------------------------------------------------
         ! Define the variable
         il_status = fi_ncError(NF90_DEF_VAR(id_file_id, &
                                             TRIM(cl_varname), &
                                             NF90_FLOAT, &
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
                                                      rl_minvalue),cl_fonction)
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_maxvalue),cl_fonction)
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_fillvalue),cl_fonction)
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
                                                      rl_missvalue),cl_fonction)
               CASE ('flagvalues')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rg_flagvalue),cl_fonction)
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
                                                NF90_FLOAT, &
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
                                             rda_varvalue),cl_fonction)
 
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
 
 
       END SUBROUTINE MIOL_writeu_field_R4_3D_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
 
         !!======================================================================
         !> \brief
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param id_file_id          A NetCDF file Id.
         !! @param cd_varname          The variable name.
         !! @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param rda_varvalue        The data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values  
         !! @param rda_specialvalues   Vector of special values of the variable:
         !!                                            spvalue(1) = minvalue
         !!                                            spvalue(2) = maxvalue
         !!                                            spvalue(3) = fillvalue
         !!                                            spvalue(4) = missvalue
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version 1.0
         !!        \n  01/2013   CREGNIER V3.5 MIOL
         !<
         !!======================================================================
 
       SUBROUTINE MIOL_writeu_field_R4_2D_NC (id_file_id, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
 
         USE netcdf
         USE MIOL_param
         
         USE MFT_error
        ! USE INT_ATTRIBUTS
         IMPLICIT NONE
 
         !-----------------------------------------------------------------------
 
         INTEGER,                          INTENT(IN) :: id_file_id
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
         CHARACTER(LEN=2),                 INTENT(IN) :: cd_key
         REAL(KIND=4), DIMENSION(:,:),     INTENT(IN) :: rda_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         REAL(KIND=4), DIMENSION(4),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         REAL(KIND=4) :: rl_missvalue, rl_minvalue, rl_maxvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue,rl_fillvalue
         CHARACTER(LEN=255) :: cl_attvalue,cl_fonction,cl_dimnameout,cl_dimname
         CHARACTER(LEN=18) :: cl_attname
         INTEGER :: il_ji, il_jj, il_jk, il_status, il_kindofdim,&
                    il_indice,il_len,il_RhVarId,il_dimid
         INTEGER,DIMENSION(1) :: il_start
         LOGICAL :: ll_timecounter,ll_def,ll_tri
       
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

         cl_fonction="MIOL_writeu_field_R4_2D_NC" 
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
         rl_minvalue = -rg_fillvalue
         rl_maxvalue = rg_fillvalue
         rl_missvalue = rg_fillvalue
         rl_fillvalue=rg_fillvalue
         
         !-----------------------------------------------------------------------
         ! Read dimensions
 
         il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE(id_file_id, &
                                             il_nbfiledim),cl_fonction)
 
         ALLOCATE(ila_filedimlen(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         ALLOCATE(cla_filedimname(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         DO il_ji = 1, il_nbfiledim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id, &
                                                          il_ji, &
                                                          cla_filedimname(il_ji), &
                                                          ila_filedimlen(il_ji)),cl_fonction)
            
            IF (cla_filedimname(il_ji) == 'time_counter' ) THEN
               il_len= ila_filedimlen(il_ji)
               ll_timecounter=.TRUE.
               il_indice=il_ji
            ENDIF  
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
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
 
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
               DO il_jk=1, il_nbfiledim
                                                    
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk                   
!!$                     IF (ila_filedimlen(il_jk).NE.SIZE(rda_varvalue, dim=il_ji)) THEN
!!$                        WRITE(0,*) ' MIOL_write_field_R4_2D_NC : dimensions error. '
!!$                        CALL flush(0)
!!$                        STOP
!!$                     ENDIF
 
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
         IF (.NOT.(PRESENT(rda_specialvalue))) THEN
                rl_minvalue = MINVAL(array=rda_varvalue,mask=rda_varvalue.NE.sNaN.AND.rda_varvalue .NE.rg_flagvalue)
                rl_maxvalue = MAXVAL(array=rda_varvalue, &
                     mask=rda_varvalue.LT.rg_fillvalue.AND.rda_varvalue.NE.sNaN.AND.rda_varvalue.NE.rg_flagvalue)
                !PRINT *,'MIN :: ',rl_minvalue
                !PRINT *,'MAX :: ',rl_maxvalue
              ELSE
            rl_minvalue = rda_specialvalue(1)
            rl_maxvalue = rda_specialvalue(2)
            rl_fillvalue = rda_specialvalue(3)
            rl_missvalue = rda_specialvalue(4)
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
         il_status = NF90_INQ_VARID(id_file_id,cl_varname,il_RhVarId)
         IF(il_status /= NF90_NoErr) THEN      
            ll_def = .FALSE.
            il_status = fi_ncError(NF90_DEF_VAR(id_file_id, &
                                             cl_varname, &
                                             NF90_FLOAT, &
                                             ila_dimsout_id, &
                                             il_varin_id),cl_fonction)
         ELSE
            ll_def = .TRUE.
            il_status = fi_ncError(NF90_INQ_DIMID(id_file_id,&
                                                  cl_dimname,&
                                                  il_dimid),'pb NF90_INQ_DIMID l2503 MIOL_writeu_field_R4_2D_NC')
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id,&
                                                  il_dimid,&
                                                  cl_dimnameout,&
                                                  il_len), 'pb NF90_INQ_DIMENSION l2507 MIOL_writeu_field_R4_2D_NC')
            il_varin_id=il_RhVarId
            ll_def=.TRUE.
         ENDIF

         DO il_ji = 1, il_nbatt
 
            READ(20, *) cl_attname
            READ(20, '(A100)') cl_attvalue
 
            SELECTCASE (cl_attvalue)
 
               CASE ('minvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_minvalue),cl_fonction)
 
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_maxvalue),cl_fonction)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_fillvalue),cl_fonction)
 
               
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
                                                      rl_missvalue),cl_fonction)
               CASE ('stepvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rda_varvalue(2,2)-rda_varvalue(1,1)),cl_fonction) 
 
                CASE ('flagvalues')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rg_flagvalue),cl_fonction)
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
                                                NF90_FLOAT, &
                                                ila_dimsout_id, &
                                                il_varin_id),cl_fonction)
         ENDIF
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(id_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Put array
         
         !IF (ll_timecounter) THEN
         !   PRINT *,'ll_timecounter',ll_def
         !   IF (ll_def) THEN
         !      il_start(1) = il_len+1
         !      IF (il_indice == 1) THEN 
         !         il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
         !              il_varin_id, &
         !              rda_varvalue,(/il_start,1/)),cl_fonction)
         !      ELSE IF (il_indice == 2) THEN 
         !         il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
         !              il_varin_id, &
         !              rda_varvalue,(/1,il_start/)),cl_fonction)
         !      ELSE
         !         PRINT *,'il_indice :: ',il_indice
         !         WRITE(0,*) 'PB dimension'
         !         STOP
         !      ENDIF
         !   ELSE 
         !      il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
         !           il_varin_id, &
         !           rda_varvalue,(/1,1/)),cl_fonction)
         !   ENDIF
         !ELSE
            il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
                                   il_varin_id, &
                                   rda_varvalue),cl_fonction)
         !
         !ENDIF
              
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
 
 
       END SUBROUTINE MIOL_writeu_field_R4_2D_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
       
         !!======================================================================
         !> \brief
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param id_file_id          A NetCDF file Id.
         !! @param cd_varname          The variable name.
         !! @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param rda_varvalue        The data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values  
         !! @param rda_specialvalues   Vector of special values of the variable:
         !!                                            spvalue(1) = minvalue
         !!                                            spvalue(2) = maxvalue
         !!                                            spvalue(3) = fillvalue
         !!                                            spvalue(4) = missvalue
         !!                                            spvalue(5) = stepvalue
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version 1.0
         !!        \n  01/2013   CREGNIER V3.5 MIOL
         !< 
         !!======================================================================
 
       SUBROUTINE MIOL_writeu_field_R4_1D_NC (id_file_id, &
                                           cd_varname, &
                                           cd_key, &
                                           rda_varvalue, &
                                           rda_offsetvalue, &
                                           rda_specialvalue)
 
         USE netcdf
         USE MIOL_param
         
         USE MFT_error
        ! USE INT_ATTRIBUTS
         IMPLICIT NONE
         
         !-----------------------------------------------------------------------
 
         INTEGER,                          INTENT(IN) :: id_file_id
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
         CHARACTER(LEN=1),                 INTENT(IN) :: cd_key
         REAL(KIND=4), DIMENSION(:),       INTENT(IN) :: rda_varvalue
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         REAL(KIND=4), DIMENSION(5),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         REAL(KIND=4) :: rl_missvalue, rl_minvalue, rl_maxvalue, &
                         rl_stepvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue,rl_fillvalue
         CHARACTER(LEN=255) :: cl_attvalue,cl_fonction
         CHARACTER(LEN=18) :: cl_attname
         INTEGER :: il_ji, il_jj, il_jk, il_status, il_kindofdim,il_RhVarId,il_len, il_i,il_dimid
         INTEGER,DIMENSION(1) :: il_start
         CHARACTER(LEN=255)   :: cl_dimname,cl_dimnameout
         LOGICAL              :: ll_def,ll_tri
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
 
         cl_fonction="MIOL_writeu_field_R4_1D_NC"

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
         rl_minvalue = -rg_fillvalue
         rl_maxvalue = rg_fillvalue
         rl_missvalue = rg_fillvalue
         rl_stepvalue = rg_fillvalue
         rl_fillvalue = rg_fillvalue
          
         !-----------------------------------------------------------------------
         ! Read dimensions
         il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)
         il_status = fi_ncError(NF90_INQUIRE(id_file_id, &
                                             il_nbfiledim),cl_fonction)
         ALLOCATE(ila_filedimlen(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         ALLOCATE(cla_filedimname(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         DO il_ji = 1, il_nbfiledim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id, &
                                                          il_ji, &
                                                          cla_filedimname(il_ji), &
                                                          ila_filedimlen(il_ji)),cl_fonction)
            IF (cla_filedimname(il_ji) == 'time_counter' ) THEN
               il_len= ila_filedimlen(il_ji)
            ENDIF
         ENDDO
 
         !-----------------------------------------------------------------------
         ! Find dimensions
         ALLOCATE(ila_dimsout_id(1), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
            
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
 
               DO il_jk=1, il_nbfiledim
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_ji) = il_jk
                     cl_dimname= cla_dimname(il_jj)
                     ! IF (ila_filedimlen(il_jk).NE.SIZE(rda_varvalue, dim=il_ji)) THEN
                     !   WRITE(0,*) ' MIOL_write_field_R4_1D_NC : dimensions error. '
                      !  CALL flush(0)
                      !  STOP
                    ! ENDIF
 
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
!            print *,'scale,offset ::',rda_offsetvalue(1), rda_offsetvalue(2)
         ELSE
            rl_scalevalue  = 1
            rl_offsetvalue = 0
         ENDIF 

         !-----------------------------------------------------------------------
         ! Find the minimum and maximum values 
 
         IF (.NOT.(PRESENT(rda_specialvalue))) THEN
            rl_minvalue = MINVAL(array=rda_varvalue,mask=rda_varvalue .NE.sNaN.AND.rda_varvalue .NE.rg_flagvalue)
            rl_maxvalue = MAXVAL(array=rda_varvalue, &
                 mask=rda_varvalue .NE. rl_fillvalue .AND.rda_varvalue .NE.sNaN &
                 & .AND.rda_varvalue .NE.rg_flagvalue)
         ELSE
            rl_minvalue = rda_specialvalue(1)
            rl_maxvalue = rda_specialvalue(2)
            rl_fillvalue = rda_specialvalue(3)
            rl_missvalue = rda_specialvalue(4)
            rl_stepvalue = rda_specialvalue(5)
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
         il_status = NF90_INQ_VARID(id_file_id,cl_varname,il_RhVarId)
         IF(il_status /= NF90_NoErr) THEN      
            ll_def = .FALSE.
            il_status = fi_ncError(NF90_DEF_VAR(id_file_id, &
                                   cl_varname, &
                                   NF90_FLOAT, &
                                   ila_dimsout_id, &
                                   il_varin_id),cl_fonction)
         !   PRINT *,'DEFINE ::',id_file_id,cl_varname,ila_dimsout_id,il_varin_id
         ELSE
            il_status = fi_ncError(NF90_INQ_DIMID(id_file_id,&
                                                  cl_dimname,&
                                                  il_dimid),'pb NF90_INQ_DIMID l2816')
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id,&
                                                  il_dimid,&
                                                  cl_dimnameout,&
                                                  il_len), 'pb NF90_INQ_DIMENSION l2820')
            il_varin_id=il_RhVarId
            ll_def=.TRUE.
            !il_len=ila_filedimlen(1)
        ENDIF
               
               
        DO il_ji = 1, il_nbatt
 
            READ(20, *) cl_attname
            READ(20, '(A100)') cl_attvalue
 
            SELECTCASE (cl_attvalue)
 
               CASE ('minvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_minvalue),cl_fonction)
 
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_maxvalue),cl_fonction)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_fillvalue),cl_fonction)
               
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
                                                      rl_missvalue),cl_fonction)
                CASE ('stepvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_stepvalue),cl_fonction)
 
                CASE ('flagvalues')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rg_flagvalue),cl_fonction)
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
                                                NF90_FLOAT, &
                                                ila_dimsout_id, &
                                                il_varin_id),cl_fonction)
         ENDIF
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(id_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Put array
         IF (ll_def) THEN
              IF (cl_dimnameout == 'time_counter' .OR. cl_dimnameout == 'time_month') THEN
                 il_start(1) = il_len+1
                 il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
                      il_varin_id, &
                      rda_varvalue,il_start),cl_fonction)
              ELSE
                 il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
                                      il_varin_id, &
                                      rda_varvalue),cl_fonction)
              ENDIF
                 

           ELSE
                 il_start(1) = 1
                 DO il_i=1,size(rda_varvalue(:))
                    il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
                         il_varin_id, &
                         rda_varvalue(il_i),start=(/il_i /)),cl_fonction)
                 ENDDO
              ENDIF

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
         
       END SUBROUTINE MIOL_writeu_field_R4_1D_NC
 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
       
         !!======================================================================
         !> \brief
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param id_file_id          A NetCDF file Id.
         !! @param cd_varname          The variable name.
         !! @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param rd_varvalue        The data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values  
         !! @param rda_specialvalues   Vector of special values of the variable:
         !!                                            spvalue(1) = minvalue
         !!                                            spvalue(2) = maxvalue
         !!                                            spvalue(3) = fillvalue
         !!                                            spvalue(4) = missvalue
         !!                                            spvalue(5) = stepvalue
         !!
         !! History :
         !!        \n  06/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version 1.0
         !!        \n  01/2013   CREGNIER V3.5 MIOL
         !< 
         !!======================================================================
 
       SUBROUTINE MIOL_writeu_field_R4_scalar_NC (id_file_id, &
                                           cd_varname, &
                                           cd_key, &
                                           rd_varvalue,& 
                                           id_indice,&
                                           rda_offsetvalue, &
                                           rda_specialvalue)
 
         USE netcdf
         USE MIOL_param
         
         USE MFT_error
        ! USE INT_ATTRIBUTS
         IMPLICIT NONE
         
         !-----------------------------------------------------------------------
 
         INTEGER,                          INTENT(IN) :: id_file_id
         CHARACTER(LEN=*),                 INTENT(IN) :: cd_varname
         CHARACTER(LEN=1),                 INTENT(IN) :: cd_key
         REAL(KIND=4),                     INTENT(IN) :: rd_varvalue
         INTEGER(KIND=4), OPTIONAL ,           INTENT(IN) :: id_indice
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         REAL(KIND=4), DIMENSION(5),       OPTIONAL, INTENT(IN) :: rda_specialvalue
 
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varfile,cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_varin_id, il_nbfiledim, il_nbatt
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         REAL(KIND=4) :: rl_missvalue, rl_minvalue, rl_maxvalue, &
                         rl_stepvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue,rl_fillvalue
         CHARACTER(LEN=255) :: cl_attvalue,cl_fonction
         CHARACTER(LEN=255)   :: cl_dimname,cl_dimnameout
         CHARACTER(LEN=18) :: cl_attname
         INTEGER :: il_ji, il_jj, il_jk, il_status, il_kindofdim,il_RhVarId,il_len,il_dimid
         INTEGER,DIMENSION(1) :: il_start
         LOGICAL :: ll_def,ll_tri
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

         cl_fonction="MIOL_writeu_field_R4_scalar_NC"
 
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
         rl_minvalue = -rg_fillvalue
         rl_maxvalue = rg_fillvalue
         rl_missvalue = rg_fillvalue
         rl_stepvalue = rg_fillvalue
         rl_fillvalue = rg_fillvalue
          
         !-----------------------------------------------------------------------
         ! Read dimensions
 
         il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_INQUIRE(id_file_id, &
                                             il_nbfiledim),cl_fonction)
 
         ALLOCATE(ila_filedimlen(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)
 
         ALLOCATE(cla_filedimname(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)
 
         DO il_ji = 1, il_nbfiledim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id, &
                                                          il_ji, &
                                                          cla_filedimname(il_ji), &
                                                          ila_filedimlen(il_ji)),cl_fonction)
         ENDDO
 
 
         !-----------------------------------------------------------------------
         ! Find dimensions
         ALLOCATE(ila_dimsout_id(1), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout',cl_fonction)
 
         DO il_ji=1, LEN(cd_key)
            cl_dimfile = TRIM(cl_dimensionsPath)//cd_key(il_ji:il_ji)// &
                         '_dimension.nml'
 
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
 
            ALLOCATE(cla_dimname(il_kindofdim), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)

               DO il_jk=1, il_nbfiledim
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     !! la variable dimension est trouve 
                     cl_dimname= cla_dimname(il_jj)
                     ila_dimsout_id(il_ji) = il_jk
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
 
         IF (.NOT.(PRESENT(rda_specialvalue))) THEN
            rl_minvalue = 0
            rl_maxvalue = 0
         ELSE
            rl_minvalue = rda_specialvalue(1)
            rl_maxvalue = rda_specialvalue(2)
            rl_fillvalue = rda_specialvalue(3)
            rl_missvalue = rda_specialvalue(4)
            rl_stepvalue = rda_specialvalue(5)
         ENDIF
 
 
 
         !------------------------------------------------------------------------
         ! Write variable attributes
         INQUIRE(FILE=cl_varfile,EXIST=ll_tri)
         !* Si le fichier.in existe on prend la namelist sinon creation d'un fichier par defaut
         IF (ll_tri) THEN
            OPEN(20, file=cl_varfile, status='old', form='formatted')
            READ(20, nb_att)
 
         !-----------------------------------------------------------------------
         ! Define the variable
         il_status = NF90_INQ_VARID(id_file_id,cl_varname,il_RhVarId)
         IF(il_status /= NF90_NoErr) THEN      
            ll_def = .FALSE.
            il_status = fi_ncError(NF90_DEF_VAR(id_file_id, &
                                   cl_varname, &
                                   NF90_FLOAT, &
                                   ila_dimsout_id, &
                                   il_varin_id),'Pb def var scalaire l3167')
         ELSE
            il_status = fi_ncError(NF90_INQ_DIMID(id_file_id,&
                                                  cl_dimname,&
                                                  il_dimid),'pb NF90_INQ_DIMID l3165')
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id,&
                                                  il_dimid,&
                                                  cl_dimnameout,&
                                                  il_len),'pb NF90_INQ_DIMENSION l3169') 
                              
            il_varin_id=il_RhVarId         
            ll_def=.TRUE.
            
         ENDIF
                                            
            DO il_ji = 1, il_nbatt
 
            READ(20, *) cl_attname
            READ(20, '(A100)') cl_attvalue
 
            SELECTCASE (cl_attvalue)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_fillvalue),cl_fonction)
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
               CASE ('minvalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rd_varvalue),cl_fonction) 
               CASE ('maxvalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, & 
                                                      il_varin_id, & 
                                                      cl_attname, & 
                                                      rd_varvalue),cl_fonction) 
               CASE ('missvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_missvalue),cl_fonction)
               CASE ('stepvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rl_stepvalue),cl_fonction)
               CASE ('flagvalues')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      il_varin_id, &
                                                      cl_attname, &
                                                      rg_flagvalue),cl_fonction)
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
                                              NF90_FLOAT, &
                                              ila_dimsout_id, &
                                              il_varin_id),cl_fonction)
       ENDIF
         !-----------------------------------------------------------------------
         ! Out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(id_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Put array
 

         IF (ll_def) THEN
            IF (cl_dimnameout == 'time_counter') THEN
            il_start(1) = il_len+1
            il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
                                             il_varin_id, &
                                             rd_varvalue,il_start),cl_fonction)
        
            ELSE IF (cl_dimnameout == 'time_month') THEN
               il_start(1) = id_indice
               il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
                                             il_varin_id, &
                                             rd_varvalue,il_start),'pb put var time month l3260')
        
            ELSE
               il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
                                      il_varin_id, &
                                      rd_varvalue),cl_fonction)
            ENDIF
         ELSE
            il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
                 il_varin_id, &
                 rd_varvalue),cl_fonction)
         ENDIF

    
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
 
 
       END SUBROUTINE MIOL_writeu_field_R4_scalar_NC


 !******************************************************************************
       
         !!======================================================================
         !> \brief
         !! Description: This function writes data values into the variable of an
         !!              NetCDF file.
         !!
         !! @param id_file_id          A NetCDF file Id.
         !! @param cda_varname          The variable names.
         !! @param cd_key              The variable 'key'. It represents the variable
         !!                       dimensions like 'XYZ ' for a longitude/latitude/
         !!                       depth variable.
         !! @param rda_varvalue        The data values to be write.
         !! @param rda_offsetvalue     optional offset and scale factor values  
         !! @param rda_specialvalues   Vector of special values of the variable:
         !!                                            spvalue(1) = minvalue
         !!                                            spvalue(2) = maxvalue
         !!                                            spvalue(3) = fillvalue
         !!                                            spvalue(4) = missvalue
         !!                                            spvalue(5) = stepvalue
         !!
         !!        \n  11/2008   CREGNIER Creation
         !< 
         !!======================================================================
 
       SUBROUTINE MIOL_writeu_multifield_R4_1D_NC (id_file_id, &
                                                   cda_varname, &
                                                   cda_key, &
                                                   rda_varvalue, &
                                                   ida_dimsout_id, &
                                                   rda_offsetvalue, &
                                                   rda_specialvalue)
 
         USE netcdf
         USE MIOL_param
         USE MFT_error
        ! USE INT_ATTRIBUTS
         IMPLICIT NONE
         
         !-----------------------------------------------------------------------
 
         INTEGER,                          INTENT(IN) :: id_file_id
         CHARACTER(LEN=255),DIMENSION(:),    INTENT(IN) :: cda_varname
         CHARACTER(LEN=1),DIMENSION(:),    INTENT(IN) :: cda_key
         REAL(KIND=4), DIMENSION(:,:),       INTENT(IN) :: rda_varvalue
         INTEGER,DIMENSION(:),              INTENT(IN) :: ida_dimsout_id
         
         REAL(KIND=8), DIMENSION(2), OPTIONAL, INTENT(IN) :: rda_offsetvalue
         REAL(KIND=4), DIMENSION(5),       OPTIONAL, INTENT(IN) :: rda_specialvalue

         CHARACTER(LEN=255),DIMENSION(:), ALLOCATABLE ::  cla_varfile
         CHARACTER(LEN=255) :: cl_equivalencesPath, cl_globalAttributesPath, &
                               cl_variableAttributesPath, cl_dimensionsPath, &
                               cl_dimfile, cl_varname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_filedimname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_dimname
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_filedimlen
         INTEGER :: il_varin_id, il_nbfiledim, il_nbatt,il_k
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimsout_id
         REAL(KIND=4) :: rl_missvalue, rl_minvalue, rl_maxvalue, &
                         rl_stepvalue
         REAL(KIND=4) :: rl_scalevalue, rl_offsetvalue,rl_fillvalue
         CHARACTER(LEN=255) :: cl_attvalue,cl_fonction,cl_dimname,cl_dimnameout
         CHARACTER(LEN=18) :: cl_attname
         INTEGER :: il_ji, il_jj, il_jk, il_status, il_kindofdim,il_RhVarId,il_len,il_i,il_dimension
         INTEGER,DIMENSION(:),ALLOCATABLE :: ila_len,ila_VarID
         INTEGER :: il_start,il_count,il_dimid
         LOGICAL  ,DIMENSION(:),ALLOCATABLE :: lla_def
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
         cl_fonction="MIOL_writeu_multifield_R4_1D_NC"

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
         il_dimension=size(cda_varname(:))

         ALLOCATE (cla_varfile(il_dimension))
         ALLOCATE (ila_len(il_dimension))
         ALLOCATE (lla_def(il_dimension))
         ALLOCATE (ila_VarID(il_dimension))
         DO il_i=1,il_dimension
            cla_varfile(il_i) = TRIM(cl_variableAttributesPath)//TRIM(cda_varname(il_i))//'.in'
         ENDDO
         rl_minvalue = -rg_fillvalue
         rl_maxvalue = rg_fillvalue
         rl_missvalue = rg_fillvalue
         rl_stepvalue = rg_fillvalue
         rl_fillvalue = rg_fillvalue
         !-----------------------------------------------------------------------
         ! Read dimensions
         il_status = fi_ncError(NF90_REDEF(id_file_id),cl_fonction)
         il_status = fi_ncError(NF90_INQUIRE(id_file_id, &
                                             il_nbfiledim),cl_fonction)
         
         ALLOCATE(ila_filedimlen(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_filedimlen',cl_fonction)

         ALLOCATE(cla_filedimname(il_nbfiledim), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' cla_filedimname',cl_fonction)

         DO il_ji = 1, il_nbfiledim
            il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id, &
                                                          il_ji, &
                                                          cla_filedimname(il_ji), &
                                                          ila_filedimlen(il_ji)),cl_fonction)
            IF (cla_filedimname(il_ji) == 'time_counter' ) THEN
               il_len= ila_filedimlen(il_ji)
            ENDIF
         ENDDO
         !-----------------------------------------------------------------------
         ! Find dimensions
         ALLOCATE(ila_dimsout_id(il_dimension), &
                  stat=il_status)
         il_status = fi_memError(il_status, ' ila_dimsout',cl_fonction)

         DO il_i=1,size(cda_varname(:))
            cl_dimfile = TRIM(cl_dimensionsPath)//cda_key(il_i)// &
                  '_dimension.nml'
 
            OPEN(20, file=TRIM(cl_dimfile), status='old', form='formatted')
            READ(20, nb_dim)
            IF (ALLOCATED(cla_dimname)) DEALLOCATE(cla_dimname)
            ALLOCATE(cla_dimname(il_kindofdim), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
            DO il_jj=1, il_kindofdim
               READ(20, *) cla_dimname(il_jj)
               DO il_jk=1, il_nbfiledim
                  IF (TRIM(cla_dimname(il_jj)) .EQ. TRIM(cla_filedimname(il_jk))) THEN
                     ila_dimsout_id(il_i) = il_jk
                     cl_dimname=cla_dimname(il_jj)
                     PRINT *,'Dimension OK : ',cl_dimname
                  ENDIF
 
               ENDDO
 
            ENDDO
 
            CLOSE(20)
         ENDDO
         IF (ALLOCATED(cla_dimname)) DEALLOCATE(cla_dimname, stat=il_status)
         il_status = fi_memError(il_status, ' cla_dimname',cl_fonction)
         !-----------------------------------------------------------------------
         ! Find the offset values

          IF (PRESENT(rda_offsetvalue)) THEN
            rl_scalevalue = rda_offsetvalue(1)
            rl_offsetvalue = rda_offsetvalue(2)
!            print *,'scale,offset ::',rda_offsetvalue(1), rda_offsetvalue(2)
         ELSE
            rl_scalevalue  = 1
            rl_offsetvalue = 0
         ENDIF

         !-----------------------------------------------------------------------
         ! Find the minimum and maximum values 
 
         IF (.NOT.(PRESENT(rda_specialvalue))) THEN
            rl_minvalue = MINVAL(array=rda_varvalue,mask=rda_varvalue .NE.sNaN.AND.rda_varvalue .NE.rg_flagvalue)
            rl_maxvalue = MAXVAL(array=rda_varvalue, &
                 mask=rda_varvalue .NE. rl_fillvalue .AND.rda_varvalue .NE.sNaN &
                 & .AND.rda_varvalue .NE.rg_flagvalue)
         ELSE
            rl_minvalue = rda_specialvalue(1)
            rl_maxvalue = rda_specialvalue(2)
            rl_fillvalue = rda_specialvalue(3)
            rl_missvalue = rda_specialvalue(4)
            rl_stepvalue = rda_specialvalue(5)
         ENDIF
 
         !------------------------------------------------------------------------
         ! Write variable attributes
 
         DO il_i=1,il_dimension
            OPEN(20, file=cla_varfile(il_i), status='old', form='formatted')
            READ(20, nb_att)
 
         !-----------------------------------------------------------------------
         ! Define the variable
            il_status = NF90_INQ_VARID(id_file_id,cl_varname,il_RhVarId)
            IF(il_status /= NF90_NoErr) THEN      
               lla_def(il_i) = .FALSE.
               il_status = fi_ncError(NF90_DEF_VAR(id_file_id, &
                                   cl_varname, &
                                   NF90_FLOAT, &
                                   ida_dimsout_id(1), &
                                   il_varin_id),cl_fonction)
               ila_VarID(il_i)=il_varin_id
             ELSE
               lla_def(il_i)=.TRUE.

               il_status = fi_ncError(NF90_INQ_DIMID(id_file_id,&
                                                  cl_dimname,&
                                                  il_dimid),'pb NF90_INQ_DIMID l2816')
               il_status = fi_ncError(NF90_INQUIRE_DIMENSION(id_file_id,&
                                                  il_dimid,&
                                                  cl_dimnameout,&
                                                  il_len), 'pb NF90_INQ_DIMENSION l2820')
               ila_len(il_i)=il_len
               ila_VarID(il_i)=il_RhVarId
             ENDIF
               
               
            DO il_ji = 1, il_nbatt
               READ(20, *) cl_attname
               READ(20, '(A100)') cl_attvalue
               
               SELECTCASE (cl_attvalue)
                  
               CASE ('minvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      ila_VarID(il_i), &
                                                      cl_attname, &
                                                      rl_minvalue),cl_fonction)
 
               CASE ('maxvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      ila_VarID(il_i), &
                                                      cl_attname, &
                                                      rl_maxvalue),cl_fonction)
 
               CASE ('fillvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      ila_VarID(il_i), &
                                                      cl_attname, &
                                                      rl_fillvalue),cl_fonction)
               
               CASE ('scalevalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, & 
                                                      ila_VarID(il_i), & 
                                                      cl_attname, & 
                                                      rl_scalevalue),cl_fonction) 
 
               CASE ('offsetvalue') 
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, & 
                                                      ila_VarID(il_i), & 
                                                      cl_attname, & 
                                                      rl_offsetvalue),cl_fonction) 
 
               CASE ('missvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      ila_VarID(il_i), &
                                                      cl_attname, &
                                                      rl_missvalue),cl_fonction)
                CASE ('stepvalue')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      ila_VarID(il_i), &
                                                      cl_attname, &
                                                      rl_stepvalue),cl_fonction)
 
                CASE ('flagvalues')
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      ila_VarID(il_i), &
                                                      cl_attname, &
                                                      rg_flagvalue),cl_fonction)
               CASE DEFAULT
                  il_status = fi_ncError(NF90_PUT_ATT(id_file_id, &
                                                      ila_VarID(il_i), &
                                                      cl_attname, &
                                                      TRIM(cl_attvalue)),cl_fonction)
               ENDSELECT
            ENDDO
               CLOSE(20)

          ENDDO
         !-----------------------------------------------------------------------
         ! Out of define mode

         il_status = fi_ncError(NF90_ENDDEF(id_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Put array
                       
         il_count=size(rda_varvalue(1,:))
         DO il_k=1,il_count
            DO il_i=1,il_dimension
               IF (lla_def(il_i) ) THEN
                  il_start = ila_len(il_i)+1
                  il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
                                       ila_VarID(il_i), &
                                       rda_varvalue(il_i,:),start = (/ il_start/)),cl_fonction)
                  !WRITE (0,*),'NEW put var'
                  ELSE
                    il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
                                                      ila_VarID(il_i), &
                                                      rda_varvalue(il_i,il_k),start = (/ il_k /)),cl_fonction)
                   !WRITE (0,*),'OLD put var'!,id_file_id,ila_VarID(il_i),rda_varvalue(il_i,:),ila_start,il_count

                 ENDIF
              ENDDO
           ENDDO



!!$         IF (lla_def(il_i) ) THEN
!!$               DO il_i=1,il_dimension
!!$
!!$                il_start = ila_len(il_i)+1
!!$                il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
!!$                                       ila_VarID(il_i), &
!!$                                       rda_varvalue(il_i,:),start = (/ il_start/)),cl_fonction)
!!$                WRITE (0,*),'NEW put var'
!!$             ENDDO
!!$          ELSE
!!$            !!   ALLOCATE(rla_var(size(rda_varvalue(1,:))))
!!$            !!   il_start(1) = 1
!!$            !!   ALLOCATE(ila_count(size(rla_var(:))))
!!$            !!   ALLOCATE(ila_start(size(rla_var(:))))
!!$            !!   ila_start(:) =  (/ (il_k,il_k=1,size(rla_var(:)),1)/)
!!$            !!   ila_count(:)=(/ (il_k,il_k=1,size(rla_var(:)),1)/)
!!$               PRINT *,'il_count :: ', il_count,il_dimension
!!$                     print *,'il_k,il_i : ',il_k,il_i,rda_varvalue(il_i,il_k)
!!$                     il_status = fi_ncError(NF90_PUT_VAR(id_file_id, &
!!$                                                      ila_VarID(il_i), &
!!$                                                      rda_varvalue(il_i,il_k),start = (/ il_k /)),cl_fonction)
!!$                     WRITE (0,*),'OLD put var',id_file_id,ila_VarID(il_i),rda_varvalue(il_i,:),ila_start,il_count
!!$                  ENDDO
!!$               ENDDO
!!$             !  WRITE (0,*),'OLD put var',id_file_id,ila_VarID(il_i),rda_varvalue(il_i,:),ila_start,il_count
!!$
!!$            ENDIF
 !!           il_status = fi_ncError(nf90_close(id_file_id),'PB close MIOL_writeu_multifield_R4_1D_NC' )

            
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
         
          WRITE(0,*) 'MIOL_writeu_multifield_R4_1D_NC'
 
       END SUBROUTINE MIOL_writeu_multifield_R4_1D_NC
 
  !******************************************************************************
  !******************************************************************************


 
  !******************************************************************************
  !******************************************************************************
  !******************************************************************************
!END MODULE src_Write_R4
