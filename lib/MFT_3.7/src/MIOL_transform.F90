!> \brief Module which contain functions to transform netcdf files
!! \author C.REGNIER Miol V3.5
!! \date September 2008 
!!  \version 3.5
!<

MODULE MIOL_transform
!public : MIOL_transform_opatocf_NC,MIOL_transform_opatocf_GODAE_NC
  !         MIOL_read_coordorca_NC,MIOL_read_bathyorca_NC,MIOL_read_bathycls_NC
 CONTAINS
 

         !!======================================================================
         !> \brief
         !! Description: This function transform OPA format variable names to a
         !!              CF format. (ex: “votemper” becomes “temperature”, etc...).
         !!   @param cd_input_filename path of the input file
         !!   @param cd_output_filename path of the output file
         !!   @param cd_gattfilename path of the global attribut file
         !!   @param id_juliandate juliandate of the file
         !!
         !! History :
         !!        \n  07/2006  (F. Messal) Creation
         !!        \n  11/2006  (F. Messal) CVS version 1.0
         !<
         !!======================================================================
 
  SUBROUTINE MIOL_transform_opatocf_NC (cd_input_filename, &
                                              cd_output_filename, &
                                              cd_gattfilename, &
                                              id_juliandate)
 
         USE netcdf
         USE MIOL_param
         USE MFT_error
         IMPLICIT NONE
          !-----------------------------------------------------------------------
 
         CHARACTER(LEN=*), INTENT(IN) :: cd_input_filename, cd_output_filename
         CHARACTER(LEN=*), INTENT(IN) :: cd_gattfilename
         INTEGER, INTENT(IN), OPTIONAL :: id_juliandate
 
         CHARACTER(LEN=255) :: cl_copycom, &
                               cl_ext, cl_dimname, cl_eqfilename, cl_varname, &
                               cl_varname_out, cl_varfilename, cl_tmpattname, &
                               cl_equivalencesPath, &
                               cl_globalAttributesPath, &
                               cl_variableAttributesPath, &
                               cl_dimensionsPath
         CHARACTER(LEN=255) :: cl_history, cl_creationdate, cl_institution, &
                               cl_references, cl_filename
         CHARACTER(LEN=8) :: cl_date
         CHARACTER(LEN=10) :: cl_time
 
         CHARACTER(LEN=255) :: cl_gattfilename,cl_fonction
         CHARACTER(LEN=18), DIMENSION(:), ALLOCATABLE :: cl_attname
         CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cl_attvalue
         INTEGER :: il_file_id, il_nbdim, il_nbvar, il_nbatt, il_nbgatt, il_ji, &
                    il_jj, il_type, il_status
         INTEGER, DIMENSION(4) :: ila_dimids, ila_len
         REAL(KIND=4) :: rl_missvalue,rl_fillvalue
         INTEGER, DIMENSION(:,:,:,:), ALLOCATABLE :: ila_4Dint_values
         INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: ila_3Dint_values
         INTEGER, DIMENSION(:,:), ALLOCATABLE :: ila_2Dint_values
         INTEGER, DIMENSION(:), ALLOCATABLE :: ila_1Dint_values
         INTEGER :: il_int_minvalue, il_int_maxvalue, il_int_stepvalue
         REAL(KIND=4), DIMENSION(:,:,:,:), ALLOCATABLE :: rla_4DR4_values
         REAL(KIND=4), DIMENSION(:,:,:), ALLOCATABLE :: rla_3DR4_values
         REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: rla_2DR4_values
         REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: rla_1DR4_values
         REAL(KIND=4) :: rl_R4_minvalue, &
                         rl_R4_maxvalue, &
                         rl_R4_stepvalue, &
                         rl_lonminvalue, &
                         rl_lonmaxvalue, &
                         rl_latminvalue, &
                         rl_latmaxvalue, &
                         rl_depthminvalue, &
                         rl_depthmaxvalue
         REAL(KIND=8), DIMENSION(:,:,:,:), ALLOCATABLE :: rla_4DR8_values
         REAL(KIND=8), DIMENSION(:,:,:), ALLOCATABLE :: rla_3DR8_values
         REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: rla_2DR8_values
         REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: rla_1DR8_values
         REAL(KIND=8) :: rl_R8_minvalue, rl_R8_maxvalue, rl_R8_stepvalue
         REAL(KIND=4) :: rl_coeff
         LOGICAL :: ll_minvalue_isdef, ll_maxvalue_isdef, ll_minname_isdef, &
                    ll_maxname_isdef, ll_min_comp, ll_max_comp, &
                    ll_stepvalue_isdef, ll_lonlat_is1D
 
         !-----------------------------------------------------------------------
         NAMELIST /miolParameterPaths/ cl_equivalencesPath, &
                                         cl_globalAttributesPath, &
                                         cl_variableAttributesPath, &
                                         cl_dimensionsPath
         NAMELIST /equival/ cl_varname_out, rl_coeff
         NAMELIST /nb_att/ il_nbatt
 
         OPEN(20, file=cp_miolParameterFile, &
                  status='old', &
                  form='formatted')
         READ(20, miolParameterPaths)
         CLOSE(20)

         cl_fonction="MIOL_transform_opatocf_NC"
 
         cl_ext = '.in'
         rl_fillvalue =  rg_fillvalue
         rl_missvalue =  rg_missvalue
 
         CALL DATE_AND_TIME(cl_date, cl_time)
         cl_institution = 'GIP MERCATOR OCEAN'
         cl_references = 'http://www.mercator-ocean.fr'
         cl_filename = TRIM(cd_output_filename)
         cl_creationdate = cl_date(1:4)//'/'//cl_date(5:6)//'/'//cl_date(7:8)// &
               ' '//cl_time(1:2)//':'//cl_time(3:4)//':'//cl_time(5:6)
         cl_history = TRIM(cl_creationdate)//' MERCATOR OCEAN Netcdf creation '
         ll_lonlat_is1D = .TRUE.
 
 
         !-----------------------------------------------------------------------
         ! Copy the file
         cl_copycom = 'cp '//TRIM(cd_input_filename)//' '//TRIM(cd_output_filename)
         CALL SYSTEM(TRIM(cl_copycom))
 
 
         !-----------------------------------------------------------------------
         ! Open the new file in wrinting and define mode
         il_status = fi_ncError(NF90_OPEN(TRIM(cd_output_filename), &
                                          NF90_WRITE, &
                                          il_file_id),cl_fonction)
 
         il_status = fi_ncError(NF90_REDEF(il_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Load the number of dimensions, the number of variables and the number
         ! of attributes
 
         il_status = fi_ncError(NF90_INQUIRE(il_file_id, &
                                             il_nbdim, &
                                             il_nbvar, &
                                             il_nbgatt),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! Delete the old attributes, change the variable name and write the new
         ! attributes
         DO il_ji = 1, il_nbvar
 
            ! initialization
            il_int_minvalue = 9999
            il_int_maxvalue = 9999
            il_int_stepvalue = 0
            rl_R4_minvalue = 9999.
            rl_R4_maxvalue = 9999.
            rl_R4_stepvalue = 0.
            rl_R8_minvalue = 9999.
            rl_R8_maxvalue = 9999.
            rl_R8_stepvalue = 0.
            ll_minvalue_isdef = .FALSE.
            ll_maxvalue_isdef = .FALSE.
            ll_stepvalue_isdef = .FALSE.
            ll_minname_isdef = .FALSE.
            ll_maxname_isdef = .FALSE.
            ll_min_comp = .FALSE.
            ll_max_comp = .FALSE.
 
            ! find variable name
            il_status = fi_ncError(NF90_INQUIRE_VARIABLE(il_file_id, &
                                                         il_ji, &
                                                         cl_varname, &
                                                         il_type, &
                                                         il_nbdim, &
                                                         ila_dimids, &
                                                         il_nbatt),cl_fonction)
 
            !WRITE(0,*) ' var = ',TRIM(cl_varname),' dim = ',il_nbdim
            IF ((TRIM(cl_varname) .EQ. 'longitude') .OR. &
                 TRIM(cl_varname) .EQ. 'latitude') THEN
               IF (il_nbdim .GT. 1) THEN
                  ll_lonlat_is1D = .FALSE.
                  !WRITE(0,*) ' flag = ',ll_lonlat_is1D
               ENDIF
            ENDIF
 
            ! find the length of dimensions
            DO il_jj = 1, il_nbdim
               il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                             ila_dimids(il_jj), &
                                                             cl_dimname, &
                                                             ila_len(il_jj)),cl_fonction)
            ENDDO
 
            ! find the value of 'valid_min', 'valid_max', 'step' attributes
            ! if they're defined, we copy the values
            DO il_jj = 1, il_nbatt
 
               il_status = fi_ncError(NF90_INQ_ATTNAME(il_file_id, &
                                                       il_ji, &
                                                       il_jj, &
                                                       cl_tmpattname),cl_fonction)
 
               SELECT CASE(TRIM(cl_tmpattname))
 
                  CASE('valid_min')
                     ll_minvalue_isdef = .TRUE.
 
                     SELECT CASE (il_type)
 
                        CASE (NF90_INT)
                           il_status = fi_ncError(NF90_GET_ATT(il_file_id, &
                                                               il_ji, &
                                                               TRIM(cl_tmpattname), &
                                                               il_int_minvalue ),cl_fonction)
 
                        CASE (NF90_FLOAT)
                           il_status = fi_ncError(NF90_GET_ATT(il_file_id, &
                                                               il_ji, &
                                                               TRIM(cl_tmpattname), &
                                                               rl_R4_minvalue ),cl_fonction)
 
                        CASE (NF90_DOUBLE)
                           il_status = fi_ncError(NF90_GET_ATT(il_file_id, &
                                                               il_ji, &
                                                               TRIM(cl_tmpattname), &
                                                               rl_R8_minvalue ),cl_fonction)
 
                        CASE DEFAULT
                           WRITE(0,*) ' MIOL_transform_opatocf_NC : not float or double or integer variable type.'
                           CALL flush(0)
                           STOP
 
                     ENDSELECT
 
                  CASE('valid_max')
                     ll_maxvalue_isdef = .TRUE.
 
                     SELECT CASE (il_type)
 
                        CASE (NF90_INT)
                           il_status = fi_ncError(NF90_GET_ATT(il_file_id, &
                                                               il_ji, &
                                                               TRIM(cl_tmpattname), &
                                                               il_int_maxvalue ),cl_fonction)
 
                        CASE (NF90_FLOAT)
                           il_status = fi_ncError(NF90_GET_ATT(il_file_id, &
                                                               il_ji, &
                                                               TRIM(cl_tmpattname), &
                                                               rl_R4_maxvalue ),cl_fonction)
 
                        CASE (NF90_DOUBLE)
                           il_status = fi_ncError(NF90_GET_ATT(il_file_id, &
                                                               il_ji, &
                                                               TRIM(cl_tmpattname), &
                                                               rl_R8_maxvalue ),cl_fonction)
 
                        CASE DEFAULT
                           WRITE(0,*) ' MIOL_transform_opatocf_NC : not float or double or integer variable type.'
                           CALL flush(0)
                           STOP
 
                     ENDSELECT
 
                  CASE('step')
                     ll_stepvalue_isdef = .TRUE.
 
                     SELECT CASE (il_type)
 
                        CASE (NF90_INT)
                           il_status = fi_ncError(NF90_GET_ATT(il_file_id, &
                                                               il_ji, &
                                                               TRIM(cl_tmpattname), &
                                                               il_int_stepvalue ),cl_fonction)
 
                        CASE (NF90_FLOAT)
                           il_status = fi_ncError(NF90_GET_ATT(il_file_id, &
                                                               il_ji, &
                                                               TRIM(cl_tmpattname), &
                                                               rl_R4_stepvalue ),cl_fonction)
 
                        CASE (NF90_DOUBLE)
                           il_status = fi_ncError(NF90_GET_ATT(il_file_id, &
                                                               il_ji, &
                                                               TRIM(cl_tmpattname), &
                                                               rl_R8_stepvalue ),cl_fonction)
 
                        CASE DEFAULT
                           WRITE(0,*) ' MIOL_transform_opatocf_NC : not float or double or integer variable type.'
                           CALL flush(0)
                           STOP
 
                     ENDSELECT
 
                  CASE DEFAULT
 
               ENDSELECT
 
               !WRITE(0,*) ' select variable : ',TRIM(cl_varname)
               SELECT CASE (TRIM(cl_varname))
 
                  CASE ('longitude')
                     rl_lonminvalue = rl_R4_minvalue
                     rl_lonmaxvalue = rl_R4_maxvalue
 
                  CASE ('latitude')
                     rl_latminvalue = rl_R4_minvalue
                     rl_latmaxvalue = rl_R4_maxvalue
 
                  CASE ('depth')
                     rl_depthminvalue= rl_R4_minvalue
                     rl_depthmaxvalue = rl_R4_maxvalue
 
                END SELECT
 
            ENDDO
 
            ! rename variable
            cl_eqfilename = TRIM(cl_equivalencesPath)//TRIM(cl_varname)//TRIM(cl_ext)
 
            OPEN(20, file=TRIM(cl_eqfilename), status='old', form='formatted')
            READ(20, equival)
            CLOSE(20)
 
            IF (TRIM(cl_varname) .NE. TRIM(cl_varname_out)) THEN
               il_status = fi_ncError(NF90_RENAME_VAR(il_file_id, &
                                                      il_ji, &
                                                      cl_varname_out),cl_fonction)
            ENDIF
 
            ! delete old attributes
            DO il_jj = 1, il_nbatt
               il_status = fi_ncError(NF90_INQ_ATTNAME(il_file_id, &
                                                       il_ji, &
                                                       1, &
                                                       cl_tmpattname),cl_fonction)
 
               il_status = fi_ncError(NF90_DEL_ATT(il_file_id, &
                                                   il_ji, &
                                                   TRIM(cl_tmpattname)),cl_fonction)
 
            ENDDO
 
 
            ! load new attributes
            cl_varfilename = TRIM(cl_variableAttributesPath)//TRIM(cl_varname_out)//TRIM(cl_ext)
 
            !WRITE(0,*) ' cl_varfilename = ',TRIM(cl_varfilename)
 
            OPEN(20, file=cl_varfilename, status='old', form='formatted')
            READ(20, nb_att)
            IF (ALLOCATED(cl_attname)) DEALLOCATE(cl_attname)
            ALLOCATE(cl_attname(il_nbatt), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cl_attname',cl_fonction)
 
            IF (ALLOCATED(cl_attvalue)) DEALLOCATE(cl_attvalue)
            ALLOCATE(cl_attvalue(il_nbatt), &
                     stat=il_status)
            il_status = fi_memError(il_status, ' cl_attvalue',cl_fonction)
 
            ll_minname_isdef = .FALSE.
            ll_maxname_isdef = .FALSE.
            DO il_jj = 1, il_nbatt
 
               READ(20, *) cl_attname(il_jj)
               READ(20, '(A100)') cl_attvalue(il_jj)
 
               SELECT CASE(cl_attvalue(il_jj))
 
                  CASE ('minvalue')
                     ll_minname_isdef = .TRUE.
                  CASE ('maxvalue')
                     ll_maxname_isdef = .TRUE.
                  CASE DEFAULT
 
               ENDSELECT
 
            ENDDO
 
            CLOSE(20)
 
 
            ! if the attribute name is defined and the attribute value is not
            ! loaded, we need to compute the value
 
            IF (ll_minname_isdef .AND. &
                (.NOT. ll_minvalue_isdef)) THEN
               ll_min_comp = .TRUE.
            ENDIF
            IF (ll_maxname_isdef .AND. &
                (.NOT. ll_maxvalue_isdef)) THEN
               ll_max_comp = .TRUE.
            ENDIF
 
 
            ! compute the new values with the coefficient
            ! and find the new min and max value
            IF (ll_min_comp .OR. ll_max_comp .OR. &
                (rl_coeff .NE. 1.0)) THEN
 
               il_status = fi_ncError(NF90_ENDDEF(il_file_id),cl_fonction)
 
               ! variable type is NF90_INT
               SELECT CASE (il_type)
 
                  CASE (NF90_INT)
 
                     SELECT CASE (il_nbdim)
 
                        ! NF90_INT array values is 1D a array
                        CASE (1)
 
                           ALLOCATE(ila_1Dint_values(ila_len(1)), &
                                    stat=il_status)
                           il_status = fi_memError(il_status, ' ila_1Dint_values',cl_fonction)
 
                           il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                               il_ji, &
                                                               ila_1Dint_values),cl_fonction)
 
                           IF (rl_coeff .NE. 1.0) THEN
                              WHERE (ila_1Dint_values .NE. rl_fillvalue)
                                 ila_1Dint_values = ila_1Dint_values*rl_coeff
                              ENDWHERE
 
                              il_status = fi_ncError(NF90_PUT_VAR(il_file_id, &
                                                                  il_ji, &
                                                                  ila_1Dint_values),cl_fonction)
                           ENDIF
 
                           IF (ll_min_comp) THEN
                              il_int_minvalue = MINVAL(ila_1Dint_values)
                           ENDIF
 
                           IF (ll_max_comp) THEN
                              il_int_maxvalue = MAXVAL(ila_1Dint_values,  &
                                  mask=ila_1Dint_values .NE. rl_fillvalue)
                           ENDIF
 
                           DEALLOCATE(ila_1Dint_values, stat = il_status)
                           il_status = fi_memError(il_status, ' ila_1Dint_values',cl_fonction)
 
 
                        ! NF90_INT array values is 2D a array
                        CASE (2)
 
                           ALLOCATE(ila_2Dint_values(ila_len(1), &
                                                     ila_len(2)), &
                                    stat=il_status)
                           il_status = fi_memError(il_status, ' ila_2Dint_values',cl_fonction)
 
                           il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                               il_ji, &
                                                               ila_2Dint_values),cl_fonction)
 
                           IF (rl_coeff .NE. 1.0) THEN
                              WHERE (ila_2Dint_values .NE. rl_fillvalue)
                                 ila_2Dint_values = ila_2Dint_values*rl_coeff
                              ENDWHERE
 
                              il_status = fi_ncError(NF90_PUT_VAR(il_file_id, &
                                                                  il_ji, &
                                                                  ila_2Dint_values),cl_fonction)
                           ENDIF
 
                           IF (ll_min_comp) THEN
                              il_int_minvalue = MINVAL(ila_2Dint_values)
                           ENDIF
 
                           IF (ll_max_comp) THEN
                              il_int_maxvalue = MAXVAL(ila_2Dint_values, &
                                 mask=ila_2Dint_values .NE. rl_fillvalue)
                           ENDIF
 
                           DEALLOCATE(ila_2Dint_values, stat = il_status)
                           il_status = fi_memError(il_status, ' ila_2Dint_values',cl_fonction)
 
                        ! NF90_INT array values is a 3D array
                        CASE (3)
 
                           ALLOCATE(ila_3Dint_values(ila_len(1), &
                                                     ila_len(2), &
                                                     ila_len(3)), &
                                    stat=il_status)
                           il_status = fi_memError(il_status, ' ila_3Dint_values',cl_fonction)
 
                           il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                               il_ji, &
                                                               ila_3Dint_values),cl_fonction)
 
                           IF (rl_coeff .NE. 1.0) THEN
                              WHERE (ila_3Dint_values .NE. rl_fillvalue)
                                 ila_3Dint_values = ila_3Dint_values*rl_coeff
                              ENDWHERE
 
                              il_status = fi_ncError(NF90_PUT_VAR(il_file_id, &
                                                                  il_ji, &
                                                                  ila_3Dint_values),cl_fonction)
                           ENDIF
 
                           IF (ll_min_comp) THEN
                              il_int_minvalue = MINVAL(ila_3Dint_values)
                           ENDIF
 
                           IF (ll_max_comp) THEN
                              il_int_maxvalue = MAXVAL(ila_3Dint_values,  &
                                 mask=ila_3Dint_values .NE. rl_fillvalue)
                           ENDIF
 
                           DEALLOCATE(ila_3Dint_values, stat=il_status)
                           il_status = fi_memError(il_status, ' ila_3Dint_values',cl_fonction)
 
 
                        ! NF90_INT array values is a 4D array
                        CASE (4)
 
                           ALLOCATE(ila_4Dint_values(ila_len(1), &
                                                     ila_len(2), &
                                                     ila_len(3), &
                                                     ila_len(4)), &
                                    stat=il_status)
                           il_status = fi_memError(il_status, ' ila_4Dint_values',cl_fonction)
 
                           il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                               il_ji, &
                                                               ila_4Dint_values),cl_fonction)
 
                           IF (rl_coeff .NE. 1.0) THEN
                              WHERE (ila_4Dint_values .NE. rl_fillvalue)
                                 ila_4Dint_values = ila_4Dint_values*rl_coeff
                              ENDWHERE
 
                              il_status = fi_ncError(NF90_PUT_VAR(il_file_id, &
                                                                  il_ji, &
                                                                  ila_4Dint_values),cl_fonction)
                           ENDIF
 
                           IF (ll_min_comp) THEN
                              il_int_minvalue = MINVAL(ila_4Dint_values)
                           ENDIF
 
                           IF (ll_max_comp) THEN
                              il_int_maxvalue = MAXVAL(ila_4Dint_values,  &
                                       mask=ila_4Dint_values .NE. rl_fillvalue)
                           ENDIF
 
                           DEALLOCATE(ila_4Dint_values, stat=il_status)
                           il_status = fi_memError(il_status, ' ila_4Dint_values',cl_fonction)
 
                        CASE DEFAULT
                           WRITE(0,*) ' MIOL_transform_opatocf_NC : number of dimension exceed'
                           CALL flush(0)
                           STOP
 
                     ENDSELECT
 
                  ! variable type is NF90_FLOAT
                  CASE (NF90_FLOAT)
 
                     ! NF90_FLOAT array is a 1D array
                     SELECT CASE (il_nbdim)
 
                        CASE (1)
 
                           ALLOCATE(rla_1DR4_values(ila_len(1)), &
                                    stat=il_status)
                           il_status = fi_memError(il_status, ' rla_1DR4_values',cl_fonction)
 
                           il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                               il_ji, &
                                                               rla_1DR4_values),cl_fonction)
 
                           IF (rl_coeff .NE. 1.0) THEN
                              WHERE (rla_1DR4_values .NE. rl_fillvalue)
                                 rla_1DR4_values = rla_1DR4_values*rl_coeff
                              ENDWHERE
 
                              il_status = fi_ncError(NF90_PUT_VAR(il_file_id, &
                                                                  il_ji, &
                                                                  rla_1DR4_values),cl_fonction)
                           ENDIF
 
                           IF (ll_min_comp) THEN
                              rl_R4_minvalue = MINVAL(rla_1DR4_values)
                           ENDIF
 
                           IF (ll_max_comp) THEN
                              rl_R4_maxvalue = MAXVAL(rla_1DR4_values,  &
                                       mask=rla_1DR4_values .NE. rl_fillvalue)
                           ENDIF
 
                           DEALLOCATE(rla_1DR4_values, stat=il_status)
                           il_status = fi_memError(il_status, ' rla_1DR4_values',cl_fonction)
 
 
                        ! NF90_FLOAT array is a 2D array
                        CASE (2)
 
                           ALLOCATE(rla_2DR4_values(ila_len(1), &
                                                    ila_len(2)), &
                                    stat=il_status)
                           il_status = fi_memError(il_status, ' rla_2DR4_values',cl_fonction)
                           il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                               il_ji, &
                                                               rla_2DR4_values),cl_fonction)
 
                           IF (rl_coeff .NE. 1.0) THEN
                              WHERE (rla_2DR4_values .NE. rl_fillvalue)
                                 rla_2DR4_values = rla_2DR4_values*rl_coeff
                              ENDWHERE
 
                              il_status = fi_ncError(NF90_PUT_VAR(il_file_id, &
                                                                  il_ji, &
                                                                  rla_2DR4_values),cl_fonction)
 
                           ENDIF
 
                           IF (ll_min_comp) THEN
                              rl_R4_minvalue = MINVAL(rla_2DR4_values)
                           ENDIF
 
                           IF (ll_max_comp) THEN
                              rl_R4_maxvalue = MAXVAL(rla_2DR4_values, &
                                       mask=rla_2DR4_values .NE. rl_fillvalue)
                           ENDIF
 
                           DEALLOCATE(rla_2DR4_values, stat=il_status)
                           il_status = fi_memError(il_status, ' rla_2DR4_values',cl_fonction)
 
 
                        ! NF90_FLOAT array is a 3D array
                        CASE (3)
 
                           ALLOCATE(rla_3DR4_values(ila_len(1), &
                                                    ila_len(2), &
                                                    ila_len(3)), &
                                    stat=il_status)
                           il_status = fi_memError(il_status, ' rla_3DR4_values',cl_fonction)
 
                           il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                               il_ji, &
                                                               rla_3DR4_values),cl_fonction)
 
                           IF (rl_coeff .NE. 1.0) THEN
                              WHERE (rla_3DR4_values .NE. rl_fillvalue)
                                 rla_3DR4_values = rla_3DR4_values*rl_coeff
                              ENDWHERE
 
                              il_status = fi_ncError(NF90_PUT_VAR(il_file_id, &
                                                                  il_ji, &
                                                                  rla_3DR4_values),cl_fonction)
                           ENDIF
 
                           IF (ll_min_comp) THEN
                              rl_R4_minvalue = MINVAL(rla_3DR4_values)
                           ENDIF
 
                           IF (ll_max_comp) THEN
                              rl_R4_maxvalue = MAXVAL(rla_3DR4_values,  &
                                       mask=rla_3DR4_values .NE. rl_fillvalue)
                           ENDIF
 
                           DEALLOCATE(rla_3DR4_values, stat=il_status)
                           il_status = fi_memError(il_status, ' rla_3DR4_values',cl_fonction)
 
                        ! NF90_FLOAT array is a 4D array
                        CASE (4)
 
                           ALLOCATE(rla_4DR4_values(ila_len(1), &
                                                    ila_len(2), &
                                                    ila_len(3), &
                                                    ila_len(4)), &
                                    stat=il_status)
                           il_status = fi_memError(il_status, ' rla_4DR4_values',cl_fonction)
 
                           il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                               il_ji, &
                                                               rla_4DR4_values),cl_fonction)
 
                           IF (rl_coeff .NE. 1.0) THEN
                              WHERE (rla_4DR4_values .NE. rl_fillvalue)
                                 rla_4DR4_values = rla_4DR4_values*rl_coeff
                              ENDWHERE
 
                              il_status = fi_ncError(NF90_PUT_VAR(il_file_id, &
                                                                  il_ji, &
                                                                  rla_4DR4_values),cl_fonction)
                           ENDIF
 
                           IF (ll_min_comp) THEN
                              rl_R4_minvalue = MINVAL(rla_4DR4_values)
                           ENDIF
 
                           IF (ll_max_comp) THEN
                              rl_R4_maxvalue = MAXVAL(rla_4DR4_values,  &
                                       mask=rla_4DR4_values .NE. rl_fillvalue)
                           ENDIF
 
                           DEALLOCATE(rla_4DR4_values, stat=il_status)
                           il_status = fi_memError(il_status, ' rla_4DR4_values',cl_fonction)
 
                        CASE DEFAULT
                           WRITE(0,*) ' MIOL_transform_opatocf_NC : number of dimension exceed'
                           CALL flush(0)
                           STOP
 
                     ENDSELECT
 
 
                  ! type is NF90_DOUBLE
                  CASE (NF90_DOUBLE)
 
                     ! NF90_FLOAT array is a 1D array
                     SELECT CASE (il_nbdim)
 
                        CASE (1)
 
                           ALLOCATE(rla_1DR8_values(ila_len(1)), &
                                    stat=il_status)
                           il_status = fi_memError(il_status, ' rla_1DR8_values',cl_fonction)
 
                           il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                               il_ji, &
                                                               rla_1DR8_values),cl_fonction)
 
                           IF (rl_coeff .NE. 1.0) THEN
                              WHERE (rla_1DR8_values .NE. rl_fillvalue)
                                 rla_1DR8_values = rla_1DR8_values*rl_coeff
                              ENDWHERE
 
                              il_status = fi_ncError(NF90_PUT_VAR(il_file_id, &
                                                                  il_ji, &
                                                                  rla_1DR8_values),cl_fonction)
                           ENDIF
 
                           IF (ll_min_comp) THEN
                              rl_R8_minvalue = MINVAL(rla_1DR8_values)
                           ENDIF
 
                           IF (ll_max_comp) THEN
                              rl_R8_maxvalue = MAXVAL(rla_1DR8_values,  &
                                       mask=rla_1DR8_values .NE. rl_fillvalue)
                           ENDIF
 
                           DEALLOCATE(rla_1DR8_values, stat=il_status)
                           il_status = fi_memError(il_status, ' rla_1DR8_values',cl_fonction)
 
 
                        ! NF90_FLOAT array is a 2D array
                        CASE (2)
 
                           ALLOCATE(rla_2DR8_values(ila_len(1), &
                                                    ila_len(2)), &
                                    stat=il_status)
                           il_status = fi_memError(il_status, ' rla_2DR8_values',cl_fonction)
 
                           il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                               il_ji, &
                                                               rla_2DR8_values),cl_fonction)
 
                           IF (rl_coeff .NE. 1.0) THEN
                              WHERE (rla_2DR8_values .NE. rl_fillvalue)
                                 rla_2DR8_values = rla_2DR8_values*rl_coeff
                              ENDWHERE
 
                              il_status = fi_ncError(NF90_PUT_VAR(il_file_id, &
                                                                  il_ji, &
                                                                  rla_2DR8_values),cl_fonction)
 
                           ENDIF
 
                           IF (ll_min_comp) THEN
                              rl_R8_minvalue = MINVAL(rla_2DR8_values)
                           ENDIF
 
                           IF (ll_max_comp) THEN
                              rl_R8_maxvalue = MAXVAL(rla_2DR8_values, &
                                       mask=rla_2DR8_values .NE. rl_fillvalue)
                           ENDIF
 
                           DEALLOCATE(rla_2DR8_values, stat=il_status)
                           il_status = fi_memError(il_status, ' rla_2DR8_values',cl_fonction)
 
 
                        ! NF90_FLOAT array is a 3D array
                        CASE (3)
 
                           ALLOCATE(rla_3DR8_values(ila_len(1), &
                                                    ila_len(2), &
                                                    ila_len(3)), &
                                    stat=il_status)
                           il_status = fi_memError(il_status, ' rla_3DR8_values',cl_fonction)
 
                           il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                               il_ji, &
                                                               rla_3DR8_values),cl_fonction)
 
                           IF (rl_coeff .NE. 1.0) THEN
                              WHERE (rla_3DR8_values .NE. rl_fillvalue)
                                 rla_3DR8_values = rla_3DR8_values*rl_coeff
                              ENDWHERE
 
                              il_status = fi_ncError(NF90_PUT_VAR(il_file_id, &
                                                                  il_ji, &
                                                                  rla_3DR8_values),cl_fonction)
                           ENDIF
 
                           IF (ll_min_comp) THEN
                              rl_R8_minvalue = MINVAL(rla_3DR8_values)
                           ENDIF
 
                           IF (ll_max_comp) THEN
                              rl_R8_maxvalue = MAXVAL(rla_3DR8_values,  &
                                       mask=rla_3DR8_values .NE. rl_fillvalue)
                           ENDIF
 
                           DEALLOCATE(rla_3DR8_values, stat=il_status)
                           il_status = fi_memError(il_status, ' rla_3DR8_values',cl_fonction)
 
                        ! NF90_FLOAT array is a 4D array
                        CASE (4)
 
                           ALLOCATE(rla_4DR8_values(ila_len(1), &
                                                    ila_len(2), &
                                                    ila_len(3), &
                                                    ila_len(4)), &
                                    stat=il_status)
                           il_status = fi_memError(il_status, ' rla_4DR8_values',cl_fonction)
 
                           il_status = fi_ncError(NF90_GET_VAR(il_file_id, &
                                                               il_ji, &
                                                               rla_4DR8_values),cl_fonction)
 
                           IF (rl_coeff .NE. 1.0) THEN
                              WHERE (rla_4DR8_values .NE. rl_fillvalue)
                                 rla_4DR8_values = rla_4DR8_values*rl_coeff
                              ENDWHERE
 
                              il_status = fi_ncError(NF90_PUT_VAR(il_file_id, &
                                                                  il_ji, &
                                                                  rla_4DR8_values),cl_fonction)
                           ENDIF
 
                           IF (ll_min_comp) THEN
                              rl_R8_minvalue = MINVAL(rla_4DR8_values)
                           ENDIF
 
                           IF (ll_max_comp) THEN
                              rl_R8_maxvalue = MAXVAL(rla_4DR8_values,  &
                                       mask=rla_4DR8_values .NE. rl_fillvalue)
                           ENDIF
 
                           DEALLOCATE(rla_4DR8_values, stat=il_status)
                           il_status = fi_memError(il_status, ' rla_4DR8_values',cl_fonction)
 
                        CASE DEFAULT
                           WRITE(0,*) ' MIOL_transform_opatocf_NC : number of dimension exceed'
                           CALL flush(0)
                           STOP
 
                     ENDSELECT
 
                  CASE DEFAULT
                     WRITE(0,*) ' MIOL_transform_opatocf_NC : variable type unknown'
                     CALL flush(0)
                     STOP
 
               ENDSELECT
 
               ! set in define mode
               il_status = fi_ncError(NF90_REDEF(il_file_id),cl_fonction)
 
            ENDIF
 
 
            ! write variable attribute
            OPEN(20, file=cl_varfilename, status='old', form='formatted')
            READ(20, nb_att)
 
            IF (ALLOCATED(cl_attname)) DEALLOCATE(cl_attname)
            ALLOCATE(cl_attname(il_nbatt), stat=il_status)
            il_status = fi_memError(il_status, ' cl_attname',cl_fonction)
 
            IF (ALLOCATED(cl_attvalue)) DEALLOCATE(cl_attvalue)
            ALLOCATE(cl_attvalue(il_nbatt), stat=il_status)
            il_status = fi_memError(il_status, ' cl_attvalue',cl_fonction)
 
            DO il_jj = 1, il_nbatt
 
               READ(20, *) cl_attname(il_jj)
               READ(20, '(A100)') cl_attvalue(il_jj)
 
               SELECT CASE (TRIM(cl_attvalue(il_jj)))
 
                  CASE ('minvalue')
 
                     SELECT CASE (il_type)
                        CASE (NF90_INT)
                           il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                               il_ji, &
                                                               cl_attname(il_jj), &
                                                               il_int_minvalue),cl_fonction)
 
                        CASE (NF90_FLOAT)
                           il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                               il_ji, &
                                                               cl_attname(il_jj), &
                                                               rl_R4_minvalue),cl_fonction)
 
                        CASE (NF90_DOUBLE)
                           il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                               il_ji, &
                                                               cl_attname(il_jj), &
                                                               rl_R8_minvalue),cl_fonction)
 
                        CASE DEFAULT
 
                     ENDSELECT
 
                  CASE ('maxvalue')
 
                     SELECT CASE (il_type)
                        CASE (NF90_INT)
                           il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                               il_ji, &
                                                               cl_attname(il_jj), &
                                                               il_int_maxvalue),cl_fonction)
 
                        CASE (NF90_FLOAT)
                           il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                               il_ji, &
                                                               cl_attname(il_jj), &
                                                               rl_R4_maxvalue),cl_fonction)
 
                        CASE (NF90_DOUBLE)
                           il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                               il_ji, &
                                                               cl_attname(il_jj), &
                                                               rl_R8_maxvalue),cl_fonction)
 
                        CASE DEFAULT
 
                     ENDSELECT
 
                  CASE ('stepvalue')
 
                     SELECT CASE (il_type)
                        CASE (NF90_INT)
                           il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                               il_ji, &
                                                               cl_attname(il_jj), &
                                                               il_int_stepvalue),cl_fonction)
 
                        CASE (NF90_FLOAT)
                           il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                               il_ji, &
                                                               cl_attname(il_jj), &
                                                               rl_R4_stepvalue),cl_fonction)
 
                        CASE (NF90_DOUBLE)
                           il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                               il_ji, &
                                                               cl_attname(il_jj), &
                                                               rl_R8_stepvalue),cl_fonction)
 
                        CASE DEFAULT
 
                     ENDSELECT
 
                  CASE ('fillvalue')
 
                     il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                         il_ji, &
                                                         cl_attname(il_jj), &
                                                         rl_fillvalue),cl_fonction)
 
                  CASE ('missvalue')
                     il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                         il_ji, &
                                                         cl_attname(il_jj), &
                                                         rl_missvalue),cl_fonction)
 
                  CASE DEFAULT
                     il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                         il_ji, &
                                                         cl_attname(il_jj), &
                                                         TRIM(cl_attvalue(il_jj))),cl_fonction)
 
               ENDSELECT
 
            ENDDO
 
            IF (ALLOCATED(cl_attname)) DEALLOCATE(cl_attname, stat = il_status)
            il_status = fi_memError(il_status, ' cl_attname',cl_fonction)
 
            IF (ALLOCATED(cl_attvalue)) DEALLOCATE(cl_attvalue, stat = il_status)
            il_status = fi_memError(il_status, ' cl_attvalue',cl_fonction)
 
         ENDDO
 
         !-----------------------------------------------------------------------
         ! reload the number of dimensions
         il_status = fi_ncError(NF90_INQUIRE(il_file_id, &
                                             il_nbdim),cl_fonction)
 
         IF (ll_lonlat_is1D) THEN
 
            !-----------------------------------------------------------------------
            ! if the longitude and the latitude are 1D variables, the dimensions
            ! name change
            DO il_ji = 1, il_nbdim
 
               il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
                                                             il_ji, &
                                                             cl_dimname),cl_fonction)
 
               cl_eqfilename = TRIM(cl_equivalencesPath)//TRIM(cl_dimname)//TRIM(cl_ext)
 
 
               OPEN(20, file=TRIM(cl_eqfilename), status='old', form='formatted')
               READ(20, equival)
               CLOSE(20)
 
               IF (TRIM(cl_dimname) .NE. TRIM(cl_varname_out)) THEN
                  il_status = fi_ncError(NF90_RENAME_DIM(il_file_id,  &
                                                         il_ji, &
                                                         TRIM(cl_varname_out)),cl_fonction)
               ENDIF
 
            ENDDO
 
         ENDIF
 
         !-----------------------------------------------------------------------
         ! delete old global attributes
         DO il_ji = 1, il_nbgatt
            il_status = fi_ncError(NF90_INQ_ATTNAME(il_file_id, &
                                                    NF90_GLOBAL, &
                                                    1, &
                                                    cl_tmpattname),cl_fonction)
            il_status = fi_ncError(NF90_DEL_ATT(il_file_id, &
                                                NF90_GLOBAL, &
                                                cl_tmpattname),cl_fonction)
         ENDDO
 
         !------------------------------------------------------------------------
         ! write new global attributes
            cl_gattfilename = cd_gattfilename
 
 
         OPEN(20, file=TRIM(cl_gattfilename), status='old', form='formatted')
         READ(20, nb_att)
 
         IF (ALLOCATED(cl_attname)) DEALLOCATE(cl_attname)
         ALLOCATE(cl_attname(il_nbatt), stat = il_status)
         il_status = fi_memError(il_status, ' cl_attname',cl_fonction)
 
         IF (ALLOCATED(cl_attvalue)) DEALLOCATE(cl_attvalue)
         ALLOCATE(cl_attvalue(il_nbatt), stat = il_status)
         il_status = fi_memError(il_status, ' cl_attvalue',cl_fonction)
 
         DO il_ji = 1, il_nbatt
 
            READ(20, *) cl_attname(il_ji)
            READ(20, '(A100)') cl_attvalue(il_ji)
 
            SELECT CASE (TRIM(cl_attvalue(il_ji)))
 
               CASE ('file_name')
 
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      NF90_GLOBAL, &
                                                      cl_attname(il_ji), &
                                                      TRIM(cl_filename)),cl_fonction)
 
               CASE ('date')
 
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      NF90_GLOBAL, &
                                                      cl_attname(il_ji), &
                                                      TRIM(cl_creationdate)),cl_fonction)
 
               CASE ('history')
 
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      NF90_GLOBAL, &
                                                      cl_attname(il_ji), &
                                                      TRIM(cl_history)),cl_fonction)
 
               CASE ('institution')
 
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      NF90_GLOBAL, &
                                                      cl_attname(il_ji), &
                                                      TRIM(cl_institution)),cl_fonction)
 
               CASE ('references')
 
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      NF90_GLOBAL, &
                                                      cl_attname(il_ji), &
                                                      TRIM(cl_references)),cl_fonction)
 
               CASE ('longitude_min')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      NF90_GLOBAL, &
                                                      cl_attname(il_ji), &
                                                      rl_lonminvalue),cl_fonction)
 
               CASE ('latitude_min')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      NF90_GLOBAL, &
                                                      cl_attname(il_ji), &
                                                      rl_latminvalue),cl_fonction)
 
               CASE ('longitude_max')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      NF90_GLOBAL, &
                                                      cl_attname(il_ji), &
                                                      rl_lonmaxvalue),cl_fonction)
 
               CASE ('latitude_max')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      NF90_GLOBAL, &
                                                      cl_attname(il_ji), &
                                                      rl_latmaxvalue),cl_fonction)
 
               CASE ('z_min')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      NF90_GLOBAL, &
                                                      cl_attname(il_ji), &
                                                      rl_depthminvalue),cl_fonction)
 
               CASE ('z_max')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      NF90_GLOBAL, &
                                                      cl_attname(il_ji), &
                                                      rl_depthmaxvalue),cl_fonction)
 
               CASE ('field_julian_date')
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      NF90_GLOBAL, &
                                                      cl_attname(il_ji), &
                                                      id_juliandate),cl_fonction)
 
               CASE DEFAULT
                  il_status = fi_ncError(NF90_PUT_ATT(il_file_id, &
                                                      NF90_GLOBAL, &
                                                      cl_attname(il_ji), &
                                                      TRIM(cl_attvalue(il_ji))),cl_fonction)
 
            ENDSELECT
 
         ENDDO
 
         CLOSE(20)
 
 
         !-----------------------------------------------------------------------
         ! out of define mode
 
         il_status = fi_ncError(NF90_ENDDEF(il_file_id),cl_fonction)
 
 
         !-----------------------------------------------------------------------
         ! close file
 
         il_status = fi_ncError(NF90_CLOSE(il_file_id),cl_fonction)
 
 
 
       END SUBROUTINE MIOL_transform_opatocf_NC
 
 
  !******************************************************************************
  !*****************************************************************************
  !******************************************************************************
 
    !!======================================================================
    !> \brief
    !! Description: This function transform OPA format variable names to a
    !!              CF format. (ex: “votemper” becomes “temperature”, etc...)
    !!              with this new function you can compute short values
    !!
    !!   @param cd_input_filename path of the input file
    !!   @param cd_output_filename path of the output file
    !!   @param cd_gattfilename path of the global attribut file
    !!   @param id_juliandate juliandate of the file
    !!   @param cd_zone zone of the file
    !!   @param ld_compute_sh logical to know if you want your output file in short
    !!
    !! History :
    !!        \n  07/2006  (F. Messal) Creation
    !!        \n  11/2006  (F. Messal) CVS version 1.0
    !!         \n  05/2008  (C.REGNIER) Write short for the 2-3-4D variables
    !!                                Add MIOL to read the output files
    !<
    !!======================================================================

  SUBROUTINE MIOL_transform_opatocf_GODAE_NC(cd_input_filename, &
                                             cd_output_filename, &
                                             cd_gattfilename, &
                                             id_juliandate,&
                                             cd_zone,&
                                             ld_compute_sh)

    USE MIOL
    USE MIOL_param
    USE MCAL, only : MCAL_compute_short,MCAL_conversion_STU50_CAL
    USE MFT_error
    USE netcdf
    IMPLICIT NONE

       !-----------------------------------------------------------------------

    CHARACTER(LEN=*), INTENT(IN) :: cd_input_filename, cd_output_filename
    CHARACTER(LEN=*), INTENT(IN) :: cd_gattfilename,cd_zone
    INTEGER, INTENT(IN)          :: id_juliandate
    LOGICAL,INTENT(IN)           :: ld_compute_sh
   !
    CHARACTER(LEN=255),DIMENSION(2) :: cla_attname
    CHARACTER(LEN=255) ::  cl_ext, cl_dimname, cl_eqfilename, cl_varname, &
                           cl_varname_out, cl_varfilename, cl_tmpattname, &
                           cl_equivalencesPath,cl_globalAttributesPath, &
                           cl_variableAttributesPath,&
                           cl_gattfilename,cl_varname_ini,cl_history, &
                           cl_creationdate,cl_institution,cl_references,&
                           cl_filename
    CHARACTER(LEN=8)  :: cl_date
    CHARACTER(LEN=10)  :: cl_time
    CHARACTER(LEN=15) :: cl_jdate
    CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_globalattname
    CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE :: cla_globalattvalue
    CHARACTER(LEN=255), DIMENSION(:), POINTER :: clpa_varname,clpa_dimname

    INTEGER  :: il_file_id, il_nbdim, il_nbvar, il_nbatt, il_nbgatt, il_ji, &
                il_jj, il_type, il_status, il_nbVariable,il_int_minvalue,&
                il_int_maxvalue, il_int_stepvalue,il_inputFileId,il_extrapFileId,il_nx, &
                il_ny,il_nz,il_nt,il_stereo
    
    INTEGER ,DIMENSION(1)                         :: ila_1Ddimsize
    INTEGER, DIMENSION(2)                         :: ila_2Ddimsize
    INTEGER, DIMENSION(3)                         :: ila_3Ddimsize
    INTEGER, DIMENSION(4)                         :: ila_4Ddimsize,ila_dimids
    !integer(KIND=1), parameter     :: sNaN    = Z"7FC00000"
    INTEGER(KIND=4) :: il_offset
    INTEGER(KIND=2) :: il_minvalue,il_maxvalue,il_missvalue,il_min,il_max
    INTEGER(KIND=2), DIMENSION(4)                 :: ila_specialvalue
    INTEGER, DIMENSION(5)                         :: ila_specialvalue2

    INTEGER,         DIMENSION(:),POINTER         :: ilpa_nbdim,ilpa_dimlen
    INTEGER(KIND=2), DIMENSION(:,:),POINTER       :: ilpa_variable_2D
    INTEGER(KIND=2), DIMENSION(:,:,:),POINTER     :: ilpa_variable_3D
    INTEGER(KIND=2), DIMENSION(:,:,:,:),POINTER   :: ilpa_variable_4D   
  
    INTEGER, DIMENSION(:,:,:,:), POINTER          :: ilpa_4Dint_values
    INTEGER, DIMENSION(:,:,:), POINTER            :: ilpa_3Dint_values
    INTEGER, DIMENSION(:,:), POINTER              :: ilpa_2Dint_values
    INTEGER, DIMENSION(:), POINTER                :: ilpa_1Dint_values
    INTEGER, DIMENSION(24)                         :: ila_typevalue
    REAL(KIND=4) :: rl_R4_minvalue,rl_R4_maxvalue,rl_R4_stepvalue, &
                    rl_lonminvalue,rl_lonmaxvalue,rl_latminvalue, &
                    rl_latmaxvalue,rl_depthminvalue,rl_depthmaxvalue,&
                     rl_missvalue,rl_fillvalue,rl_coeff,rl_min,rl_max,rl_nbsec
    
    REAL(KIND=4), DIMENSION(:,:,:,:), POINTER     :: rlpa_4DR4_values
    REAL(KIND=4), DIMENSION(:,:,:), POINTER       :: rlpa_3DR4_values
    REAL(KIND=4), DIMENSION(:,:), POINTER         :: rlpa_2DR4_values
    REAL(KIND=4), DIMENSION(:), POINTER           :: rlpa_1DR4_values
    REAL(KIND=4), DIMENSION(:),ALLOCATABLE        :: rla_globalattvalue
    REAL(KIND=8)                                  :: rl_scale_fact,rl_R8_minvalue, rl_R8_maxvalue, rl_R8_stepvalue,&
                                                     rl_offset
    REAL(KIND=8), DIMENSION(2)                    :: rla_att
    REAL(KIND=8), DIMENSION(:,:,:,:), POINTER     :: rlpa_4DR8_values
    REAL(KIND=8), DIMENSION(:,:,:), POINTER       :: rlpa_3DR8_values
    REAL(KIND=8), DIMENSION(:,:), POINTER         :: rlpa_2DR8_values
    REAL(KIND=8), DIMENSION(:), POINTER           :: rlpa_1DR8_values

    LOGICAL :: ll_minvalue_isdef, ll_maxvalue_isdef, ll_minname_isdef, &
               ll_maxname_isdef,ll_stepvalue_isdef, ll_lonlat_is1D
    !
    !*-----------------------------------------------------------------------------
    !
    !** Declaration Namelist
    NAMELIST /equival/ cl_varname_out,&
         rl_coeff,&
         rl_min,&
         rl_max,&
         rl_offset
    NAMELIST /nb_att/ il_nbatt
!
    !**-----------------------------------------------------------------------
    !** Init
    WRITE(0,*) 'ZONE ::',  cd_zone

    !cl_parampath='/home/smer/smer883/SRC/SGPS/parameter/'
    IF ( TRIM(cd_zone) == 'ARC' ) THEN
       cl_equivalencesPath = TRIM(cp_miolParameterPath)//'/equivalences/equivalences_arc/'
    ELSE
       cl_equivalencesPath = TRIM(cp_miolParameterPath)//'/equivalences/'
    ENDIF

    cl_globalAttributesPath = TRIM(cp_miolParameterPath)//'attributes/global/'
    cl_variableAttributesPath = TRIM(cp_miolParameterPath)//'/attributes/variable/'
    cl_ext = '.in'

    rl_fillvalue = rg_fillvalue
    rl_missvalue = rg_missvalue
    il_fillvalue=-(((2**16-2))/2)
    il_missvalue=il_fillvalue

   
    ll_lonlat_is1D = .TRUE.
    cla_attname(1)='scale_factor'
    cla_attname(2)='add_offset'

    !========================================================================
    ! Read variables name...
    !========================================================================
    WRITE(0,*) '  '
    WRITE(0,*) '  -> Read variables name... ',cd_input_filename
    !
    CALL MIOL_openr_file_NC(cd_input_filename, &
         il_inputFileId)
    CALL MIOL_inquire_variables_NC(il_inputFileId, &
         il_nbVariable, &
         clpa_varname, &
         ilpa_nbdim)
    CALL MIOL_inquire_dimensions_NC(il_inputFileId, &
         il_nbDim, &
         clpa_dimname, &
         ilpa_dimlen)

    ! Output field creation
    WRITE(0,*) '  -> Create Output file... ',cd_output_filename
    CALL hdlerr(NF90_CREATE(cd_output_filename, &
         NF90_NOCLOBBER, &
         il_extrapFileId),'PB NF90_CREATE L163')

    CALL flush(0)
    CALL hdlerr(NF90_CLOSE(il_extrapFileId),'PB NF90_CLOSE L166')
    CALL flush(0)
    CALL MIOL_openw_file_NC(cd_output_filename, &
         il_extrapFileId)
    CALL flush(0)
    DO il_jj=1, il_nbDim
       SELECT CASE (TRIM(clpa_dimname(il_jj)))
       CASE ('x')
          il_nx = ilpa_dimlen(il_jj)
       CASE ('y')
          il_ny = ilpa_dimlen(il_jj)
       CASE ('deptht', 'depthu', 'depthv', 'depthw','depth')
          il_nz = ilpa_dimlen(il_jj)
       CASE ('time_counter')
          il_nt = ilpa_dimlen(il_jj)
       CASE DEFAULT
       ENDSELECT
    ENDDO
    !-----------------------------------------------------------------------
    ! Copy the file
    WRITE(0,*) '  -> Copy file... '
    CALL copy_files(il_inputFileId, &
         il_extrapFileId, &
         il_nx, &
         il_ny, &
         il_nz, &
         il_nt)

    WRITE(0,*) '  -> Copy file ok ... ',il_nbVariable

    !-----------------------------------------------------------------------
    ! Open the new file in wrinting and define mode
    il_status = fi_ncError(NF90_OPEN(TRIM(cd_output_filename), &
                           NF90_WRITE, &
                           il_file_id),'PB NF90_OPEN l.201')
    il_status = fi_ncError(NF90_REDEF(il_file_id),'PB NF90_OPEN l.194')

    !! IF Arctic 
    !-----------------------------------------------------------------------
    ! Load the number of dimensions, the number of variables and the number
    ! of attributes

    il_status = fi_ncError(NF90_INQUIRE(il_file_id, &
         il_nbdim, &
         il_nbvar, &
         il_nbgatt),'NF90_INQUIRE PB l213')

    il_nbdim=il_nbDim
    !-----------------------------------------------------------------------
    ! Delete the old attributes, change the variable name and write the new
    ! attributes
    DO il_ji = 1, il_nbVariable !il_nbvar
       ! initialization
       il_int_minvalue = 9999
       il_int_maxvalue = 9999
       il_int_stepvalue = 0
       rl_R4_minvalue = 9999.
       rl_R4_maxvalue = 9999.
       rl_R4_stepvalue = 0.
       rl_R8_minvalue = 9999.
       rl_R8_maxvalue = 9999.
       rl_R8_stepvalue = 0.
       ll_minvalue_isdef = .FALSE.
       ll_maxvalue_isdef = .FALSE.
       ll_stepvalue_isdef = .FALSE.
       ll_minname_isdef = .FALSE.
       ll_maxname_isdef = .FALSE.
      

       ! find variable name
       il_status = fi_ncError(NF90_INQUIRE_VARIABLE(il_inputFileId, &
            il_ji, &
            cl_varname, &
            il_type, &
            il_nbdim, &
            ila_dimids, &
            il_nbatt),'NF90_INQUIRE_VARIABLE PB l244')
       !-------------------------------
       IF ((TRIM(cl_varname) .EQ. 'longitude') .OR. &
            TRIM(cl_varname) .EQ. 'latitude') THEN
          IF (il_nbdim .GT. 1) THEN
             ll_lonlat_is1D = .FALSE.
          ENDIF
       ENDIF

       ! find the value of 'valid_min', 'valid_max', 'step' attributes
       ! if they're defined, we copy the values
       DO il_jj = 1, il_nbatt

          il_status = fi_ncError(NF90_INQ_ATTNAME(il_inputFileId, &
               il_ji, &
               il_jj, &
               cl_tmpattname),'NF90_INQ_ATTNAME PB l260')


          SELECT CASE(TRIM(cl_tmpattname))

          CASE('valid_min')
             ll_minvalue_isdef = .TRUE.

             SELECT CASE (il_type)

             CASE (NF90_INT)
                il_status = fi_ncError(NF90_GET_ATT(il_inputFileId, &
                     il_ji, &
                     TRIM(cl_tmpattname), &
                     il_int_minvalue ),'NF90_GET_ATT INT PB l274')

             CASE (NF90_FLOAT)
                il_status = fi_ncError(NF90_GET_ATT(il_inputFileId, &
                     il_ji, &
                     TRIM(cl_tmpattname), &
                     rl_R4_minvalue ),'NF90_GET_ATT FLOAT PB l280')

             CASE (NF90_DOUBLE)
                il_status = fi_ncError(NF90_GET_ATT(il_inputFileId, &
                     il_ji, &
                     TRIM(cl_tmpattname), &
                     rl_R8_minvalue ),'NF90_GET_ATT DOUBLE PB l286')

             CASE DEFAULT
                WRITE(0,*) ' MIOL_transform_opatocf_NC : not float or double or integer variable type.'
                CALL flush(0)
                STOP

             ENDSELECT
          CASE('valid_max')
             ll_maxvalue_isdef = .TRUE.

             SELECT CASE (il_type)

             CASE (NF90_INT)
                il_status = fi_ncError(NF90_GET_ATT(il_inputFileId, &
                     il_ji, &
                     TRIM(cl_tmpattname), &
                     il_int_maxvalue ),'NF90_GET_ATT INT PB l303')

             CASE (NF90_FLOAT)
                il_status = fi_ncError(NF90_GET_ATT(il_inputFileId, &
                     il_ji, &
                     TRIM(cl_tmpattname), &
                     rl_R4_maxvalue ),'NF90_GET_ATT FLOAT PB l309')

             CASE (NF90_DOUBLE)
                il_status = fi_ncError(NF90_GET_ATT(il_inputFileId, &
                     il_ji, &
                     TRIM(cl_tmpattname), &
                     rl_R8_maxvalue ),'NF90_GET_ATT DOUBLE PB l315')

             CASE DEFAULT
                WRITE(0,*) ' MIOL_transform_opatocf_NC : not float or double or integer variable type.'
                CALL flush(0)
                STOP

             ENDSELECT

          CASE('step')
             ll_stepvalue_isdef = .TRUE.

             SELECT CASE (il_type)

             CASE (NF90_INT)
                il_status = fi_ncError(NF90_GET_ATT(il_inputFileId, &
                     il_ji, &
                     TRIM(cl_tmpattname), &
                     il_int_stepvalue ),'NF90_GET_ATT INT PB l333')

             CASE (NF90_FLOAT)
                il_status = fi_ncError(NF90_GET_ATT(il_inputFileId, &
                     il_ji, &
                     TRIM(cl_tmpattname), &
                     rl_R4_stepvalue ),'NF90_GET_ATT FLOAT PB l339')

             CASE (NF90_DOUBLE)
                il_status = fi_ncError(NF90_GET_ATT(il_inputFileId, &
                     il_ji, &
                     TRIM(cl_tmpattname), &
                     rl_R8_stepvalue ),'NF90_GET_ATT DOUBLE PB l345')

             CASE DEFAULT
                WRITE(0,*) ' MIOL_transform_opatocf_NC : not float or double or integer variable type.'
                CALL flush(0)
                STOP

             ENDSELECT

          CASE DEFAULT

          ENDSELECT

         ! WRITE(0,*) ' select variable : ',TRIM(cl_varname)
          cl_varname_ini=TRIM(cl_varname)
          SELECT CASE (TRIM(cl_varname))

          CASE ('longitude')
             rl_lonminvalue = rl_R4_minvalue
             rl_lonmaxvalue = rl_R4_maxvalue

          CASE ('latitude')
             rl_latminvalue = rl_R4_minvalue
             rl_latmaxvalue = rl_R4_maxvalue

          CASE ('depth')
             rl_depthminvalue= rl_R4_minvalue
             rl_depthmaxvalue = rl_R4_maxvalue

          END SELECT

       ENDDO

       ! rename variable
       cl_eqfilename = TRIM(cl_equivalencesPath)//TRIM(cl_varname)//TRIM(cl_ext)
       OPEN(20, file=TRIM(cl_eqfilename), status='old', form='formatted')
       READ(20, equival)
       CLOSE(20)
       IF (TRIM(cl_varname) .NE. TRIM(cl_varname_out)) THEN
          cl_varname=cl_varname_out
       ENDIF

       ! load new attributes

       cl_varfilename = TRIM(cl_variableAttributesPath)//TRIM(cl_varname_out)//TRIM(cl_ext)
      ! WRITE(0,*) ' cl_varfilename = ',TRIM(cl_varfilename)
       OPEN(20, file=cl_varfilename, status='old', form='formatted')
       READ(20, nb_att)
       IF (ALLOCATED(cla_globalattname)) DEALLOCATE(cla_globalattname)
       ALLOCATE(cla_globalattname(il_nbatt), &
            stat=il_status)
       il_status = fi_memError(il_status, ' cla_attname','PB cla_globalattname')

       IF (ALLOCATED(cla_globalattvalue)) DEALLOCATE(cla_globalattvalue)
       ALLOCATE(cla_globalattvalue(il_nbatt), &
            stat=il_status)
       il_status = fi_memError(il_status, ' cla_attvalue','PB cla_globalattvalue ')
       ll_minname_isdef = .FALSE.
       ll_maxname_isdef = .FALSE.
       DO il_jj = 1, il_nbatt
          READ(20, *) cla_globalattname(il_jj)
          READ(20, '(A100)') cla_globalattvalue(il_jj)
          SELECT CASE(cla_globalattvalue(il_jj))
          CASE ('minvalue')
             ll_minname_isdef = .TRUE.
          CASE ('maxvalue')
             ll_maxname_isdef = .TRUE.
          CASE DEFAULT
          ENDSELECT
       ENDDO
       CLOSE(20)


       ! compute the new values with the coefficient
       ! and find the new min and max value
       il_status = fi_ncError(NF90_ENDDEF(il_file_id),'NF90_ENDDEF PB')
     
       ! variable type is NF90_INT
          SELECT CASE (il_type)

          CASE (NF90_INT)

             SELECT CASE (il_nbdim)

                ! NF90_INT array values is 1D a array
            CASE (0)
             ila_specialvalue2(1)=90
             ila_specialvalue2(2)=-45
             ila_specialvalue2(3)=0.8
             ila_specialvalue2(4)=0
             ila_specialvalue2(5)=0
             !il_stereo=sNaN
             il_stereo=0
             CALL MIOL_write_field_NC(il_file_id, &
                                      cl_varname , &
                                      il_stereo,&
                                      ila_specialvalue2)
               
                  print *,'ECRITURE scalar ok',cl_varname
             CASE (1)
                call MIOL_read_field_NC (il_inputFileId,&
                                         cl_varname_ini,&
                                         ilpa_1Dint_values, &
                                         ila_1Ddimsize)               
                il_int_minvalue = MINVAL(ilpa_1Dint_values)
                il_int_maxvalue = MAXVAL(ilpa_1Dint_values,  &
                       mask=ilpa_1Dint_values .NE. rl_fillvalue)
                call MIOL_write_field_NC (il_file_id,&
                                          cl_varname,&
                                          'X',&
                                          ilpa_1Dint_values)
                DEALLOCATE(ilpa_1Dint_values, stat = il_status)
                il_status = fi_memError(il_status, ' ilpa_1Dint_values','L443 TRANFORM_GODAE')


                ! NF90_INT array values is 2D a array
             CASE (2)
                call MIOL_read_field_NC (il_inputFileId,&
                                         cl_varname_ini,&
                                         ilpa_2Dint_values, &
                                         ila_2Ddimsize)

                il_int_minvalue = MINVAL(ilpa_2Dint_values)
                il_int_maxvalue = MAXVAL(ilpa_2Dint_values, &
                        mask=ilpa_2Dint_values .NE. rl_fillvalue)               
                call MIOL_write_field_NC (il_file_id,&
                                          cl_varname,&
                                          'XY',&
                                          ilpa_2Dint_values)
                DEALLOCATE(ilpa_2Dint_values, stat = il_status)
                il_status = fi_memError(il_status, ' ilpa_2Dint_values','L461 TRANFORM_GODAE')

                ! NF90_INT array values is a 3D array
             CASE (3)
                call MIOL_read_field_NC (il_inputFileId,&
                                         cl_varname_ini,&
                                         ilpa_3Dint_values, &
                                         ila_3Ddimsize)
                il_int_minvalue = MINVAL(ilpa_3Dint_values)
                il_int_maxvalue = MAXVAL(ilpa_3Dint_values,  &
                        mask=ilpa_3Dint_values .NE. rl_fillvalue)
                call MIOL_write_field_NC (il_file_id,&
                                          cl_varname,&
                                          'XYZ',&
                                          ilpa_3Dint_values)
                DEALLOCATE(ilpa_3Dint_values, stat=il_status)
                il_status = fi_memError(il_status, ' ilpa_3Dint_values','L477 TRANFORM_GODAE')


                ! NF90_INT array values is a 4D array
             CASE (4)
                call MIOL_read_field_NC (il_inputFileId,&
                                         cl_varname_ini,&
                                         ilpa_4Dint_values, &
                                         ila_4Ddimsize)
                il_int_minvalue = MINVAL(ilpa_4Dint_values)
                il_int_maxvalue = MAXVAL(ilpa_4Dint_values,  &
                     mask=ilpa_4Dint_values .NE. rl_fillvalue)
                call MIOL_write_field_NC (il_file_id,&
                                          cl_varname,&
                                          'XYZT',&
                                          ilpa_4Dint_values)
                DEALLOCATE(ilpa_4Dint_values, stat=il_status)
                il_status = fi_memError(il_status, ' ilpa_4Dint_values','L494 TRANFORM_GODAE')

             CASE DEFAULT
                WRITE(0,*) ' MIOL_transform_opatocf_NC : number of dimension exceed'
                CALL flush(0)
                STOP

             ENDSELECT

             ! variable type is NF90_FLOAT
          CASE (NF90_FLOAT)

             ! NF90_FLOAT array is a 1D array
             SELECT CASE (il_nbdim)

             CASE (1)
                call MIOL_read_field_NC (il_inputFileId,&
                                         cl_varname_ini,&
                                         rlpa_1DR4_values,&
                                         ila_1Ddimsize)

                !* Compute min, max
                rl_R4_minvalue = MINVAL(rlpa_1DR4_values)
                rl_R4_maxvalue = MAXVAL(rlpa_1DR4_values,  &
                        mask=rlpa_1DR4_values .NE. rl_fillvalue)
                
                IF (cl_varname == 'longitude' .OR. cl_varname == 'x' ) THEN
                   call MIOL_write_field_NC (il_file_id,&
                        cl_varname,&
                        'X',&
                        rlpa_1DR4_values)
                ELSE IF  (cl_varname == 'latitude'.OR. cl_varname == 'y') THEN
                   call MIOL_write_field_NC (il_file_id,&
                        cl_varname,&
                        'Y',&
                        rlpa_1DR4_values)
                ELSE IF  (cl_varname == 'depth') THEN
                   call MIOL_write_field_NC (il_file_id,&
                        cl_varname,&
                        'Z',&
                        rlpa_1DR4_values)
                 ELSE IF (cl_varname == 'x_kmbis') THEN
                      CALL MIOL_write_field_NC(il_file_id, &
                               'x_kmbis', &
                               'X', &
                               rlpa_1DR4_values)
                   ELSE IF (cl_varname == 'y_kmbis') THEN
                      CALL MIOL_write_field_NC(il_file_id, &
                               'y_kmbis', &
                               'Y', &
                               rlpa_1DR4_values)    
                 
                  print *,'ECRITURE  FLOAT 1D ok',cl_varname
  
                ENDIF


                DEALLOCATE(rlpa_1DR4_values, stat=il_status)
                il_status = fi_memError(il_status, ' rlpa_1DR4_values','L540 TRANFORM_GODAE')


                ! NF90_FLOAT array is a 2D array
             CASE (2)
                call MIOL_read_field_NC (il_inputFileId,&
                                         cl_varname_ini,&
                                         rlpa_2DR4_values, &
                                         ila_2Ddimsize)
                                          
               !** Gestion du Cas sans les shorts
                IF (ld_compute_sh) THEN
                   
                   ! Cas particulier lon,lat(x,y)
                   IF ((TRIM(cl_varname_ini) /= 'longitude').AND. &
                        (TRIM(cl_varname_ini) /= 'latitude')) THEN
                      print *,'Compute Short'
                    WHERE(rlpa_2DR4_values > rl_max ) rlpa_2DR4_values = rl_fillvalue
                    WHERE(rlpa_2DR4_values < rl_min ) rlpa_2DR4_values = rl_fillvalue
  
                   CALL MCAL_compute_short(rlpa_2DR4_values,&
                                              ila_2Ddimsize,& 
                                              rl_min,&
                                              rl_max,&
                                              ilpa_variable_2D,&
                                              rl_scale_fact,&
                                              il_offset)
                      print *,'Compute Short ok'
                     print *,'rl_min',rl_min,il_offset
                     print *,'Il_min',int((rl_min-il_offset)/rl_scale_fact)
                     print *,'Il_max',int((rl_max-il_offset)/rl_scale_fact)
                     il_min=int((rl_min-il_offset)/rl_scale_fact)
                     il_max=int((rl_max-il_offset)/rl_scale_fact)
                     
                     ! Definition Parametres
                     rla_att(1)=rl_scale_fact
                     rla_att(2)=REAL(rl_offset+il_offset)
                     ila_specialvalue(1)= il_min
                     ila_specialvalue(2)= il_max
                     ila_specialvalue(3)= il_fillvalue
                     ila_specialvalue(4)= il_fillvalue
                   
                     call MIOL_write_field_NC (il_file_id,&
                                               cl_varname,&
                                               'XY',&
                                               ilpa_variable_2D,&
                                               rla_att,&
                                               ila_specialvalue)
                  ELSE 
                     call MIOL_write_field_NC (il_file_id,&
                                               cl_varname,&
                                               'XY',&
                                               rlpa_2DR4_values)
                  ENDIF
                  ELSE
                     !* Write Float Variable
                     call MIOL_write_field_NC (il_file_id,&
                                                cl_varname,&
                                                'XY',&
                                                rlpa_2DR4_values)
                     
                  ENDIF
                print *,'ECRITURE variable 2D R4 ok',cl_varname,il_maxvalue,il_minvalue


                !IF(ALLOCATED(rlpa_2DR4_values)) DEALLOCATE(rlpa_2DR4_values, stat=il_status)
                !il_status = fi_memError(il_status, ' rlpa_2DR4_values','L594 TRANFORM_GODAE')
               ! IF(ALLOCATED(ilpa_variable_2D)) DEALLOCATE(ilpa_variable_2D, stat=il_status)
              !  il_status = fi_memError(il_status, ' ilpa_variable_2D','596 TRANFORM_GODAE')
                
                ! NF90_FLOAT array is a 3D array
             CASE (3)
                call MIOL_read_field_NC (il_inputFileId,&
                                         cl_varname_ini,&
                                         rlpa_3DR4_values, &
                                         ila_3Ddimsize)
               
                IF (ld_compute_sh) THEN
                    print *,'Compute Short 3D R4 ',cl_varname_ini
                    print *,'Min et Max',rl_max,rl_min
                   !* Patch pour psy2v3R1
		    WHERE(rlpa_3DR4_values > rl_max ) rlpa_3DR4_values = rl_fillvalue
		    WHERE(rlpa_3DR4_values < rl_min ) rlpa_3DR4_values = rl_fillvalue
		   CALL MCAL_compute_short(rlpa_3DR4_values,&
                                           ila_3Ddimsize,&
                                           rl_min,&
                                           rl_max,&
                                           ilpa_variable_3D,&
                                           rl_scale_fact,&
                                           il_offset)
                 
                   il_min=INT((rl_min-il_offset)/rl_scale_fact)
                   il_max=INT((rl_max-il_offset)/rl_scale_fact)
                   
                   ! Definition Parametres
                   rla_att(1)=rl_scale_fact
                   rla_att(2)=REAL(rl_offset+il_offset)
                   ila_specialvalue(1)= il_min
                   ila_specialvalue(2)= il_max
                   ila_specialvalue(3)= il_fillvalue
                   ila_specialvalue(4)= il_fillvalue
                   
                   call MIOL_write_field_NC (il_file_id,&
                        cl_varname,&
                        'XYZ',&
                        ilpa_variable_3D,&
                        rla_att,&
                        ila_specialvalue)
                ELSE
                   !* Write Float Variable
                   call MIOL_write_field_NC (il_file_id,&
                                             cl_varname,&
                                             'XYZ',&
                                              rlpa_3DR4_values)
                      
                ENDIF
                print *,'ECRITURE variable 3D R4 ok',cl_varname,il_maxvalue,il_minvalue

               ! IF(ALLOCATED(rlpa_3DR4_values)) DEALLOCATE(rlpa_3DR4_values, stat=il_status)
               ! il_status = fi_memError(il_status, ' rlpa_3DR4_values','L634 TRANFORM_GODAE')
               ! IF(ALLOCATED(ilpa_variable_3D)) DEALLOCATE(ilpa_variable_3D, stat=il_status)
               ! il_status = fi_memError(il_status, ' ila_3DI2_values','L636 TRANFORM_GODAE')

                ! NF90_FLOAT array is a 4D array
             CASE (4)
                call MIOL_read_field_NC (il_inputFileId,&
                                         cl_varname_ini,&
                                         rlpa_4DR4_values, &
                                         ila_4Ddimsize)
               	WHERE(rlpa_4DR4_values > rl_max ) rlpa_4DR4_values = rl_fillvalue
                WHERE(rlpa_4DR4_values < rl_min ) rlpa_4DR4_values = rl_fillvalue

		CALL MCAL_compute_short(rlpa_4DR4_values,&
                                        ila_4Ddimsize,&
                                        rl_min,&
                                        rl_max,&
                                        ilpa_variable_4D,&
                                        rl_scale_fact,&
                                        il_offset)

                il_min=INT((rl_min-il_offset)/rl_scale_fact)
                il_max=INT((rl_max-il_offset)/rl_scale_fact)
                
                rla_att(1)=REAL(rl_offset+il_offset)
                rla_att(2)=rl_scale_fact
                ila_specialvalue(1)= il_min
                ila_specialvalue(2)= il_max
                ila_specialvalue(3)= il_fillvalue
                ila_specialvalue(4)= il_fillvalue

                call MIOL_write_field_NC (il_file_id,&
                     cl_varname,&
                     'XYZT',&
                     ilpa_variable_4D,&
                     rla_att,& 
                     ila_specialvalue)

                print *,'ECRITURE variable 4D R4 ok',cl_varname,il_maxvalue,il_minvalue

           !   IF(ALLOCATED(rlpa_4DR4_values)) DEALLOCATE(rlpa_4DR4_values, stat=il_status)
          !    il_status = fi_memError(il_status, ' rlpa_4DR4_values','L673 TRANFORM_GODAE')
           !   IF(ALLOCATED(ilpa_variable_4D)) DEALLOCATE(ilpa_variable_4D, stat=il_status)
          !    il_status = fi_memError(il_status, 'ila_variable_4D ','L675 TRANFORM_GODAE')
          CASE DEFAULT
             WRITE(0,*) ' MIOL_transform_opatocf_NC : number of dimension exceed'
             CALL flush(0)
             STOP

          ENDSELECT

          ! type is NF90_DOUBLE
       CASE (NF90_DOUBLE)

          ! NF90_FLOAT array is a 1D array
          SELECT CASE (il_nbdim)

          CASE (1)

             call MIOL_read_field_NC (il_inputFileId,&
                                      cl_varname_ini,&
                                      rlpa_1DR8_values,&
                                      ila_1Ddimsize)
               !* Compute Min,Max
                rl_R8_minvalue = MINVAL(rlpa_1DR8_values)
                rl_R8_maxvalue = MAXVAL(rlpa_1DR8_values,  &
                     mask=rlpa_1DR8_values .NE. rl_fillvalue)
             IF (cl_varname == 'longitude'.OR. cl_varname == 'x') THEN
                call MIOL_write_field_NC (il_file_id,&
                     cl_varname,&
                     'X',&
                     rlpa_1DR8_values)
             ELSE IF  (cl_varname == 'latitude'.OR. cl_varname == 'y') THEN
                call MIOL_write_field_NC (il_file_id,&
                     cl_varname,&
                     'Y',&
                     rlpa_1DR8_values)
             ELSE IF  (cl_varname == 'depth') THEN
                call MIOL_write_field_NC (il_file_id,&
                     cl_varname,&
                     'Z',&
                     rlpa_1DR8_values)
             ENDIF


             DEALLOCATE(rlpa_1DR8_values, stat=il_status)
             il_status = fi_memError(il_status, ' rlpa_1DR8_values','L718 TRANFORM_GODAE')


             ! NF90_FLOAT array is a 2D array
          CASE (2)
             print *,' NF90_DOUBLE array is a 2D array'
             call MIOL_read_field_NC (il_inputFileId,&
                                      cl_varname_ini,&
                                      rlpa_2DR8_values, &
                                      ila_2Ddimsize)
             IF (ld_compute_sh) THEN
                ! Cas particulier lon,lat(x,y)
                IF ((TRIM(cl_varname_ini) /= 'longitude').AND. &
                     (TRIM(cl_varname_ini) /= 'latitude')) THEN
                   CALL MCAL_compute_short(rlpa_2DR8_values,&
                                           ila_2Ddimsize,&
                                           rl_min,&
                                           rl_max,&
                                           ilpa_variable_2D,&
                                           rl_scale_fact,&
                                           il_offset)
                     
                   print *,'rl_min',rl_min,il_offset
                   print *,'Il_min',int((rl_min-il_offset)/rl_scale_fact)
                   print *,'Il_max',int((rl_max-il_offset)/rl_scale_fact)
                   il_min=int((rl_min-il_offset)/rl_scale_fact)
                   il_max=int((rl_max-il_offset)/rl_scale_fact)
                   
                   ! Definition Parametres
                   rla_att(1)=rl_scale_fact
                   rla_att(2)=REAL(rl_offset+il_offset)
                   ila_specialvalue(1)= il_min
                   ila_specialvalue(2)= il_max
                   ila_specialvalue(3)= il_fillvalue
                   ila_specialvalue(4)= il_fillvalue
                   
                   call MIOL_write_field_NC (il_file_id,&
                                             cl_varname,&
                                             'XY',&
                                             ilpa_variable_2D,&
                                             rla_att,&
                                             ila_specialvalue)
                ELSE 
                   call MIOL_write_field_NC (il_file_id,&
                                             cl_varname,&
                                             'XY',&
                                             rlpa_2DR8_values)
                ENDIF
                ELSE
                   !**Write Float Value
                   call MIOL_write_field_NC (il_file_id,&
                                             cl_varname,&
                                             'XY',&
                                             rlpa_2DR8_values)
                ENDIF
                


              !  IF(ALLOCATED(rlpa_2DR8_values))  DEALLOCATE(rlpa_2DR8_values, stat=il_status)
              !  il_status = fi_memError(il_status, ' rlpa_2DR8_values','L768 TRANFORM_GODAE')
             !   IF(ALLOCATED(ilpa_variable_2D)) DEALLOCATE(ilpa_variable_2D, stat=il_status)
             !   il_status = fi_memError(il_status, ' ila_I2_values_R8','L770 TRANFORM_GODAE')

          ! NF90_FLOAT array is a 3D array
             CASE (3)
                print *,'ECRITURE variable 3D R8',cl_dimname
                call MIOL_read_field_NC (il_inputFileId,&
                                         cl_varname_ini,&
                                         rlpa_3DR8_values, &
                                         ila_3Ddimsize)
                IF (ld_compute_sh) THEN
                   CALL MCAL_compute_short(rlpa_3DR8_values,&
                                           ila_3Ddimsize,&
                                           rl_min,&
                                           rl_max,&
                                           ilpa_variable_3D,&
                                           rl_scale_fact,&
                                           il_offset)
                 
                   il_min=INT((rl_min-il_offset)/rl_scale_fact)
                   il_max=INT((rl_max-il_offset)/rl_scale_fact)
                
                   ! Definition Parametres
                   rla_att(1)=rl_scale_fact
                   rla_att(2)=REAL(rl_offset+il_offset)
                   ila_specialvalue(1)= il_min
                   ila_specialvalue(2)= il_max
                   ila_specialvalue(3)= il_fillvalue
                   ila_specialvalue(4)= il_fillvalue

                   call MIOL_write_field_NC (il_file_id,&
                                             cl_varname,&
                                             'XYZ',&
                                             ilpa_variable_3D,&
                                             rla_att,&
                                             ila_specialvalue)
                   ELSE
                      !**Write Float Value
                      call MIOL_write_field_NC (il_file_id,&
                                                cl_varname,&
                                                'XYZ',&
                                                rlpa_3DR8_values)
                   ENDIF
                  
                  ! IF(ALLOCATED(rlpa_3DR8_values)) DEALLOCATE(rlpa_3DR8_values, stat=il_status)
                  ! il_status = fi_memError(il_status, ' rlpa_3DR8_values','L807 TRANFORM_GODAE')
                  ! IF(ALLOCATED(ilpa_variable_3D)) DEALLOCATE(ilpa_variable_3D, stat=il_status)
                  ! il_status = fi_memError(il_status, 'ila_variable_3D','L809 TRANFORM_GODAE')
          

          ! NF90_FLOAT array is a 4D array
       CASE (4)
          print *,'ECRITURE variable 4D R8',cl_dimname
          call MIOL_read_field_NC (il_inputFileId,&
                                   cl_varname_ini,&
                                   rlpa_4DR8_values, &
                                   ila_4Ddimsize)
          IF (ld_compute_sh) THEN
             CALL MCAL_compute_short(rlpa_4DR8_values,&
                                     ila_4Ddimsize,&
                                     rl_min,&
                                     rl_max,&
                                     ilpa_variable_4D,&
                                     rl_scale_fact,&
                                     il_offset)
             
             il_min=INT((rl_min-il_offset)/rl_scale_fact)
             il_max=INT((rl_max-il_offset)/rl_scale_fact)
                
             rla_att(1)=REAL(rl_offset+il_offset)
             rla_att(2)=rl_scale_fact
             ila_specialvalue(1)= il_min
             ila_specialvalue(2)= il_max
             ila_specialvalue(3)= il_fillvalue
             ila_specialvalue(4)= il_fillvalue
             
             call MIOL_write_field_NC (il_file_id,&
                                       cl_varname,&
                                       'XYZT',&
                                       ilpa_variable_4D,&
                                       rla_att,& 
                                       ila_specialvalue)
             print *,'ECRITURE variable 4D R8 ok',cl_varname,il_maxvalue,il_minvalue
             
          ELSE
             !**Write Float Value
             call MIOL_write_field_NC (il_file_id,&
                                       cl_varname,&
                                       'XYZT',&
                                       rlpa_4DR8_values)
          ENDIF
          !IF(ALLOCATED(rlpa_4DR8_values)) DEALLOCATE(rlpa_4DR8_values, stat=il_status)
          !il_status = fi_memError(il_status, ' rlpa_4DR8_values','L846 TRANFORM_GODAE')
          !IF(ALLOCATED(ilpa_variable_4D)) DEALLOCATE(ilpa_variable_4D, stat=il_status)
          !il_status = fi_memError(il_status, ' ila_4D_R8values','L848 TRANFORM_GODAE')

       CASE DEFAULT
          WRITE(0,*) ' MIOL_transform_opatocf_NC : number of dimension exceed'
          CALL flush(0)
          STOP

       ENDSELECT

    CASE DEFAULT
       WRITE(0,*) ' MIOL_transform_opatocf_NC : variable type unknown'
       CALL flush(0)
       STOP

    ENDSELECT

    ! set in define mode
    il_status = fi_ncError(NF90_REDEF(il_file_id),'NF90_REDEF PB L865')



 IF (ALLOCATED(cla_globalattname)) DEALLOCATE(cla_globalattname, stat = il_status)
 il_status = fi_memError(il_status, ' cla_globalattname','L870 TRANFORM_GODAE')

 IF (ALLOCATED(cla_globalattvalue)) DEALLOCATE(cla_globalattvalue, stat = il_status)
 il_status = fi_memError(il_status, ' cla_globalattvalue','L873 TRANFORM_GODAE')

ENDDO

!-----------------------------------------------------------------------
! reload the number of dimensions
il_status = fi_ncError(NF90_INQUIRE(il_file_id, &
   il_nbdim),'NF90_INQUIRE PB L880')

IF (ll_lonlat_is1D) THEN

   !-----------------------------------------------------------------------
   ! if the longitude and the latitude are 1D variables, the dimensions
   ! name change
 DO il_ji = 1, il_nbdim

    il_status = fi_ncError(NF90_INQUIRE_DIMENSION(il_file_id, &
         il_ji, &
         cl_dimname),'NF90_INQUIRE_DIMENSION PB L891')

    cl_eqfilename = TRIM(cl_equivalencesPath)//TRIM(cl_dimname)//TRIM(cl_ext)


    OPEN(20, file=TRIM(cl_eqfilename), status='old', form='formatted')
    READ(20, equival)
    CLOSE(20)

    IF (TRIM(cl_dimname) .NE. TRIM(cl_varname_out)) THEN
       il_status = fi_ncError(NF90_RENAME_DIM(il_file_id,  &
            il_ji, &
            TRIM(cl_varname_out)),'NF90_RENAME_DIM PB L903')
    ENDIF

 ENDDO

ENDIF

!-----------------------------------------------------------------------
! delete old global attributes
WRITE (0,*) "delete old global attributes"
DO il_ji = 1, il_nbgatt
 il_status = fi_ncError(NF90_INQ_ATTNAME(il_file_id, &
      NF90_GLOBAL, &
      1, &
      cl_tmpattname),'NF90_INQ_ATTNAME PB L916')
 il_status = fi_ncError(NF90_DEL_ATT(il_file_id, &
      NF90_GLOBAL, &
      cl_tmpattname),'NF90_INQ_ATTNAME PB L919')
ENDDO

!------------------------------------------------------------------------
! write new global attributes
WRITE (0,*) "write new global attributes"
 cl_gattfilename = cd_gattfilename
!* Global attributs
CALL DATE_AND_TIME(cl_date, cl_time)
cl_institution = 'GIP MERCATOR OCEAN'
cl_references = 'http://www.mercator-ocean.fr'
cl_filename = TRIM(cd_output_filename)
cl_creationdate = cl_date(1:4)//'/'//cl_date(5:6)//'/'//cl_date(7:8)// &
     ' '//cl_time(1:2)//':'//cl_time(3:4)//':'//cl_time(5:6)
cl_history = TRIM(cl_creationdate)//' MERCATOR OCEAN Netcdf creation '
IF (id_juliandate > 0) THEN
rl_nbsec=REAL(id_juliandate*86400)
CALL MCAL_conversion_STU50_CAL(rl_nbsec,cl_jdate)
ELSE
cl_jdate=""
ENDIF
WRITE(0,*) "Open global namelist : ",cl_gattfilename,cl_jdate
!* Open global namelist
OPEN(20, file=TRIM(cl_gattfilename), status='old', form='formatted')
READ(20, nb_att)

IF (ALLOCATED(cla_globalattname)) DEALLOCATE(cla_globalattname)
ALLOCATE(cla_globalattname(il_nbatt), stat = il_status)
il_status = fi_memError(il_status, ' cla_globalattname','L943 TRANSFORM_GODAE')

IF (ALLOCATED(cla_globalattvalue)) DEALLOCATE(cla_globalattvalue)
ALLOCATE(cla_globalattvalue(il_nbatt), stat = il_status)
il_status = fi_memError(il_status, ' cla_globalattvalue','L947 TRANSFORM_GODAE')

IF (ALLOCATED(rla_globalattvalue)) DEALLOCATE(rla_globalattvalue)
ALLOCATE(rla_globalattvalue(il_nbatt), stat = il_status)
il_status = fi_memError(il_status, ' cla_globalattvalue','L951 TRANSFORM_GODAE')



DO il_ji = 1, il_nbatt
 READ(20, *) cla_globalattname(il_ji)
 READ(20, '(A100)') cla_globalattvalue(il_ji)
ENDDO
CLOSE(20)
ila_typevalue(1:12)=2
ila_typevalue(14:18)=2
ila_typevalue(13)=5
ila_typevalue(19:24)=5

cla_globalattvalue(4)=TRIM(cl_history)
cla_globalattvalue(6)=TRIM(cl_institution)
cla_globalattvalue(7)=TRIM(cl_references)
!cla_globalattvalue(8)
!cla_globalattvalue(9)
!cla_globalattvalue(10)
!cla_globalattvalue(11)
!cla_globalattvalue(12)
!cla_globalattvalue(14)
!cla_globalattvalue(15)
!cla_globalattvalue(16)
!cla_globalattvalue(17)
!cla_globalattvalue(18)
cla_globalattvalue(12)=TRIM(cl_jdate)
rla_globalattvalue(13)=id_juliandate

rla_globalattvalue(19)=rl_lonminvalue
rla_globalattvalue(20)=rl_lonmaxvalue
rla_globalattvalue(21)=rl_latminvalue
rla_globalattvalue(22)=rl_latmaxvalue
rla_globalattvalue(23)=rl_depthminvalue
rla_globalattvalue(24)=rl_depthmaxvalue

WRITE(0,*) "MIOL_write_attributeslist_NC "
CALL MIOL_write_attributeslist_NC(il_file_id,&
                                  'global',&
                                  il_nbatt,&
                                  cla_globalattname(:),&
                                  cla_globalattvalue(:),&
                                  rla_globalattvalue(:),&
                                  ila_typevalue(:))

!
!*-----------------------------------------------------------------------
! out of define mode

il_status = fi_ncError(NF90_ENDDEF(il_file_id),'NF90_ENDDEF PB L1000')

!
!*-----------------------------------------------------------------------
! close file

il_status = fi_ncError(NF90_CLOSE(il_file_id),'NF90_CLOSE PB L1006')

END SUBROUTINE MIOL_transform_opatocf_GODAE_NC
 
 
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
        !!======================================================================
        !> \brief
        !! Description: Create the target file with the same dimensions of the
        !!              source file.
        !!
        !! History :
        !!        /n  10/2006  F. Messal Creation
        !!        /n  06/2008  C.REGNIER Modifications we use MFT_error for reduce the code
        !<
        !!======================================================================
!   
    SUBROUTINE copy_files(id_sourcefileId, &
                            id_targetfileId, & 
                            id_nx, &
                            id_ny, &
                            id_nz, &
                            id_nt)
! 
        USE MFT_error
        USE netcdf
        IMPLICIT NONE
!       
        !-----------------------------------------------------------------------
! 
        INTEGER, INTENT(IN) :: id_sourcefileId
        INTEGER, INTENT(IN) :: id_targetfileId
        INTEGER, INTENT(IN) :: id_nx, id_ny, id_nz, id_nt
! 
        INTEGER :: il_nbdim, il_nbvar, il_nbatt, &
                   il_ji, il_status
        INTEGER, DIMENSION(:), ALLOCATABLE :: ila_dimlen
        CHARACTER(LEN=50), DIMENSION(:), ALLOCATABLE :: cla_dimname, cla_attname
! 
        !-----------------------------------------------------------------------
! 
       CALL hdlerr(NF90_REDEF(id_targetfileId),' PB copy_files NF90_REDEF')
       CALL hdlerr(NF90_INQUIRE(id_sourcefileId, &
                                 il_nbdim, &
                                 il_nbvar, &
                                 il_nbatt),' PB copy_files NF90_INQUIRE')
            WRITE(0,*) ' inquire '
            CALL flush(0)

!
        ALLOCATE(cla_dimname(il_nbdim), stat = il_status)
        CALL memerr(il_status, ' cla_dimname','PB copy_file')
        ALLOCATE(ila_dimlen(il_nbdim), stat = il_status)
        CALL memerr(il_status, ' ila_dimlen','PB copy_file')
        ALLOCATE(cla_attname(il_nbatt), stat = il_status)
        CALL memerr(il_status, ' cla_attname','PB copy_file')
! 
        DO il_ji=1, il_nbdim
           CALL hdlerr(NF90_INQUIRE_DIMENSION(id_sourcefileId, &
                                              il_ji, &
                                              cla_dimname(il_ji), &
                                              ila_dimlen(il_ji)),'PB copy_files NF90_INQUIRE_DIMENSION')
!      
           IF (TRIM(cla_dimname(il_ji)) .EQ. 'x') THEN
              ila_dimlen(il_ji) = id_nx
           ELSE IF (TRIM(cla_dimname(il_ji)) .EQ. 'y') THEN
              ila_dimlen(il_ji) = id_ny
           ELSE IF ((TRIM(cla_dimname(il_ji)) .EQ. 'deptht') .OR. &
               (TRIM(cla_dimname(il_ji)) .EQ. 'depthu') .OR. &
               (TRIM(cla_dimname(il_ji)) .EQ. 'depthv') .OR. &
               (TRIM(cla_dimname(il_ji)) .EQ. 'depthw')) THEN
              cla_dimname(il_ji) = 'depth'
              ila_dimlen(il_ji) = id_nz
           ELSE IF (TRIM(cla_dimname(il_ji)) .EQ. 'time_counter') THEN
              ila_dimlen(il_ji) = id_nt
           ENDIF
!
           CALL hdlerr(NF90_DEF_DIM(id_targetfileId, &
                                    TRIM(cla_dimname(il_ji)), &
                                    ila_dimlen(il_ji),il_ji),'PB copy_files NF90_DEF_DIM')
           CALL flush(0)
!
        ENDDO
! 
        DO il_ji=1, il_nbatt
           CALL hdlerr(NF90_INQ_ATTNAME(id_sourcefileId, &
                                        NF90_GLOBAL, &
                                        il_ji, &
                                        cla_attname(il_ji)),'PB NF90_INQ_ATTNAME')
           CALL hdlerr(NF90_COPY_ATT(id_sourcefileId, &
                                     NF90_GLOBAL, &
                                     TRIM(cla_attname(il_ji)), &
                                     id_targetfileId, &
                                     NF90_GLOBAL),'PB NF90_COPY_ATT')
        ENDDO
! 
! 
        DEALLOCATE(cla_dimname, stat = il_status)
        CALL memerr(il_status, ' cla_dimname','PB copy_files')
        DEALLOCATE(ila_dimlen, stat = il_status)
        DEALLOCATE(cla_attname, stat = il_status)
        CALL memerr(il_status, ' cla_attname','PB copy_files')
!
        CALL hdlerr(NF90_ENDDEF(id_targetfileId),'PB NF90_ENDDEF')
!
!
       ENDSUBROUTINE copy_files
!
!*-------------------------------------------------------------------------------
!
END MODULE MIOL_transform
