  PROGRAM Test_attributs
  !
  !**** 
  !
  !     Purpose :
  !     Programme de test pour MFT
  !
  !***  Method : Test 
  !     Externals :
  !     Files :
  !     References:
  !
  !     History
  !     -------
  !      Version    Programmer    Date       Description
  !      ---------------------------------------------
  !       1.0       C.REGNIER    03/2010    creation test pour nath
  !       1.1       C.REGNIER    09/2012    integration in make test
  !
  !*-------------------------------------------------------------
  !
  !
  !** ++  MODULES used
  USE MIOL
  USE netcdf
  !
  IMPLICIT NONE
  !
  !** ++  Local Declarations 
  !
  INTEGER(KIND=4)                                    :: il_nbdim,il_file_id
  INTEGER(KIND=4)                                    :: il_ji,il_type,il_nbatt
  INTEGER(KIND=4)  ,DIMENSION(:),ALLOCATABLE         :: ila_dimlen,ila_typevalue
  CHARACTER(LEN=255)                                 :: cl_file_out,cl_attname,cl_attvalue,cl_name_evar_out
  CHARACTER(LEN=255),DIMENSION(:),ALLOCATABLE        :: cla_dimname
  CHARACTER(LEN=255), DIMENSION(:), ALLOCATABLE      :: cla_globalattname,cla_globalattvalue
  REAL(KIND=4),DIMENSION(:,:,:),ALLOCATABLE          :: rla_test 
  REAL(KIND=4),DIMENSION(:),ALLOCATABLE              :: rla_globalattvalue
  REAL(KIND=4)                                       :: rl_attvalue,rl_miss
  LOGICAL                                            :: ll_fic
  !
  !*-------------------------------------------------------------
  !*-------------------------------------------------------------------------
  !
  !** Creation du fichier de sortie
  !
  !*-------------------------------------------------------------------------

   WRITE (0,*) '               Test attributs                            '
   WRITE (0,*) '---------------------------------------------------------'
     rl_miss=1.e+35
     il_nbdim=3
     ALLOCATE(cla_dimname(il_nbdim))
     ALLOCATE(ila_dimlen(il_nbdim))
     cla_dimname(1) = 'x'
     cla_dimname(2) = 'y'
     cla_dimname(3) = 'time_counter'
   
     ila_dimlen(1) = 50
     ila_dimlen(2) = 50
     ila_dimlen(3) = 10
     ALLOCATE(rla_test(ila_dimlen(1),ila_dimlen(2),ila_dimlen(3)))
     rla_test(:,:,:)=1
     cl_file_out='tmp/Test_att.nc'
     call MIOL_create_file_NC(TRIM(cl_file_out),&
                              il_nbdim,&
                              cla_dimname,&
                              ila_dimlen)

     cl_name_evar_out='test'
     
     call MIOL_openw_file_NC(TRIM(cl_file_out), il_file_id)  
     call MIOL_write_field_NC (il_file_id,&
                               cl_name_evar_out,&
                               'XYT',&
                               rla_test)

      il_nbatt=4
      ALLOCATE(cla_globalattname(il_nbatt))
      ALLOCATE(cla_globalattvalue(il_nbatt))
      ALLOCATE(rla_globalattvalue(il_nbatt))
      ALLOCATE(ila_typevalue(il_nbatt))
      cla_globalattname(1)='long_name'
      cla_globalattname(2)='standard_name'
      cla_globalattname(3)='units'
      cla_globalattname(4)='missing_value'

      !** Type de variable : 2 => integer
      !                      5 => float
      ila_typevalue(1:3)=2
      ila_typevalue(4)=5

      
      !** Tableau d'attributs de type char
      cla_globalattvalue(:)=''
      cla_globalattvalue(1)='socosdsw'
      cla_globalattvalue(2)='socosdsw'
      cla_globalattvalue(3)='W.m**-2'
      !** Tableau d'attribut de type Float
      rla_globalattvalue(:)=0
      rla_globalattvalue(4)=rl_miss
      
     CALL MIOL_write_attributeslist_NC(il_file_id,&
                                       cl_name_evar_out,&
                                       il_nbatt,&
                                       cla_globalattname(:),&
                                       cla_globalattvalue(:),&
                                       rla_globalattvalue(:),&
                                       ila_typevalue(:))

     CALL MIOL_close_file_NC(il_file_id)
     WRITE (0,*) '               Test attributs OK                            '
     
   END PROGRAM Test_attributs
