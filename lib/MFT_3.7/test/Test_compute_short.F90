PROGRAM TEST_SHORT
!* Program de test du : calcul des shorts puis écriture
 USE MIOL
 USE MCAL
 USE netcdf
!*
IMPLICIT NONE

REAL(KIND=8), DIMENSION(2)                         :: rla_att
REAL(KIND=4), DIMENSION(20,20,50)                  :: rla_test,rla_test2
REAL(KIND=4)                                       :: rl_min,rl_max
REAL(KIND=8)                                       :: rl_scale,rl_offset
INTEGER(KIND=2),DIMENSION(20,20,50)                :: ila_test
INTEGER(KIND=2),DIMENSION(:,:,:),POINTER           :: ilpa_variable_3D
INTEGER(KIND=4)                                    :: il_nbdim,il_file_id
CHARACTER(LEN=255),DIMENSION(:),ALLOCATABLE        :: cla_dimname
INTEGER(KIND=4)  ,DIMENSION(:),ALLOCATABLE         :: ila_dimlen
INTEGER(KIND=4)  ,DIMENSION(3)                     :: ila_3Ddimsize
CHARACTER(LEN=256)                                 :: cl_file_out,cl_name
!
!       1.1       C.REGNIER    09/2012    integration in make test

!*----------------------------------------------------------

WRITE (0,*) '*********************************************************'
WRITE (0,*) '               Test Compute short                        '

il_fillvalue=-(((2**16-2))/2)
rl_min=0
rl_max=50
rla_test(1:2,1:2,:)=0
rla_test(3:10,3:10,:)=4.8
rla_test(11:20,11:20,:)=44.5

!** Compute short
ila_3Ddimsize(1)=20
ila_3Ddimsize(2)=20
ila_3Ddimsize(3)=50
 
CALL MCAL_compute_short(rla_test,&
                         ila_3Ddimsize,& 
                         rl_min,&
                         rl_max,&
                         ilpa_variable_3D,&
                         rl_scale,&
                         rl_offset)
rla_att(1)=rl_scale
rla_att(2)=rl_offset
! 2. Ecriture du fichier 
! ----------------------
     il_nbdim=3
     ALLOCATE(cla_dimname(il_nbdim))
     ALLOCATE(ila_dimlen(il_nbdim))
     cla_dimname(1) = 'x'
     cla_dimname(2)=  'y'
     cla_dimname(3)=  'time'
        
     ila_dimlen(1) = 20 
     ila_dimlen(2) = 20
     ila_dimlen(3) = 50
     cl_file_out='tmp/Short_test.nc'
     call MIOL_create_file_NC(TRIM(cl_file_out),&
                              il_nbdim,&
                              cla_dimname,&
                              ila_dimlen)

     call MIOL_openw_file_NC(TRIM(cl_file_out), il_file_id)  
     cl_name='Test_short'
     call MIOL_write_field_NC (il_file_id,&
                               'cl_name',&
                               'XYT',&
                                ilpa_variable_3D,&
                                rla_att)
     CALL MIOL_close_file_NC(il_file_id)

WRITE (0,*) '---------------------------------------------------------'
WRITE (0,*) '               Test Compute short OK                     '
WRITE (0,*) '*********************************************************'

END PROGRAM TEST_SHORT
