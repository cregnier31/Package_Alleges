  PROGRAM Test_read_write_MFT  
  !
  !**** 
  !
  !     Purpose :
  !     Programme de test read write pour MFT
  !
  !***  Method : Test de tout les types en read write
  !     Externals :
  !     Files :
  !     References:
  !
  !     History
  !     -------
  !      Version    Programmer    Date       Description
  !      ---------------------------------------------
  !       1.0       C.REGNIER    09/2512    creation test
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
  INTEGER(KIND=1)  ,DIMENSION(30)                   :: ila_1D_Byte
  INTEGER(KIND=1)  ,DIMENSION(:),POINTER             :: ilpa_1D_Byte
  INTEGER(KIND=2)  ,DIMENSION(30)                   :: ila_1D_Short
  INTEGER(KIND=2)  ,DIMENSION(:),POINTER             :: ilpa_1D_Short
  INTEGER(KIND=4)  ,DIMENSION(30)                   :: ila_1D
  INTEGER(KIND=8)  ,DIMENSION(30)                   :: ila_1D_I8
  INTEGER(KIND=4)  ,DIMENSION(:),POINTER             :: ilpa_1D
  INTEGER(KIND=8)  ,DIMENSION(:),POINTER             :: ilpa_1D_I8
  INTEGER(KIND=1)  ,DIMENSION(30,30)               :: ila_2D_Byte
  INTEGER(KIND=1)  ,DIMENSION(:,:),POINTER           :: ilpa_2D_Byte
  INTEGER(KIND=2)  ,DIMENSION(30,30)               :: ila_2D_Short
  INTEGER(KIND=2)  ,DIMENSION(:,:),POINTER           :: ilpa_2D_Short
  INTEGER(KIND=4)  ,DIMENSION(30,30)               :: ila_2D
  INTEGER(KIND=8)  ,DIMENSION(30,30)               :: ila_2D_I8
  INTEGER(KIND=4)  ,DIMENSION(:,:),POINTER           :: ilpa_2D
  INTEGER(KIND=8)  ,DIMENSION(:,:),POINTER           :: ilpa_2D_I8
  INTEGER(KIND=1)  ,DIMENSION(30,30,30)           :: ila_3D_Byte
  INTEGER(KIND=1)  ,DIMENSION(:,:,:),POINTER         :: ilpa_3D_Byte
  INTEGER(KIND=2)  ,DIMENSION(30,30,30)           :: ila_3D_Short
  INTEGER(KIND=2)  ,DIMENSION(:,:,:),POINTER         :: ilpa_3D_Short
  INTEGER(KIND=4)  ,DIMENSION(30,30,30)           :: ila_3D
  INTEGER(KIND=8)  ,DIMENSION(30,30,30)           :: ila_3D_I8
  INTEGER(KIND=4)  ,DIMENSION(:,:,:),POINTER         :: ilpa_3D
  INTEGER(KIND=8)  ,DIMENSION(:,:,:),POINTER         :: ilpa_3D_I8
  INTEGER(KIND=1)  ,DIMENSION(30,30,30,20)        :: ila_4D_Byte
  INTEGER(KIND=1)  ,DIMENSION(:,:,:,:),POINTER       :: ilpa_4D_Byte
  INTEGER(KIND=2)  ,DIMENSION(30,30,30,20)        :: ila_4D_Short
  INTEGER(KIND=2)  ,DIMENSION(:,:,:,:),POINTER       :: ilpa_4D_Short
  INTEGER(KIND=4)  ,DIMENSION(30,30,30,20)        :: ila_4D
  INTEGER(KIND=8)  ,DIMENSION(30,30,30,20)        :: ila_4D_I8
  INTEGER(KIND=4)  ,DIMENSION(:,:,:,:),POINTER       :: ilpa_4D
  INTEGER(KIND=8)  ,DIMENSION(:,:,:,:),POINTER       :: ilpa_4D_I8
  REAL(KIND=4)  ,DIMENSION(30)                      :: rla_1D
  REAL(KIND=4)  ,DIMENSION(:),POINTER                :: rlpa_1D,rlpa_1D_Short
  REAL(KIND=8)  ,DIMENSION(30)                      :: rla_1D_R8
  REAL(KIND=8)  ,DIMENSION(:),POINTER                :: rlpa_1D_R8
  REAL(KIND=4)  ,DIMENSION(30,30)                  :: rla_2D
  REAL(KIND=4)  ,DIMENSION(:,:),POINTER              :: rlpa_2D,rlpa_2D_Short
  REAL(KIND=8)  ,DIMENSION(30,30)                  :: rla_2D_R8
  REAL(KIND=8)  ,DIMENSION(:,:),POINTER              :: rlpa_2D_R8
  REAL(KIND=4)  ,DIMENSION(30,30,30)              :: rla_3D
  REAL(KIND=4)  ,DIMENSION(:,:,:),POINTER            :: rlpa_3D,rlpa_3D_Short
  REAL(KIND=8)  ,DIMENSION(30,30,30)              :: rla_3D_R8
  REAL(KIND=8)  ,DIMENSION(:,:,:),POINTER            :: rlpa_3D_R8
  REAL(KIND=4)  ,DIMENSION(30,30,30,20)           :: rla_4D
  REAL(KIND=4)  ,DIMENSION(:,:,:,:),POINTER          :: rlpa_4D,rlpa_4D_Short
  REAL(KIND=8)  ,DIMENSION(30,30,30,20)           :: rla_4D_R8
  REAL(KIND=8)  ,DIMENSION(:,:,:,:),POINTER          :: rlpa_4D_R8
  REAL(KIND=4)  ,DIMENSION(30,30,30,20,8)         :: rla_5D
  REAL(KIND=4)  ,DIMENSION(:,:,:,:,:),POINTER        :: rlpa_5D
  CHARACTER(LEN=255),DIMENSION(:),ALLOCATABLE        :: cla_dimname
  CHARACTER(LEN=25),DIMENSION(8)                     :: cla_var1D
  CHARACTER(LEN=25),DIMENSION(:),POINTER             :: clpa_1D_Char
  CHARACTER(LEN=25),DIMENSION(:,:),POINTER           :: clpa_2D_Char
  INTEGER(KIND=4),DIMENSION(:),ALLOCATABLE           :: ila_dimlen
  INTEGER(KIND=4), DIMENSION(1)                      :: ila_dimlen1D 
  INTEGER(KIND=4), DIMENSION(2)                      :: ila_dimlen2D
  INTEGER(KIND=4), DIMENSION(3)                      :: ila_dimlen3D
  INTEGER(KIND=4), DIMENSION(4)                      :: ila_dimlen4D
 
  CHARACTER(LEN=30)                                 :: cl_test 
  !
  !*-------------------------------------------------------------
  !  Character
  !*-------------------------------------------------------------
  !*   Creation des variables 1D

  !
  !*-------------------------------------------------------------
  !   Integer 
  !
  !*-------------------------------------------------------------
  !*   Creation des variables 1D
  ila_1D_Byte(:)=1
  ila_1D_Short(:)=1
  ila_1D(:)=1
  ila_1D_I8(:)=1
  !*   Creation des variables 2D
  ila_2D_Byte(:,:)=1
  ila_2D_Short(:,:)=1
  ila_2D(:,:)=1
  ila_2D_I8(:,:)=1
  !*   Creation des variables 3D
  ila_3D_Byte(:,:,:)=1
  ila_3D_Short(:,:,:)=1
  ila_3D(:,:,:)=1
  ila_3D_I8(:,:,:)=1
  !*   Creation des variables 4D
  ila_4D_Byte(:,:,:,:)=1
  ila_4D_Short(:,:,:,:)=1
  ila_4D(:,:,:,:)=1
  ila_4D_I8(:,:,:,:)=1

  !
  !*-------------------------------------------------------------
  !   Float
  !
  !*-------------------------------------------------------------
  !*   Creation des variables 1D
  rla_1D(:)=1.5
  !*   Creation des variables 2D
  rla_2D(:,:)=1.5
  !*   Creation des variables 3D
  rla_3D(:,:,:)=1.5
  !*   Creation des variables 4D
  rla_4D(:,:,:,:)=1.5
  !*   Creation des variables 4D
  rla_5D(:,:,:,:,:)=1.5
  !
  !*-------------------------------------------------------------
  !   Double
  !
  !*-------------------------------------------------------------
  !*   Creation des variables 1D
  rla_1D_R8(:)=1.5
  !*   Creation des variables 2D
  rla_2D_R8(:,:)=1.5
  !*   Creation des variables 3D
  rla_3D_R8(:,:,:)=1.5
  !*   Creation des variables 4D
  rla_4D_R8(:,:,:,:)=1.5
  !*-------------------------------------------------------------
  !   Char
  !*-------------------------------------------------------------
  cla_var1D(1)='number of data values'
  cla_var1D(2)='mean of reference'
  cla_var1D(3)='mean of product'
  cla_var1D(4)='mean square error'
  cla_var1D(5)='correlation'
  cla_var1D(6)='variance of the product'
  cla_var1D(7)='variance of the reference'
  cla_var1D(8)='anomaly correlation'
  print *,'cla_var2D :: ',cla_var1D(1) 
  print *,'cla_var2D :: ',cla_var1D(2)
  print *,'cla_var2D :: ',cla_var1D(3)
  print *,'cla_var2D :: ',cla_var1D(4)
  print *,'cla_var2D :: ',cla_var1D(5)
  print *,'cla_var2D :: ',cla_var1D(6)
  print *,'cla_var2D :: ',cla_var1D(7)
  print *,'cla_var2D :: ',cla_var1D(8)
  cl_test='tmp/test_write.nc'

  !*-------------------------------------------------------------------------
  !
  !** Creation du fichier de sortie
  !
  !*-------------------------------------------------------------------------

  il_nbdim=6
  ALLOCATE(cla_dimname(il_nbdim))
  ALLOCATE(ila_dimlen(il_nbdim))
  
  cla_dimname(1) = 'longitude'
  cla_dimname(2)=  'latitude'
  cla_dimname(3)=  'z'
  cla_dimname(4)=  't'
  cla_dimname(5)=  'metrics'
  cla_dimname(6)=  'string_length'
  ila_dimlen(1) = 30 
  ila_dimlen(2) = 30
  ila_dimlen(3) = 30
  ila_dimlen(4) = 20
  ila_dimlen(5) = 8
  ila_dimlen(6) = 25
 
  call MIOL_create_file_NC(TRIM(cl_test),&
                           il_nbdim,&
                           cla_dimname,&
                           ila_dimlen)

   !WRITE (0,*) 'MIOL_create_file_NC OK'
   call MIOL_openw_file_NC(TRIM(cl_test), il_file_id)  
   !** Ecriture 1D I1
   WRITE (0,*) '*********************************************************'
   WRITE (0,*) '                 Test Write                               '
   WRITE (0,*) '---------------------------------------------------------'
   call MIOL_write_field_NC (il_file_id,&
                             'VarChar1D',&
                             'SM',&
                              cla_var1D(:),&
                              ila_dimlen(6))
   WRITE (0,*) 'Test write 1D Char OK : '
   call MIOL_write_field_NC (il_file_id,&
                             'Var1D_Byte',&
                             'X',&
                              ila_1D_Byte)
   WRITE (0,*) 'Test write 1D Byte OK : '
   !DEALLOCATE(ila_1D_Byte)
   !** Ecriture 1D I2
   call MIOL_write_field_NC (il_file_id,&
                             'Var1D_Short',&
                             'X',&
                              ila_1D_Short)
   WRITE (0,*) 'Test write 1D Short OK'
   !DEALLOCATE(ila_1D_Short)
   !** Ecriture 1D I4
   call MIOL_write_field_NC (il_file_id,&
                             'Var1D_Integer',&
                             'X',&
                              ila_1D)
   WRITE (0,*) 'Test write 1D Integer OK'
   !** Ecriture 1D I8
   call MIOL_write_field_NC (il_file_id,&
                             'Var1D_Integer8',&
                             'X',&
                              ila_1D_I8)
   WRITE (0,*) 'Test write 1D Integer8 OK'
   !DEALLOCATE(ila_1D)
   !** Ecriture 1D R4
   call MIOL_write_field_NC (il_file_id,&
                             'Var1D_R4',&
                             'X',&
                              rla_1D)
   WRITE (0,*) 'Test write 1D Float OK'
   !DEALLOCATE(rla_1D)
   !** Ecriture 1D R8
   call MIOL_write_field_NC (il_file_id,&
                             'Var1D_R8',&
                             'X',&
                              rla_1D_R8)
   !DEALLOCATE(rla_1D_R8)
   WRITE (0,*) 'Test write 1D Double OK'
   !** Ecriture 2D I1
   call MIOL_write_field_NC (il_file_id,&
                             'Var2D_Byte',&
                             'XY',&
                              ila_2D_Byte)
   WRITE (0,*) 'Test write 2D Byte OK'
   !DEALLOCATE(ila_2D_Byte)
   !** Ecriture 2D I2
   call MIOL_write_field_NC (il_file_id,&
                             'Var2D_Short',&
                             'XY',&
                              ila_2D_Short)
   WRITE (0,*) 'Test write 2D Short OK'
   !DEALLOCATE(ila_2D_Short)
   !** Ecriture 2D I4
   call MIOL_write_field_NC (il_file_id,&
                             'Var2D_Integer',&
                             'XY',&
                              ila_2D)
   WRITE (0,*) 'Test write 2D Integer OK'
   !** Ecriture 2D I8
   call MIOL_write_field_NC (il_file_id,&
                             'Var2D_Integer8',&
                             'XY',&
                              ila_2D_I8)
   WRITE (0,*) 'Test write 2D Integer8 OK'
   !DEALLOCATE(ila_2D)
   !** Ecriture 2D R4
   call MIOL_write_field_NC (il_file_id,&
                             'Var2D_R4',&
                             'XY',&
                              rla_2D)
   WRITE (0,*) 'Test write 2D Float OK'
   !DEALLOCATE(rla_2D)
   !** Ecriture 2D R8
   call MIOL_write_field_NC (il_file_id,&
                             'Var2D_R8',&
                             'XY',&
                              rla_2D_R8)
   WRITE (0,*) 'Test write 2D Double OK'
   !DEALLOCATE(rla_2D_R8)
  !** Ecriture 3D Byte 
   call MIOL_write_field_NC (il_file_id,&
                             'Var3D_Byte',&
                             'XYZ',&
                              ila_3D_Byte)
   WRITE (0,*) 'Test write 3D Byte OK'
   !DEALLOCATE(ila_3D_Byte)
   !** Ecriture 3D I2
   call MIOL_write_field_NC (il_file_id,&
                             'Var3D_Short',&
                             'XYZ',&
                              ila_3D_Short)
   WRITE (0,*) 'Test write 3D Short OK'
   !DEALLOCATE(ila_3D_Short)
   !** Ecriture 3D I4
   call MIOL_write_field_NC (il_file_id,&
                             'Var3D_Integer',&
                             'XYZ',&
                              ila_3D)
   WRITE (0,*) 'Test write 3D Integer OK'
   !** Ecriture 3D I8
   call MIOL_write_field_NC (il_file_id,&
                             'Var3D_Integer8',&
                             'XYZ',&
                              ila_3D_I8)
   WRITE (0,*) 'Test write 3D Integer8 OK'
   !DEALLOCATE(ila_3D)
   !** Ecriture 3D R4
   call MIOL_write_field_NC (il_file_id,&
                             'Var3D_R4',&
                             'XYZ',&
                              rla_3D)
   WRITE (0,*) 'Test write 3D Float OK'
   !DEALLOCATE(rla_3D)
   !** Ecriture 3D R8
   call MIOL_write_field_NC (il_file_id,&
                             'Var3D_R8',&
                             'XYZ',&
                              rla_3D_R8)
   WRITE (0,*) 'Test write 3D Double OK'
   !DEALLOCATE(rla_3D_R8)
   !** Ecriture 4D byte
   call MIOL_write_field_NC (il_file_id,&
                             'Var4D_Byte',&
                             'XYZT',&
                              ila_4D_Byte)
   WRITE (0,*) 'Test write 4D Byte OK'
   !DEALLOCATE(ila_4D_Byte)
   !** Ecriture 4D Short
   call MIOL_write_field_NC (il_file_id,&
                             'Var4D_Short',&
                             'XYZT',&
                              ila_4D_Short)
   WRITE (0,*) 'Test write 4D Short OK'
   !DEALLOCATE(ila_4D_Short)
   !** Ecriture 4D I4
   call MIOL_write_field_NC (il_file_id,&
                             'Var4D_Integer',&
                             'XYZT',&
                              ila_4D)
   WRITE (0,*) 'Test write 4D Integer OK'
   !** Ecriture 4D I8
   call MIOL_write_field_NC (il_file_id,&
                             'Var4D_Integer8',&
                             'XYZT',&
                              ila_4D_I8)
   WRITE (0,*) 'Test write 4D Integer OK'
   !DEALLOCATE(ila_4D)
   !** Ecriture 4D R4
   call MIOL_write_field_NC (il_file_id,&
                             'Var4D_R4',&
                             'XYZT',&
                              rla_4D)
   WRITE (0,*) 'Test write 4D Float OK'
   !DEALLOCATE(rla_4D)
   !** Ecriture 4D R8
   call MIOL_write_field_NC (il_file_id,&
                             'Var4D_R8',&
                             'XYZT',&
                              rla_4D_R8)
   WRITE (0,*) 'Test write 4D Double OK'

   call MIOL_write_field_NC (il_file_id,&
                             'Var5D_R4',&
                             'XYZTM',&
                              rla_5D)
   WRITE (0,*) 'Test write 5D Float OK'
   !DEALLOCATE(rla_5D)
   CALL MIOL_close_file_NC(il_file_id)
   WRITE (0,*) '---------------------------------------------------------'
   WRITE (0,*) 'Test Write OK'
   WRITE (0,*) '*********************************************************'
   WRITE (0,*) '---------------------------------------------------------'
   WRITE (0,*) '                 Test Read                               '
   WRITE (0,*) '---------------------------------------------------------'
   !** lecture des variables
   CALL MIOL_openr_file_NC(TRIM(cl_test), il_file_id)  
   CALL MIOL_read_field_NC(il_file_id,'VarChar1D',clpa_2D_Char,ila_dimlen(6)) 
   WRITE (0,*) 'Read 1D Char OK'
   DEALLOCATE(clpa_2D_Char)
   CALL MIOL_read_field_NC(il_file_id,'Var1D_Byte',ilpa_1D_Byte) 
   WRITE (0,*) 'Read 1D Byte OK'
   DEALLOCATE(ilpa_1D_Byte)
   CALL MIOL_read_field_NC(il_file_id,'Var2D_Byte',ilpa_2D_Byte)
   WRITE (0,*) 'Read 2D Byte OK'
   DEALLOCATE(ilpa_2D_Byte)
   CALL MIOL_read_field_NC(il_file_id,'Var3D_Byte',ilpa_3D_Byte)
   WRITE (0,*) 'Read 3D Byte OK'
   DEALLOCATE(ilpa_3D_Byte)
   CALL MIOL_read_field_NC(il_file_id,'Var4D_Byte',ilpa_4D_Byte)
   WRITE (0,*) 'Read 4D Byte OK'
   DEALLOCATE(ilpa_4D_Byte)
   CALL MIOL_read_field_NC(il_file_id,'Var1D_Short',ilpa_1D_Short,ila_dimlen1D,rlpa_1D_short) 
   WRITE (0,*) 'Read 1D Short OK'
   DEALLOCATE(ilpa_1D_Short)
   DEALLOCATE(rlpa_1D_Short)
   CALL MIOL_read_field_NC(il_file_id,'Var2D_Short',ilpa_2D_Short,ila_dimlen2D,rlpa_2D_short) 
   WRITE (0,*) 'Read 2D Short OK'
   DEALLOCATE(ilpa_2D_Short)
   DEALLOCATE(rlpa_2D_Short)
   CALL MIOL_read_field_NC(il_file_id,'Var3D_Short',ilpa_3D_Short,ila_dimlen3D,rlpa_3D_short) 
   WRITE (0,*) 'Read 3D Short OK'
   DEALLOCATE(ilpa_3D_Short)
   DEALLOCATE(rlpa_3D_Short)
   CALL MIOL_read_field_NC(il_file_id,'Var4D_Short',ilpa_4D_Short,ila_dimlen4D,rlpa_4D_short) 
   WRITE (0,*) 'Read 4D Short OK'
   DEALLOCATE(ilpa_4D_Short)
   DEALLOCATE(rlpa_4D_Short)
   CALL MIOL_read_field_NC(il_file_id,'Var1D_Integer',ilpa_1D) 
   WRITE (0,*) 'Read 1D Integer OK'
   DEALLOCATE(ilpa_1D)
   CALL MIOL_read_field_NC(il_file_id,'Var1D_Integer8',ilpa_1D_I8) 
   WRITE (0,*) 'Read 1D Integer8 OK'
   DEALLOCATE(ilpa_1D_I8)
   CALL MIOL_read_field_NC(il_file_id,'Var2D_Integer',ilpa_2D)
   WRITE (0,*) 'Read 2D Integer OK'
   DEALLOCATE(ilpa_2D)
   CALL MIOL_read_field_NC(il_file_id,'Var2D_Integer8',ilpa_2D_I8)
   WRITE (0,*) 'Read 2D Integer8 OK'
   DEALLOCATE(ilpa_2D_I8)
   CALL MIOL_read_field_NC(il_file_id,'Var3D_Integer',ilpa_3D)
   WRITE (0,*) 'Read 3D Integer OK'
   DEALLOCATE(ilpa_3D)
   CALL MIOL_read_field_NC(il_file_id,'Var3D_Integer8',ilpa_3D_I8)
   WRITE (0,*) 'Read 3D Integer8 OK'
   DEALLOCATE(ilpa_3D_I8)
   CALL MIOL_read_field_NC(il_file_id,'Var4D_Integer',ilpa_4D)
   WRITE (0,*) 'Read 4D Integer OK'
   DEALLOCATE(ilpa_4D)
   CALL MIOL_read_field_NC(il_file_id,'Var4D_Integer8',ilpa_4D_I8)
   WRITE (0,*) 'Read 4D Integer8 OK'
   DEALLOCATE(ilpa_4D_I8)
   WRITE (0,*) 'Read 1D Float'
   CALL MIOL_read_field_NC(il_file_id,'Var1D_R4',rlpa_1D) 
   WRITE (0,*) 'Read 1D Float OK'
   DEALLOCATE(rlpa_1D)
   CALL MIOL_read_field_NC(il_file_id,'Var2D_R4',rlpa_2D)
   WRITE (0,*) 'Read 2D Float OK'
   DEALLOCATE(rlpa_2D)
   CALL MIOL_read_field_NC(il_file_id,'Var3D_R4',rlpa_3D)
   WRITE (0,*) 'Read 3D Float OK'
   DEALLOCATE(rlpa_3D)
   CALL MIOL_read_field_NC(il_file_id,'Var4D_R4',rlpa_4D)
   WRITE (0,*) 'Read 4D Float OK'
   DEALLOCATE(rlpa_4D)
   !CALL MIOL_read_field_NC(il_file_id,'Var5D_R4',rlpa_5D)
   !WRITE (0,*) 'Read 5D Float OK'
   !DEALLOCATE(rlpa_5D)
   CALL MIOL_read_field_NC(il_file_id,'Var1D_R8',rlpa_1D_R8) 
   WRITE (0,*) 'Read 1D Double OK'
   DEALLOCATE(rlpa_1D_R8)
   CALL MIOL_read_field_NC(il_file_id,'Var2D_R8',rlpa_2D_R8)
   WRITE (0,*) 'Read 2D Double OK'
   DEALLOCATE(rlpa_2D_R8)
   CALL MIOL_read_field_NC(il_file_id,'Var3D_R8',rlpa_3D_R8)
   WRITE (0,*) 'Read 3D Double OK'
   DEALLOCATE(rlpa_3D_R8)
   CALL MIOL_read_field_NC(il_file_id,'Var4D_R8',rlpa_4D_R8)
   WRITE (0,*) 'Read 4D Double OK'
   DEALLOCATE(rlpa_4D_R8)
   CALL MIOL_close_file_NC(il_file_id)
   WRITE (0,*) '---------------------------------------------------------'
   WRITE (0,*) '               Test Read  OK                             '
   WRITE (0,*) '*********************************************************'
 !    CALL MIOL_read_field_NC(il_filein, cl_temp, ilpa_temp, ila_dimlen3D,rlpa_temp,rla_offsetvalues)
   
   END PROGRAM Test_read_write_MFT 
