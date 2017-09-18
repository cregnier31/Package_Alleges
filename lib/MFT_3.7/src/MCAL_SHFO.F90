MODULE MCAL_SHFO
!> \brief
!***************************************************************************!
!********************* Shapiro Filtering into Orca grids *******************!
!***************************************************************************!
! This module contains the implementation of shapiro filters adapted to 
! the ORCA grid
! It contains the following subroutines :
!
! 1) SHFO_1ofilt2D_fick_isot
! 2) SHFO_1ofilt2D_fick_anisot -> sensible from 3.73
!
! 3) SHFO_1ofilt2D_ideal_isot_withmix
! 4) SHFO_1ofilt2D_ideal_anisot_withmix -> sensible from 3.73
!
! 5) SHFO_1ofilt2D_ideal_isot_nomix
! 6) SHFO_1ofilt2D_ideal_anisot_nomix -> sensible from 1.9
!
! 7) SHFO_1ofilt2D_ideal_isot_bfick
! 8) SHFO_1ofilt2D_ideal_anisot_bfick -> sensible from 1.9
!
! 9) SHFO_2mofilt2D_ideal_isot -> with the mask technique used, it does not mix "nomix"
!10) SHFO_2mofilt2D_ideal_anisot -> not stable. sensible from 1.412 < 1.8 Orca's max anisot
!
!11) SHFO_2mofilt2D_ideal_isot_bfick
!12) SHFO_2mofilt2D_ideal_anisot_bfick   -> not stable from 50 iter. sensible from 1.412 < 1.8 Orca's max anisot
!
! COMMENTS : - At sea surface, du to masks, 902 503*nb_iter is done.(rather than 1442*1021=1 472 282).
!		the deaper we get, the less operations we do.
!            - After several tests, the 2nd order implicit turns to be imlpicitly
!              "not mixing" hot and cold waters. Its is stable for its isotropic version.
!              it is perfectly adapted except for the sensibility of the anisotropic
!              version.
!            - Implement the 5th, 6th, 7th and 8th order implicit shapiro filters.
! \Author : Zakaria TEFFAH  : Creation of the module
! \date : 13/08/2010
! \date : 21/06/2016 C.REGNIER : Integration MIOL V3.6
! \version MIOL V3.6
!!==========================================================================
!<


IMPLICIT NONE
        PUBLIC ::  extraplate_fick_isot,extraplate_fick_isot_zoom,SHFO_1ofilt2D_fick_isot,SHFO_1ofilt2D_fick_anisot,&
                 & SHFO_1ofilt2D_ideal_isot_withmix,SHFO_1ofilt2D_ideal_anisot_withmix,SHFO_1ofilt2D_ideal_isot_nomix,&
                 & SHFO_1ofilt2D_ideal_anisot_nomix,SHFO_1ofilt2D_ideal_isot_bfick,SHFO_1ofilt2D_ideal_anisot_bfick,&
                 & SHFO_2mofilt2D_ideal_isot,SHFO_2mofilt2D_ideal_anisot,SHFO_2mofilt2D_ideal_isot_bfick,SHFO_2mofilt2D_ideal_anisot_bfick,&
                 & SHFO_3mofilt2D_ideal_isot,SHFO_3mofilt2D_ideal_anisot,SHFO_3mofilt2D_ideal_isot_bfick,SHFO_3mofilt2D_ideal_anisot_bfick,&
                 & SHFO_4mofilt2D_ideal_isot,SHFO_4mofilt2D_ideal_anisot,SHFO_5mofilt2D_ideal_isot,SHFO_5mofilt2D_ideal_anisot,SHFO_6mofilt2D_ideal_isot,&
                 & SHFO_6mofilt2D_ideal_anisot,SHFO_7mofilt2D_ideal_isot,SHFO_7mofilt2D_ideal_anisot,SHFO_8mofilt2D_ideal_isot,SHFO_8mofilt2D_ideal_anisot,&
                 & Sobel_filt2D_isot,Prewitt_filt2D_isot

CONTAINS
  SUBROUTINE extraplate_fick_isot(rda_field,id_npShap,id_Nxi,id_Nyj,ida_mask,ida_new_mask,rda_filtred_field)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************

    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field
    
    IMPLICIT NONE
   
    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask 
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: ida_new_mask
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj)              :: ida_tmp_mask    
    REAL(kind=8),DIMENSION(id_Nxi,id_Nyj)              :: rla_tmp_field
    REAL(kind=8) 	                               :: rl_sum_mask,test
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
     !! Interior memory allocation 
!     ALLOCATE(rda_filtred_field(id_Nxi,id_Nyj))
!
    !! to conserve the original signal, we create its copy   
    rla_tmp_field(:,:) = rda_field(:,:)
    rda_filtred_field(:,:) = rda_field(:,:)
    ida_new_mask(:,:) = ida_mask(:,:)
    ida_tmp_mask(:,:) = ida_mask(:,:)
!    
    !! Filter application's loop    
    DO il_np = 1,id_npShap
      DO il_jj = 2,id_Nyj-1
        DO il_ji = 2,id_Nxi-1
          test = ida_tmp_mask(il_ji-1,il_jj)+ida_tmp_mask(il_ji+1,il_jj)+ &
              ida_tmp_mask(il_ji,il_jj-1)+ida_tmp_mask(il_ji,il_jj+1)             
          IF ((ida_tmp_mask(il_ji,il_jj).EQ.0.).AND.(test.NE.0))  THEN !! We dont get in land                   
            rl_sum_mask = 1/(ida_tmp_mask(il_ji-1,il_jj)+ida_tmp_mask(il_ji+1,il_jj) + &
              ida_tmp_mask(il_ji,il_jj-1)+ida_tmp_mask(il_ji,il_jj+1))
            !! extrapolating cell process
            rda_filtred_field(il_ji,il_jj) = &
              rl_sum_mask * &
              ( rla_tmp_field(il_ji-1,il_jj)*ida_tmp_mask(il_ji-1,il_jj) + &
                rla_tmp_field(il_ji+1,il_jj)*ida_tmp_mask(il_ji+1,il_jj) + & 
                rla_tmp_field(il_ji,il_jj-1)*ida_tmp_mask(il_ji,il_jj-1) + &
                rla_tmp_field(il_ji,il_jj+1)*ida_tmp_mask(il_ji,il_jj+1) )
            ida_new_mask(il_ji,il_jj) = 1.              
!                
          END IF
        END DO
      END DO 
!               
      ! Est-West periodic conditions
      rda_filtred_field(1,:) = rda_filtred_field(id_Nxi-1,:) 
      rda_filtred_field(id_Nxi,:) = rda_filtred_field(2,:) 
      ida_new_mask(1,:) = ida_new_mask(id_Nxi-1,:) 
      ida_new_mask(id_Nxi,:) = ida_new_mask(2,:) 
      ! North-south periodic conditions
      rda_filtred_field(1,1) = 0.e0   
      rda_filtred_field(1,id_Nyj) = 0.e0
      rda_filtred_field(id_Nxi,id_Nyj) = 0.e0
      ida_new_mask(1,1) = 0.
      ida_new_mask(1,id_Nyj) = 0.
      ida_new_mask(id_Nxi,id_Nyj) = 0.
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi,1) = 0.e0
        ida_new_mask(il_jxi,1) = 0.
        rda_filtred_field(il_jxi,id_Nyj) = rda_filtred_field(il_ijdt,id_Nyj-2)
        ida_new_mask(il_jxi,id_Nyj) = ida_new_mask(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi,id_Nyj-1) = rda_filtred_field(il_ijdt,id_Nyj-1)
        ida_new_mask(il_jxi,id_Nyj-1) = ida_new_mask(il_ijdt,id_Nyj-1)
      ENDDO
      ! Incrementation : Shapiro filter's iteration
      rla_tmp_field(:,:) = rda_filtred_field(:,:)
      ida_tmp_mask(:,:) = ida_new_mask(:,:)
    END DO !! end the filter iterations
!      
  END SUBROUTINE extraplate_fick_isot
!******************************************************************************
!******************************************************************************
!******************************************************************************
  SUBROUTINE extraplate_fick_isot_zoom(rda_field,id_npShap,id_Nxi,id_Nyj,ida_mask,ida_new_mask,rda_filtred_field)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************

    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field
    
    IMPLICIT NONE
   
    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask 
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field
!    REAL(kind=4),DIMENSION(:,:),POINTER,  INTENT(OUT)  :: rda_filtred_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: ida_new_mask
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj)              :: ida_tmp_mask    
    REAL(kind=8),DIMENSION(id_Nxi,id_Nyj)              :: rla_tmp_field
    REAL(kind=8) 	                               :: rl_sum_mask,test
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
     !! Interior memory allocation 
!     ALLOCATE(rda_filtred_field(id_Nxi,id_Nyj))
!
    !! to conserve the original signal, we create its copy   
    rla_tmp_field(:,:) = rda_field(:,:)
    rda_filtred_field(:,:) = rda_field(:,:)
    ida_new_mask(:,:) = ida_mask(:,:)
    ida_tmp_mask(:,:) = ida_mask(:,:)
!    
    !! Filter application's loop    
    DO il_np = 1,id_npShap
      DO il_jj = 2,id_Nyj-1
        DO il_ji = 2,id_Nxi-1
          test = ida_tmp_mask(il_ji-1,il_jj)+ida_tmp_mask(il_ji+1,il_jj)+ &
              ida_tmp_mask(il_ji,il_jj-1)+ida_tmp_mask(il_ji,il_jj+1)             
          IF ((ida_tmp_mask(il_ji,il_jj).EQ.0.).AND.(test.NE.0))  THEN !! We dont get in land                   
            rl_sum_mask = 1/(ida_tmp_mask(il_ji-1,il_jj)+ida_tmp_mask(il_ji+1,il_jj) + &
              ida_tmp_mask(il_ji,il_jj-1)+ida_tmp_mask(il_ji,il_jj+1))
            !! extrapolating cell process
            rda_filtred_field(il_ji,il_jj) = &
              rl_sum_mask * &
              ( rla_tmp_field(il_ji-1,il_jj)*ida_tmp_mask(il_ji-1,il_jj) + &
                rla_tmp_field(il_ji+1,il_jj)*ida_tmp_mask(il_ji+1,il_jj) + & 
                rla_tmp_field(il_ji,il_jj-1)*ida_tmp_mask(il_ji,il_jj-1) + &
                rla_tmp_field(il_ji,il_jj+1)*ida_tmp_mask(il_ji,il_jj+1) )
            ida_new_mask(il_ji,il_jj) = 1.              
!                
          END IF
        END DO
      END DO 
!                     
      ! Incrementation : Shapiro filter's iteration
      rla_tmp_field(:,:) = rda_filtred_field(:,:)
      ida_tmp_mask(:,:) = ida_new_mask(:,:)
    END DO !! end the filter iterations
!      
  END SUBROUTINE extraplate_fick_isot_zoom
!******************************************************************************
!******************************************************************************
!******************************************************************************
!! 1)
  SUBROUTINE SHFO_1ofilt2D_fick_isot(rda_field,id_npShap,id_Nxi,id_Nyj,ida_mask,rda_filtred_field, cl_nft)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************

    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field
    !   cl_nft = 'F-fold' or 'T-fold'
    
    IMPLICIT NONE
   
    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask 
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field
    REAL(kind=4),DIMENSION(:,:),          INTENT(OUT)  :: rda_filtred_field
    CHARACTER(LEN=6),          INTENT(in)  :: cl_nft
    REAL(kind=8),DIMENSION(id_Nxi,id_Nyj)              :: rla_tmp_field
    REAL(kind=8) 	                               :: rl_smooth,rl_coefsmooth
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt,compt
!
!

!
    !! to conserve the original signal, we create its copy   
    rla_tmp_field(:,:)=rda_field(:,:)
    rda_filtred_field(:,:)=rda_field(:,:)
!
    !! The Shapiro smotting elements according to the 2 directions
    rl_smooth = 0.5
    rl_coefsmooth = rl_smooth*0.25
!    
    !! Filter application's loop    
    DO il_np = 1,id_npShap
      DO il_jj = 2,id_Nyj-1
        DO il_ji = 2,id_Nxi-1             
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN !! We dont get in land                   
!          
            !! filtering cell process
            rda_filtred_field(il_ji,il_jj) = rla_tmp_field(il_ji,il_jj) + &
              rl_coefsmooth * &
              ( (rla_tmp_field(il_ji-1,il_jj)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji-1,il_jj) + &
                (rla_tmp_field(il_ji+1,il_jj)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji+1,il_jj) + & 
                (rla_tmp_field(il_ji,il_jj-1)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj-1) + &
                (rla_tmp_field(il_ji,il_jj+1)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj+1) )			
!                
          END IF
        END DO
      END DO 
!               
      ! Est-West periodic conditions
      rda_filtred_field(1,:) = rda_filtred_field(id_Nxi-1,:) 
      rda_filtred_field(id_Nxi,:) = rda_filtred_field(2,:) 
      IF ( cl_nft .EQ. 'T-fold' ) THEN 
      ! North-south periodic conditions T-fold
        rda_filtred_field(1,1) = 0.e0   
        rda_filtred_field(1,id_Nyj) = 0.e0
        rda_filtred_field(id_Nxi,id_Nyj) = 0.e0
        DO il_jxi = 2, id_Nxi
          il_ijdt = id_Nxi-il_jxi+2
          rda_filtred_field(il_jxi, 1 ) = 0.e0
          rda_filtred_field(il_jxi,id_Nyj) = rda_filtred_field(il_ijdt,id_Nyj-2)
        ENDDO
        DO il_jxi = id_Nxi/2+1, id_Nxi
          il_ijdt = id_Nxi-il_jxi+2
          rda_filtred_field(il_jxi,id_Nyj-1) = rda_filtred_field(il_ijdt,id_Nyj-1)
        ENDDO
      ELSE IF ( cl_nft .EQ. 'F-fold' ) THEN
      ! North-south periodic conditions F-fold
!     ocean model configuration used :   cp_cfg = orca             jp_cfg = 10042
!    global domain lateral boundaries
!       jperio= 6, cyclic east-west and north fold with F-point pivot

        rda_filtred_field(1,id_Nyj) = 0.0
        rda_filtred_field(id_Nxi,id_Nyj) = 0.0
        DO il_jxi = 1,id_Nxi  
          il_ijdt = id_Nxi-il_jxi+1
          rda_filtred_field(il_jxi,id_Nyj) = rda_filtred_field(il_ijdt,id_Nyj-1)
        END DO
      ELSE
        WRITE(*,*) 'cl_nft = ',cl_nft, ' pas autorise, WE STOP'
        STOP
      ENDIF
      ! Incrementation : Shapiro filter's iteration
      rla_tmp_field(:,:) = rda_filtred_field(:,:)       
    END DO !! end the filter iterations
!      
  END SUBROUTINE SHFO_1ofilt2D_fick_isot
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
!! 2)
  SUBROUTINE SHFO_1ofilt2D_fick_anisot(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field,rda_e1t,rda_e2t,rda_e1u, &
 		rda_e2u,rda_e1v,rda_e2v)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************


    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field
    
    IMPLICIT NONE
   
    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask 
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field,rda_e1t,rda_e2t, &
    							  rda_e1u,rda_e2u,rda_e1v,rda_e2v
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    REAL(kind=8),DIMENSION(id_Nxi,id_Nyj)              :: rla_tmp_field
    REAL(kind=8) 	                               :: rl_smooth,rl_coefsmooth,rl_aniso_px,rl_aniso_mx, & 
    							  rl_aniso_py,rl_aniso_my,rl_aniso_xyi,rl_aniso_xyj
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
    !! to conserve the original signal, we create its copy   
    rla_tmp_field(:,:) = rda_field(:,:)
    rda_filtred_field(:,:)=rda_field(:,:)    
!
    !! The Shapiro smotting elements according to the 2 directions
    rl_smooth = 0.5
    rl_coefsmooth = rl_smooth*0.25
!    
    !! Filter application's loop    
    DO il_np = 1,id_npShap
      DO il_jj = 2,id_Nyj-1
        DO il_ji = 2,id_Nxi-1             
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN !! We dont get in land   
            !! Anisitrpic weight compute
            rl_aniso_px = ((rda_e1t(il_ji+1,il_jj))/(rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj))
 	    rl_aniso_mx = -((rda_e1t(il_ji-1,il_jj))/(rda_e1t(il_ji-1,il_jj)+rda_e1t(il_ji,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)) 	     	       	                  
 	    rl_aniso_py = ((rda_e2t(il_ji,il_jj+1))/(rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
              (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj))
 	    rl_aniso_my = -((rda_e2t(il_ji,il_jj-1))/(rda_e2t(il_ji,il_jj-1)+rda_e2t(il_ji,il_jj)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
 	      (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)) 	      
 	    rl_aniso_xyi = rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)-0.5*(((rda_e1t(il_ji,il_jj))/ &
 	      (rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))-((rda_e1t(il_ji,il_jj))/(rda_e1t(il_ji-1,il_jj)+ &
 	      rda_e1t(il_ji,il_jj))))*((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))- &
 	      (rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj)))
 	    rl_aniso_xyj = rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)-0.5*(((rda_e2t(il_ji,il_jj))/ &
 	      (rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))-((rda_e2t(il_ji,il_jj))/(rda_e2t(il_ji,il_jj-1)+ &
 	      rda_e2t(il_ji,il_jj))))*((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))- &
 	      (rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1)))
!
 	    !! filtering cell process 	   
            rda_filtred_field(il_ji,il_jj) = rla_tmp_field(il_ji,il_jj) + &
              rl_coefsmooth * &
              ( (rl_aniso_mx*rla_tmp_field(il_ji-1,il_jj)-rl_aniso_xyi*rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji-1,il_jj) + &
                (rl_aniso_px*rla_tmp_field(il_ji+1,il_jj)-rl_aniso_xyi*rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji+1,il_jj) + & 
                (rl_aniso_my*rla_tmp_field(il_ji,il_jj-1)-rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj-1) + &
                (rl_aniso_py*rla_tmp_field(il_ji,il_jj+1)-rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj+1) )
!                			
          END IF
        END DO
      END DO          
!            
      ! Est-West periodic conditions
      rda_filtred_field(1,:) = rda_filtred_field(id_Nxi-1,:) 
      rda_filtred_field(id_Nxi,:) = rda_filtred_field(2,:) 
      ! North-south periodic conditions
      rda_filtred_field(1,1) = 0.e0   
      rda_filtred_field(1,id_Nyj) = 0.e0
      rda_filtred_field(id_Nxi,id_Nyj) = 0.e0
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi, 1 ) = 0.e0
        rda_filtred_field(il_jxi,id_Nyj) = rda_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi,id_Nyj-1) = rda_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
      ! Incrementation : Shapiro filter's iteration
      rla_tmp_field(:,:) = rda_filtred_field(:,:)       
    END DO !! end the filter iterations
!      
  END SUBROUTINE SHFO_1ofilt2D_fick_anisot
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
!! 3)
  SUBROUTINE SHFO_1ofilt2D_ideal_isot_withmix(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************


    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field
    
    IMPLICIT NONE
   
    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask 
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    REAL(kind=8),DIMENSION(id_Nxi,id_Nyj)              :: rla_tmp_field
    REAL(kind=8) 	                               :: rl_smooth,rl_coefsmooth,rl_coefsmooth_diag
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
    !! to conserve the original signal, we create its copy   
    rla_tmp_field(:,:) = rda_field(:,:)
    rda_filtred_field(:,:)=rda_field(:,:)    
!
    !! The Shapiro smotting elements according to the 2 directions
    rl_smooth = 0.5
    rl_coefsmooth = (rl_smooth/2.)*(1.-rl_smooth)
    rl_coefsmooth_diag = (rl_smooth*rl_smooth)*0.25    
!    
    !! Filter application's loop    
    DO il_np=1,id_npShap
      DO il_jj=2,id_Nyj-1
        DO il_ji=2,id_Nxi-1             
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN !! We dont get in land   
!
            !! filtering cell process
            rda_filtred_field(il_ji,il_jj) = rla_tmp_field(il_ji,il_jj) + &
              rl_coefsmooth * &
              ( (rla_tmp_field(il_ji,il_jj-1)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj-1) + &
                (rla_tmp_field(il_ji,il_jj+1)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj+1) + & 
	        (rla_tmp_field(il_ji-1,il_jj)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji-1,il_jj) + &
                (rla_tmp_field(il_ji+1,il_jj)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji+1,il_jj) ) + &               
              rl_coefsmooth_diag * &
              ( (rla_tmp_field(il_ji-1,il_jj-1)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji-1,il_jj-1)+ &
                (rla_tmp_field(il_ji+1,il_jj-1)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji+1,il_jj-1)+ &
                (rla_tmp_field(il_ji-1,il_jj+1)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji-1,il_jj+1)+ &
                (rla_tmp_field(il_ji+1,il_jj+1)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji+1,il_jj+1) ) 	     	               		
!                
          END IF
        END DO
      END DO          
!            
      ! Est-West periodic conditions
      rda_filtred_field(1,:) = rda_filtred_field(id_Nxi-1,:) 
      rda_filtred_field(id_Nxi,:) = rda_filtred_field(2,:) 
      ! North-south periodic conditions
      rda_filtred_field(1,1) = 0.e0   
      rda_filtred_field(1,id_Nyj) = 0.e0
      rda_filtred_field(id_Nxi,id_Nyj) = 0.e0
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi, 1 ) = 0.e0
        rda_filtred_field(il_jxi,id_Nyj) = rda_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi,id_Nyj-1) = rda_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
      ! Incrementation : Shapiro filter's iteration
      rla_tmp_field(:,:) = rda_filtred_field(:,:)       
    END DO !! end the filter iterations
!      
!
  END SUBROUTINE SHFO_1ofilt2D_ideal_isot_withmix
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
!! 4)
  SUBROUTINE SHFO_1ofilt2D_ideal_anisot_withmix(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field,rda_e1t,rda_e2t,rda_e1u, &
 		rda_e2u,rda_e1v,rda_e2v)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************


    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field
    
    IMPLICIT NONE
   
    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask 
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field,rda_e1t,rda_e2t, &
    							  rda_e1u,rda_e2u,rda_e1v,rda_e2v
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    REAL(kind=8),DIMENSION(id_Nxi,id_Nyj)              :: rla_tmp_field
    REAL(kind=8) 	                               :: rl_smooth_x,rl_smooth_y,rl_coefsmooth_x, &
    							  rl_coefsmooth_y,rl_coefsmooth_diag,rl_aniso_px,rl_aniso_mx, & 
    							  rl_aniso_py,rl_aniso_my,rl_aniso_xyi,rl_aniso_xyj, &
    							  rl_smooth_x_s2,rl_smooth_y_s2
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
    !! to conserve the original signal, we create its copy   
    rla_tmp_field(:,:)=rda_field(:,:)
    rda_filtred_field(:,:)=rda_field(:,:)    
!
    !! The Shapiro smotting elements according to the 2 directions    
    rl_smooth_x=0.5;rl_smooth_y=0.5
    rl_smooth_x_s2=0.25;rl_smooth_y_s2=0.25
    rl_coefsmooth_diag=(rl_smooth_x*rl_smooth_y)*0.25    
!    
    !! Filter application's loop    
    DO il_np=1,id_npShap
      DO il_jj=2,id_Nyj-1
        DO il_ji=2,id_Nxi-1             
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN !! We dont get in land   
            !! Anisitrpic weight compute
            rl_aniso_px = ((rda_e1t(il_ji+1,il_jj))/(rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj))
 	    rl_aniso_mx = -((rda_e1t(il_ji-1,il_jj))/(rda_e1t(il_ji-1,il_jj)+rda_e1t(il_ji,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)) 	     	       	                  
 	    rl_aniso_py = ((rda_e2t(il_ji,il_jj+1))/(rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
              (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj))
 	    rl_aniso_my = -((rda_e2t(il_ji,il_jj-1))/(rda_e2t(il_ji,il_jj-1)+rda_e2t(il_ji,il_jj)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
 	      (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)) 	      
 	    rl_aniso_xyi = rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)-0.5*(((rda_e1t(il_ji,il_jj))/ &
 	      (rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))-((rda_e1t(il_ji,il_jj))/(rda_e1t(il_ji-1,il_jj)+ &
 	      rda_e1t(il_ji,il_jj))))*((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))- &
 	      (rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj)))
 	    rl_aniso_xyj = rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)-0.5*(((rda_e2t(il_ji,il_jj))/ &
 	      (rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))-((rda_e2t(il_ji,il_jj))/(rda_e2t(il_ji,il_jj-1)+ &
 	      rda_e2t(il_ji,il_jj))))*((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))- &
 	      (rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1)))
!
	    rl_coefsmooth_x = (rl_smooth_x_s2)*(1.-rl_smooth_y*rl_aniso_xyj)
	    rl_coefsmooth_y = (rl_smooth_y_s2)*(1.-rl_smooth_x*rl_aniso_xyi)
!     
            !! filtering cell process
	    rda_filtred_field(il_ji,il_jj) = rla_tmp_field(il_ji,il_jj) + &
              rl_coefsmooth_y * &
              ( (rl_aniso_my*rla_tmp_field(il_ji,il_jj-1)-rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj-1) + &
                (rl_aniso_py*rla_tmp_field(il_ji,il_jj+1)-rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj+1) ) + &              
	      rl_coefsmooth_x * &
	      ( (rl_aniso_mx*rla_tmp_field(il_ji-1,il_jj)-rl_aniso_xyi*rla_tmp_field(il_ji,il_jj))* &
	           ida_mask(il_ji-1,il_jj) + &
                (rl_aniso_px*rla_tmp_field(il_ji+1,il_jj)-rl_aniso_xyi*rla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji+1,il_jj) ) + &               
              rl_coefsmooth_diag * &
              ( (rl_aniso_mx*rl_aniso_my*rla_tmp_field(il_ji-1,il_jj-1)- &
                   rl_aniso_xyi*rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji-1,il_jj-1)+ &
                (rl_aniso_px*rl_aniso_my*rla_tmp_field(il_ji+1,il_jj-1)- &
                   rl_aniso_xyi*rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji+1,il_jj-1)+ &
                (rl_aniso_mx*rl_aniso_py*rla_tmp_field(il_ji-1,il_jj+1)- &
                   rl_aniso_xyi*rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji-1,il_jj+1)+ &
                (rl_aniso_px*rl_aniso_py*rla_tmp_field(il_ji+1,il_jj+1)- &
                   rl_aniso_xyi*rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji+1,il_jj+1) ) 
!                   	    		
          END IF
        END DO
      END DO          
!            
      ! Est-West periodic conditions
      rda_filtred_field(1,:) = rda_filtred_field(id_Nxi-1,:) 
      rda_filtred_field(id_Nxi,:) = rda_filtred_field(2,:) 
      ! North-south periodic conditions
      rda_filtred_field(1,1) = 0.e0   
      rda_filtred_field(1,id_Nyj) = 0.e0
      rda_filtred_field(id_Nxi,id_Nyj) = 0.e0
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi, 1 ) = 0.e0
        rda_filtred_field(il_jxi,id_Nyj) = rda_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi,id_Nyj-1) = rda_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
      ! Incrementation : Shapiro filter's iteration
      rla_tmp_field(:,:) = rda_filtred_field(:,:)       
    END DO !! end the filter iterations
!      
  END SUBROUTINE SHFO_1ofilt2D_ideal_anisot_withmix
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
!! 5)
  SUBROUTINE SHFO_1ofilt2D_ideal_isot_nomix(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************


    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field
    
    IMPLICIT NONE
   
    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj)              :: ila_mask
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    REAL(kind=8),DIMENSION(id_Nxi,id_Nyj)              :: rla_tmp_field
    REAL(kind=8) 	                               :: rl_smooth,rl_coefsmooth,rl_coefsmooth_diag
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt,il_test_1, &
                                                          il_test_2,il_test_3,il_test_4,il_test_5
!
!
    !! to conserve the original signal, we create its copy   
    rla_tmp_field(:,:) = rda_field(:,:)
    rda_filtred_field(:,:)=rda_field(:,:)    
!
    !! The Shapiro smotting elements
    rl_smooth = 0.5d0
    rl_coefsmooth = (rl_smooth/2.)*(1.-rl_smooth)
    rl_coefsmooth_diag = (rl_smooth*rl_smooth)*0.25
!    
    !! Filter application's loop    
    DO il_np=1,id_npShap
      DO il_jj=2,id_Nyj-1
        DO il_ji=2,id_Nxi-1             
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN !! We dont get in land
            !! The treatment of diagonal shape islands            
            ila_mask(il_ji,il_jj-1) = ida_mask(il_ji,il_jj-1)
            ila_mask(il_ji,il_jj+1) = ida_mask(il_ji,il_jj+1)
            ila_mask(il_ji-1,il_jj) = ida_mask(il_ji-1,il_jj)
            ila_mask(il_ji+1,il_jj) = ida_mask(il_ji+1,il_jj)
            ila_mask(il_ji-1,il_jj-1) = ida_mask(il_ji-1,il_jj-1)
            ila_mask(il_ji+1,il_jj-1) = ida_mask(il_ji+1,il_jj-1)
            ila_mask(il_ji-1,il_jj+1) = ida_mask(il_ji-1,il_jj+1)
            ila_mask(il_ji+1,il_jj+1) = ida_mask(il_ji+1,il_jj+1)
            il_test_1 = ida_mask(il_ji-1,il_jj)+ida_mask(il_ji,il_jj+1)
            il_test_2 = ida_mask(il_ji+1,il_jj)+ida_mask(il_ji,il_jj+1)
            il_test_3 = ida_mask(il_ji+1,il_jj)+ida_mask(il_ji,il_jj-1)
            il_test_4 = ida_mask(il_ji-1,il_jj)+ida_mask(il_ji,il_jj-1)
	    IF (il_test_1.EQ.0.) ila_mask(il_ji-1,il_jj+1)=0.	
	    IF (il_test_2.EQ.0.) ila_mask(il_ji+1,il_jj+1)=0.
	    IF (il_test_3.EQ.0.) ila_mask(il_ji+1,il_jj-1)=0.
	    IF (il_test_4.EQ.0.) ila_mask(il_ji-1,il_jj-1)=0.
!
            !! filtering cell process	    
            rda_filtred_field(il_ji,il_jj) = rla_tmp_field(il_ji,il_jj) + &
              rl_coefsmooth * &
              ( (rla_tmp_field(il_ji,il_jj-1)-rla_tmp_field(il_ji,il_jj))*ila_mask(il_ji,il_jj-1) + &
                (rla_tmp_field(il_ji,il_jj+1)-rla_tmp_field(il_ji,il_jj))*ila_mask(il_ji,il_jj+1) + &              
	        (rla_tmp_field(il_ji-1,il_jj)-rla_tmp_field(il_ji,il_jj))*ila_mask(il_ji-1,il_jj) + &
                (rla_tmp_field(il_ji+1,il_jj)-rla_tmp_field(il_ji,il_jj))*ila_mask(il_ji+1,il_jj) ) + &               
              rl_coefsmooth_diag * &
              ( (rla_tmp_field(il_ji-1,il_jj-1)-rla_tmp_field(il_ji,il_jj))*ila_mask(il_ji-1,il_jj-1)+ &
                (rla_tmp_field(il_ji+1,il_jj-1)-rla_tmp_field(il_ji,il_jj))*ila_mask(il_ji+1,il_jj-1)+ &
                (rla_tmp_field(il_ji-1,il_jj+1)-rla_tmp_field(il_ji,il_jj))*ila_mask(il_ji-1,il_jj+1)+ &
                (rla_tmp_field(il_ji+1,il_jj+1)-rla_tmp_field(il_ji,il_jj))*ila_mask(il_ji+1,il_jj+1) )
!           
          END IF
        END DO
      END DO                
!      
      ! Est-West periodic conditions
      rda_filtred_field(1,:) = rda_filtred_field(id_Nxi-1,:) 
      rda_filtred_field(id_Nxi,:) = rda_filtred_field(2,:) 
      ! North-south periodic conditions
      rda_filtred_field(1,1) = 0.e0   
      rda_filtred_field(1,id_Nyj) = 0.e0
      rda_filtred_field(id_Nxi,id_Nyj) = 0.e0
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi, 1 ) = 0.e0
        rda_filtred_field(il_jxi,id_Nyj) = rda_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi,id_Nyj-1) = rda_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!      
      ! Incrementation : Shapiro filter's iteration
      rla_tmp_field(:,:) = rda_filtred_field(:,:)       
    END DO !! end the filter iterations
!      
  END SUBROUTINE SHFO_1ofilt2D_ideal_isot_nomix
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
!! 6)
  SUBROUTINE SHFO_1ofilt2D_ideal_anisot_nomix(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field,rda_e1t,rda_e2t,rda_e1u, &
 		rda_e2u,rda_e1v,rda_e2v)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************


    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field
    
    IMPLICIT NONE
   
    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask 
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj)              :: ila_mask 
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field,rda_e1t,rda_e2t, &
    							  rda_e1u,rda_e2u,rda_e1v,rda_e2v
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    REAL(kind=8),DIMENSION(id_Nxi,id_Nyj)              :: rla_tmp_field
    REAL(kind=8) 	                               :: rl_smooth_x,rl_smooth_y,rl_coefsmooth_x, &
    							  rl_coefsmooth_y,rl_coefsmooth_diag,rl_aniso_px,rl_aniso_mx, & 
    							  rl_aniso_py,rl_aniso_my,rl_aniso_xyi,rl_aniso_xyj, &
    							  rl_smooth_x_s2,rl_smooth_y_s2
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt,il_test_1, &
                                                          il_test_2,il_test_3,il_test_4,il_test_5
!
!
    !! to conserve the original signal, we create its copy   
    rla_tmp_field(:,:)=rda_field(:,:)
    rda_filtred_field(:,:)=rda_field(:,:)
!
    !! The Shapiro smotting elements according to the 2 directions
    rl_smooth_x=0.5;rl_smooth_y=0.5
    rl_smooth_x_s2=0.25;rl_smooth_y_s2=0.25
    rl_coefsmooth_diag=(rl_smooth_x*rl_smooth_y)*0.25
!    
    !! Filter application's loop    
    DO il_np=1,id_npShap
      DO il_jj=2,id_Nyj-1
        DO il_ji=2,id_Nxi-1             
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN !! We dont get in land
            !! The treatment of diagonal shape islands
            ila_mask(il_ji,il_jj-1) = ida_mask(il_ji,il_jj-1)
            ila_mask(il_ji,il_jj+1) = ida_mask(il_ji,il_jj+1)
            ila_mask(il_ji-1,il_jj) = ida_mask(il_ji-1,il_jj)
            ila_mask(il_ji+1,il_jj) = ida_mask(il_ji+1,il_jj)
            ila_mask(il_ji-1,il_jj-1) = ida_mask(il_ji-1,il_jj-1)
            ila_mask(il_ji+1,il_jj-1) = ida_mask(il_ji+1,il_jj-1)
            ila_mask(il_ji-1,il_jj+1) = ida_mask(il_ji-1,il_jj+1)
            ila_mask(il_ji+1,il_jj+1) = ida_mask(il_ji+1,il_jj+1)            
            il_test_1 = ida_mask(il_ji-1,il_jj)+ida_mask(il_ji,il_jj+1)
            il_test_2 = ida_mask(il_ji+1,il_jj)+ida_mask(il_ji,il_jj+1)
            il_test_3 = ida_mask(il_ji+1,il_jj)+ida_mask(il_ji,il_jj-1)
            il_test_4 = ida_mask(il_ji-1,il_jj)+ida_mask(il_ji,il_jj-1)
	    IF (il_test_1.EQ.0.) ila_mask(il_ji-1,il_jj+1)=0.
	    IF (il_test_2.EQ.0.) ila_mask(il_ji+1,il_jj+1)=0.
	    IF (il_test_3.EQ.0.) ila_mask(il_ji+1,il_jj-1)=0.
	    IF (il_test_4.EQ.0.) ila_mask(il_ji-1,il_jj-1)=0.
!          
            !! Anisotrpic weight compute
            rl_aniso_px = ((rda_e1t(il_ji+1,il_jj))/(rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj))
 	    rl_aniso_mx = -((rda_e1t(il_ji-1,il_jj))/(rda_e1t(il_ji-1,il_jj)+rda_e1t(il_ji,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)) 	     	       	                  
 	    rl_aniso_py = ((rda_e2t(il_ji,il_jj+1))/(rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
              (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj))
 	    rl_aniso_my =  -((rda_e2t(il_ji,il_jj-1))/(rda_e2t(il_ji,il_jj-1)+rda_e2t(il_ji,il_jj)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
 	      (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)) 	      
 	    rl_aniso_xyi = rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)-0.5*(((rda_e1t(il_ji,il_jj))/ &
 	      (rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))-((rda_e1t(il_ji,il_jj))/(rda_e1t(il_ji-1,il_jj)+ &
 	      rda_e1t(il_ji,il_jj))))*((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))- &
 	      (rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj)))
 	    rl_aniso_xyj = rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)-0.5*(((rda_e2t(il_ji,il_jj))/ &
 	      (rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))-((rda_e2t(il_ji,il_jj))/(rda_e2t(il_ji,il_jj-1)+ &
 	      rda_e2t(il_ji,il_jj))))*((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))- &
 	      (rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1)))
!
	    rl_coefsmooth_x = (rl_smooth_x_s2)*(1.-rl_smooth_y*rl_aniso_xyj)
	    rl_coefsmooth_y = (rl_smooth_y_s2)*(1.-rl_smooth_x*rl_aniso_xyi)
!
            !! filtering cell process    
	    rda_filtred_field(il_ji,il_jj) = rla_tmp_field(il_ji,il_jj) + &
              rl_coefsmooth_y * &
              ( (rl_aniso_my*rla_tmp_field(il_ji,il_jj-1)-rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))* &
                   ila_mask(il_ji,il_jj-1) + &
                (rl_aniso_py*rla_tmp_field(il_ji,il_jj+1)-rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))* &
                   ila_mask(il_ji,il_jj+1) ) + &              
	      rl_coefsmooth_x * &
	      ( (rl_aniso_mx*rla_tmp_field(il_ji-1,il_jj)-rl_aniso_xyi*rla_tmp_field(il_ji,il_jj))* &
	           ila_mask(il_ji-1,il_jj) + &
                (rl_aniso_px*rla_tmp_field(il_ji+1,il_jj)-rl_aniso_xyi*rla_tmp_field(il_ji,il_jj))* &
                   ila_mask(il_ji+1,il_jj) ) + &               
              rl_coefsmooth_diag * &
              ( (rl_aniso_mx*rl_aniso_my*rla_tmp_field(il_ji-1,il_jj-1)- &
                   rl_aniso_xyi*rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))*ila_mask(il_ji-1,il_jj-1)+ &
                (rl_aniso_px*rl_aniso_my*rla_tmp_field(il_ji+1,il_jj-1)- &
                   rl_aniso_xyi*rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))*ila_mask(il_ji+1,il_jj-1)+ &
                (rl_aniso_mx*rl_aniso_py*rla_tmp_field(il_ji-1,il_jj+1)- &
                   rl_aniso_xyi*rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))*ila_mask(il_ji-1,il_jj+1)+ &
                (rl_aniso_px*rl_aniso_py*rla_tmp_field(il_ji+1,il_jj+1)- &
                   rl_aniso_xyi*rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))*ila_mask(il_ji+1,il_jj+1) )
!                    
          END IF
        END DO
      END DO          
!            
      ! Est-West periodic conditions
      rda_filtred_field(1,:) = rda_filtred_field(id_Nxi-1,:) 
      rda_filtred_field(id_Nxi,:) = rda_filtred_field(2,:) 
      ! North-south periodic conditions
      rda_filtred_field(1,1) = 0.e0   
      rda_filtred_field(1,id_Nyj) = 0.e0
      rda_filtred_field(id_Nxi,id_Nyj) = 0.e0
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi, 1 ) = 0.e0
        rda_filtred_field(il_jxi,id_Nyj) = rda_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi,id_Nyj-1) = rda_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
      ! Incrementation : Shapiro filter's iteration
      rla_tmp_field(:,:) = rda_filtred_field(:,:)       
    END DO !! end the filter iterations
!      
  END SUBROUTINE SHFO_1ofilt2D_ideal_anisot_nomix
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
  SUBROUTINE SHFO_1ofilt2D_ideal_isot_bfick (rda_field,id_npShap,id_Nxi,id_Nyj,ida_mask,rda_filtred_field)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field
    IMPLICIT NONE
    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask  
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    REAL(kind=8),DIMENSION(id_Nxi,id_Nyj)              :: rla_tmp_field
    REAL(kind=8) 	                               :: rl_smooth,rl_coefsmooth,rl_coefsmooth_diag,rl_coefsmooth_bf
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt,il_test
!
!
    !! to conserve the original signal, we create its copy   
    rla_tmp_field(:,:) = rda_field(:,:)
    rda_filtred_field(:,:)=rda_field(:,:)
!    
    rl_smooth = 0.5d0
    rl_coefsmooth = (rl_smooth/2.)*(1.-rl_smooth)
    rl_coefsmooth_diag = (rl_smooth*rl_smooth)*0.25
    rl_coefsmooth_bf = 0.5*0.25 ! coeff=S/4 with S=1/2
!
    !! Filter application's loop   
    DO il_np = 1,id_npShap
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN 
            il_test = ida_mask(il_ji-1,il_jj)*ida_mask(il_ji+1,il_jj)* &
              ida_mask(il_ji,il_jj-1)*ida_mask(il_ji,il_jj+1)*ida_mask(il_ji-1,il_jj-1)* &
              ida_mask(il_ji-1,il_jj+1)*ida_mask(il_ji+1,il_jj-1)*ida_mask(il_ji+1,il_jj+1)
            IF (il_test.EQ.0) THEN
!          
              !! Fick filtering cell process on ocean boards
              rda_filtred_field(il_ji,il_jj) = rla_tmp_field(il_ji,il_jj) + &
                rl_coefsmooth_bf * &
                ( (rla_tmp_field(il_ji-1,il_jj)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji-1,il_jj) + &
                  (rla_tmp_field(il_ji+1,il_jj)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji+1,il_jj) + & 
                  (rla_tmp_field(il_ji,il_jj-1)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj-1) + &
                  (rla_tmp_field(il_ji,il_jj+1)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj+1) )			
!                
            ELSE
!
            !! filtering cell process	    
            rda_filtred_field(il_ji,il_jj) = rla_tmp_field(il_ji,il_jj) + &
              rl_coefsmooth * &
              ( (rla_tmp_field(il_ji,il_jj-1)-rla_tmp_field(il_ji,il_jj)) + &
                (rla_tmp_field(il_ji,il_jj+1)-rla_tmp_field(il_ji,il_jj)) + &              
	        (rla_tmp_field(il_ji-1,il_jj)-rla_tmp_field(il_ji,il_jj)) + &
                (rla_tmp_field(il_ji+1,il_jj)-rla_tmp_field(il_ji,il_jj)) ) + &               
              rl_coefsmooth_diag * &
              ( (rla_tmp_field(il_ji-1,il_jj-1)-rla_tmp_field(il_ji,il_jj)) + &
                (rla_tmp_field(il_ji+1,il_jj-1)-rla_tmp_field(il_ji,il_jj)) + &
                (rla_tmp_field(il_ji-1,il_jj+1)-rla_tmp_field(il_ji,il_jj)) + &
                (rla_tmp_field(il_ji+1,il_jj+1)-rla_tmp_field(il_ji,il_jj)) )
!                                                                     
            END IF          
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      rda_filtred_field(1,:) = rda_filtred_field(id_Nxi-1,:) 
      rda_filtred_field(id_Nxi,:) = rda_filtred_field(2,:) 
      ! North-south periodic conditions
      rda_filtred_field(1,1) = 0.e0   
      rda_filtred_field(1,id_Nyj) = 0.e0
      rda_filtred_field(id_Nxi,id_Nyj) = 0.e0
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi, 1 ) = 0.e0
        rda_filtred_field(il_jxi,id_Nyj) = rda_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi,id_Nyj-1) = rda_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      rla_tmp_field(:,:) = rda_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
!      
  END SUBROUTINE SHFO_1ofilt2D_ideal_isot_bfick
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
  SUBROUTINE SHFO_1ofilt2D_ideal_anisot_bfick (rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field,rda_e1t,rda_e2t,rda_e1u, &
 		rda_e2u,rda_e1v,rda_e2v)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field
    IMPLICIT NONE
    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask  
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field,rda_e1t,rda_e2t, &
    							  rda_e1u,rda_e2u,rda_e1v,rda_e2v
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    REAL(kind=8),DIMENSION(id_Nxi,id_Nyj)              :: rla_tmp_field
    REAL(kind=8) 	                               :: rl_smooth_x,rl_smooth_y,rl_coefsmooth_x,rl_coefsmooth_y, &
    							  rl_coefsmooth_diag,rl_coefsmooth_bf,rl_aniso_mx, & 
    							  rl_aniso_py,rl_aniso_my,rl_aniso_xyi,rl_aniso_xyj, &
    							  rl_aniso_px,rl_smooth_x_s2,rl_smooth_y_s2
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt,il_test
!
!
    !! to conserve the original signal, we create its copy   
    rla_tmp_field(:,:) = rda_field(:,:)
    rda_filtred_field(:,:)=rda_field(:,:)
!    
    rl_smooth_x=0.5;rl_smooth_y=0.5
    rl_smooth_x_s2=0.25;rl_smooth_y_s2=0.25
    rl_coefsmooth_diag=(rl_smooth_x*rl_smooth_y)*0.25
    rl_coefsmooth_bf = 0.5*0.25 ! coeff=S/4 with S=1/2
!
    !! Filter application's loop   
    DO il_np = 1,id_npShap
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN 
!	    
            !! Anisitrpic weight process
            rl_aniso_px= ((rda_e1t(il_ji+1,il_jj))/(rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj))
 	    rl_aniso_mx= -((rda_e1t(il_ji-1,il_jj))/(rda_e1t(il_ji-1,il_jj)+rda_e1t(il_ji,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)) 	     	       	                  
 	    rl_aniso_py= ((rda_e2t(il_ji,il_jj+1))/(rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
              (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj))
 	    rl_aniso_my= -((rda_e2t(il_ji,il_jj-1))/(rda_e2t(il_ji,il_jj-1)+rda_e2t(il_ji,il_jj)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
 	      (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)) 	      
 	    rl_aniso_xyi=rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)-0.5*(((rda_e1t(il_ji,il_jj))/ &
 	      (rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))-((rda_e1t(il_ji,il_jj))/(rda_e1t(il_ji-1,il_jj)+ &
 	      rda_e1t(il_ji,il_jj))))*((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))- &
 	      (rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj)))
 	    rl_aniso_xyj=rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)-0.5*(((rda_e2t(il_ji,il_jj))/ &
 	      (rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))-((rda_e2t(il_ji,il_jj))/(rda_e2t(il_ji,il_jj-1)+ &
 	      rda_e2t(il_ji,il_jj))))*((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))- &
 	      (rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1)))
!          
            il_test = ida_mask(il_ji-1,il_jj)*ida_mask(il_ji+1,il_jj)* &
              ida_mask(il_ji,il_jj-1)*ida_mask(il_ji,il_jj+1)*ida_mask(il_ji-1,il_jj-1)* &
              ida_mask(il_ji-1,il_jj+1)*ida_mask(il_ji+1,il_jj-1)*ida_mask(il_ji+1,il_jj+1)
            IF (il_test.EQ.0) THEN
!
 	    !! filtering cell process 	   
            rda_filtred_field(il_ji,il_jj) = rla_tmp_field(il_ji,il_jj) + &
              rl_coefsmooth_bf * &
              ( (rl_aniso_mx*rla_tmp_field(il_ji-1,il_jj)-rl_aniso_xyi*rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji-1,il_jj) + &
                (rl_aniso_px*rla_tmp_field(il_ji+1,il_jj)-rl_aniso_xyi*rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji+1,il_jj) + & 
                (rl_aniso_my*rla_tmp_field(il_ji,il_jj-1)-rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj-1) + &
                (rl_aniso_py*rla_tmp_field(il_ji,il_jj+1)-rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj+1) )
!                
            ELSE

	    rl_coefsmooth_x = (rl_smooth_x_s2)*(1.-rl_smooth_y*rl_aniso_xyj)
	    rl_coefsmooth_y = (rl_smooth_y_s2)*(1.-rl_smooth_x*rl_aniso_xyi)
!
            !! filtering cell process    
	    rda_filtred_field(il_ji,il_jj) = rla_tmp_field(il_ji,il_jj) + &
              rl_coefsmooth_y * &
              ( (rl_aniso_my*rla_tmp_field(il_ji,il_jj-1)-rl_aniso_xyj*rla_tmp_field(il_ji,il_jj)) + &
                (rl_aniso_py*rla_tmp_field(il_ji,il_jj+1)-rl_aniso_xyj*rla_tmp_field(il_ji,il_jj)) ) + &              
	      rl_coefsmooth_x * &
	      ( (rl_aniso_mx*rla_tmp_field(il_ji-1,il_jj)-rl_aniso_xyi*rla_tmp_field(il_ji,il_jj)) + &
                (rl_aniso_px*rla_tmp_field(il_ji+1,il_jj)-rl_aniso_xyi*rla_tmp_field(il_ji,il_jj)) ) + &               
              rl_coefsmooth_diag * &
              ( (rl_aniso_mx*rl_aniso_my*rla_tmp_field(il_ji-1,il_jj-1) - &
                   rl_aniso_xyi*rl_aniso_xyj*rla_tmp_field(il_ji,il_jj)) + &
                (rl_aniso_px*rl_aniso_my*rla_tmp_field(il_ji+1,il_jj-1) - &
                   rl_aniso_xyi*rl_aniso_xyj*rla_tmp_field(il_ji,il_jj)) + &
                (rl_aniso_mx*rl_aniso_py*rla_tmp_field(il_ji-1,il_jj+1) - &
                   rl_aniso_xyi*rl_aniso_xyj*rla_tmp_field(il_ji,il_jj)) + &
                (rl_aniso_px*rl_aniso_py*rla_tmp_field(il_ji+1,il_jj+1) - &
                   rl_aniso_xyi*rl_aniso_xyj*rla_tmp_field(il_ji,il_jj)) )
!
            END IF          
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      rda_filtred_field(1,:) = rda_filtred_field(id_Nxi-1,:) 
      rda_filtred_field(id_Nxi,:) = rda_filtred_field(2,:) 
      ! North-south periodic conditions
      rda_filtred_field(1,1) = 0.e0   
      rda_filtred_field(1,id_Nyj) = 0.e0
      rda_filtred_field(id_Nxi,id_Nyj) = 0.e0
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi, 1 ) = 0.e0
        rda_filtred_field(il_jxi,id_Nyj) = rda_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi,id_Nyj-1) = rda_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      rla_tmp_field(:,:) = rda_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
!      
  END SUBROUTINE SHFO_1ofilt2D_ideal_anisot_bfick
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
!! 7)      
  SUBROUTINE SHFO_2mofilt2D_ideal_isot(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field

    IMPLICIT NONE

    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask   
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    REAL(kind=8),DIMENSION(id_Nxi,id_Nyj)              :: rla_tmp_field
    REAL(kind=8) 	                               :: rl_smooth,rl_coefsmooth, &
    							  rl_coefsmooth_diag,rl_equi_smooth
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
    !! to conserve the original signal, we create its copy   
    rla_tmp_field(:,:) = rda_field(:,:) 
    rda_filtred_field(:,:)=rda_field(:,:)    
    !! The equivalent smoothing element in 2nd order
    rl_equi_smooth=0.5d0
!    
    !! Filter application's loop   
    DO il_np = 1,2*id_npShap
      !! The equivalency between the second order      
      IF(mod(il_np,2).EQ.1) rl_smooth = rl_equi_smooth
      IF(mod(il_np,2).EQ.0) rl_smooth =-rl_equi_smooth
      rl_coefsmooth = rl_smooth*0.5
      rl_coefsmooth_diag = (rl_smooth*rl_smooth)*0.25  
!      
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN                                                                         
!	    
            !! filtering cell process
	    rda_filtred_field(il_ji,il_jj) = rla_tmp_field(il_ji,il_jj) + &
              rl_coefsmooth * &            
              ( (rla_tmp_field(il_ji,il_jj-1)-rla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj-1) + &
                (rla_tmp_field(il_ji,il_jj+1)-rla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj+1) + &           
	        (rla_tmp_field(il_ji-1,il_jj)-rla_tmp_field(il_ji,il_jj))* &
	           ida_mask(il_ji-1,il_jj) + &
                (rla_tmp_field(il_ji+1,il_jj)-rla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji+1,il_jj) ) + &               
              rl_coefsmooth_diag * &
              ( rla_tmp_field(il_ji-1,il_jj-1)* &
                  ida_mask(il_ji-1,il_jj-1)*ida_mask(il_ji,il_jj-1) + &
                rla_tmp_field(il_ji+1,il_jj-1)* &
                  ida_mask(il_ji+1,il_jj-1)*ida_mask(il_ji,il_jj-1) + &
                rla_tmp_field(il_ji-1,il_jj+1)* &
                  ida_mask(il_ji-1,il_jj+1)*ida_mask(il_ji,il_jj+1) + &
                rla_tmp_field(il_ji+1,il_jj+1)* &
                  ida_mask(il_ji+1,il_jj+1)*ida_mask(il_ji,il_jj+1) - &
                rla_tmp_field(il_ji,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*(ida_mask(il_ji-1,il_jj-1)+ida_mask(il_ji+1,il_jj-1)) - &
                rla_tmp_field(il_ji,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*(ida_mask(il_ji-1,il_jj+1)+ida_mask(il_ji+1,il_jj+1)) - &
                rla_tmp_field(il_ji-1,il_jj)* &
                  ida_mask(il_ji-1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) - &
                rla_tmp_field(il_ji+1,il_jj)* &
                  ida_mask(il_ji+1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) + &
                rla_tmp_field(il_ji,il_jj)* &
                (ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1))* &
                (ida_mask(il_ji-1,il_jj)+ida_mask(il_ji+1,il_jj)) )
!
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      rda_filtred_field(1,:) = rda_filtred_field(id_Nxi-1,:) 
      rda_filtred_field(id_Nxi,:) = rda_filtred_field(2,:) 
      ! North-south periodic conditions
      rda_filtred_field(1,1) = 0.e0   
      rda_filtred_field(1,id_Nyj) = 0.e0
      rda_filtred_field(id_Nxi,id_Nyj) = 0.e0
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi, 1 ) = 0.e0
        rda_filtred_field(il_jxi,id_Nyj) = rda_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi,id_Nyj-1) = rda_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      rla_tmp_field(:,:) = rda_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
!      
  END SUBROUTINE SHFO_2mofilt2D_ideal_isot
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
  SUBROUTINE SHFO_2mofilt2D_ideal_anisot(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field,rda_e1t,rda_e2t,rda_e1u, &
 		rda_e2u,rda_e1v,rda_e2v)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field

    IMPLICIT NONE

    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask  
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field,rda_e1t,rda_e2t, &
    							  rda_e1u,rda_e2u,rda_e1v,rda_e2v
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    REAL(kind=8),DIMENSION(id_Nxi,id_Nyj)              :: rla_tmp_field
    REAL(kind=8) 	                               :: rl_smooth,rl_coefsmooth, &
    							  rl_coefsmooth_diag,rl_aniso_px,rl_aniso_mx, & 
    							  rl_aniso_py,rl_aniso_my,rl_aniso_xyi,rl_aniso_xyj,&
    							  rl_equi_smooth
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt				
!
!
    !! to conserve the original signal, we create its copy   
    rla_tmp_field(:,:) = rda_field(:,:)
    rda_filtred_field(:,:)=rda_field(:,:)
    !! The equivalent smoothing element in 2nd order
    rl_equi_smooth=0.5d0
!    
    !! Filter application's loop   
    DO il_np = 1,id_npShap
      !! The equivalency between the second order      
      IF(mod(il_np,2).EQ.1) rl_smooth = rl_equi_smooth
      IF(mod(il_np,2).EQ.0) rl_smooth =-rl_equi_smooth
      rl_coefsmooth = rl_smooth*0.5d0
      rl_coefsmooth_diag = (rl_smooth*rl_smooth)*0.25d0
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN                                                                  
!	    
            !! Anisitrpic weight process
            rl_aniso_px= ((rda_e1t(il_ji+1,il_jj))/(rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj))
 	    rl_aniso_mx= -((rda_e1t(il_ji-1,il_jj))/(rda_e1t(il_ji-1,il_jj)+rda_e1t(il_ji,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)) 	     	       	                  
 	    rl_aniso_py= ((rda_e2t(il_ji,il_jj+1))/(rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
              (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj))
 	    rl_aniso_my= -((rda_e2t(il_ji,il_jj-1))/(rda_e2t(il_ji,il_jj-1)+rda_e2t(il_ji,il_jj)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
 	      (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)) 	      
 	    rl_aniso_xyi=rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)-0.5*(((rda_e1t(il_ji,il_jj))/ &
 	      (rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))-((rda_e1t(il_ji,il_jj))/(rda_e1t(il_ji-1,il_jj)+ &
 	      rda_e1t(il_ji,il_jj))))*((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))- &
 	      (rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj)))
 	    rl_aniso_xyj=rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)-0.5*(((rda_e2t(il_ji,il_jj))/ &
 	      (rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))-((rda_e2t(il_ji,il_jj))/(rda_e2t(il_ji,il_jj-1)+ &
 	      rda_e2t(il_ji,il_jj))))*((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))- &
 	      (rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1)))
! 	      
            !! filtering cell process	        
            rda_filtred_field(il_ji,il_jj)= rla_tmp_field(il_ji,il_jj) + &
              rl_coefsmooth * &
              ( (rl_aniso_my*rla_tmp_field(il_ji,il_jj-1)-rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj-1) + &
                (rl_aniso_py*rla_tmp_field(il_ji,il_jj+1)-rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj+1) + &              	   
	        (rl_aniso_mx*rla_tmp_field(il_ji-1,il_jj)-rl_aniso_xyi*rla_tmp_field(il_ji,il_jj))* &
	           ida_mask(il_ji-1,il_jj) + &
                (rl_aniso_px*rla_tmp_field(il_ji+1,il_jj)-rl_aniso_xyi*rla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji+1,il_jj) ) + &               
              rl_coefsmooth_diag * &
              ( rl_aniso_mx*rl_aniso_my*rla_tmp_field(il_ji-1,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*ida_mask(il_ji-1,il_jj-1) + &
                rl_aniso_px*rl_aniso_my*rla_tmp_field(il_ji+1,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*ida_mask(il_ji+1,il_jj-1) + &
                rl_aniso_mx*rl_aniso_py*rla_tmp_field(il_ji-1,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*ida_mask(il_ji-1,il_jj+1) + &
                rl_aniso_px*rl_aniso_py*rla_tmp_field(il_ji+1,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*ida_mask(il_ji+1,il_jj+1) - &                
                rl_aniso_my*rla_tmp_field(il_ji,il_jj-1)*rl_aniso_xyi* &
                  ida_mask(il_ji,il_jj-1)*(ida_mask(il_ji-1,il_jj-1)+ida_mask(il_ji+1,il_jj-1)) - &
                rl_aniso_py*rla_tmp_field(il_ji,il_jj+1)*rl_aniso_xyi* &
                  ida_mask(il_ji,il_jj+1)*(ida_mask(il_ji-1,il_jj+1)+ida_mask(il_ji+1,il_jj+1)) - &
                rl_aniso_mx*rla_tmp_field(il_ji-1,il_jj)*rl_aniso_xyj* &
                  ida_mask(il_ji-1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) - &
                rl_aniso_px*rla_tmp_field(il_ji+1,il_jj)*rl_aniso_xyj* &
                  ida_mask(il_ji+1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) + &
                rla_tmp_field(il_ji,il_jj)*rl_aniso_xyi*rl_aniso_xyj* &
                (ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1))* &
                (ida_mask(il_ji-1,il_jj)+ida_mask(il_ji+1,il_jj)) )
!           
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      rda_filtred_field(1,:) = rda_filtred_field(id_Nxi-1,:) 
      rda_filtred_field(id_Nxi,:) = rda_filtred_field(2,:) 
      ! North-south periodic conditions
      rda_filtred_field(1,1) = 0.0d0   
      rda_filtred_field(1,id_Nyj) = 0.0d0
      rda_filtred_field(id_Nxi,id_Nyj) = 0.0d0
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi, 1 ) = 0.0d0
        rda_filtred_field(il_jxi,id_Nyj) = rda_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi,id_Nyj-1) = rda_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      rla_tmp_field(:,:) = rda_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
!      
  END SUBROUTINE SHFO_2mofilt2D_ideal_anisot
!!******************************************************************************
!!******************************************************************************
!!******************************************************************************
!
!! 9)
  SUBROUTINE SHFO_2mofilt2D_ideal_isot_bfick (rda_field,id_npShap,id_Nxi,id_Nyj,ida_mask,rda_filtred_field)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field
    IMPLICIT NONE
    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask  
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    REAL(kind=8),DIMENSION(id_Nxi,id_Nyj)              :: rla_tmp_field
    REAL(kind=8) 	                               :: rl_smooth,rl_coefsmooth,rl_coefsmooth_diag,rl_coefsmooth_bf
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt,il_test,i
!
!
    !! to conserve the original signal, we create its copy   
    rla_tmp_field(:,:) = rda_field(:,:)
    rda_filtred_field(:,:)=rda_field(:,:)
!    
    rl_coefsmooth_bf = 0.5*0.25 ! coeff=S/4 with S=1/2
!
    !! Filter application's loop   
    DO il_np = 1,id_npShap
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN 
            il_test = ida_mask(il_ji-1,il_jj)*ida_mask(il_ji+1,il_jj)* &
              ida_mask(il_ji,il_jj-1)*ida_mask(il_ji,il_jj+1)*ida_mask(il_ji-1,il_jj-1)* &
              ida_mask(il_ji-1,il_jj+1)*ida_mask(il_ji+1,il_jj-1)*ida_mask(il_ji+1,il_jj+1)
            IF (il_test.EQ.0) THEN
!          
              !! Fick filtering cell process on ocean boards
              rda_filtred_field(il_ji,il_jj) = rla_tmp_field(il_ji,il_jj) + &
                rl_coefsmooth_bf * &
                ( (rla_tmp_field(il_ji-1,il_jj)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji-1,il_jj) + &
                  (rla_tmp_field(il_ji+1,il_jj)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji+1,il_jj) + & 
                  (rla_tmp_field(il_ji,il_jj-1)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj-1) + &
                  (rla_tmp_field(il_ji,il_jj+1)-rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj+1) )			
!                
            ELSE
              DO i=2,3
              !! The equivalency between the second order      
              IF(mod(i,2).EQ.1) rl_smooth = 0.5
              IF(mod(i,2).EQ.0) rl_smooth =-0.5
              rl_coefsmooth = rl_smooth*0.5*(1.-rl_smooth)        
              rl_coefsmooth_diag = (rl_smooth*rl_smooth)*0.25                                                         
!	    
              !! filtering cell process
	      rda_filtred_field(il_ji,il_jj) = rla_tmp_field(il_ji,il_jj) + &
                rl_coefsmooth * &            
                ( rla_tmp_field(il_ji,il_jj-1)+rla_tmp_field(il_ji,il_jj+1) + &          
                  rla_tmp_field(il_ji-1,il_jj)+rla_tmp_field(il_ji+1,il_jj) - &
	          4.*rla_tmp_field(il_ji,il_jj) ) + &               
                rl_coefsmooth_diag * &
                ( rla_tmp_field(il_ji-1,il_jj-1) + &
                  rla_tmp_field(il_ji+1,il_jj-1) + &
                  rla_tmp_field(il_ji-1,il_jj+1) + &
                  rla_tmp_field(il_ji+1,il_jj+1) - &
                  4.*rla_tmp_field(il_ji,il_jj) )
!           
	      END DO
            END IF          
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      rda_filtred_field(1,:) = rda_filtred_field(id_Nxi-1,:) 
      rda_filtred_field(id_Nxi,:) = rda_filtred_field(2,:) 
      ! North-south periodic conditions
      rda_filtred_field(1,1) = 0.e0   
      rda_filtred_field(1,id_Nyj) = 0.e0
      rda_filtred_field(id_Nxi,id_Nyj) = 0.e0
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi, 1 ) = 0.e0
        rda_filtred_field(il_jxi,id_Nyj) = rda_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi,id_Nyj-1) = rda_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      rla_tmp_field(:,:) = rda_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
!      
  END SUBROUTINE SHFO_2mofilt2D_ideal_isot_bfick
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
!! 10)                 
  SUBROUTINE SHFO_2mofilt2D_ideal_anisot_bfick (rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field,rda_e1t,rda_e2t,rda_e1u, &
 		rda_e2u,rda_e1v,rda_e2v)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field
    IMPLICIT NONE
    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask  
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field,rda_e1t,rda_e2t, &
    							  rda_e1u,rda_e2u,rda_e1v,rda_e2v
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    REAL(kind=8),DIMENSION(id_Nxi,id_Nyj)              :: rla_tmp_field
    REAL(kind=8) 	                               :: rl_smooth,rl_coefsmooth, &
    							  rl_coefsmooth_diag,rl_coefsmooth_bf,rl_aniso_mx, & 
    							  rl_aniso_py,rl_aniso_my,rl_aniso_xyi,rl_aniso_xyj, &
    							  rl_aniso_px
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt,il_test
!
!
    !! to conserve the original signal, we create its copy   
    rla_tmp_field(:,:) = rda_field(:,:)
    rda_filtred_field(:,:)=rda_field(:,:)
!    
    rl_coefsmooth_bf = 0.5*0.25 ! coeff=S/4 with S=1/2
!
    !! Filter application's loop   
    DO il_np = 1,id_npShap
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN 
!	    
            !! Anisitrpic weight process
            rl_aniso_px= ((rda_e1t(il_ji+1,il_jj))/(rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj))
 	    rl_aniso_mx= -((rda_e1t(il_ji-1,il_jj))/(rda_e1t(il_ji-1,il_jj)+rda_e1t(il_ji,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)) 	     	       	                  
 	    rl_aniso_py= ((rda_e2t(il_ji,il_jj+1))/(rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
              (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj))
 	    rl_aniso_my= -((rda_e2t(il_ji,il_jj-1))/(rda_e2t(il_ji,il_jj-1)+rda_e2t(il_ji,il_jj)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
 	      (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)) 	      
 	    rl_aniso_xyi=rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)-0.5*(((rda_e1t(il_ji,il_jj))/ &
 	      (rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))-((rda_e1t(il_ji,il_jj))/(rda_e1t(il_ji-1,il_jj)+ &
 	      rda_e1t(il_ji,il_jj))))*((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))- &
 	      (rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj)))
 	    rl_aniso_xyj=rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)-0.5*(((rda_e2t(il_ji,il_jj))/ &
 	      (rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))-((rda_e2t(il_ji,il_jj))/(rda_e2t(il_ji,il_jj-1)+ &
 	      rda_e2t(il_ji,il_jj))))*((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))- &
 	      (rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1)))
!
            il_test = ida_mask(il_ji-1,il_jj)*ida_mask(il_ji+1,il_jj)* &
              ida_mask(il_ji,il_jj-1)*ida_mask(il_ji,il_jj+1)*ida_mask(il_ji-1,il_jj-1)* &
              ida_mask(il_ji-1,il_jj+1)*ida_mask(il_ji+1,il_jj-1)*ida_mask(il_ji+1,il_jj+1)
            IF (il_test.EQ.0) THEN
!
 	    !! filtering cell process 	   
            rda_filtred_field(il_ji,il_jj) = rla_tmp_field(il_ji,il_jj) + &
              rl_coefsmooth_bf * &
              ( (rl_aniso_mx*rla_tmp_field(il_ji-1,il_jj)-rl_aniso_xyi*rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji-1,il_jj) + &
                (rl_aniso_px*rla_tmp_field(il_ji+1,il_jj)-rl_aniso_xyi*rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji+1,il_jj) + & 
                (rl_aniso_my*rla_tmp_field(il_ji,il_jj-1)-rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj-1) + &
                (rl_aniso_py*rla_tmp_field(il_ji,il_jj+1)-rl_aniso_xyj*rla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj+1) )
!                            
            ELSE
              !! The equivalency between the second order      
              IF(mod(il_np,2).EQ.1) rl_smooth = 0.5
              IF(mod(il_np,2).EQ.0) rl_smooth =-0.5
              rl_coefsmooth = rl_smooth*0.5
              rl_coefsmooth_diag = (rl_smooth*rl_smooth)*0.25                                                          
! 	      
            !! filtering cell process	        
            rda_filtred_field(il_ji,il_jj)= rla_tmp_field(il_ji,il_jj) + &
              rl_coefsmooth * &
              ( (rl_aniso_my*rla_tmp_field(il_ji,il_jj-1)-rl_aniso_xyj*rla_tmp_field(il_ji,il_jj)) + &
                (rl_aniso_py*rla_tmp_field(il_ji,il_jj+1)-rl_aniso_xyj*rla_tmp_field(il_ji,il_jj)) + &          
	        (rl_aniso_mx*rla_tmp_field(il_ji-1,il_jj)-rl_aniso_xyi*rla_tmp_field(il_ji,il_jj)) + &
                (rl_aniso_px*rla_tmp_field(il_ji+1,il_jj)-rl_aniso_xyi*rla_tmp_field(il_ji,il_jj)) ) + &
              rl_coefsmooth_diag * &
              ( rl_aniso_mx*rl_aniso_my*rla_tmp_field(il_ji-1,il_jj-1) + &
                rl_aniso_px*rl_aniso_my*rla_tmp_field(il_ji+1,il_jj-1) + &
                rl_aniso_mx*rl_aniso_py*rla_tmp_field(il_ji-1,il_jj+1) + &
                rl_aniso_px*rl_aniso_py*rla_tmp_field(il_ji+1,il_jj+1) - 2.*( &
                rl_aniso_my*rla_tmp_field(il_ji,il_jj-1)*rl_aniso_xyi + &
                rl_aniso_py*rla_tmp_field(il_ji,il_jj+1)*rl_aniso_xyi + &
                rl_aniso_mx*rla_tmp_field(il_ji-1,il_jj)*rl_aniso_xyj + &
                rl_aniso_px*rla_tmp_field(il_ji+1,il_jj)*rl_aniso_xyj - &
                rla_tmp_field(il_ji,il_jj)*rl_aniso_xyi*rl_aniso_xyj*2. ) )
!	    
            END IF          
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      rda_filtred_field(1,:) = rda_filtred_field(id_Nxi-1,:) 
      rda_filtred_field(id_Nxi,:) = rda_filtred_field(2,:) 
      ! North-south periodic conditions
      rda_filtred_field(1,1) = 0.e0   
      rda_filtred_field(1,id_Nyj) = 0.e0
      rda_filtred_field(id_Nxi,id_Nyj) = 0.e0
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi, 1 ) = 0.e0
        rda_filtred_field(il_jxi,id_Nyj) = rda_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        rda_filtred_field(il_jxi,id_Nyj-1) = rda_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      rla_tmp_field(:,:) = rda_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
!      
  END SUBROUTINE SHFO_2mofilt2D_ideal_anisot_bfick
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
!! 8)
  SUBROUTINE SHFO_3mofilt2D_ideal_isot(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field

    IMPLICIT NONE

    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask   
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_field
    COMPLEX(kind=8) 	                               :: cl_smooth,cl_coefsmooth, &
    							  cl_coefsmooth_diag
    REAL(kind=8)                                       :: rl_equi_smooth
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
    !! to conserve the original signal, we create its copy   
    cla_tmp_field(:,:) = rda_field(:,:) 
    cla_tmp_filtred_field(:,:)=rda_field(:,:)    
    !! The equivalent smoothing element in 3rd order
    rl_equi_smooth=0.5d0
!    
    !! Filter application's loop   
    DO il_np = 1,3*id_npShap
      !! The equivalency between the second order      
      IF(mod(il_np,3).EQ.0) cl_smooth = rl_equi_smooth*dcmplx(0.5d0,-0.5d0*dsqrt(3.0d0))               
      IF(mod(il_np,3).EQ.1) cl_smooth = rl_equi_smooth*dcmplx(0.5d0, 0.5d0*dsqrt(3.0d0))                       
      IF(mod(il_np,3).EQ.2) cl_smooth = rl_equi_smooth*dcmplx(1.0d0,0.0d0)               
      cl_coefsmooth = cl_smooth*0.5d0
      cl_coefsmooth_diag = (cl_smooth*cl_smooth)*0.25d0
!      
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN                                                                         
!	    
            !! filtering cell process
	    cla_tmp_filtred_field(il_ji,il_jj) = cla_tmp_field(il_ji,il_jj) + &
              cl_coefsmooth * &            
              ( (cla_tmp_field(il_ji,il_jj-1)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj-1) + &
                (cla_tmp_field(il_ji,il_jj+1)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj+1) + &           
	        (cla_tmp_field(il_ji-1,il_jj)-cla_tmp_field(il_ji,il_jj))* &
	           ida_mask(il_ji-1,il_jj) + &
                (cla_tmp_field(il_ji+1,il_jj)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji+1,il_jj) ) + &               
              cl_coefsmooth_diag * &
              ( cla_tmp_field(il_ji-1,il_jj-1)* &
                  ida_mask(il_ji-1,il_jj-1)*ida_mask(il_ji,il_jj-1) + &
                cla_tmp_field(il_ji+1,il_jj-1)* &
                  ida_mask(il_ji+1,il_jj-1)*ida_mask(il_ji,il_jj-1) + &
                cla_tmp_field(il_ji-1,il_jj+1)* &
                  ida_mask(il_ji-1,il_jj+1)*ida_mask(il_ji,il_jj+1) + &
                cla_tmp_field(il_ji+1,il_jj+1)* &
                  ida_mask(il_ji+1,il_jj+1)*ida_mask(il_ji,il_jj+1) - &
                cla_tmp_field(il_ji,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*(ida_mask(il_ji-1,il_jj-1)+ida_mask(il_ji+1,il_jj-1)) - &
                cla_tmp_field(il_ji,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*(ida_mask(il_ji-1,il_jj+1)+ida_mask(il_ji+1,il_jj+1)) - &
                cla_tmp_field(il_ji-1,il_jj)* &
                  ida_mask(il_ji-1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) - &
                cla_tmp_field(il_ji+1,il_jj)* &
                  ida_mask(il_ji+1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) + &
                cla_tmp_field(il_ji,il_jj)* &
                (ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1))* &
                (ida_mask(il_ji-1,il_jj)+ida_mask(il_ji+1,il_jj)) )
!
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      cla_tmp_filtred_field(1,:) = cla_tmp_filtred_field(id_Nxi-1,:) 
      cla_tmp_filtred_field(id_Nxi,:) = cla_tmp_filtred_field(2,:) 
      ! North-south periodic conditions
      cla_tmp_filtred_field(1,1) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(1,id_Nyj) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(id_Nxi,id_Nyj) = dcmplx(0.0d0,0.0d0)
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi, 1 ) = dcmplx(0.0d0,0.0d0)
        cla_tmp_filtred_field(il_jxi,id_Nyj) = cla_tmp_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi,id_Nyj-1) = cla_tmp_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      cla_tmp_field(:,:) = cla_tmp_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
    rda_filtred_field=dreal(cla_tmp_filtred_field)
!    
    write(*,*),
    write(*,*),'imaginary part : ',MAXVAL(ABS(dimag(cla_tmp_filtred_field)))
    write(*,*),
!      
  END SUBROUTINE SHFO_3mofilt2D_ideal_isot
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
  SUBROUTINE SHFO_3mofilt2D_ideal_anisot(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field,rda_e1t,rda_e2t,rda_e1u, &
 		rda_e2u,rda_e1v,rda_e2v)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field

    IMPLICIT NONE

    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask   
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field,rda_e1t,rda_e2t, &
    							  rda_e1u,rda_e2u,rda_e1v,rda_e2v
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_field
    COMPLEX(kind=8) 	                               :: cl_smooth,cl_coefsmooth, &
    							  cl_coefsmooth_diag
    REAL(kind=8)                                       :: rl_equi_smooth,rl_aniso_px,rl_aniso_mx, & 
    							  rl_aniso_py,rl_aniso_my,rl_aniso_xyi,rl_aniso_xyj
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
    !! to conserve the original signal, we create its copy   
    cla_tmp_field(:,:) = rda_field(:,:) 
    cla_tmp_filtred_field(:,:)=rda_field(:,:)    
    !! The equivalent smoothing element in 3rd order
    rl_equi_smooth=0.5d0
!    
    !! Filter application's loop   
    DO il_np = 1,3*id_npShap    
      !! The equivalency between the second order      
      IF(mod(il_np,3).EQ.0) cl_smooth = rl_equi_smooth*dcmplx(0.5d0,-0.5d0*dsqrt(3.0d0))               
      IF(mod(il_np,3).EQ.1) cl_smooth = rl_equi_smooth*dcmplx(0.5d0, 0.5d0*dsqrt(3.0d0))                       
      IF(mod(il_np,3).EQ.2) cl_smooth = rl_equi_smooth*dcmplx(1.0d0,0.0d0)            
      cl_coefsmooth = cl_smooth*0.5d0
      cl_coefsmooth_diag = (cl_smooth*cl_smooth)*0.25d0
!      
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN  
!	    
            !! Anisitrpic weight process
            rl_aniso_px= ((rda_e1t(il_ji+1,il_jj))/(rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj))
 	    rl_aniso_mx= -((rda_e1t(il_ji-1,il_jj))/(rda_e1t(il_ji-1,il_jj)+rda_e1t(il_ji,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)) 	     	       	                  
 	    rl_aniso_py= ((rda_e2t(il_ji,il_jj+1))/(rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
              (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj))
 	    rl_aniso_my= -((rda_e2t(il_ji,il_jj-1))/(rda_e2t(il_ji,il_jj-1)+rda_e2t(il_ji,il_jj)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
 	      (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)) 	      
 	    rl_aniso_xyi=rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)-0.5*(((rda_e1t(il_ji,il_jj))/ &
 	      (rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))-((rda_e1t(il_ji,il_jj))/(rda_e1t(il_ji-1,il_jj)+ &
 	      rda_e1t(il_ji,il_jj))))*((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))- &
 	      (rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj)))
 	    rl_aniso_xyj=rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)-0.5*(((rda_e2t(il_ji,il_jj))/ &
 	      (rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))-((rda_e2t(il_ji,il_jj))/(rda_e2t(il_ji,il_jj-1)+ &
 	      rda_e2t(il_ji,il_jj))))*((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))- &
 	      (rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1)))
! 	      
            !! filtering cell process	        
            cla_tmp_filtred_field(il_ji,il_jj)= cla_tmp_field(il_ji,il_jj) + &
              cl_coefsmooth * &
              ( (rl_aniso_my*cla_tmp_field(il_ji,il_jj-1)-rl_aniso_xyj*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj-1) + &
                (rl_aniso_py*cla_tmp_field(il_ji,il_jj+1)-rl_aniso_xyj*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj+1) + &              	   
	        (rl_aniso_mx*cla_tmp_field(il_ji-1,il_jj)-rl_aniso_xyi*cla_tmp_field(il_ji,il_jj))* &
	           ida_mask(il_ji-1,il_jj) + &
                (rl_aniso_px*cla_tmp_field(il_ji+1,il_jj)-rl_aniso_xyi*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji+1,il_jj) ) + &               
              cl_coefsmooth_diag * &
              ( rl_aniso_mx*rl_aniso_my*cla_tmp_field(il_ji-1,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*ida_mask(il_ji-1,il_jj-1) + &
                rl_aniso_px*rl_aniso_my*cla_tmp_field(il_ji+1,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*ida_mask(il_ji+1,il_jj-1) + &
                rl_aniso_mx*rl_aniso_py*cla_tmp_field(il_ji-1,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*ida_mask(il_ji-1,il_jj+1) + &
                rl_aniso_px*rl_aniso_py*cla_tmp_field(il_ji+1,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*ida_mask(il_ji+1,il_jj+1) - &                
                rl_aniso_my*cla_tmp_field(il_ji,il_jj-1)*rl_aniso_xyi* &
                  ida_mask(il_ji,il_jj-1)*(ida_mask(il_ji-1,il_jj-1)+ida_mask(il_ji+1,il_jj-1)) - &
                rl_aniso_py*cla_tmp_field(il_ji,il_jj+1)*rl_aniso_xyi* &
                  ida_mask(il_ji,il_jj+1)*(ida_mask(il_ji-1,il_jj+1)+ida_mask(il_ji+1,il_jj+1)) - &
                rl_aniso_mx*cla_tmp_field(il_ji-1,il_jj)*rl_aniso_xyj* &
                  ida_mask(il_ji-1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) - &
                rl_aniso_px*cla_tmp_field(il_ji+1,il_jj)*rl_aniso_xyj* &
                  ida_mask(il_ji+1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) + &
                cla_tmp_field(il_ji,il_jj)*rl_aniso_xyi*rl_aniso_xyj* &
                (ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1))* &
                (ida_mask(il_ji-1,il_jj)+ida_mask(il_ji+1,il_jj)) )
!                                                                                 
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      cla_tmp_filtred_field(1,:) = cla_tmp_filtred_field(id_Nxi-1,:) 
      cla_tmp_filtred_field(id_Nxi,:) = cla_tmp_filtred_field(2,:) 
      ! North-south periodic conditions
      cla_tmp_filtred_field(1,1) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(1,id_Nyj) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(id_Nxi,id_Nyj) = dcmplx(0.0d0,0.0d0)
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi, 1 ) = dcmplx(0.0d0,0.0d0)
        cla_tmp_filtred_field(il_jxi,id_Nyj) = cla_tmp_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi,id_Nyj-1) = cla_tmp_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      cla_tmp_field(:,:) = cla_tmp_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
    rda_filtred_field=dreal(cla_tmp_filtred_field)
!    
    write(*,*),
    write(*,*),'imaginary part : ',MAXVAL(ABS(dimag(cla_tmp_filtred_field)))
    write(*,*),
!      
  END SUBROUTINE SHFO_3mofilt2D_ideal_anisot
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
  SUBROUTINE SHFO_3mofilt2D_ideal_isot_bfick (rda_field,id_npShap,id_Nxi,id_Nyj,ida_mask,rda_filtred_field)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field
    IMPLICIT NONE
    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask  
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_field
    COMPLEX(kind=8) 	                               :: cl_smooth,cl_coefsmooth, &
    							  cl_coefsmooth_diag    
    REAL(kind=8) 	                               :: rl_coefsmooth_bf,rl_equi_smooth
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt,il_test,i
!
!
    !! to conserve the original signal, we create its copy   
    cla_tmp_field(:,:) = rda_field(:,:)
    rda_filtred_field(:,:)=rda_field(:,:)
!   
    rl_equi_smooth=0.5d0 
    rl_coefsmooth_bf = 0.5*0.25 ! coeff=S/4 with S=1/2
!
    !! Filter application's loop   
    DO il_np = 1,3*id_npShap
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN 
            il_test = ida_mask(il_ji-1,il_jj)*ida_mask(il_ji+1,il_jj)* &
              ida_mask(il_ji,il_jj-1)*ida_mask(il_ji,il_jj+1)*ida_mask(il_ji-1,il_jj-1)* &
              ida_mask(il_ji-1,il_jj+1)*ida_mask(il_ji+1,il_jj-1)*ida_mask(il_ji+1,il_jj+1)
            IF (il_test.EQ.0) THEN
!          
              !! Fick filtering cell process on ocean boards
              cla_tmp_filtred_field(il_ji,il_jj) = cla_tmp_field(il_ji,il_jj) + &
                rl_coefsmooth_bf * &
                ( (cla_tmp_field(il_ji-1,il_jj)-cla_tmp_field(il_ji,il_jj))*ida_mask(il_ji-1,il_jj) + &
                  (cla_tmp_field(il_ji+1,il_jj)-cla_tmp_field(il_ji,il_jj))*ida_mask(il_ji+1,il_jj) + & 
                  (cla_tmp_field(il_ji,il_jj-1)-cla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj-1) + &
                  (cla_tmp_field(il_ji,il_jj+1)-cla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj+1) )			
!                
            ELSE
              !! The equivalency between the second order      
              IF(mod(il_np,3).EQ.0) cl_smooth = rl_equi_smooth*dcmplx(0.5d0,-0.5d0*dsqrt(3.0d0))               
              IF(mod(il_np,3).EQ.1) cl_smooth = rl_equi_smooth*dcmplx(0.5d0, 0.5d0*dsqrt(3.0d0))                       
              IF(mod(il_np,3).EQ.2) cl_smooth = rl_equi_smooth*dcmplx(1.0d0,0.0d0)            
              cl_coefsmooth = cl_smooth*0.5d0
              cl_coefsmooth_diag = (cl_smooth*cl_smooth)*0.25d0                                                                  
!	    
              !! filtering cell process
	      cla_tmp_filtred_field(il_ji,il_jj) = cla_tmp_field(il_ji,il_jj) + &
                cl_coefsmooth * &            
                ( cla_tmp_field(il_ji,il_jj-1)+cla_tmp_field(il_ji,il_jj+1) + &          
                  cla_tmp_field(il_ji-1,il_jj)+cla_tmp_field(il_ji+1,il_jj) - &
	          4.*cla_tmp_field(il_ji,il_jj) ) + &               
                cl_coefsmooth_diag * &
                ( cla_tmp_field(il_ji-1,il_jj-1) + &
                  cla_tmp_field(il_ji+1,il_jj-1) + &
                  cla_tmp_field(il_ji-1,il_jj+1) + &
                  cla_tmp_field(il_ji+1,il_jj+1) - &
                  4.*cla_tmp_field(il_ji,il_jj) )
!           
	
            END IF          
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
     ! Est-West periodic conditions
      cla_tmp_filtred_field(1,:) = cla_tmp_filtred_field(id_Nxi-1,:) 
      cla_tmp_filtred_field(id_Nxi,:) = cla_tmp_filtred_field(2,:) 
      ! North-south periodic conditions
      cla_tmp_filtred_field(1,1) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(1,id_Nyj) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(id_Nxi,id_Nyj) = dcmplx(0.0d0,0.0d0)
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi, 1 ) = dcmplx(0.0d0,0.0d0)
        cla_tmp_filtred_field(il_jxi,id_Nyj) = cla_tmp_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi,id_Nyj-1) = cla_tmp_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      cla_tmp_field(:,:) = cla_tmp_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
  rda_filtred_field=dreal(cla_tmp_filtred_field)
!    
    write(*,*),
    write(*,*),'imaginary part : ',MAXVAL(ABS(dimag(cla_tmp_filtred_field)))
    write(*,*),
!      
  END SUBROUTINE SHFO_3mofilt2D_ideal_isot_bfick
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
!! 10)                 
  SUBROUTINE SHFO_3mofilt2D_ideal_anisot_bfick (rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field,rda_e1t,rda_e2t,rda_e1u, &
 		rda_e2u,rda_e1v,rda_e2v)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field
    IMPLICIT NONE
    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask 
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field,rda_e1t,rda_e2t, &
    							  rda_e1u,rda_e2u,rda_e1v,rda_e2v 
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_field
    COMPLEX(kind=8) 	                               :: cl_smooth,cl_coefsmooth, &
    							  cl_coefsmooth_diag    
    REAL(kind=8) 	                               :: rl_coefsmooth_bf,rl_equi_smooth
    REAL(kind=8)                                       :: rl_aniso_px,rl_aniso_mx, & 
    							  rl_aniso_py,rl_aniso_my,rl_aniso_xyi,rl_aniso_xyj
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt,il_test,i
!
!
    !! to conserve the original signal, we create its copy   
    cla_tmp_field(:,:) = rda_field(:,:)
    rda_filtred_field(:,:)=rda_field(:,:)
!   
    rl_equi_smooth=0.5d0 
    rl_coefsmooth_bf = 0.5*0.25 ! coeff=S/4 with S=1/2
!
    !! Filter application's loop   
    DO il_np = 1,3*id_npShap
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN 
!	    
            !! Anisitrpic weight process
            rl_aniso_px= ((rda_e1t(il_ji+1,il_jj))/(rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj))
 	    rl_aniso_mx= -((rda_e1t(il_ji-1,il_jj))/(rda_e1t(il_ji-1,il_jj)+rda_e1t(il_ji,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)) 	     	       	                  
 	    rl_aniso_py= ((rda_e2t(il_ji,il_jj+1))/(rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
              (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj))
 	    rl_aniso_my= -((rda_e2t(il_ji,il_jj-1))/(rda_e2t(il_ji,il_jj-1)+rda_e2t(il_ji,il_jj)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
 	      (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)) 	      
 	    rl_aniso_xyi=rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)-0.5*(((rda_e1t(il_ji,il_jj))/ &
 	      (rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))-((rda_e1t(il_ji,il_jj))/(rda_e1t(il_ji-1,il_jj)+ &
 	      rda_e1t(il_ji,il_jj))))*((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))- &
 	      (rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj)))
 	    rl_aniso_xyj=rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)-0.5*(((rda_e2t(il_ji,il_jj))/ &
 	      (rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))-((rda_e2t(il_ji,il_jj))/(rda_e2t(il_ji,il_jj-1)+ &
 	      rda_e2t(il_ji,il_jj))))*((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))- &
 	      (rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1)))
!
            il_test = ida_mask(il_ji-1,il_jj)*ida_mask(il_ji+1,il_jj)* &
              ida_mask(il_ji,il_jj-1)*ida_mask(il_ji,il_jj+1)*ida_mask(il_ji-1,il_jj-1)* &
              ida_mask(il_ji-1,il_jj+1)*ida_mask(il_ji+1,il_jj-1)*ida_mask(il_ji+1,il_jj+1)
            IF (il_test.EQ.0) THEN
!
 	    !! filtering cell process 	   
            cla_tmp_filtred_field(il_ji,il_jj) = cla_tmp_field(il_ji,il_jj) + &
              rl_coefsmooth_bf * &
              ( (rl_aniso_mx*cla_tmp_field(il_ji-1,il_jj)-rl_aniso_xyi*cla_tmp_field(il_ji,il_jj))*ida_mask(il_ji-1,il_jj) + &
                (rl_aniso_px*cla_tmp_field(il_ji+1,il_jj)-rl_aniso_xyi*cla_tmp_field(il_ji,il_jj))*ida_mask(il_ji+1,il_jj) + & 
                (rl_aniso_my*cla_tmp_field(il_ji,il_jj-1)-rl_aniso_xyj*cla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj-1) + &
                (rl_aniso_py*cla_tmp_field(il_ji,il_jj+1)-rl_aniso_xyj*cla_tmp_field(il_ji,il_jj))*ida_mask(il_ji,il_jj+1) )
!                            
            ELSE
             !! The equivalency between the second order      
              IF(mod(il_np,3).EQ.0) cl_smooth = rl_equi_smooth*dcmplx(0.5d0,-0.5d0*dsqrt(3.0d0))               
              IF(mod(il_np,3).EQ.1) cl_smooth = rl_equi_smooth*dcmplx(0.5d0, 0.5d0*dsqrt(3.0d0))                       
              IF(mod(il_np,3).EQ.2) cl_smooth = rl_equi_smooth*dcmplx(1.0d0,0.0d0)            
              cl_coefsmooth = cl_smooth*0.5d0
              cl_coefsmooth_diag = (cl_smooth*cl_smooth)*0.25d0                                                          
! 	      
            !! filtering cell process	        
            cla_tmp_filtred_field(il_ji,il_jj)= cla_tmp_field(il_ji,il_jj) + &
              cl_coefsmooth * &
              ( (rl_aniso_my*cla_tmp_field(il_ji,il_jj-1)-rl_aniso_xyj*cla_tmp_field(il_ji,il_jj)) + &
                (rl_aniso_py*cla_tmp_field(il_ji,il_jj+1)-rl_aniso_xyj*cla_tmp_field(il_ji,il_jj)) + &          
	        (rl_aniso_mx*cla_tmp_field(il_ji-1,il_jj)-rl_aniso_xyi*cla_tmp_field(il_ji,il_jj)) + &
                (rl_aniso_px*cla_tmp_field(il_ji+1,il_jj)-rl_aniso_xyi*cla_tmp_field(il_ji,il_jj)) ) + &
              cl_coefsmooth_diag * &
              ( rl_aniso_mx*rl_aniso_my*cla_tmp_field(il_ji-1,il_jj-1) + &
                rl_aniso_px*rl_aniso_my*cla_tmp_field(il_ji+1,il_jj-1) + &
                rl_aniso_mx*rl_aniso_py*cla_tmp_field(il_ji-1,il_jj+1) + &
                rl_aniso_px*rl_aniso_py*cla_tmp_field(il_ji+1,il_jj+1) - 2.*( &
                rl_aniso_my*cla_tmp_field(il_ji,il_jj-1)*rl_aniso_xyi + &
                rl_aniso_py*cla_tmp_field(il_ji,il_jj+1)*rl_aniso_xyi + &
                rl_aniso_mx*cla_tmp_field(il_ji-1,il_jj)*rl_aniso_xyj + &
                rl_aniso_px*cla_tmp_field(il_ji+1,il_jj)*rl_aniso_xyj - &
                cla_tmp_field(il_ji,il_jj)*rl_aniso_xyi*rl_aniso_xyj*2. ) )
!	    
            END IF          
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
     ! Est-West periodic conditions
      cla_tmp_filtred_field(1,:) = cla_tmp_filtred_field(id_Nxi-1,:) 
      cla_tmp_filtred_field(id_Nxi,:) = cla_tmp_filtred_field(2,:) 
      ! North-south periodic conditions
      cla_tmp_filtred_field(1,1) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(1,id_Nyj) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(id_Nxi,id_Nyj) = dcmplx(0.0d0,0.0d0)
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi, 1 ) = dcmplx(0.0d0,0.0d0)
        cla_tmp_filtred_field(il_jxi,id_Nyj) = cla_tmp_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi,id_Nyj-1) = cla_tmp_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      cla_tmp_field(:,:) = cla_tmp_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
  rda_filtred_field=dreal(cla_tmp_filtred_field)
!    
    write(*,*),
    write(*,*),'imaginary part : ',MAXVAL(ABS(dimag(cla_tmp_filtred_field)))
    write(*,*),
!      
  END SUBROUTINE SHFO_3mofilt2D_ideal_anisot_bfick
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
  SUBROUTINE SHFO_4mofilt2D_ideal_isot(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field

    IMPLICIT NONE

    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask   
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_field
    COMPLEX(kind=8) 	                               :: cl_smooth,cl_coefsmooth, &
    							  cl_coefsmooth_diag
    REAL(kind=8)                                       :: rl_equi_smooth
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
    !! to conserve the original signal, we create its copy   
    cla_tmp_field(:,:) = rda_field(:,:) 
    cla_tmp_filtred_field(:,:)=rda_field(:,:)    
    !! The equivalent smoothing element in 3rd order
    rl_equi_smooth=0.5d0
!    
    !! Filter application's loop   
    DO il_np = 1,4*id_npShap
      !! The equivalency between the second order      
      IF(mod(il_np,4).EQ.0) cl_smooth = rl_equi_smooth*dcmplx( 1.0d0, 0.0d0)               
      IF(mod(il_np,4).EQ.3) cl_smooth = rl_equi_smooth*dcmplx(-1.0d0, 0.0d0)                       
      IF(mod(il_np,4).EQ.2) cl_smooth = rl_equi_smooth*dcmplx( 0.0d0, 1.0d0)
      IF(mod(il_np,4).EQ.1) cl_smooth = rl_equi_smooth*dcmplx( 0.0d0,-1.0d0)               
      cl_coefsmooth = cl_smooth*0.5d0
      cl_coefsmooth_diag = (cl_smooth*cl_smooth)*0.25d0
!      
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN                                                                         
!	    
            !! filtering cell process
	    cla_tmp_filtred_field(il_ji,il_jj) = cla_tmp_field(il_ji,il_jj) + &
              cl_coefsmooth * &            
              ( (cla_tmp_field(il_ji,il_jj-1)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj-1) + &
                (cla_tmp_field(il_ji,il_jj+1)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj+1) + &           
	        (cla_tmp_field(il_ji-1,il_jj)-cla_tmp_field(il_ji,il_jj))* &
	           ida_mask(il_ji-1,il_jj) + &
                (cla_tmp_field(il_ji+1,il_jj)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji+1,il_jj) ) + &               
              cl_coefsmooth_diag * &
              ( cla_tmp_field(il_ji-1,il_jj-1)* &
                  ida_mask(il_ji-1,il_jj-1)*ida_mask(il_ji,il_jj-1) + &
                cla_tmp_field(il_ji+1,il_jj-1)* &
                  ida_mask(il_ji+1,il_jj-1)*ida_mask(il_ji,il_jj-1) + &
                cla_tmp_field(il_ji-1,il_jj+1)* &
                  ida_mask(il_ji-1,il_jj+1)*ida_mask(il_ji,il_jj+1) + &
                cla_tmp_field(il_ji+1,il_jj+1)* &
                  ida_mask(il_ji+1,il_jj+1)*ida_mask(il_ji,il_jj+1) - &
                cla_tmp_field(il_ji,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*(ida_mask(il_ji-1,il_jj-1)+ida_mask(il_ji+1,il_jj-1)) - &
                cla_tmp_field(il_ji,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*(ida_mask(il_ji-1,il_jj+1)+ida_mask(il_ji+1,il_jj+1)) - &
                cla_tmp_field(il_ji-1,il_jj)* &
                  ida_mask(il_ji-1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) - &
                cla_tmp_field(il_ji+1,il_jj)* &
                  ida_mask(il_ji+1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) + &
                cla_tmp_field(il_ji,il_jj)* &
                (ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1))* &
                (ida_mask(il_ji-1,il_jj)+ida_mask(il_ji+1,il_jj)) )
!
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      cla_tmp_filtred_field(1,:) = cla_tmp_filtred_field(id_Nxi-1,:) 
      cla_tmp_filtred_field(id_Nxi,:) = cla_tmp_filtred_field(2,:) 
      ! North-south periodic conditions
      cla_tmp_filtred_field(1,1) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(1,id_Nyj) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(id_Nxi,id_Nyj) = dcmplx(0.0d0,0.0d0)
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi, 1 ) = dcmplx(0.0d0,0.0d0)
        cla_tmp_filtred_field(il_jxi,id_Nyj) = cla_tmp_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi,id_Nyj-1) = cla_tmp_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      cla_tmp_field(:,:) = cla_tmp_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
    rda_filtred_field=dreal(cla_tmp_filtred_field)
!    
    write(*,*),
    write(*,*),'imaginary part : ',MAXVAL(ABS(dimag(cla_tmp_filtred_field)))
    write(*,*),
!      
  END SUBROUTINE SHFO_4mofilt2D_ideal_isot
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
  SUBROUTINE SHFO_4mofilt2D_ideal_anisot(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field,rda_e1t,rda_e2t,rda_e1u, &
 		rda_e2u,rda_e1v,rda_e2v)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field

    IMPLICIT NONE

    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask   
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field,rda_e1t,rda_e2t, &
    							  rda_e1u,rda_e2u,rda_e1v,rda_e2v
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_field
    COMPLEX(kind=8) 	                               :: cl_smooth,cl_coefsmooth, &
    							  cl_coefsmooth_diag
    REAL(kind=8)                                       :: rl_equi_smooth,rl_aniso_px,rl_aniso_mx, & 
    							  rl_aniso_py,rl_aniso_my,rl_aniso_xyi,rl_aniso_xyj
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
    !! to conserve the original signal, we create its copy   
    cla_tmp_field(:,:) = rda_field(:,:) 
    cla_tmp_filtred_field(:,:)=rda_field(:,:)    
    !! The equivalent smoothing element in 3rd order
    rl_equi_smooth=0.5d0
!    
    !! Filter application's loop   
    DO il_np = 1,4*id_npShap
      !! The equivalency between the second order      
      IF(mod(il_np,4).EQ.0) cl_smooth = rl_equi_smooth*dcmplx( 1.0d0, 0.0d0)               
      IF(mod(il_np,4).EQ.3) cl_smooth = rl_equi_smooth*dcmplx(-1.0d0, 0.0d0)                       
      IF(mod(il_np,4).EQ.2) cl_smooth = rl_equi_smooth*dcmplx( 0.0d0, 1.0d0)
      IF(mod(il_np,4).EQ.1) cl_smooth = rl_equi_smooth*dcmplx( 0.0d0,-1.0d0)               
      cl_coefsmooth = cl_smooth*0.5d0
      cl_coefsmooth_diag = (cl_smooth*cl_smooth)*0.25d0
!      
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN  
!	    
            !! Anisitrpic weight process
            rl_aniso_px= ((rda_e1t(il_ji+1,il_jj))/(rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj))
 	    rl_aniso_mx= -((rda_e1t(il_ji-1,il_jj))/(rda_e1t(il_ji-1,il_jj)+rda_e1t(il_ji,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)) 	     	       	                  
 	    rl_aniso_py= ((rda_e2t(il_ji,il_jj+1))/(rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
              (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj))
 	    rl_aniso_my= -((rda_e2t(il_ji,il_jj-1))/(rda_e2t(il_ji,il_jj-1)+rda_e2t(il_ji,il_jj)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
 	      (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)) 	      
 	    rl_aniso_xyi=rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)-0.5*(((rda_e1t(il_ji,il_jj))/ &
 	      (rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))-((rda_e1t(il_ji,il_jj))/(rda_e1t(il_ji-1,il_jj)+ &
 	      rda_e1t(il_ji,il_jj))))*((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))- &
 	      (rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj)))
 	    rl_aniso_xyj=rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)-0.5*(((rda_e2t(il_ji,il_jj))/ &
 	      (rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))-((rda_e2t(il_ji,il_jj))/(rda_e2t(il_ji,il_jj-1)+ &
 	      rda_e2t(il_ji,il_jj))))*((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))- &
 	      (rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1)))
! 	      
            !! filtering cell process	        
            cla_tmp_filtred_field(il_ji,il_jj)= cla_tmp_field(il_ji,il_jj) + &
              cl_coefsmooth * &
              ( (rl_aniso_my*cla_tmp_field(il_ji,il_jj-1)-rl_aniso_xyj*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj-1) + &
                (rl_aniso_py*cla_tmp_field(il_ji,il_jj+1)-rl_aniso_xyj*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj+1) + &              	   
	        (rl_aniso_mx*cla_tmp_field(il_ji-1,il_jj)-rl_aniso_xyi*cla_tmp_field(il_ji,il_jj))* &
	           ida_mask(il_ji-1,il_jj) + &
                (rl_aniso_px*cla_tmp_field(il_ji+1,il_jj)-rl_aniso_xyi*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji+1,il_jj) ) + &               
              cl_coefsmooth_diag * &
              ( rl_aniso_mx*rl_aniso_my*cla_tmp_field(il_ji-1,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*ida_mask(il_ji-1,il_jj-1) + &
                rl_aniso_px*rl_aniso_my*cla_tmp_field(il_ji+1,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*ida_mask(il_ji+1,il_jj-1) + &
                rl_aniso_mx*rl_aniso_py*cla_tmp_field(il_ji-1,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*ida_mask(il_ji-1,il_jj+1) + &
                rl_aniso_px*rl_aniso_py*cla_tmp_field(il_ji+1,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*ida_mask(il_ji+1,il_jj+1) - &                
                rl_aniso_my*cla_tmp_field(il_ji,il_jj-1)*rl_aniso_xyi* &
                  ida_mask(il_ji,il_jj-1)*(ida_mask(il_ji-1,il_jj-1)+ida_mask(il_ji+1,il_jj-1)) - &
                rl_aniso_py*cla_tmp_field(il_ji,il_jj+1)*rl_aniso_xyi* &
                  ida_mask(il_ji,il_jj+1)*(ida_mask(il_ji-1,il_jj+1)+ida_mask(il_ji+1,il_jj+1)) - &
                rl_aniso_mx*cla_tmp_field(il_ji-1,il_jj)*rl_aniso_xyj* &
                  ida_mask(il_ji-1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) - &
                rl_aniso_px*cla_tmp_field(il_ji+1,il_jj)*rl_aniso_xyj* &
                  ida_mask(il_ji+1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) + &
                cla_tmp_field(il_ji,il_jj)*rl_aniso_xyi*rl_aniso_xyj* &
                (ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1))* &
                (ida_mask(il_ji-1,il_jj)+ida_mask(il_ji+1,il_jj)) )
!                                                                                 
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      cla_tmp_filtred_field(1,:) = cla_tmp_filtred_field(id_Nxi-1,:) 
      cla_tmp_filtred_field(id_Nxi,:) = cla_tmp_filtred_field(2,:) 
      ! North-south periodic conditions
      cla_tmp_filtred_field(1,1) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(1,id_Nyj) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(id_Nxi,id_Nyj) = dcmplx(0.0d0,0.0d0)
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi, 1 ) = dcmplx(0.0d0,0.0d0)
        cla_tmp_filtred_field(il_jxi,id_Nyj) = cla_tmp_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi,id_Nyj-1) = cla_tmp_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      cla_tmp_field(:,:) = cla_tmp_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
    rda_filtred_field=dreal(cla_tmp_filtred_field)
!    
    write(*,*),
    write(*,*),'imaginary part : ',MAXVAL(ABS(dimag(cla_tmp_filtred_field)))
    write(*,*),
!      
  END SUBROUTINE SHFO_4mofilt2D_ideal_anisot
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
  SUBROUTINE SHFO_5mofilt2D_ideal_isot(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field

    IMPLICIT NONE

    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask   
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_field
    COMPLEX(kind=8) 	                               :: cl_smooth,cl_coefsmooth, &
    							  cl_coefsmooth_diag
    REAL(kind=8)                                       :: rl_equi_smooth
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
    !! to conserve the original signal, we create its copy   
    cla_tmp_field(:,:) = rda_field(:,:) 
    cla_tmp_filtred_field(:,:)=rda_field(:,:)    
    !! The equivalent smoothing element in 3rd order
    rl_equi_smooth=0.5d0
!    
    !! Filter application's loop   
    DO il_np = 1,5*id_npShap
      !! The equivalency between the 5th order   
      IF(mod(il_np,5).EQ.0) cl_smooth = rl_equi_smooth*dcmplx(0.870550576184379,0.0d0)
      IF(mod(il_np,5).EQ.1) cl_smooth = rl_equi_smooth*dcmplx(0.2690149185211857, 0.8279427859871953)	   
      IF(mod(il_np,5).EQ.2) cl_smooth = rl_equi_smooth*dcmplx(0.2690149185211857,-0.8279427859871953)
      IF(mod(il_np,5).EQ.3) cl_smooth = rl_equi_smooth*dcmplx(-0.7042902001692473, 0.5116967824803674)
      IF(mod(il_np,5).EQ.4) cl_smooth = rl_equi_smooth*dcmplx(-0.7042902001692473,-0.5116967824803674)
      cl_coefsmooth=cl_smooth*0.5d0      
      cl_coefsmooth_diag = (cl_smooth*cl_smooth)*0.25d0
!      
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN                                                                         
!	    
            !! filtering cell process
	    cla_tmp_filtred_field(il_ji,il_jj) = cla_tmp_field(il_ji,il_jj) + &
              cl_coefsmooth * &            
              ( (cla_tmp_field(il_ji,il_jj-1)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj-1) + &
                (cla_tmp_field(il_ji,il_jj+1)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj+1) + &           
	        (cla_tmp_field(il_ji-1,il_jj)-cla_tmp_field(il_ji,il_jj))* &
	           ida_mask(il_ji-1,il_jj) + &
                (cla_tmp_field(il_ji+1,il_jj)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji+1,il_jj) ) + &               
              cl_coefsmooth_diag * &
              ( cla_tmp_field(il_ji-1,il_jj-1)* &
                  ida_mask(il_ji-1,il_jj-1)*ida_mask(il_ji,il_jj-1) + &
                cla_tmp_field(il_ji+1,il_jj-1)* &
                  ida_mask(il_ji+1,il_jj-1)*ida_mask(il_ji,il_jj-1) + &
                cla_tmp_field(il_ji-1,il_jj+1)* &
                  ida_mask(il_ji-1,il_jj+1)*ida_mask(il_ji,il_jj+1) + &
                cla_tmp_field(il_ji+1,il_jj+1)* &
                  ida_mask(il_ji+1,il_jj+1)*ida_mask(il_ji,il_jj+1) - &
                cla_tmp_field(il_ji,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*(ida_mask(il_ji-1,il_jj-1)+ida_mask(il_ji+1,il_jj-1)) - &
                cla_tmp_field(il_ji,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*(ida_mask(il_ji-1,il_jj+1)+ida_mask(il_ji+1,il_jj+1)) - &
                cla_tmp_field(il_ji-1,il_jj)* &
                  ida_mask(il_ji-1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) - &
                cla_tmp_field(il_ji+1,il_jj)* &
                  ida_mask(il_ji+1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) + &
                cla_tmp_field(il_ji,il_jj)* &
                (ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1))* &
                (ida_mask(il_ji-1,il_jj)+ida_mask(il_ji+1,il_jj)) )
!
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      cla_tmp_filtred_field(1,:) = cla_tmp_filtred_field(id_Nxi-1,:) 
      cla_tmp_filtred_field(id_Nxi,:) = cla_tmp_filtred_field(2,:) 
      ! North-south periodic conditions
      cla_tmp_filtred_field(1,1) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(1,id_Nyj) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(id_Nxi,id_Nyj) = dcmplx(0.0d0,0.0d0)
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi, 1 ) = dcmplx(0.0d0,0.0d0)
        cla_tmp_filtred_field(il_jxi,id_Nyj) = cla_tmp_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi,id_Nyj-1) = cla_tmp_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      cla_tmp_field(:,:) = cla_tmp_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
    rda_filtred_field=dreal(cla_tmp_filtred_field)
!    
    write(*,*),
    write(*,*),'imaginary part : ',MAXVAL(ABS(dimag(cla_tmp_filtred_field)))
    write(*,*),
!      
  END SUBROUTINE SHFO_5mofilt2D_ideal_isot
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
  SUBROUTINE SHFO_5mofilt2D_ideal_anisot(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field,rda_e1t,rda_e2t,rda_e1u, &
 		rda_e2u,rda_e1v,rda_e2v)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field

    IMPLICIT NONE

    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask   
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field,rda_e1t,rda_e2t, &
    							  rda_e1u,rda_e2u,rda_e1v,rda_e2v
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_field
    COMPLEX(kind=8) 	                               :: cl_smooth,cl_coefsmooth, &
    							  cl_coefsmooth_diag
    REAL(kind=8)                                       :: rl_equi_smooth,rl_aniso_px,rl_aniso_mx, & 
    							  rl_aniso_py,rl_aniso_my,rl_aniso_xyi,rl_aniso_xyj
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
    !! to conserve the original signal, we create its copy   
    cla_tmp_field(:,:) = rda_field(:,:) 
    cla_tmp_filtred_field(:,:)=rda_field(:,:)    
    !! The equivalent smoothing element in 3rd order
    rl_equi_smooth=0.5d0
!    
    !! Filter application's loop   
    DO il_np = 1,5*id_npShap
     !! The equivalency between the 5th order   
      IF(mod(il_np,5).EQ.0) cl_smooth = rl_equi_smooth*dcmplx(0.870550576184379,0.0d0)
      IF(mod(il_np,5).EQ.1) cl_smooth = rl_equi_smooth*dcmplx(0.2690149185211857, 0.8279427859871953)	   
      IF(mod(il_np,5).EQ.2) cl_smooth = rl_equi_smooth*dcmplx(0.2690149185211857,-0.8279427859871953)
      IF(mod(il_np,5).EQ.3) cl_smooth = rl_equi_smooth*dcmplx(-0.7042902001692473, 0.5116967824803674)
      IF(mod(il_np,5).EQ.4) cl_smooth = rl_equi_smooth*dcmplx(-0.7042902001692473,-0.5116967824803674)
      cl_coefsmooth=cl_smooth*0.5d0      
      cl_coefsmooth_diag = (cl_smooth*cl_smooth)*0.25d0
!      
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN  
!	    
            !! Anisitrpic weight process
            rl_aniso_px= ((rda_e1t(il_ji+1,il_jj))/(rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj))
 	    rl_aniso_mx= -((rda_e1t(il_ji-1,il_jj))/(rda_e1t(il_ji-1,il_jj)+rda_e1t(il_ji,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)) 	     	       	                  
 	    rl_aniso_py= ((rda_e2t(il_ji,il_jj+1))/(rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
              (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj))
 	    rl_aniso_my= -((rda_e2t(il_ji,il_jj-1))/(rda_e2t(il_ji,il_jj-1)+rda_e2t(il_ji,il_jj)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
 	      (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)) 	      
 	    rl_aniso_xyi=rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)-0.5*(((rda_e1t(il_ji,il_jj))/ &
 	      (rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))-((rda_e1t(il_ji,il_jj))/(rda_e1t(il_ji-1,il_jj)+ &
 	      rda_e1t(il_ji,il_jj))))*((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))- &
 	      (rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj)))
 	    rl_aniso_xyj=rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)-0.5*(((rda_e2t(il_ji,il_jj))/ &
 	      (rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))-((rda_e2t(il_ji,il_jj))/(rda_e2t(il_ji,il_jj-1)+ &
 	      rda_e2t(il_ji,il_jj))))*((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))- &
 	      (rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1)))
! 	      
            !! filtering cell process	        
            cla_tmp_filtred_field(il_ji,il_jj)= cla_tmp_field(il_ji,il_jj) + &
              cl_coefsmooth * &
              ( (rl_aniso_my*cla_tmp_field(il_ji,il_jj-1)-rl_aniso_xyj*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj-1) + &
                (rl_aniso_py*cla_tmp_field(il_ji,il_jj+1)-rl_aniso_xyj*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj+1) + &              	   
	        (rl_aniso_mx*cla_tmp_field(il_ji-1,il_jj)-rl_aniso_xyi*cla_tmp_field(il_ji,il_jj))* &
	           ida_mask(il_ji-1,il_jj) + &
                (rl_aniso_px*cla_tmp_field(il_ji+1,il_jj)-rl_aniso_xyi*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji+1,il_jj) ) + &               
              cl_coefsmooth_diag * &
              ( rl_aniso_mx*rl_aniso_my*cla_tmp_field(il_ji-1,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*ida_mask(il_ji-1,il_jj-1) + &
                rl_aniso_px*rl_aniso_my*cla_tmp_field(il_ji+1,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*ida_mask(il_ji+1,il_jj-1) + &
                rl_aniso_mx*rl_aniso_py*cla_tmp_field(il_ji-1,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*ida_mask(il_ji-1,il_jj+1) + &
                rl_aniso_px*rl_aniso_py*cla_tmp_field(il_ji+1,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*ida_mask(il_ji+1,il_jj+1) - &                
                rl_aniso_my*cla_tmp_field(il_ji,il_jj-1)*rl_aniso_xyi* &
                  ida_mask(il_ji,il_jj-1)*(ida_mask(il_ji-1,il_jj-1)+ida_mask(il_ji+1,il_jj-1)) - &
                rl_aniso_py*cla_tmp_field(il_ji,il_jj+1)*rl_aniso_xyi* &
                  ida_mask(il_ji,il_jj+1)*(ida_mask(il_ji-1,il_jj+1)+ida_mask(il_ji+1,il_jj+1)) - &
                rl_aniso_mx*cla_tmp_field(il_ji-1,il_jj)*rl_aniso_xyj* &
                  ida_mask(il_ji-1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) - &
                rl_aniso_px*cla_tmp_field(il_ji+1,il_jj)*rl_aniso_xyj* &
                  ida_mask(il_ji+1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) + &
                cla_tmp_field(il_ji,il_jj)*rl_aniso_xyi*rl_aniso_xyj* &
                (ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1))* &
                (ida_mask(il_ji-1,il_jj)+ida_mask(il_ji+1,il_jj)) )
!                                                                                 
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      cla_tmp_filtred_field(1,:) = cla_tmp_filtred_field(id_Nxi-1,:) 
      cla_tmp_filtred_field(id_Nxi,:) = cla_tmp_filtred_field(2,:) 
      ! North-south periodic conditions
      cla_tmp_filtred_field(1,1) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(1,id_Nyj) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(id_Nxi,id_Nyj) = dcmplx(0.0d0,0.0d0)
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi, 1 ) = dcmplx(0.0d0,0.0d0)
        cla_tmp_filtred_field(il_jxi,id_Nyj) = cla_tmp_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi,id_Nyj-1) = cla_tmp_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      cla_tmp_field(:,:) = cla_tmp_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
    rda_filtred_field=dreal(cla_tmp_filtred_field)
!    
    write(*,*),
    write(*,*),'imaginary part : ',MAXVAL(ABS(dimag(cla_tmp_filtred_field)))
    write(*,*),
!      
  END SUBROUTINE SHFO_5mofilt2D_ideal_anisot
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
  SUBROUTINE SHFO_6mofilt2D_ideal_isot(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field

    IMPLICIT NONE

    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask   
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_field
    COMPLEX(kind=8) 	                               :: cl_smooth,cl_coefsmooth, &
    							  cl_coefsmooth_diag
    REAL(kind=8)                                       :: rl_equi_smooth
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
    !! to conserve the original signal, we create its copy   
    cla_tmp_field(:,:) = rda_field(:,:) 
    cla_tmp_filtred_field(:,:)=rda_field(:,:)    
    !! The equivalent smoothing element in 3rd order
    rl_equi_smooth=0.5d0
!    
    !! Filter application's loop   
    DO il_np = 1,6*id_npShap
      !! The equivalency between the 6th order   
       !! The equivalency between the second order 
	!S1 = .4454493590701697 %i + .7715409221085262, 
	!S2 = - .8908987181403392 %i - 1.265255773810379E-16, 
	!S3 = .8908987181403392 %i + 5.513644965825776E-17, 
	!S4 = .4454493590701694 %i - .7715409221085312, 
	!S5 = - .4454493590701722 %i - .7715409221085208, 
	!S6 = .7715409221085215 - .4454493590701757 %i,   
       IF(mod(il_np,6).EQ.0) cl_smooth = rl_equi_smooth*dcmplx(.7715409221085262, .4454493590701697)
       IF(mod(il_np,6).EQ.1) cl_smooth = rl_equi_smooth*dcmplx(0.d0,-0.8908987181403392)
       IF(mod(il_np,6).EQ.2) cl_smooth = rl_equi_smooth*dcmplx(0.d0,.8908987181403392)
       IF(mod(il_np,6).EQ.3) cl_smooth = rl_equi_smooth*dcmplx(- .7715409221085312, .4454493590701694)
       IF(mod(il_np,6).EQ.4) cl_smooth = rl_equi_smooth*dcmplx(- .7715409221085312,-.4454493590701694)
       IF(mod(il_np,6).EQ.5) cl_smooth = rl_equi_smooth*dcmplx( .7715409221085262,- .4454493590701697)
       cl_coefsmooth=cl_smooth*0.5d0   
       cl_coefsmooth_diag = (cl_smooth*cl_smooth)*0.25d0
!      
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN                                                                         
!	    
            !! filtering cell process
	    cla_tmp_filtred_field(il_ji,il_jj) = cla_tmp_field(il_ji,il_jj) + &
              cl_coefsmooth * &            
              ( (cla_tmp_field(il_ji,il_jj-1)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj-1) + &
                (cla_tmp_field(il_ji,il_jj+1)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj+1) + &           
	        (cla_tmp_field(il_ji-1,il_jj)-cla_tmp_field(il_ji,il_jj))* &
	           ida_mask(il_ji-1,il_jj) + &
                (cla_tmp_field(il_ji+1,il_jj)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji+1,il_jj) ) + &               
              cl_coefsmooth_diag * &
              ( cla_tmp_field(il_ji-1,il_jj-1)* &
                  ida_mask(il_ji-1,il_jj-1)*ida_mask(il_ji,il_jj-1) + &
                cla_tmp_field(il_ji+1,il_jj-1)* &
                  ida_mask(il_ji+1,il_jj-1)*ida_mask(il_ji,il_jj-1) + &
                cla_tmp_field(il_ji-1,il_jj+1)* &
                  ida_mask(il_ji-1,il_jj+1)*ida_mask(il_ji,il_jj+1) + &
                cla_tmp_field(il_ji+1,il_jj+1)* &
                  ida_mask(il_ji+1,il_jj+1)*ida_mask(il_ji,il_jj+1) - &
                cla_tmp_field(il_ji,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*(ida_mask(il_ji-1,il_jj-1)+ida_mask(il_ji+1,il_jj-1)) - &
                cla_tmp_field(il_ji,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*(ida_mask(il_ji-1,il_jj+1)+ida_mask(il_ji+1,il_jj+1)) - &
                cla_tmp_field(il_ji-1,il_jj)* &
                  ida_mask(il_ji-1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) - &
                cla_tmp_field(il_ji+1,il_jj)* &
                  ida_mask(il_ji+1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) + &
                cla_tmp_field(il_ji,il_jj)* &
                (ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1))* &
                (ida_mask(il_ji-1,il_jj)+ida_mask(il_ji+1,il_jj)) )
!
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      cla_tmp_filtred_field(1,:) = cla_tmp_filtred_field(id_Nxi-1,:) 
      cla_tmp_filtred_field(id_Nxi,:) = cla_tmp_filtred_field(2,:) 
      ! North-south periodic conditions
      cla_tmp_filtred_field(1,1) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(1,id_Nyj) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(id_Nxi,id_Nyj) = dcmplx(0.0d0,0.0d0)
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi, 1 ) = dcmplx(0.0d0,0.0d0)
        cla_tmp_filtred_field(il_jxi,id_Nyj) = cla_tmp_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi,id_Nyj-1) = cla_tmp_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      cla_tmp_field(:,:) = cla_tmp_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
    rda_filtred_field=dreal(cla_tmp_filtred_field)
!    
    write(*,*),
    write(*,*),'imaginary part : ',MAXVAL(ABS(dimag(cla_tmp_filtred_field)))
    write(*,*),
!      
  END SUBROUTINE SHFO_6mofilt2D_ideal_isot
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
  SUBROUTINE SHFO_6mofilt2D_ideal_anisot(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field,rda_e1t,rda_e2t,rda_e1u, &
 		rda_e2u,rda_e1v,rda_e2v)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field

    IMPLICIT NONE

    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask   
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field,rda_e1t,rda_e2t, &
    							  rda_e1u,rda_e2u,rda_e1v,rda_e2v
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_field
    COMPLEX(kind=8) 	                               :: cl_smooth,cl_coefsmooth, &
    							  cl_coefsmooth_diag
    REAL(kind=8)                                       :: rl_equi_smooth,rl_aniso_px,rl_aniso_mx, & 
    							  rl_aniso_py,rl_aniso_my,rl_aniso_xyi,rl_aniso_xyj
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
    !! to conserve the original signal, we create its copy   
    cla_tmp_field(:,:) = rda_field(:,:) 
    cla_tmp_filtred_field(:,:)=rda_field(:,:)    
    !! The equivalent smoothing element in 3rd order
    rl_equi_smooth=0.5d0
!    
    !! Filter application's loop   
    DO il_np = 1,6*id_npShap
     !! The equivalency between the 6th order   
       !! The equivalency between the second order 
	!S1 = .4454493590701697 %i + .7715409221085262, 
	!S2 = - .8908987181403392 %i - 1.265255773810379E-16, 
	!S3 = .8908987181403392 %i + 5.513644965825776E-17, 
	!S4 = .4454493590701694 %i - .7715409221085312, 
	!S5 = - .4454493590701722 %i - .7715409221085208, 
	!S6 = .7715409221085215 - .4454493590701757 %i,   
       IF(mod(il_np,6).EQ.0) cl_smooth = rl_equi_smooth*dcmplx(.7715409221085262, .4454493590701697)
       IF(mod(il_np,6).EQ.1) cl_smooth = rl_equi_smooth*dcmplx(0.d0,-0.8908987181403392)
       IF(mod(il_np,6).EQ.2) cl_smooth = rl_equi_smooth*dcmplx(0.d0,.8908987181403392)
       IF(mod(il_np,6).EQ.3) cl_smooth = rl_equi_smooth*dcmplx(- .7715409221085312, .4454493590701694)
       IF(mod(il_np,6).EQ.4) cl_smooth = rl_equi_smooth*dcmplx(- .7715409221085312,-.4454493590701694)
       IF(mod(il_np,6).EQ.5) cl_smooth = rl_equi_smooth*dcmplx( .7715409221085262,- .4454493590701697)
       cl_coefsmooth=cl_smooth*0.5d0   
       cl_coefsmooth_diag = (cl_smooth*cl_smooth)*0.25d0
!      
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN  
!	    
            !! Anisitrpic weight process
            rl_aniso_px= ((rda_e1t(il_ji+1,il_jj))/(rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj))
 	    rl_aniso_mx= -((rda_e1t(il_ji-1,il_jj))/(rda_e1t(il_ji-1,il_jj)+rda_e1t(il_ji,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)) 	     	       	                  
 	    rl_aniso_py= ((rda_e2t(il_ji,il_jj+1))/(rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
              (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj))
 	    rl_aniso_my= -((rda_e2t(il_ji,il_jj-1))/(rda_e2t(il_ji,il_jj-1)+rda_e2t(il_ji,il_jj)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
 	      (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)) 	      
 	    rl_aniso_xyi=rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)-0.5*(((rda_e1t(il_ji,il_jj))/ &
 	      (rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))-((rda_e1t(il_ji,il_jj))/(rda_e1t(il_ji-1,il_jj)+ &
 	      rda_e1t(il_ji,il_jj))))*((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))- &
 	      (rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj)))
 	    rl_aniso_xyj=rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)-0.5*(((rda_e2t(il_ji,il_jj))/ &
 	      (rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))-((rda_e2t(il_ji,il_jj))/(rda_e2t(il_ji,il_jj-1)+ &
 	      rda_e2t(il_ji,il_jj))))*((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))- &
 	      (rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1)))
! 	      
            !! filtering cell process	        
            cla_tmp_filtred_field(il_ji,il_jj)= cla_tmp_field(il_ji,il_jj) + &
              cl_coefsmooth * &
              ( (rl_aniso_my*cla_tmp_field(il_ji,il_jj-1)-rl_aniso_xyj*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj-1) + &
                (rl_aniso_py*cla_tmp_field(il_ji,il_jj+1)-rl_aniso_xyj*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj+1) + &              	   
	        (rl_aniso_mx*cla_tmp_field(il_ji-1,il_jj)-rl_aniso_xyi*cla_tmp_field(il_ji,il_jj))* &
	           ida_mask(il_ji-1,il_jj) + &
                (rl_aniso_px*cla_tmp_field(il_ji+1,il_jj)-rl_aniso_xyi*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji+1,il_jj) ) + &               
              cl_coefsmooth_diag * &
              ( rl_aniso_mx*rl_aniso_my*cla_tmp_field(il_ji-1,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*ida_mask(il_ji-1,il_jj-1) + &
                rl_aniso_px*rl_aniso_my*cla_tmp_field(il_ji+1,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*ida_mask(il_ji+1,il_jj-1) + &
                rl_aniso_mx*rl_aniso_py*cla_tmp_field(il_ji-1,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*ida_mask(il_ji-1,il_jj+1) + &
                rl_aniso_px*rl_aniso_py*cla_tmp_field(il_ji+1,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*ida_mask(il_ji+1,il_jj+1) - &                
                rl_aniso_my*cla_tmp_field(il_ji,il_jj-1)*rl_aniso_xyi* &
                  ida_mask(il_ji,il_jj-1)*(ida_mask(il_ji-1,il_jj-1)+ida_mask(il_ji+1,il_jj-1)) - &
                rl_aniso_py*cla_tmp_field(il_ji,il_jj+1)*rl_aniso_xyi* &
                  ida_mask(il_ji,il_jj+1)*(ida_mask(il_ji-1,il_jj+1)+ida_mask(il_ji+1,il_jj+1)) - &
                rl_aniso_mx*cla_tmp_field(il_ji-1,il_jj)*rl_aniso_xyj* &
                  ida_mask(il_ji-1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) - &
                rl_aniso_px*cla_tmp_field(il_ji+1,il_jj)*rl_aniso_xyj* &
                  ida_mask(il_ji+1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) + &
                cla_tmp_field(il_ji,il_jj)*rl_aniso_xyi*rl_aniso_xyj* &
                (ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1))* &
                (ida_mask(il_ji-1,il_jj)+ida_mask(il_ji+1,il_jj)) )
!                                                                                 
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      cla_tmp_filtred_field(1,:) = cla_tmp_filtred_field(id_Nxi-1,:) 
      cla_tmp_filtred_field(id_Nxi,:) = cla_tmp_filtred_field(2,:) 
      ! North-south periodic conditions
      cla_tmp_filtred_field(1,1) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(1,id_Nyj) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(id_Nxi,id_Nyj) = dcmplx(0.0d0,0.0d0)
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi, 1 ) = dcmplx(0.0d0,0.0d0)
        cla_tmp_filtred_field(il_jxi,id_Nyj) = cla_tmp_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi,id_Nyj-1) = cla_tmp_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      cla_tmp_field(:,:) = cla_tmp_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
    rda_filtred_field=dreal(cla_tmp_filtred_field)
!    
    write(*,*),
    write(*,*),'imaginary part : ',MAXVAL(ABS(dimag(cla_tmp_filtred_field)))
    write(*,*),
!      
  END SUBROUTINE SHFO_6mofilt2D_ideal_anisot
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
  SUBROUTINE SHFO_7mofilt2D_ideal_isot(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field

    IMPLICIT NONE

    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask   
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_field
    COMPLEX(kind=8) 	                               :: cl_smooth,cl_coefsmooth, &
    							  cl_coefsmooth_diag
    REAL(kind=8)                                       :: rl_equi_smooth
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
    !! to conserve the original signal, we create its copy   
    cla_tmp_field(:,:) = rda_field(:,:) 
    cla_tmp_filtred_field(:,:)=rda_field(:,:)    
    !! The equivalent smoothing element in 3rd order
    rl_equi_smooth=0.5d0
!    
    !! Filter application's loop   
    DO il_np = 1,7*id_npShap
      !! The equivalency between the second order   
       IF(mod(il_np,7).EQ.0) cl_smooth = rl_equi_smooth*dcmplx(0.90572366426390,0.0d0)
       IF(mod(il_np,7).EQ.1) cl_smooth = rl_equi_smooth*dcmplx(0.56470946797066, 0.70812327513782)	   
       IF(mod(il_np,7).EQ.2) cl_smooth = rl_equi_smooth*dcmplx(0.56470946797066,-0.70812327513782)	   	   
       IF(mod(il_np,7).EQ.3) cl_smooth = rl_equi_smooth*dcmplx(-0.20154247567833, 0.88301528101448)
       IF(mod(il_np,7).EQ.4) cl_smooth = rl_equi_smooth*dcmplx(-0.20154247567833,-0.88301528101448)
       IF(mod(il_np,7).EQ.5) cl_smooth = rl_equi_smooth*dcmplx(-0.81602882442428, 0.39297877005807)
       IF(mod(il_np,7).EQ.6) cl_smooth = rl_equi_smooth*dcmplx(-0.81602882442428,-0.39297877005807)
       cl_coefsmooth=cl_smooth*0.5d0   
       cl_coefsmooth_diag = (cl_smooth*cl_smooth)*0.25d0
!      
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN                                                                         
!	    
            !! filtering cell process
	    cla_tmp_filtred_field(il_ji,il_jj) = cla_tmp_field(il_ji,il_jj) + &
              cl_coefsmooth * &            
              ( (cla_tmp_field(il_ji,il_jj-1)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj-1) + &
                (cla_tmp_field(il_ji,il_jj+1)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj+1) + &           
	        (cla_tmp_field(il_ji-1,il_jj)-cla_tmp_field(il_ji,il_jj))* &
	           ida_mask(il_ji-1,il_jj) + &
                (cla_tmp_field(il_ji+1,il_jj)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji+1,il_jj) ) + &               
              cl_coefsmooth_diag * &
              ( cla_tmp_field(il_ji-1,il_jj-1)* &
                  ida_mask(il_ji-1,il_jj-1)*ida_mask(il_ji,il_jj-1) + &
                cla_tmp_field(il_ji+1,il_jj-1)* &
                  ida_mask(il_ji+1,il_jj-1)*ida_mask(il_ji,il_jj-1) + &
                cla_tmp_field(il_ji-1,il_jj+1)* &
                  ida_mask(il_ji-1,il_jj+1)*ida_mask(il_ji,il_jj+1) + &
                cla_tmp_field(il_ji+1,il_jj+1)* &
                  ida_mask(il_ji+1,il_jj+1)*ida_mask(il_ji,il_jj+1) - &
                cla_tmp_field(il_ji,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*(ida_mask(il_ji-1,il_jj-1)+ida_mask(il_ji+1,il_jj-1)) - &
                cla_tmp_field(il_ji,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*(ida_mask(il_ji-1,il_jj+1)+ida_mask(il_ji+1,il_jj+1)) - &
                cla_tmp_field(il_ji-1,il_jj)* &
                  ida_mask(il_ji-1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) - &
                cla_tmp_field(il_ji+1,il_jj)* &
                  ida_mask(il_ji+1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) + &
                cla_tmp_field(il_ji,il_jj)* &
                (ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1))* &
                (ida_mask(il_ji-1,il_jj)+ida_mask(il_ji+1,il_jj)) )
!
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      cla_tmp_filtred_field(1,:) = cla_tmp_filtred_field(id_Nxi-1,:) 
      cla_tmp_filtred_field(id_Nxi,:) = cla_tmp_filtred_field(2,:) 
      ! North-south periodic conditions
      cla_tmp_filtred_field(1,1) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(1,id_Nyj) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(id_Nxi,id_Nyj) = dcmplx(0.0d0,0.0d0)
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi, 1 ) = dcmplx(0.0d0,0.0d0)
        cla_tmp_filtred_field(il_jxi,id_Nyj) = cla_tmp_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi,id_Nyj-1) = cla_tmp_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      cla_tmp_field(:,:) = cla_tmp_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
    rda_filtred_field=dreal(cla_tmp_filtred_field)
!    
    write(*,*),
    write(*,*),'imaginary part : ',MAXVAL(ABS(dimag(cla_tmp_filtred_field)))
    write(*,*),
!      
  END SUBROUTINE SHFO_7mofilt2D_ideal_isot
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
  SUBROUTINE SHFO_7mofilt2D_ideal_anisot(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field,rda_e1t,rda_e2t,rda_e1u, &
 		rda_e2u,rda_e1v,rda_e2v)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field

    IMPLICIT NONE

    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask   
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field,rda_e1t,rda_e2t, &
    							  rda_e1u,rda_e2u,rda_e1v,rda_e2v
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_field
    COMPLEX(kind=8) 	                               :: cl_smooth,cl_coefsmooth, &
    							  cl_coefsmooth_diag
    REAL(kind=8)                                       :: rl_equi_smooth,rl_aniso_px,rl_aniso_mx, & 
    							  rl_aniso_py,rl_aniso_my,rl_aniso_xyi,rl_aniso_xyj
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
    !! to conserve the original signal, we create its copy   
    cla_tmp_field(:,:) = rda_field(:,:) 
    cla_tmp_filtred_field(:,:)=rda_field(:,:)    
    !! The equivalent smoothing element in 3rd order
    rl_equi_smooth=0.5d0
!    
    !! Filter application's loop   
    DO il_np = 1,7*id_npShap
     !! The equivalency between the second order   
       IF(mod(il_np,7).EQ.0) cl_smooth = rl_equi_smooth*dcmplx(0.90572366426390,0.0d0)
       IF(mod(il_np,7).EQ.1) cl_smooth = rl_equi_smooth*dcmplx(0.56470946797066, 0.70812327513782)	   
       IF(mod(il_np,7).EQ.2) cl_smooth = rl_equi_smooth*dcmplx(0.56470946797066,-0.70812327513782)	   	   
       IF(mod(il_np,7).EQ.3) cl_smooth = rl_equi_smooth*dcmplx(-0.20154247567833, 0.88301528101448)
       IF(mod(il_np,7).EQ.4) cl_smooth = rl_equi_smooth*dcmplx(-0.20154247567833,-0.88301528101448)
       IF(mod(il_np,7).EQ.5) cl_smooth = rl_equi_smooth*dcmplx(-0.81602882442428, 0.39297877005807)
       IF(mod(il_np,7).EQ.6) cl_smooth = rl_equi_smooth*dcmplx(-0.81602882442428,-0.39297877005807)
       cl_coefsmooth=cl_smooth*0.5d0   
       cl_coefsmooth_diag = (cl_smooth*cl_smooth)*0.25d0
!      
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN  
!	    
            !! Anisitrpic weight process
            rl_aniso_px= ((rda_e1t(il_ji+1,il_jj))/(rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj))
 	    rl_aniso_mx= -((rda_e1t(il_ji-1,il_jj))/(rda_e1t(il_ji-1,il_jj)+rda_e1t(il_ji,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)) 	     	       	                  
 	    rl_aniso_py= ((rda_e2t(il_ji,il_jj+1))/(rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
              (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj))
 	    rl_aniso_my= -((rda_e2t(il_ji,il_jj-1))/(rda_e2t(il_ji,il_jj-1)+rda_e2t(il_ji,il_jj)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
 	      (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)) 	      
 	    rl_aniso_xyi=rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)-0.5*(((rda_e1t(il_ji,il_jj))/ &
 	      (rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))-((rda_e1t(il_ji,il_jj))/(rda_e1t(il_ji-1,il_jj)+ &
 	      rda_e1t(il_ji,il_jj))))*((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))- &
 	      (rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj)))
 	    rl_aniso_xyj=rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)-0.5*(((rda_e2t(il_ji,il_jj))/ &
 	      (rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))-((rda_e2t(il_ji,il_jj))/(rda_e2t(il_ji,il_jj-1)+ &
 	      rda_e2t(il_ji,il_jj))))*((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))- &
 	      (rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1)))
! 	      
            !! filtering cell process	        
            cla_tmp_filtred_field(il_ji,il_jj)= cla_tmp_field(il_ji,il_jj) + &
              cl_coefsmooth * &
              ( (rl_aniso_my*cla_tmp_field(il_ji,il_jj-1)-rl_aniso_xyj*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj-1) + &
                (rl_aniso_py*cla_tmp_field(il_ji,il_jj+1)-rl_aniso_xyj*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj+1) + &              	   
	        (rl_aniso_mx*cla_tmp_field(il_ji-1,il_jj)-rl_aniso_xyi*cla_tmp_field(il_ji,il_jj))* &
	           ida_mask(il_ji-1,il_jj) + &
                (rl_aniso_px*cla_tmp_field(il_ji+1,il_jj)-rl_aniso_xyi*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji+1,il_jj) ) + &               
              cl_coefsmooth_diag * &
              ( rl_aniso_mx*rl_aniso_my*cla_tmp_field(il_ji-1,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*ida_mask(il_ji-1,il_jj-1) + &
                rl_aniso_px*rl_aniso_my*cla_tmp_field(il_ji+1,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*ida_mask(il_ji+1,il_jj-1) + &
                rl_aniso_mx*rl_aniso_py*cla_tmp_field(il_ji-1,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*ida_mask(il_ji-1,il_jj+1) + &
                rl_aniso_px*rl_aniso_py*cla_tmp_field(il_ji+1,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*ida_mask(il_ji+1,il_jj+1) - &                
                rl_aniso_my*cla_tmp_field(il_ji,il_jj-1)*rl_aniso_xyi* &
                  ida_mask(il_ji,il_jj-1)*(ida_mask(il_ji-1,il_jj-1)+ida_mask(il_ji+1,il_jj-1)) - &
                rl_aniso_py*cla_tmp_field(il_ji,il_jj+1)*rl_aniso_xyi* &
                  ida_mask(il_ji,il_jj+1)*(ida_mask(il_ji-1,il_jj+1)+ida_mask(il_ji+1,il_jj+1)) - &
                rl_aniso_mx*cla_tmp_field(il_ji-1,il_jj)*rl_aniso_xyj* &
                  ida_mask(il_ji-1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) - &
                rl_aniso_px*cla_tmp_field(il_ji+1,il_jj)*rl_aniso_xyj* &
                  ida_mask(il_ji+1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) + &
                cla_tmp_field(il_ji,il_jj)*rl_aniso_xyi*rl_aniso_xyj* &
                (ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1))* &
                (ida_mask(il_ji-1,il_jj)+ida_mask(il_ji+1,il_jj)) )
!                                                                                 
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      cla_tmp_filtred_field(1,:) = cla_tmp_filtred_field(id_Nxi-1,:) 
      cla_tmp_filtred_field(id_Nxi,:) = cla_tmp_filtred_field(2,:) 
      ! North-south periodic conditions
      cla_tmp_filtred_field(1,1) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(1,id_Nyj) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(id_Nxi,id_Nyj) = dcmplx(0.0d0,0.0d0)
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi, 1 ) = dcmplx(0.0d0,0.0d0)
        cla_tmp_filtred_field(il_jxi,id_Nyj) = cla_tmp_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi,id_Nyj-1) = cla_tmp_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      cla_tmp_field(:,:) = cla_tmp_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
    rda_filtred_field=dreal(cla_tmp_filtred_field)
!    
    write(*,*),
    write(*,*),'imaginary part : ',MAXVAL(ABS(dimag(cla_tmp_filtred_field)))
    write(*,*),
!      
  END SUBROUTINE SHFO_7mofilt2D_ideal_anisot
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
  SUBROUTINE SHFO_8mofilt2D_ideal_isot(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field

    IMPLICIT NONE

    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask   
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_field
    COMPLEX(kind=8) 	                               :: cl_smooth,cl_coefsmooth, &
    							  cl_coefsmooth_diag
    REAL(kind=8)                                       :: rl_equi_smooth
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
    !! to conserve the original signal, we create its copy   
    cla_tmp_field(:,:) = rda_field(:,:) 
    cla_tmp_filtred_field(:,:)=rda_field(:,:)    
    !! The equivalent smoothing element in 3rd order
    rl_equi_smooth=0.5d0
!    
    !! Filter application's loop   
    DO il_np = 1,8*id_npShap
      !! The equivalency between the 8th order   
      IF(mod(il_np,8).EQ.0) cl_smooth = rl_equi_smooth*dcmplx( 1.0d0,0.0d0)
      IF(mod(il_np,8).EQ.1) cl_smooth = rl_equi_smooth*dcmplx(-1.0d0,0.0d0)	   
      IF(mod(il_np,8).EQ.2) cl_smooth = rl_equi_smooth*dcmplx(0.0d0, 1.0d0)
      IF(mod(il_np,8).EQ.3) cl_smooth = rl_equi_smooth*dcmplx(0.0d0,-1.0d0)	      
      IF(mod(il_np,8).EQ.4) cl_smooth = rl_equi_smooth*cdsqrt(dcmplx(0.0d0, 1.0d0))
      IF(mod(il_np,8).EQ.5) cl_smooth = -rl_equi_smooth*cdsqrt(dcmplx(0.0d0, 1.0d0))
      IF(mod(il_np,8).EQ.6) cl_smooth = rl_equi_smooth*dcmplx(0.0d0, 1.0d0)*cdsqrt(dcmplx(0.0d0,1.0d0))
      IF(mod(il_np,8).EQ.7) cl_smooth = rl_equi_smooth*dcmplx(0.0d0,-1.0d0)*cdsqrt(dcmplx(0.0d0,1.0d0))
      cl_coefsmooth=cl_smooth*0.5d0   
      cl_coefsmooth_diag = (cl_smooth*cl_smooth)*0.25d0
!      
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN                                                                         
!	    
            !! filtering cell process
	    cla_tmp_filtred_field(il_ji,il_jj) = cla_tmp_field(il_ji,il_jj) + &
              cl_coefsmooth * &            
              ( (cla_tmp_field(il_ji,il_jj-1)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj-1) + &
                (cla_tmp_field(il_ji,il_jj+1)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj+1) + &           
	        (cla_tmp_field(il_ji-1,il_jj)-cla_tmp_field(il_ji,il_jj))* &
	           ida_mask(il_ji-1,il_jj) + &
                (cla_tmp_field(il_ji+1,il_jj)-cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji+1,il_jj) ) + &               
              cl_coefsmooth_diag * &
              ( cla_tmp_field(il_ji-1,il_jj-1)* &
                  ida_mask(il_ji-1,il_jj-1)*ida_mask(il_ji,il_jj-1) + &
                cla_tmp_field(il_ji+1,il_jj-1)* &
                  ida_mask(il_ji+1,il_jj-1)*ida_mask(il_ji,il_jj-1) + &
                cla_tmp_field(il_ji-1,il_jj+1)* &
                  ida_mask(il_ji-1,il_jj+1)*ida_mask(il_ji,il_jj+1) + &
                cla_tmp_field(il_ji+1,il_jj+1)* &
                  ida_mask(il_ji+1,il_jj+1)*ida_mask(il_ji,il_jj+1) - &
                cla_tmp_field(il_ji,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*(ida_mask(il_ji-1,il_jj-1)+ida_mask(il_ji+1,il_jj-1)) - &
                cla_tmp_field(il_ji,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*(ida_mask(il_ji-1,il_jj+1)+ida_mask(il_ji+1,il_jj+1)) - &
                cla_tmp_field(il_ji-1,il_jj)* &
                  ida_mask(il_ji-1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) - &
                cla_tmp_field(il_ji+1,il_jj)* &
                  ida_mask(il_ji+1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) + &
                cla_tmp_field(il_ji,il_jj)* &
                (ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1))* &
                (ida_mask(il_ji-1,il_jj)+ida_mask(il_ji+1,il_jj)) )
!
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      cla_tmp_filtred_field(1,:) = cla_tmp_filtred_field(id_Nxi-1,:) 
      cla_tmp_filtred_field(id_Nxi,:) = cla_tmp_filtred_field(2,:) 
      ! North-south periodic conditions
      cla_tmp_filtred_field(1,1) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(1,id_Nyj) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(id_Nxi,id_Nyj) = dcmplx(0.0d0,0.0d0)
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi, 1 ) = dcmplx(0.0d0,0.0d0)
        cla_tmp_filtred_field(il_jxi,id_Nyj) = cla_tmp_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi,id_Nyj-1) = cla_tmp_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      cla_tmp_field(:,:) = cla_tmp_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
    rda_filtred_field=dreal(cla_tmp_filtred_field)
!    
    write(*,*),
    write(*,*),'imaginary part : ',MAXVAL(ABS(dimag(cla_tmp_filtred_field)))
    write(*,*),
!      
  END SUBROUTINE SHFO_8mofilt2D_ideal_isot
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
  SUBROUTINE SHFO_8mofilt2D_ideal_anisot(rda_field,id_npShap,id_Nxi,id_Nyj, &
                ida_mask,rda_filtred_field,rda_e1t,rda_e2t,rda_e1u, &
 		rda_e2u,rda_e1v,rda_e2v)
    ! DESCRIPTION: 
    !***************************************************************
    !  -> Applies computational Shapiro filter to 2D field (cell center).
    !  -> use filtering function  = [1 - (d_xx+d_yy)^n]
    !  -> Isotropic filter for assuming constant grid spacing.
    !
    !  The 2D isotropic Shapiro-Fickian filter : the 5 points operator 
    !       | 0| | 1| | 0|        1
    !  ->   | 1| |-4| | 1|  =  1 -4  1
    !       | 0| | 1| | 0|        1
    !***************************************************************
    ! Routine arguments
    !   rlpa_field :: cell-centered 2D field on which filter applies
    !   il_npShap :: (total) power of the filter for this tracer
    !   ilpa_mask :: pixels where the tracer is not defined (land zones)
    !   rlpa_filtred_field :: the filtered field

    IMPLICIT NONE

    INTEGER,                              INTENT(IN)   :: id_npShap,id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask   
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field,rda_e1t,rda_e2t, &
    							  rda_e1u,rda_e2u,rda_e1v,rda_e2v
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_filtred_field
    COMPLEX(kind=8),DIMENSION(id_Nxi,id_Nyj)           :: cla_tmp_field
    COMPLEX(kind=8) 	                               :: cl_smooth,cl_coefsmooth, &
    							  cl_coefsmooth_diag
    REAL(kind=8)                                       :: rl_equi_smooth,rl_aniso_px,rl_aniso_mx, & 
    							  rl_aniso_py,rl_aniso_my,rl_aniso_xyi,rl_aniso_xyj
    INTEGER                                            :: il_ji,il_jj,il_np,il_jxi,il_ijdt
!
!
    !! to conserve the original signal, we create its copy   
    cla_tmp_field(:,:) = rda_field(:,:) 
    cla_tmp_filtred_field(:,:)=rda_field(:,:)    
    !! The equivalent smoothing element in 3rd order
    rl_equi_smooth=0.5d0
!    
    !! Filter application's loop   
    DO il_np = 1,8*id_npShap
      !! The equivalency between the 8th order   
      IF(mod(il_np,8).EQ.0) cl_smooth = rl_equi_smooth*dcmplx( 1.0d0,0.0d0)
      IF(mod(il_np,8).EQ.1) cl_smooth = rl_equi_smooth*dcmplx(-1.0d0,0.0d0)	   
      IF(mod(il_np,8).EQ.2) cl_smooth = rl_equi_smooth*dcmplx(0.0d0, 1.0d0)
      IF(mod(il_np,8).EQ.3) cl_smooth = rl_equi_smooth*dcmplx(0.0d0,-1.0d0)	      
      IF(mod(il_np,8).EQ.4) cl_smooth = rl_equi_smooth*cdsqrt(dcmplx(0.0d0, 1.0d0))
      IF(mod(il_np,8).EQ.5) cl_smooth = -rl_equi_smooth*cdsqrt(dcmplx(0.0d0, 1.0d0))
      IF(mod(il_np,8).EQ.6) cl_smooth = rl_equi_smooth*dcmplx(0.0d0, 1.0d0)*cdsqrt(dcmplx(0.0d0,1.0d0))
      IF(mod(il_np,8).EQ.7) cl_smooth = rl_equi_smooth*dcmplx(0.0d0,-1.0d0)*cdsqrt(dcmplx(0.0d0,1.0d0))
      cl_coefsmooth=cl_smooth*0.5d0   
      cl_coefsmooth_diag = (cl_smooth*cl_smooth)*0.25d0
!      
      DO il_jj = 2,id_Nyj-1  !! Horizontal filtering
        DO il_ji = 2,id_Nxi-1  !! Vertical filtering     
          !! the filter does not get in land            
          IF (ida_mask(il_ji,il_jj).EQ.1)  THEN  
!	    
            !! Anisitrpic weight process
            rl_aniso_px= ((rda_e1t(il_ji+1,il_jj))/(rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj))
 	    rl_aniso_mx= -((rda_e1t(il_ji-1,il_jj))/(rda_e1t(il_ji-1,il_jj)+rda_e1t(il_ji,il_jj)))* &
              ((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))-(rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj))) + &
 	      (rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)) 	     	       	                  
 	    rl_aniso_py= ((rda_e2t(il_ji,il_jj+1))/(rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
              (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj))
 	    rl_aniso_my= -((rda_e2t(il_ji,il_jj-1))/(rda_e2t(il_ji,il_jj-1)+rda_e2t(il_ji,il_jj)))* &
 	      ((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))-(rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1))) + &
 	      (rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)) 	      
 	    rl_aniso_xyi=rda_e2t(il_ji,il_jj)/rda_e1t(il_ji,il_jj)-0.5*(((rda_e1t(il_ji,il_jj))/ &
 	      (rda_e1t(il_ji,il_jj)+rda_e1t(il_ji+1,il_jj)))-((rda_e1t(il_ji,il_jj))/(rda_e1t(il_ji-1,il_jj)+ &
 	      rda_e1t(il_ji,il_jj))))*((rda_e2u(il_ji,il_jj)/rda_e1u(il_ji,il_jj))- &
 	      (rda_e2u(il_ji-1,il_jj)/rda_e1u(il_ji-1,il_jj)))
 	    rl_aniso_xyj=rda_e1t(il_ji,il_jj)/rda_e2t(il_ji,il_jj)-0.5*(((rda_e2t(il_ji,il_jj))/ &
 	      (rda_e2t(il_ji,il_jj)+rda_e2t(il_ji,il_jj+1)))-((rda_e2t(il_ji,il_jj))/(rda_e2t(il_ji,il_jj-1)+ &
 	      rda_e2t(il_ji,il_jj))))*((rda_e1v(il_ji,il_jj)/rda_e2v(il_ji,il_jj))- &
 	      (rda_e1v(il_ji,il_jj-1)/rda_e2v(il_ji,il_jj-1)))
! 	      
            !! filtering cell process	        
            cla_tmp_filtred_field(il_ji,il_jj)= cla_tmp_field(il_ji,il_jj) + &
              cl_coefsmooth * &
              ( (rl_aniso_my*cla_tmp_field(il_ji,il_jj-1)-rl_aniso_xyj*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj-1) + &
                (rl_aniso_py*cla_tmp_field(il_ji,il_jj+1)-rl_aniso_xyj*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji,il_jj+1) + &              	   
	        (rl_aniso_mx*cla_tmp_field(il_ji-1,il_jj)-rl_aniso_xyi*cla_tmp_field(il_ji,il_jj))* &
	           ida_mask(il_ji-1,il_jj) + &
                (rl_aniso_px*cla_tmp_field(il_ji+1,il_jj)-rl_aniso_xyi*cla_tmp_field(il_ji,il_jj))* &
                   ida_mask(il_ji+1,il_jj) ) + &               
              cl_coefsmooth_diag * &
              ( rl_aniso_mx*rl_aniso_my*cla_tmp_field(il_ji-1,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*ida_mask(il_ji-1,il_jj-1) + &
                rl_aniso_px*rl_aniso_my*cla_tmp_field(il_ji+1,il_jj-1)* &
                  ida_mask(il_ji,il_jj-1)*ida_mask(il_ji+1,il_jj-1) + &
                rl_aniso_mx*rl_aniso_py*cla_tmp_field(il_ji-1,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*ida_mask(il_ji-1,il_jj+1) + &
                rl_aniso_px*rl_aniso_py*cla_tmp_field(il_ji+1,il_jj+1)* &
                  ida_mask(il_ji,il_jj+1)*ida_mask(il_ji+1,il_jj+1) - &                
                rl_aniso_my*cla_tmp_field(il_ji,il_jj-1)*rl_aniso_xyi* &
                  ida_mask(il_ji,il_jj-1)*(ida_mask(il_ji-1,il_jj-1)+ida_mask(il_ji+1,il_jj-1)) - &
                rl_aniso_py*cla_tmp_field(il_ji,il_jj+1)*rl_aniso_xyi* &
                  ida_mask(il_ji,il_jj+1)*(ida_mask(il_ji-1,il_jj+1)+ida_mask(il_ji+1,il_jj+1)) - &
                rl_aniso_mx*cla_tmp_field(il_ji-1,il_jj)*rl_aniso_xyj* &
                  ida_mask(il_ji-1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) - &
                rl_aniso_px*cla_tmp_field(il_ji+1,il_jj)*rl_aniso_xyj* &
                  ida_mask(il_ji+1,il_jj)*(ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1)) + &
                cla_tmp_field(il_ji,il_jj)*rl_aniso_xyi*rl_aniso_xyj* &
                (ida_mask(il_ji,il_jj-1)+ida_mask(il_ji,il_jj+1))* &
                (ida_mask(il_ji-1,il_jj)+ida_mask(il_ji+1,il_jj)) )
!                                                                                 
          END IF
        END DO  ! end horizontal filtering il_jj
      END DO  ! end vertical filtering il_ji
!      
      ! Est-West periodic conditions
      cla_tmp_filtred_field(1,:) = cla_tmp_filtred_field(id_Nxi-1,:) 
      cla_tmp_filtred_field(id_Nxi,:) = cla_tmp_filtred_field(2,:) 
      ! North-south periodic conditions
      cla_tmp_filtred_field(1,1) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(1,id_Nyj) = dcmplx(0.0d0,0.0d0)
      cla_tmp_filtred_field(id_Nxi,id_Nyj) = dcmplx(0.0d0,0.0d0)
      DO il_jxi = 2, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi, 1 ) = dcmplx(0.0d0,0.0d0)
        cla_tmp_filtred_field(il_jxi,id_Nyj) = cla_tmp_filtred_field(il_ijdt,id_Nyj-2)
      ENDDO
      DO il_jxi = id_Nxi/2+1, id_Nxi
        il_ijdt = id_Nxi-il_jxi+2
        cla_tmp_filtred_field(il_jxi,id_Nyj-1) = cla_tmp_filtred_field(il_ijdt,id_Nyj-1)
      ENDDO
!
      ! Incrementation : Shapiro filter's iteration
      cla_tmp_field(:,:) = cla_tmp_filtred_field(:,:)       
!
    END DO !! end the filter iterations
!
    rda_filtred_field=dreal(cla_tmp_filtred_field)
!    
    write(*,*),
    write(*,*),'imaginary part : ',MAXVAL(ABS(dimag(cla_tmp_filtred_field)))
    write(*,*),
!      
  END SUBROUTINE SHFO_8mofilt2D_ideal_anisot
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
!! ##############################################################################
!
!
!
  SUBROUTINE Sobel_filt2D_isot(rda_field,id_Nxi,id_Nyj, &
                               ida_mask,rda_filtred_field)
    !! This filter approximates the gradient with a 2nd order finite differences. 
    !! Then it computes the gradient amplitude (intensity of variation)
    !! Furthermore, this filter does not shift the phase. it can be considered
    !! better than the Prewitt filter since more important weights is given to 
    !! the neibouring (vertical and horizontal) cells (pixels), so it ressembles 
    !! to a gaussien weigths (It approximates better the first derivate). But 
    !! its drawback:  it costs 2 more operations than this last, and in the image 
    !! processing discipline, it thikens the contours (twice a 1st order gradient 
    !! approximation -> Roberts filters).
    !! Implicitly, the Sobel filter implies a smoothing operator.
    !! Stencil : 
    !!    | |  | |  | |
    !!    | |  | |  | |
    !!    | |  | |  | |
    !! it is observed that the Prewitt NL extracts more curved contours, whereas,
    !! the Sobel NL square shape outlines.
    !! Remark : Noise can be amplified !!
    !! The filtred results are normalized (*1/4)
       
    IMPLICIT NONE
                
    INTEGER,                              INTENT(IN)   :: id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask  
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    REAL(kind=8) 	                               :: rl_gx,rl_gy
    INTEGER                                            :: il_ji,il_jj,il_test
!
!
    !! Convolution loop    
    DO il_jj = 2,id_Nyj-1  
      DO il_ji = 2,id_Nxi-1
        il_test = ida_mask(il_ji,il_jj)*ida_mask(il_ji-1,il_jj)*ida_mask(il_ji+1,il_jj)* &
          ida_mask(il_ji,il_jj-1)*ida_mask(il_ji,il_jj+1)*ida_mask(il_ji-1,il_jj-1)* &
          ida_mask(il_ji-1,il_jj+1)*ida_mask(il_ji+1,il_jj-1)*ida_mask(il_ji+1,il_jj+1)
        IF (il_test.EQ.1)  THEN 
          !! Gradient compute
          rl_gx = ((rda_field(il_ji+1,il_jj+1)+ 2*rda_field(il_ji+1,il_jj)+rda_field(il_ji+1,il_jj-1)) - &
            (rda_field(il_ji-1,il_jj+1)+2*rda_field(il_ji-1,il_jj)+rda_field(il_ji-1,il_jj-1)))/4.
          rl_gy = ((rda_field(il_ji-1,il_jj-1)+ 2*rda_field(il_ji,il_jj-1)+rda_field(il_ji+1,il_jj-1)) - &
            (rda_field(il_ji-1,il_jj+1)+ 2*rda_field(il_ji,il_jj+1)+rda_field(il_ji+1,il_jj+1)))/4.
          !! Amplitude gradient process. It could be processes by the approximation: 
          !! rda_filtred_field(il_ji,il_jj) = abs(rl_gx)+abs(rl_gy)
          !! rda_filtred_field(il_ji,il_jj) = max(rl_gx,rl_gy) Non linear filter
          rda_filtred_field(il_ji,il_jj) = sqrt((rl_gx*rl_gx) + (rl_gy*rl_gy))
          !! the direction of the first derivative could be computed as:
          !! rl_direction(il_ji,il_jj) = atan(rl_gy/rl_gx)          
        ELSE
          rda_filtred_field(il_ji,il_jj) = 0.e0
        END IF        
      END DO
    END DO  
!                
  END SUBROUTINE Sobel_filt2D_isot
!******************************************************************************
!******************************************************************************
!******************************************************************************
! 
  SUBROUTINE Prewitt_filt2D_isot(rda_field,id_Nxi,id_Nyj, &
                               ida_mask,rda_filtred_field)
    !! This filter approximates the gradient with a 2nd order finite differences. 
    !! Then it computes the gradient amplitude (intensity of variation). Plus,
    !! it can also compute the direction gradient.
    !! Furthermore, comparing to the sobel filter, it takes into consideration 
    !! the neigbouring cells at, rather, the same weights.
    !! it is observed that the Prewitt NL extracts more curved contours, whereas,
    !! the Sobel NL square shape outlines.
    !! The filtred results are normalized (*1/3)
    !! Remark : Noise can be amplified !
    IMPLICIT NONE
                
    INTEGER,                              INTENT(IN)   :: id_Nxi,id_Nyj
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: ida_mask  
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(IN)   :: rda_field
    REAL(kind=4),DIMENSION(id_Nxi,id_Nyj),INTENT(OUT)  :: rda_filtred_field
    REAL(kind=8) 	                               :: rl_gx,rl_gy
    INTEGER                                            :: il_ji,il_jj,il_test
!
!
    !! Convolution loop    
    DO il_jj = 2,id_Nyj-1  
      DO il_ji = 2,id_Nxi-1
        il_test = ida_mask(il_ji,il_jj)*ida_mask(il_ji-1,il_jj)*ida_mask(il_ji+1,il_jj)* &
          ida_mask(il_ji,il_jj-1)*ida_mask(il_ji,il_jj+1)*ida_mask(il_ji-1,il_jj-1)* &
          ida_mask(il_ji-1,il_jj+1)*ida_mask(il_ji+1,il_jj-1)*ida_mask(il_ji+1,il_jj+1)
        IF (il_test.EQ.1)  THEN 
          !! Gradient compute
          rl_gx = ((rda_field(il_ji+1,il_jj+1)+ rda_field(il_ji+1,il_jj)+rda_field(il_ji+1,il_jj-1)) - &
            (rda_field(il_ji-1,il_jj+1)+rda_field(il_ji-1,il_jj)+rda_field(il_ji-1,il_jj-1)))/3.
          rl_gy = ((rda_field(il_ji-1,il_jj-1)+ rda_field(il_ji,il_jj-1)+rda_field(il_ji+1,il_jj-1)) - &
            (rda_field(il_ji-1,il_jj+1)+ rda_field(il_ji,il_jj+1)+rda_field(il_ji+1,il_jj+1)))/3.
          !! Amplitude gradient process. It could be processes by the approximation: 
          !! rda_filtred_field(il_ji,il_jj) = abs(rl_gx)+abs(rl_gy)
          !! rda_filtred_field(il_ji,il_jj) = max(rl_gx,rl_gy) Non linear filter
          rda_filtred_field(il_ji,il_jj) = sqrt((rl_gx*rl_gx) + (rl_gy*rl_gy))
          !! the direction of the first derivative could be computed as:
          !! rl_direction(il_ji,il_jj) = atan(rl_gy/rl_gx)
        ELSE
          rda_filtred_field(il_ji,il_jj) = 0.e0
        END IF        
      END DO
    END DO  
!                
  END SUBROUTINE Prewitt_filt2D_isot
!******************************************************************************
!******************************************************************************
!******************************************************************************
!
                
 

END MODULE MCAL_SHFO
