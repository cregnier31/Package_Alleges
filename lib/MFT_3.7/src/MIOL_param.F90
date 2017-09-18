!> \brief Module which contain global parameters for MIOL and MCAL library
!! \author C.REGNIER Miol V3.1
!! \date September 2008 
!!  \version 3.1
!<
!///Ce module regroupe les variables globales qui sont utiles à MIOL  

MODULE MIOL_PARAM
use netcdf
IMPLICIT NONE

!REAL(KIND=4) ::       rl_fillvalue = 1.E+35 ! < fillvalue in R4
REAL(KIND=4) ::       rg_fillvalue = nf90_fill_real4 ! < fillvalue in R4
REAL(KIND=4) ::       rg_fillvalue2 = 1.E+34 ! < fillvalue in R4
REAL(KIND=4) ::       rg_flagvalue= -999999 ! < flagvalue in R4
REAL(KIND=4) ::       rg_missvalue = nf90_fill_real4 ! < fillvalue in R4

REAL(KIND=8) ::       rg_flagvalue_R8= -999999 ! < flagvalue in R8
REAL(KIND=8) ::       rg_fillvalue_R8 = nf90_fill_real8 ! < fillvalue in R8
REAL(KIND=8) ::       rg_missvalue_R8 = nf90_fill_real8 ! < fillvalue in R8
INTEGER(KIND=2)      :: il_fillvalue =  nf90_fill_short ! < fillvalue in short
INTEGER(KIND=2)      :: il_fillvalue_I2 = nf90_fill_int2 ! < fillvalue in short
INTEGER(KIND=4)      :: il_fillvalue_I4 = nf90_fill_int  ! < fillvalue in  int 4
#if defined Key_Byte
INTEGER(KIND=1)      :: il_fillvalue_I1 =  nf90_fill_byte ! < fillvalue in Byte
#endif 
CHARACTER(LEN=255), PARAMETER  :: cp_miolParameterPath='/home/cregnier/DEV/Package_alleges/INSTALL/intel/param'
CHARACTER(LEN=255), PARAMETER  :: &
	cp_miolParameterFile='/home/cregnier/DEV/Package_alleges/INSTALL/intel/include/miolParameterPaths.nml'
CHARACTER(LEN=255) :: cl_miolUserParameterFile=''
CHARACTER(LEN=255),PARAMETER :: cl_miolUser2ParameterFile=''
CHARACTER(LEN=255),PARAMETER :: cl_miolUser3ParameterFile=''

INTEGER, PARAMETER :: Single = selected_int_kind(precision(1.e0))
INTEGER, PARAMETER :: Double = selected_int_kind(precision(1.d0))
! Single precision IEEE values
INTEGER(Single), PARAMETER :: sNaN    = Z"7FC00000"
INTEGER(Single), PARAMETER :: sPosInf = Z"7F800000"
INTEGER(Single), PARAMETER :: sNegInf = Z"FF800000"

! Double precision IEEE values
INTEGER(Double), PARAMETER :: dNaN    = Z"7FF8000000000000"
INTEGER(Double), PARAMETER :: dPosInf = Z"7FF0000000000000"
INTEGER(Double), PARAMETER :: dNegInf = Z"FFF0000000000000"

! Locatation of single and double precision sign bit (Intel)
   ! Subtract one because bit numbering starts at zero
   integer, parameter :: SPSB = bit_size(sNaN) - 1
   integer, parameter :: DPSB = bit_size(dNaN) - 1



END MODULE MIOL_PARAM
