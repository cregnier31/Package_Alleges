!> \brief Global Module for read all the interfaces
!! \author C.REGNIER Miol V3.2
!! \date Janvier 2010
!!  \version 3.2
!<
MODULE MIOL
USE INT_ATTRIBUTSTYPE
USE INT_read_write_R4
USE INT_read_write_R8
USE INT_read_write_S
#if defined Key_Byte
USE INT_read_write_B
#endif 
USE INT_read_write_Integer
USE INT_read_write_Integer8
USE INT_read_write_C
USE INT_read_write_ATTLIST
USE INT_INQUIRE
USE MIOL_util
USE MIOL_param
USE MIOL_OPEN_CLOSE 
USE INT_ATTRIBUTS
END MODULE MIOL
