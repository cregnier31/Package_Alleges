/** @mainpage MFT Documentation
*   \image  html /home/cregnier/SVN/mo/mo/LIB/LIB_MFT/branches/V3.5/doc/images/logo01.gif
* @author C.REGNIER 
* @section intro Introduction
*  Ce document est un descriptif de la librairie fortran MFT (Mercator Fortran Toolbox). Cette librairie
* regroupe deux sous librairie : MIOL (Mercator Input/Output Library), et MCAL (Mercator Calcul Library).
* - MIOL 
*  Cette librairie nous permet de lire et d'ecrire des variables et des fichiers au format NETCDF. 
*  Elle permet de lire et d'écrire tout type de donnée (R4,R8,Int,Short,Byte,Char). La construction
* de la librairie à base d'interfaces permet à l'utilisateur d'avoir un mode d'utilisation simple de cet outil. 
* Ainsi pour lire n'importe quel type de données il faudra utiliser l'interface MIOL_read_field_NC et pour 
* ecrire MIOL_write_field_NC. L'interface fera le lien avec la bonne fonction.
*-# Schematisation de la librairie 
* \image html /home/cregnier/SVN/mo/mo/LIB/LIB_MFT/branches/V3.5/doc/images/MIOL_diag.jpg "Diagramme fonctionnel de MIOL" width=5cm
* - MCAL
*  Cette librairie regroupe des fonctions de calcul et de conversion.
* Si vous souhaitez participer à la librairie il faut utilser les normes de codage suivants 
* <A HREF="http://www-glast.slac.stanford.edu/software/CodeHowTo/codeStandards.html"> Coding Standards </A>
*
*
* If using the code in this package as an example - please modify the comments
* as appropriate for your own specific code.
* <hr>
* @section Exemples 
*<hr>
* @subsection  In the Fortran 90 program:
\code USE MIOL \n
USE MCAL
 \endcode
* @subsection  In the Makefile:
\code
MFT_INC = /home/cregnier/LIB/MFT3.5/intel/include
MFT_LIB = -L/home/cregnier/LIB/MFT3.5/intel/lib -lmft
FLAGS = .... -I$(MFT_INC) $(MFT_LIB)
\endcode

* @subsection Code exemple
 \code
PROGRAM 

use MIOL
implicit none

character(len=255) :: cl_filename
character(len=255) :: cl_varname
integer :: il_file_id
real(kind=4), dimension(:,:,:), pointer :: rlpa_tempvalues, rlpa_salvalues
integer, dimension(3) :: ila_dimlen

cl_filename = '/home/mercator/versions64/calval/files/mercatorPsy3v1R1v_nat_mean_20061031_R20061018.nc'
  
call MIOL_openr_file_NC(cl_filename, il_file_id)

cl_varname = 'salinity'
call MIOL_read_field_NC(il_file_id, cl_varname, rlpa_salvalues, ila_dimlen)

cl_varname = 'temperature'
call MIOL_read_field_NC(il_file_id, cl_varname, rlpa_tempvalues, ila_dimlen)

call MIOL_close_file_NC(il_file_id)

...
...


DEALLOCATE(rlpa_salvalues)
DEALLOCATE(rlpa_tempvalues)  

END PROGRAM
 \endcode
** <hr>
* @section notes release.notes
* <hr>
* @section requirements requirements
* @verbinclude requirements
* <hr> 
** @todo Give each todo item its own line
*
*/
