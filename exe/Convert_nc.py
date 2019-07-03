#!/usr/bin/python
## Program to convert lighout file in netcdf
## C.REGNIER  2017
import subprocess,shlex 
import sys,os

if len(sys.argv) != 3 :
    print 'Usage: Convert_nc.py <filename> <resol>'
    print 'example: python Convert_nc.py TN.B.d20130925.end.50.0 orca12'
    print 'example: python Convert_nc.py TN.B.d20130925.end.75.0 orca025'
    sys.exit(1)
else :
    filename=sys.argv[1]
    typeout=sys.argv[2]
if typeout == "orca12":
    meshfile="../static/coord_ORCA12.nc"
elif typeout == "orca025":
    meshfile="../static/coord_ORCA25.nc"
else:
    print('Typeout not known')
    print('orca12 , orca025...')

resol="3D"
sampling="3"
filename_tmp=filename.split('.')[0]

if filename_tmp.startswith('VAR'): 
    filename_fin=filename_tmp.split('_')[1]
    if filename_fin == "SN" : 
        varname="salinity"
    elif filename_fin == "TN" : 
        varname="temperature"
    elif filename_fin == "UN" : 
        varname="u"
    elif filename_fin == "VN" : 
        varname="v"
    else:
        print "Variable %s not known " %(filename_fin)

elif filename_tmp.startswith('TN'): 
    varname="temperature"
else: 
    varname=filename_tmp
print (varname,resol,meshfile,sampling)
SUBEXE="../bin/read_write_lightout.exe"
code_launch='%s -i %s -d %s -v %s -m %s -s %s' %(SUBEXE,filename,resol,varname,meshfile,sampling)
print code_launch
args   = shlex.split(code_launch)
print(args)
try :   subprocess.check_call(args)
except :
    print 'Launch %s failed'
    sys.exit(1)
