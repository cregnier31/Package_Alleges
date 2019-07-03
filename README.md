# Package Allégés                                                                                                                                

## 1. Installation du code

Le code nécessite l'intallation de code fortran compilés avec intel ou gfortran

Tout d'abord il faut installer la dépendance suivante:
* Librairie MFT

### Description de la lib MFT
Librairie maison fournissant une interface simplifiée pour la lecture écriture des fichiers Netcdf de tout type (int,short,byte,float,double) et de toute dimensions (1-5D)
### Compilation de la lib MFT
```bash
cd lib/MFT_3.7/src
make clean && make all
```
Auparavant il faut modifier les path d'install dans le fichier make_intel_px.macro situé dans le répertoire macro :
```bash
MFT_DIR=/home/cregnier/DEV/Package_alleges/lib/MFT_3.7
INSTALL_DIR=/home/cregnier/DEV/Package_alleges/INSTALL/intel
```
Compiler le code principal :
```bash
cd src 
make clean && make
```

## 2. Programme de conversion

Aller dans le répertoire exe et lancer le code python Convert_nc.py 

En entrée le programme prend 2 arguments : le nom du fichier à convertir et la résolution du fichier d'entrée par exemple: 

```bash
python Convert_nc.py filename orca12
```
Le programme de lancement python ne nécessite pas de module particulier

