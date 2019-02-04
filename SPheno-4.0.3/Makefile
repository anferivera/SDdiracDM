modname = SDdiracDM  # as an example  
PreDef = -DGENERATIONMIXING -DONLYDOUBLE
# setting various paths  
InDir = ../include
Mdir = ${InDir}
VPATH = 3-Body-Decays:LoopDecays:TwoLoopMasses:Observables:SM 
name = ../lib/libSPhenoSDdiracDM.a
 
# check if SARAH module and SPheno are compatibel  
minV=330.00 
cVersion =$(shell expr $(version) \>= $(minV))
#  
# options for various compilers  
#  
# Default Compiler  
F90=gfortran
comp= -c -O -module ${Mdir} -I${InDir}  
LFlagsB= -O  
# Intels ifort,debug modus  
ifeq (${F90},ifortg)  
F90=ifort  
comp= -c -g -module ${Mdir} -I${InDir}  
LFlagsB= -g  
endif  
# gfortran  
ifeq (${F90},gfortran)  
comp= -c -g -ffree-line-length-none -J${Mdir} -I${InDir}  
LFlagsB= -g  
endif  
# g95  
ifeq (${F90},g95)  
comp= -c -O -fmod=${Mdir} -I${InDir}  
LFlagsB= -O  
endif  
# Lahey F95 compiler  
ifeq (${F90},lf95)  
comp=-c -O -M ${Mdir} -I${InDir}  
LFlagsB=-O  
endif  
# NAG f95/2003  
ifeq (${F90},nagfor)  
comp= -c -O -mdir ${Mdir} -I${InDir}  
LFlagsB= -O -DONLYDOUBLE -mdir ${MDir} -I${InDir}  
endif   
.SUFFIXES : .o .ps .f90 .F90 .a 
bin/SPhenoSDdiracDM:
ifeq (${cVersion},1)
	 cd ../src ; ${MAKE} F90=${F90} 
	 ${MAKE} F90=${F90} ${name} 
	 ${MAKE} F90=${F90} SPhenoSDdiracDM.o 
	 ${F90} -o SPhenoSDdiracDM ${LFlagsB} SPhenoSDdiracDM.o ../lib/libSPhenoSDdiracDM.a ../lib/libSPheno.a
	 mv SPhenoSDdiracDM ../bin
	 rm SPhenoSDdiracDM.o  
${name}:  ${name}(Settings.o) ${name}(Model_Data_SDdiracDM.o)  \
 ${name}(RGEs_SDdiracDM.o)   \
 ${name}(Couplings_SDdiracDM.o) ${name}(TreeLevelMasses_SDdiracDM.o) ${name}(TadpoleEquations_SDdiracDM.o) \
 ${name}(LoopCouplings_SDdiracDM.o) ${name}(CouplingsForDecays_SDdiracDM.o) \
 ${name}(TreeLevel_Decays_SDdiracDM.o) \
 ${name}(Fu.o)  ${name}(Fe.o)  ${name}(Fd.o)  \
${name}(AddLoopFunctions.o) ${name}(Kinematics.o) \
 ${name}(2LPoleFunctions.o) ${name}(2LPole_SDdiracDM.o) \
 ${name}(LoopMasses_SDdiracDM.o) \
 ${name}(RGEs_SM_HC.o) ${name}(Couplings_SM_HC.o) ${name}(TreeLevelMasses_SM_HC.o) ${name}(LoopMasses_SM_HC.o)   \
${name}(CouplingsCT_SDdiracDM.o) \
${name}(Bremsstrahlung.o) ${name}(DecayFFS.o) ${name}(DecayFFV.o) ${name}(DecaySSS.o) ${name}(DecaySSV.o) ${name}(DecaySFF.o) ${name}(DecaySVV.o) \
 ${name}(LoopDecayFu_SDdiracDM.o)  ${name}(LoopDecayFe_SDdiracDM.o)  ${name}(LoopDecayFd_SDdiracDM.o)  ${name}(LoopDecayhh_SDdiracDM.o)  ${name}(LoopDecaySsc_SDdiracDM.o)  ${name}(LoopDecayFxe_SDdiracDM.o)  ${name}(LoopDecayFxv_SDdiracDM.o) \
 ${name}(Wrapper_LoopDecay_Fu_SDdiracDM.o)  ${name}(Wrapper_LoopDecay_Fe_SDdiracDM.o)  ${name}(Wrapper_LoopDecay_Fd_SDdiracDM.o)  ${name}(Wrapper_LoopDecay_hh_SDdiracDM.o)  ${name}(Wrapper_LoopDecay_Ssc_SDdiracDM.o)  ${name}(Wrapper_LoopDecay_Fxe_SDdiracDM.o)  ${name}(Wrapper_LoopDecay_Fxv_SDdiracDM.o) ${name}(OneLoopDecays_SDdiracDM.o) \
 ${name}(BranchingRatios_SDdiracDM.o) ${name}(HiggsCS_SDdiracDM.o) ${name}(RunSM_SDdiracDM.o) \
 ${name}(LowEnergy_SDdiracDM.o) \
${name}(FlavorKit_LFV_SDdiracDM.o) ${name}(FlavorKit_QFV_SDdiracDM.o) ${name}(FlavorKit_Observables_SDdiracDM.o)\
 ${name}(Boundaries_SDdiracDM.o)  ${name}(InputOutput_SDdiracDM.o) 
else 
	 @echo -------------------------------------------------------------------  
	 @echo ERROR:  
	 @echo The installed SPheno is version not compatibel with this module 
	 @echo Please, upgrade at least to SPheno version 3.3.0.  
	 @echo The current SPheno version can be downloaded from 
	 @echo http://www.hepforge.org/downloads/spheno 
	 @echo ------------------------------------------------------------------- 
endif 
clean: 
	 rm -f *.o *~ */*.o */*~
cleanall: 
	 rm -f bin/SPheno3 lib/*.a *~ */*.o */*~ include/*
#
# Suffix rules
#
.f90.a:
	 ${F90} ${comp} $< 
	 ar -ruc -U $@ $*.o
	 rm -f $*.o
.F90.a:
	 ${F90} ${comp} ${PreDef} $< 
	 ar -ruc -U $@ $*.o
	 rm -f $*.o
.f90.o:
	 ${F90} ${comp} $<
.f90.ps:
	 a2ps -o $*.ps $<
.h.ps:
	 a2ps -o $*.ps $<
