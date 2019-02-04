! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:24 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module CouplingsForDecays_SDdiracDM
 
Use Control 
Use Settings 
Use Model_Data_SDdiracDM 
Use Couplings_SDdiracDM 
Use LoopCouplings_SDdiracDM 
Use Tadpoles_SDdiracDM 
 Use TreeLevelMasses_SDdiracDM 
Use Mathematics, Only: CompareMatrices, Adjungate 
 
Use StandardModel 
Contains 
 
 
 
Subroutine CouplingsFor_Fu_decays_2B(m_in,i1,MAhinput,MAh2input,MFdinput,             & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,MFvinput,MFv2input,MFxeinput,          & 
& MFxe2input,MFxvinput,MFxv2input,Mhhinput,Mhh2input,MHpinput,MHp2input,MSscinput,       & 
& MSsc2input,MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,          & 
& ZURinput,ZDLinput,ZELinput,VSsinput,ZULinput,UVinput,UVRinput,XUinput,XVinput,         & 
& ZHinput,ZWinput,ZZinput,alphaHinput,g1input,g2input,g3input,Laminput,LS1Hinput,        & 
& LSinput,LS2Hinput,LSPinput,LSPHinput,Yuinput,Ydinput,Yeinput,YRDinput,YRB1input,       & 
& YRB2input,YRCinput,YRA1input,YRA2input,MDFinput,m2SMinput,MS12input,MS22input,         & 
& mP2input,vvSMinput,vSinput,cplcFuFdVWpL,cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,          & 
& cplcFuFuVZL,cplcFuFuVZR,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,LS1Hinput,LSinput,LS2Hinput,LSPinput,LSPHinput,               & 
& YRDinput,YRB1input(3),YRB2input(3),YRCinput,YRA1input(3),YRA2input(3),MDFinput,        & 
& m2SMinput,MS12input,MS22input,mP2input,vvSMinput,vSinput

Complex(dp),Intent(in) :: Laminput,Yuinput(3,3),Ydinput(3,3),Yeinput(3,3)

Real(dp),Intent(in) :: MAhinput,MAh2input,MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),MFuinput(3),     & 
& MFu2input(3),MFvinput(3),MFv2input(3),MFxeinput,MFxe2input,MFxvinput(2),               & 
& MFxv2input(2),Mhhinput(2),Mhh2input(2),MHpinput,MHp2input,MSscinput(2),MSsc2input(2),  & 
& MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,VSsinput(2,2),ZHinput(2,2),            & 
& ZZinput(2,2),alphaHinput

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& UVinput(3,3),UVRinput(3,3),XUinput(2,2),XVinput(2,2),ZWinput(2,2)

Real(dp) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS

Complex(dp) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Complex(dp),Intent(out) :: cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),            & 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3)

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_Fu_2B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam = Laminput 
LS1H = LS1Hinput 
LS = LSinput 
LS2H = LS2Hinput 
LSP = LSPinput 
LSPH = LSPHinput 
Yu = Yuinput 
Yd = Ydinput 
Ye = Yeinput 
YRD = YRDinput 
YRB1 = YRB1input 
YRB2 = YRB2input 
YRC = YRCinput 
YRA1 = YRA1input 
YRA2 = YRA2input 
MDF = MDFinput 
m2SM = m2SMinput 
MS12 = MS12input 
MS22 = MS22input 
mP2 = mP2input 
vvSM = vvSMinput 
vS = vSinput 
Qin=sqrt(getRenormalizationScale()) 
If (m_in.le.Qin) Then 
  If (m_in.le.mz) Then 
Call RunSMohdm(mz,deltaM, g1,g2,g3,Yu,Yd,Ye,vvSM) 
  Else 
Call RunSMohdm(m_in,deltaM, g1,g2,g3,Yu,Yd,Ye,vvSM) 
  End if 
End if 
Call SolveTadpoleEquations(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,               & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,(/ ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,              & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,            & 
& ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& .True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
VSs = VSsinput 
UV = UVinput 
UVR = UVRinput 
XU = XUinput 
XV = XVinput 
ZH = ZHinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
MFv = MFvinput 
MFv2 = MFv2input 
MFxe = MFxeinput 
MFxe2 = MFxe2input 
MFxv = MFxvinput 
MFxv2 = MFxv2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHp = MHpinput 
MHp2 = MHp2input 
MSsc = MSscinput 
MSsc2 = MSsc2input 
MVWp = MVWpinput 
MVWp2 = MVWp2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
End if 
cplcFuFuhhL = 0._dp 
cplcFuFuhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFuFuhhT(gt1,gt2,gt3,Yu,ZH,ZUL,ZUR,cplcFuFuhhL(gt1,gt2,gt3)              & 
& ,cplcFuFuhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFdVWpL = 0._dp 
cplcFuFdVWpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFdVWpT(gt1,gt2,g2,ZDL,ZUL,cplcFuFdVWpL(gt1,gt2),cplcFuFdVWpR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVZL = 0._dp 
cplcFuFuVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVZT(gt1,gt2,g1,g2,TW,cplcFuFuVZL(gt1,gt2),cplcFuFuVZR(gt1,gt2))

 End Do 
End Do 


Iname = Iname - 1 
 
End subroutine CouplingsFor_Fu_decays_2B
 
Subroutine CouplingsFor_Fe_decays_2B(m_in,i1,MAhinput,MAh2input,MFdinput,             & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,MFvinput,MFv2input,MFxeinput,          & 
& MFxe2input,MFxvinput,MFxv2input,Mhhinput,Mhh2input,MHpinput,MHp2input,MSscinput,       & 
& MSsc2input,MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,          & 
& ZURinput,ZDLinput,ZELinput,VSsinput,ZULinput,UVinput,UVRinput,XUinput,XVinput,         & 
& ZHinput,ZWinput,ZZinput,alphaHinput,g1input,g2input,g3input,Laminput,LS1Hinput,        & 
& LSinput,LS2Hinput,LSPinput,LSPHinput,Yuinput,Ydinput,Yeinput,YRDinput,YRB1input,       & 
& YRB2input,YRCinput,YRA1input,YRA2input,MDFinput,m2SMinput,MS12input,MS22input,         & 
& mP2input,vvSMinput,vSinput,cplcFeFehhL,cplcFeFehhR,cplcFeFeVZL,cplcFeFeVZR,            & 
& cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,cplcFeFxecSscR,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,LS1Hinput,LSinput,LS2Hinput,LSPinput,LSPHinput,               & 
& YRDinput,YRB1input(3),YRB2input(3),YRCinput,YRA1input(3),YRA2input(3),MDFinput,        & 
& m2SMinput,MS12input,MS22input,mP2input,vvSMinput,vSinput

Complex(dp),Intent(in) :: Laminput,Yuinput(3,3),Ydinput(3,3),Yeinput(3,3)

Real(dp),Intent(in) :: MAhinput,MAh2input,MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),MFuinput(3),     & 
& MFu2input(3),MFvinput(3),MFv2input(3),MFxeinput,MFxe2input,MFxvinput(2),               & 
& MFxv2input(2),Mhhinput(2),Mhh2input(2),MHpinput,MHp2input,MSscinput(2),MSsc2input(2),  & 
& MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,VSsinput(2,2),ZHinput(2,2),            & 
& ZZinput(2,2),alphaHinput

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& UVinput(3,3),UVRinput(3,3),XUinput(2,2),XVinput(2,2),ZWinput(2,2)

Real(dp) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS

Complex(dp) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Complex(dp),Intent(out) :: cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),              & 
& cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2)

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_Fe_2B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam = Laminput 
LS1H = LS1Hinput 
LS = LSinput 
LS2H = LS2Hinput 
LSP = LSPinput 
LSPH = LSPHinput 
Yu = Yuinput 
Yd = Ydinput 
Ye = Yeinput 
YRD = YRDinput 
YRB1 = YRB1input 
YRB2 = YRB2input 
YRC = YRCinput 
YRA1 = YRA1input 
YRA2 = YRA2input 
MDF = MDFinput 
m2SM = m2SMinput 
MS12 = MS12input 
MS22 = MS22input 
mP2 = mP2input 
vvSM = vvSMinput 
vS = vSinput 
Qin=sqrt(getRenormalizationScale()) 
If (m_in.le.Qin) Then 
  If (m_in.le.mz) Then 
Call RunSMohdm(mz,deltaM, g1,g2,g3,Yu,Yd,Ye,vvSM) 
  Else 
Call RunSMohdm(m_in,deltaM, g1,g2,g3,Yu,Yd,Ye,vvSM) 
  End if 
End if 
Call SolveTadpoleEquations(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,               & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,(/ ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,              & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,            & 
& ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& .True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
VSs = VSsinput 
UV = UVinput 
UVR = UVRinput 
XU = XUinput 
XV = XVinput 
ZH = ZHinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
MFv = MFvinput 
MFv2 = MFv2input 
MFxe = MFxeinput 
MFxe2 = MFxe2input 
MFxv = MFxvinput 
MFxv2 = MFxv2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHp = MHpinput 
MHp2 = MHp2input 
MSsc = MSscinput 
MSsc2 = MSsc2input 
MVWp = MVWpinput 
MVWp2 = MVWp2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
End if 
cplcFeFehhL = 0._dp 
cplcFeFehhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFeFehhT(gt1,gt2,gt3,Ye,ZH,ZEL,ZER,cplcFeFehhL(gt1,gt2,gt3)              & 
& ,cplcFeFehhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFxecSscL = 0._dp 
cplcFeFxecSscR = 0._dp 
Do gt1 = 1, 3
 Do gt3 = 1, 2
Call CouplingcFeFxecSscT(gt1,gt3,YRA1,YRA2,ZEL,VSs,cplcFeFxecSscL(gt1,gt3)            & 
& ,cplcFeFxecSscR(gt1,gt3))

 End Do 
End Do 


cplcFeFeVZL = 0._dp 
cplcFeFeVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFeFeVZT(gt1,gt2,g1,g2,TW,cplcFeFeVZL(gt1,gt2),cplcFeFeVZR(gt1,gt2))

 End Do 
End Do 


cplcFeFvcVWpL = 0._dp 
cplcFeFvcVWpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFeFvcVWpT(gt1,gt2,g2,ZEL,UV,cplcFeFvcVWpL(gt1,gt2),cplcFeFvcVWpR(gt1,gt2))

 End Do 
End Do 


Iname = Iname - 1 
 
End subroutine CouplingsFor_Fe_decays_2B
 
Subroutine CouplingsFor_Fd_decays_2B(m_in,i1,MAhinput,MAh2input,MFdinput,             & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,MFvinput,MFv2input,MFxeinput,          & 
& MFxe2input,MFxvinput,MFxv2input,Mhhinput,Mhh2input,MHpinput,MHp2input,MSscinput,       & 
& MSsc2input,MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,          & 
& ZURinput,ZDLinput,ZELinput,VSsinput,ZULinput,UVinput,UVRinput,XUinput,XVinput,         & 
& ZHinput,ZWinput,ZZinput,alphaHinput,g1input,g2input,g3input,Laminput,LS1Hinput,        & 
& LSinput,LS2Hinput,LSPinput,LSPHinput,Yuinput,Ydinput,Yeinput,YRDinput,YRB1input,       & 
& YRB2input,YRCinput,YRA1input,YRA2input,MDFinput,m2SMinput,MS12input,MS22input,         & 
& mP2input,vvSMinput,vSinput,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,            & 
& cplcFdFucVWpL,cplcFdFucVWpR,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,LS1Hinput,LSinput,LS2Hinput,LSPinput,LSPHinput,               & 
& YRDinput,YRB1input(3),YRB2input(3),YRCinput,YRA1input(3),YRA2input(3),MDFinput,        & 
& m2SMinput,MS12input,MS22input,mP2input,vvSMinput,vSinput

Complex(dp),Intent(in) :: Laminput,Yuinput(3,3),Ydinput(3,3),Yeinput(3,3)

Real(dp),Intent(in) :: MAhinput,MAh2input,MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),MFuinput(3),     & 
& MFu2input(3),MFvinput(3),MFv2input(3),MFxeinput,MFxe2input,MFxvinput(2),               & 
& MFxv2input(2),Mhhinput(2),Mhh2input(2),MHpinput,MHp2input,MSscinput(2),MSsc2input(2),  & 
& MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,VSsinput(2,2),ZHinput(2,2),            & 
& ZZinput(2,2),alphaHinput

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& UVinput(3,3),UVRinput(3,3),XUinput(2,2),XVinput(2,2),ZWinput(2,2)

Real(dp) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS

Complex(dp) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Complex(dp),Intent(out) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),              & 
& cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3)

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_Fd_2B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam = Laminput 
LS1H = LS1Hinput 
LS = LSinput 
LS2H = LS2Hinput 
LSP = LSPinput 
LSPH = LSPHinput 
Yu = Yuinput 
Yd = Ydinput 
Ye = Yeinput 
YRD = YRDinput 
YRB1 = YRB1input 
YRB2 = YRB2input 
YRC = YRCinput 
YRA1 = YRA1input 
YRA2 = YRA2input 
MDF = MDFinput 
m2SM = m2SMinput 
MS12 = MS12input 
MS22 = MS22input 
mP2 = mP2input 
vvSM = vvSMinput 
vS = vSinput 
Qin=sqrt(getRenormalizationScale()) 
If (m_in.le.Qin) Then 
  If (m_in.le.mz) Then 
Call RunSMohdm(mz,deltaM, g1,g2,g3,Yu,Yd,Ye,vvSM) 
  Else 
Call RunSMohdm(m_in,deltaM, g1,g2,g3,Yu,Yd,Ye,vvSM) 
  End if 
End if 
Call SolveTadpoleEquations(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,               & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,(/ ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,              & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,            & 
& ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& .True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
VSs = VSsinput 
UV = UVinput 
UVR = UVRinput 
XU = XUinput 
XV = XVinput 
ZH = ZHinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
MFv = MFvinput 
MFv2 = MFv2input 
MFxe = MFxeinput 
MFxe2 = MFxe2input 
MFxv = MFxvinput 
MFxv2 = MFxv2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHp = MHpinput 
MHp2 = MHp2input 
MSsc = MSscinput 
MSsc2 = MSsc2input 
MVWp = MVWpinput 
MVWp2 = MVWp2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
End if 
cplcFdFdhhL = 0._dp 
cplcFdFdhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFdFdhhT(gt1,gt2,gt3,Yd,ZH,ZDL,ZDR,cplcFdFdhhL(gt1,gt2,gt3)              & 
& ,cplcFdFdhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFdFdVZL = 0._dp 
cplcFdFdVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVZT(gt1,gt2,g1,g2,TW,cplcFdFdVZL(gt1,gt2),cplcFdFdVZR(gt1,gt2))

 End Do 
End Do 


cplcFdFucVWpL = 0._dp 
cplcFdFucVWpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFucVWpT(gt1,gt2,g2,ZDL,ZUL,cplcFdFucVWpL(gt1,gt2),cplcFdFucVWpR(gt1,gt2))

 End Do 
End Do 


Iname = Iname - 1 
 
End subroutine CouplingsFor_Fd_decays_2B
 
Subroutine CouplingsFor_hh_decays_2B(m_in,i1,MAhinput,MAh2input,MFdinput,             & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,MFvinput,MFv2input,MFxeinput,          & 
& MFxe2input,MFxvinput,MFxv2input,Mhhinput,Mhh2input,MHpinput,MHp2input,MSscinput,       & 
& MSsc2input,MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,          & 
& ZURinput,ZDLinput,ZELinput,VSsinput,ZULinput,UVinput,UVRinput,XUinput,XVinput,         & 
& ZHinput,ZWinput,ZZinput,alphaHinput,g1input,g2input,g3input,Laminput,LS1Hinput,        & 
& LSinput,LS2Hinput,LSPinput,LSPHinput,Yuinput,Ydinput,Yeinput,YRDinput,YRB1input,       & 
& YRB2input,YRCinput,YRA1input,YRA2input,MDFinput,m2SMinput,MS12input,MS22input,         & 
& mP2input,vvSMinput,vSinput,cplHiggsPP,cplHiggsGG,cplHiggsZZvirt,cplHiggsWWvirt,        & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplhhhhhh,cplhhSsccSsc,cplhhcVWpVWp,cplhhVZVZ,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,LS1Hinput,LSinput,LS2Hinput,LSPinput,LSPHinput,               & 
& YRDinput,YRB1input(3),YRB2input(3),YRCinput,YRA1input(3),YRA2input(3),MDFinput,        & 
& m2SMinput,MS12input,MS22input,mP2input,vvSMinput,vSinput

Complex(dp),Intent(in) :: Laminput,Yuinput(3,3),Ydinput(3,3),Yeinput(3,3)

Real(dp),Intent(in) :: MAhinput,MAh2input,MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),MFuinput(3),     & 
& MFu2input(3),MFvinput(3),MFv2input(3),MFxeinput,MFxe2input,MFxvinput(2),               & 
& MFxv2input(2),Mhhinput(2),Mhh2input(2),MHpinput,MHp2input,MSscinput(2),MSsc2input(2),  & 
& MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,VSsinput(2,2),ZHinput(2,2),            & 
& ZZinput(2,2),alphaHinput

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& UVinput(3,3),UVRinput(3,3),XUinput(2,2),XVinput(2,2),ZWinput(2,2)

Real(dp) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS

Complex(dp) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Complex(dp),Intent(out) :: cplHiggsPP(2),cplHiggsGG(2),cplHiggsZZvirt(2),cplHiggsWWvirt(2),cplcFdFdhhL(3,3,2),   & 
& cplcFdFdhhR(3,3,2),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFuFuhhL(3,3,2),           & 
& cplcFuFuhhR(3,3,2),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplhhhhhh(2,2,2),         & 
& cplhhSsccSsc(2,2,2),cplhhcVWpVWp(2),cplhhVZVZ(2)

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Complex(dp) :: ratFd(3),ratFe(3),ratFu(3),ratHp,ratVWp

Complex(dp) :: ratPFd(3),ratPFe(3),ratPFu(3),ratPHp,ratPVWp

Complex(dp) :: coup 
Real(dp) :: vev, gNLO, NLOqcd, NNLOqcd, NNNLOqcd, AlphaSQ, AlphaSQhlf 
Real(dp) :: g1SM,g2SM,g3SM,vSM
Complex(dp) ::YuSM(3,3),YdSM(3,3),YeSM(3,3)
Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_hh_2B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam = Laminput 
LS1H = LS1Hinput 
LS = LSinput 
LS2H = LS2Hinput 
LSP = LSPinput 
LSPH = LSPHinput 
Yu = Yuinput 
Yd = Ydinput 
Ye = Yeinput 
YRD = YRDinput 
YRB1 = YRB1input 
YRB2 = YRB2input 
YRC = YRCinput 
YRA1 = YRA1input 
YRA2 = YRA2input 
MDF = MDFinput 
m2SM = m2SMinput 
MS12 = MS12input 
MS22 = MS22input 
mP2 = mP2input 
vvSM = vvSMinput 
vS = vSinput 
Qin=sqrt(getRenormalizationScale()) 
If (m_in.le.Qin) Then 
  If (m_in.le.mz) Then 
Call RunSMohdm(mz,deltaM, g1,g2,g3,Yu,Yd,Ye,vvSM) 
  Else 
Call RunSMohdm(m_in,deltaM, g1,g2,g3,Yu,Yd,Ye,vvSM) 
  End if 
End if 
! Run always SM gauge couplings if present 
  Qin=sqrt(getRenormalizationScale()) 
  Call RunSMohdm(m_in,deltaM,g1SM,g2SM,g3SM,YuSM,YdSM,YeSM,vSM) 
   ! SM pole masses needed for diphoton/digluon rate 
   ! But only top and W play a role. 
   vSM=1/Sqrt((G_F*Sqrt(2._dp))) ! On-Shell VEV needed for loop 
   YuSM(3,3)=sqrt(2._dp)*mf_u(3)/vSM  ! On-Shell top needed in loop 
   ! Other running values kept to get H->ff correct 
Call SetMatchingConditions(g1SM,g2SM,g3SM,YuSM,YdSM,YeSM,vSM,vvSM,vS,g1,              & 
& g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,              & 
& m2SM,MS12,MS22,mP2,.False.)

! Run always SM gauge couplings if present 
! alphaS(mH/2) for NLO corrections to diphoton rate 
Call RunSMgauge(m_in/2._dp,deltaM, g1,g2,g3) 
AlphaSQhlf=g3**2/(4._dp*Pi) 
! alphaS(mH) for digluon rate 
Call RunSMgauge(m_in,deltaM, g1,g2,g3) 
AlphaSQ=g3**2/(4._dp*Pi) 
Call SolveTadpoleEquations(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,               & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,(/ ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,              & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,            & 
& ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& .True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
VSs = VSsinput 
UV = UVinput 
UVR = UVRinput 
XU = XUinput 
XV = XVinput 
ZH = ZHinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
cplhhhhhh = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplinghhhhhhT(gt1,gt2,gt3,Lam,LSP,LSPH,vvSM,vS,ZH,cplhhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhSsccSsc = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplinghhSsccSscT(gt1,gt2,gt3,LS1H,LS2H,vvSM,ZH,VSs,cplhhSsccSsc(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhcVWpVWp = 0._dp 
Do gt1 = 1, 2
Call CouplinghhcVWpVWpT(gt1,g2,vvSM,ZH,cplhhcVWpVWp(gt1))

End Do 


cplhhVZVZ = 0._dp 
Do gt1 = 1, 2
Call CouplinghhVZVZT(gt1,g1,g2,vvSM,ZH,TW,cplhhVZVZ(gt1))

End Do 


cplcFdFdhhL = 0._dp 
cplcFdFdhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFdFdhhT(gt1,gt2,gt3,Yd,ZH,ZDL,ZDR,cplcFdFdhhL(gt1,gt2,gt3)              & 
& ,cplcFdFdhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFehhL = 0._dp 
cplcFeFehhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFeFehhT(gt1,gt2,gt3,Ye,ZH,ZEL,ZER,cplcFeFehhL(gt1,gt2,gt3)              & 
& ,cplcFeFehhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuhhL = 0._dp 
cplcFuFuhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFuFuhhT(gt1,gt2,gt3,Yu,ZH,ZUL,ZUR,cplcFuFuhhL(gt1,gt2,gt3)              & 
& ,cplcFuFuhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFxvFxvhhL = 0._dp 
cplcFxvFxvhhR = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingcFxvFxvhhT(gt1,gt2,gt3,YRD,YRC,ZH,XV,XU,cplcFxvFxvhhL(gt1,gt2,gt3)       & 
& ,cplcFxvFxvhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


vev=1/Sqrt((G_F*Sqrt(2._dp)))
cplHiggsWWvirt = cplhhcVWpVWp/vev 
cplHiggsZZvirt = cplhhVZVZ*Sqrt(7._dp/12._dp-10._dp/9._dp*Sin(TW)**2+40._dp/27._dp*Sin(TW)**4)/vev 
 

!----------------------------------------------------
! Coupling ratios for HiggsBounds 
!----------------------------------------------------
 
rHB_S_VZ(i1) = Abs(-0.5_dp*cplhhVZVZ(i1)*vev/MVZ2) 
rHB_S_VWp(i1) = Abs(-0.5_dp*cplhhcVWpVWp(i1)*vev/MVWp2) 
Do i2=1, 3
rHB_S_S_Fd(i1,i2) = Abs((cplcFdFdhhL(i2,i2,i1)+cplcFdFdhhR(i2,i2,i1))*vev/(2._dp*MFd(i2))) 
rHB_S_P_Fd(i1,i2) = Abs((cplcFdFdhhL(i2,i2,i1)-cplcFdFdhhR(i2,i2,i1))*vev/(2._dp*MFd(i2))) 
End Do 
Do i2=1, 3
rHB_S_S_Fu(i1,i2) = Abs((cplcFuFuhhL(i2,i2,i1)+cplcFuFuhhR(i2,i2,i1))*vev/(2._dp*MFu(i2))) 
rHB_S_P_Fu(i1,i2) = Abs((cplcFuFuhhL(i2,i2,i1)-cplcFuFuhhR(i2,i2,i1))*vev/(2._dp*MFu(i2))) 
End Do 
Do i2=1, 3
rHB_S_S_Fe(i1,i2) = Abs((cplcFeFehhL(i2,i2,i1)+cplcFeFehhR(i2,i2,i1))*vev/(2._dp*MFe(i2))) 
rHB_S_P_Fe(i1,i2) = Abs((cplcFeFehhL(i2,i2,i1)-cplcFeFehhR(i2,i2,i1))*vev/(2._dp*MFe(i2))) 
End Do 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
MFv = MFvinput 
MFv2 = MFv2input 
MFxe = MFxeinput 
MFxe2 = MFxe2input 
MFxv = MFxvinput 
MFxv2 = MFxv2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHp = MHpinput 
MHp2 = MHp2input 
MSsc = MSscinput 
MSsc2 = MSsc2input 
MVWp = MVWpinput 
MVWp2 = MVWp2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
End if 
!----------------------------------------------------
! Scalar Higgs coupling ratios 
!----------------------------------------------------
 
Do i2=1, 3
ratFd(i2) = cplcFdFdhhL(i2,i2,i1)*1._dp*vev/MFd(i2) 
End Do 
Do i2=1, 3
ratFe(i2) = cplcFeFehhL(i2,i2,i1)*1._dp*vev/MFe(i2) 
End Do 
Do i2=1, 3
ratFu(i2) = cplcFuFuhhL(i2,i2,i1)*1._dp*vev/MFu(i2) 
End Do 
ratHp = 0._dp 
ratVWp = -0.5_dp*cplhhcVWpVWp(i1)*vev/MVWp2 
If (HigherOrderDiboson) Then 
  gNLO = Sqrt(AlphaSQhlf*4._dp*Pi) 
Else  
  gNLO = -1._dp 
End if 
Call CoupHiggsToPhoton(m_in,i1,ratFd,ratFe,ratFu,ratHp,ratVWp,MFd,MFe,MFu,            & 
& MHp,MVWp,gNLO,coup)

cplHiggsPP(i1) = coup*Alpha 
CoupHPP(i1) = coup 
Call CoupHiggsToPhotonSM(m_in,MFd,MFe,MFu,MHp,MVWp,gNLO,coup)

ratioPP(i1) = Abs(cplHiggsPP(i1)/(coup*Alpha)) 
  gNLO = -1._dp 
Call CoupHiggsToGluon(m_in,i1,ratFd,ratFu,MFd,MFu,gNLO,coup)

cplHiggsGG(i1) = coup*AlphaSQ 
CoupHGG(i1) = coup 
Call CoupHiggsToGluonSM(m_in,MFd,MFu,gNLO,coup)

If (HigherOrderDiboson) Then 
  NLOqcd = 12._dp*oo48pi2*(95._dp/4._dp - 7._dp/6._dp*NFlav(m_in))*g3**2 
  NNLOqcd = 0.0005785973353112832_dp*(410.52103034222284_dp - 52.326413200014684_dp*NFlav(m_in)+NFlav(m_in)**2 & 
 & +(2.6337085360233763_dp +0.7392866066030529_dp *NFlav(m_in))*Log(m_in**2/mf_u(3)**2))*g3**4 
  NNNLOqcd = 0.00017781840290519607_dp*(42.74607514668917_dp + 11.191050460173795_dp*Log(m_in**2/mf_u(3)**2) + Log(m_in**2/mf_u(3)**2)**2)*g3**6 
Else 
  NLOqcd = 0._dp 
  NNLOqcd = 0._dp 
  NNNLOqcd = 0._dp 
End if 
coup = coup*Sqrt(1._dp + NLOqcd+NNLOqcd+NNNLOqcd) 
cplHiggsGG(i1) = cplHiggsGG(i1)*Sqrt(1._dp + NLOqcd+NNLOqcd+NNNLOqcd) 
CoupHGG(i1)=cplHiggsGG(i1) 
ratioGG(i1) = Abs(cplHiggsGG(i1)/(coup*AlphaSQ)) 
If (i1.eq.1) Then 
CPL_A_H_Z = 0 
CPL_H_H_Z = 0._dp 
End if 
cplhhhhhh = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplinghhhhhhT(gt1,gt2,gt3,Lam,LSP,LSPH,vvSM,vS,ZH,cplhhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhSsccSsc = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplinghhSsccSscT(gt1,gt2,gt3,LS1H,LS2H,vvSM,ZH,VSs,cplhhSsccSsc(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhcVWpVWp = 0._dp 
Do gt1 = 1, 2
Call CouplinghhcVWpVWpT(gt1,g2,vvSM,ZH,cplhhcVWpVWp(gt1))

End Do 


cplhhVZVZ = 0._dp 
Do gt1 = 1, 2
Call CouplinghhVZVZT(gt1,g1,g2,vvSM,ZH,TW,cplhhVZVZ(gt1))

End Do 


cplcFdFdhhL = 0._dp 
cplcFdFdhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFdFdhhT(gt1,gt2,gt3,Yd,ZH,ZDL,ZDR,cplcFdFdhhL(gt1,gt2,gt3)              & 
& ,cplcFdFdhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFehhL = 0._dp 
cplcFeFehhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFeFehhT(gt1,gt2,gt3,Ye,ZH,ZEL,ZER,cplcFeFehhL(gt1,gt2,gt3)              & 
& ,cplcFeFehhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuhhL = 0._dp 
cplcFuFuhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFuFuhhT(gt1,gt2,gt3,Yu,ZH,ZUL,ZUR,cplcFuFuhhL(gt1,gt2,gt3)              & 
& ,cplcFuFuhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFxvFxvhhL = 0._dp 
cplcFxvFxvhhR = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingcFxvFxvhhT(gt1,gt2,gt3,YRD,YRC,ZH,XV,XU,cplcFxvFxvhhL(gt1,gt2,gt3)       & 
& ,cplcFxvFxvhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


Iname = Iname - 1 
 
End subroutine CouplingsFor_hh_decays_2B
 
Subroutine CouplingsFor_Ssc_decays_2B(m_in,i1,MAhinput,MAh2input,MFdinput,            & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,MFvinput,MFv2input,MFxeinput,          & 
& MFxe2input,MFxvinput,MFxv2input,Mhhinput,Mhh2input,MHpinput,MHp2input,MSscinput,       & 
& MSsc2input,MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,          & 
& ZURinput,ZDLinput,ZELinput,VSsinput,ZULinput,UVinput,UVRinput,XUinput,XVinput,         & 
& ZHinput,ZWinput,ZZinput,alphaHinput,g1input,g2input,g3input,Laminput,LS1Hinput,        & 
& LSinput,LS2Hinput,LSPinput,LSPHinput,Yuinput,Ydinput,Yeinput,YRDinput,YRB1input,       & 
& YRB2input,YRCinput,YRA1input,YRA2input,MDFinput,m2SMinput,MS12input,MS22input,         & 
& mP2input,vvSMinput,vSinput,cplcFeFxecSscL,cplcFeFxecSscR,cplcFvFxvcSscL,               & 
& cplcFvFxvcSscR,cplhhSsccSsc,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,LS1Hinput,LSinput,LS2Hinput,LSPinput,LSPHinput,               & 
& YRDinput,YRB1input(3),YRB2input(3),YRCinput,YRA1input(3),YRA2input(3),MDFinput,        & 
& m2SMinput,MS12input,MS22input,mP2input,vvSMinput,vSinput

Complex(dp),Intent(in) :: Laminput,Yuinput(3,3),Ydinput(3,3),Yeinput(3,3)

Real(dp),Intent(in) :: MAhinput,MAh2input,MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),MFuinput(3),     & 
& MFu2input(3),MFvinput(3),MFv2input(3),MFxeinput,MFxe2input,MFxvinput(2),               & 
& MFxv2input(2),Mhhinput(2),Mhh2input(2),MHpinput,MHp2input,MSscinput(2),MSsc2input(2),  & 
& MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,VSsinput(2,2),ZHinput(2,2),            & 
& ZZinput(2,2),alphaHinput

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& UVinput(3,3),UVRinput(3,3),XUinput(2,2),XVinput(2,2),ZWinput(2,2)

Real(dp) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS

Complex(dp) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Complex(dp),Intent(out) :: cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),  & 
& cplhhSsccSsc(2,2,2)

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_Ssc_2B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam = Laminput 
LS1H = LS1Hinput 
LS = LSinput 
LS2H = LS2Hinput 
LSP = LSPinput 
LSPH = LSPHinput 
Yu = Yuinput 
Yd = Ydinput 
Ye = Yeinput 
YRD = YRDinput 
YRB1 = YRB1input 
YRB2 = YRB2input 
YRC = YRCinput 
YRA1 = YRA1input 
YRA2 = YRA2input 
MDF = MDFinput 
m2SM = m2SMinput 
MS12 = MS12input 
MS22 = MS22input 
mP2 = mP2input 
vvSM = vvSMinput 
vS = vSinput 
Qin=sqrt(getRenormalizationScale()) 
Call SolveTadpoleEquations(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,               & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,(/ ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,              & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,            & 
& ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& .True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
VSs = VSsinput 
UV = UVinput 
UVR = UVRinput 
XU = XUinput 
XV = XVinput 
ZH = ZHinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
MFv = MFvinput 
MFv2 = MFv2input 
MFxe = MFxeinput 
MFxe2 = MFxe2input 
MFxv = MFxvinput 
MFxv2 = MFxv2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHp = MHpinput 
MHp2 = MHp2input 
MSsc = MSscinput 
MSsc2 = MSsc2input 
MVWp = MVWpinput 
MVWp2 = MVWp2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
End if 
cplhhSsccSsc = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplinghhSsccSscT(gt1,gt2,gt3,LS1H,LS2H,vvSM,ZH,VSs,cplhhSsccSsc(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFxecSscL = 0._dp 
cplcFeFxecSscR = 0._dp 
Do gt1 = 1, 3
 Do gt3 = 1, 2
Call CouplingcFeFxecSscT(gt1,gt3,YRA1,YRA2,ZEL,VSs,cplcFeFxecSscL(gt1,gt3)            & 
& ,cplcFeFxecSscR(gt1,gt3))

 End Do 
End Do 


cplcFvFxvcSscL = 0._dp 
cplcFvFxvcSscR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingcFvFxvcSscT(gt1,gt2,gt3,YRB1,YRB2,YRA1,YRA2,UV,UVR,XV,XU,VSs,            & 
& cplcFvFxvcSscL(gt1,gt2,gt3),cplcFvFxvcSscR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


Iname = Iname - 1 
 
End subroutine CouplingsFor_Ssc_decays_2B
 
Subroutine CouplingsFor_Fxe_decays_2B(m_in,i1,MAhinput,MAh2input,MFdinput,            & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,MFvinput,MFv2input,MFxeinput,          & 
& MFxe2input,MFxvinput,MFxv2input,Mhhinput,Mhh2input,MHpinput,MHp2input,MSscinput,       & 
& MSsc2input,MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,          & 
& ZURinput,ZDLinput,ZELinput,VSsinput,ZULinput,UVinput,UVRinput,XUinput,XVinput,         & 
& ZHinput,ZWinput,ZZinput,alphaHinput,g1input,g2input,g3input,Laminput,LS1Hinput,        & 
& LSinput,LS2Hinput,LSPinput,LSPHinput,Yuinput,Ydinput,Yeinput,YRDinput,YRB1input,       & 
& YRB2input,YRCinput,YRA1input,YRA2input,MDFinput,m2SMinput,MS12input,MS22input,         & 
& mP2input,vvSMinput,vSinput,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVZL,cplcFxeFxeVZR,    & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,LS1Hinput,LSinput,LS2Hinput,LSPinput,LSPHinput,               & 
& YRDinput,YRB1input(3),YRB2input(3),YRCinput,YRA1input(3),YRA2input(3),MDFinput,        & 
& m2SMinput,MS12input,MS22input,mP2input,vvSMinput,vSinput

Complex(dp),Intent(in) :: Laminput,Yuinput(3,3),Ydinput(3,3),Yeinput(3,3)

Real(dp),Intent(in) :: MAhinput,MAh2input,MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),MFuinput(3),     & 
& MFu2input(3),MFvinput(3),MFv2input(3),MFxeinput,MFxe2input,MFxvinput(2),               & 
& MFxv2input(2),Mhhinput(2),Mhh2input(2),MHpinput,MHp2input,MSscinput(2),MSsc2input(2),  & 
& MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,VSsinput(2,2),ZHinput(2,2),            & 
& ZZinput(2,2),alphaHinput

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& UVinput(3,3),UVRinput(3,3),XUinput(2,2),XVinput(2,2),ZWinput(2,2)

Real(dp) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS

Complex(dp) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Complex(dp),Intent(out) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcVWpL(2), & 
& cplcFxeFxvcVWpR(2)

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_Fxe_2B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam = Laminput 
LS1H = LS1Hinput 
LS = LSinput 
LS2H = LS2Hinput 
LSP = LSPinput 
LSPH = LSPHinput 
Yu = Yuinput 
Yd = Ydinput 
Ye = Yeinput 
YRD = YRDinput 
YRB1 = YRB1input 
YRB2 = YRB2input 
YRC = YRCinput 
YRA1 = YRA1input 
YRA2 = YRA2input 
MDF = MDFinput 
m2SM = m2SMinput 
MS12 = MS12input 
MS22 = MS22input 
mP2 = mP2input 
vvSM = vvSMinput 
vS = vSinput 
Qin=sqrt(getRenormalizationScale()) 
Call SolveTadpoleEquations(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,               & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,(/ ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,              & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,            & 
& ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& .True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
VSs = VSsinput 
UV = UVinput 
UVR = UVRinput 
XU = XUinput 
XV = XVinput 
ZH = ZHinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
MFv = MFvinput 
MFv2 = MFv2input 
MFxe = MFxeinput 
MFxe2 = MFxe2input 
MFxv = MFxvinput 
MFxv2 = MFxv2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHp = MHpinput 
MHp2 = MHp2input 
MSsc = MSscinput 
MSsc2 = MSsc2input 
MVWp = MVWpinput 
MVWp2 = MVWp2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
End if 
cplcFxeFeSscL = 0._dp 
cplcFxeFeSscR = 0._dp 
Do gt2 = 1, 3
 Do gt3 = 1, 2
Call CouplingcFxeFeSscT(gt2,gt3,YRA1,YRA2,ZEL,VSs,cplcFxeFeSscL(gt2,gt3)              & 
& ,cplcFxeFeSscR(gt2,gt3))

 End Do 
End Do 


cplcFxeFxeVZL = 0._dp 
cplcFxeFxeVZR = 0._dp 
Call CouplingcFxeFxeVZT(g1,g2,TW,cplcFxeFxeVZL,cplcFxeFxeVZR)



cplcFxeFxvcVWpL = 0._dp 
cplcFxeFxvcVWpR = 0._dp 
Do gt2 = 1, 2
Call CouplingcFxeFxvcVWpT(gt2,g2,XV,XU,cplcFxeFxvcVWpL(gt2),cplcFxeFxvcVWpR(gt2))

End Do 


Iname = Iname - 1 
 
End subroutine CouplingsFor_Fxe_decays_2B
 
Subroutine CouplingsFor_Fxv_decays_2B(m_in,i1,MAhinput,MAh2input,MFdinput,            & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,MFvinput,MFv2input,MFxeinput,          & 
& MFxe2input,MFxvinput,MFxv2input,Mhhinput,Mhh2input,MHpinput,MHp2input,MSscinput,       & 
& MSsc2input,MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,          & 
& ZURinput,ZDLinput,ZELinput,VSsinput,ZULinput,UVinput,UVRinput,XUinput,XVinput,         & 
& ZHinput,ZWinput,ZZinput,alphaHinput,g1input,g2input,g3input,Laminput,LS1Hinput,        & 
& LSinput,LS2Hinput,LSPinput,LSPHinput,Yuinput,Ydinput,Yeinput,YRDinput,YRB1input,       & 
& YRB2input,YRCinput,YRA1input,YRA2input,MDFinput,m2SMinput,MS12input,MS22input,         & 
& mP2input,vvSMinput,vSinput,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,  & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,LS1Hinput,LSinput,LS2Hinput,LSPinput,LSPHinput,               & 
& YRDinput,YRB1input(3),YRB2input(3),YRCinput,YRA1input(3),YRA2input(3),MDFinput,        & 
& m2SMinput,MS12input,MS22input,mP2input,vvSMinput,vSinput

Complex(dp),Intent(in) :: Laminput,Yuinput(3,3),Ydinput(3,3),Yeinput(3,3)

Real(dp),Intent(in) :: MAhinput,MAh2input,MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),MFuinput(3),     & 
& MFu2input(3),MFvinput(3),MFv2input(3),MFxeinput,MFxe2input,MFxvinput(2),               & 
& MFxv2input(2),Mhhinput(2),Mhh2input(2),MHpinput,MHp2input,MSscinput(2),MSsc2input(2),  & 
& MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,VSsinput(2,2),ZHinput(2,2),            & 
& ZZinput(2,2),alphaHinput

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& UVinput(3,3),UVRinput(3,3),XUinput(2,2),XVinput(2,2),ZWinput(2,2)

Real(dp) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS

Complex(dp) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Complex(dp),Intent(out) :: cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),        & 
& cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2)

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_Fxv_2B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam = Laminput 
LS1H = LS1Hinput 
LS = LSinput 
LS2H = LS2Hinput 
LSP = LSPinput 
LSPH = LSPHinput 
Yu = Yuinput 
Yd = Ydinput 
Ye = Yeinput 
YRD = YRDinput 
YRB1 = YRB1input 
YRB2 = YRB2input 
YRC = YRCinput 
YRA1 = YRA1input 
YRA2 = YRA2input 
MDF = MDFinput 
m2SM = m2SMinput 
MS12 = MS12input 
MS22 = MS22input 
mP2 = mP2input 
vvSM = vvSMinput 
vS = vSinput 
Qin=sqrt(getRenormalizationScale()) 
Call SolveTadpoleEquations(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,               & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,(/ ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,              & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,            & 
& ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& .True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
VSs = VSsinput 
UV = UVinput 
UVR = UVRinput 
XU = XUinput 
XV = XVinput 
ZH = ZHinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
MFv = MFvinput 
MFv2 = MFv2input 
MFxe = MFxeinput 
MFxe2 = MFxe2input 
MFxv = MFxvinput 
MFxv2 = MFxv2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHp = MHpinput 
MHp2 = MHp2input 
MSsc = MSscinput 
MSsc2 = MSsc2input 
MVWp = MVWpinput 
MVWp2 = MVWp2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
End if 
cplcFxvFvSscL = 0._dp 
cplcFxvFvSscR = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFxvFvSscT(gt1,gt2,gt3,YRB1,YRB2,YRA1,YRA2,UV,UVR,XV,XU,VSs,             & 
& cplcFxvFvSscL(gt1,gt2,gt3),cplcFxvFvSscR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFxvFxvhhL = 0._dp 
cplcFxvFxvhhR = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingcFxvFxvhhT(gt1,gt2,gt3,YRD,YRC,ZH,XV,XU,cplcFxvFxvhhL(gt1,gt2,gt3)       & 
& ,cplcFxvFxvhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFxvFxeVWpL = 0._dp 
cplcFxvFxeVWpR = 0._dp 
Do gt1 = 1, 2
Call CouplingcFxvFxeVWpT(gt1,g2,XV,XU,cplcFxvFxeVWpL(gt1),cplcFxvFxeVWpR(gt1))

End Do 


cplcFxvFxvVZL = 0._dp 
cplcFxvFxvVZR = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingcFxvFxvVZT(gt1,gt2,g1,g2,XV,XU,TW,cplcFxvFxvVZL(gt1,gt2),cplcFxvFxvVZR(gt1,gt2))

 End Do 
End Do 


Iname = Iname - 1 
 
End subroutine CouplingsFor_Fxv_decays_2B
 
Subroutine CouplingsFor_Fu_decays_3B(m_in,i1,MAhinput,MAh2input,MFdinput,             & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,MFvinput,MFv2input,MFxeinput,          & 
& MFxe2input,MFxvinput,MFxv2input,Mhhinput,Mhh2input,MHpinput,MHp2input,MSscinput,       & 
& MSsc2input,MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,          & 
& ZURinput,ZDLinput,ZELinput,VSsinput,ZULinput,UVinput,UVRinput,XUinput,XVinput,         & 
& ZHinput,ZWinput,ZZinput,alphaHinput,g1input,g2input,g3input,Laminput,LS1Hinput,        & 
& LSinput,LS2Hinput,LSPinput,LSPHinput,Yuinput,Ydinput,Yeinput,YRDinput,YRB1input,       & 
& YRB2input,YRCinput,YRA1input,YRA2input,MDFinput,m2SMinput,MS12input,MS22input,         & 
& mP2input,vvSMinput,vSinput,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,            & 
& cplcFdFucVWpL,cplcFdFucVWpR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVZL,cplcFeFeVZR,           & 
& cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,         & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFvFvVZL,cplcFvFvVZR,cplcFxeFxeVZL,cplcFxeFxeVZR,           & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,             & 
& cplcFxvFxvVZR,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,LS1Hinput,LSinput,LS2Hinput,LSPinput,LSPHinput,               & 
& YRDinput,YRB1input(3),YRB2input(3),YRCinput,YRA1input(3),YRA2input(3),MDFinput,        & 
& m2SMinput,MS12input,MS22input,mP2input,vvSMinput,vSinput

Complex(dp),Intent(in) :: Laminput,Yuinput(3,3),Ydinput(3,3),Yeinput(3,3)

Real(dp),Intent(in) :: MAhinput,MAh2input,MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),MFuinput(3),     & 
& MFu2input(3),MFvinput(3),MFv2input(3),MFxeinput,MFxe2input,MFxvinput(2),               & 
& MFxv2input(2),Mhhinput(2),Mhh2input(2),MHpinput,MHp2input,MSscinput(2),MSsc2input(2),  & 
& MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,VSsinput(2,2),ZHinput(2,2),            & 
& ZZinput(2,2),alphaHinput

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& UVinput(3,3),UVRinput(3,3),XUinput(2,2),XVinput(2,2),ZWinput(2,2)

Real(dp) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS

Complex(dp) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Complex(dp),Intent(out) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),              & 
& cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),           & 
& cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),               & 
& cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),             & 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFxeFxeVZL,     & 
& cplcFxeFxeVZR,cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplcFxvFxvhhL(2,2,2),              & 
& cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2)

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_Fu_3B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam = Laminput 
LS1H = LS1Hinput 
LS = LSinput 
LS2H = LS2Hinput 
LSP = LSPinput 
LSPH = LSPHinput 
Yu = Yuinput 
Yd = Ydinput 
Ye = Yeinput 
YRD = YRDinput 
YRB1 = YRB1input 
YRB2 = YRB2input 
YRC = YRCinput 
YRA1 = YRA1input 
YRA2 = YRA2input 
MDF = MDFinput 
m2SM = m2SMinput 
MS12 = MS12input 
MS22 = MS22input 
mP2 = mP2input 
vvSM = vvSMinput 
vS = vSinput 
Qin=sqrt(getRenormalizationScale()) 
If (m_in.le.Qin) Then 
  If (m_in.le.mz) Then 
Call RunSMohdm(mz,deltaM, g1,g2,g3,Yu,Yd,Ye,vvSM) 
  Else 
Call RunSMohdm(m_in,deltaM, g1,g2,g3,Yu,Yd,Ye,vvSM) 
  End if 
End if 
Call SolveTadpoleEquations(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,               & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,(/ ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,              & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,            & 
& ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& .True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
VSs = VSsinput 
UV = UVinput 
UVR = UVRinput 
XU = XUinput 
XV = XVinput 
ZH = ZHinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
MFv = MFvinput 
MFv2 = MFv2input 
MFxe = MFxeinput 
MFxe2 = MFxe2input 
MFxv = MFxvinput 
MFxv2 = MFxv2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHp = MHpinput 
MHp2 = MHp2input 
MSsc = MSscinput 
MSsc2 = MSsc2input 
MVWp = MVWpinput 
MVWp2 = MVWp2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
End if 
cplcFdFdhhL = 0._dp 
cplcFdFdhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFdFdhhT(gt1,gt2,gt3,Yd,ZH,ZDL,ZDR,cplcFdFdhhL(gt1,gt2,gt3)              & 
& ,cplcFdFdhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFehhL = 0._dp 
cplcFeFehhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFeFehhT(gt1,gt2,gt3,Ye,ZH,ZEL,ZER,cplcFeFehhL(gt1,gt2,gt3)              & 
& ,cplcFeFehhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuhhL = 0._dp 
cplcFuFuhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFuFuhhT(gt1,gt2,gt3,Yu,ZH,ZUL,ZUR,cplcFuFuhhL(gt1,gt2,gt3)              & 
& ,cplcFuFuhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFxvFxvhhL = 0._dp 
cplcFxvFxvhhR = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingcFxvFxvhhT(gt1,gt2,gt3,YRD,YRC,ZH,XV,XU,cplcFxvFxvhhL(gt1,gt2,gt3)       & 
& ,cplcFxvFxvhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFdVWpL = 0._dp 
cplcFuFdVWpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFdVWpT(gt1,gt2,g2,ZDL,ZUL,cplcFuFdVWpL(gt1,gt2),cplcFuFdVWpR(gt1,gt2))

 End Do 
End Do 


cplcFdFdVZL = 0._dp 
cplcFdFdVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVZT(gt1,gt2,g1,g2,TW,cplcFdFdVZL(gt1,gt2),cplcFdFdVZR(gt1,gt2))

 End Do 
End Do 


cplcFeFeVZL = 0._dp 
cplcFeFeVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFeFeVZT(gt1,gt2,g1,g2,TW,cplcFeFeVZL(gt1,gt2),cplcFeFeVZR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVZL = 0._dp 
cplcFuFuVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVZT(gt1,gt2,g1,g2,TW,cplcFuFuVZL(gt1,gt2),cplcFuFuVZR(gt1,gt2))

 End Do 
End Do 


cplcFdFucVWpL = 0._dp 
cplcFdFucVWpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFucVWpT(gt1,gt2,g2,ZDL,ZUL,cplcFdFucVWpL(gt1,gt2),cplcFdFucVWpR(gt1,gt2))

 End Do 
End Do 


cplcFvFvVZL = 0._dp 
cplcFvFvVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFvFvVZT(gt1,gt2,g1,g2,TW,cplcFvFvVZL(gt1,gt2),cplcFvFvVZR(gt1,gt2))

 End Do 
End Do 


cplcFeFvcVWpL = 0._dp 
cplcFeFvcVWpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFeFvcVWpT(gt1,gt2,g2,ZEL,UV,cplcFeFvcVWpL(gt1,gt2),cplcFeFvcVWpR(gt1,gt2))

 End Do 
End Do 


cplcFxeFxeVZL = 0._dp 
cplcFxeFxeVZR = 0._dp 
Call CouplingcFxeFxeVZT(g1,g2,TW,cplcFxeFxeVZL,cplcFxeFxeVZR)



cplcFxvFxvVZL = 0._dp 
cplcFxvFxvVZR = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingcFxvFxvVZT(gt1,gt2,g1,g2,XV,XU,TW,cplcFxvFxvVZL(gt1,gt2),cplcFxvFxvVZR(gt1,gt2))

 End Do 
End Do 


cplcFxeFxvcVWpL = 0._dp 
cplcFxeFxvcVWpR = 0._dp 
Do gt2 = 1, 2
Call CouplingcFxeFxvcVWpT(gt2,g2,XV,XU,cplcFxeFxvcVWpL(gt2),cplcFxeFxvcVWpR(gt2))

End Do 


Iname = Iname - 1 
 
End subroutine CouplingsFor_Fu_decays_3B
 
Subroutine CouplingsFor_Fe_decays_3B(m_in,i1,MAhinput,MAh2input,MFdinput,             & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,MFvinput,MFv2input,MFxeinput,          & 
& MFxe2input,MFxvinput,MFxv2input,Mhhinput,Mhh2input,MHpinput,MHp2input,MSscinput,       & 
& MSsc2input,MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,          & 
& ZURinput,ZDLinput,ZELinput,VSsinput,ZULinput,UVinput,UVRinput,XUinput,XVinput,         & 
& ZHinput,ZWinput,ZZinput,alphaHinput,g1input,g2input,g3input,Laminput,LS1Hinput,        & 
& LSinput,LS2Hinput,LSPinput,LSPHinput,Yuinput,Ydinput,Yeinput,YRDinput,YRB1input,       & 
& YRB2input,YRCinput,YRA1input,YRA2input,MDFinput,m2SMinput,MS12input,MS22input,         & 
& mP2input,vvSMinput,vSinput,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,            & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcVWpL,cplcFeFvcVWpR,           & 
& cplcFeFxecSscL,cplcFeFxecSscR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,       & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFvFvVZL,cplcFvFvVZR,             & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFvSscL,cplcFxvFvSscR,   & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,               & 
& cplcFxvFxvVZR,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,LS1Hinput,LSinput,LS2Hinput,LSPinput,LSPHinput,               & 
& YRDinput,YRB1input(3),YRB2input(3),YRCinput,YRA1input(3),YRA2input(3),MDFinput,        & 
& m2SMinput,MS12input,MS22input,mP2input,vvSMinput,vSinput

Complex(dp),Intent(in) :: Laminput,Yuinput(3,3),Ydinput(3,3),Yeinput(3,3)

Real(dp),Intent(in) :: MAhinput,MAh2input,MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),MFuinput(3),     & 
& MFu2input(3),MFvinput(3),MFv2input(3),MFxeinput,MFxe2input,MFxvinput(2),               & 
& MFxv2input(2),Mhhinput(2),Mhh2input(2),MHpinput,MHp2input,MSscinput(2),MSsc2input(2),  & 
& MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,VSsinput(2,2),ZHinput(2,2),            & 
& ZZinput(2,2),alphaHinput

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& UVinput(3,3),UVRinput(3,3),XUinput(2,2),XVinput(2,2),ZWinput(2,2)

Real(dp) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS

Complex(dp) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Complex(dp),Intent(out) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),              & 
& cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),               & 
& cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),         & 
& cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),             & 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFvFvVZL(3,3),& 
& cplcFvFvVZR(3,3),cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFxeFxeVZL,cplcFxeFxeVZR,    & 
& cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),         & 
& cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2)

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_Fe_3B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam = Laminput 
LS1H = LS1Hinput 
LS = LSinput 
LS2H = LS2Hinput 
LSP = LSPinput 
LSPH = LSPHinput 
Yu = Yuinput 
Yd = Ydinput 
Ye = Yeinput 
YRD = YRDinput 
YRB1 = YRB1input 
YRB2 = YRB2input 
YRC = YRCinput 
YRA1 = YRA1input 
YRA2 = YRA2input 
MDF = MDFinput 
m2SM = m2SMinput 
MS12 = MS12input 
MS22 = MS22input 
mP2 = mP2input 
vvSM = vvSMinput 
vS = vSinput 
Qin=sqrt(getRenormalizationScale()) 
If (m_in.le.Qin) Then 
  If (m_in.le.mz) Then 
Call RunSMohdm(mz,deltaM, g1,g2,g3,Yu,Yd,Ye,vvSM) 
  Else 
Call RunSMohdm(m_in,deltaM, g1,g2,g3,Yu,Yd,Ye,vvSM) 
  End if 
End if 
Call SolveTadpoleEquations(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,               & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,(/ ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,              & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,            & 
& ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& .True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
VSs = VSsinput 
UV = UVinput 
UVR = UVRinput 
XU = XUinput 
XV = XVinput 
ZH = ZHinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
MFv = MFvinput 
MFv2 = MFv2input 
MFxe = MFxeinput 
MFxe2 = MFxe2input 
MFxv = MFxvinput 
MFxv2 = MFxv2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHp = MHpinput 
MHp2 = MHp2input 
MSsc = MSscinput 
MSsc2 = MSsc2input 
MVWp = MVWpinput 
MVWp2 = MVWp2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
End if 
cplcFdFdhhL = 0._dp 
cplcFdFdhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFdFdhhT(gt1,gt2,gt3,Yd,ZH,ZDL,ZDR,cplcFdFdhhL(gt1,gt2,gt3)              & 
& ,cplcFdFdhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFehhL = 0._dp 
cplcFeFehhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFeFehhT(gt1,gt2,gt3,Ye,ZH,ZEL,ZER,cplcFeFehhL(gt1,gt2,gt3)              & 
& ,cplcFeFehhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFxeFeSscL = 0._dp 
cplcFxeFeSscR = 0._dp 
Do gt2 = 1, 3
 Do gt3 = 1, 2
Call CouplingcFxeFeSscT(gt2,gt3,YRA1,YRA2,ZEL,VSs,cplcFxeFeSscL(gt2,gt3)              & 
& ,cplcFxeFeSscR(gt2,gt3))

 End Do 
End Do 


cplcFuFuhhL = 0._dp 
cplcFuFuhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFuFuhhT(gt1,gt2,gt3,Yu,ZH,ZUL,ZUR,cplcFuFuhhL(gt1,gt2,gt3)              & 
& ,cplcFuFuhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFxvFvSscL = 0._dp 
cplcFxvFvSscR = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFxvFvSscT(gt1,gt2,gt3,YRB1,YRB2,YRA1,YRA2,UV,UVR,XV,XU,VSs,             & 
& cplcFxvFvSscL(gt1,gt2,gt3),cplcFxvFvSscR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFxecSscL = 0._dp 
cplcFeFxecSscR = 0._dp 
Do gt1 = 1, 3
 Do gt3 = 1, 2
Call CouplingcFeFxecSscT(gt1,gt3,YRA1,YRA2,ZEL,VSs,cplcFeFxecSscL(gt1,gt3)            & 
& ,cplcFeFxecSscR(gt1,gt3))

 End Do 
End Do 


cplcFxvFxvhhL = 0._dp 
cplcFxvFxvhhR = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingcFxvFxvhhT(gt1,gt2,gt3,YRD,YRC,ZH,XV,XU,cplcFxvFxvhhL(gt1,gt2,gt3)       & 
& ,cplcFxvFxvhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFdVWpL = 0._dp 
cplcFuFdVWpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFdVWpT(gt1,gt2,g2,ZDL,ZUL,cplcFuFdVWpL(gt1,gt2),cplcFuFdVWpR(gt1,gt2))

 End Do 
End Do 


cplcFdFdVZL = 0._dp 
cplcFdFdVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVZT(gt1,gt2,g1,g2,TW,cplcFdFdVZL(gt1,gt2),cplcFdFdVZR(gt1,gt2))

 End Do 
End Do 


cplcFvFeVWpL = 0._dp 
cplcFvFeVWpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFvFeVWpT(gt1,gt2,g2,ZEL,UV,cplcFvFeVWpL(gt1,gt2),cplcFvFeVWpR(gt1,gt2))

 End Do 
End Do 


cplcFeFeVZL = 0._dp 
cplcFeFeVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFeFeVZT(gt1,gt2,g1,g2,TW,cplcFeFeVZL(gt1,gt2),cplcFeFeVZR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVZL = 0._dp 
cplcFuFuVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVZT(gt1,gt2,g1,g2,TW,cplcFuFuVZL(gt1,gt2),cplcFuFuVZR(gt1,gt2))

 End Do 
End Do 


cplcFvFvVZL = 0._dp 
cplcFvFvVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFvFvVZT(gt1,gt2,g1,g2,TW,cplcFvFvVZL(gt1,gt2),cplcFvFvVZR(gt1,gt2))

 End Do 
End Do 


cplcFeFvcVWpL = 0._dp 
cplcFeFvcVWpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFeFvcVWpT(gt1,gt2,g2,ZEL,UV,cplcFeFvcVWpL(gt1,gt2),cplcFeFvcVWpR(gt1,gt2))

 End Do 
End Do 


cplcFxvFxeVWpL = 0._dp 
cplcFxvFxeVWpR = 0._dp 
Do gt1 = 1, 2
Call CouplingcFxvFxeVWpT(gt1,g2,XV,XU,cplcFxvFxeVWpL(gt1),cplcFxvFxeVWpR(gt1))

End Do 


cplcFxeFxeVZL = 0._dp 
cplcFxeFxeVZR = 0._dp 
Call CouplingcFxeFxeVZT(g1,g2,TW,cplcFxeFxeVZL,cplcFxeFxeVZR)



cplcFxvFxvVZL = 0._dp 
cplcFxvFxvVZR = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingcFxvFxvVZT(gt1,gt2,g1,g2,XV,XU,TW,cplcFxvFxvVZL(gt1,gt2),cplcFxvFxvVZR(gt1,gt2))

 End Do 
End Do 


Iname = Iname - 1 
 
End subroutine CouplingsFor_Fe_decays_3B
 
Subroutine CouplingsFor_Fd_decays_3B(m_in,i1,MAhinput,MAh2input,MFdinput,             & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,MFvinput,MFv2input,MFxeinput,          & 
& MFxe2input,MFxvinput,MFxv2input,Mhhinput,Mhh2input,MHpinput,MHp2input,MSscinput,       & 
& MSsc2input,MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,ZDRinput,ZERinput,          & 
& ZURinput,ZDLinput,ZELinput,VSsinput,ZULinput,UVinput,UVRinput,XUinput,XVinput,         & 
& ZHinput,ZWinput,ZZinput,alphaHinput,g1input,g2input,g3input,Laminput,LS1Hinput,        & 
& LSinput,LS2Hinput,LSPinput,LSPHinput,Yuinput,Ydinput,Yeinput,YRDinput,YRB1input,       & 
& YRB2input,YRCinput,YRA1input,YRA2input,MDFinput,m2SMinput,MS12input,MS22input,         & 
& mP2input,vvSMinput,vSinput,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,            & 
& cplcFdFucVWpL,cplcFdFucVWpR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVZL,cplcFeFeVZR,           & 
& cplcFuFdVWpL,cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,             & 
& cplcFvFeVWpL,cplcFvFeVWpR,cplcFvFvVZL,cplcFvFvVZR,cplcFxeFxeVZL,cplcFxeFxeVZR,         & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,               & 
& cplcFxvFxvVZR,deltaM)

Implicit None 

Real(dp), Intent(in) :: m_in 
Real(dp), Intent(in) :: deltaM 
Integer, Intent(in) :: i1 
Real(dp),Intent(in) :: g1input,g2input,g3input,LS1Hinput,LSinput,LS2Hinput,LSPinput,LSPHinput,               & 
& YRDinput,YRB1input(3),YRB2input(3),YRCinput,YRA1input(3),YRA2input(3),MDFinput,        & 
& m2SMinput,MS12input,MS22input,mP2input,vvSMinput,vSinput

Complex(dp),Intent(in) :: Laminput,Yuinput(3,3),Ydinput(3,3),Yeinput(3,3)

Real(dp),Intent(in) :: MAhinput,MAh2input,MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),MFuinput(3),     & 
& MFu2input(3),MFvinput(3),MFv2input(3),MFxeinput,MFxe2input,MFxvinput(2),               & 
& MFxv2input(2),Mhhinput(2),Mhh2input(2),MHpinput,MHp2input,MSscinput(2),MSsc2input(2),  & 
& MVWpinput,MVWp2input,MVZinput,MVZ2input,TWinput,VSsinput(2,2),ZHinput(2,2),            & 
& ZZinput(2,2),alphaHinput

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& UVinput(3,3),UVRinput(3,3),XUinput(2,2),XVinput(2,2),ZWinput(2,2)

Real(dp) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS

Complex(dp) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Complex(dp),Intent(out) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),              & 
& cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),           & 
& cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFuFuhhL(3,3,2),& 
& cplcFuFuhhR(3,3,2),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),& 
& cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxeVWpL(2),       & 
& cplcFxvFxeVWpR(2),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),        & 
& cplcFxvFxvVZR(2,2)

Integer :: i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Real(dp) :: gSM(11), sinW2, dt, tz, Qin 
Iname = Iname + 1 
NameOfUnit(Iname) = 'Couplings_Fd_3B'
 
sinW2=1._dp-mW2/mZ2 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam = Laminput 
LS1H = LS1Hinput 
LS = LSinput 
LS2H = LS2Hinput 
LSP = LSPinput 
LSPH = LSPHinput 
Yu = Yuinput 
Yd = Ydinput 
Ye = Yeinput 
YRD = YRDinput 
YRB1 = YRB1input 
YRB2 = YRB2input 
YRC = YRCinput 
YRA1 = YRA1input 
YRA2 = YRA2input 
MDF = MDFinput 
m2SM = m2SMinput 
MS12 = MS12input 
MS22 = MS22input 
mP2 = mP2input 
vvSM = vvSMinput 
vS = vSinput 
Qin=sqrt(getRenormalizationScale()) 
If (m_in.le.Qin) Then 
  If (m_in.le.mz) Then 
Call RunSMohdm(mz,deltaM, g1,g2,g3,Yu,Yd,Ye,vvSM) 
  Else 
Call RunSMohdm(m_in,deltaM, g1,g2,g3,Yu,Yd,Ye,vvSM) 
  End if 
End if 
Call SolveTadpoleEquations(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,               & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,(/ ZeroC, ZeroC /))

! --- Calculate running tree-level masses for loop induced couplings and Quark mixing matrices --- 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,              & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,            & 
& ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& .True.,kont)

If (ExternalZfactors) Then 
! --- Use the 1-loop mixing matrices calculated at M_SUSY in the vertices --- 
VSs = VSsinput 
UV = UVinput 
UVR = UVRinput 
XU = XUinput 
XV = XVinput 
ZH = ZHinput 
ZW = ZWinput 
ZZ = ZZinput 
End if 
If (PoleMassesInLoops) Then 
! --- Use the pole masses --- 
MAh = MAhinput 
MAh2 = MAh2input 
MFd = MFdinput 
MFd2 = MFd2input 
MFe = MFeinput 
MFe2 = MFe2input 
MFu = MFuinput 
MFu2 = MFu2input 
MFv = MFvinput 
MFv2 = MFv2input 
MFxe = MFxeinput 
MFxe2 = MFxe2input 
MFxv = MFxvinput 
MFxv2 = MFxv2input 
Mhh = Mhhinput 
Mhh2 = Mhh2input 
MHp = MHpinput 
MHp2 = MHp2input 
MSsc = MSscinput 
MSsc2 = MSsc2input 
MVWp = MVWpinput 
MVWp2 = MVWp2input 
MVZ = MVZinput 
MVZ2 = MVZ2input 
End if 
cplcFdFdhhL = 0._dp 
cplcFdFdhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFdFdhhT(gt1,gt2,gt3,Yd,ZH,ZDL,ZDR,cplcFdFdhhL(gt1,gt2,gt3)              & 
& ,cplcFdFdhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFehhL = 0._dp 
cplcFeFehhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFeFehhT(gt1,gt2,gt3,Ye,ZH,ZEL,ZER,cplcFeFehhL(gt1,gt2,gt3)              & 
& ,cplcFeFehhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuhhL = 0._dp 
cplcFuFuhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFuFuhhT(gt1,gt2,gt3,Yu,ZH,ZUL,ZUR,cplcFuFuhhL(gt1,gt2,gt3)              & 
& ,cplcFuFuhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFxvFxvhhL = 0._dp 
cplcFxvFxvhhR = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingcFxvFxvhhT(gt1,gt2,gt3,YRD,YRC,ZH,XV,XU,cplcFxvFxvhhL(gt1,gt2,gt3)       & 
& ,cplcFxvFxvhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFdVWpL = 0._dp 
cplcFuFdVWpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFdVWpT(gt1,gt2,g2,ZDL,ZUL,cplcFuFdVWpL(gt1,gt2),cplcFuFdVWpR(gt1,gt2))

 End Do 
End Do 


cplcFdFdVZL = 0._dp 
cplcFdFdVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVZT(gt1,gt2,g1,g2,TW,cplcFdFdVZL(gt1,gt2),cplcFdFdVZR(gt1,gt2))

 End Do 
End Do 


cplcFvFeVWpL = 0._dp 
cplcFvFeVWpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFvFeVWpT(gt1,gt2,g2,ZEL,UV,cplcFvFeVWpL(gt1,gt2),cplcFvFeVWpR(gt1,gt2))

 End Do 
End Do 


cplcFeFeVZL = 0._dp 
cplcFeFeVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFeFeVZT(gt1,gt2,g1,g2,TW,cplcFeFeVZL(gt1,gt2),cplcFeFeVZR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVZL = 0._dp 
cplcFuFuVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVZT(gt1,gt2,g1,g2,TW,cplcFuFuVZL(gt1,gt2),cplcFuFuVZR(gt1,gt2))

 End Do 
End Do 


cplcFdFucVWpL = 0._dp 
cplcFdFucVWpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFucVWpT(gt1,gt2,g2,ZDL,ZUL,cplcFdFucVWpL(gt1,gt2),cplcFdFucVWpR(gt1,gt2))

 End Do 
End Do 


cplcFvFvVZL = 0._dp 
cplcFvFvVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFvFvVZT(gt1,gt2,g1,g2,TW,cplcFvFvVZL(gt1,gt2),cplcFvFvVZR(gt1,gt2))

 End Do 
End Do 


cplcFxvFxeVWpL = 0._dp 
cplcFxvFxeVWpR = 0._dp 
Do gt1 = 1, 2
Call CouplingcFxvFxeVWpT(gt1,g2,XV,XU,cplcFxvFxeVWpL(gt1),cplcFxvFxeVWpR(gt1))

End Do 


cplcFxeFxeVZL = 0._dp 
cplcFxeFxeVZR = 0._dp 
Call CouplingcFxeFxeVZT(g1,g2,TW,cplcFxeFxeVZL,cplcFxeFxeVZR)



cplcFxvFxvVZL = 0._dp 
cplcFxvFxvVZR = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingcFxvFxvVZT(gt1,gt2,g1,g2,XV,XU,TW,cplcFxvFxvVZL(gt1,gt2),cplcFxvFxvVZR(gt1,gt2))

 End Do 
End Do 


Iname = Iname - 1 
 
End subroutine CouplingsFor_Fd_decays_3B
 
Function NFlav(m_in) 
Implicit None 
Real(dp), Intent(in) :: m_in 
Real(dp) :: NFlav 
If (m_in.lt.mf_d(3)) Then 
  NFlav = 4._dp 
Else If (m_in.lt.mf_u(3)) Then 
  NFlav = 5._dp 
Else 
  NFlav = 6._dp 
End if 
End Function

Subroutine RunSM(scale_out,deltaM,tb,g1,g2,g3,Yu, Yd, Ye, vd, vu) 
Implicit None
Real(dp), Intent(in) :: scale_out,deltaM, tb
Real(dp), Intent(out) :: g1, g2, g3, vd, vu
Complex(dp), Intent(out) :: Yu(3,3), Yd(3,3), Ye(3,3)
Real(dp) :: dt, gSM(14), gSM2(2), gSM3(3), mtopMS,  sinw2, vev, tz, alphaStop 
Integer :: kont

RunningTopMZ = .false.

Yd = 0._dp
Ye = 0._dp
Yu = 0._dp

If (.not.RunningTopMZ) Then

! Calculating alpha_S(m_top)
gSM2(1)=sqrt(Alpha_mZ*4*Pi) 
gSM2(2)=sqrt(AlphaS_mZ*4*Pi) 


tz=Log(sqrt(mz2)/mf_u(3)) 
dt=tz/50._dp 
Call odeint(gSM2,2,tz,0._dp,deltaM,dt,0._dp,RGEAlphaS,kont)



alphaStop = gSM2(2)**2/4._dp/Pi



! m_top^pole to m_top^MS(m_top) 

mtopMS = mf_u(3)*(1._dp - 4._dp/3._dp*alphaStop/Pi)


! Running m_top^MS(m_top) to M_Z 

gSM3(1)=gSM2(1) 
gSM3(2)=gSM2(2)
gSM3(3)=mtopMS

tz=Log(sqrt(mz2)/mf_u(3)) 
dt=tz/50._dp 
Call odeint(gSM3,3,0._dp,tz,deltaM,dt,0._dp,RGEtop,kont)


mf_u_mz_running = gSM3(3)


RunningTopMZ = .True.

End if

! Starting values at MZ

gSM(1)=sqrt(Alpha_mZ*4*Pi) 
gSM(2)=sqrt(AlphaS_mZ*4*Pi) 
gSM(3)= 0.486E-03_dp ! mf_l_mz(1) 
gSM(4)= 0.10272 !mf_l_mz(2) 
gSM(5)= 1.74624 !mf_l_mz(3) 
gSM(6)= 1.27E-03_dp ! mf_u_mz(1) 
gSM(7)= 0.619  ! mf_u_mz(2) 
gSM(8)= mf_u_mz_running ! m_top 
gSM(9)= 2.9E-03_dp !mf_d_mz(1) 
gSM(10)= 0.055 !mf_d_mz(2) 
gSM(11)= 2.85 ! mf_d_mz(3) 
 

! To get the running sin(w2) 
SinW2 = 0.22290_dp
gSM(12) = 5._dp/3._dp*Alpha_MZ/(1-sinW2)
gSM(13) = Alpha_MZ/Sinw2
gSM(14) = AlphaS_mZ

  nUp =2._dp 
  nDown =3._dp 
  nLep =3._dp 
 

If (scale_out.gt.sqrt(mz2)) Then

 ! From M_Z to Min(M_top,scale_out) 
 If (scale_out.gt.mf_u(3)) Then 
  tz=Log(sqrt(mz2)/mf_u(3)) 
  dt=tz/50._dp 
 Else 
  tz=Log(sqrt(mz2)/scale_out) 
  dt=tz/50._dp 
 End if 

  Call odeint(gSM,14,tz,0._dp,deltaM,dt,0._dp,rge11,kont)


 ! From M_top to M_SUSY if M_top < M_SUSY 
 If (scale_out.gt.mf_u(3)) Then 
  tz=Log(mf_u(3)/scale_out) 
  dt=tz/50._dp 
  nUp =3._dp 
  Call odeint(gSM,14,tz,0._dp,deltaM,dt,0._dp,rge11,kont)
 End if 
Else

 ! From M_Z down to scale_out
  tz=Log(scale_out/sqrt(mz2)) 
  dt=tz/50._dp 
  Call odeint(gSM,14,0._dp,tz,deltaM,dt,0._dp,rge11_SMa,kont)

End if

! Calculating Couplings 

 sinW2=1._dp-mW2/mZ2 
 vev=Sqrt(mZ2*(1._dp-sinW2)*SinW2/(gSM(1)**2/4._dp))
 vd=vev/Sqrt(1._dp+tb**2)
 vu=tb*vd
 
Yd(1,1) =gSM(9)*sqrt(2._dp)/vd 
Yd(2,2) =gSM(10)*sqrt(2._dp)/vd 
Yd(3,3) =gSM(11)*sqrt(2._dp)/vd 
Ye(1,1) =gSM(3)*sqrt(2._dp)/vd 
Ye(2,2)=gSM(4)*sqrt(2._dp)/vd 
Ye(3,3)=gSM(5)*sqrt(2._dp)/vd 
Yu(1,1)=gSM(6)*sqrt(2._dp)/vu 
Yu(2,2)=gSM(7)*sqrt(2._dp)/vu 
Yu(3,3)=gSM(8)*sqrt(2._dp)/vu 


g3 =gSM(2) 
g3running=gSM(2) 

g1 = sqrt(gSM(12)*4._dp*Pi*3._dp/5._dp)
g2 = sqrt(gSM(13)*4._dp*Pi)
! g3 = sqrt(gSM(3)*4._dp*Pi)

sinw2 = g1**2/(g1**2 + g2**2)

!g2=gSM(1)/sqrt(sinW2) 
!g1 = g2*Sqrt(sinW2/(1._dp-sinW2)) 

If (GenerationMixing) Then 

If (YukawaScheme.Eq.1) Then ! CKM into Yu
 If (TransposedYukawa) Then ! check, if superpotential is Yu Hu u q  or Yu Hu q u
   Yu= Matmul(Transpose(CKM),Transpose(Yu))
 Else 
   Yu=Transpose(Matmul(Transpose(CKM),Transpose(Yu)))
 End if 

Else ! CKM into Yd 
 
 If (TransposedYukawa) Then ! 
  Yd= Matmul(Conjg(CKM),Transpose(Yd))
 Else 
  Yd=Transpose(Matmul(Conjg(CKM),Transpose(Yd)))
 End if 

End if ! Yukawa scheme
End If ! Generatoin mixing


End Subroutine RunSM


Subroutine RunSMohdm(scale_out,deltaM,g1,g2,g3,Yu, Yd, Ye, v) 
Implicit None
Real(dp), Intent(in) :: scale_out,deltaM
Real(dp), Intent(out) :: g1, g2, g3, v
Complex(dp), Intent(out) :: Yu(3,3), Yd(3,3), Ye(3,3)
Real(dp) :: dt, gSM(14), gSM2(2), gSM3(3), mtopMS,  sinw2, vev, tz, alphaStop 
Integer :: kont

Yd = 0._dp
Ye = 0._dp
Yu = 0._dp

If (.not.RunningTopMZ) Then

! Calculating alpha_S(m_top)
gSM2(1)=sqrt(Alpha_mZ*4*Pi) 
gSM2(2)=sqrt(AlphaS_mZ*4*Pi) 


tz=Log(sqrt(mz2)/mf_u(3)) 
dt=tz/50._dp 
Call odeint(gSM2,2,tz,0._dp,deltaM,dt,0._dp,RGEAlphaS,kont)



alphaStop = gSM2(2)**2/4._dp/Pi



! m_top^pole to m_top^MS(m_top) 

mtopMS = mf_u(3)*(1._dp - 4._dp/3._dp*alphaStop/Pi)


! Running m_top^MS(m_top) to M_Z 

gSM3(1)=gSM2(1) 
gSM3(2)=gSM2(2)
gSM3(3)=mtopMS

tz=Log(sqrt(mz2)/mf_u(3)) 
dt=tz/50._dp 
Call odeint(gSM3,3,0._dp,tz,deltaM,dt,0._dp,RGEtop,kont)


mf_u_mz_running = gSM3(3)


RunningTopMZ = .True.

End if

! Starting values at MZ

gSM(1)=sqrt(Alpha_mZ*4*Pi) 
gSM(2)=sqrt(AlphaS_mZ*4*Pi) 
gSM(3)= 0.486E-03_dp ! mf_l_mz(1) 
gSM(4)= 0.10272 !mf_l_mz(2) 
gSM(5)= 1.74624 !mf_l_mz(3) 
gSM(6)= 1.27E-03_dp ! mf_u_mz(1) 
gSM(7)= 0.619  ! mf_u_mz(2) 
gSM(8)= mf_u_mz_running ! m_top 
gSM(9)= 2.9E-03_dp !mf_d_mz(1) 
gSM(10)= 0.055 !mf_d_mz(2) 
gSM(11)= 2.85 ! mf_d_mz(3) 
 

! To get the running sin(w2) 
SinW2 = 0.22290_dp
gSM(12) = 5._dp/3._dp*Alpha_MZ/(1-sinW2)
gSM(13) = Alpha_MZ/Sinw2
gSM(14) = AlphaS_mZ

  nUp =2._dp 
  nDown =3._dp 
  nLep =3._dp 
 

If (scale_out.gt.sqrt(mz2)) Then

 ! From M_Z to Min(M_top,scale_out) 
 If (scale_out.gt.mf_u(3)) Then 
  tz=Log(sqrt(mz2)/mf_u(3)) 
  dt=tz/50._dp 
 Else 
  tz=Log(sqrt(mz2)/scale_out) 
  dt=tz/50._dp 
 End if 

  Call odeint(gSM,14,tz,0._dp,deltaM,dt,0._dp,rge11,kont)


 ! From M_top to M_SUSY if M_top < M_SUSY 
 If (scale_out.gt.mf_u(3)) Then 
  tz=Log(mf_u(3)/scale_out) 
  dt=tz/50._dp 
  nUp =3._dp 
  Call odeint(gSM,14,tz,0._dp,deltaM,dt,0._dp,rge11,kont)
 End if 
Else

 ! From M_Z down to scale_out
  If (abs(scale_out - sqrt(mz2)).gt.1.0E-3_dp) Then 
   tz=Log(scale_out/sqrt(mz2)) 
   dt=tz/50._dp 
   Call odeint(gSM,14,0._dp,tz,deltaM,dt,0._dp,rge11_SMa,kont)
  End if
End if

! Calculating Couplings 

 sinW2=1._dp-mW2/mZ2 
 vev=Sqrt(mZ2*(1._dp-sinW2)*SinW2/(gSM(1)**2/4._dp))
 v = vev
 
Yd(1,1) =gSM(9)*sqrt(2._dp)/v 
Yd(2,2) =gSM(10)*sqrt(2._dp)/v 
Yd(3,3) =gSM(11)*sqrt(2._dp)/v 
Ye(1,1) =gSM(3)*sqrt(2._dp)/v 
Ye(2,2)=gSM(4)*sqrt(2._dp)/v 
Ye(3,3)=gSM(5)*sqrt(2._dp)/v 
Yu(1,1)=gSM(6)*sqrt(2._dp)/v 
Yu(2,2)=gSM(7)*sqrt(2._dp)/v 
Yu(3,3)=gSM(8)*sqrt(2._dp)/v 


g3 =gSM(2) 
g3running=gSM(2) 

g1 = sqrt(gSM(12)*4._dp*Pi*3._dp/5._dp)
g2 = sqrt(gSM(13)*4._dp*Pi)
! g3 = sqrt(gSM(3)*4._dp*Pi)

sinw2 = g1**2/(g1**2 + g2**2)

g2=gSM(1)/sqrt(sinW2) 
g1 = g2*Sqrt(sinW2/(1._dp-sinW2)) 

If (GenerationMixing) Then 

If (YukawaScheme.Eq.1) Then ! CKM into Yu
 If (TransposedYukawa) Then ! check, if superpotential is Yu Hu u q  or Yu Hu q u
   Yu= Matmul(Transpose(CKM),Transpose(Yu))
 Else 
   Yu=Transpose(Matmul(Transpose(CKM),Transpose(Yu)))
 End if 

Else ! CKM into Yd 
 
 If (TransposedYukawa) Then ! 
  Yd= Matmul(Conjg(CKM),Transpose(Yd))
 Else 
  Yd=Transpose(Matmul(Conjg(CKM),Transpose(Yd)))
 End if 

End if ! Yukawa scheme
End If ! Generation mixing



End Subroutine RunSMohdm

Subroutine RunSMgauge(scale_out,deltaM,g1,g2,g3) 
Implicit None
Real(dp), Intent(in) :: scale_out,deltaM
Real(dp), Intent(out) :: g1, g2, g3
Complex(dp) :: Yu(3,3), Yd(3,3), Ye(3,3)
Real(dp) :: v, dt, gSM(14), gSM2(2), gSM3(3), mtopMS,  sinw2, vev, tz, alphaStop 
Integer :: kont

Yd = 0._dp
Ye = 0._dp
Yu = 0._dp

RunningTopMZ = .false.

If (.not.RunningTopMZ) Then

! Calculating alpha_S(m_top)
gSM2(1)=sqrt(Alpha_mZ*4*Pi) 
gSM2(2)=sqrt(AlphaS_mZ*4*Pi) 


tz=Log(sqrt(mz2)/mf_u(3)) 
dt=tz/50._dp 
Call odeint(gSM2,2,tz,0._dp,deltaM,dt,0._dp,RGEAlphaS,kont)



alphaStop = gSM2(2)**2/4._dp/Pi



! m_top^pole to m_top^MS(m_top) 

mtopMS = mf_u(3)*(1._dp - 4._dp/3._dp*alphaStop/Pi)


! Running m_top^MS(m_top) to M_Z 

gSM3(1)=gSM2(1) 
gSM3(2)=gSM2(2)
gSM3(3)=mtopMS

tz=Log(sqrt(mz2)/mf_u(3)) 
dt=tz/50._dp 
Call odeint(gSM3,3,0._dp,tz,deltaM,dt,0._dp,RGEtop,kont)


mf_u_mz_running = gSM3(3)


RunningTopMZ = .True.

End if

! Starting values at MZ

gSM(1)=sqrt(Alpha_mZ*4*Pi) 
gSM(2)=sqrt(AlphaS_mZ*4*Pi) 
gSM(3)= 0.486E-03_dp ! mf_l_mz(1) 
gSM(4)= 0.10272 !mf_l_mz(2) 
gSM(5)= 1.74624 !mf_l_mz(3) 
gSM(6)= 1.27E-03_dp ! mf_u_mz(1) 
gSM(7)= 0.619  ! mf_u_mz(2) 
gSM(8)= mf_u_mz_running ! m_top 
gSM(9)= 2.9E-03_dp !mf_d_mz(1) 
gSM(10)= 0.055 !mf_d_mz(2) 
gSM(11)= 2.85 ! mf_d_mz(3) 
 

! To get the running sin(w2) 
SinW2 = 0.22290_dp
gSM(12) = 5._dp/3._dp*Alpha_MZ/(1-sinW2)
gSM(13) = Alpha_MZ/Sinw2
gSM(14) = AlphaS_mZ

  nUp =2._dp 
  nDown =3._dp 
  nLep =3._dp 
 

If (scale_out.gt.sqrt(mz2)) Then

 ! From M_Z to Min(M_top,scale_out) 
 If (scale_out.gt.mf_u(3)) Then 
  tz=Log(sqrt(mz2)/mf_u(3)) 
  dt=tz/50._dp 
 Else 
  tz=Log(sqrt(mz2)/scale_out) 
  dt=tz/50._dp 
 End if 

  Call odeint(gSM,14,tz,0._dp,deltaM,dt,0._dp,rge11,kont)


 ! From M_top to M_SUSY if M_top < M_SUSY 
 If (scale_out.gt.mf_u(3)) Then 
  tz=Log(mf_u(3)/scale_out) 
  dt=tz/50._dp 
  nUp =3._dp 
  Call odeint(gSM,14,tz,0._dp,deltaM,dt,0._dp,rge11,kont)
 End if 
Else

 ! From M_Z down to scale_out
  tz=Log(scale_out/sqrt(mz2)) 
  dt=tz/50._dp 
  Call odeint(gSM,14,0._dp,tz,deltaM,dt,0._dp,rge11_SMa,kont)

End if

! Calculating Couplings 

 sinW2=1._dp-mW2/mZ2 
 vev=Sqrt(mZ2*(1._dp-sinW2)*SinW2/(gSM(1)**2/4._dp))
 v = vev
 
Yd(1,1) =gSM(9)*sqrt(2._dp)/v 
Yd(2,2) =gSM(10)*sqrt(2._dp)/v 
Yd(3,3) =gSM(11)*sqrt(2._dp)/v 
Ye(1,1) =gSM(3)*sqrt(2._dp)/v 
Ye(2,2)=gSM(4)*sqrt(2._dp)/v 
Ye(3,3)=gSM(5)*sqrt(2._dp)/v 
Yu(1,1)=gSM(6)*sqrt(2._dp)/v 
Yu(2,2)=gSM(7)*sqrt(2._dp)/v 
Yu(3,3)=gSM(8)*sqrt(2._dp)/v 


g3 =gSM(2) 
g3running=gSM(2) 

g1 = sqrt(gSM(12)*4._dp*Pi*3._dp/5._dp)
g2 = sqrt(gSM(13)*4._dp*Pi)
! g3 = sqrt(gSM(3)*4._dp*Pi)

sinw2 = g1**2/(g1**2 + g2**2)

g2=gSM(1)/sqrt(sinW2) 
g1 = g2*Sqrt(sinW2/(1._dp-sinW2)) 

If (GenerationMixing) Then 
If (TransposedYukawa) Then ! check, if superpotential is Yu Hu u q  or Yu Hu q u
 Yu= Matmul(Transpose(CKM),Transpose(Yu))
Else 
 Yu=Transpose(Matmul(Transpose(CKM),Transpose(Yu)))
End if 
End If


End Subroutine RunSMgauge
End Module CouplingsForDecays_SDdiracDM
