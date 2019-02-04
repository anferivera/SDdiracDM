! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:22 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module Wrapper_OneLoopDecay_Fe_SDdiracDM
Use Model_Data_SDdiracDM 
Use Kinematics 
Use OneLoopDecay_Fe_SDdiracDM 
Use Control 
Use Settings 

 
Contains

 
Subroutine OneLoopDecay_Fe(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,MFeOS,              & 
& MFe2OS,MFvOS,MFv2OS,MFxvOS,MFxv2OS,MSscOS,MSsc2OS,MFxeOS,MFxe2OS,MHpOS,MHp2OS,         & 
& MAhOS,MAh2OS,MVZOS,MVZ2OS,MVWpOS,MVWp2OS,ZHOS,ZDLOS,ZDROS,ZULOS,ZUROS,ZELOS,           & 
& ZEROS,UVOS,UVROS,XVOS,XUOS,VSsOS,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,              & 
& MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,           & 
& TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,              & 
& LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,              & 
& MS22,mP2,vvSM,vS,dg1,dg2,dg3,dYu,dYd,dYe,dm2SM,dLam,dMDF,dYRD,dMS12,dMS22,             & 
& dLS1H,dLS,dLS2H,dmP2,dLSP,dLSPH,dYRB1,dYRB2,dYRC,dYRA1,dYRA2,dvvSM,dvS,dZH,            & 
& dZDL,dZDR,dZUL,dZUR,dZEL,dZER,dUV,dUVR,dXV,dXU,dVSs,dSinTW,dCosTW,dTanTW,              & 
& ZfVG,ZfHp,Zfed,Zfeu,ZfAh,ZfVP,ZfVZ,ZfVWp,Zfhh,ZfDL,ZfDR,ZfUL,ZfUR,ZfEL,ZfER,           & 
& ZfVL,ZfVR,ZfxVL,ZfxVR,ZfSsc,ZfVPVZ,ZfVZVP,cplAhAhhh,cplAhcHpVWp,cplAhhhVZ,             & 
& cplAhHpcVWp,cplcFeFeAhL,cplcFeFeAhR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,               & 
& cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,           & 
& cplcFeFvcVWpR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFvFeHpL,cplcFvFeHpR,cplcFvFeVWpL,      & 
& cplcFvFeVWpR,cplcFvFvVZL,cplcFvFvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFeSscL,      & 
& cplcFxeFeSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL,  & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplcFxvFvSscL,cplcFxvFvSscR,            & 
& cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcHpVPVWp,cplcHpVWpVZ,     & 
& cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,cplhhhhhh,cplhhHpcHp,               & 
& cplhhHpcVWp,cplhhSsccSsc,cplhhVZVZ,cplHpcHpVP,cplHpcHpVZ,cplHpcVWpVP,cplHpcVWpVZ,      & 
& ctcplcFeFeAhL,ctcplcFeFeAhR,ctcplcFeFehhL,ctcplcFeFehhR,ctcplcFeFeVPL,ctcplcFeFeVPR,   & 
& ctcplcFeFeVZL,ctcplcFeFeVZR,ctcplcFeFvcHpL,ctcplcFeFvcHpR,ctcplcFeFvcVWpL,             & 
& ctcplcFeFvcVWpR,ctcplcFeFxecSscL,ctcplcFeFxecSscR,GcplcFeFvcHpL,GcplcFeFvcHpR,         & 
& GcplcHpVPVWp,GcplHpcVWpVP,GosZcplcFeFvcHpL,GosZcplcFeFvcHpR,GosZcplcHpVPVWp,           & 
& GosZcplHpcVWpVP,GZcplcFeFvcHpL,GZcplcFeFvcHpR,GZcplcHpVPVWp,GZcplHpcVWpVP,             & 
& ZcplcFeFeAhL,ZcplcFeFeAhR,ZcplcFeFehhL,ZcplcFeFehhR,ZcplcFeFeVPL,ZcplcFeFeVPR,         & 
& ZcplcFeFeVZL,ZcplcFeFeVZR,ZcplcFeFvcHpL,ZcplcFeFvcHpR,ZcplcFeFvcVWpL,ZcplcFeFvcVWpR,   & 
& ZcplcFeFxecSscL,ZcplcFeFxecSscR,ZcplcFvFeHpL,ZcplcFvFeHpR,ZcplcFvFeVWpL,               & 
& ZcplcFvFeVWpR,ZcplcFxeFeSscL,ZcplcFxeFeSscR,ZcplcFxeFxeVPL,ZcplcFxeFxeVPR,             & 
& ZcplcHpVPVWp,ZcplcVWpVPVWp,ZcplHpcHpVP,ZcplHpcVWpVP,ZRUZH,ZRUVd,ZRUUd,ZRUVu,           & 
& ZRUUu,ZRUVe,ZRUUe,ZRUVv,ZRUVvr,ZRUXV,ZRUXU,ZRUVSs,MLambda,em,gs,deltaM,kont,gP1LFe)

Implicit None 
Real(dp),Intent(in) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2

Complex(dp),Intent(in) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Real(dp),Intent(in) :: vvSM,vS

Real(dp),Intent(in) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp),Intent(in) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Real(dp),Intent(in) :: dg1,dg2,dg3,dm2SM,dMDF,dYRD,dMS12,dMS22,dLS1H,dLS,dLS2H,dmP2,dLSP,dLSPH,              & 
& dYRB1(3),dYRB2(3),dYRC,dYRA1(3),dYRA2(3),dvvSM,dvS,dZH(2,2),dVSs(2,2),dSinTW,          & 
& dCosTW,dTanTW

Complex(dp),Intent(in) :: dYu(3,3),dYd(3,3),dYe(3,3),dLam,dZDL(3,3),dZDR(3,3),dZUL(3,3),dZUR(3,3),              & 
& dZEL(3,3),dZER(3,3),dUV(3,3),dUVR(3,3),dXV(2,2),dXU(2,2)

Complex(dp),Intent(in) :: cplAhAhhh(2),cplAhcHpVWp,cplAhhhVZ(2),cplAhHpcVWp,cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),  & 
& cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),               & 
& cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),& 
& cplcFeFvcVWpR(3,3),cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFvFeHpL(3,3),           & 
& cplcFvFeHpR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),& 
& cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),     & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL(2),             & 
& cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplcFxvFvSscL(2,3,2),          & 
& cplcFxvFvSscR(2,3,2),cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxvFxeVWpL(2),              & 
& cplcFxvFxeVWpR(2),cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp(2),    & 
& cplhhcVWpVWp(2),cplhhhhhh(2,2,2),cplhhHpcHp(2),cplhhHpcVWp(2),cplhhSsccSsc(2,2,2),     & 
& cplhhVZVZ(2),cplHpcHpVP,cplHpcHpVZ,cplHpcVWpVP,cplHpcVWpVZ,ctcplcFeFeAhL(3,3),         & 
& ctcplcFeFeAhR(3,3),ctcplcFeFehhL(3,3,2),ctcplcFeFehhR(3,3,2),ctcplcFeFeVPL(3,3),       & 
& ctcplcFeFeVPR(3,3),ctcplcFeFeVZL(3,3),ctcplcFeFeVZR(3,3),ctcplcFeFvcHpL(3,3),          & 
& ctcplcFeFvcHpR(3,3),ctcplcFeFvcVWpL(3,3),ctcplcFeFvcVWpR(3,3),ctcplcFeFxecSscL(3,2),   & 
& ctcplcFeFxecSscR(3,2),GcplcFeFvcHpL(3,3),GcplcFeFvcHpR(3,3),GcplcHpVPVWp,              & 
& GcplHpcVWpVP,GosZcplcFeFvcHpL(3,3),GosZcplcFeFvcHpR(3,3),GosZcplcHpVPVWp,              & 
& GosZcplHpcVWpVP,GZcplcFeFvcHpL(3,3),GZcplcFeFvcHpR(3,3),GZcplcHpVPVWp,GZcplHpcVWpVP,   & 
& ZcplcFeFeAhL(3,3),ZcplcFeFeAhR(3,3),ZcplcFeFehhL(3,3,2),ZcplcFeFehhR(3,3,2),           & 
& ZcplcFeFeVPL(3,3),ZcplcFeFeVPR(3,3),ZcplcFeFeVZL(3,3),ZcplcFeFeVZR(3,3),               & 
& ZcplcFeFvcHpL(3,3),ZcplcFeFvcHpR(3,3),ZcplcFeFvcVWpL(3,3),ZcplcFeFvcVWpR(3,3),         & 
& ZcplcFeFxecSscL(3,2),ZcplcFeFxecSscR(3,2),ZcplcFvFeHpL(3,3),ZcplcFvFeHpR(3,3),         & 
& ZcplcFvFeVWpL(3,3),ZcplcFvFeVWpR(3,3),ZcplcFxeFeSscL(3,2),ZcplcFxeFeSscR(3,2),         & 
& ZcplcFxeFxeVPL,ZcplcFxeFxeVPR,ZcplcHpVPVWp,ZcplcVWpVPVWp,ZcplHpcHpVP,ZcplHpcVWpVP

Real(dp), Intent(in) :: em, gs 
Complex(dp),Intent(in) :: ZfVG,ZfHp,Zfed,Zfeu,ZfAh,ZfVP,ZfVZ,ZfVWp,Zfhh(2,2),ZfDL(3,3),ZfDR(3,3),               & 
& ZfUL(3,3),ZfUR(3,3),ZfEL(3,3),ZfER(3,3),ZfVL(3,3),ZfVR(3,3),ZfxVL(2,2),ZfxVR(2,2),     & 
& ZfSsc(2,2),ZfVPVZ,ZfVZVP

Real(dp),Intent(in) :: MhhOS(2),Mhh2OS(2),MFdOS(3),MFd2OS(3),MFuOS(3),MFu2OS(3),MFeOS(3),MFe2OS(3),          & 
& MFvOS(3),MFv2OS(3),MFxvOS(2),MFxv2OS(2),MSscOS(2),MSsc2OS(2),MFxeOS,MFxe2OS,           & 
& MHpOS,MHp2OS,MAhOS,MAh2OS,MVZOS,MVZ2OS,MVWpOS,MVWp2OS,ZHOS(2,2),VSsOS(2,2)

Complex(dp),Intent(in) :: ZDLOS(3,3),ZDROS(3,3),ZULOS(3,3),ZUROS(3,3),ZELOS(3,3),ZEROS(3,3),UVOS(3,3),          & 
& UVROS(3,3),XVOS(2,2),XUOS(2,2)

Complex(dp),Intent(in) :: ZRUZH(2,2),ZRUVd(3,3),ZRUUd(3,3),ZRUVu(3,3),ZRUUu(3,3),ZRUVe(3,3),ZRUUe(3,3),         & 
& ZRUVv(3,3),ZRUVvr(3,3),ZRUXV(2,2),ZRUXU(2,2),ZRUVSs(2,2)

Real(dp), Intent(in) :: MLambda, deltaM 
Real(dp), Intent(out) :: gP1LFe(3,17) 
Integer, Intent(out) :: kont 
Real(dp) :: MVG,MVP,MVG2,MVP2, helfactor, phasespacefactor 
Integer :: i1,i2,i3,i4, isave, gt1, gt2, gt3 

Complex(dp) :: ZRUZHc(2, 2) 
Complex(dp) :: ZRUVdc(3, 3) 
Complex(dp) :: ZRUUdc(3, 3) 
Complex(dp) :: ZRUVuc(3, 3) 
Complex(dp) :: ZRUUuc(3, 3) 
Complex(dp) :: ZRUVec(3, 3) 
Complex(dp) :: ZRUUec(3, 3) 
Complex(dp) :: ZRUVvc(3, 3) 
Complex(dp) :: ZRUVvrc(3, 3) 
Complex(dp) :: ZRUXVc(2, 2) 
Complex(dp) :: ZRUXUc(2, 2) 
Complex(dp) :: ZRUVSsc(2, 2) 
Real(dp) :: MRPFeToFehh(3,3,2),MRGFeToFehh(3,3,2), MRPZFeToFehh(3,3,2),MRGZFeToFehh(3,3,2) 
Real(dp) :: MVPFeToFehh(3,3,2) 
Real(dp) :: RMsqTreeFeToFehh(3,3,2),RMsqWaveFeToFehh(3,3,2),RMsqVertexFeToFehh(3,3,2) 
Complex(dp) :: AmpTreeFeToFehh(2,3,3,2),AmpWaveFeToFehh(2,3,3,2)=(0._dp,0._dp),AmpVertexFeToFehh(2,3,3,2)& 
 & ,AmpVertexIRosFeToFehh(2,3,3,2),AmpVertexIRdrFeToFehh(2,3,3,2), AmpSumFeToFehh(2,3,3,2), AmpSum2FeToFehh(2,3,3,2) 
Complex(dp) :: AmpTreeZFeToFehh(2,3,3,2),AmpWaveZFeToFehh(2,3,3,2),AmpVertexZFeToFehh(2,3,3,2) 
Real(dp) :: AmpSqFeToFehh(3,3,2),  AmpSqTreeFeToFehh(3,3,2) 
Real(dp) :: MRPFeToFeVZ(3,3),MRGFeToFeVZ(3,3), MRPZFeToFeVZ(3,3),MRGZFeToFeVZ(3,3) 
Real(dp) :: MVPFeToFeVZ(3,3) 
Real(dp) :: RMsqTreeFeToFeVZ(3,3),RMsqWaveFeToFeVZ(3,3),RMsqVertexFeToFeVZ(3,3) 
Complex(dp) :: AmpTreeFeToFeVZ(4,3,3),AmpWaveFeToFeVZ(4,3,3)=(0._dp,0._dp),AmpVertexFeToFeVZ(4,3,3)& 
 & ,AmpVertexIRosFeToFeVZ(4,3,3),AmpVertexIRdrFeToFeVZ(4,3,3), AmpSumFeToFeVZ(4,3,3), AmpSum2FeToFeVZ(4,3,3) 
Complex(dp) :: AmpTreeZFeToFeVZ(4,3,3),AmpWaveZFeToFeVZ(4,3,3),AmpVertexZFeToFeVZ(4,3,3) 
Real(dp) :: AmpSqFeToFeVZ(3,3),  AmpSqTreeFeToFeVZ(3,3) 
Real(dp) :: MRPFeToFvcVWp(3,3),MRGFeToFvcVWp(3,3), MRPZFeToFvcVWp(3,3),MRGZFeToFvcVWp(3,3) 
Real(dp) :: MVPFeToFvcVWp(3,3) 
Real(dp) :: RMsqTreeFeToFvcVWp(3,3),RMsqWaveFeToFvcVWp(3,3),RMsqVertexFeToFvcVWp(3,3) 
Complex(dp) :: AmpTreeFeToFvcVWp(4,3,3),AmpWaveFeToFvcVWp(4,3,3)=(0._dp,0._dp),AmpVertexFeToFvcVWp(4,3,3)& 
 & ,AmpVertexIRosFeToFvcVWp(4,3,3),AmpVertexIRdrFeToFvcVWp(4,3,3), AmpSumFeToFvcVWp(4,3,3), AmpSum2FeToFvcVWp(4,3,3) 
Complex(dp) :: AmpTreeZFeToFvcVWp(4,3,3),AmpWaveZFeToFvcVWp(4,3,3),AmpVertexZFeToFvcVWp(4,3,3) 
Real(dp) :: AmpSqFeToFvcVWp(3,3),  AmpSqTreeFeToFvcVWp(3,3) 
Real(dp) :: MRPFeToFxecSsc(3,2),MRGFeToFxecSsc(3,2), MRPZFeToFxecSsc(3,2),MRGZFeToFxecSsc(3,2) 
Real(dp) :: MVPFeToFxecSsc(3,2) 
Real(dp) :: RMsqTreeFeToFxecSsc(3,2),RMsqWaveFeToFxecSsc(3,2),RMsqVertexFeToFxecSsc(3,2) 
Complex(dp) :: AmpTreeFeToFxecSsc(2,3,2),AmpWaveFeToFxecSsc(2,3,2)=(0._dp,0._dp),AmpVertexFeToFxecSsc(2,3,2)& 
 & ,AmpVertexIRosFeToFxecSsc(2,3,2),AmpVertexIRdrFeToFxecSsc(2,3,2), AmpSumFeToFxecSsc(2,3,2), AmpSum2FeToFxecSsc(2,3,2) 
Complex(dp) :: AmpTreeZFeToFxecSsc(2,3,2),AmpWaveZFeToFxecSsc(2,3,2),AmpVertexZFeToFxecSsc(2,3,2) 
Real(dp) :: AmpSqFeToFxecSsc(3,2),  AmpSqTreeFeToFxecSsc(3,2) 
Real(dp) :: MRPFeToFeVP(3,3),MRGFeToFeVP(3,3), MRPZFeToFeVP(3,3),MRGZFeToFeVP(3,3) 
Real(dp) :: MVPFeToFeVP(3,3) 
Real(dp) :: RMsqTreeFeToFeVP(3,3),RMsqWaveFeToFeVP(3,3),RMsqVertexFeToFeVP(3,3) 
Complex(dp) :: AmpTreeFeToFeVP(4,3,3),AmpWaveFeToFeVP(4,3,3)=(0._dp,0._dp),AmpVertexFeToFeVP(4,3,3)& 
 & ,AmpVertexIRosFeToFeVP(4,3,3),AmpVertexIRdrFeToFeVP(4,3,3), AmpSumFeToFeVP(4,3,3), AmpSum2FeToFeVP(4,3,3) 
Complex(dp) :: AmpTreeZFeToFeVP(4,3,3),AmpWaveZFeToFeVP(4,3,3),AmpVertexZFeToFeVP(4,3,3) 
Real(dp) :: AmpSqFeToFeVP(3,3),  AmpSqTreeFeToFeVP(3,3) 
Write(*,*) "Calculating one-loop decays of Fe " 
kont = 0 
MVG = MLambda 
MVP = MLambda 
MVG2 = MLambda**2 
MVP2 = MLambda**2 

ZRUZHc = Conjg(ZRUZH)
ZRUVdc = Conjg(ZRUVd)
ZRUUdc = Conjg(ZRUUd)
ZRUVuc = Conjg(ZRUVu)
ZRUUuc = Conjg(ZRUUu)
ZRUVec = Conjg(ZRUVe)
ZRUUec = Conjg(ZRUUe)
ZRUVvc = Conjg(ZRUVv)
ZRUVvrc = Conjg(ZRUVvr)
ZRUXVc = Conjg(ZRUXV)
ZRUXUc = Conjg(ZRUXU)
ZRUVSsc = Conjg(ZRUVSs)

 ! Counter 
isave = 1 

If (.not.CalcLoopDecay_LoopInducedOnly) Then 
!---------------- 
! Fe hh
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_FeToFehh(cplcFeFehhL,cplcFeFehhR,MFe,Mhh,               & 
& MFe2,Mhh2,AmpTreeFeToFehh)

  Else 
Call Amplitude_Tree_SDdiracDM_FeToFehh(ZcplcFeFehhL,ZcplcFeFehhR,MFe,Mhh,             & 
& MFe2,Mhh2,AmpTreeFeToFehh)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_FeToFehh(MLambda,em,gs,cplcFeFehhL,cplcFeFehhR,             & 
& MFeOS,MhhOS,MRPFeToFehh,MRGFeToFehh)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_FeToFehh(MLambda,em,gs,ZcplcFeFehhL,ZcplcFeFehhR,           & 
& MFeOS,MhhOS,MRPFeToFehh,MRGFeToFehh)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_FeToFehh(MLambda,em,gs,cplcFeFehhL,cplcFeFehhR,             & 
& MFe,Mhh,MRPFeToFehh,MRGFeToFehh)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_FeToFehh(MLambda,em,gs,ZcplcFeFehhL,ZcplcFeFehhR,           & 
& MFe,Mhh,MRPFeToFehh,MRGFeToFehh)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FeToFehh(cplcFeFehhL,cplcFeFehhR,ctcplcFeFehhL,         & 
& ctcplcFeFehhR,MFe,MFe2,Mhh,Mhh2,ZfEL,ZfER,Zfhh,AmpWaveFeToFehh)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FeToFehh(MAh,MFe,MFv,MFxe,Mhh,MHp,MSsc,               & 
& MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplAhAhhh,           & 
& cplcFeFeAhL,cplcFeFeAhR,cplAhhhVZ,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,     & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,         & 
& cplcFeFxecSscL,cplcFeFxecSscR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,           & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexFeToFehh)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFehh(MAh,MFe,MFv,MFxe,Mhh,MHp,MSsc,            & 
& MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplAhAhhh,           & 
& cplcFeFeAhL,cplcFeFeAhR,cplAhhhVZ,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,     & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,         & 
& cplcFeFxecSscL,cplcFeFxecSscR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,           & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRdrFeToFehh)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFehh(MAhOS,MFeOS,MFvOS,MFxeOS,MhhOS,           & 
& MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,Mhh2OS,MHp2OS,              & 
& MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplAhAhhh,cplcFeFeAhL,cplcFeFeAhR,cplAhhhVZ,               & 
& ZcplcFeFehhL,ZcplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,         & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,cplcFeFeVZR,             & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,cplcFeFxecSscR,   & 
& cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,      & 
& AmpVertexIRosFeToFehh)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFehh(MAh,MFe,MFv,MFxe,Mhh,MHp,MSsc,            & 
& MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplAhAhhh,           & 
& cplcFeFeAhL,cplcFeFeAhR,cplAhhhVZ,ZcplcFeFehhL,ZcplcFeFehhR,cplcFvFeHpL,               & 
& cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,          & 
& cplcFvFeVWpR,cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,          & 
& cplcFeFvcVWpR,cplcFeFxecSscL,cplcFeFxecSscR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,          & 
& cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRosFeToFehh)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFehh(MAhOS,MFeOS,MFvOS,MFxeOS,MhhOS,           & 
& MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,Mhh2OS,MHp2OS,              & 
& MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplAhAhhh,cplcFeFeAhL,cplcFeFeAhR,cplAhhhVZ,               & 
& cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,           & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,cplcFeFeVZR,             & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,cplcFeFxecSscR,   & 
& cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,      & 
& AmpVertexIRosFeToFehh)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFehh(MAh,MFe,MFv,MFxe,Mhh,MHp,MSsc,            & 
& MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplAhAhhh,           & 
& cplcFeFeAhL,cplcFeFeAhR,cplAhhhVZ,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,     & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,         & 
& cplcFeFxecSscL,cplcFeFxecSscR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,           & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRosFeToFehh)

 End if 
 End if 
AmpVertexFeToFehh = AmpVertexFeToFehh -  AmpVertexIRdrFeToFehh! +  AmpVertexIRosFeToFehh ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZFeToFehh=0._dp 
AmpVertexZFeToFehh=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFeToFehh(1,gt2,:,:) = AmpWaveZFeToFehh(1,gt2,:,:)+ZRUUe(gt2,gt1)*AmpWaveFeToFehh(1,gt1,:,:) 
AmpVertexZFeToFehh(1,gt2,:,:)= AmpVertexZFeToFehh(1,gt2,:,:) + ZRUUe(gt2,gt1)*AmpVertexFeToFehh(1,gt1,:,:) 
AmpWaveZFeToFehh(2,gt2,:,:) = AmpWaveZFeToFehh(2,gt2,:,:)+ZRUVec(gt2,gt1)*AmpWaveFeToFehh(2,gt1,:,:) 
AmpVertexZFeToFehh(2,gt2,:,:)= AmpVertexZFeToFehh(2,gt2,:,:) + ZRUVec(gt2,gt1)*AmpVertexFeToFehh(2,gt1,:,:) 
 End Do 
End Do 
AmpWaveFeToFehh=AmpWaveZFeToFehh 
AmpVertexFeToFehh= AmpVertexZFeToFehh
! Final State 1 
AmpWaveZFeToFehh=0._dp 
AmpVertexZFeToFehh=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFeToFehh(1,:,gt2,:) = AmpWaveZFeToFehh(1,:,gt2,:)+ZRUVe(gt2,gt1)*AmpWaveFeToFehh(1,:,gt1,:) 
AmpVertexZFeToFehh(1,:,gt2,:)= AmpVertexZFeToFehh(1,:,gt2,:)+ZRUVe(gt2,gt1)*AmpVertexFeToFehh(1,:,gt1,:) 
AmpWaveZFeToFehh(2,:,gt2,:) = AmpWaveZFeToFehh(2,:,gt2,:)+ZRUUec(gt2,gt1)*AmpWaveFeToFehh(2,:,gt1,:) 
AmpVertexZFeToFehh(2,:,gt2,:)= AmpVertexZFeToFehh(2,:,gt2,:)+ZRUUec(gt2,gt1)*AmpVertexFeToFehh(2,:,gt1,:) 
 End Do 
End Do 
AmpWaveFeToFehh=AmpWaveZFeToFehh 
AmpVertexFeToFehh= AmpVertexZFeToFehh
! Final State 2 
AmpWaveZFeToFehh=0._dp 
AmpVertexZFeToFehh=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZFeToFehh(:,:,:,gt2) = AmpWaveZFeToFehh(:,:,:,gt2)+ZRUZH(gt2,gt1)*AmpWaveFeToFehh(:,:,:,gt1) 
AmpVertexZFeToFehh(:,:,:,gt2)= AmpVertexZFeToFehh(:,:,:,gt2)+ZRUZH(gt2,gt1)*AmpVertexFeToFehh(:,:,:,gt1) 
 End Do 
End Do 
AmpWaveFeToFehh=AmpWaveZFeToFehh 
AmpVertexFeToFehh= AmpVertexZFeToFehh
End if
If (ShiftIRdiv) Then 
AmpVertexFeToFehh = AmpVertexFeToFehh  +  AmpVertexIRosFeToFehh
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fe->Fe hh -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumFeToFehh = AmpTreeFeToFehh 
 AmpSum2FeToFehh = AmpTreeFeToFehh + 2._dp*AmpWaveFeToFehh + 2._dp*AmpVertexFeToFehh  
Else 
 AmpSumFeToFehh = AmpTreeFeToFehh + AmpWaveFeToFehh + AmpVertexFeToFehh
 AmpSum2FeToFehh = AmpTreeFeToFehh + AmpWaveFeToFehh + AmpVertexFeToFehh 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumFeToFehh = AmpTreeFeToFehh
 AmpSum2FeToFehh = AmpTreeFeToFehh 
End if 
Do gt1=1,3
i4 = isave 
  Do gt2=1,3
    Do gt3=1,2
If (((OSkinematics).and.(MFeOS(gt1).gt.(MFeOS(gt2)+MhhOS(gt3)))).or.((.not.OSkinematics).and.(MFe(gt1).gt.(MFe(gt2)+Mhh(gt3))))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2, gt3 
  AmpSum2FeToFehh = AmpTreeFeToFehh
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFeOS(gt1),MFeOS(gt2),MhhOS(gt3),AmpSumFeToFehh(:,gt1, gt2, gt3),AmpSum2FeToFehh(:,gt1, gt2, gt3),AmpSqFeToFehh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFe(gt1),MFe(gt2),Mhh(gt3),AmpSumFeToFehh(:,gt1, gt2, gt3),AmpSum2FeToFehh(:,gt1, gt2, gt3),AmpSqFeToFehh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqFeToFehh(gt1, gt2, gt3) 
  AmpSum2FeToFehh = 2._dp*AmpWaveFeToFehh
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFeOS(gt1),MFeOS(gt2),MhhOS(gt3),AmpSumFeToFehh(:,gt1, gt2, gt3),AmpSum2FeToFehh(:,gt1, gt2, gt3),AmpSqFeToFehh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFe(gt1),MFe(gt2),Mhh(gt3),AmpSumFeToFehh(:,gt1, gt2, gt3),AmpSum2FeToFehh(:,gt1, gt2, gt3),AmpSqFeToFehh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqFeToFehh(gt1, gt2, gt3) 
  AmpSum2FeToFehh = 2._dp*AmpVertexFeToFehh
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFeOS(gt1),MFeOS(gt2),MhhOS(gt3),AmpSumFeToFehh(:,gt1, gt2, gt3),AmpSum2FeToFehh(:,gt1, gt2, gt3),AmpSqFeToFehh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFe(gt1),MFe(gt2),Mhh(gt3),AmpSumFeToFehh(:,gt1, gt2, gt3),AmpSum2FeToFehh(:,gt1, gt2, gt3),AmpSqFeToFehh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqFeToFehh(gt1, gt2, gt3) 
  AmpSum2FeToFehh = AmpTreeFeToFehh + 2._dp*AmpWaveFeToFehh + 2._dp*AmpVertexFeToFehh
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFeOS(gt1),MFeOS(gt2),MhhOS(gt3),AmpSumFeToFehh(:,gt1, gt2, gt3),AmpSum2FeToFehh(:,gt1, gt2, gt3),AmpSqFeToFehh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFe(gt1),MFe(gt2),Mhh(gt3),AmpSumFeToFehh(:,gt1, gt2, gt3),AmpSum2FeToFehh(:,gt1, gt2, gt3),AmpSqFeToFehh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqFeToFehh(gt1, gt2, gt3) 
 End if 
If (OSkinematics) Then 
  AmpSum2FeToFehh = AmpTreeFeToFehh
  Call SquareAmp_FtoFS(MFeOS(gt1),MFeOS(gt2),MhhOS(gt3),AmpSumFeToFehh(:,gt1, gt2, gt3),AmpSum2FeToFehh(:,gt1, gt2, gt3),AmpSqFeToFehh(gt1, gt2, gt3)) 
  AmpSqTreeFeToFehh(gt1, gt2, gt3) = AmpSqFeToFehh(gt1, gt2, gt3)  
  AmpSum2FeToFehh = + 2._dp*AmpWaveFeToFehh + 2._dp*AmpVertexFeToFehh
  Call SquareAmp_FtoFS(MFeOS(gt1),MFeOS(gt2),MhhOS(gt3),AmpSumFeToFehh(:,gt1, gt2, gt3),AmpSum2FeToFehh(:,gt1, gt2, gt3),AmpSqFeToFehh(gt1, gt2, gt3)) 
  AmpSqFeToFehh(gt1, gt2, gt3) = AmpSqFeToFehh(gt1, gt2, gt3) + AmpSqTreeFeToFehh(gt1, gt2, gt3)  
Else  
  AmpSum2FeToFehh = AmpTreeFeToFehh
  Call SquareAmp_FtoFS(MFe(gt1),MFe(gt2),Mhh(gt3),AmpSumFeToFehh(:,gt1, gt2, gt3),AmpSum2FeToFehh(:,gt1, gt2, gt3),AmpSqFeToFehh(gt1, gt2, gt3)) 
  AmpSqTreeFeToFehh(gt1, gt2, gt3) = AmpSqFeToFehh(gt1, gt2, gt3)  
  AmpSum2FeToFehh = + 2._dp*AmpWaveFeToFehh + 2._dp*AmpVertexFeToFehh
  Call SquareAmp_FtoFS(MFe(gt1),MFe(gt2),Mhh(gt3),AmpSumFeToFehh(:,gt1, gt2, gt3),AmpSum2FeToFehh(:,gt1, gt2, gt3),AmpSqFeToFehh(gt1, gt2, gt3)) 
  AmpSqFeToFehh(gt1, gt2, gt3) = AmpSqFeToFehh(gt1, gt2, gt3) + AmpSqTreeFeToFehh(gt1, gt2, gt3)  
End if  
Else  
  AmpSqFeToFehh(gt1, gt2, gt3) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFeToFehh(gt1, gt2, gt3).le.0._dp) Then 
  gP1LFe(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFe(gt1,i4) = 1._dp*GammaTPS(MFeOS(gt1),MFeOS(gt2),MhhOS(gt3),helfactor*AmpSqFeToFehh(gt1, gt2, gt3))
Else 
  gP1LFe(gt1,i4) = 1._dp*GammaTPS(MFe(gt1),MFe(gt2),Mhh(gt3),helfactor*AmpSqFeToFehh(gt1, gt2, gt3))
End if 
If ((Abs(MRPFeToFehh(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGFeToFehh(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFe(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPFeToFehh(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGFeToFehh(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPFeToFehh(gt1, gt2, gt3) + MRGFeToFehh(gt1, gt2, gt3)) 
  gP1LFe(gt1,i4) = gP1LFe(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPFeToFehh(gt1, gt2, gt3) + MRGFeToFehh(gt1, gt2, gt3))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LFe(gt1,i4) 
  End if 
End if 
End if 
i4=i4+1

    End do
  End do
If (gt1.eq.3) isave = i4 
End do
End If 
If (.not.CalcLoopDecay_LoopInducedOnly) Then 
!---------------- 
! Fe VZ
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_FeToFeVZ(cplcFeFeVZL,cplcFeFeVZR,MFe,MVZ,               & 
& MFe2,MVZ2,AmpTreeFeToFeVZ)

  Else 
Call Amplitude_Tree_SDdiracDM_FeToFeVZ(ZcplcFeFeVZL,ZcplcFeFeVZR,MFe,MVZ,             & 
& MFe2,MVZ2,AmpTreeFeToFeVZ)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_FeToFeVZ(MLambda,em,gs,cplcFeFeVZL,cplcFeFeVZR,             & 
& MFeOS,MVZOS,MRPFeToFeVZ,MRGFeToFeVZ)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_FeToFeVZ(MLambda,em,gs,ZcplcFeFeVZL,ZcplcFeFeVZR,           & 
& MFeOS,MVZOS,MRPFeToFeVZ,MRGFeToFeVZ)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_FeToFeVZ(MLambda,em,gs,cplcFeFeVZL,cplcFeFeVZR,             & 
& MFe,MVZ,MRPFeToFeVZ,MRGFeToFeVZ)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_FeToFeVZ(MLambda,em,gs,ZcplcFeFeVZL,ZcplcFeFeVZR,           & 
& MFe,MVZ,MRPFeToFeVZ,MRGFeToFeVZ)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FeToFeVZ(cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,           & 
& cplcFeFeVZR,ctcplcFeFeVPL,ctcplcFeFeVPR,ctcplcFeFeVZL,ctcplcFeFeVZR,MFe,               & 
& MFe2,MVP,MVP2,MVZ,MVZ2,ZfEL,ZfER,ZfVPVZ,ZfVZ,AmpWaveFeToFeVZ)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FeToFeVZ(MAh,MFe,MFv,MFxe,Mhh,MHp,MSsc,               & 
& MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFeFeAhL,         & 
& cplcFeFeAhR,cplAhhhVZ,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,   & 
& cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,           & 
& cplcFeFeVZR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,           & 
& cplcFeFvcVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,               & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexFeToFeVZ)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFeVZ(MAh,MFe,MFv,MFxe,Mhh,MHp,MSsc,            & 
& MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFeFeAhL,         & 
& cplcFeFeAhR,cplAhhhVZ,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,   & 
& cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,           & 
& cplcFeFeVZR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,           & 
& cplcFeFvcVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,               & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRdrFeToFeVZ)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFeVZ(MAhOS,MFeOS,MFvOS,MFxeOS,MhhOS,           & 
& MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,Mhh2OS,MHp2OS,              & 
& MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplcFeFeAhL,cplcFeFeAhR,cplAhhhVZ,cplcFeFehhL,             & 
& cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,           & 
& cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,ZcplcFeFeVZL,ZcplcFeFeVZR,cplcFvFvVZL,           & 
& cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxeVZL,       & 
& cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,          & 
& cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFeToFeVZ)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFeVZ(MAh,MFe,MFv,MFxe,Mhh,MHp,MSsc,            & 
& MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFeFeAhL,         & 
& cplcFeFeAhR,cplAhhhVZ,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,   & 
& cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,ZcplcFeFeVZL,          & 
& ZcplcFeFeVZR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,          & 
& cplcFeFvcVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,               & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFeToFeVZ)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFeVZ(MAhOS,MFeOS,MFvOS,MFxeOS,MhhOS,           & 
& MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,Mhh2OS,MHp2OS,              & 
& MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplcFeFeAhL,cplcFeFeAhR,cplAhhhVZ,cplcFeFehhL,             & 
& cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,           & 
& cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,cplcFeFeVZR,cplcFvFvVZL,             & 
& cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxeVZL,       & 
& cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,          & 
& cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFeToFeVZ)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFeVZ(MAh,MFe,MFv,MFxe,Mhh,MHp,MSsc,            & 
& MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFeFeAhL,         & 
& cplcFeFeAhR,cplAhhhVZ,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,   & 
& cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,           & 
& cplcFeFeVZR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,           & 
& cplcFeFvcVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,               & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFeToFeVZ)

 End if 
 End if 
AmpVertexFeToFeVZ = AmpVertexFeToFeVZ -  AmpVertexIRdrFeToFeVZ! +  AmpVertexIRosFeToFeVZ ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZFeToFeVZ=0._dp 
AmpVertexZFeToFeVZ=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFeToFeVZ(1,gt2,:) = AmpWaveZFeToFeVZ(1,gt2,:)+ZRUVec(gt2,gt1)*AmpWaveFeToFeVZ(1,gt1,:) 
AmpVertexZFeToFeVZ(1,gt2,:)= AmpVertexZFeToFeVZ(1,gt2,:) + ZRUVec(gt2,gt1)*AmpVertexFeToFeVZ(1,gt1,:) 
AmpWaveZFeToFeVZ(2,gt2,:) = AmpWaveZFeToFeVZ(2,gt2,:)+ZRUUe(gt2,gt1)*AmpWaveFeToFeVZ(2,gt1,:) 
AmpVertexZFeToFeVZ(2,gt2,:)= AmpVertexZFeToFeVZ(2,gt2,:) + ZRUUe(gt2,gt1)*AmpVertexFeToFeVZ(2,gt1,:) 
AmpWaveZFeToFeVZ(3,gt2,:) = AmpWaveZFeToFeVZ(3,gt2,:)+ZRUVec(gt2,gt1)*AmpWaveFeToFeVZ(3,gt1,:) 
AmpVertexZFeToFeVZ(3,gt2,:)= AmpVertexZFeToFeVZ(3,gt2,:) + ZRUVec(gt2,gt1)*AmpVertexFeToFeVZ(3,gt1,:) 
AmpWaveZFeToFeVZ(4,gt2,:) = AmpWaveZFeToFeVZ(4,gt2,:)+ZRUUe(gt2,gt1)*AmpWaveFeToFeVZ(4,gt1,:) 
AmpVertexZFeToFeVZ(4,gt2,:)= AmpVertexZFeToFeVZ(4,gt2,:) + ZRUUe(gt2,gt1)*AmpVertexFeToFeVZ(4,gt1,:) 
 End Do 
End Do 
AmpWaveFeToFeVZ=AmpWaveZFeToFeVZ 
AmpVertexFeToFeVZ= AmpVertexZFeToFeVZ
! Final State 1 
AmpWaveZFeToFeVZ=0._dp 
AmpVertexZFeToFeVZ=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFeToFeVZ(1,:,gt2) = AmpWaveZFeToFeVZ(1,:,gt2)+ZRUVe(gt2,gt1)*AmpWaveFeToFeVZ(1,:,gt1) 
AmpVertexZFeToFeVZ(1,:,gt2)= AmpVertexZFeToFeVZ(1,:,gt2)+ZRUVe(gt2,gt1)*AmpVertexFeToFeVZ(1,:,gt1) 
AmpWaveZFeToFeVZ(2,:,gt2) = AmpWaveZFeToFeVZ(2,:,gt2)+ZRUUec(gt2,gt1)*AmpWaveFeToFeVZ(2,:,gt1) 
AmpVertexZFeToFeVZ(2,:,gt2)= AmpVertexZFeToFeVZ(2,:,gt2)+ZRUUec(gt2,gt1)*AmpVertexFeToFeVZ(2,:,gt1) 
AmpWaveZFeToFeVZ(3,:,gt2) = AmpWaveZFeToFeVZ(3,:,gt2)+ZRUVe(gt2,gt1)*AmpWaveFeToFeVZ(3,:,gt1) 
AmpVertexZFeToFeVZ(3,:,gt2)= AmpVertexZFeToFeVZ(3,:,gt2)+ZRUVe(gt2,gt1)*AmpVertexFeToFeVZ(3,:,gt1) 
AmpWaveZFeToFeVZ(4,:,gt2) = AmpWaveZFeToFeVZ(4,:,gt2)+ZRUUec(gt2,gt1)*AmpWaveFeToFeVZ(4,:,gt1) 
AmpVertexZFeToFeVZ(4,:,gt2)= AmpVertexZFeToFeVZ(4,:,gt2)+ZRUUec(gt2,gt1)*AmpVertexFeToFeVZ(4,:,gt1) 
 End Do 
End Do 
AmpWaveFeToFeVZ=AmpWaveZFeToFeVZ 
AmpVertexFeToFeVZ= AmpVertexZFeToFeVZ
End if
If (ShiftIRdiv) Then 
AmpVertexFeToFeVZ = AmpVertexFeToFeVZ  +  AmpVertexIRosFeToFeVZ
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fe->Fe VZ -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumFeToFeVZ = AmpTreeFeToFeVZ 
 AmpSum2FeToFeVZ = AmpTreeFeToFeVZ + 2._dp*AmpWaveFeToFeVZ + 2._dp*AmpVertexFeToFeVZ  
Else 
 AmpSumFeToFeVZ = AmpTreeFeToFeVZ + AmpWaveFeToFeVZ + AmpVertexFeToFeVZ
 AmpSum2FeToFeVZ = AmpTreeFeToFeVZ + AmpWaveFeToFeVZ + AmpVertexFeToFeVZ 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumFeToFeVZ = AmpTreeFeToFeVZ
 AmpSum2FeToFeVZ = AmpTreeFeToFeVZ 
End if 
Do gt1=1,3
i4 = isave 
  Do gt2=1,3
If (((OSkinematics).and.(MFeOS(gt1).gt.(MFeOS(gt2)+MVZOS))).or.((.not.OSkinematics).and.(MFe(gt1).gt.(MFe(gt2)+MVZ)))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2 
  AmpSum2FeToFeVZ = AmpTreeFeToFeVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFeOS(gt1),MFeOS(gt2),MVZOS,AmpSumFeToFeVZ(:,gt1, gt2),AmpSum2FeToFeVZ(:,gt1, gt2),AmpSqFeToFeVZ(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFe(gt1),MFe(gt2),MVZ,AmpSumFeToFeVZ(:,gt1, gt2),AmpSum2FeToFeVZ(:,gt1, gt2),AmpSqFeToFeVZ(gt1, gt2)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqFeToFeVZ(gt1, gt2) 
  AmpSum2FeToFeVZ = 2._dp*AmpWaveFeToFeVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFeOS(gt1),MFeOS(gt2),MVZOS,AmpSumFeToFeVZ(:,gt1, gt2),AmpSum2FeToFeVZ(:,gt1, gt2),AmpSqFeToFeVZ(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFe(gt1),MFe(gt2),MVZ,AmpSumFeToFeVZ(:,gt1, gt2),AmpSum2FeToFeVZ(:,gt1, gt2),AmpSqFeToFeVZ(gt1, gt2)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqFeToFeVZ(gt1, gt2) 
  AmpSum2FeToFeVZ = 2._dp*AmpVertexFeToFeVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFeOS(gt1),MFeOS(gt2),MVZOS,AmpSumFeToFeVZ(:,gt1, gt2),AmpSum2FeToFeVZ(:,gt1, gt2),AmpSqFeToFeVZ(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFe(gt1),MFe(gt2),MVZ,AmpSumFeToFeVZ(:,gt1, gt2),AmpSum2FeToFeVZ(:,gt1, gt2),AmpSqFeToFeVZ(gt1, gt2)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqFeToFeVZ(gt1, gt2) 
  AmpSum2FeToFeVZ = AmpTreeFeToFeVZ + 2._dp*AmpWaveFeToFeVZ + 2._dp*AmpVertexFeToFeVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFeOS(gt1),MFeOS(gt2),MVZOS,AmpSumFeToFeVZ(:,gt1, gt2),AmpSum2FeToFeVZ(:,gt1, gt2),AmpSqFeToFeVZ(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFe(gt1),MFe(gt2),MVZ,AmpSumFeToFeVZ(:,gt1, gt2),AmpSum2FeToFeVZ(:,gt1, gt2),AmpSqFeToFeVZ(gt1, gt2)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqFeToFeVZ(gt1, gt2) 
 End if 
If (OSkinematics) Then 
  AmpSum2FeToFeVZ = AmpTreeFeToFeVZ
  Call SquareAmp_FtoFV(MFeOS(gt1),MFeOS(gt2),MVZOS,AmpSumFeToFeVZ(:,gt1, gt2),AmpSum2FeToFeVZ(:,gt1, gt2),AmpSqFeToFeVZ(gt1, gt2)) 
  AmpSqTreeFeToFeVZ(gt1, gt2) = AmpSqFeToFeVZ(gt1, gt2)  
  AmpSum2FeToFeVZ = + 2._dp*AmpWaveFeToFeVZ + 2._dp*AmpVertexFeToFeVZ
  Call SquareAmp_FtoFV(MFeOS(gt1),MFeOS(gt2),MVZOS,AmpSumFeToFeVZ(:,gt1, gt2),AmpSum2FeToFeVZ(:,gt1, gt2),AmpSqFeToFeVZ(gt1, gt2)) 
  AmpSqFeToFeVZ(gt1, gt2) = AmpSqFeToFeVZ(gt1, gt2) + AmpSqTreeFeToFeVZ(gt1, gt2)  
Else  
  AmpSum2FeToFeVZ = AmpTreeFeToFeVZ
  Call SquareAmp_FtoFV(MFe(gt1),MFe(gt2),MVZ,AmpSumFeToFeVZ(:,gt1, gt2),AmpSum2FeToFeVZ(:,gt1, gt2),AmpSqFeToFeVZ(gt1, gt2)) 
  AmpSqTreeFeToFeVZ(gt1, gt2) = AmpSqFeToFeVZ(gt1, gt2)  
  AmpSum2FeToFeVZ = + 2._dp*AmpWaveFeToFeVZ + 2._dp*AmpVertexFeToFeVZ
  Call SquareAmp_FtoFV(MFe(gt1),MFe(gt2),MVZ,AmpSumFeToFeVZ(:,gt1, gt2),AmpSum2FeToFeVZ(:,gt1, gt2),AmpSqFeToFeVZ(gt1, gt2)) 
  AmpSqFeToFeVZ(gt1, gt2) = AmpSqFeToFeVZ(gt1, gt2) + AmpSqTreeFeToFeVZ(gt1, gt2)  
End if  
Else  
  AmpSqFeToFeVZ(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFeToFeVZ(gt1, gt2).le.0._dp) Then 
  gP1LFe(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFe(gt1,i4) = 1._dp*GammaTPS(MFeOS(gt1),MFeOS(gt2),MVZOS,helfactor*AmpSqFeToFeVZ(gt1, gt2))
Else 
  gP1LFe(gt1,i4) = 1._dp*GammaTPS(MFe(gt1),MFe(gt2),MVZ,helfactor*AmpSqFeToFeVZ(gt1, gt2))
End if 
If ((Abs(MRPFeToFeVZ(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFeToFeVZ(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFe(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPFeToFeVZ(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFeToFeVZ(gt1, gt2)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPFeToFeVZ(gt1, gt2) + MRGFeToFeVZ(gt1, gt2)) 
  gP1LFe(gt1,i4) = gP1LFe(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPFeToFeVZ(gt1, gt2) + MRGFeToFeVZ(gt1, gt2))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LFe(gt1,i4) 
  End if 
End if 
End if 
i4=i4+1

  End do
If (gt1.eq.3) isave = i4 
End do
End If 
If (.not.CalcLoopDecay_LoopInducedOnly) Then 
!---------------- 
! Fv Conjg(VWp)
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_FeToFvcVWp(cplcFeFvcVWpL,cplcFeFvcVWpR,MFe,             & 
& MFv,MVWp,MFe2,MFv2,MVWp2,AmpTreeFeToFvcVWp)

  Else 
Call Amplitude_Tree_SDdiracDM_FeToFvcVWp(ZcplcFeFvcVWpL,ZcplcFeFvcVWpR,               & 
& MFe,MFv,MVWp,MFe2,MFv2,MVWp2,AmpTreeFeToFvcVWp)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_FeToFvcVWp(MLambda,em,gs,cplcFeFvcVWpL,cplcFeFvcVWpR,       & 
& MFeOS,MFvOS,MVWpOS,MRPFeToFvcVWp,MRGFeToFvcVWp)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_FeToFvcVWp(MLambda,em,gs,ZcplcFeFvcVWpL,ZcplcFeFvcVWpR,     & 
& MFeOS,MFvOS,MVWpOS,MRPFeToFvcVWp,MRGFeToFvcVWp)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_FeToFvcVWp(MLambda,em,gs,cplcFeFvcVWpL,cplcFeFvcVWpR,       & 
& MFe,MFv,MVWp,MRPFeToFvcVWp,MRGFeToFvcVWp)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_FeToFvcVWp(MLambda,em,gs,ZcplcFeFvcVWpL,ZcplcFeFvcVWpR,     & 
& MFe,MFv,MVWp,MRPFeToFvcVWp,MRGFeToFvcVWp)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FeToFvcVWp(cplcFeFvcVWpL,cplcFeFvcVWpR,ctcplcFeFvcVWpL, & 
& ctcplcFeFvcVWpR,MFe,MFe2,MFv,MFv2,MVWp,MVWp2,ZfEL,ZfER,ZfVL,ZfVR,ZfVWp,AmpWaveFeToFvcVWp)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FeToFvcVWp(MAh,MFe,MFv,MFxe,MFxv,Mhh,MHp,             & 
& MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,               & 
& MVZ2,cplcFeFeAhL,cplcFeFeAhR,cplAhHpcVWp,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,          & 
& cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,           & 
& cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,      & 
& cplcFeFxecSscR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcVWp,cplhhcVWpVWp,               & 
& cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,AmpVertexFeToFvcVWp)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFvcVWp(MAh,MFe,MFv,MFxe,MFxv,Mhh,              & 
& MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,           & 
& MVZ2,cplcFeFeAhL,cplcFeFeAhR,cplAhHpcVWp,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,          & 
& cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,           & 
& cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,      & 
& cplcFeFxecSscR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcVWp,cplhhcVWpVWp,               & 
& cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,AmpVertexIRdrFeToFvcVWp)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFvcVWp(MAhOS,MFeOS,MFvOS,MFxeOS,               & 
& MFxvOS,MhhOS,MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,               & 
& MFxv2OS,Mhh2OS,MHp2OS,MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplcFeFeAhL,cplcFeFeAhR,             & 
& cplAhHpcVWp,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,               & 
& cplcFeFeVZR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,GosZcplcFeFvcHpL,      & 
& GosZcplcFeFvcHpR,ZcplcFeFvcVWpL,ZcplcFeFvcVWpR,cplcFeFxecSscL,cplcFeFxecSscR,          & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcVWp,cplhhcVWpVWp,GosZcplHpcVWpVP,              & 
& cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,AmpVertexIRosFeToFvcVWp)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFvcVWp(MAh,MFe,MFv,MFxe,MFxv,Mhh,              & 
& MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,           & 
& MVZ2,cplcFeFeAhL,cplcFeFeAhR,cplAhHpcVWp,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,          & 
& cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,           & 
& cplcFvFvVZR,GZcplcFeFvcHpL,GZcplcFeFvcHpR,ZcplcFeFvcVWpL,ZcplcFeFvcVWpR,               & 
& cplcFeFxecSscL,cplcFeFxecSscR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcVWp,             & 
& cplhhcVWpVWp,GZcplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,AmpVertexIRosFeToFvcVWp)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFvcVWp(MAhOS,MFeOS,MFvOS,MFxeOS,               & 
& MFxvOS,MhhOS,MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,               & 
& MFxv2OS,Mhh2OS,MHp2OS,MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplcFeFeAhL,cplcFeFeAhR,             & 
& cplAhHpcVWp,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,               & 
& cplcFeFeVZR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,GcplcFeFvcHpL,         & 
& GcplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,cplcFeFxecSscR,               & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcVWp,cplhhcVWpVWp,GcplHpcVWpVP,cplHpcVWpVZ,     & 
& cplcVWpVPVWp,cplcVWpVWpVZ,AmpVertexIRosFeToFvcVWp)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFvcVWp(MAh,MFe,MFv,MFxe,MFxv,Mhh,              & 
& MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,           & 
& MVZ2,cplcFeFeAhL,cplcFeFeAhR,cplAhHpcVWp,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,          & 
& cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,           & 
& cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,      & 
& cplcFeFxecSscR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcVWp,cplhhcVWpVWp,               & 
& cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,AmpVertexIRosFeToFvcVWp)

 End if 
 End if 
AmpVertexFeToFvcVWp = AmpVertexFeToFvcVWp -  AmpVertexIRdrFeToFvcVWp! +  AmpVertexIRosFeToFvcVWp ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZFeToFvcVWp=0._dp 
AmpVertexZFeToFvcVWp=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFeToFvcVWp(1,gt2,:) = AmpWaveZFeToFvcVWp(1,gt2,:)+ZRUVec(gt2,gt1)*AmpWaveFeToFvcVWp(1,gt1,:) 
AmpVertexZFeToFvcVWp(1,gt2,:)= AmpVertexZFeToFvcVWp(1,gt2,:) + ZRUVec(gt2,gt1)*AmpVertexFeToFvcVWp(1,gt1,:) 
AmpWaveZFeToFvcVWp(2,gt2,:) = AmpWaveZFeToFvcVWp(2,gt2,:)+ZRUUe(gt2,gt1)*AmpWaveFeToFvcVWp(2,gt1,:) 
AmpVertexZFeToFvcVWp(2,gt2,:)= AmpVertexZFeToFvcVWp(2,gt2,:) + ZRUUe(gt2,gt1)*AmpVertexFeToFvcVWp(2,gt1,:) 
AmpWaveZFeToFvcVWp(3,gt2,:) = AmpWaveZFeToFvcVWp(3,gt2,:)+ZRUVec(gt2,gt1)*AmpWaveFeToFvcVWp(3,gt1,:) 
AmpVertexZFeToFvcVWp(3,gt2,:)= AmpVertexZFeToFvcVWp(3,gt2,:) + ZRUVec(gt2,gt1)*AmpVertexFeToFvcVWp(3,gt1,:) 
AmpWaveZFeToFvcVWp(4,gt2,:) = AmpWaveZFeToFvcVWp(4,gt2,:)+ZRUUe(gt2,gt1)*AmpWaveFeToFvcVWp(4,gt1,:) 
AmpVertexZFeToFvcVWp(4,gt2,:)= AmpVertexZFeToFvcVWp(4,gt2,:) + ZRUUe(gt2,gt1)*AmpVertexFeToFvcVWp(4,gt1,:) 
 End Do 
End Do 
AmpWaveFeToFvcVWp=AmpWaveZFeToFvcVWp 
AmpVertexFeToFvcVWp= AmpVertexZFeToFvcVWp
! Final State 1 
AmpWaveZFeToFvcVWp=0._dp 
AmpVertexZFeToFvcVWp=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFeToFvcVWp(1,:,gt2) = AmpWaveZFeToFvcVWp(1,:,gt2)+ZRUVv(gt2,gt1)*AmpWaveFeToFvcVWp(1,:,gt1) 
AmpVertexZFeToFvcVWp(1,:,gt2)= AmpVertexZFeToFvcVWp(1,:,gt2)+ZRUVv(gt2,gt1)*AmpVertexFeToFvcVWp(1,:,gt1) 
AmpWaveZFeToFvcVWp(2,:,gt2) = AmpWaveZFeToFvcVWp(2,:,gt2)+ZRUVvrc(gt2,gt1)*AmpWaveFeToFvcVWp(2,:,gt1) 
AmpVertexZFeToFvcVWp(2,:,gt2)= AmpVertexZFeToFvcVWp(2,:,gt2)+ZRUVvrc(gt2,gt1)*AmpVertexFeToFvcVWp(2,:,gt1) 
AmpWaveZFeToFvcVWp(3,:,gt2) = AmpWaveZFeToFvcVWp(3,:,gt2)+ZRUVv(gt2,gt1)*AmpWaveFeToFvcVWp(3,:,gt1) 
AmpVertexZFeToFvcVWp(3,:,gt2)= AmpVertexZFeToFvcVWp(3,:,gt2)+ZRUVv(gt2,gt1)*AmpVertexFeToFvcVWp(3,:,gt1) 
AmpWaveZFeToFvcVWp(4,:,gt2) = AmpWaveZFeToFvcVWp(4,:,gt2)+ZRUVvrc(gt2,gt1)*AmpWaveFeToFvcVWp(4,:,gt1) 
AmpVertexZFeToFvcVWp(4,:,gt2)= AmpVertexZFeToFvcVWp(4,:,gt2)+ZRUVvrc(gt2,gt1)*AmpVertexFeToFvcVWp(4,:,gt1) 
 End Do 
End Do 
AmpWaveFeToFvcVWp=AmpWaveZFeToFvcVWp 
AmpVertexFeToFvcVWp= AmpVertexZFeToFvcVWp
End if
If (ShiftIRdiv) Then 
AmpVertexFeToFvcVWp = AmpVertexFeToFvcVWp  +  AmpVertexIRosFeToFvcVWp
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fe->Fv conj[VWp] -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumFeToFvcVWp = AmpTreeFeToFvcVWp 
 AmpSum2FeToFvcVWp = AmpTreeFeToFvcVWp + 2._dp*AmpWaveFeToFvcVWp + 2._dp*AmpVertexFeToFvcVWp  
Else 
 AmpSumFeToFvcVWp = AmpTreeFeToFvcVWp + AmpWaveFeToFvcVWp + AmpVertexFeToFvcVWp
 AmpSum2FeToFvcVWp = AmpTreeFeToFvcVWp + AmpWaveFeToFvcVWp + AmpVertexFeToFvcVWp 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumFeToFvcVWp = AmpTreeFeToFvcVWp
 AmpSum2FeToFvcVWp = AmpTreeFeToFvcVWp 
End if 
Do gt1=1,3
i4 = isave 
  Do gt2=1,3
If (((OSkinematics).and.(MFeOS(gt1).gt.(MFvOS(gt2)+MVWpOS))).or.((.not.OSkinematics).and.(MFe(gt1).gt.(MFv(gt2)+MVWp)))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2 
  AmpSum2FeToFvcVWp = AmpTreeFeToFvcVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFeOS(gt1),MFvOS(gt2),MVWpOS,AmpSumFeToFvcVWp(:,gt1, gt2),AmpSum2FeToFvcVWp(:,gt1, gt2),AmpSqFeToFvcVWp(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFe(gt1),MFv(gt2),MVWp,AmpSumFeToFvcVWp(:,gt1, gt2),AmpSum2FeToFvcVWp(:,gt1, gt2),AmpSqFeToFvcVWp(gt1, gt2)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqFeToFvcVWp(gt1, gt2) 
  AmpSum2FeToFvcVWp = 2._dp*AmpWaveFeToFvcVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFeOS(gt1),MFvOS(gt2),MVWpOS,AmpSumFeToFvcVWp(:,gt1, gt2),AmpSum2FeToFvcVWp(:,gt1, gt2),AmpSqFeToFvcVWp(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFe(gt1),MFv(gt2),MVWp,AmpSumFeToFvcVWp(:,gt1, gt2),AmpSum2FeToFvcVWp(:,gt1, gt2),AmpSqFeToFvcVWp(gt1, gt2)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqFeToFvcVWp(gt1, gt2) 
  AmpSum2FeToFvcVWp = 2._dp*AmpVertexFeToFvcVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFeOS(gt1),MFvOS(gt2),MVWpOS,AmpSumFeToFvcVWp(:,gt1, gt2),AmpSum2FeToFvcVWp(:,gt1, gt2),AmpSqFeToFvcVWp(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFe(gt1),MFv(gt2),MVWp,AmpSumFeToFvcVWp(:,gt1, gt2),AmpSum2FeToFvcVWp(:,gt1, gt2),AmpSqFeToFvcVWp(gt1, gt2)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqFeToFvcVWp(gt1, gt2) 
  AmpSum2FeToFvcVWp = AmpTreeFeToFvcVWp + 2._dp*AmpWaveFeToFvcVWp + 2._dp*AmpVertexFeToFvcVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFeOS(gt1),MFvOS(gt2),MVWpOS,AmpSumFeToFvcVWp(:,gt1, gt2),AmpSum2FeToFvcVWp(:,gt1, gt2),AmpSqFeToFvcVWp(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFe(gt1),MFv(gt2),MVWp,AmpSumFeToFvcVWp(:,gt1, gt2),AmpSum2FeToFvcVWp(:,gt1, gt2),AmpSqFeToFvcVWp(gt1, gt2)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqFeToFvcVWp(gt1, gt2) 
 End if 
If (OSkinematics) Then 
  AmpSum2FeToFvcVWp = AmpTreeFeToFvcVWp
  Call SquareAmp_FtoFV(MFeOS(gt1),MFvOS(gt2),MVWpOS,AmpSumFeToFvcVWp(:,gt1, gt2),AmpSum2FeToFvcVWp(:,gt1, gt2),AmpSqFeToFvcVWp(gt1, gt2)) 
  AmpSqTreeFeToFvcVWp(gt1, gt2) = AmpSqFeToFvcVWp(gt1, gt2)  
  AmpSum2FeToFvcVWp = + 2._dp*AmpWaveFeToFvcVWp + 2._dp*AmpVertexFeToFvcVWp
  Call SquareAmp_FtoFV(MFeOS(gt1),MFvOS(gt2),MVWpOS,AmpSumFeToFvcVWp(:,gt1, gt2),AmpSum2FeToFvcVWp(:,gt1, gt2),AmpSqFeToFvcVWp(gt1, gt2)) 
  AmpSqFeToFvcVWp(gt1, gt2) = AmpSqFeToFvcVWp(gt1, gt2) + AmpSqTreeFeToFvcVWp(gt1, gt2)  
Else  
  AmpSum2FeToFvcVWp = AmpTreeFeToFvcVWp
  Call SquareAmp_FtoFV(MFe(gt1),MFv(gt2),MVWp,AmpSumFeToFvcVWp(:,gt1, gt2),AmpSum2FeToFvcVWp(:,gt1, gt2),AmpSqFeToFvcVWp(gt1, gt2)) 
  AmpSqTreeFeToFvcVWp(gt1, gt2) = AmpSqFeToFvcVWp(gt1, gt2)  
  AmpSum2FeToFvcVWp = + 2._dp*AmpWaveFeToFvcVWp + 2._dp*AmpVertexFeToFvcVWp
  Call SquareAmp_FtoFV(MFe(gt1),MFv(gt2),MVWp,AmpSumFeToFvcVWp(:,gt1, gt2),AmpSum2FeToFvcVWp(:,gt1, gt2),AmpSqFeToFvcVWp(gt1, gt2)) 
  AmpSqFeToFvcVWp(gt1, gt2) = AmpSqFeToFvcVWp(gt1, gt2) + AmpSqTreeFeToFvcVWp(gt1, gt2)  
End if  
Else  
  AmpSqFeToFvcVWp(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFeToFvcVWp(gt1, gt2).le.0._dp) Then 
  gP1LFe(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFe(gt1,i4) = 1._dp*GammaTPS(MFeOS(gt1),MFvOS(gt2),MVWpOS,helfactor*AmpSqFeToFvcVWp(gt1, gt2))
Else 
  gP1LFe(gt1,i4) = 1._dp*GammaTPS(MFe(gt1),MFv(gt2),MVWp,helfactor*AmpSqFeToFvcVWp(gt1, gt2))
End if 
If ((Abs(MRPFeToFvcVWp(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFeToFvcVWp(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFe(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPFeToFvcVWp(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFeToFvcVWp(gt1, gt2)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPFeToFvcVWp(gt1, gt2) + MRGFeToFvcVWp(gt1, gt2)) 
  gP1LFe(gt1,i4) = gP1LFe(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPFeToFvcVWp(gt1, gt2) + MRGFeToFvcVWp(gt1, gt2))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LFe(gt1,i4) 
  End if 
End if 
End if 
i4=i4+1

  End do
If (gt1.eq.3) isave = i4 
End do
End If 
If (.not.CalcLoopDecay_LoopInducedOnly) Then 
!---------------- 
! Fxe Conjg(Ssc)
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_FeToFxecSsc(cplcFeFxecSscL,cplcFeFxecSscR,              & 
& MFe,MFxe,MSsc,MFe2,MFxe2,MSsc2,AmpTreeFeToFxecSsc)

  Else 
Call Amplitude_Tree_SDdiracDM_FeToFxecSsc(ZcplcFeFxecSscL,ZcplcFeFxecSscR,            & 
& MFe,MFxe,MSsc,MFe2,MFxe2,MSsc2,AmpTreeFeToFxecSsc)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_FeToFxecSsc(MLambda,em,gs,cplcFeFxecSscL,cplcFeFxecSscR,    & 
& MFeOS,MFxeOS,MSscOS,MRPFeToFxecSsc,MRGFeToFxecSsc)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_FeToFxecSsc(MLambda,em,gs,ZcplcFeFxecSscL,ZcplcFeFxecSscR,  & 
& MFeOS,MFxeOS,MSscOS,MRPFeToFxecSsc,MRGFeToFxecSsc)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_FeToFxecSsc(MLambda,em,gs,cplcFeFxecSscL,cplcFeFxecSscR,    & 
& MFe,MFxe,MSsc,MRPFeToFxecSsc,MRGFeToFxecSsc)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_FeToFxecSsc(MLambda,em,gs,ZcplcFeFxecSscL,ZcplcFeFxecSscR,  & 
& MFe,MFxe,MSsc,MRPFeToFxecSsc,MRGFeToFxecSsc)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FeToFxecSsc(cplcFeFxecSscL,cplcFeFxecSscR,              & 
& ctcplcFeFxecSscL,ctcplcFeFxecSscR,MFe,MFe2,MFxe,MFxe2,MSsc,MSsc2,Zfed,ZfEL,            & 
& ZfER,Zfeu,ZfSsc,AmpWaveFeToFxecSsc)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FeToFxecSsc(MFe,MFv,MFxe,MFxv,Mhh,MHp,MSsc,           & 
& MVP,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFeFehhL,        & 
& cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,              & 
& cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,    & 
& cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,               & 
& cplcFeFxecSscL,cplcFeFxecSscR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplhhSsccSsc,              & 
& AmpVertexFeToFxecSsc)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFxecSsc(MFe,MFv,MFxe,MFxv,Mhh,MHp,             & 
& MSsc,MVP,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,               & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,               & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,cplcFxvFxeHpR,     & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,               & 
& cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFvFxvcSscL,cplcFvFxvcSscR,             & 
& cplhhSsccSsc,AmpVertexIRdrFeToFxecSsc)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFxecSsc(MFeOS,MFvOS,MFxeOS,MFxvOS,             & 
& MhhOS,MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MFe2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,              & 
& MHp2OS,MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,    & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,         & 
& cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR, & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,ZcplcFeFxecSscL,ZcplcFeFxecSscR,cplcFvFxvcSscL,            & 
& cplcFvFxvcSscR,cplhhSsccSsc,AmpVertexIRosFeToFxecSsc)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFxecSsc(MFe,MFv,MFxe,MFxv,Mhh,MHp,             & 
& MSsc,MVP,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,               & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,               & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,cplcFxvFxeHpR,     & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,               & 
& cplcFxeFxeVZR,ZcplcFeFxecSscL,ZcplcFeFxecSscR,cplcFvFxvcSscL,cplcFvFxvcSscR,           & 
& cplhhSsccSsc,AmpVertexIRosFeToFxecSsc)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFxecSsc(MFeOS,MFvOS,MFxeOS,MFxvOS,             & 
& MhhOS,MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MFe2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,              & 
& MHp2OS,MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,    & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,         & 
& cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR, & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFvFxvcSscL,              & 
& cplcFvFxvcSscR,cplhhSsccSsc,AmpVertexIRosFeToFxecSsc)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FeToFxecSsc(MFe,MFv,MFxe,MFxv,Mhh,MHp,             & 
& MSsc,MVP,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,               & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,               & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,cplcFxvFxeHpR,     & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,               & 
& cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFvFxvcSscL,cplcFvFxvcSscR,             & 
& cplhhSsccSsc,AmpVertexIRosFeToFxecSsc)

 End if 
 End if 
AmpVertexFeToFxecSsc = AmpVertexFeToFxecSsc -  AmpVertexIRdrFeToFxecSsc! +  AmpVertexIRosFeToFxecSsc ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZFeToFxecSsc=0._dp 
AmpVertexZFeToFxecSsc=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFeToFxecSsc(1,gt2,:) = AmpWaveZFeToFxecSsc(1,gt2,:)+ZRUUe(gt2,gt1)*AmpWaveFeToFxecSsc(1,gt1,:) 
AmpVertexZFeToFxecSsc(1,gt2,:)= AmpVertexZFeToFxecSsc(1,gt2,:) + ZRUUe(gt2,gt1)*AmpVertexFeToFxecSsc(1,gt1,:) 
AmpWaveZFeToFxecSsc(2,gt2,:) = AmpWaveZFeToFxecSsc(2,gt2,:)+ZRUVec(gt2,gt1)*AmpWaveFeToFxecSsc(2,gt1,:) 
AmpVertexZFeToFxecSsc(2,gt2,:)= AmpVertexZFeToFxecSsc(2,gt2,:) + ZRUVec(gt2,gt1)*AmpVertexFeToFxecSsc(2,gt1,:) 
 End Do 
End Do 
AmpWaveFeToFxecSsc=AmpWaveZFeToFxecSsc 
AmpVertexFeToFxecSsc= AmpVertexZFeToFxecSsc
! Final State 2 
AmpWaveZFeToFxecSsc=0._dp 
AmpVertexZFeToFxecSsc=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZFeToFxecSsc(:,:,gt2) = AmpWaveZFeToFxecSsc(:,:,gt2)+ZRUVSs(gt2,gt1)*AmpWaveFeToFxecSsc(:,:,gt1) 
AmpVertexZFeToFxecSsc(:,:,gt2)= AmpVertexZFeToFxecSsc(:,:,gt2)+ZRUVSs(gt2,gt1)*AmpVertexFeToFxecSsc(:,:,gt1) 
 End Do 
End Do 
AmpWaveFeToFxecSsc=AmpWaveZFeToFxecSsc 
AmpVertexFeToFxecSsc= AmpVertexZFeToFxecSsc
End if
If (ShiftIRdiv) Then 
AmpVertexFeToFxecSsc = AmpVertexFeToFxecSsc  +  AmpVertexIRosFeToFxecSsc
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fe->Fxe conj[Ssc] -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumFeToFxecSsc = AmpTreeFeToFxecSsc 
 AmpSum2FeToFxecSsc = AmpTreeFeToFxecSsc + 2._dp*AmpWaveFeToFxecSsc + 2._dp*AmpVertexFeToFxecSsc  
Else 
 AmpSumFeToFxecSsc = AmpTreeFeToFxecSsc + AmpWaveFeToFxecSsc + AmpVertexFeToFxecSsc
 AmpSum2FeToFxecSsc = AmpTreeFeToFxecSsc + AmpWaveFeToFxecSsc + AmpVertexFeToFxecSsc 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumFeToFxecSsc = AmpTreeFeToFxecSsc
 AmpSum2FeToFxecSsc = AmpTreeFeToFxecSsc 
End if 
Do gt1=1,3
i4 = isave 
    Do gt3=1,2
If (((OSkinematics).and.(MFeOS(gt1).gt.(MFxeOS+MSscOS(gt3)))).or.((.not.OSkinematics).and.(MFe(gt1).gt.(MFxe+MSsc(gt3))))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt3 
  AmpSum2FeToFxecSsc = AmpTreeFeToFxecSsc
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFeOS(gt1),MFxeOS,MSscOS(gt3),AmpSumFeToFxecSsc(:,gt1, gt3),AmpSum2FeToFxecSsc(:,gt1, gt3),AmpSqFeToFxecSsc(gt1, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFe(gt1),MFxe,MSsc(gt3),AmpSumFeToFxecSsc(:,gt1, gt3),AmpSum2FeToFxecSsc(:,gt1, gt3),AmpSqFeToFxecSsc(gt1, gt3)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqFeToFxecSsc(gt1, gt3) 
  AmpSum2FeToFxecSsc = 2._dp*AmpWaveFeToFxecSsc
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFeOS(gt1),MFxeOS,MSscOS(gt3),AmpSumFeToFxecSsc(:,gt1, gt3),AmpSum2FeToFxecSsc(:,gt1, gt3),AmpSqFeToFxecSsc(gt1, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFe(gt1),MFxe,MSsc(gt3),AmpSumFeToFxecSsc(:,gt1, gt3),AmpSum2FeToFxecSsc(:,gt1, gt3),AmpSqFeToFxecSsc(gt1, gt3)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqFeToFxecSsc(gt1, gt3) 
  AmpSum2FeToFxecSsc = 2._dp*AmpVertexFeToFxecSsc
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFeOS(gt1),MFxeOS,MSscOS(gt3),AmpSumFeToFxecSsc(:,gt1, gt3),AmpSum2FeToFxecSsc(:,gt1, gt3),AmpSqFeToFxecSsc(gt1, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFe(gt1),MFxe,MSsc(gt3),AmpSumFeToFxecSsc(:,gt1, gt3),AmpSum2FeToFxecSsc(:,gt1, gt3),AmpSqFeToFxecSsc(gt1, gt3)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqFeToFxecSsc(gt1, gt3) 
  AmpSum2FeToFxecSsc = AmpTreeFeToFxecSsc + 2._dp*AmpWaveFeToFxecSsc + 2._dp*AmpVertexFeToFxecSsc
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFeOS(gt1),MFxeOS,MSscOS(gt3),AmpSumFeToFxecSsc(:,gt1, gt3),AmpSum2FeToFxecSsc(:,gt1, gt3),AmpSqFeToFxecSsc(gt1, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFe(gt1),MFxe,MSsc(gt3),AmpSumFeToFxecSsc(:,gt1, gt3),AmpSum2FeToFxecSsc(:,gt1, gt3),AmpSqFeToFxecSsc(gt1, gt3)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqFeToFxecSsc(gt1, gt3) 
 End if 
If (OSkinematics) Then 
  AmpSum2FeToFxecSsc = AmpTreeFeToFxecSsc
  Call SquareAmp_FtoFS(MFeOS(gt1),MFxeOS,MSscOS(gt3),AmpSumFeToFxecSsc(:,gt1, gt3),AmpSum2FeToFxecSsc(:,gt1, gt3),AmpSqFeToFxecSsc(gt1, gt3)) 
  AmpSqTreeFeToFxecSsc(gt1, gt3) = AmpSqFeToFxecSsc(gt1, gt3)  
  AmpSum2FeToFxecSsc = + 2._dp*AmpWaveFeToFxecSsc + 2._dp*AmpVertexFeToFxecSsc
  Call SquareAmp_FtoFS(MFeOS(gt1),MFxeOS,MSscOS(gt3),AmpSumFeToFxecSsc(:,gt1, gt3),AmpSum2FeToFxecSsc(:,gt1, gt3),AmpSqFeToFxecSsc(gt1, gt3)) 
  AmpSqFeToFxecSsc(gt1, gt3) = AmpSqFeToFxecSsc(gt1, gt3) + AmpSqTreeFeToFxecSsc(gt1, gt3)  
Else  
  AmpSum2FeToFxecSsc = AmpTreeFeToFxecSsc
  Call SquareAmp_FtoFS(MFe(gt1),MFxe,MSsc(gt3),AmpSumFeToFxecSsc(:,gt1, gt3),AmpSum2FeToFxecSsc(:,gt1, gt3),AmpSqFeToFxecSsc(gt1, gt3)) 
  AmpSqTreeFeToFxecSsc(gt1, gt3) = AmpSqFeToFxecSsc(gt1, gt3)  
  AmpSum2FeToFxecSsc = + 2._dp*AmpWaveFeToFxecSsc + 2._dp*AmpVertexFeToFxecSsc
  Call SquareAmp_FtoFS(MFe(gt1),MFxe,MSsc(gt3),AmpSumFeToFxecSsc(:,gt1, gt3),AmpSum2FeToFxecSsc(:,gt1, gt3),AmpSqFeToFxecSsc(gt1, gt3)) 
  AmpSqFeToFxecSsc(gt1, gt3) = AmpSqFeToFxecSsc(gt1, gt3) + AmpSqTreeFeToFxecSsc(gt1, gt3)  
End if  
Else  
  AmpSqFeToFxecSsc(gt1, gt3) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFeToFxecSsc(gt1, gt3).le.0._dp) Then 
  gP1LFe(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFe(gt1,i4) = 1._dp*GammaTPS(MFeOS(gt1),MFxeOS,MSscOS(gt3),helfactor*AmpSqFeToFxecSsc(gt1, gt3))
Else 
  gP1LFe(gt1,i4) = 1._dp*GammaTPS(MFe(gt1),MFxe,MSsc(gt3),helfactor*AmpSqFeToFxecSsc(gt1, gt3))
End if 
If ((Abs(MRPFeToFxecSsc(gt1, gt3)).gt.1.0E-20_dp).or.(Abs(MRGFeToFxecSsc(gt1, gt3)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFe(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPFeToFxecSsc(gt1, gt3)).gt.1.0E-20_dp).or.(Abs(MRGFeToFxecSsc(gt1, gt3)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPFeToFxecSsc(gt1, gt3) + MRGFeToFxecSsc(gt1, gt3)) 
  gP1LFe(gt1,i4) = gP1LFe(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPFeToFxecSsc(gt1, gt3) + MRGFeToFxecSsc(gt1, gt3))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LFe(gt1,i4) 
  End if 
End if 
End if 
i4=i4+1

    End do
If (gt1.eq.3) isave = i4 
End do
End If 
!---------------- 
! Fe VP
!---------------- 

If (LoopInducedDecaysOS) Then 


!Self-energy Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_WAVE_SDdiracDM_FeToFeVP(ZcplcFeFeVPL,ZcplcFeFeVPR,ZcplcFeFeVZL,        & 
& ZcplcFeFeVZR,ctcplcFeFeVPL,ctcplcFeFeVPR,ctcplcFeFeVZL,ctcplcFeFeVZR,MFeOS,            & 
& MFe2OS,MVP,MVP2,ZfEL,ZfER,ZfVP,ZfVZVP,AmpWaveFeToFeVP)

 Else 
Call Amplitude_WAVE_SDdiracDM_FeToFeVP(cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,           & 
& cplcFeFeVZR,ctcplcFeFeVPL,ctcplcFeFeVPR,ctcplcFeFeVZL,ctcplcFeFeVZR,MFeOS,             & 
& MFe2OS,MVP,MVP2,ZfEL,ZfER,ZfVP,ZfVZVP,AmpWaveFeToFeVP)

 End if 


!Vertex Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_VERTEX_SDdiracDM_FeToFeVP(MAhOS,MFeOS,MFvOS,MFxeOS,MhhOS,              & 
& MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,Mhh2OS,MHp2OS,              & 
& MSsc2OS,MVP2,MVWp2OS,MVZ2OS,ZcplcFeFeAhL,ZcplcFeFeAhR,ZcplcFeFehhL,ZcplcFeFehhR,       & 
& ZcplcFvFeHpL,ZcplcFvFeHpR,ZcplcFxeFeSscL,ZcplcFxeFeSscR,ZcplcFeFeVPL,ZcplcFeFeVPR,     & 
& ZcplcFvFeVWpL,ZcplcFvFeVWpR,ZcplcFeFeVZL,ZcplcFeFeVZR,ZcplcFeFvcHpL,ZcplcFeFvcHpR,     & 
& ZcplcFeFvcVWpL,ZcplcFeFvcVWpR,ZcplcFxeFxeVPL,ZcplcFxeFxeVPR,ZcplcFeFxecSscL,           & 
& ZcplcFeFxecSscR,ZcplHpcHpVP,ZcplHpcVWpVP,ZcplcHpVPVWp,ZcplcVWpVPVWp,AmpVertexFeToFeVP)

 Else 
Call Amplitude_VERTEX_SDdiracDM_FeToFeVP(MAhOS,MFeOS,MFvOS,MFxeOS,MhhOS,              & 
& MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,Mhh2OS,MHp2OS,              & 
& MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplcFeFeAhL,cplcFeFeAhR,cplcFeFehhL,cplcFeFehhR,           & 
& cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,           & 
& cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,           & 
& cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFeFxecSscL,cplcFeFxecSscR, & 
& cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,AmpVertexFeToFeVP)

 End if 
Else 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FeToFeVP(cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,           & 
& cplcFeFeVZR,ctcplcFeFeVPL,ctcplcFeFeVPR,ctcplcFeFeVZL,ctcplcFeFeVZR,MFe,               & 
& MFe2,MVP,MVP2,ZfEL,ZfER,ZfVP,ZfVZVP,AmpWaveFeToFeVP)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FeToFeVP(MAh,MFe,MFv,MFxe,Mhh,MHp,MSsc,               & 
& MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFeFeAhL,         & 
& cplcFeFeAhR,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,             & 
& cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,           & 
& cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxeVPL,       & 
& cplcFxeFxeVPR,cplcFeFxecSscL,cplcFeFxecSscR,cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,        & 
& cplcVWpVPVWp,AmpVertexFeToFeVP)

End if 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fe->Fe VP -----------------------" 
End if 
If (OnlyTreeLevelContributions) Then 
 AmpSumFeToFeVP = 0._dp 
 AmpSum2FeToFeVP = 0._dp  
Else 
 AmpSumFeToFeVP = AmpVertexFeToFeVP + AmpWaveFeToFeVP
 AmpSum2FeToFeVP = AmpVertexFeToFeVP + AmpWaveFeToFeVP 
End If 
Do gt1=1,3
i4 = isave 
  Do gt2=1,3
If (((OSkinematics).and.(MFeOS(gt1).gt.(MFeOS(gt2)+0.))).or.((.not.OSkinematics).and.(MFe(gt1).gt.(MFe(gt2)+MVP)))) Then 
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFeOS(gt1),MFeOS(gt2),0._dp,AmpSumFeToFeVP(:,gt1, gt2),AmpSum2FeToFeVP(:,gt1, gt2),AmpSqFeToFeVP(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFe(gt1),MFe(gt2),MVP,AmpSumFeToFeVP(:,gt1, gt2),AmpSum2FeToFeVP(:,gt1, gt2),AmpSqFeToFeVP(gt1, gt2)) 
End if  
Else  
  AmpSqFeToFeVP(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFeToFeVP(gt1, gt2).le.0._dp) Then 
  gP1LFe(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFe(gt1,i4) = 1._dp*GammaTPS(MFeOS(gt1),MFeOS(gt2),0._dp,helfactor*AmpSqFeToFeVP(gt1, gt2))
Else 
  gP1LFe(gt1,i4) = 1._dp*GammaTPS(MFe(gt1),MFe(gt2),MVP,helfactor*AmpSqFeToFeVP(gt1, gt2))
End if 
If ((Abs(MRPFeToFeVP(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFeToFeVP(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFe(gt1,i4) 
End if 
i4=i4+1

  End do
If (gt1.eq.3) isave = i4 
End do
End Subroutine OneLoopDecay_Fe

End Module Wrapper_OneLoopDecay_Fe_SDdiracDM
