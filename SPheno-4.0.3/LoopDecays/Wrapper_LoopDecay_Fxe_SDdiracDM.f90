! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:22 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module Wrapper_OneLoopDecay_Fxe_SDdiracDM
Use Model_Data_SDdiracDM 
Use Kinematics 
Use OneLoopDecay_Fxe_SDdiracDM 
Use Control 
Use Settings 

 
Contains

 
Subroutine OneLoopDecay_Fxe(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,MFeOS,             & 
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
& ZfVL,ZfVR,ZfxVL,ZfxVR,ZfSsc,ZfVPVZ,ZfVZVP,cplAhcHpVWp,cplAhHpcVWp,cplcFeFeAhL,         & 
& cplcFeFeAhR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,               & 
& cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,      & 
& cplcFeFxecSscR,cplcFvFeHpL,cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFvFxvcSscL,       & 
& cplcFvFxvcSscR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,  & 
& cplcFxeFxeVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,           & 
& cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR, & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,   & 
& cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,            & 
& cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhVZVZ,cplHpcHpVP,cplHpcHpVZ,cplHpcVWpVP,       & 
& cplHpcVWpVZ,ctcplcFxeFeSscL,ctcplcFxeFeSscR,ctcplcFxeFxeVPL,ctcplcFxeFxeVPR,           & 
& ctcplcFxeFxeVZL,ctcplcFxeFxeVZR,ctcplcFxeFxvcHpL,ctcplcFxeFxvcHpR,ctcplcFxeFxvcVWpL,   & 
& ctcplcFxeFxvcVWpR,GcplcFxeFxvcHpL,GcplcFxeFxvcHpR,GcplcHpVPVWp,GcplHpcVWpVP,           & 
& GosZcplcFxeFxvcHpL,GosZcplcFxeFxvcHpR,GosZcplcHpVPVWp,GosZcplHpcVWpVP,GZcplcFxeFxvcHpL,& 
& GZcplcFxeFxvcHpR,GZcplcHpVPVWp,GZcplHpcVWpVP,ZcplAhcHpVWp,ZcplAhHpcVWp,ZcplcFeFeAhL,   & 
& ZcplcFeFeAhR,ZcplcFeFehhL,ZcplcFeFehhR,ZcplcFeFeVPL,ZcplcFeFeVPR,ZcplcFeFxecSscL,      & 
& ZcplcFeFxecSscR,ZcplcFxeFeSscL,ZcplcFxeFeSscR,ZcplcFxeFxeVPL,ZcplcFxeFxeVPR,           & 
& ZcplcFxeFxeVZL,ZcplcFxeFxeVZR,ZcplcFxeFxvcHpL,ZcplcFxeFxvcHpR,ZcplcFxeFxvcVWpL,        & 
& ZcplcFxeFxvcVWpR,ZcplcFxvFxeHpL,ZcplcFxvFxeHpR,ZcplcFxvFxeVWpL,ZcplcFxvFxeVWpR,        & 
& ZcplcFxvFxvAhL,ZcplcFxvFxvAhR,ZcplcFxvFxvhhL,ZcplcFxvFxvhhR,ZcplcHpVPVWp,              & 
& ZcplcVWpVPVWp,ZcplhhcHpVWp,ZcplhhcVWpVWp,ZcplhhHpcHp,ZcplhhHpcVWp,ZcplhhSsccSsc,       & 
& ZcplhhVZVZ,ZcplHpcHpVP,ZcplHpcVWpVP,ZRUZH,ZRUVd,ZRUUd,ZRUVu,ZRUUu,ZRUVe,               & 
& ZRUUe,ZRUVv,ZRUVvr,ZRUXV,ZRUXU,ZRUVSs,MLambda,em,gs,deltaM,kont,gP1LFxe)

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

Complex(dp),Intent(in) :: cplAhcHpVWp,cplAhHpcVWp,cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFeFehhL(3,3,2),         & 
& cplcFeFehhR(3,3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),& 
& cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),             & 
& cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),             & 
& cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),       & 
& cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,       & 
& cplcFxeFxeVZR,cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),& 
& cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),           & 
& cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),             & 
& cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),       & 
& cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp(2),cplhhcVWpVWp(2),      & 
& cplhhHpcHp(2),cplhhHpcVWp(2),cplhhSsccSsc(2,2,2),cplhhVZVZ(2),cplHpcHpVP,              & 
& cplHpcHpVZ,cplHpcVWpVP,cplHpcVWpVZ,ctcplcFxeFeSscL(3,2),ctcplcFxeFeSscR(3,2),          & 
& ctcplcFxeFxeVPL,ctcplcFxeFxeVPR,ctcplcFxeFxeVZL,ctcplcFxeFxeVZR,ctcplcFxeFxvcHpL(2),   & 
& ctcplcFxeFxvcHpR(2),ctcplcFxeFxvcVWpL(2),ctcplcFxeFxvcVWpR(2),GcplcFxeFxvcHpL(2),      & 
& GcplcFxeFxvcHpR(2),GcplcHpVPVWp,GcplHpcVWpVP,GosZcplcFxeFxvcHpL(2),GosZcplcFxeFxvcHpR(2),& 
& GosZcplcHpVPVWp,GosZcplHpcVWpVP,GZcplcFxeFxvcHpL(2),GZcplcFxeFxvcHpR(2),               & 
& GZcplcHpVPVWp,GZcplHpcVWpVP,ZcplAhcHpVWp,ZcplAhHpcVWp,ZcplcFeFeAhL(3,3),               & 
& ZcplcFeFeAhR(3,3),ZcplcFeFehhL(3,3,2),ZcplcFeFehhR(3,3,2),ZcplcFeFeVPL(3,3),           & 
& ZcplcFeFeVPR(3,3),ZcplcFeFxecSscL(3,2),ZcplcFeFxecSscR(3,2),ZcplcFxeFeSscL(3,2),       & 
& ZcplcFxeFeSscR(3,2),ZcplcFxeFxeVPL,ZcplcFxeFxeVPR,ZcplcFxeFxeVZL,ZcplcFxeFxeVZR,       & 
& ZcplcFxeFxvcHpL(2),ZcplcFxeFxvcHpR(2),ZcplcFxeFxvcVWpL(2),ZcplcFxeFxvcVWpR(2),         & 
& ZcplcFxvFxeHpL(2),ZcplcFxvFxeHpR(2),ZcplcFxvFxeVWpL(2),ZcplcFxvFxeVWpR(2),             & 
& ZcplcFxvFxvAhL(2,2),ZcplcFxvFxvAhR(2,2),ZcplcFxvFxvhhL(2,2,2),ZcplcFxvFxvhhR(2,2,2),   & 
& ZcplcHpVPVWp,ZcplcVWpVPVWp,ZcplhhcHpVWp(2),ZcplhhcVWpVWp(2),ZcplhhHpcHp(2),            & 
& ZcplhhHpcVWp(2),ZcplhhSsccSsc(2,2,2),ZcplhhVZVZ(2),ZcplHpcHpVP,ZcplHpcVWpVP

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
Real(dp), Intent(out) :: gP1LFxe(1,12) 
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
Real(dp) :: MRPFxeToFeSsc(3,2),MRGFxeToFeSsc(3,2), MRPZFxeToFeSsc(3,2),MRGZFxeToFeSsc(3,2) 
Real(dp) :: MVPFxeToFeSsc(3,2) 
Real(dp) :: RMsqTreeFxeToFeSsc(3,2),RMsqWaveFxeToFeSsc(3,2),RMsqVertexFxeToFeSsc(3,2) 
Complex(dp) :: AmpTreeFxeToFeSsc(2,3,2),AmpWaveFxeToFeSsc(2,3,2)=(0._dp,0._dp),AmpVertexFxeToFeSsc(2,3,2)& 
 & ,AmpVertexIRosFxeToFeSsc(2,3,2),AmpVertexIRdrFxeToFeSsc(2,3,2), AmpSumFxeToFeSsc(2,3,2), AmpSum2FxeToFeSsc(2,3,2) 
Complex(dp) :: AmpTreeZFxeToFeSsc(2,3,2),AmpWaveZFxeToFeSsc(2,3,2),AmpVertexZFxeToFeSsc(2,3,2) 
Real(dp) :: AmpSqFxeToFeSsc(3,2),  AmpSqTreeFxeToFeSsc(3,2) 
Real(dp) :: MRPFxeToFxeVZ,MRGFxeToFxeVZ, MRPZFxeToFxeVZ,MRGZFxeToFxeVZ 
Real(dp) :: MVPFxeToFxeVZ 
Real(dp) :: RMsqTreeFxeToFxeVZ,RMsqWaveFxeToFxeVZ,RMsqVertexFxeToFxeVZ 
Complex(dp) :: AmpTreeFxeToFxeVZ(4),AmpWaveFxeToFxeVZ(4)=(0._dp,0._dp),AmpVertexFxeToFxeVZ(4)& 
 & ,AmpVertexIRosFxeToFxeVZ(4),AmpVertexIRdrFxeToFxeVZ(4), AmpSumFxeToFxeVZ(4), AmpSum2FxeToFxeVZ(4) 
Complex(dp) :: AmpTreeZFxeToFxeVZ(4),AmpWaveZFxeToFxeVZ(4),AmpVertexZFxeToFxeVZ(4) 
Real(dp) :: AmpSqFxeToFxeVZ,  AmpSqTreeFxeToFxeVZ 
Real(dp) :: MRPFxeToFxvcVWp(2),MRGFxeToFxvcVWp(2), MRPZFxeToFxvcVWp(2),MRGZFxeToFxvcVWp(2) 
Real(dp) :: MVPFxeToFxvcVWp(2) 
Real(dp) :: RMsqTreeFxeToFxvcVWp(2),RMsqWaveFxeToFxvcVWp(2),RMsqVertexFxeToFxvcVWp(2) 
Complex(dp) :: AmpTreeFxeToFxvcVWp(4,2),AmpWaveFxeToFxvcVWp(4,2)=(0._dp,0._dp),AmpVertexFxeToFxvcVWp(4,2)& 
 & ,AmpVertexIRosFxeToFxvcVWp(4,2),AmpVertexIRdrFxeToFxvcVWp(4,2), AmpSumFxeToFxvcVWp(4,2), AmpSum2FxeToFxvcVWp(4,2) 
Complex(dp) :: AmpTreeZFxeToFxvcVWp(4,2),AmpWaveZFxeToFxvcVWp(4,2),AmpVertexZFxeToFxvcVWp(4,2) 
Real(dp) :: AmpSqFxeToFxvcVWp(2),  AmpSqTreeFxeToFxvcVWp(2) 
Real(dp) :: MRPFxeToFxehh(2),MRGFxeToFxehh(2), MRPZFxeToFxehh(2),MRGZFxeToFxehh(2) 
Real(dp) :: MVPFxeToFxehh(2) 
Real(dp) :: RMsqTreeFxeToFxehh(2),RMsqWaveFxeToFxehh(2),RMsqVertexFxeToFxehh(2) 
Complex(dp) :: AmpTreeFxeToFxehh(2,2),AmpWaveFxeToFxehh(2,2)=(0._dp,0._dp),AmpVertexFxeToFxehh(2,2)& 
 & ,AmpVertexIRosFxeToFxehh(2,2),AmpVertexIRdrFxeToFxehh(2,2), AmpSumFxeToFxehh(2,2), AmpSum2FxeToFxehh(2,2) 
Complex(dp) :: AmpTreeZFxeToFxehh(2,2),AmpWaveZFxeToFxehh(2,2),AmpVertexZFxeToFxehh(2,2) 
Real(dp) :: AmpSqFxeToFxehh(2),  AmpSqTreeFxeToFxehh(2) 
Real(dp) :: MRPFxeToFxeVP,MRGFxeToFxeVP, MRPZFxeToFxeVP,MRGZFxeToFxeVP 
Real(dp) :: MVPFxeToFxeVP 
Real(dp) :: RMsqTreeFxeToFxeVP,RMsqWaveFxeToFxeVP,RMsqVertexFxeToFxeVP 
Complex(dp) :: AmpTreeFxeToFxeVP(4),AmpWaveFxeToFxeVP(4)=(0._dp,0._dp),AmpVertexFxeToFxeVP(4)& 
 & ,AmpVertexIRosFxeToFxeVP(4),AmpVertexIRdrFxeToFxeVP(4), AmpSumFxeToFxeVP(4), AmpSum2FxeToFxeVP(4) 
Complex(dp) :: AmpTreeZFxeToFxeVP(4),AmpWaveZFxeToFxeVP(4),AmpVertexZFxeToFxeVP(4) 
Real(dp) :: AmpSqFxeToFxeVP,  AmpSqTreeFxeToFxeVP 
Write(*,*) "Calculating one-loop decays of Fxe " 
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
! Fe Ssc
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_FxeToFeSsc(cplcFxeFeSscL,cplcFxeFeSscR,MFe,             & 
& MFxe,MSsc,MFe2,MFxe2,MSsc2,AmpTreeFxeToFeSsc)

  Else 
Call Amplitude_Tree_SDdiracDM_FxeToFeSsc(ZcplcFxeFeSscL,ZcplcFxeFeSscR,               & 
& MFe,MFxe,MSsc,MFe2,MFxe2,MSsc2,AmpTreeFxeToFeSsc)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_FxeToFeSsc(MLambda,em,gs,cplcFxeFeSscL,cplcFxeFeSscR,       & 
& MFeOS,MFxeOS,MSscOS,MRPFxeToFeSsc,MRGFxeToFeSsc)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_FxeToFeSsc(MLambda,em,gs,ZcplcFxeFeSscL,ZcplcFxeFeSscR,     & 
& MFeOS,MFxeOS,MSscOS,MRPFxeToFeSsc,MRGFxeToFeSsc)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_FxeToFeSsc(MLambda,em,gs,cplcFxeFeSscL,cplcFxeFeSscR,       & 
& MFe,MFxe,MSsc,MRPFxeToFeSsc,MRGFxeToFeSsc)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_FxeToFeSsc(MLambda,em,gs,ZcplcFxeFeSscL,ZcplcFxeFeSscR,     & 
& MFe,MFxe,MSsc,MRPFxeToFeSsc,MRGFxeToFeSsc)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FxeToFeSsc(cplcFxeFeSscL,cplcFxeFeSscR,ctcplcFxeFeSscL, & 
& ctcplcFxeFeSscR,MFe,MFe2,MFxe,MFxe2,MSsc,MSsc2,Zfed,ZfEL,ZfER,Zfeu,ZfSsc,              & 
& AmpWaveFxeToFeSsc)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FxeToFeSsc(MFe,MFv,MFxe,MFxv,Mhh,MHp,MSsc,            & 
& MVP,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFeFehhL,        & 
& cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,           & 
& cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,cplcFeFeVZR,cplcFxvFvSscL,           & 
& cplcFxvFvSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL,  & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhSsccSsc,AmpVertexFxeToFeSsc)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_FxeToFeSsc(MFe,MFv,MFxe,MFxv,Mhh,MHp,              & 
& MSsc,MVP,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,               & 
& cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,           & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,cplcFeFeVZR,             & 
& cplcFxvFvSscL,cplcFxvFvSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,   & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhSsccSsc,            & 
& AmpVertexIRdrFxeToFeSsc)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxeToFeSsc(MFeOS,MFvOS,MFxeOS,MFxvOS,              & 
& MhhOS,MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MFe2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,              & 
& MHp2OS,MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,    & 
& ZcplcFxeFeSscL,ZcplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,       & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,       & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,             & 
& cplcFxeFxvcVWpR,cplhhSsccSsc,AmpVertexIRosFxeToFeSsc)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxeToFeSsc(MFe,MFv,MFxe,MFxv,Mhh,MHp,              & 
& MSsc,MVP,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,               & 
& cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,ZcplcFxeFeSscL,ZcplcFxeFeSscR,         & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,cplcFeFeVZR,             & 
& cplcFxvFvSscL,cplcFxvFvSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,   & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhSsccSsc,            & 
& AmpVertexIRosFxeToFeSsc)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxeToFeSsc(MFeOS,MFvOS,MFxeOS,MFxvOS,              & 
& MhhOS,MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MFe2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,              & 
& MHp2OS,MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,    & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,       & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,             & 
& cplcFxeFxvcVWpR,cplhhSsccSsc,AmpVertexIRosFxeToFeSsc)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxeToFeSsc(MFe,MFv,MFxe,MFxv,Mhh,MHp,              & 
& MSsc,MVP,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,               & 
& cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,           & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,cplcFeFeVZR,             & 
& cplcFxvFvSscL,cplcFxvFvSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,   & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhSsccSsc,            & 
& AmpVertexIRosFxeToFeSsc)

 End if 
 End if 
AmpVertexFxeToFeSsc = AmpVertexFxeToFeSsc -  AmpVertexIRdrFxeToFeSsc! +  AmpVertexIRosFxeToFeSsc ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Final State 1 
AmpWaveZFxeToFeSsc=0._dp 
AmpVertexZFxeToFeSsc=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFxeToFeSsc(1,gt2,:) = AmpWaveZFxeToFeSsc(1,gt2,:)+ZRUVe(gt2,gt1)*AmpWaveFxeToFeSsc(1,gt1,:) 
AmpVertexZFxeToFeSsc(1,gt2,:)= AmpVertexZFxeToFeSsc(1,gt2,:)+ZRUVe(gt2,gt1)*AmpVertexFxeToFeSsc(1,gt1,:) 
AmpWaveZFxeToFeSsc(2,gt2,:) = AmpWaveZFxeToFeSsc(2,gt2,:)+ZRUUec(gt2,gt1)*AmpWaveFxeToFeSsc(2,gt1,:) 
AmpVertexZFxeToFeSsc(2,gt2,:)= AmpVertexZFxeToFeSsc(2,gt2,:)+ZRUUec(gt2,gt1)*AmpVertexFxeToFeSsc(2,gt1,:) 
 End Do 
End Do 
AmpWaveFxeToFeSsc=AmpWaveZFxeToFeSsc 
AmpVertexFxeToFeSsc= AmpVertexZFxeToFeSsc
! Final State 2 
AmpWaveZFxeToFeSsc=0._dp 
AmpVertexZFxeToFeSsc=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZFxeToFeSsc(:,:,gt2) = AmpWaveZFxeToFeSsc(:,:,gt2)+ZRUVSs(gt2,gt1)*AmpWaveFxeToFeSsc(:,:,gt1) 
AmpVertexZFxeToFeSsc(:,:,gt2)= AmpVertexZFxeToFeSsc(:,:,gt2)+ZRUVSs(gt2,gt1)*AmpVertexFxeToFeSsc(:,:,gt1) 
 End Do 
End Do 
AmpWaveFxeToFeSsc=AmpWaveZFxeToFeSsc 
AmpVertexFxeToFeSsc= AmpVertexZFxeToFeSsc
End if
If (ShiftIRdiv) Then 
AmpVertexFxeToFeSsc = AmpVertexFxeToFeSsc  +  AmpVertexIRosFxeToFeSsc
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fxe->Fe Ssc -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumFxeToFeSsc = AmpTreeFxeToFeSsc 
 AmpSum2FxeToFeSsc = AmpTreeFxeToFeSsc + 2._dp*AmpWaveFxeToFeSsc + 2._dp*AmpVertexFxeToFeSsc  
Else 
 AmpSumFxeToFeSsc = AmpTreeFxeToFeSsc + AmpWaveFxeToFeSsc + AmpVertexFxeToFeSsc
 AmpSum2FxeToFeSsc = AmpTreeFxeToFeSsc + AmpWaveFxeToFeSsc + AmpVertexFxeToFeSsc 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumFxeToFeSsc = AmpTreeFxeToFeSsc
 AmpSum2FxeToFeSsc = AmpTreeFxeToFeSsc 
End if 
gt1=1 
i4 = isave 
  Do gt2=1,3
    Do gt3=1,2
If (((OSkinematics).and.(MFxeOS.gt.(MFeOS(gt2)+MSscOS(gt3)))).or.((.not.OSkinematics).and.(MFxe.gt.(MFe(gt2)+MSsc(gt3))))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt2, gt3 
  AmpSum2FxeToFeSsc = AmpTreeFxeToFeSsc
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFxeOS,MFeOS(gt2),MSscOS(gt3),AmpSumFxeToFeSsc(:,gt2, gt3),AmpSum2FxeToFeSsc(:,gt2, gt3),AmpSqFxeToFeSsc(gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFxe,MFe(gt2),MSsc(gt3),AmpSumFxeToFeSsc(:,gt2, gt3),AmpSum2FxeToFeSsc(:,gt2, gt3),AmpSqFxeToFeSsc(gt2, gt3)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqFxeToFeSsc(gt2, gt3) 
  AmpSum2FxeToFeSsc = 2._dp*AmpWaveFxeToFeSsc
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFxeOS,MFeOS(gt2),MSscOS(gt3),AmpSumFxeToFeSsc(:,gt2, gt3),AmpSum2FxeToFeSsc(:,gt2, gt3),AmpSqFxeToFeSsc(gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFxe,MFe(gt2),MSsc(gt3),AmpSumFxeToFeSsc(:,gt2, gt3),AmpSum2FxeToFeSsc(:,gt2, gt3),AmpSqFxeToFeSsc(gt2, gt3)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqFxeToFeSsc(gt2, gt3) 
  AmpSum2FxeToFeSsc = 2._dp*AmpVertexFxeToFeSsc
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFxeOS,MFeOS(gt2),MSscOS(gt3),AmpSumFxeToFeSsc(:,gt2, gt3),AmpSum2FxeToFeSsc(:,gt2, gt3),AmpSqFxeToFeSsc(gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFxe,MFe(gt2),MSsc(gt3),AmpSumFxeToFeSsc(:,gt2, gt3),AmpSum2FxeToFeSsc(:,gt2, gt3),AmpSqFxeToFeSsc(gt2, gt3)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqFxeToFeSsc(gt2, gt3) 
  AmpSum2FxeToFeSsc = AmpTreeFxeToFeSsc + 2._dp*AmpWaveFxeToFeSsc + 2._dp*AmpVertexFxeToFeSsc
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFxeOS,MFeOS(gt2),MSscOS(gt3),AmpSumFxeToFeSsc(:,gt2, gt3),AmpSum2FxeToFeSsc(:,gt2, gt3),AmpSqFxeToFeSsc(gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFxe,MFe(gt2),MSsc(gt3),AmpSumFxeToFeSsc(:,gt2, gt3),AmpSum2FxeToFeSsc(:,gt2, gt3),AmpSqFxeToFeSsc(gt2, gt3)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqFxeToFeSsc(gt2, gt3) 
 End if 
If (OSkinematics) Then 
  AmpSum2FxeToFeSsc = AmpTreeFxeToFeSsc
  Call SquareAmp_FtoFS(MFxeOS,MFeOS(gt2),MSscOS(gt3),AmpSumFxeToFeSsc(:,gt2, gt3),AmpSum2FxeToFeSsc(:,gt2, gt3),AmpSqFxeToFeSsc(gt2, gt3)) 
  AmpSqTreeFxeToFeSsc(gt2, gt3) = AmpSqFxeToFeSsc(gt2, gt3)  
  AmpSum2FxeToFeSsc = + 2._dp*AmpWaveFxeToFeSsc + 2._dp*AmpVertexFxeToFeSsc
  Call SquareAmp_FtoFS(MFxeOS,MFeOS(gt2),MSscOS(gt3),AmpSumFxeToFeSsc(:,gt2, gt3),AmpSum2FxeToFeSsc(:,gt2, gt3),AmpSqFxeToFeSsc(gt2, gt3)) 
  AmpSqFxeToFeSsc(gt2, gt3) = AmpSqFxeToFeSsc(gt2, gt3) + AmpSqTreeFxeToFeSsc(gt2, gt3)  
Else  
  AmpSum2FxeToFeSsc = AmpTreeFxeToFeSsc
  Call SquareAmp_FtoFS(MFxe,MFe(gt2),MSsc(gt3),AmpSumFxeToFeSsc(:,gt2, gt3),AmpSum2FxeToFeSsc(:,gt2, gt3),AmpSqFxeToFeSsc(gt2, gt3)) 
  AmpSqTreeFxeToFeSsc(gt2, gt3) = AmpSqFxeToFeSsc(gt2, gt3)  
  AmpSum2FxeToFeSsc = + 2._dp*AmpWaveFxeToFeSsc + 2._dp*AmpVertexFxeToFeSsc
  Call SquareAmp_FtoFS(MFxe,MFe(gt2),MSsc(gt3),AmpSumFxeToFeSsc(:,gt2, gt3),AmpSum2FxeToFeSsc(:,gt2, gt3),AmpSqFxeToFeSsc(gt2, gt3)) 
  AmpSqFxeToFeSsc(gt2, gt3) = AmpSqFxeToFeSsc(gt2, gt3) + AmpSqTreeFxeToFeSsc(gt2, gt3)  
End if  
Else  
  AmpSqFxeToFeSsc(gt2, gt3) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFxeToFeSsc(gt2, gt3).le.0._dp) Then 
  gP1LFxe(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFxe(gt1,i4) = 1._dp*GammaTPS(MFxeOS,MFeOS(gt2),MSscOS(gt3),helfactor*AmpSqFxeToFeSsc(gt2, gt3))
Else 
  gP1LFxe(gt1,i4) = 1._dp*GammaTPS(MFxe,MFe(gt2),MSsc(gt3),helfactor*AmpSqFxeToFeSsc(gt2, gt3))
End if 
If ((Abs(MRPFxeToFeSsc(gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGFxeToFeSsc(gt2, gt3)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFxe(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPFxeToFeSsc(gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGFxeToFeSsc(gt2, gt3)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPFxeToFeSsc(gt2, gt3) + MRGFxeToFeSsc(gt2, gt3)) 
  gP1LFxe(gt1,i4) = gP1LFxe(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPFxeToFeSsc(gt2, gt3) + MRGFxeToFeSsc(gt2, gt3))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LFxe(gt1,i4) 
  End if 
End if 
End if 
i4=i4+1

    End do
  End do
isave = i4 
End If 
If (.not.CalcLoopDecay_LoopInducedOnly) Then 
!---------------- 
! Fxe VZ
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_FxeToFxeVZ(cplcFxeFxeVZL,cplcFxeFxeVZR,MFxe,            & 
& MVZ,MFxe2,MVZ2,AmpTreeFxeToFxeVZ)

  Else 
Call Amplitude_Tree_SDdiracDM_FxeToFxeVZ(ZcplcFxeFxeVZL,ZcplcFxeFxeVZR,               & 
& MFxe,MVZ,MFxe2,MVZ2,AmpTreeFxeToFxeVZ)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_FxeToFxeVZ(MLambda,em,gs,cplcFxeFxeVZL,cplcFxeFxeVZR,       & 
& MFxeOS,MVZOS,MRPFxeToFxeVZ,MRGFxeToFxeVZ)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_FxeToFxeVZ(MLambda,em,gs,ZcplcFxeFxeVZL,ZcplcFxeFxeVZR,     & 
& MFxeOS,MVZOS,MRPFxeToFxeVZ,MRGFxeToFxeVZ)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_FxeToFxeVZ(MLambda,em,gs,cplcFxeFxeVZL,cplcFxeFxeVZR,       & 
& MFxe,MVZ,MRPFxeToFxeVZ,MRGFxeToFxeVZ)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_FxeToFxeVZ(MLambda,em,gs,ZcplcFxeFxeVZL,ZcplcFxeFxeVZR,     & 
& MFxe,MVZ,MRPFxeToFxeVZ,MRGFxeToFxeVZ)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FxeToFxeVZ(cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,   & 
& cplcFxeFxeVZR,ctcplcFxeFxeVPL,ctcplcFxeFxeVPR,ctcplcFxeFxeVZL,ctcplcFxeFxeVZR,         & 
& MFxe,MFxe2,MVP,MVP2,MVZ,MVZ2,Zfed,Zfeu,ZfVPVZ,ZfVZ,AmpWaveFxeToFxeVZ)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FxeToFxeVZ(MFe,MFxe,MFxv,MHp,MSsc,MVP,MVWp,           & 
& MVZ,MFe2,MFxe2,MFxv2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFxeFeSscL,cplcFxeFeSscR,           & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,       & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,              & 
& cplcFeFxecSscR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,              & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,       & 
& AmpVertexFxeToFxeVZ)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_FxeToFxeVZ(MFe,MFxe,MFxv,MHp,MSsc,MVP,             & 
& MVWp,MVZ,MFe2,MFxe2,MFxv2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFxeFeSscL,cplcFxeFeSscR,      & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,       & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,              & 
& cplcFeFxecSscR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,              & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,       & 
& AmpVertexIRdrFxeToFxeVZ)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxeToFxeVZ(MFeOS,MFxeOS,MFxvOS,MHpOS,              & 
& MSscOS,MVP,MVWpOS,MVZOS,MFe2OS,MFxe2OS,MFxv2OS,MHp2OS,MSsc2OS,MVP2,MVWp2OS,            & 
& MVZ2OS,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVZL,cplcFeFeVZR,cplcFxvFxeHpL,              & 
& cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,               & 
& ZcplcFxeFxeVZL,ZcplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvVZL,             & 
& cplcFxvFxvVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,           & 
& cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFxeToFxeVZ)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxeToFxeVZ(MFe,MFxe,MFxv,MHp,MSsc,MVP,             & 
& MVWp,MVZ,MFe2,MFxe2,MFxv2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFxeFeSscL,cplcFxeFeSscR,      & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,       & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,ZcplcFxeFxeVZL,ZcplcFxeFxeVZR,cplcFeFxecSscL,            & 
& cplcFeFxecSscR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,              & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,       & 
& AmpVertexIRosFxeToFxeVZ)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxeToFxeVZ(MFeOS,MFxeOS,MFxvOS,MHpOS,              & 
& MSscOS,MVP,MVWpOS,MVZOS,MFe2OS,MFxe2OS,MFxv2OS,MHp2OS,MSsc2OS,MVP2,MVWp2OS,            & 
& MVZ2OS,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVZL,cplcFeFeVZR,cplcFxvFxeHpL,              & 
& cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,               & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvVZL,               & 
& cplcFxvFxvVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,           & 
& cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFxeToFxeVZ)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxeToFxeVZ(MFe,MFxe,MFxv,MHp,MSsc,MVP,             & 
& MVWp,MVZ,MFe2,MFxe2,MFxv2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFxeFeSscL,cplcFxeFeSscR,      & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,       & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,              & 
& cplcFeFxecSscR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,              & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,       & 
& AmpVertexIRosFxeToFxeVZ)

 End if 
 End if 
AmpVertexFxeToFxeVZ = AmpVertexFxeToFxeVZ -  AmpVertexIRdrFxeToFxeVZ! +  AmpVertexIRosFxeToFxeVZ ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
End if
If (ShiftIRdiv) Then 
AmpVertexFxeToFxeVZ = AmpVertexFxeToFxeVZ  +  AmpVertexIRosFxeToFxeVZ
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fxe->Fxe VZ -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumFxeToFxeVZ = AmpTreeFxeToFxeVZ 
 AmpSum2FxeToFxeVZ = AmpTreeFxeToFxeVZ + 2._dp*AmpWaveFxeToFxeVZ + 2._dp*AmpVertexFxeToFxeVZ  
Else 
 AmpSumFxeToFxeVZ = AmpTreeFxeToFxeVZ + AmpWaveFxeToFxeVZ + AmpVertexFxeToFxeVZ
 AmpSum2FxeToFxeVZ = AmpTreeFxeToFxeVZ + AmpWaveFxeToFxeVZ + AmpVertexFxeToFxeVZ 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumFxeToFxeVZ = AmpTreeFxeToFxeVZ
 AmpSum2FxeToFxeVZ = AmpTreeFxeToFxeVZ 
End if 
gt1=1 
i4 = isave 
If (((OSkinematics).and.(MFxeOS.gt.(MFxeOS+MVZOS))).or.((.not.OSkinematics).and.(MFxe.gt.(MFxe+MVZ)))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*)  
  AmpSum2FxeToFxeVZ = AmpTreeFxeToFxeVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxeOS,MFxeOS,MVZOS,AmpSumFxeToFxeVZ(:),AmpSum2FxeToFxeVZ(:),AmpSqFxeToFxeVZ) 
Else  
  Call SquareAmp_FtoFV(MFxe,MFxe,MVZ,AmpSumFxeToFxeVZ(:),AmpSum2FxeToFxeVZ(:),AmpSqFxeToFxeVZ) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqFxeToFxeVZ 
  AmpSum2FxeToFxeVZ = 2._dp*AmpWaveFxeToFxeVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxeOS,MFxeOS,MVZOS,AmpSumFxeToFxeVZ(:),AmpSum2FxeToFxeVZ(:),AmpSqFxeToFxeVZ) 
Else  
  Call SquareAmp_FtoFV(MFxe,MFxe,MVZ,AmpSumFxeToFxeVZ(:),AmpSum2FxeToFxeVZ(:),AmpSqFxeToFxeVZ) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqFxeToFxeVZ 
  AmpSum2FxeToFxeVZ = 2._dp*AmpVertexFxeToFxeVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxeOS,MFxeOS,MVZOS,AmpSumFxeToFxeVZ(:),AmpSum2FxeToFxeVZ(:),AmpSqFxeToFxeVZ) 
Else  
  Call SquareAmp_FtoFV(MFxe,MFxe,MVZ,AmpSumFxeToFxeVZ(:),AmpSum2FxeToFxeVZ(:),AmpSqFxeToFxeVZ) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqFxeToFxeVZ 
  AmpSum2FxeToFxeVZ = AmpTreeFxeToFxeVZ + 2._dp*AmpWaveFxeToFxeVZ + 2._dp*AmpVertexFxeToFxeVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxeOS,MFxeOS,MVZOS,AmpSumFxeToFxeVZ(:),AmpSum2FxeToFxeVZ(:),AmpSqFxeToFxeVZ) 
Else  
  Call SquareAmp_FtoFV(MFxe,MFxe,MVZ,AmpSumFxeToFxeVZ(:),AmpSum2FxeToFxeVZ(:),AmpSqFxeToFxeVZ) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqFxeToFxeVZ 
 End if 
If (OSkinematics) Then 
  AmpSum2FxeToFxeVZ = AmpTreeFxeToFxeVZ
  Call SquareAmp_FtoFV(MFxeOS,MFxeOS,MVZOS,AmpSumFxeToFxeVZ(:),AmpSum2FxeToFxeVZ(:),AmpSqFxeToFxeVZ) 
  AmpSqTreeFxeToFxeVZ = AmpSqFxeToFxeVZ  
  AmpSum2FxeToFxeVZ = + 2._dp*AmpWaveFxeToFxeVZ + 2._dp*AmpVertexFxeToFxeVZ
  Call SquareAmp_FtoFV(MFxeOS,MFxeOS,MVZOS,AmpSumFxeToFxeVZ(:),AmpSum2FxeToFxeVZ(:),AmpSqFxeToFxeVZ) 
  AmpSqFxeToFxeVZ = AmpSqFxeToFxeVZ + AmpSqTreeFxeToFxeVZ  
Else  
  AmpSum2FxeToFxeVZ = AmpTreeFxeToFxeVZ
  Call SquareAmp_FtoFV(MFxe,MFxe,MVZ,AmpSumFxeToFxeVZ(:),AmpSum2FxeToFxeVZ(:),AmpSqFxeToFxeVZ) 
  AmpSqTreeFxeToFxeVZ = AmpSqFxeToFxeVZ  
  AmpSum2FxeToFxeVZ = + 2._dp*AmpWaveFxeToFxeVZ + 2._dp*AmpVertexFxeToFxeVZ
  Call SquareAmp_FtoFV(MFxe,MFxe,MVZ,AmpSumFxeToFxeVZ(:),AmpSum2FxeToFxeVZ(:),AmpSqFxeToFxeVZ) 
  AmpSqFxeToFxeVZ = AmpSqFxeToFxeVZ + AmpSqTreeFxeToFxeVZ  
End if  
Else  
  AmpSqFxeToFxeVZ = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFxeToFxeVZ.le.0._dp) Then 
  gP1LFxe(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFxe(gt1,i4) = 1._dp*GammaTPS(MFxeOS,MFxeOS,MVZOS,helfactor*AmpSqFxeToFxeVZ)
Else 
  gP1LFxe(gt1,i4) = 1._dp*GammaTPS(MFxe,MFxe,MVZ,helfactor*AmpSqFxeToFxeVZ)
End if 
If ((Abs(MRPFxeToFxeVZ).gt.1.0E-20_dp).or.(Abs(MRGFxeToFxeVZ).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFxe(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPFxeToFxeVZ).gt.1.0E-20_dp).or.(Abs(MRGFxeToFxeVZ).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPFxeToFxeVZ + MRGFxeToFxeVZ) 
  gP1LFxe(gt1,i4) = gP1LFxe(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPFxeToFxeVZ + MRGFxeToFxeVZ)
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LFxe(gt1,i4) 
  End if 
End if 
End if 
i4=i4+1

isave = i4 
End If 
If (.not.CalcLoopDecay_LoopInducedOnly) Then 
!---------------- 
! Fxv Conjg(VWp)
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_FxeToFxvcVWp(cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,           & 
& MFxe,MFxv,MVWp,MFxe2,MFxv2,MVWp2,AmpTreeFxeToFxvcVWp)

  Else 
Call Amplitude_Tree_SDdiracDM_FxeToFxvcVWp(ZcplcFxeFxvcVWpL,ZcplcFxeFxvcVWpR,         & 
& MFxe,MFxv,MVWp,MFxe2,MFxv2,MVWp2,AmpTreeFxeToFxvcVWp)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_FxeToFxvcVWp(MLambda,em,gs,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR, & 
& MFxeOS,MFxvOS,MVWpOS,MRPFxeToFxvcVWp,MRGFxeToFxvcVWp)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_FxeToFxvcVWp(MLambda,em,gs,ZcplcFxeFxvcVWpL,ZcplcFxeFxvcVWpR,& 
& MFxeOS,MFxvOS,MVWpOS,MRPFxeToFxvcVWp,MRGFxeToFxvcVWp)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_FxeToFxvcVWp(MLambda,em,gs,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR, & 
& MFxe,MFxv,MVWp,MRPFxeToFxvcVWp,MRGFxeToFxvcVWp)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_FxeToFxvcVWp(MLambda,em,gs,ZcplcFxeFxvcVWpL,ZcplcFxeFxvcVWpR,& 
& MFxe,MFxv,MVWp,MRPFxeToFxvcVWp,MRGFxeToFxvcVWp)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FxeToFxvcVWp(cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,           & 
& ctcplcFxeFxvcVWpL,ctcplcFxeFxvcVWpR,MFxe,MFxe2,MFxv,MFxv2,MVWp,MVWp2,Zfed,             & 
& Zfeu,ZfVWp,ZfxVL,ZfxVR,AmpWaveFxeToFxvcVWp)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FxeToFxvcVWp(MAh,MFe,MFv,MFxe,MFxv,Mhh,               & 
& MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,           & 
& MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhHpcVWp,cplcFxeFeSscL,cplcFxeFeSscR,              & 
& cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,   & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR, & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcVWp,             & 
& cplhhcVWpVWp,cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,AmpVertexFxeToFxvcVWp)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_FxeToFxvcVWp(MAh,MFe,MFv,MFxe,MFxv,Mhh,            & 
& MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,           & 
& MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhHpcVWp,cplcFxeFeSscL,cplcFxeFeSscR,              & 
& cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,   & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR, & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcVWp,             & 
& cplhhcVWpVWp,cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,AmpVertexIRdrFxeToFxvcVWp)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxeToFxvcVWp(MAhOS,MFeOS,MFvOS,MFxeOS,             & 
& MFxvOS,MhhOS,MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,               & 
& MFxv2OS,Mhh2OS,MHp2OS,MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplcFxvFxvAhL,cplcFxvFxvAhR,         & 
& cplAhHpcVWp,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxeVPL,     & 
& cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,   & 
& cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,GosZcplcFxeFxvcHpL,GosZcplcFxeFxvcHpR,     & 
& ZcplcFxeFxvcVWpL,ZcplcFxeFxvcVWpR,cplhhHpcVWp,cplhhcVWpVWp,GosZcplHpcVWpVP,            & 
& cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,AmpVertexIRosFxeToFxvcVWp)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxeToFxvcVWp(MAh,MFe,MFv,MFxe,MFxv,Mhh,            & 
& MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,           & 
& MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhHpcVWp,cplcFxeFeSscL,cplcFxeFeSscR,              & 
& cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,   & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR, & 
& GZcplcFxeFxvcHpL,GZcplcFxeFxvcHpR,ZcplcFxeFxvcVWpL,ZcplcFxeFxvcVWpR,cplhhHpcVWp,       & 
& cplhhcVWpVWp,GZcplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,AmpVertexIRosFxeToFxvcVWp)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxeToFxvcVWp(MAhOS,MFeOS,MFvOS,MFxeOS,             & 
& MFxvOS,MhhOS,MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,               & 
& MFxv2OS,Mhh2OS,MHp2OS,MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplcFxvFxvAhL,cplcFxvFxvAhR,         & 
& cplAhHpcVWp,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxeVPL,     & 
& cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,   & 
& cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,GcplcFxeFxvcHpL,GcplcFxeFxvcHpR,           & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcVWp,cplhhcVWpVWp,GcplHpcVWpVP,cplHpcVWpVZ,     & 
& cplcVWpVPVWp,cplcVWpVWpVZ,AmpVertexIRosFxeToFxvcVWp)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxeToFxvcVWp(MAh,MFe,MFv,MFxe,MFxv,Mhh,            & 
& MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,           & 
& MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhHpcVWp,cplcFxeFeSscL,cplcFxeFeSscR,              & 
& cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,   & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR, & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcVWp,             & 
& cplhhcVWpVWp,cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,AmpVertexIRosFxeToFxvcVWp)

 End if 
 End if 
AmpVertexFxeToFxvcVWp = AmpVertexFxeToFxvcVWp -  AmpVertexIRdrFxeToFxvcVWp! +  AmpVertexIRosFxeToFxvcVWp ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Final State 1 
AmpWaveZFxeToFxvcVWp=0._dp 
AmpVertexZFxeToFxvcVWp=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZFxeToFxvcVWp(1,gt2) = AmpWaveZFxeToFxvcVWp(1,gt2)+ZRUXV(gt2,gt1)*AmpWaveFxeToFxvcVWp(1,gt1) 
AmpVertexZFxeToFxvcVWp(1,gt2)= AmpVertexZFxeToFxvcVWp(1,gt2)+ZRUXV(gt2,gt1)*AmpVertexFxeToFxvcVWp(1,gt1) 
AmpWaveZFxeToFxvcVWp(2,gt2) = AmpWaveZFxeToFxvcVWp(2,gt2)+ZRUXUc(gt2,gt1)*AmpWaveFxeToFxvcVWp(2,gt1) 
AmpVertexZFxeToFxvcVWp(2,gt2)= AmpVertexZFxeToFxvcVWp(2,gt2)+ZRUXUc(gt2,gt1)*AmpVertexFxeToFxvcVWp(2,gt1) 
AmpWaveZFxeToFxvcVWp(3,gt2) = AmpWaveZFxeToFxvcVWp(3,gt2)+ZRUXV(gt2,gt1)*AmpWaveFxeToFxvcVWp(3,gt1) 
AmpVertexZFxeToFxvcVWp(3,gt2)= AmpVertexZFxeToFxvcVWp(3,gt2)+ZRUXV(gt2,gt1)*AmpVertexFxeToFxvcVWp(3,gt1) 
AmpWaveZFxeToFxvcVWp(4,gt2) = AmpWaveZFxeToFxvcVWp(4,gt2)+ZRUXUc(gt2,gt1)*AmpWaveFxeToFxvcVWp(4,gt1) 
AmpVertexZFxeToFxvcVWp(4,gt2)= AmpVertexZFxeToFxvcVWp(4,gt2)+ZRUXUc(gt2,gt1)*AmpVertexFxeToFxvcVWp(4,gt1) 
 End Do 
End Do 
AmpWaveFxeToFxvcVWp=AmpWaveZFxeToFxvcVWp 
AmpVertexFxeToFxvcVWp= AmpVertexZFxeToFxvcVWp
End if
If (ShiftIRdiv) Then 
AmpVertexFxeToFxvcVWp = AmpVertexFxeToFxvcVWp  +  AmpVertexIRosFxeToFxvcVWp
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fxe->Fxv conj[VWp] -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumFxeToFxvcVWp = AmpTreeFxeToFxvcVWp 
 AmpSum2FxeToFxvcVWp = AmpTreeFxeToFxvcVWp + 2._dp*AmpWaveFxeToFxvcVWp + 2._dp*AmpVertexFxeToFxvcVWp  
Else 
 AmpSumFxeToFxvcVWp = AmpTreeFxeToFxvcVWp + AmpWaveFxeToFxvcVWp + AmpVertexFxeToFxvcVWp
 AmpSum2FxeToFxvcVWp = AmpTreeFxeToFxvcVWp + AmpWaveFxeToFxvcVWp + AmpVertexFxeToFxvcVWp 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumFxeToFxvcVWp = AmpTreeFxeToFxvcVWp
 AmpSum2FxeToFxvcVWp = AmpTreeFxeToFxvcVWp 
End if 
gt1=1 
i4 = isave 
  Do gt2=1,2
If (((OSkinematics).and.(MFxeOS.gt.(MFxvOS(gt2)+MVWpOS))).or.((.not.OSkinematics).and.(MFxe.gt.(MFxv(gt2)+MVWp)))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt2 
  AmpSum2FxeToFxvcVWp = AmpTreeFxeToFxvcVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxeOS,MFxvOS(gt2),MVWpOS,AmpSumFxeToFxvcVWp(:,gt2),AmpSum2FxeToFxvcVWp(:,gt2),AmpSqFxeToFxvcVWp(gt2)) 
Else  
  Call SquareAmp_FtoFV(MFxe,MFxv(gt2),MVWp,AmpSumFxeToFxvcVWp(:,gt2),AmpSum2FxeToFxvcVWp(:,gt2),AmpSqFxeToFxvcVWp(gt2)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqFxeToFxvcVWp(gt2) 
  AmpSum2FxeToFxvcVWp = 2._dp*AmpWaveFxeToFxvcVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxeOS,MFxvOS(gt2),MVWpOS,AmpSumFxeToFxvcVWp(:,gt2),AmpSum2FxeToFxvcVWp(:,gt2),AmpSqFxeToFxvcVWp(gt2)) 
Else  
  Call SquareAmp_FtoFV(MFxe,MFxv(gt2),MVWp,AmpSumFxeToFxvcVWp(:,gt2),AmpSum2FxeToFxvcVWp(:,gt2),AmpSqFxeToFxvcVWp(gt2)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqFxeToFxvcVWp(gt2) 
  AmpSum2FxeToFxvcVWp = 2._dp*AmpVertexFxeToFxvcVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxeOS,MFxvOS(gt2),MVWpOS,AmpSumFxeToFxvcVWp(:,gt2),AmpSum2FxeToFxvcVWp(:,gt2),AmpSqFxeToFxvcVWp(gt2)) 
Else  
  Call SquareAmp_FtoFV(MFxe,MFxv(gt2),MVWp,AmpSumFxeToFxvcVWp(:,gt2),AmpSum2FxeToFxvcVWp(:,gt2),AmpSqFxeToFxvcVWp(gt2)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqFxeToFxvcVWp(gt2) 
  AmpSum2FxeToFxvcVWp = AmpTreeFxeToFxvcVWp + 2._dp*AmpWaveFxeToFxvcVWp + 2._dp*AmpVertexFxeToFxvcVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxeOS,MFxvOS(gt2),MVWpOS,AmpSumFxeToFxvcVWp(:,gt2),AmpSum2FxeToFxvcVWp(:,gt2),AmpSqFxeToFxvcVWp(gt2)) 
Else  
  Call SquareAmp_FtoFV(MFxe,MFxv(gt2),MVWp,AmpSumFxeToFxvcVWp(:,gt2),AmpSum2FxeToFxvcVWp(:,gt2),AmpSqFxeToFxvcVWp(gt2)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqFxeToFxvcVWp(gt2) 
 End if 
If (OSkinematics) Then 
  AmpSum2FxeToFxvcVWp = AmpTreeFxeToFxvcVWp
  Call SquareAmp_FtoFV(MFxeOS,MFxvOS(gt2),MVWpOS,AmpSumFxeToFxvcVWp(:,gt2),AmpSum2FxeToFxvcVWp(:,gt2),AmpSqFxeToFxvcVWp(gt2)) 
  AmpSqTreeFxeToFxvcVWp(gt2) = AmpSqFxeToFxvcVWp(gt2)  
  AmpSum2FxeToFxvcVWp = + 2._dp*AmpWaveFxeToFxvcVWp + 2._dp*AmpVertexFxeToFxvcVWp
  Call SquareAmp_FtoFV(MFxeOS,MFxvOS(gt2),MVWpOS,AmpSumFxeToFxvcVWp(:,gt2),AmpSum2FxeToFxvcVWp(:,gt2),AmpSqFxeToFxvcVWp(gt2)) 
  AmpSqFxeToFxvcVWp(gt2) = AmpSqFxeToFxvcVWp(gt2) + AmpSqTreeFxeToFxvcVWp(gt2)  
Else  
  AmpSum2FxeToFxvcVWp = AmpTreeFxeToFxvcVWp
  Call SquareAmp_FtoFV(MFxe,MFxv(gt2),MVWp,AmpSumFxeToFxvcVWp(:,gt2),AmpSum2FxeToFxvcVWp(:,gt2),AmpSqFxeToFxvcVWp(gt2)) 
  AmpSqTreeFxeToFxvcVWp(gt2) = AmpSqFxeToFxvcVWp(gt2)  
  AmpSum2FxeToFxvcVWp = + 2._dp*AmpWaveFxeToFxvcVWp + 2._dp*AmpVertexFxeToFxvcVWp
  Call SquareAmp_FtoFV(MFxe,MFxv(gt2),MVWp,AmpSumFxeToFxvcVWp(:,gt2),AmpSum2FxeToFxvcVWp(:,gt2),AmpSqFxeToFxvcVWp(gt2)) 
  AmpSqFxeToFxvcVWp(gt2) = AmpSqFxeToFxvcVWp(gt2) + AmpSqTreeFxeToFxvcVWp(gt2)  
End if  
Else  
  AmpSqFxeToFxvcVWp(gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFxeToFxvcVWp(gt2).le.0._dp) Then 
  gP1LFxe(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFxe(gt1,i4) = 1._dp*GammaTPS(MFxeOS,MFxvOS(gt2),MVWpOS,helfactor*AmpSqFxeToFxvcVWp(gt2))
Else 
  gP1LFxe(gt1,i4) = 1._dp*GammaTPS(MFxe,MFxv(gt2),MVWp,helfactor*AmpSqFxeToFxvcVWp(gt2))
End if 
If ((Abs(MRPFxeToFxvcVWp(gt2)).gt.1.0E-20_dp).or.(Abs(MRGFxeToFxvcVWp(gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFxe(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPFxeToFxvcVWp(gt2)).gt.1.0E-20_dp).or.(Abs(MRGFxeToFxvcVWp(gt2)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPFxeToFxvcVWp(gt2) + MRGFxeToFxvcVWp(gt2)) 
  gP1LFxe(gt1,i4) = gP1LFxe(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPFxeToFxvcVWp(gt2) + MRGFxeToFxvcVWp(gt2))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LFxe(gt1,i4) 
  End if 
End if 
End if 
i4=i4+1

  End do
isave = i4 
End If 
!---------------- 
! Fxe hh
!---------------- 

If (LoopInducedDecaysOS) Then 


!Self-energy Corrections 


!Vertex Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_VERTEX_SDdiracDM_FxeToFxehh(MFeOS,MFxeOS,MFxvOS,MhhOS,MHpOS,           & 
& MSscOS,MVWpOS,MVZOS,MFe2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,MSsc2OS,MVWp2OS,              & 
& MVZ2OS,ZcplcFeFehhL,ZcplcFeFehhR,ZcplcFxeFeSscL,ZcplcFxeFeSscR,ZcplcFxvFxeHpL,         & 
& ZcplcFxvFxeHpR,ZcplcFxvFxeVWpL,ZcplcFxvFxeVWpR,ZcplcFxeFxeVZL,ZcplcFxeFxeVZR,          & 
& ZcplcFeFxecSscL,ZcplcFeFxecSscR,ZcplcFxvFxvhhL,ZcplcFxvFxvhhR,ZcplcFxeFxvcHpL,         & 
& ZcplcFxeFxvcHpR,ZcplcFxeFxvcVWpL,ZcplcFxeFxvcVWpR,ZcplhhHpcHp,ZcplhhHpcVWp,            & 
& ZcplhhSsccSsc,ZcplhhcHpVWp,ZcplhhcVWpVWp,ZcplhhVZVZ,AmpVertexFxeToFxehh)

 Else 
Call Amplitude_VERTEX_SDdiracDM_FxeToFxehh(MFeOS,MFxeOS,MFxvOS,MhhOS,MHpOS,           & 
& MSscOS,MVWpOS,MVZOS,MFe2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,MSsc2OS,MVWp2OS,              & 
& MVZ2OS,cplcFeFehhL,cplcFeFehhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFxeHpL,              & 
& cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,               & 
& cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxeFxvcHpL,              & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,    & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexFxeToFxehh)

 End if 
Else 


!Self-energy Corrections 


!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FxeToFxehh(MFe,MFxe,MFxv,Mhh,MHp,MSsc,MVWp,           & 
& MVZ,MFe2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFeFehhL,cplcFeFehhR,               & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR, & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,               & 
& cplcFxvFxvhhR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,           & 
& cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexFxeToFxehh)

End if 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fxe->Fxe hh -----------------------" 
End if 
If (OnlyTreeLevelContributions) Then 
 AmpSumFxeToFxehh = 0._dp 
 AmpSum2FxeToFxehh = 0._dp  
Else 
 AmpSumFxeToFxehh = AmpVertexFxeToFxehh + AmpWaveFxeToFxehh
 AmpSum2FxeToFxehh = AmpVertexFxeToFxehh + AmpWaveFxeToFxehh 
End If 
gt1=1 
i4 = isave 
    Do gt3=1,2
If (((OSkinematics).and.(MFxeOS.gt.(MFxeOS+MhhOS(gt3)))).or.((.not.OSkinematics).and.(MFxe.gt.(MFxe+Mhh(gt3))))) Then 
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFxeOS,MFxeOS,MhhOS(gt3),AmpSumFxeToFxehh(:,gt3),AmpSum2FxeToFxehh(:,gt3),AmpSqFxeToFxehh(gt3)) 
Else  
  Call SquareAmp_FtoFS(MFxe,MFxe,Mhh(gt3),AmpSumFxeToFxehh(:,gt3),AmpSum2FxeToFxehh(:,gt3),AmpSqFxeToFxehh(gt3)) 
End if  
Else  
  AmpSqFxeToFxehh(gt3) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFxeToFxehh(gt3).le.0._dp) Then 
  gP1LFxe(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFxe(gt1,i4) = 1._dp*GammaTPS(MFxeOS,MFxeOS,MhhOS(gt3),helfactor*AmpSqFxeToFxehh(gt3))
Else 
  gP1LFxe(gt1,i4) = 1._dp*GammaTPS(MFxe,MFxe,Mhh(gt3),helfactor*AmpSqFxeToFxehh(gt3))
End if 
If ((Abs(MRPFxeToFxehh(gt3)).gt.1.0E-20_dp).or.(Abs(MRGFxeToFxehh(gt3)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFxe(gt1,i4) 
End if 
i4=i4+1

    End do
isave = i4 
!---------------- 
! Fxe VP
!---------------- 

If (LoopInducedDecaysOS) Then 


!Self-energy Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_WAVE_SDdiracDM_FxeToFxeVP(ZcplcFxeFxeVPL,ZcplcFxeFxeVPR,               & 
& ZcplcFxeFxeVZL,ZcplcFxeFxeVZR,ctcplcFxeFxeVPL,ctcplcFxeFxeVPR,ctcplcFxeFxeVZL,         & 
& ctcplcFxeFxeVZR,MFxeOS,MFxe2OS,MVP,MVP2,Zfed,Zfeu,ZfVP,ZfVZVP,AmpWaveFxeToFxeVP)

 Else 
Call Amplitude_WAVE_SDdiracDM_FxeToFxeVP(cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,   & 
& cplcFxeFxeVZR,ctcplcFxeFxeVPL,ctcplcFxeFxeVPR,ctcplcFxeFxeVZL,ctcplcFxeFxeVZR,         & 
& MFxeOS,MFxe2OS,MVP,MVP2,Zfed,Zfeu,ZfVP,ZfVZVP,AmpWaveFxeToFxeVP)

 End if 


!Vertex Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_VERTEX_SDdiracDM_FxeToFxeVP(MFeOS,MFxeOS,MFxvOS,MHpOS,MSscOS,          & 
& MVP,MVWpOS,MVZOS,MFe2OS,MFxe2OS,MFxv2OS,MHp2OS,MSsc2OS,MVP2,MVWp2OS,MVZ2OS,            & 
& ZcplcFxeFeSscL,ZcplcFxeFeSscR,ZcplcFeFeVPL,ZcplcFeFeVPR,ZcplcFxvFxeHpL,ZcplcFxvFxeHpR, & 
& ZcplcFxeFxeVPL,ZcplcFxeFxeVPR,ZcplcFxvFxeVWpL,ZcplcFxvFxeVWpR,ZcplcFxeFxeVZL,          & 
& ZcplcFxeFxeVZR,ZcplcFeFxecSscL,ZcplcFeFxecSscR,ZcplcFxeFxvcHpL,ZcplcFxeFxvcHpR,        & 
& ZcplcFxeFxvcVWpL,ZcplcFxeFxvcVWpR,ZcplHpcHpVP,ZcplHpcVWpVP,ZcplcHpVPVWp,               & 
& ZcplcVWpVPVWp,AmpVertexFxeToFxeVP)

 Else 
Call Amplitude_VERTEX_SDdiracDM_FxeToFxeVP(MFeOS,MFxeOS,MFxvOS,MHpOS,MSscOS,          & 
& MVP,MVWpOS,MVZOS,MFe2OS,MFxe2OS,MFxv2OS,MHp2OS,MSsc2OS,MVP2,MVWp2OS,MVZ2OS,            & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFxvFxeHpL,cplcFxvFxeHpR,       & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,               & 
& cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,             & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,       & 
& AmpVertexFxeToFxeVP)

 End if 
Else 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FxeToFxeVP(cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,   & 
& cplcFxeFxeVZR,ctcplcFxeFxeVPL,ctcplcFxeFxeVPR,ctcplcFxeFxeVZL,ctcplcFxeFxeVZR,         & 
& MFxe,MFxe2,MVP,MVP2,Zfed,Zfeu,ZfVP,ZfVZVP,AmpWaveFxeToFxeVP)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FxeToFxeVP(MFe,MFxe,MFxv,MHp,MSsc,MVP,MVWp,           & 
& MVZ,MFe2,MFxe2,MFxv2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFxeFeSscL,cplcFxeFeSscR,           & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,       & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,              & 
& cplcFeFxecSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,          & 
& cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,AmpVertexFxeToFxeVP)

End if 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fxe->Fxe VP -----------------------" 
End if 
If (OnlyTreeLevelContributions) Then 
 AmpSumFxeToFxeVP = 0._dp 
 AmpSum2FxeToFxeVP = 0._dp  
Else 
 AmpSumFxeToFxeVP = AmpVertexFxeToFxeVP + AmpWaveFxeToFxeVP
 AmpSum2FxeToFxeVP = AmpVertexFxeToFxeVP + AmpWaveFxeToFxeVP 
End If 
gt1=1 
i4 = isave 
If (((OSkinematics).and.(MFxeOS.gt.(MFxeOS+0.))).or.((.not.OSkinematics).and.(MFxe.gt.(MFxe+MVP)))) Then 
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxeOS,MFxeOS,0._dp,AmpSumFxeToFxeVP(:),AmpSum2FxeToFxeVP(:),AmpSqFxeToFxeVP) 
Else  
  Call SquareAmp_FtoFV(MFxe,MFxe,MVP,AmpSumFxeToFxeVP(:),AmpSum2FxeToFxeVP(:),AmpSqFxeToFxeVP) 
End if  
Else  
  AmpSqFxeToFxeVP = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFxeToFxeVP.le.0._dp) Then 
  gP1LFxe(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFxe(gt1,i4) = 1._dp*GammaTPS(MFxeOS,MFxeOS,0._dp,helfactor*AmpSqFxeToFxeVP)
Else 
  gP1LFxe(gt1,i4) = 1._dp*GammaTPS(MFxe,MFxe,MVP,helfactor*AmpSqFxeToFxeVP)
End if 
If ((Abs(MRPFxeToFxeVP).gt.1.0E-20_dp).or.(Abs(MRGFxeToFxeVP).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFxe(gt1,i4) 
End if 
i4=i4+1

isave = i4 
End Subroutine OneLoopDecay_Fxe

End Module Wrapper_OneLoopDecay_Fxe_SDdiracDM
