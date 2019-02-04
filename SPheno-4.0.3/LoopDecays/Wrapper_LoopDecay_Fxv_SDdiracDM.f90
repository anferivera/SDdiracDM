! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:22 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module Wrapper_OneLoopDecay_Fxv_SDdiracDM
Use Model_Data_SDdiracDM 
Use Kinematics 
Use OneLoopDecay_Fxv_SDdiracDM 
Use Control 
Use Settings 

 
Contains

 
Subroutine OneLoopDecay_Fxv(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,MFeOS,             & 
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
& cplAhHpcVWp,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,      & 
& cplcFeFxecSscR,cplcFvFeHpL,cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFvFvVZL,          & 
& cplcFvFvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVPL,   & 
& cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,               & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,             & 
& cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvAhL,cplcFxvFxvAhR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcHpVPVWp,cplcHpVWpVZ,       & 
& cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,cplhhhhhh,cplhhHpcHp,               & 
& cplhhHpcVWp,cplhhSsccSsc,cplhhVZVZ,cplHpcHpVP,cplHpcHpVZ,cplHpcVWpVP,cplHpcVWpVZ,      & 
& ctcplcFxvFvSscL,ctcplcFxvFvSscR,ctcplcFxvFxeHpL,ctcplcFxvFxeHpR,ctcplcFxvFxeVWpL,      & 
& ctcplcFxvFxeVWpR,ctcplcFxvFxvAhL,ctcplcFxvFxvAhR,ctcplcFxvFxvhhL,ctcplcFxvFxvhhR,      & 
& ctcplcFxvFxvVZL,ctcplcFxvFxvVZR,GcplcFxvFxeHpL,GcplcFxvFxeHpR,GcplcHpVPVWp,            & 
& GcplHpcVWpVP,GosZcplcFxvFxeHpL,GosZcplcFxvFxeHpR,GosZcplcHpVPVWp,GosZcplHpcVWpVP,      & 
& GZcplcFxvFxeHpL,GZcplcFxvFxeHpR,GZcplcHpVPVWp,GZcplHpcVWpVP,ZcplcFxeFxeVPL,            & 
& ZcplcFxeFxeVPR,ZcplcFxeFxvcHpL,ZcplcFxeFxvcHpR,ZcplcFxeFxvcVWpL,ZcplcFxeFxvcVWpR,      & 
& ZcplcFxvFvSscL,ZcplcFxvFvSscR,ZcplcFxvFxeHpL,ZcplcFxvFxeHpR,ZcplcFxvFxeVWpL,           & 
& ZcplcFxvFxeVWpR,ZcplcFxvFxvAhL,ZcplcFxvFxvAhR,ZcplcFxvFxvhhL,ZcplcFxvFxvhhR,           & 
& ZcplcFxvFxvVZL,ZcplcFxvFxvVZR,ZcplcHpVPVWp,ZcplcVWpVPVWp,ZcplHpcHpVP,ZcplHpcVWpVP,     & 
& ZRUZH,ZRUVd,ZRUUd,ZRUVu,ZRUUu,ZRUVe,ZRUUe,ZRUVv,ZRUVvr,ZRUXV,ZRUXU,ZRUVSs,             & 
& MLambda,em,gs,deltaM,kont,gP1LFxv)

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

Complex(dp),Intent(in) :: cplAhAhhh(2),cplAhcHpVWp,cplAhhhVZ(2),cplAhHpcVWp,cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),& 
& cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),         & 
& cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFvFvVZL(3,3),& 
& cplcFvFvVZR(3,3),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),cplcFxeFeSscL(3,2),       & 
& cplcFxeFeSscR(3,2),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,            & 
& cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),             & 
& cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),           & 
& cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),             & 
& cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),       & 
& cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp(2),cplhhcVWpVWp(2),      & 
& cplhhhhhh(2,2,2),cplhhHpcHp(2),cplhhHpcVWp(2),cplhhSsccSsc(2,2,2),cplhhVZVZ(2),        & 
& cplHpcHpVP,cplHpcHpVZ,cplHpcVWpVP,cplHpcVWpVZ,ctcplcFxvFvSscL(2,3,2),ctcplcFxvFvSscR(2,3,2),& 
& ctcplcFxvFxeHpL(2),ctcplcFxvFxeHpR(2),ctcplcFxvFxeVWpL(2),ctcplcFxvFxeVWpR(2),         & 
& ctcplcFxvFxvAhL(2,2),ctcplcFxvFxvAhR(2,2),ctcplcFxvFxvhhL(2,2,2),ctcplcFxvFxvhhR(2,2,2),& 
& ctcplcFxvFxvVZL(2,2),ctcplcFxvFxvVZR(2,2),GcplcFxvFxeHpL(2),GcplcFxvFxeHpR(2),         & 
& GcplcHpVPVWp,GcplHpcVWpVP,GosZcplcFxvFxeHpL(2),GosZcplcFxvFxeHpR(2),GosZcplcHpVPVWp,   & 
& GosZcplHpcVWpVP,GZcplcFxvFxeHpL(2),GZcplcFxvFxeHpR(2),GZcplcHpVPVWp,GZcplHpcVWpVP,     & 
& ZcplcFxeFxeVPL,ZcplcFxeFxeVPR,ZcplcFxeFxvcHpL(2),ZcplcFxeFxvcHpR(2),ZcplcFxeFxvcVWpL(2),& 
& ZcplcFxeFxvcVWpR(2),ZcplcFxvFvSscL(2,3,2),ZcplcFxvFvSscR(2,3,2),ZcplcFxvFxeHpL(2),     & 
& ZcplcFxvFxeHpR(2),ZcplcFxvFxeVWpL(2),ZcplcFxvFxeVWpR(2),ZcplcFxvFxvAhL(2,2),           & 
& ZcplcFxvFxvAhR(2,2),ZcplcFxvFxvhhL(2,2,2),ZcplcFxvFxvhhR(2,2,2),ZcplcFxvFxvVZL(2,2),   & 
& ZcplcFxvFxvVZR(2,2),ZcplcHpVPVWp,ZcplcVWpVPVWp,ZcplHpcHpVP,ZcplHpcVWpVP

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
Real(dp), Intent(out) :: gP1LFxv(2,15) 
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
Real(dp) :: MRPFxvToFvSsc(2,3,2),MRGFxvToFvSsc(2,3,2), MRPZFxvToFvSsc(2,3,2),MRGZFxvToFvSsc(2,3,2) 
Real(dp) :: MVPFxvToFvSsc(2,3,2) 
Real(dp) :: RMsqTreeFxvToFvSsc(2,3,2),RMsqWaveFxvToFvSsc(2,3,2),RMsqVertexFxvToFvSsc(2,3,2) 
Complex(dp) :: AmpTreeFxvToFvSsc(2,2,3,2),AmpWaveFxvToFvSsc(2,2,3,2)=(0._dp,0._dp),AmpVertexFxvToFvSsc(2,2,3,2)& 
 & ,AmpVertexIRosFxvToFvSsc(2,2,3,2),AmpVertexIRdrFxvToFvSsc(2,2,3,2), AmpSumFxvToFvSsc(2,2,3,2), AmpSum2FxvToFvSsc(2,2,3,2) 
Complex(dp) :: AmpTreeZFxvToFvSsc(2,2,3,2),AmpWaveZFxvToFvSsc(2,2,3,2),AmpVertexZFxvToFvSsc(2,2,3,2) 
Real(dp) :: AmpSqFxvToFvSsc(2,3,2),  AmpSqTreeFxvToFvSsc(2,3,2) 
Real(dp) :: MRPFxvToFxeVWp(2),MRGFxvToFxeVWp(2), MRPZFxvToFxeVWp(2),MRGZFxvToFxeVWp(2) 
Real(dp) :: MVPFxvToFxeVWp(2) 
Real(dp) :: RMsqTreeFxvToFxeVWp(2),RMsqWaveFxvToFxeVWp(2),RMsqVertexFxvToFxeVWp(2) 
Complex(dp) :: AmpTreeFxvToFxeVWp(4,2),AmpWaveFxvToFxeVWp(4,2)=(0._dp,0._dp),AmpVertexFxvToFxeVWp(4,2)& 
 & ,AmpVertexIRosFxvToFxeVWp(4,2),AmpVertexIRdrFxvToFxeVWp(4,2), AmpSumFxvToFxeVWp(4,2), AmpSum2FxvToFxeVWp(4,2) 
Complex(dp) :: AmpTreeZFxvToFxeVWp(4,2),AmpWaveZFxvToFxeVWp(4,2),AmpVertexZFxvToFxeVWp(4,2) 
Real(dp) :: AmpSqFxvToFxeVWp(2),  AmpSqTreeFxvToFxeVWp(2) 
Real(dp) :: MRPFxvToFxvhh(2,2,2),MRGFxvToFxvhh(2,2,2), MRPZFxvToFxvhh(2,2,2),MRGZFxvToFxvhh(2,2,2) 
Real(dp) :: MVPFxvToFxvhh(2,2,2) 
Real(dp) :: RMsqTreeFxvToFxvhh(2,2,2),RMsqWaveFxvToFxvhh(2,2,2),RMsqVertexFxvToFxvhh(2,2,2) 
Complex(dp) :: AmpTreeFxvToFxvhh(2,2,2,2),AmpWaveFxvToFxvhh(2,2,2,2)=(0._dp,0._dp),AmpVertexFxvToFxvhh(2,2,2,2)& 
 & ,AmpVertexIRosFxvToFxvhh(2,2,2,2),AmpVertexIRdrFxvToFxvhh(2,2,2,2), AmpSumFxvToFxvhh(2,2,2,2), AmpSum2FxvToFxvhh(2,2,2,2) 
Complex(dp) :: AmpTreeZFxvToFxvhh(2,2,2,2),AmpWaveZFxvToFxvhh(2,2,2,2),AmpVertexZFxvToFxvhh(2,2,2,2) 
Real(dp) :: AmpSqFxvToFxvhh(2,2,2),  AmpSqTreeFxvToFxvhh(2,2,2) 
Real(dp) :: MRPFxvToFxvVZ(2,2),MRGFxvToFxvVZ(2,2), MRPZFxvToFxvVZ(2,2),MRGZFxvToFxvVZ(2,2) 
Real(dp) :: MVPFxvToFxvVZ(2,2) 
Real(dp) :: RMsqTreeFxvToFxvVZ(2,2),RMsqWaveFxvToFxvVZ(2,2),RMsqVertexFxvToFxvVZ(2,2) 
Complex(dp) :: AmpTreeFxvToFxvVZ(4,2,2),AmpWaveFxvToFxvVZ(4,2,2)=(0._dp,0._dp),AmpVertexFxvToFxvVZ(4,2,2)& 
 & ,AmpVertexIRosFxvToFxvVZ(4,2,2),AmpVertexIRdrFxvToFxvVZ(4,2,2), AmpSumFxvToFxvVZ(4,2,2), AmpSum2FxvToFxvVZ(4,2,2) 
Complex(dp) :: AmpTreeZFxvToFxvVZ(4,2,2),AmpWaveZFxvToFxvVZ(4,2,2),AmpVertexZFxvToFxvVZ(4,2,2) 
Real(dp) :: AmpSqFxvToFxvVZ(2,2),  AmpSqTreeFxvToFxvVZ(2,2) 
Real(dp) :: MRPFxvToFxvVP(2,2),MRGFxvToFxvVP(2,2), MRPZFxvToFxvVP(2,2),MRGZFxvToFxvVP(2,2) 
Real(dp) :: MVPFxvToFxvVP(2,2) 
Real(dp) :: RMsqTreeFxvToFxvVP(2,2),RMsqWaveFxvToFxvVP(2,2),RMsqVertexFxvToFxvVP(2,2) 
Complex(dp) :: AmpTreeFxvToFxvVP(4,2,2),AmpWaveFxvToFxvVP(4,2,2)=(0._dp,0._dp),AmpVertexFxvToFxvVP(4,2,2)& 
 & ,AmpVertexIRosFxvToFxvVP(4,2,2),AmpVertexIRdrFxvToFxvVP(4,2,2), AmpSumFxvToFxvVP(4,2,2), AmpSum2FxvToFxvVP(4,2,2) 
Complex(dp) :: AmpTreeZFxvToFxvVP(4,2,2),AmpWaveZFxvToFxvVP(4,2,2),AmpVertexZFxvToFxvVP(4,2,2) 
Real(dp) :: AmpSqFxvToFxvVP(2,2),  AmpSqTreeFxvToFxvVP(2,2) 
Write(*,*) "Calculating one-loop decays of Fxv " 
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
! Fv Ssc
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_FxvToFvSsc(cplcFxvFvSscL,cplcFxvFvSscR,MFv,             & 
& MFxv,MSsc,MFv2,MFxv2,MSsc2,AmpTreeFxvToFvSsc)

  Else 
Call Amplitude_Tree_SDdiracDM_FxvToFvSsc(ZcplcFxvFvSscL,ZcplcFxvFvSscR,               & 
& MFv,MFxv,MSsc,MFv2,MFxv2,MSsc2,AmpTreeFxvToFvSsc)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_FxvToFvSsc(MLambda,em,gs,cplcFxvFvSscL,cplcFxvFvSscR,       & 
& MFvOS,MFxvOS,MSscOS,MRPFxvToFvSsc,MRGFxvToFvSsc)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_FxvToFvSsc(MLambda,em,gs,ZcplcFxvFvSscL,ZcplcFxvFvSscR,     & 
& MFvOS,MFxvOS,MSscOS,MRPFxvToFvSsc,MRGFxvToFvSsc)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_FxvToFvSsc(MLambda,em,gs,cplcFxvFvSscL,cplcFxvFvSscR,       & 
& MFv,MFxv,MSsc,MRPFxvToFvSsc,MRGFxvToFvSsc)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_FxvToFvSsc(MLambda,em,gs,ZcplcFxvFvSscL,ZcplcFxvFvSscR,     & 
& MFv,MFxv,MSsc,MRPFxvToFvSsc,MRGFxvToFvSsc)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FxvToFvSsc(cplcFxvFvSscL,cplcFxvFvSscR,ctcplcFxvFvSscL, & 
& ctcplcFxvFvSscR,MFv,MFv2,MFxv,MFxv2,MSsc,MSsc2,ZfSsc,ZfVL,ZfVR,ZfxVL,ZfxVR,            & 
& AmpWaveFxvToFvSsc)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FxvToFvSsc(MFe,MFv,MFxe,MFxv,Mhh,MHp,MSsc,            & 
& MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFxeFeSscL,               & 
& cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,        & 
& cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,   & 
& cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhSsccSsc,   & 
& AmpVertexFxvToFvSsc)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFvSsc(MFe,MFv,MFxe,MFxv,Mhh,MHp,              & 
& MSsc,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFxeFeSscL,          & 
& cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,        & 
& cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,   & 
& cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhSsccSsc,   & 
& AmpVertexIRdrFxvToFvSsc)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFvSsc(MFeOS,MFvOS,MFxeOS,MFxvOS,              & 
& MhhOS,MHpOS,MSscOS,MVWpOS,MVZOS,MFe2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,           & 
& MSsc2OS,MVWp2OS,MVZ2OS,cplcFxeFeSscL,cplcFxeFeSscR,ZcplcFxvFvSscL,ZcplcFxvFvSscR,      & 
& cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,         & 
& cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvhhL,               & 
& cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhSsccSsc,AmpVertexIRosFxvToFvSsc)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFvSsc(MFe,MFv,MFxe,MFxv,Mhh,MHp,              & 
& MSsc,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFxeFeSscL,          & 
& cplcFxeFeSscR,ZcplcFxvFvSscL,ZcplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,      & 
& cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,   & 
& cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhSsccSsc,   & 
& AmpVertexIRosFxvToFvSsc)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFvSsc(MFeOS,MFvOS,MFxeOS,MFxvOS,              & 
& MhhOS,MHpOS,MSscOS,MVWpOS,MVZOS,MFe2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,           & 
& MSsc2OS,MVWp2OS,MVZ2OS,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,        & 
& cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,         & 
& cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvhhL,               & 
& cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhSsccSsc,AmpVertexIRosFxvToFvSsc)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFvSsc(MFe,MFv,MFxe,MFxv,Mhh,MHp,              & 
& MSsc,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFxeFeSscL,          & 
& cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,        & 
& cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,   & 
& cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhSsccSsc,   & 
& AmpVertexIRosFxvToFvSsc)

 End if 
 End if 
AmpVertexFxvToFvSsc = AmpVertexFxvToFvSsc -  AmpVertexIRdrFxvToFvSsc! +  AmpVertexIRosFxvToFvSsc ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZFxvToFvSsc=0._dp 
AmpVertexZFxvToFvSsc=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZFxvToFvSsc(1,gt2,:,:) = AmpWaveZFxvToFvSsc(1,gt2,:,:)+ZRUXU(gt2,gt1)*AmpWaveFxvToFvSsc(1,gt1,:,:) 
AmpVertexZFxvToFvSsc(1,gt2,:,:)= AmpVertexZFxvToFvSsc(1,gt2,:,:) + ZRUXU(gt2,gt1)*AmpVertexFxvToFvSsc(1,gt1,:,:) 
AmpWaveZFxvToFvSsc(2,gt2,:,:) = AmpWaveZFxvToFvSsc(2,gt2,:,:)+ZRUXVc(gt2,gt1)*AmpWaveFxvToFvSsc(2,gt1,:,:) 
AmpVertexZFxvToFvSsc(2,gt2,:,:)= AmpVertexZFxvToFvSsc(2,gt2,:,:) + ZRUXVc(gt2,gt1)*AmpVertexFxvToFvSsc(2,gt1,:,:) 
 End Do 
End Do 
AmpWaveFxvToFvSsc=AmpWaveZFxvToFvSsc 
AmpVertexFxvToFvSsc= AmpVertexZFxvToFvSsc
! Final State 1 
AmpWaveZFxvToFvSsc=0._dp 
AmpVertexZFxvToFvSsc=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFxvToFvSsc(1,:,gt2,:) = AmpWaveZFxvToFvSsc(1,:,gt2,:)+ZRUVv(gt2,gt1)*AmpWaveFxvToFvSsc(1,:,gt1,:) 
AmpVertexZFxvToFvSsc(1,:,gt2,:)= AmpVertexZFxvToFvSsc(1,:,gt2,:)+ZRUVv(gt2,gt1)*AmpVertexFxvToFvSsc(1,:,gt1,:) 
AmpWaveZFxvToFvSsc(2,:,gt2,:) = AmpWaveZFxvToFvSsc(2,:,gt2,:)+ZRUVvrc(gt2,gt1)*AmpWaveFxvToFvSsc(2,:,gt1,:) 
AmpVertexZFxvToFvSsc(2,:,gt2,:)= AmpVertexZFxvToFvSsc(2,:,gt2,:)+ZRUVvrc(gt2,gt1)*AmpVertexFxvToFvSsc(2,:,gt1,:) 
 End Do 
End Do 
AmpWaveFxvToFvSsc=AmpWaveZFxvToFvSsc 
AmpVertexFxvToFvSsc= AmpVertexZFxvToFvSsc
! Final State 2 
AmpWaveZFxvToFvSsc=0._dp 
AmpVertexZFxvToFvSsc=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZFxvToFvSsc(:,:,:,gt2) = AmpWaveZFxvToFvSsc(:,:,:,gt2)+ZRUVSs(gt2,gt1)*AmpWaveFxvToFvSsc(:,:,:,gt1) 
AmpVertexZFxvToFvSsc(:,:,:,gt2)= AmpVertexZFxvToFvSsc(:,:,:,gt2)+ZRUVSs(gt2,gt1)*AmpVertexFxvToFvSsc(:,:,:,gt1) 
 End Do 
End Do 
AmpWaveFxvToFvSsc=AmpWaveZFxvToFvSsc 
AmpVertexFxvToFvSsc= AmpVertexZFxvToFvSsc
End if
If (ShiftIRdiv) Then 
AmpVertexFxvToFvSsc = AmpVertexFxvToFvSsc  +  AmpVertexIRosFxvToFvSsc
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fxv->Fv Ssc -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumFxvToFvSsc = AmpTreeFxvToFvSsc 
 AmpSum2FxvToFvSsc = AmpTreeFxvToFvSsc + 2._dp*AmpWaveFxvToFvSsc + 2._dp*AmpVertexFxvToFvSsc  
Else 
 AmpSumFxvToFvSsc = AmpTreeFxvToFvSsc + AmpWaveFxvToFvSsc + AmpVertexFxvToFvSsc
 AmpSum2FxvToFvSsc = AmpTreeFxvToFvSsc + AmpWaveFxvToFvSsc + AmpVertexFxvToFvSsc 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumFxvToFvSsc = AmpTreeFxvToFvSsc
 AmpSum2FxvToFvSsc = AmpTreeFxvToFvSsc 
End if 
Do gt1=1,2
i4 = isave 
  Do gt2=1,3
    Do gt3=1,2
If (((OSkinematics).and.(MFxvOS(gt1).gt.(MFvOS(gt2)+MSscOS(gt3)))).or.((.not.OSkinematics).and.(MFxv(gt1).gt.(MFv(gt2)+MSsc(gt3))))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2, gt3 
  AmpSum2FxvToFvSsc = AmpTreeFxvToFvSsc
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFxvOS(gt1),MFvOS(gt2),MSscOS(gt3),AmpSumFxvToFvSsc(:,gt1, gt2, gt3),AmpSum2FxvToFvSsc(:,gt1, gt2, gt3),AmpSqFxvToFvSsc(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFxv(gt1),MFv(gt2),MSsc(gt3),AmpSumFxvToFvSsc(:,gt1, gt2, gt3),AmpSum2FxvToFvSsc(:,gt1, gt2, gt3),AmpSqFxvToFvSsc(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqFxvToFvSsc(gt1, gt2, gt3) 
  AmpSum2FxvToFvSsc = 2._dp*AmpWaveFxvToFvSsc
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFxvOS(gt1),MFvOS(gt2),MSscOS(gt3),AmpSumFxvToFvSsc(:,gt1, gt2, gt3),AmpSum2FxvToFvSsc(:,gt1, gt2, gt3),AmpSqFxvToFvSsc(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFxv(gt1),MFv(gt2),MSsc(gt3),AmpSumFxvToFvSsc(:,gt1, gt2, gt3),AmpSum2FxvToFvSsc(:,gt1, gt2, gt3),AmpSqFxvToFvSsc(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqFxvToFvSsc(gt1, gt2, gt3) 
  AmpSum2FxvToFvSsc = 2._dp*AmpVertexFxvToFvSsc
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFxvOS(gt1),MFvOS(gt2),MSscOS(gt3),AmpSumFxvToFvSsc(:,gt1, gt2, gt3),AmpSum2FxvToFvSsc(:,gt1, gt2, gt3),AmpSqFxvToFvSsc(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFxv(gt1),MFv(gt2),MSsc(gt3),AmpSumFxvToFvSsc(:,gt1, gt2, gt3),AmpSum2FxvToFvSsc(:,gt1, gt2, gt3),AmpSqFxvToFvSsc(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqFxvToFvSsc(gt1, gt2, gt3) 
  AmpSum2FxvToFvSsc = AmpTreeFxvToFvSsc + 2._dp*AmpWaveFxvToFvSsc + 2._dp*AmpVertexFxvToFvSsc
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFxvOS(gt1),MFvOS(gt2),MSscOS(gt3),AmpSumFxvToFvSsc(:,gt1, gt2, gt3),AmpSum2FxvToFvSsc(:,gt1, gt2, gt3),AmpSqFxvToFvSsc(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFxv(gt1),MFv(gt2),MSsc(gt3),AmpSumFxvToFvSsc(:,gt1, gt2, gt3),AmpSum2FxvToFvSsc(:,gt1, gt2, gt3),AmpSqFxvToFvSsc(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqFxvToFvSsc(gt1, gt2, gt3) 
 End if 
If (OSkinematics) Then 
  AmpSum2FxvToFvSsc = AmpTreeFxvToFvSsc
  Call SquareAmp_FtoFS(MFxvOS(gt1),MFvOS(gt2),MSscOS(gt3),AmpSumFxvToFvSsc(:,gt1, gt2, gt3),AmpSum2FxvToFvSsc(:,gt1, gt2, gt3),AmpSqFxvToFvSsc(gt1, gt2, gt3)) 
  AmpSqTreeFxvToFvSsc(gt1, gt2, gt3) = AmpSqFxvToFvSsc(gt1, gt2, gt3)  
  AmpSum2FxvToFvSsc = + 2._dp*AmpWaveFxvToFvSsc + 2._dp*AmpVertexFxvToFvSsc
  Call SquareAmp_FtoFS(MFxvOS(gt1),MFvOS(gt2),MSscOS(gt3),AmpSumFxvToFvSsc(:,gt1, gt2, gt3),AmpSum2FxvToFvSsc(:,gt1, gt2, gt3),AmpSqFxvToFvSsc(gt1, gt2, gt3)) 
  AmpSqFxvToFvSsc(gt1, gt2, gt3) = AmpSqFxvToFvSsc(gt1, gt2, gt3) + AmpSqTreeFxvToFvSsc(gt1, gt2, gt3)  
Else  
  AmpSum2FxvToFvSsc = AmpTreeFxvToFvSsc
  Call SquareAmp_FtoFS(MFxv(gt1),MFv(gt2),MSsc(gt3),AmpSumFxvToFvSsc(:,gt1, gt2, gt3),AmpSum2FxvToFvSsc(:,gt1, gt2, gt3),AmpSqFxvToFvSsc(gt1, gt2, gt3)) 
  AmpSqTreeFxvToFvSsc(gt1, gt2, gt3) = AmpSqFxvToFvSsc(gt1, gt2, gt3)  
  AmpSum2FxvToFvSsc = + 2._dp*AmpWaveFxvToFvSsc + 2._dp*AmpVertexFxvToFvSsc
  Call SquareAmp_FtoFS(MFxv(gt1),MFv(gt2),MSsc(gt3),AmpSumFxvToFvSsc(:,gt1, gt2, gt3),AmpSum2FxvToFvSsc(:,gt1, gt2, gt3),AmpSqFxvToFvSsc(gt1, gt2, gt3)) 
  AmpSqFxvToFvSsc(gt1, gt2, gt3) = AmpSqFxvToFvSsc(gt1, gt2, gt3) + AmpSqTreeFxvToFvSsc(gt1, gt2, gt3)  
End if  
Else  
  AmpSqFxvToFvSsc(gt1, gt2, gt3) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFxvToFvSsc(gt1, gt2, gt3).le.0._dp) Then 
  gP1LFxv(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFxv(gt1,i4) = 1._dp*GammaTPS(MFxvOS(gt1),MFvOS(gt2),MSscOS(gt3),helfactor*AmpSqFxvToFvSsc(gt1, gt2, gt3))
Else 
  gP1LFxv(gt1,i4) = 1._dp*GammaTPS(MFxv(gt1),MFv(gt2),MSsc(gt3),helfactor*AmpSqFxvToFvSsc(gt1, gt2, gt3))
End if 
If ((Abs(MRPFxvToFvSsc(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGFxvToFvSsc(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFxv(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPFxvToFvSsc(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGFxvToFvSsc(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPFxvToFvSsc(gt1, gt2, gt3) + MRGFxvToFvSsc(gt1, gt2, gt3)) 
  gP1LFxv(gt1,i4) = gP1LFxv(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPFxvToFvSsc(gt1, gt2, gt3) + MRGFxvToFvSsc(gt1, gt2, gt3))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LFxv(gt1,i4) 
  End if 
End if 
End if 
i4=i4+1

    End do
  End do
If (gt1.eq.2) isave = i4 
End do
End If 
If (.not.CalcLoopDecay_LoopInducedOnly) Then 
!---------------- 
! Fxe VWp
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_FxvToFxeVWp(cplcFxvFxeVWpL,cplcFxvFxeVWpR,              & 
& MFxe,MFxv,MVWp,MFxe2,MFxv2,MVWp2,AmpTreeFxvToFxeVWp)

  Else 
Call Amplitude_Tree_SDdiracDM_FxvToFxeVWp(ZcplcFxvFxeVWpL,ZcplcFxvFxeVWpR,            & 
& MFxe,MFxv,MVWp,MFxe2,MFxv2,MVWp2,AmpTreeFxvToFxeVWp)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_FxvToFxeVWp(MLambda,em,gs,cplcFxvFxeVWpL,cplcFxvFxeVWpR,    & 
& MFxeOS,MFxvOS,MVWpOS,MRPFxvToFxeVWp,MRGFxvToFxeVWp)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_FxvToFxeVWp(MLambda,em,gs,ZcplcFxvFxeVWpL,ZcplcFxvFxeVWpR,  & 
& MFxeOS,MFxvOS,MVWpOS,MRPFxvToFxeVWp,MRGFxvToFxeVWp)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_FxvToFxeVWp(MLambda,em,gs,cplcFxvFxeVWpL,cplcFxvFxeVWpR,    & 
& MFxe,MFxv,MVWp,MRPFxvToFxeVWp,MRGFxvToFxeVWp)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_FxvToFxeVWp(MLambda,em,gs,ZcplcFxvFxeVWpL,ZcplcFxvFxeVWpR,  & 
& MFxe,MFxv,MVWp,MRPFxvToFxeVWp,MRGFxvToFxeVWp)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FxvToFxeVWp(cplcFxvFxeVWpL,cplcFxvFxeVWpR,              & 
& ctcplcFxvFxeVWpL,ctcplcFxvFxeVWpR,MFxe,MFxe2,MFxv,MFxv2,MVWp,MVWp2,Zfed,               & 
& Zfeu,ZfVWp,ZfxVL,ZfxVR,AmpWaveFxvToFxeVWp)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FxvToFxeVWp(MAh,MFe,MFv,MFxe,MFxv,Mhh,MHp,            & 
& MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,               & 
& MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhcHpVWp,cplcFvFeVWpL,cplcFvFeVWpR,cplcFxvFvSscL,  & 
& cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,  & 
& cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,              & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhcHpVWp,cplhhcVWpVWp,      & 
& cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexFxvToFxeVWp)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFxeVWp(MAh,MFe,MFv,MFxe,MFxv,Mhh,             & 
& MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,           & 
& MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhcHpVWp,cplcFvFeVWpL,cplcFvFeVWpR,cplcFxvFvSscL,  & 
& cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,  & 
& cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,              & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhcHpVWp,cplhhcVWpVWp,      & 
& cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRdrFxvToFxeVWp)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFxeVWp(MAhOS,MFeOS,MFvOS,MFxeOS,              & 
& MFxvOS,MhhOS,MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,               & 
& MFxv2OS,Mhh2OS,MHp2OS,MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplcFxvFxvAhL,cplcFxvFxvAhR,         & 
& cplAhcHpVWp,cplcFvFeVWpL,cplcFvFeVWpR,cplcFxvFvSscL,cplcFxvFvSscR,GosZcplcFxvFxeHpL,   & 
& GosZcplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,ZcplcFxvFxeVWpL,ZcplcFxvFxeVWpR,         & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,               & 
& cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhcHpVWp,cplhhcVWpVWp,GosZcplcHpVPVWp,    & 
& cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFxvToFxeVWp)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFxeVWp(MAh,MFe,MFv,MFxe,MFxv,Mhh,             & 
& MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,           & 
& MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhcHpVWp,cplcFvFeVWpL,cplcFvFeVWpR,cplcFxvFvSscL,  & 
& cplcFxvFvSscR,GZcplcFxvFxeHpL,GZcplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,             & 
& ZcplcFxvFxeVWpL,ZcplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,            & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhcHpVWp,    & 
& cplhhcVWpVWp,GZcplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFxvToFxeVWp)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFxeVWp(MAhOS,MFeOS,MFvOS,MFxeOS,              & 
& MFxvOS,MhhOS,MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,               & 
& MFxv2OS,Mhh2OS,MHp2OS,MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplcFxvFxvAhL,cplcFxvFxvAhR,         & 
& cplAhcHpVWp,cplcFvFeVWpL,cplcFvFeVWpR,cplcFxvFvSscL,cplcFxvFvSscR,GcplcFxvFxeHpL,      & 
& GcplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,              & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,               & 
& cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhcHpVWp,cplhhcVWpVWp,GcplcHpVPVWp,       & 
& cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFxvToFxeVWp)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFxeVWp(MAh,MFe,MFv,MFxe,MFxv,Mhh,             & 
& MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,           & 
& MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhcHpVWp,cplcFvFeVWpL,cplcFvFeVWpR,cplcFxvFvSscL,  & 
& cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,  & 
& cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,              & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhcHpVWp,cplhhcVWpVWp,      & 
& cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFxvToFxeVWp)

 End if 
 End if 
AmpVertexFxvToFxeVWp = AmpVertexFxvToFxeVWp -  AmpVertexIRdrFxvToFxeVWp! +  AmpVertexIRosFxvToFxeVWp ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZFxvToFxeVWp=0._dp 
AmpVertexZFxvToFxeVWp=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZFxvToFxeVWp(1,gt2) = AmpWaveZFxvToFxeVWp(1,gt2)+ZRUXVc(gt2,gt1)*AmpWaveFxvToFxeVWp(1,gt1) 
AmpVertexZFxvToFxeVWp(1,gt2)= AmpVertexZFxvToFxeVWp(1,gt2) + ZRUXVc(gt2,gt1)*AmpVertexFxvToFxeVWp(1,gt1) 
AmpWaveZFxvToFxeVWp(2,gt2) = AmpWaveZFxvToFxeVWp(2,gt2)+ZRUXU(gt2,gt1)*AmpWaveFxvToFxeVWp(2,gt1) 
AmpVertexZFxvToFxeVWp(2,gt2)= AmpVertexZFxvToFxeVWp(2,gt2) + ZRUXU(gt2,gt1)*AmpVertexFxvToFxeVWp(2,gt1) 
AmpWaveZFxvToFxeVWp(3,gt2) = AmpWaveZFxvToFxeVWp(3,gt2)+ZRUXVc(gt2,gt1)*AmpWaveFxvToFxeVWp(3,gt1) 
AmpVertexZFxvToFxeVWp(3,gt2)= AmpVertexZFxvToFxeVWp(3,gt2) + ZRUXVc(gt2,gt1)*AmpVertexFxvToFxeVWp(3,gt1) 
AmpWaveZFxvToFxeVWp(4,gt2) = AmpWaveZFxvToFxeVWp(4,gt2)+ZRUXU(gt2,gt1)*AmpWaveFxvToFxeVWp(4,gt1) 
AmpVertexZFxvToFxeVWp(4,gt2)= AmpVertexZFxvToFxeVWp(4,gt2) + ZRUXU(gt2,gt1)*AmpVertexFxvToFxeVWp(4,gt1) 
 End Do 
End Do 
AmpWaveFxvToFxeVWp=AmpWaveZFxvToFxeVWp 
AmpVertexFxvToFxeVWp= AmpVertexZFxvToFxeVWp
End if
If (ShiftIRdiv) Then 
AmpVertexFxvToFxeVWp = AmpVertexFxvToFxeVWp  +  AmpVertexIRosFxvToFxeVWp
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fxv->Fxe VWp -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumFxvToFxeVWp = AmpTreeFxvToFxeVWp 
 AmpSum2FxvToFxeVWp = AmpTreeFxvToFxeVWp + 2._dp*AmpWaveFxvToFxeVWp + 2._dp*AmpVertexFxvToFxeVWp  
Else 
 AmpSumFxvToFxeVWp = AmpTreeFxvToFxeVWp + AmpWaveFxvToFxeVWp + AmpVertexFxvToFxeVWp
 AmpSum2FxvToFxeVWp = AmpTreeFxvToFxeVWp + AmpWaveFxvToFxeVWp + AmpVertexFxvToFxeVWp 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumFxvToFxeVWp = AmpTreeFxvToFxeVWp
 AmpSum2FxvToFxeVWp = AmpTreeFxvToFxeVWp 
End if 
Do gt1=1,2
i4 = isave 
If (((OSkinematics).and.(MFxvOS(gt1).gt.(MFxeOS+MVWpOS))).or.((.not.OSkinematics).and.(MFxv(gt1).gt.(MFxe+MVWp)))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1 
  AmpSum2FxvToFxeVWp = AmpTreeFxvToFxeVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxvOS(gt1),MFxeOS,MVWpOS,AmpSumFxvToFxeVWp(:,gt1),AmpSum2FxvToFxeVWp(:,gt1),AmpSqFxvToFxeVWp(gt1)) 
Else  
  Call SquareAmp_FtoFV(MFxv(gt1),MFxe,MVWp,AmpSumFxvToFxeVWp(:,gt1),AmpSum2FxvToFxeVWp(:,gt1),AmpSqFxvToFxeVWp(gt1)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqFxvToFxeVWp(gt1) 
  AmpSum2FxvToFxeVWp = 2._dp*AmpWaveFxvToFxeVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxvOS(gt1),MFxeOS,MVWpOS,AmpSumFxvToFxeVWp(:,gt1),AmpSum2FxvToFxeVWp(:,gt1),AmpSqFxvToFxeVWp(gt1)) 
Else  
  Call SquareAmp_FtoFV(MFxv(gt1),MFxe,MVWp,AmpSumFxvToFxeVWp(:,gt1),AmpSum2FxvToFxeVWp(:,gt1),AmpSqFxvToFxeVWp(gt1)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqFxvToFxeVWp(gt1) 
  AmpSum2FxvToFxeVWp = 2._dp*AmpVertexFxvToFxeVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxvOS(gt1),MFxeOS,MVWpOS,AmpSumFxvToFxeVWp(:,gt1),AmpSum2FxvToFxeVWp(:,gt1),AmpSqFxvToFxeVWp(gt1)) 
Else  
  Call SquareAmp_FtoFV(MFxv(gt1),MFxe,MVWp,AmpSumFxvToFxeVWp(:,gt1),AmpSum2FxvToFxeVWp(:,gt1),AmpSqFxvToFxeVWp(gt1)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqFxvToFxeVWp(gt1) 
  AmpSum2FxvToFxeVWp = AmpTreeFxvToFxeVWp + 2._dp*AmpWaveFxvToFxeVWp + 2._dp*AmpVertexFxvToFxeVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxvOS(gt1),MFxeOS,MVWpOS,AmpSumFxvToFxeVWp(:,gt1),AmpSum2FxvToFxeVWp(:,gt1),AmpSqFxvToFxeVWp(gt1)) 
Else  
  Call SquareAmp_FtoFV(MFxv(gt1),MFxe,MVWp,AmpSumFxvToFxeVWp(:,gt1),AmpSum2FxvToFxeVWp(:,gt1),AmpSqFxvToFxeVWp(gt1)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqFxvToFxeVWp(gt1) 
 End if 
If (OSkinematics) Then 
  AmpSum2FxvToFxeVWp = AmpTreeFxvToFxeVWp
  Call SquareAmp_FtoFV(MFxvOS(gt1),MFxeOS,MVWpOS,AmpSumFxvToFxeVWp(:,gt1),AmpSum2FxvToFxeVWp(:,gt1),AmpSqFxvToFxeVWp(gt1)) 
  AmpSqTreeFxvToFxeVWp(gt1) = AmpSqFxvToFxeVWp(gt1)  
  AmpSum2FxvToFxeVWp = + 2._dp*AmpWaveFxvToFxeVWp + 2._dp*AmpVertexFxvToFxeVWp
  Call SquareAmp_FtoFV(MFxvOS(gt1),MFxeOS,MVWpOS,AmpSumFxvToFxeVWp(:,gt1),AmpSum2FxvToFxeVWp(:,gt1),AmpSqFxvToFxeVWp(gt1)) 
  AmpSqFxvToFxeVWp(gt1) = AmpSqFxvToFxeVWp(gt1) + AmpSqTreeFxvToFxeVWp(gt1)  
Else  
  AmpSum2FxvToFxeVWp = AmpTreeFxvToFxeVWp
  Call SquareAmp_FtoFV(MFxv(gt1),MFxe,MVWp,AmpSumFxvToFxeVWp(:,gt1),AmpSum2FxvToFxeVWp(:,gt1),AmpSqFxvToFxeVWp(gt1)) 
  AmpSqTreeFxvToFxeVWp(gt1) = AmpSqFxvToFxeVWp(gt1)  
  AmpSum2FxvToFxeVWp = + 2._dp*AmpWaveFxvToFxeVWp + 2._dp*AmpVertexFxvToFxeVWp
  Call SquareAmp_FtoFV(MFxv(gt1),MFxe,MVWp,AmpSumFxvToFxeVWp(:,gt1),AmpSum2FxvToFxeVWp(:,gt1),AmpSqFxvToFxeVWp(gt1)) 
  AmpSqFxvToFxeVWp(gt1) = AmpSqFxvToFxeVWp(gt1) + AmpSqTreeFxvToFxeVWp(gt1)  
End if  
Else  
  AmpSqFxvToFxeVWp(gt1) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFxvToFxeVWp(gt1).le.0._dp) Then 
  gP1LFxv(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFxv(gt1,i4) = 1._dp*GammaTPS(MFxvOS(gt1),MFxeOS,MVWpOS,helfactor*AmpSqFxvToFxeVWp(gt1))
Else 
  gP1LFxv(gt1,i4) = 1._dp*GammaTPS(MFxv(gt1),MFxe,MVWp,helfactor*AmpSqFxvToFxeVWp(gt1))
End if 
If ((Abs(MRPFxvToFxeVWp(gt1)).gt.1.0E-20_dp).or.(Abs(MRGFxvToFxeVWp(gt1)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFxv(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPFxvToFxeVWp(gt1)).gt.1.0E-20_dp).or.(Abs(MRGFxvToFxeVWp(gt1)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPFxvToFxeVWp(gt1) + MRGFxvToFxeVWp(gt1)) 
  gP1LFxv(gt1,i4) = gP1LFxv(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPFxvToFxeVWp(gt1) + MRGFxvToFxeVWp(gt1))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LFxv(gt1,i4) 
  End if 
End if 
End if 
i4=i4+1

If (gt1.eq.2) isave = i4 
End do
End If 
If (.not.CalcLoopDecay_LoopInducedOnly) Then 
!---------------- 
! Fxv hh
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_FxvToFxvhh(cplcFxvFxvhhL,cplcFxvFxvhhR,MFxv,            & 
& Mhh,MFxv2,Mhh2,AmpTreeFxvToFxvhh)

  Else 
Call Amplitude_Tree_SDdiracDM_FxvToFxvhh(ZcplcFxvFxvhhL,ZcplcFxvFxvhhR,               & 
& MFxv,Mhh,MFxv2,Mhh2,AmpTreeFxvToFxvhh)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_FxvToFxvhh(MLambda,em,gs,cplcFxvFxvhhL,cplcFxvFxvhhR,       & 
& MFxvOS,MhhOS,MRPFxvToFxvhh,MRGFxvToFxvhh)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_FxvToFxvhh(MLambda,em,gs,ZcplcFxvFxvhhL,ZcplcFxvFxvhhR,     & 
& MFxvOS,MhhOS,MRPFxvToFxvhh,MRGFxvToFxvhh)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_FxvToFxvhh(MLambda,em,gs,cplcFxvFxvhhL,cplcFxvFxvhhR,       & 
& MFxv,Mhh,MRPFxvToFxvhh,MRGFxvToFxvhh)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_FxvToFxvhh(MLambda,em,gs,ZcplcFxvFxvhhL,ZcplcFxvFxvhhR,     & 
& MFxv,Mhh,MRPFxvToFxvhh,MRGFxvToFxvhh)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FxvToFxvhh(cplcFxvFxvhhL,cplcFxvFxvhhR,ctcplcFxvFxvhhL, & 
& ctcplcFxvFxvhhR,MFxv,MFxv2,Mhh,Mhh2,Zfhh,ZfxVL,ZfxVR,AmpWaveFxvToFxvhh)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FxvToFxvhh(MAh,MFv,MFxe,MFxv,Mhh,MHp,MSsc,            & 
& MVWp,MVZ,MAh2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplAhAhhh,cplcFxvFxvAhL,     & 
& cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,       & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,               & 
& cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,             & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,         & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexFxvToFxvhh)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvhh(MAh,MFv,MFxe,MFxv,Mhh,MHp,              & 
& MSsc,MVWp,MVZ,MAh2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplAhAhhh,              & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,       & 
& cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,               & 
& cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,              & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,       & 
& cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRdrFxvToFxvhh)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvhh(MAhOS,MFvOS,MFxeOS,MFxvOS,              & 
& MhhOS,MHpOS,MSscOS,MVWpOS,MVZOS,MAh2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,           & 
& MSsc2OS,MVWp2OS,MVZ2OS,cplAhAhhh,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,  & 
& cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,               & 
& ZcplcFxvFxvhhL,ZcplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,              & 
& cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,          & 
& cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,      & 
& AmpVertexIRosFxvToFxvhh)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvhh(MAh,MFv,MFxe,MFxv,Mhh,MHp,              & 
& MSsc,MVWp,MVZ,MAh2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplAhAhhh,              & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,       & 
& cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,ZcplcFxvFxvhhL,ZcplcFxvFxvhhR,             & 
& cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,              & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,       & 
& cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRosFxvToFxvhh)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvhh(MAhOS,MFvOS,MFxeOS,MFxvOS,              & 
& MhhOS,MHpOS,MSscOS,MVWpOS,MVZOS,MAh2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,           & 
& MSsc2OS,MVWp2OS,MVZ2OS,cplAhAhhh,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,  & 
& cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR, & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhhhhh,               & 
& cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRosFxvToFxvhh)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvhh(MAh,MFv,MFxe,MFxv,Mhh,MHp,              & 
& MSsc,MVWp,MVZ,MAh2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplAhAhhh,              & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,       & 
& cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,               & 
& cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,              & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,       & 
& cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRosFxvToFxvhh)

 End if 
 End if 
AmpVertexFxvToFxvhh = AmpVertexFxvToFxvhh -  AmpVertexIRdrFxvToFxvhh! +  AmpVertexIRosFxvToFxvhh ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZFxvToFxvhh=0._dp 
AmpVertexZFxvToFxvhh=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZFxvToFxvhh(1,gt2,:,:) = AmpWaveZFxvToFxvhh(1,gt2,:,:)+ZRUXU(gt2,gt1)*AmpWaveFxvToFxvhh(1,gt1,:,:) 
AmpVertexZFxvToFxvhh(1,gt2,:,:)= AmpVertexZFxvToFxvhh(1,gt2,:,:) + ZRUXU(gt2,gt1)*AmpVertexFxvToFxvhh(1,gt1,:,:) 
AmpWaveZFxvToFxvhh(2,gt2,:,:) = AmpWaveZFxvToFxvhh(2,gt2,:,:)+ZRUXVc(gt2,gt1)*AmpWaveFxvToFxvhh(2,gt1,:,:) 
AmpVertexZFxvToFxvhh(2,gt2,:,:)= AmpVertexZFxvToFxvhh(2,gt2,:,:) + ZRUXVc(gt2,gt1)*AmpVertexFxvToFxvhh(2,gt1,:,:) 
 End Do 
End Do 
AmpWaveFxvToFxvhh=AmpWaveZFxvToFxvhh 
AmpVertexFxvToFxvhh= AmpVertexZFxvToFxvhh
! Final State 1 
AmpWaveZFxvToFxvhh=0._dp 
AmpVertexZFxvToFxvhh=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZFxvToFxvhh(1,:,gt2,:) = AmpWaveZFxvToFxvhh(1,:,gt2,:)+ZRUXV(gt2,gt1)*AmpWaveFxvToFxvhh(1,:,gt1,:) 
AmpVertexZFxvToFxvhh(1,:,gt2,:)= AmpVertexZFxvToFxvhh(1,:,gt2,:)+ZRUXV(gt2,gt1)*AmpVertexFxvToFxvhh(1,:,gt1,:) 
AmpWaveZFxvToFxvhh(2,:,gt2,:) = AmpWaveZFxvToFxvhh(2,:,gt2,:)+ZRUXUc(gt2,gt1)*AmpWaveFxvToFxvhh(2,:,gt1,:) 
AmpVertexZFxvToFxvhh(2,:,gt2,:)= AmpVertexZFxvToFxvhh(2,:,gt2,:)+ZRUXUc(gt2,gt1)*AmpVertexFxvToFxvhh(2,:,gt1,:) 
 End Do 
End Do 
AmpWaveFxvToFxvhh=AmpWaveZFxvToFxvhh 
AmpVertexFxvToFxvhh= AmpVertexZFxvToFxvhh
! Final State 2 
AmpWaveZFxvToFxvhh=0._dp 
AmpVertexZFxvToFxvhh=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZFxvToFxvhh(:,:,:,gt2) = AmpWaveZFxvToFxvhh(:,:,:,gt2)+ZRUZH(gt2,gt1)*AmpWaveFxvToFxvhh(:,:,:,gt1) 
AmpVertexZFxvToFxvhh(:,:,:,gt2)= AmpVertexZFxvToFxvhh(:,:,:,gt2)+ZRUZH(gt2,gt1)*AmpVertexFxvToFxvhh(:,:,:,gt1) 
 End Do 
End Do 
AmpWaveFxvToFxvhh=AmpWaveZFxvToFxvhh 
AmpVertexFxvToFxvhh= AmpVertexZFxvToFxvhh
End if
If (ShiftIRdiv) Then 
AmpVertexFxvToFxvhh = AmpVertexFxvToFxvhh  +  AmpVertexIRosFxvToFxvhh
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fxv->Fxv hh -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumFxvToFxvhh = AmpTreeFxvToFxvhh 
 AmpSum2FxvToFxvhh = AmpTreeFxvToFxvhh + 2._dp*AmpWaveFxvToFxvhh + 2._dp*AmpVertexFxvToFxvhh  
Else 
 AmpSumFxvToFxvhh = AmpTreeFxvToFxvhh + AmpWaveFxvToFxvhh + AmpVertexFxvToFxvhh
 AmpSum2FxvToFxvhh = AmpTreeFxvToFxvhh + AmpWaveFxvToFxvhh + AmpVertexFxvToFxvhh 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumFxvToFxvhh = AmpTreeFxvToFxvhh
 AmpSum2FxvToFxvhh = AmpTreeFxvToFxvhh 
End if 
Do gt1=1,2
i4 = isave 
  Do gt2=1,2
    Do gt3=1,2
If (((OSkinematics).and.(MFxvOS(gt1).gt.(MFxvOS(gt2)+MhhOS(gt3)))).or.((.not.OSkinematics).and.(MFxv(gt1).gt.(MFxv(gt2)+Mhh(gt3))))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2, gt3 
  AmpSum2FxvToFxvhh = AmpTreeFxvToFxvhh
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFxvOS(gt1),MFxvOS(gt2),MhhOS(gt3),AmpSumFxvToFxvhh(:,gt1, gt2, gt3),AmpSum2FxvToFxvhh(:,gt1, gt2, gt3),AmpSqFxvToFxvhh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFxv(gt1),MFxv(gt2),Mhh(gt3),AmpSumFxvToFxvhh(:,gt1, gt2, gt3),AmpSum2FxvToFxvhh(:,gt1, gt2, gt3),AmpSqFxvToFxvhh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqFxvToFxvhh(gt1, gt2, gt3) 
  AmpSum2FxvToFxvhh = 2._dp*AmpWaveFxvToFxvhh
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFxvOS(gt1),MFxvOS(gt2),MhhOS(gt3),AmpSumFxvToFxvhh(:,gt1, gt2, gt3),AmpSum2FxvToFxvhh(:,gt1, gt2, gt3),AmpSqFxvToFxvhh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFxv(gt1),MFxv(gt2),Mhh(gt3),AmpSumFxvToFxvhh(:,gt1, gt2, gt3),AmpSum2FxvToFxvhh(:,gt1, gt2, gt3),AmpSqFxvToFxvhh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqFxvToFxvhh(gt1, gt2, gt3) 
  AmpSum2FxvToFxvhh = 2._dp*AmpVertexFxvToFxvhh
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFxvOS(gt1),MFxvOS(gt2),MhhOS(gt3),AmpSumFxvToFxvhh(:,gt1, gt2, gt3),AmpSum2FxvToFxvhh(:,gt1, gt2, gt3),AmpSqFxvToFxvhh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFxv(gt1),MFxv(gt2),Mhh(gt3),AmpSumFxvToFxvhh(:,gt1, gt2, gt3),AmpSum2FxvToFxvhh(:,gt1, gt2, gt3),AmpSqFxvToFxvhh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqFxvToFxvhh(gt1, gt2, gt3) 
  AmpSum2FxvToFxvhh = AmpTreeFxvToFxvhh + 2._dp*AmpWaveFxvToFxvhh + 2._dp*AmpVertexFxvToFxvhh
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFxvOS(gt1),MFxvOS(gt2),MhhOS(gt3),AmpSumFxvToFxvhh(:,gt1, gt2, gt3),AmpSum2FxvToFxvhh(:,gt1, gt2, gt3),AmpSqFxvToFxvhh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFxv(gt1),MFxv(gt2),Mhh(gt3),AmpSumFxvToFxvhh(:,gt1, gt2, gt3),AmpSum2FxvToFxvhh(:,gt1, gt2, gt3),AmpSqFxvToFxvhh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqFxvToFxvhh(gt1, gt2, gt3) 
 End if 
If (OSkinematics) Then 
  AmpSum2FxvToFxvhh = AmpTreeFxvToFxvhh
  Call SquareAmp_FtoFS(MFxvOS(gt1),MFxvOS(gt2),MhhOS(gt3),AmpSumFxvToFxvhh(:,gt1, gt2, gt3),AmpSum2FxvToFxvhh(:,gt1, gt2, gt3),AmpSqFxvToFxvhh(gt1, gt2, gt3)) 
  AmpSqTreeFxvToFxvhh(gt1, gt2, gt3) = AmpSqFxvToFxvhh(gt1, gt2, gt3)  
  AmpSum2FxvToFxvhh = + 2._dp*AmpWaveFxvToFxvhh + 2._dp*AmpVertexFxvToFxvhh
  Call SquareAmp_FtoFS(MFxvOS(gt1),MFxvOS(gt2),MhhOS(gt3),AmpSumFxvToFxvhh(:,gt1, gt2, gt3),AmpSum2FxvToFxvhh(:,gt1, gt2, gt3),AmpSqFxvToFxvhh(gt1, gt2, gt3)) 
  AmpSqFxvToFxvhh(gt1, gt2, gt3) = AmpSqFxvToFxvhh(gt1, gt2, gt3) + AmpSqTreeFxvToFxvhh(gt1, gt2, gt3)  
Else  
  AmpSum2FxvToFxvhh = AmpTreeFxvToFxvhh
  Call SquareAmp_FtoFS(MFxv(gt1),MFxv(gt2),Mhh(gt3),AmpSumFxvToFxvhh(:,gt1, gt2, gt3),AmpSum2FxvToFxvhh(:,gt1, gt2, gt3),AmpSqFxvToFxvhh(gt1, gt2, gt3)) 
  AmpSqTreeFxvToFxvhh(gt1, gt2, gt3) = AmpSqFxvToFxvhh(gt1, gt2, gt3)  
  AmpSum2FxvToFxvhh = + 2._dp*AmpWaveFxvToFxvhh + 2._dp*AmpVertexFxvToFxvhh
  Call SquareAmp_FtoFS(MFxv(gt1),MFxv(gt2),Mhh(gt3),AmpSumFxvToFxvhh(:,gt1, gt2, gt3),AmpSum2FxvToFxvhh(:,gt1, gt2, gt3),AmpSqFxvToFxvhh(gt1, gt2, gt3)) 
  AmpSqFxvToFxvhh(gt1, gt2, gt3) = AmpSqFxvToFxvhh(gt1, gt2, gt3) + AmpSqTreeFxvToFxvhh(gt1, gt2, gt3)  
End if  
Else  
  AmpSqFxvToFxvhh(gt1, gt2, gt3) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFxvToFxvhh(gt1, gt2, gt3).le.0._dp) Then 
  gP1LFxv(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFxv(gt1,i4) = 1._dp*GammaTPS(MFxvOS(gt1),MFxvOS(gt2),MhhOS(gt3),helfactor*AmpSqFxvToFxvhh(gt1, gt2, gt3))
Else 
  gP1LFxv(gt1,i4) = 1._dp*GammaTPS(MFxv(gt1),MFxv(gt2),Mhh(gt3),helfactor*AmpSqFxvToFxvhh(gt1, gt2, gt3))
End if 
If ((Abs(MRPFxvToFxvhh(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGFxvToFxvhh(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFxv(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPFxvToFxvhh(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGFxvToFxvhh(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPFxvToFxvhh(gt1, gt2, gt3) + MRGFxvToFxvhh(gt1, gt2, gt3)) 
  gP1LFxv(gt1,i4) = gP1LFxv(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPFxvToFxvhh(gt1, gt2, gt3) + MRGFxvToFxvhh(gt1, gt2, gt3))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LFxv(gt1,i4) 
  End if 
End if 
End if 
i4=i4+1

    End do
  End do
If (gt1.eq.2) isave = i4 
End do
End If 
If (.not.CalcLoopDecay_LoopInducedOnly) Then 
!---------------- 
! Fxv VZ
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_FxvToFxvVZ(cplcFxvFxvVZL,cplcFxvFxvVZR,MFxv,            & 
& MVZ,MFxv2,MVZ2,AmpTreeFxvToFxvVZ)

  Else 
Call Amplitude_Tree_SDdiracDM_FxvToFxvVZ(ZcplcFxvFxvVZL,ZcplcFxvFxvVZR,               & 
& MFxv,MVZ,MFxv2,MVZ2,AmpTreeFxvToFxvVZ)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_FxvToFxvVZ(MLambda,em,gs,cplcFxvFxvVZL,cplcFxvFxvVZR,       & 
& MFxvOS,MVZOS,MRPFxvToFxvVZ,MRGFxvToFxvVZ)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_FxvToFxvVZ(MLambda,em,gs,ZcplcFxvFxvVZL,ZcplcFxvFxvVZR,     & 
& MFxvOS,MVZOS,MRPFxvToFxvVZ,MRGFxvToFxvVZ)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_FxvToFxvVZ(MLambda,em,gs,cplcFxvFxvVZL,cplcFxvFxvVZR,       & 
& MFxv,MVZ,MRPFxvToFxvVZ,MRGFxvToFxvVZ)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_FxvToFxvVZ(MLambda,em,gs,ZcplcFxvFxvVZL,ZcplcFxvFxvVZR,     & 
& MFxv,MVZ,MRPFxvToFxvVZ,MRGFxvToFxvVZ)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FxvToFxvVZ(cplcFxvFxvVZL,cplcFxvFxvVZR,ctcplcFxvFxvVZL, & 
& ctcplcFxvFxvVZR,MFxv,MFxv2,MVZ,MVZ2,ZfVZ,ZfxVL,ZfxVR,AmpWaveFxvToFxvVZ)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FxvToFxvVZ(MAh,MFv,MFxe,MFxv,Mhh,MHp,MSsc,            & 
& MVWp,MVZ,MAh2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFxvFxvAhL,               & 
& cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,           & 
& cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,               & 
& cplcFxeFxeVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,  & 
& cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,          & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexFxvToFxvVZ)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvVZ(MAh,MFv,MFxe,MFxv,Mhh,MHp,              & 
& MSsc,MVWp,MVZ,MAh2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFxvFxvAhL,          & 
& cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,           & 
& cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,               & 
& cplcFxeFxeVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,  & 
& cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,          & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRdrFxvToFxvVZ)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvVZ(MAhOS,MFvOS,MFxeOS,MFxvOS,              & 
& MhhOS,MHpOS,MSscOS,MVWpOS,MVZOS,MAh2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,           & 
& MSsc2OS,MVWp2OS,MVZ2OS,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,            & 
& cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,      & 
& cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,ZcplcFxvFxvVZL, & 
& ZcplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,            & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,          & 
& cplcVWpVWpVZ,AmpVertexIRosFxvToFxvVZ)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvVZ(MAh,MFv,MFxe,MFxv,Mhh,MHp,              & 
& MSsc,MVWp,MVZ,MAh2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFxvFxvAhL,          & 
& cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,           & 
& cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,               & 
& cplcFxeFxeVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,ZcplcFxvFxvVZL,ZcplcFxvFxvVZR,               & 
& cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,           & 
& cplcFxeFxvcVWpR,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,             & 
& AmpVertexIRosFxvToFxvVZ)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvVZ(MAhOS,MFvOS,MFxeOS,MFxvOS,              & 
& MhhOS,MHpOS,MSscOS,MVWpOS,MVZOS,MAh2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,           & 
& MSsc2OS,MVWp2OS,MVZ2OS,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,            & 
& cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,      & 
& cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,  & 
& cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,             & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,          & 
& cplcVWpVWpVZ,AmpVertexIRosFxvToFxvVZ)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvVZ(MAh,MFv,MFxe,MFxv,Mhh,MHp,              & 
& MSsc,MVWp,MVZ,MAh2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFxvFxvAhL,          & 
& cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,           & 
& cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,               & 
& cplcFxeFxeVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,  & 
& cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,          & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFxvToFxvVZ)

 End if 
 End if 
AmpVertexFxvToFxvVZ = AmpVertexFxvToFxvVZ -  AmpVertexIRdrFxvToFxvVZ! +  AmpVertexIRosFxvToFxvVZ ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZFxvToFxvVZ=0._dp 
AmpVertexZFxvToFxvVZ=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZFxvToFxvVZ(1,gt2,:) = AmpWaveZFxvToFxvVZ(1,gt2,:)+ZRUXVc(gt2,gt1)*AmpWaveFxvToFxvVZ(1,gt1,:) 
AmpVertexZFxvToFxvVZ(1,gt2,:)= AmpVertexZFxvToFxvVZ(1,gt2,:) + ZRUXVc(gt2,gt1)*AmpVertexFxvToFxvVZ(1,gt1,:) 
AmpWaveZFxvToFxvVZ(2,gt2,:) = AmpWaveZFxvToFxvVZ(2,gt2,:)+ZRUXU(gt2,gt1)*AmpWaveFxvToFxvVZ(2,gt1,:) 
AmpVertexZFxvToFxvVZ(2,gt2,:)= AmpVertexZFxvToFxvVZ(2,gt2,:) + ZRUXU(gt2,gt1)*AmpVertexFxvToFxvVZ(2,gt1,:) 
AmpWaveZFxvToFxvVZ(3,gt2,:) = AmpWaveZFxvToFxvVZ(3,gt2,:)+ZRUXVc(gt2,gt1)*AmpWaveFxvToFxvVZ(3,gt1,:) 
AmpVertexZFxvToFxvVZ(3,gt2,:)= AmpVertexZFxvToFxvVZ(3,gt2,:) + ZRUXVc(gt2,gt1)*AmpVertexFxvToFxvVZ(3,gt1,:) 
AmpWaveZFxvToFxvVZ(4,gt2,:) = AmpWaveZFxvToFxvVZ(4,gt2,:)+ZRUXU(gt2,gt1)*AmpWaveFxvToFxvVZ(4,gt1,:) 
AmpVertexZFxvToFxvVZ(4,gt2,:)= AmpVertexZFxvToFxvVZ(4,gt2,:) + ZRUXU(gt2,gt1)*AmpVertexFxvToFxvVZ(4,gt1,:) 
 End Do 
End Do 
AmpWaveFxvToFxvVZ=AmpWaveZFxvToFxvVZ 
AmpVertexFxvToFxvVZ= AmpVertexZFxvToFxvVZ
! Final State 1 
AmpWaveZFxvToFxvVZ=0._dp 
AmpVertexZFxvToFxvVZ=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZFxvToFxvVZ(1,:,gt2) = AmpWaveZFxvToFxvVZ(1,:,gt2)+ZRUXV(gt2,gt1)*AmpWaveFxvToFxvVZ(1,:,gt1) 
AmpVertexZFxvToFxvVZ(1,:,gt2)= AmpVertexZFxvToFxvVZ(1,:,gt2)+ZRUXV(gt2,gt1)*AmpVertexFxvToFxvVZ(1,:,gt1) 
AmpWaveZFxvToFxvVZ(2,:,gt2) = AmpWaveZFxvToFxvVZ(2,:,gt2)+ZRUXUc(gt2,gt1)*AmpWaveFxvToFxvVZ(2,:,gt1) 
AmpVertexZFxvToFxvVZ(2,:,gt2)= AmpVertexZFxvToFxvVZ(2,:,gt2)+ZRUXUc(gt2,gt1)*AmpVertexFxvToFxvVZ(2,:,gt1) 
AmpWaveZFxvToFxvVZ(3,:,gt2) = AmpWaveZFxvToFxvVZ(3,:,gt2)+ZRUXV(gt2,gt1)*AmpWaveFxvToFxvVZ(3,:,gt1) 
AmpVertexZFxvToFxvVZ(3,:,gt2)= AmpVertexZFxvToFxvVZ(3,:,gt2)+ZRUXV(gt2,gt1)*AmpVertexFxvToFxvVZ(3,:,gt1) 
AmpWaveZFxvToFxvVZ(4,:,gt2) = AmpWaveZFxvToFxvVZ(4,:,gt2)+ZRUXUc(gt2,gt1)*AmpWaveFxvToFxvVZ(4,:,gt1) 
AmpVertexZFxvToFxvVZ(4,:,gt2)= AmpVertexZFxvToFxvVZ(4,:,gt2)+ZRUXUc(gt2,gt1)*AmpVertexFxvToFxvVZ(4,:,gt1) 
 End Do 
End Do 
AmpWaveFxvToFxvVZ=AmpWaveZFxvToFxvVZ 
AmpVertexFxvToFxvVZ= AmpVertexZFxvToFxvVZ
End if
If (ShiftIRdiv) Then 
AmpVertexFxvToFxvVZ = AmpVertexFxvToFxvVZ  +  AmpVertexIRosFxvToFxvVZ
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fxv->Fxv VZ -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumFxvToFxvVZ = AmpTreeFxvToFxvVZ 
 AmpSum2FxvToFxvVZ = AmpTreeFxvToFxvVZ + 2._dp*AmpWaveFxvToFxvVZ + 2._dp*AmpVertexFxvToFxvVZ  
Else 
 AmpSumFxvToFxvVZ = AmpTreeFxvToFxvVZ + AmpWaveFxvToFxvVZ + AmpVertexFxvToFxvVZ
 AmpSum2FxvToFxvVZ = AmpTreeFxvToFxvVZ + AmpWaveFxvToFxvVZ + AmpVertexFxvToFxvVZ 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumFxvToFxvVZ = AmpTreeFxvToFxvVZ
 AmpSum2FxvToFxvVZ = AmpTreeFxvToFxvVZ 
End if 
Do gt1=1,2
i4 = isave 
  Do gt2=1,2
If (((OSkinematics).and.(MFxvOS(gt1).gt.(MFxvOS(gt2)+MVZOS))).or.((.not.OSkinematics).and.(MFxv(gt1).gt.(MFxv(gt2)+MVZ)))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2 
  AmpSum2FxvToFxvVZ = AmpTreeFxvToFxvVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxvOS(gt1),MFxvOS(gt2),MVZOS,AmpSumFxvToFxvVZ(:,gt1, gt2),AmpSum2FxvToFxvVZ(:,gt1, gt2),AmpSqFxvToFxvVZ(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFxv(gt1),MFxv(gt2),MVZ,AmpSumFxvToFxvVZ(:,gt1, gt2),AmpSum2FxvToFxvVZ(:,gt1, gt2),AmpSqFxvToFxvVZ(gt1, gt2)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqFxvToFxvVZ(gt1, gt2) 
  AmpSum2FxvToFxvVZ = 2._dp*AmpWaveFxvToFxvVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxvOS(gt1),MFxvOS(gt2),MVZOS,AmpSumFxvToFxvVZ(:,gt1, gt2),AmpSum2FxvToFxvVZ(:,gt1, gt2),AmpSqFxvToFxvVZ(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFxv(gt1),MFxv(gt2),MVZ,AmpSumFxvToFxvVZ(:,gt1, gt2),AmpSum2FxvToFxvVZ(:,gt1, gt2),AmpSqFxvToFxvVZ(gt1, gt2)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqFxvToFxvVZ(gt1, gt2) 
  AmpSum2FxvToFxvVZ = 2._dp*AmpVertexFxvToFxvVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxvOS(gt1),MFxvOS(gt2),MVZOS,AmpSumFxvToFxvVZ(:,gt1, gt2),AmpSum2FxvToFxvVZ(:,gt1, gt2),AmpSqFxvToFxvVZ(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFxv(gt1),MFxv(gt2),MVZ,AmpSumFxvToFxvVZ(:,gt1, gt2),AmpSum2FxvToFxvVZ(:,gt1, gt2),AmpSqFxvToFxvVZ(gt1, gt2)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqFxvToFxvVZ(gt1, gt2) 
  AmpSum2FxvToFxvVZ = AmpTreeFxvToFxvVZ + 2._dp*AmpWaveFxvToFxvVZ + 2._dp*AmpVertexFxvToFxvVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxvOS(gt1),MFxvOS(gt2),MVZOS,AmpSumFxvToFxvVZ(:,gt1, gt2),AmpSum2FxvToFxvVZ(:,gt1, gt2),AmpSqFxvToFxvVZ(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFxv(gt1),MFxv(gt2),MVZ,AmpSumFxvToFxvVZ(:,gt1, gt2),AmpSum2FxvToFxvVZ(:,gt1, gt2),AmpSqFxvToFxvVZ(gt1, gt2)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqFxvToFxvVZ(gt1, gt2) 
 End if 
If (OSkinematics) Then 
  AmpSum2FxvToFxvVZ = AmpTreeFxvToFxvVZ
  Call SquareAmp_FtoFV(MFxvOS(gt1),MFxvOS(gt2),MVZOS,AmpSumFxvToFxvVZ(:,gt1, gt2),AmpSum2FxvToFxvVZ(:,gt1, gt2),AmpSqFxvToFxvVZ(gt1, gt2)) 
  AmpSqTreeFxvToFxvVZ(gt1, gt2) = AmpSqFxvToFxvVZ(gt1, gt2)  
  AmpSum2FxvToFxvVZ = + 2._dp*AmpWaveFxvToFxvVZ + 2._dp*AmpVertexFxvToFxvVZ
  Call SquareAmp_FtoFV(MFxvOS(gt1),MFxvOS(gt2),MVZOS,AmpSumFxvToFxvVZ(:,gt1, gt2),AmpSum2FxvToFxvVZ(:,gt1, gt2),AmpSqFxvToFxvVZ(gt1, gt2)) 
  AmpSqFxvToFxvVZ(gt1, gt2) = AmpSqFxvToFxvVZ(gt1, gt2) + AmpSqTreeFxvToFxvVZ(gt1, gt2)  
Else  
  AmpSum2FxvToFxvVZ = AmpTreeFxvToFxvVZ
  Call SquareAmp_FtoFV(MFxv(gt1),MFxv(gt2),MVZ,AmpSumFxvToFxvVZ(:,gt1, gt2),AmpSum2FxvToFxvVZ(:,gt1, gt2),AmpSqFxvToFxvVZ(gt1, gt2)) 
  AmpSqTreeFxvToFxvVZ(gt1, gt2) = AmpSqFxvToFxvVZ(gt1, gt2)  
  AmpSum2FxvToFxvVZ = + 2._dp*AmpWaveFxvToFxvVZ + 2._dp*AmpVertexFxvToFxvVZ
  Call SquareAmp_FtoFV(MFxv(gt1),MFxv(gt2),MVZ,AmpSumFxvToFxvVZ(:,gt1, gt2),AmpSum2FxvToFxvVZ(:,gt1, gt2),AmpSqFxvToFxvVZ(gt1, gt2)) 
  AmpSqFxvToFxvVZ(gt1, gt2) = AmpSqFxvToFxvVZ(gt1, gt2) + AmpSqTreeFxvToFxvVZ(gt1, gt2)  
End if  
Else  
  AmpSqFxvToFxvVZ(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFxvToFxvVZ(gt1, gt2).le.0._dp) Then 
  gP1LFxv(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFxv(gt1,i4) = 1._dp*GammaTPS(MFxvOS(gt1),MFxvOS(gt2),MVZOS,helfactor*AmpSqFxvToFxvVZ(gt1, gt2))
Else 
  gP1LFxv(gt1,i4) = 1._dp*GammaTPS(MFxv(gt1),MFxv(gt2),MVZ,helfactor*AmpSqFxvToFxvVZ(gt1, gt2))
End if 
If ((Abs(MRPFxvToFxvVZ(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFxvToFxvVZ(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFxv(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPFxvToFxvVZ(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFxvToFxvVZ(gt1, gt2)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPFxvToFxvVZ(gt1, gt2) + MRGFxvToFxvVZ(gt1, gt2)) 
  gP1LFxv(gt1,i4) = gP1LFxv(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPFxvToFxvVZ(gt1, gt2) + MRGFxvToFxvVZ(gt1, gt2))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LFxv(gt1,i4) 
  End if 
End if 
End if 
i4=i4+1

  End do
If (gt1.eq.2) isave = i4 
End do
End If 
!---------------- 
! Fxv VP
!---------------- 

If (LoopInducedDecaysOS) Then 


!Self-energy Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_WAVE_SDdiracDM_FxvToFxvVP(ZcplcFxvFxvVZL,ZcplcFxvFxvVZR,               & 
& ctcplcFxvFxvVZL,ctcplcFxvFxvVZR,MFxvOS,MFxv2OS,MVP,MVP2,MVZOS,MVZ2OS,ZfVP,             & 
& ZfVZVP,ZfxVL,ZfxVR,AmpWaveFxvToFxvVP)

 Else 
Call Amplitude_WAVE_SDdiracDM_FxvToFxvVP(cplcFxvFxvVZL,cplcFxvFxvVZR,ctcplcFxvFxvVZL, & 
& ctcplcFxvFxvVZR,MFxvOS,MFxv2OS,MVP,MVP2,MVZOS,MVZ2OS,ZfVP,ZfVZVP,ZfxVL,ZfxVR,          & 
& AmpWaveFxvToFxvVP)

 End if 


!Vertex Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_VERTEX_SDdiracDM_FxvToFxvVP(MFxeOS,MFxvOS,MHpOS,MVP,MVWpOS,            & 
& MFxe2OS,MFxv2OS,MHp2OS,MVP2,MVWp2OS,ZcplcFxvFxeHpL,ZcplcFxvFxeHpR,ZcplcFxeFxeVPL,      & 
& ZcplcFxeFxeVPR,ZcplcFxvFxeVWpL,ZcplcFxvFxeVWpR,ZcplcFxeFxvcHpL,ZcplcFxeFxvcHpR,        & 
& ZcplcFxeFxvcVWpL,ZcplcFxeFxvcVWpR,ZcplHpcHpVP,ZcplHpcVWpVP,ZcplcHpVPVWp,               & 
& ZcplcVWpVPVWp,AmpVertexFxvToFxvVP)

 Else 
Call Amplitude_VERTEX_SDdiracDM_FxvToFxvVP(MFxeOS,MFxvOS,MHpOS,MVP,MVWpOS,            & 
& MFxe2OS,MFxv2OS,MHp2OS,MVP2,MVWp2OS,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,         & 
& cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,             & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,       & 
& AmpVertexFxvToFxvVP)

 End if 
Else 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FxvToFxvVP(cplcFxvFxvVZL,cplcFxvFxvVZR,ctcplcFxvFxvVZL, & 
& ctcplcFxvFxvVZR,MFxv,MFxv2,MVP,MVP2,MVZ,MVZ2,ZfVP,ZfVZVP,ZfxVL,ZfxVR,AmpWaveFxvToFxvVP)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FxvToFxvVP(MFxe,MFxv,MHp,MVP,MVWp,MFxe2,              & 
& MFxv2,MHp2,MVP2,MVWp2,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,         & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,           & 
& cplcFxeFxvcVWpR,cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,AmpVertexFxvToFxvVP)

End if 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fxv->Fxv VP -----------------------" 
End if 
If (OnlyTreeLevelContributions) Then 
 AmpSumFxvToFxvVP = 0._dp 
 AmpSum2FxvToFxvVP = 0._dp  
Else 
 AmpSumFxvToFxvVP = AmpVertexFxvToFxvVP + AmpWaveFxvToFxvVP
 AmpSum2FxvToFxvVP = AmpVertexFxvToFxvVP + AmpWaveFxvToFxvVP 
End If 
Do gt1=1,2
i4 = isave 
  Do gt2=1,2
If (((OSkinematics).and.(MFxvOS(gt1).gt.(MFxvOS(gt2)+0.))).or.((.not.OSkinematics).and.(MFxv(gt1).gt.(MFxv(gt2)+MVP)))) Then 
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFxvOS(gt1),MFxvOS(gt2),0._dp,AmpSumFxvToFxvVP(:,gt1, gt2),AmpSum2FxvToFxvVP(:,gt1, gt2),AmpSqFxvToFxvVP(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFxv(gt1),MFxv(gt2),MVP,AmpSumFxvToFxvVP(:,gt1, gt2),AmpSum2FxvToFxvVP(:,gt1, gt2),AmpSqFxvToFxvVP(gt1, gt2)) 
End if  
Else  
  AmpSqFxvToFxvVP(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFxvToFxvVP(gt1, gt2).le.0._dp) Then 
  gP1LFxv(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFxv(gt1,i4) = 1._dp*GammaTPS(MFxvOS(gt1),MFxvOS(gt2),0._dp,helfactor*AmpSqFxvToFxvVP(gt1, gt2))
Else 
  gP1LFxv(gt1,i4) = 1._dp*GammaTPS(MFxv(gt1),MFxv(gt2),MVP,helfactor*AmpSqFxvToFxvVP(gt1, gt2))
End if 
If ((Abs(MRPFxvToFxvVP(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFxvToFxvVP(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFxv(gt1,i4) 
End if 
i4=i4+1

  End do
If (gt1.eq.2) isave = i4 
End do
End Subroutine OneLoopDecay_Fxv

End Module Wrapper_OneLoopDecay_Fxv_SDdiracDM
