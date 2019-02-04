! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:22 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module Wrapper_OneLoopDecay_Fu_SDdiracDM
Use Model_Data_SDdiracDM 
Use Kinematics 
Use OneLoopDecay_Fu_SDdiracDM 
Use Control 
Use Settings 

 
Contains

 
Subroutine OneLoopDecay_Fu(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,MFeOS,              & 
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
& cplAhHpcVWp,cplcFdFdAhL,cplcFdFdAhR,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVGL,               & 
& cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFdFucHpL,              & 
& cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,         & 
& cplcFuFdVWpR,cplcFuFuAhL,cplcFuFuAhR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,              & 
& cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcHpVPVWp,               & 
& cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,cplhhhhhh,              & 
& cplhhHpcHp,cplhhHpcVWp,cplhhVZVZ,cplHpcHpVP,cplHpcHpVZ,cplHpcVWpVP,cplHpcVWpVZ,        & 
& cplVGVGVG,ctcplcFuFdHpL,ctcplcFuFdHpR,ctcplcFuFdVWpL,ctcplcFuFdVWpR,ctcplcFuFuAhL,     & 
& ctcplcFuFuAhR,ctcplcFuFuhhL,ctcplcFuFuhhR,ctcplcFuFuVGL,ctcplcFuFuVGR,ctcplcFuFuVPL,   & 
& ctcplcFuFuVPR,ctcplcFuFuVZL,ctcplcFuFuVZR,GcplcFuFdHpL,GcplcFuFdHpR,GcplcHpVPVWp,      & 
& GcplHpcVWpVP,GosZcplcFuFdHpL,GosZcplcFuFdHpR,GosZcplcHpVPVWp,GosZcplHpcVWpVP,          & 
& GZcplcFuFdHpL,GZcplcFuFdHpR,GZcplcHpVPVWp,GZcplHpcVWpVP,ZcplcFdFdVGL,ZcplcFdFdVGR,     & 
& ZcplcFdFdVPL,ZcplcFdFdVPR,ZcplcFdFucHpL,ZcplcFdFucHpR,ZcplcFdFucVWpL,ZcplcFdFucVWpR,   & 
& ZcplcFuFdHpL,ZcplcFuFdHpR,ZcplcFuFdVWpL,ZcplcFuFdVWpR,ZcplcFuFuAhL,ZcplcFuFuAhR,       & 
& ZcplcFuFuhhL,ZcplcFuFuhhR,ZcplcFuFuVGL,ZcplcFuFuVGR,ZcplcFuFuVPL,ZcplcFuFuVPR,         & 
& ZcplcFuFuVZL,ZcplcFuFuVZR,ZcplcHpVPVWp,ZcplcVWpVPVWp,ZcplHpcHpVP,ZcplHpcVWpVP,         & 
& ZcplVGVGVG,ZRUZH,ZRUVd,ZRUUd,ZRUVu,ZRUUu,ZRUVe,ZRUUe,ZRUVv,ZRUVvr,ZRUXV,               & 
& ZRUXU,ZRUVSs,MLambda,em,gs,deltaM,kont,gP1LFu)

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

Complex(dp),Intent(in) :: cplAhAhhh(2),cplAhcHpVWp,cplAhhhVZ(2),cplAhHpcVWp,cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),  & 
& cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),               & 
& cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFdFucHpL(3,3), & 
& cplcFdFucHpR(3,3),cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),cplcFuFdHpL(3,3),              & 
& cplcFuFdHpR(3,3),cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),& 
& cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),               & 
& cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcHpVPVWp,       & 
& cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp(2),cplhhcVWpVWp(2),cplhhhhhh(2,2,2), & 
& cplhhHpcHp(2),cplhhHpcVWp(2),cplhhVZVZ(2),cplHpcHpVP,cplHpcHpVZ,cplHpcVWpVP,           & 
& cplHpcVWpVZ,cplVGVGVG,ctcplcFuFdHpL(3,3),ctcplcFuFdHpR(3,3),ctcplcFuFdVWpL(3,3),       & 
& ctcplcFuFdVWpR(3,3),ctcplcFuFuAhL(3,3),ctcplcFuFuAhR(3,3),ctcplcFuFuhhL(3,3,2),        & 
& ctcplcFuFuhhR(3,3,2),ctcplcFuFuVGL(3,3),ctcplcFuFuVGR(3,3),ctcplcFuFuVPL(3,3),         & 
& ctcplcFuFuVPR(3,3),ctcplcFuFuVZL(3,3),ctcplcFuFuVZR(3,3),GcplcFuFdHpL(3,3),            & 
& GcplcFuFdHpR(3,3),GcplcHpVPVWp,GcplHpcVWpVP,GosZcplcFuFdHpL(3,3),GosZcplcFuFdHpR(3,3), & 
& GosZcplcHpVPVWp,GosZcplHpcVWpVP,GZcplcFuFdHpL(3,3),GZcplcFuFdHpR(3,3),GZcplcHpVPVWp,   & 
& GZcplHpcVWpVP,ZcplcFdFdVGL(3,3),ZcplcFdFdVGR(3,3),ZcplcFdFdVPL(3,3),ZcplcFdFdVPR(3,3), & 
& ZcplcFdFucHpL(3,3),ZcplcFdFucHpR(3,3),ZcplcFdFucVWpL(3,3),ZcplcFdFucVWpR(3,3),         & 
& ZcplcFuFdHpL(3,3),ZcplcFuFdHpR(3,3),ZcplcFuFdVWpL(3,3),ZcplcFuFdVWpR(3,3),             & 
& ZcplcFuFuAhL(3,3),ZcplcFuFuAhR(3,3),ZcplcFuFuhhL(3,3,2),ZcplcFuFuhhR(3,3,2),           & 
& ZcplcFuFuVGL(3,3),ZcplcFuFuVGR(3,3),ZcplcFuFuVPL(3,3),ZcplcFuFuVPR(3,3),               & 
& ZcplcFuFuVZL(3,3),ZcplcFuFuVZR(3,3),ZcplcHpVPVWp,ZcplcVWpVPVWp,ZcplHpcHpVP,            & 
& ZcplHpcVWpVP,ZcplVGVGVG

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
Real(dp), Intent(out) :: gP1LFu(3,18) 
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
Real(dp) :: MRPFuToFdVWp(3,3),MRGFuToFdVWp(3,3), MRPZFuToFdVWp(3,3),MRGZFuToFdVWp(3,3) 
Real(dp) :: MVPFuToFdVWp(3,3) 
Real(dp) :: RMsqTreeFuToFdVWp(3,3),RMsqWaveFuToFdVWp(3,3),RMsqVertexFuToFdVWp(3,3) 
Complex(dp) :: AmpTreeFuToFdVWp(4,3,3),AmpWaveFuToFdVWp(4,3,3)=(0._dp,0._dp),AmpVertexFuToFdVWp(4,3,3)& 
 & ,AmpVertexIRosFuToFdVWp(4,3,3),AmpVertexIRdrFuToFdVWp(4,3,3), AmpSumFuToFdVWp(4,3,3), AmpSum2FuToFdVWp(4,3,3) 
Complex(dp) :: AmpTreeZFuToFdVWp(4,3,3),AmpWaveZFuToFdVWp(4,3,3),AmpVertexZFuToFdVWp(4,3,3) 
Real(dp) :: AmpSqFuToFdVWp(3,3),  AmpSqTreeFuToFdVWp(3,3) 
Real(dp) :: MRPFuToFuhh(3,3,2),MRGFuToFuhh(3,3,2), MRPZFuToFuhh(3,3,2),MRGZFuToFuhh(3,3,2) 
Real(dp) :: MVPFuToFuhh(3,3,2) 
Real(dp) :: RMsqTreeFuToFuhh(3,3,2),RMsqWaveFuToFuhh(3,3,2),RMsqVertexFuToFuhh(3,3,2) 
Complex(dp) :: AmpTreeFuToFuhh(2,3,3,2),AmpWaveFuToFuhh(2,3,3,2)=(0._dp,0._dp),AmpVertexFuToFuhh(2,3,3,2)& 
 & ,AmpVertexIRosFuToFuhh(2,3,3,2),AmpVertexIRdrFuToFuhh(2,3,3,2), AmpSumFuToFuhh(2,3,3,2), AmpSum2FuToFuhh(2,3,3,2) 
Complex(dp) :: AmpTreeZFuToFuhh(2,3,3,2),AmpWaveZFuToFuhh(2,3,3,2),AmpVertexZFuToFuhh(2,3,3,2) 
Real(dp) :: AmpSqFuToFuhh(3,3,2),  AmpSqTreeFuToFuhh(3,3,2) 
Real(dp) :: MRPFuToFuVZ(3,3),MRGFuToFuVZ(3,3), MRPZFuToFuVZ(3,3),MRGZFuToFuVZ(3,3) 
Real(dp) :: MVPFuToFuVZ(3,3) 
Real(dp) :: RMsqTreeFuToFuVZ(3,3),RMsqWaveFuToFuVZ(3,3),RMsqVertexFuToFuVZ(3,3) 
Complex(dp) :: AmpTreeFuToFuVZ(4,3,3),AmpWaveFuToFuVZ(4,3,3)=(0._dp,0._dp),AmpVertexFuToFuVZ(4,3,3)& 
 & ,AmpVertexIRosFuToFuVZ(4,3,3),AmpVertexIRdrFuToFuVZ(4,3,3), AmpSumFuToFuVZ(4,3,3), AmpSum2FuToFuVZ(4,3,3) 
Complex(dp) :: AmpTreeZFuToFuVZ(4,3,3),AmpWaveZFuToFuVZ(4,3,3),AmpVertexZFuToFuVZ(4,3,3) 
Real(dp) :: AmpSqFuToFuVZ(3,3),  AmpSqTreeFuToFuVZ(3,3) 
Real(dp) :: MRPFuToFuVG(3,3),MRGFuToFuVG(3,3), MRPZFuToFuVG(3,3),MRGZFuToFuVG(3,3) 
Real(dp) :: MVPFuToFuVG(3,3) 
Real(dp) :: RMsqTreeFuToFuVG(3,3),RMsqWaveFuToFuVG(3,3),RMsqVertexFuToFuVG(3,3) 
Complex(dp) :: AmpTreeFuToFuVG(4,3,3),AmpWaveFuToFuVG(4,3,3)=(0._dp,0._dp),AmpVertexFuToFuVG(4,3,3)& 
 & ,AmpVertexIRosFuToFuVG(4,3,3),AmpVertexIRdrFuToFuVG(4,3,3), AmpSumFuToFuVG(4,3,3), AmpSum2FuToFuVG(4,3,3) 
Complex(dp) :: AmpTreeZFuToFuVG(4,3,3),AmpWaveZFuToFuVG(4,3,3),AmpVertexZFuToFuVG(4,3,3) 
Real(dp) :: AmpSqFuToFuVG(3,3),  AmpSqTreeFuToFuVG(3,3) 
Real(dp) :: MRPFuToFuVP(3,3),MRGFuToFuVP(3,3), MRPZFuToFuVP(3,3),MRGZFuToFuVP(3,3) 
Real(dp) :: MVPFuToFuVP(3,3) 
Real(dp) :: RMsqTreeFuToFuVP(3,3),RMsqWaveFuToFuVP(3,3),RMsqVertexFuToFuVP(3,3) 
Complex(dp) :: AmpTreeFuToFuVP(4,3,3),AmpWaveFuToFuVP(4,3,3)=(0._dp,0._dp),AmpVertexFuToFuVP(4,3,3)& 
 & ,AmpVertexIRosFuToFuVP(4,3,3),AmpVertexIRdrFuToFuVP(4,3,3), AmpSumFuToFuVP(4,3,3), AmpSum2FuToFuVP(4,3,3) 
Complex(dp) :: AmpTreeZFuToFuVP(4,3,3),AmpWaveZFuToFuVP(4,3,3),AmpVertexZFuToFuVP(4,3,3) 
Real(dp) :: AmpSqFuToFuVP(3,3),  AmpSqTreeFuToFuVP(3,3) 
Write(*,*) "Calculating one-loop decays of Fu " 
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
! Fd VWp
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_FuToFdVWp(cplcFuFdVWpL,cplcFuFdVWpR,MFd,MFu,            & 
& MVWp,MFd2,MFu2,MVWp2,AmpTreeFuToFdVWp)

  Else 
Call Amplitude_Tree_SDdiracDM_FuToFdVWp(ZcplcFuFdVWpL,ZcplcFuFdVWpR,MFd,              & 
& MFu,MVWp,MFd2,MFu2,MVWp2,AmpTreeFuToFdVWp)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_FuToFdVWp(MLambda,em,gs,cplcFuFdVWpL,cplcFuFdVWpR,          & 
& MFdOS,MFuOS,MVWpOS,MRPFuToFdVWp,MRGFuToFdVWp)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_FuToFdVWp(MLambda,em,gs,ZcplcFuFdVWpL,ZcplcFuFdVWpR,        & 
& MFdOS,MFuOS,MVWpOS,MRPFuToFdVWp,MRGFuToFdVWp)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_FuToFdVWp(MLambda,em,gs,cplcFuFdVWpL,cplcFuFdVWpR,          & 
& MFd,MFu,MVWp,MRPFuToFdVWp,MRGFuToFdVWp)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_FuToFdVWp(MLambda,em,gs,ZcplcFuFdVWpL,ZcplcFuFdVWpR,        & 
& MFd,MFu,MVWp,MRPFuToFdVWp,MRGFuToFdVWp)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FuToFdVWp(cplcFuFdVWpL,cplcFuFdVWpR,ctcplcFuFdVWpL,     & 
& ctcplcFuFdVWpR,MFd,MFd2,MFu,MFu2,MVWp,MVWp2,ZfDL,ZfDR,ZfUL,ZfUR,ZfVWp,AmpWaveFuToFdVWp)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FuToFdVWp(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,MVWp,           & 
& MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,cplcFdFdAhR,             & 
& cplcFuFuAhL,cplcFuFuAhR,cplAhcHpVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,               & 
& cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,              & 
& cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,              & 
& cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplhhcHpVWp,               & 
& cplhhcVWpVWp,cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexFuToFdVWp)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_FuToFdVWp(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,             & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,cplcFdFdAhR,        & 
& cplcFuFuAhL,cplcFuFuAhR,cplAhcHpVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,               & 
& cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,              & 
& cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,              & 
& cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplhhcHpVWp,               & 
& cplhhcVWpVWp,cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRdrFuToFdVWp)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FuToFdVWp(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,           & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplcFdFdAhL,cplcFdFdAhR,cplcFuFuAhL,cplcFuFuAhR,cplAhcHpVWp,cplcFdFdhhL,        & 
& cplcFdFdhhR,GosZcplcFuFdHpL,GosZcplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,       & 
& cplcFdFdVPR,ZcplcFuFdVWpL,ZcplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,           & 
& cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,               & 
& cplcFuFuVZR,cplhhcHpVWp,cplhhcVWpVWp,GosZcplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,         & 
& cplcVWpVWpVZ,AmpVertexIRosFuToFdVWp)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FuToFdVWp(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,             & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,cplcFdFdAhR,        & 
& cplcFuFuAhL,cplcFuFuAhR,cplAhcHpVWp,cplcFdFdhhL,cplcFdFdhhR,GZcplcFuFdHpL,             & 
& GZcplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,ZcplcFuFdVWpL,           & 
& ZcplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,             & 
& cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplhhcHpVWp,               & 
& cplhhcVWpVWp,GZcplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFuToFdVWp)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FuToFdVWp(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,           & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplcFdFdAhL,cplcFdFdAhR,cplcFuFuAhL,cplcFuFuAhR,cplAhcHpVWp,cplcFdFdhhL,        & 
& cplcFdFdhhR,GcplcFuFdHpL,GcplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,             & 
& cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,             & 
& cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,               & 
& cplcFuFuVZR,cplhhcHpVWp,cplhhcVWpVWp,GcplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,            & 
& cplcVWpVWpVZ,AmpVertexIRosFuToFdVWp)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FuToFdVWp(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,             & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,cplcFdFdAhR,        & 
& cplcFuFuAhL,cplcFuFuAhR,cplAhcHpVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,               & 
& cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,              & 
& cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,              & 
& cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplhhcHpVWp,               & 
& cplhhcVWpVWp,cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFuToFdVWp)

 End if 
 End if 
AmpVertexFuToFdVWp = AmpVertexFuToFdVWp -  AmpVertexIRdrFuToFdVWp! +  AmpVertexIRosFuToFdVWp ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZFuToFdVWp=0._dp 
AmpVertexZFuToFdVWp=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFuToFdVWp(1,gt2,:) = AmpWaveZFuToFdVWp(1,gt2,:)+ZRUVuc(gt2,gt1)*AmpWaveFuToFdVWp(1,gt1,:) 
AmpVertexZFuToFdVWp(1,gt2,:)= AmpVertexZFuToFdVWp(1,gt2,:) + ZRUVuc(gt2,gt1)*AmpVertexFuToFdVWp(1,gt1,:) 
AmpWaveZFuToFdVWp(2,gt2,:) = AmpWaveZFuToFdVWp(2,gt2,:)+ZRUUu(gt2,gt1)*AmpWaveFuToFdVWp(2,gt1,:) 
AmpVertexZFuToFdVWp(2,gt2,:)= AmpVertexZFuToFdVWp(2,gt2,:) + ZRUUu(gt2,gt1)*AmpVertexFuToFdVWp(2,gt1,:) 
AmpWaveZFuToFdVWp(3,gt2,:) = AmpWaveZFuToFdVWp(3,gt2,:)+ZRUVuc(gt2,gt1)*AmpWaveFuToFdVWp(3,gt1,:) 
AmpVertexZFuToFdVWp(3,gt2,:)= AmpVertexZFuToFdVWp(3,gt2,:) + ZRUVuc(gt2,gt1)*AmpVertexFuToFdVWp(3,gt1,:) 
AmpWaveZFuToFdVWp(4,gt2,:) = AmpWaveZFuToFdVWp(4,gt2,:)+ZRUUu(gt2,gt1)*AmpWaveFuToFdVWp(4,gt1,:) 
AmpVertexZFuToFdVWp(4,gt2,:)= AmpVertexZFuToFdVWp(4,gt2,:) + ZRUUu(gt2,gt1)*AmpVertexFuToFdVWp(4,gt1,:) 
 End Do 
End Do 
AmpWaveFuToFdVWp=AmpWaveZFuToFdVWp 
AmpVertexFuToFdVWp= AmpVertexZFuToFdVWp
! Final State 1 
AmpWaveZFuToFdVWp=0._dp 
AmpVertexZFuToFdVWp=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFuToFdVWp(1,:,gt2) = AmpWaveZFuToFdVWp(1,:,gt2)+ZRUVd(gt2,gt1)*AmpWaveFuToFdVWp(1,:,gt1) 
AmpVertexZFuToFdVWp(1,:,gt2)= AmpVertexZFuToFdVWp(1,:,gt2)+ZRUVd(gt2,gt1)*AmpVertexFuToFdVWp(1,:,gt1) 
AmpWaveZFuToFdVWp(2,:,gt2) = AmpWaveZFuToFdVWp(2,:,gt2)+ZRUUdc(gt2,gt1)*AmpWaveFuToFdVWp(2,:,gt1) 
AmpVertexZFuToFdVWp(2,:,gt2)= AmpVertexZFuToFdVWp(2,:,gt2)+ZRUUdc(gt2,gt1)*AmpVertexFuToFdVWp(2,:,gt1) 
AmpWaveZFuToFdVWp(3,:,gt2) = AmpWaveZFuToFdVWp(3,:,gt2)+ZRUVd(gt2,gt1)*AmpWaveFuToFdVWp(3,:,gt1) 
AmpVertexZFuToFdVWp(3,:,gt2)= AmpVertexZFuToFdVWp(3,:,gt2)+ZRUVd(gt2,gt1)*AmpVertexFuToFdVWp(3,:,gt1) 
AmpWaveZFuToFdVWp(4,:,gt2) = AmpWaveZFuToFdVWp(4,:,gt2)+ZRUUdc(gt2,gt1)*AmpWaveFuToFdVWp(4,:,gt1) 
AmpVertexZFuToFdVWp(4,:,gt2)= AmpVertexZFuToFdVWp(4,:,gt2)+ZRUUdc(gt2,gt1)*AmpVertexFuToFdVWp(4,:,gt1) 
 End Do 
End Do 
AmpWaveFuToFdVWp=AmpWaveZFuToFdVWp 
AmpVertexFuToFdVWp= AmpVertexZFuToFdVWp
End if
If (ShiftIRdiv) Then 
AmpVertexFuToFdVWp = AmpVertexFuToFdVWp  +  AmpVertexIRosFuToFdVWp
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fu->Fd VWp -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumFuToFdVWp = AmpTreeFuToFdVWp 
 AmpSum2FuToFdVWp = AmpTreeFuToFdVWp + 2._dp*AmpWaveFuToFdVWp + 2._dp*AmpVertexFuToFdVWp  
Else 
 AmpSumFuToFdVWp = AmpTreeFuToFdVWp + AmpWaveFuToFdVWp + AmpVertexFuToFdVWp
 AmpSum2FuToFdVWp = AmpTreeFuToFdVWp + AmpWaveFuToFdVWp + AmpVertexFuToFdVWp 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumFuToFdVWp = AmpTreeFuToFdVWp
 AmpSum2FuToFdVWp = AmpTreeFuToFdVWp 
End if 
Do gt1=1,3
i4 = isave 
  Do gt2=1,3
If (((OSkinematics).and.(MFuOS(gt1).gt.(MFdOS(gt2)+MVWpOS))).or.((.not.OSkinematics).and.(MFu(gt1).gt.(MFd(gt2)+MVWp)))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2 
  AmpSum2FuToFdVWp = AmpTreeFuToFdVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFuOS(gt1),MFdOS(gt2),MVWpOS,AmpSumFuToFdVWp(:,gt1, gt2),AmpSum2FuToFdVWp(:,gt1, gt2),AmpSqFuToFdVWp(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFu(gt1),MFd(gt2),MVWp,AmpSumFuToFdVWp(:,gt1, gt2),AmpSum2FuToFdVWp(:,gt1, gt2),AmpSqFuToFdVWp(gt1, gt2)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqFuToFdVWp(gt1, gt2) 
  AmpSum2FuToFdVWp = 2._dp*AmpWaveFuToFdVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFuOS(gt1),MFdOS(gt2),MVWpOS,AmpSumFuToFdVWp(:,gt1, gt2),AmpSum2FuToFdVWp(:,gt1, gt2),AmpSqFuToFdVWp(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFu(gt1),MFd(gt2),MVWp,AmpSumFuToFdVWp(:,gt1, gt2),AmpSum2FuToFdVWp(:,gt1, gt2),AmpSqFuToFdVWp(gt1, gt2)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqFuToFdVWp(gt1, gt2) 
  AmpSum2FuToFdVWp = 2._dp*AmpVertexFuToFdVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFuOS(gt1),MFdOS(gt2),MVWpOS,AmpSumFuToFdVWp(:,gt1, gt2),AmpSum2FuToFdVWp(:,gt1, gt2),AmpSqFuToFdVWp(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFu(gt1),MFd(gt2),MVWp,AmpSumFuToFdVWp(:,gt1, gt2),AmpSum2FuToFdVWp(:,gt1, gt2),AmpSqFuToFdVWp(gt1, gt2)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqFuToFdVWp(gt1, gt2) 
  AmpSum2FuToFdVWp = AmpTreeFuToFdVWp + 2._dp*AmpWaveFuToFdVWp + 2._dp*AmpVertexFuToFdVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFuOS(gt1),MFdOS(gt2),MVWpOS,AmpSumFuToFdVWp(:,gt1, gt2),AmpSum2FuToFdVWp(:,gt1, gt2),AmpSqFuToFdVWp(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFu(gt1),MFd(gt2),MVWp,AmpSumFuToFdVWp(:,gt1, gt2),AmpSum2FuToFdVWp(:,gt1, gt2),AmpSqFuToFdVWp(gt1, gt2)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqFuToFdVWp(gt1, gt2) 
 End if 
If (OSkinematics) Then 
  AmpSum2FuToFdVWp = AmpTreeFuToFdVWp
  Call SquareAmp_FtoFV(MFuOS(gt1),MFdOS(gt2),MVWpOS,AmpSumFuToFdVWp(:,gt1, gt2),AmpSum2FuToFdVWp(:,gt1, gt2),AmpSqFuToFdVWp(gt1, gt2)) 
  AmpSqTreeFuToFdVWp(gt1, gt2) = AmpSqFuToFdVWp(gt1, gt2)  
  AmpSum2FuToFdVWp = + 2._dp*AmpWaveFuToFdVWp + 2._dp*AmpVertexFuToFdVWp
  Call SquareAmp_FtoFV(MFuOS(gt1),MFdOS(gt2),MVWpOS,AmpSumFuToFdVWp(:,gt1, gt2),AmpSum2FuToFdVWp(:,gt1, gt2),AmpSqFuToFdVWp(gt1, gt2)) 
  AmpSqFuToFdVWp(gt1, gt2) = AmpSqFuToFdVWp(gt1, gt2) + AmpSqTreeFuToFdVWp(gt1, gt2)  
Else  
  AmpSum2FuToFdVWp = AmpTreeFuToFdVWp
  Call SquareAmp_FtoFV(MFu(gt1),MFd(gt2),MVWp,AmpSumFuToFdVWp(:,gt1, gt2),AmpSum2FuToFdVWp(:,gt1, gt2),AmpSqFuToFdVWp(gt1, gt2)) 
  AmpSqTreeFuToFdVWp(gt1, gt2) = AmpSqFuToFdVWp(gt1, gt2)  
  AmpSum2FuToFdVWp = + 2._dp*AmpWaveFuToFdVWp + 2._dp*AmpVertexFuToFdVWp
  Call SquareAmp_FtoFV(MFu(gt1),MFd(gt2),MVWp,AmpSumFuToFdVWp(:,gt1, gt2),AmpSum2FuToFdVWp(:,gt1, gt2),AmpSqFuToFdVWp(gt1, gt2)) 
  AmpSqFuToFdVWp(gt1, gt2) = AmpSqFuToFdVWp(gt1, gt2) + AmpSqTreeFuToFdVWp(gt1, gt2)  
End if  
Else  
  AmpSqFuToFdVWp(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFuToFdVWp(gt1, gt2).le.0._dp) Then 
  gP1LFu(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFu(gt1,i4) = 1._dp*GammaTPS(MFuOS(gt1),MFdOS(gt2),MVWpOS,helfactor*AmpSqFuToFdVWp(gt1, gt2))
Else 
  gP1LFu(gt1,i4) = 1._dp*GammaTPS(MFu(gt1),MFd(gt2),MVWp,helfactor*AmpSqFuToFdVWp(gt1, gt2))
End if 
If ((Abs(MRPFuToFdVWp(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFuToFdVWp(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFu(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPFuToFdVWp(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFuToFdVWp(gt1, gt2)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPFuToFdVWp(gt1, gt2) + MRGFuToFdVWp(gt1, gt2)) 
  gP1LFu(gt1,i4) = gP1LFu(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPFuToFdVWp(gt1, gt2) + MRGFuToFdVWp(gt1, gt2))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LFu(gt1,i4) 
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
! Fu hh
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_FuToFuhh(cplcFuFuhhL,cplcFuFuhhR,MFu,Mhh,               & 
& MFu2,Mhh2,AmpTreeFuToFuhh)

  Else 
Call Amplitude_Tree_SDdiracDM_FuToFuhh(ZcplcFuFuhhL,ZcplcFuFuhhR,MFu,Mhh,             & 
& MFu2,Mhh2,AmpTreeFuToFuhh)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_FuToFuhh(MLambda,em,gs,cplcFuFuhhL,cplcFuFuhhR,             & 
& MFuOS,MhhOS,MRPFuToFuhh,MRGFuToFuhh)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_FuToFuhh(MLambda,em,gs,ZcplcFuFuhhL,ZcplcFuFuhhR,           & 
& MFuOS,MhhOS,MRPFuToFuhh,MRGFuToFuhh)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_FuToFuhh(MLambda,em,gs,cplcFuFuhhL,cplcFuFuhhR,             & 
& MFu,Mhh,MRPFuToFuhh,MRGFuToFuhh)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_FuToFuhh(MLambda,em,gs,ZcplcFuFuhhL,ZcplcFuFuhhR,           & 
& MFu,Mhh,MRPFuToFuhh,MRGFuToFuhh)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FuToFuhh(cplcFuFuhhL,cplcFuFuhhR,ctcplcFuFuhhL,         & 
& ctcplcFuFuhhR,MFu,MFu2,Mhh,Mhh2,Zfhh,ZfUL,ZfUR,AmpWaveFuToFuhh)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FuToFuhh(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,MVWp,            & 
& MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFuFuAhL,               & 
& cplcFuFuAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,    & 
& cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,              & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,               & 
& cplhhVZVZ,AmpVertexFuToFuhh)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_FuToFuhh(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,              & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFuFuAhL,          & 
& cplcFuFuAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,    & 
& cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,              & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,               & 
& cplhhVZVZ,AmpVertexIRdrFuToFuhh)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FuToFuhh(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,            & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplAhAhhh,cplcFuFuAhL,cplcFuFuAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,            & 
& cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,cplcFuFdVWpR,ZcplcFuFuhhL,ZcplcFuFuhhR,           & 
& cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,            & 
& cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRosFuToFuhh)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FuToFuhh(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,              & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFuFuAhL,          & 
& cplcFuFuAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,    & 
& cplcFuFdVWpR,ZcplcFuFuhhL,ZcplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,            & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,               & 
& cplhhVZVZ,AmpVertexIRosFuToFuhh)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FuToFuhh(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,            & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplAhAhhh,cplcFuFuAhL,cplcFuFuAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,            & 
& cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,             & 
& cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,            & 
& cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRosFuToFuhh)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FuToFuhh(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,              & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFuFuAhL,          & 
& cplcFuFuAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,    & 
& cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,              & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,               & 
& cplhhVZVZ,AmpVertexIRosFuToFuhh)

 End if 
 End if 
AmpVertexFuToFuhh = AmpVertexFuToFuhh -  AmpVertexIRdrFuToFuhh! +  AmpVertexIRosFuToFuhh ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZFuToFuhh=0._dp 
AmpVertexZFuToFuhh=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFuToFuhh(1,gt2,:,:) = AmpWaveZFuToFuhh(1,gt2,:,:)+ZRUUu(gt2,gt1)*AmpWaveFuToFuhh(1,gt1,:,:) 
AmpVertexZFuToFuhh(1,gt2,:,:)= AmpVertexZFuToFuhh(1,gt2,:,:) + ZRUUu(gt2,gt1)*AmpVertexFuToFuhh(1,gt1,:,:) 
AmpWaveZFuToFuhh(2,gt2,:,:) = AmpWaveZFuToFuhh(2,gt2,:,:)+ZRUVuc(gt2,gt1)*AmpWaveFuToFuhh(2,gt1,:,:) 
AmpVertexZFuToFuhh(2,gt2,:,:)= AmpVertexZFuToFuhh(2,gt2,:,:) + ZRUVuc(gt2,gt1)*AmpVertexFuToFuhh(2,gt1,:,:) 
 End Do 
End Do 
AmpWaveFuToFuhh=AmpWaveZFuToFuhh 
AmpVertexFuToFuhh= AmpVertexZFuToFuhh
! Final State 1 
AmpWaveZFuToFuhh=0._dp 
AmpVertexZFuToFuhh=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFuToFuhh(1,:,gt2,:) = AmpWaveZFuToFuhh(1,:,gt2,:)+ZRUVu(gt2,gt1)*AmpWaveFuToFuhh(1,:,gt1,:) 
AmpVertexZFuToFuhh(1,:,gt2,:)= AmpVertexZFuToFuhh(1,:,gt2,:)+ZRUVu(gt2,gt1)*AmpVertexFuToFuhh(1,:,gt1,:) 
AmpWaveZFuToFuhh(2,:,gt2,:) = AmpWaveZFuToFuhh(2,:,gt2,:)+ZRUUuc(gt2,gt1)*AmpWaveFuToFuhh(2,:,gt1,:) 
AmpVertexZFuToFuhh(2,:,gt2,:)= AmpVertexZFuToFuhh(2,:,gt2,:)+ZRUUuc(gt2,gt1)*AmpVertexFuToFuhh(2,:,gt1,:) 
 End Do 
End Do 
AmpWaveFuToFuhh=AmpWaveZFuToFuhh 
AmpVertexFuToFuhh= AmpVertexZFuToFuhh
! Final State 2 
AmpWaveZFuToFuhh=0._dp 
AmpVertexZFuToFuhh=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZFuToFuhh(:,:,:,gt2) = AmpWaveZFuToFuhh(:,:,:,gt2)+ZRUZH(gt2,gt1)*AmpWaveFuToFuhh(:,:,:,gt1) 
AmpVertexZFuToFuhh(:,:,:,gt2)= AmpVertexZFuToFuhh(:,:,:,gt2)+ZRUZH(gt2,gt1)*AmpVertexFuToFuhh(:,:,:,gt1) 
 End Do 
End Do 
AmpWaveFuToFuhh=AmpWaveZFuToFuhh 
AmpVertexFuToFuhh= AmpVertexZFuToFuhh
End if
If (ShiftIRdiv) Then 
AmpVertexFuToFuhh = AmpVertexFuToFuhh  +  AmpVertexIRosFuToFuhh
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fu->Fu hh -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumFuToFuhh = AmpTreeFuToFuhh 
 AmpSum2FuToFuhh = AmpTreeFuToFuhh + 2._dp*AmpWaveFuToFuhh + 2._dp*AmpVertexFuToFuhh  
Else 
 AmpSumFuToFuhh = AmpTreeFuToFuhh + AmpWaveFuToFuhh + AmpVertexFuToFuhh
 AmpSum2FuToFuhh = AmpTreeFuToFuhh + AmpWaveFuToFuhh + AmpVertexFuToFuhh 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumFuToFuhh = AmpTreeFuToFuhh
 AmpSum2FuToFuhh = AmpTreeFuToFuhh 
End if 
Do gt1=1,3
i4 = isave 
  Do gt2=1,3
    Do gt3=1,2
If (((OSkinematics).and.(MFuOS(gt1).gt.(MFuOS(gt2)+MhhOS(gt3)))).or.((.not.OSkinematics).and.(MFu(gt1).gt.(MFu(gt2)+Mhh(gt3))))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2, gt3 
  AmpSum2FuToFuhh = AmpTreeFuToFuhh
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFuOS(gt1),MFuOS(gt2),MhhOS(gt3),AmpSumFuToFuhh(:,gt1, gt2, gt3),AmpSum2FuToFuhh(:,gt1, gt2, gt3),AmpSqFuToFuhh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFu(gt1),MFu(gt2),Mhh(gt3),AmpSumFuToFuhh(:,gt1, gt2, gt3),AmpSum2FuToFuhh(:,gt1, gt2, gt3),AmpSqFuToFuhh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqFuToFuhh(gt1, gt2, gt3) 
  AmpSum2FuToFuhh = 2._dp*AmpWaveFuToFuhh
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFuOS(gt1),MFuOS(gt2),MhhOS(gt3),AmpSumFuToFuhh(:,gt1, gt2, gt3),AmpSum2FuToFuhh(:,gt1, gt2, gt3),AmpSqFuToFuhh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFu(gt1),MFu(gt2),Mhh(gt3),AmpSumFuToFuhh(:,gt1, gt2, gt3),AmpSum2FuToFuhh(:,gt1, gt2, gt3),AmpSqFuToFuhh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqFuToFuhh(gt1, gt2, gt3) 
  AmpSum2FuToFuhh = 2._dp*AmpVertexFuToFuhh
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFuOS(gt1),MFuOS(gt2),MhhOS(gt3),AmpSumFuToFuhh(:,gt1, gt2, gt3),AmpSum2FuToFuhh(:,gt1, gt2, gt3),AmpSqFuToFuhh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFu(gt1),MFu(gt2),Mhh(gt3),AmpSumFuToFuhh(:,gt1, gt2, gt3),AmpSum2FuToFuhh(:,gt1, gt2, gt3),AmpSqFuToFuhh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqFuToFuhh(gt1, gt2, gt3) 
  AmpSum2FuToFuhh = AmpTreeFuToFuhh + 2._dp*AmpWaveFuToFuhh + 2._dp*AmpVertexFuToFuhh
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFuOS(gt1),MFuOS(gt2),MhhOS(gt3),AmpSumFuToFuhh(:,gt1, gt2, gt3),AmpSum2FuToFuhh(:,gt1, gt2, gt3),AmpSqFuToFuhh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFu(gt1),MFu(gt2),Mhh(gt3),AmpSumFuToFuhh(:,gt1, gt2, gt3),AmpSum2FuToFuhh(:,gt1, gt2, gt3),AmpSqFuToFuhh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqFuToFuhh(gt1, gt2, gt3) 
 End if 
If (OSkinematics) Then 
  AmpSum2FuToFuhh = AmpTreeFuToFuhh
  Call SquareAmp_FtoFS(MFuOS(gt1),MFuOS(gt2),MhhOS(gt3),AmpSumFuToFuhh(:,gt1, gt2, gt3),AmpSum2FuToFuhh(:,gt1, gt2, gt3),AmpSqFuToFuhh(gt1, gt2, gt3)) 
  AmpSqTreeFuToFuhh(gt1, gt2, gt3) = AmpSqFuToFuhh(gt1, gt2, gt3)  
  AmpSum2FuToFuhh = + 2._dp*AmpWaveFuToFuhh + 2._dp*AmpVertexFuToFuhh
  Call SquareAmp_FtoFS(MFuOS(gt1),MFuOS(gt2),MhhOS(gt3),AmpSumFuToFuhh(:,gt1, gt2, gt3),AmpSum2FuToFuhh(:,gt1, gt2, gt3),AmpSqFuToFuhh(gt1, gt2, gt3)) 
  AmpSqFuToFuhh(gt1, gt2, gt3) = AmpSqFuToFuhh(gt1, gt2, gt3) + AmpSqTreeFuToFuhh(gt1, gt2, gt3)  
Else  
  AmpSum2FuToFuhh = AmpTreeFuToFuhh
  Call SquareAmp_FtoFS(MFu(gt1),MFu(gt2),Mhh(gt3),AmpSumFuToFuhh(:,gt1, gt2, gt3),AmpSum2FuToFuhh(:,gt1, gt2, gt3),AmpSqFuToFuhh(gt1, gt2, gt3)) 
  AmpSqTreeFuToFuhh(gt1, gt2, gt3) = AmpSqFuToFuhh(gt1, gt2, gt3)  
  AmpSum2FuToFuhh = + 2._dp*AmpWaveFuToFuhh + 2._dp*AmpVertexFuToFuhh
  Call SquareAmp_FtoFS(MFu(gt1),MFu(gt2),Mhh(gt3),AmpSumFuToFuhh(:,gt1, gt2, gt3),AmpSum2FuToFuhh(:,gt1, gt2, gt3),AmpSqFuToFuhh(gt1, gt2, gt3)) 
  AmpSqFuToFuhh(gt1, gt2, gt3) = AmpSqFuToFuhh(gt1, gt2, gt3) + AmpSqTreeFuToFuhh(gt1, gt2, gt3)  
End if  
Else  
  AmpSqFuToFuhh(gt1, gt2, gt3) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFuToFuhh(gt1, gt2, gt3).le.0._dp) Then 
  gP1LFu(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFu(gt1,i4) = 1._dp*GammaTPS(MFuOS(gt1),MFuOS(gt2),MhhOS(gt3),helfactor*AmpSqFuToFuhh(gt1, gt2, gt3))
Else 
  gP1LFu(gt1,i4) = 1._dp*GammaTPS(MFu(gt1),MFu(gt2),Mhh(gt3),helfactor*AmpSqFuToFuhh(gt1, gt2, gt3))
End if 
If ((Abs(MRPFuToFuhh(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGFuToFuhh(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFu(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPFuToFuhh(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGFuToFuhh(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPFuToFuhh(gt1, gt2, gt3) + MRGFuToFuhh(gt1, gt2, gt3)) 
  gP1LFu(gt1,i4) = gP1LFu(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPFuToFuhh(gt1, gt2, gt3) + MRGFuToFuhh(gt1, gt2, gt3))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LFu(gt1,i4) 
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
! Fu VZ
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_FuToFuVZ(cplcFuFuVZL,cplcFuFuVZR,MFu,MVZ,               & 
& MFu2,MVZ2,AmpTreeFuToFuVZ)

  Else 
Call Amplitude_Tree_SDdiracDM_FuToFuVZ(ZcplcFuFuVZL,ZcplcFuFuVZR,MFu,MVZ,             & 
& MFu2,MVZ2,AmpTreeFuToFuVZ)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_FuToFuVZ(MLambda,em,gs,cplcFuFuVZL,cplcFuFuVZR,             & 
& MFuOS,MVZOS,MRPFuToFuVZ,MRGFuToFuVZ)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_FuToFuVZ(MLambda,em,gs,ZcplcFuFuVZL,ZcplcFuFuVZR,           & 
& MFuOS,MVZOS,MRPFuToFuVZ,MRGFuToFuVZ)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_FuToFuVZ(MLambda,em,gs,cplcFuFuVZL,cplcFuFuVZR,             & 
& MFu,MVZ,MRPFuToFuVZ,MRGFuToFuVZ)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_FuToFuVZ(MLambda,em,gs,ZcplcFuFuVZL,ZcplcFuFuVZR,           & 
& MFu,MVZ,MRPFuToFuVZ,MRGFuToFuVZ)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FuToFuVZ(cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,           & 
& cplcFuFuVZR,ctcplcFuFuVPL,ctcplcFuFuVPR,ctcplcFuFuVZL,ctcplcFuFuVZR,MFu,               & 
& MFu2,MVP,MVP2,MVZ,MVZ2,ZfUL,ZfUR,ZfVPVZ,ZfVZ,AmpWaveFuToFuVZ)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FuToFuVZ(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,MVWp,            & 
& MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFuFuAhL,cplcFuFuAhR,             & 
& cplAhhhVZ,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,               & 
& cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,               & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,               & 
& AmpVertexFuToFuVZ)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_FuToFuVZ(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,              & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFuFuAhL,cplcFuFuAhR,        & 
& cplAhhhVZ,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,               & 
& cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,               & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,               & 
& AmpVertexIRdrFuToFuVZ)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FuToFuVZ(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,            & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplcFuFuAhL,cplcFuFuAhR,cplAhhhVZ,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,         & 
& cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,              & 
& cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,ZcplcFuFuVZL,ZcplcFuFuVZR,cplcFdFucHpL,            & 
& cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,             & 
& cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFuToFuVZ)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FuToFuVZ(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,              & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFuFuAhL,cplcFuFuAhR,        & 
& cplAhhhVZ,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,               & 
& cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,               & 
& cplcFuFuVPR,ZcplcFuFuVZL,ZcplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,         & 
& cplcFdFucVWpR,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,               & 
& AmpVertexIRosFuToFuVZ)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FuToFuVZ(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,            & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplcFuFuAhL,cplcFuFuAhR,cplAhhhVZ,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,         & 
& cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,              & 
& cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,              & 
& cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,             & 
& cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFuToFuVZ)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FuToFuVZ(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,              & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFuFuAhL,cplcFuFuAhR,        & 
& cplAhhhVZ,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,               & 
& cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,               & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,               & 
& AmpVertexIRosFuToFuVZ)

 End if 
 End if 
AmpVertexFuToFuVZ = AmpVertexFuToFuVZ -  AmpVertexIRdrFuToFuVZ! +  AmpVertexIRosFuToFuVZ ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZFuToFuVZ=0._dp 
AmpVertexZFuToFuVZ=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFuToFuVZ(1,gt2,:) = AmpWaveZFuToFuVZ(1,gt2,:)+ZRUVuc(gt2,gt1)*AmpWaveFuToFuVZ(1,gt1,:) 
AmpVertexZFuToFuVZ(1,gt2,:)= AmpVertexZFuToFuVZ(1,gt2,:) + ZRUVuc(gt2,gt1)*AmpVertexFuToFuVZ(1,gt1,:) 
AmpWaveZFuToFuVZ(2,gt2,:) = AmpWaveZFuToFuVZ(2,gt2,:)+ZRUUu(gt2,gt1)*AmpWaveFuToFuVZ(2,gt1,:) 
AmpVertexZFuToFuVZ(2,gt2,:)= AmpVertexZFuToFuVZ(2,gt2,:) + ZRUUu(gt2,gt1)*AmpVertexFuToFuVZ(2,gt1,:) 
AmpWaveZFuToFuVZ(3,gt2,:) = AmpWaveZFuToFuVZ(3,gt2,:)+ZRUVuc(gt2,gt1)*AmpWaveFuToFuVZ(3,gt1,:) 
AmpVertexZFuToFuVZ(3,gt2,:)= AmpVertexZFuToFuVZ(3,gt2,:) + ZRUVuc(gt2,gt1)*AmpVertexFuToFuVZ(3,gt1,:) 
AmpWaveZFuToFuVZ(4,gt2,:) = AmpWaveZFuToFuVZ(4,gt2,:)+ZRUUu(gt2,gt1)*AmpWaveFuToFuVZ(4,gt1,:) 
AmpVertexZFuToFuVZ(4,gt2,:)= AmpVertexZFuToFuVZ(4,gt2,:) + ZRUUu(gt2,gt1)*AmpVertexFuToFuVZ(4,gt1,:) 
 End Do 
End Do 
AmpWaveFuToFuVZ=AmpWaveZFuToFuVZ 
AmpVertexFuToFuVZ= AmpVertexZFuToFuVZ
! Final State 1 
AmpWaveZFuToFuVZ=0._dp 
AmpVertexZFuToFuVZ=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFuToFuVZ(1,:,gt2) = AmpWaveZFuToFuVZ(1,:,gt2)+ZRUVu(gt2,gt1)*AmpWaveFuToFuVZ(1,:,gt1) 
AmpVertexZFuToFuVZ(1,:,gt2)= AmpVertexZFuToFuVZ(1,:,gt2)+ZRUVu(gt2,gt1)*AmpVertexFuToFuVZ(1,:,gt1) 
AmpWaveZFuToFuVZ(2,:,gt2) = AmpWaveZFuToFuVZ(2,:,gt2)+ZRUUuc(gt2,gt1)*AmpWaveFuToFuVZ(2,:,gt1) 
AmpVertexZFuToFuVZ(2,:,gt2)= AmpVertexZFuToFuVZ(2,:,gt2)+ZRUUuc(gt2,gt1)*AmpVertexFuToFuVZ(2,:,gt1) 
AmpWaveZFuToFuVZ(3,:,gt2) = AmpWaveZFuToFuVZ(3,:,gt2)+ZRUVu(gt2,gt1)*AmpWaveFuToFuVZ(3,:,gt1) 
AmpVertexZFuToFuVZ(3,:,gt2)= AmpVertexZFuToFuVZ(3,:,gt2)+ZRUVu(gt2,gt1)*AmpVertexFuToFuVZ(3,:,gt1) 
AmpWaveZFuToFuVZ(4,:,gt2) = AmpWaveZFuToFuVZ(4,:,gt2)+ZRUUuc(gt2,gt1)*AmpWaveFuToFuVZ(4,:,gt1) 
AmpVertexZFuToFuVZ(4,:,gt2)= AmpVertexZFuToFuVZ(4,:,gt2)+ZRUUuc(gt2,gt1)*AmpVertexFuToFuVZ(4,:,gt1) 
 End Do 
End Do 
AmpWaveFuToFuVZ=AmpWaveZFuToFuVZ 
AmpVertexFuToFuVZ= AmpVertexZFuToFuVZ
End if
If (ShiftIRdiv) Then 
AmpVertexFuToFuVZ = AmpVertexFuToFuVZ  +  AmpVertexIRosFuToFuVZ
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fu->Fu VZ -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumFuToFuVZ = AmpTreeFuToFuVZ 
 AmpSum2FuToFuVZ = AmpTreeFuToFuVZ + 2._dp*AmpWaveFuToFuVZ + 2._dp*AmpVertexFuToFuVZ  
Else 
 AmpSumFuToFuVZ = AmpTreeFuToFuVZ + AmpWaveFuToFuVZ + AmpVertexFuToFuVZ
 AmpSum2FuToFuVZ = AmpTreeFuToFuVZ + AmpWaveFuToFuVZ + AmpVertexFuToFuVZ 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumFuToFuVZ = AmpTreeFuToFuVZ
 AmpSum2FuToFuVZ = AmpTreeFuToFuVZ 
End if 
Do gt1=1,3
i4 = isave 
  Do gt2=1,3
If (((OSkinematics).and.(MFuOS(gt1).gt.(MFuOS(gt2)+MVZOS))).or.((.not.OSkinematics).and.(MFu(gt1).gt.(MFu(gt2)+MVZ)))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2 
  AmpSum2FuToFuVZ = AmpTreeFuToFuVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFuOS(gt1),MFuOS(gt2),MVZOS,AmpSumFuToFuVZ(:,gt1, gt2),AmpSum2FuToFuVZ(:,gt1, gt2),AmpSqFuToFuVZ(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFu(gt1),MFu(gt2),MVZ,AmpSumFuToFuVZ(:,gt1, gt2),AmpSum2FuToFuVZ(:,gt1, gt2),AmpSqFuToFuVZ(gt1, gt2)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqFuToFuVZ(gt1, gt2) 
  AmpSum2FuToFuVZ = 2._dp*AmpWaveFuToFuVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFuOS(gt1),MFuOS(gt2),MVZOS,AmpSumFuToFuVZ(:,gt1, gt2),AmpSum2FuToFuVZ(:,gt1, gt2),AmpSqFuToFuVZ(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFu(gt1),MFu(gt2),MVZ,AmpSumFuToFuVZ(:,gt1, gt2),AmpSum2FuToFuVZ(:,gt1, gt2),AmpSqFuToFuVZ(gt1, gt2)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqFuToFuVZ(gt1, gt2) 
  AmpSum2FuToFuVZ = 2._dp*AmpVertexFuToFuVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFuOS(gt1),MFuOS(gt2),MVZOS,AmpSumFuToFuVZ(:,gt1, gt2),AmpSum2FuToFuVZ(:,gt1, gt2),AmpSqFuToFuVZ(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFu(gt1),MFu(gt2),MVZ,AmpSumFuToFuVZ(:,gt1, gt2),AmpSum2FuToFuVZ(:,gt1, gt2),AmpSqFuToFuVZ(gt1, gt2)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqFuToFuVZ(gt1, gt2) 
  AmpSum2FuToFuVZ = AmpTreeFuToFuVZ + 2._dp*AmpWaveFuToFuVZ + 2._dp*AmpVertexFuToFuVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFuOS(gt1),MFuOS(gt2),MVZOS,AmpSumFuToFuVZ(:,gt1, gt2),AmpSum2FuToFuVZ(:,gt1, gt2),AmpSqFuToFuVZ(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFu(gt1),MFu(gt2),MVZ,AmpSumFuToFuVZ(:,gt1, gt2),AmpSum2FuToFuVZ(:,gt1, gt2),AmpSqFuToFuVZ(gt1, gt2)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqFuToFuVZ(gt1, gt2) 
 End if 
If (OSkinematics) Then 
  AmpSum2FuToFuVZ = AmpTreeFuToFuVZ
  Call SquareAmp_FtoFV(MFuOS(gt1),MFuOS(gt2),MVZOS,AmpSumFuToFuVZ(:,gt1, gt2),AmpSum2FuToFuVZ(:,gt1, gt2),AmpSqFuToFuVZ(gt1, gt2)) 
  AmpSqTreeFuToFuVZ(gt1, gt2) = AmpSqFuToFuVZ(gt1, gt2)  
  AmpSum2FuToFuVZ = + 2._dp*AmpWaveFuToFuVZ + 2._dp*AmpVertexFuToFuVZ
  Call SquareAmp_FtoFV(MFuOS(gt1),MFuOS(gt2),MVZOS,AmpSumFuToFuVZ(:,gt1, gt2),AmpSum2FuToFuVZ(:,gt1, gt2),AmpSqFuToFuVZ(gt1, gt2)) 
  AmpSqFuToFuVZ(gt1, gt2) = AmpSqFuToFuVZ(gt1, gt2) + AmpSqTreeFuToFuVZ(gt1, gt2)  
Else  
  AmpSum2FuToFuVZ = AmpTreeFuToFuVZ
  Call SquareAmp_FtoFV(MFu(gt1),MFu(gt2),MVZ,AmpSumFuToFuVZ(:,gt1, gt2),AmpSum2FuToFuVZ(:,gt1, gt2),AmpSqFuToFuVZ(gt1, gt2)) 
  AmpSqTreeFuToFuVZ(gt1, gt2) = AmpSqFuToFuVZ(gt1, gt2)  
  AmpSum2FuToFuVZ = + 2._dp*AmpWaveFuToFuVZ + 2._dp*AmpVertexFuToFuVZ
  Call SquareAmp_FtoFV(MFu(gt1),MFu(gt2),MVZ,AmpSumFuToFuVZ(:,gt1, gt2),AmpSum2FuToFuVZ(:,gt1, gt2),AmpSqFuToFuVZ(gt1, gt2)) 
  AmpSqFuToFuVZ(gt1, gt2) = AmpSqFuToFuVZ(gt1, gt2) + AmpSqTreeFuToFuVZ(gt1, gt2)  
End if  
Else  
  AmpSqFuToFuVZ(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFuToFuVZ(gt1, gt2).le.0._dp) Then 
  gP1LFu(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFu(gt1,i4) = 1._dp*GammaTPS(MFuOS(gt1),MFuOS(gt2),MVZOS,helfactor*AmpSqFuToFuVZ(gt1, gt2))
Else 
  gP1LFu(gt1,i4) = 1._dp*GammaTPS(MFu(gt1),MFu(gt2),MVZ,helfactor*AmpSqFuToFuVZ(gt1, gt2))
End if 
If ((Abs(MRPFuToFuVZ(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFuToFuVZ(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFu(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPFuToFuVZ(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFuToFuVZ(gt1, gt2)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPFuToFuVZ(gt1, gt2) + MRGFuToFuVZ(gt1, gt2)) 
  gP1LFu(gt1,i4) = gP1LFu(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPFuToFuVZ(gt1, gt2) + MRGFuToFuVZ(gt1, gt2))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LFu(gt1,i4) 
  End if 
End if 
End if 
i4=i4+1

  End do
If (gt1.eq.3) isave = i4 
End do
End If 
!---------------- 
! Fu VG
!---------------- 

If (LoopInducedDecaysOS) Then 


!Self-energy Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_WAVE_SDdiracDM_FuToFuVG(ZcplcFuFuVGL,ZcplcFuFuVGR,ctcplcFuFuVGL,       & 
& ctcplcFuFuVGR,MFuOS,MFu2OS,MVG,MVG2,ZfUL,ZfUR,ZfVG,AmpWaveFuToFuVG)

 Else 
Call Amplitude_WAVE_SDdiracDM_FuToFuVG(cplcFuFuVGL,cplcFuFuVGR,ctcplcFuFuVGL,         & 
& ctcplcFuFuVGR,MFuOS,MFu2OS,MVG,MVG2,ZfUL,ZfUR,ZfVG,AmpWaveFuToFuVG)

 End if 


!Vertex Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_VERTEX_SDdiracDM_FuToFuVG(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,               & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,ZcplcFuFuAhL,ZcplcFuFuAhR,ZcplcFuFdHpL,ZcplcFuFdHpR,ZcplcFdFdVGL,               & 
& ZcplcFdFdVGR,ZcplcFuFdVWpL,ZcplcFuFdVWpR,ZcplcFuFuhhL,ZcplcFuFuhhR,ZcplcFuFuVGL,       & 
& ZcplcFuFuVGR,ZcplcFuFuVPL,ZcplcFuFuVPR,ZcplcFuFuVZL,ZcplcFuFuVZR,ZcplcFdFucHpL,        & 
& ZcplcFdFucHpR,ZcplcFdFucVWpL,ZcplcFdFucVWpR,ZcplVGVGVG,AmpVertexFuToFuVG)

 Else 
Call Amplitude_VERTEX_SDdiracDM_FuToFuVG(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,               & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplcFuFuAhL,cplcFuFuAhR,cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,        & 
& cplcFuFdVWpL,cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,             & 
& cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,             & 
& cplcFdFucVWpL,cplcFdFucVWpR,cplVGVGVG,AmpVertexFuToFuVG)

 End if 
Else 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FuToFuVG(cplcFuFuVGL,cplcFuFuVGR,ctcplcFuFuVGL,         & 
& ctcplcFuFuVGR,MFu,MFu2,MVG,MVG2,ZfUL,ZfUR,ZfVG,AmpWaveFuToFuVG)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FuToFuVG(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,MVWp,            & 
& MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFuFuAhL,cplcFuFuAhR,             & 
& cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFuFdVWpL,cplcFuFdVWpR,             & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,               & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,         & 
& cplVGVGVG,AmpVertexFuToFuVG)

End if 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fu->Fu VG -----------------------" 
End if 
If (OnlyTreeLevelContributions) Then 
 AmpSumFuToFuVG = 0._dp 
 AmpSum2FuToFuVG = 0._dp  
Else 
 AmpSumFuToFuVG = AmpVertexFuToFuVG + AmpWaveFuToFuVG
 AmpSum2FuToFuVG = AmpVertexFuToFuVG + AmpWaveFuToFuVG 
End If 
Do gt1=1,3
i4 = isave 
  Do gt2=1,3
If (((OSkinematics).and.(MFuOS(gt1).gt.(MFuOS(gt2)+0.))).or.((.not.OSkinematics).and.(MFu(gt1).gt.(MFu(gt2)+MVG)))) Then 
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFuOS(gt1),MFuOS(gt2),0._dp,AmpSumFuToFuVG(:,gt1, gt2),AmpSum2FuToFuVG(:,gt1, gt2),AmpSqFuToFuVG(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFu(gt1),MFu(gt2),MVG,AmpSumFuToFuVG(:,gt1, gt2),AmpSum2FuToFuVG(:,gt1, gt2),AmpSqFuToFuVG(gt1, gt2)) 
End if  
Else  
  AmpSqFuToFuVG(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFuToFuVG(gt1, gt2).le.0._dp) Then 
  gP1LFu(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFu(gt1,i4) = 4._dp/3._dp*GammaTPS(MFuOS(gt1),MFuOS(gt2),0._dp,helfactor*AmpSqFuToFuVG(gt1, gt2))
Else 
  gP1LFu(gt1,i4) = 4._dp/3._dp*GammaTPS(MFu(gt1),MFu(gt2),MVG,helfactor*AmpSqFuToFuVG(gt1, gt2))
End if 
If ((Abs(MRPFuToFuVG(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFuToFuVG(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFu(gt1,i4) 
End if 
i4=i4+1

  End do
If (gt1.eq.3) isave = i4 
End do
!---------------- 
! Fu VP
!---------------- 

If (LoopInducedDecaysOS) Then 


!Self-energy Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_WAVE_SDdiracDM_FuToFuVP(ZcplcFuFuVPL,ZcplcFuFuVPR,ZcplcFuFuVZL,        & 
& ZcplcFuFuVZR,ctcplcFuFuVPL,ctcplcFuFuVPR,ctcplcFuFuVZL,ctcplcFuFuVZR,MFuOS,            & 
& MFu2OS,MVP,MVP2,ZfUL,ZfUR,ZfVP,ZfVZVP,AmpWaveFuToFuVP)

 Else 
Call Amplitude_WAVE_SDdiracDM_FuToFuVP(cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,           & 
& cplcFuFuVZR,ctcplcFuFuVPL,ctcplcFuFuVPR,ctcplcFuFuVZL,ctcplcFuFuVZR,MFuOS,             & 
& MFu2OS,MVP,MVP2,ZfUL,ZfUR,ZfVP,ZfVZVP,AmpWaveFuToFuVP)

 End if 


!Vertex Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_VERTEX_SDdiracDM_FuToFuVP(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,               & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,ZcplcFuFuAhL,ZcplcFuFuAhR,ZcplcFuFdHpL,ZcplcFuFdHpR,ZcplcFdFdVPL,               & 
& ZcplcFdFdVPR,ZcplcFuFdVWpL,ZcplcFuFdVWpR,ZcplcFuFuhhL,ZcplcFuFuhhR,ZcplcFuFuVGL,       & 
& ZcplcFuFuVGR,ZcplcFuFuVPL,ZcplcFuFuVPR,ZcplcFuFuVZL,ZcplcFuFuVZR,ZcplcFdFucHpL,        & 
& ZcplcFdFucHpR,ZcplcFdFucVWpL,ZcplcFdFucVWpR,ZcplHpcHpVP,ZcplHpcVWpVP,ZcplcHpVPVWp,     & 
& ZcplcVWpVPVWp,AmpVertexFuToFuVP)

 Else 
Call Amplitude_VERTEX_SDdiracDM_FuToFuVP(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,               & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplcFuFuAhL,cplcFuFuAhR,cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVPL,cplcFdFdVPR,        & 
& cplcFuFdVWpL,cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,             & 
& cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,             & 
& cplcFdFucVWpL,cplcFdFucVWpR,cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,           & 
& AmpVertexFuToFuVP)

 End if 
Else 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FuToFuVP(cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,           & 
& cplcFuFuVZR,ctcplcFuFuVPL,ctcplcFuFuVPR,ctcplcFuFuVZL,ctcplcFuFuVZR,MFu,               & 
& MFu2,MVP,MVP2,ZfUL,ZfUR,ZfVP,ZfVZVP,AmpWaveFuToFuVP)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FuToFuVP(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,MVWp,            & 
& MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFuFuAhL,cplcFuFuAhR,             & 
& cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,             & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,               & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,         & 
& cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,AmpVertexFuToFuVP)

End if 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fu->Fu VP -----------------------" 
End if 
If (OnlyTreeLevelContributions) Then 
 AmpSumFuToFuVP = 0._dp 
 AmpSum2FuToFuVP = 0._dp  
Else 
 AmpSumFuToFuVP = AmpVertexFuToFuVP + AmpWaveFuToFuVP
 AmpSum2FuToFuVP = AmpVertexFuToFuVP + AmpWaveFuToFuVP 
End If 
Do gt1=1,3
i4 = isave 
  Do gt2=1,3
If (((OSkinematics).and.(MFuOS(gt1).gt.(MFuOS(gt2)+0.))).or.((.not.OSkinematics).and.(MFu(gt1).gt.(MFu(gt2)+MVP)))) Then 
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFuOS(gt1),MFuOS(gt2),0._dp,AmpSumFuToFuVP(:,gt1, gt2),AmpSum2FuToFuVP(:,gt1, gt2),AmpSqFuToFuVP(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFu(gt1),MFu(gt2),MVP,AmpSumFuToFuVP(:,gt1, gt2),AmpSum2FuToFuVP(:,gt1, gt2),AmpSqFuToFuVP(gt1, gt2)) 
End if  
Else  
  AmpSqFuToFuVP(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFuToFuVP(gt1, gt2).le.0._dp) Then 
  gP1LFu(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFu(gt1,i4) = 1._dp*GammaTPS(MFuOS(gt1),MFuOS(gt2),0._dp,helfactor*AmpSqFuToFuVP(gt1, gt2))
Else 
  gP1LFu(gt1,i4) = 1._dp*GammaTPS(MFu(gt1),MFu(gt2),MVP,helfactor*AmpSqFuToFuVP(gt1, gt2))
End if 
If ((Abs(MRPFuToFuVP(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFuToFuVP(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFu(gt1,i4) 
End if 
i4=i4+1

  End do
If (gt1.eq.3) isave = i4 
End do
End Subroutine OneLoopDecay_Fu

End Module Wrapper_OneLoopDecay_Fu_SDdiracDM
