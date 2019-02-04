! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:22 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module Wrapper_OneLoopDecay_Fd_SDdiracDM
Use Model_Data_SDdiracDM 
Use Kinematics 
Use OneLoopDecay_Fd_SDdiracDM 
Use Control 
Use Settings 

 
Contains

 
Subroutine OneLoopDecay_Fd(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,MFeOS,              & 
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
& cplVGVGVG,ctcplcFdFdAhL,ctcplcFdFdAhR,ctcplcFdFdhhL,ctcplcFdFdhhR,ctcplcFdFdVGL,       & 
& ctcplcFdFdVGR,ctcplcFdFdVPL,ctcplcFdFdVPR,ctcplcFdFdVZL,ctcplcFdFdVZR,ctcplcFdFucHpL,  & 
& ctcplcFdFucHpR,ctcplcFdFucVWpL,ctcplcFdFucVWpR,GcplcFdFucHpL,GcplcFdFucHpR,            & 
& GcplcHpVPVWp,GcplHpcVWpVP,GosZcplcFdFucHpL,GosZcplcFdFucHpR,GosZcplcHpVPVWp,           & 
& GosZcplHpcVWpVP,GZcplcFdFucHpL,GZcplcFdFucHpR,GZcplcHpVPVWp,GZcplHpcVWpVP,             & 
& ZcplcFdFdAhL,ZcplcFdFdAhR,ZcplcFdFdhhL,ZcplcFdFdhhR,ZcplcFdFdVGL,ZcplcFdFdVGR,         & 
& ZcplcFdFdVPL,ZcplcFdFdVPR,ZcplcFdFdVZL,ZcplcFdFdVZR,ZcplcFdFucHpL,ZcplcFdFucHpR,       & 
& ZcplcFdFucVWpL,ZcplcFdFucVWpR,ZcplcFuFdHpL,ZcplcFuFdHpR,ZcplcFuFdVWpL,ZcplcFuFdVWpR,   & 
& ZcplcFuFuVGL,ZcplcFuFuVGR,ZcplcFuFuVPL,ZcplcFuFuVPR,ZcplcHpVPVWp,ZcplcVWpVPVWp,        & 
& ZcplHpcHpVP,ZcplHpcVWpVP,ZcplVGVGVG,ZRUZH,ZRUVd,ZRUUd,ZRUVu,ZRUUu,ZRUVe,               & 
& ZRUUe,ZRUVv,ZRUVvr,ZRUXV,ZRUXU,ZRUVSs,MLambda,em,gs,deltaM,kont,gP1LFd)

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
& cplHpcVWpVZ,cplVGVGVG,ctcplcFdFdAhL(3,3),ctcplcFdFdAhR(3,3),ctcplcFdFdhhL(3,3,2),      & 
& ctcplcFdFdhhR(3,3,2),ctcplcFdFdVGL(3,3),ctcplcFdFdVGR(3,3),ctcplcFdFdVPL(3,3),         & 
& ctcplcFdFdVPR(3,3),ctcplcFdFdVZL(3,3),ctcplcFdFdVZR(3,3),ctcplcFdFucHpL(3,3),          & 
& ctcplcFdFucHpR(3,3),ctcplcFdFucVWpL(3,3),ctcplcFdFucVWpR(3,3),GcplcFdFucHpL(3,3),      & 
& GcplcFdFucHpR(3,3),GcplcHpVPVWp,GcplHpcVWpVP,GosZcplcFdFucHpL(3,3),GosZcplcFdFucHpR(3,3),& 
& GosZcplcHpVPVWp,GosZcplHpcVWpVP,GZcplcFdFucHpL(3,3),GZcplcFdFucHpR(3,3),               & 
& GZcplcHpVPVWp,GZcplHpcVWpVP,ZcplcFdFdAhL(3,3),ZcplcFdFdAhR(3,3),ZcplcFdFdhhL(3,3,2),   & 
& ZcplcFdFdhhR(3,3,2),ZcplcFdFdVGL(3,3),ZcplcFdFdVGR(3,3),ZcplcFdFdVPL(3,3),             & 
& ZcplcFdFdVPR(3,3),ZcplcFdFdVZL(3,3),ZcplcFdFdVZR(3,3),ZcplcFdFucHpL(3,3),              & 
& ZcplcFdFucHpR(3,3),ZcplcFdFucVWpL(3,3),ZcplcFdFucVWpR(3,3),ZcplcFuFdHpL(3,3),          & 
& ZcplcFuFdHpR(3,3),ZcplcFuFdVWpL(3,3),ZcplcFuFdVWpR(3,3),ZcplcFuFuVGL(3,3),             & 
& ZcplcFuFuVGR(3,3),ZcplcFuFuVPL(3,3),ZcplcFuFuVPR(3,3),ZcplcHpVPVWp,ZcplcVWpVPVWp,      & 
& ZcplHpcHpVP,ZcplHpcVWpVP,ZcplVGVGVG

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
Real(dp), Intent(out) :: gP1LFd(3,18) 
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
Real(dp) :: MRPFdToFdhh(3,3,2),MRGFdToFdhh(3,3,2), MRPZFdToFdhh(3,3,2),MRGZFdToFdhh(3,3,2) 
Real(dp) :: MVPFdToFdhh(3,3,2) 
Real(dp) :: RMsqTreeFdToFdhh(3,3,2),RMsqWaveFdToFdhh(3,3,2),RMsqVertexFdToFdhh(3,3,2) 
Complex(dp) :: AmpTreeFdToFdhh(2,3,3,2),AmpWaveFdToFdhh(2,3,3,2)=(0._dp,0._dp),AmpVertexFdToFdhh(2,3,3,2)& 
 & ,AmpVertexIRosFdToFdhh(2,3,3,2),AmpVertexIRdrFdToFdhh(2,3,3,2), AmpSumFdToFdhh(2,3,3,2), AmpSum2FdToFdhh(2,3,3,2) 
Complex(dp) :: AmpTreeZFdToFdhh(2,3,3,2),AmpWaveZFdToFdhh(2,3,3,2),AmpVertexZFdToFdhh(2,3,3,2) 
Real(dp) :: AmpSqFdToFdhh(3,3,2),  AmpSqTreeFdToFdhh(3,3,2) 
Real(dp) :: MRPFdToFdVZ(3,3),MRGFdToFdVZ(3,3), MRPZFdToFdVZ(3,3),MRGZFdToFdVZ(3,3) 
Real(dp) :: MVPFdToFdVZ(3,3) 
Real(dp) :: RMsqTreeFdToFdVZ(3,3),RMsqWaveFdToFdVZ(3,3),RMsqVertexFdToFdVZ(3,3) 
Complex(dp) :: AmpTreeFdToFdVZ(4,3,3),AmpWaveFdToFdVZ(4,3,3)=(0._dp,0._dp),AmpVertexFdToFdVZ(4,3,3)& 
 & ,AmpVertexIRosFdToFdVZ(4,3,3),AmpVertexIRdrFdToFdVZ(4,3,3), AmpSumFdToFdVZ(4,3,3), AmpSum2FdToFdVZ(4,3,3) 
Complex(dp) :: AmpTreeZFdToFdVZ(4,3,3),AmpWaveZFdToFdVZ(4,3,3),AmpVertexZFdToFdVZ(4,3,3) 
Real(dp) :: AmpSqFdToFdVZ(3,3),  AmpSqTreeFdToFdVZ(3,3) 
Real(dp) :: MRPFdToFucVWp(3,3),MRGFdToFucVWp(3,3), MRPZFdToFucVWp(3,3),MRGZFdToFucVWp(3,3) 
Real(dp) :: MVPFdToFucVWp(3,3) 
Real(dp) :: RMsqTreeFdToFucVWp(3,3),RMsqWaveFdToFucVWp(3,3),RMsqVertexFdToFucVWp(3,3) 
Complex(dp) :: AmpTreeFdToFucVWp(4,3,3),AmpWaveFdToFucVWp(4,3,3)=(0._dp,0._dp),AmpVertexFdToFucVWp(4,3,3)& 
 & ,AmpVertexIRosFdToFucVWp(4,3,3),AmpVertexIRdrFdToFucVWp(4,3,3), AmpSumFdToFucVWp(4,3,3), AmpSum2FdToFucVWp(4,3,3) 
Complex(dp) :: AmpTreeZFdToFucVWp(4,3,3),AmpWaveZFdToFucVWp(4,3,3),AmpVertexZFdToFucVWp(4,3,3) 
Real(dp) :: AmpSqFdToFucVWp(3,3),  AmpSqTreeFdToFucVWp(3,3) 
Real(dp) :: MRPFdToFdVG(3,3),MRGFdToFdVG(3,3), MRPZFdToFdVG(3,3),MRGZFdToFdVG(3,3) 
Real(dp) :: MVPFdToFdVG(3,3) 
Real(dp) :: RMsqTreeFdToFdVG(3,3),RMsqWaveFdToFdVG(3,3),RMsqVertexFdToFdVG(3,3) 
Complex(dp) :: AmpTreeFdToFdVG(4,3,3),AmpWaveFdToFdVG(4,3,3)=(0._dp,0._dp),AmpVertexFdToFdVG(4,3,3)& 
 & ,AmpVertexIRosFdToFdVG(4,3,3),AmpVertexIRdrFdToFdVG(4,3,3), AmpSumFdToFdVG(4,3,3), AmpSum2FdToFdVG(4,3,3) 
Complex(dp) :: AmpTreeZFdToFdVG(4,3,3),AmpWaveZFdToFdVG(4,3,3),AmpVertexZFdToFdVG(4,3,3) 
Real(dp) :: AmpSqFdToFdVG(3,3),  AmpSqTreeFdToFdVG(3,3) 
Real(dp) :: MRPFdToFdVP(3,3),MRGFdToFdVP(3,3), MRPZFdToFdVP(3,3),MRGZFdToFdVP(3,3) 
Real(dp) :: MVPFdToFdVP(3,3) 
Real(dp) :: RMsqTreeFdToFdVP(3,3),RMsqWaveFdToFdVP(3,3),RMsqVertexFdToFdVP(3,3) 
Complex(dp) :: AmpTreeFdToFdVP(4,3,3),AmpWaveFdToFdVP(4,3,3)=(0._dp,0._dp),AmpVertexFdToFdVP(4,3,3)& 
 & ,AmpVertexIRosFdToFdVP(4,3,3),AmpVertexIRdrFdToFdVP(4,3,3), AmpSumFdToFdVP(4,3,3), AmpSum2FdToFdVP(4,3,3) 
Complex(dp) :: AmpTreeZFdToFdVP(4,3,3),AmpWaveZFdToFdVP(4,3,3),AmpVertexZFdToFdVP(4,3,3) 
Real(dp) :: AmpSqFdToFdVP(3,3),  AmpSqTreeFdToFdVP(3,3) 
Write(*,*) "Calculating one-loop decays of Fd " 
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
! Fd hh
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_FdToFdhh(cplcFdFdhhL,cplcFdFdhhR,MFd,Mhh,               & 
& MFd2,Mhh2,AmpTreeFdToFdhh)

  Else 
Call Amplitude_Tree_SDdiracDM_FdToFdhh(ZcplcFdFdhhL,ZcplcFdFdhhR,MFd,Mhh,             & 
& MFd2,Mhh2,AmpTreeFdToFdhh)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_FdToFdhh(MLambda,em,gs,cplcFdFdhhL,cplcFdFdhhR,             & 
& MFdOS,MhhOS,MRPFdToFdhh,MRGFdToFdhh)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_FdToFdhh(MLambda,em,gs,ZcplcFdFdhhL,ZcplcFdFdhhR,           & 
& MFdOS,MhhOS,MRPFdToFdhh,MRGFdToFdhh)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_FdToFdhh(MLambda,em,gs,cplcFdFdhhL,cplcFdFdhhR,             & 
& MFd,Mhh,MRPFdToFdhh,MRGFdToFdhh)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_FdToFdhh(MLambda,em,gs,ZcplcFdFdhhL,ZcplcFdFdhhR,           & 
& MFd,Mhh,MRPFdToFdhh,MRGFdToFdhh)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FdToFdhh(cplcFdFdhhL,cplcFdFdhhR,ctcplcFdFdhhL,         & 
& ctcplcFdFdhhR,MFd,MFd2,Mhh,Mhh2,ZfDL,ZfDR,Zfhh,AmpWaveFdToFdhh)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FdToFdhh(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,MVWp,            & 
& MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFdFdAhL,               & 
& cplcFdFdAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,     & 
& cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,             & 
& cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,               & 
& cplhhVZVZ,AmpVertexFdToFdhh)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_FdToFdhh(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,              & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFdFdAhL,          & 
& cplcFdFdAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,     & 
& cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,             & 
& cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,               & 
& cplhhVZVZ,AmpVertexIRdrFdToFdhh)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FdToFdhh(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,            & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplAhAhhh,cplcFdFdAhL,cplcFdFdAhR,cplAhhhVZ,ZcplcFdFdhhL,ZcplcFdFdhhR,          & 
& cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,               & 
& cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,             & 
& cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,            & 
& cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRosFdToFdhh)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FdToFdhh(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,              & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFdFdAhL,          & 
& cplcFdFdAhR,cplAhhhVZ,ZcplcFdFdhhL,ZcplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,               & 
& cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,             & 
& cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucHpL,cplcFdFucHpR,             & 
& cplcFdFucVWpL,cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,              & 
& cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRosFdToFdhh)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FdToFdhh(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,            & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplAhAhhh,cplcFdFdAhL,cplcFdFdAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,            & 
& cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,               & 
& cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,             & 
& cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,            & 
& cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRosFdToFdhh)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FdToFdhh(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,              & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFdFdAhL,          & 
& cplcFdFdAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,     & 
& cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,             & 
& cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,               & 
& cplhhVZVZ,AmpVertexIRosFdToFdhh)

 End if 
 End if 
AmpVertexFdToFdhh = AmpVertexFdToFdhh -  AmpVertexIRdrFdToFdhh! +  AmpVertexIRosFdToFdhh ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZFdToFdhh=0._dp 
AmpVertexZFdToFdhh=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFdToFdhh(1,gt2,:,:) = AmpWaveZFdToFdhh(1,gt2,:,:)+ZRUUd(gt2,gt1)*AmpWaveFdToFdhh(1,gt1,:,:) 
AmpVertexZFdToFdhh(1,gt2,:,:)= AmpVertexZFdToFdhh(1,gt2,:,:) + ZRUUd(gt2,gt1)*AmpVertexFdToFdhh(1,gt1,:,:) 
AmpWaveZFdToFdhh(2,gt2,:,:) = AmpWaveZFdToFdhh(2,gt2,:,:)+ZRUVdc(gt2,gt1)*AmpWaveFdToFdhh(2,gt1,:,:) 
AmpVertexZFdToFdhh(2,gt2,:,:)= AmpVertexZFdToFdhh(2,gt2,:,:) + ZRUVdc(gt2,gt1)*AmpVertexFdToFdhh(2,gt1,:,:) 
 End Do 
End Do 
AmpWaveFdToFdhh=AmpWaveZFdToFdhh 
AmpVertexFdToFdhh= AmpVertexZFdToFdhh
! Final State 1 
AmpWaveZFdToFdhh=0._dp 
AmpVertexZFdToFdhh=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFdToFdhh(1,:,gt2,:) = AmpWaveZFdToFdhh(1,:,gt2,:)+ZRUVd(gt2,gt1)*AmpWaveFdToFdhh(1,:,gt1,:) 
AmpVertexZFdToFdhh(1,:,gt2,:)= AmpVertexZFdToFdhh(1,:,gt2,:)+ZRUVd(gt2,gt1)*AmpVertexFdToFdhh(1,:,gt1,:) 
AmpWaveZFdToFdhh(2,:,gt2,:) = AmpWaveZFdToFdhh(2,:,gt2,:)+ZRUUdc(gt2,gt1)*AmpWaveFdToFdhh(2,:,gt1,:) 
AmpVertexZFdToFdhh(2,:,gt2,:)= AmpVertexZFdToFdhh(2,:,gt2,:)+ZRUUdc(gt2,gt1)*AmpVertexFdToFdhh(2,:,gt1,:) 
 End Do 
End Do 
AmpWaveFdToFdhh=AmpWaveZFdToFdhh 
AmpVertexFdToFdhh= AmpVertexZFdToFdhh
! Final State 2 
AmpWaveZFdToFdhh=0._dp 
AmpVertexZFdToFdhh=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZFdToFdhh(:,:,:,gt2) = AmpWaveZFdToFdhh(:,:,:,gt2)+ZRUZH(gt2,gt1)*AmpWaveFdToFdhh(:,:,:,gt1) 
AmpVertexZFdToFdhh(:,:,:,gt2)= AmpVertexZFdToFdhh(:,:,:,gt2)+ZRUZH(gt2,gt1)*AmpVertexFdToFdhh(:,:,:,gt1) 
 End Do 
End Do 
AmpWaveFdToFdhh=AmpWaveZFdToFdhh 
AmpVertexFdToFdhh= AmpVertexZFdToFdhh
End if
If (ShiftIRdiv) Then 
AmpVertexFdToFdhh = AmpVertexFdToFdhh  +  AmpVertexIRosFdToFdhh
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fd->Fd hh -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumFdToFdhh = AmpTreeFdToFdhh 
 AmpSum2FdToFdhh = AmpTreeFdToFdhh + 2._dp*AmpWaveFdToFdhh + 2._dp*AmpVertexFdToFdhh  
Else 
 AmpSumFdToFdhh = AmpTreeFdToFdhh + AmpWaveFdToFdhh + AmpVertexFdToFdhh
 AmpSum2FdToFdhh = AmpTreeFdToFdhh + AmpWaveFdToFdhh + AmpVertexFdToFdhh 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumFdToFdhh = AmpTreeFdToFdhh
 AmpSum2FdToFdhh = AmpTreeFdToFdhh 
End if 
Do gt1=1,3
i4 = isave 
  Do gt2=1,3
    Do gt3=1,2
If (((OSkinematics).and.(MFdOS(gt1).gt.(MFdOS(gt2)+MhhOS(gt3)))).or.((.not.OSkinematics).and.(MFd(gt1).gt.(MFd(gt2)+Mhh(gt3))))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2, gt3 
  AmpSum2FdToFdhh = AmpTreeFdToFdhh
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFdOS(gt1),MFdOS(gt2),MhhOS(gt3),AmpSumFdToFdhh(:,gt1, gt2, gt3),AmpSum2FdToFdhh(:,gt1, gt2, gt3),AmpSqFdToFdhh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFd(gt1),MFd(gt2),Mhh(gt3),AmpSumFdToFdhh(:,gt1, gt2, gt3),AmpSum2FdToFdhh(:,gt1, gt2, gt3),AmpSqFdToFdhh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqFdToFdhh(gt1, gt2, gt3) 
  AmpSum2FdToFdhh = 2._dp*AmpWaveFdToFdhh
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFdOS(gt1),MFdOS(gt2),MhhOS(gt3),AmpSumFdToFdhh(:,gt1, gt2, gt3),AmpSum2FdToFdhh(:,gt1, gt2, gt3),AmpSqFdToFdhh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFd(gt1),MFd(gt2),Mhh(gt3),AmpSumFdToFdhh(:,gt1, gt2, gt3),AmpSum2FdToFdhh(:,gt1, gt2, gt3),AmpSqFdToFdhh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqFdToFdhh(gt1, gt2, gt3) 
  AmpSum2FdToFdhh = 2._dp*AmpVertexFdToFdhh
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFdOS(gt1),MFdOS(gt2),MhhOS(gt3),AmpSumFdToFdhh(:,gt1, gt2, gt3),AmpSum2FdToFdhh(:,gt1, gt2, gt3),AmpSqFdToFdhh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFd(gt1),MFd(gt2),Mhh(gt3),AmpSumFdToFdhh(:,gt1, gt2, gt3),AmpSum2FdToFdhh(:,gt1, gt2, gt3),AmpSqFdToFdhh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqFdToFdhh(gt1, gt2, gt3) 
  AmpSum2FdToFdhh = AmpTreeFdToFdhh + 2._dp*AmpWaveFdToFdhh + 2._dp*AmpVertexFdToFdhh
If (OSkinematics) Then 
  Call SquareAmp_FtoFS(MFdOS(gt1),MFdOS(gt2),MhhOS(gt3),AmpSumFdToFdhh(:,gt1, gt2, gt3),AmpSum2FdToFdhh(:,gt1, gt2, gt3),AmpSqFdToFdhh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_FtoFS(MFd(gt1),MFd(gt2),Mhh(gt3),AmpSumFdToFdhh(:,gt1, gt2, gt3),AmpSum2FdToFdhh(:,gt1, gt2, gt3),AmpSqFdToFdhh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqFdToFdhh(gt1, gt2, gt3) 
 End if 
If (OSkinematics) Then 
  AmpSum2FdToFdhh = AmpTreeFdToFdhh
  Call SquareAmp_FtoFS(MFdOS(gt1),MFdOS(gt2),MhhOS(gt3),AmpSumFdToFdhh(:,gt1, gt2, gt3),AmpSum2FdToFdhh(:,gt1, gt2, gt3),AmpSqFdToFdhh(gt1, gt2, gt3)) 
  AmpSqTreeFdToFdhh(gt1, gt2, gt3) = AmpSqFdToFdhh(gt1, gt2, gt3)  
  AmpSum2FdToFdhh = + 2._dp*AmpWaveFdToFdhh + 2._dp*AmpVertexFdToFdhh
  Call SquareAmp_FtoFS(MFdOS(gt1),MFdOS(gt2),MhhOS(gt3),AmpSumFdToFdhh(:,gt1, gt2, gt3),AmpSum2FdToFdhh(:,gt1, gt2, gt3),AmpSqFdToFdhh(gt1, gt2, gt3)) 
  AmpSqFdToFdhh(gt1, gt2, gt3) = AmpSqFdToFdhh(gt1, gt2, gt3) + AmpSqTreeFdToFdhh(gt1, gt2, gt3)  
Else  
  AmpSum2FdToFdhh = AmpTreeFdToFdhh
  Call SquareAmp_FtoFS(MFd(gt1),MFd(gt2),Mhh(gt3),AmpSumFdToFdhh(:,gt1, gt2, gt3),AmpSum2FdToFdhh(:,gt1, gt2, gt3),AmpSqFdToFdhh(gt1, gt2, gt3)) 
  AmpSqTreeFdToFdhh(gt1, gt2, gt3) = AmpSqFdToFdhh(gt1, gt2, gt3)  
  AmpSum2FdToFdhh = + 2._dp*AmpWaveFdToFdhh + 2._dp*AmpVertexFdToFdhh
  Call SquareAmp_FtoFS(MFd(gt1),MFd(gt2),Mhh(gt3),AmpSumFdToFdhh(:,gt1, gt2, gt3),AmpSum2FdToFdhh(:,gt1, gt2, gt3),AmpSqFdToFdhh(gt1, gt2, gt3)) 
  AmpSqFdToFdhh(gt1, gt2, gt3) = AmpSqFdToFdhh(gt1, gt2, gt3) + AmpSqTreeFdToFdhh(gt1, gt2, gt3)  
End if  
Else  
  AmpSqFdToFdhh(gt1, gt2, gt3) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFdToFdhh(gt1, gt2, gt3).le.0._dp) Then 
  gP1LFd(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFd(gt1,i4) = 1._dp*GammaTPS(MFdOS(gt1),MFdOS(gt2),MhhOS(gt3),helfactor*AmpSqFdToFdhh(gt1, gt2, gt3))
Else 
  gP1LFd(gt1,i4) = 1._dp*GammaTPS(MFd(gt1),MFd(gt2),Mhh(gt3),helfactor*AmpSqFdToFdhh(gt1, gt2, gt3))
End if 
If ((Abs(MRPFdToFdhh(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGFdToFdhh(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFd(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPFdToFdhh(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGFdToFdhh(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPFdToFdhh(gt1, gt2, gt3) + MRGFdToFdhh(gt1, gt2, gt3)) 
  gP1LFd(gt1,i4) = gP1LFd(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPFdToFdhh(gt1, gt2, gt3) + MRGFdToFdhh(gt1, gt2, gt3))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LFd(gt1,i4) 
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
! Fd VZ
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_FdToFdVZ(cplcFdFdVZL,cplcFdFdVZR,MFd,MVZ,               & 
& MFd2,MVZ2,AmpTreeFdToFdVZ)

  Else 
Call Amplitude_Tree_SDdiracDM_FdToFdVZ(ZcplcFdFdVZL,ZcplcFdFdVZR,MFd,MVZ,             & 
& MFd2,MVZ2,AmpTreeFdToFdVZ)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_FdToFdVZ(MLambda,em,gs,cplcFdFdVZL,cplcFdFdVZR,             & 
& MFdOS,MVZOS,MRPFdToFdVZ,MRGFdToFdVZ)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_FdToFdVZ(MLambda,em,gs,ZcplcFdFdVZL,ZcplcFdFdVZR,           & 
& MFdOS,MVZOS,MRPFdToFdVZ,MRGFdToFdVZ)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_FdToFdVZ(MLambda,em,gs,cplcFdFdVZL,cplcFdFdVZR,             & 
& MFd,MVZ,MRPFdToFdVZ,MRGFdToFdVZ)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_FdToFdVZ(MLambda,em,gs,ZcplcFdFdVZL,ZcplcFdFdVZR,           & 
& MFd,MVZ,MRPFdToFdVZ,MRGFdToFdVZ)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FdToFdVZ(cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,           & 
& cplcFdFdVZR,ctcplcFdFdVPL,ctcplcFdFdVPR,ctcplcFdFdVZL,ctcplcFdFdVZR,MFd,               & 
& MFd2,MVP,MVP2,MVZ,MVZ2,ZfDL,ZfDR,ZfVPVZ,ZfVZ,AmpWaveFdToFdVZ)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FdToFdVZ(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,MVWp,            & 
& MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,cplcFdFdAhR,             & 
& cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,     & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,             & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,         & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexFdToFdVZ)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_FdToFdVZ(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,              & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,cplcFdFdAhR,        & 
& cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,     & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,             & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,         & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRdrFdToFdVZ)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FdToFdVZ(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,            & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplcFdFdAhL,cplcFdFdAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,          & 
& cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,              & 
& cplcFuFdVWpR,ZcplcFdFdVZL,ZcplcFdFdVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,           & 
& cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,             & 
& cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFdToFdVZ)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FdToFdVZ(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,              & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,cplcFdFdAhR,        & 
& cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,     & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,ZcplcFdFdVZL,ZcplcFdFdVZR,           & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,         & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFdToFdVZ)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FdToFdVZ(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,            & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplcFdFdAhL,cplcFdFdAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,          & 
& cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,              & 
& cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,             & 
& cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,             & 
& cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFdToFdVZ)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FdToFdVZ(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,              & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,cplcFdFdAhR,        & 
& cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,     & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,             & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,         & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,AmpVertexIRosFdToFdVZ)

 End if 
 End if 
AmpVertexFdToFdVZ = AmpVertexFdToFdVZ -  AmpVertexIRdrFdToFdVZ! +  AmpVertexIRosFdToFdVZ ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZFdToFdVZ=0._dp 
AmpVertexZFdToFdVZ=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFdToFdVZ(1,gt2,:) = AmpWaveZFdToFdVZ(1,gt2,:)+ZRUVdc(gt2,gt1)*AmpWaveFdToFdVZ(1,gt1,:) 
AmpVertexZFdToFdVZ(1,gt2,:)= AmpVertexZFdToFdVZ(1,gt2,:) + ZRUVdc(gt2,gt1)*AmpVertexFdToFdVZ(1,gt1,:) 
AmpWaveZFdToFdVZ(2,gt2,:) = AmpWaveZFdToFdVZ(2,gt2,:)+ZRUUd(gt2,gt1)*AmpWaveFdToFdVZ(2,gt1,:) 
AmpVertexZFdToFdVZ(2,gt2,:)= AmpVertexZFdToFdVZ(2,gt2,:) + ZRUUd(gt2,gt1)*AmpVertexFdToFdVZ(2,gt1,:) 
AmpWaveZFdToFdVZ(3,gt2,:) = AmpWaveZFdToFdVZ(3,gt2,:)+ZRUVdc(gt2,gt1)*AmpWaveFdToFdVZ(3,gt1,:) 
AmpVertexZFdToFdVZ(3,gt2,:)= AmpVertexZFdToFdVZ(3,gt2,:) + ZRUVdc(gt2,gt1)*AmpVertexFdToFdVZ(3,gt1,:) 
AmpWaveZFdToFdVZ(4,gt2,:) = AmpWaveZFdToFdVZ(4,gt2,:)+ZRUUd(gt2,gt1)*AmpWaveFdToFdVZ(4,gt1,:) 
AmpVertexZFdToFdVZ(4,gt2,:)= AmpVertexZFdToFdVZ(4,gt2,:) + ZRUUd(gt2,gt1)*AmpVertexFdToFdVZ(4,gt1,:) 
 End Do 
End Do 
AmpWaveFdToFdVZ=AmpWaveZFdToFdVZ 
AmpVertexFdToFdVZ= AmpVertexZFdToFdVZ
! Final State 1 
AmpWaveZFdToFdVZ=0._dp 
AmpVertexZFdToFdVZ=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFdToFdVZ(1,:,gt2) = AmpWaveZFdToFdVZ(1,:,gt2)+ZRUVd(gt2,gt1)*AmpWaveFdToFdVZ(1,:,gt1) 
AmpVertexZFdToFdVZ(1,:,gt2)= AmpVertexZFdToFdVZ(1,:,gt2)+ZRUVd(gt2,gt1)*AmpVertexFdToFdVZ(1,:,gt1) 
AmpWaveZFdToFdVZ(2,:,gt2) = AmpWaveZFdToFdVZ(2,:,gt2)+ZRUUdc(gt2,gt1)*AmpWaveFdToFdVZ(2,:,gt1) 
AmpVertexZFdToFdVZ(2,:,gt2)= AmpVertexZFdToFdVZ(2,:,gt2)+ZRUUdc(gt2,gt1)*AmpVertexFdToFdVZ(2,:,gt1) 
AmpWaveZFdToFdVZ(3,:,gt2) = AmpWaveZFdToFdVZ(3,:,gt2)+ZRUVd(gt2,gt1)*AmpWaveFdToFdVZ(3,:,gt1) 
AmpVertexZFdToFdVZ(3,:,gt2)= AmpVertexZFdToFdVZ(3,:,gt2)+ZRUVd(gt2,gt1)*AmpVertexFdToFdVZ(3,:,gt1) 
AmpWaveZFdToFdVZ(4,:,gt2) = AmpWaveZFdToFdVZ(4,:,gt2)+ZRUUdc(gt2,gt1)*AmpWaveFdToFdVZ(4,:,gt1) 
AmpVertexZFdToFdVZ(4,:,gt2)= AmpVertexZFdToFdVZ(4,:,gt2)+ZRUUdc(gt2,gt1)*AmpVertexFdToFdVZ(4,:,gt1) 
 End Do 
End Do 
AmpWaveFdToFdVZ=AmpWaveZFdToFdVZ 
AmpVertexFdToFdVZ= AmpVertexZFdToFdVZ
End if
If (ShiftIRdiv) Then 
AmpVertexFdToFdVZ = AmpVertexFdToFdVZ  +  AmpVertexIRosFdToFdVZ
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fd->Fd VZ -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumFdToFdVZ = AmpTreeFdToFdVZ 
 AmpSum2FdToFdVZ = AmpTreeFdToFdVZ + 2._dp*AmpWaveFdToFdVZ + 2._dp*AmpVertexFdToFdVZ  
Else 
 AmpSumFdToFdVZ = AmpTreeFdToFdVZ + AmpWaveFdToFdVZ + AmpVertexFdToFdVZ
 AmpSum2FdToFdVZ = AmpTreeFdToFdVZ + AmpWaveFdToFdVZ + AmpVertexFdToFdVZ 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumFdToFdVZ = AmpTreeFdToFdVZ
 AmpSum2FdToFdVZ = AmpTreeFdToFdVZ 
End if 
Do gt1=1,3
i4 = isave 
  Do gt2=1,3
If (((OSkinematics).and.(MFdOS(gt1).gt.(MFdOS(gt2)+MVZOS))).or.((.not.OSkinematics).and.(MFd(gt1).gt.(MFd(gt2)+MVZ)))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2 
  AmpSum2FdToFdVZ = AmpTreeFdToFdVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFdOS(gt1),MFdOS(gt2),MVZOS,AmpSumFdToFdVZ(:,gt1, gt2),AmpSum2FdToFdVZ(:,gt1, gt2),AmpSqFdToFdVZ(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFd(gt1),MFd(gt2),MVZ,AmpSumFdToFdVZ(:,gt1, gt2),AmpSum2FdToFdVZ(:,gt1, gt2),AmpSqFdToFdVZ(gt1, gt2)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqFdToFdVZ(gt1, gt2) 
  AmpSum2FdToFdVZ = 2._dp*AmpWaveFdToFdVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFdOS(gt1),MFdOS(gt2),MVZOS,AmpSumFdToFdVZ(:,gt1, gt2),AmpSum2FdToFdVZ(:,gt1, gt2),AmpSqFdToFdVZ(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFd(gt1),MFd(gt2),MVZ,AmpSumFdToFdVZ(:,gt1, gt2),AmpSum2FdToFdVZ(:,gt1, gt2),AmpSqFdToFdVZ(gt1, gt2)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqFdToFdVZ(gt1, gt2) 
  AmpSum2FdToFdVZ = 2._dp*AmpVertexFdToFdVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFdOS(gt1),MFdOS(gt2),MVZOS,AmpSumFdToFdVZ(:,gt1, gt2),AmpSum2FdToFdVZ(:,gt1, gt2),AmpSqFdToFdVZ(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFd(gt1),MFd(gt2),MVZ,AmpSumFdToFdVZ(:,gt1, gt2),AmpSum2FdToFdVZ(:,gt1, gt2),AmpSqFdToFdVZ(gt1, gt2)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqFdToFdVZ(gt1, gt2) 
  AmpSum2FdToFdVZ = AmpTreeFdToFdVZ + 2._dp*AmpWaveFdToFdVZ + 2._dp*AmpVertexFdToFdVZ
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFdOS(gt1),MFdOS(gt2),MVZOS,AmpSumFdToFdVZ(:,gt1, gt2),AmpSum2FdToFdVZ(:,gt1, gt2),AmpSqFdToFdVZ(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFd(gt1),MFd(gt2),MVZ,AmpSumFdToFdVZ(:,gt1, gt2),AmpSum2FdToFdVZ(:,gt1, gt2),AmpSqFdToFdVZ(gt1, gt2)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqFdToFdVZ(gt1, gt2) 
 End if 
If (OSkinematics) Then 
  AmpSum2FdToFdVZ = AmpTreeFdToFdVZ
  Call SquareAmp_FtoFV(MFdOS(gt1),MFdOS(gt2),MVZOS,AmpSumFdToFdVZ(:,gt1, gt2),AmpSum2FdToFdVZ(:,gt1, gt2),AmpSqFdToFdVZ(gt1, gt2)) 
  AmpSqTreeFdToFdVZ(gt1, gt2) = AmpSqFdToFdVZ(gt1, gt2)  
  AmpSum2FdToFdVZ = + 2._dp*AmpWaveFdToFdVZ + 2._dp*AmpVertexFdToFdVZ
  Call SquareAmp_FtoFV(MFdOS(gt1),MFdOS(gt2),MVZOS,AmpSumFdToFdVZ(:,gt1, gt2),AmpSum2FdToFdVZ(:,gt1, gt2),AmpSqFdToFdVZ(gt1, gt2)) 
  AmpSqFdToFdVZ(gt1, gt2) = AmpSqFdToFdVZ(gt1, gt2) + AmpSqTreeFdToFdVZ(gt1, gt2)  
Else  
  AmpSum2FdToFdVZ = AmpTreeFdToFdVZ
  Call SquareAmp_FtoFV(MFd(gt1),MFd(gt2),MVZ,AmpSumFdToFdVZ(:,gt1, gt2),AmpSum2FdToFdVZ(:,gt1, gt2),AmpSqFdToFdVZ(gt1, gt2)) 
  AmpSqTreeFdToFdVZ(gt1, gt2) = AmpSqFdToFdVZ(gt1, gt2)  
  AmpSum2FdToFdVZ = + 2._dp*AmpWaveFdToFdVZ + 2._dp*AmpVertexFdToFdVZ
  Call SquareAmp_FtoFV(MFd(gt1),MFd(gt2),MVZ,AmpSumFdToFdVZ(:,gt1, gt2),AmpSum2FdToFdVZ(:,gt1, gt2),AmpSqFdToFdVZ(gt1, gt2)) 
  AmpSqFdToFdVZ(gt1, gt2) = AmpSqFdToFdVZ(gt1, gt2) + AmpSqTreeFdToFdVZ(gt1, gt2)  
End if  
Else  
  AmpSqFdToFdVZ(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFdToFdVZ(gt1, gt2).le.0._dp) Then 
  gP1LFd(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFd(gt1,i4) = 1._dp*GammaTPS(MFdOS(gt1),MFdOS(gt2),MVZOS,helfactor*AmpSqFdToFdVZ(gt1, gt2))
Else 
  gP1LFd(gt1,i4) = 1._dp*GammaTPS(MFd(gt1),MFd(gt2),MVZ,helfactor*AmpSqFdToFdVZ(gt1, gt2))
End if 
If ((Abs(MRPFdToFdVZ(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFdToFdVZ(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFd(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPFdToFdVZ(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFdToFdVZ(gt1, gt2)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPFdToFdVZ(gt1, gt2) + MRGFdToFdVZ(gt1, gt2)) 
  gP1LFd(gt1,i4) = gP1LFd(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPFdToFdVZ(gt1, gt2) + MRGFdToFdVZ(gt1, gt2))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LFd(gt1,i4) 
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
! Fu Conjg(VWp)
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_FdToFucVWp(cplcFdFucVWpL,cplcFdFucVWpR,MFd,             & 
& MFu,MVWp,MFd2,MFu2,MVWp2,AmpTreeFdToFucVWp)

  Else 
Call Amplitude_Tree_SDdiracDM_FdToFucVWp(ZcplcFdFucVWpL,ZcplcFdFucVWpR,               & 
& MFd,MFu,MVWp,MFd2,MFu2,MVWp2,AmpTreeFdToFucVWp)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_FdToFucVWp(MLambda,em,gs,cplcFdFucVWpL,cplcFdFucVWpR,       & 
& MFdOS,MFuOS,MVWpOS,MRPFdToFucVWp,MRGFdToFucVWp)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_FdToFucVWp(MLambda,em,gs,ZcplcFdFucVWpL,ZcplcFdFucVWpR,     & 
& MFdOS,MFuOS,MVWpOS,MRPFdToFucVWp,MRGFdToFucVWp)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_FdToFucVWp(MLambda,em,gs,cplcFdFucVWpL,cplcFdFucVWpR,       & 
& MFd,MFu,MVWp,MRPFdToFucVWp,MRGFdToFucVWp)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_FdToFucVWp(MLambda,em,gs,ZcplcFdFucVWpL,ZcplcFdFucVWpR,     & 
& MFd,MFu,MVWp,MRPFdToFucVWp,MRGFdToFucVWp)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FdToFucVWp(cplcFdFucVWpL,cplcFdFucVWpR,ctcplcFdFucVWpL, & 
& ctcplcFdFucVWpR,MFd,MFd2,MFu,MFu2,MVWp,MVWp2,ZfDL,ZfDR,ZfUL,ZfUR,ZfVWp,AmpWaveFdToFucVWp)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FdToFucVWp(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,               & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,cplcFdFdAhR,        & 
& cplcFuFuAhL,cplcFuFuAhR,cplAhHpcVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVGL,               & 
& cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,               & 
& cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,               & 
& cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,cplhhHpcVWp,         & 
& cplhhcVWpVWp,cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,AmpVertexFdToFucVWp)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_FdToFucVWp(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,            & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,cplcFdFdAhR,        & 
& cplcFuFuAhL,cplcFuFuAhR,cplAhHpcVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVGL,               & 
& cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,               & 
& cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,               & 
& cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,cplhhHpcVWp,         & 
& cplhhcVWpVWp,cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,AmpVertexIRdrFdToFucVWp)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FdToFucVWp(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,          & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplcFdFdAhL,cplcFdFdAhR,cplcFuFuAhL,cplcFuFuAhR,cplAhHpcVWp,cplcFdFdhhL,        & 
& cplcFdFdhhR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,               & 
& cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,               & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,GosZcplcFdFucHpL,GosZcplcFdFucHpR,ZcplcFdFucVWpL,  & 
& ZcplcFdFucVWpR,cplhhHpcVWp,cplhhcVWpVWp,GosZcplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,      & 
& cplcVWpVWpVZ,AmpVertexIRosFdToFucVWp)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FdToFucVWp(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,            & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,cplcFdFdAhR,        & 
& cplcFuFuAhL,cplcFuFuAhR,cplAhHpcVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVGL,               & 
& cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,               & 
& cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,               & 
& cplcFuFuVZR,GZcplcFdFucHpL,GZcplcFdFucHpR,ZcplcFdFucVWpL,ZcplcFdFucVWpR,               & 
& cplhhHpcVWp,cplhhcVWpVWp,GZcplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,          & 
& AmpVertexIRosFdToFucVWp)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FdToFucVWp(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,          & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplcFdFdAhL,cplcFdFdAhR,cplcFuFuAhL,cplcFuFuAhR,cplAhHpcVWp,cplcFdFdhhL,        & 
& cplcFdFdhhR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,               & 
& cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,               & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,GcplcFdFucHpL,GcplcFdFucHpR,cplcFdFucVWpL,         & 
& cplcFdFucVWpR,cplhhHpcVWp,cplhhcVWpVWp,GcplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,          & 
& cplcVWpVWpVZ,AmpVertexIRosFdToFucVWp)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_FdToFucVWp(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,            & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,cplcFdFdAhR,        & 
& cplcFuFuAhL,cplcFuFuAhR,cplAhHpcVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVGL,               & 
& cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,               & 
& cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,               & 
& cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,cplhhHpcVWp,         & 
& cplhhcVWpVWp,cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,AmpVertexIRosFdToFucVWp)

 End if 
 End if 
AmpVertexFdToFucVWp = AmpVertexFdToFucVWp -  AmpVertexIRdrFdToFucVWp! +  AmpVertexIRosFdToFucVWp ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZFdToFucVWp=0._dp 
AmpVertexZFdToFucVWp=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFdToFucVWp(1,gt2,:) = AmpWaveZFdToFucVWp(1,gt2,:)+ZRUVdc(gt2,gt1)*AmpWaveFdToFucVWp(1,gt1,:) 
AmpVertexZFdToFucVWp(1,gt2,:)= AmpVertexZFdToFucVWp(1,gt2,:) + ZRUVdc(gt2,gt1)*AmpVertexFdToFucVWp(1,gt1,:) 
AmpWaveZFdToFucVWp(2,gt2,:) = AmpWaveZFdToFucVWp(2,gt2,:)+ZRUUd(gt2,gt1)*AmpWaveFdToFucVWp(2,gt1,:) 
AmpVertexZFdToFucVWp(2,gt2,:)= AmpVertexZFdToFucVWp(2,gt2,:) + ZRUUd(gt2,gt1)*AmpVertexFdToFucVWp(2,gt1,:) 
AmpWaveZFdToFucVWp(3,gt2,:) = AmpWaveZFdToFucVWp(3,gt2,:)+ZRUVdc(gt2,gt1)*AmpWaveFdToFucVWp(3,gt1,:) 
AmpVertexZFdToFucVWp(3,gt2,:)= AmpVertexZFdToFucVWp(3,gt2,:) + ZRUVdc(gt2,gt1)*AmpVertexFdToFucVWp(3,gt1,:) 
AmpWaveZFdToFucVWp(4,gt2,:) = AmpWaveZFdToFucVWp(4,gt2,:)+ZRUUd(gt2,gt1)*AmpWaveFdToFucVWp(4,gt1,:) 
AmpVertexZFdToFucVWp(4,gt2,:)= AmpVertexZFdToFucVWp(4,gt2,:) + ZRUUd(gt2,gt1)*AmpVertexFdToFucVWp(4,gt1,:) 
 End Do 
End Do 
AmpWaveFdToFucVWp=AmpWaveZFdToFucVWp 
AmpVertexFdToFucVWp= AmpVertexZFdToFucVWp
! Final State 1 
AmpWaveZFdToFucVWp=0._dp 
AmpVertexZFdToFucVWp=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZFdToFucVWp(1,:,gt2) = AmpWaveZFdToFucVWp(1,:,gt2)+ZRUVu(gt2,gt1)*AmpWaveFdToFucVWp(1,:,gt1) 
AmpVertexZFdToFucVWp(1,:,gt2)= AmpVertexZFdToFucVWp(1,:,gt2)+ZRUVu(gt2,gt1)*AmpVertexFdToFucVWp(1,:,gt1) 
AmpWaveZFdToFucVWp(2,:,gt2) = AmpWaveZFdToFucVWp(2,:,gt2)+ZRUUuc(gt2,gt1)*AmpWaveFdToFucVWp(2,:,gt1) 
AmpVertexZFdToFucVWp(2,:,gt2)= AmpVertexZFdToFucVWp(2,:,gt2)+ZRUUuc(gt2,gt1)*AmpVertexFdToFucVWp(2,:,gt1) 
AmpWaveZFdToFucVWp(3,:,gt2) = AmpWaveZFdToFucVWp(3,:,gt2)+ZRUVu(gt2,gt1)*AmpWaveFdToFucVWp(3,:,gt1) 
AmpVertexZFdToFucVWp(3,:,gt2)= AmpVertexZFdToFucVWp(3,:,gt2)+ZRUVu(gt2,gt1)*AmpVertexFdToFucVWp(3,:,gt1) 
AmpWaveZFdToFucVWp(4,:,gt2) = AmpWaveZFdToFucVWp(4,:,gt2)+ZRUUuc(gt2,gt1)*AmpWaveFdToFucVWp(4,:,gt1) 
AmpVertexZFdToFucVWp(4,:,gt2)= AmpVertexZFdToFucVWp(4,:,gt2)+ZRUUuc(gt2,gt1)*AmpVertexFdToFucVWp(4,:,gt1) 
 End Do 
End Do 
AmpWaveFdToFucVWp=AmpWaveZFdToFucVWp 
AmpVertexFdToFucVWp= AmpVertexZFdToFucVWp
End if
If (ShiftIRdiv) Then 
AmpVertexFdToFucVWp = AmpVertexFdToFucVWp  +  AmpVertexIRosFdToFucVWp
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fd->Fu conj[VWp] -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumFdToFucVWp = AmpTreeFdToFucVWp 
 AmpSum2FdToFucVWp = AmpTreeFdToFucVWp + 2._dp*AmpWaveFdToFucVWp + 2._dp*AmpVertexFdToFucVWp  
Else 
 AmpSumFdToFucVWp = AmpTreeFdToFucVWp + AmpWaveFdToFucVWp + AmpVertexFdToFucVWp
 AmpSum2FdToFucVWp = AmpTreeFdToFucVWp + AmpWaveFdToFucVWp + AmpVertexFdToFucVWp 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumFdToFucVWp = AmpTreeFdToFucVWp
 AmpSum2FdToFucVWp = AmpTreeFdToFucVWp 
End if 
Do gt1=1,3
i4 = isave 
  Do gt2=1,3
If (((OSkinematics).and.(MFdOS(gt1).gt.(MFuOS(gt2)+MVWpOS))).or.((.not.OSkinematics).and.(MFd(gt1).gt.(MFu(gt2)+MVWp)))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2 
  AmpSum2FdToFucVWp = AmpTreeFdToFucVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFdOS(gt1),MFuOS(gt2),MVWpOS,AmpSumFdToFucVWp(:,gt1, gt2),AmpSum2FdToFucVWp(:,gt1, gt2),AmpSqFdToFucVWp(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFd(gt1),MFu(gt2),MVWp,AmpSumFdToFucVWp(:,gt1, gt2),AmpSum2FdToFucVWp(:,gt1, gt2),AmpSqFdToFucVWp(gt1, gt2)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqFdToFucVWp(gt1, gt2) 
  AmpSum2FdToFucVWp = 2._dp*AmpWaveFdToFucVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFdOS(gt1),MFuOS(gt2),MVWpOS,AmpSumFdToFucVWp(:,gt1, gt2),AmpSum2FdToFucVWp(:,gt1, gt2),AmpSqFdToFucVWp(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFd(gt1),MFu(gt2),MVWp,AmpSumFdToFucVWp(:,gt1, gt2),AmpSum2FdToFucVWp(:,gt1, gt2),AmpSqFdToFucVWp(gt1, gt2)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqFdToFucVWp(gt1, gt2) 
  AmpSum2FdToFucVWp = 2._dp*AmpVertexFdToFucVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFdOS(gt1),MFuOS(gt2),MVWpOS,AmpSumFdToFucVWp(:,gt1, gt2),AmpSum2FdToFucVWp(:,gt1, gt2),AmpSqFdToFucVWp(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFd(gt1),MFu(gt2),MVWp,AmpSumFdToFucVWp(:,gt1, gt2),AmpSum2FdToFucVWp(:,gt1, gt2),AmpSqFdToFucVWp(gt1, gt2)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqFdToFucVWp(gt1, gt2) 
  AmpSum2FdToFucVWp = AmpTreeFdToFucVWp + 2._dp*AmpWaveFdToFucVWp + 2._dp*AmpVertexFdToFucVWp
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFdOS(gt1),MFuOS(gt2),MVWpOS,AmpSumFdToFucVWp(:,gt1, gt2),AmpSum2FdToFucVWp(:,gt1, gt2),AmpSqFdToFucVWp(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFd(gt1),MFu(gt2),MVWp,AmpSumFdToFucVWp(:,gt1, gt2),AmpSum2FdToFucVWp(:,gt1, gt2),AmpSqFdToFucVWp(gt1, gt2)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqFdToFucVWp(gt1, gt2) 
 End if 
If (OSkinematics) Then 
  AmpSum2FdToFucVWp = AmpTreeFdToFucVWp
  Call SquareAmp_FtoFV(MFdOS(gt1),MFuOS(gt2),MVWpOS,AmpSumFdToFucVWp(:,gt1, gt2),AmpSum2FdToFucVWp(:,gt1, gt2),AmpSqFdToFucVWp(gt1, gt2)) 
  AmpSqTreeFdToFucVWp(gt1, gt2) = AmpSqFdToFucVWp(gt1, gt2)  
  AmpSum2FdToFucVWp = + 2._dp*AmpWaveFdToFucVWp + 2._dp*AmpVertexFdToFucVWp
  Call SquareAmp_FtoFV(MFdOS(gt1),MFuOS(gt2),MVWpOS,AmpSumFdToFucVWp(:,gt1, gt2),AmpSum2FdToFucVWp(:,gt1, gt2),AmpSqFdToFucVWp(gt1, gt2)) 
  AmpSqFdToFucVWp(gt1, gt2) = AmpSqFdToFucVWp(gt1, gt2) + AmpSqTreeFdToFucVWp(gt1, gt2)  
Else  
  AmpSum2FdToFucVWp = AmpTreeFdToFucVWp
  Call SquareAmp_FtoFV(MFd(gt1),MFu(gt2),MVWp,AmpSumFdToFucVWp(:,gt1, gt2),AmpSum2FdToFucVWp(:,gt1, gt2),AmpSqFdToFucVWp(gt1, gt2)) 
  AmpSqTreeFdToFucVWp(gt1, gt2) = AmpSqFdToFucVWp(gt1, gt2)  
  AmpSum2FdToFucVWp = + 2._dp*AmpWaveFdToFucVWp + 2._dp*AmpVertexFdToFucVWp
  Call SquareAmp_FtoFV(MFd(gt1),MFu(gt2),MVWp,AmpSumFdToFucVWp(:,gt1, gt2),AmpSum2FdToFucVWp(:,gt1, gt2),AmpSqFdToFucVWp(gt1, gt2)) 
  AmpSqFdToFucVWp(gt1, gt2) = AmpSqFdToFucVWp(gt1, gt2) + AmpSqTreeFdToFucVWp(gt1, gt2)  
End if  
Else  
  AmpSqFdToFucVWp(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFdToFucVWp(gt1, gt2).le.0._dp) Then 
  gP1LFd(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFd(gt1,i4) = 1._dp*GammaTPS(MFdOS(gt1),MFuOS(gt2),MVWpOS,helfactor*AmpSqFdToFucVWp(gt1, gt2))
Else 
  gP1LFd(gt1,i4) = 1._dp*GammaTPS(MFd(gt1),MFu(gt2),MVWp,helfactor*AmpSqFdToFucVWp(gt1, gt2))
End if 
If ((Abs(MRPFdToFucVWp(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFdToFucVWp(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFd(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPFdToFucVWp(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFdToFucVWp(gt1, gt2)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPFdToFucVWp(gt1, gt2) + MRGFdToFucVWp(gt1, gt2)) 
  gP1LFd(gt1,i4) = gP1LFd(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPFdToFucVWp(gt1, gt2) + MRGFdToFucVWp(gt1, gt2))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LFd(gt1,i4) 
  End if 
End if 
End if 
i4=i4+1

  End do
If (gt1.eq.3) isave = i4 
End do
End If 
!---------------- 
! Fd VG
!---------------- 

If (LoopInducedDecaysOS) Then 


!Self-energy Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_WAVE_SDdiracDM_FdToFdVG(ZcplcFdFdVGL,ZcplcFdFdVGR,ctcplcFdFdVGL,       & 
& ctcplcFdFdVGR,MFdOS,MFd2OS,MVG,MVG2,ZfDL,ZfDR,ZfVG,AmpWaveFdToFdVG)

 Else 
Call Amplitude_WAVE_SDdiracDM_FdToFdVG(cplcFdFdVGL,cplcFdFdVGR,ctcplcFdFdVGL,         & 
& ctcplcFdFdVGR,MFdOS,MFd2OS,MVG,MVG2,ZfDL,ZfDR,ZfVG,AmpWaveFdToFdVG)

 End if 


!Vertex Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_VERTEX_SDdiracDM_FdToFdVG(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,               & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,ZcplcFdFdAhL,ZcplcFdFdAhR,ZcplcFdFdhhL,ZcplcFdFdhhR,ZcplcFuFdHpL,               & 
& ZcplcFuFdHpR,ZcplcFdFdVGL,ZcplcFdFdVGR,ZcplcFdFdVPL,ZcplcFdFdVPR,ZcplcFuFdVWpL,        & 
& ZcplcFuFdVWpR,ZcplcFdFdVZL,ZcplcFdFdVZR,ZcplcFuFuVGL,ZcplcFuFuVGR,ZcplcFdFucHpL,       & 
& ZcplcFdFucHpR,ZcplcFdFucVWpL,ZcplcFdFucVWpR,ZcplVGVGVG,AmpVertexFdToFdVG)

 Else 
Call Amplitude_VERTEX_SDdiracDM_FdToFdVG(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,               & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplcFdFdAhL,cplcFdFdAhR,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,        & 
& cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,             & 
& cplcFdFdVZL,cplcFdFdVZR,cplcFuFuVGL,cplcFuFuVGR,cplcFdFucHpL,cplcFdFucHpR,             & 
& cplcFdFucVWpL,cplcFdFucVWpR,cplVGVGVG,AmpVertexFdToFdVG)

 End if 
Else 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FdToFdVG(cplcFdFdVGL,cplcFdFdVGR,ctcplcFdFdVGL,         & 
& ctcplcFdFdVGR,MFd,MFd2,MVG,MVG2,ZfDL,ZfDR,ZfVG,AmpWaveFdToFdVG)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FdToFdVG(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,MVWp,            & 
& MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,cplcFdFdAhR,             & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,               & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,             & 
& cplcFuFuVGL,cplcFuFuVGR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,         & 
& cplVGVGVG,AmpVertexFdToFdVG)

End if 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fd->Fd VG -----------------------" 
End if 
If (OnlyTreeLevelContributions) Then 
 AmpSumFdToFdVG = 0._dp 
 AmpSum2FdToFdVG = 0._dp  
Else 
 AmpSumFdToFdVG = AmpVertexFdToFdVG + AmpWaveFdToFdVG
 AmpSum2FdToFdVG = AmpVertexFdToFdVG + AmpWaveFdToFdVG 
End If 
Do gt1=1,3
i4 = isave 
  Do gt2=1,3
If (((OSkinematics).and.(MFdOS(gt1).gt.(MFdOS(gt2)+0.))).or.((.not.OSkinematics).and.(MFd(gt1).gt.(MFd(gt2)+MVG)))) Then 
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFdOS(gt1),MFdOS(gt2),0._dp,AmpSumFdToFdVG(:,gt1, gt2),AmpSum2FdToFdVG(:,gt1, gt2),AmpSqFdToFdVG(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFd(gt1),MFd(gt2),MVG,AmpSumFdToFdVG(:,gt1, gt2),AmpSum2FdToFdVG(:,gt1, gt2),AmpSqFdToFdVG(gt1, gt2)) 
End if  
Else  
  AmpSqFdToFdVG(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFdToFdVG(gt1, gt2).le.0._dp) Then 
  gP1LFd(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFd(gt1,i4) = 4._dp/3._dp*GammaTPS(MFdOS(gt1),MFdOS(gt2),0._dp,helfactor*AmpSqFdToFdVG(gt1, gt2))
Else 
  gP1LFd(gt1,i4) = 4._dp/3._dp*GammaTPS(MFd(gt1),MFd(gt2),MVG,helfactor*AmpSqFdToFdVG(gt1, gt2))
End if 
If ((Abs(MRPFdToFdVG(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFdToFdVG(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFd(gt1,i4) 
End if 
i4=i4+1

  End do
If (gt1.eq.3) isave = i4 
End do
!---------------- 
! Fd VP
!---------------- 

If (LoopInducedDecaysOS) Then 


!Self-energy Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_WAVE_SDdiracDM_FdToFdVP(ZcplcFdFdVPL,ZcplcFdFdVPR,ZcplcFdFdVZL,        & 
& ZcplcFdFdVZR,ctcplcFdFdVPL,ctcplcFdFdVPR,ctcplcFdFdVZL,ctcplcFdFdVZR,MFdOS,            & 
& MFd2OS,MVP,MVP2,ZfDL,ZfDR,ZfVP,ZfVZVP,AmpWaveFdToFdVP)

 Else 
Call Amplitude_WAVE_SDdiracDM_FdToFdVP(cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,           & 
& cplcFdFdVZR,ctcplcFdFdVPL,ctcplcFdFdVPR,ctcplcFdFdVZL,ctcplcFdFdVZR,MFdOS,             & 
& MFd2OS,MVP,MVP2,ZfDL,ZfDR,ZfVP,ZfVZVP,AmpWaveFdToFdVP)

 End if 


!Vertex Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_VERTEX_SDdiracDM_FdToFdVP(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,               & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,ZcplcFdFdAhL,ZcplcFdFdAhR,ZcplcFdFdhhL,ZcplcFdFdhhR,ZcplcFuFdHpL,               & 
& ZcplcFuFdHpR,ZcplcFdFdVGL,ZcplcFdFdVGR,ZcplcFdFdVPL,ZcplcFdFdVPR,ZcplcFuFdVWpL,        & 
& ZcplcFuFdVWpR,ZcplcFdFdVZL,ZcplcFdFdVZR,ZcplcFuFuVPL,ZcplcFuFuVPR,ZcplcFdFucHpL,       & 
& ZcplcFdFucHpR,ZcplcFdFucVWpL,ZcplcFdFucVWpR,ZcplHpcHpVP,ZcplHpcVWpVP,ZcplcHpVPVWp,     & 
& ZcplcVWpVPVWp,AmpVertexFdToFdVP)

 Else 
Call Amplitude_VERTEX_SDdiracDM_FdToFdVP(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,               & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplcFdFdAhL,cplcFdFdAhR,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,        & 
& cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,             & 
& cplcFdFdVZL,cplcFdFdVZR,cplcFuFuVPL,cplcFuFuVPR,cplcFdFucHpL,cplcFdFucHpR,             & 
& cplcFdFucVWpL,cplcFdFucVWpR,cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,           & 
& AmpVertexFdToFdVP)

 End if 
Else 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_FdToFdVP(cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,           & 
& cplcFdFdVZR,ctcplcFdFdVPL,ctcplcFdFdVPR,ctcplcFdFdVZL,ctcplcFdFdVZR,MFd,               & 
& MFd2,MVP,MVP2,ZfDL,ZfDR,ZfVP,ZfVZVP,AmpWaveFdToFdVP)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_FdToFdVP(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,MVWp,            & 
& MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,cplcFdFdAhR,             & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,               & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,             & 
& cplcFuFuVPL,cplcFuFuVPR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,         & 
& cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,AmpVertexFdToFdVP)

End if 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Fd->Fd VP -----------------------" 
End if 
If (OnlyTreeLevelContributions) Then 
 AmpSumFdToFdVP = 0._dp 
 AmpSum2FdToFdVP = 0._dp  
Else 
 AmpSumFdToFdVP = AmpVertexFdToFdVP + AmpWaveFdToFdVP
 AmpSum2FdToFdVP = AmpVertexFdToFdVP + AmpWaveFdToFdVP 
End If 
Do gt1=1,3
i4 = isave 
  Do gt2=1,3
If (((OSkinematics).and.(MFdOS(gt1).gt.(MFdOS(gt2)+0.))).or.((.not.OSkinematics).and.(MFd(gt1).gt.(MFd(gt2)+MVP)))) Then 
If (OSkinematics) Then 
  Call SquareAmp_FtoFV(MFdOS(gt1),MFdOS(gt2),0._dp,AmpSumFdToFdVP(:,gt1, gt2),AmpSum2FdToFdVP(:,gt1, gt2),AmpSqFdToFdVP(gt1, gt2)) 
Else  
  Call SquareAmp_FtoFV(MFd(gt1),MFd(gt2),MVP,AmpSumFdToFdVP(:,gt1, gt2),AmpSum2FdToFdVP(:,gt1, gt2),AmpSqFdToFdVP(gt1, gt2)) 
End if  
Else  
  AmpSqFdToFdVP(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 2._dp 
If (AmpSqFdToFdVP(gt1, gt2).le.0._dp) Then 
  gP1LFd(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LFd(gt1,i4) = 1._dp*GammaTPS(MFdOS(gt1),MFdOS(gt2),0._dp,helfactor*AmpSqFdToFdVP(gt1, gt2))
Else 
  gP1LFd(gt1,i4) = 1._dp*GammaTPS(MFd(gt1),MFd(gt2),MVP,helfactor*AmpSqFdToFdVP(gt1, gt2))
End if 
If ((Abs(MRPFdToFdVP(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGFdToFdVP(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LFd(gt1,i4) 
End if 
i4=i4+1

  End do
If (gt1.eq.3) isave = i4 
End do
End Subroutine OneLoopDecay_Fd

End Module Wrapper_OneLoopDecay_Fd_SDdiracDM
