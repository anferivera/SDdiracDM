! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:22 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module Wrapper_OneLoopDecay_Ssc_SDdiracDM
Use Model_Data_SDdiracDM 
Use Kinematics 
Use OneLoopDecay_Ssc_SDdiracDM 
Use Control 
Use Settings 

 
Contains

 
Subroutine OneLoopDecay_Ssc(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,MFeOS,             & 
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
& ZfVL,ZfVR,ZfxVL,ZfxVR,ZfSsc,ZfVPVZ,ZfVZVP,cplAhAhhh,cplAhAhSsccSsc1,cplcFeFeAhL,       & 
& cplcFeFeAhR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,               & 
& cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,      & 
& cplcFeFxecSscR,cplcFvFeHpL,cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFvFvVZL,          & 
& cplcFvFvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVPL,   & 
& cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,               & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,             & 
& cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvAhL,cplcFxvFxvAhR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhhhhh,cplhhhhSsccSsc1,     & 
& cplhhHpcHp,cplhhSsccSsc,cplHpSsccHpcSsc1,cplSscSsccSsccSsc1,ctcplcFeFxecSscL,          & 
& ctcplcFeFxecSscR,ctcplcFvFxvcSscL,ctcplcFvFxvcSscR,ctcplhhSsccSsc,ZcplcFeFxecSscL,     & 
& ZcplcFeFxecSscR,ZcplcFvFxvcSscL,ZcplcFvFxvcSscR,ZcplhhSsccSsc,ZRUZH,ZRUVd,             & 
& ZRUUd,ZRUVu,ZRUUu,ZRUVe,ZRUUe,ZRUVv,ZRUVvr,ZRUXV,ZRUXU,ZRUVSs,MLambda,em,              & 
& gs,deltaM,kont,gP1LSsc)

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

Complex(dp),Intent(in) :: cplAhAhhh(2),cplAhAhSsccSsc1(2,2),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFeFehhL(3,3,2),& 
& cplcFeFehhR(3,3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),& 
& cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),             & 
& cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),             & 
& cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFvFxvcSscL(3,2,2),& 
& cplcFvFxvcSscR(3,2,2),cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFxeFxeVPL,             & 
& cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),         & 
& cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),       & 
& cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxvFxvAhL(2,2),& 
& cplcFxvFxvAhR(2,2),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),       & 
& cplcFxvFxvVZR(2,2),cplhhhhhh(2,2,2),cplhhhhSsccSsc1(2,2,2,2),cplhhHpcHp(2),            & 
& cplhhSsccSsc(2,2,2),cplHpSsccHpcSsc1(2,2),cplSscSsccSsccSsc1(2,2,2,2),ctcplcFeFxecSscL(3,2),& 
& ctcplcFeFxecSscR(3,2),ctcplcFvFxvcSscL(3,2,2),ctcplcFvFxvcSscR(3,2,2),ctcplhhSsccSsc(2,2,2),& 
& ZcplcFeFxecSscL(3,2),ZcplcFeFxecSscR(3,2),ZcplcFvFxvcSscL(3,2,2),ZcplcFvFxvcSscR(3,2,2),& 
& ZcplhhSsccSsc(2,2,2)

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
Real(dp), Intent(out) :: gP1LSsc(2,17) 
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
Real(dp) :: MRPSscTocFeFxe(2,3),MRGSscTocFeFxe(2,3), MRPZSscTocFeFxe(2,3),MRGZSscTocFeFxe(2,3) 
Real(dp) :: MVPSscTocFeFxe(2,3) 
Real(dp) :: RMsqTreeSscTocFeFxe(2,3),RMsqWaveSscTocFeFxe(2,3),RMsqVertexSscTocFeFxe(2,3) 
Complex(dp) :: AmpTreeSscTocFeFxe(2,2,3),AmpWaveSscTocFeFxe(2,2,3)=(0._dp,0._dp),AmpVertexSscTocFeFxe(2,2,3)& 
 & ,AmpVertexIRosSscTocFeFxe(2,2,3),AmpVertexIRdrSscTocFeFxe(2,2,3), AmpSumSscTocFeFxe(2,2,3), AmpSum2SscTocFeFxe(2,2,3) 
Complex(dp) :: AmpTreeZSscTocFeFxe(2,2,3),AmpWaveZSscTocFeFxe(2,2,3),AmpVertexZSscTocFeFxe(2,2,3) 
Real(dp) :: AmpSqSscTocFeFxe(2,3),  AmpSqTreeSscTocFeFxe(2,3) 
Real(dp) :: MRPSscTocFvFxv(2,3,2),MRGSscTocFvFxv(2,3,2), MRPZSscTocFvFxv(2,3,2),MRGZSscTocFvFxv(2,3,2) 
Real(dp) :: MVPSscTocFvFxv(2,3,2) 
Real(dp) :: RMsqTreeSscTocFvFxv(2,3,2),RMsqWaveSscTocFvFxv(2,3,2),RMsqVertexSscTocFvFxv(2,3,2) 
Complex(dp) :: AmpTreeSscTocFvFxv(2,2,3,2),AmpWaveSscTocFvFxv(2,2,3,2)=(0._dp,0._dp),AmpVertexSscTocFvFxv(2,2,3,2)& 
 & ,AmpVertexIRosSscTocFvFxv(2,2,3,2),AmpVertexIRdrSscTocFvFxv(2,2,3,2), AmpSumSscTocFvFxv(2,2,3,2), AmpSum2SscTocFvFxv(2,2,3,2) 
Complex(dp) :: AmpTreeZSscTocFvFxv(2,2,3,2),AmpWaveZSscTocFvFxv(2,2,3,2),AmpVertexZSscTocFvFxv(2,2,3,2) 
Real(dp) :: AmpSqSscTocFvFxv(2,3,2),  AmpSqTreeSscTocFvFxv(2,3,2) 
Real(dp) :: MRPSscToSschh(2,2,2),MRGSscToSschh(2,2,2), MRPZSscToSschh(2,2,2),MRGZSscToSschh(2,2,2) 
Real(dp) :: MVPSscToSschh(2,2,2) 
Real(dp) :: RMsqTreeSscToSschh(2,2,2),RMsqWaveSscToSschh(2,2,2),RMsqVertexSscToSschh(2,2,2) 
Complex(dp) :: AmpTreeSscToSschh(2,2,2),AmpWaveSscToSschh(2,2,2)=(0._dp,0._dp),AmpVertexSscToSschh(2,2,2)& 
 & ,AmpVertexIRosSscToSschh(2,2,2),AmpVertexIRdrSscToSschh(2,2,2), AmpSumSscToSschh(2,2,2), AmpSum2SscToSschh(2,2,2) 
Complex(dp) :: AmpTreeZSscToSschh(2,2,2),AmpWaveZSscToSschh(2,2,2),AmpVertexZSscToSschh(2,2,2) 
Real(dp) :: AmpSqSscToSschh(2,2,2),  AmpSqTreeSscToSschh(2,2,2) 
Real(dp) :: MRPSscToSscVP(2,2),MRGSscToSscVP(2,2), MRPZSscToSscVP(2,2),MRGZSscToSscVP(2,2) 
Real(dp) :: MVPSscToSscVP(2,2) 
Real(dp) :: RMsqTreeSscToSscVP(2,2),RMsqWaveSscToSscVP(2,2),RMsqVertexSscToSscVP(2,2) 
Complex(dp) :: AmpTreeSscToSscVP(2,2,2),AmpWaveSscToSscVP(2,2,2)=(0._dp,0._dp),AmpVertexSscToSscVP(2,2,2)& 
 & ,AmpVertexIRosSscToSscVP(2,2,2),AmpVertexIRdrSscToSscVP(2,2,2), AmpSumSscToSscVP(2,2,2), AmpSum2SscToSscVP(2,2,2) 
Complex(dp) :: AmpTreeZSscToSscVP(2,2,2),AmpWaveZSscToSscVP(2,2,2),AmpVertexZSscToSscVP(2,2,2) 
Real(dp) :: AmpSqSscToSscVP(2,2),  AmpSqTreeSscToSscVP(2,2) 
Real(dp) :: MRPSscToSscVZ(2,2),MRGSscToSscVZ(2,2), MRPZSscToSscVZ(2,2),MRGZSscToSscVZ(2,2) 
Real(dp) :: MVPSscToSscVZ(2,2) 
Real(dp) :: RMsqTreeSscToSscVZ(2,2),RMsqWaveSscToSscVZ(2,2),RMsqVertexSscToSscVZ(2,2) 
Complex(dp) :: AmpTreeSscToSscVZ(2,2,2),AmpWaveSscToSscVZ(2,2,2)=(0._dp,0._dp),AmpVertexSscToSscVZ(2,2,2)& 
 & ,AmpVertexIRosSscToSscVZ(2,2,2),AmpVertexIRdrSscToSscVZ(2,2,2), AmpSumSscToSscVZ(2,2,2), AmpSum2SscToSscVZ(2,2,2) 
Complex(dp) :: AmpTreeZSscToSscVZ(2,2,2),AmpWaveZSscToSscVZ(2,2,2),AmpVertexZSscToSscVZ(2,2,2) 
Real(dp) :: AmpSqSscToSscVZ(2,2),  AmpSqTreeSscToSscVZ(2,2) 
Write(*,*) "Calculating one-loop decays of Ssc " 
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
! bar(Fe) Fxe
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_SscTocFeFxe(cplcFeFxecSscL,cplcFeFxecSscR,              & 
& MFe,MFxe,MSsc,MFe2,MFxe2,MSsc2,AmpTreeSscTocFeFxe)

  Else 
Call Amplitude_Tree_SDdiracDM_SscTocFeFxe(ZcplcFeFxecSscL,ZcplcFeFxecSscR,            & 
& MFe,MFxe,MSsc,MFe2,MFxe2,MSsc2,AmpTreeSscTocFeFxe)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_SscTocFeFxe(MLambda,em,gs,cplcFeFxecSscL,cplcFeFxecSscR,    & 
& MFeOS,MFxeOS,MSscOS,MRPSscTocFeFxe,MRGSscTocFeFxe)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_SscTocFeFxe(MLambda,em,gs,ZcplcFeFxecSscL,ZcplcFeFxecSscR,  & 
& MFeOS,MFxeOS,MSscOS,MRPSscTocFeFxe,MRGSscTocFeFxe)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_SscTocFeFxe(MLambda,em,gs,cplcFeFxecSscL,cplcFeFxecSscR,    & 
& MFe,MFxe,MSsc,MRPSscTocFeFxe,MRGSscTocFeFxe)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_SscTocFeFxe(MLambda,em,gs,ZcplcFeFxecSscL,ZcplcFeFxecSscR,  & 
& MFe,MFxe,MSsc,MRPSscTocFeFxe,MRGSscTocFeFxe)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_SscTocFeFxe(cplcFeFxecSscL,cplcFeFxecSscR,              & 
& ctcplcFeFxecSscL,ctcplcFeFxecSscR,MFe,MFe2,MFxe,MFxe2,MSsc,MSsc2,Zfed,ZfEL,            & 
& ZfER,Zfeu,ZfSsc,AmpWaveSscTocFeFxe)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_SscTocFeFxe(MFe,MFv,MFxe,MFxv,Mhh,MHp,MSsc,           & 
& MVP,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFeFehhL,        & 
& cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,              & 
& cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,    & 
& cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,               & 
& cplcFeFxecSscL,cplcFeFxecSscR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplhhSsccSsc,              & 
& AmpVertexSscTocFeFxe)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_SscTocFeFxe(MFe,MFv,MFxe,MFxv,Mhh,MHp,             & 
& MSsc,MVP,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,               & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,               & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,cplcFxvFxeHpR,     & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,               & 
& cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFvFxvcSscL,cplcFvFxvcSscR,             & 
& cplhhSsccSsc,AmpVertexIRdrSscTocFeFxe)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_SscTocFeFxe(MFeOS,MFvOS,MFxeOS,MFxvOS,             & 
& MhhOS,MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MFe2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,              & 
& MHp2OS,MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,    & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,         & 
& cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR, & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,ZcplcFeFxecSscL,ZcplcFeFxecSscR,cplcFvFxvcSscL,            & 
& cplcFvFxvcSscR,cplhhSsccSsc,AmpVertexIRosSscTocFeFxe)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_SscTocFeFxe(MFe,MFv,MFxe,MFxv,Mhh,MHp,             & 
& MSsc,MVP,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,               & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,               & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,cplcFxvFxeHpR,     & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,               & 
& cplcFxeFxeVZR,ZcplcFeFxecSscL,ZcplcFeFxecSscR,cplcFvFxvcSscL,cplcFvFxvcSscR,           & 
& cplhhSsccSsc,AmpVertexIRosSscTocFeFxe)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_SscTocFeFxe(MFeOS,MFvOS,MFxeOS,MFxvOS,             & 
& MhhOS,MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MFe2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,              & 
& MHp2OS,MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,    & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,         & 
& cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR, & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFvFxvcSscL,              & 
& cplcFvFxvcSscR,cplhhSsccSsc,AmpVertexIRosSscTocFeFxe)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_SscTocFeFxe(MFe,MFv,MFxe,MFxv,Mhh,MHp,             & 
& MSsc,MVP,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,               & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,               & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,cplcFxvFxeHpR,     & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,               & 
& cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFvFxvcSscL,cplcFvFxvcSscR,             & 
& cplhhSsccSsc,AmpVertexIRosSscTocFeFxe)

 End if 
 End if 
AmpVertexSscTocFeFxe = AmpVertexSscTocFeFxe -  AmpVertexIRdrSscTocFeFxe! +  AmpVertexIRosSscTocFeFxe ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZSscTocFeFxe=0._dp 
AmpVertexZSscTocFeFxe=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZSscTocFeFxe(:,gt2,:) = AmpWaveZSscTocFeFxe(:,gt2,:)+ZRUVSs(gt2,gt1)*AmpWaveSscTocFeFxe(:,gt1,:) 
AmpVertexZSscTocFeFxe(:,gt2,:)= AmpVertexZSscTocFeFxe(:,gt2,:) + ZRUVSs(gt2,gt1)*AmpVertexSscTocFeFxe(:,gt1,:) 
 End Do 
End Do 
AmpWaveSscTocFeFxe=AmpWaveZSscTocFeFxe 
AmpVertexSscTocFeFxe= AmpVertexZSscTocFeFxe
! Final State 1 
AmpWaveZSscTocFeFxe=0._dp 
AmpVertexZSscTocFeFxe=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZSscTocFeFxe(1,:,gt2) = AmpWaveZSscTocFeFxe(1,:,gt2)+ZRUUe(gt2,gt1)*AmpWaveSscTocFeFxe(1,:,gt1) 
AmpVertexZSscTocFeFxe(1,:,gt2)= AmpVertexZSscTocFeFxe(1,:,gt2)+ZRUUe(gt2,gt1)*AmpVertexSscTocFeFxe(1,:,gt1) 
AmpWaveZSscTocFeFxe(2,:,gt2) = AmpWaveZSscTocFeFxe(2,:,gt2)+ZRUVec(gt2,gt1)*AmpWaveSscTocFeFxe(2,:,gt1) 
AmpVertexZSscTocFeFxe(2,:,gt2)= AmpVertexZSscTocFeFxe(2,:,gt2)+ZRUVec(gt2,gt1)*AmpVertexSscTocFeFxe(2,:,gt1) 
 End Do 
End Do 
AmpWaveSscTocFeFxe=AmpWaveZSscTocFeFxe 
AmpVertexSscTocFeFxe= AmpVertexZSscTocFeFxe
End if
If (ShiftIRdiv) Then 
AmpVertexSscTocFeFxe = AmpVertexSscTocFeFxe  +  AmpVertexIRosSscTocFeFxe
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Ssc->bar[Fe] Fxe -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumSscTocFeFxe = AmpTreeSscTocFeFxe 
 AmpSum2SscTocFeFxe = AmpTreeSscTocFeFxe + 2._dp*AmpWaveSscTocFeFxe + 2._dp*AmpVertexSscTocFeFxe  
Else 
 AmpSumSscTocFeFxe = AmpTreeSscTocFeFxe + AmpWaveSscTocFeFxe + AmpVertexSscTocFeFxe
 AmpSum2SscTocFeFxe = AmpTreeSscTocFeFxe + AmpWaveSscTocFeFxe + AmpVertexSscTocFeFxe 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumSscTocFeFxe = AmpTreeSscTocFeFxe
 AmpSum2SscTocFeFxe = AmpTreeSscTocFeFxe 
End if 
Do gt1=1,2
i4 = isave 
  Do gt2=1,3
If (((OSkinematics).and.(MSscOS(gt1).gt.(MFeOS(gt2)+MFxeOS))).or.((.not.OSkinematics).and.(MSsc(gt1).gt.(MFe(gt2)+MFxe)))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2 
  AmpSum2SscTocFeFxe = AmpTreeSscTocFeFxe
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MSscOS(gt1),MFeOS(gt2),MFxeOS,AmpSumSscTocFeFxe(:,gt1, gt2),AmpSum2SscTocFeFxe(:,gt1, gt2),AmpSqSscTocFeFxe(gt1, gt2)) 
Else  
  Call SquareAmp_StoFF(MSsc(gt1),MFe(gt2),MFxe,AmpSumSscTocFeFxe(:,gt1, gt2),AmpSum2SscTocFeFxe(:,gt1, gt2),AmpSqSscTocFeFxe(gt1, gt2)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqSscTocFeFxe(gt1, gt2) 
  AmpSum2SscTocFeFxe = 2._dp*AmpWaveSscTocFeFxe
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MSscOS(gt1),MFeOS(gt2),MFxeOS,AmpSumSscTocFeFxe(:,gt1, gt2),AmpSum2SscTocFeFxe(:,gt1, gt2),AmpSqSscTocFeFxe(gt1, gt2)) 
Else  
  Call SquareAmp_StoFF(MSsc(gt1),MFe(gt2),MFxe,AmpSumSscTocFeFxe(:,gt1, gt2),AmpSum2SscTocFeFxe(:,gt1, gt2),AmpSqSscTocFeFxe(gt1, gt2)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqSscTocFeFxe(gt1, gt2) 
  AmpSum2SscTocFeFxe = 2._dp*AmpVertexSscTocFeFxe
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MSscOS(gt1),MFeOS(gt2),MFxeOS,AmpSumSscTocFeFxe(:,gt1, gt2),AmpSum2SscTocFeFxe(:,gt1, gt2),AmpSqSscTocFeFxe(gt1, gt2)) 
Else  
  Call SquareAmp_StoFF(MSsc(gt1),MFe(gt2),MFxe,AmpSumSscTocFeFxe(:,gt1, gt2),AmpSum2SscTocFeFxe(:,gt1, gt2),AmpSqSscTocFeFxe(gt1, gt2)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqSscTocFeFxe(gt1, gt2) 
  AmpSum2SscTocFeFxe = AmpTreeSscTocFeFxe + 2._dp*AmpWaveSscTocFeFxe + 2._dp*AmpVertexSscTocFeFxe
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MSscOS(gt1),MFeOS(gt2),MFxeOS,AmpSumSscTocFeFxe(:,gt1, gt2),AmpSum2SscTocFeFxe(:,gt1, gt2),AmpSqSscTocFeFxe(gt1, gt2)) 
Else  
  Call SquareAmp_StoFF(MSsc(gt1),MFe(gt2),MFxe,AmpSumSscTocFeFxe(:,gt1, gt2),AmpSum2SscTocFeFxe(:,gt1, gt2),AmpSqSscTocFeFxe(gt1, gt2)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqSscTocFeFxe(gt1, gt2) 
 End if 
If (OSkinematics) Then 
  AmpSum2SscTocFeFxe = AmpTreeSscTocFeFxe
  Call SquareAmp_StoFF(MSscOS(gt1),MFeOS(gt2),MFxeOS,AmpSumSscTocFeFxe(:,gt1, gt2),AmpSum2SscTocFeFxe(:,gt1, gt2),AmpSqSscTocFeFxe(gt1, gt2)) 
  AmpSqTreeSscTocFeFxe(gt1, gt2) = AmpSqSscTocFeFxe(gt1, gt2)  
  AmpSum2SscTocFeFxe = + 2._dp*AmpWaveSscTocFeFxe + 2._dp*AmpVertexSscTocFeFxe
  Call SquareAmp_StoFF(MSscOS(gt1),MFeOS(gt2),MFxeOS,AmpSumSscTocFeFxe(:,gt1, gt2),AmpSum2SscTocFeFxe(:,gt1, gt2),AmpSqSscTocFeFxe(gt1, gt2)) 
  AmpSqSscTocFeFxe(gt1, gt2) = AmpSqSscTocFeFxe(gt1, gt2) + AmpSqTreeSscTocFeFxe(gt1, gt2)  
Else  
  AmpSum2SscTocFeFxe = AmpTreeSscTocFeFxe
  Call SquareAmp_StoFF(MSsc(gt1),MFe(gt2),MFxe,AmpSumSscTocFeFxe(:,gt1, gt2),AmpSum2SscTocFeFxe(:,gt1, gt2),AmpSqSscTocFeFxe(gt1, gt2)) 
  AmpSqTreeSscTocFeFxe(gt1, gt2) = AmpSqSscTocFeFxe(gt1, gt2)  
  AmpSum2SscTocFeFxe = + 2._dp*AmpWaveSscTocFeFxe + 2._dp*AmpVertexSscTocFeFxe
  Call SquareAmp_StoFF(MSsc(gt1),MFe(gt2),MFxe,AmpSumSscTocFeFxe(:,gt1, gt2),AmpSum2SscTocFeFxe(:,gt1, gt2),AmpSqSscTocFeFxe(gt1, gt2)) 
  AmpSqSscTocFeFxe(gt1, gt2) = AmpSqSscTocFeFxe(gt1, gt2) + AmpSqTreeSscTocFeFxe(gt1, gt2)  
End if  
Else  
  AmpSqSscTocFeFxe(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 4._dp 
If (AmpSqSscTocFeFxe(gt1, gt2).le.0._dp) Then 
  gP1LSsc(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LSsc(gt1,i4) = 1._dp*GammaTPS(MSscOS(gt1),MFeOS(gt2),MFxeOS,helfactor*AmpSqSscTocFeFxe(gt1, gt2))
Else 
  gP1LSsc(gt1,i4) = 1._dp*GammaTPS(MSsc(gt1),MFe(gt2),MFxe,helfactor*AmpSqSscTocFeFxe(gt1, gt2))
End if 
If ((Abs(MRPSscTocFeFxe(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGSscTocFeFxe(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LSsc(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPSscTocFeFxe(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGSscTocFeFxe(gt1, gt2)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPSscTocFeFxe(gt1, gt2) + MRGSscTocFeFxe(gt1, gt2)) 
  gP1LSsc(gt1,i4) = gP1LSsc(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPSscTocFeFxe(gt1, gt2) + MRGSscTocFeFxe(gt1, gt2))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LSsc(gt1,i4) 
  End if 
End if 
End if 
i4=i4+1

  End do
If (gt1.eq.2) isave = i4 
End do
End If 
If (.not.CalcLoopDecay_LoopInducedOnly) Then 
!---------------- 
! bar(Fv) Fxv
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_SscTocFvFxv(cplcFvFxvcSscL,cplcFvFxvcSscR,              & 
& MFv,MFxv,MSsc,MFv2,MFxv2,MSsc2,AmpTreeSscTocFvFxv)

  Else 
Call Amplitude_Tree_SDdiracDM_SscTocFvFxv(ZcplcFvFxvcSscL,ZcplcFvFxvcSscR,            & 
& MFv,MFxv,MSsc,MFv2,MFxv2,MSsc2,AmpTreeSscTocFvFxv)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_SscTocFvFxv(MLambda,em,gs,cplcFvFxvcSscL,cplcFvFxvcSscR,    & 
& MFvOS,MFxvOS,MSscOS,MRPSscTocFvFxv,MRGSscTocFvFxv)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_SscTocFvFxv(MLambda,em,gs,ZcplcFvFxvcSscL,ZcplcFvFxvcSscR,  & 
& MFvOS,MFxvOS,MSscOS,MRPSscTocFvFxv,MRGSscTocFvFxv)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_SscTocFvFxv(MLambda,em,gs,cplcFvFxvcSscL,cplcFvFxvcSscR,    & 
& MFv,MFxv,MSsc,MRPSscTocFvFxv,MRGSscTocFvFxv)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_SscTocFvFxv(MLambda,em,gs,ZcplcFvFxvcSscL,ZcplcFvFxvcSscR,  & 
& MFv,MFxv,MSsc,MRPSscTocFvFxv,MRGSscTocFvFxv)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_SscTocFvFxv(cplcFvFxvcSscL,cplcFvFxvcSscR,              & 
& ctcplcFvFxvcSscL,ctcplcFvFxvcSscR,MFv,MFv2,MFxv,MFxv2,MSsc,MSsc2,ZfSsc,ZfVL,           & 
& ZfVR,ZfxVL,ZfxVR,AmpWaveSscTocFvFxv)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_SscTocFvFxv(MFe,MFv,MFxe,MFxv,Mhh,MHp,MSsc,           & 
& MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFvFeHpL,cplcFvFeHpR,     & 
& cplcFvFeVWpL,cplcFvFeVWpR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFxecSscL,cplcFeFxecSscR,       & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR, & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhSsccSsc,            & 
& AmpVertexSscTocFvFxv)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_SscTocFvFxv(MFe,MFv,MFxe,MFxv,Mhh,MHp,             & 
& MSsc,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFvFeHpL,            & 
& cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFxecSscL,          & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL, & 
& cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,          & 
& cplhhSsccSsc,AmpVertexIRdrSscTocFvFxv)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_SscTocFvFxv(MFeOS,MFvOS,MFxeOS,MFxvOS,             & 
& MhhOS,MHpOS,MSscOS,MVWpOS,MVZOS,MFe2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,           & 
& MSsc2OS,MVWp2OS,MVZ2OS,cplcFvFeHpL,cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,              & 
& cplcFvFvVZL,cplcFvFvVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,     & 
& cplcFxvFxvVZL,cplcFxvFxvVZR,ZcplcFvFxvcSscL,ZcplcFvFxvcSscR,cplcFxeFxvcHpL,            & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhSsccSsc,AmpVertexIRosSscTocFvFxv)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_SscTocFvFxv(MFe,MFv,MFxe,MFxv,Mhh,MHp,             & 
& MSsc,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFvFeHpL,            & 
& cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFxecSscL,          & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,ZcplcFvFxvcSscL,& 
& ZcplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,         & 
& cplhhSsccSsc,AmpVertexIRosSscTocFvFxv)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_SscTocFvFxv(MFeOS,MFvOS,MFxeOS,MFxvOS,             & 
& MhhOS,MHpOS,MSscOS,MVWpOS,MVZOS,MFe2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,           & 
& MSsc2OS,MVWp2OS,MVZ2OS,cplcFvFeHpL,cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,              & 
& cplcFvFvVZL,cplcFvFvVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,     & 
& cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,              & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhSsccSsc,AmpVertexIRosSscTocFvFxv)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_SscTocFvFxv(MFe,MFv,MFxe,MFxv,Mhh,MHp,             & 
& MSsc,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFvFeHpL,            & 
& cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFxecSscL,          & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL, & 
& cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,          & 
& cplhhSsccSsc,AmpVertexIRosSscTocFvFxv)

 End if 
 End if 
AmpVertexSscTocFvFxv = AmpVertexSscTocFvFxv -  AmpVertexIRdrSscTocFvFxv! +  AmpVertexIRosSscTocFvFxv ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZSscTocFvFxv=0._dp 
AmpVertexZSscTocFvFxv=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZSscTocFvFxv(:,gt2,:,:) = AmpWaveZSscTocFvFxv(:,gt2,:,:)+ZRUVSs(gt2,gt1)*AmpWaveSscTocFvFxv(:,gt1,:,:) 
AmpVertexZSscTocFvFxv(:,gt2,:,:)= AmpVertexZSscTocFvFxv(:,gt2,:,:) + ZRUVSs(gt2,gt1)*AmpVertexSscTocFvFxv(:,gt1,:,:) 
 End Do 
End Do 
AmpWaveSscTocFvFxv=AmpWaveZSscTocFvFxv 
AmpVertexSscTocFvFxv= AmpVertexZSscTocFvFxv
! Final State 1 
AmpWaveZSscTocFvFxv=0._dp 
AmpVertexZSscTocFvFxv=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZSscTocFvFxv(1,:,gt2,:) = AmpWaveZSscTocFvFxv(1,:,gt2,:)+ZRUVvr(gt2,gt1)*AmpWaveSscTocFvFxv(1,:,gt1,:) 
AmpVertexZSscTocFvFxv(1,:,gt2,:)= AmpVertexZSscTocFvFxv(1,:,gt2,:)+ZRUVvr(gt2,gt1)*AmpVertexSscTocFvFxv(1,:,gt1,:) 
AmpWaveZSscTocFvFxv(2,:,gt2,:) = AmpWaveZSscTocFvFxv(2,:,gt2,:)+ZRUVvc(gt2,gt1)*AmpWaveSscTocFvFxv(2,:,gt1,:) 
AmpVertexZSscTocFvFxv(2,:,gt2,:)= AmpVertexZSscTocFvFxv(2,:,gt2,:)+ZRUVvc(gt2,gt1)*AmpVertexSscTocFvFxv(2,:,gt1,:) 
 End Do 
End Do 
AmpWaveSscTocFvFxv=AmpWaveZSscTocFvFxv 
AmpVertexSscTocFvFxv= AmpVertexZSscTocFvFxv
! Final State 2 
AmpWaveZSscTocFvFxv=0._dp 
AmpVertexZSscTocFvFxv=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZSscTocFvFxv(1,:,:,gt2) = AmpWaveZSscTocFvFxv(1,:,:,gt2)+ZRUXV(gt2,gt1)*AmpWaveSscTocFvFxv(1,:,:,gt1) 
AmpVertexZSscTocFvFxv(1,:,:,gt2)= AmpVertexZSscTocFvFxv(1,:,:,gt2)+ZRUXV(gt2,gt1)*AmpVertexSscTocFvFxv(1,:,:,gt1) 
AmpWaveZSscTocFvFxv(2,:,:,gt2) = AmpWaveZSscTocFvFxv(2,:,:,gt2)+ZRUXU(gt2,gt1)*AmpWaveSscTocFvFxv(2,:,:,gt1) 
AmpVertexZSscTocFvFxv(2,:,:,gt2)= AmpVertexZSscTocFvFxv(2,:,:,gt2)+ZRUXU(gt2,gt1)*AmpVertexSscTocFvFxv(2,:,:,gt1) 
 End Do 
End Do 
AmpWaveSscTocFvFxv=AmpWaveZSscTocFvFxv 
AmpVertexSscTocFvFxv= AmpVertexZSscTocFvFxv
End if
If (ShiftIRdiv) Then 
AmpVertexSscTocFvFxv = AmpVertexSscTocFvFxv  +  AmpVertexIRosSscTocFvFxv
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Ssc->bar[Fv] Fxv -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumSscTocFvFxv = AmpTreeSscTocFvFxv 
 AmpSum2SscTocFvFxv = AmpTreeSscTocFvFxv + 2._dp*AmpWaveSscTocFvFxv + 2._dp*AmpVertexSscTocFvFxv  
Else 
 AmpSumSscTocFvFxv = AmpTreeSscTocFvFxv + AmpWaveSscTocFvFxv + AmpVertexSscTocFvFxv
 AmpSum2SscTocFvFxv = AmpTreeSscTocFvFxv + AmpWaveSscTocFvFxv + AmpVertexSscTocFvFxv 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumSscTocFvFxv = AmpTreeSscTocFvFxv
 AmpSum2SscTocFvFxv = AmpTreeSscTocFvFxv 
End if 
Do gt1=1,2
i4 = isave 
  Do gt2=1,3
    Do gt3=1,2
If (((OSkinematics).and.(MSscOS(gt1).gt.(MFvOS(gt2)+MFxvOS(gt3)))).or.((.not.OSkinematics).and.(MSsc(gt1).gt.(MFv(gt2)+MFxv(gt3))))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2, gt3 
  AmpSum2SscTocFvFxv = AmpTreeSscTocFvFxv
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MSscOS(gt1),MFvOS(gt2),MFxvOS(gt3),AmpSumSscTocFvFxv(:,gt1, gt2, gt3),AmpSum2SscTocFvFxv(:,gt1, gt2, gt3),AmpSqSscTocFvFxv(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(MSsc(gt1),MFv(gt2),MFxv(gt3),AmpSumSscTocFvFxv(:,gt1, gt2, gt3),AmpSum2SscTocFvFxv(:,gt1, gt2, gt3),AmpSqSscTocFvFxv(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqSscTocFvFxv(gt1, gt2, gt3) 
  AmpSum2SscTocFvFxv = 2._dp*AmpWaveSscTocFvFxv
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MSscOS(gt1),MFvOS(gt2),MFxvOS(gt3),AmpSumSscTocFvFxv(:,gt1, gt2, gt3),AmpSum2SscTocFvFxv(:,gt1, gt2, gt3),AmpSqSscTocFvFxv(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(MSsc(gt1),MFv(gt2),MFxv(gt3),AmpSumSscTocFvFxv(:,gt1, gt2, gt3),AmpSum2SscTocFvFxv(:,gt1, gt2, gt3),AmpSqSscTocFvFxv(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqSscTocFvFxv(gt1, gt2, gt3) 
  AmpSum2SscTocFvFxv = 2._dp*AmpVertexSscTocFvFxv
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MSscOS(gt1),MFvOS(gt2),MFxvOS(gt3),AmpSumSscTocFvFxv(:,gt1, gt2, gt3),AmpSum2SscTocFvFxv(:,gt1, gt2, gt3),AmpSqSscTocFvFxv(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(MSsc(gt1),MFv(gt2),MFxv(gt3),AmpSumSscTocFvFxv(:,gt1, gt2, gt3),AmpSum2SscTocFvFxv(:,gt1, gt2, gt3),AmpSqSscTocFvFxv(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqSscTocFvFxv(gt1, gt2, gt3) 
  AmpSum2SscTocFvFxv = AmpTreeSscTocFvFxv + 2._dp*AmpWaveSscTocFvFxv + 2._dp*AmpVertexSscTocFvFxv
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MSscOS(gt1),MFvOS(gt2),MFxvOS(gt3),AmpSumSscTocFvFxv(:,gt1, gt2, gt3),AmpSum2SscTocFvFxv(:,gt1, gt2, gt3),AmpSqSscTocFvFxv(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(MSsc(gt1),MFv(gt2),MFxv(gt3),AmpSumSscTocFvFxv(:,gt1, gt2, gt3),AmpSum2SscTocFvFxv(:,gt1, gt2, gt3),AmpSqSscTocFvFxv(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqSscTocFvFxv(gt1, gt2, gt3) 
 End if 
If (OSkinematics) Then 
  AmpSum2SscTocFvFxv = AmpTreeSscTocFvFxv
  Call SquareAmp_StoFF(MSscOS(gt1),MFvOS(gt2),MFxvOS(gt3),AmpSumSscTocFvFxv(:,gt1, gt2, gt3),AmpSum2SscTocFvFxv(:,gt1, gt2, gt3),AmpSqSscTocFvFxv(gt1, gt2, gt3)) 
  AmpSqTreeSscTocFvFxv(gt1, gt2, gt3) = AmpSqSscTocFvFxv(gt1, gt2, gt3)  
  AmpSum2SscTocFvFxv = + 2._dp*AmpWaveSscTocFvFxv + 2._dp*AmpVertexSscTocFvFxv
  Call SquareAmp_StoFF(MSscOS(gt1),MFvOS(gt2),MFxvOS(gt3),AmpSumSscTocFvFxv(:,gt1, gt2, gt3),AmpSum2SscTocFvFxv(:,gt1, gt2, gt3),AmpSqSscTocFvFxv(gt1, gt2, gt3)) 
  AmpSqSscTocFvFxv(gt1, gt2, gt3) = AmpSqSscTocFvFxv(gt1, gt2, gt3) + AmpSqTreeSscTocFvFxv(gt1, gt2, gt3)  
Else  
  AmpSum2SscTocFvFxv = AmpTreeSscTocFvFxv
  Call SquareAmp_StoFF(MSsc(gt1),MFv(gt2),MFxv(gt3),AmpSumSscTocFvFxv(:,gt1, gt2, gt3),AmpSum2SscTocFvFxv(:,gt1, gt2, gt3),AmpSqSscTocFvFxv(gt1, gt2, gt3)) 
  AmpSqTreeSscTocFvFxv(gt1, gt2, gt3) = AmpSqSscTocFvFxv(gt1, gt2, gt3)  
  AmpSum2SscTocFvFxv = + 2._dp*AmpWaveSscTocFvFxv + 2._dp*AmpVertexSscTocFvFxv
  Call SquareAmp_StoFF(MSsc(gt1),MFv(gt2),MFxv(gt3),AmpSumSscTocFvFxv(:,gt1, gt2, gt3),AmpSum2SscTocFvFxv(:,gt1, gt2, gt3),AmpSqSscTocFvFxv(gt1, gt2, gt3)) 
  AmpSqSscTocFvFxv(gt1, gt2, gt3) = AmpSqSscTocFvFxv(gt1, gt2, gt3) + AmpSqTreeSscTocFvFxv(gt1, gt2, gt3)  
End if  
Else  
  AmpSqSscTocFvFxv(gt1, gt2, gt3) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 4._dp 
If (AmpSqSscTocFvFxv(gt1, gt2, gt3).le.0._dp) Then 
  gP1LSsc(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LSsc(gt1,i4) = 1._dp*GammaTPS(MSscOS(gt1),MFvOS(gt2),MFxvOS(gt3),helfactor*AmpSqSscTocFvFxv(gt1, gt2, gt3))
Else 
  gP1LSsc(gt1,i4) = 1._dp*GammaTPS(MSsc(gt1),MFv(gt2),MFxv(gt3),helfactor*AmpSqSscTocFvFxv(gt1, gt2, gt3))
End if 
If ((Abs(MRPSscTocFvFxv(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGSscTocFvFxv(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LSsc(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPSscTocFvFxv(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGSscTocFvFxv(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPSscTocFvFxv(gt1, gt2, gt3) + MRGSscTocFvFxv(gt1, gt2, gt3)) 
  gP1LSsc(gt1,i4) = gP1LSsc(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPSscTocFvFxv(gt1, gt2, gt3) + MRGSscTocFvFxv(gt1, gt2, gt3))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LSsc(gt1,i4) 
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
! Ssc hh
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_SscToSschh(cplhhSsccSsc,Mhh,MSsc,Mhh2,MSsc2,            & 
& AmpTreeSscToSschh)

  Else 
Call Amplitude_Tree_SDdiracDM_SscToSschh(ZcplhhSsccSsc,Mhh,MSsc,Mhh2,MSsc2,           & 
& AmpTreeSscToSschh)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_SscToSschh(MLambda,em,gs,cplhhSsccSsc,MhhOS,MSscOS,         & 
& MRPSscToSschh,MRGSscToSschh)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_SscToSschh(MLambda,em,gs,ZcplhhSsccSsc,MhhOS,               & 
& MSscOS,MRPSscToSschh,MRGSscToSschh)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_SscToSschh(MLambda,em,gs,cplhhSsccSsc,Mhh,MSsc,             & 
& MRPSscToSschh,MRGSscToSschh)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_SscToSschh(MLambda,em,gs,ZcplhhSsccSsc,Mhh,MSsc,            & 
& MRPSscToSschh,MRGSscToSschh)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_SscToSschh(cplhhSsccSsc,ctcplhhSsccSsc,Mhh,             & 
& Mhh2,MSsc,MSsc2,Zfhh,ZfSsc,AmpWaveSscToSschh)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_SscToSschh(MAh,MFe,MFv,MFxe,MFxv,Mhh,MHp,             & 
& MSsc,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,cplAhAhhh,cplcFeFehhL,cplcFeFehhR,     & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFeFxecSscL,cplcFeFxecSscR, & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplhhhhhh,cplhhHpcHp,        & 
& cplhhSsccSsc,cplAhAhSsccSsc1,cplhhhhSsccSsc1,cplHpSsccHpcSsc1,cplSscSsccSsccSsc1,      & 
& AmpVertexSscToSschh)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_SscToSschh(MAh,MFe,MFv,MFxe,MFxv,Mhh,              & 
& MHp,MSsc,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,cplAhAhhh,cplcFeFehhL,             & 
& cplcFeFehhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFeFxecSscL,    & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFvFxvcSscL,cplcFvFxvcSscR,              & 
& cplhhhhhh,cplhhHpcHp,cplhhSsccSsc,cplAhAhSsccSsc1,cplhhhhSsccSsc1,cplHpSsccHpcSsc1,    & 
& cplSscSsccSsccSsc1,AmpVertexIRdrSscToSschh)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_SscToSschh(MAhOS,MFeOS,MFvOS,MFxeOS,               & 
& MFxvOS,MhhOS,MHpOS,MSscOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,          & 
& MSsc2OS,cplAhAhhh,cplcFeFehhL,cplcFeFehhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFvSscL,   & 
& cplcFxvFvSscR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,               & 
& cplcFvFxvcSscL,cplcFvFxvcSscR,cplhhhhhh,cplhhHpcHp,ZcplhhSsccSsc,cplAhAhSsccSsc1,      & 
& cplhhhhSsccSsc1,cplHpSsccHpcSsc1,cplSscSsccSsccSsc1,AmpVertexIRosSscToSschh)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_SscToSschh(MAh,MFe,MFv,MFxe,MFxv,Mhh,              & 
& MHp,MSsc,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,cplAhAhhh,cplcFeFehhL,             & 
& cplcFeFehhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFeFxecSscL,    & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFvFxvcSscL,cplcFvFxvcSscR,              & 
& cplhhhhhh,cplhhHpcHp,ZcplhhSsccSsc,cplAhAhSsccSsc1,cplhhhhSsccSsc1,cplHpSsccHpcSsc1,   & 
& cplSscSsccSsccSsc1,AmpVertexIRosSscToSschh)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_SscToSschh(MAhOS,MFeOS,MFvOS,MFxeOS,               & 
& MFxvOS,MhhOS,MHpOS,MSscOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,          & 
& MSsc2OS,cplAhAhhh,cplcFeFehhL,cplcFeFehhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFvSscL,   & 
& cplcFxvFvSscR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,               & 
& cplcFvFxvcSscL,cplcFvFxvcSscR,cplhhhhhh,cplhhHpcHp,cplhhSsccSsc,cplAhAhSsccSsc1,       & 
& cplhhhhSsccSsc1,cplHpSsccHpcSsc1,cplSscSsccSsccSsc1,AmpVertexIRosSscToSschh)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_SscToSschh(MAh,MFe,MFv,MFxe,MFxv,Mhh,              & 
& MHp,MSsc,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,cplAhAhhh,cplcFeFehhL,             & 
& cplcFeFehhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFeFxecSscL,    & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFvFxvcSscL,cplcFvFxvcSscR,              & 
& cplhhhhhh,cplhhHpcHp,cplhhSsccSsc,cplAhAhSsccSsc1,cplhhhhSsccSsc1,cplHpSsccHpcSsc1,    & 
& cplSscSsccSsccSsc1,AmpVertexIRosSscToSschh)

 End if 
 End if 
AmpVertexSscToSschh = AmpVertexSscToSschh -  AmpVertexIRdrSscToSschh! +  AmpVertexIRosSscToSschh ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZSscToSschh=0._dp 
AmpVertexZSscToSschh=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZSscToSschh(gt2,:,:) = AmpWaveZSscToSschh(gt2,:,:)+ZRUVSs(gt2,gt1)*AmpWaveSscToSschh(gt1,:,:) 
AmpVertexZSscToSschh(gt2,:,:)= AmpVertexZSscToSschh(gt2,:,:) + ZRUVSs(gt2,gt1)*AmpVertexSscToSschh(gt1,:,:) 
 End Do 
End Do 
AmpWaveSscToSschh=AmpWaveZSscToSschh 
AmpVertexSscToSschh= AmpVertexZSscToSschh
! Final State 1 
AmpWaveZSscToSschh=0._dp 
AmpVertexZSscToSschh=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZSscToSschh(:,gt2,:) = AmpWaveZSscToSschh(:,gt2,:)+ZRUVSs(gt2,gt1)*AmpWaveSscToSschh(:,gt1,:) 
AmpVertexZSscToSschh(:,gt2,:)= AmpVertexZSscToSschh(:,gt2,:)+ZRUVSs(gt2,gt1)*AmpVertexSscToSschh(:,gt1,:) 
 End Do 
End Do 
AmpWaveSscToSschh=AmpWaveZSscToSschh 
AmpVertexSscToSschh= AmpVertexZSscToSschh
! Final State 2 
AmpWaveZSscToSschh=0._dp 
AmpVertexZSscToSschh=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZSscToSschh(:,:,gt2) = AmpWaveZSscToSschh(:,:,gt2)+ZRUZH(gt2,gt1)*AmpWaveSscToSschh(:,:,gt1) 
AmpVertexZSscToSschh(:,:,gt2)= AmpVertexZSscToSschh(:,:,gt2)+ZRUZH(gt2,gt1)*AmpVertexSscToSschh(:,:,gt1) 
 End Do 
End Do 
AmpWaveSscToSschh=AmpWaveZSscToSschh 
AmpVertexSscToSschh= AmpVertexZSscToSschh
End if
If (ShiftIRdiv) Then 
AmpVertexSscToSschh = AmpVertexSscToSschh  +  AmpVertexIRosSscToSschh
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Ssc->Ssc hh -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumSscToSschh = AmpTreeSscToSschh 
 AmpSum2SscToSschh = AmpTreeSscToSschh + 2._dp*AmpWaveSscToSschh + 2._dp*AmpVertexSscToSschh  
Else 
 AmpSumSscToSschh = AmpTreeSscToSschh + AmpWaveSscToSschh + AmpVertexSscToSschh
 AmpSum2SscToSschh = AmpTreeSscToSschh + AmpWaveSscToSschh + AmpVertexSscToSschh 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumSscToSschh = AmpTreeSscToSschh
 AmpSum2SscToSschh = AmpTreeSscToSschh 
End if 
Do gt1=1,2
i4 = isave 
  Do gt2=1,2
    Do gt3=1,2
If (((OSkinematics).and.(MSscOS(gt1).gt.(MSscOS(gt2)+MhhOS(gt3)))).or.((.not.OSkinematics).and.(MSsc(gt1).gt.(MSsc(gt2)+Mhh(gt3))))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2, gt3 
  AmpSum2SscToSschh = AmpTreeSscToSschh
If (OSkinematics) Then 
  Call SquareAmp_StoSS(MSscOS(gt1),MSscOS(gt2),MhhOS(gt3),AmpSumSscToSschh(gt1, gt2, gt3),AmpSum2SscToSschh(gt1, gt2, gt3),AmpSqSscToSschh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoSS(MSsc(gt1),MSsc(gt2),Mhh(gt3),AmpSumSscToSschh(gt1, gt2, gt3),AmpSum2SscToSschh(gt1, gt2, gt3),AmpSqSscToSschh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqSscToSschh(gt1, gt2, gt3) 
  AmpSum2SscToSschh = 2._dp*AmpWaveSscToSschh
If (OSkinematics) Then 
  Call SquareAmp_StoSS(MSscOS(gt1),MSscOS(gt2),MhhOS(gt3),AmpSumSscToSschh(gt1, gt2, gt3),AmpSum2SscToSschh(gt1, gt2, gt3),AmpSqSscToSschh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoSS(MSsc(gt1),MSsc(gt2),Mhh(gt3),AmpSumSscToSschh(gt1, gt2, gt3),AmpSum2SscToSschh(gt1, gt2, gt3),AmpSqSscToSschh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqSscToSschh(gt1, gt2, gt3) 
  AmpSum2SscToSschh = 2._dp*AmpVertexSscToSschh
If (OSkinematics) Then 
  Call SquareAmp_StoSS(MSscOS(gt1),MSscOS(gt2),MhhOS(gt3),AmpSumSscToSschh(gt1, gt2, gt3),AmpSum2SscToSschh(gt1, gt2, gt3),AmpSqSscToSschh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoSS(MSsc(gt1),MSsc(gt2),Mhh(gt3),AmpSumSscToSschh(gt1, gt2, gt3),AmpSum2SscToSschh(gt1, gt2, gt3),AmpSqSscToSschh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqSscToSschh(gt1, gt2, gt3) 
  AmpSum2SscToSschh = AmpTreeSscToSschh + 2._dp*AmpWaveSscToSschh + 2._dp*AmpVertexSscToSschh
If (OSkinematics) Then 
  Call SquareAmp_StoSS(MSscOS(gt1),MSscOS(gt2),MhhOS(gt3),AmpSumSscToSschh(gt1, gt2, gt3),AmpSum2SscToSschh(gt1, gt2, gt3),AmpSqSscToSschh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoSS(MSsc(gt1),MSsc(gt2),Mhh(gt3),AmpSumSscToSschh(gt1, gt2, gt3),AmpSum2SscToSschh(gt1, gt2, gt3),AmpSqSscToSschh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqSscToSschh(gt1, gt2, gt3) 
 End if 
If (OSkinematics) Then 
  AmpSum2SscToSschh = AmpTreeSscToSschh
  Call SquareAmp_StoSS(MSscOS(gt1),MSscOS(gt2),MhhOS(gt3),AmpSumSscToSschh(gt1, gt2, gt3),AmpSum2SscToSschh(gt1, gt2, gt3),AmpSqSscToSschh(gt1, gt2, gt3)) 
  AmpSqTreeSscToSschh(gt1, gt2, gt3) = AmpSqSscToSschh(gt1, gt2, gt3)  
  AmpSum2SscToSschh = + 2._dp*AmpWaveSscToSschh + 2._dp*AmpVertexSscToSschh
  Call SquareAmp_StoSS(MSscOS(gt1),MSscOS(gt2),MhhOS(gt3),AmpSumSscToSschh(gt1, gt2, gt3),AmpSum2SscToSschh(gt1, gt2, gt3),AmpSqSscToSschh(gt1, gt2, gt3)) 
  AmpSqSscToSschh(gt1, gt2, gt3) = AmpSqSscToSschh(gt1, gt2, gt3) + AmpSqTreeSscToSschh(gt1, gt2, gt3)  
Else  
  AmpSum2SscToSschh = AmpTreeSscToSschh
  Call SquareAmp_StoSS(MSsc(gt1),MSsc(gt2),Mhh(gt3),AmpSumSscToSschh(gt1, gt2, gt3),AmpSum2SscToSschh(gt1, gt2, gt3),AmpSqSscToSschh(gt1, gt2, gt3)) 
  AmpSqTreeSscToSschh(gt1, gt2, gt3) = AmpSqSscToSschh(gt1, gt2, gt3)  
  AmpSum2SscToSschh = + 2._dp*AmpWaveSscToSschh + 2._dp*AmpVertexSscToSschh
  Call SquareAmp_StoSS(MSsc(gt1),MSsc(gt2),Mhh(gt3),AmpSumSscToSschh(gt1, gt2, gt3),AmpSum2SscToSschh(gt1, gt2, gt3),AmpSqSscToSschh(gt1, gt2, gt3)) 
  AmpSqSscToSschh(gt1, gt2, gt3) = AmpSqSscToSschh(gt1, gt2, gt3) + AmpSqTreeSscToSschh(gt1, gt2, gt3)  
End if  
Else  
  AmpSqSscToSschh(gt1, gt2, gt3) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 1._dp 
If (AmpSqSscToSschh(gt1, gt2, gt3).le.0._dp) Then 
  gP1LSsc(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LSsc(gt1,i4) = 1._dp*GammaTPS(MSscOS(gt1),MSscOS(gt2),MhhOS(gt3),helfactor*AmpSqSscToSschh(gt1, gt2, gt3))
Else 
  gP1LSsc(gt1,i4) = 1._dp*GammaTPS(MSsc(gt1),MSsc(gt2),Mhh(gt3),helfactor*AmpSqSscToSschh(gt1, gt2, gt3))
End if 
If ((Abs(MRPSscToSschh(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGSscToSschh(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LSsc(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPSscToSschh(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGSscToSschh(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPSscToSschh(gt1, gt2, gt3) + MRGSscToSschh(gt1, gt2, gt3)) 
  gP1LSsc(gt1,i4) = gP1LSsc(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPSscToSschh(gt1, gt2, gt3) + MRGSscToSschh(gt1, gt2, gt3))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1LSsc(gt1,i4) 
  End if 
End if 
End if 
i4=i4+1

    End do
  End do
If (gt1.eq.2) isave = i4 
End do
End If 
!---------------- 
! Ssc VP
!---------------- 

If (LoopInducedDecaysOS) Then 


!Self-energy Corrections 


!Vertex Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_VERTEX_SDdiracDM_SscToSscVP(MFeOS,MFxeOS,MSscOS,MVP,MFe2OS,            & 
& MFxe2OS,MSsc2OS,MVP2,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,              & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFeFxecSscL,cplcFeFxecSscR,AmpVertexSscToSscVP)

 Else 
Call Amplitude_VERTEX_SDdiracDM_SscToSscVP(MFeOS,MFxeOS,MSscOS,MVP,MFe2OS,            & 
& MFxe2OS,MSsc2OS,MVP2,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,              & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFeFxecSscL,cplcFeFxecSscR,AmpVertexSscToSscVP)

 End if 
Else 


!Self-energy Corrections 


!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_SscToSscVP(MFe,MFxe,MSsc,MVP,MFe2,MFxe2,              & 
& MSsc2,MVP2,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFxeFxeVPL,          & 
& cplcFxeFxeVPR,cplcFeFxecSscL,cplcFeFxecSscR,AmpVertexSscToSscVP)

End if 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Ssc->Ssc VP -----------------------" 
End if 
If (OnlyTreeLevelContributions) Then 
 AmpSumSscToSscVP = 0._dp 
 AmpSum2SscToSscVP = 0._dp  
Else 
 AmpSumSscToSscVP = AmpVertexSscToSscVP + AmpWaveSscToSscVP
 AmpSum2SscToSscVP = AmpVertexSscToSscVP + AmpWaveSscToSscVP 
End If 
Do gt1=1,2
i4 = isave 
  Do gt2=1,2
If (((OSkinematics).and.(MSscOS(gt1).gt.(MSscOS(gt2)+0.))).or.((.not.OSkinematics).and.(MSsc(gt1).gt.(MSsc(gt2)+MVP)))) Then 
If (OSkinematics) Then 
  Call SquareAmp_StoSV(MSscOS(gt1),MSscOS(gt2),0._dp,AmpSumSscToSscVP(:,gt1, gt2),AmpSum2SscToSscVP(:,gt1, gt2),AmpSqSscToSscVP(gt1, gt2)) 
Else  
  Call SquareAmp_StoSV(MSsc(gt1),MSsc(gt2),MVP,AmpSumSscToSscVP(:,gt1, gt2),AmpSum2SscToSscVP(:,gt1, gt2),AmpSqSscToSscVP(gt1, gt2)) 
End if  
Else  
  AmpSqSscToSscVP(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 1._dp 
If (AmpSqSscToSscVP(gt1, gt2).le.0._dp) Then 
  gP1LSsc(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LSsc(gt1,i4) = 1._dp*GammaTPS(MSscOS(gt1),MSscOS(gt2),0._dp,helfactor*AmpSqSscToSscVP(gt1, gt2))
Else 
  gP1LSsc(gt1,i4) = 1._dp*GammaTPS(MSsc(gt1),MSsc(gt2),MVP,helfactor*AmpSqSscToSscVP(gt1, gt2))
End if 
If ((Abs(MRPSscToSscVP(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGSscToSscVP(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LSsc(gt1,i4) 
End if 
i4=i4+1

  End do
If (gt1.eq.2) isave = i4 
End do
!---------------- 
! Ssc VZ
!---------------- 

If (LoopInducedDecaysOS) Then 


!Self-energy Corrections 


!Vertex Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_VERTEX_SDdiracDM_SscToSscVZ(MFeOS,MFvOS,MFxeOS,MFxvOS,MSscOS,          & 
& MVZOS,MFe2OS,MFv2OS,MFxe2OS,MFxv2OS,MSsc2OS,MVZ2OS,cplcFxeFeSscL,cplcFxeFeSscR,        & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,           & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvVZL,               & 
& cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,AmpVertexSscToSscVZ)

 Else 
Call Amplitude_VERTEX_SDdiracDM_SscToSscVZ(MFeOS,MFvOS,MFxeOS,MFxvOS,MSscOS,          & 
& MVZOS,MFe2OS,MFv2OS,MFxe2OS,MFxv2OS,MSsc2OS,MVZ2OS,cplcFxeFeSscL,cplcFxeFeSscR,        & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,           & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvVZL,               & 
& cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,AmpVertexSscToSscVZ)

 End if 
Else 


!Self-energy Corrections 


!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_SscToSscVZ(MFe,MFv,MFxe,MFxv,MSsc,MVZ,MFe2,           & 
& MFv2,MFxe2,MFxv2,MSsc2,MVZ2,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVZL,cplcFeFeVZR,       & 
& cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,cplcFxeFxeVZL,cplcFxeFxeVZR,       & 
& cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,              & 
& cplcFvFxvcSscR,AmpVertexSscToSscVZ)

End if 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ Ssc->Ssc VZ -----------------------" 
End if 
If (OnlyTreeLevelContributions) Then 
 AmpSumSscToSscVZ = 0._dp 
 AmpSum2SscToSscVZ = 0._dp  
Else 
 AmpSumSscToSscVZ = AmpVertexSscToSscVZ + AmpWaveSscToSscVZ
 AmpSum2SscToSscVZ = AmpVertexSscToSscVZ + AmpWaveSscToSscVZ 
End If 
Do gt1=1,2
i4 = isave 
  Do gt2=1,2
If (((OSkinematics).and.(MSscOS(gt1).gt.(MSscOS(gt2)+MVZOS))).or.((.not.OSkinematics).and.(MSsc(gt1).gt.(MSsc(gt2)+MVZ)))) Then 
If (OSkinematics) Then 
  Call SquareAmp_StoSV(MSscOS(gt1),MSscOS(gt2),MVZOS,AmpSumSscToSscVZ(:,gt1, gt2),AmpSum2SscToSscVZ(:,gt1, gt2),AmpSqSscToSscVZ(gt1, gt2)) 
Else  
  Call SquareAmp_StoSV(MSsc(gt1),MSsc(gt2),MVZ,AmpSumSscToSscVZ(:,gt1, gt2),AmpSum2SscToSscVZ(:,gt1, gt2),AmpSqSscToSscVZ(gt1, gt2)) 
End if  
Else  
  AmpSqSscToSscVZ(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 1._dp 
If (AmpSqSscToSscVZ(gt1, gt2).le.0._dp) Then 
  gP1LSsc(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1LSsc(gt1,i4) = 1._dp*GammaTPS(MSscOS(gt1),MSscOS(gt2),MVZOS,helfactor*AmpSqSscToSscVZ(gt1, gt2))
Else 
  gP1LSsc(gt1,i4) = 1._dp*GammaTPS(MSsc(gt1),MSsc(gt2),MVZ,helfactor*AmpSqSscToSscVZ(gt1, gt2))
End if 
If ((Abs(MRPSscToSscVZ(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGSscToSscVZ(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1LSsc(gt1,i4) 
End if 
i4=i4+1

  End do
If (gt1.eq.2) isave = i4 
End do
End Subroutine OneLoopDecay_Ssc

End Module Wrapper_OneLoopDecay_Ssc_SDdiracDM
