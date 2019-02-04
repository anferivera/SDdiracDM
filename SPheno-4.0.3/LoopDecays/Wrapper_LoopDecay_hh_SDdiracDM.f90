! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:22 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module Wrapper_OneLoopDecay_hh_SDdiracDM
Use Model_Data_SDdiracDM 
Use Kinematics 
Use OneLoopDecay_hh_SDdiracDM 
Use Control 
Use Settings 

 
Contains

 
Subroutine OneLoopDecay_hh(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,MFeOS,              & 
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
& ZfVL,ZfVR,ZfxVL,ZfxVR,ZfSsc,ZfVPVZ,ZfVZVP,cplAhAhAhAh1,cplAhAhcVWpVWp1,cplAhAhhh,      & 
& cplAhAhhhhh1,cplAhAhHpcHp1,cplAhAhSsccSsc1,cplAhAhVZVZ1,cplAhcHpVPVWp1,cplAhcHpVWp,    & 
& cplAhcHpVWpVZ1,cplAhhhVZ,cplAhHpcVWp,cplAhHpcVWpVP1,cplAhHpcVWpVZ1,cplcFdFdAhL,        & 
& cplcFdFdAhR,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,               & 
& cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplcFeFeAhL,cplcFeFeAhR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,             & 
& cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,           & 
& cplcFeFvcVWpR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,      & 
& cplcFuFdVWpR,cplcFuFuAhL,cplcFuFuAhR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,              & 
& cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFvFeHpL,               & 
& cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFvFvVZL,cplcFvFvVZR,cplcFvFxvcSscL,          & 
& cplcFvFxvcSscR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL, & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplcFxvFvSscL,cplcFxvFvSscR,            & 
& cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvAhL,               & 
& cplcFxvFxvAhR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgAgWCVWp,    & 
& cplcgAgWpcVWp,cplcgWCgAcVWp,cplcgWCgWCAh,cplcgWCgWChh,cplcgWCgWCVP,cplcgWCgWCVZ,       & 
& cplcgWCgZcHp,cplcgWCgZcVWp,cplcgWpgAHp,cplcgWpgAVWp,cplcgWpgWpAh,cplcgWpgWphh,         & 
& cplcgWpgWpVP,cplcgWpgWpVZ,cplcgWpgZHp,cplcgWpgZVWp,cplcgZgAhh,cplcgZgWCHp,             & 
& cplcgZgWCVWp,cplcgZgWpcHp,cplcgZgWpcVWp,cplcgZgZhh,cplcHpVPVWp,cplcHpVWpVZ,            & 
& cplcVWpcVWpVWpVWp1Q,cplcVWpcVWpVWpVWp2Q,cplcVWpcVWpVWpVWp3Q,cplcVWpVPVPVWp1Q,          & 
& cplcVWpVPVPVWp2Q,cplcVWpVPVPVWp3Q,cplcVWpVPVWp,cplcVWpVPVWpVZ1Q,cplcVWpVPVWpVZ2Q,      & 
& cplcVWpVPVWpVZ3Q,cplcVWpVWpVZ,cplcVWpVWpVZVZ1Q,cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q,      & 
& cplhhcHpVPVWp1,cplhhcHpVWp,cplhhcHpVWpVZ1,cplhhcVWpVWp,cplhhhhcVWpVWp1,cplhhhhhh,      & 
& cplhhhhhhhh1,cplhhhhHpcHp1,cplhhhhSsccSsc1,cplhhhhVZVZ1,cplhhHpcHp,cplhhHpcVWp,        & 
& cplhhHpcVWpVP1,cplhhHpcVWpVZ1,cplhhSsccSsc,cplhhVZVZ,cplHpcHpcVWpVWp1,cplHpcHpVP,      & 
& cplHpcHpVPVP1,cplHpcHpVPVZ1,cplHpcHpVZ,cplHpcHpVZVZ1,cplHpcVWpVP,cplHpcVWpVZ,          & 
& cplHpHpcHpcHp1,cplHpSsccHpcSsc1,cplSscSsccSsccSsc1,ctcplAhAhhh,ctcplAhhhVZ,            & 
& ctcplcFdFdhhL,ctcplcFdFdhhR,ctcplcFeFehhL,ctcplcFeFehhR,ctcplcFuFuhhL,ctcplcFuFuhhR,   & 
& ctcplcFxvFxvhhL,ctcplcFxvFxvhhR,ctcplhhcVWpVWp,ctcplhhhhhh,ctcplhhHpcHp,               & 
& ctcplhhHpcVWp,ctcplhhSsccSsc,ctcplhhVZVZ,GcplcHpVPVWp,GcplhhcHpVWp,GcplhhHpcHp,        & 
& GcplhhHpcVWp,GcplHpcVWpVP,GosZcplcHpVPVWp,GosZcplhhcHpVWp,GosZcplhhHpcHp,              & 
& GosZcplhhHpcVWp,GosZcplHpcVWpVP,GZcplcHpVPVWp,GZcplhhcHpVWp,GZcplhhHpcHp,              & 
& GZcplhhHpcVWp,GZcplHpcVWpVP,ZcplAhAhhh,ZcplAhhhVZ,ZcplcFdFdhhL,ZcplcFdFdhhR,           & 
& ZcplcFeFehhL,ZcplcFeFehhR,ZcplcFuFuhhL,ZcplcFuFuhhR,ZcplcFxvFxvhhL,ZcplcFxvFxvhhR,     & 
& ZcplhhcVWpVWp,Zcplhhhhhh,ZcplhhHpcHp,ZcplhhHpcVWp,ZcplhhSsccSsc,ZcplhhVZVZ,            & 
& ZRUZH,ZRUVd,ZRUUd,ZRUVu,ZRUUu,ZRUVe,ZRUUe,ZRUVv,ZRUVvr,ZRUXV,ZRUXU,ZRUVSs,             & 
& MLambda,em,gs,deltaM,kont,gP1Lhh)

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

Complex(dp),Intent(in) :: cplAhAhAhAh1,cplAhAhcVWpVWp1,cplAhAhhh(2),cplAhAhhhhh1(2,2),cplAhAhHpcHp1,            & 
& cplAhAhSsccSsc1(2,2),cplAhAhVZVZ1,cplAhcHpVPVWp1,cplAhcHpVWp,cplAhcHpVWpVZ1,           & 
& cplAhhhVZ(2),cplAhHpcVWp,cplAhHpcVWpVP1,cplAhHpcVWpVZ1,cplcFdFdAhL(3,3),               & 
& cplcFdFdAhR(3,3),cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVGL(3,3),               & 
& cplcFdFdVGR(3,3),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),  & 
& cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),             & 
& cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),               & 
& cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFeFvcHpL(3,3), & 
& cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFeFxecSscL(3,2),           & 
& cplcFeFxecSscR(3,2),cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),cplcFuFdVWpL(3,3),               & 
& cplcFuFdVWpR(3,3),cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),& 
& cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),  & 
& cplcFuFuVZR(3,3),cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),& 
& cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),         & 
& cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL(2),   & 
& cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplcFxvFvSscL(2,3,2),          & 
& cplcFxvFvSscR(2,3,2),cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxvFxeVWpL(2),              & 
& cplcFxvFxeVWpR(2),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplcFxvFxvhhL(2,2,2),          & 
& cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),cplcgAgWCVWp,               & 
& cplcgAgWpcVWp,cplcgWCgAcVWp,cplcgWCgWCAh,cplcgWCgWChh(2),cplcgWCgWCVP,cplcgWCgWCVZ,    & 
& cplcgWCgZcHp,cplcgWCgZcVWp,cplcgWpgAHp,cplcgWpgAVWp,cplcgWpgWpAh,cplcgWpgWphh(2),      & 
& cplcgWpgWpVP,cplcgWpgWpVZ,cplcgWpgZHp,cplcgWpgZVWp,cplcgZgAhh(2),cplcgZgWCHp,          & 
& cplcgZgWCVWp,cplcgZgWpcHp,cplcgZgWpcVWp,cplcgZgZhh(2),cplcHpVPVWp,cplcHpVWpVZ,         & 
& cplcVWpcVWpVWpVWp1Q,cplcVWpcVWpVWpVWp2Q,cplcVWpcVWpVWpVWp3Q,cplcVWpVPVPVWp1Q,          & 
& cplcVWpVPVPVWp2Q,cplcVWpVPVPVWp3Q,cplcVWpVPVWp,cplcVWpVPVWpVZ1Q,cplcVWpVPVWpVZ2Q,      & 
& cplcVWpVPVWpVZ3Q,cplcVWpVWpVZ,cplcVWpVWpVZVZ1Q,cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q,      & 
& cplhhcHpVPVWp1(2),cplhhcHpVWp(2),cplhhcHpVWpVZ1(2),cplhhcVWpVWp(2),cplhhhhcVWpVWp1(2,2),& 
& cplhhhhhh(2,2,2),cplhhhhhhhh1(2,2,2,2),cplhhhhHpcHp1(2,2),cplhhhhSsccSsc1(2,2,2,2),    & 
& cplhhhhVZVZ1(2,2),cplhhHpcHp(2),cplhhHpcVWp(2),cplhhHpcVWpVP1(2),cplhhHpcVWpVZ1(2),    & 
& cplhhSsccSsc(2,2,2),cplhhVZVZ(2),cplHpcHpcVWpVWp1,cplHpcHpVP,cplHpcHpVPVP1,            & 
& cplHpcHpVPVZ1,cplHpcHpVZ,cplHpcHpVZVZ1,cplHpcVWpVP,cplHpcVWpVZ,cplHpHpcHpcHp1,         & 
& cplHpSsccHpcSsc1(2,2),cplSscSsccSsccSsc1(2,2,2,2),ctcplAhAhhh(2),ctcplAhhhVZ(2),       & 
& ctcplcFdFdhhL(3,3,2),ctcplcFdFdhhR(3,3,2),ctcplcFeFehhL(3,3,2),ctcplcFeFehhR(3,3,2),   & 
& ctcplcFuFuhhL(3,3,2),ctcplcFuFuhhR(3,3,2),ctcplcFxvFxvhhL(2,2,2),ctcplcFxvFxvhhR(2,2,2),& 
& ctcplhhcVWpVWp(2),ctcplhhhhhh(2,2,2),ctcplhhHpcHp(2),ctcplhhHpcVWp(2),ctcplhhSsccSsc(2,2,2),& 
& ctcplhhVZVZ(2),GcplcHpVPVWp,GcplhhcHpVWp(2),GcplhhHpcHp(2),GcplhhHpcVWp(2),            & 
& GcplHpcVWpVP,GosZcplcHpVPVWp,GosZcplhhcHpVWp(2),GosZcplhhHpcHp(2),GosZcplhhHpcVWp(2)

Complex(dp),Intent(in) :: GosZcplHpcVWpVP,GZcplcHpVPVWp,GZcplhhcHpVWp(2),GZcplhhHpcHp(2),GZcplhhHpcVWp(2),       & 
& GZcplHpcVWpVP,ZcplAhAhhh(2),ZcplAhhhVZ(2),ZcplcFdFdhhL(3,3,2),ZcplcFdFdhhR(3,3,2),     & 
& ZcplcFeFehhL(3,3,2),ZcplcFeFehhR(3,3,2),ZcplcFuFuhhL(3,3,2),ZcplcFuFuhhR(3,3,2),       & 
& ZcplcFxvFxvhhL(2,2,2),ZcplcFxvFxvhhR(2,2,2),ZcplhhcVWpVWp(2),Zcplhhhhhh(2,2,2),        & 
& ZcplhhHpcHp(2),ZcplhhHpcVWp(2),ZcplhhSsccSsc(2,2,2),ZcplhhVZVZ(2)

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
Real(dp), Intent(out) :: gP1Lhh(2,61) 
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
Real(dp) :: MRPhhTocFdFd(2,3,3),MRGhhTocFdFd(2,3,3), MRPZhhTocFdFd(2,3,3),MRGZhhTocFdFd(2,3,3) 
Real(dp) :: MVPhhTocFdFd(2,3,3) 
Real(dp) :: RMsqTreehhTocFdFd(2,3,3),RMsqWavehhTocFdFd(2,3,3),RMsqVertexhhTocFdFd(2,3,3) 
Complex(dp) :: AmpTreehhTocFdFd(2,2,3,3),AmpWavehhTocFdFd(2,2,3,3)=(0._dp,0._dp),AmpVertexhhTocFdFd(2,2,3,3)& 
 & ,AmpVertexIRoshhTocFdFd(2,2,3,3),AmpVertexIRdrhhTocFdFd(2,2,3,3), AmpSumhhTocFdFd(2,2,3,3), AmpSum2hhTocFdFd(2,2,3,3) 
Complex(dp) :: AmpTreeZhhTocFdFd(2,2,3,3),AmpWaveZhhTocFdFd(2,2,3,3),AmpVertexZhhTocFdFd(2,2,3,3) 
Real(dp) :: AmpSqhhTocFdFd(2,3,3),  AmpSqTreehhTocFdFd(2,3,3) 
Real(dp) :: MRPhhTocFeFe(2,3,3),MRGhhTocFeFe(2,3,3), MRPZhhTocFeFe(2,3,3),MRGZhhTocFeFe(2,3,3) 
Real(dp) :: MVPhhTocFeFe(2,3,3) 
Real(dp) :: RMsqTreehhTocFeFe(2,3,3),RMsqWavehhTocFeFe(2,3,3),RMsqVertexhhTocFeFe(2,3,3) 
Complex(dp) :: AmpTreehhTocFeFe(2,2,3,3),AmpWavehhTocFeFe(2,2,3,3)=(0._dp,0._dp),AmpVertexhhTocFeFe(2,2,3,3)& 
 & ,AmpVertexIRoshhTocFeFe(2,2,3,3),AmpVertexIRdrhhTocFeFe(2,2,3,3), AmpSumhhTocFeFe(2,2,3,3), AmpSum2hhTocFeFe(2,2,3,3) 
Complex(dp) :: AmpTreeZhhTocFeFe(2,2,3,3),AmpWaveZhhTocFeFe(2,2,3,3),AmpVertexZhhTocFeFe(2,2,3,3) 
Real(dp) :: AmpSqhhTocFeFe(2,3,3),  AmpSqTreehhTocFeFe(2,3,3) 
Real(dp) :: MRPhhTocFuFu(2,3,3),MRGhhTocFuFu(2,3,3), MRPZhhTocFuFu(2,3,3),MRGZhhTocFuFu(2,3,3) 
Real(dp) :: MVPhhTocFuFu(2,3,3) 
Real(dp) :: RMsqTreehhTocFuFu(2,3,3),RMsqWavehhTocFuFu(2,3,3),RMsqVertexhhTocFuFu(2,3,3) 
Complex(dp) :: AmpTreehhTocFuFu(2,2,3,3),AmpWavehhTocFuFu(2,2,3,3)=(0._dp,0._dp),AmpVertexhhTocFuFu(2,2,3,3)& 
 & ,AmpVertexIRoshhTocFuFu(2,2,3,3),AmpVertexIRdrhhTocFuFu(2,2,3,3), AmpSumhhTocFuFu(2,2,3,3), AmpSum2hhTocFuFu(2,2,3,3) 
Complex(dp) :: AmpTreeZhhTocFuFu(2,2,3,3),AmpWaveZhhTocFuFu(2,2,3,3),AmpVertexZhhTocFuFu(2,2,3,3) 
Real(dp) :: AmpSqhhTocFuFu(2,3,3),  AmpSqTreehhTocFuFu(2,3,3) 
Real(dp) :: MRPhhTocFxvFxv(2,2,2),MRGhhTocFxvFxv(2,2,2), MRPZhhTocFxvFxv(2,2,2),MRGZhhTocFxvFxv(2,2,2) 
Real(dp) :: MVPhhTocFxvFxv(2,2,2) 
Real(dp) :: RMsqTreehhTocFxvFxv(2,2,2),RMsqWavehhTocFxvFxv(2,2,2),RMsqVertexhhTocFxvFxv(2,2,2) 
Complex(dp) :: AmpTreehhTocFxvFxv(2,2,2,2),AmpWavehhTocFxvFxv(2,2,2,2)=(0._dp,0._dp),AmpVertexhhTocFxvFxv(2,2,2,2)& 
 & ,AmpVertexIRoshhTocFxvFxv(2,2,2,2),AmpVertexIRdrhhTocFxvFxv(2,2,2,2), AmpSumhhTocFxvFxv(2,2,2,2), AmpSum2hhTocFxvFxv(2,2,2,2) 
Complex(dp) :: AmpTreeZhhTocFxvFxv(2,2,2,2),AmpWaveZhhTocFxvFxv(2,2,2,2),AmpVertexZhhTocFxvFxv(2,2,2,2) 
Real(dp) :: AmpSqhhTocFxvFxv(2,2,2),  AmpSqTreehhTocFxvFxv(2,2,2) 
Real(dp) :: MRPhhTohhhh(2,2,2),MRGhhTohhhh(2,2,2), MRPZhhTohhhh(2,2,2),MRGZhhTohhhh(2,2,2) 
Real(dp) :: MVPhhTohhhh(2,2,2) 
Real(dp) :: RMsqTreehhTohhhh(2,2,2),RMsqWavehhTohhhh(2,2,2),RMsqVertexhhTohhhh(2,2,2) 
Complex(dp) :: AmpTreehhTohhhh(2,2,2),AmpWavehhTohhhh(2,2,2)=(0._dp,0._dp),AmpVertexhhTohhhh(2,2,2)& 
 & ,AmpVertexIRoshhTohhhh(2,2,2),AmpVertexIRdrhhTohhhh(2,2,2), AmpSumhhTohhhh(2,2,2), AmpSum2hhTohhhh(2,2,2) 
Complex(dp) :: AmpTreeZhhTohhhh(2,2,2),AmpWaveZhhTohhhh(2,2,2),AmpVertexZhhTohhhh(2,2,2) 
Real(dp) :: AmpSqhhTohhhh(2,2,2),  AmpSqTreehhTohhhh(2,2,2) 
Real(dp) :: MRPhhTocSscSsc(2,2,2),MRGhhTocSscSsc(2,2,2), MRPZhhTocSscSsc(2,2,2),MRGZhhTocSscSsc(2,2,2) 
Real(dp) :: MVPhhTocSscSsc(2,2,2) 
Real(dp) :: RMsqTreehhTocSscSsc(2,2,2),RMsqWavehhTocSscSsc(2,2,2),RMsqVertexhhTocSscSsc(2,2,2) 
Complex(dp) :: AmpTreehhTocSscSsc(2,2,2),AmpWavehhTocSscSsc(2,2,2)=(0._dp,0._dp),AmpVertexhhTocSscSsc(2,2,2)& 
 & ,AmpVertexIRoshhTocSscSsc(2,2,2),AmpVertexIRdrhhTocSscSsc(2,2,2), AmpSumhhTocSscSsc(2,2,2), AmpSum2hhTocSscSsc(2,2,2) 
Complex(dp) :: AmpTreeZhhTocSscSsc(2,2,2),AmpWaveZhhTocSscSsc(2,2,2),AmpVertexZhhTocSscSsc(2,2,2) 
Real(dp) :: AmpSqhhTocSscSsc(2,2,2),  AmpSqTreehhTocSscSsc(2,2,2) 
Real(dp) :: MRPhhTocVWpVWp(2),MRGhhTocVWpVWp(2), MRPZhhTocVWpVWp(2),MRGZhhTocVWpVWp(2) 
Real(dp) :: MVPhhTocVWpVWp(2) 
Real(dp) :: RMsqTreehhTocVWpVWp(2),RMsqWavehhTocVWpVWp(2),RMsqVertexhhTocVWpVWp(2) 
Complex(dp) :: AmpTreehhTocVWpVWp(2,2),AmpWavehhTocVWpVWp(2,2)=(0._dp,0._dp),AmpVertexhhTocVWpVWp(2,2)& 
 & ,AmpVertexIRoshhTocVWpVWp(2,2),AmpVertexIRdrhhTocVWpVWp(2,2), AmpSumhhTocVWpVWp(2,2), AmpSum2hhTocVWpVWp(2,2) 
Complex(dp) :: AmpTreeZhhTocVWpVWp(2,2),AmpWaveZhhTocVWpVWp(2,2),AmpVertexZhhTocVWpVWp(2,2) 
Real(dp) :: AmpSqhhTocVWpVWp(2),  AmpSqTreehhTocVWpVWp(2) 
Real(dp) :: MRPhhToVZVZ(2),MRGhhToVZVZ(2), MRPZhhToVZVZ(2),MRGZhhToVZVZ(2) 
Real(dp) :: MVPhhToVZVZ(2) 
Real(dp) :: RMsqTreehhToVZVZ(2),RMsqWavehhToVZVZ(2),RMsqVertexhhToVZVZ(2) 
Complex(dp) :: AmpTreehhToVZVZ(2,2),AmpWavehhToVZVZ(2,2)=(0._dp,0._dp),AmpVertexhhToVZVZ(2,2)& 
 & ,AmpVertexIRoshhToVZVZ(2,2),AmpVertexIRdrhhToVZVZ(2,2), AmpSumhhToVZVZ(2,2), AmpSum2hhToVZVZ(2,2) 
Complex(dp) :: AmpTreeZhhToVZVZ(2,2),AmpWaveZhhToVZVZ(2,2),AmpVertexZhhToVZVZ(2,2) 
Real(dp) :: AmpSqhhToVZVZ(2),  AmpSqTreehhToVZVZ(2) 
Real(dp) :: MRPhhToFvcFv(2,3,3),MRGhhToFvcFv(2,3,3), MRPZhhToFvcFv(2,3,3),MRGZhhToFvcFv(2,3,3) 
Real(dp) :: MVPhhToFvcFv(2,3,3) 
Real(dp) :: RMsqTreehhToFvcFv(2,3,3),RMsqWavehhToFvcFv(2,3,3),RMsqVertexhhToFvcFv(2,3,3) 
Complex(dp) :: AmpTreehhToFvcFv(2,2,3,3),AmpWavehhToFvcFv(2,2,3,3)=(0._dp,0._dp),AmpVertexhhToFvcFv(2,2,3,3)& 
 & ,AmpVertexIRoshhToFvcFv(2,2,3,3),AmpVertexIRdrhhToFvcFv(2,2,3,3), AmpSumhhToFvcFv(2,2,3,3), AmpSum2hhToFvcFv(2,2,3,3) 
Complex(dp) :: AmpTreeZhhToFvcFv(2,2,3,3),AmpWaveZhhToFvcFv(2,2,3,3),AmpVertexZhhToFvcFv(2,2,3,3) 
Real(dp) :: AmpSqhhToFvcFv(2,3,3),  AmpSqTreehhToFvcFv(2,3,3) 
Real(dp) :: MRPhhToFxecFxe(2),MRGhhToFxecFxe(2), MRPZhhToFxecFxe(2),MRGZhhToFxecFxe(2) 
Real(dp) :: MVPhhToFxecFxe(2) 
Real(dp) :: RMsqTreehhToFxecFxe(2),RMsqWavehhToFxecFxe(2),RMsqVertexhhToFxecFxe(2) 
Complex(dp) :: AmpTreehhToFxecFxe(2,2),AmpWavehhToFxecFxe(2,2)=(0._dp,0._dp),AmpVertexhhToFxecFxe(2,2)& 
 & ,AmpVertexIRoshhToFxecFxe(2,2),AmpVertexIRdrhhToFxecFxe(2,2), AmpSumhhToFxecFxe(2,2), AmpSum2hhToFxecFxe(2,2) 
Complex(dp) :: AmpTreeZhhToFxecFxe(2,2),AmpWaveZhhToFxecFxe(2,2),AmpVertexZhhToFxecFxe(2,2) 
Real(dp) :: AmpSqhhToFxecFxe(2),  AmpSqTreehhToFxecFxe(2) 
Real(dp) :: MRPhhTohhVP(2,2),MRGhhTohhVP(2,2), MRPZhhTohhVP(2,2),MRGZhhTohhVP(2,2) 
Real(dp) :: MVPhhTohhVP(2,2) 
Real(dp) :: RMsqTreehhTohhVP(2,2),RMsqWavehhTohhVP(2,2),RMsqVertexhhTohhVP(2,2) 
Complex(dp) :: AmpTreehhTohhVP(2,2,2),AmpWavehhTohhVP(2,2,2)=(0._dp,0._dp),AmpVertexhhTohhVP(2,2,2)& 
 & ,AmpVertexIRoshhTohhVP(2,2,2),AmpVertexIRdrhhTohhVP(2,2,2), AmpSumhhTohhVP(2,2,2), AmpSum2hhTohhVP(2,2,2) 
Complex(dp) :: AmpTreeZhhTohhVP(2,2,2),AmpWaveZhhTohhVP(2,2,2),AmpVertexZhhTohhVP(2,2,2) 
Real(dp) :: AmpSqhhTohhVP(2,2),  AmpSqTreehhTohhVP(2,2) 
Real(dp) :: MRPhhTohhVZ(2,2),MRGhhTohhVZ(2,2), MRPZhhTohhVZ(2,2),MRGZhhTohhVZ(2,2) 
Real(dp) :: MVPhhTohhVZ(2,2) 
Real(dp) :: RMsqTreehhTohhVZ(2,2),RMsqWavehhTohhVZ(2,2),RMsqVertexhhTohhVZ(2,2) 
Complex(dp) :: AmpTreehhTohhVZ(2,2,2),AmpWavehhTohhVZ(2,2,2)=(0._dp,0._dp),AmpVertexhhTohhVZ(2,2,2)& 
 & ,AmpVertexIRoshhTohhVZ(2,2,2),AmpVertexIRdrhhTohhVZ(2,2,2), AmpSumhhTohhVZ(2,2,2), AmpSum2hhTohhVZ(2,2,2) 
Complex(dp) :: AmpTreeZhhTohhVZ(2,2,2),AmpWaveZhhTohhVZ(2,2,2),AmpVertexZhhTohhVZ(2,2,2) 
Real(dp) :: AmpSqhhTohhVZ(2,2),  AmpSqTreehhTohhVZ(2,2) 
Real(dp) :: MRPhhToVGVG(2),MRGhhToVGVG(2), MRPZhhToVGVG(2),MRGZhhToVGVG(2) 
Real(dp) :: MVPhhToVGVG(2) 
Real(dp) :: RMsqTreehhToVGVG(2),RMsqWavehhToVGVG(2),RMsqVertexhhToVGVG(2) 
Complex(dp) :: AmpTreehhToVGVG(2,2),AmpWavehhToVGVG(2,2)=(0._dp,0._dp),AmpVertexhhToVGVG(2,2)& 
 & ,AmpVertexIRoshhToVGVG(2,2),AmpVertexIRdrhhToVGVG(2,2), AmpSumhhToVGVG(2,2), AmpSum2hhToVGVG(2,2) 
Complex(dp) :: AmpTreeZhhToVGVG(2,2),AmpWaveZhhToVGVG(2,2),AmpVertexZhhToVGVG(2,2) 
Real(dp) :: AmpSqhhToVGVG(2),  AmpSqTreehhToVGVG(2) 
Real(dp) :: MRPhhToVPVP(2),MRGhhToVPVP(2), MRPZhhToVPVP(2),MRGZhhToVPVP(2) 
Real(dp) :: MVPhhToVPVP(2) 
Real(dp) :: RMsqTreehhToVPVP(2),RMsqWavehhToVPVP(2),RMsqVertexhhToVPVP(2) 
Complex(dp) :: AmpTreehhToVPVP(2,2),AmpWavehhToVPVP(2,2)=(0._dp,0._dp),AmpVertexhhToVPVP(2,2)& 
 & ,AmpVertexIRoshhToVPVP(2,2),AmpVertexIRdrhhToVPVP(2,2), AmpSumhhToVPVP(2,2), AmpSum2hhToVPVP(2,2) 
Complex(dp) :: AmpTreeZhhToVPVP(2,2),AmpWaveZhhToVPVP(2,2),AmpVertexZhhToVPVP(2,2) 
Real(dp) :: AmpSqhhToVPVP(2),  AmpSqTreehhToVPVP(2) 
Real(dp) :: MRPhhToVPVZ(2),MRGhhToVPVZ(2), MRPZhhToVPVZ(2),MRGZhhToVPVZ(2) 
Real(dp) :: MVPhhToVPVZ(2) 
Real(dp) :: RMsqTreehhToVPVZ(2),RMsqWavehhToVPVZ(2),RMsqVertexhhToVPVZ(2) 
Complex(dp) :: AmpTreehhToVPVZ(2,2),AmpWavehhToVPVZ(2,2)=(0._dp,0._dp),AmpVertexhhToVPVZ(2,2)& 
 & ,AmpVertexIRoshhToVPVZ(2,2),AmpVertexIRdrhhToVPVZ(2,2), AmpSumhhToVPVZ(2,2), AmpSum2hhToVPVZ(2,2) 
Complex(dp) :: AmpTreeZhhToVPVZ(2,2),AmpWaveZhhToVPVZ(2,2),AmpVertexZhhToVPVZ(2,2) 
Real(dp) :: AmpSqhhToVPVZ(2),  AmpSqTreehhToVPVZ(2) 
Write(*,*) "Calculating one-loop decays of hh " 
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
isave = 5

If (.not.CalcLoopDecay_LoopInducedOnly) Then 
!---------------- 
! bar(Fd) Fd
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_hhTocFdFd(cplcFdFdhhL,cplcFdFdhhR,MFd,Mhh,              & 
& MFd2,Mhh2,AmpTreehhTocFdFd)

  Else 
Call Amplitude_Tree_SDdiracDM_hhTocFdFd(ZcplcFdFdhhL,ZcplcFdFdhhR,MFd,Mhh,            & 
& MFd2,Mhh2,AmpTreehhTocFdFd)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_hhTocFdFd(MLambda,em,gs,cplcFdFdhhL,cplcFdFdhhR,            & 
& MFdOS,MhhOS,MRPhhTocFdFd,MRGhhTocFdFd)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_hhTocFdFd(MLambda,em,gs,ZcplcFdFdhhL,ZcplcFdFdhhR,          & 
& MFdOS,MhhOS,MRPhhTocFdFd,MRGhhTocFdFd)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_hhTocFdFd(MLambda,em,gs,cplcFdFdhhL,cplcFdFdhhR,            & 
& MFd,Mhh,MRPhhTocFdFd,MRGhhTocFdFd)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_hhTocFdFd(MLambda,em,gs,ZcplcFdFdhhL,ZcplcFdFdhhR,          & 
& MFd,Mhh,MRPhhTocFdFd,MRGhhTocFdFd)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_hhTocFdFd(cplcFdFdhhL,cplcFdFdhhR,ctcplcFdFdhhL,        & 
& ctcplcFdFdhhR,MFd,MFd2,Mhh,Mhh2,ZfDL,ZfDR,Zfhh,AmpWavehhTocFdFd)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_hhTocFdFd(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,MVWp,           & 
& MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFdFdAhL,               & 
& cplcFdFdAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,     & 
& cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,             & 
& cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,               & 
& cplhhVZVZ,AmpVertexhhTocFdFd)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFdFd(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,             & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFdFdAhL,          & 
& cplcFdFdAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,     & 
& cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,             & 
& cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,               & 
& cplhhVZVZ,AmpVertexIRdrhhTocFdFd)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFdFd(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,           & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplAhAhhh,cplcFdFdAhL,cplcFdFdAhR,cplAhhhVZ,ZcplcFdFdhhL,ZcplcFdFdhhR,          & 
& cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,               & 
& cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,             & 
& cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,            & 
& cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRoshhTocFdFd)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFdFd(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,             & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFdFdAhL,          & 
& cplcFdFdAhR,cplAhhhVZ,ZcplcFdFdhhL,ZcplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,               & 
& cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,             & 
& cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucHpL,cplcFdFucHpR,             & 
& cplcFdFucVWpL,cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,              & 
& cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRoshhTocFdFd)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFdFd(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,           & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplAhAhhh,cplcFdFdAhL,cplcFdFdAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,            & 
& cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,               & 
& cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,             & 
& cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,            & 
& cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRoshhTocFdFd)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFdFd(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,             & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFdFdAhL,          & 
& cplcFdFdAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,     & 
& cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,             & 
& cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,               & 
& cplhhVZVZ,AmpVertexIRoshhTocFdFd)

 End if 
 End if 
AmpVertexhhTocFdFd = AmpVertexhhTocFdFd -  AmpVertexIRdrhhTocFdFd! +  AmpVertexIRoshhTocFdFd ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZhhTocFdFd=0._dp 
AmpVertexZhhTocFdFd=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZhhTocFdFd(:,gt2,:,:) = AmpWaveZhhTocFdFd(:,gt2,:,:)+ZRUZH(gt2,gt1)*AmpWavehhTocFdFd(:,gt1,:,:) 
AmpVertexZhhTocFdFd(:,gt2,:,:)= AmpVertexZhhTocFdFd(:,gt2,:,:) + ZRUZH(gt2,gt1)*AmpVertexhhTocFdFd(:,gt1,:,:) 
 End Do 
End Do 
AmpWavehhTocFdFd=AmpWaveZhhTocFdFd 
AmpVertexhhTocFdFd= AmpVertexZhhTocFdFd
! Final State 1 
AmpWaveZhhTocFdFd=0._dp 
AmpVertexZhhTocFdFd=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZhhTocFdFd(1,:,gt2,:) = AmpWaveZhhTocFdFd(1,:,gt2,:)+ZRUUd(gt2,gt1)*AmpWavehhTocFdFd(1,:,gt1,:) 
AmpVertexZhhTocFdFd(1,:,gt2,:)= AmpVertexZhhTocFdFd(1,:,gt2,:)+ZRUUd(gt2,gt1)*AmpVertexhhTocFdFd(1,:,gt1,:) 
AmpWaveZhhTocFdFd(2,:,gt2,:) = AmpWaveZhhTocFdFd(2,:,gt2,:)+ZRUVdc(gt2,gt1)*AmpWavehhTocFdFd(2,:,gt1,:) 
AmpVertexZhhTocFdFd(2,:,gt2,:)= AmpVertexZhhTocFdFd(2,:,gt2,:)+ZRUVdc(gt2,gt1)*AmpVertexhhTocFdFd(2,:,gt1,:) 
 End Do 
End Do 
AmpWavehhTocFdFd=AmpWaveZhhTocFdFd 
AmpVertexhhTocFdFd= AmpVertexZhhTocFdFd
! Final State 2 
AmpWaveZhhTocFdFd=0._dp 
AmpVertexZhhTocFdFd=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZhhTocFdFd(1,:,:,gt2) = AmpWaveZhhTocFdFd(1,:,:,gt2)+ZRUVd(gt2,gt1)*AmpWavehhTocFdFd(1,:,:,gt1) 
AmpVertexZhhTocFdFd(1,:,:,gt2)= AmpVertexZhhTocFdFd(1,:,:,gt2)+ZRUVd(gt2,gt1)*AmpVertexhhTocFdFd(1,:,:,gt1) 
AmpWaveZhhTocFdFd(2,:,:,gt2) = AmpWaveZhhTocFdFd(2,:,:,gt2)+ZRUUd(gt2,gt1)*AmpWavehhTocFdFd(2,:,:,gt1) 
AmpVertexZhhTocFdFd(2,:,:,gt2)= AmpVertexZhhTocFdFd(2,:,:,gt2)+ZRUUd(gt2,gt1)*AmpVertexhhTocFdFd(2,:,:,gt1) 
 End Do 
End Do 
AmpWavehhTocFdFd=AmpWaveZhhTocFdFd 
AmpVertexhhTocFdFd= AmpVertexZhhTocFdFd
End if
If (ShiftIRdiv) Then 
AmpVertexhhTocFdFd = AmpVertexhhTocFdFd  +  AmpVertexIRoshhTocFdFd
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ hh->bar[Fd] Fd -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumhhTocFdFd = AmpTreehhTocFdFd 
 AmpSum2hhTocFdFd = AmpTreehhTocFdFd + 2._dp*AmpWavehhTocFdFd + 2._dp*AmpVertexhhTocFdFd  
Else 
 AmpSumhhTocFdFd = AmpTreehhTocFdFd + AmpWavehhTocFdFd + AmpVertexhhTocFdFd
 AmpSum2hhTocFdFd = AmpTreehhTocFdFd + AmpWavehhTocFdFd + AmpVertexhhTocFdFd 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumhhTocFdFd = AmpTreehhTocFdFd
 AmpSum2hhTocFdFd = AmpTreehhTocFdFd 
End if 
Do gt1=1,2
i4 = isave 
  Do gt2=1,3
    Do gt3=1,3
If (((OSkinematics).and.(MhhOS(gt1).gt.(MFdOS(gt2)+MFdOS(gt3)))).or.((.not.OSkinematics).and.(Mhh(gt1).gt.(MFd(gt2)+MFd(gt3))))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2, gt3 
  AmpSum2hhTocFdFd = AmpTreehhTocFdFd
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFdOS(gt2),MFdOS(gt3),AmpSumhhTocFdFd(:,gt1, gt2, gt3),AmpSum2hhTocFdFd(:,gt1, gt2, gt3),AmpSqhhTocFdFd(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFd(gt2),MFd(gt3),AmpSumhhTocFdFd(:,gt1, gt2, gt3),AmpSum2hhTocFdFd(:,gt1, gt2, gt3),AmpSqhhTocFdFd(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqhhTocFdFd(gt1, gt2, gt3) 
  AmpSum2hhTocFdFd = 2._dp*AmpWavehhTocFdFd
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFdOS(gt2),MFdOS(gt3),AmpSumhhTocFdFd(:,gt1, gt2, gt3),AmpSum2hhTocFdFd(:,gt1, gt2, gt3),AmpSqhhTocFdFd(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFd(gt2),MFd(gt3),AmpSumhhTocFdFd(:,gt1, gt2, gt3),AmpSum2hhTocFdFd(:,gt1, gt2, gt3),AmpSqhhTocFdFd(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqhhTocFdFd(gt1, gt2, gt3) 
  AmpSum2hhTocFdFd = 2._dp*AmpVertexhhTocFdFd
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFdOS(gt2),MFdOS(gt3),AmpSumhhTocFdFd(:,gt1, gt2, gt3),AmpSum2hhTocFdFd(:,gt1, gt2, gt3),AmpSqhhTocFdFd(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFd(gt2),MFd(gt3),AmpSumhhTocFdFd(:,gt1, gt2, gt3),AmpSum2hhTocFdFd(:,gt1, gt2, gt3),AmpSqhhTocFdFd(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqhhTocFdFd(gt1, gt2, gt3) 
  AmpSum2hhTocFdFd = AmpTreehhTocFdFd + 2._dp*AmpWavehhTocFdFd + 2._dp*AmpVertexhhTocFdFd
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFdOS(gt2),MFdOS(gt3),AmpSumhhTocFdFd(:,gt1, gt2, gt3),AmpSum2hhTocFdFd(:,gt1, gt2, gt3),AmpSqhhTocFdFd(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFd(gt2),MFd(gt3),AmpSumhhTocFdFd(:,gt1, gt2, gt3),AmpSum2hhTocFdFd(:,gt1, gt2, gt3),AmpSqhhTocFdFd(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqhhTocFdFd(gt1, gt2, gt3) 
 End if 
If (OSkinematics) Then 
  AmpSum2hhTocFdFd = AmpTreehhTocFdFd
  Call SquareAmp_StoFF(MhhOS(gt1),MFdOS(gt2),MFdOS(gt3),AmpSumhhTocFdFd(:,gt1, gt2, gt3),AmpSum2hhTocFdFd(:,gt1, gt2, gt3),AmpSqhhTocFdFd(gt1, gt2, gt3)) 
  AmpSqTreehhTocFdFd(gt1, gt2, gt3) = AmpSqhhTocFdFd(gt1, gt2, gt3)  
  AmpSum2hhTocFdFd = + 2._dp*AmpWavehhTocFdFd + 2._dp*AmpVertexhhTocFdFd
  Call SquareAmp_StoFF(MhhOS(gt1),MFdOS(gt2),MFdOS(gt3),AmpSumhhTocFdFd(:,gt1, gt2, gt3),AmpSum2hhTocFdFd(:,gt1, gt2, gt3),AmpSqhhTocFdFd(gt1, gt2, gt3)) 
  AmpSqhhTocFdFd(gt1, gt2, gt3) = AmpSqhhTocFdFd(gt1, gt2, gt3) + AmpSqTreehhTocFdFd(gt1, gt2, gt3)  
Else  
  AmpSum2hhTocFdFd = AmpTreehhTocFdFd
  Call SquareAmp_StoFF(Mhh(gt1),MFd(gt2),MFd(gt3),AmpSumhhTocFdFd(:,gt1, gt2, gt3),AmpSum2hhTocFdFd(:,gt1, gt2, gt3),AmpSqhhTocFdFd(gt1, gt2, gt3)) 
  AmpSqTreehhTocFdFd(gt1, gt2, gt3) = AmpSqhhTocFdFd(gt1, gt2, gt3)  
  AmpSum2hhTocFdFd = + 2._dp*AmpWavehhTocFdFd + 2._dp*AmpVertexhhTocFdFd
  Call SquareAmp_StoFF(Mhh(gt1),MFd(gt2),MFd(gt3),AmpSumhhTocFdFd(:,gt1, gt2, gt3),AmpSum2hhTocFdFd(:,gt1, gt2, gt3),AmpSqhhTocFdFd(gt1, gt2, gt3)) 
  AmpSqhhTocFdFd(gt1, gt2, gt3) = AmpSqhhTocFdFd(gt1, gt2, gt3) + AmpSqTreehhTocFdFd(gt1, gt2, gt3)  
End if  
Else  
  AmpSqhhTocFdFd(gt1, gt2, gt3) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 4._dp 
If (AmpSqhhTocFdFd(gt1, gt2, gt3).le.0._dp) Then 
  gP1Lhh(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1Lhh(gt1,i4) = 3._dp*GammaTPS(MhhOS(gt1),MFdOS(gt2),MFdOS(gt3),helfactor*AmpSqhhTocFdFd(gt1, gt2, gt3))
Else 
  gP1Lhh(gt1,i4) = 3._dp*GammaTPS(Mhh(gt1),MFd(gt2),MFd(gt3),helfactor*AmpSqhhTocFdFd(gt1, gt2, gt3))
End if 
If ((Abs(MRPhhTocFdFd(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGhhTocFdFd(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1Lhh(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPhhTocFdFd(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGhhTocFdFd(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPhhTocFdFd(gt1, gt2, gt3) + MRGhhTocFdFd(gt1, gt2, gt3)) 
  gP1Lhh(gt1,i4) = gP1Lhh(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPhhTocFdFd(gt1, gt2, gt3) + MRGhhTocFdFd(gt1, gt2, gt3))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1Lhh(gt1,i4) 
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
! bar(Fe) Fe
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_hhTocFeFe(cplcFeFehhL,cplcFeFehhR,MFe,Mhh,              & 
& MFe2,Mhh2,AmpTreehhTocFeFe)

  Else 
Call Amplitude_Tree_SDdiracDM_hhTocFeFe(ZcplcFeFehhL,ZcplcFeFehhR,MFe,Mhh,            & 
& MFe2,Mhh2,AmpTreehhTocFeFe)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_hhTocFeFe(MLambda,em,gs,cplcFeFehhL,cplcFeFehhR,            & 
& MFeOS,MhhOS,MRPhhTocFeFe,MRGhhTocFeFe)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_hhTocFeFe(MLambda,em,gs,ZcplcFeFehhL,ZcplcFeFehhR,          & 
& MFeOS,MhhOS,MRPhhTocFeFe,MRGhhTocFeFe)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_hhTocFeFe(MLambda,em,gs,cplcFeFehhL,cplcFeFehhR,            & 
& MFe,Mhh,MRPhhTocFeFe,MRGhhTocFeFe)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_hhTocFeFe(MLambda,em,gs,ZcplcFeFehhL,ZcplcFeFehhR,          & 
& MFe,Mhh,MRPhhTocFeFe,MRGhhTocFeFe)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_hhTocFeFe(cplcFeFehhL,cplcFeFehhR,ctcplcFeFehhL,        & 
& ctcplcFeFehhR,MFe,MFe2,Mhh,Mhh2,ZfEL,ZfER,Zfhh,AmpWavehhTocFeFe)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_hhTocFeFe(MAh,MFe,MFv,MFxe,Mhh,MHp,MSsc,              & 
& MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplAhAhhh,           & 
& cplcFeFeAhL,cplcFeFeAhR,cplAhhhVZ,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,     & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,         & 
& cplcFeFxecSscL,cplcFeFxecSscR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,           & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexhhTocFeFe)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFeFe(MAh,MFe,MFv,MFxe,Mhh,MHp,MSsc,           & 
& MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplAhAhhh,           & 
& cplcFeFeAhL,cplcFeFeAhR,cplAhhhVZ,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,     & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,         & 
& cplcFeFxecSscL,cplcFeFxecSscR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,           & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRdrhhTocFeFe)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFeFe(MAhOS,MFeOS,MFvOS,MFxeOS,MhhOS,          & 
& MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,Mhh2OS,MHp2OS,              & 
& MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplAhAhhh,cplcFeFeAhL,cplcFeFeAhR,cplAhhhVZ,               & 
& ZcplcFeFehhL,ZcplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,         & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,cplcFeFeVZR,             & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,cplcFeFxecSscR,   & 
& cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,      & 
& AmpVertexIRoshhTocFeFe)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFeFe(MAh,MFe,MFv,MFxe,Mhh,MHp,MSsc,           & 
& MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplAhAhhh,           & 
& cplcFeFeAhL,cplcFeFeAhR,cplAhhhVZ,ZcplcFeFehhL,ZcplcFeFehhR,cplcFvFeHpL,               & 
& cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,          & 
& cplcFvFeVWpR,cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,          & 
& cplcFeFvcVWpR,cplcFeFxecSscL,cplcFeFxecSscR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,          & 
& cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRoshhTocFeFe)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFeFe(MAhOS,MFeOS,MFvOS,MFxeOS,MhhOS,          & 
& MHpOS,MSscOS,MVP,MVWpOS,MVZOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,Mhh2OS,MHp2OS,              & 
& MSsc2OS,MVP2,MVWp2OS,MVZ2OS,cplAhAhhh,cplcFeFeAhL,cplcFeFeAhR,cplAhhhVZ,               & 
& cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,           & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,cplcFeFeVZR,             & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,cplcFeFxecSscR,   & 
& cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,      & 
& AmpVertexIRoshhTocFeFe)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFeFe(MAh,MFe,MFv,MFxe,Mhh,MHp,MSsc,           & 
& MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplAhAhhh,           & 
& cplcFeFeAhL,cplcFeFeAhR,cplAhhhVZ,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,     & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,         & 
& cplcFeFxecSscL,cplcFeFxecSscR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,           & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRoshhTocFeFe)

 End if 
 End if 
AmpVertexhhTocFeFe = AmpVertexhhTocFeFe -  AmpVertexIRdrhhTocFeFe! +  AmpVertexIRoshhTocFeFe ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZhhTocFeFe=0._dp 
AmpVertexZhhTocFeFe=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZhhTocFeFe(:,gt2,:,:) = AmpWaveZhhTocFeFe(:,gt2,:,:)+ZRUZH(gt2,gt1)*AmpWavehhTocFeFe(:,gt1,:,:) 
AmpVertexZhhTocFeFe(:,gt2,:,:)= AmpVertexZhhTocFeFe(:,gt2,:,:) + ZRUZH(gt2,gt1)*AmpVertexhhTocFeFe(:,gt1,:,:) 
 End Do 
End Do 
AmpWavehhTocFeFe=AmpWaveZhhTocFeFe 
AmpVertexhhTocFeFe= AmpVertexZhhTocFeFe
! Final State 1 
AmpWaveZhhTocFeFe=0._dp 
AmpVertexZhhTocFeFe=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZhhTocFeFe(1,:,gt2,:) = AmpWaveZhhTocFeFe(1,:,gt2,:)+ZRUUe(gt2,gt1)*AmpWavehhTocFeFe(1,:,gt1,:) 
AmpVertexZhhTocFeFe(1,:,gt2,:)= AmpVertexZhhTocFeFe(1,:,gt2,:)+ZRUUe(gt2,gt1)*AmpVertexhhTocFeFe(1,:,gt1,:) 
AmpWaveZhhTocFeFe(2,:,gt2,:) = AmpWaveZhhTocFeFe(2,:,gt2,:)+ZRUVec(gt2,gt1)*AmpWavehhTocFeFe(2,:,gt1,:) 
AmpVertexZhhTocFeFe(2,:,gt2,:)= AmpVertexZhhTocFeFe(2,:,gt2,:)+ZRUVec(gt2,gt1)*AmpVertexhhTocFeFe(2,:,gt1,:) 
 End Do 
End Do 
AmpWavehhTocFeFe=AmpWaveZhhTocFeFe 
AmpVertexhhTocFeFe= AmpVertexZhhTocFeFe
! Final State 2 
AmpWaveZhhTocFeFe=0._dp 
AmpVertexZhhTocFeFe=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZhhTocFeFe(1,:,:,gt2) = AmpWaveZhhTocFeFe(1,:,:,gt2)+ZRUVe(gt2,gt1)*AmpWavehhTocFeFe(1,:,:,gt1) 
AmpVertexZhhTocFeFe(1,:,:,gt2)= AmpVertexZhhTocFeFe(1,:,:,gt2)+ZRUVe(gt2,gt1)*AmpVertexhhTocFeFe(1,:,:,gt1) 
AmpWaveZhhTocFeFe(2,:,:,gt2) = AmpWaveZhhTocFeFe(2,:,:,gt2)+ZRUUe(gt2,gt1)*AmpWavehhTocFeFe(2,:,:,gt1) 
AmpVertexZhhTocFeFe(2,:,:,gt2)= AmpVertexZhhTocFeFe(2,:,:,gt2)+ZRUUe(gt2,gt1)*AmpVertexhhTocFeFe(2,:,:,gt1) 
 End Do 
End Do 
AmpWavehhTocFeFe=AmpWaveZhhTocFeFe 
AmpVertexhhTocFeFe= AmpVertexZhhTocFeFe
End if
If (ShiftIRdiv) Then 
AmpVertexhhTocFeFe = AmpVertexhhTocFeFe  +  AmpVertexIRoshhTocFeFe
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ hh->bar[Fe] Fe -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumhhTocFeFe = AmpTreehhTocFeFe 
 AmpSum2hhTocFeFe = AmpTreehhTocFeFe + 2._dp*AmpWavehhTocFeFe + 2._dp*AmpVertexhhTocFeFe  
Else 
 AmpSumhhTocFeFe = AmpTreehhTocFeFe + AmpWavehhTocFeFe + AmpVertexhhTocFeFe
 AmpSum2hhTocFeFe = AmpTreehhTocFeFe + AmpWavehhTocFeFe + AmpVertexhhTocFeFe 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumhhTocFeFe = AmpTreehhTocFeFe
 AmpSum2hhTocFeFe = AmpTreehhTocFeFe 
End if 
Do gt1=1,2
i4 = isave 
  Do gt2=1,3
    Do gt3=1,3
If (((OSkinematics).and.(MhhOS(gt1).gt.(MFeOS(gt2)+MFeOS(gt3)))).or.((.not.OSkinematics).and.(Mhh(gt1).gt.(MFe(gt2)+MFe(gt3))))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2, gt3 
  AmpSum2hhTocFeFe = AmpTreehhTocFeFe
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFeOS(gt2),MFeOS(gt3),AmpSumhhTocFeFe(:,gt1, gt2, gt3),AmpSum2hhTocFeFe(:,gt1, gt2, gt3),AmpSqhhTocFeFe(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFe(gt2),MFe(gt3),AmpSumhhTocFeFe(:,gt1, gt2, gt3),AmpSum2hhTocFeFe(:,gt1, gt2, gt3),AmpSqhhTocFeFe(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqhhTocFeFe(gt1, gt2, gt3) 
  AmpSum2hhTocFeFe = 2._dp*AmpWavehhTocFeFe
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFeOS(gt2),MFeOS(gt3),AmpSumhhTocFeFe(:,gt1, gt2, gt3),AmpSum2hhTocFeFe(:,gt1, gt2, gt3),AmpSqhhTocFeFe(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFe(gt2),MFe(gt3),AmpSumhhTocFeFe(:,gt1, gt2, gt3),AmpSum2hhTocFeFe(:,gt1, gt2, gt3),AmpSqhhTocFeFe(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqhhTocFeFe(gt1, gt2, gt3) 
  AmpSum2hhTocFeFe = 2._dp*AmpVertexhhTocFeFe
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFeOS(gt2),MFeOS(gt3),AmpSumhhTocFeFe(:,gt1, gt2, gt3),AmpSum2hhTocFeFe(:,gt1, gt2, gt3),AmpSqhhTocFeFe(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFe(gt2),MFe(gt3),AmpSumhhTocFeFe(:,gt1, gt2, gt3),AmpSum2hhTocFeFe(:,gt1, gt2, gt3),AmpSqhhTocFeFe(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqhhTocFeFe(gt1, gt2, gt3) 
  AmpSum2hhTocFeFe = AmpTreehhTocFeFe + 2._dp*AmpWavehhTocFeFe + 2._dp*AmpVertexhhTocFeFe
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFeOS(gt2),MFeOS(gt3),AmpSumhhTocFeFe(:,gt1, gt2, gt3),AmpSum2hhTocFeFe(:,gt1, gt2, gt3),AmpSqhhTocFeFe(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFe(gt2),MFe(gt3),AmpSumhhTocFeFe(:,gt1, gt2, gt3),AmpSum2hhTocFeFe(:,gt1, gt2, gt3),AmpSqhhTocFeFe(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqhhTocFeFe(gt1, gt2, gt3) 
 End if 
If (OSkinematics) Then 
  AmpSum2hhTocFeFe = AmpTreehhTocFeFe
  Call SquareAmp_StoFF(MhhOS(gt1),MFeOS(gt2),MFeOS(gt3),AmpSumhhTocFeFe(:,gt1, gt2, gt3),AmpSum2hhTocFeFe(:,gt1, gt2, gt3),AmpSqhhTocFeFe(gt1, gt2, gt3)) 
  AmpSqTreehhTocFeFe(gt1, gt2, gt3) = AmpSqhhTocFeFe(gt1, gt2, gt3)  
  AmpSum2hhTocFeFe = + 2._dp*AmpWavehhTocFeFe + 2._dp*AmpVertexhhTocFeFe
  Call SquareAmp_StoFF(MhhOS(gt1),MFeOS(gt2),MFeOS(gt3),AmpSumhhTocFeFe(:,gt1, gt2, gt3),AmpSum2hhTocFeFe(:,gt1, gt2, gt3),AmpSqhhTocFeFe(gt1, gt2, gt3)) 
  AmpSqhhTocFeFe(gt1, gt2, gt3) = AmpSqhhTocFeFe(gt1, gt2, gt3) + AmpSqTreehhTocFeFe(gt1, gt2, gt3)  
Else  
  AmpSum2hhTocFeFe = AmpTreehhTocFeFe
  Call SquareAmp_StoFF(Mhh(gt1),MFe(gt2),MFe(gt3),AmpSumhhTocFeFe(:,gt1, gt2, gt3),AmpSum2hhTocFeFe(:,gt1, gt2, gt3),AmpSqhhTocFeFe(gt1, gt2, gt3)) 
  AmpSqTreehhTocFeFe(gt1, gt2, gt3) = AmpSqhhTocFeFe(gt1, gt2, gt3)  
  AmpSum2hhTocFeFe = + 2._dp*AmpWavehhTocFeFe + 2._dp*AmpVertexhhTocFeFe
  Call SquareAmp_StoFF(Mhh(gt1),MFe(gt2),MFe(gt3),AmpSumhhTocFeFe(:,gt1, gt2, gt3),AmpSum2hhTocFeFe(:,gt1, gt2, gt3),AmpSqhhTocFeFe(gt1, gt2, gt3)) 
  AmpSqhhTocFeFe(gt1, gt2, gt3) = AmpSqhhTocFeFe(gt1, gt2, gt3) + AmpSqTreehhTocFeFe(gt1, gt2, gt3)  
End if  
Else  
  AmpSqhhTocFeFe(gt1, gt2, gt3) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 4._dp 
If (AmpSqhhTocFeFe(gt1, gt2, gt3).le.0._dp) Then 
  gP1Lhh(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(MhhOS(gt1),MFeOS(gt2),MFeOS(gt3),helfactor*AmpSqhhTocFeFe(gt1, gt2, gt3))
Else 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(Mhh(gt1),MFe(gt2),MFe(gt3),helfactor*AmpSqhhTocFeFe(gt1, gt2, gt3))
End if 
If ((Abs(MRPhhTocFeFe(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGhhTocFeFe(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1Lhh(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPhhTocFeFe(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGhhTocFeFe(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPhhTocFeFe(gt1, gt2, gt3) + MRGhhTocFeFe(gt1, gt2, gt3)) 
  gP1Lhh(gt1,i4) = gP1Lhh(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPhhTocFeFe(gt1, gt2, gt3) + MRGhhTocFeFe(gt1, gt2, gt3))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1Lhh(gt1,i4) 
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
! bar(Fu) Fu
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_hhTocFuFu(cplcFuFuhhL,cplcFuFuhhR,MFu,Mhh,              & 
& MFu2,Mhh2,AmpTreehhTocFuFu)

  Else 
Call Amplitude_Tree_SDdiracDM_hhTocFuFu(ZcplcFuFuhhL,ZcplcFuFuhhR,MFu,Mhh,            & 
& MFu2,Mhh2,AmpTreehhTocFuFu)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_hhTocFuFu(MLambda,em,gs,cplcFuFuhhL,cplcFuFuhhR,            & 
& MFuOS,MhhOS,MRPhhTocFuFu,MRGhhTocFuFu)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_hhTocFuFu(MLambda,em,gs,ZcplcFuFuhhL,ZcplcFuFuhhR,          & 
& MFuOS,MhhOS,MRPhhTocFuFu,MRGhhTocFuFu)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_hhTocFuFu(MLambda,em,gs,cplcFuFuhhL,cplcFuFuhhR,            & 
& MFu,Mhh,MRPhhTocFuFu,MRGhhTocFuFu)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_hhTocFuFu(MLambda,em,gs,ZcplcFuFuhhL,ZcplcFuFuhhR,          & 
& MFu,Mhh,MRPhhTocFuFu,MRGhhTocFuFu)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_hhTocFuFu(cplcFuFuhhL,cplcFuFuhhR,ctcplcFuFuhhL,        & 
& ctcplcFuFuhhR,MFu,MFu2,Mhh,Mhh2,Zfhh,ZfUL,ZfUR,AmpWavehhTocFuFu)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_hhTocFuFu(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,MVWp,           & 
& MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFuFuAhL,               & 
& cplcFuFuAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,    & 
& cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,              & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,               & 
& cplhhVZVZ,AmpVertexhhTocFuFu)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFuFu(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,             & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFuFuAhL,          & 
& cplcFuFuAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,    & 
& cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,              & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,               & 
& cplhhVZVZ,AmpVertexIRdrhhTocFuFu)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFuFu(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,           & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplAhAhhh,cplcFuFuAhL,cplcFuFuAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,            & 
& cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,cplcFuFdVWpR,ZcplcFuFuhhL,ZcplcFuFuhhR,           & 
& cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,            & 
& cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRoshhTocFuFu)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFuFu(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,             & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFuFuAhL,          & 
& cplcFuFuAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,    & 
& cplcFuFdVWpR,ZcplcFuFuhhL,ZcplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,            & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,               & 
& cplhhVZVZ,AmpVertexIRoshhTocFuFu)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFuFu(MAhOS,MFdOS,MFuOS,MhhOS,MHpOS,           & 
& MVG,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFu2OS,Mhh2OS,MHp2OS,MVG2,MVP2,MVWp2OS,             & 
& MVZ2OS,cplAhAhhh,cplcFuFuAhL,cplcFuFuAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,            & 
& cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,             & 
& cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,            & 
& cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRoshhTocFuFu)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFuFu(MAh,MFd,MFu,Mhh,MHp,MVG,MVP,             & 
& MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFuFuAhL,          & 
& cplcFuFuAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,    & 
& cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,              & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,               & 
& cplhhVZVZ,AmpVertexIRoshhTocFuFu)

 End if 
 End if 
AmpVertexhhTocFuFu = AmpVertexhhTocFuFu -  AmpVertexIRdrhhTocFuFu! +  AmpVertexIRoshhTocFuFu ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZhhTocFuFu=0._dp 
AmpVertexZhhTocFuFu=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZhhTocFuFu(:,gt2,:,:) = AmpWaveZhhTocFuFu(:,gt2,:,:)+ZRUZH(gt2,gt1)*AmpWavehhTocFuFu(:,gt1,:,:) 
AmpVertexZhhTocFuFu(:,gt2,:,:)= AmpVertexZhhTocFuFu(:,gt2,:,:) + ZRUZH(gt2,gt1)*AmpVertexhhTocFuFu(:,gt1,:,:) 
 End Do 
End Do 
AmpWavehhTocFuFu=AmpWaveZhhTocFuFu 
AmpVertexhhTocFuFu= AmpVertexZhhTocFuFu
! Final State 1 
AmpWaveZhhTocFuFu=0._dp 
AmpVertexZhhTocFuFu=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZhhTocFuFu(1,:,gt2,:) = AmpWaveZhhTocFuFu(1,:,gt2,:)+ZRUUu(gt2,gt1)*AmpWavehhTocFuFu(1,:,gt1,:) 
AmpVertexZhhTocFuFu(1,:,gt2,:)= AmpVertexZhhTocFuFu(1,:,gt2,:)+ZRUUu(gt2,gt1)*AmpVertexhhTocFuFu(1,:,gt1,:) 
AmpWaveZhhTocFuFu(2,:,gt2,:) = AmpWaveZhhTocFuFu(2,:,gt2,:)+ZRUVuc(gt2,gt1)*AmpWavehhTocFuFu(2,:,gt1,:) 
AmpVertexZhhTocFuFu(2,:,gt2,:)= AmpVertexZhhTocFuFu(2,:,gt2,:)+ZRUVuc(gt2,gt1)*AmpVertexhhTocFuFu(2,:,gt1,:) 
 End Do 
End Do 
AmpWavehhTocFuFu=AmpWaveZhhTocFuFu 
AmpVertexhhTocFuFu= AmpVertexZhhTocFuFu
! Final State 2 
AmpWaveZhhTocFuFu=0._dp 
AmpVertexZhhTocFuFu=0._dp 
Do gt1=1,3
  Do gt2=1,3
AmpWaveZhhTocFuFu(1,:,:,gt2) = AmpWaveZhhTocFuFu(1,:,:,gt2)+ZRUVu(gt2,gt1)*AmpWavehhTocFuFu(1,:,:,gt1) 
AmpVertexZhhTocFuFu(1,:,:,gt2)= AmpVertexZhhTocFuFu(1,:,:,gt2)+ZRUVu(gt2,gt1)*AmpVertexhhTocFuFu(1,:,:,gt1) 
AmpWaveZhhTocFuFu(2,:,:,gt2) = AmpWaveZhhTocFuFu(2,:,:,gt2)+ZRUUu(gt2,gt1)*AmpWavehhTocFuFu(2,:,:,gt1) 
AmpVertexZhhTocFuFu(2,:,:,gt2)= AmpVertexZhhTocFuFu(2,:,:,gt2)+ZRUUu(gt2,gt1)*AmpVertexhhTocFuFu(2,:,:,gt1) 
 End Do 
End Do 
AmpWavehhTocFuFu=AmpWaveZhhTocFuFu 
AmpVertexhhTocFuFu= AmpVertexZhhTocFuFu
End if
If (ShiftIRdiv) Then 
AmpVertexhhTocFuFu = AmpVertexhhTocFuFu  +  AmpVertexIRoshhTocFuFu
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ hh->bar[Fu] Fu -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumhhTocFuFu = AmpTreehhTocFuFu 
 AmpSum2hhTocFuFu = AmpTreehhTocFuFu + 2._dp*AmpWavehhTocFuFu + 2._dp*AmpVertexhhTocFuFu  
Else 
 AmpSumhhTocFuFu = AmpTreehhTocFuFu + AmpWavehhTocFuFu + AmpVertexhhTocFuFu
 AmpSum2hhTocFuFu = AmpTreehhTocFuFu + AmpWavehhTocFuFu + AmpVertexhhTocFuFu 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumhhTocFuFu = AmpTreehhTocFuFu
 AmpSum2hhTocFuFu = AmpTreehhTocFuFu 
End if 
Do gt1=1,2
i4 = isave 
  Do gt2=1,3
    Do gt3=1,3
If (((OSkinematics).and.(MhhOS(gt1).gt.(MFuOS(gt2)+MFuOS(gt3)))).or.((.not.OSkinematics).and.(Mhh(gt1).gt.(MFu(gt2)+MFu(gt3))))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2, gt3 
  AmpSum2hhTocFuFu = AmpTreehhTocFuFu
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFuOS(gt2),MFuOS(gt3),AmpSumhhTocFuFu(:,gt1, gt2, gt3),AmpSum2hhTocFuFu(:,gt1, gt2, gt3),AmpSqhhTocFuFu(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFu(gt2),MFu(gt3),AmpSumhhTocFuFu(:,gt1, gt2, gt3),AmpSum2hhTocFuFu(:,gt1, gt2, gt3),AmpSqhhTocFuFu(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqhhTocFuFu(gt1, gt2, gt3) 
  AmpSum2hhTocFuFu = 2._dp*AmpWavehhTocFuFu
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFuOS(gt2),MFuOS(gt3),AmpSumhhTocFuFu(:,gt1, gt2, gt3),AmpSum2hhTocFuFu(:,gt1, gt2, gt3),AmpSqhhTocFuFu(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFu(gt2),MFu(gt3),AmpSumhhTocFuFu(:,gt1, gt2, gt3),AmpSum2hhTocFuFu(:,gt1, gt2, gt3),AmpSqhhTocFuFu(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqhhTocFuFu(gt1, gt2, gt3) 
  AmpSum2hhTocFuFu = 2._dp*AmpVertexhhTocFuFu
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFuOS(gt2),MFuOS(gt3),AmpSumhhTocFuFu(:,gt1, gt2, gt3),AmpSum2hhTocFuFu(:,gt1, gt2, gt3),AmpSqhhTocFuFu(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFu(gt2),MFu(gt3),AmpSumhhTocFuFu(:,gt1, gt2, gt3),AmpSum2hhTocFuFu(:,gt1, gt2, gt3),AmpSqhhTocFuFu(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqhhTocFuFu(gt1, gt2, gt3) 
  AmpSum2hhTocFuFu = AmpTreehhTocFuFu + 2._dp*AmpWavehhTocFuFu + 2._dp*AmpVertexhhTocFuFu
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFuOS(gt2),MFuOS(gt3),AmpSumhhTocFuFu(:,gt1, gt2, gt3),AmpSum2hhTocFuFu(:,gt1, gt2, gt3),AmpSqhhTocFuFu(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFu(gt2),MFu(gt3),AmpSumhhTocFuFu(:,gt1, gt2, gt3),AmpSum2hhTocFuFu(:,gt1, gt2, gt3),AmpSqhhTocFuFu(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqhhTocFuFu(gt1, gt2, gt3) 
 End if 
If (OSkinematics) Then 
  AmpSum2hhTocFuFu = AmpTreehhTocFuFu
  Call SquareAmp_StoFF(MhhOS(gt1),MFuOS(gt2),MFuOS(gt3),AmpSumhhTocFuFu(:,gt1, gt2, gt3),AmpSum2hhTocFuFu(:,gt1, gt2, gt3),AmpSqhhTocFuFu(gt1, gt2, gt3)) 
  AmpSqTreehhTocFuFu(gt1, gt2, gt3) = AmpSqhhTocFuFu(gt1, gt2, gt3)  
  AmpSum2hhTocFuFu = + 2._dp*AmpWavehhTocFuFu + 2._dp*AmpVertexhhTocFuFu
  Call SquareAmp_StoFF(MhhOS(gt1),MFuOS(gt2),MFuOS(gt3),AmpSumhhTocFuFu(:,gt1, gt2, gt3),AmpSum2hhTocFuFu(:,gt1, gt2, gt3),AmpSqhhTocFuFu(gt1, gt2, gt3)) 
  AmpSqhhTocFuFu(gt1, gt2, gt3) = AmpSqhhTocFuFu(gt1, gt2, gt3) + AmpSqTreehhTocFuFu(gt1, gt2, gt3)  
Else  
  AmpSum2hhTocFuFu = AmpTreehhTocFuFu
  Call SquareAmp_StoFF(Mhh(gt1),MFu(gt2),MFu(gt3),AmpSumhhTocFuFu(:,gt1, gt2, gt3),AmpSum2hhTocFuFu(:,gt1, gt2, gt3),AmpSqhhTocFuFu(gt1, gt2, gt3)) 
  AmpSqTreehhTocFuFu(gt1, gt2, gt3) = AmpSqhhTocFuFu(gt1, gt2, gt3)  
  AmpSum2hhTocFuFu = + 2._dp*AmpWavehhTocFuFu + 2._dp*AmpVertexhhTocFuFu
  Call SquareAmp_StoFF(Mhh(gt1),MFu(gt2),MFu(gt3),AmpSumhhTocFuFu(:,gt1, gt2, gt3),AmpSum2hhTocFuFu(:,gt1, gt2, gt3),AmpSqhhTocFuFu(gt1, gt2, gt3)) 
  AmpSqhhTocFuFu(gt1, gt2, gt3) = AmpSqhhTocFuFu(gt1, gt2, gt3) + AmpSqTreehhTocFuFu(gt1, gt2, gt3)  
End if  
Else  
  AmpSqhhTocFuFu(gt1, gt2, gt3) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 4._dp 
If (AmpSqhhTocFuFu(gt1, gt2, gt3).le.0._dp) Then 
  gP1Lhh(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1Lhh(gt1,i4) = 3._dp*GammaTPS(MhhOS(gt1),MFuOS(gt2),MFuOS(gt3),helfactor*AmpSqhhTocFuFu(gt1, gt2, gt3))
Else 
  gP1Lhh(gt1,i4) = 3._dp*GammaTPS(Mhh(gt1),MFu(gt2),MFu(gt3),helfactor*AmpSqhhTocFuFu(gt1, gt2, gt3))
End if 
If ((Abs(MRPhhTocFuFu(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGhhTocFuFu(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1Lhh(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPhhTocFuFu(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGhhTocFuFu(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPhhTocFuFu(gt1, gt2, gt3) + MRGhhTocFuFu(gt1, gt2, gt3)) 
  gP1Lhh(gt1,i4) = gP1Lhh(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPhhTocFuFu(gt1, gt2, gt3) + MRGhhTocFuFu(gt1, gt2, gt3))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1Lhh(gt1,i4) 
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
! bar(Fxv) Fxv
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_hhTocFxvFxv(cplcFxvFxvhhL,cplcFxvFxvhhR,MFxv,           & 
& Mhh,MFxv2,Mhh2,AmpTreehhTocFxvFxv)

  Else 
Call Amplitude_Tree_SDdiracDM_hhTocFxvFxv(ZcplcFxvFxvhhL,ZcplcFxvFxvhhR,              & 
& MFxv,Mhh,MFxv2,Mhh2,AmpTreehhTocFxvFxv)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_hhTocFxvFxv(MLambda,em,gs,cplcFxvFxvhhL,cplcFxvFxvhhR,      & 
& MFxvOS,MhhOS,MRPhhTocFxvFxv,MRGhhTocFxvFxv)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_hhTocFxvFxv(MLambda,em,gs,ZcplcFxvFxvhhL,ZcplcFxvFxvhhR,    & 
& MFxvOS,MhhOS,MRPhhTocFxvFxv,MRGhhTocFxvFxv)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_hhTocFxvFxv(MLambda,em,gs,cplcFxvFxvhhL,cplcFxvFxvhhR,      & 
& MFxv,Mhh,MRPhhTocFxvFxv,MRGhhTocFxvFxv)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_hhTocFxvFxv(MLambda,em,gs,ZcplcFxvFxvhhL,ZcplcFxvFxvhhR,    & 
& MFxv,Mhh,MRPhhTocFxvFxv,MRGhhTocFxvFxv)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_hhTocFxvFxv(cplcFxvFxvhhL,cplcFxvFxvhhR,ctcplcFxvFxvhhL,& 
& ctcplcFxvFxvhhR,MFxv,MFxv2,Mhh,Mhh2,Zfhh,ZfxVL,ZfxVR,AmpWavehhTocFxvFxv)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_hhTocFxvFxv(MAh,MFv,MFxe,MFxv,Mhh,MHp,MSsc,           & 
& MVWp,MVZ,MAh2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplAhAhhh,cplcFxvFxvAhL,     & 
& cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,       & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,               & 
& cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,             & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,         & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexhhTocFxvFxv)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFxvFxv(MAh,MFv,MFxe,MFxv,Mhh,MHp,             & 
& MSsc,MVWp,MVZ,MAh2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplAhAhhh,              & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,       & 
& cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,               & 
& cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,              & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,       & 
& cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRdrhhTocFxvFxv)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFxvFxv(MAhOS,MFvOS,MFxeOS,MFxvOS,             & 
& MhhOS,MHpOS,MSscOS,MVWpOS,MVZOS,MAh2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,           & 
& MSsc2OS,MVWp2OS,MVZ2OS,cplAhAhhh,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,  & 
& cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,               & 
& ZcplcFxvFxvhhL,ZcplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,              & 
& cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,          & 
& cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,      & 
& AmpVertexIRoshhTocFxvFxv)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFxvFxv(MAh,MFv,MFxe,MFxv,Mhh,MHp,             & 
& MSsc,MVWp,MVZ,MAh2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplAhAhhh,              & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,       & 
& cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,ZcplcFxvFxvhhL,ZcplcFxvFxvhhR,             & 
& cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,              & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,       & 
& cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRoshhTocFxvFxv)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFxvFxv(MAhOS,MFvOS,MFxeOS,MFxvOS,             & 
& MhhOS,MHpOS,MSscOS,MVWpOS,MVZOS,MAh2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,           & 
& MSsc2OS,MVWp2OS,MVZ2OS,cplAhAhhh,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,  & 
& cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR, & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhhhhh,               & 
& cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRoshhTocFxvFxv)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocFxvFxv(MAh,MFv,MFxe,MFxv,Mhh,MHp,             & 
& MSsc,MVWp,MVZ,MAh2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplAhAhhh,              & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,       & 
& cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,               & 
& cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,              & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,       & 
& cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexIRoshhTocFxvFxv)

 End if 
 End if 
AmpVertexhhTocFxvFxv = AmpVertexhhTocFxvFxv -  AmpVertexIRdrhhTocFxvFxv! +  AmpVertexIRoshhTocFxvFxv ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZhhTocFxvFxv=0._dp 
AmpVertexZhhTocFxvFxv=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZhhTocFxvFxv(:,gt2,:,:) = AmpWaveZhhTocFxvFxv(:,gt2,:,:)+ZRUZH(gt2,gt1)*AmpWavehhTocFxvFxv(:,gt1,:,:) 
AmpVertexZhhTocFxvFxv(:,gt2,:,:)= AmpVertexZhhTocFxvFxv(:,gt2,:,:) + ZRUZH(gt2,gt1)*AmpVertexhhTocFxvFxv(:,gt1,:,:) 
 End Do 
End Do 
AmpWavehhTocFxvFxv=AmpWaveZhhTocFxvFxv 
AmpVertexhhTocFxvFxv= AmpVertexZhhTocFxvFxv
! Final State 1 
AmpWaveZhhTocFxvFxv=0._dp 
AmpVertexZhhTocFxvFxv=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZhhTocFxvFxv(1,:,gt2,:) = AmpWaveZhhTocFxvFxv(1,:,gt2,:)+ZRUXU(gt2,gt1)*AmpWavehhTocFxvFxv(1,:,gt1,:) 
AmpVertexZhhTocFxvFxv(1,:,gt2,:)= AmpVertexZhhTocFxvFxv(1,:,gt2,:)+ZRUXU(gt2,gt1)*AmpVertexhhTocFxvFxv(1,:,gt1,:) 
AmpWaveZhhTocFxvFxv(2,:,gt2,:) = AmpWaveZhhTocFxvFxv(2,:,gt2,:)+ZRUXVc(gt2,gt1)*AmpWavehhTocFxvFxv(2,:,gt1,:) 
AmpVertexZhhTocFxvFxv(2,:,gt2,:)= AmpVertexZhhTocFxvFxv(2,:,gt2,:)+ZRUXVc(gt2,gt1)*AmpVertexhhTocFxvFxv(2,:,gt1,:) 
 End Do 
End Do 
AmpWavehhTocFxvFxv=AmpWaveZhhTocFxvFxv 
AmpVertexhhTocFxvFxv= AmpVertexZhhTocFxvFxv
! Final State 2 
AmpWaveZhhTocFxvFxv=0._dp 
AmpVertexZhhTocFxvFxv=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZhhTocFxvFxv(1,:,:,gt2) = AmpWaveZhhTocFxvFxv(1,:,:,gt2)+ZRUXV(gt2,gt1)*AmpWavehhTocFxvFxv(1,:,:,gt1) 
AmpVertexZhhTocFxvFxv(1,:,:,gt2)= AmpVertexZhhTocFxvFxv(1,:,:,gt2)+ZRUXV(gt2,gt1)*AmpVertexhhTocFxvFxv(1,:,:,gt1) 
AmpWaveZhhTocFxvFxv(2,:,:,gt2) = AmpWaveZhhTocFxvFxv(2,:,:,gt2)+ZRUXU(gt2,gt1)*AmpWavehhTocFxvFxv(2,:,:,gt1) 
AmpVertexZhhTocFxvFxv(2,:,:,gt2)= AmpVertexZhhTocFxvFxv(2,:,:,gt2)+ZRUXU(gt2,gt1)*AmpVertexhhTocFxvFxv(2,:,:,gt1) 
 End Do 
End Do 
AmpWavehhTocFxvFxv=AmpWaveZhhTocFxvFxv 
AmpVertexhhTocFxvFxv= AmpVertexZhhTocFxvFxv
End if
If (ShiftIRdiv) Then 
AmpVertexhhTocFxvFxv = AmpVertexhhTocFxvFxv  +  AmpVertexIRoshhTocFxvFxv
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ hh->bar[Fxv] Fxv -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumhhTocFxvFxv = AmpTreehhTocFxvFxv 
 AmpSum2hhTocFxvFxv = AmpTreehhTocFxvFxv + 2._dp*AmpWavehhTocFxvFxv + 2._dp*AmpVertexhhTocFxvFxv  
Else 
 AmpSumhhTocFxvFxv = AmpTreehhTocFxvFxv + AmpWavehhTocFxvFxv + AmpVertexhhTocFxvFxv
 AmpSum2hhTocFxvFxv = AmpTreehhTocFxvFxv + AmpWavehhTocFxvFxv + AmpVertexhhTocFxvFxv 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumhhTocFxvFxv = AmpTreehhTocFxvFxv
 AmpSum2hhTocFxvFxv = AmpTreehhTocFxvFxv 
End if 
Do gt1=1,2
i4 = isave 
  Do gt2=1,2
    Do gt3=1,2
If (((OSkinematics).and.(MhhOS(gt1).gt.(MFxvOS(gt2)+MFxvOS(gt3)))).or.((.not.OSkinematics).and.(Mhh(gt1).gt.(MFxv(gt2)+MFxv(gt3))))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2, gt3 
  AmpSum2hhTocFxvFxv = AmpTreehhTocFxvFxv
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFxvOS(gt2),MFxvOS(gt3),AmpSumhhTocFxvFxv(:,gt1, gt2, gt3),AmpSum2hhTocFxvFxv(:,gt1, gt2, gt3),AmpSqhhTocFxvFxv(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFxv(gt2),MFxv(gt3),AmpSumhhTocFxvFxv(:,gt1, gt2, gt3),AmpSum2hhTocFxvFxv(:,gt1, gt2, gt3),AmpSqhhTocFxvFxv(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqhhTocFxvFxv(gt1, gt2, gt3) 
  AmpSum2hhTocFxvFxv = 2._dp*AmpWavehhTocFxvFxv
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFxvOS(gt2),MFxvOS(gt3),AmpSumhhTocFxvFxv(:,gt1, gt2, gt3),AmpSum2hhTocFxvFxv(:,gt1, gt2, gt3),AmpSqhhTocFxvFxv(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFxv(gt2),MFxv(gt3),AmpSumhhTocFxvFxv(:,gt1, gt2, gt3),AmpSum2hhTocFxvFxv(:,gt1, gt2, gt3),AmpSqhhTocFxvFxv(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqhhTocFxvFxv(gt1, gt2, gt3) 
  AmpSum2hhTocFxvFxv = 2._dp*AmpVertexhhTocFxvFxv
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFxvOS(gt2),MFxvOS(gt3),AmpSumhhTocFxvFxv(:,gt1, gt2, gt3),AmpSum2hhTocFxvFxv(:,gt1, gt2, gt3),AmpSqhhTocFxvFxv(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFxv(gt2),MFxv(gt3),AmpSumhhTocFxvFxv(:,gt1, gt2, gt3),AmpSum2hhTocFxvFxv(:,gt1, gt2, gt3),AmpSqhhTocFxvFxv(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqhhTocFxvFxv(gt1, gt2, gt3) 
  AmpSum2hhTocFxvFxv = AmpTreehhTocFxvFxv + 2._dp*AmpWavehhTocFxvFxv + 2._dp*AmpVertexhhTocFxvFxv
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFxvOS(gt2),MFxvOS(gt3),AmpSumhhTocFxvFxv(:,gt1, gt2, gt3),AmpSum2hhTocFxvFxv(:,gt1, gt2, gt3),AmpSqhhTocFxvFxv(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFxv(gt2),MFxv(gt3),AmpSumhhTocFxvFxv(:,gt1, gt2, gt3),AmpSum2hhTocFxvFxv(:,gt1, gt2, gt3),AmpSqhhTocFxvFxv(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqhhTocFxvFxv(gt1, gt2, gt3) 
 End if 
If (OSkinematics) Then 
  AmpSum2hhTocFxvFxv = AmpTreehhTocFxvFxv
  Call SquareAmp_StoFF(MhhOS(gt1),MFxvOS(gt2),MFxvOS(gt3),AmpSumhhTocFxvFxv(:,gt1, gt2, gt3),AmpSum2hhTocFxvFxv(:,gt1, gt2, gt3),AmpSqhhTocFxvFxv(gt1, gt2, gt3)) 
  AmpSqTreehhTocFxvFxv(gt1, gt2, gt3) = AmpSqhhTocFxvFxv(gt1, gt2, gt3)  
  AmpSum2hhTocFxvFxv = + 2._dp*AmpWavehhTocFxvFxv + 2._dp*AmpVertexhhTocFxvFxv
  Call SquareAmp_StoFF(MhhOS(gt1),MFxvOS(gt2),MFxvOS(gt3),AmpSumhhTocFxvFxv(:,gt1, gt2, gt3),AmpSum2hhTocFxvFxv(:,gt1, gt2, gt3),AmpSqhhTocFxvFxv(gt1, gt2, gt3)) 
  AmpSqhhTocFxvFxv(gt1, gt2, gt3) = AmpSqhhTocFxvFxv(gt1, gt2, gt3) + AmpSqTreehhTocFxvFxv(gt1, gt2, gt3)  
Else  
  AmpSum2hhTocFxvFxv = AmpTreehhTocFxvFxv
  Call SquareAmp_StoFF(Mhh(gt1),MFxv(gt2),MFxv(gt3),AmpSumhhTocFxvFxv(:,gt1, gt2, gt3),AmpSum2hhTocFxvFxv(:,gt1, gt2, gt3),AmpSqhhTocFxvFxv(gt1, gt2, gt3)) 
  AmpSqTreehhTocFxvFxv(gt1, gt2, gt3) = AmpSqhhTocFxvFxv(gt1, gt2, gt3)  
  AmpSum2hhTocFxvFxv = + 2._dp*AmpWavehhTocFxvFxv + 2._dp*AmpVertexhhTocFxvFxv
  Call SquareAmp_StoFF(Mhh(gt1),MFxv(gt2),MFxv(gt3),AmpSumhhTocFxvFxv(:,gt1, gt2, gt3),AmpSum2hhTocFxvFxv(:,gt1, gt2, gt3),AmpSqhhTocFxvFxv(gt1, gt2, gt3)) 
  AmpSqhhTocFxvFxv(gt1, gt2, gt3) = AmpSqhhTocFxvFxv(gt1, gt2, gt3) + AmpSqTreehhTocFxvFxv(gt1, gt2, gt3)  
End if  
Else  
  AmpSqhhTocFxvFxv(gt1, gt2, gt3) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 4._dp 
If (AmpSqhhTocFxvFxv(gt1, gt2, gt3).le.0._dp) Then 
  gP1Lhh(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(MhhOS(gt1),MFxvOS(gt2),MFxvOS(gt3),helfactor*AmpSqhhTocFxvFxv(gt1, gt2, gt3))
Else 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(Mhh(gt1),MFxv(gt2),MFxv(gt3),helfactor*AmpSqhhTocFxvFxv(gt1, gt2, gt3))
End if 
If ((Abs(MRPhhTocFxvFxv(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGhhTocFxvFxv(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1Lhh(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPhhTocFxvFxv(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGhhTocFxvFxv(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPhhTocFxvFxv(gt1, gt2, gt3) + MRGhhTocFxvFxv(gt1, gt2, gt3)) 
  gP1Lhh(gt1,i4) = gP1Lhh(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPhhTocFxvFxv(gt1, gt2, gt3) + MRGhhTocFxvFxv(gt1, gt2, gt3))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1Lhh(gt1,i4) 
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
! hh hh
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_hhTohhhh(cplhhhhhh,Mhh,Mhh2,AmpTreehhTohhhh)

  Else 
Call Amplitude_Tree_SDdiracDM_hhTohhhh(Zcplhhhhhh,Mhh,Mhh2,AmpTreehhTohhhh)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_hhTohhhh(MLambda,em,gs,cplhhhhhh,MhhOS,MRPhhTohhhh,         & 
& MRGhhTohhhh)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_hhTohhhh(MLambda,em,gs,Zcplhhhhhh,MhhOS,MRPhhTohhhh,        & 
& MRGhhTohhhh)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_hhTohhhh(MLambda,em,gs,cplhhhhhh,Mhh,MRPhhTohhhh,           & 
& MRGhhTohhhh)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_hhTohhhh(MLambda,em,gs,Zcplhhhhhh,Mhh,MRPhhTohhhh,          & 
& MRGhhTohhhh)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_hhTohhhh(cplhhhhhh,ctcplhhhhhh,Mhh,Mhh2,Zfhh,           & 
& AmpWavehhTohhhh)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_hhTohhhh(MAh,MFd,MFe,MFu,MFxv,Mhh,MHp,MSsc,           & 
& MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplAhAhhh,               & 
& cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,     & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcgWpgWphh,cplcgWCgWChh,cplcgZgZhh,cplhhhhhh,            & 
& cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,cplAhAhhhhh1,   & 
& cplhhhhhhhh1,cplhhhhHpcHp1,cplhhhhSsccSsc1,cplhhhhcVWpVWp1,cplhhhhVZVZ1,               & 
& AmpVertexhhTohhhh)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTohhhh(MAh,MFd,MFe,MFu,MFxv,Mhh,MHp,             & 
& MSsc,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplAhAhhh,          & 
& cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,     & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcgWpgWphh,cplcgWCgWChh,cplcgZgZhh,cplhhhhhh,            & 
& cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,cplAhAhhhhh1,   & 
& cplhhhhhhhh1,cplhhhhHpcHp1,cplhhhhSsccSsc1,cplhhhhcVWpVWp1,cplhhhhVZVZ1,               & 
& AmpVertexIRdrhhTohhhh)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTohhhh(MAhOS,MFdOS,MFeOS,MFuOS,MFxvOS,           & 
& MhhOS,MHpOS,MSscOS,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFe2OS,MFu2OS,MFxv2OS,Mhh2OS,            & 
& MHp2OS,MSsc2OS,MVWp2OS,MVZ2OS,cplAhAhhh,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,             & 
& cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,cplcFxvFxvhhL,cplcFxvFxvhhR,           & 
& cplcgWpgWphh,cplcgWCgWChh,cplcgZgZhh,Zcplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,   & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,cplAhAhhhhh1,cplhhhhhhhh1,cplhhhhHpcHp1,            & 
& cplhhhhSsccSsc1,cplhhhhcVWpVWp1,cplhhhhVZVZ1,AmpVertexIRoshhTohhhh)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTohhhh(MAh,MFd,MFe,MFu,MFxv,Mhh,MHp,             & 
& MSsc,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplAhAhhh,          & 
& cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,     & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcgWpgWphh,cplcgWCgWChh,cplcgZgZhh,Zcplhhhhhh,           & 
& cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,cplAhAhhhhh1,   & 
& cplhhhhhhhh1,cplhhhhHpcHp1,cplhhhhSsccSsc1,cplhhhhcVWpVWp1,cplhhhhVZVZ1,               & 
& AmpVertexIRoshhTohhhh)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTohhhh(MAhOS,MFdOS,MFeOS,MFuOS,MFxvOS,           & 
& MhhOS,MHpOS,MSscOS,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFe2OS,MFu2OS,MFxv2OS,Mhh2OS,            & 
& MHp2OS,MSsc2OS,MVWp2OS,MVZ2OS,cplAhAhhh,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,             & 
& cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,cplcFxvFxvhhL,cplcFxvFxvhhR,           & 
& cplcgWpgWphh,cplcgWCgWChh,cplcgZgZhh,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,    & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,cplAhAhhhhh1,cplhhhhhhhh1,cplhhhhHpcHp1,            & 
& cplhhhhSsccSsc1,cplhhhhcVWpVWp1,cplhhhhVZVZ1,AmpVertexIRoshhTohhhh)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTohhhh(MAh,MFd,MFe,MFu,MFxv,Mhh,MHp,             & 
& MSsc,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplAhAhhh,          & 
& cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,     & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcgWpgWphh,cplcgWCgWChh,cplcgZgZhh,cplhhhhhh,            & 
& cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,cplAhAhhhhh1,   & 
& cplhhhhhhhh1,cplhhhhHpcHp1,cplhhhhSsccSsc1,cplhhhhcVWpVWp1,cplhhhhVZVZ1,               & 
& AmpVertexIRoshhTohhhh)

 End if 
 End if 
AmpVertexhhTohhhh = AmpVertexhhTohhhh -  AmpVertexIRdrhhTohhhh! +  AmpVertexIRoshhTohhhh ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZhhTohhhh=0._dp 
AmpVertexZhhTohhhh=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZhhTohhhh(gt2,:,:) = AmpWaveZhhTohhhh(gt2,:,:)+ZRUZH(gt2,gt1)*AmpWavehhTohhhh(gt1,:,:) 
AmpVertexZhhTohhhh(gt2,:,:)= AmpVertexZhhTohhhh(gt2,:,:) + ZRUZH(gt2,gt1)*AmpVertexhhTohhhh(gt1,:,:) 
 End Do 
End Do 
AmpWavehhTohhhh=AmpWaveZhhTohhhh 
AmpVertexhhTohhhh= AmpVertexZhhTohhhh
! Final State 1 
AmpWaveZhhTohhhh=0._dp 
AmpVertexZhhTohhhh=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZhhTohhhh(:,gt2,:) = AmpWaveZhhTohhhh(:,gt2,:)+ZRUZH(gt2,gt1)*AmpWavehhTohhhh(:,gt1,:) 
AmpVertexZhhTohhhh(:,gt2,:)= AmpVertexZhhTohhhh(:,gt2,:)+ZRUZH(gt2,gt1)*AmpVertexhhTohhhh(:,gt1,:) 
 End Do 
End Do 
AmpWavehhTohhhh=AmpWaveZhhTohhhh 
AmpVertexhhTohhhh= AmpVertexZhhTohhhh
! Final State 2 
AmpWaveZhhTohhhh=0._dp 
AmpVertexZhhTohhhh=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZhhTohhhh(:,:,gt2) = AmpWaveZhhTohhhh(:,:,gt2)+ZRUZH(gt2,gt1)*AmpWavehhTohhhh(:,:,gt1) 
AmpVertexZhhTohhhh(:,:,gt2)= AmpVertexZhhTohhhh(:,:,gt2)+ZRUZH(gt2,gt1)*AmpVertexhhTohhhh(:,:,gt1) 
 End Do 
End Do 
AmpWavehhTohhhh=AmpWaveZhhTohhhh 
AmpVertexhhTohhhh= AmpVertexZhhTohhhh
End if
If (ShiftIRdiv) Then 
AmpVertexhhTohhhh = AmpVertexhhTohhhh  +  AmpVertexIRoshhTohhhh
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ hh->hh hh -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumhhTohhhh = AmpTreehhTohhhh 
 AmpSum2hhTohhhh = AmpTreehhTohhhh + 2._dp*AmpWavehhTohhhh + 2._dp*AmpVertexhhTohhhh  
Else 
 AmpSumhhTohhhh = AmpTreehhTohhhh + AmpWavehhTohhhh + AmpVertexhhTohhhh
 AmpSum2hhTohhhh = AmpTreehhTohhhh + AmpWavehhTohhhh + AmpVertexhhTohhhh 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumhhTohhhh = AmpTreehhTohhhh
 AmpSum2hhTohhhh = AmpTreehhTohhhh 
End if 
Do gt1=1,2
i4 = isave 
  Do gt2=1,2
    Do gt3=gt2,2
If (((OSkinematics).and.(MhhOS(gt1).gt.(MhhOS(gt2)+MhhOS(gt3)))).or.((.not.OSkinematics).and.(Mhh(gt1).gt.(Mhh(gt2)+Mhh(gt3))))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2, gt3 
  AmpSum2hhTohhhh = AmpTreehhTohhhh
If (OSkinematics) Then 
  Call SquareAmp_StoSS(MhhOS(gt1),MhhOS(gt2),MhhOS(gt3),AmpSumhhTohhhh(gt1, gt2, gt3),AmpSum2hhTohhhh(gt1, gt2, gt3),AmpSqhhTohhhh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoSS(Mhh(gt1),Mhh(gt2),Mhh(gt3),AmpSumhhTohhhh(gt1, gt2, gt3),AmpSum2hhTohhhh(gt1, gt2, gt3),AmpSqhhTohhhh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqhhTohhhh(gt1, gt2, gt3) 
  AmpSum2hhTohhhh = 2._dp*AmpWavehhTohhhh
If (OSkinematics) Then 
  Call SquareAmp_StoSS(MhhOS(gt1),MhhOS(gt2),MhhOS(gt3),AmpSumhhTohhhh(gt1, gt2, gt3),AmpSum2hhTohhhh(gt1, gt2, gt3),AmpSqhhTohhhh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoSS(Mhh(gt1),Mhh(gt2),Mhh(gt3),AmpSumhhTohhhh(gt1, gt2, gt3),AmpSum2hhTohhhh(gt1, gt2, gt3),AmpSqhhTohhhh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqhhTohhhh(gt1, gt2, gt3) 
  AmpSum2hhTohhhh = 2._dp*AmpVertexhhTohhhh
If (OSkinematics) Then 
  Call SquareAmp_StoSS(MhhOS(gt1),MhhOS(gt2),MhhOS(gt3),AmpSumhhTohhhh(gt1, gt2, gt3),AmpSum2hhTohhhh(gt1, gt2, gt3),AmpSqhhTohhhh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoSS(Mhh(gt1),Mhh(gt2),Mhh(gt3),AmpSumhhTohhhh(gt1, gt2, gt3),AmpSum2hhTohhhh(gt1, gt2, gt3),AmpSqhhTohhhh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqhhTohhhh(gt1, gt2, gt3) 
  AmpSum2hhTohhhh = AmpTreehhTohhhh + 2._dp*AmpWavehhTohhhh + 2._dp*AmpVertexhhTohhhh
If (OSkinematics) Then 
  Call SquareAmp_StoSS(MhhOS(gt1),MhhOS(gt2),MhhOS(gt3),AmpSumhhTohhhh(gt1, gt2, gt3),AmpSum2hhTohhhh(gt1, gt2, gt3),AmpSqhhTohhhh(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoSS(Mhh(gt1),Mhh(gt2),Mhh(gt3),AmpSumhhTohhhh(gt1, gt2, gt3),AmpSum2hhTohhhh(gt1, gt2, gt3),AmpSqhhTohhhh(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqhhTohhhh(gt1, gt2, gt3) 
 End if 
If (OSkinematics) Then 
  AmpSum2hhTohhhh = AmpTreehhTohhhh
  Call SquareAmp_StoSS(MhhOS(gt1),MhhOS(gt2),MhhOS(gt3),AmpSumhhTohhhh(gt1, gt2, gt3),AmpSum2hhTohhhh(gt1, gt2, gt3),AmpSqhhTohhhh(gt1, gt2, gt3)) 
  AmpSqTreehhTohhhh(gt1, gt2, gt3) = AmpSqhhTohhhh(gt1, gt2, gt3)  
  AmpSum2hhTohhhh = + 2._dp*AmpWavehhTohhhh + 2._dp*AmpVertexhhTohhhh
  Call SquareAmp_StoSS(MhhOS(gt1),MhhOS(gt2),MhhOS(gt3),AmpSumhhTohhhh(gt1, gt2, gt3),AmpSum2hhTohhhh(gt1, gt2, gt3),AmpSqhhTohhhh(gt1, gt2, gt3)) 
  AmpSqhhTohhhh(gt1, gt2, gt3) = AmpSqhhTohhhh(gt1, gt2, gt3) + AmpSqTreehhTohhhh(gt1, gt2, gt3)  
Else  
  AmpSum2hhTohhhh = AmpTreehhTohhhh
  Call SquareAmp_StoSS(Mhh(gt1),Mhh(gt2),Mhh(gt3),AmpSumhhTohhhh(gt1, gt2, gt3),AmpSum2hhTohhhh(gt1, gt2, gt3),AmpSqhhTohhhh(gt1, gt2, gt3)) 
  AmpSqTreehhTohhhh(gt1, gt2, gt3) = AmpSqhhTohhhh(gt1, gt2, gt3)  
  AmpSum2hhTohhhh = + 2._dp*AmpWavehhTohhhh + 2._dp*AmpVertexhhTohhhh
  Call SquareAmp_StoSS(Mhh(gt1),Mhh(gt2),Mhh(gt3),AmpSumhhTohhhh(gt1, gt2, gt3),AmpSum2hhTohhhh(gt1, gt2, gt3),AmpSqhhTohhhh(gt1, gt2, gt3)) 
  AmpSqhhTohhhh(gt1, gt2, gt3) = AmpSqhhTohhhh(gt1, gt2, gt3) + AmpSqTreehhTohhhh(gt1, gt2, gt3)  
End if  
Else  
  AmpSqhhTohhhh(gt1, gt2, gt3) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 1._dp 
If (gt2.ne.gt3) helfactor = 2._dp*helfactor 
If (AmpSqhhTohhhh(gt1, gt2, gt3).le.0._dp) Then 
  gP1Lhh(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1Lhh(gt1,i4) = 1._dp/2._dp*GammaTPS(MhhOS(gt1),MhhOS(gt2),MhhOS(gt3),helfactor*AmpSqhhTohhhh(gt1, gt2, gt3))
Else 
  gP1Lhh(gt1,i4) = 1._dp/2._dp*GammaTPS(Mhh(gt1),Mhh(gt2),Mhh(gt3),helfactor*AmpSqhhTohhhh(gt1, gt2, gt3))
End if 
If ((Abs(MRPhhTohhhh(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGhhTohhhh(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1Lhh(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPhhTohhhh(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGhhTohhhh(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*1._dp/4._dp*helfactor*(MRPhhTohhhh(gt1, gt2, gt3) + MRGhhTohhhh(gt1, gt2, gt3)) 
  gP1Lhh(gt1,i4) = gP1Lhh(gt1,i4) + phasespacefactor*1._dp/4._dp*helfactor*(MRPhhTohhhh(gt1, gt2, gt3) + MRGhhTohhhh(gt1, gt2, gt3))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1Lhh(gt1,i4) 
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
! Conjg(Ssc) Ssc
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_hhTocSscSsc(cplhhSsccSsc,Mhh,MSsc,Mhh2,MSsc2,           & 
& AmpTreehhTocSscSsc)

  Else 
Call Amplitude_Tree_SDdiracDM_hhTocSscSsc(ZcplhhSsccSsc,Mhh,MSsc,Mhh2,MSsc2,          & 
& AmpTreehhTocSscSsc)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_hhTocSscSsc(MLambda,em,gs,cplhhSsccSsc,MhhOS,               & 
& MSscOS,MRPhhTocSscSsc,MRGhhTocSscSsc)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_hhTocSscSsc(MLambda,em,gs,ZcplhhSsccSsc,MhhOS,              & 
& MSscOS,MRPhhTocSscSsc,MRGhhTocSscSsc)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_hhTocSscSsc(MLambda,em,gs,cplhhSsccSsc,Mhh,MSsc,            & 
& MRPhhTocSscSsc,MRGhhTocSscSsc)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_hhTocSscSsc(MLambda,em,gs,ZcplhhSsccSsc,Mhh,MSsc,           & 
& MRPhhTocSscSsc,MRGhhTocSscSsc)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_hhTocSscSsc(cplhhSsccSsc,ctcplhhSsccSsc,Mhh,            & 
& Mhh2,MSsc,MSsc2,Zfhh,ZfSsc,AmpWavehhTocSscSsc)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_hhTocSscSsc(MAh,MFe,MFv,MFxe,MFxv,Mhh,MHp,            & 
& MSsc,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,cplAhAhhh,cplcFeFehhL,cplcFeFehhR,     & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFeFxecSscL,cplcFeFxecSscR, & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplhhhhhh,cplhhHpcHp,        & 
& cplhhSsccSsc,cplAhAhSsccSsc1,cplhhhhSsccSsc1,cplHpSsccHpcSsc1,cplSscSsccSsccSsc1,      & 
& AmpVertexhhTocSscSsc)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocSscSsc(MAh,MFe,MFv,MFxe,MFxv,Mhh,             & 
& MHp,MSsc,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,cplAhAhhh,cplcFeFehhL,             & 
& cplcFeFehhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFeFxecSscL,    & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFvFxvcSscL,cplcFvFxvcSscR,              & 
& cplhhhhhh,cplhhHpcHp,cplhhSsccSsc,cplAhAhSsccSsc1,cplhhhhSsccSsc1,cplHpSsccHpcSsc1,    & 
& cplSscSsccSsccSsc1,AmpVertexIRdrhhTocSscSsc)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocSscSsc(MAhOS,MFeOS,MFvOS,MFxeOS,              & 
& MFxvOS,MhhOS,MHpOS,MSscOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,          & 
& MSsc2OS,cplAhAhhh,cplcFeFehhL,cplcFeFehhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFvSscL,   & 
& cplcFxvFvSscR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,               & 
& cplcFvFxvcSscL,cplcFvFxvcSscR,cplhhhhhh,cplhhHpcHp,ZcplhhSsccSsc,cplAhAhSsccSsc1,      & 
& cplhhhhSsccSsc1,cplHpSsccHpcSsc1,cplSscSsccSsccSsc1,AmpVertexIRoshhTocSscSsc)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocSscSsc(MAh,MFe,MFv,MFxe,MFxv,Mhh,             & 
& MHp,MSsc,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,cplAhAhhh,cplcFeFehhL,             & 
& cplcFeFehhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFeFxecSscL,    & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFvFxvcSscL,cplcFvFxvcSscR,              & 
& cplhhhhhh,cplhhHpcHp,ZcplhhSsccSsc,cplAhAhSsccSsc1,cplhhhhSsccSsc1,cplHpSsccHpcSsc1,   & 
& cplSscSsccSsccSsc1,AmpVertexIRoshhTocSscSsc)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocSscSsc(MAhOS,MFeOS,MFvOS,MFxeOS,              & 
& MFxvOS,MhhOS,MHpOS,MSscOS,MAh2OS,MFe2OS,MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,          & 
& MSsc2OS,cplAhAhhh,cplcFeFehhL,cplcFeFehhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFvSscL,   & 
& cplcFxvFvSscR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,               & 
& cplcFvFxvcSscL,cplcFvFxvcSscR,cplhhhhhh,cplhhHpcHp,cplhhSsccSsc,cplAhAhSsccSsc1,       & 
& cplhhhhSsccSsc1,cplHpSsccHpcSsc1,cplSscSsccSsccSsc1,AmpVertexIRoshhTocSscSsc)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocSscSsc(MAh,MFe,MFv,MFxe,MFxv,Mhh,             & 
& MHp,MSsc,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,cplAhAhhh,cplcFeFehhL,             & 
& cplcFeFehhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFeFxecSscL,    & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFvFxvcSscL,cplcFvFxvcSscR,              & 
& cplhhhhhh,cplhhHpcHp,cplhhSsccSsc,cplAhAhSsccSsc1,cplhhhhSsccSsc1,cplHpSsccHpcSsc1,    & 
& cplSscSsccSsccSsc1,AmpVertexIRoshhTocSscSsc)

 End if 
 End if 
AmpVertexhhTocSscSsc = AmpVertexhhTocSscSsc -  AmpVertexIRdrhhTocSscSsc! +  AmpVertexIRoshhTocSscSsc ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZhhTocSscSsc=0._dp 
AmpVertexZhhTocSscSsc=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZhhTocSscSsc(gt2,:,:) = AmpWaveZhhTocSscSsc(gt2,:,:)+ZRUZH(gt2,gt1)*AmpWavehhTocSscSsc(gt1,:,:) 
AmpVertexZhhTocSscSsc(gt2,:,:)= AmpVertexZhhTocSscSsc(gt2,:,:) + ZRUZH(gt2,gt1)*AmpVertexhhTocSscSsc(gt1,:,:) 
 End Do 
End Do 
AmpWavehhTocSscSsc=AmpWaveZhhTocSscSsc 
AmpVertexhhTocSscSsc= AmpVertexZhhTocSscSsc
! Final State 1 
AmpWaveZhhTocSscSsc=0._dp 
AmpVertexZhhTocSscSsc=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZhhTocSscSsc(:,gt2,:) = AmpWaveZhhTocSscSsc(:,gt2,:)+ZRUVSs(gt2,gt1)*AmpWavehhTocSscSsc(:,gt1,:) 
AmpVertexZhhTocSscSsc(:,gt2,:)= AmpVertexZhhTocSscSsc(:,gt2,:)+ZRUVSs(gt2,gt1)*AmpVertexhhTocSscSsc(:,gt1,:) 
 End Do 
End Do 
AmpWavehhTocSscSsc=AmpWaveZhhTocSscSsc 
AmpVertexhhTocSscSsc= AmpVertexZhhTocSscSsc
! Final State 2 
AmpWaveZhhTocSscSsc=0._dp 
AmpVertexZhhTocSscSsc=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZhhTocSscSsc(:,:,gt2) = AmpWaveZhhTocSscSsc(:,:,gt2)+ZRUVSs(gt2,gt1)*AmpWavehhTocSscSsc(:,:,gt1) 
AmpVertexZhhTocSscSsc(:,:,gt2)= AmpVertexZhhTocSscSsc(:,:,gt2)+ZRUVSs(gt2,gt1)*AmpVertexhhTocSscSsc(:,:,gt1) 
 End Do 
End Do 
AmpWavehhTocSscSsc=AmpWaveZhhTocSscSsc 
AmpVertexhhTocSscSsc= AmpVertexZhhTocSscSsc
End if
If (ShiftIRdiv) Then 
AmpVertexhhTocSscSsc = AmpVertexhhTocSscSsc  +  AmpVertexIRoshhTocSscSsc
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ hh->conj[Ssc] Ssc -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumhhTocSscSsc = AmpTreehhTocSscSsc 
 AmpSum2hhTocSscSsc = AmpTreehhTocSscSsc + 2._dp*AmpWavehhTocSscSsc + 2._dp*AmpVertexhhTocSscSsc  
Else 
 AmpSumhhTocSscSsc = AmpTreehhTocSscSsc + AmpWavehhTocSscSsc + AmpVertexhhTocSscSsc
 AmpSum2hhTocSscSsc = AmpTreehhTocSscSsc + AmpWavehhTocSscSsc + AmpVertexhhTocSscSsc 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumhhTocSscSsc = AmpTreehhTocSscSsc
 AmpSum2hhTocSscSsc = AmpTreehhTocSscSsc 
End if 
Do gt1=1,2
i4 = isave 
  Do gt2=1,2
    Do gt3=1,2
If (((OSkinematics).and.(MhhOS(gt1).gt.(MSscOS(gt2)+MSscOS(gt3)))).or.((.not.OSkinematics).and.(Mhh(gt1).gt.(MSsc(gt2)+MSsc(gt3))))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1, gt2, gt3 
  AmpSum2hhTocSscSsc = AmpTreehhTocSscSsc
If (OSkinematics) Then 
  Call SquareAmp_StoSS(MhhOS(gt1),MSscOS(gt2),MSscOS(gt3),AmpSumhhTocSscSsc(gt1, gt2, gt3),AmpSum2hhTocSscSsc(gt1, gt2, gt3),AmpSqhhTocSscSsc(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoSS(Mhh(gt1),MSsc(gt2),MSsc(gt3),AmpSumhhTocSscSsc(gt1, gt2, gt3),AmpSum2hhTocSscSsc(gt1, gt2, gt3),AmpSqhhTocSscSsc(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqhhTocSscSsc(gt1, gt2, gt3) 
  AmpSum2hhTocSscSsc = 2._dp*AmpWavehhTocSscSsc
If (OSkinematics) Then 
  Call SquareAmp_StoSS(MhhOS(gt1),MSscOS(gt2),MSscOS(gt3),AmpSumhhTocSscSsc(gt1, gt2, gt3),AmpSum2hhTocSscSsc(gt1, gt2, gt3),AmpSqhhTocSscSsc(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoSS(Mhh(gt1),MSsc(gt2),MSsc(gt3),AmpSumhhTocSscSsc(gt1, gt2, gt3),AmpSum2hhTocSscSsc(gt1, gt2, gt3),AmpSqhhTocSscSsc(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqhhTocSscSsc(gt1, gt2, gt3) 
  AmpSum2hhTocSscSsc = 2._dp*AmpVertexhhTocSscSsc
If (OSkinematics) Then 
  Call SquareAmp_StoSS(MhhOS(gt1),MSscOS(gt2),MSscOS(gt3),AmpSumhhTocSscSsc(gt1, gt2, gt3),AmpSum2hhTocSscSsc(gt1, gt2, gt3),AmpSqhhTocSscSsc(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoSS(Mhh(gt1),MSsc(gt2),MSsc(gt3),AmpSumhhTocSscSsc(gt1, gt2, gt3),AmpSum2hhTocSscSsc(gt1, gt2, gt3),AmpSqhhTocSscSsc(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqhhTocSscSsc(gt1, gt2, gt3) 
  AmpSum2hhTocSscSsc = AmpTreehhTocSscSsc + 2._dp*AmpWavehhTocSscSsc + 2._dp*AmpVertexhhTocSscSsc
If (OSkinematics) Then 
  Call SquareAmp_StoSS(MhhOS(gt1),MSscOS(gt2),MSscOS(gt3),AmpSumhhTocSscSsc(gt1, gt2, gt3),AmpSum2hhTocSscSsc(gt1, gt2, gt3),AmpSqhhTocSscSsc(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoSS(Mhh(gt1),MSsc(gt2),MSsc(gt3),AmpSumhhTocSscSsc(gt1, gt2, gt3),AmpSum2hhTocSscSsc(gt1, gt2, gt3),AmpSqhhTocSscSsc(gt1, gt2, gt3)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqhhTocSscSsc(gt1, gt2, gt3) 
 End if 
If (OSkinematics) Then 
  AmpSum2hhTocSscSsc = AmpTreehhTocSscSsc
  Call SquareAmp_StoSS(MhhOS(gt1),MSscOS(gt2),MSscOS(gt3),AmpSumhhTocSscSsc(gt1, gt2, gt3),AmpSum2hhTocSscSsc(gt1, gt2, gt3),AmpSqhhTocSscSsc(gt1, gt2, gt3)) 
  AmpSqTreehhTocSscSsc(gt1, gt2, gt3) = AmpSqhhTocSscSsc(gt1, gt2, gt3)  
  AmpSum2hhTocSscSsc = + 2._dp*AmpWavehhTocSscSsc + 2._dp*AmpVertexhhTocSscSsc
  Call SquareAmp_StoSS(MhhOS(gt1),MSscOS(gt2),MSscOS(gt3),AmpSumhhTocSscSsc(gt1, gt2, gt3),AmpSum2hhTocSscSsc(gt1, gt2, gt3),AmpSqhhTocSscSsc(gt1, gt2, gt3)) 
  AmpSqhhTocSscSsc(gt1, gt2, gt3) = AmpSqhhTocSscSsc(gt1, gt2, gt3) + AmpSqTreehhTocSscSsc(gt1, gt2, gt3)  
Else  
  AmpSum2hhTocSscSsc = AmpTreehhTocSscSsc
  Call SquareAmp_StoSS(Mhh(gt1),MSsc(gt2),MSsc(gt3),AmpSumhhTocSscSsc(gt1, gt2, gt3),AmpSum2hhTocSscSsc(gt1, gt2, gt3),AmpSqhhTocSscSsc(gt1, gt2, gt3)) 
  AmpSqTreehhTocSscSsc(gt1, gt2, gt3) = AmpSqhhTocSscSsc(gt1, gt2, gt3)  
  AmpSum2hhTocSscSsc = + 2._dp*AmpWavehhTocSscSsc + 2._dp*AmpVertexhhTocSscSsc
  Call SquareAmp_StoSS(Mhh(gt1),MSsc(gt2),MSsc(gt3),AmpSumhhTocSscSsc(gt1, gt2, gt3),AmpSum2hhTocSscSsc(gt1, gt2, gt3),AmpSqhhTocSscSsc(gt1, gt2, gt3)) 
  AmpSqhhTocSscSsc(gt1, gt2, gt3) = AmpSqhhTocSscSsc(gt1, gt2, gt3) + AmpSqTreehhTocSscSsc(gt1, gt2, gt3)  
End if  
Else  
  AmpSqhhTocSscSsc(gt1, gt2, gt3) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 1._dp 
If (AmpSqhhTocSscSsc(gt1, gt2, gt3).le.0._dp) Then 
  gP1Lhh(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(MhhOS(gt1),MSscOS(gt2),MSscOS(gt3),helfactor*AmpSqhhTocSscSsc(gt1, gt2, gt3))
Else 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(Mhh(gt1),MSsc(gt2),MSsc(gt3),helfactor*AmpSqhhTocSscSsc(gt1, gt2, gt3))
End if 
If ((Abs(MRPhhTocSscSsc(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGhhTocSscSsc(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1Lhh(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPhhTocSscSsc(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGhhTocSscSsc(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*0.5_dp*helfactor*(MRPhhTocSscSsc(gt1, gt2, gt3) + MRGhhTocSscSsc(gt1, gt2, gt3)) 
  gP1Lhh(gt1,i4) = gP1Lhh(gt1,i4) + phasespacefactor*0.5_dp*helfactor*(MRPhhTocSscSsc(gt1, gt2, gt3) + MRGhhTocSscSsc(gt1, gt2, gt3))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1Lhh(gt1,i4) 
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
! Conjg(VWp) VWp
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_hhTocVWpVWp(cplhhcVWpVWp,Mhh,MVWp,Mhh2,MVWp2,           & 
& AmpTreehhTocVWpVWp)

  Else 
Call Amplitude_Tree_SDdiracDM_hhTocVWpVWp(ZcplhhcVWpVWp,Mhh,MVWp,Mhh2,MVWp2,          & 
& AmpTreehhTocVWpVWp)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_hhTocVWpVWp(MLambda,em,gs,cplhhcVWpVWp,MhhOS,               & 
& MVWpOS,MRPhhTocVWpVWp,MRGhhTocVWpVWp)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_hhTocVWpVWp(MLambda,em,gs,ZcplhhcVWpVWp,MhhOS,              & 
& MVWpOS,MRPhhTocVWpVWp,MRGhhTocVWpVWp)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_hhTocVWpVWp(MLambda,em,gs,cplhhcVWpVWp,Mhh,MVWp,            & 
& MRPhhTocVWpVWp,MRGhhTocVWpVWp)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_hhTocVWpVWp(MLambda,em,gs,ZcplhhcVWpVWp,Mhh,MVWp,           & 
& MRPhhTocVWpVWp,MRGhhTocVWpVWp)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_hhTocVWpVWp(cplhhcVWpVWp,ctcplhhcVWpVWp,Mhh,            & 
& Mhh2,MVWp,MVWp2,Zfhh,ZfVWp,AmpWavehhTocVWpVWp)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_hhTocVWpVWp(MAh,MFd,MFe,MFu,MFv,MFxe,MFxv,            & 
& Mhh,MHp,MVP,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MVP2,              & 
& MVWp2,MVZ2,cplAhAhhh,cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL,cplcFdFdhhR,        & 
& cplcFuFdVWpL,cplcFuFdVWpR,cplcFeFehhL,cplcFeFehhR,cplcFvFeVWpL,cplcFvFeVWpR,           & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFdFucVWpL,cplcFdFucVWpR,cplcFeFvcVWpL,cplcFeFvcVWpR,       & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxeFxvcVWpL,             & 
& cplcFxeFxvcVWpR,cplcgZgAhh,cplcgWpgAVWp,cplcgWCgAcVWp,cplcgWpgWphh,cplcgAgWpcVWp,      & 
& cplcgZgWpcVWp,cplcgWCgWChh,cplcgAgWCVWp,cplcgZgWCVWp,cplcgZgZhh,cplcgWpgZVWp,          & 
& cplcgWCgZcVWp,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,               & 
& cplhhVZVZ,cplHpcVWpVP,cplHpcVWpVZ,cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,   & 
& cplAhAhcVWpVWp1,cplhhhhcVWpVWp1,cplhhHpcVWpVP1,cplhhHpcVWpVZ1,cplhhcHpVPVWp1,          & 
& cplhhcHpVWpVZ1,cplHpcHpcVWpVWp1,cplcVWpcVWpVWpVWp1Q,cplcVWpcVWpVWpVWp2Q,               & 
& cplcVWpcVWpVWpVWp3Q,cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q,cplcVWpVWpVZVZ1Q,AmpVertexhhTocVWpVWp)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocVWpVWp(MAh,MFd,MFe,MFu,MFv,MFxe,              & 
& MFxv,Mhh,MHp,MVP,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,              & 
& MVP2,MVWp2,MVZ2,cplAhAhhh,cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL,               & 
& cplcFdFdhhR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFeFehhL,cplcFeFehhR,cplcFvFeVWpL,            & 
& cplcFvFeVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucVWpL,cplcFdFucVWpR,cplcFeFvcVWpL,        & 
& cplcFeFvcVWpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,               & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplcgZgAhh,cplcgWpgAVWp,cplcgWCgAcVWp,cplcgWpgWphh,    & 
& cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgWChh,cplcgAgWCVWp,cplcgZgWCVWp,cplcgZgZhh,         & 
& cplcgWpgZVWp,cplcgWCgZcVWp,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,               & 
& cplhhcVWpVWp,cplhhVZVZ,cplHpcVWpVP,cplHpcVWpVZ,cplcHpVPVWp,cplcVWpVPVWp,               & 
& cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhcVWpVWp1,cplhhhhcVWpVWp1,cplhhHpcVWpVP1,               & 
& cplhhHpcVWpVZ1,cplhhcHpVPVWp1,cplhhcHpVWpVZ1,cplHpcHpcVWpVWp1,cplcVWpcVWpVWpVWp1Q,     & 
& cplcVWpcVWpVWpVWp2Q,cplcVWpcVWpVWpVWp3Q,cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q,             & 
& cplcVWpVWpVZVZ1Q,AmpVertexIRdrhhTocVWpVWp)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocVWpVWp(MAhOS,MFdOS,MFeOS,MFuOS,               & 
& MFvOS,MFxeOS,MFxvOS,MhhOS,MHpOS,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFe2OS,MFu2OS,          & 
& MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,MVP2,MVWp2OS,MVZ2OS,cplAhAhhh,cplAhhhVZ,          & 
& cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdVWpL,cplcFuFdVWpR,             & 
& cplcFeFehhL,cplcFeFehhR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFuFuhhL,cplcFuFuhhR,             & 
& cplcFdFucVWpL,cplcFdFucVWpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR, & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplcgZgAhh,cplcgWpgAVWp,   & 
& cplcgWCgAcVWp,cplcgWpgWphh,cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgWChh,cplcgAgWCVWp,      & 
& cplcgZgWCVWp,cplcgZgZhh,cplcgWpgZVWp,cplcgWCgZcVWp,cplhhhhhh,GosZcplhhHpcHp,           & 
& GosZcplhhHpcVWp,GosZcplhhcHpVWp,ZcplhhcVWpVWp,cplhhVZVZ,GosZcplHpcVWpVP,               & 
& cplHpcVWpVZ,GosZcplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhcVWpVWp1,     & 
& cplhhhhcVWpVWp1,cplhhHpcVWpVP1,cplhhHpcVWpVZ1,cplhhcHpVPVWp1,cplhhcHpVWpVZ1,           & 
& cplHpcHpcVWpVWp1,cplcVWpcVWpVWpVWp1Q,cplcVWpcVWpVWpVWp2Q,cplcVWpcVWpVWpVWp3Q,          & 
& cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q,cplcVWpVWpVZVZ1Q,AmpVertexIRoshhTocVWpVWp)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocVWpVWp(MAh,MFd,MFe,MFu,MFv,MFxe,              & 
& MFxv,Mhh,MHp,MVP,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,              & 
& MVP2,MVWp2,MVZ2,cplAhAhhh,cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL,               & 
& cplcFdFdhhR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFeFehhL,cplcFeFehhR,cplcFvFeVWpL,            & 
& cplcFvFeVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucVWpL,cplcFdFucVWpR,cplcFeFvcVWpL,        & 
& cplcFeFvcVWpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,               & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplcgZgAhh,cplcgWpgAVWp,cplcgWCgAcVWp,cplcgWpgWphh,    & 
& cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgWChh,cplcgAgWCVWp,cplcgZgWCVWp,cplcgZgZhh,         & 
& cplcgWpgZVWp,cplcgWCgZcVWp,cplhhhhhh,GZcplhhHpcHp,GZcplhhHpcVWp,GZcplhhcHpVWp,         & 
& ZcplhhcVWpVWp,cplhhVZVZ,GZcplHpcVWpVP,cplHpcVWpVZ,GZcplcHpVPVWp,cplcVWpVPVWp,          & 
& cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhcVWpVWp1,cplhhhhcVWpVWp1,cplhhHpcVWpVP1,               & 
& cplhhHpcVWpVZ1,cplhhcHpVPVWp1,cplhhcHpVWpVZ1,cplHpcHpcVWpVWp1,cplcVWpcVWpVWpVWp1Q,     & 
& cplcVWpcVWpVWpVWp2Q,cplcVWpcVWpVWpVWp3Q,cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q,             & 
& cplcVWpVWpVZVZ1Q,AmpVertexIRoshhTocVWpVWp)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocVWpVWp(MAhOS,MFdOS,MFeOS,MFuOS,               & 
& MFvOS,MFxeOS,MFxvOS,MhhOS,MHpOS,MVP,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFe2OS,MFu2OS,          & 
& MFv2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,MVP2,MVWp2OS,MVZ2OS,cplAhAhhh,cplAhhhVZ,          & 
& cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdVWpL,cplcFuFdVWpR,             & 
& cplcFeFehhL,cplcFeFehhR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFuFuhhL,cplcFuFuhhR,             & 
& cplcFdFucVWpL,cplcFdFucVWpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR, & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplcgZgAhh,cplcgWpgAVWp,   & 
& cplcgWCgAcVWp,cplcgWpgWphh,cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgWChh,cplcgAgWCVWp,      & 
& cplcgZgWCVWp,cplcgZgZhh,cplcgWpgZVWp,cplcgWCgZcVWp,cplhhhhhh,cplhhHpcHp,               & 
& cplhhHpcVWp,GcplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,cplHpcVWpVP,cplHpcVWpVZ,               & 
& cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhcVWpVWp1,cplhhhhcVWpVWp1,     & 
& cplhhHpcVWpVP1,cplhhHpcVWpVZ1,cplhhcHpVPVWp1,cplhhcHpVWpVZ1,cplHpcHpcVWpVWp1,          & 
& cplcVWpcVWpVWpVWp1Q,cplcVWpcVWpVWpVWp2Q,cplcVWpcVWpVWpVWp3Q,cplcVWpVWpVZVZ2Q,          & 
& cplcVWpVWpVZVZ3Q,cplcVWpVWpVZVZ1Q,AmpVertexIRoshhTocVWpVWp)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhTocVWpVWp(MAh,MFd,MFe,MFu,MFv,MFxe,              & 
& MFxv,Mhh,MHp,MVP,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,              & 
& MVP2,MVWp2,MVZ2,cplAhAhhh,cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL,               & 
& cplcFdFdhhR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFeFehhL,cplcFeFehhR,cplcFvFeVWpL,            & 
& cplcFvFeVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucVWpL,cplcFdFucVWpR,cplcFeFvcVWpL,        & 
& cplcFeFvcVWpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,               & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplcgZgAhh,cplcgWpgAVWp,cplcgWCgAcVWp,cplcgWpgWphh,    & 
& cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgWChh,cplcgAgWCVWp,cplcgZgWCVWp,cplcgZgZhh,         & 
& cplcgWpgZVWp,cplcgWCgZcVWp,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,               & 
& cplhhcVWpVWp,cplhhVZVZ,cplHpcVWpVP,cplHpcVWpVZ,cplcHpVPVWp,cplcVWpVPVWp,               & 
& cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhcVWpVWp1,cplhhhhcVWpVWp1,cplhhHpcVWpVP1,               & 
& cplhhHpcVWpVZ1,cplhhcHpVPVWp1,cplhhcHpVWpVZ1,cplHpcHpcVWpVWp1,cplcVWpcVWpVWpVWp1Q,     & 
& cplcVWpcVWpVWpVWp2Q,cplcVWpcVWpVWpVWp3Q,cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q,             & 
& cplcVWpVWpVZVZ1Q,AmpVertexIRoshhTocVWpVWp)

 End if 
 End if 
AmpVertexhhTocVWpVWp = AmpVertexhhTocVWpVWp -  AmpVertexIRdrhhTocVWpVWp! +  AmpVertexIRoshhTocVWpVWp ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZhhTocVWpVWp=0._dp 
AmpVertexZhhTocVWpVWp=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZhhTocVWpVWp(:,gt2) = AmpWaveZhhTocVWpVWp(:,gt2)+ZRUZH(gt2,gt1)*AmpWavehhTocVWpVWp(:,gt1) 
AmpVertexZhhTocVWpVWp(:,gt2)= AmpVertexZhhTocVWpVWp(:,gt2) + ZRUZH(gt2,gt1)*AmpVertexhhTocVWpVWp(:,gt1) 
 End Do 
End Do 
AmpWavehhTocVWpVWp=AmpWaveZhhTocVWpVWp 
AmpVertexhhTocVWpVWp= AmpVertexZhhTocVWpVWp
End if
If (ShiftIRdiv) Then 
AmpVertexhhTocVWpVWp = AmpVertexhhTocVWpVWp  +  AmpVertexIRoshhTocVWpVWp
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ hh->conj[VWp] VWp -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumhhTocVWpVWp = AmpTreehhTocVWpVWp 
 AmpSum2hhTocVWpVWp = AmpTreehhTocVWpVWp + 2._dp*AmpWavehhTocVWpVWp + 2._dp*AmpVertexhhTocVWpVWp  
Else 
 AmpSumhhTocVWpVWp = AmpTreehhTocVWpVWp + AmpWavehhTocVWpVWp + AmpVertexhhTocVWpVWp
 AmpSum2hhTocVWpVWp = AmpTreehhTocVWpVWp + AmpWavehhTocVWpVWp + AmpVertexhhTocVWpVWp 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumhhTocVWpVWp = AmpTreehhTocVWpVWp
 AmpSum2hhTocVWpVWp = AmpTreehhTocVWpVWp 
End if 
Do gt1=1,2
i4 = isave 
If (((OSkinematics).and.(MhhOS(gt1).gt.(MVWpOS+MVWpOS))).or.((.not.OSkinematics).and.(Mhh(gt1).gt.(MVWp+MVWp)))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1 
  AmpSum2hhTocVWpVWp = AmpTreehhTocVWpVWp
If (OSkinematics) Then 
  Call SquareAmp_StoVV(MhhOS(gt1),MVWpOS,MVWpOS,AmpSumhhTocVWpVWp(:,gt1),AmpSum2hhTocVWpVWp(:,gt1),AmpSqhhTocVWpVWp(gt1)) 
Else  
  Call SquareAmp_StoVV(Mhh(gt1),MVWp,MVWp,AmpSumhhTocVWpVWp(:,gt1),AmpSum2hhTocVWpVWp(:,gt1),AmpSqhhTocVWpVWp(gt1)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqhhTocVWpVWp(gt1) 
  AmpSum2hhTocVWpVWp = 2._dp*AmpWavehhTocVWpVWp
If (OSkinematics) Then 
  Call SquareAmp_StoVV(MhhOS(gt1),MVWpOS,MVWpOS,AmpSumhhTocVWpVWp(:,gt1),AmpSum2hhTocVWpVWp(:,gt1),AmpSqhhTocVWpVWp(gt1)) 
Else  
  Call SquareAmp_StoVV(Mhh(gt1),MVWp,MVWp,AmpSumhhTocVWpVWp(:,gt1),AmpSum2hhTocVWpVWp(:,gt1),AmpSqhhTocVWpVWp(gt1)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqhhTocVWpVWp(gt1) 
  AmpSum2hhTocVWpVWp = 2._dp*AmpVertexhhTocVWpVWp
If (OSkinematics) Then 
  Call SquareAmp_StoVV(MhhOS(gt1),MVWpOS,MVWpOS,AmpSumhhTocVWpVWp(:,gt1),AmpSum2hhTocVWpVWp(:,gt1),AmpSqhhTocVWpVWp(gt1)) 
Else  
  Call SquareAmp_StoVV(Mhh(gt1),MVWp,MVWp,AmpSumhhTocVWpVWp(:,gt1),AmpSum2hhTocVWpVWp(:,gt1),AmpSqhhTocVWpVWp(gt1)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqhhTocVWpVWp(gt1) 
  AmpSum2hhTocVWpVWp = AmpTreehhTocVWpVWp + 2._dp*AmpWavehhTocVWpVWp + 2._dp*AmpVertexhhTocVWpVWp
If (OSkinematics) Then 
  Call SquareAmp_StoVV(MhhOS(gt1),MVWpOS,MVWpOS,AmpSumhhTocVWpVWp(:,gt1),AmpSum2hhTocVWpVWp(:,gt1),AmpSqhhTocVWpVWp(gt1)) 
Else  
  Call SquareAmp_StoVV(Mhh(gt1),MVWp,MVWp,AmpSumhhTocVWpVWp(:,gt1),AmpSum2hhTocVWpVWp(:,gt1),AmpSqhhTocVWpVWp(gt1)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqhhTocVWpVWp(gt1) 
 End if 
If (OSkinematics) Then 
  AmpSum2hhTocVWpVWp = AmpTreehhTocVWpVWp
  Call SquareAmp_StoVV(MhhOS(gt1),MVWpOS,MVWpOS,AmpSumhhTocVWpVWp(:,gt1),AmpSum2hhTocVWpVWp(:,gt1),AmpSqhhTocVWpVWp(gt1)) 
  AmpSqTreehhTocVWpVWp(gt1) = AmpSqhhTocVWpVWp(gt1)  
  AmpSum2hhTocVWpVWp = + 2._dp*AmpWavehhTocVWpVWp + 2._dp*AmpVertexhhTocVWpVWp
  Call SquareAmp_StoVV(MhhOS(gt1),MVWpOS,MVWpOS,AmpSumhhTocVWpVWp(:,gt1),AmpSum2hhTocVWpVWp(:,gt1),AmpSqhhTocVWpVWp(gt1)) 
  AmpSqhhTocVWpVWp(gt1) = AmpSqhhTocVWpVWp(gt1) + AmpSqTreehhTocVWpVWp(gt1)  
Else  
  AmpSum2hhTocVWpVWp = AmpTreehhTocVWpVWp
  Call SquareAmp_StoVV(Mhh(gt1),MVWp,MVWp,AmpSumhhTocVWpVWp(:,gt1),AmpSum2hhTocVWpVWp(:,gt1),AmpSqhhTocVWpVWp(gt1)) 
  AmpSqTreehhTocVWpVWp(gt1) = AmpSqhhTocVWpVWp(gt1)  
  AmpSum2hhTocVWpVWp = + 2._dp*AmpWavehhTocVWpVWp + 2._dp*AmpVertexhhTocVWpVWp
  Call SquareAmp_StoVV(Mhh(gt1),MVWp,MVWp,AmpSumhhTocVWpVWp(:,gt1),AmpSum2hhTocVWpVWp(:,gt1),AmpSqhhTocVWpVWp(gt1)) 
  AmpSqhhTocVWpVWp(gt1) = AmpSqhhTocVWpVWp(gt1) + AmpSqTreehhTocVWpVWp(gt1)  
End if  
Else  
  AmpSqhhTocVWpVWp(gt1) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 1._dp 
If (AmpSqhhTocVWpVWp(gt1).le.0._dp) Then 
  gP1Lhh(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1Lhh(gt1,i4) = 2._dp*GammaTPS(MhhOS(gt1),MVWpOS,MVWpOS,helfactor*AmpSqhhTocVWpVWp(gt1))
Else 
  gP1Lhh(gt1,i4) = 2._dp*GammaTPS(Mhh(gt1),MVWp,MVWp,helfactor*AmpSqhhTocVWpVWp(gt1))
End if 
If ((Abs(MRPhhTocVWpVWp(gt1)).gt.1.0E-20_dp).or.(Abs(MRGhhTocVWpVWp(gt1)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1Lhh(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPhhTocVWpVWp(gt1)).gt.1.0E-20_dp).or.(Abs(MRGhhTocVWpVWp(gt1)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*2._dp*helfactor*(MRPhhTocVWpVWp(gt1) + MRGhhTocVWpVWp(gt1)) 
  gP1Lhh(gt1,i4) = gP1Lhh(gt1,i4) + phasespacefactor*2._dp*helfactor*(MRPhhTocVWpVWp(gt1) + MRGhhTocVWpVWp(gt1))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1Lhh(gt1,i4) 
  End if 
End if 
End if 
i4=i4+1

If (gt1.eq.2) isave = i4 
End do
End If 
If (.not.CalcLoopDecay_LoopInducedOnly) Then 
!---------------- 
! VZ VZ
!---------------- 

!Tree Level 
  If (.not.ExternalZfactors) Then 
Call Amplitude_Tree_SDdiracDM_hhToVZVZ(cplhhVZVZ,Mhh,MVZ,Mhh2,MVZ2,AmpTreehhToVZVZ)

  Else 
Call Amplitude_Tree_SDdiracDM_hhToVZVZ(ZcplhhVZVZ,Mhh,MVZ,Mhh2,MVZ2,AmpTreehhToVZVZ)

  End if 


!Real Corrections 
If (OSkinematics) Then 
  If (.not.ExternalZfactors) Then 
 ! OS and no Z-factors 
Call Gamma_Real_SDdiracDM_hhToVZVZ(MLambda,em,gs,cplhhVZVZ,MhhOS,MVZOS,               & 
& MRPhhToVZVZ,MRGhhToVZVZ)

  Else 
 ! OS and Z-factors 
Call Gamma_Real_SDdiracDM_hhToVZVZ(MLambda,em,gs,ZcplhhVZVZ,MhhOS,MVZOS,              & 
& MRPhhToVZVZ,MRGhhToVZVZ)

  End if 
Else 
 ! DR and no Z-factors 
  If (.not.ExternalZfactors) Then 
Call Gamma_Real_SDdiracDM_hhToVZVZ(MLambda,em,gs,cplhhVZVZ,Mhh,MVZ,MRPhhToVZVZ,       & 
& MRGhhToVZVZ)

  Else 
 ! DR and Z-factors 
Call Gamma_Real_SDdiracDM_hhToVZVZ(MLambda,em,gs,ZcplhhVZVZ,Mhh,MVZ,MRPhhToVZVZ,      & 
& MRGhhToVZVZ)

  End if 
End if 


!Self-energy Corrections 
Call Amplitude_WAVE_SDdiracDM_hhToVZVZ(cplhhVZVZ,ctcplhhVZVZ,Mhh,Mhh2,MVZ,            & 
& MVZ2,Zfhh,ZfVZ,AmpWavehhToVZVZ)



!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_hhToVZVZ(MAh,MFd,MFe,MFu,MFxv,Mhh,MHp,MVWp,           & 
& MVZ,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MVWp2,MVZ2,cplAhAhhh,cplAhhhVZ,cplcFdFdhhL,    & 
& cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVZL,               & 
& cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,cplcFxvFxvhhL,             & 
& cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWphh,cplcgWpgWpVZ,cplcgWCgWChh,      & 
& cplcgWCgWCVZ,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,      & 
& cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ1,cplhhhhVZVZ1,             & 
& cplhhHpcVWpVZ1,cplhhcHpVWpVZ1,cplHpcHpVZVZ1,cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q,         & 
& cplcVWpVWpVZVZ1Q,AmpVertexhhToVZVZ)

If (ShiftIRdiv) Then 
Call Amplitude_IR_VERTEX_SDdiracDM_hhToVZVZ(MAh,MFd,MFe,MFu,MFxv,Mhh,MHp,             & 
& MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MVWp2,MVZ2,cplAhAhhh,cplAhhhVZ,           & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWphh,cplcgWpgWpVZ,     & 
& cplcgWCgWChh,cplcgWCgWCVZ,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,   & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ1,cplhhhhVZVZ1,   & 
& cplhhHpcVWpVZ1,cplhhcHpVWpVZ1,cplHpcHpVZVZ1,cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q,         & 
& cplcVWpVWpVZVZ1Q,AmpVertexIRdrhhToVZVZ)

 If (ExternalZfactors) Then 
  If (OSkinematics) Then 
 ! OS and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhToVZVZ(MAhOS,MFdOS,MFeOS,MFuOS,MFxvOS,           & 
& MhhOS,MHpOS,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFe2OS,MFu2OS,MFxv2OS,Mhh2OS,MHp2OS,            & 
& MVWp2OS,MVZ2OS,cplAhAhhh,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,    & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,               & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,       & 
& cplcgWpgWphh,cplcgWpgWpVZ,cplcgWCgWChh,cplcgWCgWCVZ,cplhhhhhh,cplhhHpcHp,              & 
& cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,ZcplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,    & 
& cplcVWpVWpVZ,cplAhAhVZVZ1,cplhhhhVZVZ1,cplhhHpcVWpVZ1,cplhhcHpVWpVZ1,cplHpcHpVZVZ1,    & 
& cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q,cplcVWpVWpVZVZ1Q,AmpVertexIRoshhToVZVZ)

   Else 
 ! DR and Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhToVZVZ(MAh,MFd,MFe,MFu,MFxv,Mhh,MHp,             & 
& MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MVWp2,MVZ2,cplAhAhhh,cplAhhhVZ,           & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWphh,cplcgWpgWpVZ,     & 
& cplcgWCgWChh,cplcgWCgWCVZ,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,   & 
& ZcplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ1,               & 
& cplhhhhVZVZ1,cplhhHpcVWpVZ1,cplhhcHpVWpVZ1,cplHpcHpVZVZ1,cplcVWpVWpVZVZ2Q,             & 
& cplcVWpVWpVZVZ3Q,cplcVWpVWpVZVZ1Q,AmpVertexIRoshhToVZVZ)

 End if 
 Else 
  If (OSkinematics) Then 
 ! OS and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhToVZVZ(MAhOS,MFdOS,MFeOS,MFuOS,MFxvOS,           & 
& MhhOS,MHpOS,MVWpOS,MVZOS,MAh2OS,MFd2OS,MFe2OS,MFu2OS,MFxv2OS,Mhh2OS,MHp2OS,            & 
& MVWp2OS,MVZ2OS,cplAhAhhh,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,    & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,               & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,       & 
& cplcgWpgWphh,cplcgWpgWpVZ,cplcgWCgWChh,cplcgWCgWCVZ,cplhhhhhh,cplhhHpcHp,              & 
& cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,     & 
& cplcVWpVWpVZ,cplAhAhVZVZ1,cplhhhhVZVZ1,cplhhHpcVWpVZ1,cplhhcHpVWpVZ1,cplHpcHpVZVZ1,    & 
& cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q,cplcVWpVWpVZVZ1Q,AmpVertexIRoshhToVZVZ)

   Else 
 ! DR and no Z-factors 
Call Amplitude_IR_VERTEX_SDdiracDM_hhToVZVZ(MAh,MFd,MFe,MFu,MFxv,Mhh,MHp,             & 
& MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MVWp2,MVZ2,cplAhAhhh,cplAhhhVZ,           & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWphh,cplcgWpgWpVZ,     & 
& cplcgWCgWChh,cplcgWCgWCVZ,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,   & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ1,cplhhhhVZVZ1,   & 
& cplhhHpcVWpVZ1,cplhhcHpVWpVZ1,cplHpcHpVZVZ1,cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q,         & 
& cplcVWpVWpVZVZ1Q,AmpVertexIRoshhToVZVZ)

 End if 
 End if 
AmpVertexhhToVZVZ = AmpVertexhhToVZVZ -  AmpVertexIRdrhhToVZVZ! +  AmpVertexIRoshhToVZVZ ! Shift added later
End if 


 ! Add Z-factors to have external fields on-shell 
 If (ExternalZfactors) Then 
! Decaying particle 
AmpWaveZhhToVZVZ=0._dp 
AmpVertexZhhToVZVZ=0._dp 
Do gt1=1,2
  Do gt2=1,2
AmpWaveZhhToVZVZ(:,gt2) = AmpWaveZhhToVZVZ(:,gt2)+ZRUZH(gt2,gt1)*AmpWavehhToVZVZ(:,gt1) 
AmpVertexZhhToVZVZ(:,gt2)= AmpVertexZhhToVZVZ(:,gt2) + ZRUZH(gt2,gt1)*AmpVertexhhToVZVZ(:,gt1) 
 End Do 
End Do 
AmpWavehhToVZVZ=AmpWaveZhhToVZVZ 
AmpVertexhhToVZVZ= AmpVertexZhhToVZVZ
End if
If (ShiftIRdiv) Then 
AmpVertexhhToVZVZ = AmpVertexhhToVZVZ  +  AmpVertexIRoshhToVZVZ
End if
 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ hh->VZ VZ -----------------------" 
End if 
If (.not.SquareFullAmplitudeDecays) Then 
 AmpSumhhToVZVZ = AmpTreehhToVZVZ 
 AmpSum2hhToVZVZ = AmpTreehhToVZVZ + 2._dp*AmpWavehhToVZVZ + 2._dp*AmpVertexhhToVZVZ  
Else 
 AmpSumhhToVZVZ = AmpTreehhToVZVZ + AmpWavehhToVZVZ + AmpVertexhhToVZVZ
 AmpSum2hhToVZVZ = AmpTreehhToVZVZ + AmpWavehhToVZVZ + AmpVertexhhToVZVZ 
End If 
If (OnlyTreeLevelContributions) Then 
 AmpSumhhToVZVZ = AmpTreehhToVZVZ
 AmpSum2hhToVZVZ = AmpTreehhToVZVZ 
End if 
Do gt1=1,2
i4 = isave 
If (((OSkinematics).and.(MhhOS(gt1).gt.(MVZOS+MVZOS))).or.((.not.OSkinematics).and.(Mhh(gt1).gt.(MVZ+MVZ)))) Then 
 If (DebugLoopDecays) Then 
  Write(*,*) gt1 
  AmpSum2hhToVZVZ = AmpTreehhToVZVZ
If (OSkinematics) Then 
  Call SquareAmp_StoVV(MhhOS(gt1),MVZOS,MVZOS,AmpSumhhToVZVZ(:,gt1),AmpSum2hhToVZVZ(:,gt1),AmpSqhhToVZVZ(gt1)) 
Else  
  Call SquareAmp_StoVV(Mhh(gt1),MVZ,MVZ,AmpSumhhToVZVZ(:,gt1),AmpSum2hhToVZVZ(:,gt1),AmpSqhhToVZVZ(gt1)) 
End if  
  Write(*,*) "TREE x TREE: ",AmpSqhhToVZVZ(gt1) 
  AmpSum2hhToVZVZ = 2._dp*AmpWavehhToVZVZ
If (OSkinematics) Then 
  Call SquareAmp_StoVV(MhhOS(gt1),MVZOS,MVZOS,AmpSumhhToVZVZ(:,gt1),AmpSum2hhToVZVZ(:,gt1),AmpSqhhToVZVZ(gt1)) 
Else  
  Call SquareAmp_StoVV(Mhh(gt1),MVZ,MVZ,AmpSumhhToVZVZ(:,gt1),AmpSum2hhToVZVZ(:,gt1),AmpSqhhToVZVZ(gt1)) 
End if  
  Write(*,*) "TREE x WAVE: ",AmpSqhhToVZVZ(gt1) 
  AmpSum2hhToVZVZ = 2._dp*AmpVertexhhToVZVZ
If (OSkinematics) Then 
  Call SquareAmp_StoVV(MhhOS(gt1),MVZOS,MVZOS,AmpSumhhToVZVZ(:,gt1),AmpSum2hhToVZVZ(:,gt1),AmpSqhhToVZVZ(gt1)) 
Else  
  Call SquareAmp_StoVV(Mhh(gt1),MVZ,MVZ,AmpSumhhToVZVZ(:,gt1),AmpSum2hhToVZVZ(:,gt1),AmpSqhhToVZVZ(gt1)) 
End if  
  Write(*,*) "TREE x VERTEX: ",AmpSqhhToVZVZ(gt1) 
  AmpSum2hhToVZVZ = AmpTreehhToVZVZ + 2._dp*AmpWavehhToVZVZ + 2._dp*AmpVertexhhToVZVZ
If (OSkinematics) Then 
  Call SquareAmp_StoVV(MhhOS(gt1),MVZOS,MVZOS,AmpSumhhToVZVZ(:,gt1),AmpSum2hhToVZVZ(:,gt1),AmpSqhhToVZVZ(gt1)) 
Else  
  Call SquareAmp_StoVV(Mhh(gt1),MVZ,MVZ,AmpSumhhToVZVZ(:,gt1),AmpSum2hhToVZVZ(:,gt1),AmpSqhhToVZVZ(gt1)) 
End if  
  Write(*,*) "TREE x (TREE+WAVE+VERTEX): ",AmpSqhhToVZVZ(gt1) 
 End if 
If (OSkinematics) Then 
  AmpSum2hhToVZVZ = AmpTreehhToVZVZ
  Call SquareAmp_StoVV(MhhOS(gt1),MVZOS,MVZOS,AmpSumhhToVZVZ(:,gt1),AmpSum2hhToVZVZ(:,gt1),AmpSqhhToVZVZ(gt1)) 
  AmpSqTreehhToVZVZ(gt1) = AmpSqhhToVZVZ(gt1)  
  AmpSum2hhToVZVZ = + 2._dp*AmpWavehhToVZVZ + 2._dp*AmpVertexhhToVZVZ
  Call SquareAmp_StoVV(MhhOS(gt1),MVZOS,MVZOS,AmpSumhhToVZVZ(:,gt1),AmpSum2hhToVZVZ(:,gt1),AmpSqhhToVZVZ(gt1)) 
  AmpSqhhToVZVZ(gt1) = AmpSqhhToVZVZ(gt1) + AmpSqTreehhToVZVZ(gt1)  
Else  
  AmpSum2hhToVZVZ = AmpTreehhToVZVZ
  Call SquareAmp_StoVV(Mhh(gt1),MVZ,MVZ,AmpSumhhToVZVZ(:,gt1),AmpSum2hhToVZVZ(:,gt1),AmpSqhhToVZVZ(gt1)) 
  AmpSqTreehhToVZVZ(gt1) = AmpSqhhToVZVZ(gt1)  
  AmpSum2hhToVZVZ = + 2._dp*AmpWavehhToVZVZ + 2._dp*AmpVertexhhToVZVZ
  Call SquareAmp_StoVV(Mhh(gt1),MVZ,MVZ,AmpSumhhToVZVZ(:,gt1),AmpSum2hhToVZVZ(:,gt1),AmpSqhhToVZVZ(gt1)) 
  AmpSqhhToVZVZ(gt1) = AmpSqhhToVZVZ(gt1) + AmpSqTreehhToVZVZ(gt1)  
End if  
Else  
  AmpSqhhToVZVZ(gt1) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 1._dp 
If (AmpSqhhToVZVZ(gt1).le.0._dp) Then 
  gP1Lhh(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(MhhOS(gt1),MVZOS,MVZOS,helfactor*AmpSqhhToVZVZ(gt1))
Else 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(Mhh(gt1),MVZ,MVZ,helfactor*AmpSqhhToVZVZ(gt1))
End if 
If ((Abs(MRPhhToVZVZ(gt1)).gt.1.0E-20_dp).or.(Abs(MRGhhToVZVZ(gt1)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1Lhh(gt1,i4) 
 ! Adding real corrections 
If ((Abs(MRPhhToVZVZ(gt1)).gt.1.0E-20_dp).or.(Abs(MRGhhToVZVZ(gt1)).gt.1.0E-20_dp)) Then 
 If (.not.OnlyTreeLevelContributions) Then 
   If (DebugLoopDecays) Write(*,*) "real", phasespacefactor*1._dp*helfactor*(MRPhhToVZVZ(gt1) + MRGhhToVZVZ(gt1)) 
  gP1Lhh(gt1,i4) = gP1Lhh(gt1,i4) + phasespacefactor*1._dp*helfactor*(MRPhhToVZVZ(gt1) + MRGhhToVZVZ(gt1))
   If (DebugLoopDecays) Write(*,*) "sum",  gP1Lhh(gt1,i4) 
  End if 
End if 
End if 
i4=i4+1

If (gt1.eq.2) isave = i4 
End do
End If 
!---------------- 
! Fv bar(Fv)
!---------------- 

If (LoopInducedDecaysOS) Then 


!Self-energy Corrections 


!Vertex Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_VERTEX_SDdiracDM_hhToFvcFv(MFeOS,MFvOS,MFxvOS,MhhOS,MHpOS,             & 
& MSscOS,MVWpOS,MVZOS,MFe2OS,MFv2OS,MFxv2OS,Mhh2OS,MHp2OS,MSsc2OS,MVWp2OS,               & 
& MVZ2OS,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,      & 
& cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,         & 
& cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFvFxvcSscL,cplcFvFxvcSscR, & 
& cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexhhToFvcFv)

 Else 
Call Amplitude_VERTEX_SDdiracDM_hhToFvcFv(MFeOS,MFvOS,MFxvOS,MhhOS,MHpOS,             & 
& MSscOS,MVWpOS,MVZOS,MFe2OS,MFv2OS,MFxv2OS,Mhh2OS,MHp2OS,MSsc2OS,MVWp2OS,               & 
& MVZ2OS,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,      & 
& cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,         & 
& cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFvFxvcSscL,cplcFvFxvcSscR, & 
& cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexhhToFvcFv)

 End if 
Else 


!Self-energy Corrections 


!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_hhToFvcFv(MFe,MFv,MFxv,Mhh,MHp,MSsc,MVWp,             & 
& MVZ,MFe2,MFv2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,    & 
& cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,         & 
& cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxvhhL,       & 
& cplcFxvFxvhhR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,       & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexhhToFvcFv)

End if 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ hh->Fv bar[Fv] -----------------------" 
End if 
If (OnlyTreeLevelContributions) Then 
 AmpSumhhToFvcFv = 0._dp 
 AmpSum2hhToFvcFv = 0._dp  
Else 
 AmpSumhhToFvcFv = AmpVertexhhToFvcFv + AmpWavehhToFvcFv
 AmpSum2hhToFvcFv = AmpVertexhhToFvcFv + AmpWavehhToFvcFv 
End If 
Do gt1=1,2
i4 = isave 
  Do gt2=1,3
    Do gt3=1,3
If (((OSkinematics).and.(MhhOS(gt1).gt.(MFvOS(gt2)+MFvOS(gt3)))).or.((.not.OSkinematics).and.(Mhh(gt1).gt.(MFv(gt2)+MFv(gt3))))) Then 
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFvOS(gt2),MFvOS(gt3),AmpSumhhToFvcFv(:,gt1, gt2, gt3),AmpSum2hhToFvcFv(:,gt1, gt2, gt3),AmpSqhhToFvcFv(gt1, gt2, gt3)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFv(gt2),MFv(gt3),AmpSumhhToFvcFv(:,gt1, gt2, gt3),AmpSum2hhToFvcFv(:,gt1, gt2, gt3),AmpSqhhToFvcFv(gt1, gt2, gt3)) 
End if  
Else  
  AmpSqhhToFvcFv(gt1, gt2, gt3) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 4._dp 
If (AmpSqhhToFvcFv(gt1, gt2, gt3).le.0._dp) Then 
  gP1Lhh(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(MhhOS(gt1),MFvOS(gt2),MFvOS(gt3),helfactor*AmpSqhhToFvcFv(gt1, gt2, gt3))
Else 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(Mhh(gt1),MFv(gt2),MFv(gt3),helfactor*AmpSqhhToFvcFv(gt1, gt2, gt3))
End if 
If ((Abs(MRPhhToFvcFv(gt1, gt2, gt3)).gt.1.0E-20_dp).or.(Abs(MRGhhToFvcFv(gt1, gt2, gt3)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1Lhh(gt1,i4) 
End if 
i4=i4+1

    End do
  End do
If (gt1.eq.2) isave = i4 
End do
!---------------- 
! Fxe bar(Fxe)
!---------------- 

If (LoopInducedDecaysOS) Then 


!Self-energy Corrections 


!Vertex Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_VERTEX_SDdiracDM_hhToFxecFxe(MFeOS,MFxeOS,MFxvOS,MhhOS,MHpOS,          & 
& MSscOS,MVWpOS,MVZOS,MFe2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,MSsc2OS,MVWp2OS,              & 
& MVZ2OS,cplcFeFehhL,cplcFeFehhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFxeHpL,              & 
& cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,               & 
& cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxeFxvcHpL,              & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,    & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexhhToFxecFxe)

 Else 
Call Amplitude_VERTEX_SDdiracDM_hhToFxecFxe(MFeOS,MFxeOS,MFxvOS,MhhOS,MHpOS,          & 
& MSscOS,MVWpOS,MVZOS,MFe2OS,MFxe2OS,MFxv2OS,Mhh2OS,MHp2OS,MSsc2OS,MVWp2OS,              & 
& MVZ2OS,cplcFeFehhL,cplcFeFehhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFxeHpL,              & 
& cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,               & 
& cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxeFxvcHpL,              & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,    & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexhhToFxecFxe)

 End if 
Else 


!Self-energy Corrections 


!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_hhToFxecFxe(MFe,MFxe,MFxv,Mhh,MHp,MSsc,               & 
& MVWp,MVZ,MFe2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFeFehhL,cplcFeFehhR,          & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR, & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,               & 
& cplcFxvFxvhhR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,           & 
& cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,AmpVertexhhToFxecFxe)

End if 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ hh->Fxe bar[Fxe] -----------------------" 
End if 
If (OnlyTreeLevelContributions) Then 
 AmpSumhhToFxecFxe = 0._dp 
 AmpSum2hhToFxecFxe = 0._dp  
Else 
 AmpSumhhToFxecFxe = AmpVertexhhToFxecFxe + AmpWavehhToFxecFxe
 AmpSum2hhToFxecFxe = AmpVertexhhToFxecFxe + AmpWavehhToFxecFxe 
End If 
Do gt1=1,2
i4 = isave 
If (((OSkinematics).and.(MhhOS(gt1).gt.(MFxeOS+MFxeOS))).or.((.not.OSkinematics).and.(Mhh(gt1).gt.(MFxe+MFxe)))) Then 
If (OSkinematics) Then 
  Call SquareAmp_StoFF(MhhOS(gt1),MFxeOS,MFxeOS,AmpSumhhToFxecFxe(:,gt1),AmpSum2hhToFxecFxe(:,gt1),AmpSqhhToFxecFxe(gt1)) 
Else  
  Call SquareAmp_StoFF(Mhh(gt1),MFxe,MFxe,AmpSumhhToFxecFxe(:,gt1),AmpSum2hhToFxecFxe(:,gt1),AmpSqhhToFxecFxe(gt1)) 
End if  
Else  
  AmpSqhhToFxecFxe(gt1) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 4._dp 
If (AmpSqhhToFxecFxe(gt1).le.0._dp) Then 
  gP1Lhh(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(MhhOS(gt1),MFxeOS,MFxeOS,helfactor*AmpSqhhToFxecFxe(gt1))
Else 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(Mhh(gt1),MFxe,MFxe,helfactor*AmpSqhhToFxecFxe(gt1))
End if 
If ((Abs(MRPhhToFxecFxe(gt1)).gt.1.0E-20_dp).or.(Abs(MRGhhToFxecFxe(gt1)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1Lhh(gt1,i4) 
End if 
i4=i4+1

If (gt1.eq.2) isave = i4 
End do
!---------------- 
! hh VP
!---------------- 

If (LoopInducedDecaysOS) Then 


!Self-energy Corrections 


!Vertex Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_VERTEX_SDdiracDM_hhTohhVP(MFdOS,MFeOS,MFuOS,MhhOS,MHpOS,               & 
& MVP,MVWpOS,MFd2OS,MFe2OS,MFu2OS,Mhh2OS,MHp2OS,MVP2,MVWp2OS,cplcFdFdhhL,cplcFdFdhhR,    & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,               & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVPL,cplcFuFuVPR,cplcgWpgWphh,cplcgWpgWpVP,             & 
& cplcgWCgWChh,cplcgWCgWCVP,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,             & 
& cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,cplhhHpcVWpVP1,cplhhcHpVPVWp1,         & 
& AmpVertexhhTohhVP)

 Else 
Call Amplitude_VERTEX_SDdiracDM_hhTohhVP(MFdOS,MFeOS,MFuOS,MhhOS,MHpOS,               & 
& MVP,MVWpOS,MFd2OS,MFe2OS,MFu2OS,Mhh2OS,MHp2OS,MVP2,MVWp2OS,cplcFdFdhhL,cplcFdFdhhR,    & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,               & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVPL,cplcFuFuVPR,cplcgWpgWphh,cplcgWpgWpVP,             & 
& cplcgWCgWChh,cplcgWCgWCVP,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,             & 
& cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,cplhhHpcVWpVP1,cplhhcHpVPVWp1,         & 
& AmpVertexhhTohhVP)

 End if 
Else 


!Self-energy Corrections 


!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_hhTohhVP(MFd,MFe,MFu,Mhh,MHp,MVP,MVWp,MFd2,           & 
& MFe2,MFu2,Mhh2,MHp2,MVP2,MVWp2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVPL,cplcFdFdVPR,        & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuhhL,cplcFuFuhhR,               & 
& cplcFuFuVPL,cplcFuFuVPR,cplcgWpgWphh,cplcgWpgWpVP,cplcgWCgWChh,cplcgWCgWCVP,           & 
& cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,    & 
& cplcVWpVPVWp,cplhhHpcVWpVP1,cplhhcHpVPVWp1,AmpVertexhhTohhVP)

End if 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ hh->hh VP -----------------------" 
End if 
If (OnlyTreeLevelContributions) Then 
 AmpSumhhTohhVP = 0._dp 
 AmpSum2hhTohhVP = 0._dp  
Else 
 AmpSumhhTohhVP = AmpVertexhhTohhVP + AmpWavehhTohhVP
 AmpSum2hhTohhVP = AmpVertexhhTohhVP + AmpWavehhTohhVP 
End If 
Do gt1=1,2
i4 = isave 
  Do gt2=1,2
If (((OSkinematics).and.(MhhOS(gt1).gt.(MhhOS(gt2)+0.))).or.((.not.OSkinematics).and.(Mhh(gt1).gt.(Mhh(gt2)+MVP)))) Then 
If (OSkinematics) Then 
  Call SquareAmp_StoSV(MhhOS(gt1),MhhOS(gt2),0._dp,AmpSumhhTohhVP(:,gt1, gt2),AmpSum2hhTohhVP(:,gt1, gt2),AmpSqhhTohhVP(gt1, gt2)) 
Else  
  Call SquareAmp_StoSV(Mhh(gt1),Mhh(gt2),MVP,AmpSumhhTohhVP(:,gt1, gt2),AmpSum2hhTohhVP(:,gt1, gt2),AmpSqhhTohhVP(gt1, gt2)) 
End if  
Else  
  AmpSqhhTohhVP(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 1._dp 
If (AmpSqhhTohhVP(gt1, gt2).le.0._dp) Then 
  gP1Lhh(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(MhhOS(gt1),MhhOS(gt2),0._dp,helfactor*AmpSqhhTohhVP(gt1, gt2))
Else 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(Mhh(gt1),Mhh(gt2),MVP,helfactor*AmpSqhhTohhVP(gt1, gt2))
End if 
If ((Abs(MRPhhTohhVP(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGhhTohhVP(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1Lhh(gt1,i4) 
End if 
i4=i4+1

  End do
If (gt1.eq.2) isave = i4 
End do
!---------------- 
! hh VZ
!---------------- 

If (LoopInducedDecaysOS) Then 


!Self-energy Corrections 


!Vertex Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_VERTEX_SDdiracDM_hhTohhVZ(MFdOS,MFeOS,MFuOS,MFxvOS,MhhOS,              & 
& MHpOS,MVWpOS,MVZOS,MFd2OS,MFe2OS,MFu2OS,MFxv2OS,Mhh2OS,MHp2OS,MVWp2OS,MVZ2OS,          & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWphh,cplcgWpgWpVZ,     & 
& cplcgWCgWChh,cplcgWCgWCVZ,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,             & 
& cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplhhHpcVWpVZ1,cplhhcHpVWpVZ1,         & 
& AmpVertexhhTohhVZ)

 Else 
Call Amplitude_VERTEX_SDdiracDM_hhTohhVZ(MFdOS,MFeOS,MFuOS,MFxvOS,MhhOS,              & 
& MHpOS,MVWpOS,MVZOS,MFd2OS,MFe2OS,MFu2OS,MFxv2OS,Mhh2OS,MHp2OS,MVWp2OS,MVZ2OS,          & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWphh,cplcgWpgWpVZ,     & 
& cplcgWCgWChh,cplcgWCgWCVZ,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,             & 
& cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplhhHpcVWpVZ1,cplhhcHpVWpVZ1,         & 
& AmpVertexhhTohhVZ)

 End if 
Else 


!Self-energy Corrections 


!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_hhTohhVZ(MFd,MFe,MFu,MFxv,Mhh,MHp,MVWp,               & 
& MVZ,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MVWp2,MVZ2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,     & 
& cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,               & 
& cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,         & 
& cplcFxvFxvVZR,cplcgWpgWphh,cplcgWpgWpVZ,cplcgWCgWChh,cplcgWCgWCVZ,cplhhHpcHp,          & 
& cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,               & 
& cplcVWpVWpVZ,cplhhHpcVWpVZ1,cplhhcHpVWpVZ1,AmpVertexhhTohhVZ)

End if 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ hh->hh VZ -----------------------" 
End if 
If (OnlyTreeLevelContributions) Then 
 AmpSumhhTohhVZ = 0._dp 
 AmpSum2hhTohhVZ = 0._dp  
Else 
 AmpSumhhTohhVZ = AmpVertexhhTohhVZ + AmpWavehhTohhVZ
 AmpSum2hhTohhVZ = AmpVertexhhTohhVZ + AmpWavehhTohhVZ 
End If 
Do gt1=1,2
i4 = isave 
  Do gt2=1,2
If (((OSkinematics).and.(MhhOS(gt1).gt.(MhhOS(gt2)+MVZOS))).or.((.not.OSkinematics).and.(Mhh(gt1).gt.(Mhh(gt2)+MVZ)))) Then 
If (OSkinematics) Then 
  Call SquareAmp_StoSV(MhhOS(gt1),MhhOS(gt2),MVZOS,AmpSumhhTohhVZ(:,gt1, gt2),AmpSum2hhTohhVZ(:,gt1, gt2),AmpSqhhTohhVZ(gt1, gt2)) 
Else  
  Call SquareAmp_StoSV(Mhh(gt1),Mhh(gt2),MVZ,AmpSumhhTohhVZ(:,gt1, gt2),AmpSum2hhTohhVZ(:,gt1, gt2),AmpSqhhTohhVZ(gt1, gt2)) 
End if  
Else  
  AmpSqhhTohhVZ(gt1, gt2) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 1._dp 
If (AmpSqhhTohhVZ(gt1, gt2).le.0._dp) Then 
  gP1Lhh(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(MhhOS(gt1),MhhOS(gt2),MVZOS,helfactor*AmpSqhhTohhVZ(gt1, gt2))
Else 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(Mhh(gt1),Mhh(gt2),MVZ,helfactor*AmpSqhhTohhVZ(gt1, gt2))
End if 
If ((Abs(MRPhhTohhVZ(gt1, gt2)).gt.1.0E-20_dp).or.(Abs(MRGhhTohhVZ(gt1, gt2)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1Lhh(gt1,i4) 
End if 
i4=i4+1

  End do
If (gt1.eq.2) isave = i4 
End do
!---------------- 
! VG VG
!---------------- 

If (LoopInducedDecaysOS) Then 


!Self-energy Corrections 


!Vertex Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_VERTEX_SDdiracDM_hhToVGVG(MFdOS,MFuOS,MhhOS,MVG,MFd2OS,MFu2OS,         & 
& Mhh2OS,MVG2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVGL,cplcFdFdVGR,cplcFuFuhhL,               & 
& cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,AmpVertexhhToVGVG)

 Else 
Call Amplitude_VERTEX_SDdiracDM_hhToVGVG(MFdOS,MFuOS,MhhOS,MVG,MFd2OS,MFu2OS,         & 
& Mhh2OS,MVG2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVGL,cplcFdFdVGR,cplcFuFuhhL,               & 
& cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,AmpVertexhhToVGVG)

 End if 
Else 


!Self-energy Corrections 


!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_hhToVGVG(MFd,MFu,Mhh,MVG,MFd2,MFu2,Mhh2,              & 
& MVG2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVGL,cplcFdFdVGR,cplcFuFuhhL,cplcFuFuhhR,          & 
& cplcFuFuVGL,cplcFuFuVGR,AmpVertexhhToVGVG)

End if 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ hh->VG VG -----------------------" 
End if 
If (OnlyTreeLevelContributions) Then 
 AmpSumhhToVGVG = 0._dp 
 AmpSum2hhToVGVG = 0._dp  
Else 
 AmpSumhhToVGVG = AmpVertexhhToVGVG + AmpWavehhToVGVG
 AmpSum2hhToVGVG = AmpVertexhhToVGVG + AmpWavehhToVGVG 
End If 
Do gt1=1,2
i4 = isave 
If (((OSkinematics).and.(MhhOS(gt1).gt.(0.+0.))).or.((.not.OSkinematics).and.(Mhh(gt1).gt.(MVG+MVG)))) Then 
If (OSkinematics) Then 
  Call SquareAmp_StoVV(MhhOS(gt1),0._dp,0._dp,AmpSumhhToVGVG(:,gt1),AmpSum2hhToVGVG(:,gt1),AmpSqhhToVGVG(gt1)) 
Else  
  Call SquareAmp_StoVV(Mhh(gt1),MVG,MVG,AmpSumhhToVGVG(:,gt1),AmpSum2hhToVGVG(:,gt1),AmpSqhhToVGVG(gt1)) 
End if  
Else  
  AmpSqhhToVGVG(gt1) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 1._dp 
If (AmpSqhhToVGVG(gt1).le.0._dp) Then 
  gP1Lhh(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1Lhh(gt1,i4) = 8._dp*GammaTPS(MhhOS(gt1),0._dp,0._dp,helfactor*AmpSqhhToVGVG(gt1))
Else 
  gP1Lhh(gt1,i4) = 8._dp*GammaTPS(Mhh(gt1),MVG,MVG,helfactor*AmpSqhhToVGVG(gt1))
End if 
If ((Abs(MRPhhToVGVG(gt1)).gt.1.0E-20_dp).or.(Abs(MRGhhToVGVG(gt1)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1Lhh(gt1,i4) 
End if 
i4=i4+1

If (gt1.eq.2) isave = i4 
End do
!---------------- 
! VP VP
!---------------- 

If (LoopInducedDecaysOS) Then 


!Self-energy Corrections 


!Vertex Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_VERTEX_SDdiracDM_hhToVPVP(MFdOS,MFeOS,MFuOS,MhhOS,MHpOS,               & 
& MVP,MVWpOS,MFd2OS,MFe2OS,MFu2OS,Mhh2OS,MHp2OS,MVP2,MVWp2OS,cplcFdFdhhL,cplcFdFdhhR,    & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,               & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVPL,cplcFuFuVPR,cplcgWpgWphh,cplcgWpgWpVP,             & 
& cplcgWCgWChh,cplcgWCgWCVP,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,             & 
& cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,cplhhHpcVWpVP1,cplhhcHpVPVWp1,         & 
& cplHpcHpVPVP1,cplcVWpVPVPVWp3Q,cplcVWpVPVPVWp1Q,cplcVWpVPVPVWp2Q,AmpVertexhhToVPVP)

 Else 
Call Amplitude_VERTEX_SDdiracDM_hhToVPVP(MFdOS,MFeOS,MFuOS,MhhOS,MHpOS,               & 
& MVP,MVWpOS,MFd2OS,MFe2OS,MFu2OS,Mhh2OS,MHp2OS,MVP2,MVWp2OS,cplcFdFdhhL,cplcFdFdhhR,    & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,               & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVPL,cplcFuFuVPR,cplcgWpgWphh,cplcgWpgWpVP,             & 
& cplcgWCgWChh,cplcgWCgWCVP,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,             & 
& cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,cplhhHpcVWpVP1,cplhhcHpVPVWp1,         & 
& cplHpcHpVPVP1,cplcVWpVPVPVWp3Q,cplcVWpVPVPVWp1Q,cplcVWpVPVPVWp2Q,AmpVertexhhToVPVP)

 End if 
Else 


!Self-energy Corrections 


!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_hhToVPVP(MFd,MFe,MFu,Mhh,MHp,MVP,MVWp,MFd2,           & 
& MFe2,MFu2,Mhh2,MHp2,MVP2,MVWp2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVPL,cplcFdFdVPR,        & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuhhL,cplcFuFuhhR,               & 
& cplcFuFuVPL,cplcFuFuVPR,cplcgWpgWphh,cplcgWpgWpVP,cplcgWCgWChh,cplcgWCgWCVP,           & 
& cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,    & 
& cplcVWpVPVWp,cplhhHpcVWpVP1,cplhhcHpVPVWp1,cplHpcHpVPVP1,cplcVWpVPVPVWp3Q,             & 
& cplcVWpVPVPVWp1Q,cplcVWpVPVPVWp2Q,AmpVertexhhToVPVP)

End if 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ hh->VP VP -----------------------" 
End if 
If (OnlyTreeLevelContributions) Then 
 AmpSumhhToVPVP = 0._dp 
 AmpSum2hhToVPVP = 0._dp  
Else 
 AmpSumhhToVPVP = AmpVertexhhToVPVP + AmpWavehhToVPVP
 AmpSum2hhToVPVP = AmpVertexhhToVPVP + AmpWavehhToVPVP 
End If 
Do gt1=1,2
i4 = isave 
If (((OSkinematics).and.(MhhOS(gt1).gt.(0.+0.))).or.((.not.OSkinematics).and.(Mhh(gt1).gt.(MVP+MVP)))) Then 
If (OSkinematics) Then 
  Call SquareAmp_StoVV(MhhOS(gt1),0._dp,0._dp,AmpSumhhToVPVP(:,gt1),AmpSum2hhToVPVP(:,gt1),AmpSqhhToVPVP(gt1)) 
Else  
  Call SquareAmp_StoVV(Mhh(gt1),MVP,MVP,AmpSumhhToVPVP(:,gt1),AmpSum2hhToVPVP(:,gt1),AmpSqhhToVPVP(gt1)) 
End if  
Else  
  AmpSqhhToVPVP(gt1) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 1._dp 
If (AmpSqhhToVPVP(gt1).le.0._dp) Then 
  gP1Lhh(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(MhhOS(gt1),0._dp,0._dp,helfactor*AmpSqhhToVPVP(gt1))
Else 
  gP1Lhh(gt1,i4) = 1._dp*GammaTPS(Mhh(gt1),MVP,MVP,helfactor*AmpSqhhToVPVP(gt1))
End if 
If ((Abs(MRPhhToVPVP(gt1)).gt.1.0E-20_dp).or.(Abs(MRGhhToVPVP(gt1)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1Lhh(gt1,i4) 
End if 
i4=i4+1

If (gt1.eq.2) isave = i4 
End do
!---------------- 
! VP VZ
!---------------- 

If (LoopInducedDecaysOS) Then 


!Self-energy Corrections 


!Vertex Corrections 
 If (ExternalZfactors) Then 
Call Amplitude_VERTEX_SDdiracDM_hhToVPVZ(MFdOS,MFeOS,MFuOS,MhhOS,MHpOS,               & 
& MVP,MVWpOS,MVZOS,MFd2OS,MFe2OS,MFu2OS,Mhh2OS,MHp2OS,MVP2,MVWp2OS,MVZ2OS,               & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,               & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,               & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcgWpgWphh,cplcgWpgWpVP,cplcgWpgWpVZ,cplcgWCgWChh,cplcgWCgWCVP,cplcgWCgWCVZ,         & 
& cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,     & 
& cplHpcVWpVZ,cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,cplhhHpcVWpVP1,          & 
& cplhhHpcVWpVZ1,cplhhcHpVPVWp1,cplhhcHpVWpVZ1,cplHpcHpVPVZ1,cplcVWpVPVWpVZ3Q,           & 
& cplcVWpVPVWpVZ2Q,cplcVWpVPVWpVZ1Q,AmpVertexhhToVPVZ)

 Else 
Call Amplitude_VERTEX_SDdiracDM_hhToVPVZ(MFdOS,MFeOS,MFuOS,MhhOS,MHpOS,               & 
& MVP,MVWpOS,MVZOS,MFd2OS,MFe2OS,MFu2OS,Mhh2OS,MHp2OS,MVP2,MVWp2OS,MVZ2OS,               & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,               & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,               & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcgWpgWphh,cplcgWpgWpVP,cplcgWpgWpVZ,cplcgWCgWChh,cplcgWCgWCVP,cplcgWCgWCVZ,         & 
& cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,     & 
& cplHpcVWpVZ,cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,cplhhHpcVWpVP1,          & 
& cplhhHpcVWpVZ1,cplhhcHpVPVWp1,cplhhcHpVWpVZ1,cplHpcHpVPVZ1,cplcVWpVPVWpVZ3Q,           & 
& cplcVWpVPVWpVZ2Q,cplcVWpVPVWpVZ1Q,AmpVertexhhToVPVZ)

 End if 
Else 


!Self-energy Corrections 


!Vertex Corrections 
Call Amplitude_VERTEX_SDdiracDM_hhToVPVZ(MFd,MFe,MFu,Mhh,MHp,MVP,MVWp,MVZ,            & 
& MFd2,MFe2,MFu2,Mhh2,MHp2,MVP2,MVWp2,MVZ2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVPL,          & 
& cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,               & 
& cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVPL,               & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcgWpgWphh,cplcgWpgWpVP,cplcgWpgWpVZ,            & 
& cplcgWCgWChh,cplcgWCgWCVP,cplcgWCgWCVZ,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,             & 
& cplhhcVWpVWp,cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVPVWp,cplcVWpVPVWp,   & 
& cplcHpVWpVZ,cplcVWpVWpVZ,cplhhHpcVWpVP1,cplhhHpcVWpVZ1,cplhhcHpVPVWp1,cplhhcHpVWpVZ1,  & 
& cplHpcHpVPVZ1,cplcVWpVPVWpVZ3Q,cplcVWpVPVWpVZ2Q,cplcVWpVPVWpVZ1Q,AmpVertexhhToVPVZ)

End if 


 !Square the amplitude 
If (DebugLoopDecays) Then 
Write(*,*) "------------------ hh->VP VZ -----------------------" 
End if 
If (OnlyTreeLevelContributions) Then 
 AmpSumhhToVPVZ = 0._dp 
 AmpSum2hhToVPVZ = 0._dp  
Else 
 AmpSumhhToVPVZ = AmpVertexhhToVPVZ + AmpWavehhToVPVZ
 AmpSum2hhToVPVZ = AmpVertexhhToVPVZ + AmpWavehhToVPVZ 
End If 
Do gt1=1,2
i4 = isave 
If (((OSkinematics).and.(MhhOS(gt1).gt.(0.+MVZOS))).or.((.not.OSkinematics).and.(Mhh(gt1).gt.(MVP+MVZ)))) Then 
If (OSkinematics) Then 
  Call SquareAmp_StoVV(MhhOS(gt1),0._dp,MVZOS,AmpSumhhToVPVZ(:,gt1),AmpSum2hhToVPVZ(:,gt1),AmpSqhhToVPVZ(gt1)) 
Else  
  Call SquareAmp_StoVV(Mhh(gt1),MVP,MVZ,AmpSumhhToVPVZ(:,gt1),AmpSum2hhToVPVZ(:,gt1),AmpSqhhToVPVZ(gt1)) 
End if  
Else  
  AmpSqhhToVPVZ(gt1) = 0._dp 
End if  

! Calculate Partial widths 
helfactor = 1._dp 
If (AmpSqhhToVPVZ(gt1).le.0._dp) Then 
  gP1Lhh(gt1,i4) = 0._dp 
Else 
If (OSkinematics) Then 
  gP1Lhh(gt1,i4) = 2._dp*GammaTPS(MhhOS(gt1),0._dp,MVZOS,helfactor*AmpSqhhToVPVZ(gt1))
Else 
  gP1Lhh(gt1,i4) = 2._dp*GammaTPS(Mhh(gt1),MVP,MVZ,helfactor*AmpSqhhToVPVZ(gt1))
End if 
If ((Abs(MRPhhToVPVZ(gt1)).gt.1.0E-20_dp).or.(Abs(MRGhhToVPVZ(gt1)).gt.1.0E-20_dp)) Then 
  phasespacefactor = 1._dp 
End if 
 If (DebugLoopDecays) Write(*,*) "virtual", gP1Lhh(gt1,i4) 
End if 
i4=i4+1

If (gt1.eq.2) isave = i4 
End do
End Subroutine OneLoopDecay_hh

End Module Wrapper_OneLoopDecay_hh_SDdiracDM
