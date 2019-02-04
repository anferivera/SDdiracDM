! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:22 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module OneLoopDecays_SDdiracDM 
Use Couplings_SDdiracDM 
Use CouplingsCT_SDdiracDM 
Use Model_Data_SDdiracDM 
Use LoopCouplings_SDdiracDM 
Use LoopMasses_SDdiracDM 
Use RGEs_SDdiracDM 
Use Tadpoles_SDdiracDM 
Use Kinematics 
Use CouplingsForDecays_SDdiracDM 
 
Use Wrapper_OneLoopDecay_Fu_SDdiracDM 
Use Wrapper_OneLoopDecay_Fe_SDdiracDM 
Use Wrapper_OneLoopDecay_Fd_SDdiracDM 
Use Wrapper_OneLoopDecay_hh_SDdiracDM 
Use Wrapper_OneLoopDecay_Ssc_SDdiracDM 
Use Wrapper_OneLoopDecay_Fxe_SDdiracDM 
Use Wrapper_OneLoopDecay_Fxv_SDdiracDM 

 
Contains 
 
Subroutine getZCouplings(Lam,LSPH,vvSM,vS,ZH,LSP,LS1H,LS2H,VSs,LS,g1,g2,              & 
& TW,g3,Yd,ZDL,ZDR,Ye,ZEL,ZER,Yu,ZUL,ZUR,YRD,XV,XU,UV,YRA1,YRA2,YRB1,YRB2,               & 
& UVR,YRC,cplAhAhhh,cplhhhhhh,cplhhHpcHp,cplhhSsccSsc,cplAhAhAhAh,cplAhAhhhhh,           & 
& cplAhAhHpcHp,cplAhAhSsccSsc,cplhhhhhhhh,cplhhhhHpcHp,cplhhhhSsccSsc,cplHpHpcHpcHp,     & 
& cplHpSsccHpcSsc,cplSscSsccSsccSsc,cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,cplhhHpcVWp,       & 
& cplhhcHpVWp,cplHpcHpVP,cplHpcHpVZ,cplhhcVWpVWp,cplhhVZVZ,cplHpcVWpVP,cplHpcVWpVZ,      & 
& cplcHpVPVWp,cplcHpVWpVZ,cplAhAhcVWpVWp,cplAhAhVZVZ,cplAhHpcVWpVP,cplAhHpcVWpVZ,        & 
& cplAhcHpVPVWp,cplAhcHpVWpVZ,cplhhhhcVWpVWp,cplhhhhVZVZ,cplhhHpcVWpVP,cplhhHpcVWpVZ,    & 
& cplhhcHpVPVWp,cplhhcHpVWpVZ,cplHpcHpVPVP,cplHpcHpVPVZ,cplHpcHpcVWpVWp,cplHpcHpVZVZ,    & 
& cplVGVGVG,cplcVWpVPVWp,cplcVWpVWpVZ,cplcFdFdAhL,cplcFdFdAhR,cplcFeFeAhL,               & 
& cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFdFdhhL,           & 
& cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,               & 
& cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucHpL,          & 
& cplcFdFucHpR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFxvFxeHpL,      & 
& cplcFxvFxeHpR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,               & 
& cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFdFdVGL,               & 
& cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,             & 
& cplcFdFdVZR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,             & 
& cplcFeFeVZR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,               & 
& cplcFuFuVZR,cplcFdFucVWpL,cplcFdFucVWpR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcVWpL,         & 
& cplcFeFvcVWpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,               & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFxeFxvcVWpL,               & 
& cplcFxeFxvcVWpR,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,& 
& cplcVWpVPVPVWp3,cplcVWpVPVWpVZ1,cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpcVWpVWpVWp1,    & 
& cplcVWpcVWpVWpVWp2,cplcVWpcVWpVWpVWp3,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3, & 
& cplcgGgGVG,cplcgWpgAVWp,cplcgWCgAcVWp,cplcgWpgWpVP,cplcgWpgWpVZ,cplcgAgWpcVWp,         & 
& cplcgZgWpcVWp,cplcgWCgWCVP,cplcgAgWCVWp,cplcgZgWCVWp,cplcgWCgWCVZ,cplcgWpgZVWp,        & 
& cplcgWCgZcVWp,cplcgWpgWpAh,cplcgWCgWCAh,cplcgZgAhh,cplcgWpgAHp,cplcgWCgAcHp,           & 
& cplcgWpgWphh,cplcgZgWpcHp,cplcgWCgWChh,cplcgZgWCHp,cplcgZgZhh,cplcgWpgZHp,             & 
& cplcgWCgZcHp,ZRUZH,ZRUVd,ZRUUd,ZRUVu,ZRUUu,ZRUVe,ZRUUe,ZRUVv,ZRUVvr,ZRUXV,             & 
& ZRUXU,ZRUVSs,ZcplAhAhhh,Zcplhhhhhh,ZcplhhHpcHp,ZcplhhSsccSsc,ZcplAhAhAhAh,             & 
& ZcplAhAhhhhh,ZcplAhAhHpcHp,ZcplAhAhSsccSsc,Zcplhhhhhhhh,ZcplhhhhHpcHp,ZcplhhhhSsccSsc, & 
& ZcplHpHpcHpcHp,ZcplHpSsccHpcSsc,ZcplSscSsccSsccSsc,ZcplAhhhVZ,ZcplAhHpcVWp,            & 
& ZcplAhcHpVWp,ZcplhhHpcVWp,ZcplhhcHpVWp,ZcplHpcHpVP,ZcplHpcHpVZ,ZcplhhcVWpVWp,          & 
& ZcplhhVZVZ,ZcplHpcVWpVP,ZcplHpcVWpVZ,ZcplcHpVPVWp,ZcplcHpVWpVZ,ZcplAhAhcVWpVWp,        & 
& ZcplAhAhVZVZ,ZcplAhHpcVWpVP,ZcplAhHpcVWpVZ,ZcplAhcHpVPVWp,ZcplAhcHpVWpVZ,              & 
& ZcplhhhhcVWpVWp,ZcplhhhhVZVZ,ZcplhhHpcVWpVP,ZcplhhHpcVWpVZ,ZcplhhcHpVPVWp,             & 
& ZcplhhcHpVWpVZ,ZcplHpcHpVPVP,ZcplHpcHpVPVZ,ZcplHpcHpcVWpVWp,ZcplHpcHpVZVZ,             & 
& ZcplVGVGVG,ZcplcVWpVPVWp,ZcplcVWpVWpVZ,ZcplcFdFdAhL,ZcplcFdFdAhR,ZcplcFeFeAhL,         & 
& ZcplcFeFeAhR,ZcplcFuFuAhL,ZcplcFuFuAhR,ZcplcFxvFxvAhL,ZcplcFxvFxvAhR,ZcplcFdFdhhL,     & 
& ZcplcFdFdhhR,ZcplcFuFdHpL,ZcplcFuFdHpR,ZcplcFeFehhL,ZcplcFeFehhR,ZcplcFvFeHpL,         & 
& ZcplcFvFeHpR,ZcplcFxeFeSscL,ZcplcFxeFeSscR,ZcplcFuFuhhL,ZcplcFuFuhhR,ZcplcFdFucHpL,    & 
& ZcplcFdFucHpR,ZcplcFxvFvSscL,ZcplcFxvFvSscR,ZcplcFeFvcHpL,ZcplcFeFvcHpR,               & 
& ZcplcFxvFxeHpL,ZcplcFxvFxeHpR,ZcplcFeFxecSscL,ZcplcFeFxecSscR,ZcplcFxvFxvhhL,          & 
& ZcplcFxvFxvhhR,ZcplcFvFxvcSscL,ZcplcFvFxvcSscR,ZcplcFxeFxvcHpL,ZcplcFxeFxvcHpR,        & 
& ZcplcFdFdVGL,ZcplcFdFdVGR,ZcplcFdFdVPL,ZcplcFdFdVPR,ZcplcFuFdVWpL,ZcplcFuFdVWpR,       & 
& ZcplcFdFdVZL,ZcplcFdFdVZR,ZcplcFeFeVPL,ZcplcFeFeVPR,ZcplcFvFeVWpL,ZcplcFvFeVWpR,       & 
& ZcplcFeFeVZL,ZcplcFeFeVZR,ZcplcFuFuVGL,ZcplcFuFuVGR,ZcplcFuFuVPL,ZcplcFuFuVPR,         & 
& ZcplcFuFuVZL,ZcplcFuFuVZR,ZcplcFdFucVWpL,ZcplcFdFucVWpR,ZcplcFvFvVZL,ZcplcFvFvVZR,     & 
& ZcplcFeFvcVWpL,ZcplcFeFvcVWpR,ZcplcFxeFxeVPL,ZcplcFxeFxeVPR,ZcplcFxvFxeVWpL,           & 
& ZcplcFxvFxeVWpR,ZcplcFxeFxeVZL,ZcplcFxeFxeVZR,ZcplcFxvFxvVZL,ZcplcFxvFxvVZR,           & 
& ZcplcFxeFxvcVWpL,ZcplcFxeFxvcVWpR,ZcplVGVGVGVG1,ZcplVGVGVGVG2,ZcplVGVGVGVG3,           & 
& ZcplcVWpVPVPVWp1,ZcplcVWpVPVPVWp2,ZcplcVWpVPVPVWp3,ZcplcVWpVPVWpVZ1,ZcplcVWpVPVWpVZ2,  & 
& ZcplcVWpVPVWpVZ3,ZcplcVWpcVWpVWpVWp1,ZcplcVWpcVWpVWpVWp2,ZcplcVWpcVWpVWpVWp3,          & 
& ZcplcVWpVWpVZVZ1,ZcplcVWpVWpVZVZ2,ZcplcVWpVWpVZVZ3,ZcplcgGgGVG,ZcplcgWpgAVWp,          & 
& ZcplcgWCgAcVWp,ZcplcgWpgWpVP,ZcplcgWpgWpVZ,ZcplcgAgWpcVWp,ZcplcgZgWpcVWp,              & 
& ZcplcgWCgWCVP,ZcplcgAgWCVWp,ZcplcgZgWCVWp,ZcplcgWCgWCVZ,ZcplcgWpgZVWp,ZcplcgWCgZcVWp,  & 
& ZcplcgWpgWpAh,ZcplcgWCgWCAh,ZcplcgZgAhh,ZcplcgWpgAHp,ZcplcgWCgAcHp,ZcplcgWpgWphh,      & 
& ZcplcgZgWpcHp,ZcplcgWCgWChh,ZcplcgZgWCHp,ZcplcgZgZhh,ZcplcgWpgZHp,ZcplcgWCgZcHp)

Implicit None

Real(dp), Intent(in) :: LSPH,vvSM,vS,ZH(2,2),LSP,LS1H,LS2H,VSs(2,2),LS,g1,g2,TW,g3,YRD,YRA1(3),               & 
& YRA2(3),YRB1(3),YRB2(3),YRC

Complex(dp), Intent(in) :: Lam,Yd(3,3),ZDL(3,3),ZDR(3,3),Ye(3,3),ZEL(3,3),ZER(3,3),Yu(3,3),ZUL(3,3),             & 
& ZUR(3,3),XV(2,2),XU(2,2),UV(3,3),UVR(3,3)

Complex(dp), Intent(in) :: cplAhAhhh(2),cplhhhhhh(2,2,2),cplhhHpcHp(2),cplhhSsccSsc(2,2,2),cplAhAhAhAh,          & 
& cplAhAhhhhh(2,2),cplAhAhHpcHp,cplAhAhSsccSsc(2,2),cplhhhhhhhh(2,2,2,2),cplhhhhHpcHp(2,2),& 
& cplhhhhSsccSsc(2,2,2,2),cplHpHpcHpcHp,cplHpSsccHpcSsc(2,2),cplSscSsccSsccSsc(2,2,2,2), & 
& cplAhhhVZ(2),cplAhHpcVWp,cplAhcHpVWp,cplhhHpcVWp(2),cplhhcHpVWp(2),cplHpcHpVP,         & 
& cplHpcHpVZ,cplhhcVWpVWp(2),cplhhVZVZ(2),cplHpcVWpVP,cplHpcVWpVZ,cplcHpVPVWp,           & 
& cplcHpVWpVZ,cplAhAhcVWpVWp,cplAhAhVZVZ,cplAhHpcVWpVP,cplAhHpcVWpVZ,cplAhcHpVPVWp,      & 
& cplAhcHpVWpVZ,cplhhhhcVWpVWp(2,2),cplhhhhVZVZ(2,2),cplhhHpcVWpVP(2),cplhhHpcVWpVZ(2),  & 
& cplhhcHpVPVWp(2),cplhhcHpVWpVZ(2),cplHpcHpVPVP,cplHpcHpVPVZ,cplHpcHpcVWpVWp,           & 
& cplHpcHpVZVZ,cplVGVGVG,cplcVWpVPVWp,cplcVWpVWpVZ,cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),    & 
& cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFxvFxvAhL(2,2),& 
& cplcFxvFxvAhR(2,2),cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFuFdHpL(3,3),             & 
& cplcFuFdHpR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFvFeHpL(3,3),               & 
& cplcFvFeHpR(3,3),cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFuFuhhL(3,3,2),             & 
& cplcFuFuhhR(3,3,2),cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFxvFvSscL(2,3,2),           & 
& cplcFxvFvSscR(2,3,2),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFxvFxeHpL(2),             & 
& cplcFxvFxeHpR(2),cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFxvFxvhhL(2,2,2),         & 
& cplcFxvFxvhhR(2,2,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),cplcFxeFxvcHpL(2),    & 
& cplcFxeFxvcHpR(2),cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3), & 
& cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVPL(3,3),& 
& cplcFeFeVPR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),& 
& cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),  & 
& cplcFuFuVZR(3,3),cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),cplcFvFvVZL(3,3),               & 
& cplcFvFvVZR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFxeFxeVPL,cplcFxeFxeVPR,    & 
& cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvVZL(2,2),    & 
& cplcFxvFxvVZR(2,2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplVGVGVGVG1,cplVGVGVGVG2,    & 
& cplVGVGVGVG3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,cplcVWpVPVPVWp3,cplcVWpVPVWpVZ1,          & 
& cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpcVWpVWpVWp1,cplcVWpcVWpVWpVWp2,cplcVWpcVWpVWpVWp3,& 
& cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,cplcgGgGVG,cplcgWpgAVWp,               & 
& cplcgWCgAcVWp,cplcgWpgWpVP,cplcgWpgWpVZ,cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgWCVP,      & 
& cplcgAgWCVWp,cplcgZgWCVWp,cplcgWCgWCVZ,cplcgWpgZVWp,cplcgWCgZcVWp,cplcgWpgWpAh,        & 
& cplcgWCgWCAh,cplcgZgAhh(2),cplcgWpgAHp,cplcgWCgAcHp,cplcgWpgWphh(2),cplcgZgWpcHp,      & 
& cplcgWCgWChh(2),cplcgZgWCHp,cplcgZgZhh(2),cplcgWpgZHp,cplcgWCgZcHp

Complex(dp), Intent(in) :: ZRUZH(2,2),ZRUVd(3,3),ZRUUd(3,3),ZRUVu(3,3),ZRUUu(3,3),ZRUVe(3,3),ZRUUe(3,3),         & 
& ZRUVv(3,3),ZRUVvr(3,3),ZRUXV(2,2),ZRUXU(2,2),ZRUVSs(2,2)

Integer :: gt1, gt2
Complex(dp) :: TempcplAhAhhh(2),Tempcplhhhhhh(2,2,2),TempcplhhHpcHp(2),TempcplhhSsccSsc(2,2,2),      & 
& TempcplAhAhAhAh,TempcplAhAhhhhh(2,2),TempcplAhAhHpcHp,TempcplAhAhSsccSsc(2,2),         & 
& Tempcplhhhhhhhh(2,2,2,2),TempcplhhhhHpcHp(2,2),TempcplhhhhSsccSsc(2,2,2,2),            & 
& TempcplHpHpcHpcHp,TempcplHpSsccHpcSsc(2,2),TempcplSscSsccSsccSsc(2,2,2,2),             & 
& TempcplAhhhVZ(2),TempcplAhHpcVWp,TempcplAhcHpVWp,TempcplhhHpcVWp(2),TempcplhhcHpVWp(2),& 
& TempcplHpcHpVP,TempcplHpcHpVZ,TempcplhhcVWpVWp(2),TempcplhhVZVZ(2),TempcplHpcVWpVP,    & 
& TempcplHpcVWpVZ,TempcplcHpVPVWp,TempcplcHpVWpVZ,TempcplAhAhcVWpVWp,TempcplAhAhVZVZ,    & 
& TempcplAhHpcVWpVP,TempcplAhHpcVWpVZ,TempcplAhcHpVPVWp,TempcplAhcHpVWpVZ,               & 
& TempcplhhhhcVWpVWp(2,2),TempcplhhhhVZVZ(2,2),TempcplhhHpcVWpVP(2),TempcplhhHpcVWpVZ(2),& 
& TempcplhhcHpVPVWp(2),TempcplhhcHpVWpVZ(2),TempcplHpcHpVPVP,TempcplHpcHpVPVZ,           & 
& TempcplHpcHpcVWpVWp,TempcplHpcHpVZVZ,TempcplVGVGVG,TempcplcVWpVPVWp,TempcplcVWpVWpVZ,  & 
& TempcplcFdFdAhL(3,3),TempcplcFdFdAhR(3,3),TempcplcFeFeAhL(3,3),TempcplcFeFeAhR(3,3),   & 
& TempcplcFuFuAhL(3,3),TempcplcFuFuAhR(3,3),TempcplcFxvFxvAhL(2,2),TempcplcFxvFxvAhR(2,2),& 
& TempcplcFdFdhhL(3,3,2),TempcplcFdFdhhR(3,3,2),TempcplcFuFdHpL(3,3),TempcplcFuFdHpR(3,3),& 
& TempcplcFeFehhL(3,3,2),TempcplcFeFehhR(3,3,2),TempcplcFvFeHpL(3,3),TempcplcFvFeHpR(3,3),& 
& TempcplcFxeFeSscL(3,2),TempcplcFxeFeSscR(3,2),TempcplcFuFuhhL(3,3,2),TempcplcFuFuhhR(3,3,2),& 
& TempcplcFdFucHpL(3,3),TempcplcFdFucHpR(3,3),TempcplcFxvFvSscL(2,3,2),TempcplcFxvFvSscR(2,3,2),& 
& TempcplcFeFvcHpL(3,3),TempcplcFeFvcHpR(3,3),TempcplcFxvFxeHpL(2),TempcplcFxvFxeHpR(2), & 
& TempcplcFeFxecSscL(3,2),TempcplcFeFxecSscR(3,2),TempcplcFxvFxvhhL(2,2,2),              & 
& TempcplcFxvFxvhhR(2,2,2),TempcplcFvFxvcSscL(3,2,2),TempcplcFvFxvcSscR(3,2,2),          & 
& TempcplcFxeFxvcHpL(2),TempcplcFxeFxvcHpR(2),TempcplcFdFdVGL(3,3),TempcplcFdFdVGR(3,3), & 
& TempcplcFdFdVPL(3,3),TempcplcFdFdVPR(3,3),TempcplcFuFdVWpL(3,3),TempcplcFuFdVWpR(3,3), & 
& TempcplcFdFdVZL(3,3),TempcplcFdFdVZR(3,3),TempcplcFeFeVPL(3,3),TempcplcFeFeVPR(3,3),   & 
& TempcplcFvFeVWpL(3,3),TempcplcFvFeVWpR(3,3),TempcplcFeFeVZL(3,3),TempcplcFeFeVZR(3,3), & 
& TempcplcFuFuVGL(3,3),TempcplcFuFuVGR(3,3),TempcplcFuFuVPL(3,3),TempcplcFuFuVPR(3,3),   & 
& TempcplcFuFuVZL(3,3),TempcplcFuFuVZR(3,3),TempcplcFdFucVWpL(3,3),TempcplcFdFucVWpR(3,3),& 
& TempcplcFvFvVZL(3,3),TempcplcFvFvVZR(3,3),TempcplcFeFvcVWpL(3,3),TempcplcFeFvcVWpR(3,3),& 
& TempcplcFxeFxeVPL,TempcplcFxeFxeVPR,TempcplcFxvFxeVWpL(2),TempcplcFxvFxeVWpR(2),       & 
& TempcplcFxeFxeVZL,TempcplcFxeFxeVZR,TempcplcFxvFxvVZL(2,2),TempcplcFxvFxvVZR(2,2),     & 
& TempcplcFxeFxvcVWpL(2),TempcplcFxeFxvcVWpR(2),TempcplVGVGVGVG1,TempcplVGVGVGVG2,       & 
& TempcplVGVGVGVG3,TempcplcVWpVPVPVWp1,TempcplcVWpVPVPVWp2,TempcplcVWpVPVPVWp3,          & 
& TempcplcVWpVPVWpVZ1,TempcplcVWpVPVWpVZ2,TempcplcVWpVPVWpVZ3,TempcplcVWpcVWpVWpVWp1,    & 
& TempcplcVWpcVWpVWpVWp2,TempcplcVWpcVWpVWpVWp3,TempcplcVWpVWpVZVZ1,TempcplcVWpVWpVZVZ2, & 
& TempcplcVWpVWpVZVZ3,TempcplcgGgGVG,TempcplcgWpgAVWp,TempcplcgWCgAcVWp,TempcplcgWpgWpVP,& 
& TempcplcgWpgWpVZ,TempcplcgAgWpcVWp,TempcplcgZgWpcVWp,TempcplcgWCgWCVP,TempcplcgAgWCVWp,& 
& TempcplcgZgWCVWp,TempcplcgWCgWCVZ,TempcplcgWpgZVWp,TempcplcgWCgZcVWp,TempcplcgWpgWpAh, & 
& TempcplcgWCgWCAh,TempcplcgZgAhh(2),TempcplcgWpgAHp,TempcplcgWCgAcHp,TempcplcgWpgWphh(2)

Complex(dp) :: TempcplcgZgWpcHp,TempcplcgWCgWChh(2),TempcplcgZgWCHp,TempcplcgZgZhh(2),TempcplcgWpgZHp,& 
& TempcplcgWCgZcHp

Complex(dp), Intent(out) :: ZcplAhAhhh(2),Zcplhhhhhh(2,2,2),ZcplhhHpcHp(2),ZcplhhSsccSsc(2,2,2),ZcplAhAhAhAh,     & 
& ZcplAhAhhhhh(2,2),ZcplAhAhHpcHp,ZcplAhAhSsccSsc(2,2),Zcplhhhhhhhh(2,2,2,2),            & 
& ZcplhhhhHpcHp(2,2),ZcplhhhhSsccSsc(2,2,2,2),ZcplHpHpcHpcHp,ZcplHpSsccHpcSsc(2,2),      & 
& ZcplSscSsccSsccSsc(2,2,2,2),ZcplAhhhVZ(2),ZcplAhHpcVWp,ZcplAhcHpVWp,ZcplhhHpcVWp(2),   & 
& ZcplhhcHpVWp(2),ZcplHpcHpVP,ZcplHpcHpVZ,ZcplhhcVWpVWp(2),ZcplhhVZVZ(2),ZcplHpcVWpVP,   & 
& ZcplHpcVWpVZ,ZcplcHpVPVWp,ZcplcHpVWpVZ,ZcplAhAhcVWpVWp,ZcplAhAhVZVZ,ZcplAhHpcVWpVP,    & 
& ZcplAhHpcVWpVZ,ZcplAhcHpVPVWp,ZcplAhcHpVWpVZ,ZcplhhhhcVWpVWp(2,2),ZcplhhhhVZVZ(2,2),   & 
& ZcplhhHpcVWpVP(2),ZcplhhHpcVWpVZ(2),ZcplhhcHpVPVWp(2),ZcplhhcHpVWpVZ(2),               & 
& ZcplHpcHpVPVP,ZcplHpcHpVPVZ,ZcplHpcHpcVWpVWp,ZcplHpcHpVZVZ,ZcplVGVGVG,ZcplcVWpVPVWp,   & 
& ZcplcVWpVWpVZ,ZcplcFdFdAhL(3,3),ZcplcFdFdAhR(3,3),ZcplcFeFeAhL(3,3),ZcplcFeFeAhR(3,3), & 
& ZcplcFuFuAhL(3,3),ZcplcFuFuAhR(3,3),ZcplcFxvFxvAhL(2,2),ZcplcFxvFxvAhR(2,2),           & 
& ZcplcFdFdhhL(3,3,2),ZcplcFdFdhhR(3,3,2),ZcplcFuFdHpL(3,3),ZcplcFuFdHpR(3,3),           & 
& ZcplcFeFehhL(3,3,2),ZcplcFeFehhR(3,3,2),ZcplcFvFeHpL(3,3),ZcplcFvFeHpR(3,3),           & 
& ZcplcFxeFeSscL(3,2),ZcplcFxeFeSscR(3,2),ZcplcFuFuhhL(3,3,2),ZcplcFuFuhhR(3,3,2),       & 
& ZcplcFdFucHpL(3,3),ZcplcFdFucHpR(3,3),ZcplcFxvFvSscL(2,3,2),ZcplcFxvFvSscR(2,3,2),     & 
& ZcplcFeFvcHpL(3,3),ZcplcFeFvcHpR(3,3),ZcplcFxvFxeHpL(2),ZcplcFxvFxeHpR(2),             & 
& ZcplcFeFxecSscL(3,2),ZcplcFeFxecSscR(3,2),ZcplcFxvFxvhhL(2,2,2),ZcplcFxvFxvhhR(2,2,2), & 
& ZcplcFvFxvcSscL(3,2,2),ZcplcFvFxvcSscR(3,2,2),ZcplcFxeFxvcHpL(2),ZcplcFxeFxvcHpR(2),   & 
& ZcplcFdFdVGL(3,3),ZcplcFdFdVGR(3,3),ZcplcFdFdVPL(3,3),ZcplcFdFdVPR(3,3),               & 
& ZcplcFuFdVWpL(3,3),ZcplcFuFdVWpR(3,3),ZcplcFdFdVZL(3,3),ZcplcFdFdVZR(3,3),             & 
& ZcplcFeFeVPL(3,3),ZcplcFeFeVPR(3,3),ZcplcFvFeVWpL(3,3),ZcplcFvFeVWpR(3,3),             & 
& ZcplcFeFeVZL(3,3),ZcplcFeFeVZR(3,3),ZcplcFuFuVGL(3,3),ZcplcFuFuVGR(3,3),               & 
& ZcplcFuFuVPL(3,3),ZcplcFuFuVPR(3,3),ZcplcFuFuVZL(3,3),ZcplcFuFuVZR(3,3),               & 
& ZcplcFdFucVWpL(3,3),ZcplcFdFucVWpR(3,3),ZcplcFvFvVZL(3,3),ZcplcFvFvVZR(3,3),           & 
& ZcplcFeFvcVWpL(3,3),ZcplcFeFvcVWpR(3,3),ZcplcFxeFxeVPL,ZcplcFxeFxeVPR,ZcplcFxvFxeVWpL(2),& 
& ZcplcFxvFxeVWpR(2),ZcplcFxeFxeVZL,ZcplcFxeFxeVZR,ZcplcFxvFxvVZL(2,2),ZcplcFxvFxvVZR(2,2),& 
& ZcplcFxeFxvcVWpL(2),ZcplcFxeFxvcVWpR(2),ZcplVGVGVGVG1,ZcplVGVGVGVG2,ZcplVGVGVGVG3,     & 
& ZcplcVWpVPVPVWp1,ZcplcVWpVPVPVWp2,ZcplcVWpVPVPVWp3,ZcplcVWpVPVWpVZ1,ZcplcVWpVPVWpVZ2,  & 
& ZcplcVWpVPVWpVZ3,ZcplcVWpcVWpVWpVWp1,ZcplcVWpcVWpVWpVWp2,ZcplcVWpcVWpVWpVWp3,          & 
& ZcplcVWpVWpVZVZ1,ZcplcVWpVWpVZVZ2,ZcplcVWpVWpVZVZ3,ZcplcgGgGVG,ZcplcgWpgAVWp,          & 
& ZcplcgWCgAcVWp,ZcplcgWpgWpVP,ZcplcgWpgWpVZ,ZcplcgAgWpcVWp,ZcplcgZgWpcVWp,              & 
& ZcplcgWCgWCVP,ZcplcgAgWCVWp,ZcplcgZgWCVWp,ZcplcgWCgWCVZ,ZcplcgWpgZVWp,ZcplcgWCgZcVWp,  & 
& ZcplcgWpgWpAh,ZcplcgWCgWCAh,ZcplcgZgAhh(2),ZcplcgWpgAHp,ZcplcgWCgAcHp,ZcplcgWpgWphh(2),& 
& ZcplcgZgWpcHp,ZcplcgWCgWChh(2),ZcplcgZgWCHp,ZcplcgZgZhh(2),ZcplcgWpgZHp,               & 
& ZcplcgWCgZcHp

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


 ! ## ZcplAhAhhh ## 
ZcplAhAhhh = 0._dp 
TempcplAhAhhh = cplAhAhhh 
ZcplAhAhhh = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplAhAhhh(gt2) = ZcplAhAhhh(gt2) + ZRUZH(gt2,gt1)*TempcplAhAhhh(gt1) 
 End Do 
End Do 


 ! ## Zcplhhhhhh ## 
Zcplhhhhhh = 0._dp 
Tempcplhhhhhh = cplhhhhhh 
Do gt1=1,2
  Do gt2=1,2
Zcplhhhhhh(gt2,:,:) = Zcplhhhhhh(gt2,:,:) + ZRUZH(gt2,gt1)*Tempcplhhhhhh(gt1,:,:) 
 End Do 
End Do 
Tempcplhhhhhh = Zcplhhhhhh 
Zcplhhhhhh = 0._dp 
Do gt1=1,2
  Do gt2=1,2
Zcplhhhhhh(:,gt2,:) = Zcplhhhhhh(:,gt2,:) + ZRUZH(gt2,gt1)*Tempcplhhhhhh(:,gt1,:) 
 End Do 
End Do 
Tempcplhhhhhh = Zcplhhhhhh 
Zcplhhhhhh = 0._dp 
Do gt1=1,2
  Do gt2=1,2
Zcplhhhhhh(:,:,gt2) = Zcplhhhhhh(:,:,gt2) + ZRUZH(gt2,gt1)*Tempcplhhhhhh(:,:,gt1) 
 End Do 
End Do 


 ! ## ZcplhhHpcHp ## 
ZcplhhHpcHp = 0._dp 
TempcplhhHpcHp = cplhhHpcHp 
Do gt1=1,2
  Do gt2=1,2
ZcplhhHpcHp(gt2) = ZcplhhHpcHp(gt2) + ZRUZH(gt2,gt1)*TempcplhhHpcHp(gt1) 
 End Do 
End Do 
TempcplhhHpcHp = ZcplhhHpcHp 


 ! ## ZcplhhSsccSsc ## 
ZcplhhSsccSsc = 0._dp 
TempcplhhSsccSsc = cplhhSsccSsc 
Do gt1=1,2
  Do gt2=1,2
ZcplhhSsccSsc(gt2,:,:) = ZcplhhSsccSsc(gt2,:,:) + ZRUZH(gt2,gt1)*TempcplhhSsccSsc(gt1,:,:) 
 End Do 
End Do 
TempcplhhSsccSsc = ZcplhhSsccSsc 
ZcplhhSsccSsc = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplhhSsccSsc(:,gt2,:) = ZcplhhSsccSsc(:,gt2,:) + ZRUVSs(gt2,gt1)*TempcplhhSsccSsc(:,gt1,:) 
 End Do 
End Do 
TempcplhhSsccSsc = ZcplhhSsccSsc 
ZcplhhSsccSsc = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplhhSsccSsc(:,:,gt2) = ZcplhhSsccSsc(:,:,gt2) + ZRUVSs(gt2,gt1)*TempcplhhSsccSsc(:,:,gt1) 
 End Do 
End Do 


 ! ## ZcplAhAhAhAh ## 
ZcplAhAhAhAh = 0._dp 


 ! ## ZcplAhAhhhhh ## 
ZcplAhAhhhhh = 0._dp 


 ! ## ZcplAhAhHpcHp ## 
ZcplAhAhHpcHp = 0._dp 


 ! ## ZcplAhAhSsccSsc ## 
ZcplAhAhSsccSsc = 0._dp 


 ! ## Zcplhhhhhhhh ## 
Zcplhhhhhhhh = 0._dp 


 ! ## ZcplhhhhHpcHp ## 
ZcplhhhhHpcHp = 0._dp 


 ! ## ZcplhhhhSsccSsc ## 
ZcplhhhhSsccSsc = 0._dp 


 ! ## ZcplHpHpcHpcHp ## 
ZcplHpHpcHpcHp = 0._dp 


 ! ## ZcplHpSsccHpcSsc ## 
ZcplHpSsccHpcSsc = 0._dp 


 ! ## ZcplSscSsccSsccSsc ## 
ZcplSscSsccSsccSsc = 0._dp 


 ! ## ZcplAhhhVZ ## 
ZcplAhhhVZ = 0._dp 
TempcplAhhhVZ = cplAhhhVZ 
ZcplAhhhVZ = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplAhhhVZ(gt2) = ZcplAhhhVZ(gt2) + ZRUZH(gt2,gt1)*TempcplAhhhVZ(gt1) 
 End Do 
End Do 
TempcplAhhhVZ = ZcplAhhhVZ 


 ! ## ZcplAhHpcVWp ## 
ZcplAhHpcVWp = 0._dp 
TempcplAhHpcVWp = cplAhHpcVWp 
ZcplAhHpcVWp = TempcplAhHpcVWp 


 ! ## ZcplAhcHpVWp ## 
ZcplAhcHpVWp = 0._dp 
TempcplAhcHpVWp = cplAhcHpVWp 
ZcplAhcHpVWp = TempcplAhcHpVWp 


 ! ## ZcplhhHpcVWp ## 
ZcplhhHpcVWp = 0._dp 
TempcplhhHpcVWp = cplhhHpcVWp 
Do gt1=1,2
  Do gt2=1,2
ZcplhhHpcVWp(gt2) = ZcplhhHpcVWp(gt2) + ZRUZH(gt2,gt1)*TempcplhhHpcVWp(gt1) 
 End Do 
End Do 
TempcplhhHpcVWp = ZcplhhHpcVWp 


 ! ## ZcplhhcHpVWp ## 
ZcplhhcHpVWp = 0._dp 
TempcplhhcHpVWp = cplhhcHpVWp 
Do gt1=1,2
  Do gt2=1,2
ZcplhhcHpVWp(gt2) = ZcplhhcHpVWp(gt2) + ZRUZH(gt2,gt1)*TempcplhhcHpVWp(gt1) 
 End Do 
End Do 
TempcplhhcHpVWp = ZcplhhcHpVWp 


 ! ## ZcplHpcHpVP ## 
ZcplHpcHpVP = 0._dp 
TempcplHpcHpVP = cplHpcHpVP 
ZcplHpcHpVP = TempcplHpcHpVP 


 ! ## ZcplHpcHpVZ ## 
ZcplHpcHpVZ = 0._dp 
TempcplHpcHpVZ = cplHpcHpVZ 
ZcplHpcHpVZ = TempcplHpcHpVZ 


 ! ## ZcplhhcVWpVWp ## 
ZcplhhcVWpVWp = 0._dp 
TempcplhhcVWpVWp = cplhhcVWpVWp 
Do gt1=1,2
  Do gt2=1,2
ZcplhhcVWpVWp(gt2) = ZcplhhcVWpVWp(gt2) + ZRUZH(gt2,gt1)*TempcplhhcVWpVWp(gt1) 
 End Do 
End Do 
TempcplhhcVWpVWp = ZcplhhcVWpVWp 


 ! ## ZcplhhVZVZ ## 
ZcplhhVZVZ = 0._dp 
TempcplhhVZVZ = cplhhVZVZ 
Do gt1=1,2
  Do gt2=1,2
ZcplhhVZVZ(gt2) = ZcplhhVZVZ(gt2) + ZRUZH(gt2,gt1)*TempcplhhVZVZ(gt1) 
 End Do 
End Do 
TempcplhhVZVZ = ZcplhhVZVZ 


 ! ## ZcplHpcVWpVP ## 
ZcplHpcVWpVP = 0._dp 
TempcplHpcVWpVP = cplHpcVWpVP 
ZcplHpcVWpVP = TempcplHpcVWpVP 


 ! ## ZcplHpcVWpVZ ## 
ZcplHpcVWpVZ = 0._dp 
TempcplHpcVWpVZ = cplHpcVWpVZ 
ZcplHpcVWpVZ = TempcplHpcVWpVZ 


 ! ## ZcplcHpVPVWp ## 
ZcplcHpVPVWp = 0._dp 
TempcplcHpVPVWp = cplcHpVPVWp 
ZcplcHpVPVWp = TempcplcHpVPVWp 


 ! ## ZcplcHpVWpVZ ## 
ZcplcHpVWpVZ = 0._dp 
TempcplcHpVWpVZ = cplcHpVWpVZ 
ZcplcHpVWpVZ = TempcplcHpVWpVZ 


 ! ## ZcplAhAhcVWpVWp ## 
ZcplAhAhcVWpVWp = 0._dp 


 ! ## ZcplAhAhVZVZ ## 
ZcplAhAhVZVZ = 0._dp 


 ! ## ZcplAhHpcVWpVP ## 
ZcplAhHpcVWpVP = 0._dp 


 ! ## ZcplAhHpcVWpVZ ## 
ZcplAhHpcVWpVZ = 0._dp 


 ! ## ZcplAhcHpVPVWp ## 
ZcplAhcHpVPVWp = 0._dp 


 ! ## ZcplAhcHpVWpVZ ## 
ZcplAhcHpVWpVZ = 0._dp 


 ! ## ZcplhhhhcVWpVWp ## 
ZcplhhhhcVWpVWp = 0._dp 


 ! ## ZcplhhhhVZVZ ## 
ZcplhhhhVZVZ = 0._dp 


 ! ## ZcplhhHpcVWpVP ## 
ZcplhhHpcVWpVP = 0._dp 


 ! ## ZcplhhHpcVWpVZ ## 
ZcplhhHpcVWpVZ = 0._dp 


 ! ## ZcplhhcHpVPVWp ## 
ZcplhhcHpVPVWp = 0._dp 


 ! ## ZcplhhcHpVWpVZ ## 
ZcplhhcHpVWpVZ = 0._dp 


 ! ## ZcplHpcHpVPVP ## 
ZcplHpcHpVPVP = 0._dp 


 ! ## ZcplHpcHpVPVZ ## 
ZcplHpcHpVPVZ = 0._dp 


 ! ## ZcplHpcHpcVWpVWp ## 
ZcplHpcHpcVWpVWp = 0._dp 


 ! ## ZcplHpcHpVZVZ ## 
ZcplHpcHpVZVZ = 0._dp 


 ! ## ZcplVGVGVG ## 
ZcplVGVGVG = 0._dp 
TempcplVGVGVG = cplVGVGVG 
ZcplVGVGVG = TempcplVGVGVG 


 ! ## ZcplcVWpVPVWp ## 
ZcplcVWpVPVWp = 0._dp 
TempcplcVWpVPVWp = cplcVWpVPVWp 
ZcplcVWpVPVWp = TempcplcVWpVPVWp 


 ! ## ZcplcVWpVWpVZ ## 
ZcplcVWpVWpVZ = 0._dp 
TempcplcVWpVWpVZ = cplcVWpVWpVZ 
ZcplcVWpVWpVZ = TempcplcVWpVWpVZ 


 ! ## ZcplcFdFdAhL ## 
ZcplcFdFdAhL = 0._dp 
TempcplcFdFdAhL = cplcFdFdAhL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdAhL(gt2,:) = ZcplcFdFdAhL(gt2,:) + ZRUUd(gt2,gt1)*TempcplcFdFdAhL(gt1,:) 
 End Do 
End Do 
TempcplcFdFdAhL = ZcplcFdFdAhL 
ZcplcFdFdAhL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdAhL(:,gt2) = ZcplcFdFdAhL(:,gt2) + ZRUVd(gt2,gt1)*TempcplcFdFdAhL(:,gt1) 
 End Do 
End Do 
TempcplcFdFdAhL = ZcplcFdFdAhL 


 ! ## ZcplcFdFdAhR ## 
ZcplcFdFdAhR = 0._dp 
TempcplcFdFdAhR = cplcFdFdAhR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdAhR(gt2,:) = ZcplcFdFdAhR(gt2,:) + ZRUVdc(gt2,gt1)*TempcplcFdFdAhR(gt1,:) 
 End Do 
End Do 
TempcplcFdFdAhR = ZcplcFdFdAhR 
ZcplcFdFdAhR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdAhR(:,gt2) = ZcplcFdFdAhR(:,gt2) + ZRUUdc(gt2,gt1)*TempcplcFdFdAhR(:,gt1) 
 End Do 
End Do 
TempcplcFdFdAhR = ZcplcFdFdAhR 


 ! ## ZcplcFeFeAhL ## 
ZcplcFeFeAhL = 0._dp 
TempcplcFeFeAhL = cplcFeFeAhL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFeAhL(gt2,:) = ZcplcFeFeAhL(gt2,:) + ZRUUe(gt2,gt1)*TempcplcFeFeAhL(gt1,:) 
 End Do 
End Do 
TempcplcFeFeAhL = ZcplcFeFeAhL 
ZcplcFeFeAhL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFeAhL(:,gt2) = ZcplcFeFeAhL(:,gt2) + ZRUVe(gt2,gt1)*TempcplcFeFeAhL(:,gt1) 
 End Do 
End Do 
TempcplcFeFeAhL = ZcplcFeFeAhL 


 ! ## ZcplcFeFeAhR ## 
ZcplcFeFeAhR = 0._dp 
TempcplcFeFeAhR = cplcFeFeAhR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFeAhR(gt2,:) = ZcplcFeFeAhR(gt2,:) + ZRUVec(gt2,gt1)*TempcplcFeFeAhR(gt1,:) 
 End Do 
End Do 
TempcplcFeFeAhR = ZcplcFeFeAhR 
ZcplcFeFeAhR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFeAhR(:,gt2) = ZcplcFeFeAhR(:,gt2) + ZRUUec(gt2,gt1)*TempcplcFeFeAhR(:,gt1) 
 End Do 
End Do 
TempcplcFeFeAhR = ZcplcFeFeAhR 


 ! ## ZcplcFuFuAhL ## 
ZcplcFuFuAhL = 0._dp 
TempcplcFuFuAhL = cplcFuFuAhL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuAhL(gt2,:) = ZcplcFuFuAhL(gt2,:) + ZRUUu(gt2,gt1)*TempcplcFuFuAhL(gt1,:) 
 End Do 
End Do 
TempcplcFuFuAhL = ZcplcFuFuAhL 
ZcplcFuFuAhL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuAhL(:,gt2) = ZcplcFuFuAhL(:,gt2) + ZRUVu(gt2,gt1)*TempcplcFuFuAhL(:,gt1) 
 End Do 
End Do 
TempcplcFuFuAhL = ZcplcFuFuAhL 


 ! ## ZcplcFuFuAhR ## 
ZcplcFuFuAhR = 0._dp 
TempcplcFuFuAhR = cplcFuFuAhR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuAhR(gt2,:) = ZcplcFuFuAhR(gt2,:) + ZRUVuc(gt2,gt1)*TempcplcFuFuAhR(gt1,:) 
 End Do 
End Do 
TempcplcFuFuAhR = ZcplcFuFuAhR 
ZcplcFuFuAhR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuAhR(:,gt2) = ZcplcFuFuAhR(:,gt2) + ZRUUuc(gt2,gt1)*TempcplcFuFuAhR(:,gt1) 
 End Do 
End Do 
TempcplcFuFuAhR = ZcplcFuFuAhR 


 ! ## ZcplcFxvFxvAhL ## 
ZcplcFxvFxvAhL = 0._dp 
TempcplcFxvFxvAhL = cplcFxvFxvAhL 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxvAhL(gt2,:) = ZcplcFxvFxvAhL(gt2,:) + ZRUXU(gt2,gt1)*TempcplcFxvFxvAhL(gt1,:) 
 End Do 
End Do 
TempcplcFxvFxvAhL = ZcplcFxvFxvAhL 
ZcplcFxvFxvAhL = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxvAhL(:,gt2) = ZcplcFxvFxvAhL(:,gt2) + ZRUXV(gt2,gt1)*TempcplcFxvFxvAhL(:,gt1) 
 End Do 
End Do 
TempcplcFxvFxvAhL = ZcplcFxvFxvAhL 


 ! ## ZcplcFxvFxvAhR ## 
ZcplcFxvFxvAhR = 0._dp 
TempcplcFxvFxvAhR = cplcFxvFxvAhR 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxvAhR(gt2,:) = ZcplcFxvFxvAhR(gt2,:) + ZRUXVc(gt2,gt1)*TempcplcFxvFxvAhR(gt1,:) 
 End Do 
End Do 
TempcplcFxvFxvAhR = ZcplcFxvFxvAhR 
ZcplcFxvFxvAhR = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxvAhR(:,gt2) = ZcplcFxvFxvAhR(:,gt2) + ZRUXUc(gt2,gt1)*TempcplcFxvFxvAhR(:,gt1) 
 End Do 
End Do 
TempcplcFxvFxvAhR = ZcplcFxvFxvAhR 


 ! ## ZcplcFdFdhhL ## 
ZcplcFdFdhhL = 0._dp 
TempcplcFdFdhhL = cplcFdFdhhL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdhhL(gt2,:,:) = ZcplcFdFdhhL(gt2,:,:) + ZRUUd(gt2,gt1)*TempcplcFdFdhhL(gt1,:,:) 
 End Do 
End Do 
TempcplcFdFdhhL = ZcplcFdFdhhL 
ZcplcFdFdhhL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdhhL(:,gt2,:) = ZcplcFdFdhhL(:,gt2,:) + ZRUVd(gt2,gt1)*TempcplcFdFdhhL(:,gt1,:) 
 End Do 
End Do 
TempcplcFdFdhhL = ZcplcFdFdhhL 
ZcplcFdFdhhL = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFdFdhhL(:,:,gt2) = ZcplcFdFdhhL(:,:,gt2) + ZRUZH(gt2,gt1)*TempcplcFdFdhhL(:,:,gt1) 
 End Do 
End Do 


 ! ## ZcplcFdFdhhR ## 
ZcplcFdFdhhR = 0._dp 
TempcplcFdFdhhR = cplcFdFdhhR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdhhR(gt2,:,:) = ZcplcFdFdhhR(gt2,:,:) + ZRUVdc(gt2,gt1)*TempcplcFdFdhhR(gt1,:,:) 
 End Do 
End Do 
TempcplcFdFdhhR = ZcplcFdFdhhR 
ZcplcFdFdhhR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdhhR(:,gt2,:) = ZcplcFdFdhhR(:,gt2,:) + ZRUUdc(gt2,gt1)*TempcplcFdFdhhR(:,gt1,:) 
 End Do 
End Do 
TempcplcFdFdhhR = ZcplcFdFdhhR 
ZcplcFdFdhhR = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFdFdhhR(:,:,gt2) = ZcplcFdFdhhR(:,:,gt2) + ZRUZH(gt2,gt1)*TempcplcFdFdhhR(:,:,gt1) 
 End Do 
End Do 


 ! ## ZcplcFuFdHpL ## 
ZcplcFuFdHpL = 0._dp 
TempcplcFuFdHpL = cplcFuFdHpL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFdHpL(gt2,:) = ZcplcFuFdHpL(gt2,:) + ZRUUu(gt2,gt1)*TempcplcFuFdHpL(gt1,:) 
 End Do 
End Do 
TempcplcFuFdHpL = ZcplcFuFdHpL 
ZcplcFuFdHpL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFdHpL(:,gt2) = ZcplcFuFdHpL(:,gt2) + ZRUVd(gt2,gt1)*TempcplcFuFdHpL(:,gt1) 
 End Do 
End Do 
TempcplcFuFdHpL = ZcplcFuFdHpL 


 ! ## ZcplcFuFdHpR ## 
ZcplcFuFdHpR = 0._dp 
TempcplcFuFdHpR = cplcFuFdHpR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFdHpR(gt2,:) = ZcplcFuFdHpR(gt2,:) + ZRUVuc(gt2,gt1)*TempcplcFuFdHpR(gt1,:) 
 End Do 
End Do 
TempcplcFuFdHpR = ZcplcFuFdHpR 
ZcplcFuFdHpR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFdHpR(:,gt2) = ZcplcFuFdHpR(:,gt2) + ZRUUdc(gt2,gt1)*TempcplcFuFdHpR(:,gt1) 
 End Do 
End Do 
TempcplcFuFdHpR = ZcplcFuFdHpR 


 ! ## ZcplcFeFehhL ## 
ZcplcFeFehhL = 0._dp 
TempcplcFeFehhL = cplcFeFehhL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFehhL(gt2,:,:) = ZcplcFeFehhL(gt2,:,:) + ZRUUe(gt2,gt1)*TempcplcFeFehhL(gt1,:,:) 
 End Do 
End Do 
TempcplcFeFehhL = ZcplcFeFehhL 
ZcplcFeFehhL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFehhL(:,gt2,:) = ZcplcFeFehhL(:,gt2,:) + ZRUVe(gt2,gt1)*TempcplcFeFehhL(:,gt1,:) 
 End Do 
End Do 
TempcplcFeFehhL = ZcplcFeFehhL 
ZcplcFeFehhL = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFeFehhL(:,:,gt2) = ZcplcFeFehhL(:,:,gt2) + ZRUZH(gt2,gt1)*TempcplcFeFehhL(:,:,gt1) 
 End Do 
End Do 


 ! ## ZcplcFeFehhR ## 
ZcplcFeFehhR = 0._dp 
TempcplcFeFehhR = cplcFeFehhR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFehhR(gt2,:,:) = ZcplcFeFehhR(gt2,:,:) + ZRUVec(gt2,gt1)*TempcplcFeFehhR(gt1,:,:) 
 End Do 
End Do 
TempcplcFeFehhR = ZcplcFeFehhR 
ZcplcFeFehhR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFehhR(:,gt2,:) = ZcplcFeFehhR(:,gt2,:) + ZRUUec(gt2,gt1)*TempcplcFeFehhR(:,gt1,:) 
 End Do 
End Do 
TempcplcFeFehhR = ZcplcFeFehhR 
ZcplcFeFehhR = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFeFehhR(:,:,gt2) = ZcplcFeFehhR(:,:,gt2) + ZRUZH(gt2,gt1)*TempcplcFeFehhR(:,:,gt1) 
 End Do 
End Do 


 ! ## ZcplcFvFeHpL ## 
ZcplcFvFeHpL = 0._dp 
TempcplcFvFeHpL = cplcFvFeHpL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFvFeHpL(gt2,:) = ZcplcFvFeHpL(gt2,:) + ZRUVvr(gt2,gt1)*TempcplcFvFeHpL(gt1,:) 
 End Do 
End Do 
TempcplcFvFeHpL = ZcplcFvFeHpL 
ZcplcFvFeHpL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFvFeHpL(:,gt2) = ZcplcFvFeHpL(:,gt2) + ZRUVe(gt2,gt1)*TempcplcFvFeHpL(:,gt1) 
 End Do 
End Do 
TempcplcFvFeHpL = ZcplcFvFeHpL 


 ! ## ZcplcFvFeHpR ## 
ZcplcFvFeHpR = 0._dp 
TempcplcFvFeHpR = cplcFvFeHpR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFvFeHpR(gt2,:) = ZcplcFvFeHpR(gt2,:) + ZRUVvc(gt2,gt1)*TempcplcFvFeHpR(gt1,:) 
 End Do 
End Do 
TempcplcFvFeHpR = ZcplcFvFeHpR 
ZcplcFvFeHpR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFvFeHpR(:,gt2) = ZcplcFvFeHpR(:,gt2) + ZRUUec(gt2,gt1)*TempcplcFvFeHpR(:,gt1) 
 End Do 
End Do 
TempcplcFvFeHpR = ZcplcFvFeHpR 


 ! ## ZcplcFxeFeSscL ## 
ZcplcFxeFeSscL = 0._dp 
TempcplcFxeFeSscL = cplcFxeFeSscL 
ZcplcFxeFeSscL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFxeFeSscL(gt2,:) = ZcplcFxeFeSscL(gt2,:) + ZRUVe(gt2,gt1)*TempcplcFxeFeSscL(gt1,:) 
 End Do 
End Do 
TempcplcFxeFeSscL = ZcplcFxeFeSscL 
ZcplcFxeFeSscL = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxeFeSscL(:,gt2) = ZcplcFxeFeSscL(:,gt2) + ZRUVSs(gt2,gt1)*TempcplcFxeFeSscL(:,gt1) 
 End Do 
End Do 


 ! ## ZcplcFxeFeSscR ## 
ZcplcFxeFeSscR = 0._dp 
TempcplcFxeFeSscR = cplcFxeFeSscR 
ZcplcFxeFeSscR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFxeFeSscR(gt2,:) = ZcplcFxeFeSscR(gt2,:) + ZRUUec(gt2,gt1)*TempcplcFxeFeSscR(gt1,:) 
 End Do 
End Do 
TempcplcFxeFeSscR = ZcplcFxeFeSscR 
ZcplcFxeFeSscR = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxeFeSscR(:,gt2) = ZcplcFxeFeSscR(:,gt2) + ZRUVSs(gt2,gt1)*TempcplcFxeFeSscR(:,gt1) 
 End Do 
End Do 


 ! ## ZcplcFuFuhhL ## 
ZcplcFuFuhhL = 0._dp 
TempcplcFuFuhhL = cplcFuFuhhL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuhhL(gt2,:,:) = ZcplcFuFuhhL(gt2,:,:) + ZRUUu(gt2,gt1)*TempcplcFuFuhhL(gt1,:,:) 
 End Do 
End Do 
TempcplcFuFuhhL = ZcplcFuFuhhL 
ZcplcFuFuhhL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuhhL(:,gt2,:) = ZcplcFuFuhhL(:,gt2,:) + ZRUVu(gt2,gt1)*TempcplcFuFuhhL(:,gt1,:) 
 End Do 
End Do 
TempcplcFuFuhhL = ZcplcFuFuhhL 
ZcplcFuFuhhL = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFuFuhhL(:,:,gt2) = ZcplcFuFuhhL(:,:,gt2) + ZRUZH(gt2,gt1)*TempcplcFuFuhhL(:,:,gt1) 
 End Do 
End Do 


 ! ## ZcplcFuFuhhR ## 
ZcplcFuFuhhR = 0._dp 
TempcplcFuFuhhR = cplcFuFuhhR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuhhR(gt2,:,:) = ZcplcFuFuhhR(gt2,:,:) + ZRUVuc(gt2,gt1)*TempcplcFuFuhhR(gt1,:,:) 
 End Do 
End Do 
TempcplcFuFuhhR = ZcplcFuFuhhR 
ZcplcFuFuhhR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuhhR(:,gt2,:) = ZcplcFuFuhhR(:,gt2,:) + ZRUUuc(gt2,gt1)*TempcplcFuFuhhR(:,gt1,:) 
 End Do 
End Do 
TempcplcFuFuhhR = ZcplcFuFuhhR 
ZcplcFuFuhhR = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFuFuhhR(:,:,gt2) = ZcplcFuFuhhR(:,:,gt2) + ZRUZH(gt2,gt1)*TempcplcFuFuhhR(:,:,gt1) 
 End Do 
End Do 


 ! ## ZcplcFdFucHpL ## 
ZcplcFdFucHpL = 0._dp 
TempcplcFdFucHpL = cplcFdFucHpL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFucHpL(gt2,:) = ZcplcFdFucHpL(gt2,:) + ZRUUd(gt2,gt1)*TempcplcFdFucHpL(gt1,:) 
 End Do 
End Do 
TempcplcFdFucHpL = ZcplcFdFucHpL 
ZcplcFdFucHpL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFucHpL(:,gt2) = ZcplcFdFucHpL(:,gt2) + ZRUVu(gt2,gt1)*TempcplcFdFucHpL(:,gt1) 
 End Do 
End Do 
TempcplcFdFucHpL = ZcplcFdFucHpL 


 ! ## ZcplcFdFucHpR ## 
ZcplcFdFucHpR = 0._dp 
TempcplcFdFucHpR = cplcFdFucHpR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFucHpR(gt2,:) = ZcplcFdFucHpR(gt2,:) + ZRUVdc(gt2,gt1)*TempcplcFdFucHpR(gt1,:) 
 End Do 
End Do 
TempcplcFdFucHpR = ZcplcFdFucHpR 
ZcplcFdFucHpR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFucHpR(:,gt2) = ZcplcFdFucHpR(:,gt2) + ZRUUuc(gt2,gt1)*TempcplcFdFucHpR(:,gt1) 
 End Do 
End Do 
TempcplcFdFucHpR = ZcplcFdFucHpR 


 ! ## ZcplcFxvFvSscL ## 
ZcplcFxvFvSscL = 0._dp 
TempcplcFxvFvSscL = cplcFxvFvSscL 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFvSscL(gt2,:,:) = ZcplcFxvFvSscL(gt2,:,:) + ZRUXU(gt2,gt1)*TempcplcFxvFvSscL(gt1,:,:) 
 End Do 
End Do 
TempcplcFxvFvSscL = ZcplcFxvFvSscL 
ZcplcFxvFvSscL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFxvFvSscL(:,gt2,:) = ZcplcFxvFvSscL(:,gt2,:) + ZRUVv(gt2,gt1)*TempcplcFxvFvSscL(:,gt1,:) 
 End Do 
End Do 
TempcplcFxvFvSscL = ZcplcFxvFvSscL 
ZcplcFxvFvSscL = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFvSscL(:,:,gt2) = ZcplcFxvFvSscL(:,:,gt2) + ZRUVSs(gt2,gt1)*TempcplcFxvFvSscL(:,:,gt1) 
 End Do 
End Do 


 ! ## ZcplcFxvFvSscR ## 
ZcplcFxvFvSscR = 0._dp 
TempcplcFxvFvSscR = cplcFxvFvSscR 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFvSscR(gt2,:,:) = ZcplcFxvFvSscR(gt2,:,:) + ZRUXVc(gt2,gt1)*TempcplcFxvFvSscR(gt1,:,:) 
 End Do 
End Do 
TempcplcFxvFvSscR = ZcplcFxvFvSscR 
ZcplcFxvFvSscR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFxvFvSscR(:,gt2,:) = ZcplcFxvFvSscR(:,gt2,:) + ZRUVvrc(gt2,gt1)*TempcplcFxvFvSscR(:,gt1,:) 
 End Do 
End Do 
TempcplcFxvFvSscR = ZcplcFxvFvSscR 
ZcplcFxvFvSscR = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFvSscR(:,:,gt2) = ZcplcFxvFvSscR(:,:,gt2) + ZRUVSs(gt2,gt1)*TempcplcFxvFvSscR(:,:,gt1) 
 End Do 
End Do 


 ! ## ZcplcFeFvcHpL ## 
ZcplcFeFvcHpL = 0._dp 
TempcplcFeFvcHpL = cplcFeFvcHpL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFvcHpL(gt2,:) = ZcplcFeFvcHpL(gt2,:) + ZRUUe(gt2,gt1)*TempcplcFeFvcHpL(gt1,:) 
 End Do 
End Do 
TempcplcFeFvcHpL = ZcplcFeFvcHpL 
ZcplcFeFvcHpL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFvcHpL(:,gt2) = ZcplcFeFvcHpL(:,gt2) + ZRUVv(gt2,gt1)*TempcplcFeFvcHpL(:,gt1) 
 End Do 
End Do 
TempcplcFeFvcHpL = ZcplcFeFvcHpL 


 ! ## ZcplcFeFvcHpR ## 
ZcplcFeFvcHpR = 0._dp 
TempcplcFeFvcHpR = cplcFeFvcHpR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFvcHpR(gt2,:) = ZcplcFeFvcHpR(gt2,:) + ZRUVec(gt2,gt1)*TempcplcFeFvcHpR(gt1,:) 
 End Do 
End Do 
TempcplcFeFvcHpR = ZcplcFeFvcHpR 
ZcplcFeFvcHpR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFvcHpR(:,gt2) = ZcplcFeFvcHpR(:,gt2) + ZRUVvrc(gt2,gt1)*TempcplcFeFvcHpR(:,gt1) 
 End Do 
End Do 
TempcplcFeFvcHpR = ZcplcFeFvcHpR 


 ! ## ZcplcFxvFxeHpL ## 
ZcplcFxvFxeHpL = 0._dp 
TempcplcFxvFxeHpL = cplcFxvFxeHpL 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxeHpL(gt2) = ZcplcFxvFxeHpL(gt2) + ZRUXU(gt2,gt1)*TempcplcFxvFxeHpL(gt1) 
 End Do 
End Do 
TempcplcFxvFxeHpL = ZcplcFxvFxeHpL 


 ! ## ZcplcFxvFxeHpR ## 
ZcplcFxvFxeHpR = 0._dp 
TempcplcFxvFxeHpR = cplcFxvFxeHpR 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxeHpR(gt2) = ZcplcFxvFxeHpR(gt2) + ZRUXVc(gt2,gt1)*TempcplcFxvFxeHpR(gt1) 
 End Do 
End Do 
TempcplcFxvFxeHpR = ZcplcFxvFxeHpR 


 ! ## ZcplcFeFxecSscL ## 
ZcplcFeFxecSscL = 0._dp 
TempcplcFeFxecSscL = cplcFeFxecSscL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFxecSscL(gt2,:) = ZcplcFeFxecSscL(gt2,:) + ZRUUe(gt2,gt1)*TempcplcFeFxecSscL(gt1,:) 
 End Do 
End Do 
TempcplcFeFxecSscL = ZcplcFeFxecSscL 
ZcplcFeFxecSscL = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFeFxecSscL(:,gt2) = ZcplcFeFxecSscL(:,gt2) + ZRUVSs(gt2,gt1)*TempcplcFeFxecSscL(:,gt1) 
 End Do 
End Do 


 ! ## ZcplcFeFxecSscR ## 
ZcplcFeFxecSscR = 0._dp 
TempcplcFeFxecSscR = cplcFeFxecSscR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFxecSscR(gt2,:) = ZcplcFeFxecSscR(gt2,:) + ZRUVec(gt2,gt1)*TempcplcFeFxecSscR(gt1,:) 
 End Do 
End Do 
TempcplcFeFxecSscR = ZcplcFeFxecSscR 
ZcplcFeFxecSscR = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFeFxecSscR(:,gt2) = ZcplcFeFxecSscR(:,gt2) + ZRUVSs(gt2,gt1)*TempcplcFeFxecSscR(:,gt1) 
 End Do 
End Do 


 ! ## ZcplcFxvFxvhhL ## 
ZcplcFxvFxvhhL = 0._dp 
TempcplcFxvFxvhhL = cplcFxvFxvhhL 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxvhhL(gt2,:,:) = ZcplcFxvFxvhhL(gt2,:,:) + ZRUXU(gt2,gt1)*TempcplcFxvFxvhhL(gt1,:,:) 
 End Do 
End Do 
TempcplcFxvFxvhhL = ZcplcFxvFxvhhL 
ZcplcFxvFxvhhL = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxvhhL(:,gt2,:) = ZcplcFxvFxvhhL(:,gt2,:) + ZRUXV(gt2,gt1)*TempcplcFxvFxvhhL(:,gt1,:) 
 End Do 
End Do 
TempcplcFxvFxvhhL = ZcplcFxvFxvhhL 
ZcplcFxvFxvhhL = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxvhhL(:,:,gt2) = ZcplcFxvFxvhhL(:,:,gt2) + ZRUZH(gt2,gt1)*TempcplcFxvFxvhhL(:,:,gt1) 
 End Do 
End Do 


 ! ## ZcplcFxvFxvhhR ## 
ZcplcFxvFxvhhR = 0._dp 
TempcplcFxvFxvhhR = cplcFxvFxvhhR 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxvhhR(gt2,:,:) = ZcplcFxvFxvhhR(gt2,:,:) + ZRUXVc(gt2,gt1)*TempcplcFxvFxvhhR(gt1,:,:) 
 End Do 
End Do 
TempcplcFxvFxvhhR = ZcplcFxvFxvhhR 
ZcplcFxvFxvhhR = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxvhhR(:,gt2,:) = ZcplcFxvFxvhhR(:,gt2,:) + ZRUXUc(gt2,gt1)*TempcplcFxvFxvhhR(:,gt1,:) 
 End Do 
End Do 
TempcplcFxvFxvhhR = ZcplcFxvFxvhhR 
ZcplcFxvFxvhhR = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxvhhR(:,:,gt2) = ZcplcFxvFxvhhR(:,:,gt2) + ZRUZH(gt2,gt1)*TempcplcFxvFxvhhR(:,:,gt1) 
 End Do 
End Do 


 ! ## ZcplcFvFxvcSscL ## 
ZcplcFvFxvcSscL = 0._dp 
TempcplcFvFxvcSscL = cplcFvFxvcSscL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFvFxvcSscL(gt2,:,:) = ZcplcFvFxvcSscL(gt2,:,:) + ZRUVvr(gt2,gt1)*TempcplcFvFxvcSscL(gt1,:,:) 
 End Do 
End Do 
TempcplcFvFxvcSscL = ZcplcFvFxvcSscL 
ZcplcFvFxvcSscL = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFvFxvcSscL(:,gt2,:) = ZcplcFvFxvcSscL(:,gt2,:) + ZRUXV(gt2,gt1)*TempcplcFvFxvcSscL(:,gt1,:) 
 End Do 
End Do 
TempcplcFvFxvcSscL = ZcplcFvFxvcSscL 
ZcplcFvFxvcSscL = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFvFxvcSscL(:,:,gt2) = ZcplcFvFxvcSscL(:,:,gt2) + ZRUVSs(gt2,gt1)*TempcplcFvFxvcSscL(:,:,gt1) 
 End Do 
End Do 


 ! ## ZcplcFvFxvcSscR ## 
ZcplcFvFxvcSscR = 0._dp 
TempcplcFvFxvcSscR = cplcFvFxvcSscR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFvFxvcSscR(gt2,:,:) = ZcplcFvFxvcSscR(gt2,:,:) + ZRUVvc(gt2,gt1)*TempcplcFvFxvcSscR(gt1,:,:) 
 End Do 
End Do 
TempcplcFvFxvcSscR = ZcplcFvFxvcSscR 
ZcplcFvFxvcSscR = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFvFxvcSscR(:,gt2,:) = ZcplcFvFxvcSscR(:,gt2,:) + ZRUXUc(gt2,gt1)*TempcplcFvFxvcSscR(:,gt1,:) 
 End Do 
End Do 
TempcplcFvFxvcSscR = ZcplcFvFxvcSscR 
ZcplcFvFxvcSscR = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFvFxvcSscR(:,:,gt2) = ZcplcFvFxvcSscR(:,:,gt2) + ZRUVSs(gt2,gt1)*TempcplcFvFxvcSscR(:,:,gt1) 
 End Do 
End Do 


 ! ## ZcplcFxeFxvcHpL ## 
ZcplcFxeFxvcHpL = 0._dp 
TempcplcFxeFxvcHpL = cplcFxeFxvcHpL 
ZcplcFxeFxvcHpL = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxeFxvcHpL(gt2) = ZcplcFxeFxvcHpL(gt2) + ZRUXV(gt2,gt1)*TempcplcFxeFxvcHpL(gt1) 
 End Do 
End Do 
TempcplcFxeFxvcHpL = ZcplcFxeFxvcHpL 


 ! ## ZcplcFxeFxvcHpR ## 
ZcplcFxeFxvcHpR = 0._dp 
TempcplcFxeFxvcHpR = cplcFxeFxvcHpR 
ZcplcFxeFxvcHpR = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxeFxvcHpR(gt2) = ZcplcFxeFxvcHpR(gt2) + ZRUXUc(gt2,gt1)*TempcplcFxeFxvcHpR(gt1) 
 End Do 
End Do 
TempcplcFxeFxvcHpR = ZcplcFxeFxvcHpR 


 ! ## ZcplcFdFdVGL ## 
ZcplcFdFdVGL = 0._dp 
TempcplcFdFdVGL = cplcFdFdVGL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdVGL(gt2,:) = ZcplcFdFdVGL(gt2,:) + ZRUVdc(gt2,gt1)*TempcplcFdFdVGL(gt1,:) 
 End Do 
End Do 
TempcplcFdFdVGL = ZcplcFdFdVGL 
ZcplcFdFdVGL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdVGL(:,gt2) = ZcplcFdFdVGL(:,gt2) + ZRUVd(gt2,gt1)*TempcplcFdFdVGL(:,gt1) 
 End Do 
End Do 
TempcplcFdFdVGL = ZcplcFdFdVGL 


 ! ## ZcplcFdFdVGR ## 
ZcplcFdFdVGR = 0._dp 
TempcplcFdFdVGR = cplcFdFdVGR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdVGR(gt2,:) = ZcplcFdFdVGR(gt2,:) + ZRUUd(gt2,gt1)*TempcplcFdFdVGR(gt1,:) 
 End Do 
End Do 
TempcplcFdFdVGR = ZcplcFdFdVGR 
ZcplcFdFdVGR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdVGR(:,gt2) = ZcplcFdFdVGR(:,gt2) + ZRUUdc(gt2,gt1)*TempcplcFdFdVGR(:,gt1) 
 End Do 
End Do 
TempcplcFdFdVGR = ZcplcFdFdVGR 


 ! ## ZcplcFdFdVPL ## 
ZcplcFdFdVPL = 0._dp 
TempcplcFdFdVPL = cplcFdFdVPL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdVPL(gt2,:) = ZcplcFdFdVPL(gt2,:) + ZRUVdc(gt2,gt1)*TempcplcFdFdVPL(gt1,:) 
 End Do 
End Do 
TempcplcFdFdVPL = ZcplcFdFdVPL 
ZcplcFdFdVPL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdVPL(:,gt2) = ZcplcFdFdVPL(:,gt2) + ZRUVd(gt2,gt1)*TempcplcFdFdVPL(:,gt1) 
 End Do 
End Do 
TempcplcFdFdVPL = ZcplcFdFdVPL 


 ! ## ZcplcFdFdVPR ## 
ZcplcFdFdVPR = 0._dp 
TempcplcFdFdVPR = cplcFdFdVPR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdVPR(gt2,:) = ZcplcFdFdVPR(gt2,:) + ZRUUd(gt2,gt1)*TempcplcFdFdVPR(gt1,:) 
 End Do 
End Do 
TempcplcFdFdVPR = ZcplcFdFdVPR 
ZcplcFdFdVPR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdVPR(:,gt2) = ZcplcFdFdVPR(:,gt2) + ZRUUdc(gt2,gt1)*TempcplcFdFdVPR(:,gt1) 
 End Do 
End Do 
TempcplcFdFdVPR = ZcplcFdFdVPR 


 ! ## ZcplcFuFdVWpL ## 
ZcplcFuFdVWpL = 0._dp 
TempcplcFuFdVWpL = cplcFuFdVWpL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFdVWpL(gt2,:) = ZcplcFuFdVWpL(gt2,:) + ZRUVuc(gt2,gt1)*TempcplcFuFdVWpL(gt1,:) 
 End Do 
End Do 
TempcplcFuFdVWpL = ZcplcFuFdVWpL 
ZcplcFuFdVWpL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFdVWpL(:,gt2) = ZcplcFuFdVWpL(:,gt2) + ZRUVd(gt2,gt1)*TempcplcFuFdVWpL(:,gt1) 
 End Do 
End Do 
TempcplcFuFdVWpL = ZcplcFuFdVWpL 


 ! ## ZcplcFuFdVWpR ## 
ZcplcFuFdVWpR = 0._dp 
TempcplcFuFdVWpR = cplcFuFdVWpR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFdVWpR(gt2,:) = ZcplcFuFdVWpR(gt2,:) + ZRUUu(gt2,gt1)*TempcplcFuFdVWpR(gt1,:) 
 End Do 
End Do 
TempcplcFuFdVWpR = ZcplcFuFdVWpR 
ZcplcFuFdVWpR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFdVWpR(:,gt2) = ZcplcFuFdVWpR(:,gt2) + ZRUUdc(gt2,gt1)*TempcplcFuFdVWpR(:,gt1) 
 End Do 
End Do 
TempcplcFuFdVWpR = ZcplcFuFdVWpR 


 ! ## ZcplcFdFdVZL ## 
ZcplcFdFdVZL = 0._dp 
TempcplcFdFdVZL = cplcFdFdVZL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdVZL(gt2,:) = ZcplcFdFdVZL(gt2,:) + ZRUVdc(gt2,gt1)*TempcplcFdFdVZL(gt1,:) 
 End Do 
End Do 
TempcplcFdFdVZL = ZcplcFdFdVZL 
ZcplcFdFdVZL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdVZL(:,gt2) = ZcplcFdFdVZL(:,gt2) + ZRUVd(gt2,gt1)*TempcplcFdFdVZL(:,gt1) 
 End Do 
End Do 
TempcplcFdFdVZL = ZcplcFdFdVZL 


 ! ## ZcplcFdFdVZR ## 
ZcplcFdFdVZR = 0._dp 
TempcplcFdFdVZR = cplcFdFdVZR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdVZR(gt2,:) = ZcplcFdFdVZR(gt2,:) + ZRUUd(gt2,gt1)*TempcplcFdFdVZR(gt1,:) 
 End Do 
End Do 
TempcplcFdFdVZR = ZcplcFdFdVZR 
ZcplcFdFdVZR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFdVZR(:,gt2) = ZcplcFdFdVZR(:,gt2) + ZRUUdc(gt2,gt1)*TempcplcFdFdVZR(:,gt1) 
 End Do 
End Do 
TempcplcFdFdVZR = ZcplcFdFdVZR 


 ! ## ZcplcFeFeVPL ## 
ZcplcFeFeVPL = 0._dp 
TempcplcFeFeVPL = cplcFeFeVPL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFeVPL(gt2,:) = ZcplcFeFeVPL(gt2,:) + ZRUVec(gt2,gt1)*TempcplcFeFeVPL(gt1,:) 
 End Do 
End Do 
TempcplcFeFeVPL = ZcplcFeFeVPL 
ZcplcFeFeVPL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFeVPL(:,gt2) = ZcplcFeFeVPL(:,gt2) + ZRUVe(gt2,gt1)*TempcplcFeFeVPL(:,gt1) 
 End Do 
End Do 
TempcplcFeFeVPL = ZcplcFeFeVPL 


 ! ## ZcplcFeFeVPR ## 
ZcplcFeFeVPR = 0._dp 
TempcplcFeFeVPR = cplcFeFeVPR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFeVPR(gt2,:) = ZcplcFeFeVPR(gt2,:) + ZRUUe(gt2,gt1)*TempcplcFeFeVPR(gt1,:) 
 End Do 
End Do 
TempcplcFeFeVPR = ZcplcFeFeVPR 
ZcplcFeFeVPR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFeVPR(:,gt2) = ZcplcFeFeVPR(:,gt2) + ZRUUec(gt2,gt1)*TempcplcFeFeVPR(:,gt1) 
 End Do 
End Do 
TempcplcFeFeVPR = ZcplcFeFeVPR 


 ! ## ZcplcFvFeVWpL ## 
ZcplcFvFeVWpL = 0._dp 
TempcplcFvFeVWpL = cplcFvFeVWpL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFvFeVWpL(gt2,:) = ZcplcFvFeVWpL(gt2,:) + ZRUVvc(gt2,gt1)*TempcplcFvFeVWpL(gt1,:) 
 End Do 
End Do 
TempcplcFvFeVWpL = ZcplcFvFeVWpL 
ZcplcFvFeVWpL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFvFeVWpL(:,gt2) = ZcplcFvFeVWpL(:,gt2) + ZRUVe(gt2,gt1)*TempcplcFvFeVWpL(:,gt1) 
 End Do 
End Do 
TempcplcFvFeVWpL = ZcplcFvFeVWpL 


 ! ## ZcplcFvFeVWpR ## 
ZcplcFvFeVWpR = 0._dp 
TempcplcFvFeVWpR = cplcFvFeVWpR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFvFeVWpR(gt2,:) = ZcplcFvFeVWpR(gt2,:) + ZRUVvr(gt2,gt1)*TempcplcFvFeVWpR(gt1,:) 
 End Do 
End Do 
TempcplcFvFeVWpR = ZcplcFvFeVWpR 
ZcplcFvFeVWpR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFvFeVWpR(:,gt2) = ZcplcFvFeVWpR(:,gt2) + ZRUUec(gt2,gt1)*TempcplcFvFeVWpR(:,gt1) 
 End Do 
End Do 
TempcplcFvFeVWpR = ZcplcFvFeVWpR 


 ! ## ZcplcFeFeVZL ## 
ZcplcFeFeVZL = 0._dp 
TempcplcFeFeVZL = cplcFeFeVZL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFeVZL(gt2,:) = ZcplcFeFeVZL(gt2,:) + ZRUVec(gt2,gt1)*TempcplcFeFeVZL(gt1,:) 
 End Do 
End Do 
TempcplcFeFeVZL = ZcplcFeFeVZL 
ZcplcFeFeVZL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFeVZL(:,gt2) = ZcplcFeFeVZL(:,gt2) + ZRUVe(gt2,gt1)*TempcplcFeFeVZL(:,gt1) 
 End Do 
End Do 
TempcplcFeFeVZL = ZcplcFeFeVZL 


 ! ## ZcplcFeFeVZR ## 
ZcplcFeFeVZR = 0._dp 
TempcplcFeFeVZR = cplcFeFeVZR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFeVZR(gt2,:) = ZcplcFeFeVZR(gt2,:) + ZRUUe(gt2,gt1)*TempcplcFeFeVZR(gt1,:) 
 End Do 
End Do 
TempcplcFeFeVZR = ZcplcFeFeVZR 
ZcplcFeFeVZR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFeVZR(:,gt2) = ZcplcFeFeVZR(:,gt2) + ZRUUec(gt2,gt1)*TempcplcFeFeVZR(:,gt1) 
 End Do 
End Do 
TempcplcFeFeVZR = ZcplcFeFeVZR 


 ! ## ZcplcFuFuVGL ## 
ZcplcFuFuVGL = 0._dp 
TempcplcFuFuVGL = cplcFuFuVGL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuVGL(gt2,:) = ZcplcFuFuVGL(gt2,:) + ZRUVuc(gt2,gt1)*TempcplcFuFuVGL(gt1,:) 
 End Do 
End Do 
TempcplcFuFuVGL = ZcplcFuFuVGL 
ZcplcFuFuVGL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuVGL(:,gt2) = ZcplcFuFuVGL(:,gt2) + ZRUVu(gt2,gt1)*TempcplcFuFuVGL(:,gt1) 
 End Do 
End Do 
TempcplcFuFuVGL = ZcplcFuFuVGL 


 ! ## ZcplcFuFuVGR ## 
ZcplcFuFuVGR = 0._dp 
TempcplcFuFuVGR = cplcFuFuVGR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuVGR(gt2,:) = ZcplcFuFuVGR(gt2,:) + ZRUUu(gt2,gt1)*TempcplcFuFuVGR(gt1,:) 
 End Do 
End Do 
TempcplcFuFuVGR = ZcplcFuFuVGR 
ZcplcFuFuVGR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuVGR(:,gt2) = ZcplcFuFuVGR(:,gt2) + ZRUUuc(gt2,gt1)*TempcplcFuFuVGR(:,gt1) 
 End Do 
End Do 
TempcplcFuFuVGR = ZcplcFuFuVGR 


 ! ## ZcplcFuFuVPL ## 
ZcplcFuFuVPL = 0._dp 
TempcplcFuFuVPL = cplcFuFuVPL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuVPL(gt2,:) = ZcplcFuFuVPL(gt2,:) + ZRUVuc(gt2,gt1)*TempcplcFuFuVPL(gt1,:) 
 End Do 
End Do 
TempcplcFuFuVPL = ZcplcFuFuVPL 
ZcplcFuFuVPL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuVPL(:,gt2) = ZcplcFuFuVPL(:,gt2) + ZRUVu(gt2,gt1)*TempcplcFuFuVPL(:,gt1) 
 End Do 
End Do 
TempcplcFuFuVPL = ZcplcFuFuVPL 


 ! ## ZcplcFuFuVPR ## 
ZcplcFuFuVPR = 0._dp 
TempcplcFuFuVPR = cplcFuFuVPR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuVPR(gt2,:) = ZcplcFuFuVPR(gt2,:) + ZRUUu(gt2,gt1)*TempcplcFuFuVPR(gt1,:) 
 End Do 
End Do 
TempcplcFuFuVPR = ZcplcFuFuVPR 
ZcplcFuFuVPR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuVPR(:,gt2) = ZcplcFuFuVPR(:,gt2) + ZRUUuc(gt2,gt1)*TempcplcFuFuVPR(:,gt1) 
 End Do 
End Do 
TempcplcFuFuVPR = ZcplcFuFuVPR 


 ! ## ZcplcFuFuVZL ## 
ZcplcFuFuVZL = 0._dp 
TempcplcFuFuVZL = cplcFuFuVZL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuVZL(gt2,:) = ZcplcFuFuVZL(gt2,:) + ZRUVuc(gt2,gt1)*TempcplcFuFuVZL(gt1,:) 
 End Do 
End Do 
TempcplcFuFuVZL = ZcplcFuFuVZL 
ZcplcFuFuVZL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuVZL(:,gt2) = ZcplcFuFuVZL(:,gt2) + ZRUVu(gt2,gt1)*TempcplcFuFuVZL(:,gt1) 
 End Do 
End Do 
TempcplcFuFuVZL = ZcplcFuFuVZL 


 ! ## ZcplcFuFuVZR ## 
ZcplcFuFuVZR = 0._dp 
TempcplcFuFuVZR = cplcFuFuVZR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuVZR(gt2,:) = ZcplcFuFuVZR(gt2,:) + ZRUUu(gt2,gt1)*TempcplcFuFuVZR(gt1,:) 
 End Do 
End Do 
TempcplcFuFuVZR = ZcplcFuFuVZR 
ZcplcFuFuVZR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFuFuVZR(:,gt2) = ZcplcFuFuVZR(:,gt2) + ZRUUuc(gt2,gt1)*TempcplcFuFuVZR(:,gt1) 
 End Do 
End Do 
TempcplcFuFuVZR = ZcplcFuFuVZR 


 ! ## ZcplcFdFucVWpL ## 
ZcplcFdFucVWpL = 0._dp 
TempcplcFdFucVWpL = cplcFdFucVWpL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFucVWpL(gt2,:) = ZcplcFdFucVWpL(gt2,:) + ZRUVdc(gt2,gt1)*TempcplcFdFucVWpL(gt1,:) 
 End Do 
End Do 
TempcplcFdFucVWpL = ZcplcFdFucVWpL 
ZcplcFdFucVWpL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFucVWpL(:,gt2) = ZcplcFdFucVWpL(:,gt2) + ZRUVu(gt2,gt1)*TempcplcFdFucVWpL(:,gt1) 
 End Do 
End Do 
TempcplcFdFucVWpL = ZcplcFdFucVWpL 


 ! ## ZcplcFdFucVWpR ## 
ZcplcFdFucVWpR = 0._dp 
TempcplcFdFucVWpR = cplcFdFucVWpR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFucVWpR(gt2,:) = ZcplcFdFucVWpR(gt2,:) + ZRUUd(gt2,gt1)*TempcplcFdFucVWpR(gt1,:) 
 End Do 
End Do 
TempcplcFdFucVWpR = ZcplcFdFucVWpR 
ZcplcFdFucVWpR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFdFucVWpR(:,gt2) = ZcplcFdFucVWpR(:,gt2) + ZRUUuc(gt2,gt1)*TempcplcFdFucVWpR(:,gt1) 
 End Do 
End Do 
TempcplcFdFucVWpR = ZcplcFdFucVWpR 


 ! ## ZcplcFvFvVZL ## 
ZcplcFvFvVZL = 0._dp 
TempcplcFvFvVZL = cplcFvFvVZL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFvFvVZL(gt2,:) = ZcplcFvFvVZL(gt2,:) + ZRUVvc(gt2,gt1)*TempcplcFvFvVZL(gt1,:) 
 End Do 
End Do 
TempcplcFvFvVZL = ZcplcFvFvVZL 
ZcplcFvFvVZL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFvFvVZL(:,gt2) = ZcplcFvFvVZL(:,gt2) + ZRUVv(gt2,gt1)*TempcplcFvFvVZL(:,gt1) 
 End Do 
End Do 
TempcplcFvFvVZL = ZcplcFvFvVZL 


 ! ## ZcplcFvFvVZR ## 
ZcplcFvFvVZR = 0._dp 
TempcplcFvFvVZR = cplcFvFvVZR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFvFvVZR(gt2,:) = ZcplcFvFvVZR(gt2,:) + ZRUVvr(gt2,gt1)*TempcplcFvFvVZR(gt1,:) 
 End Do 
End Do 
TempcplcFvFvVZR = ZcplcFvFvVZR 
ZcplcFvFvVZR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFvFvVZR(:,gt2) = ZcplcFvFvVZR(:,gt2) + ZRUVvrc(gt2,gt1)*TempcplcFvFvVZR(:,gt1) 
 End Do 
End Do 
TempcplcFvFvVZR = ZcplcFvFvVZR 


 ! ## ZcplcFeFvcVWpL ## 
ZcplcFeFvcVWpL = 0._dp 
TempcplcFeFvcVWpL = cplcFeFvcVWpL 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFvcVWpL(gt2,:) = ZcplcFeFvcVWpL(gt2,:) + ZRUVec(gt2,gt1)*TempcplcFeFvcVWpL(gt1,:) 
 End Do 
End Do 
TempcplcFeFvcVWpL = ZcplcFeFvcVWpL 
ZcplcFeFvcVWpL = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFvcVWpL(:,gt2) = ZcplcFeFvcVWpL(:,gt2) + ZRUVv(gt2,gt1)*TempcplcFeFvcVWpL(:,gt1) 
 End Do 
End Do 
TempcplcFeFvcVWpL = ZcplcFeFvcVWpL 


 ! ## ZcplcFeFvcVWpR ## 
ZcplcFeFvcVWpR = 0._dp 
TempcplcFeFvcVWpR = cplcFeFvcVWpR 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFvcVWpR(gt2,:) = ZcplcFeFvcVWpR(gt2,:) + ZRUUe(gt2,gt1)*TempcplcFeFvcVWpR(gt1,:) 
 End Do 
End Do 
TempcplcFeFvcVWpR = ZcplcFeFvcVWpR 
ZcplcFeFvcVWpR = 0._dp 
Do gt1=1,3
  Do gt2=1,3
ZcplcFeFvcVWpR(:,gt2) = ZcplcFeFvcVWpR(:,gt2) + ZRUVvrc(gt2,gt1)*TempcplcFeFvcVWpR(:,gt1) 
 End Do 
End Do 
TempcplcFeFvcVWpR = ZcplcFeFvcVWpR 


 ! ## ZcplcFxeFxeVPL ## 
ZcplcFxeFxeVPL = 0._dp 
TempcplcFxeFxeVPL = cplcFxeFxeVPL 
ZcplcFxeFxeVPL = TempcplcFxeFxeVPL 


 ! ## ZcplcFxeFxeVPR ## 
ZcplcFxeFxeVPR = 0._dp 
TempcplcFxeFxeVPR = cplcFxeFxeVPR 
ZcplcFxeFxeVPR = TempcplcFxeFxeVPR 


 ! ## ZcplcFxvFxeVWpL ## 
ZcplcFxvFxeVWpL = 0._dp 
TempcplcFxvFxeVWpL = cplcFxvFxeVWpL 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxeVWpL(gt2) = ZcplcFxvFxeVWpL(gt2) + ZRUXVc(gt2,gt1)*TempcplcFxvFxeVWpL(gt1) 
 End Do 
End Do 
TempcplcFxvFxeVWpL = ZcplcFxvFxeVWpL 


 ! ## ZcplcFxvFxeVWpR ## 
ZcplcFxvFxeVWpR = 0._dp 
TempcplcFxvFxeVWpR = cplcFxvFxeVWpR 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxeVWpR(gt2) = ZcplcFxvFxeVWpR(gt2) + ZRUXU(gt2,gt1)*TempcplcFxvFxeVWpR(gt1) 
 End Do 
End Do 
TempcplcFxvFxeVWpR = ZcplcFxvFxeVWpR 


 ! ## ZcplcFxeFxeVZL ## 
ZcplcFxeFxeVZL = 0._dp 
TempcplcFxeFxeVZL = cplcFxeFxeVZL 
ZcplcFxeFxeVZL = TempcplcFxeFxeVZL 


 ! ## ZcplcFxeFxeVZR ## 
ZcplcFxeFxeVZR = 0._dp 
TempcplcFxeFxeVZR = cplcFxeFxeVZR 
ZcplcFxeFxeVZR = TempcplcFxeFxeVZR 


 ! ## ZcplcFxvFxvVZL ## 
ZcplcFxvFxvVZL = 0._dp 
TempcplcFxvFxvVZL = cplcFxvFxvVZL 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxvVZL(gt2,:) = ZcplcFxvFxvVZL(gt2,:) + ZRUXVc(gt2,gt1)*TempcplcFxvFxvVZL(gt1,:) 
 End Do 
End Do 
TempcplcFxvFxvVZL = ZcplcFxvFxvVZL 
ZcplcFxvFxvVZL = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxvVZL(:,gt2) = ZcplcFxvFxvVZL(:,gt2) + ZRUXV(gt2,gt1)*TempcplcFxvFxvVZL(:,gt1) 
 End Do 
End Do 
TempcplcFxvFxvVZL = ZcplcFxvFxvVZL 


 ! ## ZcplcFxvFxvVZR ## 
ZcplcFxvFxvVZR = 0._dp 
TempcplcFxvFxvVZR = cplcFxvFxvVZR 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxvVZR(gt2,:) = ZcplcFxvFxvVZR(gt2,:) + ZRUXU(gt2,gt1)*TempcplcFxvFxvVZR(gt1,:) 
 End Do 
End Do 
TempcplcFxvFxvVZR = ZcplcFxvFxvVZR 
ZcplcFxvFxvVZR = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxvFxvVZR(:,gt2) = ZcplcFxvFxvVZR(:,gt2) + ZRUXUc(gt2,gt1)*TempcplcFxvFxvVZR(:,gt1) 
 End Do 
End Do 
TempcplcFxvFxvVZR = ZcplcFxvFxvVZR 


 ! ## ZcplcFxeFxvcVWpL ## 
ZcplcFxeFxvcVWpL = 0._dp 
TempcplcFxeFxvcVWpL = cplcFxeFxvcVWpL 
ZcplcFxeFxvcVWpL = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxeFxvcVWpL(gt2) = ZcplcFxeFxvcVWpL(gt2) + ZRUXV(gt2,gt1)*TempcplcFxeFxvcVWpL(gt1) 
 End Do 
End Do 
TempcplcFxeFxvcVWpL = ZcplcFxeFxvcVWpL 


 ! ## ZcplcFxeFxvcVWpR ## 
ZcplcFxeFxvcVWpR = 0._dp 
TempcplcFxeFxvcVWpR = cplcFxeFxvcVWpR 
ZcplcFxeFxvcVWpR = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcFxeFxvcVWpR(gt2) = ZcplcFxeFxvcVWpR(gt2) + ZRUXUc(gt2,gt1)*TempcplcFxeFxvcVWpR(gt1) 
 End Do 
End Do 
TempcplcFxeFxvcVWpR = ZcplcFxeFxvcVWpR 


 ! ## ZcplVGVGVGVG1 ## 
ZcplVGVGVGVG1 = 0._dp 


 ! ## ZcplVGVGVGVG2 ## 
ZcplVGVGVGVG2 = 0._dp 


 ! ## ZcplVGVGVGVG3 ## 
ZcplVGVGVGVG3 = 0._dp 


 ! ## ZcplcVWpVPVPVWp1 ## 
ZcplcVWpVPVPVWp1 = 0._dp 


 ! ## ZcplcVWpVPVPVWp2 ## 
ZcplcVWpVPVPVWp2 = 0._dp 


 ! ## ZcplcVWpVPVPVWp3 ## 
ZcplcVWpVPVPVWp3 = 0._dp 


 ! ## ZcplcVWpVPVWpVZ1 ## 
ZcplcVWpVPVWpVZ1 = 0._dp 


 ! ## ZcplcVWpVPVWpVZ2 ## 
ZcplcVWpVPVWpVZ2 = 0._dp 


 ! ## ZcplcVWpVPVWpVZ3 ## 
ZcplcVWpVPVWpVZ3 = 0._dp 


 ! ## ZcplcVWpcVWpVWpVWp1 ## 
ZcplcVWpcVWpVWpVWp1 = 0._dp 


 ! ## ZcplcVWpcVWpVWpVWp2 ## 
ZcplcVWpcVWpVWpVWp2 = 0._dp 


 ! ## ZcplcVWpcVWpVWpVWp3 ## 
ZcplcVWpcVWpVWpVWp3 = 0._dp 


 ! ## ZcplcVWpVWpVZVZ1 ## 
ZcplcVWpVWpVZVZ1 = 0._dp 


 ! ## ZcplcVWpVWpVZVZ2 ## 
ZcplcVWpVWpVZVZ2 = 0._dp 


 ! ## ZcplcVWpVWpVZVZ3 ## 
ZcplcVWpVWpVZVZ3 = 0._dp 


 ! ## ZcplcgGgGVG ## 
ZcplcgGgGVG = 0._dp 
TempcplcgGgGVG = cplcgGgGVG 
ZcplcgGgGVG = TempcplcgGgGVG 


 ! ## ZcplcgWpgAVWp ## 
ZcplcgWpgAVWp = 0._dp 
TempcplcgWpgAVWp = cplcgWpgAVWp 
ZcplcgWpgAVWp = TempcplcgWpgAVWp 


 ! ## ZcplcgWCgAcVWp ## 
ZcplcgWCgAcVWp = 0._dp 
TempcplcgWCgAcVWp = cplcgWCgAcVWp 
ZcplcgWCgAcVWp = TempcplcgWCgAcVWp 


 ! ## ZcplcgWpgWpVP ## 
ZcplcgWpgWpVP = 0._dp 
TempcplcgWpgWpVP = cplcgWpgWpVP 
ZcplcgWpgWpVP = TempcplcgWpgWpVP 


 ! ## ZcplcgWpgWpVZ ## 
ZcplcgWpgWpVZ = 0._dp 
TempcplcgWpgWpVZ = cplcgWpgWpVZ 
ZcplcgWpgWpVZ = TempcplcgWpgWpVZ 


 ! ## ZcplcgAgWpcVWp ## 
ZcplcgAgWpcVWp = 0._dp 
TempcplcgAgWpcVWp = cplcgAgWpcVWp 
ZcplcgAgWpcVWp = TempcplcgAgWpcVWp 


 ! ## ZcplcgZgWpcVWp ## 
ZcplcgZgWpcVWp = 0._dp 
TempcplcgZgWpcVWp = cplcgZgWpcVWp 
ZcplcgZgWpcVWp = TempcplcgZgWpcVWp 


 ! ## ZcplcgWCgWCVP ## 
ZcplcgWCgWCVP = 0._dp 
TempcplcgWCgWCVP = cplcgWCgWCVP 
ZcplcgWCgWCVP = TempcplcgWCgWCVP 


 ! ## ZcplcgAgWCVWp ## 
ZcplcgAgWCVWp = 0._dp 
TempcplcgAgWCVWp = cplcgAgWCVWp 
ZcplcgAgWCVWp = TempcplcgAgWCVWp 


 ! ## ZcplcgZgWCVWp ## 
ZcplcgZgWCVWp = 0._dp 
TempcplcgZgWCVWp = cplcgZgWCVWp 
ZcplcgZgWCVWp = TempcplcgZgWCVWp 


 ! ## ZcplcgWCgWCVZ ## 
ZcplcgWCgWCVZ = 0._dp 
TempcplcgWCgWCVZ = cplcgWCgWCVZ 
ZcplcgWCgWCVZ = TempcplcgWCgWCVZ 


 ! ## ZcplcgWpgZVWp ## 
ZcplcgWpgZVWp = 0._dp 
TempcplcgWpgZVWp = cplcgWpgZVWp 
ZcplcgWpgZVWp = TempcplcgWpgZVWp 


 ! ## ZcplcgWCgZcVWp ## 
ZcplcgWCgZcVWp = 0._dp 
TempcplcgWCgZcVWp = cplcgWCgZcVWp 
ZcplcgWCgZcVWp = TempcplcgWCgZcVWp 


 ! ## ZcplcgWpgWpAh ## 
ZcplcgWpgWpAh = 0._dp 
TempcplcgWpgWpAh = cplcgWpgWpAh 
ZcplcgWpgWpAh = TempcplcgWpgWpAh 


 ! ## ZcplcgWCgWCAh ## 
ZcplcgWCgWCAh = 0._dp 
TempcplcgWCgWCAh = cplcgWCgWCAh 
ZcplcgWCgWCAh = TempcplcgWCgWCAh 


 ! ## ZcplcgZgAhh ## 
ZcplcgZgAhh = 0._dp 
TempcplcgZgAhh = cplcgZgAhh 
ZcplcgZgAhh = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcgZgAhh(gt2) = ZcplcgZgAhh(gt2) + ZRUZH(gt2,gt1)*TempcplcgZgAhh(gt1) 
 End Do 
End Do 


 ! ## ZcplcgWpgAHp ## 
ZcplcgWpgAHp = 0._dp 
TempcplcgWpgAHp = cplcgWpgAHp 
ZcplcgWpgAHp = TempcplcgWpgAHp 


 ! ## ZcplcgWCgAcHp ## 
ZcplcgWCgAcHp = 0._dp 
TempcplcgWCgAcHp = cplcgWCgAcHp 
ZcplcgWCgAcHp = TempcplcgWCgAcHp 


 ! ## ZcplcgWpgWphh ## 
ZcplcgWpgWphh = 0._dp 
TempcplcgWpgWphh = cplcgWpgWphh 
ZcplcgWpgWphh = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcgWpgWphh(gt2) = ZcplcgWpgWphh(gt2) + ZRUZH(gt2,gt1)*TempcplcgWpgWphh(gt1) 
 End Do 
End Do 


 ! ## ZcplcgZgWpcHp ## 
ZcplcgZgWpcHp = 0._dp 
TempcplcgZgWpcHp = cplcgZgWpcHp 
ZcplcgZgWpcHp = TempcplcgZgWpcHp 


 ! ## ZcplcgWCgWChh ## 
ZcplcgWCgWChh = 0._dp 
TempcplcgWCgWChh = cplcgWCgWChh 
ZcplcgWCgWChh = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcgWCgWChh(gt2) = ZcplcgWCgWChh(gt2) + ZRUZH(gt2,gt1)*TempcplcgWCgWChh(gt1) 
 End Do 
End Do 


 ! ## ZcplcgZgWCHp ## 
ZcplcgZgWCHp = 0._dp 
TempcplcgZgWCHp = cplcgZgWCHp 
ZcplcgZgWCHp = TempcplcgZgWCHp 


 ! ## ZcplcgZgZhh ## 
ZcplcgZgZhh = 0._dp 
TempcplcgZgZhh = cplcgZgZhh 
ZcplcgZgZhh = 0._dp 
Do gt1=1,2
  Do gt2=1,2
ZcplcgZgZhh(gt2) = ZcplcgZgZhh(gt2) + ZRUZH(gt2,gt1)*TempcplcgZgZhh(gt1) 
 End Do 
End Do 


 ! ## ZcplcgWpgZHp ## 
ZcplcgWpgZHp = 0._dp 
TempcplcgWpgZHp = cplcgWpgZHp 
ZcplcgWpgZHp = TempcplcgWpgZHp 


 ! ## ZcplcgWCgZcHp ## 
ZcplcgWCgZcHp = 0._dp 
TempcplcgWCgZcHp = cplcgWCgZcHp 
ZcplcgWCgZcHp = TempcplcgWCgZcHp 
End Subroutine  getZCouplings 

Subroutine getGBCouplings(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,MFeOS,               & 
& MFe2OS,MFvOS,MFv2OS,MFxvOS,MFxv2OS,MSscOS,MSsc2OS,MFxeOS,MFxe2OS,MHpOS,MHp2OS,         & 
& MAhOS,MAh2OS,MVZOS,MVZ2OS,MVWpOS,MVWp2OS,ZHOS,ZDLOS,ZDROS,ZULOS,ZUROS,ZELOS,           & 
& ZEROS,UVOS,UVROS,XVOS,XUOS,VSsOS,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,              & 
& MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,           & 
& TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,cplcFdFucVWpL,             & 
& cplcFdFucVWpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFvFeVWpL,      & 
& cplcFvFeVWpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,            & 
& cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,            & 
& ZcplcFdFucVWpL,ZcplcFdFucVWpR,ZcplcFeFvcVWpL,ZcplcFeFvcVWpR,ZcplcFuFdVWpL,             & 
& ZcplcFuFdVWpR,ZcplcFvFeVWpL,ZcplcFvFeVWpR,ZcplcFxeFxvcVWpL,ZcplcFxeFxvcVWpR,           & 
& ZcplcFxvFxeVWpL,ZcplcFxvFxeVWpR,ZcplcHpVPVWp,ZcplcHpVWpVZ,ZcplcVWpVPVWp,               & 
& ZcplcVWpVWpVZ,ZcplhhcHpVWp,ZcplhhcVWpVWp,GcplhhHpcHp,GcplAhHpcVWp,GcplAhcHpVWp,        & 
& GcplhhHpcVWp,GcplhhcHpVWp,GcplHpcHpVP,GcplHpcHpVZ,GcplHpcVWpVP,GcplHpcVWpVZ,           & 
& GcplcHpVPVWp,GcplcHpVWpVZ,GcplcFuFdHpL,GcplcFuFdHpR,GcplcFvFeHpL,GcplcFvFeHpR,         & 
& GcplcFdFucHpL,GcplcFdFucHpR,GcplcFeFvcHpL,GcplcFeFvcHpR,GcplcFxvFxeHpL,GcplcFxvFxeHpR, & 
& GcplcFxeFxvcHpL,GcplcFxeFxvcHpR,GZcplhhHpcHp,GZcplAhHpcVWp,GZcplAhcHpVWp,              & 
& GZcplhhHpcVWp,GZcplhhcHpVWp,GZcplHpcHpVP,GZcplHpcHpVZ,GZcplHpcVWpVP,GZcplHpcVWpVZ,     & 
& GZcplcHpVPVWp,GZcplcHpVWpVZ,GZcplcFuFdHpL,GZcplcFuFdHpR,GZcplcFvFeHpL,GZcplcFvFeHpR,   & 
& GZcplcFdFucHpL,GZcplcFdFucHpR,GZcplcFeFvcHpL,GZcplcFeFvcHpR,GZcplcFxvFxeHpL,           & 
& GZcplcFxvFxeHpR,GZcplcFxeFxvcHpL,GZcplcFxeFxvcHpR,GosZcplhhHpcHp,GosZcplAhHpcVWp,      & 
& GosZcplAhcHpVWp,GosZcplhhHpcVWp,GosZcplhhcHpVWp,GosZcplHpcHpVP,GosZcplHpcHpVZ,         & 
& GosZcplHpcVWpVP,GosZcplHpcVWpVZ,GosZcplcHpVPVWp,GosZcplcHpVWpVZ,GosZcplcFuFdHpL,       & 
& GosZcplcFuFdHpR,GosZcplcFvFeHpL,GosZcplcFvFeHpR,GosZcplcFdFucHpL,GosZcplcFdFucHpR,     & 
& GosZcplcFeFvcHpL,GosZcplcFeFvcHpR,GosZcplcFxvFxeHpL,GosZcplcFxvFxeHpR,GosZcplcFxeFxvcHpL,& 
& GosZcplcFxeFxvcHpR)

Implicit None

Real(dp), Intent(in) :: MhhOS(2),Mhh2OS(2),MFdOS(3),MFd2OS(3),MFuOS(3),MFu2OS(3),MFeOS(3),MFe2OS(3),          & 
& MFvOS(3),MFv2OS(3),MFxvOS(2),MFxv2OS(2),MSscOS(2),MSsc2OS(2),MFxeOS,MFxe2OS,           & 
& MHpOS,MHp2OS,MAhOS,MAh2OS,MVZOS,MVZ2OS,MVWpOS,MVWp2OS,ZHOS(2,2),VSsOS(2,2)

Complex(dp), Intent(in) :: ZDLOS(3,3),ZDROS(3,3),ZULOS(3,3),ZUROS(3,3),ZELOS(3,3),ZEROS(3,3),UVOS(3,3),          & 
& UVROS(3,3),XVOS(2,2),XUOS(2,2)

Real(dp), Intent(in) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp), Intent(in) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Complex(dp), Intent(in) :: cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),          & 
& cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),               & 
& cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),             & 
& cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp(2),cplhhcVWpVWp(2),      & 
& ZcplcFdFucVWpL(3,3),ZcplcFdFucVWpR(3,3),ZcplcFeFvcVWpL(3,3),ZcplcFeFvcVWpR(3,3),       & 
& ZcplcFuFdVWpL(3,3),ZcplcFuFdVWpR(3,3),ZcplcFvFeVWpL(3,3),ZcplcFvFeVWpR(3,3),           & 
& ZcplcFxeFxvcVWpL(2),ZcplcFxeFxvcVWpR(2),ZcplcFxvFxeVWpL(2),ZcplcFxvFxeVWpR(2),         & 
& ZcplcHpVPVWp,ZcplcHpVWpVZ,ZcplcVWpVPVWp,ZcplcVWpVWpVZ,ZcplhhcHpVWp(2),ZcplhhcVWpVWp(2)

Integer :: gt1, gt2, gt3, i1, i2
Complex(dp), Intent(out) :: GcplhhHpcHp(2),GcplAhHpcVWp,GcplAhcHpVWp,GcplhhHpcVWp(2),GcplhhcHpVWp(2),             & 
& GcplHpcHpVP,GcplHpcHpVZ,GcplHpcVWpVP,GcplHpcVWpVZ,GcplcHpVPVWp,GcplcHpVWpVZ,           & 
& GcplcFuFdHpL(3,3),GcplcFuFdHpR(3,3),GcplcFvFeHpL(3,3),GcplcFvFeHpR(3,3),               & 
& GcplcFdFucHpL(3,3),GcplcFdFucHpR(3,3),GcplcFeFvcHpL(3,3),GcplcFeFvcHpR(3,3),           & 
& GcplcFxvFxeHpL(2),GcplcFxvFxeHpR(2),GcplcFxeFxvcHpL(2),GcplcFxeFxvcHpR(2)

Complex(dp), Intent(out) :: GZcplhhHpcHp(2),GZcplAhHpcVWp,GZcplAhcHpVWp,GZcplhhHpcVWp(2),GZcplhhcHpVWp(2),        & 
& GZcplHpcHpVP,GZcplHpcHpVZ,GZcplHpcVWpVP,GZcplHpcVWpVZ,GZcplcHpVPVWp,GZcplcHpVWpVZ,     & 
& GZcplcFuFdHpL(3,3),GZcplcFuFdHpR(3,3),GZcplcFvFeHpL(3,3),GZcplcFvFeHpR(3,3),           & 
& GZcplcFdFucHpL(3,3),GZcplcFdFucHpR(3,3),GZcplcFeFvcHpL(3,3),GZcplcFeFvcHpR(3,3),       & 
& GZcplcFxvFxeHpL(2),GZcplcFxvFxeHpR(2),GZcplcFxeFxvcHpL(2),GZcplcFxeFxvcHpR(2)

Complex(dp), Intent(out) :: GosZcplhhHpcHp(2),GosZcplAhHpcVWp,GosZcplAhcHpVWp,GosZcplhhHpcVWp(2),GosZcplhhcHpVWp(2),& 
& GosZcplHpcHpVP,GosZcplHpcHpVZ,GosZcplHpcVWpVP,GosZcplHpcVWpVZ,GosZcplcHpVPVWp,         & 
& GosZcplcHpVWpVZ,GosZcplcFuFdHpL(3,3),GosZcplcFuFdHpR(3,3),GosZcplcFvFeHpL(3,3),        & 
& GosZcplcFvFeHpR(3,3),GosZcplcFdFucHpL(3,3),GosZcplcFdFucHpR(3,3),GosZcplcFeFvcHpL(3,3),& 
& GosZcplcFeFvcHpR(3,3),GosZcplcFxvFxeHpL(2),GosZcplcFxvFxeHpR(2),GosZcplcFxeFxvcHpL(2), & 
& GosZcplcFxeFxvcHpR(2)

Do i1=1,2
GcplhhHpcHp(i1) = (1)*(Mhh2OS(i1) - MHp2OS)/MVWpOS*cplhhcHpVWp(i1)
GosZcplhhHpcHp(i1) = (1)*(Mhh2OS(i1) - MHp2OS)/MVWpOS*ZcplhhcHpVWp(i1)
GZcplhhHpcHp(i1) = (1)*(Mhh2(i1) - MHp2)/MVWp*ZcplhhcHpVWp(i1)
End Do 
GcplAhHpcVWp = 0._dp 
GosZcplAhHpcVWp = 0._dp
GZcplAhHpcVWp = 0._dp
GcplAhcHpVWp = 0._dp 
GosZcplAhcHpVWp = 0._dp
GZcplAhcHpVWp = 0._dp
Do i1=1,2
GcplhhHpcVWp(i1) = 0.5_dp*(1)/MVWpOS*cplhhcVWpVWp(i1)
GosZcplhhHpcVWp(i1) = 0.5_dp*(1)/MVWpOS*ZcplhhcVWpVWp(i1)
GZcplhhHpcVWp(i1) = 0.5_dp*(1)/MVWp*ZcplhhcVWpVWp(i1)
End Do 
Do i1=1,2
GcplhhcHpVWp(i1) = 0.5_dp*(1)/MVWpOS*cplhhcVWpVWp(i1)
GosZcplhhcHpVWp(i1) = 0.5_dp*(1)/MVWpOS*ZcplhhcVWpVWp(i1)
GZcplhhcHpVWp(i1) = 0.5_dp*(1)/MVWp*ZcplhhcVWpVWp(i1)
End Do 
GcplHpcHpVP = 0.5_dp*(1)/MVWpOS*cplcHpVPVWp
GosZcplHpcHpVP = 0.5_dp*(1)/MVWpOS*ZcplcHpVPVWp
GZcplHpcHpVP = 0.5_dp*(1)/MVWp*ZcplcHpVPVWp
GcplHpcHpVZ = 0.5_dp*(1)/MVWpOS*cplcHpVWpVZ
GosZcplHpcHpVZ = 0.5_dp*(1)/MVWpOS*ZcplcHpVWpVZ
GZcplHpcHpVZ = 0.5_dp*(1)/MVWp*ZcplcHpVWpVZ
GcplHpcVWpVP = (-1)*(MVWp2OS - 0._dp)/MVWpOS*cplcVWpVPVWp
GosZcplHpcVWpVP = (-1)*(MVWp2OS - 0._dp)/MVWpOS*ZcplcVWpVPVWp
GZcplHpcVWpVP = (-1)*(MVWp2 - 0._dp)/MVWpOS*ZcplcVWpVPVWp 
GcplHpcVWpVZ = (1)*(MVWp2OS - MVZ2OS)/MVWpOS*cplcVWpVWpVZ
GosZcplHpcVWpVZ = (1)*(MVWp2OS - MVZ2OS)/MVWpOS*ZcplcVWpVWpVZ
GZcplHpcVWpVZ = (1)*(MVWp2 - MVZ2)/MVWpOS*ZcplcVWpVWpVZ 
GcplcHpVPVWp = (-1)*(0._dp - MVWp2OS)/MVWpOS*cplcVWpVPVWp
GosZcplcHpVPVWp = (-1)*(0._dp - MVWp2OS)/MVWpOS*ZcplcVWpVPVWp
GZcplcHpVPVWp = (-1)*(0._dp - MVWp2)/MVWpOS*ZcplcVWpVPVWp 
GcplcHpVWpVZ = (-1)*(MVWp2OS - MVZ2OS)/MVWpOS*cplcVWpVWpVZ
GosZcplcHpVWpVZ = (-1)*(MVWp2OS - MVZ2OS)/MVWpOS*ZcplcVWpVWpVZ
GZcplcHpVWpVZ = (-1)*(MVWp2 - MVZ2)/MVWpOS*ZcplcVWpVWpVZ 
Do i1=1,3
 Do i2=1,3
GcplcFuFdHpL(i1,i2) = (MFuOS(i1)*cplcFuFdVWpL(i1,i2) - MFdOS(i2)*cplcFuFdVWpR(i1,i2))/MVWpOS
GcplcFuFdHpR(i1,i2) = -(MFdOS(i2)*cplcFuFdVWpL(i1,i2) - MFuOS(i1)*cplcFuFdVWpR(i1,i2))/MVWpOS
GosZcplcFuFdHpL(i1,i2) = (MFuOS(i1)*ZcplcFuFdVWpL(i1,i2) - MFdOS(i2)*ZcplcFuFdVWpR(i1,i2))/MVWpOS
GosZcplcFuFdHpR(i1,i2) = -(MFdOS(i2)*ZcplcFuFdVWpL(i1,i2) - MFuOS(i1)*ZcplcFuFdVWpR(i1,i2))/MVWpOS
GZcplcFuFdHpL(i1,i2) = (MFu(i1)*ZcplcFuFdVWpL(i1,i2) - MFd(i2)*ZcplcFuFdVWpR(i1,i2))/MVWp
GZcplcFuFdHpR(i1,i2) = -(MFd(i2)*ZcplcFuFdVWpL(i1,i2) - MFu(i1)*ZcplcFuFdVWpR(i1,i2))/MVWp
 End Do
End Do 
Do i1=1,3
 Do i2=1,3
GcplcFvFeHpL(i1,i2) = (MFvOS(i1)*cplcFvFeVWpL(i1,i2) - MFeOS(i2)*cplcFvFeVWpR(i1,i2))/MVWpOS
GcplcFvFeHpR(i1,i2) = -(MFeOS(i2)*cplcFvFeVWpL(i1,i2) - MFvOS(i1)*cplcFvFeVWpR(i1,i2))/MVWpOS
GosZcplcFvFeHpL(i1,i2) = (MFvOS(i1)*ZcplcFvFeVWpL(i1,i2) - MFeOS(i2)*ZcplcFvFeVWpR(i1,i2))/MVWpOS
GosZcplcFvFeHpR(i1,i2) = -(MFeOS(i2)*ZcplcFvFeVWpL(i1,i2) - MFvOS(i1)*ZcplcFvFeVWpR(i1,i2))/MVWpOS
GZcplcFvFeHpL(i1,i2) = (MFv(i1)*ZcplcFvFeVWpL(i1,i2) - MFe(i2)*ZcplcFvFeVWpR(i1,i2))/MVWp
GZcplcFvFeHpR(i1,i2) = -(MFe(i2)*ZcplcFvFeVWpL(i1,i2) - MFv(i1)*ZcplcFvFeVWpR(i1,i2))/MVWp
 End Do
End Do 
Do i1=1,3
 Do i2=1,3
GcplcFdFucHpL(i1,i2) = (MFdOS(i1)*cplcFdFucVWpL(i1,i2) - MFuOS(i2)*cplcFdFucVWpR(i1,i2))/MVWpOS
GcplcFdFucHpR(i1,i2) = -(MFuOS(i2)*cplcFdFucVWpL(i1,i2) - MFdOS(i1)*cplcFdFucVWpR(i1,i2))/MVWpOS
GosZcplcFdFucHpL(i1,i2) = (MFdOS(i1)*ZcplcFdFucVWpL(i1,i2) - MFuOS(i2)*ZcplcFdFucVWpR(i1,i2))/MVWpOS
GosZcplcFdFucHpR(i1,i2) = -(MFuOS(i2)*ZcplcFdFucVWpL(i1,i2) - MFdOS(i1)*ZcplcFdFucVWpR(i1,i2))/MVWpOS
GZcplcFdFucHpL(i1,i2) = (MFd(i1)*ZcplcFdFucVWpL(i1,i2) - MFu(i2)*ZcplcFdFucVWpR(i1,i2))/MVWp
GZcplcFdFucHpR(i1,i2) = -(MFu(i2)*ZcplcFdFucVWpL(i1,i2) - MFd(i1)*ZcplcFdFucVWpR(i1,i2))/MVWp
 End Do
End Do 
Do i1=1,3
 Do i2=1,3
GcplcFeFvcHpL(i1,i2) = (MFeOS(i1)*cplcFeFvcVWpL(i1,i2) - MFvOS(i2)*cplcFeFvcVWpR(i1,i2))/MVWpOS
GcplcFeFvcHpR(i1,i2) = -(MFvOS(i2)*cplcFeFvcVWpL(i1,i2) - MFeOS(i1)*cplcFeFvcVWpR(i1,i2))/MVWpOS
GosZcplcFeFvcHpL(i1,i2) = (MFeOS(i1)*ZcplcFeFvcVWpL(i1,i2) - MFvOS(i2)*ZcplcFeFvcVWpR(i1,i2))/MVWpOS
GosZcplcFeFvcHpR(i1,i2) = -(MFvOS(i2)*ZcplcFeFvcVWpL(i1,i2) - MFeOS(i1)*ZcplcFeFvcVWpR(i1,i2))/MVWpOS
GZcplcFeFvcHpL(i1,i2) = (MFe(i1)*ZcplcFeFvcVWpL(i1,i2) - MFv(i2)*ZcplcFeFvcVWpR(i1,i2))/MVWp
GZcplcFeFvcHpR(i1,i2) = -(MFv(i2)*ZcplcFeFvcVWpL(i1,i2) - MFe(i1)*ZcplcFeFvcVWpR(i1,i2))/MVWp
 End Do
End Do 
Do i1=1,2
GcplcFxvFxeHpL(i1) = (MFxvOS(i1)*cplcFxvFxeVWpL(i1) - MFxeOS*cplcFxvFxeVWpR(i1))/MVWpOS
GcplcFxvFxeHpR(i1) = -(MFxeOS*cplcFxvFxeVWpL(i1) - MFxvOS(i1)*cplcFxvFxeVWpR(i1))/MVWpOS
GosZcplcFxvFxeHpL(i1) = (MFxvOS(i1)*ZcplcFxvFxeVWpL(i1) - MFxeOS*ZcplcFxvFxeVWpR(i1))/MVWpOS
GosZcplcFxvFxeHpR(i1) = -(MFxeOS*ZcplcFxvFxeVWpL(i1) - MFxvOS(i1)*ZcplcFxvFxeVWpR(i1))/MVWpOS
GZcplcFxvFxeHpL(i1) = (MFxv(i1)*ZcplcFxvFxeVWpL(i1) - MFxe*ZcplcFxvFxeVWpR(i1))/MVWp
GZcplcFxvFxeHpR(i1) = -(MFxe*ZcplcFxvFxeVWpL(i1) - MFxv(i1)*ZcplcFxvFxeVWpR(i1))/MVWp
End Do 
 Do i2=1,2
GcplcFxeFxvcHpL(i2) = (MFxeOS*cplcFxeFxvcVWpL(i2) - MFxvOS(i2)*cplcFxeFxvcVWpR(i2))/MVWpOS
GcplcFxeFxvcHpR(i2) = -(MFxvOS(i2)*cplcFxeFxvcVWpL(i2) - MFxeOS*cplcFxeFxvcVWpR(i2))/MVWpOS
GosZcplcFxeFxvcHpL(i2) = (MFxeOS*ZcplcFxeFxvcVWpL(i2) - MFxvOS(i2)*ZcplcFxeFxvcVWpR(i2))/MVWpOS
GosZcplcFxeFxvcHpR(i2) = -(MFxvOS(i2)*ZcplcFxeFxvcVWpL(i2) - MFxeOS*ZcplcFxeFxvcVWpR(i2))/MVWpOS
GZcplcFxeFxvcHpL(i2) = (MFxe*ZcplcFxeFxvcVWpL(i2) - MFxv(i2)*ZcplcFxeFxvcVWpR(i2))/MVWp
GZcplcFxeFxvcHpR(i2) = -(MFxv(i2)*ZcplcFxeFxvcVWpL(i2) - MFxe*ZcplcFxeFxvcVWpR(i2))/MVWp
 End Do
End Subroutine  getGBCouplings 

Subroutine WaveFunctionRenormalisation(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,               & 
& MFu2OS,MFeOS,MFe2OS,MFvOS,MFv2OS,MFxvOS,MFxv2OS,MSscOS,MSsc2OS,MFxeOS,MFxe2OS,         & 
& MHpOS,MHp2OS,MAhOS,MAh2OS,MVZOS,MVZ2OS,MVWpOS,MVWp2OS,ZHOS,ZDLOS,ZDROS,ZULOS,          & 
& ZUROS,ZELOS,ZEROS,UVOS,UVROS,XVOS,XUOS,VSsOS,MAh,MAh2,MFd,MFd2,MFe,MFe2,               & 
& MFu,MFu2,MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,             & 
& MVWp2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,            & 
& g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS,cplAhAhhh,cplhhhhhh,cplhhHpcHp,cplhhSsccSsc,            & 
& cplAhAhAhAh,cplAhAhhhhh,cplAhAhHpcHp,cplAhAhSsccSsc,cplhhhhhhhh,cplhhhhHpcHp,          & 
& cplhhhhSsccSsc,cplHpHpcHpcHp,cplHpSsccHpcSsc,cplSscSsccSsccSsc,cplAhhhVZ,              & 
& cplAhHpcVWp,cplAhcHpVWp,cplhhHpcVWp,cplhhcHpVWp,cplHpcHpVP,cplHpcHpVZ,cplhhcVWpVWp,    & 
& cplhhVZVZ,cplHpcVWpVP,cplHpcVWpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplAhAhcVWpVWp,              & 
& cplAhAhVZVZ,cplAhHpcVWpVP,cplAhHpcVWpVZ,cplAhcHpVPVWp,cplAhcHpVWpVZ,cplhhhhcVWpVWp,    & 
& cplhhhhVZVZ,cplhhHpcVWpVP,cplhhHpcVWpVZ,cplhhcHpVPVWp,cplhhcHpVWpVZ,cplHpcHpVPVP,      & 
& cplHpcHpVPVZ,cplHpcHpcVWpVWp,cplHpcHpVZVZ,cplVGVGVG,cplcVWpVPVWp,cplcVWpVWpVZ,         & 
& cplcFdFdAhL,cplcFdFdAhR,cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,               & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,           & 
& cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,           & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFdFucHpL,cplcFdFucHpR,cplcFxvFvSscL,cplcFxvFvSscR,         & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFeFxecSscL,cplcFeFxecSscR,   & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,              & 
& cplcFxeFxvcHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,           & 
& cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,             & 
& cplcFvFeVWpR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,              & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucVWpL,cplcFdFucVWpR,cplcFvFvVZL,           & 
& cplcFvFvVZR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,    & 
& cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFxeFxvcVWpL,& 
& cplcFxeFxvcVWpR,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,& 
& cplcVWpVPVPVWp3,cplcVWpVPVWpVZ1,cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpcVWpVWpVWp1,    & 
& cplcVWpcVWpVWpVWp2,cplcVWpcVWpVWpVWp3,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3, & 
& cplcgGgGVG,cplcgWpgAVWp,cplcgWCgAcVWp,cplcgWpgWpVP,cplcgWpgWpVZ,cplcgAgWpcVWp,         & 
& cplcgZgWpcVWp,cplcgWCgWCVP,cplcgAgWCVWp,cplcgZgWCVWp,cplcgWCgWCVZ,cplcgWpgZVWp,        & 
& cplcgWCgZcVWp,cplcgWpgWpAh,cplcgWCgWCAh,cplcgZgAhh,cplcgWpgAHp,cplcgWCgAcHp,           & 
& cplcgWpgWphh,cplcgZgWpcHp,cplcgWCgWChh,cplcgZgWCHp,cplcgZgZhh,cplcgWpgZHp,             & 
& cplcgWCgZcHp,GcplhhHpcHp,GcplAhHpcVWp,GcplAhcHpVWp,GcplhhHpcVWp,GcplhhcHpVWp,          & 
& GcplHpcHpVP,GcplHpcHpVZ,GcplHpcVWpVP,GcplHpcVWpVZ,GcplcHpVPVWp,GcplcHpVWpVZ,           & 
& GcplcFuFdHpL,GcplcFuFdHpR,GcplcFvFeHpL,GcplcFvFeHpR,GcplcFdFucHpL,GcplcFdFucHpR,       & 
& GcplcFeFvcHpL,GcplcFeFvcHpR,GcplcFxvFxeHpL,GcplcFxvFxeHpR,GcplcFxeFxvcHpL,             & 
& GcplcFxeFxvcHpR,dg1,dg2,dg3,dYu,dYd,dYe,dm2SM,dLam,dMDF,dYRD,dMS12,dMS22,              & 
& dLS1H,dLS,dLS2H,dmP2,dLSP,dLSPH,dYRB1,dYRB2,dYRC,dYRA1,dYRA2,dvvSM,dvS,dZH,            & 
& dZDL,dZDR,dZUL,dZUR,dZEL,dZER,dUV,dUVR,dXV,dXU,dVSs,dSinTW,dCosTW,dTanTW,              & 
& ZfVG,ZfHp,Zfed,Zfeu,ZfAh,ZfVP,ZfVZ,ZfVWp,Zfhh,ZfDL,ZfDR,ZfUL,ZfUR,ZfEL,ZfER,           & 
& ZfVL,ZfVR,ZfxVL,ZfxVR,ZfSsc,ZfVPVZ,ZfVZVP,ctcplAhAhhh,ctcplhhhhhh,ctcplhhHpcHp,        & 
& ctcplhhSsccSsc,ctcplAhhhVZ,ctcplAhHpcVWp,ctcplAhcHpVWp,ctcplhhHpcVWp,ctcplhhcHpVWp,    & 
& ctcplHpcHpVP,ctcplHpcHpVZ,ctcplhhcVWpVWp,ctcplhhVZVZ,ctcplHpcVWpVP,ctcplHpcVWpVZ,      & 
& ctcplcHpVPVWp,ctcplcHpVWpVZ,ctcplVGVGVG,ctcplcVWpVPVWp,ctcplcVWpVWpVZ,ctcplcFdFdAhL,   & 
& ctcplcFdFdAhR,ctcplcFeFeAhL,ctcplcFeFeAhR,ctcplcFuFuAhL,ctcplcFuFuAhR,ctcplcFxvFxvAhL, & 
& ctcplcFxvFxvAhR,ctcplcFdFdhhL,ctcplcFdFdhhR,ctcplcFuFdHpL,ctcplcFuFdHpR,               & 
& ctcplcFeFehhL,ctcplcFeFehhR,ctcplcFvFeHpL,ctcplcFvFeHpR,ctcplcFxeFeSscL,               & 
& ctcplcFxeFeSscR,ctcplcFuFuhhL,ctcplcFuFuhhR,ctcplcFdFucHpL,ctcplcFdFucHpR,             & 
& ctcplcFxvFvSscL,ctcplcFxvFvSscR,ctcplcFeFvcHpL,ctcplcFeFvcHpR,ctcplcFxvFxeHpL,         & 
& ctcplcFxvFxeHpR,ctcplcFeFxecSscL,ctcplcFeFxecSscR,ctcplcFxvFxvhhL,ctcplcFxvFxvhhR,     & 
& ctcplcFvFxvcSscL,ctcplcFvFxvcSscR,ctcplcFxeFxvcHpL,ctcplcFxeFxvcHpR,ctcplcFdFdVGL,     & 
& ctcplcFdFdVGR,ctcplcFdFdVPL,ctcplcFdFdVPR,ctcplcFuFdVWpL,ctcplcFuFdVWpR,               & 
& ctcplcFdFdVZL,ctcplcFdFdVZR,ctcplcFeFeVPL,ctcplcFeFeVPR,ctcplcFvFeVWpL,ctcplcFvFeVWpR, & 
& ctcplcFeFeVZL,ctcplcFeFeVZR,ctcplcFuFuVGL,ctcplcFuFuVGR,ctcplcFuFuVPL,ctcplcFuFuVPR,   & 
& ctcplcFuFuVZL,ctcplcFuFuVZR,ctcplcFdFucVWpL,ctcplcFdFucVWpR,ctcplcFvFvVZL,             & 
& ctcplcFvFvVZR,ctcplcFeFvcVWpL,ctcplcFeFvcVWpR,ctcplcFxeFxeVPL,ctcplcFxeFxeVPR,         & 
& ctcplcFxvFxeVWpL,ctcplcFxvFxeVWpR,ctcplcFxeFxeVZL,ctcplcFxeFxeVZR,ctcplcFxvFxvVZL,     & 
& ctcplcFxvFxvVZR,ctcplcFxeFxvcVWpL,ctcplcFxeFxvcVWpR,MLambda,deltaM,kont)

Implicit None 
Real(dp),Intent(inout) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2

Complex(dp),Intent(inout) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Real(dp),Intent(inout) :: vvSM,vS

Complex(dp),Intent(in) :: cplAhAhhh(2),cplhhhhhh(2,2,2),cplhhHpcHp(2),cplhhSsccSsc(2,2,2),cplAhAhAhAh,          & 
& cplAhAhhhhh(2,2),cplAhAhHpcHp,cplAhAhSsccSsc(2,2),cplhhhhhhhh(2,2,2,2),cplhhhhHpcHp(2,2),& 
& cplhhhhSsccSsc(2,2,2,2),cplHpHpcHpcHp,cplHpSsccHpcSsc(2,2),cplSscSsccSsccSsc(2,2,2,2), & 
& cplAhhhVZ(2),cplAhHpcVWp,cplAhcHpVWp,cplhhHpcVWp(2),cplhhcHpVWp(2),cplHpcHpVP,         & 
& cplHpcHpVZ,cplhhcVWpVWp(2),cplhhVZVZ(2),cplHpcVWpVP,cplHpcVWpVZ,cplcHpVPVWp,           & 
& cplcHpVWpVZ,cplAhAhcVWpVWp,cplAhAhVZVZ,cplAhHpcVWpVP,cplAhHpcVWpVZ,cplAhcHpVPVWp,      & 
& cplAhcHpVWpVZ,cplhhhhcVWpVWp(2,2),cplhhhhVZVZ(2,2),cplhhHpcVWpVP(2),cplhhHpcVWpVZ(2),  & 
& cplhhcHpVPVWp(2),cplhhcHpVWpVZ(2),cplHpcHpVPVP,cplHpcHpVPVZ,cplHpcHpcVWpVWp,           & 
& cplHpcHpVZVZ,cplVGVGVG,cplcVWpVPVWp,cplcVWpVWpVZ,cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),    & 
& cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFxvFxvAhL(2,2),& 
& cplcFxvFxvAhR(2,2),cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFuFdHpL(3,3),             & 
& cplcFuFdHpR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFvFeHpL(3,3),               & 
& cplcFvFeHpR(3,3),cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFuFuhhL(3,3,2),             & 
& cplcFuFuhhR(3,3,2),cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFxvFvSscL(2,3,2),           & 
& cplcFxvFvSscR(2,3,2),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFxvFxeHpL(2),             & 
& cplcFxvFxeHpR(2),cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFxvFxvhhL(2,2,2),         & 
& cplcFxvFxvhhR(2,2,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),cplcFxeFxvcHpL(2),    & 
& cplcFxeFxvcHpR(2),cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3), & 
& cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVPL(3,3),& 
& cplcFeFeVPR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),& 
& cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),  & 
& cplcFuFuVZR(3,3),cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),cplcFvFvVZL(3,3),               & 
& cplcFvFvVZR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFxeFxeVPL,cplcFxeFxeVPR,    & 
& cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvVZL(2,2),    & 
& cplcFxvFxvVZR(2,2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplVGVGVGVG1,cplVGVGVGVG2,    & 
& cplVGVGVGVG3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,cplcVWpVPVPVWp3,cplcVWpVPVWpVZ1,          & 
& cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpcVWpVWpVWp1,cplcVWpcVWpVWpVWp2,cplcVWpcVWpVWpVWp3,& 
& cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,cplcgGgGVG,cplcgWpgAVWp,               & 
& cplcgWCgAcVWp,cplcgWpgWpVP,cplcgWpgWpVZ,cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgWCVP,      & 
& cplcgAgWCVWp,cplcgZgWCVWp,cplcgWCgWCVZ,cplcgWpgZVWp,cplcgWCgZcVWp,cplcgWpgWpAh,        & 
& cplcgWCgWCAh,cplcgZgAhh(2),cplcgWpgAHp,cplcgWCgAcHp,cplcgWpgWphh(2),cplcgZgWpcHp,      & 
& cplcgWCgWChh(2),cplcgZgWCHp,cplcgZgZhh(2),cplcgWpgZHp,cplcgWCgZcHp

Real(dp),Intent(in) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp),Intent(in) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Real(dp),Intent(in) :: MhhOS(2),Mhh2OS(2),MFdOS(3),MFd2OS(3),MFuOS(3),MFu2OS(3),MFeOS(3),MFe2OS(3),          & 
& MFvOS(3),MFv2OS(3),MFxvOS(2),MFxv2OS(2),MSscOS(2),MSsc2OS(2),MFxeOS,MFxe2OS,           & 
& MHpOS,MHp2OS,MAhOS,MAh2OS,MVZOS,MVZ2OS,MVWpOS,MVWp2OS,ZHOS(2,2),VSsOS(2,2)

Complex(dp),Intent(in) :: ZDLOS(3,3),ZDROS(3,3),ZULOS(3,3),ZUROS(3,3),ZELOS(3,3),ZEROS(3,3),UVOS(3,3),          & 
& UVROS(3,3),XVOS(2,2),XUOS(2,2)

Complex(dp) :: Pihh(2,2,2),DerPihh(2,2,2),SigmaLFd(3,3,3),SigmaSLFd(3,3,3),SigmaSRFd(3,3,3),         & 
& SigmaRFd(3,3,3),DerSigmaLFd(3,3,3),DerSigmaSLFd(3,3,3),DerSigmaSRFd(3,3,3),            & 
& DerSigmaRFd(3,3,3),DerSigmaLirFd(3,3,3),DerSigmaSLirFd(3,3,3),DerSigmaSRirFd(3,3,3),   & 
& DerSigmaRirFd(3,3,3),SigmaLFu(3,3,3),SigmaSLFu(3,3,3),SigmaSRFu(3,3,3),SigmaRFu(3,3,3),& 
& DerSigmaLFu(3,3,3),DerSigmaSLFu(3,3,3),DerSigmaSRFu(3,3,3),DerSigmaRFu(3,3,3),         & 
& DerSigmaLirFu(3,3,3),DerSigmaSLirFu(3,3,3),DerSigmaSRirFu(3,3,3),DerSigmaRirFu(3,3,3), & 
& SigmaLFe(3,3,3),SigmaSLFe(3,3,3),SigmaSRFe(3,3,3),SigmaRFe(3,3,3),DerSigmaLFe(3,3,3),  & 
& DerSigmaSLFe(3,3,3),DerSigmaSRFe(3,3,3),DerSigmaRFe(3,3,3),DerSigmaLirFe(3,3,3),       & 
& DerSigmaSLirFe(3,3,3),DerSigmaSRirFe(3,3,3),DerSigmaRirFe(3,3,3),SigmaLFv(3,3,3),      & 
& SigmaSLFv(3,3,3),SigmaSRFv(3,3,3),SigmaRFv(3,3,3),DerSigmaLFv(3,3,3),DerSigmaSLFv(3,3,3),& 
& DerSigmaSRFv(3,3,3),DerSigmaRFv(3,3,3),DerSigmaLirFv(3,3,3),DerSigmaSLirFv(3,3,3),     & 
& DerSigmaSRirFv(3,3,3),DerSigmaRirFv(3,3,3),SigmaLFxv(2,2,2),SigmaSLFxv(2,2,2),         & 
& SigmaSRFxv(2,2,2),SigmaRFxv(2,2,2),DerSigmaLFxv(2,2,2),DerSigmaSLFxv(2,2,2),           & 
& DerSigmaSRFxv(2,2,2),DerSigmaRFxv(2,2,2),DerSigmaLirFxv(2,2,2),DerSigmaSLirFxv(2,2,2), & 
& DerSigmaSRirFxv(2,2,2),DerSigmaRirFxv(2,2,2),PiSsc(2,2,2),DerPiSsc(2,2,2),             & 
& SigmaLFxe,SigmaSLFxe,SigmaSRFxe,SigmaRFxe,DerSigmaLFxe,DerSigmaSLFxe,DerSigmaSRFxe,    & 
& DerSigmaRFxe,DerSigmaLirFxe,DerSigmaSLirFxe,DerSigmaSRirFxe,DerSigmaRirFxe,            & 
& PiHp,DerPiHp,PiAh,DerPiAh,PiVG,DerPiVG,PiVP,DerPiVP,PiVZ,DerPiVZ,PiVWp,DerPiVWp,       & 
& PiVPlight0,DerPiVPlight0,PiVPheavy0,DerPiVPheavy0,PiVPlightMZ,DerPiVPlightMZ,          & 
& PiVPheavyMZ,DerPiVPheavyMZ,PiVPVZ,DerPiVPVZ,PiVZVP,DerPiVZVP,PiVZAh,DerPiVZAh,         & 
& PiAhVZ,DerPiAhVZ,PiVZhh(2,2,2),DerPiVZhh(2,2,2),PihhVZ(2,2,2),DerPihhVZ(2,2,2),        & 
& PiVWpHp,DerPiVWpHp,PiHpVWp,DerPiHpVWp

Complex(dp) :: PihhDR(2,2,2),DerPihhDR(2,2,2),SigmaLFdDR(3,3,3),SigmaSLFdDR(3,3,3),SigmaSRFdDR(3,3,3),& 
& SigmaRFdDR(3,3,3),DerSigmaLFdDR(3,3,3),DerSigmaSLFdDR(3,3,3),DerSigmaSRFdDR(3,3,3),    & 
& DerSigmaRFdDR(3,3,3),DerSigmaLirFdDR(3,3,3),DerSigmaSLirFdDR(3,3,3),DerSigmaSRirFdDR(3,3,3),& 
& DerSigmaRirFdDR(3,3,3),SigmaLFuDR(3,3,3),SigmaSLFuDR(3,3,3),SigmaSRFuDR(3,3,3),        & 
& SigmaRFuDR(3,3,3),DerSigmaLFuDR(3,3,3),DerSigmaSLFuDR(3,3,3),DerSigmaSRFuDR(3,3,3),    & 
& DerSigmaRFuDR(3,3,3),DerSigmaLirFuDR(3,3,3),DerSigmaSLirFuDR(3,3,3),DerSigmaSRirFuDR(3,3,3),& 
& DerSigmaRirFuDR(3,3,3),SigmaLFeDR(3,3,3),SigmaSLFeDR(3,3,3),SigmaSRFeDR(3,3,3),        & 
& SigmaRFeDR(3,3,3),DerSigmaLFeDR(3,3,3),DerSigmaSLFeDR(3,3,3),DerSigmaSRFeDR(3,3,3),    & 
& DerSigmaRFeDR(3,3,3),DerSigmaLirFeDR(3,3,3),DerSigmaSLirFeDR(3,3,3),DerSigmaSRirFeDR(3,3,3),& 
& DerSigmaRirFeDR(3,3,3),SigmaLFvDR(3,3,3),SigmaSLFvDR(3,3,3),SigmaSRFvDR(3,3,3),        & 
& SigmaRFvDR(3,3,3),DerSigmaLFvDR(3,3,3),DerSigmaSLFvDR(3,3,3),DerSigmaSRFvDR(3,3,3),    & 
& DerSigmaRFvDR(3,3,3),DerSigmaLirFvDR(3,3,3),DerSigmaSLirFvDR(3,3,3),DerSigmaSRirFvDR(3,3,3),& 
& DerSigmaRirFvDR(3,3,3),SigmaLFxvDR(2,2,2),SigmaSLFxvDR(2,2,2),SigmaSRFxvDR(2,2,2),     & 
& SigmaRFxvDR(2,2,2),DerSigmaLFxvDR(2,2,2),DerSigmaSLFxvDR(2,2,2),DerSigmaSRFxvDR(2,2,2),& 
& DerSigmaRFxvDR(2,2,2),DerSigmaLirFxvDR(2,2,2),DerSigmaSLirFxvDR(2,2,2),DerSigmaSRirFxvDR(2,2,2),& 
& DerSigmaRirFxvDR(2,2,2),PiSscDR(2,2,2),DerPiSscDR(2,2,2),SigmaLFxeDR,SigmaSLFxeDR,     & 
& SigmaSRFxeDR,SigmaRFxeDR,DerSigmaLFxeDR,DerSigmaSLFxeDR,DerSigmaSRFxeDR,               & 
& DerSigmaRFxeDR,DerSigmaLirFxeDR,DerSigmaSLirFxeDR,DerSigmaSRirFxeDR,DerSigmaRirFxeDR,  & 
& PiHpDR,DerPiHpDR,PiAhDR,DerPiAhDR,PiVGDR,DerPiVGDR,PiVPDR,DerPiVPDR,PiVZDR,            & 
& DerPiVZDR,PiVWpDR,DerPiVWpDR,PiVPlight0DR,DerPiVPlight0DR,PiVPheavy0DR,DerPiVPheavy0DR,& 
& PiVPlightMZDR,DerPiVPlightMZDR,PiVPheavyMZDR,DerPiVPheavyMZDR,PiVPVZDR,DerPiVPVZDR,    & 
& PiVZVPDR,DerPiVZVPDR,PiVZAhDR,DerPiVZAhDR,PiAhVZDR,DerPiAhVZDR,PiVZhhDR(2,2,2),        & 
& DerPiVZhhDR(2,2,2),PihhVZDR(2,2,2),DerPihhVZDR(2,2,2),PiVWpHpDR,DerPiVWpHpDR,          & 
& PiHpVWpDR,DerPiHpVWpDR

Complex(dp) :: PihhOS(2,2,2),DerPihhOS(2,2,2),SigmaLFdOS(3,3,3),SigmaSLFdOS(3,3,3),SigmaSRFdOS(3,3,3),& 
& SigmaRFdOS(3,3,3),DerSigmaLFdOS(3,3,3),DerSigmaSLFdOS(3,3,3),DerSigmaSRFdOS(3,3,3),    & 
& DerSigmaRFdOS(3,3,3),DerSigmaLirFdOS(3,3,3),DerSigmaSLirFdOS(3,3,3),DerSigmaSRirFdOS(3,3,3),& 
& DerSigmaRirFdOS(3,3,3),SigmaLFuOS(3,3,3),SigmaSLFuOS(3,3,3),SigmaSRFuOS(3,3,3),        & 
& SigmaRFuOS(3,3,3),DerSigmaLFuOS(3,3,3),DerSigmaSLFuOS(3,3,3),DerSigmaSRFuOS(3,3,3),    & 
& DerSigmaRFuOS(3,3,3),DerSigmaLirFuOS(3,3,3),DerSigmaSLirFuOS(3,3,3),DerSigmaSRirFuOS(3,3,3),& 
& DerSigmaRirFuOS(3,3,3),SigmaLFeOS(3,3,3),SigmaSLFeOS(3,3,3),SigmaSRFeOS(3,3,3),        & 
& SigmaRFeOS(3,3,3),DerSigmaLFeOS(3,3,3),DerSigmaSLFeOS(3,3,3),DerSigmaSRFeOS(3,3,3),    & 
& DerSigmaRFeOS(3,3,3),DerSigmaLirFeOS(3,3,3),DerSigmaSLirFeOS(3,3,3),DerSigmaSRirFeOS(3,3,3),& 
& DerSigmaRirFeOS(3,3,3),SigmaLFvOS(3,3,3),SigmaSLFvOS(3,3,3),SigmaSRFvOS(3,3,3),        & 
& SigmaRFvOS(3,3,3),DerSigmaLFvOS(3,3,3),DerSigmaSLFvOS(3,3,3),DerSigmaSRFvOS(3,3,3),    & 
& DerSigmaRFvOS(3,3,3),DerSigmaLirFvOS(3,3,3),DerSigmaSLirFvOS(3,3,3),DerSigmaSRirFvOS(3,3,3),& 
& DerSigmaRirFvOS(3,3,3),SigmaLFxvOS(2,2,2),SigmaSLFxvOS(2,2,2),SigmaSRFxvOS(2,2,2),     & 
& SigmaRFxvOS(2,2,2),DerSigmaLFxvOS(2,2,2),DerSigmaSLFxvOS(2,2,2),DerSigmaSRFxvOS(2,2,2),& 
& DerSigmaRFxvOS(2,2,2),DerSigmaLirFxvOS(2,2,2),DerSigmaSLirFxvOS(2,2,2),DerSigmaSRirFxvOS(2,2,2),& 
& DerSigmaRirFxvOS(2,2,2),PiSscOS(2,2,2),DerPiSscOS(2,2,2),SigmaLFxeOS,SigmaSLFxeOS,     & 
& SigmaSRFxeOS,SigmaRFxeOS,DerSigmaLFxeOS,DerSigmaSLFxeOS,DerSigmaSRFxeOS,               & 
& DerSigmaRFxeOS,DerSigmaLirFxeOS,DerSigmaSLirFxeOS,DerSigmaSRirFxeOS,DerSigmaRirFxeOS,  & 
& PiHpOS,DerPiHpOS,PiAhOS,DerPiAhOS,PiVGOS,DerPiVGOS,PiVPOS,DerPiVPOS,PiVZOS,            & 
& DerPiVZOS,PiVWpOS,DerPiVWpOS,PiVPlight0OS,DerPiVPlight0OS,PiVPheavy0OS,DerPiVPheavy0OS,& 
& PiVPlightMZOS,DerPiVPlightMZOS,PiVPheavyMZOS,DerPiVPheavyMZOS,PiVPVZOS,DerPiVPVZOS,    & 
& PiVZVPOS,DerPiVZVPOS,PiVZAhOS,DerPiVZAhOS,PiAhVZOS,DerPiAhVZOS,PiVZhhOS(2,2,2),        & 
& DerPiVZhhOS(2,2,2),PihhVZOS(2,2,2),DerPihhVZOS(2,2,2),PiVWpHpOS,DerPiVWpHpOS,          & 
& PiHpVWpOS,DerPiHpVWpOS

Real(dp), Intent(in) :: MLambda, deltaM 

Integer, Intent(out) :: kont 
Real(dp),Intent(out) :: dg1,dg2,dg3,dm2SM,dMDF,dYRD,dMS12,dMS22,dLS1H,dLS,dLS2H,dmP2,dLSP,dLSPH,              & 
& dYRB1(3),dYRB2(3),dYRC,dYRA1(3),dYRA2(3),dvvSM,dvS,dZH(2,2),dVSs(2,2),dSinTW,          & 
& dCosTW,dTanTW

Complex(dp),Intent(out) :: dYu(3,3),dYd(3,3),dYe(3,3),dLam,dZDL(3,3),dZDR(3,3),dZUL(3,3),dZUR(3,3),              & 
& dZEL(3,3),dZER(3,3),dUV(3,3),dUVR(3,3),dXV(2,2),dXU(2,2)

Complex(dp),Intent(out) :: ZfVG,ZfHp,Zfed,Zfeu,ZfAh,ZfVP,ZfVZ,ZfVWp,Zfhh(2,2),ZfDL(3,3),ZfDR(3,3),               & 
& ZfUL(3,3),ZfUR(3,3),ZfEL(3,3),ZfER(3,3),ZfVL(3,3),ZfVR(3,3),ZfxVL(2,2),ZfxVR(2,2),     & 
& ZfSsc(2,2),ZfVPVZ,ZfVZVP

Complex(dp),Intent(out) :: ctcplAhAhhh(2),ctcplhhhhhh(2,2,2),ctcplhhHpcHp(2),ctcplhhSsccSsc(2,2,2),              & 
& ctcplAhhhVZ(2),ctcplAhHpcVWp,ctcplAhcHpVWp,ctcplhhHpcVWp(2),ctcplhhcHpVWp(2),          & 
& ctcplHpcHpVP,ctcplHpcHpVZ,ctcplhhcVWpVWp(2),ctcplhhVZVZ(2),ctcplHpcVWpVP,              & 
& ctcplHpcVWpVZ,ctcplcHpVPVWp,ctcplcHpVWpVZ,ctcplVGVGVG,ctcplcVWpVPVWp,ctcplcVWpVWpVZ,   & 
& ctcplcFdFdAhL(3,3),ctcplcFdFdAhR(3,3),ctcplcFeFeAhL(3,3),ctcplcFeFeAhR(3,3),           & 
& ctcplcFuFuAhL(3,3),ctcplcFuFuAhR(3,3),ctcplcFxvFxvAhL(2,2),ctcplcFxvFxvAhR(2,2),       & 
& ctcplcFdFdhhL(3,3,2),ctcplcFdFdhhR(3,3,2),ctcplcFuFdHpL(3,3),ctcplcFuFdHpR(3,3),       & 
& ctcplcFeFehhL(3,3,2),ctcplcFeFehhR(3,3,2),ctcplcFvFeHpL(3,3),ctcplcFvFeHpR(3,3),       & 
& ctcplcFxeFeSscL(3,2),ctcplcFxeFeSscR(3,2),ctcplcFuFuhhL(3,3,2),ctcplcFuFuhhR(3,3,2),   & 
& ctcplcFdFucHpL(3,3),ctcplcFdFucHpR(3,3),ctcplcFxvFvSscL(2,3,2),ctcplcFxvFvSscR(2,3,2), & 
& ctcplcFeFvcHpL(3,3),ctcplcFeFvcHpR(3,3),ctcplcFxvFxeHpL(2),ctcplcFxvFxeHpR(2),         & 
& ctcplcFeFxecSscL(3,2),ctcplcFeFxecSscR(3,2),ctcplcFxvFxvhhL(2,2,2),ctcplcFxvFxvhhR(2,2,2),& 
& ctcplcFvFxvcSscL(3,2,2),ctcplcFvFxvcSscR(3,2,2),ctcplcFxeFxvcHpL(2),ctcplcFxeFxvcHpR(2),& 
& ctcplcFdFdVGL(3,3),ctcplcFdFdVGR(3,3),ctcplcFdFdVPL(3,3),ctcplcFdFdVPR(3,3),           & 
& ctcplcFuFdVWpL(3,3),ctcplcFuFdVWpR(3,3),ctcplcFdFdVZL(3,3),ctcplcFdFdVZR(3,3),         & 
& ctcplcFeFeVPL(3,3),ctcplcFeFeVPR(3,3),ctcplcFvFeVWpL(3,3),ctcplcFvFeVWpR(3,3),         & 
& ctcplcFeFeVZL(3,3),ctcplcFeFeVZR(3,3),ctcplcFuFuVGL(3,3),ctcplcFuFuVGR(3,3),           & 
& ctcplcFuFuVPL(3,3),ctcplcFuFuVPR(3,3),ctcplcFuFuVZL(3,3),ctcplcFuFuVZR(3,3),           & 
& ctcplcFdFucVWpL(3,3),ctcplcFdFucVWpR(3,3),ctcplcFvFvVZL(3,3),ctcplcFvFvVZR(3,3),       & 
& ctcplcFeFvcVWpL(3,3),ctcplcFeFvcVWpR(3,3),ctcplcFxeFxeVPL,ctcplcFxeFxeVPR,             & 
& ctcplcFxvFxeVWpL(2),ctcplcFxvFxeVWpR(2),ctcplcFxeFxeVZL,ctcplcFxeFxeVZR,               & 
& ctcplcFxvFxvVZL(2,2),ctcplcFxvFxvVZR(2,2),ctcplcFxeFxvcVWpL(2),ctcplcFxeFxvcVWpR(2)

Complex(dp),Intent(in) :: GcplhhHpcHp(2),GcplAhHpcVWp,GcplAhcHpVWp,GcplhhHpcVWp(2),GcplhhcHpVWp(2),             & 
& GcplHpcHpVP,GcplHpcHpVZ,GcplHpcVWpVP,GcplHpcVWpVZ,GcplcHpVPVWp,GcplcHpVWpVZ,           & 
& GcplcFuFdHpL(3,3),GcplcFuFdHpR(3,3),GcplcFvFeHpL(3,3),GcplcFvFeHpR(3,3),               & 
& GcplcFdFucHpL(3,3),GcplcFdFucHpR(3,3),GcplcFeFvcHpL(3,3),GcplcFeFvcHpR(3,3),           & 
& GcplcFxvFxeHpL(2),GcplcFxvFxeHpR(2),GcplcFxeFxvcHpL(2),GcplcFxeFxvcHpR(2)

Real(dp) ::  g1D(85) 
Real(dp) :: p2 
Logical :: TwoLoopRGEsave 
Real(dp) ::MVG,MVP,MVG2,MVP2
Complex(dp) ::  Tad1Loop(2)
Complex(dp) :: MatTad_hh(2,2)=0._dp 
Integer :: i1,i2,i3 

MVG = MLambda 
MVP = MLambda 
MVG2 = MLambda**2 
MVP2 = MLambda**2 

Call OneLoopTadpoleshh(vvSM,vS,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,              & 
& MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,cplAhAhhh,cplcFdFdhhL,          & 
& cplcFdFdhhR,cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,cplcFxvFxvhhL,             & 
& cplcFxvFxvhhR,cplcgWpgWphh,cplcgWCgWChh,cplcgZgZhh,cplhhhhhh,cplhhHpcHp,               & 
& cplhhSsccSsc,cplhhcVWpVWp,cplhhVZVZ,Tad1Loop(1:2))

Tad1Loop(1:2) = MatMul(ZH,Tad1Loop(1:2)) 
Tad1Loop(1) = Tad1Loop(1)/vvSM 
Tad1Loop(2) = Tad1Loop(2)/vS 
Do i1=1,2
MatTad_hh(i1,i1) = Tad1Loop(0+ i1) 
End Do 
MatTad_hh = MatMul(MatMul(ZH,MatTad_hh),Transpose(ZH)) 
! Not working!! 
MatTad_hh= 0._dp
!--------------------------
!hh
!--------------------------
Do i1=1,2
p2 = Mhh2(i1)
Call Pi1Loophh(p2,MAh,MAh2,MVZ,MVZ2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,            & 
& Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MSsc,MSsc2,cplAhAhhh,cplAhhhVZ,cplcFdFdhhL,               & 
& cplcFdFdhhR,cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,cplcFxvFxvhhL,             & 
& cplcFxvFxvhhR,cplcgWpgWphh,cplcgWCgWChh,cplcgZgZhh,cplhhhhhh,cplhhHpcHp,               & 
& cplhhHpcVWp,cplhhSsccSsc,cplhhcVWpVWp,cplhhVZVZ,cplAhAhhhhh,cplhhhhhhhh,               & 
& cplhhhhHpcHp,cplhhhhSsccSsc,cplhhhhcVWpVWp,cplhhhhVZVZ,kont,Pihh(i1,:,:))

Pihh(i1,:,:) = Pihh(i1,:,:) + MatTad_hh
Call DerPi1Loophh(p2,MAh,MAh2,MVZ,MVZ2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,               & 
& MFxv2,Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MSsc,MSsc2,cplAhAhhh,cplAhhhVZ,cplcFdFdhhL,         & 
& cplcFdFdhhR,cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,cplcFxvFxvhhL,             & 
& cplcFxvFxvhhR,cplcgWpgWphh,cplcgWCgWChh,cplcgZgZhh,cplhhhhhh,cplhhHpcHp,               & 
& cplhhHpcVWp,cplhhSsccSsc,cplhhcVWpVWp,cplhhVZVZ,cplAhAhhhhh,cplhhhhhhhh,               & 
& cplhhhhHpcHp,cplhhhhSsccSsc,cplhhhhcVWpVWp,cplhhhhVZVZ,kont,DerPihh(i1,:,:))

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerPi1Loophh(p2,MAh,MAh2,MVZ,MVZ2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,               & 
& MFxv2,Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MSsc,MSsc2,cplAhAhhh,cplAhhhVZ,cplcFdFdhhL,         & 
& cplcFdFdhhR,cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,cplcFxvFxvhhL,             & 
& cplcFxvFxvhhR,cplcgWpgWphh,cplcgWCgWChh,cplcgZgZhh,cplhhhhhh,cplhhHpcHp,               & 
& cplhhHpcVWp,cplhhSsccSsc,cplhhcVWpVWp,cplhhVZVZ,cplAhAhhhhh,cplhhhhhhhh,               & 
& cplhhhhHpcHp,cplhhhhSsccSsc,cplhhhhcVWpVWp,cplhhhhVZVZ,kont,DerPihhDR(i1,:,:))

p2 =Mhh2OS(i1)
Call DerPi1Loophh(p2,MAhOS,MAh2OS,MVZOS,MVZ2OS,MFdOS,MFd2OS,MFeOS,MFe2OS,             & 
& MFuOS,MFu2OS,MFxvOS,MFxv2OS,MhhOS,Mhh2OS,MHpOS,MHp2OS,MVWpOS,MVWp2OS,MSscOS,           & 
& MSsc2OS,cplAhAhhh,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFeFehhL,cplcFeFehhR,           & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcgWpgWphh,cplcgWCgWChh,         & 
& cplcgZgZhh,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcVWpVWp,cplhhVZVZ,       & 
& cplAhAhhhhh,cplhhhhhhhh,cplhhhhHpcHp,cplhhhhSsccSsc,cplhhhhcVWpVWp,cplhhhhVZVZ,        & 
& kont,DerPihhOS(i1,:,:))

DerPihh(i1,:,:) = DerPihh(i1,:,:)- DerPihhDR(i1,:,:) + DerPihhOS(i1,:,:)
IRdivonly=.False. 
End if
End do


!--------------------------
!Fd
!--------------------------
Do i1=1,3
p2 =MFd2(i1)
Call Sigma1LoopFd(p2,MFd,MFd2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,MFu,MFu2,           & 
& MVWp,MVWp2,cplcFdFdAhL,cplcFdFdAhR,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVGL,cplcFdFdVGR,    & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFdFucHpL,cplcFdFucHpR,             & 
& cplcFdFucVWpL,cplcFdFucVWpR,SigmaLFd(i1,:,:),SigmaRFd(i1,:,:),SigmaSLFd(i1,:,:)        & 
& ,SigmaSRFd(i1,:,:))

Call DerSigma1LoopFd(p2,MFd,MFd2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,MFu,             & 
& MFu2,MVWp,MVWp2,cplcFdFdAhL,cplcFdFdAhR,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVGL,           & 
& cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFdFucHpL,              & 
& cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,DerSigmaLFd(i1,:,:),DerSigmaRFd(i1,:,:)       & 
& ,DerSigmaSLFd(i1,:,:),DerSigmaSRFd(i1,:,:))

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerSigma1LoopFd(p2,MFd,MFd2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,MFu,             & 
& MFu2,MVWp,MVWp2,cplcFdFdAhL,cplcFdFdAhR,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVGL,           & 
& cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFdFucHpL,              & 
& cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,DerSigmaLFdDR(i1,:,:),DerSigmaRFdDR(i1,:,:)   & 
& ,DerSigmaSLFdDR(i1,:,:),DerSigmaSRFdDR(i1,:,:))

p2 =MFd2OS(i1)
Call DerSigma1LoopFd(p2,MFdOS,MFd2OS,MAhOS,MAh2OS,MhhOS,Mhh2OS,MVZOS,MVZ2OS,          & 
& MHpOS,MHp2OS,MFuOS,MFu2OS,MVWpOS,MVWp2OS,cplcFdFdAhL,cplcFdFdAhR,cplcFdFdhhL,          & 
& cplcFdFdhhR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,               & 
& cplcFdFdVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,DerSigmaLFdOS(i1,:,:)& 
& ,DerSigmaRFdOS(i1,:,:),DerSigmaSLFdOS(i1,:,:),DerSigmaSRFdOS(i1,:,:))

DerSigmaLFd(i1,:,:) = DerSigmaLFd(i1,:,:) - DerSigmaLFdDR(i1,:,:)! + DerSigmaLFdOS(i1,:,:)
DerSigmaRFd(i1,:,:) = DerSigmaRFd(i1,:,:) - DerSigmaRFdDR(i1,:,:)! + DerSigmaRFdOS(i1,:,:)
DerSigmaSLFd(i1,:,:) = DerSigmaSLFd(i1,:,:) - DerSigmaSLFdDR(i1,:,:)! + DerSigmaSLFdOS(i1,:,:)
DerSigmaSRFd(i1,:,:) = DerSigmaSRFd(i1,:,:) - DerSigmaSRFdDR(i1,:,:)! + DerSigmaSRFdOS(i1,:,:)
DerSigmaLirFd(i1,:,:) = + DerSigmaLFdOS(i1,:,:)
DerSigmaRirFd(i1,:,:) = + DerSigmaRFdOS(i1,:,:)
DerSigmaSLirFd(i1,:,:) = + DerSigmaSLFdOS(i1,:,:)
DerSigmaSRirFd(i1,:,:) = + DerSigmaSRFdOS(i1,:,:)
IRdivonly=.False. 
Else
DerSigmaLirFd(i1,:,:) = 0._dp
DerSigmaRirFd(i1,:,:) = 0._dp
DerSigmaSLirFd(i1,:,:) = 0._dp
DerSigmaSRirFd(i1,:,:) = 0._dp
End if
End do


!--------------------------
!Fu
!--------------------------
Do i1=1,3
p2 =MFu2(i1)
Call Sigma1LoopFu(p2,MFu,MFu2,MAh,MAh2,MHp,MHp2,MFd,MFd2,MVWp,MVWp2,Mhh,              & 
& Mhh2,MVZ,MVZ2,cplcFuFuAhL,cplcFuFuAhR,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,            & 
& cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,              & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,SigmaLFu(i1,:,:),SigmaRFu(i1,:,:),SigmaSLFu(i1,:,:)& 
& ,SigmaSRFu(i1,:,:))

Call DerSigma1LoopFu(p2,MFu,MFu2,MAh,MAh2,MHp,MHp2,MFd,MFd2,MVWp,MVWp2,               & 
& Mhh,Mhh2,MVZ,MVZ2,cplcFuFuAhL,cplcFuFuAhR,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,        & 
& cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,              & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,DerSigmaLFu(i1,:,:),DerSigmaRFu(i1,:,:)            & 
& ,DerSigmaSLFu(i1,:,:),DerSigmaSRFu(i1,:,:))

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerSigma1LoopFu(p2,MFu,MFu2,MAh,MAh2,MHp,MHp2,MFd,MFd2,MVWp,MVWp2,               & 
& Mhh,Mhh2,MVZ,MVZ2,cplcFuFuAhL,cplcFuFuAhR,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,        & 
& cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,              & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,DerSigmaLFuDR(i1,:,:),DerSigmaRFuDR(i1,:,:)        & 
& ,DerSigmaSLFuDR(i1,:,:),DerSigmaSRFuDR(i1,:,:))

p2 =MFu2OS(i1)
Call DerSigma1LoopFu(p2,MFuOS,MFu2OS,MAhOS,MAh2OS,MHpOS,MHp2OS,MFdOS,MFd2OS,          & 
& MVWpOS,MVWp2OS,MhhOS,Mhh2OS,MVZOS,MVZ2OS,cplcFuFuAhL,cplcFuFuAhR,cplcFuFdHpL,          & 
& cplcFuFdHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,             & 
& cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,DerSigmaLFuOS(i1,:,:)      & 
& ,DerSigmaRFuOS(i1,:,:),DerSigmaSLFuOS(i1,:,:),DerSigmaSRFuOS(i1,:,:))

DerSigmaLFu(i1,:,:) = DerSigmaLFu(i1,:,:) - DerSigmaLFuDR(i1,:,:)! + DerSigmaLFuOS(i1,:,:)
DerSigmaRFu(i1,:,:) = DerSigmaRFu(i1,:,:) - DerSigmaRFuDR(i1,:,:)! + DerSigmaRFuOS(i1,:,:)
DerSigmaSLFu(i1,:,:) = DerSigmaSLFu(i1,:,:) - DerSigmaSLFuDR(i1,:,:)! + DerSigmaSLFuOS(i1,:,:)
DerSigmaSRFu(i1,:,:) = DerSigmaSRFu(i1,:,:) - DerSigmaSRFuDR(i1,:,:)! + DerSigmaSRFuOS(i1,:,:)
DerSigmaLirFu(i1,:,:) = + DerSigmaLFuOS(i1,:,:)
DerSigmaRirFu(i1,:,:) = + DerSigmaRFuOS(i1,:,:)
DerSigmaSLirFu(i1,:,:) = + DerSigmaSLFuOS(i1,:,:)
DerSigmaSRirFu(i1,:,:) = + DerSigmaSRFuOS(i1,:,:)
IRdivonly=.False. 
Else
DerSigmaLirFu(i1,:,:) = 0._dp
DerSigmaRirFu(i1,:,:) = 0._dp
DerSigmaSLirFu(i1,:,:) = 0._dp
DerSigmaSRirFu(i1,:,:) = 0._dp
End if
End do


!--------------------------
!Fe
!--------------------------
Do i1=1,3
p2 =MFe2(i1)
Call Sigma1LoopFe(p2,MFe,MFe2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,MFv,MFv2,           & 
& MVWp,MVWp2,MSsc,MSsc2,MFxe,MFxe2,cplcFeFeAhL,cplcFeFeAhR,cplcFeFehhL,cplcFeFehhR,      & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,             & 
& cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,cplcFeFxecSscR,SigmaLFe(i1,:,:)             & 
& ,SigmaRFe(i1,:,:),SigmaSLFe(i1,:,:),SigmaSRFe(i1,:,:))

Call DerSigma1LoopFe(p2,MFe,MFe2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,MFv,             & 
& MFv2,MVWp,MVWp2,MSsc,MSsc2,MFxe,MFxe2,cplcFeFeAhL,cplcFeFeAhR,cplcFeFehhL,             & 
& cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,              & 
& cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,cplcFeFxecSscR,DerSigmaLFe(i1,:,:)& 
& ,DerSigmaRFe(i1,:,:),DerSigmaSLFe(i1,:,:),DerSigmaSRFe(i1,:,:))

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerSigma1LoopFe(p2,MFe,MFe2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,MFv,             & 
& MFv2,MVWp,MVWp2,MSsc,MSsc2,MFxe,MFxe2,cplcFeFeAhL,cplcFeFeAhR,cplcFeFehhL,             & 
& cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,              & 
& cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,cplcFeFxecSscR,DerSigmaLFeDR(i1,:,:)& 
& ,DerSigmaRFeDR(i1,:,:),DerSigmaSLFeDR(i1,:,:),DerSigmaSRFeDR(i1,:,:))

p2 =MFe2OS(i1)
Call DerSigma1LoopFe(p2,MFeOS,MFe2OS,MAhOS,MAh2OS,MhhOS,Mhh2OS,MVZOS,MVZ2OS,          & 
& MHpOS,MHp2OS,MFvOS,MFv2OS,MVWpOS,MVWp2OS,MSscOS,MSsc2OS,MFxeOS,MFxe2OS,cplcFeFeAhL,    & 
& cplcFeFeAhR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,               & 
& cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,      & 
& cplcFeFxecSscR,DerSigmaLFeOS(i1,:,:),DerSigmaRFeOS(i1,:,:),DerSigmaSLFeOS(i1,:,:)      & 
& ,DerSigmaSRFeOS(i1,:,:))

DerSigmaLFe(i1,:,:) = DerSigmaLFe(i1,:,:) - DerSigmaLFeDR(i1,:,:)! + DerSigmaLFeOS(i1,:,:)
DerSigmaRFe(i1,:,:) = DerSigmaRFe(i1,:,:) - DerSigmaRFeDR(i1,:,:)! + DerSigmaRFeOS(i1,:,:)
DerSigmaSLFe(i1,:,:) = DerSigmaSLFe(i1,:,:) - DerSigmaSLFeDR(i1,:,:)! + DerSigmaSLFeOS(i1,:,:)
DerSigmaSRFe(i1,:,:) = DerSigmaSRFe(i1,:,:) - DerSigmaSRFeDR(i1,:,:)! + DerSigmaSRFeOS(i1,:,:)
DerSigmaLirFe(i1,:,:) = + DerSigmaLFeOS(i1,:,:)
DerSigmaRirFe(i1,:,:) = + DerSigmaRFeOS(i1,:,:)
DerSigmaSLirFe(i1,:,:) = + DerSigmaSLFeOS(i1,:,:)
DerSigmaSRirFe(i1,:,:) = + DerSigmaSRFeOS(i1,:,:)
IRdivonly=.False. 
Else
DerSigmaLirFe(i1,:,:) = 0._dp
DerSigmaRirFe(i1,:,:) = 0._dp
DerSigmaSLirFe(i1,:,:) = 0._dp
DerSigmaSRirFe(i1,:,:) = 0._dp
End if
End do


!--------------------------
!Fv
!--------------------------
Do i1=1,3
p2 =MFv2(i1)
Call Sigma1LoopFv(p2,MHp,MHp2,MFe,MFe2,MVWp,MVWp2,MVZ,MVZ2,MFv,MFv2,MSsc,             & 
& MSsc2,MFxv,MFxv2,cplcFvFeHpL,cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFvFvVZL,        & 
& cplcFvFvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,SigmaLFv(i1,:,:),SigmaRFv(i1,:,:)            & 
& ,SigmaSLFv(i1,:,:),SigmaSRFv(i1,:,:))

Call DerSigma1LoopFv(p2,MHp,MHp2,MFe,MFe2,MVWp,MVWp2,MVZ,MVZ2,MFv,MFv2,               & 
& MSsc,MSsc2,MFxv,MFxv2,cplcFvFeHpL,cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,               & 
& cplcFvFvVZL,cplcFvFvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,DerSigmaLFv(i1,:,:)              & 
& ,DerSigmaRFv(i1,:,:),DerSigmaSLFv(i1,:,:),DerSigmaSRFv(i1,:,:))

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerSigma1LoopFv(p2,MHp,MHp2,MFe,MFe2,MVWp,MVWp2,MVZ,MVZ2,MFv,MFv2,               & 
& MSsc,MSsc2,MFxv,MFxv2,cplcFvFeHpL,cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,               & 
& cplcFvFvVZL,cplcFvFvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,DerSigmaLFvDR(i1,:,:)            & 
& ,DerSigmaRFvDR(i1,:,:),DerSigmaSLFvDR(i1,:,:),DerSigmaSRFvDR(i1,:,:))

p2 =MFv2OS(i1)
Call DerSigma1LoopFv(p2,MHpOS,MHp2OS,MFeOS,MFe2OS,MVWpOS,MVWp2OS,MVZOS,               & 
& MVZ2OS,MFvOS,MFv2OS,MSscOS,MSsc2OS,MFxvOS,MFxv2OS,cplcFvFeHpL,cplcFvFeHpR,             & 
& cplcFvFeVWpL,cplcFvFeVWpR,cplcFvFvVZL,cplcFvFvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,       & 
& DerSigmaLFvOS(i1,:,:),DerSigmaRFvOS(i1,:,:),DerSigmaSLFvOS(i1,:,:),DerSigmaSRFvOS(i1,:,:))

DerSigmaLFv(i1,:,:) = DerSigmaLFv(i1,:,:) - DerSigmaLFvDR(i1,:,:)! + DerSigmaLFvOS(i1,:,:)
DerSigmaRFv(i1,:,:) = DerSigmaRFv(i1,:,:) - DerSigmaRFvDR(i1,:,:)! + DerSigmaRFvOS(i1,:,:)
DerSigmaSLFv(i1,:,:) = DerSigmaSLFv(i1,:,:) - DerSigmaSLFvDR(i1,:,:)! + DerSigmaSLFvOS(i1,:,:)
DerSigmaSRFv(i1,:,:) = DerSigmaSRFv(i1,:,:) - DerSigmaSRFvDR(i1,:,:)! + DerSigmaSRFvOS(i1,:,:)
DerSigmaLirFv(i1,:,:) = + DerSigmaLFvOS(i1,:,:)
DerSigmaRirFv(i1,:,:) = + DerSigmaRFvOS(i1,:,:)
DerSigmaSLirFv(i1,:,:) = + DerSigmaSLFvOS(i1,:,:)
DerSigmaSRirFv(i1,:,:) = + DerSigmaSRFvOS(i1,:,:)
IRdivonly=.False. 
Else
DerSigmaLirFv(i1,:,:) = 0._dp
DerSigmaRirFv(i1,:,:) = 0._dp
DerSigmaSLirFv(i1,:,:) = 0._dp
DerSigmaSRirFv(i1,:,:) = 0._dp
End if
End do


!--------------------------
!Fxv
!--------------------------
Do i1=1,2
p2 =MFxv2(i1)
Call Sigma1LoopFxv(p2,MFxv,MFxv2,MAh,MAh2,MSsc,MSsc2,MFv,MFv2,MHp,MHp2,               & 
& MFxe,MFxe2,MVWp,MVWp2,Mhh,Mhh2,MVZ,MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFxvFvSscL,     & 
& cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,SigmaLFxv(i1,:,:)              & 
& ,SigmaRFxv(i1,:,:),SigmaSLFxv(i1,:,:),SigmaSRFxv(i1,:,:))

Call DerSigma1LoopFxv(p2,MFxv,MFxv2,MAh,MAh2,MSsc,MSsc2,MFv,MFv2,MHp,MHp2,            & 
& MFxe,MFxe2,MVWp,MVWp2,Mhh,Mhh2,MVZ,MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFxvFvSscL,     & 
& cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,DerSigmaLFxv(i1,:,:)           & 
& ,DerSigmaRFxv(i1,:,:),DerSigmaSLFxv(i1,:,:),DerSigmaSRFxv(i1,:,:))

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerSigma1LoopFxv(p2,MFxv,MFxv2,MAh,MAh2,MSsc,MSsc2,MFv,MFv2,MHp,MHp2,            & 
& MFxe,MFxe2,MVWp,MVWp2,Mhh,Mhh2,MVZ,MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFxvFvSscL,     & 
& cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,DerSigmaLFxvDR(i1,:,:)         & 
& ,DerSigmaRFxvDR(i1,:,:),DerSigmaSLFxvDR(i1,:,:),DerSigmaSRFxvDR(i1,:,:))

p2 =MFxv2OS(i1)
Call DerSigma1LoopFxv(p2,MFxvOS,MFxv2OS,MAhOS,MAh2OS,MSscOS,MSsc2OS,MFvOS,            & 
& MFv2OS,MHpOS,MHp2OS,MFxeOS,MFxe2OS,MVWpOS,MVWp2OS,MhhOS,Mhh2OS,MVZOS,MVZ2OS,           & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,   & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,               & 
& cplcFxvFxvVZR,DerSigmaLFxvOS(i1,:,:),DerSigmaRFxvOS(i1,:,:),DerSigmaSLFxvOS(i1,:,:)    & 
& ,DerSigmaSRFxvOS(i1,:,:))

DerSigmaLFxv(i1,:,:) = DerSigmaLFxv(i1,:,:) - DerSigmaLFxvDR(i1,:,:)! + DerSigmaLFxvOS(i1,:,:)
DerSigmaRFxv(i1,:,:) = DerSigmaRFxv(i1,:,:) - DerSigmaRFxvDR(i1,:,:)! + DerSigmaRFxvOS(i1,:,:)
DerSigmaSLFxv(i1,:,:) = DerSigmaSLFxv(i1,:,:) - DerSigmaSLFxvDR(i1,:,:)! + DerSigmaSLFxvOS(i1,:,:)
DerSigmaSRFxv(i1,:,:) = DerSigmaSRFxv(i1,:,:) - DerSigmaSRFxvDR(i1,:,:)! + DerSigmaSRFxvOS(i1,:,:)
DerSigmaLirFxv(i1,:,:) = + DerSigmaLFxvOS(i1,:,:)
DerSigmaRirFxv(i1,:,:) = + DerSigmaRFxvOS(i1,:,:)
DerSigmaSLirFxv(i1,:,:) = + DerSigmaSLFxvOS(i1,:,:)
DerSigmaSRirFxv(i1,:,:) = + DerSigmaSRFxvOS(i1,:,:)
IRdivonly=.False. 
Else
DerSigmaLirFxv(i1,:,:) = 0._dp
DerSigmaRirFxv(i1,:,:) = 0._dp
DerSigmaSLirFxv(i1,:,:) = 0._dp
DerSigmaSRirFxv(i1,:,:) = 0._dp
End if
End do


!--------------------------
!Ssc
!--------------------------
Do i1=1,2
p2 = MSsc2(i1)
Call Pi1LoopSsc(p2,MFe,MFe2,MFxe,MFxe2,MFv,MFv2,MFxv,MFxv2,MSsc,MSsc2,Mhh,            & 
& Mhh2,MAh,MAh2,MHp,MHp2,cplcFeFxecSscL,cplcFeFxecSscR,cplcFvFxvcSscL,cplcFvFxvcSscR,    & 
& cplhhSsccSsc,cplAhAhSsccSsc,cplhhhhSsccSsc,cplHpSsccHpcSsc,cplSscSsccSsccSsc,          & 
& kont,PiSsc(i1,:,:))

Call DerPi1LoopSsc(p2,MFe,MFe2,MFxe,MFxe2,MFv,MFv2,MFxv,MFxv2,MSsc,MSsc2,             & 
& Mhh,Mhh2,MAh,MAh2,MHp,MHp2,cplcFeFxecSscL,cplcFeFxecSscR,cplcFvFxvcSscL,               & 
& cplcFvFxvcSscR,cplhhSsccSsc,cplAhAhSsccSsc,cplhhhhSsccSsc,cplHpSsccHpcSsc,             & 
& cplSscSsccSsccSsc,kont,DerPiSsc(i1,:,:))

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerPi1LoopSsc(p2,MFe,MFe2,MFxe,MFxe2,MFv,MFv2,MFxv,MFxv2,MSsc,MSsc2,             & 
& Mhh,Mhh2,MAh,MAh2,MHp,MHp2,cplcFeFxecSscL,cplcFeFxecSscR,cplcFvFxvcSscL,               & 
& cplcFvFxvcSscR,cplhhSsccSsc,cplAhAhSsccSsc,cplhhhhSsccSsc,cplHpSsccHpcSsc,             & 
& cplSscSsccSsccSsc,kont,DerPiSscDR(i1,:,:))

p2 =MSsc2OS(i1)
Call DerPi1LoopSsc(p2,MFeOS,MFe2OS,MFxeOS,MFxe2OS,MFvOS,MFv2OS,MFxvOS,MFxv2OS,        & 
& MSscOS,MSsc2OS,MhhOS,Mhh2OS,MAhOS,MAh2OS,MHpOS,MHp2OS,cplcFeFxecSscL,cplcFeFxecSscR,   & 
& cplcFvFxvcSscL,cplcFvFxvcSscR,cplhhSsccSsc,cplAhAhSsccSsc,cplhhhhSsccSsc,              & 
& cplHpSsccHpcSsc,cplSscSsccSsccSsc,kont,DerPiSscOS(i1,:,:))

DerPiSsc(i1,:,:) = DerPiSsc(i1,:,:)- DerPiSscDR(i1,:,:) + DerPiSscOS(i1,:,:)
IRdivonly=.False. 
End if
End do


!--------------------------
!Fxe
!--------------------------
p2 = MFxe2
Call Sigma1LoopFxe(p2,MSsc,MSsc2,MFe,MFe2,MFxe,MFxe2,MVZ,MVZ2,MHp,MHp2,               & 
& MFxv,MFxv2,MVWp,MVWp2,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,         & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,             & 
& cplcFxeFxvcVWpR,SigmaLFxe,SigmaRFxe,SigmaSLFxe,SigmaSRFxe)

Call DerSigma1LoopFxe(p2,MSsc,MSsc2,MFe,MFe2,MFxe,MFxe2,MVZ,MVZ2,MHp,MHp2,            & 
& MFxv,MFxv2,MVWp,MVWp2,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,         & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,             & 
& cplcFxeFxvcVWpR,DerSigmaLFxe,DerSigmaRFxe,DerSigmaSLFxe,DerSigmaSRFxe)

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerSigma1LoopFxe(p2,MSsc,MSsc2,MFe,MFe2,MFxe,MFxe2,MVZ,MVZ2,MHp,MHp2,            & 
& MFxv,MFxv2,MVWp,MVWp2,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,         & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,             & 
& cplcFxeFxvcVWpR,DerSigmaLFxeDR,DerSigmaRFxeDR,DerSigmaSLFxeDR,DerSigmaSRFxeDR)

p2 = MFxe2OS
Call DerSigma1LoopFxe(p2,MSscOS,MSsc2OS,MFeOS,MFe2OS,MFxeOS,MFxe2OS,MVZOS,            & 
& MVZ2OS,MHpOS,MHp2OS,MFxvOS,MFxv2OS,MVWpOS,MVWp2OS,cplcFxeFeSscL,cplcFxeFeSscR,         & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,GcplcFxeFxvcHpL,               & 
& GcplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,DerSigmaLFxeOS,DerSigmaRFxeOS,         & 
& DerSigmaSLFxeOS,DerSigmaSRFxeOS)

DerSigmaLFxe = DerSigmaLFxe - DerSigmaLFxeDR !+ DerSigmaLFxeOS
DerSigmaRFxe = DerSigmaRFxe - DerSigmaRFxeDR !+ DerSigmaRFxeOS
DerSigmaSLFxe = DerSigmaSLFxe - DerSigmaSLFxeDR !+ DerSigmaSLFxeOS
DerSigmaSRFxe = DerSigmaSRFxe - DerSigmaSRFxeDR !+ DerSigmaSRFxeOS
DerSigmaLirFxe = + DerSigmaLFxeOS
DerSigmaRirFxe = + DerSigmaRFxeOS
DerSigmaSLirFxe = + DerSigmaSLFxeOS
DerSigmaSRirFxe = + DerSigmaSRFxeOS
IRdivonly=.False. 
Else 
DerSigmaLirFxe = 0._dp
DerSigmaRirFxe = 0._dp
DerSigmaSLirFxe = 0._dp
DerSigmaSRirFxe = 0._dp 
End if 
!--------------------------
!Hp
!--------------------------
p2 = MHp2
Call Pi1LoopHp(p2,MVWp,MVWp2,MAh,MAh2,MFd,MFd2,MFu,MFu2,MFe,MFe2,MFv,MFv2,            & 
& MFxe,MFxe2,MFxv,MFxv2,MHp,MHp2,Mhh,Mhh2,MVZ,MVZ2,MSsc,MSsc2,cplAhcHpVWp,               & 
& cplcFdFucHpL,cplcFdFucHpR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,     & 
& cplcgZgWpcHp,cplcgWpgZHp,cplcgWCgZcHp,cplcgZgWCHp,cplhhHpcHp,cplhhcHpVWp,              & 
& cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplAhAhHpcHp,cplhhhhHpcHp,               & 
& cplHpHpcHpcHp,cplHpSsccHpcSsc,cplHpcHpVPVP,cplHpcHpcVWpVWp,cplHpcHpVZVZ,               & 
& kont,PiHp)

Call DerPi1LoopHp(p2,MVWp,MVWp2,MAh,MAh2,MFd,MFd2,MFu,MFu2,MFe,MFe2,MFv,              & 
& MFv2,MFxe,MFxe2,MFxv,MFxv2,MHp,MHp2,Mhh,Mhh2,MVZ,MVZ2,MSsc,MSsc2,cplAhcHpVWp,          & 
& cplcFdFucHpL,cplcFdFucHpR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,     & 
& cplcgZgWpcHp,cplcgWpgZHp,cplcgWCgZcHp,cplcgZgWCHp,cplhhHpcHp,cplhhcHpVWp,              & 
& cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplAhAhHpcHp,cplhhhhHpcHp,               & 
& cplHpHpcHpcHp,cplHpSsccHpcSsc,cplHpcHpVPVP,cplHpcHpcVWpVWp,cplHpcHpVZVZ,               & 
& kont,DerPiHp)

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerPi1LoopHp(p2,MVWp,MVWp2,MAh,MAh2,MFd,MFd2,MFu,MFu2,MFe,MFe2,MFv,              & 
& MFv2,MFxe,MFxe2,MFxv,MFxv2,MHp,MHp2,Mhh,Mhh2,MVZ,MVZ2,MSsc,MSsc2,cplAhcHpVWp,          & 
& cplcFdFucHpL,cplcFdFucHpR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,     & 
& cplcgZgWpcHp,cplcgWpgZHp,cplcgWCgZcHp,cplcgZgWCHp,cplhhHpcHp,cplhhcHpVWp,              & 
& cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplAhAhHpcHp,cplhhhhHpcHp,               & 
& cplHpHpcHpcHp,cplHpSsccHpcSsc,cplHpcHpVPVP,cplHpcHpcVWpVWp,cplHpcHpVZVZ,               & 
& kont,DerPiHpDR)

p2 = MHp2OS
Call DerPi1LoopHp(p2,MVWpOS,MVWp2OS,MAhOS,MAh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,           & 
& MFeOS,MFe2OS,MFvOS,MFv2OS,MFxeOS,MFxe2OS,MFxvOS,MFxv2OS,MHpOS,MHp2OS,MhhOS,            & 
& Mhh2OS,MVZOS,MVZ2OS,MSscOS,MSsc2OS,GcplAhcHpVWp,GcplcFdFucHpL,GcplcFdFucHpR,           & 
& GcplcFeFvcHpL,GcplcFeFvcHpR,GcplcFxeFxvcHpL,GcplcFxeFxvcHpR,cplcgZgWpcHp,              & 
& cplcgWpgZHp,cplcgWCgZcHp,cplcgZgWCHp,GcplhhHpcHp,GcplhhcHpVWp,GcplHpcHpVP,             & 
& GcplHpcHpVZ,GcplcHpVPVWp,GcplcHpVWpVZ,cplAhAhHpcHp,cplhhhhHpcHp,cplHpHpcHpcHp,         & 
& cplHpSsccHpcSsc,cplHpcHpVPVP,cplHpcHpcVWpVWp,cplHpcHpVZVZ,kont,DerPiHpOS)

DerPiHp = DerPiHp-DerPiHpDR + DerPiHpOS
IRdivonly=.False. 
End if 
!--------------------------
!Ah
!--------------------------
p2 = MAh2
Call Pi1LoopAh(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,            & 
& MVZ,MVZ2,MVWp,MVWp2,MHp,MHp2,MSsc,MSsc2,cplAhAhhh,cplcFdFdAhL,cplcFdFdAhR,             & 
& cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplcFxvFxvAhL,cplcFxvFxvAhR,           & 
& cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ,cplAhHpcVWp,cplAhAhAhAh,cplAhAhhhhh,               & 
& cplAhAhHpcHp,cplAhAhSsccSsc,cplAhAhcVWpVWp,cplAhAhVZVZ,kont,PiAh)

Call DerPi1LoopAh(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,               & 
& MFxv2,MVZ,MVZ2,MVWp,MVWp2,MHp,MHp2,MSsc,MSsc2,cplAhAhhh,cplcFdFdAhL,cplcFdFdAhR,       & 
& cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplcFxvFxvAhL,cplcFxvFxvAhR,           & 
& cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ,cplAhHpcVWp,cplAhAhAhAh,cplAhAhhhhh,               & 
& cplAhAhHpcHp,cplAhAhSsccSsc,cplAhAhcVWpVWp,cplAhAhVZVZ,kont,DerPiAh)

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerPi1LoopAh(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,               & 
& MFxv2,MVZ,MVZ2,MVWp,MVWp2,MHp,MHp2,MSsc,MSsc2,cplAhAhhh,cplcFdFdAhL,cplcFdFdAhR,       & 
& cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplcFxvFxvAhL,cplcFxvFxvAhR,           & 
& cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ,cplAhHpcVWp,cplAhAhAhAh,cplAhAhhhhh,               & 
& cplAhAhHpcHp,cplAhAhSsccSsc,cplAhAhcVWpVWp,cplAhAhVZVZ,kont,DerPiAhDR)

p2 = MAh2OS
Call DerPi1LoopAh(p2,MhhOS,Mhh2OS,MAhOS,MAh2OS,MFdOS,MFd2OS,MFeOS,MFe2OS,             & 
& MFuOS,MFu2OS,MFxvOS,MFxv2OS,MVZOS,MVZ2OS,MVWpOS,MVWp2OS,MHpOS,MHp2OS,MSscOS,           & 
& MSsc2OS,cplAhAhhh,cplcFdFdAhL,cplcFdFdAhR,cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,         & 
& cplcFuFuAhR,cplcFxvFxvAhL,cplcFxvFxvAhR,cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ,           & 
& GcplAhHpcVWp,cplAhAhAhAh,cplAhAhhhhh,cplAhAhHpcHp,cplAhAhSsccSsc,cplAhAhcVWpVWp,       & 
& cplAhAhVZVZ,kont,DerPiAhOS)

DerPiAh = DerPiAh-DerPiAhDR + DerPiAhOS
IRdivonly=.False. 
End if 
!--------------------------
!VG
!--------------------------
p2 = MVG2
Call Pi1LoopVG(p2,MFd,MFd2,MFu,MFu2,cplcFdFdVGL,cplcFdFdVGR,cplcFuFuVGL,              & 
& cplcFuFuVGR,cplcgGgGVG,cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3,               & 
& kont,PiVG)

Call DerPi1LoopVG(p2,MFd,MFd2,MFu,MFu2,cplcFdFdVGL,cplcFdFdVGR,cplcFuFuVGL,           & 
& cplcFuFuVGR,cplcgGgGVG,cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3,               & 
& kont,DerPiVG)

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerPi1LoopVG(p2,MFd,MFd2,MFu,MFu2,cplcFdFdVGL,cplcFdFdVGR,cplcFuFuVGL,           & 
& cplcFuFuVGR,cplcgGgGVG,cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3,               & 
& kont,DerPiVGDR)

p2 = 0.
Call DerPi1LoopVG(p2,MFdOS,MFd2OS,MFuOS,MFu2OS,cplcFdFdVGL,cplcFdFdVGR,               & 
& cplcFuFuVGL,cplcFuFuVGR,cplcgGgGVG,cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3,   & 
& kont,DerPiVGOS)

DerPiVG = DerPiVG-DerPiVGDR + DerPiVGOS
IRdivonly=.False. 
End if 
!--------------------------
!VP
!--------------------------
p2 = MVP2
Call Pi1LoopVP(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,MVWp,MVWp2,          & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,               & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcgWpgWpVP,cplcgWCgWCVP,cplHpcHpVP,cplHpcVWpVP,          & 
& cplcVWpVPVWp,cplHpcHpVPVP,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,             & 
& kont,PiVP)

Call DerPi1LoopVP(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,MVWp,             & 
& MVWp2,cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,         & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcgWpgWpVP,cplcgWCgWCVP,cplHpcHpVP,cplHpcVWpVP,          & 
& cplcVWpVPVWp,cplHpcHpVPVP,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,             & 
& kont,DerPiVP)

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerPi1LoopVP(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,MVWp,             & 
& MVWp2,cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,         & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcgWpgWpVP,cplcgWCgWCVP,cplHpcHpVP,cplHpcVWpVP,          & 
& cplcVWpVPVWp,cplHpcHpVPVP,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,             & 
& kont,DerPiVPDR)

p2 = 0.
Call DerPi1LoopVP(p2,MFdOS,MFd2OS,MFeOS,MFe2OS,MFuOS,MFu2OS,MFxeOS,MFxe2OS,           & 
& MHpOS,MHp2OS,MVWpOS,MVWp2OS,cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,           & 
& cplcFuFuVPL,cplcFuFuVPR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcgWpgWpVP,cplcgWCgWCVP,         & 
& GcplHpcHpVP,GcplHpcVWpVP,cplcVWpVPVWp,cplHpcHpVPVP,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,    & 
& cplcVWpVPVPVWp2,kont,DerPiVPOS)

DerPiVP = DerPiVP-DerPiVPDR + DerPiVPOS
IRdivonly=.False. 
End if 
!--------------------------
!VZ
!--------------------------
p2 = MVZ2
Call Pi1LoopVZ(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,              & 
& MFxe,MFxe2,MFxv,MFxv2,MVZ,MVZ2,MHp,MHp2,MVWp,MVWp2,cplAhhhVZ,cplcFdFdVZL,              & 
& cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFvFvVZL,               & 
& cplcFvFvVZR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWpVZ,      & 
& cplcgWCgWCVZ,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ,cplhhhhVZVZ,    & 
& cplHpcHpVZVZ,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,kont,PiVZ)

Call DerPi1LoopVZ(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,           & 
& MFxe,MFxe2,MFxv,MFxv2,MVZ,MVZ2,MHp,MHp2,MVWp,MVWp2,cplAhhhVZ,cplcFdFdVZL,              & 
& cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFvFvVZL,               & 
& cplcFvFvVZR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWpVZ,      & 
& cplcgWCgWCVZ,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ,cplhhhhVZVZ,    & 
& cplHpcHpVZVZ,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,kont,DerPiVZ)

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerPi1LoopVZ(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,           & 
& MFxe,MFxe2,MFxv,MFxv2,MVZ,MVZ2,MHp,MHp2,MVWp,MVWp2,cplAhhhVZ,cplcFdFdVZL,              & 
& cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFvFvVZL,               & 
& cplcFvFvVZR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWpVZ,      & 
& cplcgWCgWCVZ,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ,cplhhhhVZVZ,    & 
& cplHpcHpVZVZ,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,kont,DerPiVZDR)

p2 = MVZ2OS
Call DerPi1LoopVZ(p2,MhhOS,Mhh2OS,MAhOS,MAh2OS,MFdOS,MFd2OS,MFeOS,MFe2OS,             & 
& MFuOS,MFu2OS,MFvOS,MFv2OS,MFxeOS,MFxe2OS,MFxvOS,MFxv2OS,MVZOS,MVZ2OS,MHpOS,            & 
& MHp2OS,MVWpOS,MVWp2OS,cplAhhhVZ,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,       & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFvFvVZL,cplcFvFvVZR,cplcFxeFxeVZL,cplcFxeFxeVZR,           & 
& cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWpVZ,cplcgWCgWCVZ,cplhhVZVZ,GcplHpcHpVZ,           & 
& GcplHpcVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ,cplhhhhVZVZ,cplHpcHpVZVZ,cplcVWpVWpVZVZ1,        & 
& cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,kont,DerPiVZOS)

DerPiVZ = DerPiVZ-DerPiVZDR + DerPiVZOS
IRdivonly=.False. 
End if 
!--------------------------
!VWp
!--------------------------
p2 = MVWp2
Call Pi1LoopVWp(p2,MHp,MHp2,MAh,MAh2,MFd,MFd2,MFu,MFu2,MFe,MFe2,MFv,MFv2,             & 
& MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MVWp,MVWp2,MVZ,MVZ2,cplAhHpcVWp,cplcFdFucVWpL,          & 
& cplcFdFucVWpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,             & 
& cplcgWCgAcVWp,cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgZcVWp,cplhhHpcVWp,cplhhcVWpVWp,      & 
& cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplAhAhcVWpVWp,cplhhhhcVWpVWp,       & 
& cplHpcHpcVWpVWp,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,cplcVWpcVWpVWpVWp2,    & 
& cplcVWpcVWpVWpVWp3,cplcVWpcVWpVWpVWp1,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3, & 
& kont,PiVWp)

Call DerPi1LoopVWp(p2,MHp,MHp2,MAh,MAh2,MFd,MFd2,MFu,MFu2,MFe,MFe2,MFv,               & 
& MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MVWp,MVWp2,MVZ,MVZ2,cplAhHpcVWp,cplcFdFucVWpL,     & 
& cplcFdFucVWpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,             & 
& cplcgWCgAcVWp,cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgZcVWp,cplhhHpcVWp,cplhhcVWpVWp,      & 
& cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplAhAhcVWpVWp,cplhhhhcVWpVWp,       & 
& cplHpcHpcVWpVWp,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,cplcVWpcVWpVWpVWp2,    & 
& cplcVWpcVWpVWpVWp3,cplcVWpcVWpVWpVWp1,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3, & 
& kont,DerPiVWp)

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerPi1LoopVWp(p2,MHp,MHp2,MAh,MAh2,MFd,MFd2,MFu,MFu2,MFe,MFe2,MFv,               & 
& MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MVWp,MVWp2,MVZ,MVZ2,cplAhHpcVWp,cplcFdFucVWpL,     & 
& cplcFdFucVWpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,             & 
& cplcgWCgAcVWp,cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgZcVWp,cplhhHpcVWp,cplhhcVWpVWp,      & 
& cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplAhAhcVWpVWp,cplhhhhcVWpVWp,       & 
& cplHpcHpcVWpVWp,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,cplcVWpcVWpVWpVWp2,    & 
& cplcVWpcVWpVWpVWp3,cplcVWpcVWpVWpVWp1,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3, & 
& kont,DerPiVWpDR)

p2 = MVWp2OS
Call DerPi1LoopVWp(p2,MHpOS,MHp2OS,MAhOS,MAh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,            & 
& MFeOS,MFe2OS,MFvOS,MFv2OS,MFxeOS,MFxe2OS,MFxvOS,MFxv2OS,MhhOS,Mhh2OS,MVWpOS,           & 
& MVWp2OS,MVZOS,MVZ2OS,GcplAhHpcVWp,cplcFdFucVWpL,cplcFdFucVWpR,cplcFeFvcVWpL,           & 
& cplcFeFvcVWpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplcgWCgAcVWp,cplcgAgWpcVWp,             & 
& cplcgZgWpcVWp,cplcgWCgZcVWp,GcplhhHpcVWp,cplhhcVWpVWp,GcplHpcVWpVP,GcplHpcVWpVZ,       & 
& cplcVWpVPVWp,cplcVWpVWpVZ,cplAhAhcVWpVWp,cplhhhhcVWpVWp,cplHpcHpcVWpVWp,               & 
& cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,cplcVWpcVWpVWpVWp2,cplcVWpcVWpVWpVWp3, & 
& cplcVWpcVWpVWpVWp1,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,kont,               & 
& DerPiVWpOS)

DerPiVWp = DerPiVWp-DerPiVWpDR + DerPiVWpOS
IRdivonly=.False. 
End if 
!--------------------------
! Additional Self-Energies for Photon
!--------------------------
p2 = 0._dp
OnlyLightStates = .True. 
Call Pi1LoopVP(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,MVWp,MVWp2,          & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,               & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcgWpgWpVP,cplcgWCgWCVP,cplHpcHpVP,cplHpcVWpVP,          & 
& cplcVWpVPVWp,cplHpcHpVPVP,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,             & 
& kont,PiVPlight0)

Call DerPi1LoopVP(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,MVWp,             & 
& MVWp2,cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,         & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcgWpgWpVP,cplcgWCgWCVP,cplHpcHpVP,cplHpcVWpVP,          & 
& cplcVWpVPVWp,cplHpcHpVPVP,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,             & 
& kont,DerPiVPlight0)

OnlyLightStates = .False. 
p2 = 0._dp
OnlyHeavyStates = .True. 
Call Pi1LoopVP(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,MVWp,MVWp2,          & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,               & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcgWpgWpVP,cplcgWCgWCVP,cplHpcHpVP,cplHpcVWpVP,          & 
& cplcVWpVPVWp,cplHpcHpVPVP,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,             & 
& kont,PiVPheavy0)

Call DerPi1LoopVP(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,MVWp,             & 
& MVWp2,cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,         & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcgWpgWpVP,cplcgWCgWCVP,cplHpcHpVP,cplHpcVWpVP,          & 
& cplcVWpVPVWp,cplHpcHpVPVP,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,             & 
& kont,DerPiVPheavy0)

OnlyHeavyStates = .False. 
p2 = MVZ2
OnlyLightStates = .True. 
Call Pi1LoopVP(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,MVWp,MVWp2,          & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,               & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcgWpgWpVP,cplcgWCgWCVP,cplHpcHpVP,cplHpcVWpVP,          & 
& cplcVWpVPVWp,cplHpcHpVPVP,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,             & 
& kont,PiVPlightMZ)

Call DerPi1LoopVP(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,MVWp,             & 
& MVWp2,cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,         & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcgWpgWpVP,cplcgWCgWCVP,cplHpcHpVP,cplHpcVWpVP,          & 
& cplcVWpVPVWp,cplHpcHpVPVP,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,             & 
& kont,DerPiVPlightMZ)

OnlyLightStates = .False. 
p2 = MVZ2
OnlyHeavyStates = .True. 
Call Pi1LoopVP(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,MVWp,MVWp2,          & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,               & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcgWpgWpVP,cplcgWCgWCVP,cplHpcHpVP,cplHpcVWpVP,          & 
& cplcVWpVPVWp,cplHpcHpVPVP,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,             & 
& kont,PiVPheavyMZ)

Call DerPi1LoopVP(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,MVWp,             & 
& MVWp2,cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,         & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcgWpgWpVP,cplcgWCgWCVP,cplHpcHpVP,cplHpcVWpVP,          & 
& cplcVWpVPVWp,cplHpcHpVPVP,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,             & 
& kont,DerPiVPheavyMZ)

OnlyHeavyStates = .False. 
!--------------------------
!VP
!--------------------------
p2 = MVZ2
Call Pi1LoopVPVZ(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,MVWp,              & 
& MVWp2,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVPL,cplcFeFeVPR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcgWCgWCVP,cplcgWCgWCVZ,     & 
& cplcgWpgWpVP,cplcgWpgWpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVPVWpVZ1,        & 
& cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpVWpVZ,cplHpcHpVP,cplHpcHpVPVZ,cplHpcHpVZ,       & 
& cplHpcVWpVP,cplHpcVWpVZ,kont,PiVPVZ)

Call DerPi1LoopVPVZ(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,MVWp,           & 
& MVWp2,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVPL,cplcFeFeVPR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcgWCgWCVP,cplcgWCgWCVZ,     & 
& cplcgWpgWpVP,cplcgWpgWpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVPVWpVZ1,        & 
& cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpVWpVZ,cplHpcHpVP,cplHpcHpVPVZ,cplHpcHpVZ,       & 
& cplHpcVWpVP,cplHpcVWpVZ,kont,DerPiVPVZ)

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerPi1LoopVPVZ(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,MVWp,           & 
& MVWp2,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVPL,cplcFeFeVPR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcgWCgWCVP,cplcgWCgWCVZ,     & 
& cplcgWpgWpVP,cplcgWpgWpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVPVWpVZ1,        & 
& cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpVWpVZ,cplHpcHpVP,cplHpcHpVPVZ,cplHpcHpVZ,       & 
& cplHpcVWpVP,cplHpcVWpVZ,kont,DerPiVPVZDR)

p2 =MVZ2OS
Call DerPi1LoopVPVZ(p2,MFdOS,MFd2OS,MFeOS,MFe2OS,MFuOS,MFu2OS,MFxeOS,MFxe2OS,         & 
& MHpOS,MHp2OS,MVWpOS,MVWp2OS,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,           & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVPL,cplcFuFuVPR,               & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,       & 
& cplcgWCgWCVP,cplcgWCgWCVZ,cplcgWpgWpVP,cplcgWpgWpVZ,GcplcHpVPVWp,GcplcHpVWpVZ,         & 
& cplcVWpVPVWp,cplcVWpVPVWpVZ1,cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpVWpVZ,             & 
& GcplHpcHpVP,cplHpcHpVPVZ,GcplHpcHpVZ,GcplHpcVWpVP,GcplHpcVWpVZ,kont,DerPiVPVZOS)

DerPiVPVZ = DerPiVPVZ- DerPiVPVZDR + DerPiVPVZOS
IRdivonly=.False. 
End if
p2 = 0._dp 
Call Pi1LoopVPVZ(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,MVWp,              & 
& MVWp2,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVPL,cplcFeFeVPR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcgWCgWCVP,cplcgWCgWCVZ,     & 
& cplcgWpgWpVP,cplcgWpgWpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVPVWpVZ1,        & 
& cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpVWpVZ,cplHpcHpVP,cplHpcHpVPVZ,cplHpcHpVZ,       & 
& cplHpcVWpVP,cplHpcVWpVZ,kont,PiVZVP)

Call DerPi1LoopVPVZ(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,MVWp,           & 
& MVWp2,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVPL,cplcFeFeVPR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcgWCgWCVP,cplcgWCgWCVZ,     & 
& cplcgWpgWpVP,cplcgWpgWpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVPVWpVZ1,        & 
& cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpVWpVZ,cplHpcHpVP,cplHpcHpVPVZ,cplHpcHpVZ,       & 
& cplHpcVWpVP,cplHpcVWpVZ,kont,DerPiVZVP)

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerPi1LoopVPVZ(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,MVWp,           & 
& MVWp2,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVPL,cplcFeFeVPR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcgWCgWCVP,cplcgWCgWCVZ,     & 
& cplcgWpgWpVP,cplcgWpgWpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVPVWpVZ1,        & 
& cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpVWpVZ,cplHpcHpVP,cplHpcHpVPVZ,cplHpcHpVZ,       & 
& cplHpcVWpVP,cplHpcVWpVZ,kont,DerPiVPVZDR)

p2 = 0._dp 
Call DerPi1LoopVPVZ(p2,MFdOS,MFd2OS,MFeOS,MFe2OS,MFuOS,MFu2OS,MFxeOS,MFxe2OS,         & 
& MHpOS,MHp2OS,MVWpOS,MVWp2OS,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,           & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVPL,cplcFuFuVPR,               & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,       & 
& cplcgWCgWCVP,cplcgWCgWCVZ,cplcgWpgWpVP,cplcgWpgWpVZ,GcplcHpVPVWp,GcplcHpVWpVZ,         & 
& cplcVWpVPVWp,cplcVWpVPVWpVZ1,cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpVWpVZ,             & 
& GcplHpcHpVP,cplHpcHpVPVZ,GcplHpcHpVZ,GcplHpcVWpVP,GcplHpcVWpVZ,kont,DerPiVPVZOS)

DerPiVPVZ = DerPiVPVZ- DerPiVPVZDR + DerPiVPVZOS
IRdivonly=.False. 
End if
!--------------------------
!VZ
!--------------------------
p2 = MAh2
Call Pi1LoopVZAh(p2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,Mhh,               & 
& Mhh2,MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2,cplAhAhhh,cplAhcHpVWp,cplAhhhVZ,cplAhHpcVWp,         & 
& cplcFdFdAhL,cplcFdFdAhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeAhL,cplcFeFeAhR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuAhL,cplcFuFuAhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWCgWCAh,cplcgWCgWCVZ,     & 
& cplcgWpgWpAh,cplcgWpgWpVZ,cplcHpVWpVZ,cplhhVZVZ,cplHpcVWpVZ,kont,PiVZAh)

Call DerPi1LoopVZAh(p2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,Mhh,            & 
& Mhh2,MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2,cplAhAhhh,cplAhcHpVWp,cplAhhhVZ,cplAhHpcVWp,         & 
& cplcFdFdAhL,cplcFdFdAhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeAhL,cplcFeFeAhR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuAhL,cplcFuFuAhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWCgWCAh,cplcgWCgWCVZ,     & 
& cplcgWpgWpAh,cplcgWpgWpVZ,cplcHpVWpVZ,cplhhVZVZ,cplHpcVWpVZ,kont,DerPiVZAh)

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerPi1LoopVZAh(p2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,Mhh,            & 
& Mhh2,MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2,cplAhAhhh,cplAhcHpVWp,cplAhhhVZ,cplAhHpcVWp,         & 
& cplcFdFdAhL,cplcFdFdAhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeAhL,cplcFeFeAhR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuAhL,cplcFuFuAhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWCgWCAh,cplcgWCgWCVZ,     & 
& cplcgWpgWpAh,cplcgWpgWpVZ,cplcHpVWpVZ,cplhhVZVZ,cplHpcVWpVZ,kont,DerPiVZAhDR)

p2 =MAh2OS
Call DerPi1LoopVZAh(p2,MAhOS,MAh2OS,MFdOS,MFd2OS,MFeOS,MFe2OS,MFuOS,MFu2OS,           & 
& MFxvOS,MFxv2OS,MhhOS,Mhh2OS,MHpOS,MHp2OS,MVWpOS,MVWp2OS,MVZOS,MVZ2OS,cplAhAhhh,        & 
& GcplAhcHpVWp,cplAhhhVZ,GcplAhHpcVWp,cplcFdFdAhL,cplcFdFdAhR,cplcFdFdVZL,               & 
& cplcFdFdVZR,cplcFeFeAhL,cplcFeFeAhR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuAhL,               & 
& cplcFuFuAhR,cplcFuFuVZL,cplcFuFuVZR,cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFxvFxvVZL,         & 
& cplcFxvFxvVZR,cplcgWCgWCAh,cplcgWCgWCVZ,cplcgWpgWpAh,cplcgWpgWpVZ,GcplcHpVWpVZ,        & 
& cplhhVZVZ,GcplHpcVWpVZ,kont,DerPiVZAhOS)

DerPiVZAh = DerPiVZAh- DerPiVZAhDR + DerPiVZAhOS
IRdivonly=.False. 
End if
p2 = MVZ2
Call Pi1LoopVZAh(p2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,Mhh,               & 
& Mhh2,MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2,cplAhAhhh,cplAhcHpVWp,cplAhhhVZ,cplAhHpcVWp,         & 
& cplcFdFdAhL,cplcFdFdAhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeAhL,cplcFeFeAhR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuAhL,cplcFuFuAhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWCgWCAh,cplcgWCgWCVZ,     & 
& cplcgWpgWpAh,cplcgWpgWpVZ,cplcHpVWpVZ,cplhhVZVZ,cplHpcVWpVZ,kont,PiAhVZ)

Call DerPi1LoopVZAh(p2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,Mhh,            & 
& Mhh2,MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2,cplAhAhhh,cplAhcHpVWp,cplAhhhVZ,cplAhHpcVWp,         & 
& cplcFdFdAhL,cplcFdFdAhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeAhL,cplcFeFeAhR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuAhL,cplcFuFuAhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWCgWCAh,cplcgWCgWCVZ,     & 
& cplcgWpgWpAh,cplcgWpgWpVZ,cplcHpVWpVZ,cplhhVZVZ,cplHpcVWpVZ,kont,DerPiAhVZ)

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerPi1LoopVZAh(p2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,Mhh,            & 
& Mhh2,MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2,cplAhAhhh,cplAhcHpVWp,cplAhhhVZ,cplAhHpcVWp,         & 
& cplcFdFdAhL,cplcFdFdAhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeAhL,cplcFeFeAhR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuAhL,cplcFuFuAhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWCgWCAh,cplcgWCgWCVZ,     & 
& cplcgWpgWpAh,cplcgWpgWpVZ,cplcHpVWpVZ,cplhhVZVZ,cplHpcVWpVZ,kont,DerPiVZAhDR)

p2 =MVZ2OS
Call DerPi1LoopVZAh(p2,MAhOS,MAh2OS,MFdOS,MFd2OS,MFeOS,MFe2OS,MFuOS,MFu2OS,           & 
& MFxvOS,MFxv2OS,MhhOS,Mhh2OS,MHpOS,MHp2OS,MVWpOS,MVWp2OS,MVZOS,MVZ2OS,cplAhAhhh,        & 
& GcplAhcHpVWp,cplAhhhVZ,GcplAhHpcVWp,cplcFdFdAhL,cplcFdFdAhR,cplcFdFdVZL,               & 
& cplcFdFdVZR,cplcFeFeAhL,cplcFeFeAhR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuAhL,               & 
& cplcFuFuAhR,cplcFuFuVZL,cplcFuFuVZR,cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFxvFxvVZL,         & 
& cplcFxvFxvVZR,cplcgWCgWCAh,cplcgWCgWCVZ,cplcgWpgWpAh,cplcgWpgWpVZ,GcplcHpVWpVZ,        & 
& cplhhVZVZ,GcplHpcVWpVZ,kont,DerPiVZAhOS)

DerPiVZAh = DerPiVZAh- DerPiVZAhDR + DerPiVZAhOS
IRdivonly=.False. 
End if
!--------------------------
!VZ
!--------------------------
Do i1=1,2
p2 = Mhh2(i1)
Call Pi1LoopVZhh(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,MHp,MHp2,MVWp,              & 
& MVWp2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWCgWChh,cplcgWCgWCVZ,     & 
& cplcgWpgWphh,cplcgWpgWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,           & 
& cplhhHpcHp,cplhhHpcVWp,cplHpcHpVZ,cplHpcVWpVZ,kont,PiVZhh(i1,:,:))

Call DerPi1LoopVZhh(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,MHp,MHp2,MVWp,           & 
& MVWp2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWCgWChh,cplcgWCgWCVZ,     & 
& cplcgWpgWphh,cplcgWpgWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,           & 
& cplhhHpcHp,cplhhHpcVWp,cplHpcHpVZ,cplHpcVWpVZ,kont,DerPiVZhh(i1,:,:))

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerPi1LoopVZhh(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,MHp,MHp2,MVWp,           & 
& MVWp2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWCgWChh,cplcgWCgWCVZ,     & 
& cplcgWpgWphh,cplcgWpgWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,           & 
& cplhhHpcHp,cplhhHpcVWp,cplHpcHpVZ,cplHpcVWpVZ,kont,DerPiVZhhDR(i1,:,:))

p2 =Mhh2OS(i1)
Call DerPi1LoopVZhh(p2,MFdOS,MFd2OS,MFeOS,MFe2OS,MFuOS,MFu2OS,MFxvOS,MFxv2OS,         & 
& MHpOS,MHp2OS,MVWpOS,MVWp2OS,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,           & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,               & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,       & 
& cplcgWCgWChh,cplcgWCgWCVZ,cplcgWpgWphh,cplcgWpgWpVZ,GcplcHpVWpVZ,cplcVWpVWpVZ,         & 
& GcplhhcHpVWp,cplhhcVWpVWp,GcplhhHpcHp,GcplhhHpcVWp,GcplHpcHpVZ,GcplHpcVWpVZ,           & 
& kont,DerPiVZhhOS(i1,:,:))

DerPiVZhh(i1,:,:) = DerPiVZhh(i1,:,:)- DerPiVZhhDR(i1,:,:) + DerPiVZhhOS(i1,:,:)
IRdivonly=.False. 
End if
End do
p2 = MVZ2
Call Pi1LoopVZhh(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,MHp,MHp2,MVWp,              & 
& MVWp2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWCgWChh,cplcgWCgWCVZ,     & 
& cplcgWpgWphh,cplcgWpgWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,           & 
& cplhhHpcHp,cplhhHpcVWp,cplHpcHpVZ,cplHpcVWpVZ,kont,PihhVZ)

Call DerPi1LoopVZhh(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,MHp,MHp2,MVWp,           & 
& MVWp2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWCgWChh,cplcgWCgWCVZ,     & 
& cplcgWpgWphh,cplcgWpgWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,           & 
& cplhhHpcHp,cplhhHpcVWp,cplHpcHpVZ,cplHpcVWpVZ,kont,DerPihhVZ)

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerPi1LoopVZhh(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,MHp,MHp2,MVWp,           & 
& MVWp2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWCgWChh,cplcgWCgWCVZ,     & 
& cplcgWpgWphh,cplcgWpgWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,           & 
& cplhhHpcHp,cplhhHpcVWp,cplHpcHpVZ,cplHpcVWpVZ,kont,DerPiVZhhDR)

p2 =MVZ2OS
Call DerPi1LoopVZhh(p2,MFdOS,MFd2OS,MFeOS,MFe2OS,MFuOS,MFu2OS,MFxvOS,MFxv2OS,         & 
& MHpOS,MHp2OS,MVWpOS,MVWp2OS,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,           & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,               & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,       & 
& cplcgWCgWChh,cplcgWCgWCVZ,cplcgWpgWphh,cplcgWpgWpVZ,GcplcHpVWpVZ,cplcVWpVWpVZ,         & 
& GcplhhcHpVWp,cplhhcVWpVWp,GcplhhHpcHp,GcplhhHpcVWp,GcplHpcHpVZ,GcplHpcVWpVZ,           & 
& kont,DerPiVZhhOS)

DerPiVZhh = DerPiVZhh- DerPiVZhhDR + DerPiVZhhOS
IRdivonly=.False. 
End if
!--------------------------
!VWp
!--------------------------
p2 = MHp2
Call Pi1LoopVWpHp(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,MFxv,             & 
& MFxv2,Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2,cplcFdFucHpL,cplcFdFucHpR,cplcFeFvcHpL,    & 
& cplcFeFvcHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFxeFxvcHpL,       & 
& cplcFxeFxvcHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcgAgWCVWp,cplcgWCgAcHp,cplcgWCgZcHp,   & 
& cplcgWpgZVWp,cplcgZgWCVWp,cplcgZgWpcHp,cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,           & 
& cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,cplhhHpcHp,cplHpcHpVP,cplHpcHpVZ,kont,PiVWpHp)

Call DerPi1LoopVWpHp(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,               & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2,cplcFdFucHpL,cplcFdFucHpR,            & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFvFeVWpL,cplcFvFeVWpR,         & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcgAgWCVWp,              & 
& cplcgWCgAcHp,cplcgWCgZcHp,cplcgWpgZVWp,cplcgZgWCVWp,cplcgZgWpcHp,cplcHpVPVWp,          & 
& cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,cplhhHpcHp,             & 
& cplHpcHpVP,cplHpcHpVZ,kont,DerPiVWpHp)

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerPi1LoopVWpHp(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,               & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2,cplcFdFucHpL,cplcFdFucHpR,            & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFvFeVWpL,cplcFvFeVWpR,         & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcgAgWCVWp,              & 
& cplcgWCgAcHp,cplcgWCgZcHp,cplcgWpgZVWp,cplcgZgWCVWp,cplcgZgWpcHp,cplcHpVPVWp,          & 
& cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,cplhhHpcHp,             & 
& cplHpcHpVP,cplHpcHpVZ,kont,DerPiVWpHpDR)

p2 =MHp2OS
Call DerPi1LoopVWpHp(p2,MFdOS,MFd2OS,MFeOS,MFe2OS,MFuOS,MFu2OS,MFvOS,MFv2OS,          & 
& MFxeOS,MFxe2OS,MFxvOS,MFxv2OS,MhhOS,Mhh2OS,MHpOS,MHp2OS,MVWpOS,MVWp2OS,MVZOS,          & 
& MVZ2OS,GcplcFdFucHpL,GcplcFdFucHpR,GcplcFeFvcHpL,GcplcFeFvcHpR,cplcFuFdVWpL,           & 
& cplcFuFdVWpR,cplcFvFeVWpL,cplcFvFeVWpR,GcplcFxeFxvcHpL,GcplcFxeFxvcHpR,cplcFxvFxeVWpL, & 
& cplcFxvFxeVWpR,cplcgAgWCVWp,cplcgWCgAcHp,cplcgWCgZcHp,cplcgWpgZVWp,cplcgZgWCVWp,       & 
& cplcgZgWpcHp,GcplcHpVPVWp,GcplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,GcplhhcHpVWp,         & 
& cplhhcVWpVWp,GcplhhHpcHp,GcplHpcHpVP,GcplHpcHpVZ,kont,DerPiVWpHpOS)

DerPiVWpHp = DerPiVWpHp- DerPiVWpHpDR + DerPiVWpHpOS
IRdivonly=.False. 
End if
p2 = MVWp2
Call Pi1LoopVWpHp(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,MFxv,             & 
& MFxv2,Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2,cplcFdFucHpL,cplcFdFucHpR,cplcFeFvcHpL,    & 
& cplcFeFvcHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFxeFxvcHpL,       & 
& cplcFxeFxvcHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcgAgWCVWp,cplcgWCgAcHp,cplcgWCgZcHp,   & 
& cplcgWpgZVWp,cplcgZgWCVWp,cplcgZgWpcHp,cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,           & 
& cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,cplhhHpcHp,cplHpcHpVP,cplHpcHpVZ,kont,PiHpVWp)

Call DerPi1LoopVWpHp(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,               & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2,cplcFdFucHpL,cplcFdFucHpR,            & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFvFeVWpL,cplcFvFeVWpR,         & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcgAgWCVWp,              & 
& cplcgWCgAcHp,cplcgWCgZcHp,cplcgWpgZVWp,cplcgZgWCVWp,cplcgZgWpcHp,cplcHpVPVWp,          & 
& cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,cplhhHpcHp,             & 
& cplHpcHpVP,cplHpcHpVZ,kont,DerPiHpVWp)

If ((ShiftIRdiv).and.(OSkinematics)) Then 
IRdivonly=.True. 
Call DerPi1LoopVWpHp(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,               & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2,cplcFdFucHpL,cplcFdFucHpR,            & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFvFeVWpL,cplcFvFeVWpR,         & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcgAgWCVWp,              & 
& cplcgWCgAcHp,cplcgWCgZcHp,cplcgWpgZVWp,cplcgZgWCVWp,cplcgZgWpcHp,cplcHpVPVWp,          & 
& cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,cplhhHpcHp,             & 
& cplHpcHpVP,cplHpcHpVZ,kont,DerPiVWpHpDR)

p2 =MVWp2OS
Call DerPi1LoopVWpHp(p2,MFdOS,MFd2OS,MFeOS,MFe2OS,MFuOS,MFu2OS,MFvOS,MFv2OS,          & 
& MFxeOS,MFxe2OS,MFxvOS,MFxv2OS,MhhOS,Mhh2OS,MHpOS,MHp2OS,MVWpOS,MVWp2OS,MVZOS,          & 
& MVZ2OS,GcplcFdFucHpL,GcplcFdFucHpR,GcplcFeFvcHpL,GcplcFeFvcHpR,cplcFuFdVWpL,           & 
& cplcFuFdVWpR,cplcFvFeVWpL,cplcFvFeVWpR,GcplcFxeFxvcHpL,GcplcFxeFxvcHpR,cplcFxvFxeVWpL, & 
& cplcFxvFxeVWpR,cplcgAgWCVWp,cplcgWCgAcHp,cplcgWCgZcHp,cplcgWpgZVWp,cplcgZgWCVWp,       & 
& cplcgZgWpcHp,GcplcHpVPVWp,GcplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,GcplhhcHpVWp,         & 
& cplhhcVWpVWp,GcplhhHpcHp,GcplHpcHpVP,GcplHpcHpVZ,kont,DerPiVWpHpOS)

DerPiVWpHp = DerPiVWpHp- DerPiVWpHpDR + DerPiVWpHpOS
IRdivonly=.False. 
End if
! -----------------------------------------------------------
! Calculate now all wave-function renormalisation constants 
! -----------------------------------------------------------


!  ######    VG    ###### 
ZfVG = -DerPiVG


!  ######    Hp    ###### 
ZfHp = -DerPiHp


!  ######    ed    ###### 
Zfed = -SigmaRFxe + &
& -MFxe*(MFxe*DerSigmaRFxe+MFxe*DerSigmaLFxe+DerSigmaSRFxe+DerSigmaSLFxe)
If (OSkinematics) Then 
Zfed = Zfed &
& + -MFxeOS*(MFxeOS*DerSigmaRirFxe+MFxeOS*DerSigmaLirFxe+(DerSigmaSRirFxe+DerSigmaSLirFxe))
Else 
Zfed = Zfed &
& + -MFxe*(MFxe*DerSigmaRirFxe+MFxe*DerSigmaLirFxe+(DerSigmaSRirFxe+DerSigmaSLirFxe))
End if 


!  ######    eu    ###### 
Zfeu = -SigmaLFxe + &
& -MFxe*(MFxe*DerSigmaLFxe+MFxe*DerSigmaRFxe+DerSigmaSLFxe+DerSigmaSRFxe)
If (OSkinematics) Then 
Zfeu = Zfeu &
& + -MFxeOS*(MFxeOS*DerSigmaLirFxe+MFxeOS*DerSigmaRirFxe+(DerSigmaSLirFxe+DerSigmaSRirFxe))
Else 
Zfeu = Zfeu &
& + -MFxe*(MFxe*DerSigmaLirFxe+MFxe*DerSigmaRirFxe+(DerSigmaSLirFxe+DerSigmaSRirFxe))
End if 


!  ######    Ah    ###### 
ZfAh = -DerPiAh


!  ######    VP    ###### 
ZfVP = -DerPiVP


!  ######    VZ    ###### 
ZfVZ = -DerPiVZ


!  ######    VWp    ###### 
ZfVWp = -DerPiVWp


!  ######    hh    ###### 
Do i1=1,2
  Do i2=1,2
   If ((i1.eq.i2).or.(Mhh(i1).eq.Mhh(i2))) Then 
       Zfhh(i1,i2) = -DerPihh(i1,i1,i2)
   Else 
       Zfhh(i1,i2) = 2._dp/(Mhh2(i1)-Mhh2(i2))*Pihh(i2,i1,i2)
   End if 
  End Do 
End Do 


!  ######    DL    ###### 
Do i1=1,3
  Do i2=1,3
   If ((i1.eq.i2).or.(MFd(i1).eq.MFd(i2))) Then 
     ZfDL(i1,i2) = -SigmaRFd(i2,i1,i2) &
      & -MFd2(i1)*(DerSigmaRFd(i2,i1,i2) + DerSigmaLFd(i2,i1,i2))&
      & -MFd(i1)*(DerSigmaSRFd(i2,i1,i2)+DerSigmaSLFd(i2,i1,i2))
     If (OSkinematics) Then 
     ZfDL(i1,i2) = ZfDL(i1,i2) &
      & -MFd2OS(i1)*(DerSigmaRirFd(i2,i1,i2) + DerSigmaLirFd(i2,i1,i2))&
      & -MFdOS(i1)*(DerSigmaSRirFd(i2,i1,i2)+DerSigmaSLirFd(i2,i1,i2))
     Else 
     ZfDL(i1,i2) = ZfDL(i1,i2) &
      & -MFd2(i1)*(DerSigmaRirFd(i2,i1,i2) + DerSigmaLirFd(i2,i1,i2))&
      & -MFd(i1)*(DerSigmaSRirFd(i2,i1,i2)+DerSigmaSLirFd(i2,i1,i2))
     End if 
   Else 
     ZfDL(i1,i2) = 2._dp/(MFd2(i1) - MFd2(i2))* &
      & (MFd2(i2)*SigmaRFd(i2,i1,i2) + MFd(i1)*MFd(i2)*SigmaLFd(i2,i1,i2) + MFd(i1)*SigmaSRFd(i2,i1,i2) + MFd(i2)*SigmaSLFd(i2,i1,i2))
   End if 
  End Do 
End Do 


!  ######    DR    ###### 
Do i1=1,3
  Do i2=1,3
   If ((i1.eq.i2).or.(MFd(i1).eq.MFd(i2))) Then 
     ZfDR(i1,i2) = -SigmaLFd(i2,i1,i2) &
      & -MFd2(i1)*(DerSigmaLFd(i2,i1,i2) + DerSigmaRFd(i2,i1,i2))&
      & -MFd(i1)*(DerSigmaSLFd(i2,i1,i2)+DerSigmaSRFd(i2,i1,i2))
     If (OSkinematics) Then 
     ZfDR(i1,i2) = ZfDR(i1,i2) &
      & -MFd2OS(i1)*(DerSigmaLirFd(i2,i1,i2) + DerSigmaRirFd(i2,i1,i2))&
      & -MFdOS(i1)*(DerSigmaSLirFd(i2,i1,i2)+DerSigmaSRirFd(i2,i1,i2))
     Else 
     ZfDR(i1,i2) = ZfDR(i1,i2) &
      & -MFd2(i1)*(DerSigmaLirFd(i2,i1,i2) + DerSigmaRirFd(i2,i1,i2))&
      & -MFd(i1)*(DerSigmaSLirFd(i2,i1,i2)+DerSigmaSRirFd(i2,i1,i2))
     End if 
   Else 
     ZfDR(i1,i2) = 2._dp/(MFd2(i1) - MFd2(i2))* &
      & (MFd2(i2)*SigmaLFd(i2,i1,i2) + MFd(i1)*MFd(i2)*SigmaRFd(i2,i1,i2) + MFd(i1)*SigmaSLFd(i2,i1,i2) + MFd(i2)*SigmaSRFd(i2,i1,i2))
   End if 
  End Do 
End Do 


!  ######    UL    ###### 
Do i1=1,3
  Do i2=1,3
   If ((i1.eq.i2).or.(MFu(i1).eq.MFu(i2))) Then 
     ZfUL(i1,i2) = -SigmaRFu(i2,i1,i2) &
      & -MFu2(i1)*(DerSigmaRFu(i2,i1,i2) + DerSigmaLFu(i2,i1,i2))&
      & -MFu(i1)*(DerSigmaSRFu(i2,i1,i2)+DerSigmaSLFu(i2,i1,i2))
     If (OSkinematics) Then 
     ZfUL(i1,i2) = ZfUL(i1,i2) &
      & -MFu2OS(i1)*(DerSigmaRirFu(i2,i1,i2) + DerSigmaLirFu(i2,i1,i2))&
      & -MFuOS(i1)*(DerSigmaSRirFu(i2,i1,i2)+DerSigmaSLirFu(i2,i1,i2))
     Else 
     ZfUL(i1,i2) = ZfUL(i1,i2) &
      & -MFu2(i1)*(DerSigmaRirFu(i2,i1,i2) + DerSigmaLirFu(i2,i1,i2))&
      & -MFu(i1)*(DerSigmaSRirFu(i2,i1,i2)+DerSigmaSLirFu(i2,i1,i2))
     End if 
   Else 
     ZfUL(i1,i2) = 2._dp/(MFu2(i1) - MFu2(i2))* &
      & (MFu2(i2)*SigmaRFu(i2,i1,i2) + MFu(i1)*MFu(i2)*SigmaLFu(i2,i1,i2) + MFu(i1)*SigmaSRFu(i2,i1,i2) + MFu(i2)*SigmaSLFu(i2,i1,i2))
   End if 
  End Do 
End Do 


!  ######    UR    ###### 
Do i1=1,3
  Do i2=1,3
   If ((i1.eq.i2).or.(MFu(i1).eq.MFu(i2))) Then 
     ZfUR(i1,i2) = -SigmaLFu(i2,i1,i2) &
      & -MFu2(i1)*(DerSigmaLFu(i2,i1,i2) + DerSigmaRFu(i2,i1,i2))&
      & -MFu(i1)*(DerSigmaSLFu(i2,i1,i2)+DerSigmaSRFu(i2,i1,i2))
     If (OSkinematics) Then 
     ZfUR(i1,i2) = ZfUR(i1,i2) &
      & -MFu2OS(i1)*(DerSigmaLirFu(i2,i1,i2) + DerSigmaRirFu(i2,i1,i2))&
      & -MFuOS(i1)*(DerSigmaSLirFu(i2,i1,i2)+DerSigmaSRirFu(i2,i1,i2))
     Else 
     ZfUR(i1,i2) = ZfUR(i1,i2) &
      & -MFu2(i1)*(DerSigmaLirFu(i2,i1,i2) + DerSigmaRirFu(i2,i1,i2))&
      & -MFu(i1)*(DerSigmaSLirFu(i2,i1,i2)+DerSigmaSRirFu(i2,i1,i2))
     End if 
   Else 
     ZfUR(i1,i2) = 2._dp/(MFu2(i1) - MFu2(i2))* &
      & (MFu2(i2)*SigmaLFu(i2,i1,i2) + MFu(i1)*MFu(i2)*SigmaRFu(i2,i1,i2) + MFu(i1)*SigmaSLFu(i2,i1,i2) + MFu(i2)*SigmaSRFu(i2,i1,i2))
   End if 
  End Do 
End Do 


!  ######    EL    ###### 
Do i1=1,3
  Do i2=1,3
   If ((i1.eq.i2).or.(MFe(i1).eq.MFe(i2))) Then 
     ZfEL(i1,i2) = -SigmaRFe(i2,i1,i2) &
      & -MFe2(i1)*(DerSigmaRFe(i2,i1,i2) + DerSigmaLFe(i2,i1,i2))&
      & -MFe(i1)*(DerSigmaSRFe(i2,i1,i2)+DerSigmaSLFe(i2,i1,i2))
     If (OSkinematics) Then 
     ZfEL(i1,i2) = ZfEL(i1,i2) &
      & -MFe2OS(i1)*(DerSigmaRirFe(i2,i1,i2) + DerSigmaLirFe(i2,i1,i2))&
      & -MFeOS(i1)*(DerSigmaSRirFe(i2,i1,i2)+DerSigmaSLirFe(i2,i1,i2))
     Else 
     ZfEL(i1,i2) = ZfEL(i1,i2) &
      & -MFe2(i1)*(DerSigmaRirFe(i2,i1,i2) + DerSigmaLirFe(i2,i1,i2))&
      & -MFe(i1)*(DerSigmaSRirFe(i2,i1,i2)+DerSigmaSLirFe(i2,i1,i2))
     End if 
   Else 
     ZfEL(i1,i2) = 2._dp/(MFe2(i1) - MFe2(i2))* &
      & (MFe2(i2)*SigmaRFe(i2,i1,i2) + MFe(i1)*MFe(i2)*SigmaLFe(i2,i1,i2) + MFe(i1)*SigmaSRFe(i2,i1,i2) + MFe(i2)*SigmaSLFe(i2,i1,i2))
   End if 
  End Do 
End Do 


!  ######    ER    ###### 
Do i1=1,3
  Do i2=1,3
   If ((i1.eq.i2).or.(MFe(i1).eq.MFe(i2))) Then 
     ZfER(i1,i2) = -SigmaLFe(i2,i1,i2) &
      & -MFe2(i1)*(DerSigmaLFe(i2,i1,i2) + DerSigmaRFe(i2,i1,i2))&
      & -MFe(i1)*(DerSigmaSLFe(i2,i1,i2)+DerSigmaSRFe(i2,i1,i2))
     If (OSkinematics) Then 
     ZfER(i1,i2) = ZfER(i1,i2) &
      & -MFe2OS(i1)*(DerSigmaLirFe(i2,i1,i2) + DerSigmaRirFe(i2,i1,i2))&
      & -MFeOS(i1)*(DerSigmaSLirFe(i2,i1,i2)+DerSigmaSRirFe(i2,i1,i2))
     Else 
     ZfER(i1,i2) = ZfER(i1,i2) &
      & -MFe2(i1)*(DerSigmaLirFe(i2,i1,i2) + DerSigmaRirFe(i2,i1,i2))&
      & -MFe(i1)*(DerSigmaSLirFe(i2,i1,i2)+DerSigmaSRirFe(i2,i1,i2))
     End if 
   Else 
     ZfER(i1,i2) = 2._dp/(MFe2(i1) - MFe2(i2))* &
      & (MFe2(i2)*SigmaLFe(i2,i1,i2) + MFe(i1)*MFe(i2)*SigmaRFe(i2,i1,i2) + MFe(i1)*SigmaSLFe(i2,i1,i2) + MFe(i2)*SigmaSRFe(i2,i1,i2))
   End if 
  End Do 
End Do 


!  ######    VL    ###### 
Do i1=1,3
  Do i2=1,3
   If ((i1.eq.i2).or.(MFv(i1).eq.MFv(i2))) Then 
     ZfVL(i1,i2) = -SigmaRFv(i2,i1,i2) &
      & -MFv2(i1)*(DerSigmaRFv(i2,i1,i2) + DerSigmaLFv(i2,i1,i2))&
      & -MFv(i1)*(DerSigmaSRFv(i2,i1,i2)+DerSigmaSLFv(i2,i1,i2))
     If (OSkinematics) Then 
     ZfVL(i1,i2) = ZfVL(i1,i2) &
      & -MFv2OS(i1)*(DerSigmaRirFv(i2,i1,i2) + DerSigmaLirFv(i2,i1,i2))&
      & -MFvOS(i1)*(DerSigmaSRirFv(i2,i1,i2)+DerSigmaSLirFv(i2,i1,i2))
     Else 
     ZfVL(i1,i2) = ZfVL(i1,i2) &
      & -MFv2(i1)*(DerSigmaRirFv(i2,i1,i2) + DerSigmaLirFv(i2,i1,i2))&
      & -MFv(i1)*(DerSigmaSRirFv(i2,i1,i2)+DerSigmaSLirFv(i2,i1,i2))
     End if 
   Else 
     ZfVL(i1,i2) = 2._dp/(MFv2(i1) - MFv2(i2))* &
      & (MFv2(i2)*SigmaRFv(i2,i1,i2) + MFv(i1)*MFv(i2)*SigmaLFv(i2,i1,i2) + MFv(i1)*SigmaSRFv(i2,i1,i2) + MFv(i2)*SigmaSLFv(i2,i1,i2))
   End if 
  End Do 
End Do 


!  ######    VR    ###### 
Do i1=1,3
  Do i2=1,3
   If ((i1.eq.i2).or.(MFv(i1).eq.MFv(i2))) Then 
     ZfVR(i1,i2) = -SigmaLFv(i2,i1,i2) &
      & -MFv2(i1)*(DerSigmaLFv(i2,i1,i2) + DerSigmaRFv(i2,i1,i2))&
      & -MFv(i1)*(DerSigmaSLFv(i2,i1,i2)+DerSigmaSRFv(i2,i1,i2))
     If (OSkinematics) Then 
     ZfVR(i1,i2) = ZfVR(i1,i2) &
      & -MFv2OS(i1)*(DerSigmaLirFv(i2,i1,i2) + DerSigmaRirFv(i2,i1,i2))&
      & -MFvOS(i1)*(DerSigmaSLirFv(i2,i1,i2)+DerSigmaSRirFv(i2,i1,i2))
     Else 
     ZfVR(i1,i2) = ZfVR(i1,i2) &
      & -MFv2(i1)*(DerSigmaLirFv(i2,i1,i2) + DerSigmaRirFv(i2,i1,i2))&
      & -MFv(i1)*(DerSigmaSLirFv(i2,i1,i2)+DerSigmaSRirFv(i2,i1,i2))
     End if 
   Else 
     ZfVR(i1,i2) = 2._dp/(MFv2(i1) - MFv2(i2))* &
      & (MFv2(i2)*SigmaLFv(i2,i1,i2) + MFv(i1)*MFv(i2)*SigmaRFv(i2,i1,i2) + MFv(i1)*SigmaSLFv(i2,i1,i2) + MFv(i2)*SigmaSRFv(i2,i1,i2))
   End if 
  End Do 
End Do 


!  ######    xVL    ###### 
Do i1=1,2
  Do i2=1,2
   If ((i1.eq.i2).or.(MFxv(i1).eq.MFxv(i2))) Then 
     ZfxVL(i1,i2) = -SigmaRFxv(i2,i1,i2) &
      & -MFxv2(i1)*(DerSigmaRFxv(i2,i1,i2) + DerSigmaLFxv(i2,i1,i2))&
      & -MFxv(i1)*(DerSigmaSRFxv(i2,i1,i2)+DerSigmaSLFxv(i2,i1,i2))
     If (OSkinematics) Then 
     ZfxVL(i1,i2) = ZfxVL(i1,i2) &
      & -MFxv2OS(i1)*(DerSigmaRirFxv(i2,i1,i2) + DerSigmaLirFxv(i2,i1,i2))&
      & -MFxvOS(i1)*(DerSigmaSRirFxv(i2,i1,i2)+DerSigmaSLirFxv(i2,i1,i2))
     Else 
     ZfxVL(i1,i2) = ZfxVL(i1,i2) &
      & -MFxv2(i1)*(DerSigmaRirFxv(i2,i1,i2) + DerSigmaLirFxv(i2,i1,i2))&
      & -MFxv(i1)*(DerSigmaSRirFxv(i2,i1,i2)+DerSigmaSLirFxv(i2,i1,i2))
     End if 
   Else 
     ZfxVL(i1,i2) = 2._dp/(MFxv2(i1) - MFxv2(i2))* &
      & (MFxv2(i2)*SigmaRFxv(i2,i1,i2) + MFxv(i1)*MFxv(i2)*SigmaLFxv(i2,i1,i2) + MFxv(i1)*SigmaSRFxv(i2,i1,i2) + MFxv(i2)*SigmaSLFxv(i2,i1,i2))
   End if 
  End Do 
End Do 


!  ######    xVR    ###### 
Do i1=1,2
  Do i2=1,2
   If ((i1.eq.i2).or.(MFxv(i1).eq.MFxv(i2))) Then 
     ZfxVR(i1,i2) = -SigmaLFxv(i2,i1,i2) &
      & -MFxv2(i1)*(DerSigmaLFxv(i2,i1,i2) + DerSigmaRFxv(i2,i1,i2))&
      & -MFxv(i1)*(DerSigmaSLFxv(i2,i1,i2)+DerSigmaSRFxv(i2,i1,i2))
     If (OSkinematics) Then 
     ZfxVR(i1,i2) = ZfxVR(i1,i2) &
      & -MFxv2OS(i1)*(DerSigmaLirFxv(i2,i1,i2) + DerSigmaRirFxv(i2,i1,i2))&
      & -MFxvOS(i1)*(DerSigmaSLirFxv(i2,i1,i2)+DerSigmaSRirFxv(i2,i1,i2))
     Else 
     ZfxVR(i1,i2) = ZfxVR(i1,i2) &
      & -MFxv2(i1)*(DerSigmaLirFxv(i2,i1,i2) + DerSigmaRirFxv(i2,i1,i2))&
      & -MFxv(i1)*(DerSigmaSLirFxv(i2,i1,i2)+DerSigmaSRirFxv(i2,i1,i2))
     End if 
   Else 
     ZfxVR(i1,i2) = 2._dp/(MFxv2(i1) - MFxv2(i2))* &
      & (MFxv2(i2)*SigmaLFxv(i2,i1,i2) + MFxv(i1)*MFxv(i2)*SigmaRFxv(i2,i1,i2) + MFxv(i1)*SigmaSLFxv(i2,i1,i2) + MFxv(i2)*SigmaSRFxv(i2,i1,i2))
   End if 
  End Do 
End Do 


!  ######    Ssc    ###### 
Do i1=1,2
  Do i2=1,2
   If ((i1.eq.i2).or.(MSsc(i1).eq.MSsc(i2))) Then 
       ZfSsc(i1,i2) = -DerPiSsc(i1,i1,i2)
   Else 
       ZfSsc(i1,i2) = 2._dp/(MSsc2(i1)-MSsc2(i2))*PiSsc(i2,i1,i2)
   End if 
  End Do 
End Do 


!  ######    VPVZ    ###### 
ZfVPVZ = 2._dp*PiVPVZ/(MVP2-MVZ2 )
ZfVZVP = 2._dp*PiVZVP/(MVZ2-MVP2 )
! -----------------------------------------------------------
! Setting the Counter-Terms 
! -----------------------------------------------------------
! ----------- getting the divergent pieces ---------

 
 ! --- GUT normalize gauge couplings --- 
g1 = Sqrt(5._dp/3._dp)*g1 
! ----------------------- 
 
Call ParametersToG85(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,            & 
& YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,g1D)

TwoLoopRGEsave=TwoLoopRGE 
TwoLoopRGE=.False. 
Call rge85(85,0._dp,g1D,g1D) 
TwoLoopRGE=TwoLoopRGEsave 
Call GToParameters85(g1D,dg1,dg2,dg3,dLam,dLS1H,dLS,dLS2H,dLSP,dLSPH,dYu,             & 
& dYd,dYe,dYRD,dYRB1,dYRB2,dYRC,dYRA1,dYRA2,dMDF,dm2SM,dMS12,dMS22,dmP2,dvvSM,dvS)


 
 ! --- Remove GUT-normalization of gauge couplings --- 
dg1 = Sqrt(3._dp/5._dp)*dg1 
! ----------------------- 
 

 
 ! --- Remove GUT-normalization of gauge couplings --- 
g1 = Sqrt(3._dp/5._dp)*g1 
! ----------------------- 
 
dg1 = 0.5_dp*divergence*dg1 
dg2 = 0.5_dp*divergence*dg2 
dg3 = 0.5_dp*divergence*dg3 
dYu = 0.5_dp*divergence*dYu 
dYd = 0.5_dp*divergence*dYd 
dYe = 0.5_dp*divergence*dYe 
dm2SM = 0.5_dp*divergence*dm2SM 
dLam = 0.5_dp*divergence*dLam 
dMDF = 0.5_dp*divergence*dMDF 
dYRD = 0.5_dp*divergence*dYRD 
dMS12 = 0.5_dp*divergence*dMS12 
dMS22 = 0.5_dp*divergence*dMS22 
dLS1H = 0.5_dp*divergence*dLS1H 
dLS = 0.5_dp*divergence*dLS 
dLS2H = 0.5_dp*divergence*dLS2H 
dmP2 = 0.5_dp*divergence*dmP2 
dLSP = 0.5_dp*divergence*dLSP 
dLSPH = 0.5_dp*divergence*dLSPH 
dYRB1 = 0.5_dp*divergence*dYRB1 
dYRB2 = 0.5_dp*divergence*dYRB2 
dYRC = 0.5_dp*divergence*dYRC 
dYRA1 = 0.5_dp*divergence*dYRA1 
dYRA2 = 0.5_dp*divergence*dYRA2 
dvvSM = 0.5_dp*divergence*dvvSM 
dvS = 0.5_dp*divergence*dvS 
dZH = 0._dp 
dZDL = 0._dp 
dZDR = 0._dp 
dZUL = 0._dp 
dZUR = 0._dp 
dZEL = 0._dp 
dZER = 0._dp 
dUV = 0._dp 
dUVR = 0._dp 
dXV = 0._dp 
dXU = 0._dp 
dVSs = 0._dp 
dSinTW = 0._dp 
dCosTW = 0._dp 
dTanTW = 0._dp 
If (CTinLoopDecays) Then 
dCosTW = ((PiVWp/MVWp**2 - PiVZ/mVZ**2)*Cos(TW))/2._dp 
dSinTW = -(dCosTW*1/Tan(TW)) 
dg2 = (g2*(derPiVPheavy0 + PiVPlightMZ/MVZ**2 - (-(PiVWp/MVWp**2) + PiVZ/MVZ**2)*1/Tan(TW)**2 + (2*PiVZVP*Tan(TW))/MVZ**2))/2._dp 
dg1 = dSinTW*g2*1/Cos(TW) + dg2*Tan(TW) - dCosTW*g2*1/Cos(TW)*Tan(TW) 
End if 
 
dZDR = 0.25_dp*MatMul(ZfDR- Conjg(Transpose(ZfDR)),ZDR)
dZER = 0.25_dp*MatMul(ZfER- Conjg(Transpose(ZfER)),ZER)
dZUR = 0.25_dp*MatMul(ZfUR- Conjg(Transpose(ZfUR)),ZUR)
dZDL = 0.25_dp*MatMul(ZfDL- Conjg(Transpose(ZfDL)),ZDL)
dZEL = 0.25_dp*MatMul(ZfEL- Conjg(Transpose(ZfEL)),ZEL)
dVSs = 0.25_dp*MatMul(ZfSsc- Conjg(Transpose(ZfSsc)),VSs)
dZUL = 0.25_dp*MatMul(ZfUL- Conjg(Transpose(ZfUL)),ZUL)
dUV = 0.25_dp*MatMul(ZfVL- Conjg(Transpose(ZfVL)),UV)
dUVR = 0.25_dp*MatMul(ZfVR- Conjg(Transpose(ZfVR)),UVR)
dXU = 0.25_dp*MatMul(ZfxVR- Conjg(Transpose(ZfxVR)),XU)
dXV = 0.25_dp*MatMul(ZfxVL- Conjg(Transpose(ZfxVL)),XV)
dZH = 0.25_dp*MatMul(Zfhh- Conjg(Transpose(Zfhh)),ZH)


! -----------------------------------------------------------
! Calculating the CT vertices 
! -----------------------------------------------------------
Call CalculateCouplingCT(Lam,LSPH,vvSM,vS,ZH,LSP,LS1H,LS2H,VSs,g1,g2,TW,              & 
& g3,Yd,ZDL,ZDR,Ye,ZEL,ZER,Yu,ZUL,ZUR,YRD,XV,XU,UV,YRA1,YRA2,YRB1,YRB2,UVR,              & 
& YRC,dLam,dLSPH,dvvSM,dvS,dZH,dLSP,dLS1H,dLS2H,dVSs,dg1,dg2,dSinTW,dCosTW,              & 
& dTanTW,dg3,dYd,dZDL,dZDR,dYe,dZEL,dZER,dYu,dZUL,dZUR,dYRD,dXV,dXU,dUV,dYRA1,           & 
& dYRA2,dYRB1,dYRB2,dUVR,dYRC,ctcplAhAhhh,ctcplhhhhhh,ctcplhhHpcHp,ctcplhhSsccSsc,       & 
& ctcplAhhhVZ,ctcplAhHpcVWp,ctcplAhcHpVWp,ctcplhhHpcVWp,ctcplhhcHpVWp,ctcplHpcHpVP,      & 
& ctcplHpcHpVZ,ctcplhhcVWpVWp,ctcplhhVZVZ,ctcplHpcVWpVP,ctcplHpcVWpVZ,ctcplcHpVPVWp,     & 
& ctcplcHpVWpVZ,ctcplVGVGVG,ctcplcVWpVPVWp,ctcplcVWpVWpVZ,ctcplcFdFdAhL,ctcplcFdFdAhR,   & 
& ctcplcFeFeAhL,ctcplcFeFeAhR,ctcplcFuFuAhL,ctcplcFuFuAhR,ctcplcFxvFxvAhL,               & 
& ctcplcFxvFxvAhR,ctcplcFdFdhhL,ctcplcFdFdhhR,ctcplcFuFdHpL,ctcplcFuFdHpR,               & 
& ctcplcFeFehhL,ctcplcFeFehhR,ctcplcFvFeHpL,ctcplcFvFeHpR,ctcplcFxeFeSscL,               & 
& ctcplcFxeFeSscR,ctcplcFuFuhhL,ctcplcFuFuhhR,ctcplcFdFucHpL,ctcplcFdFucHpR,             & 
& ctcplcFxvFvSscL,ctcplcFxvFvSscR,ctcplcFeFvcHpL,ctcplcFeFvcHpR,ctcplcFxvFxeHpL,         & 
& ctcplcFxvFxeHpR,ctcplcFeFxecSscL,ctcplcFeFxecSscR,ctcplcFxvFxvhhL,ctcplcFxvFxvhhR,     & 
& ctcplcFvFxvcSscL,ctcplcFvFxvcSscR,ctcplcFxeFxvcHpL,ctcplcFxeFxvcHpR,ctcplcFdFdVGL,     & 
& ctcplcFdFdVGR,ctcplcFdFdVPL,ctcplcFdFdVPR,ctcplcFuFdVWpL,ctcplcFuFdVWpR,               & 
& ctcplcFdFdVZL,ctcplcFdFdVZR,ctcplcFeFeVPL,ctcplcFeFeVPR,ctcplcFvFeVWpL,ctcplcFvFeVWpR, & 
& ctcplcFeFeVZL,ctcplcFeFeVZR,ctcplcFuFuVGL,ctcplcFuFuVGR,ctcplcFuFuVPL,ctcplcFuFuVPR,   & 
& ctcplcFuFuVZL,ctcplcFuFuVZR,ctcplcFdFucVWpL,ctcplcFdFucVWpR,ctcplcFvFvVZL,             & 
& ctcplcFvFvVZR,ctcplcFeFvcVWpL,ctcplcFeFvcVWpR,ctcplcFxeFxeVPL,ctcplcFxeFxeVPR,         & 
& ctcplcFxvFxeVWpL,ctcplcFxvFxeVWpR,ctcplcFxeFxeVZL,ctcplcFxeFxeVZR,ctcplcFxvFxvVZL,     & 
& ctcplcFxvFxvVZR,ctcplcFxeFxvcVWpL,ctcplcFxeFxvcVWpR)

End Subroutine WaveFunctionRenormalisation 
Subroutine CalculateOneLoopDecays(gP1LFu,gP1LFe,gP1LFd,gP1Lhh,gP1LSsc,gP1LFxe,        & 
& gP1LFxv,MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,MFeOS,MFe2OS,MFvOS,MFv2OS,              & 
& MFxvOS,MFxv2OS,MSscOS,MSsc2OS,MFxeOS,MFxe2OS,MHpOS,MHp2OS,MAhOS,MAh2OS,MVZOS,          & 
& MVZ2OS,MVWpOS,MVWp2OS,ZHOS,ZDLOS,ZDROS,ZULOS,ZUROS,ZELOS,ZEROS,UVOS,UVROS,             & 
& XVOS,XUOS,VSsOS,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,               & 
& YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,epsI,deltaM,kont)

Implicit None 
Real(dp), Intent(in) :: epsI, deltaM 
Integer, Intent(inout) :: kont 
Real(dp) :: MLambda, em, gs, vSM, sinW2, g1SM, g2SM 
Integer :: divset, i1 
Complex(dp) :: divvalue, YuSM(3,3), YdSM(3,3), YeSM(3,3) 
Real(dp),Intent(inout) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2

Complex(dp),Intent(inout) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Real(dp),Intent(inout) :: vvSM,vS

Real(dp) :: dg1,dg2,dg3,dm2SM,dMDF,dYRD,dMS12,dMS22,dLS1H,dLS,dLS2H,dmP2,dLSP,dLSPH,              & 
& dYRB1(3),dYRB2(3),dYRC,dYRA1(3),dYRA2(3),dvvSM,dvS,dZH(2,2),dVSs(2,2),dSinTW,          & 
& dCosTW,dTanTW

Complex(dp) :: dYu(3,3),dYd(3,3),dYe(3,3),dLam,dZDL(3,3),dZDR(3,3),dZUL(3,3),dZUR(3,3),              & 
& dZEL(3,3),dZER(3,3),dUV(3,3),dUVR(3,3),dXV(2,2),dXU(2,2)

Complex(dp) :: ZfVG,ZfHp,Zfed,Zfeu,ZfAh,ZfVP,ZfVZ,ZfVWp,Zfhh(2,2),ZfDL(3,3),ZfDR(3,3),               & 
& ZfUL(3,3),ZfUR(3,3),ZfEL(3,3),ZfER(3,3),ZfVL(3,3),ZfVR(3,3),ZfxVL(2,2),ZfxVR(2,2),     & 
& ZfSsc(2,2),ZfVPVZ,ZfVZVP

Real(dp),Intent(in) :: MhhOS(2),Mhh2OS(2),MFdOS(3),MFd2OS(3),MFuOS(3),MFu2OS(3),MFeOS(3),MFe2OS(3),          & 
& MFvOS(3),MFv2OS(3),MFxvOS(2),MFxv2OS(2),MSscOS(2),MSsc2OS(2),MFxeOS,MFxe2OS,           & 
& MHpOS,MHp2OS,MAhOS,MAh2OS,MVZOS,MVZ2OS,MVWpOS,MVWp2OS,ZHOS(2,2),VSsOS(2,2)

Complex(dp),Intent(in) :: ZDLOS(3,3),ZDROS(3,3),ZULOS(3,3),ZUROS(3,3),ZELOS(3,3),ZEROS(3,3),UVOS(3,3),          & 
& UVROS(3,3),XVOS(2,2),XUOS(2,2)

Real(dp) :: p2, q2, q2_save 
Real(dp) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Complex(dp) :: cplAhAhAhAh1,cplAhAhhhhh1(2,2),cplAhAhHpcHp1,cplAhAhSsccSsc1(2,2),cplhhhhhhhh1(2,2,2,2),& 
& cplhhhhHpcHp1(2,2),cplhhhhSsccSsc1(2,2,2,2),cplHpHpcHpcHp1,cplHpSsccHpcSsc1(2,2),      & 
& cplSscSsccSsccSsc1(2,2,2,2),cplAhAhcVWpVWp1,cplAhAhVZVZ1,cplAhHpcVWpVP1,               & 
& cplAhHpcVWpVZ1,cplAhcHpVPVWp1,cplAhcHpVWpVZ1,cplhhhhcVWpVWp1(2,2),cplhhhhVZVZ1(2,2),   & 
& cplhhHpcVWpVP1(2),cplhhHpcVWpVZ1(2),cplhhcHpVPVWp1(2),cplhhcHpVWpVZ1(2),               & 
& cplHpcHpVPVP1,cplHpcHpVPVZ1,cplHpcHpcVWpVWp1,cplHpcHpVZVZ1,cplVGVGVGVG1Q,              & 
& cplVGVGVGVG2Q,cplVGVGVGVG3Q,cplcVWpVPVPVWp1Q,cplcVWpVPVPVWp2Q,cplcVWpVPVPVWp3Q,        & 
& cplcVWpVPVWpVZ1Q,cplcVWpVPVWpVZ2Q,cplcVWpVPVWpVZ3Q,cplcVWpcVWpVWpVWp1Q,cplcVWpcVWpVWpVWp2Q,& 
& cplcVWpcVWpVWpVWp3Q,cplcVWpVWpVZVZ1Q,cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q

Complex(dp) :: cplAhAhhh(2),cplhhhhhh(2,2,2),cplhhHpcHp(2),cplhhSsccSsc(2,2,2),cplAhAhAhAh,          & 
& cplAhAhhhhh(2,2),cplAhAhHpcHp,cplAhAhSsccSsc(2,2),cplhhhhhhhh(2,2,2,2),cplhhhhHpcHp(2,2),& 
& cplhhhhSsccSsc(2,2,2,2),cplHpHpcHpcHp,cplHpSsccHpcSsc(2,2),cplSscSsccSsccSsc(2,2,2,2), & 
& cplAhhhVZ(2),cplAhHpcVWp,cplAhcHpVWp,cplhhHpcVWp(2),cplhhcHpVWp(2),cplHpcHpVP,         & 
& cplHpcHpVZ,cplhhcVWpVWp(2),cplhhVZVZ(2),cplHpcVWpVP,cplHpcVWpVZ,cplcHpVPVWp,           & 
& cplcHpVWpVZ,cplAhAhcVWpVWp,cplAhAhVZVZ,cplAhHpcVWpVP,cplAhHpcVWpVZ,cplAhcHpVPVWp,      & 
& cplAhcHpVWpVZ,cplhhhhcVWpVWp(2,2),cplhhhhVZVZ(2,2),cplhhHpcVWpVP(2),cplhhHpcVWpVZ(2),  & 
& cplhhcHpVPVWp(2),cplhhcHpVWpVZ(2),cplHpcHpVPVP,cplHpcHpVPVZ,cplHpcHpcVWpVWp,           & 
& cplHpcHpVZVZ,cplVGVGVG,cplcVWpVPVWp,cplcVWpVWpVZ,cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),    & 
& cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFxvFxvAhL(2,2),& 
& cplcFxvFxvAhR(2,2),cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFuFdHpL(3,3),             & 
& cplcFuFdHpR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFvFeHpL(3,3),               & 
& cplcFvFeHpR(3,3),cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFuFuhhL(3,3,2),             & 
& cplcFuFuhhR(3,3,2),cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFxvFvSscL(2,3,2),           & 
& cplcFxvFvSscR(2,3,2),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFxvFxeHpL(2),             & 
& cplcFxvFxeHpR(2),cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFxvFxvhhL(2,2,2),         & 
& cplcFxvFxvhhR(2,2,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),cplcFxeFxvcHpL(2),    & 
& cplcFxeFxvcHpR(2),cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3), & 
& cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVPL(3,3),& 
& cplcFeFeVPR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),& 
& cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),  & 
& cplcFuFuVZR(3,3),cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),cplcFvFvVZL(3,3),               & 
& cplcFvFvVZR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFxeFxeVPL,cplcFxeFxeVPR,    & 
& cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvVZL(2,2),    & 
& cplcFxvFxvVZR(2,2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplVGVGVGVG1,cplVGVGVGVG2,    & 
& cplVGVGVGVG3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,cplcVWpVPVPVWp3,cplcVWpVPVWpVZ1,          & 
& cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpcVWpVWpVWp1,cplcVWpcVWpVWpVWp2,cplcVWpcVWpVWpVWp3,& 
& cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,cplcgGgGVG,cplcgWpgAVWp,               & 
& cplcgWCgAcVWp,cplcgWpgWpVP,cplcgWpgWpVZ,cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgWCVP,      & 
& cplcgAgWCVWp,cplcgZgWCVWp,cplcgWCgWCVZ,cplcgWpgZVWp,cplcgWCgZcVWp,cplcgWpgWpAh,        & 
& cplcgWCgWCAh,cplcgZgAhh(2),cplcgWpgAHp,cplcgWCgAcHp,cplcgWpgWphh(2),cplcgZgWpcHp,      & 
& cplcgWCgWChh(2),cplcgZgWCHp,cplcgZgZhh(2),cplcgWpgZHp,cplcgWCgZcHp

Complex(dp) :: ctcplAhAhhh(2),ctcplhhhhhh(2,2,2),ctcplhhHpcHp(2),ctcplhhSsccSsc(2,2,2),              & 
& ctcplAhhhVZ(2),ctcplAhHpcVWp,ctcplAhcHpVWp,ctcplhhHpcVWp(2),ctcplhhcHpVWp(2),          & 
& ctcplHpcHpVP,ctcplHpcHpVZ,ctcplhhcVWpVWp(2),ctcplhhVZVZ(2),ctcplHpcVWpVP,              & 
& ctcplHpcVWpVZ,ctcplcHpVPVWp,ctcplcHpVWpVZ,ctcplVGVGVG,ctcplcVWpVPVWp,ctcplcVWpVWpVZ,   & 
& ctcplcFdFdAhL(3,3),ctcplcFdFdAhR(3,3),ctcplcFeFeAhL(3,3),ctcplcFeFeAhR(3,3),           & 
& ctcplcFuFuAhL(3,3),ctcplcFuFuAhR(3,3),ctcplcFxvFxvAhL(2,2),ctcplcFxvFxvAhR(2,2),       & 
& ctcplcFdFdhhL(3,3,2),ctcplcFdFdhhR(3,3,2),ctcplcFuFdHpL(3,3),ctcplcFuFdHpR(3,3),       & 
& ctcplcFeFehhL(3,3,2),ctcplcFeFehhR(3,3,2),ctcplcFvFeHpL(3,3),ctcplcFvFeHpR(3,3),       & 
& ctcplcFxeFeSscL(3,2),ctcplcFxeFeSscR(3,2),ctcplcFuFuhhL(3,3,2),ctcplcFuFuhhR(3,3,2),   & 
& ctcplcFdFucHpL(3,3),ctcplcFdFucHpR(3,3),ctcplcFxvFvSscL(2,3,2),ctcplcFxvFvSscR(2,3,2), & 
& ctcplcFeFvcHpL(3,3),ctcplcFeFvcHpR(3,3),ctcplcFxvFxeHpL(2),ctcplcFxvFxeHpR(2),         & 
& ctcplcFeFxecSscL(3,2),ctcplcFeFxecSscR(3,2),ctcplcFxvFxvhhL(2,2,2),ctcplcFxvFxvhhR(2,2,2),& 
& ctcplcFvFxvcSscL(3,2,2),ctcplcFvFxvcSscR(3,2,2),ctcplcFxeFxvcHpL(2),ctcplcFxeFxvcHpR(2),& 
& ctcplcFdFdVGL(3,3),ctcplcFdFdVGR(3,3),ctcplcFdFdVPL(3,3),ctcplcFdFdVPR(3,3),           & 
& ctcplcFuFdVWpL(3,3),ctcplcFuFdVWpR(3,3),ctcplcFdFdVZL(3,3),ctcplcFdFdVZR(3,3),         & 
& ctcplcFeFeVPL(3,3),ctcplcFeFeVPR(3,3),ctcplcFvFeVWpL(3,3),ctcplcFvFeVWpR(3,3),         & 
& ctcplcFeFeVZL(3,3),ctcplcFeFeVZR(3,3),ctcplcFuFuVGL(3,3),ctcplcFuFuVGR(3,3),           & 
& ctcplcFuFuVPL(3,3),ctcplcFuFuVPR(3,3),ctcplcFuFuVZL(3,3),ctcplcFuFuVZR(3,3),           & 
& ctcplcFdFucVWpL(3,3),ctcplcFdFucVWpR(3,3),ctcplcFvFvVZL(3,3),ctcplcFvFvVZR(3,3),       & 
& ctcplcFeFvcVWpL(3,3),ctcplcFeFvcVWpR(3,3),ctcplcFxeFxeVPL,ctcplcFxeFxeVPR,             & 
& ctcplcFxvFxeVWpL(2),ctcplcFxvFxeVWpR(2),ctcplcFxeFxeVZL,ctcplcFxeFxeVZR,               & 
& ctcplcFxvFxvVZL(2,2),ctcplcFxvFxvVZR(2,2),ctcplcFxeFxvcVWpL(2),ctcplcFxeFxvcVWpR(2)

Complex(dp) :: ZRUZH(2,2),ZRUVd(3,3),ZRUUd(3,3),ZRUVu(3,3),ZRUUu(3,3),ZRUVe(3,3),ZRUUe(3,3),         & 
& ZRUVv(3,3),ZRUVvr(3,3),ZRUXV(2,2),ZRUXU(2,2),ZRUVSs(2,2)

Complex(dp) :: ZcplAhAhhh(2),Zcplhhhhhh(2,2,2),ZcplhhHpcHp(2),ZcplhhSsccSsc(2,2,2),ZcplAhAhAhAh,     & 
& ZcplAhAhhhhh(2,2),ZcplAhAhHpcHp,ZcplAhAhSsccSsc(2,2),Zcplhhhhhhhh(2,2,2,2),            & 
& ZcplhhhhHpcHp(2,2),ZcplhhhhSsccSsc(2,2,2,2),ZcplHpHpcHpcHp,ZcplHpSsccHpcSsc(2,2),      & 
& ZcplSscSsccSsccSsc(2,2,2,2),ZcplAhhhVZ(2),ZcplAhHpcVWp,ZcplAhcHpVWp,ZcplhhHpcVWp(2),   & 
& ZcplhhcHpVWp(2),ZcplHpcHpVP,ZcplHpcHpVZ,ZcplhhcVWpVWp(2),ZcplhhVZVZ(2),ZcplHpcVWpVP,   & 
& ZcplHpcVWpVZ,ZcplcHpVPVWp,ZcplcHpVWpVZ,ZcplAhAhcVWpVWp,ZcplAhAhVZVZ,ZcplAhHpcVWpVP,    & 
& ZcplAhHpcVWpVZ,ZcplAhcHpVPVWp,ZcplAhcHpVWpVZ,ZcplhhhhcVWpVWp(2,2),ZcplhhhhVZVZ(2,2),   & 
& ZcplhhHpcVWpVP(2),ZcplhhHpcVWpVZ(2),ZcplhhcHpVPVWp(2),ZcplhhcHpVWpVZ(2),               & 
& ZcplHpcHpVPVP,ZcplHpcHpVPVZ,ZcplHpcHpcVWpVWp,ZcplHpcHpVZVZ,ZcplVGVGVG,ZcplcVWpVPVWp,   & 
& ZcplcVWpVWpVZ,ZcplcFdFdAhL(3,3),ZcplcFdFdAhR(3,3),ZcplcFeFeAhL(3,3),ZcplcFeFeAhR(3,3), & 
& ZcplcFuFuAhL(3,3),ZcplcFuFuAhR(3,3),ZcplcFxvFxvAhL(2,2),ZcplcFxvFxvAhR(2,2),           & 
& ZcplcFdFdhhL(3,3,2),ZcplcFdFdhhR(3,3,2),ZcplcFuFdHpL(3,3),ZcplcFuFdHpR(3,3),           & 
& ZcplcFeFehhL(3,3,2),ZcplcFeFehhR(3,3,2),ZcplcFvFeHpL(3,3),ZcplcFvFeHpR(3,3),           & 
& ZcplcFxeFeSscL(3,2),ZcplcFxeFeSscR(3,2),ZcplcFuFuhhL(3,3,2),ZcplcFuFuhhR(3,3,2),       & 
& ZcplcFdFucHpL(3,3),ZcplcFdFucHpR(3,3),ZcplcFxvFvSscL(2,3,2),ZcplcFxvFvSscR(2,3,2),     & 
& ZcplcFeFvcHpL(3,3),ZcplcFeFvcHpR(3,3),ZcplcFxvFxeHpL(2),ZcplcFxvFxeHpR(2),             & 
& ZcplcFeFxecSscL(3,2),ZcplcFeFxecSscR(3,2),ZcplcFxvFxvhhL(2,2,2),ZcplcFxvFxvhhR(2,2,2), & 
& ZcplcFvFxvcSscL(3,2,2),ZcplcFvFxvcSscR(3,2,2),ZcplcFxeFxvcHpL(2),ZcplcFxeFxvcHpR(2),   & 
& ZcplcFdFdVGL(3,3),ZcplcFdFdVGR(3,3),ZcplcFdFdVPL(3,3),ZcplcFdFdVPR(3,3),               & 
& ZcplcFuFdVWpL(3,3),ZcplcFuFdVWpR(3,3),ZcplcFdFdVZL(3,3),ZcplcFdFdVZR(3,3),             & 
& ZcplcFeFeVPL(3,3),ZcplcFeFeVPR(3,3),ZcplcFvFeVWpL(3,3),ZcplcFvFeVWpR(3,3),             & 
& ZcplcFeFeVZL(3,3),ZcplcFeFeVZR(3,3),ZcplcFuFuVGL(3,3),ZcplcFuFuVGR(3,3),               & 
& ZcplcFuFuVPL(3,3),ZcplcFuFuVPR(3,3),ZcplcFuFuVZL(3,3),ZcplcFuFuVZR(3,3),               & 
& ZcplcFdFucVWpL(3,3),ZcplcFdFucVWpR(3,3),ZcplcFvFvVZL(3,3),ZcplcFvFvVZR(3,3),           & 
& ZcplcFeFvcVWpL(3,3),ZcplcFeFvcVWpR(3,3),ZcplcFxeFxeVPL,ZcplcFxeFxeVPR,ZcplcFxvFxeVWpL(2),& 
& ZcplcFxvFxeVWpR(2),ZcplcFxeFxeVZL,ZcplcFxeFxeVZR,ZcplcFxvFxvVZL(2,2),ZcplcFxvFxvVZR(2,2),& 
& ZcplcFxeFxvcVWpL(2),ZcplcFxeFxvcVWpR(2),ZcplVGVGVGVG1,ZcplVGVGVGVG2,ZcplVGVGVGVG3,     & 
& ZcplcVWpVPVPVWp1,ZcplcVWpVPVPVWp2,ZcplcVWpVPVPVWp3,ZcplcVWpVPVWpVZ1,ZcplcVWpVPVWpVZ2,  & 
& ZcplcVWpVPVWpVZ3,ZcplcVWpcVWpVWpVWp1,ZcplcVWpcVWpVWpVWp2,ZcplcVWpcVWpVWpVWp3,          & 
& ZcplcVWpVWpVZVZ1,ZcplcVWpVWpVZVZ2,ZcplcVWpVWpVZVZ3,ZcplcgGgGVG,ZcplcgWpgAVWp,          & 
& ZcplcgWCgAcVWp,ZcplcgWpgWpVP,ZcplcgWpgWpVZ,ZcplcgAgWpcVWp,ZcplcgZgWpcVWp,              & 
& ZcplcgWCgWCVP,ZcplcgAgWCVWp,ZcplcgZgWCVWp,ZcplcgWCgWCVZ,ZcplcgWpgZVWp,ZcplcgWCgZcVWp,  & 
& ZcplcgWpgWpAh,ZcplcgWCgWCAh,ZcplcgZgAhh(2),ZcplcgWpgAHp,ZcplcgWCgAcHp,ZcplcgWpgWphh(2),& 
& ZcplcgZgWpcHp,ZcplcgWCgWChh(2),ZcplcgZgWCHp,ZcplcgZgZhh(2),ZcplcgWpgZHp,               & 
& ZcplcgWCgZcHp

Complex(dp) :: GcplhhHpcHp(2),GcplAhHpcVWp,GcplAhcHpVWp,GcplhhHpcVWp(2),GcplhhcHpVWp(2),             & 
& GcplHpcHpVP,GcplHpcHpVZ,GcplHpcVWpVP,GcplHpcVWpVZ,GcplcHpVPVWp,GcplcHpVWpVZ,           & 
& GcplcFuFdHpL(3,3),GcplcFuFdHpR(3,3),GcplcFvFeHpL(3,3),GcplcFvFeHpR(3,3),               & 
& GcplcFdFucHpL(3,3),GcplcFdFucHpR(3,3),GcplcFeFvcHpL(3,3),GcplcFeFvcHpR(3,3),           & 
& GcplcFxvFxeHpL(2),GcplcFxvFxeHpR(2),GcplcFxeFxvcHpL(2),GcplcFxeFxvcHpR(2)

Complex(dp) :: GZcplhhHpcHp(2),GZcplAhHpcVWp,GZcplAhcHpVWp,GZcplhhHpcVWp(2),GZcplhhcHpVWp(2),        & 
& GZcplHpcHpVP,GZcplHpcHpVZ,GZcplHpcVWpVP,GZcplHpcVWpVZ,GZcplcHpVPVWp,GZcplcHpVWpVZ,     & 
& GZcplcFuFdHpL(3,3),GZcplcFuFdHpR(3,3),GZcplcFvFeHpL(3,3),GZcplcFvFeHpR(3,3),           & 
& GZcplcFdFucHpL(3,3),GZcplcFdFucHpR(3,3),GZcplcFeFvcHpL(3,3),GZcplcFeFvcHpR(3,3),       & 
& GZcplcFxvFxeHpL(2),GZcplcFxvFxeHpR(2),GZcplcFxeFxvcHpL(2),GZcplcFxeFxvcHpR(2)

Complex(dp) :: GosZcplhhHpcHp(2),GosZcplAhHpcVWp,GosZcplAhcHpVWp,GosZcplhhHpcVWp(2),GosZcplhhcHpVWp(2),& 
& GosZcplHpcHpVP,GosZcplHpcHpVZ,GosZcplHpcVWpVP,GosZcplHpcVWpVZ,GosZcplcHpVPVWp,         & 
& GosZcplcHpVWpVZ,GosZcplcFuFdHpL(3,3),GosZcplcFuFdHpR(3,3),GosZcplcFvFeHpL(3,3),        & 
& GosZcplcFvFeHpR(3,3),GosZcplcFdFucHpL(3,3),GosZcplcFdFucHpR(3,3),GosZcplcFeFvcHpL(3,3),& 
& GosZcplcFeFvcHpR(3,3),GosZcplcFxvFxeHpL(2),GosZcplcFxvFxeHpR(2),GosZcplcFxeFxvcHpL(2), & 
& GosZcplcFxeFxvcHpR(2)

Real(dp), Intent(out) :: gP1LFu(3,18) 
Real(dp), Intent(out) :: gP1LFe(3,17) 
Real(dp), Intent(out) :: gP1LFd(3,18) 
Real(dp), Intent(out) :: gP1Lhh(2,61) 
Real(dp), Intent(out) :: gP1LSsc(2,17) 
Real(dp), Intent(out) :: gP1LFxe(1,12) 
Real(dp), Intent(out) :: gP1LFxv(2,15) 
Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateOneLoopDecays'
 
Write(*,*) "Calculating one loop decays" 
! Regulator mass for gluon/photon 
MLambda = Mass_Regulator_PhotonGluon 
divset=SetDivonlyAdd(INT(divonly_save)) 
divvalue=SetDivergenceAdd(divergence_save) 
If (.not.CalculateOneLoopMasses) Then 
 If (OSkinematics) Then 
  Write(*,*) "Loop masses not calculated: tree-level masses used for kinematics" 
  OSkinematics = .false. 
 End if
 If (ExternalZfactors) Then 
  Write(*,*) "Loop masses not calculated: no U-factors are applied" 
  ExternalZfactors = .false. 
 End if
End if

If (Extra_scale_loopDecays) Then 
q2_save = GetRenormalizationScale() 
q2 = SetRenormalizationScale(scale_loopdecays **2) 
End if 
If ((OSkinematics).or.(ExternalZfactors)) ShiftIRdiv = .true. 
If (ewOSinDecays) Then 
sinW2=1._dp-mW2/mZ2 
g1SM=sqrt(4*Pi*Alpha_MZ/(1-sinW2)) 
g2SM=sqrt(4*Pi*Alpha_MZ/Sinw2) 
vSM=sqrt(mz2*4._dp/(g1SM**2+g2SM**2)) 
vvSM=vSM 
g1=g1SM 
g2=g2SM 
 If (yukOSinDecays) Then !! Allow OS Yukawas only together with vSM 
    YuSM = 0._dp 
    YdSM = 0._dp 
    YuSM = 0._dp 
   Do i1=1,3 
      YuSM(i1,i1)=sqrt(2._dp)*mf_u(i1)/vSM 
      YeSM(i1,i1)=sqrt(2._dp)*mf_l(i1)/vSM 
      YdSM(i1,i1)=sqrt(2._dp)*mf_d(i1)/vSM 
    End Do 
   If(GenerationMixing) Then 
    YuSM = Transpose(Matmul(Transpose(CKM),Transpose(YuSM))) 
   End if 
Ye=YeSM 
Yd=YdSM 
Yu=YuSM 
 End if 
End if 
! -------------------------------------------- 
! General information needed in the following 
! -------------------------------------------- 

! DR parameters 
Call SolveTadpoleEquations(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,               & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,(/ ZeroC, ZeroC /))

Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,              & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,            & 
& ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& GenerationMixing,kont)

! Stabilize numerics 
If (Abs(MHp).lt.1.0E-15_dp) MHp=0._dp
If (Abs(MHp2).lt.1.0E-30_dp) MHp2=0._dp
If (Abs(MAh).lt.1.0E-15_dp) MAh=0._dp
If (Abs(MAh2).lt.1.0E-30_dp) MAh2=0._dp
Where (Abs(Mhh).lt.1.0E-15_dp) Mhh=0._dp
Where (Abs(Mhh2).lt.1.0E-30_dp) Mhh2=0._dp
Where (Abs(MSsc).lt.1.0E-15_dp) MSsc=0._dp
Where (Abs(MSsc2).lt.1.0E-30_dp) MSsc2=0._dp
If (Abs(MFxe).lt.1.0E-15_dp) MFxe=0._dp
If (Abs(MFxe2).lt.1.0E-30_dp) MFxe2=0._dp
Where (Abs(MFd).lt.1.0E-15_dp) MFd=0._dp
Where (Abs(MFd2).lt.1.0E-30_dp) MFd2=0._dp
Where (Abs(MFu).lt.1.0E-15_dp) MFu=0._dp
Where (Abs(MFu2).lt.1.0E-30_dp) MFu2=0._dp
Where (Abs(MFe).lt.1.0E-15_dp) MFe=0._dp
Where (Abs(MFe2).lt.1.0E-30_dp) MFe2=0._dp
Where (Abs(MFv).lt.1.0E-15_dp) MFv=0._dp
Where (Abs(MFv2).lt.1.0E-30_dp) MFv2=0._dp
Where (Abs(MFxv).lt.1.0E-15_dp) MFxv=0._dp
Where (Abs(MFxv2).lt.1.0E-30_dp) MFxv2=0._dp
If (UseZeroRotationMatrices) Then  ! Rotation matrices calculated for p2=0
ZRUZH = MatMul(ZHOS_0, Transpose(ZH))
ZRUVd = MatMul(ZDLOS_0, Conjg(Transpose(ZDL)))
ZRUUd = MatMul(ZDROS_0, Conjg(Transpose(ZDR)))
ZRUVu = MatMul(ZULOS_0, Conjg(Transpose(ZUL)))
ZRUUu = MatMul(ZUROS_0, Conjg(Transpose(ZUR)))
ZRUVe = MatMul(ZELOS_0, Conjg(Transpose(ZEL)))
ZRUUe = MatMul(ZEROS_0, Conjg(Transpose(ZER)))
ZRUVv = MatMul(UVOS_0, Conjg(Transpose(UV)))
ZRUVvr = MatMul(UVROS_0, Conjg(Transpose(UVR)))
ZRUXV = MatMul(XVOS_0, Conjg(Transpose(XV)))
ZRUXU = MatMul(XUOS_0, Conjg(Transpose(XU)))
ZRUVSs = MatMul(VSsOS_0, Transpose(VSs))
Else If (UseP2Matrices) Then   ! p2 dependent matrix 
ZRUZH = MatMul(ZHOS_p2, Transpose(ZH))
ZRUVd = MatMul(ZDLOS_p2, Conjg(Transpose(ZDL)))
ZRUUd = MatMul(ZDROS_p2, Conjg(Transpose(ZDR)))
ZRUVu = MatMul(ZULOS_p2, Conjg(Transpose(ZUL)))
ZRUUu = MatMul(ZUROS_p2, Conjg(Transpose(ZUR)))
ZRUVe = MatMul(ZELOS_p2, Conjg(Transpose(ZEL)))
ZRUUe = MatMul(ZEROS_p2, Conjg(Transpose(ZER)))
ZRUVv = MatMul(UVOS_p2, Conjg(Transpose(UV)))
ZRUVvr = MatMul(UVROS_p2, Conjg(Transpose(UVR)))
ZRUXV = MatMul(XVOS_p2, Conjg(Transpose(XV)))
ZRUXU = MatMul(XUOS_p2, Conjg(Transpose(XU)))
ZRUVSs = MatMul(VSsOS_p2, Transpose(VSs))
Else  ! Rotation matrix for lightest state
ZRUZH = MatMul(ZHOS, Transpose(ZH))
ZRUVd = MatMul(ZDLOS, Conjg(Transpose(ZDL)))
ZRUUd = MatMul(ZDROS, Conjg(Transpose(ZDR)))
ZRUVu = MatMul(ZULOS, Conjg(Transpose(ZUL)))
ZRUUu = MatMul(ZUROS, Conjg(Transpose(ZUR)))
ZRUVe = MatMul(ZELOS, Conjg(Transpose(ZEL)))
ZRUUe = MatMul(ZEROS, Conjg(Transpose(ZER)))
ZRUVv = MatMul(UVOS, Conjg(Transpose(UV)))
ZRUVvr = MatMul(UVROS, Conjg(Transpose(UVR)))
ZRUXV = MatMul(XVOS, Conjg(Transpose(XV)))
ZRUXU = MatMul(XUOS, Conjg(Transpose(XU)))
ZRUVSs = MatMul(VSsOS, Transpose(VSs))
End if 
! Couplings 
Call AllCouplingsReallyAll(Lam,LSPH,vvSM,vS,ZH,LSP,LS1H,LS2H,VSs,LS,g1,               & 
& g2,TW,g3,Yd,ZDL,ZDR,Ye,ZEL,ZER,Yu,ZUL,ZUR,YRD,XV,XU,UV,YRA1,YRA2,YRB1,YRB2,            & 
& UVR,YRC,cplAhAhhh,cplhhhhhh,cplhhHpcHp,cplhhSsccSsc,cplAhAhAhAh,cplAhAhhhhh,           & 
& cplAhAhHpcHp,cplAhAhSsccSsc,cplhhhhhhhh,cplhhhhHpcHp,cplhhhhSsccSsc,cplHpHpcHpcHp,     & 
& cplHpSsccHpcSsc,cplSscSsccSsccSsc,cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,cplhhHpcVWp,       & 
& cplhhcHpVWp,cplHpcHpVP,cplHpcHpVZ,cplhhcVWpVWp,cplhhVZVZ,cplHpcVWpVP,cplHpcVWpVZ,      & 
& cplcHpVPVWp,cplcHpVWpVZ,cplAhAhcVWpVWp,cplAhAhVZVZ,cplAhHpcVWpVP,cplAhHpcVWpVZ,        & 
& cplAhcHpVPVWp,cplAhcHpVWpVZ,cplhhhhcVWpVWp,cplhhhhVZVZ,cplhhHpcVWpVP,cplhhHpcVWpVZ,    & 
& cplhhcHpVPVWp,cplhhcHpVWpVZ,cplHpcHpVPVP,cplHpcHpVPVZ,cplHpcHpcVWpVWp,cplHpcHpVZVZ,    & 
& cplVGVGVG,cplcVWpVPVWp,cplcVWpVWpVZ,cplcFdFdAhL,cplcFdFdAhR,cplcFeFeAhL,               & 
& cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFdFdhhL,           & 
& cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,               & 
& cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucHpL,          & 
& cplcFdFucHpR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFxvFxeHpL,      & 
& cplcFxvFxeHpR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,               & 
& cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFdFdVGL,               & 
& cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,             & 
& cplcFdFdVZR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,             & 
& cplcFeFeVZR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,               & 
& cplcFuFuVZR,cplcFdFucVWpL,cplcFdFucVWpR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcVWpL,         & 
& cplcFeFvcVWpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,               & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFxeFxvcVWpL,               & 
& cplcFxeFxvcVWpR,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,& 
& cplcVWpVPVPVWp3,cplcVWpVPVWpVZ1,cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpcVWpVWpVWp1,    & 
& cplcVWpcVWpVWpVWp2,cplcVWpcVWpVWpVWp3,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3, & 
& cplcgGgGVG,cplcgWpgAVWp,cplcgWCgAcVWp,cplcgWpgWpVP,cplcgWpgWpVZ,cplcgAgWpcVWp,         & 
& cplcgZgWpcVWp,cplcgWCgWCVP,cplcgAgWCVWp,cplcgZgWCVWp,cplcgWCgWCVZ,cplcgWpgZVWp,        & 
& cplcgWCgZcVWp,cplcgWpgWpAh,cplcgWCgWCAh,cplcgZgAhh,cplcgWpgAHp,cplcgWCgAcHp,           & 
& cplcgWpgWphh,cplcgZgWpcHp,cplcgWCgWChh,cplcgZgWCHp,cplcgZgZhh,cplcgWpgZHp,             & 
& cplcgWCgZcHp)

em = cplcVWpVPVWp 
gs = cplcFdFdVGL(1,1) 
Call CouplingsColoredQuartics(Lam,LSPH,ZH,LS1H,LS2H,VSs,LSP,LS,g2,g1,TW,              & 
& g3,cplAhAhAhAh1,cplAhAhhhhh1,cplAhAhHpcHp1,cplAhAhSsccSsc1,cplhhhhhhhh1,               & 
& cplhhhhHpcHp1,cplhhhhSsccSsc1,cplHpHpcHpcHp1,cplHpSsccHpcSsc1,cplSscSsccSsccSsc1,      & 
& cplAhAhcVWpVWp1,cplAhAhVZVZ1,cplAhHpcVWpVP1,cplAhHpcVWpVZ1,cplAhcHpVPVWp1,             & 
& cplAhcHpVWpVZ1,cplhhhhcVWpVWp1,cplhhhhVZVZ1,cplhhHpcVWpVP1,cplhhHpcVWpVZ1,             & 
& cplhhcHpVPVWp1,cplhhcHpVWpVZ1,cplHpcHpVPVP1,cplHpcHpVPVZ1,cplHpcHpcVWpVWp1,            & 
& cplHpcHpVZVZ1,cplVGVGVGVG1Q,cplVGVGVGVG2Q,cplVGVGVGVG3Q,cplcVWpVPVPVWp1Q,              & 
& cplcVWpVPVPVWp2Q,cplcVWpVPVPVWp3Q,cplcVWpVPVWpVZ1Q,cplcVWpVPVWpVZ2Q,cplcVWpVPVWpVZ3Q,  & 
& cplcVWpcVWpVWpVWp1Q,cplcVWpcVWpVWpVWp2Q,cplcVWpcVWpVWpVWp3Q,cplcVWpVWpVZVZ1Q,          & 
& cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q)

If (externalZfactors) Then 
Call getZCouplings(Lam,LSPH,vvSM,vS,ZH,LSP,LS1H,LS2H,VSs,LS,g1,g2,TW,g3,              & 
& Yd,ZDL,ZDR,Ye,ZEL,ZER,Yu,ZUL,ZUR,YRD,XV,XU,UV,YRA1,YRA2,YRB1,YRB2,UVR,YRC,             & 
& cplAhAhhh,cplhhhhhh,cplhhHpcHp,cplhhSsccSsc,cplAhAhAhAh,cplAhAhhhhh,cplAhAhHpcHp,      & 
& cplAhAhSsccSsc,cplhhhhhhhh,cplhhhhHpcHp,cplhhhhSsccSsc,cplHpHpcHpcHp,cplHpSsccHpcSsc,  & 
& cplSscSsccSsccSsc,cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,cplhhHpcVWp,cplhhcHpVWp,           & 
& cplHpcHpVP,cplHpcHpVZ,cplhhcVWpVWp,cplhhVZVZ,cplHpcVWpVP,cplHpcVWpVZ,cplcHpVPVWp,      & 
& cplcHpVWpVZ,cplAhAhcVWpVWp,cplAhAhVZVZ,cplAhHpcVWpVP,cplAhHpcVWpVZ,cplAhcHpVPVWp,      & 
& cplAhcHpVWpVZ,cplhhhhcVWpVWp,cplhhhhVZVZ,cplhhHpcVWpVP,cplhhHpcVWpVZ,cplhhcHpVPVWp,    & 
& cplhhcHpVWpVZ,cplHpcHpVPVP,cplHpcHpVPVZ,cplHpcHpcVWpVWp,cplHpcHpVZVZ,cplVGVGVG,        & 
& cplcVWpVPVWp,cplcVWpVWpVZ,cplcFdFdAhL,cplcFdFdAhR,cplcFeFeAhL,cplcFeFeAhR,             & 
& cplcFuFuAhL,cplcFuFuAhR,cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFdFdhhL,cplcFdFdhhR,           & 
& cplcFuFdHpL,cplcFuFdHpR,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,               & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucHpL,cplcFdFucHpR,         & 
& cplcFxvFvSscL,cplcFxvFvSscR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFxvFxeHpL,cplcFxvFxeHpR,     & 
& cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFvFxvcSscL,              & 
& cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,      & 
& cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVPL,             & 
& cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVGL,             & 
& cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucVWpL,             & 
& cplcFdFucVWpR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxeVPL,       & 
& cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,               & 
& cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplVGVGVGVG1,              & 
& cplVGVGVGVG2,cplVGVGVGVG3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,cplcVWpVPVPVWp3,             & 
& cplcVWpVPVWpVZ1,cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpcVWpVWpVWp1,cplcVWpcVWpVWpVWp2, & 
& cplcVWpcVWpVWpVWp3,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,cplcgGgGVG,         & 
& cplcgWpgAVWp,cplcgWCgAcVWp,cplcgWpgWpVP,cplcgWpgWpVZ,cplcgAgWpcVWp,cplcgZgWpcVWp,      & 
& cplcgWCgWCVP,cplcgAgWCVWp,cplcgZgWCVWp,cplcgWCgWCVZ,cplcgWpgZVWp,cplcgWCgZcVWp,        & 
& cplcgWpgWpAh,cplcgWCgWCAh,cplcgZgAhh,cplcgWpgAHp,cplcgWCgAcHp,cplcgWpgWphh,            & 
& cplcgZgWpcHp,cplcgWCgWChh,cplcgZgWCHp,cplcgZgZhh,cplcgWpgZHp,cplcgWCgZcHp,             & 
& ZRUZH,ZRUVd,ZRUUd,ZRUVu,ZRUUu,ZRUVe,ZRUUe,ZRUVv,ZRUVvr,ZRUXV,ZRUXU,ZRUVSs,             & 
& ZcplAhAhhh,Zcplhhhhhh,ZcplhhHpcHp,ZcplhhSsccSsc,ZcplAhAhAhAh,ZcplAhAhhhhh,             & 
& ZcplAhAhHpcHp,ZcplAhAhSsccSsc,Zcplhhhhhhhh,ZcplhhhhHpcHp,ZcplhhhhSsccSsc,              & 
& ZcplHpHpcHpcHp,ZcplHpSsccHpcSsc,ZcplSscSsccSsccSsc,ZcplAhhhVZ,ZcplAhHpcVWp,            & 
& ZcplAhcHpVWp,ZcplhhHpcVWp,ZcplhhcHpVWp,ZcplHpcHpVP,ZcplHpcHpVZ,ZcplhhcVWpVWp,          & 
& ZcplhhVZVZ,ZcplHpcVWpVP,ZcplHpcVWpVZ,ZcplcHpVPVWp,ZcplcHpVWpVZ,ZcplAhAhcVWpVWp,        & 
& ZcplAhAhVZVZ,ZcplAhHpcVWpVP,ZcplAhHpcVWpVZ,ZcplAhcHpVPVWp,ZcplAhcHpVWpVZ,              & 
& ZcplhhhhcVWpVWp,ZcplhhhhVZVZ,ZcplhhHpcVWpVP,ZcplhhHpcVWpVZ,ZcplhhcHpVPVWp,             & 
& ZcplhhcHpVWpVZ,ZcplHpcHpVPVP,ZcplHpcHpVPVZ,ZcplHpcHpcVWpVWp,ZcplHpcHpVZVZ,             & 
& ZcplVGVGVG,ZcplcVWpVPVWp,ZcplcVWpVWpVZ,ZcplcFdFdAhL,ZcplcFdFdAhR,ZcplcFeFeAhL,         & 
& ZcplcFeFeAhR,ZcplcFuFuAhL,ZcplcFuFuAhR,ZcplcFxvFxvAhL,ZcplcFxvFxvAhR,ZcplcFdFdhhL,     & 
& ZcplcFdFdhhR,ZcplcFuFdHpL,ZcplcFuFdHpR,ZcplcFeFehhL,ZcplcFeFehhR,ZcplcFvFeHpL,         & 
& ZcplcFvFeHpR,ZcplcFxeFeSscL,ZcplcFxeFeSscR,ZcplcFuFuhhL,ZcplcFuFuhhR,ZcplcFdFucHpL,    & 
& ZcplcFdFucHpR,ZcplcFxvFvSscL,ZcplcFxvFvSscR,ZcplcFeFvcHpL,ZcplcFeFvcHpR,               & 
& ZcplcFxvFxeHpL,ZcplcFxvFxeHpR,ZcplcFeFxecSscL,ZcplcFeFxecSscR,ZcplcFxvFxvhhL,          & 
& ZcplcFxvFxvhhR,ZcplcFvFxvcSscL,ZcplcFvFxvcSscR,ZcplcFxeFxvcHpL,ZcplcFxeFxvcHpR,        & 
& ZcplcFdFdVGL,ZcplcFdFdVGR,ZcplcFdFdVPL,ZcplcFdFdVPR,ZcplcFuFdVWpL,ZcplcFuFdVWpR,       & 
& ZcplcFdFdVZL,ZcplcFdFdVZR,ZcplcFeFeVPL,ZcplcFeFeVPR,ZcplcFvFeVWpL,ZcplcFvFeVWpR,       & 
& ZcplcFeFeVZL,ZcplcFeFeVZR,ZcplcFuFuVGL,ZcplcFuFuVGR,ZcplcFuFuVPL,ZcplcFuFuVPR,         & 
& ZcplcFuFuVZL,ZcplcFuFuVZR,ZcplcFdFucVWpL,ZcplcFdFucVWpR,ZcplcFvFvVZL,ZcplcFvFvVZR,     & 
& ZcplcFeFvcVWpL,ZcplcFeFvcVWpR,ZcplcFxeFxeVPL,ZcplcFxeFxeVPR,ZcplcFxvFxeVWpL,           & 
& ZcplcFxvFxeVWpR,ZcplcFxeFxeVZL,ZcplcFxeFxeVZR,ZcplcFxvFxvVZL,ZcplcFxvFxvVZR,           & 
& ZcplcFxeFxvcVWpL,ZcplcFxeFxvcVWpR,ZcplVGVGVGVG1,ZcplVGVGVGVG2,ZcplVGVGVGVG3,           & 
& ZcplcVWpVPVPVWp1,ZcplcVWpVPVPVWp2,ZcplcVWpVPVPVWp3,ZcplcVWpVPVWpVZ1,ZcplcVWpVPVWpVZ2,  & 
& ZcplcVWpVPVWpVZ3,ZcplcVWpcVWpVWpVWp1,ZcplcVWpcVWpVWpVWp2,ZcplcVWpcVWpVWpVWp3,          & 
& ZcplcVWpVWpVZVZ1,ZcplcVWpVWpVZVZ2,ZcplcVWpVWpVZVZ3,ZcplcgGgGVG,ZcplcgWpgAVWp,          & 
& ZcplcgWCgAcVWp,ZcplcgWpgWpVP,ZcplcgWpgWpVZ,ZcplcgAgWpcVWp,ZcplcgZgWpcVWp,              & 
& ZcplcgWCgWCVP,ZcplcgAgWCVWp,ZcplcgZgWCVWp,ZcplcgWCgWCVZ,ZcplcgWpgZVWp,ZcplcgWCgZcVWp,  & 
& ZcplcgWpgWpAh,ZcplcgWCgWCAh,ZcplcgZgAhh,ZcplcgWpgAHp,ZcplcgWCgAcHp,ZcplcgWpgWphh,      & 
& ZcplcgZgWpcHp,ZcplcgWCgWChh,ZcplcgZgWCHp,ZcplcgZgZhh,ZcplcgWpgZHp,ZcplcgWCgZcHp)

End if 
Call getGBCouplings(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,MFeOS,MFe2OS,              & 
& MFvOS,MFv2OS,MFxvOS,MFxv2OS,MSscOS,MSsc2OS,MFxeOS,MFxe2OS,MHpOS,MHp2OS,MAhOS,          & 
& MAh2OS,MVZOS,MVZ2OS,MVWpOS,MVWp2OS,ZHOS,ZDLOS,ZDROS,ZULOS,ZUROS,ZELOS,ZEROS,           & 
& UVOS,UVROS,XVOS,XUOS,VSsOS,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,               & 
& MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,             & 
& ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,cplcFdFucVWpL,cplcFdFucVWpR,  & 
& cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFvFeVWpL,cplcFvFeVWpR,       & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcHpVPVWp,             & 
& cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,ZcplcFdFucVWpL,         & 
& ZcplcFdFucVWpR,ZcplcFeFvcVWpL,ZcplcFeFvcVWpR,ZcplcFuFdVWpL,ZcplcFuFdVWpR,              & 
& ZcplcFvFeVWpL,ZcplcFvFeVWpR,ZcplcFxeFxvcVWpL,ZcplcFxeFxvcVWpR,ZcplcFxvFxeVWpL,         & 
& ZcplcFxvFxeVWpR,ZcplcHpVPVWp,ZcplcHpVWpVZ,ZcplcVWpVPVWp,ZcplcVWpVWpVZ,ZcplhhcHpVWp,    & 
& ZcplhhcVWpVWp,GcplhhHpcHp,GcplAhHpcVWp,GcplAhcHpVWp,GcplhhHpcVWp,GcplhhcHpVWp,         & 
& GcplHpcHpVP,GcplHpcHpVZ,GcplHpcVWpVP,GcplHpcVWpVZ,GcplcHpVPVWp,GcplcHpVWpVZ,           & 
& GcplcFuFdHpL,GcplcFuFdHpR,GcplcFvFeHpL,GcplcFvFeHpR,GcplcFdFucHpL,GcplcFdFucHpR,       & 
& GcplcFeFvcHpL,GcplcFeFvcHpR,GcplcFxvFxeHpL,GcplcFxvFxeHpR,GcplcFxeFxvcHpL,             & 
& GcplcFxeFxvcHpR,GZcplhhHpcHp,GZcplAhHpcVWp,GZcplAhcHpVWp,GZcplhhHpcVWp,GZcplhhcHpVWp,  & 
& GZcplHpcHpVP,GZcplHpcHpVZ,GZcplHpcVWpVP,GZcplHpcVWpVZ,GZcplcHpVPVWp,GZcplcHpVWpVZ,     & 
& GZcplcFuFdHpL,GZcplcFuFdHpR,GZcplcFvFeHpL,GZcplcFvFeHpR,GZcplcFdFucHpL,GZcplcFdFucHpR, & 
& GZcplcFeFvcHpL,GZcplcFeFvcHpR,GZcplcFxvFxeHpL,GZcplcFxvFxeHpR,GZcplcFxeFxvcHpL,        & 
& GZcplcFxeFxvcHpR,GosZcplhhHpcHp,GosZcplAhHpcVWp,GosZcplAhcHpVWp,GosZcplhhHpcVWp,       & 
& GosZcplhhcHpVWp,GosZcplHpcHpVP,GosZcplHpcHpVZ,GosZcplHpcVWpVP,GosZcplHpcVWpVZ,         & 
& GosZcplcHpVPVWp,GosZcplcHpVWpVZ,GosZcplcFuFdHpL,GosZcplcFuFdHpR,GosZcplcFvFeHpL,       & 
& GosZcplcFvFeHpR,GosZcplcFdFucHpL,GosZcplcFdFucHpR,GosZcplcFeFvcHpL,GosZcplcFeFvcHpR,   & 
& GosZcplcFxvFxeHpL,GosZcplcFxvFxeHpR,GosZcplcFxeFxvcHpL,GosZcplcFxeFxvcHpR)

! Write intilization of all counter terms 
Call WaveFunctionRenormalisation(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,              & 
& MFeOS,MFe2OS,MFvOS,MFv2OS,MFxvOS,MFxv2OS,MSscOS,MSsc2OS,MFxeOS,MFxe2OS,MHpOS,          & 
& MHp2OS,MAhOS,MAh2OS,MVZOS,MVZ2OS,MVWpOS,MVWp2OS,ZHOS,ZDLOS,ZDROS,ZULOS,ZUROS,          & 
& ZELOS,ZEROS,UVOS,UVROS,XVOS,XUOS,VSsOS,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,            & 
& MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,            & 
& MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,             & 
& Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,               & 
& MS12,MS22,mP2,vvSM,vS,cplAhAhhh,cplhhhhhh,cplhhHpcHp,cplhhSsccSsc,cplAhAhAhAh,         & 
& cplAhAhhhhh,cplAhAhHpcHp,cplAhAhSsccSsc,cplhhhhhhhh,cplhhhhHpcHp,cplhhhhSsccSsc,       & 
& cplHpHpcHpcHp,cplHpSsccHpcSsc,cplSscSsccSsccSsc,cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,     & 
& cplhhHpcVWp,cplhhcHpVWp,cplHpcHpVP,cplHpcHpVZ,cplhhcVWpVWp,cplhhVZVZ,cplHpcVWpVP,      & 
& cplHpcVWpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplAhAhcVWpVWp,cplAhAhVZVZ,cplAhHpcVWpVP,          & 
& cplAhHpcVWpVZ,cplAhcHpVPVWp,cplAhcHpVWpVZ,cplhhhhcVWpVWp,cplhhhhVZVZ,cplhhHpcVWpVP,    & 
& cplhhHpcVWpVZ,cplhhcHpVPVWp,cplhhcHpVWpVZ,cplHpcHpVPVP,cplHpcHpVPVZ,cplHpcHpcVWpVWp,   & 
& cplHpcHpVZVZ,cplVGVGVG,cplcVWpVPVWp,cplcVWpVWpVZ,cplcFdFdAhL,cplcFdFdAhR,              & 
& cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplcFxvFxvAhL,cplcFxvFxvAhR,           & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFeFehhL,cplcFeFehhR,               & 
& cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFuFuhhL,cplcFuFuhhR,           & 
& cplcFdFucHpL,cplcFdFucHpR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFeFvcHpL,cplcFeFvcHpR,       & 
& cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,               & 
& cplcFxvFxvhhR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,             & 
& cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,             & 
& cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,             & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,               & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFdFucVWpL,cplcFdFucVWpR,cplcFvFvVZL,cplcFvFvVZR,           & 
& cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR, & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFxeFxvcVWpL,               & 
& cplcFxeFxvcVWpR,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,& 
& cplcVWpVPVPVWp3,cplcVWpVPVWpVZ1,cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpcVWpVWpVWp1,    & 
& cplcVWpcVWpVWpVWp2,cplcVWpcVWpVWpVWp3,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3, & 
& cplcgGgGVG,cplcgWpgAVWp,cplcgWCgAcVWp,cplcgWpgWpVP,cplcgWpgWpVZ,cplcgAgWpcVWp,         & 
& cplcgZgWpcVWp,cplcgWCgWCVP,cplcgAgWCVWp,cplcgZgWCVWp,cplcgWCgWCVZ,cplcgWpgZVWp,        & 
& cplcgWCgZcVWp,cplcgWpgWpAh,cplcgWCgWCAh,cplcgZgAhh,cplcgWpgAHp,cplcgWCgAcHp,           & 
& cplcgWpgWphh,cplcgZgWpcHp,cplcgWCgWChh,cplcgZgWCHp,cplcgZgZhh,cplcgWpgZHp,             & 
& cplcgWCgZcHp,GcplhhHpcHp,GcplAhHpcVWp,GcplAhcHpVWp,GcplhhHpcVWp,GcplhhcHpVWp,          & 
& GcplHpcHpVP,GcplHpcHpVZ,GcplHpcVWpVP,GcplHpcVWpVZ,GcplcHpVPVWp,GcplcHpVWpVZ,           & 
& GcplcFuFdHpL,GcplcFuFdHpR,GcplcFvFeHpL,GcplcFvFeHpR,GcplcFdFucHpL,GcplcFdFucHpR,       & 
& GcplcFeFvcHpL,GcplcFeFvcHpR,GcplcFxvFxeHpL,GcplcFxvFxeHpR,GcplcFxeFxvcHpL,             & 
& GcplcFxeFxvcHpR,dg1,dg2,dg3,dYu,dYd,dYe,dm2SM,dLam,dMDF,dYRD,dMS12,dMS22,              & 
& dLS1H,dLS,dLS2H,dmP2,dLSP,dLSPH,dYRB1,dYRB2,dYRC,dYRA1,dYRA2,dvvSM,dvS,dZH,            & 
& dZDL,dZDR,dZUL,dZUR,dZEL,dZER,dUV,dUVR,dXV,dXU,dVSs,dSinTW,dCosTW,dTanTW,              & 
& ZfVG,ZfHp,Zfed,Zfeu,ZfAh,ZfVP,ZfVZ,ZfVWp,Zfhh,ZfDL,ZfDR,ZfUL,ZfUR,ZfEL,ZfER,           & 
& ZfVL,ZfVR,ZfxVL,ZfxVR,ZfSsc,ZfVPVZ,ZfVZVP,ctcplAhAhhh,ctcplhhhhhh,ctcplhhHpcHp,        & 
& ctcplhhSsccSsc,ctcplAhhhVZ,ctcplAhHpcVWp,ctcplAhcHpVWp,ctcplhhHpcVWp,ctcplhhcHpVWp,    & 
& ctcplHpcHpVP,ctcplHpcHpVZ,ctcplhhcVWpVWp,ctcplhhVZVZ,ctcplHpcVWpVP,ctcplHpcVWpVZ,      & 
& ctcplcHpVPVWp,ctcplcHpVWpVZ,ctcplVGVGVG,ctcplcVWpVPVWp,ctcplcVWpVWpVZ,ctcplcFdFdAhL,   & 
& ctcplcFdFdAhR,ctcplcFeFeAhL,ctcplcFeFeAhR,ctcplcFuFuAhL,ctcplcFuFuAhR,ctcplcFxvFxvAhL, & 
& ctcplcFxvFxvAhR,ctcplcFdFdhhL,ctcplcFdFdhhR,ctcplcFuFdHpL,ctcplcFuFdHpR,               & 
& ctcplcFeFehhL,ctcplcFeFehhR,ctcplcFvFeHpL,ctcplcFvFeHpR,ctcplcFxeFeSscL,               & 
& ctcplcFxeFeSscR,ctcplcFuFuhhL,ctcplcFuFuhhR,ctcplcFdFucHpL,ctcplcFdFucHpR,             & 
& ctcplcFxvFvSscL,ctcplcFxvFvSscR,ctcplcFeFvcHpL,ctcplcFeFvcHpR,ctcplcFxvFxeHpL,         & 
& ctcplcFxvFxeHpR,ctcplcFeFxecSscL,ctcplcFeFxecSscR,ctcplcFxvFxvhhL,ctcplcFxvFxvhhR,     & 
& ctcplcFvFxvcSscL,ctcplcFvFxvcSscR,ctcplcFxeFxvcHpL,ctcplcFxeFxvcHpR,ctcplcFdFdVGL,     & 
& ctcplcFdFdVGR,ctcplcFdFdVPL,ctcplcFdFdVPR,ctcplcFuFdVWpL,ctcplcFuFdVWpR,               & 
& ctcplcFdFdVZL,ctcplcFdFdVZR,ctcplcFeFeVPL,ctcplcFeFeVPR,ctcplcFvFeVWpL,ctcplcFvFeVWpR, & 
& ctcplcFeFeVZL,ctcplcFeFeVZR,ctcplcFuFuVGL,ctcplcFuFuVGR,ctcplcFuFuVPL,ctcplcFuFuVPR,   & 
& ctcplcFuFuVZL,ctcplcFuFuVZR,ctcplcFdFucVWpL,ctcplcFdFucVWpR,ctcplcFvFvVZL,             & 
& ctcplcFvFvVZR,ctcplcFeFvcVWpL,ctcplcFeFvcVWpR,ctcplcFxeFxeVPL,ctcplcFxeFxeVPR,         & 
& ctcplcFxvFxeVWpL,ctcplcFxvFxeVWpR,ctcplcFxeFxeVZL,ctcplcFxeFxeVZR,ctcplcFxvFxvVZL,     & 
& ctcplcFxvFxvVZR,ctcplcFxeFxvcVWpL,ctcplcFxeFxvcVWpR,MLambda,deltaM,kont)

! -------------------------------------------- 
! The decays at one-loop 
! -------------------------------------------- 

! Fu
If (CalcLoopDecay_Fu) Then 
Call OneLoopDecay_Fu(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,MFeOS,MFe2OS,             & 
& MFvOS,MFv2OS,MFxvOS,MFxv2OS,MSscOS,MSsc2OS,MFxeOS,MFxe2OS,MHpOS,MHp2OS,MAhOS,          & 
& MAh2OS,MVZOS,MVZ2OS,MVWpOS,MVWp2OS,ZHOS,ZDLOS,ZDROS,ZULOS,ZUROS,ZELOS,ZEROS,           & 
& UVOS,UVROS,XVOS,XUOS,VSsOS,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,               & 
& MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,             & 
& ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,            & 
& LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,              & 
& mP2,vvSM,vS,dg1,dg2,dg3,dYu,dYd,dYe,dm2SM,dLam,dMDF,dYRD,dMS12,dMS22,dLS1H,            & 
& dLS,dLS2H,dmP2,dLSP,dLSPH,dYRB1,dYRB2,dYRC,dYRA1,dYRA2,dvvSM,dvS,dZH,dZDL,             & 
& dZDR,dZUL,dZUR,dZEL,dZER,dUV,dUVR,dXV,dXU,dVSs,dSinTW,dCosTW,dTanTW,ZfVG,              & 
& ZfHp,Zfed,Zfeu,ZfAh,ZfVP,ZfVZ,ZfVWp,Zfhh,ZfDL,ZfDR,ZfUL,ZfUR,ZfEL,ZfER,ZfVL,           & 
& ZfVR,ZfxVL,ZfxVR,ZfSsc,ZfVPVZ,ZfVZVP,cplAhAhhh,cplAhcHpVWp,cplAhhhVZ,cplAhHpcVWp,      & 
& cplcFdFdAhL,cplcFdFdAhR,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVGL,cplcFdFdVGR,               & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFdFucHpL,cplcFdFucHpR,             & 
& cplcFdFucVWpL,cplcFdFucVWpR,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,cplcFuFdVWpR,         & 
& cplcFuFuAhL,cplcFuFuAhR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,               & 
& cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcHpVPVWp,cplcHpVWpVZ,               & 
& cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,cplhhhhhh,cplhhHpcHp,               & 
& cplhhHpcVWp,cplhhVZVZ,cplHpcHpVP,cplHpcHpVZ,cplHpcVWpVP,cplHpcVWpVZ,cplVGVGVG,         & 
& ctcplcFuFdHpL,ctcplcFuFdHpR,ctcplcFuFdVWpL,ctcplcFuFdVWpR,ctcplcFuFuAhL,               & 
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

End if 
! Fe
If (CalcLoopDecay_Fe) Then 
Call OneLoopDecay_Fe(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,MFeOS,MFe2OS,             & 
& MFvOS,MFv2OS,MFxvOS,MFxv2OS,MSscOS,MSsc2OS,MFxeOS,MFxe2OS,MHpOS,MHp2OS,MAhOS,          & 
& MAh2OS,MVZOS,MVZ2OS,MVWpOS,MVWp2OS,ZHOS,ZDLOS,ZDROS,ZULOS,ZUROS,ZELOS,ZEROS,           & 
& UVOS,UVROS,XVOS,XUOS,VSsOS,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,               & 
& MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,             & 
& ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,            & 
& LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,              & 
& mP2,vvSM,vS,dg1,dg2,dg3,dYu,dYd,dYe,dm2SM,dLam,dMDF,dYRD,dMS12,dMS22,dLS1H,            & 
& dLS,dLS2H,dmP2,dLSP,dLSPH,dYRB1,dYRB2,dYRC,dYRA1,dYRA2,dvvSM,dvS,dZH,dZDL,             & 
& dZDR,dZUL,dZUR,dZEL,dZER,dUV,dUVR,dXV,dXU,dVSs,dSinTW,dCosTW,dTanTW,ZfVG,              & 
& ZfHp,Zfed,Zfeu,ZfAh,ZfVP,ZfVZ,ZfVWp,Zfhh,ZfDL,ZfDR,ZfUL,ZfUR,ZfEL,ZfER,ZfVL,           & 
& ZfVR,ZfxVL,ZfxVR,ZfSsc,ZfVPVZ,ZfVZVP,cplAhAhhh,cplAhcHpVWp,cplAhhhVZ,cplAhHpcVWp,      & 
& cplcFeFeAhL,cplcFeFeAhR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,         & 
& cplcFeFxecSscL,cplcFeFxecSscR,cplcFvFeHpL,cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,       & 
& cplcFvFvVZL,cplcFvFvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFeSscL,cplcFxeFeSscR,     & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR, & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,             & 
& cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,      & 
& cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,   & 
& cplhhVZVZ,cplHpcHpVP,cplHpcHpVZ,cplHpcVWpVP,cplHpcVWpVZ,ctcplcFeFeAhL,ctcplcFeFeAhR,   & 
& ctcplcFeFehhL,ctcplcFeFehhR,ctcplcFeFeVPL,ctcplcFeFeVPR,ctcplcFeFeVZL,ctcplcFeFeVZR,   & 
& ctcplcFeFvcHpL,ctcplcFeFvcHpR,ctcplcFeFvcVWpL,ctcplcFeFvcVWpR,ctcplcFeFxecSscL,        & 
& ctcplcFeFxecSscR,GcplcFeFvcHpL,GcplcFeFvcHpR,GcplcHpVPVWp,GcplHpcVWpVP,GosZcplcFeFvcHpL,& 
& GosZcplcFeFvcHpR,GosZcplcHpVPVWp,GosZcplHpcVWpVP,GZcplcFeFvcHpL,GZcplcFeFvcHpR,        & 
& GZcplcHpVPVWp,GZcplHpcVWpVP,ZcplcFeFeAhL,ZcplcFeFeAhR,ZcplcFeFehhL,ZcplcFeFehhR,       & 
& ZcplcFeFeVPL,ZcplcFeFeVPR,ZcplcFeFeVZL,ZcplcFeFeVZR,ZcplcFeFvcHpL,ZcplcFeFvcHpR,       & 
& ZcplcFeFvcVWpL,ZcplcFeFvcVWpR,ZcplcFeFxecSscL,ZcplcFeFxecSscR,ZcplcFvFeHpL,            & 
& ZcplcFvFeHpR,ZcplcFvFeVWpL,ZcplcFvFeVWpR,ZcplcFxeFeSscL,ZcplcFxeFeSscR,ZcplcFxeFxeVPL, & 
& ZcplcFxeFxeVPR,ZcplcHpVPVWp,ZcplcVWpVPVWp,ZcplHpcHpVP,ZcplHpcVWpVP,ZRUZH,              & 
& ZRUVd,ZRUUd,ZRUVu,ZRUUu,ZRUVe,ZRUUe,ZRUVv,ZRUVvr,ZRUXV,ZRUXU,ZRUVSs,MLambda,           & 
& em,gs,deltaM,kont,gP1LFe)

End if 
! Fd
If (CalcLoopDecay_Fd) Then 
Call OneLoopDecay_Fd(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,MFeOS,MFe2OS,             & 
& MFvOS,MFv2OS,MFxvOS,MFxv2OS,MSscOS,MSsc2OS,MFxeOS,MFxe2OS,MHpOS,MHp2OS,MAhOS,          & 
& MAh2OS,MVZOS,MVZ2OS,MVWpOS,MVWp2OS,ZHOS,ZDLOS,ZDROS,ZULOS,ZUROS,ZELOS,ZEROS,           & 
& UVOS,UVROS,XVOS,XUOS,VSsOS,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,               & 
& MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,             & 
& ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,            & 
& LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,              & 
& mP2,vvSM,vS,dg1,dg2,dg3,dYu,dYd,dYe,dm2SM,dLam,dMDF,dYRD,dMS12,dMS22,dLS1H,            & 
& dLS,dLS2H,dmP2,dLSP,dLSPH,dYRB1,dYRB2,dYRC,dYRA1,dYRA2,dvvSM,dvS,dZH,dZDL,             & 
& dZDR,dZUL,dZUR,dZEL,dZER,dUV,dUVR,dXV,dXU,dVSs,dSinTW,dCosTW,dTanTW,ZfVG,              & 
& ZfHp,Zfed,Zfeu,ZfAh,ZfVP,ZfVZ,ZfVWp,Zfhh,ZfDL,ZfDR,ZfUL,ZfUR,ZfEL,ZfER,ZfVL,           & 
& ZfVR,ZfxVL,ZfxVR,ZfSsc,ZfVPVZ,ZfVZVP,cplAhAhhh,cplAhcHpVWp,cplAhhhVZ,cplAhHpcVWp,      & 
& cplcFdFdAhL,cplcFdFdAhR,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVGL,cplcFdFdVGR,               & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFdFucHpL,cplcFdFucHpR,             & 
& cplcFdFucVWpL,cplcFdFucVWpR,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,cplcFuFdVWpR,         & 
& cplcFuFuAhL,cplcFuFuAhR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,               & 
& cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcHpVPVWp,cplcHpVWpVZ,               & 
& cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,cplhhhhhh,cplhhHpcHp,               & 
& cplhhHpcVWp,cplhhVZVZ,cplHpcHpVP,cplHpcHpVZ,cplHpcVWpVP,cplHpcVWpVZ,cplVGVGVG,         & 
& ctcplcFdFdAhL,ctcplcFdFdAhR,ctcplcFdFdhhL,ctcplcFdFdhhR,ctcplcFdFdVGL,ctcplcFdFdVGR,   & 
& ctcplcFdFdVPL,ctcplcFdFdVPR,ctcplcFdFdVZL,ctcplcFdFdVZR,ctcplcFdFucHpL,ctcplcFdFucHpR, & 
& ctcplcFdFucVWpL,ctcplcFdFucVWpR,GcplcFdFucHpL,GcplcFdFucHpR,GcplcHpVPVWp,              & 
& GcplHpcVWpVP,GosZcplcFdFucHpL,GosZcplcFdFucHpR,GosZcplcHpVPVWp,GosZcplHpcVWpVP,        & 
& GZcplcFdFucHpL,GZcplcFdFucHpR,GZcplcHpVPVWp,GZcplHpcVWpVP,ZcplcFdFdAhL,ZcplcFdFdAhR,   & 
& ZcplcFdFdhhL,ZcplcFdFdhhR,ZcplcFdFdVGL,ZcplcFdFdVGR,ZcplcFdFdVPL,ZcplcFdFdVPR,         & 
& ZcplcFdFdVZL,ZcplcFdFdVZR,ZcplcFdFucHpL,ZcplcFdFucHpR,ZcplcFdFucVWpL,ZcplcFdFucVWpR,   & 
& ZcplcFuFdHpL,ZcplcFuFdHpR,ZcplcFuFdVWpL,ZcplcFuFdVWpR,ZcplcFuFuVGL,ZcplcFuFuVGR,       & 
& ZcplcFuFuVPL,ZcplcFuFuVPR,ZcplcHpVPVWp,ZcplcVWpVPVWp,ZcplHpcHpVP,ZcplHpcVWpVP,         & 
& ZcplVGVGVG,ZRUZH,ZRUVd,ZRUUd,ZRUVu,ZRUUu,ZRUVe,ZRUUe,ZRUVv,ZRUVvr,ZRUXV,               & 
& ZRUXU,ZRUVSs,MLambda,em,gs,deltaM,kont,gP1LFd)

End if 
! hh
If (CalcLoopDecay_hh) Then 
Call OneLoopDecay_hh(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,MFeOS,MFe2OS,             & 
& MFvOS,MFv2OS,MFxvOS,MFxv2OS,MSscOS,MSsc2OS,MFxeOS,MFxe2OS,MHpOS,MHp2OS,MAhOS,          & 
& MAh2OS,MVZOS,MVZ2OS,MVWpOS,MVWp2OS,ZHOS,ZDLOS,ZDROS,ZULOS,ZUROS,ZELOS,ZEROS,           & 
& UVOS,UVROS,XVOS,XUOS,VSsOS,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,               & 
& MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,             & 
& ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,            & 
& LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,              & 
& mP2,vvSM,vS,dg1,dg2,dg3,dYu,dYd,dYe,dm2SM,dLam,dMDF,dYRD,dMS12,dMS22,dLS1H,            & 
& dLS,dLS2H,dmP2,dLSP,dLSPH,dYRB1,dYRB2,dYRC,dYRA1,dYRA2,dvvSM,dvS,dZH,dZDL,             & 
& dZDR,dZUL,dZUR,dZEL,dZER,dUV,dUVR,dXV,dXU,dVSs,dSinTW,dCosTW,dTanTW,ZfVG,              & 
& ZfHp,Zfed,Zfeu,ZfAh,ZfVP,ZfVZ,ZfVWp,Zfhh,ZfDL,ZfDR,ZfUL,ZfUR,ZfEL,ZfER,ZfVL,           & 
& ZfVR,ZfxVL,ZfxVR,ZfSsc,ZfVPVZ,ZfVZVP,cplAhAhAhAh1,cplAhAhcVWpVWp1,cplAhAhhh,           & 
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

End if 
! Ssc
If (CalcLoopDecay_Ssc) Then 
Call OneLoopDecay_Ssc(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,MFeOS,MFe2OS,            & 
& MFvOS,MFv2OS,MFxvOS,MFxv2OS,MSscOS,MSsc2OS,MFxeOS,MFxe2OS,MHpOS,MHp2OS,MAhOS,          & 
& MAh2OS,MVZOS,MVZ2OS,MVWpOS,MVWp2OS,ZHOS,ZDLOS,ZDROS,ZULOS,ZUROS,ZELOS,ZEROS,           & 
& UVOS,UVROS,XVOS,XUOS,VSsOS,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,               & 
& MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,             & 
& ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,            & 
& LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,              & 
& mP2,vvSM,vS,dg1,dg2,dg3,dYu,dYd,dYe,dm2SM,dLam,dMDF,dYRD,dMS12,dMS22,dLS1H,            & 
& dLS,dLS2H,dmP2,dLSP,dLSPH,dYRB1,dYRB2,dYRC,dYRA1,dYRA2,dvvSM,dvS,dZH,dZDL,             & 
& dZDR,dZUL,dZUR,dZEL,dZER,dUV,dUVR,dXV,dXU,dVSs,dSinTW,dCosTW,dTanTW,ZfVG,              & 
& ZfHp,Zfed,Zfeu,ZfAh,ZfVP,ZfVZ,ZfVWp,Zfhh,ZfDL,ZfDR,ZfUL,ZfUR,ZfEL,ZfER,ZfVL,           & 
& ZfVR,ZfxVL,ZfxVR,ZfSsc,ZfVPVZ,ZfVZVP,cplAhAhhh,cplAhAhSsccSsc1,cplcFeFeAhL,            & 
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

End if 
! Fxe
If (CalcLoopDecay_Fxe) Then 
Call OneLoopDecay_Fxe(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,MFeOS,MFe2OS,            & 
& MFvOS,MFv2OS,MFxvOS,MFxv2OS,MSscOS,MSsc2OS,MFxeOS,MFxe2OS,MHpOS,MHp2OS,MAhOS,          & 
& MAh2OS,MVZOS,MVZ2OS,MVWpOS,MVWp2OS,ZHOS,ZDLOS,ZDROS,ZULOS,ZUROS,ZELOS,ZEROS,           & 
& UVOS,UVROS,XVOS,XUOS,VSsOS,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,               & 
& MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,             & 
& ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,            & 
& LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,              & 
& mP2,vvSM,vS,dg1,dg2,dg3,dYu,dYd,dYe,dm2SM,dLam,dMDF,dYRD,dMS12,dMS22,dLS1H,            & 
& dLS,dLS2H,dmP2,dLSP,dLSPH,dYRB1,dYRB2,dYRC,dYRA1,dYRA2,dvvSM,dvS,dZH,dZDL,             & 
& dZDR,dZUL,dZUR,dZEL,dZER,dUV,dUVR,dXV,dXU,dVSs,dSinTW,dCosTW,dTanTW,ZfVG,              & 
& ZfHp,Zfed,Zfeu,ZfAh,ZfVP,ZfVZ,ZfVWp,Zfhh,ZfDL,ZfDR,ZfUL,ZfUR,ZfEL,ZfER,ZfVL,           & 
& ZfVR,ZfxVL,ZfxVR,ZfSsc,ZfVPVZ,ZfVZVP,cplAhcHpVWp,cplAhHpcVWp,cplcFeFeAhL,              & 
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

End if 
! Fxv
If (CalcLoopDecay_Fxv) Then 
Call OneLoopDecay_Fxv(MhhOS,Mhh2OS,MFdOS,MFd2OS,MFuOS,MFu2OS,MFeOS,MFe2OS,            & 
& MFvOS,MFv2OS,MFxvOS,MFxv2OS,MSscOS,MSsc2OS,MFxeOS,MFxe2OS,MHpOS,MHp2OS,MAhOS,          & 
& MAh2OS,MVZOS,MVZ2OS,MVWpOS,MVWp2OS,ZHOS,ZDLOS,ZDROS,ZULOS,ZUROS,ZELOS,ZEROS,           & 
& UVOS,UVROS,XVOS,XUOS,VSsOS,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,               & 
& MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,             & 
& ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,            & 
& LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,              & 
& mP2,vvSM,vS,dg1,dg2,dg3,dYu,dYd,dYe,dm2SM,dLam,dMDF,dYRD,dMS12,dMS22,dLS1H,            & 
& dLS,dLS2H,dmP2,dLSP,dLSPH,dYRB1,dYRB2,dYRC,dYRA1,dYRA2,dvvSM,dvS,dZH,dZDL,             & 
& dZDR,dZUL,dZUR,dZEL,dZER,dUV,dUVR,dXV,dXU,dVSs,dSinTW,dCosTW,dTanTW,ZfVG,              & 
& ZfHp,Zfed,Zfeu,ZfAh,ZfVP,ZfVZ,ZfVWp,Zfhh,ZfDL,ZfDR,ZfUL,ZfUR,ZfEL,ZfER,ZfVL,           & 
& ZfVR,ZfxVL,ZfxVR,ZfSsc,ZfVPVZ,ZfVZVP,cplAhAhhh,cplAhcHpVWp,cplAhhhVZ,cplAhHpcVWp,      & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,cplcFeFxecSscR,   & 
& cplcFvFeHpL,cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFvFvVZL,cplcFvFvVZR,             & 
& cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVPL,               & 
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

End if 
If (Extra_scale_loopDecays) Then 
q2 = SetRenormalizationScale(q2_save) 
End if 
Iname = Iname - 1 
 
End Subroutine CalculateOneLoopDecays  
 
 
End Module OneLoopDecays_SDdiracDM 
 