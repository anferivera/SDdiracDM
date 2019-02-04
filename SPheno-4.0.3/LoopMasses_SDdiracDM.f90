! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:21 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module LoopMasses_SDdiracDM 
 
Use Control 
Use Settings 
Use Couplings_SDdiracDM 
Use LoopFunctions 
Use AddLoopFunctions 
Use Mathematics 
Use MathematicsQP 
Use Model_Data_SDdiracDM 
Use StandardModel 
Use Tadpoles_SDdiracDM 
 Use Pole2L_SDdiracDM 
 Use TreeLevelMasses_SDdiracDM 
 
Real(dp), Private :: Mhh_1L(2), Mhh2_1L(2)  
Complex(dp), Private :: ZH_1L(2,2)  
Real(dp), Private :: MFd_1L(3), MFd2_1L(3)  
Complex(dp), Private :: ZDL_1L(3,3),ZDR_1L(3,3)
Real(dp), Private :: MFu_1L(3), MFu2_1L(3)  
Complex(dp), Private :: ZUL_1L(3,3),ZUR_1L(3,3)
Real(dp), Private :: MFe_1L(3), MFe2_1L(3)  
Complex(dp), Private :: ZEL_1L(3,3),ZER_1L(3,3)
Real(dp), Private :: MFv_1L(3), MFv2_1L(3)  
Complex(dp), Private :: UV_1L(3,3),UVR_1L(3,3)
Real(dp), Private :: MFxv_1L(2), MFxv2_1L(2)  
Complex(dp), Private :: XV_1L(2,2),XU_1L(2,2)
Real(dp), Private :: MSsc_1L(2), MSsc2_1L(2)  
Complex(dp), Private :: VSs_1L(2,2)  
Real(dp), Private :: MFxe_1L, MFxe2_1L  
Real(dp), Private :: MHp_1L, MHp2_1L  
Real(dp), Private :: MAh_1L, MAh2_1L  
Real(dp), Private :: MVZ_1L, MVZ2_1L  
Real(dp), Private :: MVWp_1L, MVWp2_1L  
Real(dp) :: pi2A0  
Real(dp) :: ti_ep2L(2)  
Real(dp) :: pi_ep2L(2,2)
Real(dp) :: Pi2S_EffPot(2,2)
Real(dp) :: PiP2S_EffPot(1,1)
Contains 
 
Subroutine OneLoopMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,           & 
& MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,              & 
& ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,vvSM,vS,g1,g2,g3,Lam,             & 
& LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,              & 
& MS22,mP2,kont)

Implicit None 
Real(dp),Intent(inout) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2

Complex(dp),Intent(inout) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Real(dp),Intent(inout) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp),Intent(inout) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Real(dp),Intent(inout) :: vvSM,vS

Complex(dp) :: cplAhAhAhAh,cplAhAhcVWpVWp,cplAhAhhh(2),cplAhAhhhhh(2,2),cplAhAhHpcHp,cplAhAhSsccSsc(2,2),& 
& cplAhAhUhh(2),cplAhAhUhhUhh(2,2),cplAhAhUSsccUSsc(2,2),cplAhAhVZVZ,cplAhcHpVWp,        & 
& cplAhhhVZ(2),cplAhHpcVWp,cplAhUhhVZ(2),cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),              & 
& cplcFdFdUhhL(3,3,2),cplcFdFdUhhR(3,3,2),cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),             & 
& cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFdFucHpL(3,3), & 
& cplcFdFucHpR(3,3),cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),cplcFeFeAhL(3,3),              & 
& cplcFeFeAhR(3,3),cplcFeFeUhhL(3,3,2),cplcFeFeUhhR(3,3,2),cplcFeFeVPL(3,3),             & 
& cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),& 
& cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFeFxecUSscL(3,2),cplcFeFxecUSscR(3,2),       & 
& cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFuFuUhhL(3,3,2),cplcFuFuUhhR(3,3,2),             & 
& cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),  & 
& cplcFuFuVZR(3,3),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFvFxvcUSscL(3,2,2),             & 
& cplcFvFxvcUSscR(3,2,2),cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFxeFxeVPL,            & 
& cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),         & 
& cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),           & 
& cplcFxvFxvUhhL(2,2,2),cplcFxvFxvUhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),     & 
& cplcgAgWpcVWp,cplcgGgGVG,cplcgWCgAcVWp,cplcgWCgWCAh,cplcgWCgWCUhh(2),cplcgWCgWCVP,     & 
& cplcgWCgWCVZ,cplcgWCgZcHp,cplcgWCgZcVWp,cplcgWpgWpAh,cplcgWpgWpUhh(2),cplcgWpgWpVP,    & 
& cplcgWpgWpVZ,cplcgWpgZHp,cplcgZgWCHp,cplcgZgWpcHp,cplcgZgWpcVWp,cplcgZgZUhh(2),        & 
& cplcHpVPVWp,cplcHpVWpVZ,cplcUFdFdAhL(3,3),cplcUFdFdAhR(3,3),cplcUFdFdhhL(3,3,2),       & 
& cplcUFdFdhhR(3,3,2),cplcUFdFdVGL(3,3),cplcUFdFdVGR(3,3),cplcUFdFdVPL(3,3),             & 
& cplcUFdFdVPR(3,3),cplcUFdFdVZL(3,3),cplcUFdFdVZR(3,3),cplcUFdFucHpL(3,3),              & 
& cplcUFdFucHpR(3,3),cplcUFdFucVWpL(3,3),cplcUFdFucVWpR(3,3),cplcUFeFeAhL(3,3),          & 
& cplcUFeFeAhR(3,3),cplcUFeFehhL(3,3,2),cplcUFeFehhR(3,3,2),cplcUFeFeVPL(3,3),           & 
& cplcUFeFeVPR(3,3),cplcUFeFeVZL(3,3),cplcUFeFeVZR(3,3),cplcUFeFvcHpL(3,3),              & 
& cplcUFeFvcHpR(3,3),cplcUFeFvcVWpL(3,3),cplcUFeFvcVWpR(3,3),cplcUFeFxecSscL(3,2),       & 
& cplcUFeFxecSscR(3,2),cplcUFuFdHpL(3,3),cplcUFuFdHpR(3,3),cplcUFuFdVWpL(3,3),           & 
& cplcUFuFdVWpR(3,3),cplcUFuFuAhL(3,3),cplcUFuFuAhR(3,3),cplcUFuFuhhL(3,3,2),            & 
& cplcUFuFuhhR(3,3,2),cplcUFuFuVGL(3,3),cplcUFuFuVGR(3,3),cplcUFuFuVPL(3,3),             & 
& cplcUFuFuVPR(3,3),cplcUFuFuVZL(3,3),cplcUFuFuVZR(3,3),cplcUFvFeHpL(3,3),               & 
& cplcUFvFeHpR(3,3),cplcUFvFeVWpL(3,3),cplcUFvFeVWpR(3,3),cplcUFvFvVZL(3,3),             & 
& cplcUFvFvVZR(3,3),cplcUFvFxvcSscL(3,2,2),cplcUFvFxvcSscR(3,2,2),cplcUFxvFvSscL(2,3,2), & 
& cplcUFxvFvSscR(2,3,2),cplcUFxvFxeHpL(2),cplcUFxvFxeHpR(2),cplcUFxvFxeVWpL(2),          & 
& cplcUFxvFxeVWpR(2),cplcUFxvFxvAhL(2,2),cplcUFxvFxvAhR(2,2),cplcUFxvFxvhhL(2,2,2),      & 
& cplcUFxvFxvhhR(2,2,2),cplcUFxvFxvVZL(2,2),cplcUFxvFxvVZR(2,2),cplcVWpcVWpVWpVWp1,      & 
& cplcVWpcVWpVWpVWp2,cplcVWpcVWpVWpVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,cplcVWpVPVPVWp3, & 
& cplcVWpVPVWp,cplcVWpVPVWpVZ1,cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpVWpVZ

Complex(dp) :: cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,cplhhcHpVWp(2),cplhhcVWpVWp(2),        & 
& cplhhhhcVWpVWp(2,2),cplhhhhHpcHp(2,2),cplhhhhUSsccUSsc(2,2,2,2),cplhhhhVZVZ(2,2),      & 
& cplhhHpcHp(2),cplhhHpcVWp(2),cplhhSsccUSsc(2,2,2),cplhhVZVZ(2),cplHpcHpcVWpVWp,        & 
& cplHpcHpVP,cplHpcHpVPVP,cplHpcHpVPVZ,cplHpcHpVZ,cplHpcHpVZVZ,cplHpcVWpVP,              & 
& cplHpcVWpVZ,cplHpHpcHpcHp,cplHpSsccHpcSsc(2,2),cplHpUSsccHpcUSsc(2,2),cplUhhcVWpVWp(2),& 
& cplUhhhhhh(2,2,2),cplUhhHpcHp(2),cplUhhHpcVWp(2),cplUhhSsccSsc(2,2,2),cplUhhUhhcVWpVWp(2,2),& 
& cplUhhUhhhhhh(2,2,2,2),cplUhhUhhHpcHp(2,2),cplUhhUhhSsccSsc(2,2,2,2),cplUhhUhhVZVZ(2,2),& 
& cplUhhVZVZ(2),cplUSscSsccUSsccSsc(2,2,2,2),cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,        & 
& cplVGVGVGVG3

Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1, j2, j3, j4, il, i_count, ierr 
Integer :: i2L, iFin 
Logical :: Convergence2L 
Real(dp) :: Pi2S_EffPot_save(2,2), diff(2,2)
Complex(dp) :: Tad1Loop(2), dmz2  
Real(dp) :: comp(2), tanbQ, vev2, vSM
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopMasses' 
 
kont = 0 
 
! Set Gauge fixing parameters 
RXi= RXiNew 
RXiG = RXi 
RXiP = RXi 
RXiWp = RXi 
RXiZ = RXi 

 ! Running angles 

 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,              & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,            & 
& ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& GenerationMixing,kont)

mP2Tree  = mP2
m2SMTree  = m2SM

 
 If (CalculateOneLoopMasses) Then 
 
If ((DecoupleAtRenScale).and.(Abs(1._dp-RXiNew).lt.0.01_dp)) Then 
vSM=vSM_Q 
vvSM=vSM 
Else 
Call CouplingsForVectorBosons(g1,g2,TW,XV,XU,ZH,vvSM,ZDL,ZUL,ZEL,UV,cplcFxeFxeVPL,    & 
& cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,             & 
& cplAhcHpVWp,cplhhcHpVWp,cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplHpcHpVPVP,    & 
& cplHpcHpcVWpVWp,cplHpcHpVZVZ,cplAhhhVZ,cplAhHpcVWp,cplAhAhcVWpVWp,cplAhAhVZVZ,         & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,               & 
& cplcgWpgWpVP,cplcgWCgWCVP,cplHpcVWpVP,cplcVWpVPVWp,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,    & 
& cplcVWpVPVPVWp3,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,           & 
& cplcFuFuVZR,cplcFvFvVZL,cplcFvFvVZR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWpVZ,          & 
& cplcgWCgWCVZ,cplhhVZVZ,cplHpcVWpVZ,cplcVWpVWpVZ,cplhhhhVZVZ,cplcVWpVWpVZVZ1,           & 
& cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,cplcFdFucVWpL,cplcFdFucVWpR,cplcFeFvcVWpL,             & 
& cplcFeFvcVWpR,cplcgWCgAcVWp,cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgZcVWp,cplhhHpcVWp,     & 
& cplhhcVWpVWp,cplhhhhcVWpVWp,cplcVWpcVWpVWpVWp1,cplcVWpcVWpVWpVWp2,cplcVWpcVWpVWpVWp3,  & 
& cplHpcHpVPVZ,cplcVWpVPVWpVZ1,cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3)

Call Pi1LoopVZ(mZ2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,             & 
& MFxe,MFxe2,MFxv,MFxv2,MVZ,MVZ2,MHp,MHp2,MVWp,MVWp2,cplAhhhVZ,cplcFdFdVZL,              & 
& cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFvFvVZL,               & 
& cplcFvFvVZR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWpVZ,      & 
& cplcgWCgWCVZ,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ,cplhhhhVZVZ,    & 
& cplHpcHpVZVZ,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,kont,dmZ2)

vev2=4._dp*Real(mZ2+dmz2,dp)/(g1**2+g2**2) -0 
vSM=sqrt(vev2) 
vvSM=vSM 
End if 
Call SolveTadpoleEquations(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,               & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,(/ ZeroC, ZeroC /))

Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,              & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,            & 
& ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& GenerationMixing,kont)

Call CouplingsForLoopMasses(Lam,LSPH,vvSM,vS,g1,g2,TW,Yd,ZDL,ZDR,Ye,ZEL,              & 
& ZER,Yu,ZUL,ZUR,YRD,YRC,XV,XU,LSP,ZH,LS1H,LS2H,VSs,g3,UV,YRA1,YRA2,YRB1,YRB2,           & 
& UVR,LS,cplAhAhUhh,cplAhUhhVZ,cplcFdFdUhhL,cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,      & 
& cplcFuFuUhhL,cplcFuFuUhhR,cplcFxvFxvUhhL,cplcFxvFxvUhhR,cplcgWpgWpUhh,cplcgWCgWCUhh,   & 
& cplcgZgZUhh,cplUhhhhhh,cplUhhHpcHp,cplUhhHpcVWp,cplUhhSsccSsc,cplUhhcVWpVWp,           & 
& cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,cplUhhUhhHpcHp,cplUhhUhhSsccSsc,cplUhhUhhcVWpVWp,& 
& cplUhhUhhVZVZ,cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdFdhhL,cplcUFdFdhhR,cplcUFdFdVGL,        & 
& cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,cplcUFdFucHpL,        & 
& cplcUFdFucHpR,cplcUFdFucVWpL,cplcUFdFucVWpR,cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuFdHpL,    & 
& cplcUFuFdHpR,cplcUFuFdVWpL,cplcUFuFdVWpR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,       & 
& cplcUFuFuVGR,cplcUFuFuVPL,cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,cplcUFeFeAhL,         & 
& cplcUFeFeAhR,cplcUFeFehhL,cplcUFeFehhR,cplcUFeFeVPL,cplcUFeFeVPR,cplcUFeFeVZL,         & 
& cplcUFeFeVZR,cplcUFeFvcHpL,cplcUFeFvcHpR,cplcUFeFvcVWpL,cplcUFeFvcVWpR,cplcUFeFxecSscL,& 
& cplcUFeFxecSscR,cplcUFvFeHpL,cplcUFvFeHpR,cplcUFvFeVWpL,cplcUFvFeVWpR,cplcUFvFvVZL,    & 
& cplcUFvFvVZR,cplcUFvFxvcSscL,cplcUFvFxvcSscR,cplcUFxvFxvAhL,cplcUFxvFxvAhR,            & 
& cplcUFxvFvSscL,cplcUFxvFvSscR,cplcUFxvFxeHpL,cplcUFxvFxeHpR,cplcUFxvFxeVWpL,           & 
& cplcUFxvFxeVWpR,cplcUFxvFxvhhL,cplcUFxvFxvhhR,cplcUFxvFxvVZL,cplcUFxvFxvVZR,           & 
& cplcFeFxecUSscL,cplcFeFxecUSscR,cplcFvFxvcUSscL,cplcFvFxvcUSscR,cplhhSsccUSsc,         & 
& cplAhAhUSsccUSsc,cplhhhhUSsccUSsc,cplHpUSsccHpcUSsc,cplUSscSsccUSsccSsc,               & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,   & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplAhcHpVWp,             & 
& cplcFdFucHpL,cplcFdFucHpR,cplcFeFvcHpL,cplcFeFvcHpR,cplcgZgWpcHp,cplcgWpgZHp,          & 
& cplcgWCgZcHp,cplcgZgWCHp,cplhhHpcHp,cplhhcHpVWp,cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,     & 
& cplcHpVWpVZ,cplAhAhHpcHp,cplhhhhHpcHp,cplHpHpcHpcHp,cplHpSsccHpcSsc,cplHpcHpVPVP,      & 
& cplHpcHpcVWpVWp,cplHpcHpVZVZ,cplAhAhhh,cplcFdFdAhL,cplcFdFdAhR,cplcFeFeAhL,            & 
& cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplcFxvFxvAhL,cplcFxvFxvAhR,cplcgWpgWpAh,          & 
& cplcgWCgWCAh,cplAhhhVZ,cplAhHpcVWp,cplAhAhAhAh,cplAhAhhhhh,cplAhAhSsccSsc,             & 
& cplAhAhcVWpVWp,cplAhAhVZVZ,cplcFdFdVGL,cplcFdFdVGR,cplcFuFuVGL,cplcFuFuVGR,            & 
& cplcgGgGVG,cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3,cplcFdFdVPL,               & 
& cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,cplcgWpgWpVP,              & 
& cplcgWCgWCVP,cplHpcVWpVP,cplcVWpVPVWp,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,cplcVWpVPVPVWp3, & 
& cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFvFvVZL,cplcFvFvVZR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWpVZ,cplcgWCgWCVZ,         & 
& cplhhVZVZ,cplHpcVWpVZ,cplcVWpVWpVZ,cplhhhhVZVZ,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,        & 
& cplcVWpVWpVZVZ3,cplcFdFucVWpL,cplcFdFucVWpR,cplcFeFvcVWpL,cplcFeFvcVWpR,               & 
& cplcgWCgAcVWp,cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgZcVWp,cplhhHpcVWp,cplhhcVWpVWp,      & 
& cplhhhhcVWpVWp,cplcVWpcVWpVWpVWp1,cplcVWpcVWpVWpVWp2,cplcVWpcVWpVWpVWp3,               & 
& cplHpcHpVPVZ,cplcVWpVPVWpVZ1,cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3)

Call OneLoopTadpoleshh(vvSM,vS,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,              & 
& MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,cplAhAhUhh,cplcFdFdUhhL,        & 
& cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcFxvFxvUhhL,       & 
& cplcFxvFxvUhhR,cplcgWpgWpUhh,cplcgWCgWCUhh,cplcgZgZUhh,cplUhhhhhh,cplUhhHpcHp,         & 
& cplUhhSsccSsc,cplUhhcVWpVWp,cplUhhVZVZ,Tad1Loop(1:2))

mP2Tree  = mP2
m2SMTree  = m2SM
If (CalculateTwoLoopHiggsMasses) Then 
    If(GaugelessLimit) Then 
  vvSMFix = 0._dp 
  vSFix = 0._dp 
   g1_saveEP =g1
   g1 = 0._dp 
   g2_saveEP =g2
   g2 = 0._dp 
     Else 
  vvSMFix = vvSM 
  vSFix = vS 
     End if 

SELECT CASE (TwoLoopMethod) 
CASE ( 1 , 2 ) 


 CASE ( 3 ) ! Diagrammatic method 
  ! Make sure that there are no exactly degenerated masses! 
   Yu_saveEP =Yu
   where (aint(Abs(Yu)).eq.Yu) Yu=Yu*(1 + 1*1.0E-12_dp)
   Yd_saveEP =Yd
   where (aint(Abs(Yd)).eq.Yd) Yd=Yd*(1 + 2*1.0E-12_dp)
   Ye_saveEP =Ye
   where (aint(Abs(Ye)).eq.Ye) Ye=Ye*(1 + 3*1.0E-12_dp)

If (NewGBC) Then 
Call CalculatePi2S(125._dp**2,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,             & 
& Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,kont,ti_ep2L,              & 
& Pi2S_EffPot,PiP2S_EffPot)

Else 
Call CalculatePi2S(0._dp,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,               & 
& Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,kont,ti_ep2L,Pi2S_EffPot,     & 
& PiP2S_EffPot)

End if 
   Yu =Yu_saveEP 
   Yd =Yd_saveEP 
   Ye =Ye_saveEP 


 CASE ( 8 , 9 ) ! Hard-coded routines 
  
 END SELECT
 
   If(GaugelessLimit) Then 
   g1 =g1_saveEP 
   g2 =g2_saveEP 
   End if 

Else ! Two loop turned off 
Pi2S_EffPot = 0._dp 

Pi2A0 = 0._dp 

ti_ep2L = 0._dp 

End if 
Call SolveTadpoleEquations(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,               & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,Tad1Loop)

mP21L = mP2
m2SM1L = m2SM
Tad1Loop(1:2) = Tad1Loop(1:2) - ti_ep2L 
Call SolveTadpoleEquations(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,               & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,Tad1Loop)

mP22L = mP2
m2SM2L = m2SM
Call OneLoophh(m2SM2L,Lam,mP22L,LSP,LSPH,vvSM,vS,MAh,MAh2,MVZ,MVZ2,MFd,               & 
& MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MSsc,MSsc2,             & 
& cplAhAhUhh,cplAhUhhVZ,cplcFdFdUhhL,cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,             & 
& cplcFuFuUhhL,cplcFuFuUhhR,cplcFxvFxvUhhL,cplcFxvFxvUhhR,cplcgWpgWpUhh,cplcgWCgWCUhh,   & 
& cplcgZgZUhh,cplUhhhhhh,cplUhhHpcHp,cplUhhHpcVWp,cplUhhSsccSsc,cplUhhcVWpVWp,           & 
& cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,cplUhhUhhHpcHp,cplUhhUhhSsccSsc,cplUhhUhhcVWpVWp,& 
& cplUhhUhhVZVZ,0.1_dp*delta_mass,Mhh_1L,Mhh2_1L,ZH_1L,kont)

Call OneLoopFd(Yd,vvSM,MFd,MFd2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,MFu,              & 
& MFu2,MVWp,MVWp2,cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdFdhhL,cplcUFdFdhhR,cplcUFdFdVGL,      & 
& cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,cplcUFdFucHpL,        & 
& cplcUFdFucHpR,cplcUFdFucVWpL,cplcUFdFucVWpR,0.1_dp*delta_mass,MFd_1L,MFd2_1L,          & 
& ZDL_1L,ZDR_1L,kont)

Call OneLoopFu(Yu,vvSM,MFu,MFu2,MAh,MAh2,MHp,MHp2,MFd,MFd2,MVWp,MVWp2,Mhh,            & 
& Mhh2,MVZ,MVZ2,cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuFdHpL,cplcUFuFdHpR,cplcUFuFdVWpL,       & 
& cplcUFuFdVWpR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,        & 
& cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,0.1_dp*delta_mass,MFu_1L,MFu2_1L,               & 
& ZUL_1L,ZUR_1L,kont)

Call OneLoopFe(Ye,vvSM,MFe,MFe2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,MFv,              & 
& MFv2,MVWp,MVWp2,MSsc,MSsc2,MFxe,MFxe2,cplcUFeFeAhL,cplcUFeFeAhR,cplcUFeFehhL,          & 
& cplcUFeFehhR,cplcUFeFeVPL,cplcUFeFeVPR,cplcUFeFeVZL,cplcUFeFeVZR,cplcUFeFvcHpL,        & 
& cplcUFeFvcHpR,cplcUFeFvcVWpL,cplcUFeFvcVWpR,cplcUFeFxecSscL,cplcUFeFxecSscR,           & 
& 0.1_dp*delta_mass,MFe_1L,MFe2_1L,ZEL_1L,ZER_1L,kont)

Call OneLoopFv(MHp,MHp2,MFe,MFe2,MVWp,MVWp2,MVZ,MVZ2,MFv,MFv2,MSsc,MSsc2,             & 
& MFxv,MFxv2,cplcUFvFeHpL,cplcUFvFeHpR,cplcUFvFeVWpL,cplcUFvFeVWpR,cplcUFvFvVZL,         & 
& cplcUFvFvVZR,cplcUFvFxvcSscL,cplcUFvFxvcSscR,0.1_dp*delta_mass,MFv_1L,MFv2_1L,         & 
& UV_1L,UVR_1L,kont)

Call OneLoopFxv(MDF,YRD,YRC,vvSM,vS,MFxv,MFxv2,MAh,MAh2,MSsc,MSsc2,MFv,               & 
& MFv2,MHp,MHp2,MFxe,MFxe2,MVWp,MVWp2,Mhh,Mhh2,MVZ,MVZ2,cplcUFxvFxvAhL,cplcUFxvFxvAhR,   & 
& cplcUFxvFvSscL,cplcUFxvFvSscR,cplcUFxvFxeHpL,cplcUFxvFxeHpR,cplcUFxvFxeVWpL,           & 
& cplcUFxvFxeVWpR,cplcUFxvFxvhhL,cplcUFxvFxvhhR,cplcUFxvFxvVZL,cplcUFxvFxvVZR,           & 
& 0.1_dp*delta_mass,MFxv_1L,MFxv2_1L,XV_1L,XU_1L,kont)

Call OneLoopSsc(MS12,MS22,LS1H,LS2H,vvSM,MFe,MFe2,MFxe,MFxe2,MFv,MFv2,MFxv,           & 
& MFxv2,MSsc,MSsc2,Mhh,Mhh2,MAh,MAh2,MHp,MHp2,cplcFeFxecUSscL,cplcFeFxecUSscR,           & 
& cplcFvFxvcUSscL,cplcFvFxvcUSscR,cplhhSsccUSsc,cplAhAhUSsccUSsc,cplhhhhUSsccUSsc,       & 
& cplHpUSsccHpcUSsc,cplUSscSsccUSsccSsc,0.1_dp*delta_mass,MSsc_1L,MSsc2_1L,              & 
& VSs_1L,kont)

Call OneLoopFxe(MDF,MSsc,MSsc2,MFe,MFe2,MFxe,MFxe2,MVZ,MVZ2,MHp,MHp2,MFxv,            & 
& MFxv2,MVWp,MVWp2,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,              & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,             & 
& cplcFxeFxvcVWpR,0.1_dp*delta_mass,MFxe_1L,MFxe2_1L,kont)

Call OneLoopHp(g2,m2SM1L,Lam,LSPH,vvSM,vS,MVWp,MVWp2,MAh,MAh2,MFd,MFd2,               & 
& MFu,MFu2,MFe,MFe2,MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,MHp,MHp2,Mhh,Mhh2,MVZ,MVZ2,           & 
& MSsc,MSsc2,cplAhcHpVWp,cplcFdFucHpL,cplcFdFucHpR,cplcFeFvcHpL,cplcFeFvcHpR,            & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcgZgWpcHp,cplcgWpgZHp,cplcgWCgZcHp,cplcgZgWCHp,       & 
& cplhhHpcHp,cplhhcHpVWp,cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplAhAhHpcHp,     & 
& cplhhhhHpcHp,cplHpHpcHpcHp,cplHpSsccHpcSsc,cplHpcHpVPVP,cplHpcHpcVWpVWp,               & 
& cplHpcHpVZVZ,0.1_dp*delta_mass,MHp_1L,MHp2_1L,kont)

Call OneLoopAh(g1,g2,m2SM2L,Lam,LSPH,vvSM,vS,TW,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,           & 
& MFe,MFe2,MFu,MFu2,MFxv,MFxv2,MVZ,MVZ2,MVWp,MVWp2,MHp,MHp2,MSsc,MSsc2,cplAhAhhh,        & 
& cplcFdFdAhL,cplcFdFdAhR,cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,               & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ,cplAhHpcVWp,           & 
& cplAhAhAhAh,cplAhAhhhhh,cplAhAhHpcHp,cplAhAhSsccSsc,cplAhAhcVWpVWp,cplAhAhVZVZ,        & 
& 0.1_dp*delta_mass,MAh_1L,MAh2_1L,kont)

Mhh = Mhh_1L 
Mhh2 = Mhh2_1L 
ZH = ZH_1L 
MFv = MFv_1L 
MFv2 = MFv2_1L 
UV = UV_1L 
UVR = UVR_1L 
MFxv = MFxv_1L 
MFxv2 = MFxv2_1L 
XV = XV_1L 
XU = XU_1L 
MSsc = MSsc_1L 
MSsc2 = MSsc2_1L 
VSs = VSs_1L 
MFxe = MFxe_1L 
MFxe2 = MFxe2_1L 
MHp = MHp_1L 
MHp2 = MHp2_1L 
MAh = MAh_1L 
MAh2 = MAh2_1L 
End If 
 
Call SortGoldstones(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,          & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,            & 
& ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,kont)

! Set pole masses 
MVWp = mW 
MVWp2 = mW2 
MVZ = mZ 
MVZ2 = mZ2 
MFe(1:3) = mf_l 
MFe2(1:3) = mf_l**2 
MFu(1:3) = mf_u 
MFu2(1:3) = mf_u**2 
MFd(1:3) = mf_d 
MFd2(1:3) = mf_d**2 
! Shift Everything to t'Hooft Gauge
RXi=  1._dp 
RXiG = 1._dp 
RXiP = 1._dp 
RXiWp = 1._dp 
RXiZ = 1._dp 
MAh=MVZ
MAh2=MVZ2
MHp=MVWp
MHp2=MVWp2
mf_u2 = mf_u**2 
mf_d2 = mf_d**2 
mf_l2 = mf_l**2 
 

 If (WriteTreeLevelTadpoleSolutions) Then 
! Saving tree-level parameters for output
mP2  = mP2Tree 
m2SM  = m2SMTree 
End if 


Iname = Iname -1 
End Subroutine OneLoopMasses 
 
Subroutine OneLoopTadpoleshh(vvSM,vS,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,             & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,cplAhAhUhh,cplcFdFdUhhL,   & 
& cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcFxvFxvUhhL,       & 
& cplcFxvFxvUhhR,cplcgWpgWpUhh,cplcgWCgWCUhh,cplcgZgZUhh,cplUhhhhhh,cplUhhHpcHp,         & 
& cplUhhSsccSsc,cplUhhcVWpVWp,cplUhhVZVZ,tadpoles)

Implicit None 
Real(dp), Intent(in) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFxv(2),MFxv2(2),               & 
& Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhAhUhh(2),cplcFdFdUhhL(3,3,2),cplcFdFdUhhR(3,3,2),cplcFeFeUhhL(3,3,2),            & 
& cplcFeFeUhhR(3,3,2),cplcFuFuUhhL(3,3,2),cplcFuFuUhhR(3,3,2),cplcFxvFxvUhhL(2,2,2),     & 
& cplcFxvFxvUhhR(2,2,2),cplcgWpgWpUhh(2),cplcgWCgWCUhh(2),cplcgZgZUhh(2),cplUhhhhhh(2,2,2),& 
& cplUhhHpcHp(2),cplUhhSsccSsc(2,2,2),cplUhhcVWpVWp(2),cplUhhVZVZ(2)

Real(dp), Intent(in) :: vvSM,vS

Integer :: i1,i2, gO1, gO2 
Complex(dp) :: coupL, coupR, coup, temp, res, A0m, sumI(2)  
Real(dp) :: m1 
Complex(dp), Intent(out) :: tadpoles(2) 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopTadpoleshh'
 
tadpoles = 0._dp 
 
!------------------------ 
! Ah 
!------------------------ 
If (Include_in_loopAh) Then 
A0m = SA_A0(MAh2) 
  Do gO1 = 1, 2
   coup = cplAhAhUhh(gO1)
   sumI(gO1) = -coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp/2._dp*sumI 
End if 
!------------------------ 
! bar[Fd] 
!------------------------ 
If (Include_in_loopFd) Then 
Do i1 = 1, 3
 A0m = 2._dp*MFd(i1)*SA_A0(MFd2(i1)) 
  Do gO1 = 1, 2
   coupL = cplcFdFdUhhL(i1,i1,gO1)
   coupR = cplcFdFdUhhR(i1,i1,gO1)
   sumI(gO1) = (coupL+coupR)*A0m 
  End Do 
 
tadpoles =  tadpoles + 3._dp*sumI 
End Do 
 End if 
!------------------------ 
! bar[Fe] 
!------------------------ 
If (Include_in_loopFe) Then 
Do i1 = 1, 3
 A0m = 2._dp*MFe(i1)*SA_A0(MFe2(i1)) 
  Do gO1 = 1, 2
   coupL = cplcFeFeUhhL(i1,i1,gO1)
   coupR = cplcFeFeUhhR(i1,i1,gO1)
   sumI(gO1) = (coupL+coupR)*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
End Do 
 End if 
!------------------------ 
! bar[Fu] 
!------------------------ 
If (Include_in_loopFu) Then 
Do i1 = 1, 3
 A0m = 2._dp*MFu(i1)*SA_A0(MFu2(i1)) 
  Do gO1 = 1, 2
   coupL = cplcFuFuUhhL(i1,i1,gO1)
   coupR = cplcFuFuUhhR(i1,i1,gO1)
   sumI(gO1) = (coupL+coupR)*A0m 
  End Do 
 
tadpoles =  tadpoles + 3._dp*sumI 
End Do 
 End if 
!------------------------ 
! bar[Fxv] 
!------------------------ 
If (Include_in_loopFxv) Then 
Do i1 = 1, 2
 A0m = 2._dp*MFxv(i1)*SA_A0(MFxv2(i1)) 
  Do gO1 = 1, 2
   coupL = cplcFxvFxvUhhL(i1,i1,gO1)
   coupR = cplcFxvFxvUhhR(i1,i1,gO1)
   sumI(gO1) = (coupL+coupR)*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
End Do 
 End if 
!------------------------ 
! bar[gWp] 
!------------------------ 
If (Include_in_loopgWp) Then 
A0m = 1._dp*SA_A0(MVWp2*RXi) 
  Do gO1 = 1, 2
    coup = cplcgWpgWpUhh(gO1)
    sumI(gO1) = coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
End if 
!------------------------ 
! bar[gWpC] 
!------------------------ 
If (Include_in_loopgWC) Then 
A0m = 1._dp*SA_A0(MVWp2*RXi) 
  Do gO1 = 1, 2
    coup = cplcgWCgWCUhh(gO1)
    sumI(gO1) = coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
End if 
!------------------------ 
! bar[gZ] 
!------------------------ 
If (Include_in_loopgZ) Then 
A0m = 1._dp*SA_A0(MVZ2*RXi) 
  Do gO1 = 1, 2
    coup = cplcgZgZUhh(gO1)
    sumI(gO1) = coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
End if 
!------------------------ 
! hh 
!------------------------ 
If (Include_in_loophh) Then 
Do i1 = 1, 2
 A0m = SA_A0(Mhh2(i1)) 
  Do gO1 = 1, 2
   coup = cplUhhhhhh(gO1,i1,i1)
   sumI(gO1) = -coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp/2._dp*sumI 
End Do 
 End if 
!------------------------ 
! conj[Hp] 
!------------------------ 
If (Include_in_loopHp) Then 
A0m = SA_A0(MHp2) 
  Do gO1 = 1, 2
   coup = cplUhhHpcHp(gO1)
   sumI(gO1) = -coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
End if 
!------------------------ 
! conj[Ssc] 
!------------------------ 
If (Include_in_loopSsc) Then 
Do i1 = 1, 2
 A0m = SA_A0(MSsc2(i1)) 
  Do gO1 = 1, 2
   coup = cplUhhSsccSsc(gO1,i1,i1)
   sumI(gO1) = -coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
End Do 
 End if 
!------------------------ 
! conj[VWp] 
!------------------------ 
If (Include_in_loopVWp) Then 
A0m = 3._dp*SA_A0(MVWp2)+RXi*SA_A0(MVWp2*RXi) - 2._dp*MVWp2*rMS 
  Do gO1 = 1, 2
    coup = cplUhhcVWpVWp(gO1)
    sumI(gO1) = coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp*sumI 
End if 
!------------------------ 
! VZ 
!------------------------ 
If (Include_in_loopVZ) Then 
A0m = 3._dp*SA_A0(MVZ2)+RXi*SA_A0(MVZ2*RXi) - 2._dp*MVZ2*rMS 
  Do gO1 = 1, 2
    coup = cplUhhVZVZ(gO1)
    sumI(gO1) = coup*A0m 
  End Do 
 
tadpoles =  tadpoles + 1._dp/2._dp*sumI 
End if 



tadpoles = oo16pi2*tadpoles 
Iname = Iname - 1 
End Subroutine OneLoopTadpoleshh 
 
Subroutine OneLoophh(m2SM,Lam,mP2,LSP,LSPH,vvSM,vS,MAh,MAh2,MVZ,MVZ2,MFd,             & 
& MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MSsc,MSsc2,             & 
& cplAhAhUhh,cplAhUhhVZ,cplcFdFdUhhL,cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,             & 
& cplcFuFuUhhL,cplcFuFuUhhR,cplcFxvFxvUhhL,cplcFxvFxvUhhR,cplcgWpgWpUhh,cplcgWCgWCUhh,   & 
& cplcgZgZUhh,cplUhhhhhh,cplUhhHpcHp,cplUhhHpcVWp,cplUhhSsccSsc,cplUhhcVWpVWp,           & 
& cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,cplUhhUhhHpcHp,cplUhhUhhSsccSsc,cplUhhUhhcVWpVWp,& 
& cplUhhUhhVZVZ,delta,mass,mass2,RS,kont)

Implicit None 
Real(dp), Intent(in) :: MAh,MAh2,MVZ,MVZ2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFxv(2),               & 
& MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MVWp,MVWp2,MSsc(2),MSsc2(2)

Real(dp), Intent(in) :: m2SM,mP2,LSP,LSPH,vvSM,vS

Complex(dp), Intent(in) :: Lam

Complex(dp), Intent(in) :: cplAhAhUhh(2),cplAhUhhVZ(2),cplcFdFdUhhL(3,3,2),cplcFdFdUhhR(3,3,2),cplcFeFeUhhL(3,3,2),& 
& cplcFeFeUhhR(3,3,2),cplcFuFuUhhL(3,3,2),cplcFuFuUhhR(3,3,2),cplcFxvFxvUhhL(2,2,2),     & 
& cplcFxvFxvUhhR(2,2,2),cplcgWpgWpUhh(2),cplcgWCgWCUhh(2),cplcgZgZUhh(2),cplUhhhhhh(2,2,2),& 
& cplUhhHpcHp(2),cplUhhHpcVWp(2),cplUhhSsccSsc(2,2,2),cplUhhcVWpVWp(2),cplUhhVZVZ(2),    & 
& cplAhAhUhhUhh(2,2),cplUhhUhhhhhh(2,2,2,2),cplUhhUhhHpcHp(2,2),cplUhhUhhSsccSsc(2,2,2,2),& 
& cplUhhUhhcVWpVWp(2,2),cplUhhUhhVZVZ(2,2)

Complex(dp) :: mat2a(2,2), mat2(2,2),  PiSf(2,2,2)
Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(2), test_m2(2),p2, test(2) 
Real(dp), Intent(out) :: mass(2), mass2(2) 
Complex(dp), Intent(out) ::  RS(2,2) 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoophh'
 
mat2a(1,1) = 0._dp 
mat2a(1,1) = mat2a(1,1)-1._dp*(m2SM)
mat2a(1,1) = mat2a(1,1)+(3*Lam*vvSM**2)/2._dp
mat2a(1,1) = mat2a(1,1)+(LSPH*vS**2)/2._dp
mat2a(1,2) = 0._dp 
mat2a(1,2) = mat2a(1,2)+LSPH*vvSM*vS
mat2a(2,2) = 0._dp 
mat2a(2,2) = mat2a(2,2)+mP2/2._dp
mat2a(2,2) = mat2a(2,2)+(LSPH*vvSM**2)/2._dp
mat2a(2,2) = mat2a(2,2)+(3*LSP*vS**2)/2._dp

 
 Do i1=2,2
  Do i2 = 1, i1-1 
  mat2a(i1,i2) = (mat2a(i2,i1)) 
  End do 
End do 

 
! Rotation matrix for p2=0 
PiSf(1,:,:) = ZeroC 
p2 = 0._dp 
Call Pi1Loophh(p2,MAh,MAh2,MVZ,MVZ2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,            & 
& Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MSsc,MSsc2,cplAhAhUhh,cplAhUhhVZ,cplcFdFdUhhL,            & 
& cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcFxvFxvUhhL,       & 
& cplcFxvFxvUhhR,cplcgWpgWpUhh,cplcgWCgWCUhh,cplcgZgZUhh,cplUhhhhhh,cplUhhHpcHp,         & 
& cplUhhHpcVWp,cplUhhSsccSsc,cplUhhcVWpVWp,cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,       & 
& cplUhhUhhHpcHp,cplUhhUhhSsccSsc,cplUhhUhhcVWpVWp,cplUhhUhhVZVZ,kont,PiSf(1,:,:))

PiSf(1,:,:) = PiSf(1,:,:) - Pi2S_EffPot 
mat2 = mat2a - Real(PiSf(1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
ZHOS_0 = RS 
 
 
! Now with momenta 
Do i1=1,2
PiSf(i1,:,:) = ZeroC 
p2 = Mhh2(i1)
Call Pi1Loophh(p2,MAh,MAh2,MVZ,MVZ2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,            & 
& Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MSsc,MSsc2,cplAhAhUhh,cplAhUhhVZ,cplcFdFdUhhL,            & 
& cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcFxvFxvUhhL,       & 
& cplcFxvFxvUhhR,cplcgWpgWpUhh,cplcgWCgWCUhh,cplcgZgZUhh,cplUhhhhhh,cplUhhHpcHp,         & 
& cplUhhHpcVWp,cplUhhSsccSsc,cplUhhcVWpVWp,cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,       & 
& cplUhhUhhHpcHp,cplUhhUhhSsccSsc,cplUhhUhhcVWpVWp,cplUhhUhhVZVZ,kont,PiSf(i1,:,:))

End Do 
Do i1=2,1,-1 
PiSf(i1,:,:) = PiSf(i1,:,:) - Pi2S_EffPot 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
End If 
mass2(i1) = mi2(i1) 
End do 
 
Do i1=1,2
  If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
  If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = Sqrt(mass2(i1)) 
  Else 
   If (ErrorLevel.Ge.0) Then 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
   Call TerminateProgram 
   End If 
   kont = -301 
   mass(i1) = 0._dp 
  End If 
End Do 
 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
Do i1=1,2
PiSf(i1,:,:) = ZeroC 
p2 =  mass2(i1) 
Call Pi1Loophh(p2,MAh,MAh2,MVZ,MVZ2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,            & 
& Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MSsc,MSsc2,cplAhAhUhh,cplAhUhhVZ,cplcFdFdUhhL,            & 
& cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcFxvFxvUhhL,       & 
& cplcFxvFxvUhhR,cplcgWpgWpUhh,cplcgWCgWCUhh,cplcgZgZUhh,cplUhhhhhh,cplUhhHpcHp,         & 
& cplUhhHpcVWp,cplUhhSsccSsc,cplUhhcVWpVWp,cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,       & 
& cplUhhUhhHpcHp,cplUhhUhhSsccSsc,cplUhhUhhcVWpVWp,cplUhhUhhVZVZ,kont,PiSf(i1,:,:))

End Do 
Do i1=2,1,-1 
PiSf(i1,:,:) = PiSf(i1,:,:) - Pi2S_EffPot 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
ZHOS_p2(i1,:) = RS(i1,:) 
 If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
End If 
mass2(i1) = mi2(i1) 
End do 
Do i1=1,2
 If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
 If (test_m2(i1).Ne.0._dp) Then 
    test_m2(i1) = Abs(test_m2(i1) - mass2(i1)) / test_m2(i1) 
 Else 
    test_m2(i1) = Abs(mass2(i1)) 
 End If 
 If (Abs(mass2(i1)).lt.1.0E-30_dp) test_m2(i1) = 0._dp 
 If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = sqrt(mass2(i1)) 
  Else 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses occurred a negative mass squared!' 
     Write(*,*) 'generation: ',i1 
     Write(*,*) 'mass: ',mass2(i1) 
   SignOfMassChanged = .True. 
   mass(i1) = 0._dp 
  End If 
End Do 
 
If (Maxval(test_m2).LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoophh
 
 
Subroutine Pi1Loophh(p2,MAh,MAh2,MVZ,MVZ2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,            & 
& MFxv2,Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MSsc,MSsc2,cplAhAhUhh,cplAhUhhVZ,cplcFdFdUhhL,      & 
& cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplcFxvFxvUhhL,       & 
& cplcFxvFxvUhhR,cplcgWpgWpUhh,cplcgWCgWCUhh,cplcgZgZUhh,cplUhhhhhh,cplUhhHpcHp,         & 
& cplUhhHpcVWp,cplUhhSsccSsc,cplUhhcVWpVWp,cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,       & 
& cplUhhUhhHpcHp,cplUhhUhhSsccSsc,cplUhhUhhcVWpVWp,cplUhhUhhVZVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MAh,MAh2,MVZ,MVZ2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFxv(2),               & 
& MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MVWp,MVWp2,MSsc(2),MSsc2(2)

Complex(dp), Intent(in) :: cplAhAhUhh(2),cplAhUhhVZ(2),cplcFdFdUhhL(3,3,2),cplcFdFdUhhR(3,3,2),cplcFeFeUhhL(3,3,2),& 
& cplcFeFeUhhR(3,3,2),cplcFuFuUhhL(3,3,2),cplcFuFuUhhR(3,3,2),cplcFxvFxvUhhL(2,2,2),     & 
& cplcFxvFxvUhhR(2,2,2),cplcgWpgWpUhh(2),cplcgWCgWCUhh(2),cplcgZgZUhh(2),cplUhhhhhh(2,2,2),& 
& cplUhhHpcHp(2),cplUhhHpcVWp(2),cplUhhSsccSsc(2,2,2),cplUhhcVWpVWp(2),cplUhhVZVZ(2),    & 
& cplAhAhUhhUhh(2,2),cplUhhUhhhhhh(2,2,2,2),cplUhhUhhHpcHp(2,2),cplUhhUhhSsccSsc(2,2,2,2),& 
& cplUhhUhhcVWpVWp(2,2),cplUhhUhhVZVZ(2,2)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(2,2) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumI(2,2) 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
B0m2 = Real(SA_B0(p2,MAh2,MAh2),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplAhAhUhh(gO1)
coup2 = Conjg(cplAhAhUhh(gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
End If 
 !------------------------ 
! VZ, Ah 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVZ).and.(Include_in_loopAh)) Then 
F0m2 = FloopRXi(p2,MAh2,MVZ2) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplAhUhhVZ(gO1)
coup2 =  Conjg(cplAhUhhVZ(gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
End If 
 !------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFd).and.(Include_in_loopFd)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Real(SA_Gloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = -2._dp*MFd(i1)*MFd(i2)*Real(SA_B0(p2,MFd2(i1),MFd2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coupL1 = cplcFdFdUhhL(i1,i2,gO1)
coupR1 = cplcFdFdUhhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFdFdUhhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFdFdUhhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End If 
 !------------------------ 
! bar[Fe], Fe 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFe).and.(Include_in_loopFe)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Real(SA_Gloop(p2,MFe2(i1),MFe2(i2)),dp) 
B0m2 = -2._dp*MFe(i1)*MFe(i2)*Real(SA_B0(p2,MFe2(i1),MFe2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coupL1 = cplcFeFeUhhL(i1,i2,gO1)
coupR1 = cplcFeFeUhhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFeFeUhhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFeFeUhhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End If 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFu).and.(Include_in_loopFu)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Real(SA_Gloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = -2._dp*MFu(i1)*MFu(i2)*Real(SA_B0(p2,MFu2(i1),MFu2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coupL1 = cplcFuFuUhhL(i1,i2,gO1)
coupR1 = cplcFuFuUhhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFuFuUhhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFuFuUhhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End If 
 !------------------------ 
! bar[Fxv], Fxv 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 2
 G0m2 = Real(SA_Gloop(p2,MFxv2(i1),MFxv2(i2)),dp) 
B0m2 = -2._dp*MFxv(i1)*MFxv(i2)*Real(SA_B0(p2,MFxv2(i1),MFxv2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coupL1 = cplcFxvFxvUhhL(i1,i2,gO1)
coupR1 = cplcFxvFxvUhhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFxvFxvUhhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFxvFxvUhhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End If 
 !------------------------ 
! bar[gWp], gWp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
F0m2 = -Real(SA_B0(p2,MVWp2*RXi,MVWp2*RXi),dp) 
 Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplcgWpgWpUhh(gO1)
coup2 =  cplcgWpgWpUhh(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
End If 
 !------------------------ 
! bar[gWpC], gWpC 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
F0m2 = -Real(SA_B0(p2,MVWp2*RXi,MVWp2*RXi),dp) 
 Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplcgWCgWCUhh(gO1)
coup2 =  cplcgWCgWCUhh(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
End If 
 !------------------------ 
! bar[gZ], gZ 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopgZ).and.(Include_in_loopgZ)) Then 
F0m2 = -Real(SA_B0(p2,MVZ2*RXi,MVZ2*RXi),dp) 
 Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplcgZgZUhh(gO1)
coup2 =  cplcgZgZUhh(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
End If 
 !------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 2
 B0m2 = Real(SA_B0(p2,Mhh2(i1),Mhh2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhhhhh(gO1,i1,i2)
coup2 = Conjg(cplUhhhhhh(gO2,i1,i2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
     End Do 
 End If 
 !------------------------ 
! conj[Hp], Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
B0m2 = Real(SA_B0(p2,MHp2,MHp2),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhHpcHp(gO1)
coup2 = Conjg(cplUhhHpcHp(gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
End If 
 !------------------------ 
! conj[VWp], Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
F0m2 = FloopRXi(p2,MHp2,MVWp2) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhHpcVWp(gO1)
coup2 =  Conjg(cplUhhHpcVWp(gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +2._dp* SumI  
End If 
 !------------------------ 
! conj[Ssc], Ssc 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 2
 B0m2 = Real(SA_B0(p2,MSsc2(i1),MSsc2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhSsccSsc(gO1,i2,i1)
coup2 = Conjg(cplUhhSsccSsc(gO2,i2,i1))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End If 
 !------------------------ 
! conj[VWp], VWp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
F0m2 = Real(SVVloop(p2,MVWp2,MVWp2),dp)   
 Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhcVWpVWp(gO1)
coup2 =  Conjg(cplUhhcVWpVWp(gO2)) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
End If 
 !------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
F0m2 = Real(SVVloop(p2,MVZ2,MVZ2),dp)   
 Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhVZVZ(gO1)
coup2 =  Conjg(cplUhhVZVZ(gO2)) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
End If 
 !------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
A0m2 = SA_A0(MAh2) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplAhAhUhhUhh(gO1,gO2)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
End If 
 !------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
    Do i1 = 1, 2
 A0m2 = SA_A0(Mhh2(i1)) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhUhhhhhh(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 End If 
 !------------------------ 
! conj[Hp], Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
A0m2 = SA_A0(MHp2) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhUhhHpcHp(gO1,gO2)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
End If 
 !------------------------ 
! conj[Ssc], Ssc 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
    Do i1 = 1, 2
 A0m2 = SA_A0(MSsc2(i1)) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhUhhSsccSsc(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 End If 
 !------------------------ 
! conj[VWp], VWp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
A0m2 = 0.75_dp*SA_A0(MVWp2) + 0.25_dp*RXi*SA_A0(MVWp2*RXi) - 0.5_dp*MVWp2*rMS 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhUhhcVWpVWp(gO1,gO2)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +4._dp* SumI  
End If 
 !------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
A0m2 = 0.75_dp*SA_A0(MVZ2) + 0.25_dp*RXi*SA_A0(MVZ2*RXi) - 0.5_dp*MVZ2*rMS 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhUhhVZVZ(gO1,gO2)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +2._dp* SumI  
End If 
 

Do gO2 = 1, 2
  Do gO1 = gO2+1, 2
     res(gO1,gO2) = (res(gO2,gO1))  
   End Do 
End Do 
 
res = oo16pi2*res 
 
End Subroutine Pi1Loophh 
 
Subroutine DerPi1Loophh(p2,MAh,MAh2,MVZ,MVZ2,MFd,MFd2,MFe,MFe2,MFu,MFu2,              & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MSsc,MSsc2,cplAhAhUhh,cplAhUhhVZ,              & 
& cplcFdFdUhhL,cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,cplcFuFuUhhL,cplcFuFuUhhR,         & 
& cplcFxvFxvUhhL,cplcFxvFxvUhhR,cplcgWpgWpUhh,cplcgWCgWCUhh,cplcgZgZUhh,cplUhhhhhh,      & 
& cplUhhHpcHp,cplUhhHpcVWp,cplUhhSsccSsc,cplUhhcVWpVWp,cplUhhVZVZ,cplAhAhUhhUhh,         & 
& cplUhhUhhhhhh,cplUhhUhhHpcHp,cplUhhUhhSsccSsc,cplUhhUhhcVWpVWp,cplUhhUhhVZVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MAh,MAh2,MVZ,MVZ2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFxv(2),               & 
& MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MVWp,MVWp2,MSsc(2),MSsc2(2)

Complex(dp), Intent(in) :: cplAhAhUhh(2),cplAhUhhVZ(2),cplcFdFdUhhL(3,3,2),cplcFdFdUhhR(3,3,2),cplcFeFeUhhL(3,3,2),& 
& cplcFeFeUhhR(3,3,2),cplcFuFuUhhL(3,3,2),cplcFuFuUhhR(3,3,2),cplcFxvFxvUhhL(2,2,2),     & 
& cplcFxvFxvUhhR(2,2,2),cplcgWpgWpUhh(2),cplcgWCgWCUhh(2),cplcgZgZUhh(2),cplUhhhhhh(2,2,2),& 
& cplUhhHpcHp(2),cplUhhHpcVWp(2),cplUhhSsccSsc(2,2,2),cplUhhcVWpVWp(2),cplUhhVZVZ(2),    & 
& cplAhAhUhhUhh(2,2),cplUhhUhhhhhh(2,2,2,2),cplUhhUhhHpcHp(2,2),cplUhhUhhSsccSsc(2,2,2,2),& 
& cplUhhUhhcVWpVWp(2,2),cplUhhUhhVZVZ(2,2)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(2,2) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumI(2,2) 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
B0m2 = Real(SA_DerB0(p2,MAh2,MAh2),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplAhAhUhh(gO1)
coup2 = Conjg(cplAhAhUhh(gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
End If 
 !------------------------ 
! VZ, Ah 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVZ).and.(Include_in_loopAh)) Then 
F0m2 = DerFloopRXi(p2,MAh2,MVZ2) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplAhUhhVZ(gO1)
coup2 =  Conjg(cplAhUhhVZ(gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
End If 
 !------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFd).and.(Include_in_loopFd)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Real(SA_DerGloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = -2._dp*MFd(i1)*MFd(i2)*Real(SA_DerB0(p2,MFd2(i1),MFd2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coupL1 = cplcFdFdUhhL(i1,i2,gO1)
coupR1 = cplcFdFdUhhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFdFdUhhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFdFdUhhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End If 
 !------------------------ 
! bar[Fe], Fe 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFe).and.(Include_in_loopFe)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Real(SA_DerGloop(p2,MFe2(i1),MFe2(i2)),dp) 
B0m2 = -2._dp*MFe(i1)*MFe(i2)*Real(SA_DerB0(p2,MFe2(i1),MFe2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coupL1 = cplcFeFeUhhL(i1,i2,gO1)
coupR1 = cplcFeFeUhhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFeFeUhhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFeFeUhhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End If 
 !------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFu).and.(Include_in_loopFu)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = Real(SA_DerGloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = -2._dp*MFu(i1)*MFu(i2)*Real(SA_DerB0(p2,MFu2(i1),MFu2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coupL1 = cplcFuFuUhhL(i1,i2,gO1)
coupR1 = cplcFuFuUhhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFuFuUhhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFuFuUhhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End If 
 !------------------------ 
! bar[Fxv], Fxv 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 2
 G0m2 = Real(SA_DerGloop(p2,MFxv2(i1),MFxv2(i2)),dp) 
B0m2 = -2._dp*MFxv(i1)*MFxv(i2)*Real(SA_DerB0(p2,MFxv2(i1),MFxv2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coupL1 = cplcFxvFxvUhhL(i1,i2,gO1)
coupR1 = cplcFxvFxvUhhR(i1,i2,gO1)
coupL2 =  Conjg(cplcFxvFxvUhhL(i1,i2,gO2))
coupR2 =  Conjg(cplcFxvFxvUhhR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End If 
 !------------------------ 
! bar[gWp], gWp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
F0m2 = -Real(SA_DerB0(p2,MVWp2*RXi,MVWp2*RXi),dp) 
 Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplcgWpgWpUhh(gO1)
coup2 =  cplcgWpgWpUhh(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
End If 
 !------------------------ 
! bar[gWpC], gWpC 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
F0m2 = -Real(SA_DerB0(p2,MVWp2*RXi,MVWp2*RXi),dp) 
 Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplcgWCgWCUhh(gO1)
coup2 =  cplcgWCgWCUhh(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
End If 
 !------------------------ 
! bar[gZ], gZ 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopgZ).and.(Include_in_loopgZ)) Then 
F0m2 = -Real(SA_DerB0(p2,MVZ2*RXi,MVZ2*RXi),dp) 
 Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplcgZgZUhh(gO1)
coup2 =  cplcgZgZUhh(gO2) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
End If 
 !------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 2
 B0m2 = Real(SA_DerB0(p2,Mhh2(i1),Mhh2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhhhhh(gO1,i1,i2)
coup2 = Conjg(cplUhhhhhh(gO2,i1,i2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
     End Do 
 End If 
 !------------------------ 
! conj[Hp], Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
B0m2 = Real(SA_DerB0(p2,MHp2,MHp2),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhHpcHp(gO1)
coup2 = Conjg(cplUhhHpcHp(gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
End If 
 !------------------------ 
! conj[VWp], Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
F0m2 = DerFloopRXi(p2,MHp2,MVWp2) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhHpcVWp(gO1)
coup2 =  Conjg(cplUhhHpcVWp(gO2))
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +2._dp* SumI  
End If 
 !------------------------ 
! conj[Ssc], Ssc 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 2
 B0m2 = Real(SA_DerB0(p2,MSsc2(i1),MSsc2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhSsccSsc(gO1,i2,i1)
coup2 = Conjg(cplUhhSsccSsc(gO2,i2,i1))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End If 
 !------------------------ 
! conj[VWp], VWp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
F0m2 = Real(DerSVVloop(p2,MVWp2,MVWp2),dp)   
 Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhcVWpVWp(gO1)
coup2 =  Conjg(cplUhhcVWpVWp(gO2)) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
End If 
 !------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
F0m2 = Real(DerSVVloop(p2,MVZ2,MVZ2),dp)   
 Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhVZVZ(gO1)
coup2 =  Conjg(cplUhhVZVZ(gO2)) 
    SumI(gO1,gO2) = coup1*coup2*F0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
End If 
 !------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
A0m2 = SA_DerA0(MAh2) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplAhAhUhhUhh(gO1,gO2)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
End If 
 !------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
    Do i1 = 1, 2
 A0m2 = SA_DerA0(Mhh2(i1)) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhUhhhhhh(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 End If 
 !------------------------ 
! conj[Hp], Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
A0m2 = SA_DerA0(MHp2) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhUhhHpcHp(gO1,gO2)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
End If 
 !------------------------ 
! conj[Ssc], Ssc 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
    Do i1 = 1, 2
 A0m2 = SA_DerA0(MSsc2(i1)) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhUhhSsccSsc(gO1,gO2,i1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 End If 
 !------------------------ 
! conj[VWp], VWp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
A0m2 = 0.75_dp*SA_DerA0(MVWp2) + 0.25_dp*RXi*SA_DerA0(MVWp2*RXi) - 0.5_dp*MVWp2*DerrMS 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhUhhcVWpVWp(gO1,gO2)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +4._dp* SumI  
End If 
 !------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
A0m2 = 0.75_dp*SA_DerA0(MVZ2) + 0.25_dp*RXi*SA_DerA0(MVZ2*RXi) - 0.5_dp*MVZ2*DerrMS 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUhhUhhVZVZ(gO1,gO2)
    SumI(gO1,gO2) = coup1*A0m2 
   End Do 
End Do 
res = res +2._dp* SumI  
End If 
 

Do gO2 = 1, 2
  Do gO1 = gO2+1, 2
     res(gO1,gO2) = (res(gO2,gO1))  
   End Do 
End Do 
 
res = oo16pi2*res 
 
End Subroutine DerPi1Loophh 
 
Subroutine OneLoopFd(Yd,vvSM,MFd,MFd2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,            & 
& MFu,MFu2,MVWp,MVWp2,cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdFdhhL,cplcUFdFdhhR,               & 
& cplcUFdFdVGL,cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,         & 
& cplcUFdFucHpL,cplcUFdFucHpR,cplcUFdFucVWpL,cplcUFdFucVWpR,delta,MFd_1L,MFd2_1L,        & 
& ZDL_1L,ZDR_1L,ierr)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MAh,MAh2,Mhh(2),Mhh2(2),MVZ,MVZ2,MHp,MHp2,MFu(3),MFu2(3),              & 
& MVWp,MVWp2

Real(dp), Intent(in) :: vvSM

Complex(dp), Intent(in) :: Yd(3,3)

Complex(dp), Intent(in) :: cplcUFdFdAhL(3,3),cplcUFdFdAhR(3,3),cplcUFdFdhhL(3,3,2),cplcUFdFdhhR(3,3,2),          & 
& cplcUFdFdVGL(3,3),cplcUFdFdVGR(3,3),cplcUFdFdVPL(3,3),cplcUFdFdVPR(3,3),               & 
& cplcUFdFdVZL(3,3),cplcUFdFdVZR(3,3),cplcUFdFucHpL(3,3),cplcUFdFucHpR(3,3),             & 
& cplcUFdFucVWpL(3,3),cplcUFdFucVWpR(3,3)

Complex(dp) :: mat1a(3,3), mat1(3,3) 
Integer , Intent(inout):: ierr 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(3), test_m2(3), p2 
Real(dp), Intent(out) :: MFd_1L(3),MFd2_1L(3) 
 Complex(dp), Intent(out) :: ZDL_1L(3,3), ZDR_1L(3,3) 
 
 Real(dp) :: MFd_t(3),MFd2_t(3) 
 Complex(dp) :: ZDL_t(3,3), ZDR_t(3,3), sigL(3,3), sigR(3,3), sigSL(3,3), sigSR(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZDL2(3,3), ZDR2(3,3) 
 
 Real(dp) :: ZDL1(3,3), ZDR1(3,3), test(2) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopMFd'
 
mat1a(1,1) = 0._dp 
mat1a(1,1) = mat1a(1,1)+(vvSM*Yd(1,1))/sqrt(2._dp)
mat1a(1,2) = 0._dp 
mat1a(1,2) = mat1a(1,2)+(vvSM*Yd(2,1))/sqrt(2._dp)
mat1a(1,3) = 0._dp 
mat1a(1,3) = mat1a(1,3)+(vvSM*Yd(3,1))/sqrt(2._dp)
mat1a(2,1) = 0._dp 
mat1a(2,1) = mat1a(2,1)+(vvSM*Yd(1,2))/sqrt(2._dp)
mat1a(2,2) = 0._dp 
mat1a(2,2) = mat1a(2,2)+(vvSM*Yd(2,2))/sqrt(2._dp)
mat1a(2,3) = 0._dp 
mat1a(2,3) = mat1a(2,3)+(vvSM*Yd(3,2))/sqrt(2._dp)
mat1a(3,1) = 0._dp 
mat1a(3,1) = mat1a(3,1)+(vvSM*Yd(1,3))/sqrt(2._dp)
mat1a(3,2) = 0._dp 
mat1a(3,2) = mat1a(3,2)+(vvSM*Yd(2,3))/sqrt(2._dp)
mat1a(3,3) = 0._dp 
mat1a(3,3) = mat1a(3,3)+(vvSM*Yd(3,3))/sqrt(2._dp)

 
 !---------------------------------------- 
! Rotation matrix for p2=0 
!----------------------------------------- 
 
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = 0._dp 
Call Sigma1LoopFd(p2,MFd,MFd2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,MFu,MFu2,           & 
& MVWp,MVWp2,cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdFdhhL,cplcUFdFdhhR,cplcUFdFdVGL,           & 
& cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,cplcUFdFucHpL,        & 
& cplcUFdFucHpR,cplcUFdFucVWpL,cplcUFdFucVWpR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFd2_t,ZDR1,ierr,test) 
ZDR2 = ZDR1 
Else 
Call EigenSystem(mat2,MFd2_t,ZDR2,ierr,test) 
 End If 
 
ZDROS_0 = ZDR2 
 mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFd2_t,ZDL1,ierr,test) 
 
 
ZDL2 = ZDL1 
Else 
Call EigenSystem(mat2,MFd2_t,ZDL2,ierr,test) 
 
 
End If 
ZDL2 = Conjg(ZDL2) 
ZDLOS_0 = ZDL2 
 
!---------------------------------------- 
! Now, with momenta
!----------------------------------------- 
 
Do il=3,1,-1
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = MFd2(il) 
Call Sigma1LoopFd(p2,MFd,MFd2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,MFu,MFu2,           & 
& MVWp,MVWp2,cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdFdhhL,cplcUFdFdhhR,cplcUFdFdVGL,           & 
& cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,cplcUFdFucHpL,        & 
& cplcUFdFucHpR,cplcUFdFucVWpL,cplcUFdFucVWpR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFd2_t,ZDR1,ierr,test) 
ZDR2 = ZDR1 
Else 
Call EigenSystem(mat2,MFd2_t,ZDR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
!---------------------------------------- 
! Redoing Calculation using redefined p2 
!----------------------------------------- 
 
i_count = 0 
p2_loop: Do  
i_count = i_count + 1 
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = MFd2_t(iL)
Call Sigma1LoopFd(p2,MFd,MFd2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,MFu,MFu2,           & 
& MVWp,MVWp2,cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdFdhhL,cplcUFdFdhhR,cplcUFdFdVGL,           & 
& cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,cplcUFdFucHpL,        & 
& cplcUFdFucHpR,cplcUFdFucVWpL,cplcUFdFucVWpR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFd2_t,ZDR1,ierr,test) 
ZDR2 = ZDR1 
Else 
Call EigenSystem(mat2,MFd2_t,ZDR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
MFd2_1L(il) = MFd2_t(il) 
MFd_1L(il) = Sqrt(MFd2_1L(il)) 
 
If (p2.Ne.0._dp) Then 
  test(1) = Abs(MFd2_1L(il)-p2)/p2
Else 
  test(2) = Abs(MFd2_1L(il))
End If 
If (Abs(MFd2_1L(il)).lt.1.0E-30_dp) Exit p2_loop 
If (test(1).lt.0.1_dp*delta) Exit p2_loop 
If(i_count.gt.30) then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Exit p2_loop 
End if
End Do p2_loop 
mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFd2_t,ZDL1,ierr,test) 
 
 
ZDL2 = ZDL1 
Else 
Call EigenSystem(mat2,MFd2_t,ZDL2,ierr,test) 
 
 
End If 
ZDL2 = Conjg(ZDL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZDL2),mat1),Transpose( Conjg(ZDR2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZDR2(i1,:) = phaseM *ZDR2(i1,:) 
 End if 
End Do 
 
ZDLOS_p2(il,:) = ZDL2(il,:) 
 ZDROS_p2(il,:) = ZDR2(il,:) 
 ZDL_1L = ZDL2 
 ZDR_1L = ZDR2 
 End Do  
 
Iname = Iname -1 
End Subroutine OneLoopFd
 
 
Subroutine Sigma1LoopFd(p2,MFd,MFd2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,              & 
& MFu,MFu2,MVWp,MVWp2,cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdFdhhL,cplcUFdFdhhR,               & 
& cplcUFdFdVGL,cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,         & 
& cplcUFdFucHpL,cplcUFdFucHpR,cplcUFdFucVWpL,cplcUFdFucVWpR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MAh,MAh2,Mhh(2),Mhh2(2),MVZ,MVZ2,MHp,MHp2,MFu(3),MFu2(3),              & 
& MVWp,MVWp2

Complex(dp), Intent(in) :: cplcUFdFdAhL(3,3),cplcUFdFdAhR(3,3),cplcUFdFdhhL(3,3,2),cplcUFdFdhhR(3,3,2),          & 
& cplcUFdFdVGL(3,3),cplcUFdFdVGR(3,3),cplcUFdFdVPL(3,3),cplcUFdFdVPR(3,3),               & 
& cplcUFdFdVZL(3,3),cplcUFdFdVZR(3,3),cplcUFdFucHpL(3,3),cplcUFdFucHpR(3,3),             & 
& cplcUFdFucVWpL(3,3),cplcUFdFucVWpR(3,3)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigSL(3,3), SigSR(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumSL(3,3), sumSR(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fd, Ah 
!------------------------ 
If ((Include_in_loopFd).and.(Include_in_loopAh)) Then 
    Do i1 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFd2(i1),MAh2),dp) 
B0m2 = MFd(i1)*Real(SA_B0(p2,MFd2(i1),MAh2),dp) 
coupL1 = cplcUFdFdAhL(gO1,i1)
coupR1 = cplcUFdFdAhR(gO1,i1)
coupL2 =  Conjg(cplcUFdFdAhL(gO2,i1))
coupR2 =  Conjg(cplcUFdFdAhR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 End if 
!------------------------ 
! hh, Fd 
!------------------------ 
If ((Include_in_loophh).and.(Include_in_loopFd)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFd2(i2),Mhh2(i1)),dp) 
B0m2 = MFd(i2)*Real(SA_B0(p2,MFd2(i2),Mhh2(i1)),dp) 
coupL1 = cplcUFdFdhhL(gO1,i2,i1)
coupR1 = cplcUFdFdhhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFdFdhhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFdFdhhR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 End if 
!------------------------ 
! VG, Fd 
!------------------------ 
If ((Include_in_loopVG).and.(Include_in_loopFd)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFd2(i2),0._dp)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_B0(p2,MFd2(i2),0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcUFdFdVGL(gO1,i2)
coupR1 = cplcUFdFdVGR(gO1,i2)
coupL2 =  Conjg(cplcUFdFdVGL(gO2,i2))
coupR2 =  Conjg(cplcUFdFdVGR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +4._dp/3._dp* sumL
SigR = SigR +4._dp/3._dp* sumR 
SigSL = SigSL +4._dp/3._dp* sumSL 
SigSR = SigSR +4._dp/3._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! VP, Fd 
!------------------------ 
If ((Include_in_loopVP).and.(Include_in_loopFd)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFd2(i2),0._dp)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_B0(p2,MFd2(i2),0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcUFdFdVPL(gO1,i2)
coupR1 = cplcUFdFdVPR(gO1,i2)
coupL2 =  Conjg(cplcUFdFdVPL(gO2,i2))
coupR2 =  Conjg(cplcUFdFdVPR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! VZ, Fd 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopFd)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFd2(i2),MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_B0(p2,MFd2(i2),MVZ2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFdFdVZL(gO1,i2)
coupR1 = cplcUFdFdVZR(gO1,i2)
coupL2 =  Conjg(cplcUFdFdVZL(gO2,i2))
coupR2 =  Conjg(cplcUFdFdVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! conj[Hp], Fu 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopFu)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFu2(i2),MHp2),dp) 
B0m2 = MFu(i2)*Real(SA_B0(p2,MFu2(i2),MHp2),dp) 
coupL1 = cplcUFdFucHpL(gO1,i2)
coupR1 = cplcUFdFucHpR(gO1,i2)
coupL2 =  Conjg(cplcUFdFucHpL(gO2,i2))
coupR2 =  Conjg(cplcUFdFucHpR(gO2,i2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! conj[VWp], Fu 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopFu)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFu2(i2),MVWp2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_B0(p2,MFu2(i2),MVWp2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFdFucVWpL(gO1,i2)
coupR1 = cplcUFdFucVWpR(gO1,i2)
coupL2 =  Conjg(cplcUFdFucVWpL(gO2,i2))
coupR2 =  Conjg(cplcUFdFucVWpR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine Sigma1LoopFd 
 
Subroutine DerSigma1LoopFd(p2,MFd,MFd2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,           & 
& MFu,MFu2,MVWp,MVWp2,cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdFdhhL,cplcUFdFdhhR,               & 
& cplcUFdFdVGL,cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,         & 
& cplcUFdFucHpL,cplcUFdFucHpR,cplcUFdFucVWpL,cplcUFdFucVWpR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MAh,MAh2,Mhh(2),Mhh2(2),MVZ,MVZ2,MHp,MHp2,MFu(3),MFu2(3),              & 
& MVWp,MVWp2

Complex(dp), Intent(in) :: cplcUFdFdAhL(3,3),cplcUFdFdAhR(3,3),cplcUFdFdhhL(3,3,2),cplcUFdFdhhR(3,3,2),          & 
& cplcUFdFdVGL(3,3),cplcUFdFdVGR(3,3),cplcUFdFdVPL(3,3),cplcUFdFdVPR(3,3),               & 
& cplcUFdFdVZL(3,3),cplcUFdFdVZR(3,3),cplcUFdFucHpL(3,3),cplcUFdFucHpR(3,3),             & 
& cplcUFdFucVWpL(3,3),cplcUFdFucVWpR(3,3)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigSL(3,3), SigSR(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumSL(3,3), sumSR(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fd, Ah 
!------------------------ 
If ((Include_in_loopFd).and.(Include_in_loopAh)) Then 
    Do i1 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_DerB1(p2,MFd2(i1),MAh2),dp) 
B0m2 = MFd(i1)*Real(SA_DerB0(p2,MFd2(i1),MAh2),dp) 
coupL1 = cplcUFdFdAhL(gO1,i1)
coupR1 = cplcUFdFdAhR(gO1,i1)
coupL2 =  Conjg(cplcUFdFdAhL(gO2,i1))
coupR2 =  Conjg(cplcUFdFdAhR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 End if 
!------------------------ 
! hh, Fd 
!------------------------ 
If ((Include_in_loophh).and.(Include_in_loopFd)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_DerB1(p2,MFd2(i2),Mhh2(i1)),dp) 
B0m2 = MFd(i2)*Real(SA_DerB0(p2,MFd2(i2),Mhh2(i1)),dp) 
coupL1 = cplcUFdFdhhL(gO1,i2,i1)
coupR1 = cplcUFdFdhhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFdFdhhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFdFdhhR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 End if 
!------------------------ 
! VG, Fd 
!------------------------ 
If ((Include_in_loopVG).and.(Include_in_loopFd)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_DerB1(p2,MFd2(i2),MVG2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_DerB0(p2,MFd2(i2),MVG2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcUFdFdVGL(gO1,i2)
coupR1 = cplcUFdFdVGR(gO1,i2)
coupL2 =  Conjg(cplcUFdFdVGL(gO2,i2))
coupR2 =  Conjg(cplcUFdFdVGR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +4._dp/3._dp* sumL
SigR = SigR +4._dp/3._dp* sumR 
SigSL = SigSL +4._dp/3._dp* sumSL 
SigSR = SigSR +4._dp/3._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! VP, Fd 
!------------------------ 
If ((Include_in_loopVP).and.(Include_in_loopFd)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_DerB1(p2,MFd2(i2),MVP2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_DerB0(p2,MFd2(i2),MVP2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcUFdFdVPL(gO1,i2)
coupR1 = cplcUFdFdVPR(gO1,i2)
coupL2 =  Conjg(cplcUFdFdVPL(gO2,i2))
coupR2 =  Conjg(cplcUFdFdVPR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! VZ, Fd 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopFd)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_DerB1(p2,MFd2(i2),MVZ2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_DerB0(p2,MFd2(i2),MVZ2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcUFdFdVZL(gO1,i2)
coupR1 = cplcUFdFdVZR(gO1,i2)
coupL2 =  Conjg(cplcUFdFdVZL(gO2,i2))
coupR2 =  Conjg(cplcUFdFdVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! conj[Hp], Fu 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopFu)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_DerB1(p2,MFu2(i2),MHp2),dp) 
B0m2 = MFu(i2)*Real(SA_DerB0(p2,MFu2(i2),MHp2),dp) 
coupL1 = cplcUFdFucHpL(gO1,i2)
coupR1 = cplcUFdFucHpR(gO1,i2)
coupL2 =  Conjg(cplcUFdFucHpL(gO2,i2))
coupR2 =  Conjg(cplcUFdFucHpR(gO2,i2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! conj[VWp], Fu 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopFu)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_DerB1(p2,MFu2(i2),MVWp2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_DerB0(p2,MFu2(i2),MVWp2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcUFdFucVWpL(gO1,i2)
coupR1 = cplcUFdFucVWpR(gO1,i2)
coupL2 =  Conjg(cplcUFdFucVWpL(gO2,i2))
coupR2 =  Conjg(cplcUFdFucVWpR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine DerSigma1LoopFd 
 
Subroutine OneLoopFu(Yu,vvSM,MFu,MFu2,MAh,MAh2,MHp,MHp2,MFd,MFd2,MVWp,MVWp2,          & 
& Mhh,Mhh2,MVZ,MVZ2,cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuFdHpL,cplcUFuFdHpR,cplcUFuFdVWpL,   & 
& cplcUFuFdVWpR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,        & 
& cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,delta,MFu_1L,MFu2_1L,ZUL_1L,ZUR_1L,ierr)

Implicit None 
Real(dp), Intent(in) :: MFu(3),MFu2(3),MAh,MAh2,MHp,MHp2,MFd(3),MFd2(3),MVWp,MVWp2,Mhh(2),Mhh2(2),MVZ,MVZ2

Real(dp), Intent(in) :: vvSM

Complex(dp), Intent(in) :: Yu(3,3)

Complex(dp), Intent(in) :: cplcUFuFuAhL(3,3),cplcUFuFuAhR(3,3),cplcUFuFdHpL(3,3),cplcUFuFdHpR(3,3),              & 
& cplcUFuFdVWpL(3,3),cplcUFuFdVWpR(3,3),cplcUFuFuhhL(3,3,2),cplcUFuFuhhR(3,3,2),         & 
& cplcUFuFuVGL(3,3),cplcUFuFuVGR(3,3),cplcUFuFuVPL(3,3),cplcUFuFuVPR(3,3),               & 
& cplcUFuFuVZL(3,3),cplcUFuFuVZR(3,3)

Complex(dp) :: mat1a(3,3), mat1(3,3) 
Integer , Intent(inout):: ierr 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(3), test_m2(3), p2 
Real(dp), Intent(out) :: MFu_1L(3),MFu2_1L(3) 
 Complex(dp), Intent(out) :: ZUL_1L(3,3), ZUR_1L(3,3) 
 
 Real(dp) :: MFu_t(3),MFu2_t(3) 
 Complex(dp) :: ZUL_t(3,3), ZUR_t(3,3), sigL(3,3), sigR(3,3), sigSL(3,3), sigSR(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZUL2(3,3), ZUR2(3,3) 
 
 Real(dp) :: ZUL1(3,3), ZUR1(3,3), test(2) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopMFu'
 
mat1a(1,1) = 0._dp 
mat1a(1,1) = mat1a(1,1)+(vvSM*Yu(1,1))/sqrt(2._dp)
mat1a(1,2) = 0._dp 
mat1a(1,2) = mat1a(1,2)+(vvSM*Yu(2,1))/sqrt(2._dp)
mat1a(1,3) = 0._dp 
mat1a(1,3) = mat1a(1,3)+(vvSM*Yu(3,1))/sqrt(2._dp)
mat1a(2,1) = 0._dp 
mat1a(2,1) = mat1a(2,1)+(vvSM*Yu(1,2))/sqrt(2._dp)
mat1a(2,2) = 0._dp 
mat1a(2,2) = mat1a(2,2)+(vvSM*Yu(2,2))/sqrt(2._dp)
mat1a(2,3) = 0._dp 
mat1a(2,3) = mat1a(2,3)+(vvSM*Yu(3,2))/sqrt(2._dp)
mat1a(3,1) = 0._dp 
mat1a(3,1) = mat1a(3,1)+(vvSM*Yu(1,3))/sqrt(2._dp)
mat1a(3,2) = 0._dp 
mat1a(3,2) = mat1a(3,2)+(vvSM*Yu(2,3))/sqrt(2._dp)
mat1a(3,3) = 0._dp 
mat1a(3,3) = mat1a(3,3)+(vvSM*Yu(3,3))/sqrt(2._dp)

 
 !---------------------------------------- 
! Rotation matrix for p2=0 
!----------------------------------------- 
 
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = 0._dp 
Call Sigma1LoopFu(p2,MFu,MFu2,MAh,MAh2,MHp,MHp2,MFd,MFd2,MVWp,MVWp2,Mhh,              & 
& Mhh2,MVZ,MVZ2,cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuFdHpL,cplcUFuFdHpR,cplcUFuFdVWpL,       & 
& cplcUFuFdVWpR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,        & 
& cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFu2_t,ZUR1,ierr,test) 
ZUR2 = ZUR1 
Else 
Call EigenSystem(mat2,MFu2_t,ZUR2,ierr,test) 
 End If 
 
ZUROS_0 = ZUR2 
 mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFu2_t,ZUL1,ierr,test) 
 
 
ZUL2 = ZUL1 
Else 
Call EigenSystem(mat2,MFu2_t,ZUL2,ierr,test) 
 
 
End If 
ZUL2 = Conjg(ZUL2) 
ZULOS_0 = ZUL2 
 
!---------------------------------------- 
! Now, with momenta
!----------------------------------------- 
 
Do il=3,1,-1
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = MFu2(il) 
Call Sigma1LoopFu(p2,MFu,MFu2,MAh,MAh2,MHp,MHp2,MFd,MFd2,MVWp,MVWp2,Mhh,              & 
& Mhh2,MVZ,MVZ2,cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuFdHpL,cplcUFuFdHpR,cplcUFuFdVWpL,       & 
& cplcUFuFdVWpR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,        & 
& cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFu2_t,ZUR1,ierr,test) 
ZUR2 = ZUR1 
Else 
Call EigenSystem(mat2,MFu2_t,ZUR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
!---------------------------------------- 
! Redoing Calculation using redefined p2 
!----------------------------------------- 
 
i_count = 0 
p2_loop: Do  
i_count = i_count + 1 
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = MFu2_t(iL)
Call Sigma1LoopFu(p2,MFu,MFu2,MAh,MAh2,MHp,MHp2,MFd,MFd2,MVWp,MVWp2,Mhh,              & 
& Mhh2,MVZ,MVZ2,cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuFdHpL,cplcUFuFdHpR,cplcUFuFdVWpL,       & 
& cplcUFuFdVWpR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,        & 
& cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFu2_t,ZUR1,ierr,test) 
ZUR2 = ZUR1 
Else 
Call EigenSystem(mat2,MFu2_t,ZUR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
MFu2_1L(il) = MFu2_t(il) 
MFu_1L(il) = Sqrt(MFu2_1L(il)) 
 
If (p2.Ne.0._dp) Then 
  test(1) = Abs(MFu2_1L(il)-p2)/p2
Else 
  test(2) = Abs(MFu2_1L(il))
End If 
If (Abs(MFu2_1L(il)).lt.1.0E-30_dp) Exit p2_loop 
If (test(1).lt.0.1_dp*delta) Exit p2_loop 
If(i_count.gt.30) then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Exit p2_loop 
End if
End Do p2_loop 
mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFu2_t,ZUL1,ierr,test) 
 
 
ZUL2 = ZUL1 
Else 
Call EigenSystem(mat2,MFu2_t,ZUL2,ierr,test) 
 
 
End If 
ZUL2 = Conjg(ZUL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZUL2),mat1),Transpose( Conjg(ZUR2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZUR2(i1,:) = phaseM *ZUR2(i1,:) 
 End if 
End Do 
 
ZULOS_p2(il,:) = ZUL2(il,:) 
 ZUROS_p2(il,:) = ZUR2(il,:) 
 ZUL_1L = ZUL2 
 ZUR_1L = ZUR2 
 End Do  
 
Iname = Iname -1 
End Subroutine OneLoopFu
 
 
Subroutine Sigma1LoopFu(p2,MFu,MFu2,MAh,MAh2,MHp,MHp2,MFd,MFd2,MVWp,MVWp2,            & 
& Mhh,Mhh2,MVZ,MVZ2,cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuFdHpL,cplcUFuFdHpR,cplcUFuFdVWpL,   & 
& cplcUFuFdVWpR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,        & 
& cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MFu(3),MFu2(3),MAh,MAh2,MHp,MHp2,MFd(3),MFd2(3),MVWp,MVWp2,Mhh(2),Mhh2(2),MVZ,MVZ2

Complex(dp), Intent(in) :: cplcUFuFuAhL(3,3),cplcUFuFuAhR(3,3),cplcUFuFdHpL(3,3),cplcUFuFdHpR(3,3),              & 
& cplcUFuFdVWpL(3,3),cplcUFuFdVWpR(3,3),cplcUFuFuhhL(3,3,2),cplcUFuFuhhR(3,3,2),         & 
& cplcUFuFuVGL(3,3),cplcUFuFuVGR(3,3),cplcUFuFuVPL(3,3),cplcUFuFuVPR(3,3),               & 
& cplcUFuFuVZL(3,3),cplcUFuFuVZR(3,3)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigSL(3,3), SigSR(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumSL(3,3), sumSR(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fu, Ah 
!------------------------ 
If ((Include_in_loopFu).and.(Include_in_loopAh)) Then 
    Do i1 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFu2(i1),MAh2),dp) 
B0m2 = MFu(i1)*Real(SA_B0(p2,MFu2(i1),MAh2),dp) 
coupL1 = cplcUFuFuAhL(gO1,i1)
coupR1 = cplcUFuFuAhR(gO1,i1)
coupL2 =  Conjg(cplcUFuFuAhL(gO2,i1))
coupR2 =  Conjg(cplcUFuFuAhR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 End if 
!------------------------ 
! Hp, Fd 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopFd)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFd2(i2),MHp2),dp) 
B0m2 = MFd(i2)*Real(SA_B0(p2,MFd2(i2),MHp2),dp) 
coupL1 = cplcUFuFdHpL(gO1,i2)
coupR1 = cplcUFuFdHpR(gO1,i2)
coupL2 =  Conjg(cplcUFuFdHpL(gO2,i2))
coupR2 =  Conjg(cplcUFuFdHpR(gO2,i2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! VWp, Fd 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopFd)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFd2(i2),MVWp2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_B0(p2,MFd2(i2),MVWp2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFuFdVWpL(gO1,i2)
coupR1 = cplcUFuFdVWpR(gO1,i2)
coupL2 =  Conjg(cplcUFuFdVWpL(gO2,i2))
coupR2 =  Conjg(cplcUFuFdVWpR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! hh, Fu 
!------------------------ 
If ((Include_in_loophh).and.(Include_in_loopFu)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFu2(i2),Mhh2(i1)),dp) 
B0m2 = MFu(i2)*Real(SA_B0(p2,MFu2(i2),Mhh2(i1)),dp) 
coupL1 = cplcUFuFuhhL(gO1,i2,i1)
coupR1 = cplcUFuFuhhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFuFuhhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFuFuhhR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 End if 
!------------------------ 
! VG, Fu 
!------------------------ 
If ((Include_in_loopVG).and.(Include_in_loopFu)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFu2(i2),0._dp)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_B0(p2,MFu2(i2),0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcUFuFuVGL(gO1,i2)
coupR1 = cplcUFuFuVGR(gO1,i2)
coupL2 =  Conjg(cplcUFuFuVGL(gO2,i2))
coupR2 =  Conjg(cplcUFuFuVGR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +4._dp/3._dp* sumL
SigR = SigR +4._dp/3._dp* sumR 
SigSL = SigSL +4._dp/3._dp* sumSL 
SigSR = SigSR +4._dp/3._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! VP, Fu 
!------------------------ 
If ((Include_in_loopVP).and.(Include_in_loopFu)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFu2(i2),0._dp)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_B0(p2,MFu2(i2),0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcUFuFuVPL(gO1,i2)
coupR1 = cplcUFuFuVPR(gO1,i2)
coupL2 =  Conjg(cplcUFuFuVPL(gO2,i2))
coupR2 =  Conjg(cplcUFuFuVPR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! VZ, Fu 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopFu)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFu2(i2),MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_B0(p2,MFu2(i2),MVZ2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFuFuVZL(gO1,i2)
coupR1 = cplcUFuFuVZR(gO1,i2)
coupL2 =  Conjg(cplcUFuFuVZL(gO2,i2))
coupR2 =  Conjg(cplcUFuFuVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine Sigma1LoopFu 
 
Subroutine DerSigma1LoopFu(p2,MFu,MFu2,MAh,MAh2,MHp,MHp2,MFd,MFd2,MVWp,               & 
& MVWp2,Mhh,Mhh2,MVZ,MVZ2,cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuFdHpL,cplcUFuFdHpR,           & 
& cplcUFuFdVWpL,cplcUFuFdVWpR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,       & 
& cplcUFuFuVPL,cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MFu(3),MFu2(3),MAh,MAh2,MHp,MHp2,MFd(3),MFd2(3),MVWp,MVWp2,Mhh(2),Mhh2(2),MVZ,MVZ2

Complex(dp), Intent(in) :: cplcUFuFuAhL(3,3),cplcUFuFuAhR(3,3),cplcUFuFdHpL(3,3),cplcUFuFdHpR(3,3),              & 
& cplcUFuFdVWpL(3,3),cplcUFuFdVWpR(3,3),cplcUFuFuhhL(3,3,2),cplcUFuFuhhR(3,3,2),         & 
& cplcUFuFuVGL(3,3),cplcUFuFuVGR(3,3),cplcUFuFuVPL(3,3),cplcUFuFuVPR(3,3),               & 
& cplcUFuFuVZL(3,3),cplcUFuFuVZR(3,3)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigSL(3,3), SigSR(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumSL(3,3), sumSR(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fu, Ah 
!------------------------ 
If ((Include_in_loopFu).and.(Include_in_loopAh)) Then 
    Do i1 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_DerB1(p2,MFu2(i1),MAh2),dp) 
B0m2 = MFu(i1)*Real(SA_DerB0(p2,MFu2(i1),MAh2),dp) 
coupL1 = cplcUFuFuAhL(gO1,i1)
coupR1 = cplcUFuFuAhR(gO1,i1)
coupL2 =  Conjg(cplcUFuFuAhL(gO2,i1))
coupR2 =  Conjg(cplcUFuFuAhR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 End if 
!------------------------ 
! Hp, Fd 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopFd)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_DerB1(p2,MFd2(i2),MHp2),dp) 
B0m2 = MFd(i2)*Real(SA_DerB0(p2,MFd2(i2),MHp2),dp) 
coupL1 = cplcUFuFdHpL(gO1,i2)
coupR1 = cplcUFuFdHpR(gO1,i2)
coupL2 =  Conjg(cplcUFuFdHpL(gO2,i2))
coupR2 =  Conjg(cplcUFuFdHpR(gO2,i2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! VWp, Fd 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopFd)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_DerB1(p2,MFd2(i2),MVWp2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_DerB0(p2,MFd2(i2),MVWp2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcUFuFdVWpL(gO1,i2)
coupR1 = cplcUFuFdVWpR(gO1,i2)
coupL2 =  Conjg(cplcUFuFdVWpL(gO2,i2))
coupR2 =  Conjg(cplcUFuFdVWpR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! hh, Fu 
!------------------------ 
If ((Include_in_loophh).and.(Include_in_loopFu)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_DerB1(p2,MFu2(i2),Mhh2(i1)),dp) 
B0m2 = MFu(i2)*Real(SA_DerB0(p2,MFu2(i2),Mhh2(i1)),dp) 
coupL1 = cplcUFuFuhhL(gO1,i2,i1)
coupR1 = cplcUFuFuhhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFuFuhhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFuFuhhR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 End if 
!------------------------ 
! VG, Fu 
!------------------------ 
If ((Include_in_loopVG).and.(Include_in_loopFu)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_DerB1(p2,MFu2(i2),MVG2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_DerB0(p2,MFu2(i2),MVG2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcUFuFuVGL(gO1,i2)
coupR1 = cplcUFuFuVGR(gO1,i2)
coupL2 =  Conjg(cplcUFuFuVGL(gO2,i2))
coupR2 =  Conjg(cplcUFuFuVGR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +4._dp/3._dp* sumL
SigR = SigR +4._dp/3._dp* sumR 
SigSL = SigSL +4._dp/3._dp* sumSL 
SigSR = SigSR +4._dp/3._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! VP, Fu 
!------------------------ 
If ((Include_in_loopVP).and.(Include_in_loopFu)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_DerB1(p2,MFu2(i2),MVP2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_DerB0(p2,MFu2(i2),MVP2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcUFuFuVPL(gO1,i2)
coupR1 = cplcUFuFuVPR(gO1,i2)
coupL2 =  Conjg(cplcUFuFuVPL(gO2,i2))
coupR2 =  Conjg(cplcUFuFuVPR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! VZ, Fu 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopFu)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_DerB1(p2,MFu2(i2),MVZ2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_DerB0(p2,MFu2(i2),MVZ2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcUFuFuVZL(gO1,i2)
coupR1 = cplcUFuFuVZR(gO1,i2)
coupL2 =  Conjg(cplcUFuFuVZL(gO2,i2))
coupR2 =  Conjg(cplcUFuFuVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine DerSigma1LoopFu 
 
Subroutine OneLoopFe(Ye,vvSM,MFe,MFe2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,            & 
& MFv,MFv2,MVWp,MVWp2,MSsc,MSsc2,MFxe,MFxe2,cplcUFeFeAhL,cplcUFeFeAhR,cplcUFeFehhL,      & 
& cplcUFeFehhR,cplcUFeFeVPL,cplcUFeFeVPR,cplcUFeFeVZL,cplcUFeFeVZR,cplcUFeFvcHpL,        & 
& cplcUFeFvcHpR,cplcUFeFvcVWpL,cplcUFeFvcVWpR,cplcUFeFxecSscL,cplcUFeFxecSscR,           & 
& delta,MFe_1L,MFe2_1L,ZEL_1L,ZER_1L,ierr)

Implicit None 
Real(dp), Intent(in) :: MFe(3),MFe2(3),MAh,MAh2,Mhh(2),Mhh2(2),MVZ,MVZ2,MHp,MHp2,MFv(3),MFv2(3),              & 
& MVWp,MVWp2,MSsc(2),MSsc2(2),MFxe,MFxe2

Real(dp), Intent(in) :: vvSM

Complex(dp), Intent(in) :: Ye(3,3)

Complex(dp), Intent(in) :: cplcUFeFeAhL(3,3),cplcUFeFeAhR(3,3),cplcUFeFehhL(3,3,2),cplcUFeFehhR(3,3,2),          & 
& cplcUFeFeVPL(3,3),cplcUFeFeVPR(3,3),cplcUFeFeVZL(3,3),cplcUFeFeVZR(3,3),               & 
& cplcUFeFvcHpL(3,3),cplcUFeFvcHpR(3,3),cplcUFeFvcVWpL(3,3),cplcUFeFvcVWpR(3,3),         & 
& cplcUFeFxecSscL(3,2),cplcUFeFxecSscR(3,2)

Complex(dp) :: mat1a(3,3), mat1(3,3) 
Integer , Intent(inout):: ierr 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(3), test_m2(3), p2 
Real(dp), Intent(out) :: MFe_1L(3),MFe2_1L(3) 
 Complex(dp), Intent(out) :: ZEL_1L(3,3), ZER_1L(3,3) 
 
 Real(dp) :: MFe_t(3),MFe2_t(3) 
 Complex(dp) :: ZEL_t(3,3), ZER_t(3,3), sigL(3,3), sigR(3,3), sigSL(3,3), sigSR(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZEL2(3,3), ZER2(3,3) 
 
 Real(dp) :: ZEL1(3,3), ZER1(3,3), test(2) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopMFe'
 
mat1a(1,1) = 0._dp 
mat1a(1,1) = mat1a(1,1)+(vvSM*Ye(1,1))/sqrt(2._dp)
mat1a(1,2) = 0._dp 
mat1a(1,2) = mat1a(1,2)+(vvSM*Ye(2,1))/sqrt(2._dp)
mat1a(1,3) = 0._dp 
mat1a(1,3) = mat1a(1,3)+(vvSM*Ye(3,1))/sqrt(2._dp)
mat1a(2,1) = 0._dp 
mat1a(2,1) = mat1a(2,1)+(vvSM*Ye(1,2))/sqrt(2._dp)
mat1a(2,2) = 0._dp 
mat1a(2,2) = mat1a(2,2)+(vvSM*Ye(2,2))/sqrt(2._dp)
mat1a(2,3) = 0._dp 
mat1a(2,3) = mat1a(2,3)+(vvSM*Ye(3,2))/sqrt(2._dp)
mat1a(3,1) = 0._dp 
mat1a(3,1) = mat1a(3,1)+(vvSM*Ye(1,3))/sqrt(2._dp)
mat1a(3,2) = 0._dp 
mat1a(3,2) = mat1a(3,2)+(vvSM*Ye(2,3))/sqrt(2._dp)
mat1a(3,3) = 0._dp 
mat1a(3,3) = mat1a(3,3)+(vvSM*Ye(3,3))/sqrt(2._dp)

 
 !---------------------------------------- 
! Rotation matrix for p2=0 
!----------------------------------------- 
 
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = 0._dp 
Call Sigma1LoopFe(p2,MFe,MFe2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,MFv,MFv2,           & 
& MVWp,MVWp2,MSsc,MSsc2,MFxe,MFxe2,cplcUFeFeAhL,cplcUFeFeAhR,cplcUFeFehhL,               & 
& cplcUFeFehhR,cplcUFeFeVPL,cplcUFeFeVPR,cplcUFeFeVZL,cplcUFeFeVZR,cplcUFeFvcHpL,        & 
& cplcUFeFvcHpR,cplcUFeFvcVWpL,cplcUFeFvcVWpR,cplcUFeFxecSscL,cplcUFeFxecSscR,           & 
& sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFe2_t,ZER1,ierr,test) 
ZER2 = ZER1 
Else 
Call EigenSystem(mat2,MFe2_t,ZER2,ierr,test) 
 End If 
 
ZEROS_0 = ZER2 
 mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFe2_t,ZEL1,ierr,test) 
 
 
ZEL2 = ZEL1 
Else 
Call EigenSystem(mat2,MFe2_t,ZEL2,ierr,test) 
 
 
End If 
ZEL2 = Conjg(ZEL2) 
ZELOS_0 = ZEL2 
 
!---------------------------------------- 
! Now, with momenta
!----------------------------------------- 
 
Do il=3,1,-1
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = MFe2(il) 
Call Sigma1LoopFe(p2,MFe,MFe2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,MFv,MFv2,           & 
& MVWp,MVWp2,MSsc,MSsc2,MFxe,MFxe2,cplcUFeFeAhL,cplcUFeFeAhR,cplcUFeFehhL,               & 
& cplcUFeFehhR,cplcUFeFeVPL,cplcUFeFeVPR,cplcUFeFeVZL,cplcUFeFeVZR,cplcUFeFvcHpL,        & 
& cplcUFeFvcHpR,cplcUFeFvcVWpL,cplcUFeFvcVWpR,cplcUFeFxecSscL,cplcUFeFxecSscR,           & 
& sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFe2_t,ZER1,ierr,test) 
ZER2 = ZER1 
Else 
Call EigenSystem(mat2,MFe2_t,ZER2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
!---------------------------------------- 
! Redoing Calculation using redefined p2 
!----------------------------------------- 
 
i_count = 0 
p2_loop: Do  
i_count = i_count + 1 
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = MFe2_t(iL)
Call Sigma1LoopFe(p2,MFe,MFe2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,MFv,MFv2,           & 
& MVWp,MVWp2,MSsc,MSsc2,MFxe,MFxe2,cplcUFeFeAhL,cplcUFeFeAhR,cplcUFeFehhL,               & 
& cplcUFeFehhR,cplcUFeFeVPL,cplcUFeFeVPR,cplcUFeFeVZL,cplcUFeFeVZR,cplcUFeFvcHpL,        & 
& cplcUFeFvcHpR,cplcUFeFvcVWpL,cplcUFeFvcVWpR,cplcUFeFxecSscL,cplcUFeFxecSscR,           & 
& sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFe2_t,ZER1,ierr,test) 
ZER2 = ZER1 
Else 
Call EigenSystem(mat2,MFe2_t,ZER2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
MFe2_1L(il) = MFe2_t(il) 
MFe_1L(il) = Sqrt(MFe2_1L(il)) 
 
If (p2.Ne.0._dp) Then 
  test(1) = Abs(MFe2_1L(il)-p2)/p2
Else 
  test(2) = Abs(MFe2_1L(il))
End If 
If (Abs(MFe2_1L(il)).lt.1.0E-30_dp) Exit p2_loop 
If (test(1).lt.0.1_dp*delta) Exit p2_loop 
If(i_count.gt.30) then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Exit p2_loop 
End if
End Do p2_loop 
mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFe2_t,ZEL1,ierr,test) 
 
 
ZEL2 = ZEL1 
Else 
Call EigenSystem(mat2,MFe2_t,ZEL2,ierr,test) 
 
 
End If 
ZEL2 = Conjg(ZEL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZEL2),mat1),Transpose( Conjg(ZER2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZER2(i1,:) = phaseM *ZER2(i1,:) 
 End if 
End Do 
 
ZELOS_p2(il,:) = ZEL2(il,:) 
 ZEROS_p2(il,:) = ZER2(il,:) 
 ZEL_1L = ZEL2 
 ZER_1L = ZER2 
 End Do  
 
Iname = Iname -1 
End Subroutine OneLoopFe
 
 
Subroutine Sigma1LoopFe(p2,MFe,MFe2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,              & 
& MFv,MFv2,MVWp,MVWp2,MSsc,MSsc2,MFxe,MFxe2,cplcUFeFeAhL,cplcUFeFeAhR,cplcUFeFehhL,      & 
& cplcUFeFehhR,cplcUFeFeVPL,cplcUFeFeVPR,cplcUFeFeVZL,cplcUFeFeVZR,cplcUFeFvcHpL,        & 
& cplcUFeFvcHpR,cplcUFeFvcVWpL,cplcUFeFvcVWpR,cplcUFeFxecSscL,cplcUFeFxecSscR,           & 
& sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MFe(3),MFe2(3),MAh,MAh2,Mhh(2),Mhh2(2),MVZ,MVZ2,MHp,MHp2,MFv(3),MFv2(3),              & 
& MVWp,MVWp2,MSsc(2),MSsc2(2),MFxe,MFxe2

Complex(dp), Intent(in) :: cplcUFeFeAhL(3,3),cplcUFeFeAhR(3,3),cplcUFeFehhL(3,3,2),cplcUFeFehhR(3,3,2),          & 
& cplcUFeFeVPL(3,3),cplcUFeFeVPR(3,3),cplcUFeFeVZL(3,3),cplcUFeFeVZR(3,3),               & 
& cplcUFeFvcHpL(3,3),cplcUFeFvcHpR(3,3),cplcUFeFvcVWpL(3,3),cplcUFeFvcVWpR(3,3),         & 
& cplcUFeFxecSscL(3,2),cplcUFeFxecSscR(3,2)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigSL(3,3), SigSR(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumSL(3,3), sumSR(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fe, Ah 
!------------------------ 
If ((Include_in_loopFe).and.(Include_in_loopAh)) Then 
    Do i1 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFe2(i1),MAh2),dp) 
B0m2 = MFe(i1)*Real(SA_B0(p2,MFe2(i1),MAh2),dp) 
coupL1 = cplcUFeFeAhL(gO1,i1)
coupR1 = cplcUFeFeAhR(gO1,i1)
coupL2 =  Conjg(cplcUFeFeAhL(gO2,i1))
coupR2 =  Conjg(cplcUFeFeAhR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 End if 
!------------------------ 
! hh, Fe 
!------------------------ 
If ((Include_in_loophh).and.(Include_in_loopFe)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFe2(i2),Mhh2(i1)),dp) 
B0m2 = MFe(i2)*Real(SA_B0(p2,MFe2(i2),Mhh2(i1)),dp) 
coupL1 = cplcUFeFehhL(gO1,i2,i1)
coupR1 = cplcUFeFehhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFeFehhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFeFehhR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 End if 
!------------------------ 
! VP, Fe 
!------------------------ 
If ((Include_in_loopVP).and.(Include_in_loopFe)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFe2(i2),0._dp)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFe(i2)*Real(SA_B0(p2,MFe2(i2),0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcUFeFeVPL(gO1,i2)
coupR1 = cplcUFeFeVPR(gO1,i2)
coupL2 =  Conjg(cplcUFeFeVPL(gO2,i2))
coupR2 =  Conjg(cplcUFeFeVPR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! VZ, Fe 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopFe)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFe2(i2),MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFe(i2)*Real(SA_B0(p2,MFe2(i2),MVZ2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFeFeVZL(gO1,i2)
coupR1 = cplcUFeFeVZR(gO1,i2)
coupL2 =  Conjg(cplcUFeFeVZL(gO2,i2))
coupR2 =  Conjg(cplcUFeFeVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! conj[Hp], Fv 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopFv)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFv2(i2),MHp2),dp) 
B0m2 = MFv(i2)*Real(SA_B0(p2,MFv2(i2),MHp2),dp) 
coupL1 = cplcUFeFvcHpL(gO1,i2)
coupR1 = cplcUFeFvcHpR(gO1,i2)
coupL2 =  Conjg(cplcUFeFvcHpL(gO2,i2))
coupR2 =  Conjg(cplcUFeFvcHpR(gO2,i2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! conj[VWp], Fv 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopFv)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFv2(i2),MVWp2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFv(i2)*Real(SA_B0(p2,MFv2(i2),MVWp2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFeFvcVWpL(gO1,i2)
coupR1 = cplcUFeFvcVWpR(gO1,i2)
coupL2 =  Conjg(cplcUFeFvcVWpL(gO2,i2))
coupR2 =  Conjg(cplcUFeFvcVWpR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! conj[Ssc], Fxe 
!------------------------ 
If ((Include_in_loopSsc).and.(Include_in_loopFxe)) Then 
    Do i1 = 1, 2
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFxe2,MSsc2(i1)),dp) 
B0m2 = MFxe*Real(SA_B0(p2,MFxe2,MSsc2(i1)),dp) 
coupL1 = cplcUFeFxecSscL(gO1,i1)
coupR1 = cplcUFeFxecSscR(gO1,i1)
coupL2 =  Conjg(cplcUFeFxecSscL(gO2,i1))
coupR2 =  Conjg(cplcUFeFxecSscR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 End if 
SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine Sigma1LoopFe 
 
Subroutine DerSigma1LoopFe(p2,MFe,MFe2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,           & 
& MFv,MFv2,MVWp,MVWp2,MSsc,MSsc2,MFxe,MFxe2,cplcUFeFeAhL,cplcUFeFeAhR,cplcUFeFehhL,      & 
& cplcUFeFehhR,cplcUFeFeVPL,cplcUFeFeVPR,cplcUFeFeVZL,cplcUFeFeVZR,cplcUFeFvcHpL,        & 
& cplcUFeFvcHpR,cplcUFeFvcVWpL,cplcUFeFvcVWpR,cplcUFeFxecSscL,cplcUFeFxecSscR,           & 
& sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MFe(3),MFe2(3),MAh,MAh2,Mhh(2),Mhh2(2),MVZ,MVZ2,MHp,MHp2,MFv(3),MFv2(3),              & 
& MVWp,MVWp2,MSsc(2),MSsc2(2),MFxe,MFxe2

Complex(dp), Intent(in) :: cplcUFeFeAhL(3,3),cplcUFeFeAhR(3,3),cplcUFeFehhL(3,3,2),cplcUFeFehhR(3,3,2),          & 
& cplcUFeFeVPL(3,3),cplcUFeFeVPR(3,3),cplcUFeFeVZL(3,3),cplcUFeFeVZR(3,3),               & 
& cplcUFeFvcHpL(3,3),cplcUFeFvcHpR(3,3),cplcUFeFvcVWpL(3,3),cplcUFeFvcVWpR(3,3),         & 
& cplcUFeFxecSscL(3,2),cplcUFeFxecSscR(3,2)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigSL(3,3), SigSR(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumSL(3,3), sumSR(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fe, Ah 
!------------------------ 
If ((Include_in_loopFe).and.(Include_in_loopAh)) Then 
    Do i1 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_DerB1(p2,MFe2(i1),MAh2),dp) 
B0m2 = MFe(i1)*Real(SA_DerB0(p2,MFe2(i1),MAh2),dp) 
coupL1 = cplcUFeFeAhL(gO1,i1)
coupR1 = cplcUFeFeAhR(gO1,i1)
coupL2 =  Conjg(cplcUFeFeAhL(gO2,i1))
coupR2 =  Conjg(cplcUFeFeAhR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 End if 
!------------------------ 
! hh, Fe 
!------------------------ 
If ((Include_in_loophh).and.(Include_in_loopFe)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_DerB1(p2,MFe2(i2),Mhh2(i1)),dp) 
B0m2 = MFe(i2)*Real(SA_DerB0(p2,MFe2(i2),Mhh2(i1)),dp) 
coupL1 = cplcUFeFehhL(gO1,i2,i1)
coupR1 = cplcUFeFehhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFeFehhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFeFehhR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 End if 
!------------------------ 
! VP, Fe 
!------------------------ 
If ((Include_in_loopVP).and.(Include_in_loopFe)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_DerB1(p2,MFe2(i2),MVP2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFe(i2)*Real(SA_DerB0(p2,MFe2(i2),MVP2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcUFeFeVPL(gO1,i2)
coupR1 = cplcUFeFeVPR(gO1,i2)
coupL2 =  Conjg(cplcUFeFeVPL(gO2,i2))
coupR2 =  Conjg(cplcUFeFeVPR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! VZ, Fe 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopFe)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_DerB1(p2,MFe2(i2),MVZ2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFe(i2)*Real(SA_DerB0(p2,MFe2(i2),MVZ2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcUFeFeVZL(gO1,i2)
coupR1 = cplcUFeFeVZR(gO1,i2)
coupL2 =  Conjg(cplcUFeFeVZL(gO2,i2))
coupR2 =  Conjg(cplcUFeFeVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! conj[Hp], Fv 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopFv)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_DerB1(p2,MFv2(i2),MHp2),dp) 
B0m2 = MFv(i2)*Real(SA_DerB0(p2,MFv2(i2),MHp2),dp) 
coupL1 = cplcUFeFvcHpL(gO1,i2)
coupR1 = cplcUFeFvcHpR(gO1,i2)
coupL2 =  Conjg(cplcUFeFvcHpL(gO2,i2))
coupR2 =  Conjg(cplcUFeFvcHpR(gO2,i2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! conj[VWp], Fv 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopFv)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_DerB1(p2,MFv2(i2),MVWp2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFv(i2)*Real(SA_DerB0(p2,MFv2(i2),MVWp2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcUFeFvcVWpL(gO1,i2)
coupR1 = cplcUFeFvcVWpR(gO1,i2)
coupL2 =  Conjg(cplcUFeFvcVWpL(gO2,i2))
coupR2 =  Conjg(cplcUFeFvcVWpR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! conj[Ssc], Fxe 
!------------------------ 
If ((Include_in_loopSsc).and.(Include_in_loopFxe)) Then 
    Do i1 = 1, 2
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_DerB1(p2,MFxe2,MSsc2(i1)),dp) 
B0m2 = MFxe*Real(SA_DerB0(p2,MFxe2,MSsc2(i1)),dp) 
coupL1 = cplcUFeFxecSscL(gO1,i1)
coupR1 = cplcUFeFxecSscR(gO1,i1)
coupL2 =  Conjg(cplcUFeFxecSscL(gO2,i1))
coupR2 =  Conjg(cplcUFeFxecSscR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 End if 
SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine DerSigma1LoopFe 
 
Subroutine OneLoopFv(MHp,MHp2,MFe,MFe2,MVWp,MVWp2,MVZ,MVZ2,MFv,MFv2,MSsc,             & 
& MSsc2,MFxv,MFxv2,cplcUFvFeHpL,cplcUFvFeHpR,cplcUFvFeVWpL,cplcUFvFeVWpR,cplcUFvFvVZL,   & 
& cplcUFvFvVZR,cplcUFvFxvcSscL,cplcUFvFxvcSscR,delta,MFv_1L,MFv2_1L,UV_1L,               & 
& UVR_1L,ierr)

Implicit None 
Real(dp), Intent(in) :: MHp,MHp2,MFe(3),MFe2(3),MVWp,MVWp2,MVZ,MVZ2,MFv(3),MFv2(3),MSsc(2),MSsc2(2),          & 
& MFxv(2),MFxv2(2)

Complex(dp), Intent(in) :: cplcUFvFeHpL(3,3),cplcUFvFeHpR(3,3),cplcUFvFeVWpL(3,3),cplcUFvFeVWpR(3,3),            & 
& cplcUFvFvVZL(3,3),cplcUFvFvVZR(3,3),cplcUFvFxvcSscL(3,2,2),cplcUFvFxvcSscR(3,2,2)

Complex(dp) :: mat1a(3,3), mat1(3,3) 
Integer , Intent(inout):: ierr 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(3), test_m2(3), p2 
Real(dp), Intent(out) :: MFv_1L(3),MFv2_1L(3) 
 Complex(dp), Intent(out) :: UV_1L(3,3), UVR_1L(3,3) 
 
 Real(dp) :: MFv_t(3),MFv2_t(3) 
 Complex(dp) :: UV_t(3,3), UVR_t(3,3), sigL(3,3), sigR(3,3), sigSL(3,3), sigSR(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: UV2(3,3), UVR2(3,3) 
 
 Real(dp) :: UV1(3,3), UVR1(3,3), test(2) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopMFv'
 
mat1a(1,1) = 0._dp 
mat1a(1,2) = 0._dp 
mat1a(1,3) = 0._dp 
mat1a(2,1) = 0._dp 
mat1a(2,2) = 0._dp 
mat1a(2,3) = 0._dp 
mat1a(3,1) = 0._dp 
mat1a(3,2) = 0._dp 
mat1a(3,3) = 0._dp 

 
 !---------------------------------------- 
! Rotation matrix for p2=0 
!----------------------------------------- 
 
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = 0._dp 
Call Sigma1LoopFv(p2,MHp,MHp2,MFe,MFe2,MVWp,MVWp2,MVZ,MVZ2,MFv,MFv2,MSsc,             & 
& MSsc2,MFxv,MFxv2,cplcUFvFeHpL,cplcUFvFeHpR,cplcUFvFeVWpL,cplcUFvFeVWpR,cplcUFvFvVZL,   & 
& cplcUFvFvVZR,cplcUFvFxvcSscL,cplcUFvFxvcSscR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFv2_t,UVR1,ierr,test) 
UVR2 = UVR1 
Else 
Call EigenSystem(mat2,MFv2_t,UVR2,ierr,test) 
 End If 
 
UVROS_0 = UVR2 
 mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFv2_t,UV1,ierr,test) 
 
 
UV2 = UV1 
Else 
Call EigenSystem(mat2,MFv2_t,UV2,ierr,test) 
 
 
End If 
UV2 = Conjg(UV2) 
UVOS_0 = UV2 
 
!---------------------------------------- 
! Now, with momenta
!----------------------------------------- 
 
Do il=3,1,-1
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = MFv2(il) 
Call Sigma1LoopFv(p2,MHp,MHp2,MFe,MFe2,MVWp,MVWp2,MVZ,MVZ2,MFv,MFv2,MSsc,             & 
& MSsc2,MFxv,MFxv2,cplcUFvFeHpL,cplcUFvFeHpR,cplcUFvFeVWpL,cplcUFvFeVWpR,cplcUFvFvVZL,   & 
& cplcUFvFvVZR,cplcUFvFxvcSscL,cplcUFvFxvcSscR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFv2_t,UVR1,ierr,test) 
UVR2 = UVR1 
Else 
Call EigenSystem(mat2,MFv2_t,UVR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
!---------------------------------------- 
! Redoing Calculation using redefined p2 
!----------------------------------------- 
 
i_count = 0 
p2_loop: Do  
i_count = i_count + 1 
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = MFv2_t(iL)
Call Sigma1LoopFv(p2,MHp,MHp2,MFe,MFe2,MVWp,MVWp2,MVZ,MVZ2,MFv,MFv2,MSsc,             & 
& MSsc2,MFxv,MFxv2,cplcUFvFeHpL,cplcUFvFeHpR,cplcUFvFeVWpL,cplcUFvFeVWpR,cplcUFvFvVZL,   & 
& cplcUFvFvVZR,cplcUFvFxvcSscL,cplcUFvFxvcSscR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFv2_t,UVR1,ierr,test) 
UVR2 = UVR1 
Else 
Call EigenSystem(mat2,MFv2_t,UVR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
MFv2_1L(il) = MFv2_t(il) 
MFv_1L(il) = Sqrt(MFv2_1L(il)) 
 
If (p2.Ne.0._dp) Then 
  test(1) = Abs(MFv2_1L(il)-p2)/p2
Else 
  test(2) = Abs(MFv2_1L(il))
End If 
If (Abs(MFv2_1L(il)).lt.1.0E-30_dp) Exit p2_loop 
If (test(1).lt.0.1_dp*delta) Exit p2_loop 
If(i_count.gt.30) then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Exit p2_loop 
End if
End Do p2_loop 
mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFv2_t,UV1,ierr,test) 
 
 
UV2 = UV1 
Else 
Call EigenSystem(mat2,MFv2_t,UV2,ierr,test) 
 
 
End If 
UV2 = Conjg(UV2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(UV2),mat1),Transpose( Conjg(UVR2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
UVR2(i1,:) = phaseM *UVR2(i1,:) 
 End if 
End Do 
 
UVOS_p2(il,:) = UV2(il,:) 
 UVROS_p2(il,:) = UVR2(il,:) 
 UV_1L = UV2 
 UVR_1L = UVR2 
 End Do  
 
Iname = Iname -1 
End Subroutine OneLoopFv
 
 
Subroutine Sigma1LoopFv(p2,MHp,MHp2,MFe,MFe2,MVWp,MVWp2,MVZ,MVZ2,MFv,MFv2,            & 
& MSsc,MSsc2,MFxv,MFxv2,cplcUFvFeHpL,cplcUFvFeHpR,cplcUFvFeVWpL,cplcUFvFeVWpR,           & 
& cplcUFvFvVZL,cplcUFvFvVZR,cplcUFvFxvcSscL,cplcUFvFxvcSscR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MHp,MHp2,MFe(3),MFe2(3),MVWp,MVWp2,MVZ,MVZ2,MFv(3),MFv2(3),MSsc(2),MSsc2(2),          & 
& MFxv(2),MFxv2(2)

Complex(dp), Intent(in) :: cplcUFvFeHpL(3,3),cplcUFvFeHpR(3,3),cplcUFvFeVWpL(3,3),cplcUFvFeVWpR(3,3),            & 
& cplcUFvFvVZL(3,3),cplcUFvFvVZR(3,3),cplcUFvFxvcSscL(3,2,2),cplcUFvFxvcSscR(3,2,2)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigSL(3,3), SigSR(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumSL(3,3), sumSR(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Hp, Fe 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopFe)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFe2(i2),MHp2),dp) 
B0m2 = MFe(i2)*Real(SA_B0(p2,MFe2(i2),MHp2),dp) 
coupL1 = cplcUFvFeHpL(gO1,i2)
coupR1 = cplcUFvFeHpR(gO1,i2)
coupL2 =  Conjg(cplcUFvFeHpL(gO2,i2))
coupR2 =  Conjg(cplcUFvFeHpR(gO2,i2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! VWp, Fe 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopFe)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFe2(i2),MVWp2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFe(i2)*Real(SA_B0(p2,MFe2(i2),MVWp2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFvFeVWpL(gO1,i2)
coupR1 = cplcUFvFeVWpR(gO1,i2)
coupL2 =  Conjg(cplcUFvFeVWpL(gO2,i2))
coupR2 =  Conjg(cplcUFvFeVWpR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! VZ, Fv 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopFv)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_B1(p2,MFv2(i2),MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFv(i2)*Real(SA_B0(p2,MFv2(i2),MVZ2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFvFvVZL(gO1,i2)
coupR1 = cplcUFvFvVZR(gO1,i2)
coupL2 =  Conjg(cplcUFvFvVZL(gO2,i2))
coupR2 =  Conjg(cplcUFvFvVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! conj[Ssc], Fxv 
!------------------------ 
If ((Include_in_loopSsc).and.(Include_in_loopFxv)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 2
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_B1(p2,MFxv2(i2),MSsc2(i1)),dp) 
B0m2 = MFxv(i2)*Real(SA_B0(p2,MFxv2(i2),MSsc2(i1)),dp) 
coupL1 = cplcUFvFxvcSscL(gO1,i2,i1)
coupR1 = cplcUFvFxvcSscR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFvFxvcSscL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFvFxvcSscR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 End if 
SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine Sigma1LoopFv 
 
Subroutine DerSigma1LoopFv(p2,MHp,MHp2,MFe,MFe2,MVWp,MVWp2,MVZ,MVZ2,MFv,              & 
& MFv2,MSsc,MSsc2,MFxv,MFxv2,cplcUFvFeHpL,cplcUFvFeHpR,cplcUFvFeVWpL,cplcUFvFeVWpR,      & 
& cplcUFvFvVZL,cplcUFvFvVZR,cplcUFvFxvcSscL,cplcUFvFxvcSscR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MHp,MHp2,MFe(3),MFe2(3),MVWp,MVWp2,MVZ,MVZ2,MFv(3),MFv2(3),MSsc(2),MSsc2(2),          & 
& MFxv(2),MFxv2(2)

Complex(dp), Intent(in) :: cplcUFvFeHpL(3,3),cplcUFvFeHpR(3,3),cplcUFvFeVWpL(3,3),cplcUFvFeVWpR(3,3),            & 
& cplcUFvFvVZL(3,3),cplcUFvFvVZR(3,3),cplcUFvFxvcSscL(3,2,2),cplcUFvFxvcSscR(3,2,2)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigSL(3,3), SigSR(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumSL(3,3), sumSR(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Hp, Fe 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopFe)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_DerB1(p2,MFe2(i2),MHp2),dp) 
B0m2 = MFe(i2)*Real(SA_DerB0(p2,MFe2(i2),MHp2),dp) 
coupL1 = cplcUFvFeHpL(gO1,i2)
coupR1 = cplcUFvFeHpR(gO1,i2)
coupL2 =  Conjg(cplcUFvFeHpL(gO2,i2))
coupR2 =  Conjg(cplcUFvFeHpR(gO2,i2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! VWp, Fe 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopFe)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_DerB1(p2,MFe2(i2),MVWp2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFe(i2)*Real(SA_DerB0(p2,MFe2(i2),MVWp2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcUFvFeVWpL(gO1,i2)
coupR1 = cplcUFvFeVWpR(gO1,i2)
coupL2 =  Conjg(cplcUFvFeVWpL(gO2,i2))
coupR2 =  Conjg(cplcUFvFeVWpR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! VZ, Fv 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopFv)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -2._dp*Real(SA_DerB1(p2,MFv2(i2),MVZ2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFv(i2)*Real(SA_DerB0(p2,MFv2(i2),MVZ2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcUFvFvVZL(gO1,i2)
coupR1 = cplcUFvFvVZR(gO1,i2)
coupL2 =  Conjg(cplcUFvFvVZL(gO2,i2))
coupR2 =  Conjg(cplcUFvFvVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! conj[Ssc], Fxv 
!------------------------ 
If ((Include_in_loopSsc).and.(Include_in_loopFxv)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 2
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
B1m2 = -Real(SA_DerB1(p2,MFxv2(i2),MSsc2(i1)),dp) 
B0m2 = MFxv(i2)*Real(SA_DerB0(p2,MFxv2(i2),MSsc2(i1)),dp) 
coupL1 = cplcUFvFxvcSscL(gO1,i2,i1)
coupR1 = cplcUFvFxvcSscR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFvFxvcSscL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFvFxvcSscR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 End if 
SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine DerSigma1LoopFv 
 
Subroutine OneLoopFxv(MDF,YRD,YRC,vvSM,vS,MFxv,MFxv2,MAh,MAh2,MSsc,MSsc2,             & 
& MFv,MFv2,MHp,MHp2,MFxe,MFxe2,MVWp,MVWp2,Mhh,Mhh2,MVZ,MVZ2,cplcUFxvFxvAhL,              & 
& cplcUFxvFxvAhR,cplcUFxvFvSscL,cplcUFxvFvSscR,cplcUFxvFxeHpL,cplcUFxvFxeHpR,            & 
& cplcUFxvFxeVWpL,cplcUFxvFxeVWpR,cplcUFxvFxvhhL,cplcUFxvFxvhhR,cplcUFxvFxvVZL,          & 
& cplcUFxvFxvVZR,delta,MFxv_1L,MFxv2_1L,XV_1L,XU_1L,ierr)

Implicit None 
Real(dp), Intent(in) :: MFxv(2),MFxv2(2),MAh,MAh2,MSsc(2),MSsc2(2),MFv(3),MFv2(3),MHp,MHp2,MFxe,              & 
& MFxe2,MVWp,MVWp2,Mhh(2),Mhh2(2),MVZ,MVZ2

Real(dp), Intent(in) :: MDF,YRD,YRC,vvSM,vS

Complex(dp), Intent(in) :: cplcUFxvFxvAhL(2,2),cplcUFxvFxvAhR(2,2),cplcUFxvFvSscL(2,3,2),cplcUFxvFvSscR(2,3,2),  & 
& cplcUFxvFxeHpL(2),cplcUFxvFxeHpR(2),cplcUFxvFxeVWpL(2),cplcUFxvFxeVWpR(2),             & 
& cplcUFxvFxvhhL(2,2,2),cplcUFxvFxvhhR(2,2,2),cplcUFxvFxvVZL(2,2),cplcUFxvFxvVZR(2,2)

Complex(dp) :: mat1a(2,2), mat1(2,2) 
Integer , Intent(inout):: ierr 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(2), test_m2(2), p2 
Real(dp), Intent(out) :: MFxv_1L(2),MFxv2_1L(2) 
 Complex(dp), Intent(out) :: XV_1L(2,2), XU_1L(2,2) 
 
 Real(dp) :: MFxv_t(2),MFxv2_t(2) 
 Complex(dp) :: XV_t(2,2), XU_t(2,2), sigL(2,2), sigR(2,2), sigSL(2,2), sigSR(2,2) 
 
 Complex(dp) :: mat(2,2)=0._dp, mat2(2,2)=0._dp, phaseM 

Complex(dp) :: XV2(2,2), XU2(2,2) 
 
 Real(dp) :: XV1(2,2), XU1(2,2), test(2) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopMFxv'
 
mat1a(1,1) = 0._dp 
mat1a(1,1) = mat1a(1,1)+MDF
mat1a(1,2) = 0._dp 
mat1a(1,2) = mat1a(1,2)+(vvSM*YRD)/sqrt(2._dp)
mat1a(2,1) = 0._dp 
mat1a(2,2) = 0._dp 
mat1a(2,2) = mat1a(2,2)+(vS*YRC)/sqrt(2._dp)

 
 !---------------------------------------- 
! Rotation matrix for p2=0 
!----------------------------------------- 
 
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = 0._dp 
Call Sigma1LoopFxv(p2,MFxv,MFxv2,MAh,MAh2,MSsc,MSsc2,MFv,MFv2,MHp,MHp2,               & 
& MFxe,MFxe2,MVWp,MVWp2,Mhh,Mhh2,MVZ,MVZ2,cplcUFxvFxvAhL,cplcUFxvFxvAhR,cplcUFxvFvSscL,  & 
& cplcUFxvFvSscR,cplcUFxvFxeHpL,cplcUFxvFxeHpR,cplcUFxvFxeVWpL,cplcUFxvFxeVWpR,          & 
& cplcUFxvFxvhhL,cplcUFxvFxvhhR,cplcUFxvFxvVZL,cplcUFxvFxvVZR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFxv2_t,XU1,ierr,test) 
XU2 = XU1 
Else 
Call EigenSystem(mat2,MFxv2_t,XU2,ierr,test) 
 End If 
 
XUOS_0 = XU2 
 mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFxv2_t,XV1,ierr,test) 
 
 
XV2 = XV1 
Else 
Call EigenSystem(mat2,MFxv2_t,XV2,ierr,test) 
 
 
End If 
XV2 = Conjg(XV2) 
XVOS_0 = XV2 
 
!---------------------------------------- 
! Now, with momenta
!----------------------------------------- 
 
Do il=2,1,-1
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = MFxv2(il) 
Call Sigma1LoopFxv(p2,MFxv,MFxv2,MAh,MAh2,MSsc,MSsc2,MFv,MFv2,MHp,MHp2,               & 
& MFxe,MFxe2,MVWp,MVWp2,Mhh,Mhh2,MVZ,MVZ2,cplcUFxvFxvAhL,cplcUFxvFxvAhR,cplcUFxvFvSscL,  & 
& cplcUFxvFvSscR,cplcUFxvFxeHpL,cplcUFxvFxeHpR,cplcUFxvFxeVWpL,cplcUFxvFxeVWpR,          & 
& cplcUFxvFxvhhL,cplcUFxvFxvhhR,cplcUFxvFxvVZL,cplcUFxvFxvVZR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFxv2_t,XU1,ierr,test) 
XU2 = XU1 
Else 
Call EigenSystem(mat2,MFxv2_t,XU2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
!---------------------------------------- 
! Redoing Calculation using redefined p2 
!----------------------------------------- 
 
i_count = 0 
p2_loop: Do  
i_count = i_count + 1 
sigL=0._dp 
sigR=0._dp 
sigSL=0._dp 
sigSR=0._dp 
p2 = MFxv2_t(iL)
Call Sigma1LoopFxv(p2,MFxv,MFxv2,MAh,MAh2,MSsc,MSsc2,MFv,MFv2,MHp,MHp2,               & 
& MFxe,MFxe2,MVWp,MVWp2,Mhh,Mhh2,MVZ,MVZ2,cplcUFxvFxvAhL,cplcUFxvFxvAhR,cplcUFxvFvSscL,  & 
& cplcUFxvFvSscR,cplcUFxvFxeHpL,cplcUFxvFxeHpR,cplcUFxvFxeVWpL,cplcUFxvFxeVWpR,          & 
& cplcUFxvFxvhhL,cplcUFxvFxvhhR,cplcUFxvFxvVZL,cplcUFxvFxvVZR,sigL,sigR,sigSL,sigSR)

mat1 = mat1a - SigSL - 0.5_dp*(MatMul(SigR,mat1a) + MatMul(mat1a,SigL)) 
 
mat2 = Matmul(Transpose(Conjg(mat1)),mat1) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFxv2_t,XU1,ierr,test) 
XU2 = XU1 
Else 
Call EigenSystem(mat2,MFxv2_t,XU2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
MFxv2_1L(il) = MFxv2_t(il) 
MFxv_1L(il) = Sqrt(MFxv2_1L(il)) 
 
If (p2.Ne.0._dp) Then 
  test(1) = Abs(MFxv2_1L(il)-p2)/p2
Else 
  test(2) = Abs(MFxv2_1L(il))
End If 
If (Abs(MFxv2_1L(il)).lt.1.0E-30_dp) Exit p2_loop 
If (test(1).lt.0.1_dp*delta) Exit p2_loop 
If(i_count.gt.30) then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Exit p2_loop 
End if
End Do p2_loop 
mat2 = Matmul(mat1,Transpose(Conjg(mat1))) 
If (ForceRealMatrices) mat2 = Real(mat2,dp) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFxv2_t,XV1,ierr,test) 
 
 
XV2 = XV1 
Else 
Call EigenSystem(mat2,MFxv2_t,XV2,ierr,test) 
 
 
End If 
XV2 = Conjg(XV2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(XV2),mat1),Transpose( Conjg(XU2))) 
Do i1=1,2
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
XU2(i1,:) = phaseM *XU2(i1,:) 
 End if 
End Do 
 
XVOS_p2(il,:) = XV2(il,:) 
 XUOS_p2(il,:) = XU2(il,:) 
 XV_1L = XV2 
 XU_1L = XU2 
 End Do  
 
Iname = Iname -1 
End Subroutine OneLoopFxv
 
 
Subroutine Sigma1LoopFxv(p2,MFxv,MFxv2,MAh,MAh2,MSsc,MSsc2,MFv,MFv2,MHp,              & 
& MHp2,MFxe,MFxe2,MVWp,MVWp2,Mhh,Mhh2,MVZ,MVZ2,cplcUFxvFxvAhL,cplcUFxvFxvAhR,            & 
& cplcUFxvFvSscL,cplcUFxvFvSscR,cplcUFxvFxeHpL,cplcUFxvFxeHpR,cplcUFxvFxeVWpL,           & 
& cplcUFxvFxeVWpR,cplcUFxvFxvhhL,cplcUFxvFxvhhR,cplcUFxvFxvVZL,cplcUFxvFxvVZR,           & 
& sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MFxv(2),MFxv2(2),MAh,MAh2,MSsc(2),MSsc2(2),MFv(3),MFv2(3),MHp,MHp2,MFxe,              & 
& MFxe2,MVWp,MVWp2,Mhh(2),Mhh2(2),MVZ,MVZ2

Complex(dp), Intent(in) :: cplcUFxvFxvAhL(2,2),cplcUFxvFxvAhR(2,2),cplcUFxvFvSscL(2,3,2),cplcUFxvFvSscR(2,3,2),  & 
& cplcUFxvFxeHpL(2),cplcUFxvFxeHpR(2),cplcUFxvFxeVWpL(2),cplcUFxvFxeVWpR(2),             & 
& cplcUFxvFxvhhL(2,2,2),cplcUFxvFxvhhR(2,2,2),cplcUFxvFxvVZL(2,2),cplcUFxvFxvVZR(2,2)

Complex(dp), Intent(out) :: SigL(2,2),SigR(2,2), SigSL(2,2), SigSR(2,2) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(2,2), sumR(2,2), sumSL(2,2), sumSR(2,2) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fxv, Ah 
!------------------------ 
If ((Include_in_loopFxv).and.(Include_in_loopAh)) Then 
    Do i1 = 1, 2
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 2
  Do gO2 = 1, 2
B1m2 = -Real(SA_B1(p2,MFxv2(i1),MAh2),dp) 
B0m2 = MFxv(i1)*Real(SA_B0(p2,MFxv2(i1),MAh2),dp) 
coupL1 = cplcUFxvFxvAhL(gO1,i1)
coupR1 = cplcUFxvFxvAhR(gO1,i1)
coupL2 =  Conjg(cplcUFxvFxvAhL(gO2,i1))
coupR2 =  Conjg(cplcUFxvFxvAhR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 End if 
!------------------------ 
! Ssc, Fv 
!------------------------ 
If ((Include_in_loopSsc).and.(Include_in_loopFv)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 2
  Do gO2 = 1, 2
B1m2 = -Real(SA_B1(p2,MFv2(i2),MSsc2(i1)),dp) 
B0m2 = MFv(i2)*Real(SA_B0(p2,MFv2(i2),MSsc2(i1)),dp) 
coupL1 = cplcUFxvFvSscL(gO1,i2,i1)
coupR1 = cplcUFxvFvSscR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFxvFvSscL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFxvFvSscR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 End if 
!------------------------ 
! Hp, Fxe 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopFxe)) Then 
SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 2
  Do gO2 = 1, 2
B1m2 = -Real(SA_B1(p2,MFxe2,MHp2),dp) 
B0m2 = MFxe*Real(SA_B0(p2,MFxe2,MHp2),dp) 
coupL1 = cplcUFxvFxeHpL(gO1)
coupR1 = cplcUFxvFxeHpR(gO1)
coupL2 =  Conjg(cplcUFxvFxeHpL(gO2))
coupR2 =  Conjg(cplcUFxvFxeHpR(gO2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
End if 
!------------------------ 
! VWp, Fxe 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopFxe)) Then 
SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 2
  Do gO2 = 1, 2
B1m2 = -2._dp*Real(SA_B1(p2,MFxe2,MVWp2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFxe*Real(SA_B0(p2,MFxe2,MVWp2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFxvFxeVWpL(gO1)
coupR1 = cplcUFxvFxeVWpR(gO1)
coupL2 =  Conjg(cplcUFxvFxeVWpL(gO2))
coupR2 =  Conjg(cplcUFxvFxeVWpR(gO2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
End if 
!------------------------ 
! hh, Fxv 
!------------------------ 
If ((Include_in_loophh).and.(Include_in_loopFxv)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 2
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 2
  Do gO2 = 1, 2
B1m2 = -Real(SA_B1(p2,MFxv2(i2),Mhh2(i1)),dp) 
B0m2 = MFxv(i2)*Real(SA_B0(p2,MFxv2(i2),Mhh2(i1)),dp) 
coupL1 = cplcUFxvFxvhhL(gO1,i2,i1)
coupR1 = cplcUFxvFxvhhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFxvFxvhhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFxvFxvhhR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 End if 
!------------------------ 
! VZ, Fxv 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopFxv)) Then 
      Do i2 = 1, 2
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 2
  Do gO2 = 1, 2
B1m2 = -2._dp*Real(SA_B1(p2,MFxv2(i2),MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFxv(i2)*Real(SA_B0(p2,MFxv2(i2),MVZ2)-0.5_dp*rMS,dp) 
coupL1 = cplcUFxvFxvVZL(gO1,i2)
coupR1 = cplcUFxvFxvVZR(gO1,i2)
coupL2 =  Conjg(cplcUFxvFxvVZL(gO2,i2))
coupR2 =  Conjg(cplcUFxvFxvVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine Sigma1LoopFxv 
 
Subroutine DerSigma1LoopFxv(p2,MFxv,MFxv2,MAh,MAh2,MSsc,MSsc2,MFv,MFv2,               & 
& MHp,MHp2,MFxe,MFxe2,MVWp,MVWp2,Mhh,Mhh2,MVZ,MVZ2,cplcUFxvFxvAhL,cplcUFxvFxvAhR,        & 
& cplcUFxvFvSscL,cplcUFxvFvSscR,cplcUFxvFxeHpL,cplcUFxvFxeHpR,cplcUFxvFxeVWpL,           & 
& cplcUFxvFxeVWpR,cplcUFxvFxvhhL,cplcUFxvFxvhhR,cplcUFxvFxvVZL,cplcUFxvFxvVZR,           & 
& sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MFxv(2),MFxv2(2),MAh,MAh2,MSsc(2),MSsc2(2),MFv(3),MFv2(3),MHp,MHp2,MFxe,              & 
& MFxe2,MVWp,MVWp2,Mhh(2),Mhh2(2),MVZ,MVZ2

Complex(dp), Intent(in) :: cplcUFxvFxvAhL(2,2),cplcUFxvFxvAhR(2,2),cplcUFxvFvSscL(2,3,2),cplcUFxvFvSscR(2,3,2),  & 
& cplcUFxvFxeHpL(2),cplcUFxvFxeHpR(2),cplcUFxvFxeVWpL(2),cplcUFxvFxeVWpR(2),             & 
& cplcUFxvFxvhhL(2,2,2),cplcUFxvFxvhhR(2,2,2),cplcUFxvFxvVZL(2,2),cplcUFxvFxvVZR(2,2)

Complex(dp), Intent(out) :: SigL(2,2),SigR(2,2), SigSL(2,2), SigSR(2,2) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(2,2), sumR(2,2), sumSL(2,2), sumSR(2,2) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fxv, Ah 
!------------------------ 
If ((Include_in_loopFxv).and.(Include_in_loopAh)) Then 
    Do i1 = 1, 2
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 2
  Do gO2 = 1, 2
B1m2 = -Real(SA_DerB1(p2,MFxv2(i1),MAh2),dp) 
B0m2 = MFxv(i1)*Real(SA_DerB0(p2,MFxv2(i1),MAh2),dp) 
coupL1 = cplcUFxvFxvAhL(gO1,i1)
coupR1 = cplcUFxvFxvAhR(gO1,i1)
coupL2 =  Conjg(cplcUFxvFxvAhL(gO2,i1))
coupR2 =  Conjg(cplcUFxvFxvAhR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 End if 
!------------------------ 
! Ssc, Fv 
!------------------------ 
If ((Include_in_loopSsc).and.(Include_in_loopFv)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 2
  Do gO2 = 1, 2
B1m2 = -Real(SA_DerB1(p2,MFv2(i2),MSsc2(i1)),dp) 
B0m2 = MFv(i2)*Real(SA_DerB0(p2,MFv2(i2),MSsc2(i1)),dp) 
coupL1 = cplcUFxvFvSscL(gO1,i2,i1)
coupR1 = cplcUFxvFvSscR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFxvFvSscL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFxvFvSscR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 End if 
!------------------------ 
! Hp, Fxe 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopFxe)) Then 
SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 2
  Do gO2 = 1, 2
B1m2 = -Real(SA_DerB1(p2,MFxe2,MHp2),dp) 
B0m2 = MFxe*Real(SA_DerB0(p2,MFxe2,MHp2),dp) 
coupL1 = cplcUFxvFxeHpL(gO1)
coupR1 = cplcUFxvFxeHpR(gO1)
coupL2 =  Conjg(cplcUFxvFxeHpL(gO2))
coupR2 =  Conjg(cplcUFxvFxeHpR(gO2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
End if 
!------------------------ 
! VWp, Fxe 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopFxe)) Then 
SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 2
  Do gO2 = 1, 2
B1m2 = -2._dp*Real(SA_DerB1(p2,MFxe2,MVWp2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFxe*Real(SA_DerB0(p2,MFxe2,MVWp2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcUFxvFxeVWpL(gO1)
coupR1 = cplcUFxvFxeVWpR(gO1)
coupL2 =  Conjg(cplcUFxvFxeVWpL(gO2))
coupR2 =  Conjg(cplcUFxvFxeVWpR(gO2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
End if 
!------------------------ 
! hh, Fxv 
!------------------------ 
If ((Include_in_loophh).and.(Include_in_loopFxv)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 2
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 2
  Do gO2 = 1, 2
B1m2 = -Real(SA_DerB1(p2,MFxv2(i2),Mhh2(i1)),dp) 
B0m2 = MFxv(i2)*Real(SA_DerB0(p2,MFxv2(i2),Mhh2(i1)),dp) 
coupL1 = cplcUFxvFxvhhL(gO1,i2,i1)
coupR1 = cplcUFxvFxvhhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFxvFxvhhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFxvFxvhhR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 End if 
!------------------------ 
! VZ, Fxv 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopFxv)) Then 
      Do i2 = 1, 2
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 2
  Do gO2 = 1, 2
B1m2 = -2._dp*Real(SA_DerB1(p2,MFxv2(i2),MVZ2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFxv(i2)*Real(SA_DerB0(p2,MFxv2(i2),MVZ2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcUFxvFxvVZL(gO1,i2)
coupR1 = cplcUFxvFxvVZR(gO1,i2)
coupL2 =  Conjg(cplcUFxvFxvVZL(gO2,i2))
coupR2 =  Conjg(cplcUFxvFxvVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine DerSigma1LoopFxv 
 
Subroutine OneLoopSsc(MS12,MS22,LS1H,LS2H,vvSM,MFe,MFe2,MFxe,MFxe2,MFv,               & 
& MFv2,MFxv,MFxv2,MSsc,MSsc2,Mhh,Mhh2,MAh,MAh2,MHp,MHp2,cplcFeFxecUSscL,cplcFeFxecUSscR, & 
& cplcFvFxvcUSscL,cplcFvFxvcUSscR,cplhhSsccUSsc,cplAhAhUSsccUSsc,cplhhhhUSsccUSsc,       & 
& cplHpUSsccHpcUSsc,cplUSscSsccUSsccSsc,delta,mass,mass2,RS,kont)

Implicit None 
Real(dp), Intent(in) :: MFe(3),MFe2(3),MFxe,MFxe2,MFv(3),MFv2(3),MFxv(2),MFxv2(2),MSsc(2),MSsc2(2),           & 
& Mhh(2),Mhh2(2),MAh,MAh2,MHp,MHp2

Real(dp), Intent(in) :: MS12,MS22,LS1H,LS2H,vvSM

Complex(dp), Intent(in) :: cplcFeFxecUSscL(3,2),cplcFeFxecUSscR(3,2),cplcFvFxvcUSscL(3,2,2),cplcFvFxvcUSscR(3,2,2),& 
& cplhhSsccUSsc(2,2,2),cplAhAhUSsccUSsc(2,2),cplhhhhUSsccUSsc(2,2,2,2),cplHpUSsccHpcUSsc(2,2),& 
& cplUSscSsccUSsccSsc(2,2,2,2)

Complex(dp) :: mat2a(2,2), mat2(2,2),  PiSf(2,2,2)
Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi2(2), test_m2(2),p2, test(2) 
Real(dp), Intent(out) :: mass(2), mass2(2) 
Complex(dp), Intent(out) ::  RS(2,2) 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopSsc'
 
mat2a(1,1) = 0._dp 
mat2a(1,1) = mat2a(1,1)+MS12
mat2a(1,1) = mat2a(1,1)+(LS1H*vvSM**2)/2._dp
mat2a(1,2) = 0._dp 
mat2a(2,2) = 0._dp 
mat2a(2,2) = mat2a(2,2)+MS22
mat2a(2,2) = mat2a(2,2)+(LS2H*vvSM**2)/2._dp

 
 Do i1=2,2
  Do i2 = 1, i1-1 
  mat2a(i1,i2) = Conjg(mat2a(i2,i1)) 
  End do 
End do 

 
! Rotation matrix for p2=0 
PiSf(1,:,:) = ZeroC 
p2 = 0._dp 
Call Pi1LoopSsc(p2,MFe,MFe2,MFxe,MFxe2,MFv,MFv2,MFxv,MFxv2,MSsc,MSsc2,Mhh,            & 
& Mhh2,MAh,MAh2,MHp,MHp2,cplcFeFxecUSscL,cplcFeFxecUSscR,cplcFvFxvcUSscL,cplcFvFxvcUSscR,& 
& cplhhSsccUSsc,cplAhAhUSsccUSsc,cplhhhhUSsccUSsc,cplHpUSsccHpcUSsc,cplUSscSsccUSsccSsc, & 
& kont,PiSf(1,:,:))

mat2 = mat2a - Real(PiSf(1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
VSsOS_0 = RS 
 
 
! Now with momenta 
Do i1=1,2
PiSf(i1,:,:) = ZeroC 
p2 = MSsc2(i1)
Call Pi1LoopSsc(p2,MFe,MFe2,MFxe,MFxe2,MFv,MFv2,MFxv,MFxv2,MSsc,MSsc2,Mhh,            & 
& Mhh2,MAh,MAh2,MHp,MHp2,cplcFeFxecUSscL,cplcFeFxecUSscR,cplcFvFxvcUSscL,cplcFvFxvcUSscR,& 
& cplhhSsccUSsc,cplAhAhUSsccUSsc,cplhhhhUSsccUSsc,cplHpUSsccHpcUSsc,cplUSscSsccUSsccSsc, & 
& kont,PiSf(i1,:,:))

End Do 
Do i1=2,1,-1 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
End If 
mass2(i1) = mi2(i1) 
End do 
 
Do i1=1,2
  If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
  If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = Sqrt(mass2(i1)) 
  Else 
   If (ErrorLevel.Ge.0) Then 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
   Call TerminateProgram 
   End If 
   kont = -301 
   mass(i1) = 0._dp 
  End If 
End Do 
 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
Do i1=1,2
PiSf(i1,:,:) = ZeroC 
p2 =  mass2(i1) 
Call Pi1LoopSsc(p2,MFe,MFe2,MFxe,MFxe2,MFv,MFv2,MFxv,MFxv2,MSsc,MSsc2,Mhh,            & 
& Mhh2,MAh,MAh2,MHp,MHp2,cplcFeFxecUSscL,cplcFeFxecUSscR,cplcFvFxvcUSscL,cplcFvFxvcUSscR,& 
& cplhhSsccUSsc,cplAhAhUSsccUSsc,cplhhhhUSsccUSsc,cplHpUSsccHpcUSsc,cplUSscSsccUSsccSsc, & 
& kont,PiSf(i1,:,:))

End Do 
Do i1=2,1,-1 
mat2 = mat2a - Real(PiSf(i1,:,:),dp) 
Call Chop(mat2) 
Call Eigensystem(mat2,mi2,RS,kont,test) 
VSsOS_p2(i1,:) = RS(i1,:) 
 If ((kont.Eq.-8).Or.(kont.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
  kont = 0 
End If 
If ((kont.Ne.0).And.(ErrorLevel.Ge.0)) Then 
  Write(ErrCan,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  Write(*,*) "Diagonalization did not work in routine "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Call TerminateProgram 
End If 
mass2(i1) = mi2(i1) 
End do 
Do i1=1,2
 If (Abs(mass2(i1)).Le.MaxMassNumericalZero**2) mass2(i1) = 0._dp 
 If (test_m2(i1).Ne.0._dp) Then 
    test_m2(i1) = Abs(test_m2(i1) - mass2(i1)) / test_m2(i1) 
 Else 
    test_m2(i1) = Abs(mass2(i1)) 
 End If 
 If (Abs(mass2(i1)).lt.1.0E-30_dp) test_m2(i1) = 0._dp 
 If (mass2(i1).Ge.0._dp) Then 
    mass(i1) = sqrt(mass2(i1)) 
  Else 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses occurred a negative mass squared!' 
     Write(*,*) 'generation: ',i1 
     Write(*,*) 'mass: ',mass2(i1) 
   SignOfMassChanged = .True. 
   mass(i1) = 0._dp 
  End If 
End Do 
 
If (Maxval(test_m2).LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoopSsc
 
 
Subroutine Pi1LoopSsc(p2,MFe,MFe2,MFxe,MFxe2,MFv,MFv2,MFxv,MFxv2,MSsc,MSsc2,          & 
& Mhh,Mhh2,MAh,MAh2,MHp,MHp2,cplcFeFxecUSscL,cplcFeFxecUSscR,cplcFvFxvcUSscL,            & 
& cplcFvFxvcUSscR,cplhhSsccUSsc,cplAhAhUSsccUSsc,cplhhhhUSsccUSsc,cplHpUSsccHpcUSsc,     & 
& cplUSscSsccUSsccSsc,kont,res)

Implicit None 
Real(dp), Intent(in) :: MFe(3),MFe2(3),MFxe,MFxe2,MFv(3),MFv2(3),MFxv(2),MFxv2(2),MSsc(2),MSsc2(2),           & 
& Mhh(2),Mhh2(2),MAh,MAh2,MHp,MHp2

Complex(dp), Intent(in) :: cplcFeFxecUSscL(3,2),cplcFeFxecUSscR(3,2),cplcFvFxvcUSscL(3,2,2),cplcFvFxvcUSscR(3,2,2),& 
& cplhhSsccUSsc(2,2,2),cplAhAhUSsccUSsc(2,2),cplhhhhUSsccUSsc(2,2,2,2),cplHpUSsccHpcUSsc(2,2),& 
& cplUSscSsccUSsccSsc(2,2,2,2)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(2,2) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumI(2,2) 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! bar[Fe], Fxe 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFe).and.(Include_in_loopFxe)) Then 
    Do i1 = 1, 3
 G0m2 = Real(SA_Gloop(p2,MFe2(i1),MFxe2),dp) 
B0m2 = -2._dp*MFe(i1)*MFxe*Real(SA_B0(p2,MFe2(i1),MFxe2),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coupL1 = cplcFeFxecUSscL(i1,gO1)
coupR1 = cplcFeFxecUSscR(i1,gO1)
coupL2 =  Conjg(cplcFeFxecUSscL(i1,gO2))
coupR2 =  Conjg(cplcFeFxecUSscR(i1,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 End If 
 !------------------------ 
! bar[Fv], Fxv 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFv).and.(Include_in_loopFxv)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 2
 G0m2 = Real(SA_Gloop(p2,MFv2(i1),MFxv2(i2)),dp) 
B0m2 = -2._dp*MFv(i1)*MFxv(i2)*Real(SA_B0(p2,MFv2(i1),MFxv2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coupL1 = cplcFvFxvcUSscL(i1,i2,gO1)
coupR1 = cplcFvFxvcUSscR(i1,i2,gO1)
coupL2 =  Conjg(cplcFvFxvcUSscL(i1,i2,gO2))
coupR2 =  Conjg(cplcFvFxvcUSscR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End If 
 !------------------------ 
! Ssc, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopSsc).and.(Include_in_loophh)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 2
 B0m2 = Real(SA_B0(p2,MSsc2(i1),Mhh2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplhhSsccUSsc(i2,i1,gO1)
coup2 = Conjg(cplhhSsccUSsc(i2,i1,gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End If 
 !------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
A0m2 = SA_A0(MAh2) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplAhAhUSsccUSsc(gO2,gO1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
End If 
 !------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
    Do i1 = 1, 2
 A0m2 = SA_A0(Mhh2(i1)) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplhhhhUSsccUSsc(i1,i1,gO2,gO1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 End If 
 !------------------------ 
! conj[Hp], Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
A0m2 = SA_A0(MHp2) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplHpUSsccHpcUSsc(gO2,gO1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
End If 
 !------------------------ 
! conj[Ssc], Ssc 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
    Do i1 = 1, 2
 A0m2 = SA_A0(MSsc2(i1)) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUSscSsccUSsccSsc(gO2,i1,gO1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 End If 
 

Do gO2 = 1, 2
  Do gO1 = gO2+1, 2
     res(gO1,gO2) = Conjg(res(gO2,gO1))  
   End Do 
End Do 
 
res = oo16pi2*res 
 
End Subroutine Pi1LoopSsc 
 
Subroutine DerPi1LoopSsc(p2,MFe,MFe2,MFxe,MFxe2,MFv,MFv2,MFxv,MFxv2,MSsc,             & 
& MSsc2,Mhh,Mhh2,MAh,MAh2,MHp,MHp2,cplcFeFxecUSscL,cplcFeFxecUSscR,cplcFvFxvcUSscL,      & 
& cplcFvFxvcUSscR,cplhhSsccUSsc,cplAhAhUSsccUSsc,cplhhhhUSsccUSsc,cplHpUSsccHpcUSsc,     & 
& cplUSscSsccUSsccSsc,kont,res)

Implicit None 
Real(dp), Intent(in) :: MFe(3),MFe2(3),MFxe,MFxe2,MFv(3),MFv2(3),MFxv(2),MFxv2(2),MSsc(2),MSsc2(2),           & 
& Mhh(2),Mhh2(2),MAh,MAh2,MHp,MHp2

Complex(dp), Intent(in) :: cplcFeFxecUSscL(3,2),cplcFeFxecUSscR(3,2),cplcFvFxvcUSscL(3,2,2),cplcFvFxvcUSscR(3,2,2),& 
& cplhhSsccUSsc(2,2,2),cplAhAhUSsccUSsc(2,2),cplhhhhUSsccUSsc(2,2,2,2),cplHpUSsccHpcUSsc(2,2),& 
& cplUSscSsccUSsccSsc(2,2,2,2)

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(2,2) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumI(2,2) 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! bar[Fe], Fxe 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFe).and.(Include_in_loopFxe)) Then 
    Do i1 = 1, 3
 G0m2 = Real(SA_DerGloop(p2,MFe2(i1),MFxe2),dp) 
B0m2 = -2._dp*MFe(i1)*MFxe*Real(SA_DerB0(p2,MFe2(i1),MFxe2),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coupL1 = cplcFeFxecUSscL(i1,gO1)
coupR1 = cplcFeFxecUSscR(i1,gO1)
coupL2 =  Conjg(cplcFeFxecUSscL(i1,gO2))
coupR2 =  Conjg(cplcFeFxecUSscR(i1,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 End If 
 !------------------------ 
! bar[Fv], Fxv 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFv).and.(Include_in_loopFxv)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 2
 G0m2 = Real(SA_DerGloop(p2,MFv2(i1),MFxv2(i2)),dp) 
B0m2 = -2._dp*MFv(i1)*MFxv(i2)*Real(SA_DerB0(p2,MFv2(i1),MFxv2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coupL1 = cplcFvFxvcUSscL(i1,i2,gO1)
coupR1 = cplcFvFxvcUSscR(i1,i2,gO1)
coupL2 =  Conjg(cplcFvFxvcUSscL(i1,i2,gO2))
coupR2 =  Conjg(cplcFvFxvcUSscR(i1,i2,gO2))
    SumI(gO1,gO2) = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End If 
 !------------------------ 
! Ssc, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopSsc).and.(Include_in_loophh)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 2
 B0m2 = Real(SA_DerB0(p2,MSsc2(i1),Mhh2(i2)),dp) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplhhSsccUSsc(i2,i1,gO1)
coup2 = Conjg(cplhhSsccUSsc(i2,i1,gO2))
    SumI(gO1,gO2) = coup1*coup2*B0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End If 
 !------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
A0m2 = SA_DerA0(MAh2) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplAhAhUSsccUSsc(gO2,gO1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
End If 
 !------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
    Do i1 = 1, 2
 A0m2 = SA_DerA0(Mhh2(i1)) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplhhhhUSsccUSsc(i1,i1,gO2,gO1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp/2._dp* SumI  
      End Do 
 End If 
 !------------------------ 
! conj[Hp], Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
A0m2 = SA_DerA0(MHp2) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplHpUSsccHpcUSsc(gO2,gO1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
End If 
 !------------------------ 
! conj[Ssc], Ssc 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
    Do i1 = 1, 2
 A0m2 = SA_DerA0(MSsc2(i1)) 
Do gO1 = 1, 2
  Do gO2 = gO1, 2
coup1 = cplUSscSsccUSsccSsc(gO2,i1,gO1,i1)
    SumI(gO1,gO2) = -coup1*A0m2 
   End Do 
End Do 
res = res +1._dp* SumI  
      End Do 
 End If 
 

Do gO2 = 1, 2
  Do gO1 = gO2+1, 2
     res(gO1,gO2) = Conjg(res(gO2,gO1))  
   End Do 
End Do 
 
res = oo16pi2*res 
 
End Subroutine DerPi1LoopSsc 
 
Subroutine OneLoopFxe(MDF,MSsc,MSsc2,MFe,MFe2,MFxe,MFxe2,MVZ,MVZ2,MHp,MHp2,           & 
& MFxv,MFxv2,MVWp,MVWp2,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,         & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,             & 
& cplcFxeFxvcVWpR,delta,mass,mass2,kont)

Real(dp), Intent(in) :: MSsc(2),MSsc2(2),MFe(3),MFe2(3),MFxe,MFxe2,MVZ,MVZ2,MHp,MHp2,MFxv(2),MFxv2(2),        & 
& MVWp,MVWp2

Real(dp), Intent(in) :: MDF

Complex(dp), Intent(in) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,      & 
& cplcFxeFxeVZR,cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2)

Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi, mi2, p2, test_m2 
Complex(dp) :: PiSf, SigL, SigR, SigSL, SigSR 
Real(dp), Intent(out) :: mass, mass2 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopFxe'
 
mi = MFxe 

 
p2 = MFxe2
sigL = ZeroC 
sigR = ZeroC 
sigSL = ZeroC 
sigSR = ZeroC 
Call Sigma1LoopFxe(p2,MSsc,MSsc2,MFe,MFe2,MFxe,MFxe2,MVZ,MVZ2,MHp,MHp2,               & 
& MFxv,MFxv2,MVWp,MVWp2,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,         & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,             & 
& cplcFxeFxvcVWpR,sigL,sigR,sigSL,sigSR)

mass = mi - 0.5_dp*(sigSL + sigSR)- 0.5_dp*MFxe*(SigR+SigL) 
mass2= mass**2 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
p2 =  mass2 
sig = ZeroC 
Call Sigma1LoopFxe(p2,MSsc,MSsc2,MFe,MFe2,MFxe,MFxe2,MVZ,MVZ2,MHp,MHp2,               & 
& MFxv,MFxv2,MVWp,MVWp2,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,         & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,             & 
& cplcFxeFxvcVWpR,sigL,sigR,sigSL,sigSR)

mass = mi - SigSR - 0.5_dp*MFxe*(SigR+SigL) 
mass2= mass**2 
 If (test_m2.Ne.0._dp) Then 
    test_m2 = Abs(test_m2 - mass2) / test_m2 
 Else 
    test_m2 = Abs(mass2) 
 End If 
 If (mass2.Ge.0._dp) Then 
   If (RotateNegativeFermionMasses) Then 
    mass = sqrt(mass2) 
   End if 
  Else 
 If (Abs(mass2).lt.1.0E-30_dp) test_m2 = 0._dp 
     Write(ErrCan,*) 'Warning from routine'//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
   SignOfMassChanged = .True. 
   mass = 0._dp 
  End If 
If (test_m2.LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoopFxe
 
 
Subroutine Sigma1LoopFxe(p2,MSsc,MSsc2,MFe,MFe2,MFxe,MFxe2,MVZ,MVZ2,MHp,              & 
& MHp2,MFxv,MFxv2,MVWp,MVWp2,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,    & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,             & 
& cplcFxeFxvcVWpR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MSsc(2),MSsc2(2),MFe(3),MFe2(3),MFxe,MFxe2,MVZ,MVZ2,MHp,MHp2,MFxv(2),MFxv2(2),        & 
& MVWp,MVWp2

Complex(dp), Intent(in) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,      & 
& cplcFxeFxeVZR,cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2)

Complex(dp), Intent(out) :: SigL, SigR, SigSL, SigSR 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL, sumR, sumSL, sumSR 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
SigSR = Cmplx(0._dp,0._dp,dp) 
!------------------------ 
! Ssc, Fe 
!------------------------ 
If ((Include_in_loopSsc).and.(Include_in_loopFe)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -Real(SA_B1(p2,MFe2(i2),MSsc2(i1)),dp) 
B0m2 = MFe(i2)*Real(SA_B0(p2,MFe2(i2),MSsc2(i1)),dp) 
coupL1 = cplcFxeFeSscL(i2,i1)
coupR1 = cplcFxeFeSscR(i2,i1)
coupL2 =  Conjg(cplcFxeFeSscL(i2,i1))
coupR2 =  Conjg(cplcFxeFeSscR(i2,i1))
SumSL = coupR1*coupL2*B0m2 
SumSR = coupL1*coupR2*B0m2 
sumR = coupR1*coupR2*B1m2 
sumL = coupL1*coupL2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
      End Do 
     End Do 
 End if 
!------------------------ 
! VP, Fxe 
!------------------------ 
If ((Include_in_loopVP).and.(Include_in_loopFxe)) Then 
SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -2._dp*Real(SA_B1(p2,MFxe2,0._dp)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFxe*Real(SA_B0(p2,MFxe2,0._dp)-0.5_dp*rMS,dp) 
coupL1 = cplcFxeFxeVPL
coupR1 = cplcFxeFxeVPR
coupL2 =  Conjg(cplcFxeFxeVPL)
coupR2 =  Conjg(cplcFxeFxeVPR)
SumSR = coupR1*coupL2*B0m2 
SumSL = coupL1*coupR2*B0m2 
sumR = coupL1*coupL2*B1m2 
sumL = coupR1*coupR2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
End if 
!------------------------ 
! VZ, Fxe 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopFxe)) Then 
SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -2._dp*Real(SA_B1(p2,MFxe2,MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFxe*Real(SA_B0(p2,MFxe2,MVZ2)-0.5_dp*rMS,dp) 
coupL1 = cplcFxeFxeVZL
coupR1 = cplcFxeFxeVZR
coupL2 =  Conjg(cplcFxeFxeVZL)
coupR2 =  Conjg(cplcFxeFxeVZR)
SumSR = coupR1*coupL2*B0m2 
SumSL = coupL1*coupR2*B0m2 
sumR = coupL1*coupL2*B1m2 
sumL = coupR1*coupR2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
End if 
!------------------------ 
! conj[Hp], Fxv 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopFxv)) Then 
      Do i2 = 1, 2
 SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -Real(SA_B1(p2,MFxv2(i2),MHp2),dp) 
B0m2 = MFxv(i2)*Real(SA_B0(p2,MFxv2(i2),MHp2),dp) 
coupL1 = cplcFxeFxvcHpL(i2)
coupR1 = cplcFxeFxvcHpR(i2)
coupL2 =  Conjg(cplcFxeFxvcHpL(i2))
coupR2 =  Conjg(cplcFxeFxvcHpR(i2))
SumSL = coupR1*coupL2*B0m2 
SumSR = coupL1*coupR2*B0m2 
sumR = coupR1*coupR2*B1m2 
sumL = coupL1*coupL2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
    End Do 
 End if 
!------------------------ 
! conj[VWp], Fxv 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopFxv)) Then 
      Do i2 = 1, 2
 SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -2._dp*Real(SA_B1(p2,MFxv2(i2),MVWp2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFxv(i2)*Real(SA_B0(p2,MFxv2(i2),MVWp2)-0.5_dp*rMS,dp) 
coupL1 = cplcFxeFxvcVWpL(i2)
coupR1 = cplcFxeFxvcVWpR(i2)
coupL2 =  Conjg(cplcFxeFxvcVWpL(i2))
coupR2 =  Conjg(cplcFxeFxvcVWpR(i2))
SumSR = coupR1*coupL2*B0m2 
SumSL = coupL1*coupR2*B0m2 
sumR = coupL1*coupL2*B1m2 
sumL = coupR1*coupR2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
    End Do 
 End if 


SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
SigR = oo16pi2*SigR 
SigL = oo16pi2*SigL 
 
End Subroutine Sigma1LoopFxe 
 
Subroutine DerSigma1LoopFxe(p2,MSsc,MSsc2,MFe,MFe2,MFxe,MFxe2,MVZ,MVZ2,               & 
& MHp,MHp2,MFxv,MFxv2,MVWp,MVWp2,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVPL,              & 
& cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,               & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MSsc(2),MSsc2(2),MFe(3),MFe2(3),MFxe,MFxe2,MVZ,MVZ2,MHp,MHp2,MFxv(2),MFxv2(2),        & 
& MVWp,MVWp2

Complex(dp), Intent(in) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,      & 
& cplcFxeFxeVZR,cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2)

Complex(dp), Intent(out) :: SigL, SigR, SigSL, SigSR 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL, sumR, sumSL, sumSR 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
SigSR = Cmplx(0._dp,0._dp,dp) 
!------------------------ 
! Ssc, Fe 
!------------------------ 
If ((Include_in_loopSsc).and.(Include_in_loopFe)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -Real(SA_DerB1(p2,MFe2(i2),MSsc2(i1)),dp) 
B0m2 = MFe(i2)*Real(SA_DerB0(p2,MFe2(i2),MSsc2(i1)),dp) 
coupL1 = cplcFxeFeSscL(i2,i1)
coupR1 = cplcFxeFeSscR(i2,i1)
coupL2 =  Conjg(cplcFxeFeSscL(i2,i1))
coupR2 =  Conjg(cplcFxeFeSscR(i2,i1))
SumSL = coupR1*coupL2*B0m2 
SumSR = coupL1*coupR2*B0m2 
sumR = coupR1*coupR2*B1m2 
sumL = coupL1*coupL2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
      End Do 
     End Do 
 End if 
!------------------------ 
! VP, Fxe 
!------------------------ 
If ((Include_in_loopVP).and.(Include_in_loopFxe)) Then 
SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -2._dp*Real(SA_DerB1(p2,MFxe2,MVP2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFxe*Real(SA_DerB0(p2,MFxe2,MVP2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcFxeFxeVPL
coupR1 = cplcFxeFxeVPR
coupL2 =  Conjg(cplcFxeFxeVPL)
coupR2 =  Conjg(cplcFxeFxeVPR)
SumSR = coupR1*coupL2*B0m2 
SumSL = coupL1*coupR2*B0m2 
sumR = coupL1*coupL2*B1m2 
sumL = coupR1*coupR2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
End if 
!------------------------ 
! VZ, Fxe 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopFxe)) Then 
SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -2._dp*Real(SA_DerB1(p2,MFxe2,MVZ2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFxe*Real(SA_DerB0(p2,MFxe2,MVZ2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcFxeFxeVZL
coupR1 = cplcFxeFxeVZR
coupL2 =  Conjg(cplcFxeFxeVZL)
coupR2 =  Conjg(cplcFxeFxeVZR)
SumSR = coupR1*coupL2*B0m2 
SumSL = coupL1*coupR2*B0m2 
sumR = coupL1*coupL2*B1m2 
sumL = coupR1*coupR2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
End if 
!------------------------ 
! conj[Hp], Fxv 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopFxv)) Then 
      Do i2 = 1, 2
 SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -Real(SA_DerB1(p2,MFxv2(i2),MHp2),dp) 
B0m2 = MFxv(i2)*Real(SA_DerB0(p2,MFxv2(i2),MHp2),dp) 
coupL1 = cplcFxeFxvcHpL(i2)
coupR1 = cplcFxeFxvcHpR(i2)
coupL2 =  Conjg(cplcFxeFxvcHpL(i2))
coupR2 =  Conjg(cplcFxeFxvcHpR(i2))
SumSL = coupR1*coupL2*B0m2 
SumSR = coupL1*coupR2*B0m2 
sumR = coupR1*coupR2*B1m2 
sumL = coupL1*coupL2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
    End Do 
 End if 
!------------------------ 
! conj[VWp], Fxv 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopFxv)) Then 
      Do i2 = 1, 2
 SumSL = 0._dp 
sumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
B1m2 = -2._dp*Real(SA_DerB1(p2,MFxv2(i2),MVWp2)+ 0.5_dp*DerrMS,dp) 
B0m2 = -4._dp*MFxv(i2)*Real(SA_DerB0(p2,MFxv2(i2),MVWp2)-0.5_dp*DerrMS,dp) 
coupL1 = cplcFxeFxvcVWpL(i2)
coupR1 = cplcFxeFxvcVWpR(i2)
coupL2 =  Conjg(cplcFxeFxvcVWpL(i2))
coupR2 =  Conjg(cplcFxeFxvcVWpR(i2))
SumSR = coupR1*coupL2*B0m2 
SumSL = coupL1*coupR2*B0m2 
sumR = coupL1*coupL2*B1m2 
sumL = coupR1*coupR2*B1m2 
SigL = SigL +1._dp*sumL 
SigR = SigR +1._dp*sumR 
SigSL = SigSL +1._dp*sumSL 
SigSR = SigSR +1._dp*sumSR 
    End Do 
 End if 


SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
SigR = oo16pi2*SigR 
SigL = oo16pi2*SigL 
 
End Subroutine DerSigma1LoopFxe 
 
Subroutine OneLoopHp(g2,m2SM,Lam,LSPH,vvSM,vS,MVWp,MVWp2,MAh,MAh2,MFd,MFd2,           & 
& MFu,MFu2,MFe,MFe2,MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,MHp,MHp2,Mhh,Mhh2,MVZ,MVZ2,           & 
& MSsc,MSsc2,cplAhcHpVWp,cplcFdFucHpL,cplcFdFucHpR,cplcFeFvcHpL,cplcFeFvcHpR,            & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcgZgWpcHp,cplcgWpgZHp,cplcgWCgZcHp,cplcgZgWCHp,       & 
& cplhhHpcHp,cplhhcHpVWp,cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplAhAhHpcHp,     & 
& cplhhhhHpcHp,cplHpHpcHpcHp,cplHpSsccHpcSsc,cplHpcHpVPVP,cplHpcHpcVWpVWp,               & 
& cplHpcHpVZVZ,delta,mass,mass2,kont)

Real(dp), Intent(in) :: MVWp,MVWp2,MAh,MAh2,MFd(3),MFd2(3),MFu(3),MFu2(3),MFe(3),MFe2(3),MFv(3),              & 
& MFv2(3),MFxe,MFxe2,MFxv(2),MFxv2(2),MHp,MHp2,Mhh(2),Mhh2(2),MVZ,MVZ2,MSsc(2),MSsc2(2)

Real(dp), Intent(in) :: g2,m2SM,LSPH,vvSM,vS

Complex(dp), Intent(in) :: Lam

Complex(dp), Intent(in) :: cplAhcHpVWp,cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),  & 
& cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcgZgWpcHp,cplcgWpgZHp,cplcgWCgZcHp,             & 
& cplcgZgWCHp,cplhhHpcHp(2),cplhhcHpVWp(2),cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,            & 
& cplcHpVWpVZ,cplAhAhHpcHp,cplhhhhHpcHp(2,2),cplHpHpcHpcHp,cplHpSsccHpcSsc(2,2),         & 
& cplHpcHpVPVP,cplHpcHpcVWpVWp,cplHpcHpVZVZ

Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi, mi2, p2, test_m2 
Complex(dp) :: PiSf, SigL, SigR, SigSL, SigSR 
Real(dp), Intent(out) :: mass, mass2 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopHp'
 
mi2 = (-4._dp*(m2SM) + 2*Lam*vvSM**2 + 2*LSPH*vS**2 + g2**2*vvSM**2*RXiWp)/4._dp 

 
p2 = 0._dp 
PiSf = ZeroC 
Call Pi1LoopHp(p2,MVWp,MVWp2,MAh,MAh2,MFd,MFd2,MFu,MFu2,MFe,MFe2,MFv,MFv2,            & 
& MFxe,MFxe2,MFxv,MFxv2,MHp,MHp2,Mhh,Mhh2,MVZ,MVZ2,MSsc,MSsc2,cplAhcHpVWp,               & 
& cplcFdFucHpL,cplcFdFucHpR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,     & 
& cplcgZgWpcHp,cplcgWpgZHp,cplcgWCgZcHp,cplcgZgWCHp,cplhhHpcHp,cplhhcHpVWp,              & 
& cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplAhAhHpcHp,cplhhhhHpcHp,               & 
& cplHpHpcHpcHp,cplHpSsccHpcSsc,cplHpcHpVPVP,cplHpcHpcVWpVWp,cplHpcHpVZVZ,               & 
& kont,PiSf)

mass2 = mi2 - Real(PiSf,dp) 
mass = sqrt(mass2) 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
p2 =  mass2 
PiSf = ZeroC 
Call Pi1LoopHp(p2,MVWp,MVWp2,MAh,MAh2,MFd,MFd2,MFu,MFu2,MFe,MFe2,MFv,MFv2,            & 
& MFxe,MFxe2,MFxv,MFxv2,MHp,MHp2,Mhh,Mhh2,MVZ,MVZ2,MSsc,MSsc2,cplAhcHpVWp,               & 
& cplcFdFucHpL,cplcFdFucHpR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,     & 
& cplcgZgWpcHp,cplcgWpgZHp,cplcgWCgZcHp,cplcgZgWCHp,cplhhHpcHp,cplhhcHpVWp,              & 
& cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplAhAhHpcHp,cplhhhhHpcHp,               & 
& cplHpHpcHpcHp,cplHpSsccHpcSsc,cplHpcHpVPVP,cplHpcHpcVWpVWp,cplHpcHpVZVZ,               & 
& kont,PiSf)

mass2 = mi2 - Real(PiSf,dp) 
mass = sqrt(mass2) 
 If (test_m2.Ne.0._dp) Then 
    test_m2 = Abs(test_m2 - mass2) / test_m2 
 Else 
    test_m2 = Abs(mass2) 
 End If 
 If (mass2.Ge.0._dp) Then 
   If (RotateNegativeFermionMasses) Then 
    mass = sqrt(mass2) 
   End if 
  Else 
 If (Abs(mass2).lt.1.0E-30_dp) test_m2 = 0._dp 
     Write(ErrCan,*) 'Warning from routine'//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
   SignOfMassChanged = .True. 
   mass = 0._dp 
  End If 
If (test_m2.LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoopHp
 
 
Subroutine Pi1LoopHp(p2,MVWp,MVWp2,MAh,MAh2,MFd,MFd2,MFu,MFu2,MFe,MFe2,               & 
& MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,MHp,MHp2,Mhh,Mhh2,MVZ,MVZ2,MSsc,MSsc2,cplAhcHpVWp,      & 
& cplcFdFucHpL,cplcFdFucHpR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,     & 
& cplcgZgWpcHp,cplcgWpgZHp,cplcgWCgZcHp,cplcgZgWCHp,cplhhHpcHp,cplhhcHpVWp,              & 
& cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplAhAhHpcHp,cplhhhhHpcHp,               & 
& cplHpHpcHpcHp,cplHpSsccHpcSsc,cplHpcHpVPVP,cplHpcHpcVWpVWp,cplHpcHpVZVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MVWp,MVWp2,MAh,MAh2,MFd(3),MFd2(3),MFu(3),MFu2(3),MFe(3),MFe2(3),MFv(3),              & 
& MFv2(3),MFxe,MFxe2,MFxv(2),MFxv2(2),MHp,MHp2,Mhh(2),Mhh2(2),MVZ,MVZ2,MSsc(2),MSsc2(2)

Complex(dp), Intent(in) :: cplAhcHpVWp,cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),  & 
& cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcgZgWpcHp,cplcgWpgZHp,cplcgWCgZcHp,             & 
& cplcgZgWCHp,cplhhHpcHp(2),cplhhcHpVWp(2),cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,            & 
& cplcHpVWpVZ,cplAhAhHpcHp,cplhhhhHpcHp(2,2),cplHpHpcHpcHp,cplHpSsccHpcSsc(2,2),         & 
& cplHpcHpVPVP,cplHpcHpcVWpVWp,cplHpcHpVZVZ

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumI 
Integer :: i1,i2,i3,i4,ierr 
 
 
res = 0._dp 
 
!------------------------ 
! VWp, Ah 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loopAh)) Then 
F0m2 = Real(FloopRXi(p2,MAh2,MVWp2),dp) 
coup1 = cplAhcHpVWp
coup2 =  Conjg(cplAhcHpVWp)
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[Fd], Fu 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFd).and.(Include_in_loopFu)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = SA_Gloop(p2,MFd2(i1),MFu2(i2)) 
B0m2 = -2._dp*MFd(i1)*MFu(i2)*SA_B0(p2,MFd2(i1),MFu2(i2)) 
coupL1 = cplcFdFucHpL(i1,i2)
coupR1 = cplcFdFucHpR(i1,i2)
coupL2 =  Conjg(cplcFdFucHpL(i1,i2))
coupR2 =  Conjg(cplcFdFucHpR(i1,i2))
    SumI = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fe], Fv 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFe).and.(Include_in_loopFv)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = SA_Gloop(p2,MFe2(i1),MFv2(i2)) 
B0m2 = -2._dp*MFe(i1)*MFv(i2)*SA_B0(p2,MFe2(i1),MFv2(i2)) 
coupL1 = cplcFeFvcHpL(i1,i2)
coupR1 = cplcFeFvcHpR(i1,i2)
coupL2 =  Conjg(cplcFeFvcHpL(i1,i2))
coupR2 =  Conjg(cplcFeFvcHpR(i1,i2))
    SumI = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxe], Fxv 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFxe).and.(Include_in_loopFxv)) Then 
      Do i2 = 1, 2
 G0m2 = SA_Gloop(p2,MFxe2,MFxv2(i2)) 
B0m2 = -2._dp*MFxe*MFxv(i2)*SA_B0(p2,MFxe2,MFxv2(i2)) 
coupL1 = cplcFxeFxvcHpL(i2)
coupR1 = cplcFxeFxvcHpR(i2)
coupL2 =  Conjg(cplcFxeFxvcHpL(i2))
coupR2 =  Conjg(cplcFxeFxvcHpR(i2))
    SumI = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! bar[gZ], gWp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopgZ).and.(Include_in_loopgWp)) Then 
F0m2 =  -Real(SA_B0(p2,MVWp2*RXi,MVZ2*RXi),dp) 
 coup1 = cplcgZgWpcHp
coup2 =  cplcgWpgZHp 
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWpC], gZ 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopgWC).and.(Include_in_loopgZ)) Then 
F0m2 =  -Real(SA_B0(p2,MVZ2*RXi,MVWp2*RXi),dp) 
 coup1 = cplcgWCgZcHp
coup2 =  cplcgZgWCHp 
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! Hp, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopHp).and.(Include_in_loophh)) Then 
      Do i2 = 1, 2
 B0m2 = Real(SA_B0(p2,MHp2,Mhh2(i2)),dp) 
coup1 = cplhhHpcHp(i2)
coup2 = Conjg(cplhhHpcHp(i2))
    SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! VWp, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loophh)) Then 
      Do i2 = 1, 2
 F0m2 = Real(FloopRXi(p2,Mhh2(i2),MVWp2),dp) 
coup1 = cplhhcHpVWp(i2)
coup2 =  Conjg(cplhhcHpVWp(i2))
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! VP, Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVP).and.(Include_in_loopHp)) Then 
F0m2 = Real(FloopRXi(p2,MHp2,0._dp),dp) 
coup1 = cplHpcHpVP
coup2 =  Conjg(cplHpcHpVP)
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VZ, Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVZ).and.(Include_in_loopHp)) Then 
F0m2 = Real(FloopRXi(p2,MHp2,MVZ2),dp) 
coup1 = cplHpcHpVZ
coup2 =  Conjg(cplHpcHpVZ)
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VWp, VP 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loopVP)) Then 
F0m2 = Real(SVVloop(p2,0._dp,MVWp2),dp) 
coup1 = cplcHpVPVWp
coup2 =  Conjg(cplcHpVPVWp)
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VZ, VWp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
F0m2 = Real(SVVloop(p2,MVWp2,MVZ2),dp) 
coup1 = cplcHpVWpVZ
coup2 =  Conjg(cplcHpVWpVZ)
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
A0m2 = SA_A0(MAh2) 
coup1 = cplAhAhHpcHp
    SumI = -coup1*A0m2 
res = res +1._dp/2._dp* SumI  
End if 
!------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
    Do i1 = 1, 2
 A0m2 = SA_A0(Mhh2(i1)) 
coup1 = cplhhhhHpcHp(i1,i1)
    SumI = -coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 End if 
!------------------------ 
! conj[Hp], Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
A0m2 = SA_A0(MHp2) 
coup1 = cplHpHpcHpcHp
    SumI = -coup1*A0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Ssc], Ssc 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
    Do i1 = 1, 2
 A0m2 = SA_A0(MSsc2(i1)) 
coup1 = cplHpSsccHpcSsc(i1,i1)
    SumI = -coup1*A0m2 
res = res +1._dp* SumI  
      End Do 
 End if 
!------------------------ 
! VP, VP 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVP).and.(Include_in_loopVP)) Then 
A0m2 =  0.75_dp*SA_A0(0._dp) + 0.25_dp*RXi*SA_A0(0._dp*RXi) - 0.5_dp*0._dp*rMS 
coup1 = cplHpcHpVPVP
    SumI = coup1*A0m2 
res = res +2._dp* SumI  
End if 
!------------------------ 
! conj[VWp], VWp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
A0m2 =  0.75_dp*SA_A0(MVWp2) + 0.25_dp*RXi*SA_A0(MVWp2*RXi) - 0.5_dp*MVWp2*rMS 
coup1 = cplHpcHpcVWpVWp
    SumI = coup1*A0m2 
res = res +4._dp* SumI  
End if 
!------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
A0m2 =  0.75_dp*SA_A0(MVZ2) + 0.25_dp*RXi*SA_A0(MVZ2*RXi) - 0.5_dp*MVZ2*rMS 
coup1 = cplHpcHpVZVZ
    SumI = coup1*A0m2 
res = res +2._dp* SumI  
End if 


res = oo16pi2*res 
 
End Subroutine Pi1LoopHp 
 
Subroutine DerPi1LoopHp(p2,MVWp,MVWp2,MAh,MAh2,MFd,MFd2,MFu,MFu2,MFe,MFe2,            & 
& MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,MHp,MHp2,Mhh,Mhh2,MVZ,MVZ2,MSsc,MSsc2,cplAhcHpVWp,      & 
& cplcFdFucHpL,cplcFdFucHpR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,     & 
& cplcgZgWpcHp,cplcgWpgZHp,cplcgWCgZcHp,cplcgZgWCHp,cplhhHpcHp,cplhhcHpVWp,              & 
& cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplAhAhHpcHp,cplhhhhHpcHp,               & 
& cplHpHpcHpcHp,cplHpSsccHpcSsc,cplHpcHpVPVP,cplHpcHpcVWpVWp,cplHpcHpVZVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MVWp,MVWp2,MAh,MAh2,MFd(3),MFd2(3),MFu(3),MFu2(3),MFe(3),MFe2(3),MFv(3),              & 
& MFv2(3),MFxe,MFxe2,MFxv(2),MFxv2(2),MHp,MHp2,Mhh(2),Mhh2(2),MVZ,MVZ2,MSsc(2),MSsc2(2)

Complex(dp), Intent(in) :: cplAhcHpVWp,cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),  & 
& cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcgZgWpcHp,cplcgWpgZHp,cplcgWCgZcHp,             & 
& cplcgZgWCHp,cplhhHpcHp(2),cplhhcHpVWp(2),cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,            & 
& cplcHpVWpVZ,cplAhAhHpcHp,cplhhhhHpcHp(2,2),cplHpHpcHpcHp,cplHpSsccHpcSsc(2,2),         & 
& cplHpcHpVPVP,cplHpcHpcVWpVWp,cplHpcHpVZVZ

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumI 
Integer :: i1,i2,i3,i4,ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! VWp, Ah 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loopAh)) Then 
F0m2 = Real(DerFloopRXi(p2,MAh2,MVWp2),dp) 
coup1 = cplAhcHpVWp
coup2 =  Conjg(cplAhcHpVWp)
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[Fd], Fu 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFd).and.(Include_in_loopFu)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = SA_DerGloop(p2,MFd2(i1),MFu2(i2)) 
B0m2 = -2._dp*MFd(i1)*MFu(i2)*SA_DerB0(p2,MFd2(i1),MFu2(i2)) 
coupL1 = cplcFdFucHpL(i1,i2)
coupR1 = cplcFdFucHpR(i1,i2)
coupL2 =  Conjg(cplcFdFucHpL(i1,i2))
coupR2 =  Conjg(cplcFdFucHpR(i1,i2))
    SumI = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fe], Fv 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFe).and.(Include_in_loopFv)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = SA_DerGloop(p2,MFe2(i1),MFv2(i2)) 
B0m2 = -2._dp*MFe(i1)*MFv(i2)*SA_DerB0(p2,MFe2(i1),MFv2(i2)) 
coupL1 = cplcFeFvcHpL(i1,i2)
coupR1 = cplcFeFvcHpR(i1,i2)
coupL2 =  Conjg(cplcFeFvcHpL(i1,i2))
coupR2 =  Conjg(cplcFeFvcHpR(i1,i2))
    SumI = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxe], Fxv 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFxe).and.(Include_in_loopFxv)) Then 
      Do i2 = 1, 2
 G0m2 = SA_DerGloop(p2,MFxe2,MFxv2(i2)) 
B0m2 = -2._dp*MFxe*MFxv(i2)*SA_DerB0(p2,MFxe2,MFxv2(i2)) 
coupL1 = cplcFxeFxvcHpL(i2)
coupR1 = cplcFxeFxvcHpR(i2)
coupL2 =  Conjg(cplcFxeFxvcHpL(i2))
coupR2 =  Conjg(cplcFxeFxvcHpR(i2))
    SumI = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! bar[gZ], gWp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopgZ).and.(Include_in_loopgWp)) Then 
F0m2 =  -Real(SA_DerB0(p2,MVWp2*RXi,MVZ2*RXi),dp) 
 coup1 = cplcgZgWpcHp
coup2 =  cplcgWpgZHp 
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWpC], gZ 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopgWC).and.(Include_in_loopgZ)) Then 
F0m2 =  -Real(SA_DerB0(p2,MVZ2*RXi,MVWp2*RXi),dp) 
 coup1 = cplcgWCgZcHp
coup2 =  cplcgZgWCHp 
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! Hp, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopHp).and.(Include_in_loophh)) Then 
      Do i2 = 1, 2
 B0m2 = Real(SA_DerB0(p2,MHp2,Mhh2(i2)),dp) 
coup1 = cplhhHpcHp(i2)
coup2 = Conjg(cplhhHpcHp(i2))
    SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! VWp, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loophh)) Then 
      Do i2 = 1, 2
 F0m2 = Real(DerFloopRXi(p2,Mhh2(i2),MVWp2),dp) 
coup1 = cplhhcHpVWp(i2)
coup2 =  Conjg(cplhhcHpVWp(i2))
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! VP, Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVP).and.(Include_in_loopHp)) Then 
F0m2 = Real(DerFloopRXi(p2,MHp2,MVP2),dp) 
coup1 = cplHpcHpVP
coup2 =  Conjg(cplHpcHpVP)
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VZ, Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVZ).and.(Include_in_loopHp)) Then 
F0m2 = Real(DerFloopRXi(p2,MHp2,MVZ2),dp) 
coup1 = cplHpcHpVZ
coup2 =  Conjg(cplHpcHpVZ)
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VWp, VP 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loopVP)) Then 
F0m2 = Real(DerSVVloop(p2,MVP2,MVWp2),dp) 
coup1 = cplcHpVPVWp
coup2 =  Conjg(cplcHpVPVWp)
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VZ, VWp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
F0m2 = Real(DerSVVloop(p2,MVWp2,MVZ2),dp) 
coup1 = cplcHpVWpVZ
coup2 =  Conjg(cplcHpVWpVZ)
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
A0m2 = SA_DerA0(MAh2) 
coup1 = cplAhAhHpcHp
    SumI = -coup1*A0m2 
res = res +1._dp/2._dp* SumI  
End if 
!------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
    Do i1 = 1, 2
 A0m2 = SA_DerA0(Mhh2(i1)) 
coup1 = cplhhhhHpcHp(i1,i1)
    SumI = -coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 End if 
!------------------------ 
! conj[Hp], Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
A0m2 = SA_DerA0(MHp2) 
coup1 = cplHpHpcHpcHp
    SumI = -coup1*A0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Ssc], Ssc 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
    Do i1 = 1, 2
 A0m2 = SA_DerA0(MSsc2(i1)) 
coup1 = cplHpSsccHpcSsc(i1,i1)
    SumI = -coup1*A0m2 
res = res +1._dp* SumI  
      End Do 
 End if 
!------------------------ 
! VP, VP 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVP).and.(Include_in_loopVP)) Then 
A0m2 =  0.75_dp*SA_DerA0(MVP2) + 0.25_dp*RXi*SA_DerA0(MVP2*RXi) - 0.5_dp*MVP2*DerrMS 
coup1 = cplHpcHpVPVP
    SumI = coup1*A0m2 
res = res +2._dp* SumI  
End if 
!------------------------ 
! conj[VWp], VWp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
A0m2 =  0.75_dp*SA_DerA0(MVWp2) + 0.25_dp*RXi*SA_DerA0(MVWp2*RXi) - 0.5_dp*MVWp2*DerrMS 
coup1 = cplHpcHpcVWpVWp
    SumI = coup1*A0m2 
res = res +4._dp* SumI  
End if 
!------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
A0m2 =  0.75_dp*SA_DerA0(MVZ2) + 0.25_dp*RXi*SA_DerA0(MVZ2*RXi) - 0.5_dp*MVZ2*DerrMS 
coup1 = cplHpcHpVZVZ
    SumI = coup1*A0m2 
res = res +2._dp* SumI  
End if 


res = oo16pi2*res 
 
End Subroutine DerPi1LoopHp 
 
Subroutine OneLoopAh(g1,g2,m2SM,Lam,LSPH,vvSM,vS,TW,Mhh,Mhh2,MAh,MAh2,MFd,            & 
& MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,MVZ,MVZ2,MVWp,MVWp2,MHp,MHp2,MSsc,MSsc2,             & 
& cplAhAhhh,cplcFdFdAhL,cplcFdFdAhR,cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,     & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ,cplAhHpcVWp,           & 
& cplAhAhAhAh,cplAhAhhhhh,cplAhAhHpcHp,cplAhAhSsccSsc,cplAhAhcVWpVWp,cplAhAhVZVZ,        & 
& delta,mass,mass2,kont)

Real(dp), Intent(in) :: Mhh(2),Mhh2(2),MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFxv(2),         & 
& MFxv2(2),MVZ,MVZ2,MVWp,MVWp2,MHp,MHp2,MSsc(2),MSsc2(2)

Real(dp), Intent(in) :: g1,g2,m2SM,LSPH,vvSM,vS,TW

Complex(dp), Intent(in) :: Lam

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),     & 
& cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),               & 
& cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ(2),cplAhHpcVWp,cplAhAhAhAh,cplAhAhhhhh(2,2),       & 
& cplAhAhHpcHp,cplAhAhSsccSsc(2,2),cplAhAhcVWpVWp,cplAhAhVZVZ

Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi, mi2, p2, test_m2 
Complex(dp) :: PiSf, SigL, SigR, SigSL, SigSR 
Real(dp), Intent(out) :: mass, mass2 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopAh'
 
mi2 = (2*(-2._dp*(m2SM) + Lam*vvSM**2 + LSPH*vS**2) + vvSM**2*RXiZ*(g2*Cos(TW) + g1*Sin(TW))**2)/4._dp 

 
p2 = 0._dp 
PiSf = ZeroC 
Call Pi1LoopAh(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,            & 
& MVZ,MVZ2,MVWp,MVWp2,MHp,MHp2,MSsc,MSsc2,cplAhAhhh,cplcFdFdAhL,cplcFdFdAhR,             & 
& cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplcFxvFxvAhL,cplcFxvFxvAhR,           & 
& cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ,cplAhHpcVWp,cplAhAhAhAh,cplAhAhhhhh,               & 
& cplAhAhHpcHp,cplAhAhSsccSsc,cplAhAhcVWpVWp,cplAhAhVZVZ,kont,PiSf)

PiSf=PiSf- real(pip2s_effpot(1,1),dp)
mass2 = mi2 - Real(PiSf,dp) 
mass = sqrt(mass2) 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
p2 =  mass2 
PiSf = ZeroC 
Call Pi1LoopAh(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,            & 
& MVZ,MVZ2,MVWp,MVWp2,MHp,MHp2,MSsc,MSsc2,cplAhAhhh,cplcFdFdAhL,cplcFdFdAhR,             & 
& cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplcFxvFxvAhL,cplcFxvFxvAhR,           & 
& cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ,cplAhHpcVWp,cplAhAhAhAh,cplAhAhhhhh,               & 
& cplAhAhHpcHp,cplAhAhSsccSsc,cplAhAhcVWpVWp,cplAhAhVZVZ,kont,PiSf)

PiSf=PiSf- real(pip2s_effpot(1,1),dp)
mass2 = mi2 - Real(PiSf,dp) 
mass = sqrt(mass2) 
 If (test_m2.Ne.0._dp) Then 
    test_m2 = Abs(test_m2 - mass2) / test_m2 
 Else 
    test_m2 = Abs(mass2) 
 End If 
 If (mass2.Ge.0._dp) Then 
   If (RotateNegativeFermionMasses) Then 
    mass = sqrt(mass2) 
   End if 
  Else 
 If (Abs(mass2).lt.1.0E-30_dp) test_m2 = 0._dp 
     Write(ErrCan,*) 'Warning from routine'//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
   SignOfMassChanged = .True. 
   mass = 0._dp 
  End If 
If (test_m2.LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoopAh
 
 
Subroutine Pi1LoopAh(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,            & 
& MFxv2,MVZ,MVZ2,MVWp,MVWp2,MHp,MHp2,MSsc,MSsc2,cplAhAhhh,cplcFdFdAhL,cplcFdFdAhR,       & 
& cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplcFxvFxvAhL,cplcFxvFxvAhR,           & 
& cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ,cplAhHpcVWp,cplAhAhAhAh,cplAhAhhhhh,               & 
& cplAhAhHpcHp,cplAhAhSsccSsc,cplAhAhcVWpVWp,cplAhAhVZVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: Mhh(2),Mhh2(2),MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFxv(2),         & 
& MFxv2(2),MVZ,MVZ2,MVWp,MVWp2,MHp,MHp2,MSsc(2),MSsc2(2)

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),     & 
& cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),               & 
& cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ(2),cplAhHpcVWp,cplAhAhAhAh,cplAhAhhhhh(2,2),       & 
& cplAhAhHpcHp,cplAhAhSsccSsc(2,2),cplAhAhcVWpVWp,cplAhAhVZVZ

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumI 
Integer :: i1,i2,i3,i4,ierr 
 
 
res = 0._dp 
 
!------------------------ 
! hh, Ah 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loophh).and.(Include_in_loopAh)) Then 
    Do i1 = 1, 2
 B0m2 = Real(SA_B0(p2,Mhh2(i1),MAh2),dp) 
coup1 = cplAhAhhh(i1)
coup2 = Conjg(cplAhAhhh(i1))
    SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
      End Do 
 End if 
!------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFd).and.(Include_in_loopFd)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = SA_Gloop(p2,MFd2(i1),MFd2(i2)) 
B0m2 = -2._dp*MFd(i1)*MFd(i2)*SA_B0(p2,MFd2(i1),MFd2(i2)) 
coupL1 = cplcFdFdAhL(i1,i2)
coupR1 = cplcFdFdAhR(i1,i2)
coupL2 =  Conjg(cplcFdFdAhL(i1,i2))
coupR2 =  Conjg(cplcFdFdAhR(i1,i2))
    SumI = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fe], Fe 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFe).and.(Include_in_loopFe)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = SA_Gloop(p2,MFe2(i1),MFe2(i2)) 
B0m2 = -2._dp*MFe(i1)*MFe(i2)*SA_B0(p2,MFe2(i1),MFe2(i2)) 
coupL1 = cplcFeFeAhL(i1,i2)
coupR1 = cplcFeFeAhR(i1,i2)
coupL2 =  Conjg(cplcFeFeAhL(i1,i2))
coupR2 =  Conjg(cplcFeFeAhR(i1,i2))
    SumI = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFu).and.(Include_in_loopFu)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = SA_Gloop(p2,MFu2(i1),MFu2(i2)) 
B0m2 = -2._dp*MFu(i1)*MFu(i2)*SA_B0(p2,MFu2(i1),MFu2(i2)) 
coupL1 = cplcFuFuAhL(i1,i2)
coupR1 = cplcFuFuAhR(i1,i2)
coupL2 =  Conjg(cplcFuFuAhL(i1,i2))
coupR2 =  Conjg(cplcFuFuAhR(i1,i2))
    SumI = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxv], Fxv 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 2
 G0m2 = SA_Gloop(p2,MFxv2(i1),MFxv2(i2)) 
B0m2 = -2._dp*MFxv(i1)*MFxv(i2)*SA_B0(p2,MFxv2(i1),MFxv2(i2)) 
coupL1 = cplcFxvFxvAhL(i1,i2)
coupR1 = cplcFxvFxvAhR(i1,i2)
coupL2 =  Conjg(cplcFxvFxvAhL(i1,i2))
coupR2 =  Conjg(cplcFxvFxvAhR(i1,i2))
    SumI = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[gWp], gWp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
F0m2 =  -Real(SA_B0(p2,MVWp2*RXi,MVWp2*RXi),dp) 
 coup1 = cplcgWpgWpAh
coup2 =  cplcgWpgWpAh 
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWpC], gWpC 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
F0m2 =  -Real(SA_B0(p2,MVWp2*RXi,MVWp2*RXi),dp) 
 coup1 = cplcgWCgWCAh
coup2 =  cplcgWCgWCAh 
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VZ, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVZ).and.(Include_in_loophh)) Then 
      Do i2 = 1, 2
 F0m2 = Real(FloopRXi(p2,Mhh2(i2),MVZ2),dp) 
coup1 = cplAhhhVZ(i2)
coup2 =  Conjg(cplAhhhVZ(i2))
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! conj[VWp], Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
F0m2 = Real(FloopRXi(p2,MHp2,MVWp2),dp) 
coup1 = cplAhHpcVWp
coup2 =  Conjg(cplAhHpcVWp)
    SumI = coup1*coup2*F0m2 
res = res +2._dp* SumI  
End if 
!------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
A0m2 = SA_A0(MAh2) 
coup1 = cplAhAhAhAh
    SumI = -coup1*A0m2 
res = res +1._dp/2._dp* SumI  
End if 
!------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
    Do i1 = 1, 2
 A0m2 = SA_A0(Mhh2(i1)) 
coup1 = cplAhAhhhhh(i1,i1)
    SumI = -coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 End if 
!------------------------ 
! conj[Hp], Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
A0m2 = SA_A0(MHp2) 
coup1 = cplAhAhHpcHp
    SumI = -coup1*A0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Ssc], Ssc 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
    Do i1 = 1, 2
 A0m2 = SA_A0(MSsc2(i1)) 
coup1 = cplAhAhSsccSsc(i1,i1)
    SumI = -coup1*A0m2 
res = res +1._dp* SumI  
      End Do 
 End if 
!------------------------ 
! conj[VWp], VWp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
A0m2 =  0.75_dp*SA_A0(MVWp2) + 0.25_dp*RXi*SA_A0(MVWp2*RXi) - 0.5_dp*MVWp2*rMS 
coup1 = cplAhAhcVWpVWp
    SumI = coup1*A0m2 
res = res +4._dp* SumI  
End if 
!------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
A0m2 =  0.75_dp*SA_A0(MVZ2) + 0.25_dp*RXi*SA_A0(MVZ2*RXi) - 0.5_dp*MVZ2*rMS 
coup1 = cplAhAhVZVZ
    SumI = coup1*A0m2 
res = res +2._dp* SumI  
End if 


res = oo16pi2*res 
 
End Subroutine Pi1LoopAh 
 
Subroutine DerPi1LoopAh(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,              & 
& MFxv,MFxv2,MVZ,MVZ2,MVWp,MVWp2,MHp,MHp2,MSsc,MSsc2,cplAhAhhh,cplcFdFdAhL,              & 
& cplcFdFdAhR,cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplcFxvFxvAhL,             & 
& cplcFxvFxvAhR,cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ,cplAhHpcVWp,cplAhAhAhAh,             & 
& cplAhAhhhhh,cplAhAhHpcHp,cplAhAhSsccSsc,cplAhAhcVWpVWp,cplAhAhVZVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: Mhh(2),Mhh2(2),MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFxv(2),         & 
& MFxv2(2),MVZ,MVZ2,MVWp,MVWp2,MHp,MHp2,MSsc(2),MSsc2(2)

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),     & 
& cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),               & 
& cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ(2),cplAhHpcVWp,cplAhAhAhAh,cplAhAhhhhh(2,2),       & 
& cplAhAhHpcHp,cplAhAhSsccSsc(2,2),cplAhAhcVWpVWp,cplAhAhVZVZ

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumI 
Integer :: i1,i2,i3,i4,ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! hh, Ah 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loophh).and.(Include_in_loopAh)) Then 
    Do i1 = 1, 2
 B0m2 = Real(SA_DerB0(p2,Mhh2(i1),MAh2),dp) 
coup1 = cplAhAhhh(i1)
coup2 = Conjg(cplAhAhhh(i1))
    SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
      End Do 
 End if 
!------------------------ 
! bar[Fd], Fd 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFd).and.(Include_in_loopFd)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = SA_DerGloop(p2,MFd2(i1),MFd2(i2)) 
B0m2 = -2._dp*MFd(i1)*MFd(i2)*SA_DerB0(p2,MFd2(i1),MFd2(i2)) 
coupL1 = cplcFdFdAhL(i1,i2)
coupR1 = cplcFdFdAhR(i1,i2)
coupL2 =  Conjg(cplcFdFdAhL(i1,i2))
coupR2 =  Conjg(cplcFdFdAhR(i1,i2))
    SumI = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fe], Fe 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFe).and.(Include_in_loopFe)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = SA_DerGloop(p2,MFe2(i1),MFe2(i2)) 
B0m2 = -2._dp*MFe(i1)*MFe(i2)*SA_DerB0(p2,MFe2(i1),MFe2(i2)) 
coupL1 = cplcFeFeAhL(i1,i2)
coupR1 = cplcFeFeAhR(i1,i2)
coupL2 =  Conjg(cplcFeFeAhL(i1,i2))
coupR2 =  Conjg(cplcFeFeAhR(i1,i2))
    SumI = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fu], Fu 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFu).and.(Include_in_loopFu)) Then 
    Do i1 = 1, 3
       Do i2 = 1, 3
 G0m2 = SA_DerGloop(p2,MFu2(i1),MFu2(i2)) 
B0m2 = -2._dp*MFu(i1)*MFu(i2)*SA_DerB0(p2,MFu2(i1),MFu2(i2)) 
coupL1 = cplcFuFuAhL(i1,i2)
coupR1 = cplcFuFuAhR(i1,i2)
coupL2 =  Conjg(cplcFuFuAhL(i1,i2))
coupR2 =  Conjg(cplcFuFuAhR(i1,i2))
    SumI = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxv], Fxv 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 2
 G0m2 = SA_DerGloop(p2,MFxv2(i1),MFxv2(i2)) 
B0m2 = -2._dp*MFxv(i1)*MFxv(i2)*SA_DerB0(p2,MFxv2(i1),MFxv2(i2)) 
coupL1 = cplcFxvFxvAhL(i1,i2)
coupR1 = cplcFxvFxvAhR(i1,i2)
coupL2 =  Conjg(cplcFxvFxvAhL(i1,i2))
coupR2 =  Conjg(cplcFxvFxvAhR(i1,i2))
    SumI = (coupL1*coupL2+coupR1*coupR2)*G0m2 & 
                & + (coupL1*coupR2+coupR1*coupL2)*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[gWp], gWp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
F0m2 =  -Real(SA_DerB0(p2,MVWp2*RXi,MVWp2*RXi),dp) 
 coup1 = cplcgWpgWpAh
coup2 =  cplcgWpgWpAh 
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWpC], gWpC 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
F0m2 =  -Real(SA_DerB0(p2,MVWp2*RXi,MVWp2*RXi),dp) 
 coup1 = cplcgWCgWCAh
coup2 =  cplcgWCgWCAh 
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VZ, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVZ).and.(Include_in_loophh)) Then 
      Do i2 = 1, 2
 F0m2 = Real(DerFloopRXi(p2,Mhh2(i2),MVZ2),dp) 
coup1 = cplAhhhVZ(i2)
coup2 =  Conjg(cplAhhhVZ(i2))
    SumI = coup1*coup2*F0m2 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! conj[VWp], Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
F0m2 = Real(DerFloopRXi(p2,MHp2,MVWp2),dp) 
coup1 = cplAhHpcVWp
coup2 =  Conjg(cplAhHpcVWp)
    SumI = coup1*coup2*F0m2 
res = res +2._dp* SumI  
End if 
!------------------------ 
! Ah, Ah 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
A0m2 = SA_DerA0(MAh2) 
coup1 = cplAhAhAhAh
    SumI = -coup1*A0m2 
res = res +1._dp/2._dp* SumI  
End if 
!------------------------ 
! hh, hh 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
    Do i1 = 1, 2
 A0m2 = SA_DerA0(Mhh2(i1)) 
coup1 = cplAhAhhhhh(i1,i1)
    SumI = -coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 End if 
!------------------------ 
! conj[Hp], Hp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
A0m2 = SA_DerA0(MHp2) 
coup1 = cplAhAhHpcHp
    SumI = -coup1*A0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Ssc], Ssc 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
    Do i1 = 1, 2
 A0m2 = SA_DerA0(MSsc2(i1)) 
coup1 = cplAhAhSsccSsc(i1,i1)
    SumI = -coup1*A0m2 
res = res +1._dp* SumI  
      End Do 
 End if 
!------------------------ 
! conj[VWp], VWp 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
A0m2 =  0.75_dp*SA_DerA0(MVWp2) + 0.25_dp*RXi*SA_DerA0(MVWp2*RXi) - 0.5_dp*MVWp2*DerrMS 
coup1 = cplAhAhcVWpVWp
    SumI = coup1*A0m2 
res = res +4._dp* SumI  
End if 
!------------------------ 
! VZ, VZ 
!------------------------ 
sumI = 0._dp 
 
If ((Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
A0m2 =  0.75_dp*SA_DerA0(MVZ2) + 0.25_dp*RXi*SA_DerA0(MVZ2*RXi) - 0.5_dp*MVZ2*DerrMS 
coup1 = cplAhAhVZVZ
    SumI = coup1*A0m2 
res = res +2._dp* SumI  
End if 


res = oo16pi2*res 
 
End Subroutine DerPi1LoopAh 
 
Subroutine Pi1LoopVG(p2,MFd,MFd2,MFu,MFu2,cplcFdFdVGL,cplcFdFdVGR,cplcFuFuVGL,        & 
& cplcFuFuVGR,cplcgGgGVG,cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3,kont,res)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MFu(3),MFu2(3)

Complex(dp), Intent(in) :: cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcgGgGVG,       & 
& cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! bar[Fd], Fd 
!------------------------ 
If ((Include_in_loopFd).and.(Include_in_loopFd)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = 4._dp*MFd(i1)*MFd(i2)*Real(SA_B0(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVGL(i1,i2)
coupR1 = cplcFdFdVGR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +0.5_dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fu], Fu 
!------------------------ 
If ((Include_in_loopFu).and.(Include_in_loopFu)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = 4._dp*MFu(i1)*MFu(i2)*Real(SA_B0(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVGL(i1,i2)
coupR1 = cplcFuFuVGR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +0.5_dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[gG], gG 
!------------------------ 
If ((Include_in_loopgG).and.(Include_in_loopgG)) Then 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,0._dp,0._dp),dp)
coup1 = cplcgGgGVG
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +3._dp* SumI  
End if 
!------------------------ 
! VG, VG 
!------------------------ 
If ((Include_in_loopVG).and.(Include_in_loopVG)) Then 
sumI = 0._dp 
 
coup1 = cplVGVGVG
coup2 = Conjg(coup1) 
    SumI = -VVVloop(p2,0._dp,0._dp)*coup1*coup2 
res = res +1.5_dp* SumI  
End if 
!------------------------ 
! VG 
!------------------------ 
If (Include_in_loopVG) Then 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_A0(0._dp) +RXi/4._dp*SA_A0(0._dp*RXi) 
coup1 = cplVGVGVGVG1
coup2 = cplVGVGVGVG2
coup3 = cplVGVGVGVG3
SumI = ((2._dp*rMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*0._dp-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1._dp/2._dp* SumI  
End if 
res = oo16pi2*res 
 
End Subroutine Pi1LoopVG 
 
Subroutine DerPi1LoopVG(p2,MFd,MFd2,MFu,MFu2,cplcFdFdVGL,cplcFdFdVGR,cplcFuFuVGL,     & 
& cplcFuFuVGR,cplcgGgGVG,cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3,kont,res)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MFu(3),MFu2(3)

Complex(dp), Intent(in) :: cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcgGgGVG,       & 
& cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! bar[Fd], Fd 
!------------------------ 
If ((Include_in_loopFd).and.(Include_in_loopFd)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = 4._dp*MFd(i1)*MFd(i2)*Real(SA_DerB0(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVGL(i1,i2)
coupR1 = cplcFdFdVGR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +0.5_dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fu], Fu 
!------------------------ 
If ((Include_in_loopFu).and.(Include_in_loopFu)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = 4._dp*MFu(i1)*MFu(i2)*Real(SA_DerB0(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVGL(i1,i2)
coupR1 = cplcFuFuVGR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +0.5_dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[gG], gG 
!------------------------ 
If ((Include_in_loopgG).and.(Include_in_loopgG)) Then 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVG2,MVG2),dp)
coup1 = cplcgGgGVG
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +3._dp* SumI  
End if 
!------------------------ 
! VG, VG 
!------------------------ 
If ((Include_in_loopVG).and.(Include_in_loopVG)) Then 
sumI = 0._dp 
 
coup1 = cplVGVGVG
coup2 = Conjg(coup1) 
    SumI = -DerVVVloop(p2,MVG2,MVG2)*coup1*coup2 
res = res +1.5_dp* SumI  
End if 
!------------------------ 
! VG 
!------------------------ 
If (Include_in_loopVG) Then 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_DerA0(MVG2) +RXi/4._dp*SA_DerA0(MVG2*RXi) 
coup1 = cplVGVGVGVG1
coup2 = cplVGVGVGVG2
coup3 = cplVGVGVGVG3
SumI = ((2._dp*DerrMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVG2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1._dp/2._dp* SumI  
End if 
res = oo16pi2*res 
 
End Subroutine DerPi1LoopVG 
 
Subroutine Pi1LoopVP(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,               & 
& MVWp,MVWp2,cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,    & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcgWpgWpVP,cplcgWCgWCVP,cplHpcHpVP,cplHpcVWpVP,          & 
& cplcVWpVPVWp,cplHpcHpVPVP,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,kont,res)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFxe,MFxe2,MHp,MHp2,MVWp,MVWp2

Complex(dp), Intent(in) :: cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFuFuVPL(3,3), & 
& cplcFuFuVPR(3,3),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcgWpgWpVP,cplcgWCgWCVP,cplHpcHpVP,     & 
& cplHpcVWpVP,cplcVWpVPVWp,cplHpcHpVPVP,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! bar[Fd], Fd 
!------------------------ 
If ((Include_in_loopFd).and.(Include_in_loopFd)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MFd2(i1).gt.50._dp**2).and.(MFd2(i2).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MFd2(i1).lt.50._dp**2).and.(MFd2(i2).lt.50._dp**2)) )   Then 
H0m2 = Real(SA_Hloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = 4._dp*MFd(i1)*MFd(i2)*Real(SA_B0(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVPL(i1,i2)
coupR1 = cplcFdFdVPR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
End If 
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fe], Fe 
!------------------------ 
If ((Include_in_loopFe).and.(Include_in_loopFe)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MFe2(i1).gt.50._dp**2).and.(MFe2(i2).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MFe2(i1).lt.50._dp**2).and.(MFe2(i2).lt.50._dp**2)) )   Then 
H0m2 = Real(SA_Hloop(p2,MFe2(i1),MFe2(i2)),dp) 
B0m2 = 4._dp*MFe(i1)*MFe(i2)*Real(SA_B0(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVPL(i1,i2)
coupR1 = cplcFeFeVPR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
End If 
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fu], Fu 
!------------------------ 
If ((Include_in_loopFu).and.(Include_in_loopFu)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MFu2(i1).gt.50._dp**2).and.(MFu2(i2).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MFu2(i1).lt.50._dp**2).and.(MFu2(i2).lt.50._dp**2)) )   Then 
H0m2 = Real(SA_Hloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = 4._dp*MFu(i1)*MFu(i2)*Real(SA_B0(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVPL(i1,i2)
coupR1 = cplcFuFuVPR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
End If 
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxe], Fxe 
!------------------------ 
If ((Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MFxe2.gt.50._dp**2).and.(MFxe2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MFxe2.lt.50._dp**2).and.(MFxe2.lt.50._dp**2)) )   Then 
H0m2 = Real(SA_Hloop(p2,MFxe2,MFxe2),dp) 
B0m2 = 4._dp*MFxe*MFxe*Real(SA_B0(p2,MFxe2,MFxe2),dp) 
coupL1 = cplcFxeFxeVPL
coupR1 = cplcFxeFxeVPR
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
End If 
End if 
!------------------------ 
! bar[gWp], gWp 
!------------------------ 
If ((Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWp2.gt.50._dp**2).and.(MVWp2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWp2.lt.50._dp**2).and.(MVWp2.lt.50._dp**2)) )   Then 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,MVWp2,MVWp2),dp)
coup1 = cplcgWpgWpVP
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End If 
End if 
!------------------------ 
! bar[gWpC], gWpC 
!------------------------ 
If ((Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWp2.gt.50._dp**2).and.(MVWp2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWp2.lt.50._dp**2).and.(MVWp2.lt.50._dp**2)) )   Then 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,MVWp2,MVWp2),dp)
coup1 = cplcgWCgWCVP
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End If 
End if 
!------------------------ 
! conj[Hp], Hp 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MHp2.gt.50._dp**2).and.(MHp2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MHp2.lt.50._dp**2).and.(MHp2.lt.50._dp**2)) )   Then 
B22m2 = Real(VSSloop(p2,MHp2,MHp2),dp)  
coup1 = cplHpcHpVP
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
End If 
End if 
!------------------------ 
! conj[VWp], Hp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWp2.gt.50._dp**2).and.(MHp2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWp2.lt.50._dp**2).and.(MHp2.lt.50._dp**2)) )   Then 
B0m2 = Real(VVSloop(p2,MVWp2,MHp2),dp)
coup1 = cplHpcVWpVP
    SumI = Abs(coup1)**2*B0m2 
res = res +2._dp* SumI  
End If 
End if 
!------------------------ 
! conj[VWp], VWp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWp2.gt.50._dp**2).and.(MVWp2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWp2.lt.50._dp**2).and.(MVWp2.lt.50._dp**2)) )   Then 
coup1 = cplcVWpVPVWp
coup2 = Conjg(coup1) 
    SumI = -VVVloop(p2,MVWp2,MVWp2)*coup1*coup2 
res = res +1._dp* SumI  
End If 
End if 
!------------------------ 
! conj[Hp] 
!------------------------ 
If (Include_in_loopHp) Then 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MHp2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MHp2.lt.50._dp**2)) )   Then 
SumI = 0._dp 
 A0m2 = SA_A0(MHp2)
 coup1 = cplHpcHpVPVP
 SumI = coup1*A0m2 
res = res +1* SumI  
End If 
End if 
!------------------------ 
! conj[VWp] 
!------------------------ 
If (Include_in_loopVWp) Then 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWp2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWp2.lt.50._dp**2)) )   Then 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_A0(MVWp2) +RXi/4._dp*SA_A0(MVWp2*RXi) 
coup1 = cplcVWpVPVPVWp3
coup2 = cplcVWpVPVPVWp1
coup3 = cplcVWpVPVPVWp2
SumI = ((2._dp*rMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVWp2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
End If 
End if 
res = oo16pi2*res 
 
End Subroutine Pi1LoopVP 
 
Subroutine DerPi1LoopVP(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,            & 
& MVWp,MVWp2,cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,    & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcgWpgWpVP,cplcgWCgWCVP,cplHpcHpVP,cplHpcVWpVP,          & 
& cplcVWpVPVWp,cplHpcHpVPVP,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,kont,res)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFxe,MFxe2,MHp,MHp2,MVWp,MVWp2

Complex(dp), Intent(in) :: cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFuFuVPL(3,3), & 
& cplcFuFuVPR(3,3),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcgWpgWpVP,cplcgWCgWCVP,cplHpcHpVP,     & 
& cplHpcVWpVP,cplcVWpVPVWp,cplHpcHpVPVP,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! bar[Fd], Fd 
!------------------------ 
If ((Include_in_loopFd).and.(Include_in_loopFd)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MFd2(i1).gt.50._dp**2).and.(MFd2(i2).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MFd2(i1).lt.50._dp**2).and.(MFd2(i2).lt.50._dp**2)) )   Then 
H0m2 = Real(SA_DerHloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = 4._dp*MFd(i1)*MFd(i2)*Real(SA_DerB0(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVPL(i1,i2)
coupR1 = cplcFdFdVPR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
End If 
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fe], Fe 
!------------------------ 
If ((Include_in_loopFe).and.(Include_in_loopFe)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MFe2(i1).gt.50._dp**2).and.(MFe2(i2).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MFe2(i1).lt.50._dp**2).and.(MFe2(i2).lt.50._dp**2)) )   Then 
H0m2 = Real(SA_DerHloop(p2,MFe2(i1),MFe2(i2)),dp) 
B0m2 = 4._dp*MFe(i1)*MFe(i2)*Real(SA_DerB0(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVPL(i1,i2)
coupR1 = cplcFeFeVPR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
End If 
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fu], Fu 
!------------------------ 
If ((Include_in_loopFu).and.(Include_in_loopFu)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MFu2(i1).gt.50._dp**2).and.(MFu2(i2).gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MFu2(i1).lt.50._dp**2).and.(MFu2(i2).lt.50._dp**2)) )   Then 
H0m2 = Real(SA_DerHloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = 4._dp*MFu(i1)*MFu(i2)*Real(SA_DerB0(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVPL(i1,i2)
coupR1 = cplcFuFuVPR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
End If 
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxe], Fxe 
!------------------------ 
If ((Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MFxe2.gt.50._dp**2).and.(MFxe2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MFxe2.lt.50._dp**2).and.(MFxe2.lt.50._dp**2)) )   Then 
H0m2 = Real(SA_DerHloop(p2,MFxe2,MFxe2),dp) 
B0m2 = 4._dp*MFxe*MFxe*Real(SA_DerB0(p2,MFxe2,MFxe2),dp) 
coupL1 = cplcFxeFxeVPL
coupR1 = cplcFxeFxeVPR
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
End If 
End if 
!------------------------ 
! bar[gWp], gWp 
!------------------------ 
If ((Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWp2.gt.50._dp**2).and.(MVWp2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWp2.lt.50._dp**2).and.(MVWp2.lt.50._dp**2)) )   Then 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVWp2,MVWp2),dp)
coup1 = cplcgWpgWpVP
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End If 
End if 
!------------------------ 
! bar[gWpC], gWpC 
!------------------------ 
If ((Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWp2.gt.50._dp**2).and.(MVWp2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWp2.lt.50._dp**2).and.(MVWp2.lt.50._dp**2)) )   Then 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVWp2,MVWp2),dp)
coup1 = cplcgWCgWCVP
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End If 
End if 
!------------------------ 
! conj[Hp], Hp 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MHp2.gt.50._dp**2).and.(MHp2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MHp2.lt.50._dp**2).and.(MHp2.lt.50._dp**2)) )   Then 
B22m2 = Real(DerVSSloop(p2,MHp2,MHp2),dp)  
coup1 = cplHpcHpVP
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
End If 
End if 
!------------------------ 
! conj[VWp], Hp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWp2.gt.50._dp**2).and.(MHp2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWp2.lt.50._dp**2).and.(MHp2.lt.50._dp**2)) )   Then 
B0m2 = Real(DerVVSloop(p2,MVWp2,MHp2),dp)
coup1 = cplHpcVWpVP
    SumI = Abs(coup1)**2*B0m2 
res = res +2._dp* SumI  
End If 
End if 
!------------------------ 
! conj[VWp], VWp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
sumI = 0._dp 
 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWp2.gt.50._dp**2).and.(MVWp2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWp2.lt.50._dp**2).and.(MVWp2.lt.50._dp**2)) )   Then 
coup1 = cplcVWpVPVWp
coup2 = Conjg(coup1) 
    SumI = -DerVVVloop(p2,MVWp2,MVWp2)*coup1*coup2 
res = res +1._dp* SumI  
End If 
End if 
!------------------------ 
! conj[Hp] 
!------------------------ 
If (Include_in_loopHp) Then 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MHp2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MHp2.lt.50._dp**2)) )   Then 
SumI = 0._dp 
 A0m2 = SA_DerA0(MHp2)
 coup1 = cplHpcHpVPVP
 SumI = coup1*A0m2 
res = res +1* SumI  
End If 
End if 
!------------------------ 
! conj[VWp] 
!------------------------ 
If (Include_in_loopVWp) Then 
If (((.not.OnlyHeavyStates).and.(.not.OnlyLightStates)) & 
  & .or.((OnlyHeavyStates).and.(MVWp2.gt.50._dp**2))   & 
  & .or.((OnlyLightStates).and.(MVWp2.lt.50._dp**2)) )   Then 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_DerA0(MVWp2) +RXi/4._dp*SA_DerA0(MVWp2*RXi) 
coup1 = cplcVWpVPVPVWp3
coup2 = cplcVWpVPVPVWp1
coup3 = cplcVWpVPVPVWp2
SumI = ((2._dp*DerrMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVWp2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
End If 
End if 
res = oo16pi2*res 
 
End Subroutine DerPi1LoopVP 
 
Subroutine OneLoopVZ(g1,g2,vvSM,TW,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,               & 
& MFu,MFu2,MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,MVZ,MVZ2,MHp,MHp2,MVWp,MVWp2,cplAhhhVZ,        & 
& cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFvFvVZL,cplcFvFvVZR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvVZL,cplcFxvFxvVZR,       & 
& cplcgWpgWpVZ,cplcgWCgWCVZ,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcVWpVWpVZ,               & 
& cplAhAhVZVZ,cplhhhhVZVZ,cplHpcHpVZVZ,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,  & 
& delta,mass,mass2,kont)

Real(dp), Intent(in) :: Mhh(2),Mhh2(2),MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),          & 
& MFv2(3),MFxe,MFxe2,MFxv(2),MFxv2(2),MVZ,MVZ2,MHp,MHp2,MVWp,MVWp2

Real(dp), Intent(in) :: g1,g2,vvSM,TW

Complex(dp), Intent(in) :: cplAhhhVZ(2),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),     & 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFxeFxeVZL,     & 
& cplcFxeFxeVZR,cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),cplcgWpgWpVZ,cplcgWCgWCVZ,         & 
& cplhhVZVZ(2),cplHpcHpVZ,cplHpcVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ,cplhhhhVZVZ(2,2),         & 
& cplHpcHpVZVZ,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3

Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi, mi2, p2, test_m2 
Complex(dp) :: PiSf, SigL, SigR, SigSL, SigSR 
Real(dp), Intent(out) :: mass, mass2 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopVZ'
 
mi2 = MVZ2 

 
p2 = MVZ2
PiSf = ZeroC 
Call Pi1LoopVZ(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,              & 
& MFxe,MFxe2,MFxv,MFxv2,MVZ,MVZ2,MHp,MHp2,MVWp,MVWp2,cplAhhhVZ,cplcFdFdVZL,              & 
& cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFvFvVZL,               & 
& cplcFvFvVZR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWpVZ,      & 
& cplcgWCgWCVZ,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ,cplhhhhVZVZ,    & 
& cplHpcHpVZVZ,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,kont,PiSf)

mass2 = mi2 + Real(PiSf,dp) 
mass = sqrt(mass2) 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
p2 =  mass2 
PiSf = ZeroC 
Call Pi1LoopVZ(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,              & 
& MFxe,MFxe2,MFxv,MFxv2,MVZ,MVZ2,MHp,MHp2,MVWp,MVWp2,cplAhhhVZ,cplcFdFdVZL,              & 
& cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFvFvVZL,               & 
& cplcFvFvVZR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWpVZ,      & 
& cplcgWCgWCVZ,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ,cplhhhhVZVZ,    & 
& cplHpcHpVZVZ,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,kont,PiSf)

mass2 = mi2 + Real(PiSf,dp) 
mass = sqrt(mass2) 
 If (test_m2.Ne.0._dp) Then 
    test_m2 = Abs(test_m2 - mass2) / test_m2 
 Else 
    test_m2 = Abs(mass2) 
 End If 
 If (mass2.Ge.0._dp) Then 
   If (RotateNegativeFermionMasses) Then 
    mass = sqrt(mass2) 
   End if 
  Else 
 If (Abs(mass2).lt.1.0E-30_dp) test_m2 = 0._dp 
     Write(ErrCan,*) 'Warning from routine'//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
   SignOfMassChanged = .True. 
   mass = 0._dp 
  End If 
If (test_m2.LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoopVZ
 
 
Subroutine Pi1LoopVZ(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,             & 
& MFv2,MFxe,MFxe2,MFxv,MFxv2,MVZ,MVZ2,MHp,MHp2,MVWp,MVWp2,cplAhhhVZ,cplcFdFdVZL,         & 
& cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFvFvVZL,               & 
& cplcFvFvVZR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWpVZ,      & 
& cplcgWCgWCVZ,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ,cplhhhhVZVZ,    & 
& cplHpcHpVZVZ,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,kont,res)

Implicit None 
Real(dp), Intent(in) :: Mhh(2),Mhh2(2),MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),          & 
& MFv2(3),MFxe,MFxe2,MFxv(2),MFxv2(2),MVZ,MVZ2,MHp,MHp2,MVWp,MVWp2

Complex(dp), Intent(in) :: cplAhhhVZ(2),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),     & 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFxeFxeVZL,     & 
& cplcFxeFxeVZR,cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),cplcgWpgWpVZ,cplcgWCgWCVZ,         & 
& cplhhVZVZ(2),cplHpcHpVZ,cplHpcVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ,cplhhhhVZVZ(2,2),         & 
& cplHpcHpVZVZ,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! hh, Ah 
!------------------------ 
If ((Include_in_loophh).and.(Include_in_loopAh)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 2
 B22m2 = Real(VSSloop(p2,MAh2,Mhh2(i1)),dp)  
coup1 = cplAhhhVZ(i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
      End Do 
 End if 
!------------------------ 
! bar[Fd], Fd 
!------------------------ 
If ((Include_in_loopFd).and.(Include_in_loopFd)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = 4._dp*MFd(i1)*MFd(i2)*Real(SA_B0(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVZL(i1,i2)
coupR1 = cplcFdFdVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fe], Fe 
!------------------------ 
If ((Include_in_loopFe).and.(Include_in_loopFe)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFe2(i1),MFe2(i2)),dp) 
B0m2 = 4._dp*MFe(i1)*MFe(i2)*Real(SA_B0(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVZL(i1,i2)
coupR1 = cplcFeFeVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fu], Fu 
!------------------------ 
If ((Include_in_loopFu).and.(Include_in_loopFu)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = 4._dp*MFu(i1)*MFu(i2)*Real(SA_B0(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVZL(i1,i2)
coupR1 = cplcFuFuVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fv], Fv 
!------------------------ 
If ((Include_in_loopFv).and.(Include_in_loopFv)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFv2(i1),MFv2(i2)),dp) 
B0m2 = 4._dp*MFv(i1)*MFv(i2)*Real(SA_B0(p2,MFv2(i1),MFv2(i2)),dp) 
coupL1 = cplcFvFvVZL(i1,i2)
coupR1 = cplcFvFvVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxe], Fxe 
!------------------------ 
If ((Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
sumI = 0._dp 
 
H0m2 = Real(SA_Hloop(p2,MFxe2,MFxe2),dp) 
B0m2 = 4._dp*MFxe*MFxe*Real(SA_B0(p2,MFxe2,MFxe2),dp) 
coupL1 = cplcFxeFxeVZL
coupR1 = cplcFxeFxeVZR
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[Fxv], Fxv 
!------------------------ 
If ((Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 H0m2 = Real(SA_Hloop(p2,MFxv2(i1),MFxv2(i2)),dp) 
B0m2 = 4._dp*MFxv(i1)*MFxv(i2)*Real(SA_B0(p2,MFxv2(i1),MFxv2(i2)),dp) 
coupL1 = cplcFxvFxvVZL(i1,i2)
coupR1 = cplcFxvFxvVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[gWp], gWp 
!------------------------ 
If ((Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,MVWp2,MVWp2),dp)
coup1 = cplcgWpgWpVZ
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWpC], gWpC 
!------------------------ 
If ((Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,MVWp2,MVWp2),dp)
coup1 = cplcgWCgWCVZ
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VZ, hh 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loophh)) Then 
sumI = 0._dp 
 
      Do i2 = 1, 2
 B0m2 = Real(VVSloop(p2,MVZ2,Mhh2(i2)),dp)
coup1 = cplhhVZVZ(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! conj[Hp], Hp 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
B22m2 = Real(VSSloop(p2,MHp2,MHp2),dp)  
coup1 = cplHpcHpVZ
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[VWp], Hp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
B0m2 = Real(VVSloop(p2,MVWp2,MHp2),dp)
coup1 = cplHpcVWpVZ
    SumI = Abs(coup1)**2*B0m2 
res = res +2._dp* SumI  
End if 
!------------------------ 
! conj[VWp], VWp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
sumI = 0._dp 
 
coup1 = cplcVWpVWpVZ
coup2 = Conjg(coup1) 
    SumI = -VVVloop(p2,MVWp2,MVWp2)*coup1*coup2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! Ah 
!------------------------ 
If (Include_in_loopAh) Then 
SumI = 0._dp 
 A0m2 = SA_A0(MAh2)
 coup1 = cplAhAhVZVZ
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
End if 
!------------------------ 
! hh 
!------------------------ 
If (Include_in_loophh) Then 
    Do i1 = 1, 2
 SumI = 0._dp 
 A0m2 = SA_A0(Mhh2(i1))
 coup1 = cplhhhhVZVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 End if 
!------------------------ 
! conj[Hp] 
!------------------------ 
If (Include_in_loopHp) Then 
SumI = 0._dp 
 A0m2 = SA_A0(MHp2)
 coup1 = cplHpcHpVZVZ
 SumI = coup1*A0m2 
res = res +1* SumI  
End if 
!------------------------ 
! conj[VWp] 
!------------------------ 
If (Include_in_loopVWp) Then 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_A0(MVWp2) +RXi/4._dp*SA_A0(MVWp2*RXi) 
coup1 = cplcVWpVWpVZVZ1
coup2 = cplcVWpVWpVZVZ2
coup3 = cplcVWpVWpVZVZ3
SumI = ((2._dp*rMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVWp2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
End if 
res = oo16pi2*res 
 
End Subroutine Pi1LoopVZ 
 
Subroutine DerPi1LoopVZ(p2,Mhh,Mhh2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,              & 
& MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,MVZ,MVZ2,MHp,MHp2,MVWp,MVWp2,cplAhhhVZ,cplcFdFdVZL,     & 
& cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplcFvFvVZL,               & 
& cplcFvFvVZR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWpVZ,      & 
& cplcgWCgWCVZ,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ,cplhhhhVZVZ,    & 
& cplHpcHpVZVZ,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,kont,res)

Implicit None 
Real(dp), Intent(in) :: Mhh(2),Mhh2(2),MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),          & 
& MFv2(3),MFxe,MFxe2,MFxv(2),MFxv2(2),MVZ,MVZ2,MHp,MHp2,MVWp,MVWp2

Complex(dp), Intent(in) :: cplAhhhVZ(2),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),     & 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFxeFxeVZL,     & 
& cplcFxeFxeVZR,cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),cplcgWpgWpVZ,cplcgWCgWCVZ,         & 
& cplhhVZVZ(2),cplHpcHpVZ,cplHpcVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ,cplhhhhVZVZ(2,2),         & 
& cplHpcHpVZVZ,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! hh, Ah 
!------------------------ 
If ((Include_in_loophh).and.(Include_in_loopAh)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 2
 B22m2 = Real(DerVSSloop(p2,MAh2,Mhh2(i1)),dp)  
coup1 = cplAhhhVZ(i1)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
      End Do 
 End if 
!------------------------ 
! bar[Fd], Fd 
!------------------------ 
If ((Include_in_loopFd).and.(Include_in_loopFd)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = 4._dp*MFd(i1)*MFd(i2)*Real(SA_DerB0(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVZL(i1,i2)
coupR1 = cplcFdFdVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fe], Fe 
!------------------------ 
If ((Include_in_loopFe).and.(Include_in_loopFe)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFe2(i1),MFe2(i2)),dp) 
B0m2 = 4._dp*MFe(i1)*MFe(i2)*Real(SA_DerB0(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVZL(i1,i2)
coupR1 = cplcFeFeVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fu], Fu 
!------------------------ 
If ((Include_in_loopFu).and.(Include_in_loopFu)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = 4._dp*MFu(i1)*MFu(i2)*Real(SA_DerB0(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVZL(i1,i2)
coupR1 = cplcFuFuVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fv], Fv 
!------------------------ 
If ((Include_in_loopFv).and.(Include_in_loopFv)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFv2(i1),MFv2(i2)),dp) 
B0m2 = 4._dp*MFv(i1)*MFv(i2)*Real(SA_DerB0(p2,MFv2(i1),MFv2(i2)),dp) 
coupL1 = cplcFvFvVZL(i1,i2)
coupR1 = cplcFvFvVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxe], Fxe 
!------------------------ 
If ((Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
sumI = 0._dp 
 
H0m2 = Real(SA_DerHloop(p2,MFxe2,MFxe2),dp) 
B0m2 = 4._dp*MFxe*MFxe*Real(SA_DerB0(p2,MFxe2,MFxe2),dp) 
coupL1 = cplcFxeFxeVZL
coupR1 = cplcFxeFxeVZR
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[Fxv], Fxv 
!------------------------ 
If ((Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 H0m2 = Real(SA_DerHloop(p2,MFxv2(i1),MFxv2(i2)),dp) 
B0m2 = 4._dp*MFxv(i1)*MFxv(i2)*Real(SA_DerB0(p2,MFxv2(i1),MFxv2(i2)),dp) 
coupL1 = cplcFxvFxvVZL(i1,i2)
coupR1 = cplcFxvFxvVZR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[gWp], gWp 
!------------------------ 
If ((Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVWp2,MVWp2),dp)
coup1 = cplcgWpgWpVZ
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWpC], gWpC 
!------------------------ 
If ((Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVWp2,MVWp2),dp)
coup1 = cplcgWCgWCVZ
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VZ, hh 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loophh)) Then 
sumI = 0._dp 
 
      Do i2 = 1, 2
 B0m2 = Real(DerVVSloop(p2,MVZ2,Mhh2(i2)),dp)
coup1 = cplhhVZVZ(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! conj[Hp], Hp 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
B22m2 = Real(DerVSSloop(p2,MHp2,MHp2),dp)  
coup1 = cplHpcHpVZ
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[VWp], Hp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
B0m2 = Real(DerVVSloop(p2,MVWp2,MHp2),dp)
coup1 = cplHpcVWpVZ
    SumI = Abs(coup1)**2*B0m2 
res = res +2._dp* SumI  
End if 
!------------------------ 
! conj[VWp], VWp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
sumI = 0._dp 
 
coup1 = cplcVWpVWpVZ
coup2 = Conjg(coup1) 
    SumI = -DerVVVloop(p2,MVWp2,MVWp2)*coup1*coup2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! Ah 
!------------------------ 
If (Include_in_loopAh) Then 
SumI = 0._dp 
 A0m2 = SA_DerA0(MAh2)
 coup1 = cplAhAhVZVZ
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
End if 
!------------------------ 
! hh 
!------------------------ 
If (Include_in_loophh) Then 
    Do i1 = 1, 2
 SumI = 0._dp 
 A0m2 = SA_DerA0(Mhh2(i1))
 coup1 = cplhhhhVZVZ(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 End if 
!------------------------ 
! conj[Hp] 
!------------------------ 
If (Include_in_loopHp) Then 
SumI = 0._dp 
 A0m2 = SA_DerA0(MHp2)
 coup1 = cplHpcHpVZVZ
 SumI = coup1*A0m2 
res = res +1* SumI  
End if 
!------------------------ 
! conj[VWp] 
!------------------------ 
If (Include_in_loopVWp) Then 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_DerA0(MVWp2) +RXi/4._dp*SA_DerA0(MVWp2*RXi) 
coup1 = cplcVWpVWpVZVZ1
coup2 = cplcVWpVWpVZVZ2
coup3 = cplcVWpVWpVZVZ3
SumI = ((2._dp*DerrMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVWp2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
End if 
res = oo16pi2*res 
 
End Subroutine DerPi1LoopVZ 
 
Subroutine OneLoopVWp(g2,vvSM,MHp,MHp2,MAh,MAh2,MFd,MFd2,MFu,MFu2,MFe,MFe2,           & 
& MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MVWp,MVWp2,MVZ,MVZ2,cplAhHpcVWp,               & 
& cplcFdFucVWpL,cplcFdFucVWpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxvcVWpL,               & 
& cplcFxeFxvcVWpR,cplcgWCgAcVWp,cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgZcVWp,               & 
& cplhhHpcVWp,cplhhcVWpVWp,cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,            & 
& cplAhAhcVWpVWp,cplhhhhcVWpVWp,cplHpcHpcVWpVWp,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,         & 
& cplcVWpVPVPVWp2,cplcVWpcVWpVWpVWp2,cplcVWpcVWpVWpVWp3,cplcVWpcVWpVWpVWp1,              & 
& cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,delta,mass,mass2,kont)

Real(dp), Intent(in) :: MHp,MHp2,MAh,MAh2,MFd(3),MFd2(3),MFu(3),MFu2(3),MFe(3),MFe2(3),MFv(3),MFv2(3),        & 
& MFxe,MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MVWp,MVWp2,MVZ,MVZ2

Real(dp), Intent(in) :: g2,vvSM

Complex(dp), Intent(in) :: cplAhHpcVWp,cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),& 
& cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplcgWCgAcVWp,cplcgAgWpcVWp,cplcgZgWpcVWp,       & 
& cplcgWCgZcVWp,cplhhHpcVWp(2),cplhhcVWpVWp(2),cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,     & 
& cplcVWpVWpVZ,cplAhAhcVWpVWp,cplhhhhcVWpVWp(2,2),cplHpcHpcVWpVWp,cplcVWpVPVPVWp3,       & 
& cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,cplcVWpcVWpVWpVWp2,cplcVWpcVWpVWpVWp3,cplcVWpcVWpVWpVWp1,& 
& cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3

Integer , Intent(inout):: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,j4,il,i_count, ierr 
Real(dp), Intent(in) :: delta 
Real(dp) :: mi, mi2, p2, test_m2 
Complex(dp) :: PiSf, SigL, SigR, SigSL, SigSR 
Real(dp), Intent(out) :: mass, mass2 
Iname = Iname + 1 
NameOfUnit(Iname) = 'OneLoopVWp'
 
mi2 = MVWp2 

 
p2 = MVWp2
PiSf = ZeroC 
Call Pi1LoopVWp(p2,MHp,MHp2,MAh,MAh2,MFd,MFd2,MFu,MFu2,MFe,MFe2,MFv,MFv2,             & 
& MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MVWp,MVWp2,MVZ,MVZ2,cplAhHpcVWp,cplcFdFucVWpL,          & 
& cplcFdFucVWpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,             & 
& cplcgWCgAcVWp,cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgZcVWp,cplhhHpcVWp,cplhhcVWpVWp,      & 
& cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplAhAhcVWpVWp,cplhhhhcVWpVWp,       & 
& cplHpcHpcVWpVWp,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,cplcVWpcVWpVWpVWp2,    & 
& cplcVWpcVWpVWpVWp3,cplcVWpcVWpVWpVWp1,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3, & 
& kont,PiSf)

mass2 = mi2 + Real(PiSf,dp) 
mass = sqrt(mass2) 
i_count = 0 
Do  
i_count = i_count + 1 
test_m2 = mass2 
p2 =  mass2 
PiSf = ZeroC 
Call Pi1LoopVWp(p2,MHp,MHp2,MAh,MAh2,MFd,MFd2,MFu,MFu2,MFe,MFe2,MFv,MFv2,             & 
& MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MVWp,MVWp2,MVZ,MVZ2,cplAhHpcVWp,cplcFdFucVWpL,          & 
& cplcFdFucVWpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,             & 
& cplcgWCgAcVWp,cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgZcVWp,cplhhHpcVWp,cplhhcVWpVWp,      & 
& cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplAhAhcVWpVWp,cplhhhhcVWpVWp,       & 
& cplHpcHpcVWpVWp,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,cplcVWpcVWpVWpVWp2,    & 
& cplcVWpcVWpVWpVWp3,cplcVWpcVWpVWpVWp1,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3, & 
& kont,PiSf)

mass2 = mi2 + Real(PiSf,dp) 
mass = sqrt(mass2) 
 If (test_m2.Ne.0._dp) Then 
    test_m2 = Abs(test_m2 - mass2) / test_m2 
 Else 
    test_m2 = Abs(mass2) 
 End If 
 If (mass2.Ge.0._dp) Then 
   If (RotateNegativeFermionMasses) Then 
    mass = sqrt(mass2) 
   End if 
  Else 
 If (Abs(mass2).lt.1.0E-30_dp) test_m2 = 0._dp 
     Write(ErrCan,*) 'Warning from routine'//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
   SignOfMassChanged = .True. 
   mass = 0._dp 
  End If 
If (test_m2.LT.0.1_dp*delta) Exit 
If (i_count.Gt.30) Then 
  Write(*,*) "Problem in "//NameOfUnit(Iname), test_m2, mass2 
  kont = -510 
  Call AddError(510) 
 Exit 
End If 
End Do 
 
 
Iname = Iname -1 
End Subroutine OneLoopVWp
 
 
Subroutine Pi1LoopVWp(p2,MHp,MHp2,MAh,MAh2,MFd,MFd2,MFu,MFu2,MFe,MFe2,MFv,            & 
& MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MVWp,MVWp2,MVZ,MVZ2,cplAhHpcVWp,cplcFdFucVWpL,     & 
& cplcFdFucVWpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,             & 
& cplcgWCgAcVWp,cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgZcVWp,cplhhHpcVWp,cplhhcVWpVWp,      & 
& cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplAhAhcVWpVWp,cplhhhhcVWpVWp,       & 
& cplHpcHpcVWpVWp,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,cplcVWpcVWpVWpVWp2,    & 
& cplcVWpcVWpVWpVWp3,cplcVWpcVWpVWpVWp1,cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,kont,res)

Implicit None 
Real(dp), Intent(in) :: MHp,MHp2,MAh,MAh2,MFd(3),MFd2(3),MFu(3),MFu2(3),MFe(3),MFe2(3),MFv(3),MFv2(3),        & 
& MFxe,MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MVWp,MVWp2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhHpcVWp,cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),& 
& cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplcgWCgAcVWp,cplcgAgWpcVWp,cplcgZgWpcVWp,       & 
& cplcgWCgZcVWp,cplhhHpcVWp(2),cplhhcVWpVWp(2),cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,     & 
& cplcVWpVWpVZ,cplAhAhcVWpVWp,cplhhhhcVWpVWp(2,2),cplHpcHpcVWpVWp,cplcVWpVPVPVWp3,       & 
& cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,cplcVWpcVWpVWpVWp2,cplcVWpcVWpVWpVWp3,cplcVWpcVWpVWpVWp1,& 
& cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! Hp, Ah 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopAh)) Then 
sumI = 0._dp 
 
B22m2 = Real(VSSloop(p2,MAh2,MHp2),dp)  
coup1 = cplAhHpcVWp
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[Fd], Fu 
!------------------------ 
If ((Include_in_loopFd).and.(Include_in_loopFu)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFd2(i1),MFu2(i2)),dp) 
B0m2 = 4._dp*MFd(i1)*MFu(i2)*Real(SA_B0(p2,MFd2(i1),MFu2(i2)),dp) 
coupL1 = cplcFdFucVWpL(i1,i2)
coupR1 = cplcFdFucVWpR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fe], Fv 
!------------------------ 
If ((Include_in_loopFe).and.(Include_in_loopFv)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFe2(i1),MFv2(i2)),dp) 
B0m2 = 4._dp*MFe(i1)*MFv(i2)*Real(SA_B0(p2,MFe2(i1),MFv2(i2)),dp) 
coupL1 = cplcFeFvcVWpL(i1,i2)
coupR1 = cplcFeFvcVWpR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxe], Fxv 
!------------------------ 
If ((Include_in_loopFxe).and.(Include_in_loopFxv)) Then 
sumI = 0._dp 
 
      Do i2 = 1, 2
 H0m2 = Real(SA_Hloop(p2,MFxe2,MFxv2(i2)),dp) 
B0m2 = 4._dp*MFxe*MFxv(i2)*Real(SA_B0(p2,MFxe2,MFxv2(i2)),dp) 
coupL1 = cplcFxeFxvcVWpL(i2)
coupR1 = cplcFxeFxvcVWpR(i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! bar[gWpC], gP 
!------------------------ 
If ((Include_in_loopgWC).and.(Include_in_loopgA)) Then 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,0._dp,MVWp2),dp)
coup1 = cplcgWCgAcVWp
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gP], gWp 
!------------------------ 
If ((Include_in_loopgA).and.(Include_in_loopgWp)) Then 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,MVWp2,0._dp),dp)
coup1 = cplcgAgWpcVWp
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gZ], gWp 
!------------------------ 
If ((Include_in_loopgZ).and.(Include_in_loopgWp)) Then 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,MVWp2,MVZ2),dp)
coup1 = cplcgZgWpcVWp
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWpC], gZ 
!------------------------ 
If ((Include_in_loopgWC).and.(Include_in_loopgZ)) Then 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(VGGloop(p2,MVZ2,MVWp2),dp)
coup1 = cplcgWCgZcVWp
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! Hp, hh 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loophh)) Then 
sumI = 0._dp 
 
      Do i2 = 1, 2
 B22m2 = Real(VSSloop(p2,Mhh2(i2),MHp2),dp)  
coup1 = cplhhHpcVWp(i2)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! VWp, hh 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loophh)) Then 
sumI = 0._dp 
 
      Do i2 = 1, 2
 B0m2 = Real(VVSloop(p2,MVWp2,Mhh2(i2)),dp)
coup1 = cplhhcVWpVWp(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! VP, Hp 
!------------------------ 
If ((Include_in_loopVP).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
B0m2 = Real(VVSloop(p2,0._dp,MHp2),dp)
coup1 = cplHpcVWpVP
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VZ, Hp 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
B0m2 = Real(VVSloop(p2,MVZ2,MHp2),dp)
coup1 = cplHpcVWpVZ
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VWp, VP 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopVP)) Then 
sumI = 0._dp 
 
coup1 = cplcVWpVPVWp
coup2 = Conjg(coup1) 
    SumI = -VVVloop(p2,MVWp2,0._dp)*coup1*coup2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VZ, VWp 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
sumI = 0._dp 
 
coup1 = cplcVWpVWpVZ
coup2 = Conjg(coup1) 
    SumI = -VVVloop(p2,MVZ2,MVWp2)*coup1*coup2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! Ah 
!------------------------ 
If (Include_in_loopAh) Then 
SumI = 0._dp 
 A0m2 = SA_A0(MAh2)
 coup1 = cplAhAhcVWpVWp
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
End if 
!------------------------ 
! hh 
!------------------------ 
If (Include_in_loophh) Then 
    Do i1 = 1, 2
 SumI = 0._dp 
 A0m2 = SA_A0(Mhh2(i1))
 coup1 = cplhhhhcVWpVWp(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 End if 
!------------------------ 
! conj[Hp] 
!------------------------ 
If (Include_in_loopHp) Then 
SumI = 0._dp 
 A0m2 = SA_A0(MHp2)
 coup1 = cplHpcHpcVWpVWp
 SumI = coup1*A0m2 
res = res +1* SumI  
End if 
!------------------------ 
! VP 
!------------------------ 
If (Include_in_loopVP) Then 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_A0(0._dp) +RXi/4._dp*SA_A0(0._dp*RXi) 
coup1 = cplcVWpVPVPVWp3
coup2 = cplcVWpVPVPVWp1
coup3 = cplcVWpVPVPVWp2
SumI = ((2._dp*rMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*0._dp-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1._dp/2._dp* SumI  
End if 
!------------------------ 
! conj[VWp] 
!------------------------ 
If (Include_in_loopVWp) Then 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_A0(MVWp2) +RXi/4._dp*SA_A0(MVWp2*RXi) 
coup1 = cplcVWpcVWpVWpVWp2
coup2 = cplcVWpcVWpVWpVWp3
coup3 = cplcVWpcVWpVWpVWp1
SumI = ((2._dp*rMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVWp2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
End if 
!------------------------ 
! VZ 
!------------------------ 
If (Include_in_loopVZ) Then 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_A0(MVZ2) +RXi/4._dp*SA_A0(MVZ2*RXi) 
coup1 = cplcVWpVWpVZVZ1
coup2 = cplcVWpVWpVZVZ2
coup3 = cplcVWpVWpVZVZ3
SumI = ((2._dp*rMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVZ2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1._dp/2._dp* SumI  
End if 
res = oo16pi2*res 
 
End Subroutine Pi1LoopVWp 
 
Subroutine DerPi1LoopVWp(p2,MHp,MHp2,MAh,MAh2,MFd,MFd2,MFu,MFu2,MFe,MFe2,             & 
& MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MVWp,MVWp2,MVZ,MVZ2,cplAhHpcVWp,               & 
& cplcFdFucVWpL,cplcFdFucVWpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxvcVWpL,               & 
& cplcFxeFxvcVWpR,cplcgWCgAcVWp,cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgZcVWp,               & 
& cplhhHpcVWp,cplhhcVWpVWp,cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,            & 
& cplAhAhcVWpVWp,cplhhhhcVWpVWp,cplHpcHpcVWpVWp,cplcVWpVPVPVWp3,cplcVWpVPVPVWp1,         & 
& cplcVWpVPVPVWp2,cplcVWpcVWpVWpVWp2,cplcVWpcVWpVWpVWp3,cplcVWpcVWpVWpVWp1,              & 
& cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3,kont,res)

Implicit None 
Real(dp), Intent(in) :: MHp,MHp2,MAh,MAh2,MFd(3),MFd2(3),MFu(3),MFu2(3),MFe(3),MFe2(3),MFv(3),MFv2(3),        & 
& MFxe,MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MVWp,MVWp2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhHpcVWp,cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),& 
& cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplcgWCgAcVWp,cplcgAgWpcVWp,cplcgZgWpcVWp,       & 
& cplcgWCgZcVWp,cplhhHpcVWp(2),cplhhcVWpVWp(2),cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,     & 
& cplcVWpVWpVZ,cplAhAhcVWpVWp,cplhhhhcVWpVWp(2,2),cplHpcHpcVWpVWp,cplcVWpVPVPVWp3,       & 
& cplcVWpVPVPVWp1,cplcVWpVPVPVWp2,cplcVWpcVWpVWpVWp2,cplcVWpcVWpVWpVWp3,cplcVWpcVWpVWpVWp1,& 
& cplcVWpVWpVZVZ1,cplcVWpVWpVZVZ2,cplcVWpVWpVZVZ3

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2, A0m12, A0m22 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! Hp, Ah 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopAh)) Then 
sumI = 0._dp 
 
B22m2 = Real(DerVSSloop(p2,MAh2,MHp2),dp)  
coup1 = cplAhHpcVWp
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[Fd], Fu 
!------------------------ 
If ((Include_in_loopFd).and.(Include_in_loopFu)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFd2(i1),MFu2(i2)),dp) 
B0m2 = 4._dp*MFd(i1)*MFu(i2)*Real(SA_DerB0(p2,MFd2(i1),MFu2(i2)),dp) 
coupL1 = cplcFdFucVWpL(i1,i2)
coupR1 = cplcFdFucVWpR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fe], Fv 
!------------------------ 
If ((Include_in_loopFe).and.(Include_in_loopFv)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFe2(i1),MFv2(i2)),dp) 
B0m2 = 4._dp*MFe(i1)*MFv(i2)*Real(SA_DerB0(p2,MFe2(i1),MFv2(i2)),dp) 
coupL1 = cplcFeFvcVWpL(i1,i2)
coupR1 = cplcFeFvcVWpR(i1,i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxe], Fxv 
!------------------------ 
If ((Include_in_loopFxe).and.(Include_in_loopFxv)) Then 
sumI = 0._dp 
 
      Do i2 = 1, 2
 H0m2 = Real(SA_DerHloop(p2,MFxe2,MFxv2(i2)),dp) 
B0m2 = 4._dp*MFxe*MFxv(i2)*Real(SA_DerB0(p2,MFxe2,MFxv2(i2)),dp) 
coupL1 = cplcFxeFxvcVWpL(i2)
coupR1 = cplcFxeFxvcVWpR(i2)
    SumI = (Abs(coupL1)**2+Abs(coupR1)**2)*H0m2 & 
                & + (Real(Conjg(coupL1)*coupR1,dp))*B0m2 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! bar[gWpC], gP 
!------------------------ 
If ((Include_in_loopgWC).and.(Include_in_loopgA)) Then 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVP2,MVWp2),dp)
coup1 = cplcgWCgAcVWp
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gP], gWp 
!------------------------ 
If ((Include_in_loopgA).and.(Include_in_loopgWp)) Then 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVWp2,MVP2),dp)
coup1 = cplcgAgWpcVWp
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gZ], gWp 
!------------------------ 
If ((Include_in_loopgZ).and.(Include_in_loopgWp)) Then 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVWp2,MVZ2),dp)
coup1 = cplcgZgWpcVWp
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWpC], gZ 
!------------------------ 
If ((Include_in_loopgWC).and.(Include_in_loopgZ)) Then 
sumI = 0._dp 
 
SumI = 0._dp 
B0m2 = Real(DerVGGloop(p2,MVZ2,MVWp2),dp)
coup1 = cplcgWCgZcVWp
coup2 = Conjg(coup1) 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! Hp, hh 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loophh)) Then 
sumI = 0._dp 
 
      Do i2 = 1, 2
 B22m2 = Real(DerVSSloop(p2,Mhh2(i2),MHp2),dp)  
coup1 = cplhhHpcVWp(i2)
    SumI = Abs(coup1)**2*B22m2 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! VWp, hh 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loophh)) Then 
sumI = 0._dp 
 
      Do i2 = 1, 2
 B0m2 = Real(DerVVSloop(p2,MVWp2,Mhh2(i2)),dp)
coup1 = cplhhcVWpVWp(i2)
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! VP, Hp 
!------------------------ 
If ((Include_in_loopVP).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
B0m2 = Real(DerVVSloop(p2,MVP2,MHp2),dp)
coup1 = cplHpcVWpVP
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VZ, Hp 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
B0m2 = Real(DerVVSloop(p2,MVZ2,MHp2),dp)
coup1 = cplHpcVWpVZ
    SumI = Abs(coup1)**2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VWp, VP 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopVP)) Then 
sumI = 0._dp 
 
coup1 = cplcVWpVPVWp
coup2 = Conjg(coup1) 
    SumI = -DerVVVloop(p2,MVWp2,MVP2)*coup1*coup2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VZ, VWp 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
sumI = 0._dp 
 
coup1 = cplcVWpVWpVZ
coup2 = Conjg(coup1) 
    SumI = -DerVVVloop(p2,MVZ2,MVWp2)*coup1*coup2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! Ah 
!------------------------ 
If (Include_in_loopAh) Then 
SumI = 0._dp 
 A0m2 = SA_DerA0(MAh2)
 coup1 = cplAhAhcVWpVWp
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
End if 
!------------------------ 
! hh 
!------------------------ 
If (Include_in_loophh) Then 
    Do i1 = 1, 2
 SumI = 0._dp 
 A0m2 = SA_DerA0(Mhh2(i1))
 coup1 = cplhhhhcVWpVWp(i1,i1)
 SumI = coup1*A0m2 
res = res +1._dp/2._dp* SumI  
      End Do 
 End if 
!------------------------ 
! conj[Hp] 
!------------------------ 
If (Include_in_loopHp) Then 
SumI = 0._dp 
 A0m2 = SA_DerA0(MHp2)
 coup1 = cplHpcHpcVWpVWp
 SumI = coup1*A0m2 
res = res +1* SumI  
End if 
!------------------------ 
! VP 
!------------------------ 
If (Include_in_loopVP) Then 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_DerA0(MVP2) +RXi/4._dp*SA_DerA0(MVP2*RXi) 
coup1 = cplcVWpVPVPVWp3
coup2 = cplcVWpVPVPVWp1
coup3 = cplcVWpVPVPVWp2
SumI = ((2._dp*DerrMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVP2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1._dp/2._dp* SumI  
End if 
!------------------------ 
! conj[VWp] 
!------------------------ 
If (Include_in_loopVWp) Then 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_DerA0(MVWp2) +RXi/4._dp*SA_DerA0(MVWp2*RXi) 
coup1 = cplcVWpcVWpVWpVWp2
coup2 = cplcVWpcVWpVWpVWp3
coup3 = cplcVWpcVWpVWpVWp1
SumI = ((2._dp*DerrMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVWp2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
End if 
!------------------------ 
! VZ 
!------------------------ 
If (Include_in_loopVZ) Then 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_DerA0(MVZ2) +RXi/4._dp*SA_DerA0(MVZ2*RXi) 
coup1 = cplcVWpVWpVZVZ1
coup2 = cplcVWpVWpVZVZ2
coup3 = cplcVWpVWpVZVZ3
SumI = ((2._dp*DerrMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVZ2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1._dp/2._dp* SumI  
End if 
res = oo16pi2*res 
 
End Subroutine DerPi1LoopVWp 
 
Subroutine Sigma1LoopFeMZ(p2,MFe,MFe2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,            & 
& MFv,MFv2,MVWp,MVWp2,MSsc,MSsc2,MFxe,MFxe2,cplcUFeFeAhL,cplcUFeFeAhR,cplcUFeFehhL,      & 
& cplcUFeFehhR,cplcUFeFeVPL,cplcUFeFeVPR,cplcUFeFeVZL,cplcUFeFeVZR,cplcUFeFvcHpL,        & 
& cplcUFeFvcHpR,cplcUFeFvcVWpL,cplcUFeFvcVWpR,cplcUFeFxecSscL,cplcUFeFxecSscR,           & 
& sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MFe(3),MFe2(3),MAh,MAh2,Mhh(2),Mhh2(2),MVZ,MVZ2,MHp,MHp2,MFv(3),MFv2(3),              & 
& MVWp,MVWp2,MSsc(2),MSsc2(2),MFxe,MFxe2

Complex(dp), Intent(in) :: cplcUFeFeAhL(3,3),cplcUFeFeAhR(3,3),cplcUFeFehhL(3,3,2),cplcUFeFehhR(3,3,2),          & 
& cplcUFeFeVPL(3,3),cplcUFeFeVPR(3,3),cplcUFeFeVZL(3,3),cplcUFeFeVZR(3,3),               & 
& cplcUFeFvcHpL(3,3),cplcUFeFvcHpR(3,3),cplcUFeFvcVWpL(3,3),cplcUFeFvcVWpR(3,3),         & 
& cplcUFeFxecSscL(3,2),cplcUFeFxecSscR(3,2)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigSL(3,3), SigSR(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumSL(3,3), sumSR(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fe, Ah 
!------------------------ 
If ((Include_in_loopFe).and.(Include_in_loopAh)) Then 
    Do i1 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFe2(gO1),MFe2(i1),MAh2),dp) 
B0m2 = MFe(i1)*Real(SA_B0(MFe2(gO1),MFe2(i1),MAh2),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFe2(i1),MAh2),dp) 
B0m2 = MFe(i1)*Real(SA_B0(p2,MFe2(i1),MAh2),dp) 
End If 
coupL1 = cplcUFeFeAhL(gO1,i1)
coupR1 = cplcUFeFeAhR(gO1,i1)
coupL2 =  Conjg(cplcUFeFeAhL(gO2,i1))
coupR2 =  Conjg(cplcUFeFeAhR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 End if 
!------------------------ 
! hh, Fe 
!------------------------ 
If ((Include_in_loophh).and.(Include_in_loopFe)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFe2(gO1),MFe2(i2),Mhh2(i1)),dp) 
B0m2 = MFe(i2)*Real(SA_B0(MFe2(gO1),MFe2(i2),Mhh2(i1)),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFe2(i2),Mhh2(i1)),dp) 
B0m2 = MFe(i2)*Real(SA_B0(p2,MFe2(i2),Mhh2(i1)),dp) 
End If 
coupL1 = cplcUFeFehhL(gO1,i2,i1)
coupR1 = cplcUFeFehhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFeFehhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFeFehhR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 End if 
!------------------------ 
! VZ, Fe 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopFe)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -2._dp*Real(SA_B1(MFe2(gO1),MFe2(i2),MVZ2),dp) 
B0m2 = -4._dp*MFe(i2)*Real(SA_B0(MFe2(gO1),MFe2(i2),MVZ2)-0.5_dp*rMS,dp) 
Else 
B1m2 = -2._dp*Real(SA_B1(p2,MFe2(i2),MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFe(i2)*Real(SA_B0(p2,MFe2(i2),MVZ2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFeFeVZL(gO1,i2)
coupR1 = cplcUFeFeVZR(gO1,i2)
coupL2 =  Conjg(cplcUFeFeVZL(gO2,i2))
coupR2 =  Conjg(cplcUFeFeVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! conj[Hp], Fv 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopFv)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFe2(gO1),MFv2(i2),MHp2),dp) 
B0m2 = MFv(i2)*Real(SA_B0(MFe2(gO1),MFv2(i2),MHp2),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFv2(i2),MHp2),dp) 
B0m2 = MFv(i2)*Real(SA_B0(p2,MFv2(i2),MHp2),dp) 
End If 
coupL1 = cplcUFeFvcHpL(gO1,i2)
coupR1 = cplcUFeFvcHpR(gO1,i2)
coupL2 =  Conjg(cplcUFeFvcHpL(gO2,i2))
coupR2 =  Conjg(cplcUFeFvcHpR(gO2,i2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! conj[VWp], Fv 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopFv)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -2._dp*Real(SA_B1(MFe2(gO1),MFv2(i2),MVWp2),dp) 
B0m2 = -4._dp*MFv(i2)*Real(SA_B0(MFe2(gO1),MFv2(i2),MVWp2)-0.5_dp*rMS,dp) 
Else 
B1m2 = -2._dp*Real(SA_B1(p2,MFv2(i2),MVWp2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFv(i2)*Real(SA_B0(p2,MFv2(i2),MVWp2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFeFvcVWpL(gO1,i2)
coupR1 = cplcUFeFvcVWpR(gO1,i2)
coupL2 =  Conjg(cplcUFeFvcVWpL(gO2,i2))
coupR2 =  Conjg(cplcUFeFvcVWpR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! conj[Ssc], Fxe 
!------------------------ 
If ((Include_in_loopSsc).and.(Include_in_loopFxe)) Then 
    Do i1 = 1, 2
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFe2(gO1),MFxe2,MSsc2(i1)),dp) 
B0m2 = MFxe*Real(SA_B0(MFe2(gO1),MFxe2,MSsc2(i1)),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFxe2,MSsc2(i1)),dp) 
B0m2 = MFxe*Real(SA_B0(p2,MFxe2,MSsc2(i1)),dp) 
End If 
coupL1 = cplcUFeFxecSscL(gO1,i1)
coupR1 = cplcUFeFxecSscR(gO1,i1)
coupL2 =  Conjg(cplcUFeFxecSscL(gO2,i1))
coupR2 =  Conjg(cplcUFeFxecSscR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 End if 
SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine Sigma1LoopFeMZ 
 
Subroutine Sigma1LoopFdMZ(p2,MFd,MFd2,MAh,MAh2,Mhh,Mhh2,MVZ,MVZ2,MHp,MHp2,            & 
& MFu,MFu2,MVWp,MVWp2,cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdFdhhL,cplcUFdFdhhR,               & 
& cplcUFdFdVGL,cplcUFdFdVGR,cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,         & 
& cplcUFdFucHpL,cplcUFdFucHpR,cplcUFdFucVWpL,cplcUFdFucVWpR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MAh,MAh2,Mhh(2),Mhh2(2),MVZ,MVZ2,MHp,MHp2,MFu(3),MFu2(3),              & 
& MVWp,MVWp2

Complex(dp), Intent(in) :: cplcUFdFdAhL(3,3),cplcUFdFdAhR(3,3),cplcUFdFdhhL(3,3,2),cplcUFdFdhhR(3,3,2),          & 
& cplcUFdFdVGL(3,3),cplcUFdFdVGR(3,3),cplcUFdFdVPL(3,3),cplcUFdFdVPR(3,3),               & 
& cplcUFdFdVZL(3,3),cplcUFdFdVZR(3,3),cplcUFdFucHpL(3,3),cplcUFdFucHpR(3,3),             & 
& cplcUFdFucVWpL(3,3),cplcUFdFucVWpR(3,3)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigSL(3,3), SigSR(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumSL(3,3), sumSR(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fd, Ah 
!------------------------ 
If ((Include_in_loopFd).and.(Include_in_loopAh)) Then 
    Do i1 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFd2(gO1),MFd2(i1),MAh2),dp) 
B0m2 = MFd(i1)*Real(SA_B0(MFd2(gO1),MFd2(i1),MAh2),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFd2(i1),MAh2),dp) 
B0m2 = MFd(i1)*Real(SA_B0(p2,MFd2(i1),MAh2),dp) 
End If 
coupL1 = cplcUFdFdAhL(gO1,i1)
coupR1 = cplcUFdFdAhR(gO1,i1)
coupL2 =  Conjg(cplcUFdFdAhL(gO2,i1))
coupR2 =  Conjg(cplcUFdFdAhR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 End if 
!------------------------ 
! hh, Fd 
!------------------------ 
If ((Include_in_loophh).and.(Include_in_loopFd)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFd2(gO1),MFd2(i2),Mhh2(i1)),dp) 
B0m2 = MFd(i2)*Real(SA_B0(MFd2(gO1),MFd2(i2),Mhh2(i1)),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFd2(i2),Mhh2(i1)),dp) 
B0m2 = MFd(i2)*Real(SA_B0(p2,MFd2(i2),Mhh2(i1)),dp) 
End If 
coupL1 = cplcUFdFdhhL(gO1,i2,i1)
coupR1 = cplcUFdFdhhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFdFdhhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFdFdhhR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 End if 
!------------------------ 
! VZ, Fd 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopFd)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -2._dp*Real(SA_B1(MFd2(gO1),MFd2(i2),MVZ2),dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_B0(MFd2(gO1),MFd2(i2),MVZ2)-0.5_dp*rMS,dp) 
Else 
B1m2 = -2._dp*Real(SA_B1(p2,MFd2(i2),MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_B0(p2,MFd2(i2),MVZ2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFdFdVZL(gO1,i2)
coupR1 = cplcUFdFdVZR(gO1,i2)
coupL2 =  Conjg(cplcUFdFdVZL(gO2,i2))
coupR2 =  Conjg(cplcUFdFdVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! conj[Hp], Fu 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopFu)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFd2(gO1),MFu2(i2),MHp2),dp) 
B0m2 = MFu(i2)*Real(SA_B0(MFd2(gO1),MFu2(i2),MHp2),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFu2(i2),MHp2),dp) 
B0m2 = MFu(i2)*Real(SA_B0(p2,MFu2(i2),MHp2),dp) 
End If 
coupL1 = cplcUFdFucHpL(gO1,i2)
coupR1 = cplcUFdFucHpR(gO1,i2)
coupL2 =  Conjg(cplcUFdFucHpL(gO2,i2))
coupR2 =  Conjg(cplcUFdFucHpR(gO2,i2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! conj[VWp], Fu 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopFu)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -2._dp*Real(SA_B1(MFd2(gO1),MFu2(i2),MVWp2),dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_B0(MFd2(gO1),MFu2(i2),MVWp2)-0.5_dp*rMS,dp) 
Else 
B1m2 = -2._dp*Real(SA_B1(p2,MFu2(i2),MVWp2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_B0(p2,MFu2(i2),MVWp2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFdFucVWpL(gO1,i2)
coupR1 = cplcUFdFucVWpR(gO1,i2)
coupL2 =  Conjg(cplcUFdFucVWpL(gO2,i2))
coupR2 =  Conjg(cplcUFdFucVWpR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine Sigma1LoopFdMZ 
 
Subroutine Sigma1LoopFuMZ(p2,MFu,MFu2,MAh,MAh2,MHp,MHp2,MFd,MFd2,MVWp,MVWp2,          & 
& Mhh,Mhh2,MVZ,MVZ2,cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuFdHpL,cplcUFuFdHpR,cplcUFuFdVWpL,   & 
& cplcUFuFdVWpR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,cplcUFuFuVPL,        & 
& cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,sigL,sigR,sigSL,sigSR)

Implicit None 
Real(dp), Intent(in) :: MFu(3),MFu2(3),MAh,MAh2,MHp,MHp2,MFd(3),MFd2(3),MVWp,MVWp2,Mhh(2),Mhh2(2),MVZ,MVZ2

Complex(dp), Intent(in) :: cplcUFuFuAhL(3,3),cplcUFuFuAhR(3,3),cplcUFuFdHpL(3,3),cplcUFuFdHpR(3,3),              & 
& cplcUFuFdVWpL(3,3),cplcUFuFdVWpR(3,3),cplcUFuFuhhL(3,3,2),cplcUFuFuhhR(3,3,2),         & 
& cplcUFuFuVGL(3,3),cplcUFuFuVGR(3,3),cplcUFuFuVPL(3,3),cplcUFuFuVPR(3,3),               & 
& cplcUFuFuVZL(3,3),cplcUFuFuVZR(3,3)

Complex(dp), Intent(out) :: SigL(3,3),SigR(3,3), SigSL(3,3), SigSR(3,3) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2,temp, sumL(3,3), sumR(3,3), sumSL(3,3), sumSR(3,3) 
Real(dp) :: B0m2, F0m2, G0m2,B1m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
SigL = Cmplx(0._dp,0._dp,dp) 
SigR = Cmplx(0._dp,0._dp,dp) 
SigSL = Cmplx(0._dp,0._dp,dp) 
 SigSR = Cmplx(0._dp,0._dp,dp) 
 
!------------------------ 
! Fu, Ah 
!------------------------ 
If ((Include_in_loopFu).and.(Include_in_loopAh)) Then 
    Do i1 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFu2(gO1),MFu2(i1),MAh2),dp) 
B0m2 = MFu(i1)*Real(SA_B0(MFu2(gO1),MFu2(i1),MAh2),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFu2(i1),MAh2),dp) 
B0m2 = MFu(i1)*Real(SA_B0(p2,MFu2(i1),MAh2),dp) 
End If 
coupL1 = cplcUFuFuAhL(gO1,i1)
coupR1 = cplcUFuFuAhR(gO1,i1)
coupL2 =  Conjg(cplcUFuFuAhL(gO2,i1))
coupR2 =  Conjg(cplcUFuFuAhR(gO2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
 End if 
!------------------------ 
! Hp, Fd 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopFd)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFu2(gO1),MFd2(i2),MHp2),dp) 
B0m2 = MFd(i2)*Real(SA_B0(MFu2(gO1),MFd2(i2),MHp2),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFd2(i2),MHp2),dp) 
B0m2 = MFd(i2)*Real(SA_B0(p2,MFd2(i2),MHp2),dp) 
End If 
coupL1 = cplcUFuFdHpL(gO1,i2)
coupR1 = cplcUFuFdHpR(gO1,i2)
coupL2 =  Conjg(cplcUFuFdHpL(gO2,i2))
coupR2 =  Conjg(cplcUFuFdHpR(gO2,i2))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! VWp, Fd 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopFd)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -2._dp*Real(SA_B1(MFu2(gO1),MFd2(i2),MVWp2),dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_B0(MFu2(gO1),MFd2(i2),MVWp2)-0.5_dp*rMS,dp) 
Else 
B1m2 = -2._dp*Real(SA_B1(p2,MFd2(i2),MVWp2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFd(i2)*Real(SA_B0(p2,MFd2(i2),MVWp2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFuFdVWpL(gO1,i2)
coupR1 = cplcUFuFdVWpR(gO1,i2)
coupL2 =  Conjg(cplcUFuFdVWpL(gO2,i2))
coupR2 =  Conjg(cplcUFuFdVWpR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
!------------------------ 
! hh, Fu 
!------------------------ 
If ((Include_in_loophh).and.(Include_in_loopFu)) Then 
    Do i1 = 1, 2
       Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -Real(SA_B1(MFu2(gO1),MFu2(i2),Mhh2(i1)),dp) 
B0m2 = MFu(i2)*Real(SA_B0(MFu2(gO1),MFu2(i2),Mhh2(i1)),dp) 
Else 
B1m2 = -Real(SA_B1(p2,MFu2(i2),Mhh2(i1)),dp) 
B0m2 = MFu(i2)*Real(SA_B0(p2,MFu2(i2),Mhh2(i1)),dp) 
End If 
coupL1 = cplcUFuFuhhL(gO1,i2,i1)
coupR1 = cplcUFuFuhhR(gO1,i2,i1)
coupL2 =  Conjg(cplcUFuFuhhL(gO2,i2,i1))
coupR2 =  Conjg(cplcUFuFuhhR(gO2,i2,i1))
SumSL(gO1,gO2) = coupR1*coupL2*B0m2 
SumSR(gO1,gO2) = coupL1*coupR2*B0m2 
sumR(gO1,gO2) = coupR1*coupR2*B1m2 
sumL(gO1,gO2) = coupL1*coupL2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
      End Do 
     End Do 
 End if 
!------------------------ 
! VZ, Fu 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loopFu)) Then 
      Do i2 = 1, 3
 SumSL = 0._dp 
SumSR = 0._dp 
sumR = 0._dp 
sumL = 0._dp 
Do gO1 = 1, 3
  Do gO2 = 1, 3
If(gO1.eq.gO2) Then 
B1m2 = -2._dp*Real(SA_B1(MFu2(gO1),MFu2(i2),MVZ2),dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_B0(MFu2(gO1),MFu2(i2),MVZ2)-0.5_dp*rMS,dp) 
Else 
B1m2 = -2._dp*Real(SA_B1(p2,MFu2(i2),MVZ2)+ 0.5_dp*rMS,dp) 
B0m2 = -4._dp*MFu(i2)*Real(SA_B0(p2,MFu2(i2),MVZ2)-0.5_dp*rMS,dp) 
End If 
coupL1 = cplcUFuFuVZL(gO1,i2)
coupR1 = cplcUFuFuVZR(gO1,i2)
coupL2 =  Conjg(cplcUFuFuVZL(gO2,i2))
coupR2 =  Conjg(cplcUFuFuVZR(gO2,i2))
SumSL(gO1,gO2) = coupL1*coupR2*B0m2 
SumSR(gO1,gO2) = coupR1*coupL2*B0m2 
sumR(gO1,gO2) = coupL1*coupL2*B1m2 
sumL(gO1,gO2) = coupR1*coupR2*B1m2 
   End Do 
End Do 
SigL = SigL +1._dp* sumL
SigR = SigR +1._dp* sumR 
SigSL = SigSL +1._dp* sumSL 
SigSR = SigSR +1._dp* sumSR 
    End Do 
 End if 
SigL = oo16pi2*SigL 
SigR = oo16pi2*SigR 
SigSL = oo16pi2*SigSL 
SigSR = oo16pi2*SigSR 
 
End Subroutine Sigma1LoopFuMZ 
 
Subroutine Pi1LoopVPVZ(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,MHp2,             & 
& MVWp,MVWp2,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVPL,cplcFeFeVPR,    & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcgWCgWCVP,cplcgWCgWCVZ,     & 
& cplcgWpgWpVP,cplcgWpgWpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVPVWpVZ1,        & 
& cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpVWpVZ,cplHpcHpVP,cplHpcHpVPVZ,cplHpcHpVZ,       & 
& cplHpcVWpVP,cplHpcVWpVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFxe,MFxe2,MHp,MHp2,MVWp,MVWp2

Complex(dp), Intent(in) :: cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVPL(3,3), & 
& cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),  & 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,           & 
& cplcFxeFxeVZR,cplcgWCgWCVP,cplcgWCgWCVZ,cplcgWpgWpVP,cplcgWpgWpVZ,cplcHpVPVWp,         & 
& cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVPVWpVZ1,cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,              & 
& cplcVWpVWpVZ,cplHpcHpVP,cplHpcHpVPVZ,cplHpcHpVZ,cplHpcVWpVP,cplHpcVWpVZ

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! bar[Fd], Fd 
!------------------------ 
If ((Include_in_loopFd).and.(Include_in_loopFd)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = 4._dp*MFd(i1)*MFd(i2)*Real(SA_B0(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVPL(i1,i2)
coupR1 = cplcFdFdVPR(i1,i2)
coupL2 = cplcFdFdVZL(i2,i1)
coupR2 = cplcFdFdVZR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fe], Fe 
!------------------------ 
If ((Include_in_loopFe).and.(Include_in_loopFe)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFe2(i1),MFe2(i2)),dp) 
B0m2 = 4._dp*MFe(i1)*MFe(i2)*Real(SA_B0(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVPL(i1,i2)
coupR1 = cplcFeFeVPR(i1,i2)
coupL2 = cplcFeFeVZL(i2,i1)
coupR2 = cplcFeFeVZR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fu], Fu 
!------------------------ 
If ((Include_in_loopFu).and.(Include_in_loopFu)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_Hloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = 4._dp*MFu(i1)*MFu(i2)*Real(SA_B0(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVPL(i1,i2)
coupR1 = cplcFuFuVPR(i1,i2)
coupL2 = cplcFuFuVZL(i2,i1)
coupR2 = cplcFuFuVZR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxe], Fxe 
!------------------------ 
If ((Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
sumI = 0._dp 
 
H0m2 = Real(SA_Hloop(p2,MFxe2,MFxe2),dp) 
B0m2 = 4._dp*MFxe*MFxe*Real(SA_B0(p2,MFxe2,MFxe2),dp) 
coupL1 = cplcFxeFxeVPL
coupR1 = cplcFxeFxeVPR
coupL2 = cplcFxeFxeVZL
coupR2 = cplcFxeFxeVZR
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWp], gWp 
!------------------------ 
If ((Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
sumI = 0._dp 
 
B0m2 = Real(VGGloop(p2,MVWp2,MVWp2),dp)
coup1 = cplcgWpgWpVP
coup2 = cplcgWpgWpVZ 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWpC], gWpC 
!------------------------ 
If ((Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
sumI = 0._dp 
 
B0m2 = Real(VGGloop(p2,MVWp2,MVWp2),dp)
coup1 = cplcgWCgWCVP
coup2 = cplcgWCgWCVZ 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Hp], Hp 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
B22m2 = Real(VSSloop(p2,MHp2,MHp2),dp) 
coup1 = cplHpcHpVP
coup2 = cplHpcHpVZ
    SumI = coup1*coup2*B22m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[VWp], Hp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
B0m2 = Real(VVSloop(p2,MVWp2,MHp2),dp) 
coup1 = cplHpcVWpVP
coup2 = cplcHpVWpVZ
    SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Hp], VWp 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
sumI = 0._dp 
 
B0m2 = Real(VVSloop(p2,MVWp2,MHp2),dp) 
coup1 = cplcHpVPVWp
coup2 = cplHpcVWpVZ
    SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[VWp], VWp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
sumI = 0._dp 
 
B0m2 = Real(VVVloop(p2,MVWp2,MVWp2),dp) 
coup1 = cplcVWpVPVWp
coup2 = cplcVWpVWpVZ
    SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Hp] 
!------------------------ 
If (Include_in_loopHp) Then 
SumI = 0._dp 
 A0m2 = SA_A0(MHp2)
 coup1 = cplHpcHpVPVZ
 SumI = coup1*A0m2 
res = res +1* SumI  
End if 
!------------------------ 
! conj[VWp] 
!------------------------ 
If (Include_in_loopVWp) Then 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_A0(MVWp2) +RXi/4._dp*SA_A0(MVWp2*RXi) 
coup1 = cplcVWpVPVWpVZ2
coup2 = cplcVWpVPVWpVZ1
coup3 = cplcVWpVPVWpVZ3
SumI = ((2._dp*rMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVWp2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
End if 
res = oo16pi2*res 
 
End Subroutine Pi1LoopVPVZ 
 
Subroutine DerPi1LoopVPVZ(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxe,MFxe2,MHp,               & 
& MHp2,MVWp,MVWp2,cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVPL,           & 
& cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,               & 
& cplcFuFuVZR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcgWCgWCVP,      & 
& cplcgWCgWCVZ,cplcgWpgWpVP,cplcgWpgWpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,           & 
& cplcVWpVPVWpVZ1,cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,cplcVWpVWpVZ,cplHpcHpVP,               & 
& cplHpcHpVPVZ,cplHpcHpVZ,cplHpcVWpVP,cplHpcVWpVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFxe,MFxe2,MHp,MHp2,MVWp,MVWp2

Complex(dp), Intent(in) :: cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVPL(3,3), & 
& cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),  & 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,           & 
& cplcFxeFxeVZR,cplcgWCgWCVP,cplcgWCgWCVZ,cplcgWpgWpVP,cplcgWpgWpVZ,cplcHpVPVWp,         & 
& cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVPVWpVZ1,cplcVWpVPVWpVZ2,cplcVWpVPVWpVZ3,              & 
& cplcVWpVWpVZ,cplHpcHpVP,cplHpcHpVPVZ,cplHpcHpVZ,cplHpcVWpVP,cplHpcVWpVZ

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! bar[Fd], Fd 
!------------------------ 
If ((Include_in_loopFd).and.(Include_in_loopFd)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFd2(i1),MFd2(i2)),dp) 
B0m2 = 2._dp*MFd(i1)*MFd(i2)*Real(SA_DerB0(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVPL(i1,i2)
coupR1 = cplcFdFdVPR(i1,i2)
coupL2 = cplcFdFdVZL(i2,i1)
coupR2 = cplcFdFdVZR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fe], Fe 
!------------------------ 
If ((Include_in_loopFe).and.(Include_in_loopFe)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFe2(i1),MFe2(i2)),dp) 
B0m2 = 2._dp*MFe(i1)*MFe(i2)*Real(SA_DerB0(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVPL(i1,i2)
coupR1 = cplcFeFeVPR(i1,i2)
coupL2 = cplcFeFeVZL(i2,i1)
coupR2 = cplcFeFeVZR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fu], Fu 
!------------------------ 
If ((Include_in_loopFu).and.(Include_in_loopFu)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 H0m2 = Real(SA_DerHloop(p2,MFu2(i1),MFu2(i2)),dp) 
B0m2 = 2._dp*MFu(i1)*MFu(i2)*Real(SA_DerB0(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVPL(i1,i2)
coupR1 = cplcFuFuVPR(i1,i2)
coupL2 = cplcFuFuVZL(i2,i1)
coupR2 = cplcFuFuVZR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxe], Fxe 
!------------------------ 
If ((Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
sumI = 0._dp 
 
H0m2 = Real(SA_DerHloop(p2,MFxe2,MFxe2),dp) 
B0m2 = 2._dp*MFxe*MFxe*Real(SA_DerB0(p2,MFxe2,MFxe2),dp) 
coupL1 = cplcFxeFxeVPL
coupR1 = cplcFxeFxeVPR
coupL2 = cplcFxeFxeVZL
coupR2 = cplcFxeFxeVZR
    SumI = (coupL1*coupL2+coupR1*coupR2)*H0m2 & 
                & + 0.5_dp*(coupL1*coupR2 + coupL2*coupR1)*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWp], gWp 
!------------------------ 
If ((Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
sumI = 0._dp 
 
B0m2 = Real(DerVGGloop(p2,MVWp2,MVWp2),dp)
coup1 = cplcgWpgWpVP
coup2 = cplcgWpgWpVZ 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWpC], gWpC 
!------------------------ 
If ((Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
sumI = 0._dp 
 
B0m2 = Real(DerVGGloop(p2,MVWp2,MVWp2),dp)
coup1 = cplcgWCgWCVP
coup2 = cplcgWCgWCVZ 
   SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Hp], Hp 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
B22m2 = Real(DerVSSloop(p2,MHp2,MHp2),dp) 
coup1 = cplHpcHpVP
coup2 = cplHpcHpVZ
    SumI = coup1*coup2*B22m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[VWp], Hp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
B0m2 = Real(DerVVSloop(p2,MVWp2,MHp2),dp) 
coup1 = cplHpcVWpVP
coup2 = cplcHpVWpVZ
    SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Hp], VWp 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
sumI = 0._dp 
 
B0m2 = Real(DerVVSloop(p2,MVWp2,MHp2),dp) 
coup1 = cplcHpVPVWp
coup2 = cplHpcVWpVZ
    SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[VWp], VWp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
sumI = 0._dp 
 
B0m2 = Real(DerVVVloop(p2,MVWp2,MVWp2),dp) 
coup1 = cplcVWpVPVWp
coup2 = cplcVWpVWpVZ
    SumI = coup1*coup2*B0m2 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Hp] 
!------------------------ 
If (Include_in_loopHp) Then 
SumI = 0._dp 
 A0m2 = SA_DerA0(MHp2)
 coup1 = cplHpcHpVPVZ
 SumI = coup1*A0m2 
res = res +1* SumI  
End if 
!------------------------ 
! conj[VWp] 
!------------------------ 
If (Include_in_loopVWp) Then 
SumI = 0._dp 
A0m2 = 3._dp/4._dp*SA_DerA0(MVWp2) +RXi/4._dp*SA_DerA0(MVWp2*RXi) 
coup1 = cplcVWpVPVWpVZ2
coup2 = cplcVWpVPVWpVZ1
coup3 = cplcVWpVPVWpVZ3
SumI = ((2._dp*DerrMS*coup1+(1-RXi**2)/8._dp*(coup2+coup3))*MVWp2-(4._dp*coup1+coup2+coup3)*A0m2)
res = res +1* SumI  
End if 
res = oo16pi2*res 
 
End Subroutine DerPi1LoopVPVZ 
 
Subroutine Pi1LoopVZAh(p2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,             & 
& Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2,cplAhAhhh,cplAhcHpVWp,cplAhhhVZ,cplAhHpcVWp,     & 
& cplcFdFdAhL,cplcFdFdAhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeAhL,cplcFeFeAhR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuAhL,cplcFuFuAhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWCgWCAh,cplcgWCgWCVZ,     & 
& cplcgWpgWpAh,cplcgWpgWpVZ,cplcHpVWpVZ,cplhhVZVZ,cplHpcVWpVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFxv(2),MFxv2(2),               & 
& Mhh(2),Mhh2(2),MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplAhcHpVWp,cplAhhhVZ(2),cplAhHpcVWp,cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),  & 
& cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFeFeVZL(3,3),  & 
& cplcFeFeVZR(3,3),cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),  & 
& cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),           & 
& cplcgWCgWCAh,cplcgWCgWCVZ,cplcgWpgWpAh,cplcgWpgWpVZ,cplcHpVWpVZ,cplhhVZVZ(2),          & 
& cplHpcVWpVZ

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! hh, Ah 
!------------------------ 
If ((Include_in_loophh).and.(Include_in_loopAh)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 2
 Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MAh2,Mhh2(i1)),dp) 
B1m2 = Real(SA_B1(p2,MAh2,Mhh2(i1)),dp) 
coup1 = cplAhhhVZ(i1)
coup2 = cplAhAhhh(i1)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
      End Do 
 End if 
!------------------------ 
! bar[Fd], Fd 
!------------------------ 
If ((Include_in_loopFd).and.(Include_in_loopFd)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MFd2(i1),MFd2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVZL(i1,i2)
coupR1 = cplcFdFdVZR(i1,i2)
coupL2 = cplcFdFdAhL(i2,i1)
coupR2 = cplcFdFdAhR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFd(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFd(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fe], Fe 
!------------------------ 
If ((Include_in_loopFe).and.(Include_in_loopFe)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MFe2(i1),MFe2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVZL(i1,i2)
coupR1 = cplcFeFeVZR(i1,i2)
coupL2 = cplcFeFeAhL(i2,i1)
coupR2 = cplcFeFeAhR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFe(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFe(i2)*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fu], Fu 
!------------------------ 
If ((Include_in_loopFu).and.(Include_in_loopFu)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MFu2(i1),MFu2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVZL(i1,i2)
coupR1 = cplcFuFuVZR(i1,i2)
coupL2 = cplcFuFuAhL(i2,i1)
coupR2 = cplcFuFuAhR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFu(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFu(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxv], Fxv 
!------------------------ 
If ((Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MFxv2(i1),MFxv2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MFxv2(i1),MFxv2(i2)),dp) 
coupL1 = cplcFxvFxvVZL(i1,i2)
coupR1 = cplcFxvFxvVZR(i1,i2)
coupL2 = cplcFxvFxvAhL(i2,i1)
coupR2 = cplcFxvFxvAhR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFxv(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFxv(i2)*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[gWp], gWp 
!------------------------ 
If ((Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MVWp2,MVWp2),dp) 
B1m2 = Real(SA_B1(p2,MVWp2,MVWp2),dp) 
coup1 = cplcgWpgWpVZ
coup2 = cplcgWpgWpAh 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWpC], gWpC 
!------------------------ 
If ((Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MVWp2,MVWp2),dp) 
B1m2 = Real(SA_B1(p2,MVWp2,MVWp2),dp) 
coup1 = cplcgWCgWCVZ
coup2 = cplcgWCgWCAh 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VZ, hh 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loophh)) Then 
sumI = 0._dp 
 
      Do i2 = 1, 2
 Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MVZ2,Mhh2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MVZ2,Mhh2(i2)),dp) 
coup1 = cplhhVZVZ(i2)
coup2 = cplAhhhVZ(i2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +0.5_dp* SumI  
    End Do 
 End if 
!------------------------ 
! conj[VWp], Hp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MVWp2,MHp2),dp) 
B1m2 = Real(SA_B1(p2,MVWp2,MHp2),dp) 
coup1 = cplHpcVWpVZ
coup2 = cplAhcHpVWp
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Hp], VWp 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MVWp2,MHp2),dp) 
B1m2 = Real(SA_B1(p2,MVWp2,MHp2),dp) 
coup1 = cplcHpVWpVZ
coup2 = cplAhHpcVWp
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
End if 
res = oo16pi2*res 
 
End Subroutine Pi1LoopVZAh 
 
Subroutine DerPi1LoopVZAh(p2,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,          & 
& Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2,cplAhAhhh,cplAhcHpVWp,cplAhhhVZ,cplAhHpcVWp,     & 
& cplcFdFdAhL,cplcFdFdAhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeAhL,cplcFeFeAhR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuAhL,cplcFuFuAhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWCgWCAh,cplcgWCgWCVZ,     & 
& cplcgWpgWpAh,cplcgWpgWpVZ,cplcHpVWpVZ,cplhhVZVZ,cplHpcVWpVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFxv(2),MFxv2(2),               & 
& Mhh(2),Mhh2(2),MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplAhcHpVWp,cplAhhhVZ(2),cplAhHpcVWp,cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),  & 
& cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFeFeVZL(3,3),  & 
& cplcFeFeVZR(3,3),cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),  & 
& cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),           & 
& cplcgWCgWCAh,cplcgWCgWCVZ,cplcgWpgWpAh,cplcgWpgWpVZ,cplcHpVWpVZ,cplhhVZVZ(2),          & 
& cplHpcVWpVZ

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! hh, Ah 
!------------------------ 
If ((Include_in_loophh).and.(Include_in_loopAh)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 2
 Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MAh2,Mhh2(i1)),dp) 
B1m2 = Real(SA_DerB1(p2,MAh2,Mhh2(i1)),dp) 
coup1 = cplAhhhVZ(i1)
coup2 = cplAhAhhh(i1)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
      End Do 
 End if 
!------------------------ 
! bar[Fd], Fd 
!------------------------ 
If ((Include_in_loopFd).and.(Include_in_loopFd)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MFd2(i1),MFd2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVZL(i1,i2)
coupR1 = cplcFdFdVZR(i1,i2)
coupL2 = cplcFdFdAhL(i2,i1)
coupR2 = cplcFdFdAhR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFd(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFd(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fe], Fe 
!------------------------ 
If ((Include_in_loopFe).and.(Include_in_loopFe)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MFe2(i1),MFe2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVZL(i1,i2)
coupR1 = cplcFeFeVZR(i1,i2)
coupL2 = cplcFeFeAhL(i2,i1)
coupR2 = cplcFeFeAhR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFe(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFe(i2)*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fu], Fu 
!------------------------ 
If ((Include_in_loopFu).and.(Include_in_loopFu)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MFu2(i1),MFu2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVZL(i1,i2)
coupR1 = cplcFuFuVZR(i1,i2)
coupL2 = cplcFuFuAhL(i2,i1)
coupR2 = cplcFuFuAhR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFu(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFu(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxv], Fxv 
!------------------------ 
If ((Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MFxv2(i1),MFxv2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MFxv2(i1),MFxv2(i2)),dp) 
coupL1 = cplcFxvFxvVZL(i1,i2)
coupR1 = cplcFxvFxvVZR(i1,i2)
coupL2 = cplcFxvFxvAhL(i2,i1)
coupR2 = cplcFxvFxvAhR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFxv(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFxv(i2)*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[gWp], gWp 
!------------------------ 
If ((Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MVWp2,MVWp2),dp) 
B1m2 = Real(SA_DerB1(p2,MVWp2,MVWp2),dp) 
coup1 = cplcgWpgWpVZ
coup2 = cplcgWpgWpAh 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWpC], gWpC 
!------------------------ 
If ((Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MVWp2,MVWp2),dp) 
B1m2 = Real(SA_DerB1(p2,MVWp2,MVWp2),dp) 
coup1 = cplcgWCgWCVZ
coup2 = cplcgWCgWCAh 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! VZ, hh 
!------------------------ 
If ((Include_in_loopVZ).and.(Include_in_loophh)) Then 
sumI = 0._dp 
 
      Do i2 = 1, 2
 Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MVZ2,Mhh2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MVZ2,Mhh2(i2)),dp) 
coup1 = cplhhVZVZ(i2)
coup2 = cplAhhhVZ(i2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +0.5_dp* SumI  
    End Do 
 End if 
!------------------------ 
! conj[VWp], Hp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MVWp2,MHp2),dp) 
B1m2 = Real(SA_DerB1(p2,MVWp2,MHp2),dp) 
coup1 = cplHpcVWpVZ
coup2 = cplAhcHpVWp
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Hp], VWp 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MVWp2,MHp2),dp) 
B1m2 = Real(SA_DerB1(p2,MVWp2,MHp2),dp) 
coup1 = cplcHpVWpVZ
coup2 = cplAhHpcVWp
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
End if 
res = oo16pi2*res 
 
End Subroutine DerPi1LoopVZAh 
 
Subroutine Pi1LoopVZhh(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,MHp,MHp2,             & 
& MVWp,MVWp2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,    & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWCgWChh,cplcgWCgWCVZ,     & 
& cplcgWpgWphh,cplcgWpgWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,           & 
& cplhhHpcHp,cplhhHpcVWp,cplHpcHpVZ,cplHpcVWpVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFxv(2),MFxv2(2),MHp,MHp2,               & 
& MVWp,MVWp2

Complex(dp), Intent(in) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),              & 
& cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),               & 
& cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),               & 
& cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),       & 
& cplcgWCgWChh(2),cplcgWCgWCVZ,cplcgWpgWphh(2),cplcgWpgWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,    & 
& cplhhcHpVWp(2),cplhhcVWpVWp(2),cplhhHpcHp(2),cplhhHpcVWp(2),cplHpcHpVZ,cplHpcVWpVZ

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(2) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! bar[Fd], Fd 
!------------------------ 
If ((Include_in_loopFd).and.(Include_in_loopFd)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MFd2(i1),MFd2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVZL(i1,i2)
coupR1 = cplcFdFdVZR(i1,i2)
coupL2 = cplcFdFdhhL(i2,i1,gO2)
coupR2 = cplcFdFdhhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFd(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFd(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fe], Fe 
!------------------------ 
If ((Include_in_loopFe).and.(Include_in_loopFe)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MFe2(i1),MFe2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVZL(i1,i2)
coupR1 = cplcFeFeVZR(i1,i2)
coupL2 = cplcFeFehhL(i2,i1,gO2)
coupR2 = cplcFeFehhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFe(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFe(i2)*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fu], Fu 
!------------------------ 
If ((Include_in_loopFu).and.(Include_in_loopFu)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MFu2(i1),MFu2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVZL(i1,i2)
coupR1 = cplcFuFuVZR(i1,i2)
coupL2 = cplcFuFuhhL(i2,i1,gO2)
coupR2 = cplcFuFuhhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFu(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFu(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxv], Fxv 
!------------------------ 
If ((Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MFxv2(i1),MFxv2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MFxv2(i1),MFxv2(i2)),dp) 
coupL1 = cplcFxvFxvVZL(i1,i2)
coupR1 = cplcFxvFxvVZR(i1,i2)
coupL2 = cplcFxvFxvhhL(i2,i1,gO2)
coupR2 = cplcFxvFxvhhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFxv(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFxv(i2)*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[gWp], gWp 
!------------------------ 
If ((Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MVWp2,MVWp2),dp) 
B1m2 = Real(SA_B1(p2,MVWp2,MVWp2),dp) 
coup1 = cplcgWpgWpVZ
coup2 = cplcgWpgWphh(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWpC], gWpC 
!------------------------ 
If ((Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MVWp2,MVWp2),dp) 
B1m2 = Real(SA_B1(p2,MVWp2,MVWp2),dp) 
coup1 = cplcgWCgWCVZ
coup2 = cplcgWCgWChh(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Hp], Hp 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MHp2,MHp2),dp) 
B1m2 = Real(SA_B1(p2,MHp2,MHp2),dp) 
coup1 = cplHpcHpVZ
coup2 = cplhhHpcHp(gO2)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[VWp], Hp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MVWp2,MHp2),dp) 
B1m2 = Real(SA_B1(p2,MVWp2,MHp2),dp) 
coup1 = cplHpcVWpVZ
coup2 = cplhhcHpVWp(gO2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Hp], VWp 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MVWp2,MHp2),dp) 
B1m2 = Real(SA_B1(p2,MVWp2,MHp2),dp) 
coup1 = cplcHpVWpVZ
coup2 = cplhhHpcVWp(gO2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[VWp], VWp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_B0(p2,MVWp2,MVWp2),dp)
B1m2 = Real(SA_B1(p2,MVWp2,MVWp2),dp) 
coup1 = cplcVWpVWpVZ
coup2 = cplhhcVWpVWp(gO2)
    SumI = coup1*coup2*(3._dp/2._dp*B0m2+3._dp*B1m2) 
End do 
res = res +1._dp* SumI  
End if 
res = oo16pi2*res 
 
End Subroutine Pi1LoopVZhh 
 
Subroutine DerPi1LoopVZhh(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFxv,MFxv2,MHp,               & 
& MHp2,MVWp,MVWp2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,           & 
& cplcFeFehhR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,               & 
& cplcFuFuVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWCgWChh,      & 
& cplcgWCgWCVZ,cplcgWpgWphh,cplcgWpgWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplhhcHpVWp,           & 
& cplhhcVWpVWp,cplhhHpcHp,cplhhHpcVWp,cplHpcHpVZ,cplHpcVWpVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFxv(2),MFxv2(2),MHp,MHp2,               & 
& MVWp,MVWp2

Complex(dp), Intent(in) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),              & 
& cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),               & 
& cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),               & 
& cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),       & 
& cplcgWCgWChh(2),cplcgWCgWCVZ,cplcgWpgWphh(2),cplcgWpgWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,    & 
& cplhhcHpVWp(2),cplhhcVWpVWp(2),cplhhHpcHp(2),cplhhHpcVWp(2),cplHpcHpVZ,cplHpcVWpVZ

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res(2) 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! bar[Fd], Fd 
!------------------------ 
If ((Include_in_loopFd).and.(Include_in_loopFd)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MFd2(i1),MFd2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MFd2(i1),MFd2(i2)),dp) 
coupL1 = cplcFdFdVZL(i1,i2)
coupR1 = cplcFdFdVZR(i1,i2)
coupL2 = cplcFdFdhhL(i2,i1,gO2)
coupR2 = cplcFdFdhhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFd(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFd(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fe], Fe 
!------------------------ 
If ((Include_in_loopFe).and.(Include_in_loopFe)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MFe2(i1),MFe2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MFe2(i1),MFe2(i2)),dp) 
coupL1 = cplcFeFeVZL(i1,i2)
coupR1 = cplcFeFeVZR(i1,i2)
coupL2 = cplcFeFehhL(i2,i1,gO2)
coupR2 = cplcFeFehhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFe(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFe(i2)*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fu], Fu 
!------------------------ 
If ((Include_in_loopFu).and.(Include_in_loopFu)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MFu2(i1),MFu2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MFu2(i1),MFu2(i2)),dp) 
coupL1 = cplcFuFuVZL(i1,i2)
coupR1 = cplcFuFuVZR(i1,i2)
coupL2 = cplcFuFuhhL(i2,i1,gO2)
coupR2 = cplcFuFuhhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFu(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFu(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxv], Fxv 
!------------------------ 
If ((Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 2
       Do i2 = 1, 2
 Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MFxv2(i1),MFxv2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MFxv2(i1),MFxv2(i2)),dp) 
coupL1 = cplcFxvFxvVZL(i1,i2)
coupR1 = cplcFxvFxvVZR(i1,i2)
coupL2 = cplcFxvFxvhhL(i2,i1,gO2)
coupR2 = cplcFxvFxvhhR(i2,i1,gO2)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFxv(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFxv(i2)*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[gWp], gWp 
!------------------------ 
If ((Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MVWp2,MVWp2),dp) 
B1m2 = Real(SA_DerB1(p2,MVWp2,MVWp2),dp) 
coup1 = cplcgWpgWpVZ
coup2 = cplcgWpgWphh(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWpC], gWpC 
!------------------------ 
If ((Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MVWp2,MVWp2),dp) 
B1m2 = Real(SA_DerB1(p2,MVWp2,MVWp2),dp) 
coup1 = cplcgWCgWCVZ
coup2 = cplcgWCgWChh(gO2) 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Hp], Hp 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MHp2,MHp2),dp) 
B1m2 = Real(SA_DerB1(p2,MHp2,MHp2),dp) 
coup1 = cplHpcHpVZ
coup2 = cplhhHpcHp(gO2)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[VWp], Hp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MVWp2,MHp2),dp) 
B1m2 = Real(SA_DerB1(p2,MVWp2,MHp2),dp) 
coup1 = cplHpcVWpVZ
coup2 = cplhhcHpVWp(gO2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Hp], VWp 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MVWp2,MHp2),dp) 
B1m2 = Real(SA_DerB1(p2,MVWp2,MHp2),dp) 
coup1 = cplcHpVWpVZ
coup2 = cplhhHpcVWp(gO2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[VWp], VWp 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
sumI = 0._dp 
 
Do gO2=1,2 
B0m2 = Real(SA_DerB0(p2,MVWp2,MVWp2),dp)
B1m2 = Real(SA_DerB1(p2,MVWp2,MVWp2),dp) 
coup1 = cplcVWpVWpVZ
coup2 = cplhhcVWpVWp(gO2)
    SumI = coup1*coup2*(3._dp/2._dp*B0m2+3._dp*B1m2) 
End do 
res = res +1._dp* SumI  
End if 
res = oo16pi2*res 
 
End Subroutine DerPi1LoopVZhh 
 
Subroutine Pi1LoopVWpHp(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,            & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2,cplcFdFucHpL,cplcFdFucHpR,            & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFvFeVWpL,cplcFvFeVWpR,         & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcgAgWCVWp,              & 
& cplcgWCgAcHp,cplcgWCgZcHp,cplcgWpgZVWp,cplcgZgWCVWp,cplcgZgWpcHp,cplcHpVPVWp,          & 
& cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,cplhhHpcHp,             & 
& cplHpcHpVP,cplHpcHpVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,MFxe2,               & 
& MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),              & 
& cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),               & 
& cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),               & 
& cplcgAgWCVWp,cplcgWCgAcHp,cplcgWCgZcHp,cplcgWpgZVWp,cplcgZgWCVWp,cplcgZgWpcHp,         & 
& cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp(2),cplhhcVWpVWp(2),      & 
& cplhhHpcHp(2),cplHpcHpVP,cplHpcHpVZ

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
res = 0._dp 
 
!------------------------ 
! bar[Fu], Fd 
!------------------------ 
If ((Include_in_loopFu).and.(Include_in_loopFd)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MFu2(i1),MFd2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MFu2(i1),MFd2(i2)),dp) 
coupL1 = cplcFuFdVWpL(i1,i2)
coupR1 = cplcFuFdVWpR(i1,i2)
coupL2 = cplcFdFucHpL(i2,i1)
coupR2 = cplcFdFucHpR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFu(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFd(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fv], Fe 
!------------------------ 
If ((Include_in_loopFv).and.(Include_in_loopFe)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MFv2(i1),MFe2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MFv2(i1),MFe2(i2)),dp) 
coupL1 = cplcFvFeVWpL(i1,i2)
coupR1 = cplcFvFeVWpR(i1,i2)
coupL2 = cplcFeFvcHpL(i2,i1)
coupR2 = cplcFeFvcHpR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFv(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFe(i2)*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxv], Fxe 
!------------------------ 
If ((Include_in_loopFxv).and.(Include_in_loopFxe)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 2
 Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MFxv2(i1),MFxe2),dp) 
B1m2 = Real(SA_B1(p2,MFxv2(i1),MFxe2),dp) 
coupL1 = cplcFxvFxeVWpL(i1)
coupR1 = cplcFxvFxeVWpR(i1)
coupL2 = cplcFxeFxvcHpL(i1)
coupR2 = cplcFxeFxvcHpR(i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFxv(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFxe*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
 End if 
!------------------------ 
! bar[gP], gWpC 
!------------------------ 
If ((Include_in_loopgA).and.(Include_in_loopgWC)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MVWp2,0._dp),dp) 
B1m2 = Real(SA_B1(p2,MVWp2,0._dp),dp) 
coup1 = cplcgAgWCVWp
coup2 = cplcgWCgAcHp 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gZ], gWpC 
!------------------------ 
If ((Include_in_loopgZ).and.(Include_in_loopgWC)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MVWp2,MVZ2),dp) 
B1m2 = Real(SA_B1(p2,MVWp2,MVZ2),dp) 
coup1 = cplcgZgWCVWp
coup2 = cplcgWCgZcHp 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWp], gZ 
!------------------------ 
If ((Include_in_loopgWp).and.(Include_in_loopgZ)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MVZ2,MVWp2),dp) 
B1m2 = Real(SA_B1(p2,MVZ2,MVWp2),dp) 
coup1 = cplcgWpgZVWp
coup2 = cplcgZgWpcHp 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Hp], hh 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loophh)) Then 
sumI = 0._dp 
 
      Do i2 = 1, 2
 Do gO2=1,1 
B0m2 = Real(SA_B0(p2,Mhh2(i2),MHp2),dp) 
B1m2 = Real(SA_B1(p2,Mhh2(i2),MHp2),dp) 
coup1 = cplhhcHpVWp(i2)
coup2 = cplhhHpcHp(i2)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! conj[VWp], hh 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loophh)) Then 
sumI = 0._dp 
 
      Do i2 = 1, 2
 Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MVWp2,Mhh2(i2)),dp) 
B1m2 = Real(SA_B1(p2,MVWp2,Mhh2(i2)),dp) 
coup1 = cplhhcVWpVWp(i2)
coup2 = cplhhcHpVWp(i2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! conj[Hp], VP 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopVP)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_B0(p2,0._dp,MHp2),dp) 
B1m2 = Real(SA_B1(p2,0._dp,MHp2),dp) 
coup1 = cplcHpVPVWp
coup2 = cplHpcHpVP
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[VWp], VP 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopVP)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MVWp2,0._dp),dp)
B1m2 = Real(SA_B1(p2,MVWp2,0._dp),dp) 
coup1 = cplcVWpVPVWp
coup2 = cplcHpVPVWp
    SumI = coup1*coup2*(3._dp/2._dp*B0m2+3._dp*B1m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Hp], VZ 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopVZ)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MVZ2,MHp2),dp) 
B1m2 = Real(SA_B1(p2,MVZ2,MHp2),dp) 
coup1 = cplcHpVWpVZ
coup2 = cplHpcHpVZ
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[VWp], VZ 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopVZ)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_B0(p2,MVWp2,MVZ2),dp)
B1m2 = Real(SA_B1(p2,MVWp2,MVZ2),dp) 
coup1 = cplcVWpVWpVZ
coup2 = cplcHpVWpVZ
    SumI = coup1*coup2*(3._dp/2._dp*B0m2+3._dp*B1m2) 
End do 
res = res +1._dp* SumI  
End if 
res = oo16pi2*res 
 
End Subroutine Pi1LoopVWpHp 
 
Subroutine DerPi1LoopVWpHp(p2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,               & 
& MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2,cplcFdFucHpL,cplcFdFucHpR,      & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFvFeVWpL,cplcFvFeVWpR,         & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcgAgWCVWp,              & 
& cplcgWCgAcHp,cplcgWCgZcHp,cplcgWpgZVWp,cplcgZgWCVWp,cplcgZgWpcHp,cplcHpVPVWp,          & 
& cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp,cplhhcVWpVWp,cplhhHpcHp,             & 
& cplHpcHpVP,cplHpcHpVZ,kont,res)

Implicit None 
Real(dp), Intent(in) :: MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,MFxe2,               & 
& MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MVWp,MVWp2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),              & 
& cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),               & 
& cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),               & 
& cplcgAgWCVWp,cplcgWCgAcHp,cplcgWCgZcHp,cplcgWpgZVWp,cplcgZgWCVWp,cplcgZgWpcHp,         & 
& cplcHpVPVWp,cplcHpVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,cplhhcHpVWp(2),cplhhcVWpVWp(2),      & 
& cplhhHpcHp(2),cplHpcHpVP,cplHpcHpVZ

Integer, Intent(inout) :: kont 
Real(dp) :: B0m2, F0m2, G0m2, B1m2, H0m2, B22m2, m1, m2 
Real(dp), Intent(in) :: p2 
Complex(dp) :: A0m2 
Complex(dp), Intent(inout) :: res 
Complex(dp) :: coupL1, coupR1, coupL2,coupR2, coup1,coup2, coup3, temp, sumI 
Integer :: i1,i2,i3,i4, gO1, gO2, ierr 
 
 
Real(dp) ::MVG,MVP,MVG2,MVP2
MVG = Mass_Regulator_PhotonGluon 
MVP = Mass_Regulator_PhotonGluon 
MVG2 = Mass_Regulator_PhotonGluon**2 
MVP2 = Mass_Regulator_PhotonGluon**2 

res = 0._dp 
 
!------------------------ 
! bar[Fu], Fd 
!------------------------ 
If ((Include_in_loopFu).and.(Include_in_loopFd)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MFu2(i1),MFd2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MFu2(i1),MFd2(i2)),dp) 
coupL1 = cplcFuFdVWpL(i1,i2)
coupR1 = cplcFuFdVWpR(i1,i2)
coupL2 = cplcFdFucHpL(i2,i1)
coupR2 = cplcFdFucHpR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFu(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFd(i2)*B1m2  
End do 
res = res +3._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fv], Fe 
!------------------------ 
If ((Include_in_loopFv).and.(Include_in_loopFe)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 3
       Do i2 = 1, 3
 Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MFv2(i1),MFe2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MFv2(i1),MFe2(i2)),dp) 
coupL1 = cplcFvFeVWpL(i1,i2)
coupR1 = cplcFvFeVWpR(i1,i2)
coupL2 = cplcFeFvcHpL(i2,i1)
coupR2 = cplcFeFvcHpR(i2,i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFv(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFe(i2)*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
     End Do 
 End if 
!------------------------ 
! bar[Fxv], Fxe 
!------------------------ 
If ((Include_in_loopFxv).and.(Include_in_loopFxe)) Then 
sumI = 0._dp 
 
    Do i1 = 1, 2
 Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MFxv2(i1),MFxe2),dp) 
B1m2 = Real(SA_DerB1(p2,MFxv2(i1),MFxe2),dp) 
coupL1 = cplcFxvFxeVWpL(i1)
coupR1 = cplcFxvFxeVWpR(i1)
coupL2 = cplcFxeFxvcHpL(i1)
coupR2 = cplcFxeFxvcHpR(i1)
    SumI = (coupL1*coupL2+coupR1*coupR2)*MFxv(i1)*(B0m2+B1m2) & 
  & + (coupL1*coupR2+coupR1*coupL2)*MFxe*B1m2  
End do 
res = res +1._dp* SumI  
      End Do 
 End if 
!------------------------ 
! bar[gP], gWpC 
!------------------------ 
If ((Include_in_loopgA).and.(Include_in_loopgWC)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MVWp2,MVP2),dp) 
B1m2 = Real(SA_DerB1(p2,MVWp2,MVP2),dp) 
coup1 = cplcgAgWCVWp
coup2 = cplcgWCgAcHp 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gZ], gWpC 
!------------------------ 
If ((Include_in_loopgZ).and.(Include_in_loopgWC)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MVWp2,MVZ2),dp) 
B1m2 = Real(SA_DerB1(p2,MVWp2,MVZ2),dp) 
coup1 = cplcgZgWCVWp
coup2 = cplcgWCgZcHp 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! bar[gWp], gZ 
!------------------------ 
If ((Include_in_loopgWp).and.(Include_in_loopgZ)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MVZ2,MVWp2),dp) 
B1m2 = Real(SA_DerB1(p2,MVZ2,MVWp2),dp) 
coup1 = cplcgWpgZVWp
coup2 = cplcgZgWpcHp 
   SumI = -0.5_dp*coup1*coup2*(B0m2+B1m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Hp], hh 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loophh)) Then 
sumI = 0._dp 
 
      Do i2 = 1, 2
 Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,Mhh2(i2),MHp2),dp) 
B1m2 = Real(SA_DerB1(p2,Mhh2(i2),MHp2),dp) 
coup1 = cplhhcHpVWp(i2)
coup2 = cplhhHpcHp(i2)
    SumI = -coup1*coup2*(B1m2+0.5_dp*B0m2)
End do 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! conj[VWp], hh 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loophh)) Then 
sumI = 0._dp 
 
      Do i2 = 1, 2
 Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MVWp2,Mhh2(i2)),dp) 
B1m2 = Real(SA_DerB1(p2,MVWp2,Mhh2(i2)),dp) 
coup1 = cplhhcVWpVWp(i2)
coup2 = cplhhcHpVWp(i2)
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
    End Do 
 End if 
!------------------------ 
! conj[Hp], VP 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopVP)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MVP2,MHp2),dp) 
B1m2 = Real(SA_DerB1(p2,MVP2,MHp2),dp) 
coup1 = cplcHpVPVWp
coup2 = cplHpcHpVP
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[VWp], VP 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopVP)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MVWp2,MVP2),dp)
B1m2 = Real(SA_DerB1(p2,MVWp2,MVP2),dp) 
coup1 = cplcVWpVPVWp
coup2 = cplcHpVPVWp
    SumI = coup1*coup2*(3._dp/2._dp*B0m2+3._dp*B1m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[Hp], VZ 
!------------------------ 
If ((Include_in_loopHp).and.(Include_in_loopVZ)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MVZ2,MHp2),dp) 
B1m2 = Real(SA_DerB1(p2,MVZ2,MHp2),dp) 
coup1 = cplcHpVWpVZ
coup2 = cplHpcHpVZ
    SumI = coup1*coup2*(B1m2-B0m2) 
End do 
res = res +1._dp* SumI  
End if 
!------------------------ 
! conj[VWp], VZ 
!------------------------ 
If ((Include_in_loopVWp).and.(Include_in_loopVZ)) Then 
sumI = 0._dp 
 
Do gO2=1,1 
B0m2 = Real(SA_DerB0(p2,MVWp2,MVZ2),dp)
B1m2 = Real(SA_DerB1(p2,MVWp2,MVZ2),dp) 
coup1 = cplcVWpVWpVZ
coup2 = cplcHpVWpVZ
    SumI = coup1*coup2*(3._dp/2._dp*B0m2+3._dp*B1m2) 
End do 
res = res +1._dp* SumI  
End if 
res = oo16pi2*res 
 
End Subroutine DerPi1LoopVWpHp 
 
End Module LoopMasses_SDdiracDM 
