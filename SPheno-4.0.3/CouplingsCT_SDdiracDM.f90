! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:21 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module CouplingsCT_SDdiracDM
 
Use Control 
Use Settings 
Use Model_Data_SDdiracDM 
Use Mathematics, Only: CompareMatrices, Adjungate 
 
Contains 
 
 Subroutine CalculateCouplingCT(Lam,LSPH,vvSM,vS,ZH,LSP,LS1H,LS2H,VSs,g1,              & 
& g2,TW,g3,Yd,ZDL,ZDR,Ye,ZEL,ZER,Yu,ZUL,ZUR,YRD,XV,XU,UV,YRA1,YRA2,YRB1,YRB2,            & 
& UVR,YRC,dLam,dLSPH,dvvSM,dvS,dZH,dLSP,dLS1H,dLS2H,dVSs,dg1,dg2,dSinTW,dCosTW,          & 
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

Implicit None 
Real(dp), Intent(in) :: LSPH,vvSM,vS,ZH(2,2),LSP,LS1H,LS2H,VSs(2,2),g1,g2,TW,g3,YRD,YRA1(3),YRA2(3),          & 
& YRB1(3),YRB2(3),YRC,dLSPH,dvvSM,dvS,dZH(2,2),dLSP,dLS1H,dLS2H,dVSs(2,2),               & 
& dg1,dg2,dSinTW,dCosTW,dTanTW,dg3,dYRD,dYRA1(3),dYRA2(3),dYRB1(3),dYRB2(3),dYRC

Complex(dp), Intent(in) :: Lam,Yd(3,3),ZDL(3,3),ZDR(3,3),Ye(3,3),ZEL(3,3),ZER(3,3),Yu(3,3),ZUL(3,3),             & 
& ZUR(3,3),XV(2,2),XU(2,2),UV(3,3),UVR(3,3),dLam,dYd(3,3),dZDL(3,3),dZDR(3,3),           & 
& dYe(3,3),dZEL(3,3),dZER(3,3),dYu(3,3),dZUL(3,3),dZUR(3,3),dXV(2,2),dXU(2,2),           & 
& dUV(3,3),dUVR(3,3)

Complex(dp), Intent(out) :: ctcplAhAhhh(2),ctcplhhhhhh(2,2,2),ctcplhhHpcHp(2),ctcplhhSsccSsc(2,2,2),              & 
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

Integer :: gt1, gt2, gt3, gt4, ct1, ct2, ct3, ct4

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateCouplingCT'
 
ctcplAhAhhh = 0._dp 
Do gt3 = 1, 2
Call CT_CouplingAhAhhh(gt3,Lam,LSPH,vvSM,vS,ZH,dLam,dLSPH,dvvSM,dvS,dZH,              & 
& ctcplAhAhhh(gt3))

End Do 


ctcplhhhhhh = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CT_Couplinghhhhhh(gt1,gt2,gt3,Lam,LSP,LSPH,vvSM,vS,ZH,dLam,dLSP,dLSPH,           & 
& dvvSM,dvS,dZH,ctcplhhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


ctcplhhHpcHp = 0._dp 
Do gt1 = 1, 2
Call CT_CouplinghhHpcHp(gt1,Lam,LSPH,vvSM,vS,ZH,dLam,dLSPH,dvvSM,dvS,dZH,             & 
& ctcplhhHpcHp(gt1))

End Do 


ctcplhhSsccSsc = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CT_CouplinghhSsccSsc(gt1,gt2,gt3,LS1H,LS2H,vvSM,ZH,VSs,dLS1H,dLS2H,              & 
& dvvSM,dZH,dVSs,ctcplhhSsccSsc(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


ctcplAhhhVZ = 0._dp 
Do gt2 = 1, 2
Call CT_CouplingAhhhVZ(gt2,g1,g2,ZH,TW,dg1,dg2,dZH,dSinTW,dCosTW,dTanTW,              & 
& ctcplAhhhVZ(gt2))

End Do 


ctcplAhHpcVWp = 0._dp 
Call CT_CouplingAhHpcVWp(g2,dg2,ctcplAhHpcVWp)



ctcplAhcHpVWp = 0._dp 
Call CT_CouplingAhcHpVWp(g2,dg2,ctcplAhcHpVWp)



ctcplhhHpcVWp = 0._dp 
Do gt1 = 1, 2
Call CT_CouplinghhHpcVWp(gt1,g2,ZH,dg2,dZH,ctcplhhHpcVWp(gt1))

End Do 


ctcplhhcHpVWp = 0._dp 
Do gt1 = 1, 2
Call CT_CouplinghhcHpVWp(gt1,g2,ZH,dg2,dZH,ctcplhhcHpVWp(gt1))

End Do 


ctcplHpcHpVP = 0._dp 
Call CT_CouplingHpcHpVP(g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,ctcplHpcHpVP)



ctcplHpcHpVZ = 0._dp 
Call CT_CouplingHpcHpVZ(g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,ctcplHpcHpVZ)



ctcplhhcVWpVWp = 0._dp 
Do gt1 = 1, 2
Call CT_CouplinghhcVWpVWp(gt1,g2,vvSM,ZH,dg2,dvvSM,dZH,ctcplhhcVWpVWp(gt1))

End Do 


ctcplhhVZVZ = 0._dp 
Do gt1 = 1, 2
Call CT_CouplinghhVZVZ(gt1,g1,g2,vvSM,ZH,TW,dg1,dg2,dvvSM,dZH,dSinTW,dCosTW,          & 
& dTanTW,ctcplhhVZVZ(gt1))

End Do 


ctcplHpcVWpVP = 0._dp 
Call CT_CouplingHpcVWpVP(g1,g2,vvSM,TW,dg1,dg2,dvvSM,dSinTW,dCosTW,dTanTW,            & 
& ctcplHpcVWpVP)



ctcplHpcVWpVZ = 0._dp 
Call CT_CouplingHpcVWpVZ(g1,g2,vvSM,TW,dg1,dg2,dvvSM,dSinTW,dCosTW,dTanTW,            & 
& ctcplHpcVWpVZ)



ctcplcHpVPVWp = 0._dp 
Call CT_CouplingcHpVPVWp(g1,g2,vvSM,TW,dg1,dg2,dvvSM,dSinTW,dCosTW,dTanTW,            & 
& ctcplcHpVPVWp)



ctcplcHpVWpVZ = 0._dp 
Call CT_CouplingcHpVWpVZ(g1,g2,vvSM,TW,dg1,dg2,dvvSM,dSinTW,dCosTW,dTanTW,            & 
& ctcplcHpVWpVZ)



ctcplVGVGVG = 0._dp 
Call CT_CouplingVGVGVG(g3,dg3,ctcplVGVGVG)



ctcplcVWpVPVWp = 0._dp 
Call CT_CouplingcVWpVPVWp(g2,TW,dg2,dSinTW,dCosTW,dTanTW,ctcplcVWpVPVWp)



ctcplcVWpVWpVZ = 0._dp 
Call CT_CouplingcVWpVWpVZ(g2,TW,dg2,dSinTW,dCosTW,dTanTW,ctcplcVWpVWpVZ)



ctcplcFdFdAhL = 0._dp 
ctcplcFdFdAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFdFdAh(gt1,gt2,Yd,ZDL,ZDR,dYd,dZDL,dZDR,ctcplcFdFdAhL(gt1,gt2)       & 
& ,ctcplcFdFdAhR(gt1,gt2))

 End Do 
End Do 


ctcplcFeFeAhL = 0._dp 
ctcplcFeFeAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFeFeAh(gt1,gt2,Ye,ZEL,ZER,dYe,dZEL,dZER,ctcplcFeFeAhL(gt1,gt2)       & 
& ,ctcplcFeFeAhR(gt1,gt2))

 End Do 
End Do 


ctcplcFuFuAhL = 0._dp 
ctcplcFuFuAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFuFuAh(gt1,gt2,Yu,ZUL,ZUR,dYu,dZUL,dZUR,ctcplcFuFuAhL(gt1,gt2)       & 
& ,ctcplcFuFuAhR(gt1,gt2))

 End Do 
End Do 


ctcplcFxvFxvAhL = 0._dp 
ctcplcFxvFxvAhR = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CT_CouplingcFxvFxvAh(gt1,gt2,YRD,XV,XU,dYRD,dXV,dXU,ctcplcFxvFxvAhL(gt1,gt2)     & 
& ,ctcplcFxvFxvAhR(gt1,gt2))

 End Do 
End Do 


ctcplcFdFdhhL = 0._dp 
ctcplcFdFdhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CT_CouplingcFdFdhh(gt1,gt2,gt3,Yd,ZH,ZDL,ZDR,dYd,dZH,dZDL,dZDR,ctcplcFdFdhhL(gt1,gt2,gt3)& 
& ,ctcplcFdFdhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


ctcplcFuFdHpL = 0._dp 
ctcplcFuFdHpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFuFdHp(gt1,gt2,Yu,Yd,ZDL,ZDR,ZUL,ZUR,dYu,dYd,dZDL,dZDR,              & 
& dZUL,dZUR,ctcplcFuFdHpL(gt1,gt2),ctcplcFuFdHpR(gt1,gt2))

 End Do 
End Do 


ctcplcFeFehhL = 0._dp 
ctcplcFeFehhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CT_CouplingcFeFehh(gt1,gt2,gt3,Ye,ZH,ZEL,ZER,dYe,dZH,dZEL,dZER,ctcplcFeFehhL(gt1,gt2,gt3)& 
& ,ctcplcFeFehhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


ctcplcFvFeHpL = 0._dp 
ctcplcFvFeHpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFvFeHp(gt1,gt2,Ye,ZER,UV,dYe,dZER,dUV,ctcplcFvFeHpL(gt1,gt2)         & 
& ,ctcplcFvFeHpR(gt1,gt2))

 End Do 
End Do 


ctcplcFxeFeSscL = 0._dp 
ctcplcFxeFeSscR = 0._dp 
Do gt2 = 1, 3
 Do gt3 = 1, 2
Call CT_CouplingcFxeFeSsc(gt2,gt3,YRA1,YRA2,ZEL,VSs,dYRA1,dYRA2,dZEL,dVSs,            & 
& ctcplcFxeFeSscL(gt2,gt3),ctcplcFxeFeSscR(gt2,gt3))

 End Do 
End Do 


ctcplcFuFuhhL = 0._dp 
ctcplcFuFuhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CT_CouplingcFuFuhh(gt1,gt2,gt3,Yu,ZH,ZUL,ZUR,dYu,dZH,dZUL,dZUR,ctcplcFuFuhhL(gt1,gt2,gt3)& 
& ,ctcplcFuFuhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


ctcplcFdFucHpL = 0._dp 
ctcplcFdFucHpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFdFucHp(gt1,gt2,Yu,Yd,ZDL,ZDR,ZUL,ZUR,dYu,dYd,dZDL,dZDR,             & 
& dZUL,dZUR,ctcplcFdFucHpL(gt1,gt2),ctcplcFdFucHpR(gt1,gt2))

 End Do 
End Do 


ctcplcFxvFvSscL = 0._dp 
ctcplcFxvFvSscR = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CT_CouplingcFxvFvSsc(gt1,gt2,gt3,YRB1,YRB2,YRA1,YRA2,UV,UVR,XV,XU,               & 
& VSs,dYRB1,dYRB2,dYRA1,dYRA2,dUV,dUVR,dXV,dXU,dVSs,ctcplcFxvFvSscL(gt1,gt2,gt3)         & 
& ,ctcplcFxvFvSscR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


ctcplcFeFvcHpL = 0._dp 
ctcplcFeFvcHpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFeFvcHp(gt1,gt2,Ye,ZER,UV,dYe,dZER,dUV,ctcplcFeFvcHpL(gt1,gt2)       & 
& ,ctcplcFeFvcHpR(gt1,gt2))

 End Do 
End Do 


ctcplcFxvFxeHpL = 0._dp 
ctcplcFxvFxeHpR = 0._dp 
Do gt1 = 1, 2
Call CT_CouplingcFxvFxeHp(gt1,YRD,XU,dYRD,dXU,ctcplcFxvFxeHpL(gt1),ctcplcFxvFxeHpR(gt1))

End Do 


ctcplcFeFxecSscL = 0._dp 
ctcplcFeFxecSscR = 0._dp 
Do gt1 = 1, 3
 Do gt3 = 1, 2
Call CT_CouplingcFeFxecSsc(gt1,gt3,YRA1,YRA2,ZEL,VSs,dYRA1,dYRA2,dZEL,dVSs,           & 
& ctcplcFeFxecSscL(gt1,gt3),ctcplcFeFxecSscR(gt1,gt3))

 End Do 
End Do 


ctcplcFxvFxvhhL = 0._dp 
ctcplcFxvFxvhhR = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CT_CouplingcFxvFxvhh(gt1,gt2,gt3,YRD,YRC,ZH,XV,XU,dYRD,dYRC,dZH,dXV,             & 
& dXU,ctcplcFxvFxvhhL(gt1,gt2,gt3),ctcplcFxvFxvhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


ctcplcFvFxvcSscL = 0._dp 
ctcplcFvFxvcSscR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CT_CouplingcFvFxvcSsc(gt1,gt2,gt3,YRB1,YRB2,YRA1,YRA2,UV,UVR,XV,XU,              & 
& VSs,dYRB1,dYRB2,dYRA1,dYRA2,dUV,dUVR,dXV,dXU,dVSs,ctcplcFvFxvcSscL(gt1,gt2,gt3)        & 
& ,ctcplcFvFxvcSscR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


ctcplcFxeFxvcHpL = 0._dp 
ctcplcFxeFxvcHpR = 0._dp 
Do gt2 = 1, 2
Call CT_CouplingcFxeFxvcHp(gt2,YRD,XU,dYRD,dXU,ctcplcFxeFxvcHpL(gt2),ctcplcFxeFxvcHpR(gt2))

End Do 


ctcplcFdFdVGL = 0._dp 
ctcplcFdFdVGR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFdFdVG(gt1,gt2,g3,dg3,ctcplcFdFdVGL(gt1,gt2),ctcplcFdFdVGR(gt1,gt2))

 End Do 
End Do 


ctcplcFdFdVPL = 0._dp 
ctcplcFdFdVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFdFdVP(gt1,gt2,g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,ctcplcFdFdVPL(gt1,gt2)& 
& ,ctcplcFdFdVPR(gt1,gt2))

 End Do 
End Do 


ctcplcFuFdVWpL = 0._dp 
ctcplcFuFdVWpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFuFdVWp(gt1,gt2,g2,ZDL,ZUL,dg2,dZDL,dZUL,ctcplcFuFdVWpL(gt1,gt2)     & 
& ,ctcplcFuFdVWpR(gt1,gt2))

 End Do 
End Do 


ctcplcFdFdVZL = 0._dp 
ctcplcFdFdVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFdFdVZ(gt1,gt2,g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,ctcplcFdFdVZL(gt1,gt2)& 
& ,ctcplcFdFdVZR(gt1,gt2))

 End Do 
End Do 


ctcplcFeFeVPL = 0._dp 
ctcplcFeFeVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFeFeVP(gt1,gt2,g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,ctcplcFeFeVPL(gt1,gt2)& 
& ,ctcplcFeFeVPR(gt1,gt2))

 End Do 
End Do 


ctcplcFvFeVWpL = 0._dp 
ctcplcFvFeVWpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFvFeVWp(gt1,gt2,g2,ZEL,UV,dg2,dZEL,dUV,ctcplcFvFeVWpL(gt1,gt2)       & 
& ,ctcplcFvFeVWpR(gt1,gt2))

 End Do 
End Do 


ctcplcFeFeVZL = 0._dp 
ctcplcFeFeVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFeFeVZ(gt1,gt2,g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,ctcplcFeFeVZL(gt1,gt2)& 
& ,ctcplcFeFeVZR(gt1,gt2))

 End Do 
End Do 


ctcplcFuFuVGL = 0._dp 
ctcplcFuFuVGR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFuFuVG(gt1,gt2,g3,dg3,ctcplcFuFuVGL(gt1,gt2),ctcplcFuFuVGR(gt1,gt2))

 End Do 
End Do 


ctcplcFuFuVPL = 0._dp 
ctcplcFuFuVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFuFuVP(gt1,gt2,g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,ctcplcFuFuVPL(gt1,gt2)& 
& ,ctcplcFuFuVPR(gt1,gt2))

 End Do 
End Do 


ctcplcFuFuVZL = 0._dp 
ctcplcFuFuVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFuFuVZ(gt1,gt2,g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,ctcplcFuFuVZL(gt1,gt2)& 
& ,ctcplcFuFuVZR(gt1,gt2))

 End Do 
End Do 


ctcplcFdFucVWpL = 0._dp 
ctcplcFdFucVWpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFdFucVWp(gt1,gt2,g2,ZDL,ZUL,dg2,dZDL,dZUL,ctcplcFdFucVWpL(gt1,gt2)   & 
& ,ctcplcFdFucVWpR(gt1,gt2))

 End Do 
End Do 


ctcplcFvFvVZL = 0._dp 
ctcplcFvFvVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFvFvVZ(gt1,gt2,g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,ctcplcFvFvVZL(gt1,gt2)& 
& ,ctcplcFvFvVZR(gt1,gt2))

 End Do 
End Do 


ctcplcFeFvcVWpL = 0._dp 
ctcplcFeFvcVWpR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CT_CouplingcFeFvcVWp(gt1,gt2,g2,ZEL,UV,dg2,dZEL,dUV,ctcplcFeFvcVWpL(gt1,gt2)     & 
& ,ctcplcFeFvcVWpR(gt1,gt2))

 End Do 
End Do 


ctcplcFxeFxeVPL = 0._dp 
ctcplcFxeFxeVPR = 0._dp 
Call CT_CouplingcFxeFxeVP(g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,ctcplcFxeFxeVPL,      & 
& ctcplcFxeFxeVPR)



ctcplcFxvFxeVWpL = 0._dp 
ctcplcFxvFxeVWpR = 0._dp 
Do gt1 = 1, 2
Call CT_CouplingcFxvFxeVWp(gt1,g2,XV,XU,dg2,dXV,dXU,ctcplcFxvFxeVWpL(gt1)             & 
& ,ctcplcFxvFxeVWpR(gt1))

End Do 


ctcplcFxeFxeVZL = 0._dp 
ctcplcFxeFxeVZR = 0._dp 
Call CT_CouplingcFxeFxeVZ(g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,ctcplcFxeFxeVZL,      & 
& ctcplcFxeFxeVZR)



ctcplcFxvFxvVZL = 0._dp 
ctcplcFxvFxvVZR = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CT_CouplingcFxvFxvVZ(gt1,gt2,g1,g2,XV,XU,TW,dg1,dg2,dXV,dXU,dSinTW,              & 
& dCosTW,dTanTW,ctcplcFxvFxvVZL(gt1,gt2),ctcplcFxvFxvVZR(gt1,gt2))

 End Do 
End Do 


ctcplcFxeFxvcVWpL = 0._dp 
ctcplcFxeFxvcVWpR = 0._dp 
Do gt2 = 1, 2
Call CT_CouplingcFxeFxvcVWp(gt2,g2,XV,XU,dg2,dXV,dXU,ctcplcFxeFxvcVWpL(gt2)           & 
& ,ctcplcFxeFxvcVWpR(gt2))

End Do 


Iname = Iname - 1 
End Subroutine CalculateCouplingCT

Subroutine CT_CouplingAhAhhh(gt3,Lam,LSPH,vvSM,vS,ZH,dLam,dLSPH,dvvSM,dvS,dZH,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: LSPH,vvSM,vS,ZH(2,2),dLSPH,dvvSM,dvS,dZH(2,2)

Complex(dp), Intent(in) :: Lam,dLam

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingAhAhhh' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(Lam*vvSM*dZH(gt3,1))
res = res-(LSPH*vS*dZH(gt3,2))
res = res-(dvvSM*Lam*ZH(gt3,1))
res = res-(dLam*vvSM*ZH(gt3,1))
res = res-(dvS*LSPH*ZH(gt3,2))
res = res-(dLSPH*vS*ZH(gt3,2))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingAhAhhh  
 
 
Subroutine CT_Couplinghhhhhh(gt1,gt2,gt3,Lam,LSP,LSPH,vvSM,vS,ZH,dLam,dLSP,           & 
& dLSPH,dvvSM,dvS,dZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: LSP,LSPH,vvSM,vS,ZH(2,2),dLSP,dLSPH,dvvSM,dvS,dZH(2,2)

Complex(dp), Intent(in) :: Lam,dLam

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_Couplinghhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-3*Lam*vvSM*dZH(gt3,1)*ZH(gt1,1)*ZH(gt2,1)
res = res-(LSPH*vS*dZH(gt3,2)*ZH(gt1,1)*ZH(gt2,1))
res = res-(LSPH*vS*dZH(gt3,1)*ZH(gt1,2)*ZH(gt2,1))
res = res-(LSPH*vvSM*dZH(gt3,2)*ZH(gt1,2)*ZH(gt2,1))
res = res-(LSPH*vS*dZH(gt3,1)*ZH(gt1,1)*ZH(gt2,2))
res = res-(LSPH*vvSM*dZH(gt3,2)*ZH(gt1,1)*ZH(gt2,2))
res = res-(LSPH*vvSM*dZH(gt3,1)*ZH(gt1,2)*ZH(gt2,2))
res = res-3*LSP*vS*dZH(gt3,2)*ZH(gt1,2)*ZH(gt2,2)
res = res-3*Lam*vvSM*dZH(gt2,1)*ZH(gt1,1)*ZH(gt3,1)
res = res-(LSPH*vS*dZH(gt2,2)*ZH(gt1,1)*ZH(gt3,1))
res = res-(LSPH*vS*dZH(gt2,1)*ZH(gt1,2)*ZH(gt3,1))
res = res-(LSPH*vvSM*dZH(gt2,2)*ZH(gt1,2)*ZH(gt3,1))
res = res-3*Lam*vvSM*dZH(gt1,1)*ZH(gt2,1)*ZH(gt3,1)
res = res-(LSPH*vS*dZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1))
res = res-3*dvvSM*Lam*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,1)
res = res-3*dLam*vvSM*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,1)
res = res-(dvS*LSPH*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1))
res = res-(dLSPH*vS*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1))
res = res-(LSPH*vS*dZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1))
res = res-(LSPH*vvSM*dZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1))
res = res-(dvS*LSPH*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1))
res = res-(dLSPH*vS*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1))
res = res-(dvvSM*LSPH*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1))
res = res-(dLSPH*vvSM*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1))
res = res-(LSPH*vS*dZH(gt2,1)*ZH(gt1,1)*ZH(gt3,2))
res = res-(LSPH*vvSM*dZH(gt2,2)*ZH(gt1,1)*ZH(gt3,2))
res = res-(LSPH*vvSM*dZH(gt2,1)*ZH(gt1,2)*ZH(gt3,2))
res = res-3*LSP*vS*dZH(gt2,2)*ZH(gt1,2)*ZH(gt3,2)
res = res-(LSPH*vS*dZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2))
res = res-(LSPH*vvSM*dZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2))
res = res-(dvS*LSPH*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2))
res = res-(dLSPH*vS*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2))
res = res-(dvvSM*LSPH*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2))
res = res-(dLSPH*vvSM*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2))
res = res-(LSPH*vvSM*dZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2))
res = res-3*LSP*vS*dZH(gt1,2)*ZH(gt2,2)*ZH(gt3,2)
res = res-(dvvSM*LSPH*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2))
res = res-(dLSPH*vvSM*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2))
res = res-3*dvS*LSP*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,2)
res = res-3*dLSP*vS*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,2)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_Couplinghhhhhh  
 
 
Subroutine CT_CouplinghhHpcHp(gt1,Lam,LSPH,vvSM,vS,ZH,dLam,dLSPH,dvvSM,               & 
& dvS,dZH,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: LSPH,vvSM,vS,ZH(2,2),dLSPH,dvvSM,dvS,dZH(2,2)

Complex(dp), Intent(in) :: Lam,dLam

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplinghhHpcHp' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(Lam*vvSM*dZH(gt1,1))
res = res-(LSPH*vS*dZH(gt1,2))
res = res-(dvvSM*Lam*ZH(gt1,1))
res = res-(dLam*vvSM*ZH(gt1,1))
res = res-(dvS*LSPH*ZH(gt1,2))
res = res-(dLSPH*vS*ZH(gt1,2))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplinghhHpcHp  
 
 
Subroutine CT_CouplinghhSsccSsc(gt1,gt2,gt3,LS1H,LS2H,vvSM,ZH,VSs,dLS1H,              & 
& dLS2H,dvvSM,dZH,dVSs,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: LS1H,LS2H,vvSM,ZH(2,2),VSs(2,2),dLS1H,dLS2H,dvvSM,dZH(2,2),dVSs(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplinghhSsccSsc' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(LS1H*vvSM*dZH(gt1,1)*VSs(gt2,1)*VSs(gt3,1))
res = res-(LS2H*vvSM*dZH(gt1,1)*VSs(gt2,2)*VSs(gt3,2))
res = res-(LS1H*vvSM*dVSs(gt3,1)*VSs(gt2,1)*ZH(gt1,1))
res = res-(LS2H*vvSM*dVSs(gt3,2)*VSs(gt2,2)*ZH(gt1,1))
res = res-(LS1H*vvSM*dVSs(gt2,1)*VSs(gt3,1)*ZH(gt1,1))
res = res-(dvvSM*LS1H*VSs(gt2,1)*VSs(gt3,1)*ZH(gt1,1))
res = res-(dLS1H*vvSM*VSs(gt2,1)*VSs(gt3,1)*ZH(gt1,1))
res = res-(LS2H*vvSM*dVSs(gt2,2)*VSs(gt3,2)*ZH(gt1,1))
res = res-(dvvSM*LS2H*VSs(gt2,2)*VSs(gt3,2)*ZH(gt1,1))
res = res-(dLS2H*vvSM*VSs(gt2,2)*VSs(gt3,2)*ZH(gt1,1))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplinghhSsccSsc  
 
 
Subroutine CT_CouplingAhhhVZ(gt2,g1,g2,ZH,TW,dg1,dg2,dZH,dSinTW,dCosTW,               & 
& dTanTW,res)

Implicit None 

Integer, Intent(in) :: gt2
Real(dp), Intent(in) :: g1,g2,ZH(2,2),TW,dg1,dg2,dZH(2,2),dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingAhhhVZ' 
 
If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g2*Cos(TW)*dZH(gt2,1))/2._dp
res = res+(g1*dZH(gt2,1)*Sin(TW))/2._dp
res = res+(dSinTW*g1*ZH(gt2,1))/2._dp
res = res+(dCosTW*g2*ZH(gt2,1))/2._dp
res = res+(dg2*Cos(TW)*ZH(gt2,1))/2._dp
res = res+(dg1*Sin(TW)*ZH(gt2,1))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingAhhhVZ  
 
 
Subroutine CT_CouplingAhHpcVWp(g2,dg2,res)

Implicit None 

Real(dp), Intent(in) :: g2,dg2

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingAhHpcVWp' 
 
res = 0._dp 
res = res-1._dp*(dg2)/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingAhHpcVWp  
 
 
Subroutine CT_CouplingAhcHpVWp(g2,dg2,res)

Implicit None 

Real(dp), Intent(in) :: g2,dg2

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingAhcHpVWp' 
 
res = 0._dp 
res = res-1._dp*(dg2)/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingAhcHpVWp  
 
 
Subroutine CT_CouplinghhHpcVWp(gt1,g2,ZH,dg2,dZH,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g2,ZH(2,2),dg2,dZH(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplinghhHpcVWp' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g2*dZH(gt1,1))/2._dp
res = res-(dg2*ZH(gt1,1))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplinghhHpcVWp  
 
 
Subroutine CT_CouplinghhcHpVWp(gt1,g2,ZH,dg2,dZH,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g2,ZH(2,2),dg2,dZH(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplinghhcHpVWp' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g2*dZH(gt1,1))/2._dp
res = res+(dg2*ZH(gt1,1))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplinghhcHpVWp  
 
 
Subroutine CT_CouplingHpcHpVP(g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,res)

Implicit None 

Real(dp), Intent(in) :: g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingHpcHpVP' 
 
res = 0._dp 
res = res+(dCosTW*g1)/2._dp
res = res+(dSinTW*g2)/2._dp
res = res+(dg1*Cos(TW))/2._dp
res = res+(dg2*Sin(TW))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingHpcHpVP  
 
 
Subroutine CT_CouplingHpcHpVZ(g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,res)

Implicit None 

Real(dp), Intent(in) :: g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingHpcHpVZ' 
 
res = 0._dp 
res = res-(dSinTW*g1)/2._dp
res = res+(dCosTW*g2)/2._dp
res = res+(dg2*Cos(TW))/2._dp
res = res-(dg1*Sin(TW))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingHpcHpVZ  
 
 
Subroutine CT_CouplinghhcVWpVWp(gt1,g2,vvSM,ZH,dg2,dvvSM,dZH,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g2,vvSM,ZH(2,2),dg2,dvvSM,dZH(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplinghhcVWpVWp' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g2**2*vvSM*dZH(gt1,1))/2._dp
res = res+(dvvSM*g2**2*ZH(gt1,1))/2._dp
res = res+dg2*g2*vvSM*ZH(gt1,1)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplinghhcVWpVWp  
 
 
Subroutine CT_CouplinghhVZVZ(gt1,g1,g2,vvSM,ZH,TW,dg1,dg2,dvvSM,dZH,dSinTW,           & 
& dCosTW,dTanTW,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g1,g2,vvSM,ZH(2,2),TW,dg1,dg2,dvvSM,dZH(2,2),dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplinghhVZVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*vvSM*dZH(gt1,1))/4._dp
res = res+(g2**2*vvSM*dZH(gt1,1))/4._dp
res = res-(g1**2*vvSM*Cos(TW)**2*dZH(gt1,1))/4._dp
res = res+(g2**2*vvSM*Cos(TW)**2*dZH(gt1,1))/4._dp
res = res+g1*g2*vvSM*Cos(TW)*dZH(gt1,1)*Sin(TW)
res = res+(g1**2*vvSM*dZH(gt1,1)*Sin(TW)**2)/4._dp
res = res-(g2**2*vvSM*dZH(gt1,1)*Sin(TW)**2)/4._dp
res = res+(dvvSM*g1**2*ZH(gt1,1))/4._dp
res = res+(dvvSM*g2**2*ZH(gt1,1))/4._dp
res = res+(dg1*g1*vvSM*ZH(gt1,1))/2._dp
res = res+(dg2*g2*vvSM*ZH(gt1,1))/2._dp
res = res-(dCosTW*g1**2*vvSM*Cos(TW)*ZH(gt1,1))/2._dp
res = res+dSinTW*g1*g2*vvSM*Cos(TW)*ZH(gt1,1)
res = res+(dCosTW*g2**2*vvSM*Cos(TW)*ZH(gt1,1))/2._dp
res = res-(dvvSM*g1**2*Cos(TW)**2*ZH(gt1,1))/4._dp
res = res+(dvvSM*g2**2*Cos(TW)**2*ZH(gt1,1))/4._dp
res = res-(dg1*g1*vvSM*Cos(TW)**2*ZH(gt1,1))/2._dp
res = res+(dg2*g2*vvSM*Cos(TW)**2*ZH(gt1,1))/2._dp
res = res+(dSinTW*g1**2*vvSM*Sin(TW)*ZH(gt1,1))/2._dp
res = res+dCosTW*g1*g2*vvSM*Sin(TW)*ZH(gt1,1)
res = res-(dSinTW*g2**2*vvSM*Sin(TW)*ZH(gt1,1))/2._dp
res = res+dvvSM*g1*g2*Cos(TW)*Sin(TW)*ZH(gt1,1)
res = res+dg2*g1*vvSM*Cos(TW)*Sin(TW)*ZH(gt1,1)
res = res+dg1*g2*vvSM*Cos(TW)*Sin(TW)*ZH(gt1,1)
res = res+(dvvSM*g1**2*Sin(TW)**2*ZH(gt1,1))/4._dp
res = res-(dvvSM*g2**2*Sin(TW)**2*ZH(gt1,1))/4._dp
res = res+(dg1*g1*vvSM*Sin(TW)**2*ZH(gt1,1))/2._dp
res = res-(dg2*g2*vvSM*Sin(TW)**2*ZH(gt1,1))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplinghhVZVZ  
 
 
Subroutine CT_CouplingHpcVWpVP(g1,g2,vvSM,TW,dg1,dg2,dvvSM,dSinTW,dCosTW,             & 
& dTanTW,res)

Implicit None 

Real(dp), Intent(in) :: g1,g2,vvSM,TW,dg1,dg2,dvvSM,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingHpcVWpVP' 
 
res = 0._dp 
res = res+(dCosTW*g1*g2*vvSM)/2._dp
res = res+(dvvSM*g1*g2*Cos(TW))/2._dp
res = res+(dg2*g1*vvSM*Cos(TW))/2._dp
res = res+(dg1*g2*vvSM*Cos(TW))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingHpcVWpVP  
 
 
Subroutine CT_CouplingHpcVWpVZ(g1,g2,vvSM,TW,dg1,dg2,dvvSM,dSinTW,dCosTW,             & 
& dTanTW,res)

Implicit None 

Real(dp), Intent(in) :: g1,g2,vvSM,TW,dg1,dg2,dvvSM,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingHpcVWpVZ' 
 
res = 0._dp 
res = res-(dSinTW*g1*g2*vvSM)/2._dp
res = res-(dvvSM*g1*g2*Sin(TW))/2._dp
res = res-(dg2*g1*vvSM*Sin(TW))/2._dp
res = res-(dg1*g2*vvSM*Sin(TW))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingHpcVWpVZ  
 
 
Subroutine CT_CouplingcHpVPVWp(g1,g2,vvSM,TW,dg1,dg2,dvvSM,dSinTW,dCosTW,             & 
& dTanTW,res)

Implicit None 

Real(dp), Intent(in) :: g1,g2,vvSM,TW,dg1,dg2,dvvSM,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcHpVPVWp' 
 
res = 0._dp 
res = res+(dCosTW*g1*g2*vvSM)/2._dp
res = res+(dvvSM*g1*g2*Cos(TW))/2._dp
res = res+(dg2*g1*vvSM*Cos(TW))/2._dp
res = res+(dg1*g2*vvSM*Cos(TW))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcHpVPVWp  
 
 
Subroutine CT_CouplingcHpVWpVZ(g1,g2,vvSM,TW,dg1,dg2,dvvSM,dSinTW,dCosTW,             & 
& dTanTW,res)

Implicit None 

Real(dp), Intent(in) :: g1,g2,vvSM,TW,dg1,dg2,dvvSM,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcHpVWpVZ' 
 
res = 0._dp 
res = res-(dSinTW*g1*g2*vvSM)/2._dp
res = res-(dvvSM*g1*g2*Sin(TW))/2._dp
res = res-(dg2*g1*vvSM*Sin(TW))/2._dp
res = res-(dg1*g2*vvSM*Sin(TW))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcHpVWpVZ  
 
 
Subroutine CT_CouplingVGVGVG(g3,dg3,res)

Implicit None 

Real(dp), Intent(in) :: g3,dg3

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingVGVGVG' 
 
res = 0._dp 
res = res+dg3
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingVGVGVG  
 
 
Subroutine CT_CouplingcVWpVPVWp(g2,TW,dg2,dSinTW,dCosTW,dTanTW,res)

Implicit None 

Real(dp), Intent(in) :: g2,TW,dg2,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcVWpVPVWp' 
 
res = 0._dp 
res = res-(dSinTW*g2)
res = res-(dg2*Sin(TW))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcVWpVPVWp  
 
 
Subroutine CT_CouplingcVWpVWpVZ(g2,TW,dg2,dSinTW,dCosTW,dTanTW,res)

Implicit None 

Real(dp), Intent(in) :: g2,TW,dg2,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcVWpVWpVZ' 
 
res = 0._dp 
res = res+dCosTW*g2
res = res+dg2*Cos(TW)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcVWpVWpVZ  
 
 
Subroutine CT_CouplingcFdFdAh(gt1,gt2,Yd,ZDL,ZDR,dYd,dZDL,dZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Complex(dp), Intent(in) :: Yd(3,3),ZDL(3,3),ZDR(3,3),dYd(3,3),dZDL(3,3),dZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFdFdAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(ZDR(gt1,j1))*Conjg(ZDL(gt2,j2))*dYd(j1,j2))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(dZDL(gt2,j2))*Conjg(ZDR(gt1,j1))*Yd(j1,j2))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(dZDR(gt1,j1))*Conjg(ZDL(gt2,j2))*Yd(j1,j2))/sqrt(2._dp))
End Do 
End Do 
resR = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resR = resR+(Conjg(Yd(j1,j2))*dZDL(gt1,j2)*ZDR(gt2,j1))/sqrt(2._dp)
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR+(Conjg(Yd(j1,j2))*dZDR(gt2,j1)*ZDL(gt1,j2))/sqrt(2._dp)
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR+(Conjg(dYd(j1,j2))*ZDR(gt2,j1)*ZDL(gt1,j2))/sqrt(2._dp)
End Do 
End Do 
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFdFdAh  
 
 
Subroutine CT_CouplingcFeFeAh(gt1,gt2,Ye,ZEL,ZER,dYe,dZEL,dZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Complex(dp), Intent(in) :: Ye(3,3),ZEL(3,3),ZER(3,3),dYe(3,3),dZEL(3,3),dZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFeFeAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(ZER(gt1,j1))*Conjg(ZEL(gt2,j2))*dYe(j1,j2))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(dZEL(gt2,j2))*Conjg(ZER(gt1,j1))*Ye(j1,j2))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(dZER(gt1,j1))*Conjg(ZEL(gt2,j2))*Ye(j1,j2))/sqrt(2._dp))
End Do 
End Do 
resR = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resR = resR+(Conjg(Ye(j1,j2))*dZEL(gt1,j2)*ZER(gt2,j1))/sqrt(2._dp)
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR+(Conjg(Ye(j1,j2))*dZER(gt2,j1)*ZEL(gt1,j2))/sqrt(2._dp)
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR+(Conjg(dYe(j1,j2))*ZER(gt2,j1)*ZEL(gt1,j2))/sqrt(2._dp)
End Do 
End Do 
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFeFeAh  
 
 
Subroutine CT_CouplingcFuFuAh(gt1,gt2,Yu,ZUL,ZUR,dYu,dZUL,dZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Complex(dp), Intent(in) :: Yu(3,3),ZUL(3,3),ZUR(3,3),dYu(3,3),dZUL(3,3),dZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFuFuAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resL = resL+(Conjg(ZUR(gt1,j1))*Conjg(ZUL(gt2,j2))*dYu(j1,j2))/sqrt(2._dp)
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL+(Conjg(dZUL(gt2,j2))*Conjg(ZUR(gt1,j1))*Yu(j1,j2))/sqrt(2._dp)
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL+(Conjg(dZUR(gt1,j1))*Conjg(ZUL(gt2,j2))*Yu(j1,j2))/sqrt(2._dp)
End Do 
End Do 
resR = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-((Conjg(Yu(j1,j2))*dZUL(gt1,j2)*ZUR(gt2,j1))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-((Conjg(Yu(j1,j2))*dZUR(gt2,j1)*ZUL(gt1,j2))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-((Conjg(dYu(j1,j2))*ZUR(gt2,j1)*ZUL(gt1,j2))/sqrt(2._dp))
End Do 
End Do 
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFuFuAh  
 
 
Subroutine CT_CouplingcFxvFxvAh(gt1,gt2,YRD,XV,XU,dYRD,dXV,dXU,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: YRD,dYRD

Complex(dp), Intent(in) :: XV(2,2),XU(2,2),dXV(2,2),dXU(2,2)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFxvFxvAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL+(YRD*Conjg(dXV(gt2,1))*Conjg(XU(gt1,2)))/sqrt(2._dp)
resL = resL+(YRD*Conjg(dXU(gt1,2))*Conjg(XV(gt2,1)))/sqrt(2._dp)
resL = resL+(dYRD*Conjg(XU(gt1,2))*Conjg(XV(gt2,1)))/sqrt(2._dp)
resR = 0._dp 
resR = resR-((YRD*dXV(gt1,1)*XU(gt2,2))/sqrt(2._dp))
resR = resR-((YRD*dXU(gt2,2)*XV(gt1,1))/sqrt(2._dp))
resR = resR-((dYRD*XU(gt2,2)*XV(gt1,1))/sqrt(2._dp))
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFxvFxvAh  
 
 
Subroutine CT_CouplingcFdFdhh(gt1,gt2,gt3,Yd,ZH,ZDL,ZDR,dYd,dZH,dZDL,dZDR,            & 
& resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(2,2),dZH(2,2)

Complex(dp), Intent(in) :: Yd(3,3),ZDL(3,3),ZDR(3,3),dYd(3,3),dZDL(3,3),dZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFdFdhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(ZDR(gt1,j1))*Conjg(ZDL(gt2,j2))*dZH(gt3,1)*Yd(j1,j2))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(ZDR(gt1,j1))*Conjg(ZDL(gt2,j2))*dYd(j1,j2)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(dZDL(gt2,j2))*Conjg(ZDR(gt1,j1))*Yd(j1,j2)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(dZDR(gt1,j1))*Conjg(ZDL(gt2,j2))*Yd(j1,j2)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
resR = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-((Conjg(Yd(j1,j2))*dZH(gt3,1)*ZDR(gt2,j1)*ZDL(gt1,j2))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-((Conjg(Yd(j1,j2))*dZDL(gt1,j2)*ZDR(gt2,j1)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-((Conjg(Yd(j1,j2))*dZDR(gt2,j1)*ZDL(gt1,j2)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-((Conjg(dYd(j1,j2))*ZDR(gt2,j1)*ZDL(gt1,j2)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFdFdhh  
 
 
Subroutine CT_CouplingcFuFdHp(gt1,gt2,Yu,Yd,ZDL,ZDR,ZUL,ZUR,dYu,dYd,dZDL,             & 
& dZDR,dZUL,dZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Complex(dp), Intent(in) :: Yu(3,3),Yd(3,3),ZDL(3,3),ZDR(3,3),ZUL(3,3),ZUR(3,3),dYu(3,3),dYd(3,3),dZDL(3,3),      & 
& dZDR(3,3),dZUL(3,3),dZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFuFdHp' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resL = resL+Conjg(ZUR(gt1,j1))*Conjg(ZDL(gt2,j2))*dYu(j1,j2)
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL+Conjg(dZDL(gt2,j2))*Conjg(ZUR(gt1,j1))*Yu(j1,j2)
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL+Conjg(dZUR(gt1,j1))*Conjg(ZDL(gt2,j2))*Yu(j1,j2)
End Do 
End Do 
resR = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-(Conjg(Yd(j1,j2))*dZUL(gt1,j2)*ZDR(gt2,j1))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-(Conjg(Yd(j1,j2))*dZDR(gt2,j1)*ZUL(gt1,j2))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-(Conjg(dYd(j1,j2))*ZDR(gt2,j1)*ZUL(gt1,j2))
End Do 
End Do 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFuFdHp  
 
 
Subroutine CT_CouplingcFeFehh(gt1,gt2,gt3,Ye,ZH,ZEL,ZER,dYe,dZH,dZEL,dZER,            & 
& resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(2,2),dZH(2,2)

Complex(dp), Intent(in) :: Ye(3,3),ZEL(3,3),ZER(3,3),dYe(3,3),dZEL(3,3),dZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFeFehh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(ZER(gt1,j1))*Conjg(ZEL(gt2,j2))*dZH(gt3,1)*Ye(j1,j2))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(ZER(gt1,j1))*Conjg(ZEL(gt2,j2))*dYe(j1,j2)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(dZEL(gt2,j2))*Conjg(ZER(gt1,j1))*Ye(j1,j2)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(dZER(gt1,j1))*Conjg(ZEL(gt2,j2))*Ye(j1,j2)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
resR = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-((Conjg(Ye(j1,j2))*dZH(gt3,1)*ZER(gt2,j1)*ZEL(gt1,j2))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-((Conjg(Ye(j1,j2))*dZEL(gt1,j2)*ZER(gt2,j1)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-((Conjg(Ye(j1,j2))*dZER(gt2,j1)*ZEL(gt1,j2)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-((Conjg(dYe(j1,j2))*ZER(gt2,j1)*ZEL(gt1,j2)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFeFehh  
 
 
Subroutine CT_CouplingcFvFeHp(gt1,gt2,Ye,ZER,UV,dYe,dZER,dUV,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Complex(dp), Intent(in) :: Ye(3,3),ZER(3,3),UV(3,3),dYe(3,3),dZER(3,3),dUV(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFvFeHp' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resR = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-(Conjg(Ye(j1,j2))*dUV(gt1,j2)*ZER(gt2,j1))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-(Conjg(Ye(j1,j2))*dZER(gt2,j1)*UV(gt1,j2))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-(Conjg(dYe(j1,j2))*ZER(gt2,j1)*UV(gt1,j2))
End Do 
End Do 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFvFeHp  
 
 
Subroutine CT_CouplingcFxeFeSsc(gt2,gt3,YRA1,YRA2,ZEL,VSs,dYRA1,dYRA2,dZEL,           & 
& dVSs,resL,resR)

Implicit None 

Integer, Intent(in) :: gt2,gt3
Real(dp), Intent(in) :: YRA1(3),YRA2(3),VSs(2,2),dYRA1(3),dYRA2(3),dVSs(2,2)

Complex(dp), Intent(in) :: ZEL(3,3),dZEL(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFxeFeSsc' 
 
If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
Do j1 = 1,3
resL = resL+Conjg(ZEL(gt2,j1))*dYRA1(j1)*VSs(gt3,1)
End Do 
Do j1 = 1,3
resL = resL+Conjg(ZEL(gt2,j1))*dYRA2(j1)*VSs(gt3,2)
End Do 
Do j1 = 1,3
resL = resL+Conjg(ZEL(gt2,j1))*dVSs(gt3,1)*YRA1(j1)
End Do 
Do j1 = 1,3
resL = resL+Conjg(dZEL(gt2,j1))*VSs(gt3,1)*YRA1(j1)
End Do 
Do j1 = 1,3
resL = resL+Conjg(ZEL(gt2,j1))*dVSs(gt3,2)*YRA2(j1)
End Do 
Do j1 = 1,3
resL = resL+Conjg(dZEL(gt2,j1))*VSs(gt3,2)*YRA2(j1)
End Do 
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFxeFeSsc  
 
 
Subroutine CT_CouplingcFuFuhh(gt1,gt2,gt3,Yu,ZH,ZUL,ZUR,dYu,dZH,dZUL,dZUR,            & 
& resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(2,2),dZH(2,2)

Complex(dp), Intent(in) :: Yu(3,3),ZUL(3,3),ZUR(3,3),dYu(3,3),dZUL(3,3),dZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFuFuhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(ZUR(gt1,j1))*Conjg(ZUL(gt2,j2))*dZH(gt3,1)*Yu(j1,j2))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(ZUR(gt1,j1))*Conjg(ZUL(gt2,j2))*dYu(j1,j2)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(dZUL(gt2,j2))*Conjg(ZUR(gt1,j1))*Yu(j1,j2)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-((Conjg(dZUR(gt1,j1))*Conjg(ZUL(gt2,j2))*Yu(j1,j2)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
resR = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-((Conjg(Yu(j1,j2))*dZH(gt3,1)*ZUR(gt2,j1)*ZUL(gt1,j2))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-((Conjg(Yu(j1,j2))*dZUL(gt1,j2)*ZUR(gt2,j1)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-((Conjg(Yu(j1,j2))*dZUR(gt2,j1)*ZUL(gt1,j2)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR-((Conjg(dYu(j1,j2))*ZUR(gt2,j1)*ZUL(gt1,j2)*ZH(gt3,1))/sqrt(2._dp))
End Do 
End Do 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFuFuhh  
 
 
Subroutine CT_CouplingcFdFucHp(gt1,gt2,Yu,Yd,ZDL,ZDR,ZUL,ZUR,dYu,dYd,dZDL,            & 
& dZDR,dZUL,dZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Complex(dp), Intent(in) :: Yu(3,3),Yd(3,3),ZDL(3,3),ZDR(3,3),ZUL(3,3),ZUR(3,3),dYu(3,3),dYd(3,3),dZDL(3,3),      & 
& dZDR(3,3),dZUL(3,3),dZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFdFucHp' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-(Conjg(ZDR(gt1,j1))*Conjg(ZUL(gt2,j2))*dYd(j1,j2))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-(Conjg(dZUL(gt2,j2))*Conjg(ZDR(gt1,j1))*Yd(j1,j2))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-(Conjg(dZDR(gt1,j1))*Conjg(ZUL(gt2,j2))*Yd(j1,j2))
End Do 
End Do 
resR = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resR = resR+Conjg(Yu(j1,j2))*dZDL(gt1,j2)*ZUR(gt2,j1)
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR+Conjg(Yu(j1,j2))*dZUR(gt2,j1)*ZDL(gt1,j2)
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resR = resR+Conjg(dYu(j1,j2))*ZUR(gt2,j1)*ZDL(gt1,j2)
End Do 
End Do 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFdFucHp  
 
 
Subroutine CT_CouplingcFxvFvSsc(gt1,gt2,gt3,YRB1,YRB2,YRA1,YRA2,UV,UVR,               & 
& XV,XU,VSs,dYRB1,dYRB2,dYRA1,dYRA2,dUV,dUVR,dXV,dXU,dVSs,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: YRB1(3),YRB2(3),YRA1(3),YRA2(3),VSs(2,2),dYRB1(3),dYRB2(3),dYRA1(3),dYRA2(3),dVSs(2,2)

Complex(dp), Intent(in) :: UV(3,3),UVR(3,3),XV(2,2),XU(2,2),dUV(3,3),dUVR(3,3),dXV(2,2),dXU(2,2)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFxvFvSsc' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
Do j1 = 1,3
resL = resL+Conjg(UV(gt2,j1))*Conjg(XU(gt1,1))*dYRA1(j1)*VSs(gt3,1)
End Do 
Do j1 = 1,3
resL = resL+Conjg(UV(gt2,j1))*Conjg(XU(gt1,1))*dYRA2(j1)*VSs(gt3,2)
End Do 
Do j1 = 1,3
resL = resL+Conjg(UV(gt2,j1))*Conjg(XU(gt1,1))*dVSs(gt3,1)*YRA1(j1)
End Do 
Do j1 = 1,3
resL = resL+Conjg(dXU(gt1,1))*Conjg(UV(gt2,j1))*VSs(gt3,1)*YRA1(j1)
End Do 
Do j1 = 1,3
resL = resL+Conjg(dUV(gt2,j1))*Conjg(XU(gt1,1))*VSs(gt3,1)*YRA1(j1)
End Do 
Do j1 = 1,3
resL = resL+Conjg(UV(gt2,j1))*Conjg(XU(gt1,1))*dVSs(gt3,2)*YRA2(j1)
End Do 
Do j1 = 1,3
resL = resL+Conjg(dXU(gt1,1))*Conjg(UV(gt2,j1))*VSs(gt3,2)*YRA2(j1)
End Do 
Do j1 = 1,3
resL = resL+Conjg(dUV(gt2,j1))*Conjg(XU(gt1,1))*VSs(gt3,2)*YRA2(j1)
End Do 
resR = 0._dp 
Do j1 = 1,3
resR = resR-(dYRB1(j1)*VSs(gt3,1)*UVR(gt2,j1)*XV(gt1,2))
End Do 
Do j1 = 1,3
resR = resR-(dYRB2(j1)*VSs(gt3,2)*UVR(gt2,j1)*XV(gt1,2))
End Do 
Do j1 = 1,3
resR = resR-(dXV(gt1,2)*VSs(gt3,1)*UVR(gt2,j1)*YRB1(j1))
End Do 
Do j1 = 1,3
resR = resR-(dUVR(gt2,j1)*VSs(gt3,1)*XV(gt1,2)*YRB1(j1))
End Do 
Do j1 = 1,3
resR = resR-(dVSs(gt3,1)*UVR(gt2,j1)*XV(gt1,2)*YRB1(j1))
End Do 
Do j1 = 1,3
resR = resR-(dXV(gt1,2)*VSs(gt3,2)*UVR(gt2,j1)*YRB2(j1))
End Do 
Do j1 = 1,3
resR = resR-(dUVR(gt2,j1)*VSs(gt3,2)*XV(gt1,2)*YRB2(j1))
End Do 
Do j1 = 1,3
resR = resR-(dVSs(gt3,2)*UVR(gt2,j1)*XV(gt1,2)*YRB2(j1))
End Do 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFxvFvSsc  
 
 
Subroutine CT_CouplingcFeFvcHp(gt1,gt2,Ye,ZER,UV,dYe,dZER,dUV,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Complex(dp), Intent(in) :: Ye(3,3),ZER(3,3),UV(3,3),dYe(3,3),dZER(3,3),dUV(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFeFvcHp' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-(Conjg(ZER(gt1,j1))*Conjg(UV(gt2,j2))*dYe(j1,j2))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-(Conjg(dUV(gt2,j2))*Conjg(ZER(gt1,j1))*Ye(j1,j2))
End Do 
End Do 
Do j2 = 1,3
Do j1 = 1,3
resL = resL-(Conjg(dZER(gt1,j1))*Conjg(UV(gt2,j2))*Ye(j1,j2))
End Do 
End Do 
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFeFvcHp  
 
 
Subroutine CT_CouplingcFxvFxeHp(gt1,YRD,XU,dYRD,dXU,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: YRD,dYRD

Complex(dp), Intent(in) :: XU(2,2),dXU(2,2)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFxvFxeHp' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL+YRD*Conjg(dXU(gt1,2))
resL = resL+dYRD*Conjg(XU(gt1,2))
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFxvFxeHp  
 
 
Subroutine CT_CouplingcFeFxecSsc(gt1,gt3,YRA1,YRA2,ZEL,VSs,dYRA1,dYRA2,               & 
& dZEL,dVSs,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt3
Real(dp), Intent(in) :: YRA1(3),YRA2(3),VSs(2,2),dYRA1(3),dYRA2(3),dVSs(2,2)

Complex(dp), Intent(in) :: ZEL(3,3),dZEL(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFeFxecSsc' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resR = 0._dp 
Do j1 = 1,3
resR = resR+dYRA1(j1)*ZEL(gt1,j1)*VSs(gt3,1)
End Do 
Do j1 = 1,3
resR = resR+dYRA2(j1)*ZEL(gt1,j1)*VSs(gt3,2)
End Do 
Do j1 = 1,3
resR = resR+dVSs(gt3,1)*ZEL(gt1,j1)*YRA1(j1)
End Do 
Do j1 = 1,3
resR = resR+dZEL(gt1,j1)*VSs(gt3,1)*YRA1(j1)
End Do 
Do j1 = 1,3
resR = resR+dVSs(gt3,2)*ZEL(gt1,j1)*YRA2(j1)
End Do 
Do j1 = 1,3
resR = resR+dZEL(gt1,j1)*VSs(gt3,2)*YRA2(j1)
End Do 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFeFxecSsc  
 
 
Subroutine CT_CouplingcFxvFxvhh(gt1,gt2,gt3,YRD,YRC,ZH,XV,XU,dYRD,dYRC,               & 
& dZH,dXV,dXU,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: YRD,YRC,ZH(2,2),dYRD,dYRC,dZH(2,2)

Complex(dp), Intent(in) :: XV(2,2),XU(2,2),dXV(2,2),dXU(2,2)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFxvFxvhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((YRD*Conjg(XU(gt1,2))*Conjg(XV(gt2,1))*dZH(gt3,1))/sqrt(2._dp))
resL = resL-((YRC*Conjg(XU(gt1,2))*Conjg(XV(gt2,2))*dZH(gt3,2))/sqrt(2._dp))
resL = resL-((YRD*Conjg(dXV(gt2,1))*Conjg(XU(gt1,2))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((YRD*Conjg(dXU(gt1,2))*Conjg(XV(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((dYRD*Conjg(XU(gt1,2))*Conjg(XV(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((YRC*Conjg(dXV(gt2,2))*Conjg(XU(gt1,2))*ZH(gt3,2))/sqrt(2._dp))
resL = resL-((YRC*Conjg(dXU(gt1,2))*Conjg(XV(gt2,2))*ZH(gt3,2))/sqrt(2._dp))
resL = resL-((dYRC*Conjg(XU(gt1,2))*Conjg(XV(gt2,2))*ZH(gt3,2))/sqrt(2._dp))
resR = 0._dp 
resR = resR-((YRD*dZH(gt3,1)*XU(gt2,2)*XV(gt1,1))/sqrt(2._dp))
resR = resR-((YRC*dZH(gt3,2)*XU(gt2,2)*XV(gt1,2))/sqrt(2._dp))
resR = resR-((YRD*dXV(gt1,1)*XU(gt2,2)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((YRD*dXU(gt2,2)*XV(gt1,1)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((dYRD*XU(gt2,2)*XV(gt1,1)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((YRC*dXV(gt1,2)*XU(gt2,2)*ZH(gt3,2))/sqrt(2._dp))
resR = resR-((YRC*dXU(gt2,2)*XV(gt1,2)*ZH(gt3,2))/sqrt(2._dp))
resR = resR-((dYRC*XU(gt2,2)*XV(gt1,2)*ZH(gt3,2))/sqrt(2._dp))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFxvFxvhh  
 
 
Subroutine CT_CouplingcFvFxvcSsc(gt1,gt2,gt3,YRB1,YRB2,YRA1,YRA2,UV,UVR,              & 
& XV,XU,VSs,dYRB1,dYRB2,dYRA1,dYRA2,dUV,dUVR,dXV,dXU,dVSs,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: YRB1(3),YRB2(3),YRA1(3),YRA2(3),VSs(2,2),dYRB1(3),dYRB2(3),dYRA1(3),dYRA2(3),dVSs(2,2)

Complex(dp), Intent(in) :: UV(3,3),UVR(3,3),XV(2,2),XU(2,2),dUV(3,3),dUVR(3,3),dXV(2,2),dXU(2,2)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFvFxvcSsc' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
Do j1 = 1,3
resL = resL-(Conjg(UVR(gt1,j1))*Conjg(XV(gt2,2))*dYRB1(j1)*VSs(gt3,1))
End Do 
Do j1 = 1,3
resL = resL-(Conjg(UVR(gt1,j1))*Conjg(XV(gt2,2))*dYRB2(j1)*VSs(gt3,2))
End Do 
Do j1 = 1,3
resL = resL-(Conjg(UVR(gt1,j1))*Conjg(XV(gt2,2))*dVSs(gt3,1)*YRB1(j1))
End Do 
Do j1 = 1,3
resL = resL-(Conjg(dXV(gt2,2))*Conjg(UVR(gt1,j1))*VSs(gt3,1)*YRB1(j1))
End Do 
Do j1 = 1,3
resL = resL-(Conjg(dUVR(gt1,j1))*Conjg(XV(gt2,2))*VSs(gt3,1)*YRB1(j1))
End Do 
Do j1 = 1,3
resL = resL-(Conjg(UVR(gt1,j1))*Conjg(XV(gt2,2))*dVSs(gt3,2)*YRB2(j1))
End Do 
Do j1 = 1,3
resL = resL-(Conjg(dXV(gt2,2))*Conjg(UVR(gt1,j1))*VSs(gt3,2)*YRB2(j1))
End Do 
Do j1 = 1,3
resL = resL-(Conjg(dUVR(gt1,j1))*Conjg(XV(gt2,2))*VSs(gt3,2)*YRB2(j1))
End Do 
resR = 0._dp 
Do j1 = 1,3
resR = resR+dYRA1(j1)*VSs(gt3,1)*UV(gt1,j1)*XU(gt2,1)
End Do 
Do j1 = 1,3
resR = resR+dYRA2(j1)*VSs(gt3,2)*UV(gt1,j1)*XU(gt2,1)
End Do 
Do j1 = 1,3
resR = resR+dXU(gt2,1)*VSs(gt3,1)*UV(gt1,j1)*YRA1(j1)
End Do 
Do j1 = 1,3
resR = resR+dUV(gt1,j1)*VSs(gt3,1)*XU(gt2,1)*YRA1(j1)
End Do 
Do j1 = 1,3
resR = resR+dVSs(gt3,1)*UV(gt1,j1)*XU(gt2,1)*YRA1(j1)
End Do 
Do j1 = 1,3
resR = resR+dXU(gt2,1)*VSs(gt3,2)*UV(gt1,j1)*YRA2(j1)
End Do 
Do j1 = 1,3
resR = resR+dUV(gt1,j1)*VSs(gt3,2)*XU(gt2,1)*YRA2(j1)
End Do 
Do j1 = 1,3
resR = resR+dVSs(gt3,2)*UV(gt1,j1)*XU(gt2,1)*YRA2(j1)
End Do 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFvFxvcSsc  
 
 
Subroutine CT_CouplingcFxeFxvcHp(gt2,YRD,XU,dYRD,dXU,resL,resR)

Implicit None 

Integer, Intent(in) :: gt2
Real(dp), Intent(in) :: YRD,dYRD

Complex(dp), Intent(in) :: XU(2,2),dXU(2,2)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFxeFxvcHp' 
 
If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resR = 0._dp 
resR = resR+YRD*dXU(gt2,2)
resR = resR+dYRD*XU(gt2,2)
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFxeFxvcHp  
 
 
Subroutine CT_CouplingcFdFdVG(gt1,gt2,g3,dg3,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g3,dg3

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFdFdVG' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((gt1.eq.gt2)) Then 
resL = resL-1._dp*(dg3)
End If 
resR = 0._dp 
If ((gt1.eq.gt2)) Then 
resR = resR-1._dp*(dg3)
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFdFdVG  
 
 
Subroutine CT_CouplingcFdFdVP(gt1,gt2,g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,          & 
& resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFdFdVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((gt1.eq.gt2)) Then 
resL = resL-(dCosTW*g1)/6._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL+(dSinTW*g2)/2._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL-(dg1*Cos(TW))/6._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL+(dg2*Sin(TW))/2._dp
End If 
resR = 0._dp 
If ((gt1.eq.gt2)) Then 
resR = resR+(dCosTW*g1)/3._dp
End If 
If ((gt1.eq.gt2)) Then 
resR = resR+(dg1*Cos(TW))/3._dp
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFdFdVP  
 
 
Subroutine CT_CouplingcFuFdVWp(gt1,gt2,g2,ZDL,ZUL,dg2,dZDL,dZUL,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2,dg2

Complex(dp), Intent(in) :: ZDL(3,3),ZUL(3,3),dZDL(3,3),dZUL(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFuFdVWp' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
Do j1 = 1,3
resL = resL-((g2*Conjg(ZDL(gt2,j1))*dZUL(gt1,j1))/sqrt(2._dp))
End Do 
Do j1 = 1,3
resL = resL-((g2*Conjg(dZDL(gt2,j1))*ZUL(gt1,j1))/sqrt(2._dp))
End Do 
Do j1 = 1,3
resL = resL-((dg2*Conjg(ZDL(gt2,j1))*ZUL(gt1,j1))/sqrt(2._dp))
End Do 
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFuFdVWp  
 
 
Subroutine CT_CouplingcFdFdVZ(gt1,gt2,g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,          & 
& resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFdFdVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((gt1.eq.gt2)) Then 
resL = resL+(dSinTW*g1)/6._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL+(dCosTW*g2)/2._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL+(dg2*Cos(TW))/2._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL+(dg1*Sin(TW))/6._dp
End If 
resR = 0._dp 
If ((gt1.eq.gt2)) Then 
resR = resR-(dSinTW*g1)/3._dp
End If 
If ((gt1.eq.gt2)) Then 
resR = resR-(dg1*Sin(TW))/3._dp
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFdFdVZ  
 
 
Subroutine CT_CouplingcFeFeVP(gt1,gt2,g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,          & 
& resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFeFeVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((gt1.eq.gt2)) Then 
resL = resL+(dCosTW*g1)/2._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL+(dSinTW*g2)/2._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL+(dg1*Cos(TW))/2._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL+(dg2*Sin(TW))/2._dp
End If 
resR = 0._dp 
If ((gt1.eq.gt2)) Then 
resR = resR+dCosTW*g1
End If 
If ((gt1.eq.gt2)) Then 
resR = resR+dg1*Cos(TW)
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFeFeVP  
 
 
Subroutine CT_CouplingcFvFeVWp(gt1,gt2,g2,ZEL,UV,dg2,dZEL,dUV,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2,dg2

Complex(dp), Intent(in) :: ZEL(3,3),UV(3,3),dZEL(3,3),dUV(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFvFeVWp' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
Do j1 = 1,3
resL = resL-((g2*Conjg(ZEL(gt2,j1))*dUV(gt1,j1))/sqrt(2._dp))
End Do 
Do j1 = 1,3
resL = resL-((g2*Conjg(dZEL(gt2,j1))*UV(gt1,j1))/sqrt(2._dp))
End Do 
Do j1 = 1,3
resL = resL-((dg2*Conjg(ZEL(gt2,j1))*UV(gt1,j1))/sqrt(2._dp))
End Do 
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFvFeVWp  
 
 
Subroutine CT_CouplingcFeFeVZ(gt1,gt2,g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,          & 
& resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFeFeVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((gt1.eq.gt2)) Then 
resL = resL-(dSinTW*g1)/2._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL+(dCosTW*g2)/2._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL+(dg2*Cos(TW))/2._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL-(dg1*Sin(TW))/2._dp
End If 
resR = 0._dp 
If ((gt1.eq.gt2)) Then 
resR = resR-(dSinTW*g1)
End If 
If ((gt1.eq.gt2)) Then 
resR = resR-(dg1*Sin(TW))
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFeFeVZ  
 
 
Subroutine CT_CouplingcFuFuVG(gt1,gt2,g3,dg3,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g3,dg3

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFuFuVG' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((gt1.eq.gt2)) Then 
resL = resL-1._dp*(dg3)
End If 
resR = 0._dp 
If ((gt1.eq.gt2)) Then 
resR = resR-1._dp*(dg3)
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFuFuVG  
 
 
Subroutine CT_CouplingcFuFuVP(gt1,gt2,g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,          & 
& resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFuFuVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((gt1.eq.gt2)) Then 
resL = resL-(dCosTW*g1)/6._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL-(dSinTW*g2)/2._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL-(dg1*Cos(TW))/6._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL-(dg2*Sin(TW))/2._dp
End If 
resR = 0._dp 
If ((gt1.eq.gt2)) Then 
resR = resR+(-2*dCosTW*g1)/3._dp
End If 
If ((gt1.eq.gt2)) Then 
resR = resR+(-2*dg1*Cos(TW))/3._dp
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFuFuVP  
 
 
Subroutine CT_CouplingcFuFuVZ(gt1,gt2,g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,          & 
& resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFuFuVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((gt1.eq.gt2)) Then 
resL = resL+(dSinTW*g1)/6._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL-(dCosTW*g2)/2._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL-(dg2*Cos(TW))/2._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL+(dg1*Sin(TW))/6._dp
End If 
resR = 0._dp 
If ((gt1.eq.gt2)) Then 
resR = resR+(2*dSinTW*g1)/3._dp
End If 
If ((gt1.eq.gt2)) Then 
resR = resR+(2*dg1*Sin(TW))/3._dp
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFuFuVZ  
 
 
Subroutine CT_CouplingcFdFucVWp(gt1,gt2,g2,ZDL,ZUL,dg2,dZDL,dZUL,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2,dg2

Complex(dp), Intent(in) :: ZDL(3,3),ZUL(3,3),dZDL(3,3),dZUL(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFdFucVWp' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
Do j1 = 1,3
resL = resL-((g2*Conjg(ZUL(gt2,j1))*dZDL(gt1,j1))/sqrt(2._dp))
End Do 
Do j1 = 1,3
resL = resL-((g2*Conjg(dZUL(gt2,j1))*ZDL(gt1,j1))/sqrt(2._dp))
End Do 
Do j1 = 1,3
resL = resL-((dg2*Conjg(ZUL(gt2,j1))*ZDL(gt1,j1))/sqrt(2._dp))
End Do 
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFdFucVWp  
 
 
Subroutine CT_CouplingcFvFvVZ(gt1,gt2,g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,          & 
& resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFvFvVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((gt1.eq.gt2)) Then 
resL = resL-(dSinTW*g1)/2._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL-(dCosTW*g2)/2._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL-(dg2*Cos(TW))/2._dp
End If 
If ((gt1.eq.gt2)) Then 
resL = resL-(dg1*Sin(TW))/2._dp
End If 
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFvFvVZ  
 
 
Subroutine CT_CouplingcFeFvcVWp(gt1,gt2,g2,ZEL,UV,dg2,dZEL,dUV,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2,dg2

Complex(dp), Intent(in) :: ZEL(3,3),UV(3,3),dZEL(3,3),dUV(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFeFvcVWp' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
Do j1 = 1,3
resL = resL-((g2*Conjg(UV(gt2,j1))*dZEL(gt1,j1))/sqrt(2._dp))
End Do 
Do j1 = 1,3
resL = resL-((g2*Conjg(dUV(gt2,j1))*ZEL(gt1,j1))/sqrt(2._dp))
End Do 
Do j1 = 1,3
resL = resL-((dg2*Conjg(UV(gt2,j1))*ZEL(gt1,j1))/sqrt(2._dp))
End Do 
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFeFvcVWp  
 
 
Subroutine CT_CouplingcFxeFxeVP(g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,resL,resR)

Implicit None 

Real(dp), Intent(in) :: g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFxeFxeVP' 
 
resL = 0._dp 
resL = resL+(dCosTW*g1)/2._dp
resL = resL+(dSinTW*g2)/2._dp
resL = resL+(dg1*Cos(TW))/2._dp
resL = resL+(dg2*Sin(TW))/2._dp
resR = 0._dp 
resR = resR+(dCosTW*g1)/2._dp
resR = resR+(dSinTW*g2)/2._dp
resR = resR+(dg1*Cos(TW))/2._dp
resR = resR+(dg2*Sin(TW))/2._dp
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFxeFxeVP  
 
 
Subroutine CT_CouplingcFxvFxeVWp(gt1,g2,XV,XU,dg2,dXV,dXU,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g2,dg2

Complex(dp), Intent(in) :: XV(2,2),XU(2,2),dXV(2,2),dXU(2,2)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFxvFxeVWp' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((g2*dXV(gt1,1))/sqrt(2._dp))
resL = resL-((dg2*XV(gt1,1))/sqrt(2._dp))
resR = 0._dp 
resR = resR-((g2*Conjg(dXU(gt1,1)))/sqrt(2._dp))
resR = resR-((dg2*Conjg(XU(gt1,1)))/sqrt(2._dp))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFxvFxeVWp  
 
 
Subroutine CT_CouplingcFxeFxeVZ(g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW,resL,resR)

Implicit None 

Real(dp), Intent(in) :: g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFxeFxeVZ' 
 
resL = 0._dp 
resL = resL-(dSinTW*g1)/2._dp
resL = resL+(dCosTW*g2)/2._dp
resL = resL+(dg2*Cos(TW))/2._dp
resL = resL-(dg1*Sin(TW))/2._dp
resR = 0._dp 
resR = resR-(dSinTW*g1)/2._dp
resR = resR+(dCosTW*g2)/2._dp
resR = resR+(dg2*Cos(TW))/2._dp
resR = resR-(dg1*Sin(TW))/2._dp
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFxeFxeVZ  
 
 
Subroutine CT_CouplingcFxvFxvVZ(gt1,gt2,g1,g2,XV,XU,TW,dg1,dg2,dXV,dXU,               & 
& dSinTW,dCosTW,dTanTW,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,TW,dg1,dg2,dSinTW,dCosTW,dTanTW

Complex(dp), Intent(in) :: XV(2,2),XU(2,2),dXV(2,2),dXU(2,2)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFxvFxvVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(g2*Conjg(XV(gt2,1))*Cos(TW)*dXV(gt1,1))/2._dp
resL = resL-(g1*Conjg(XV(gt2,1))*dXV(gt1,1)*Sin(TW))/2._dp
resL = resL-(dSinTW*g1*Conjg(XV(gt2,1))*XV(gt1,1))/2._dp
resL = resL-(dCosTW*g2*Conjg(XV(gt2,1))*XV(gt1,1))/2._dp
resL = resL-(g2*Conjg(dXV(gt2,1))*Cos(TW)*XV(gt1,1))/2._dp
resL = resL-(dg2*Conjg(XV(gt2,1))*Cos(TW)*XV(gt1,1))/2._dp
resL = resL-(g1*Conjg(dXV(gt2,1))*Sin(TW)*XV(gt1,1))/2._dp
resL = resL-(dg1*Conjg(XV(gt2,1))*Sin(TW)*XV(gt1,1))/2._dp
resR = 0._dp 
resR = resR-(g2*Conjg(XU(gt1,1))*Cos(TW)*dXU(gt2,1))/2._dp
resR = resR-(g1*Conjg(XU(gt1,1))*dXU(gt2,1)*Sin(TW))/2._dp
resR = resR-(dSinTW*g1*Conjg(XU(gt1,1))*XU(gt2,1))/2._dp
resR = resR-(dCosTW*g2*Conjg(XU(gt1,1))*XU(gt2,1))/2._dp
resR = resR-(g2*Conjg(dXU(gt1,1))*Cos(TW)*XU(gt2,1))/2._dp
resR = resR-(dg2*Conjg(XU(gt1,1))*Cos(TW)*XU(gt2,1))/2._dp
resR = resR-(g1*Conjg(dXU(gt1,1))*Sin(TW)*XU(gt2,1))/2._dp
resR = resR-(dg1*Conjg(XU(gt1,1))*Sin(TW)*XU(gt2,1))/2._dp
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFxvFxvVZ  
 
 
Subroutine CT_CouplingcFxeFxvcVWp(gt2,g2,XV,XU,dg2,dXV,dXU,resL,resR)

Implicit None 

Integer, Intent(in) :: gt2
Real(dp), Intent(in) :: g2,dg2

Complex(dp), Intent(in) :: XV(2,2),XU(2,2),dXV(2,2),dXU(2,2)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CT_CouplingcFxeFxvcVWp' 
 
If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((g2*Conjg(dXV(gt2,1)))/sqrt(2._dp))
resL = resL-((dg2*Conjg(XV(gt2,1)))/sqrt(2._dp))
resR = 0._dp 
resR = resR-((g2*dXU(gt2,1))/sqrt(2._dp))
resR = resR-((dg2*XU(gt2,1))/sqrt(2._dp))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CT_CouplingcFxeFxvcVWp  
 
 
End Module CouplingsCT_SDdiracDM 
