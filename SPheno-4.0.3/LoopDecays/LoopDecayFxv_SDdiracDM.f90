! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:22 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module OneLoopDecay_Fxv_SDdiracDM
Use Control 
Use Settings 
Use LoopFunctions 
Use AddLoopFunctions 
Use Model_Data_SDdiracDM 
Use DecayFFS 
Use DecayFFV 
Use DecaySSS 
Use DecaySFF 
Use DecaySSV 
Use DecaySVV 
Use Bremsstrahlung 

Contains 

Subroutine Amplitude_Tree_SDdiracDM_FxvToFxvAh(cplcFxvFxvAhL,cplcFxvFxvAhR,           & 
& MAh,MFxv,MAh2,MFxv2,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFxv(2),MAh2,MFxv2(2)

Complex(dp), Intent(in) :: cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2)

Complex(dp) :: Amp(2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
  Do gt2=1,2
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = MAh 
! Tree-Level Vertex 
coupT1L = cplcFxvFxvAhL(gt1,gt2)
coupT1R = cplcFxvFxvAhR(gt1,gt2)
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2) = AmpC 
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_FxvToFxvAh


Subroutine Gamma_Real_SDdiracDM_FxvToFxvAh(MLambda,em,gs,cplcFxvFxvAhL,               & 
& cplcFxvFxvAhR,MAh,MFxv,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2)

Real(dp), Intent(in) :: MAh,MFxv(2)

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2,2), GammarealGluon(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,2
  Do i2=1,2
CoupL = cplcFxvFxvAhL(i1,i2)
CoupR = cplcFxvFxvAhR(i1,i2)
Mex1 = MFxv(i1)
Mex2 = MFxv(i2)
Mex3 = MAh
If (Mex1.gt.(Mex2+Mex3)) Then 
 Gammarealphoton(i1,i2) = 0._dp 
  GammarealGluon(i1,i2) = 0._dp 
Else 
  GammarealGluon(i1,i2) = 0._dp 
  GammarealPhoton(i1,i2) = 0._dp 

End if 
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_FxvToFxvAh


Subroutine Amplitude_WAVE_SDdiracDM_FxvToFxvAh(cplcFxvFxvAhL,cplcFxvFxvAhR,           & 
& ctcplcFxvFxvAhL,ctcplcFxvFxvAhR,MAh,MAh2,MFxv,MFxv2,ZfAh,ZfxVL,ZfxVR,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MAh2,MFxv(2),MFxv2(2)

Complex(dp), Intent(in) :: cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2)

Complex(dp), Intent(in) :: ctcplcFxvFxvAhL(2,2),ctcplcFxvFxvAhR(2,2)

Complex(dp), Intent(in) :: ZfAh,ZfxVL(2,2),ZfxVR(2,2)

Complex(dp), Intent(out) :: Amp(2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
  Do gt2=1,2
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = MAh 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFxvFxvAhL(gt1,gt2) 
ZcoupT1R = ctcplcFxvFxvAhR(gt1,gt2) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfxVR(i1,gt1)*cplcFxvFxvAhL(i1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfxVL(i1,gt1))*cplcFxvFxvAhR(i1,gt2)
End Do


! External Field 2 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfxVL(i1,gt2)*cplcFxvFxvAhL(gt1,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfxVR(i1,gt2))*cplcFxvFxvAhR(gt1,i1)
End Do


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfAh*cplcFxvFxvAhL(gt1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfAh*cplcFxvFxvAhR(gt1,gt2)


! Getting the amplitude 
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt1, gt2) = AmpC 
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FxvToFxvAh


Subroutine Amplitude_VERTEX_SDdiracDM_FxvToFxvAh(MAh,MFxe,MFxv,Mhh,MHp,               & 
& MVWp,MVZ,MAh2,MFxe2,MFxv2,Mhh2,MHp2,MVWp2,MVZ2,cplAhAhhh,cplcFxvFxvAhL,cplcFxvFxvAhR,  & 
& cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,          & 
& cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFxeFxvcHpL, & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFxe,MFxv(2),Mhh(2),MHp,MVWp,MVZ,MAh2,MFxe2,MFxv2(2),Mhh2(2),MHp2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplAhhhVZ(2),cplAhHpcVWp,          & 
& cplAhcHpVWp,cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),     & 
& cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),       & 
& cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2)

Complex(dp), Intent(out) :: Amp(2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,2
Amp(:,gt1, gt2) = 0._dp 
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = MAh 


! {Ah, bar[Fxv], bar[Fxv]}
If ((Include_in_loopAh).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
  Do i2=1,2
    Do i3=1,2
ML1 = MAh 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxvFxvAhL(gt1,i2)
coup1R = cplcFxvFxvAhR(gt1,i2)
coup2L = cplcFxvFxvAhL(i3,gt2)
coup2R = cplcFxvFxvAhR(i3,gt2)
coup3L = cplcFxvFxvAhL(i2,i3)
coup3R = cplcFxvFxvAhR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 


! {Fxe, conj[VWp], conj[Hp]}
If ((Include_in_loopFxe).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MFxe 
ML2 = MVWp 
ML3 = MHp 
coup1L = cplcFxvFxeVWpL(gt1)
coup1R = cplcFxvFxeVWpR(gt1)
coup2L = cplcFxeFxvcHpL(gt2)
coup2R = cplcFxeFxvcHpR(gt2)
coup3 = -cplAhHpcVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[Hp], conj[VWp]}
If ((Include_in_loopFxe).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MFxe 
ML2 = MHp 
ML3 = MVWp 
coup1L = cplcFxvFxeHpL(gt1)
coup1R = cplcFxvFxeHpR(gt1)
coup2L = cplcFxeFxvcVWpL(gt2)
coup2R = cplcFxeFxvcVWpR(gt2)
coup3 = -cplAhcHpVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxv, hh, Ah}
If ((Include_in_loopFxv).and.(Include_in_loophh).and.(Include_in_loopAh)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MFxv(i1) 
ML2 = Mhh(i2) 
ML3 = MAh 
coup1L = cplcFxvFxvhhL(gt1,i1,i2)
coup1R = cplcFxvFxvhhR(gt1,i1,i2)
coup2L = cplcFxvFxvAhL(i1,gt2)
coup2R = cplcFxvFxvAhR(i1,gt2)
coup3 = cplAhAhhh(i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {Fxv, Ah, hh}
If ((Include_in_loopFxv).and.(Include_in_loopAh).and.(Include_in_loophh)) Then 
Do i1=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = MAh 
ML3 = Mhh(i3) 
coup1L = cplcFxvFxvAhL(gt1,i1)
coup1R = cplcFxvFxvAhR(gt1,i1)
coup2L = cplcFxvFxvhhL(i1,gt2,i3)
coup2R = cplcFxvFxvhhR(i1,gt2,i3)
coup3 = cplAhAhhh(i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
End Do
End if 


! {Fxv, VZ, hh}
If ((Include_in_loopFxv).and.(Include_in_loopVZ).and.(Include_in_loophh)) Then 
Do i1=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = MVZ 
ML3 = Mhh(i3) 
coup1L = -cplcFxvFxvVZR(gt1,i1)
coup1R = -cplcFxvFxvVZL(gt1,i1)
coup2L = cplcFxvFxvhhL(i1,gt2,i3)
coup2R = cplcFxvFxvhhR(i1,gt2,i3)
coup3 = -cplAhhhVZ(i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
End Do
End if 


! {Fxv, hh, VZ}
If ((Include_in_loopFxv).and.(Include_in_loophh).and.(Include_in_loopVZ)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MFxv(i1) 
ML2 = Mhh(i2) 
ML3 = MVZ 
coup1L = cplcFxvFxvhhL(gt1,i1,i2)
coup1R = cplcFxvFxvhhR(gt1,i1,i2)
coup2L = -cplcFxvFxvVZR(i1,gt2)
coup2R = -cplcFxvFxvVZL(i1,gt2)
coup3 = -cplAhhhVZ(i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {hh, bar[Fxv], bar[Fxv]}
If ((Include_in_loophh).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = Mhh(i1) 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxvFxvhhL(gt1,i2,i1)
coup1R = cplcFxvFxvhhR(gt1,i2,i1)
coup2L = cplcFxvFxvhhL(i3,gt2,i1)
coup2R = cplcFxvFxvhhR(i3,gt2,i1)
coup3L = cplcFxvFxvAhL(i2,i3)
coup3R = cplcFxvFxvAhR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {VZ, bar[Fxv], bar[Fxv]}
If ((Include_in_loopVZ).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
  Do i2=1,2
    Do i3=1,2
ML1 = MVZ 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = -cplcFxvFxvVZR(gt1,i2)
coup1R = -cplcFxvFxvVZL(gt1,i2)
coup2L = -cplcFxvFxvVZR(i3,gt2)
coup2R = -cplcFxvFxvVZL(i3,gt2)
coup3L = cplcFxvFxvAhL(i2,i3)
coup3R = cplcFxvFxvAhR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_FxvToFxvAh


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvAh(MAh,MFxe,MFxv,Mhh,MHp,            & 
& MVWp,MVZ,MAh2,MFxe2,MFxv2,Mhh2,MHp2,MVWp2,MVZ2,cplAhAhhh,cplcFxvFxvAhL,cplcFxvFxvAhR,  & 
& cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,          & 
& cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFxeFxvcHpL, & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFxe,MFxv(2),Mhh(2),MHp,MVWp,MVZ,MAh2,MFxe2,MFxv2(2),Mhh2(2),MHp2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplAhhhVZ(2),cplAhHpcVWp,          & 
& cplAhcHpVWp,cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),     & 
& cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),       & 
& cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2)

Complex(dp), Intent(out) :: Amp(2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,2
Amp(:,gt1, gt2) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = MAh 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvAh


Subroutine Amplitude_Tree_SDdiracDM_FxvToFvSsc(cplcFxvFvSscL,cplcFxvFvSscR,           & 
& MFv,MFxv,MSsc,MFv2,MFxv2,MSsc2,Amp)

Implicit None

Real(dp), Intent(in) :: MFv(3),MFxv(2),MSsc(2),MFv2(3),MFxv2(2),MSsc2(2)

Complex(dp), Intent(in) :: cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2)

Complex(dp) :: Amp(2,2,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,2
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MSsc(gt3) 
! Tree-Level Vertex 
coupT1L = cplcFxvFvSscL(gt1,gt2,gt3)
coupT1R = cplcFxvFvSscR(gt1,gt2,gt3)
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_FxvToFvSsc


Subroutine Gamma_Real_SDdiracDM_FxvToFvSsc(MLambda,em,gs,cplcFxvFvSscL,               & 
& cplcFxvFvSscR,MFv,MFxv,MSsc,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2)

Real(dp), Intent(in) :: MFv(3),MFxv(2),MSsc(2)

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2,3,2), GammarealGluon(2,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,2
  Do i2=1,3
    Do i3=1,2
CoupL = cplcFxvFvSscL(i1,i2,i3)
CoupR = cplcFxvFvSscR(i1,i2,i3)
Mex1 = MFxv(i1)
Mex2 = MFv(i2)
Mex3 = MSsc(i3)
If (Mex1.gt.(Mex2+Mex3)) Then 
 Gammarealphoton(i1,i2,i3) = 0._dp 
  GammarealGluon(i1,i2,i3) = 0._dp 
Else 
  GammarealGluon(i1,i2,i3) = 0._dp 
  GammarealPhoton(i1,i2,i3) = 0._dp 

End if 
    End Do
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_FxvToFvSsc


Subroutine Amplitude_WAVE_SDdiracDM_FxvToFvSsc(cplcFxvFvSscL,cplcFxvFvSscR,           & 
& ctcplcFxvFvSscL,ctcplcFxvFvSscR,MFv,MFv2,MFxv,MFxv2,MSsc,MSsc2,ZfSsc,ZfVL,             & 
& ZfVR,ZfxVL,ZfxVR,Amp)

Implicit None

Real(dp), Intent(in) :: MFv(3),MFv2(3),MFxv(2),MFxv2(2),MSsc(2),MSsc2(2)

Complex(dp), Intent(in) :: cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2)

Complex(dp), Intent(in) :: ctcplcFxvFvSscL(2,3,2),ctcplcFxvFvSscR(2,3,2)

Complex(dp), Intent(in) :: ZfSsc(2,2),ZfVL(3,3),ZfVR(3,3),ZfxVL(2,2),ZfxVR(2,2)

Complex(dp), Intent(out) :: Amp(2,2,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,2
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MSsc(gt3) 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFxvFvSscL(gt1,gt2,gt3) 
ZcoupT1R = ctcplcFxvFvSscR(gt1,gt2,gt3) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfxVR(i1,gt1)*cplcFxvFvSscL(i1,gt2,gt3)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfxVL(i1,gt1))*cplcFxvFvSscR(i1,gt2,gt3)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVL(i1,gt2)*cplcFxvFvSscL(gt1,i1,gt3)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfVR(i1,gt2))*cplcFxvFvSscR(gt1,i1,gt3)
End Do


! External Field 3 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfSsc(i1,gt3)*cplcFxvFvSscL(gt1,gt2,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfSsc(i1,gt3)*cplcFxvFvSscR(gt1,gt2,i1)
End Do


! Getting the amplitude 
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FxvToFvSsc


Subroutine Amplitude_VERTEX_SDdiracDM_FxvToFvSsc(MFe,MFv,MFxe,MFxv,Mhh,               & 
& MHp,MSsc,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFxeFeSscL,      & 
& cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,        & 
& cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,   & 
& cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhSsccSsc,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVWp,MVZ,MFe2(3),MFv2(3),               & 
& MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),      & 
& cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),& 
& cplcFeFvcVWpR(3,3),cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),& 
& cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),       & 
& cplhhSsccSsc(2,2,2)

Complex(dp), Intent(out) :: Amp(2,2,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,2
Amp(:,gt1, gt2, gt3) = 0._dp 
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MSsc(gt3) 


! {Fxv, hh, Ssc}
If ((Include_in_loopFxv).and.(Include_in_loophh).and.(Include_in_loopSsc)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = Mhh(i2) 
ML3 = MSsc(i3) 
coup1L = cplcFxvFxvhhL(gt1,i1,i2)
coup1R = cplcFxvFxvhhR(gt1,i1,i2)
coup2L = cplcFxvFvSscL(i1,gt2,i3)
coup2R = cplcFxvFvSscR(i1,gt2,i3)
coup3 = cplhhSsccSsc(i2,gt3,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {Hp, bar[Fxe], bar[Fe]}
If ((Include_in_loopHp).and.(Include_in_loopFxe).and.(Include_in_loopFe)) Then 
    Do i3=1,3
ML1 = MHp 
ML2 = MFxe 
ML3 = MFe(i3) 
coup1L = cplcFxvFxeHpL(gt1)
coup1R = cplcFxvFxeHpR(gt1)
coup2L = cplcFeFvcHpL(i3,gt2)
coup2R = cplcFeFvcHpR(i3,gt2)
coup3L = cplcFxeFeSscL(i3,gt3)
coup3R = cplcFxeFeSscR(i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VWp, bar[Fxe], bar[Fe]}
If ((Include_in_loopVWp).and.(Include_in_loopFxe).and.(Include_in_loopFe)) Then 
    Do i3=1,3
ML1 = MVWp 
ML2 = MFxe 
ML3 = MFe(i3) 
coup1L = cplcFxvFxeVWpL(gt1)
coup1R = cplcFxvFxeVWpR(gt1)
coup2L = -cplcFeFvcVWpR(i3,gt2)
coup2R = -cplcFeFvcVWpL(i3,gt2)
coup3L = cplcFxeFeSscL(i3,gt3)
coup3R = cplcFxeFeSscR(i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VZ, bar[Fxv], bar[Fv]}
If ((Include_in_loopVZ).and.(Include_in_loopFxv).and.(Include_in_loopFv)) Then 
  Do i2=1,2
    Do i3=1,3
ML1 = MVZ 
ML2 = MFxv(i2) 
ML3 = MFv(i3) 
coup1L = -cplcFxvFxvVZR(gt1,i2)
coup1R = -cplcFxvFxvVZL(gt1,i2)
coup2L = -cplcFvFvVZR(i3,gt2)
coup2R = -cplcFvFvVZL(i3,gt2)
coup3L = cplcFxvFvSscL(i2,i3,gt3)
coup3R = cplcFxvFvSscR(i2,i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
    End Do
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_FxvToFvSsc


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxvToFvSsc(MFe,MFv,MFxe,MFxv,Mhh,            & 
& MHp,MSsc,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFxeFeSscL,      & 
& cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,        & 
& cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,   & 
& cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhSsccSsc,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVWp,MVZ,MFe2(3),MFv2(3),               & 
& MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),      & 
& cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),& 
& cplcFeFvcVWpR(3,3),cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),& 
& cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),       & 
& cplhhSsccSsc(2,2,2)

Complex(dp), Intent(out) :: Amp(2,2,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,2
Amp(:,gt1, gt2, gt3) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MSsc(gt3) 
    End Do
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxvToFvSsc


Subroutine Amplitude_Tree_SDdiracDM_FxvToFxeHp(cplcFxvFxeHpL,cplcFxvFxeHpR,           & 
& MFxe,MFxv,MHp,MFxe2,MFxv2,MHp2,Amp)

Implicit None

Real(dp), Intent(in) :: MFxe,MFxv(2),MHp,MFxe2,MFxv2(2),MHp2

Complex(dp), Intent(in) :: cplcFxvFxeHpL(2),cplcFxvFxeHpR(2)

Complex(dp) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxe 
Mex3 = MHp 
! Tree-Level Vertex 
coupT1L = cplcFxvFxeHpL(gt1)
coupT1R = cplcFxvFxeHpR(gt1)
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1) = AmpC 
End Do
End Subroutine Amplitude_Tree_SDdiracDM_FxvToFxeHp


Subroutine Gamma_Real_SDdiracDM_FxvToFxeHp(MLambda,em,gs,cplcFxvFxeHpL,               & 
& cplcFxvFxeHpR,MFxe,MFxv,MHp,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFxvFxeHpL(2),cplcFxvFxeHpR(2)

Real(dp), Intent(in) :: MFxe,MFxv(2),MHp

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2), GammarealGluon(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,2
CoupL = cplcFxvFxeHpL(i1)
CoupR = cplcFxvFxeHpR(i1)
Mex1 = MFxv(i1)
Mex2 = MFxe
Mex3 = MHp
If (Mex1.gt.(Mex2+Mex3)) Then 
 Call hardradiationFFS(Mex1,Mex2,Mex3,MLambda,em,0._dp,0._dp,0._dp,1._dp,-1._dp,1._dp,CoupL,CoupR,Gammarealphoton(i1),kont)
  GammarealGluon(i1) = 0._dp 
Else 
  GammarealGluon(i1) = 0._dp 
  GammarealPhoton(i1) = 0._dp 

End if 
End Do
End Subroutine Gamma_Real_SDdiracDM_FxvToFxeHp


Subroutine Amplitude_WAVE_SDdiracDM_FxvToFxeHp(cplcFxvFxeHpL,cplcFxvFxeHpR,           & 
& ctcplcFxvFxeHpL,ctcplcFxvFxeHpR,MFxe,MFxe2,MFxv,MFxv2,MHp,MHp2,Zfed,Zfeu,              & 
& ZfHp,ZfxVL,ZfxVR,Amp)

Implicit None

Real(dp), Intent(in) :: MFxe,MFxe2,MFxv(2),MFxv2(2),MHp,MHp2

Complex(dp), Intent(in) :: cplcFxvFxeHpL(2),cplcFxvFxeHpR(2)

Complex(dp), Intent(in) :: ctcplcFxvFxeHpL(2),ctcplcFxvFxeHpR(2)

Complex(dp), Intent(in) :: Zfed,Zfeu,ZfHp,ZfxVL(2,2),ZfxVR(2,2)

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxe 
Mex3 = MHp 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFxvFxeHpL(gt1) 
ZcoupT1R = ctcplcFxvFxeHpR(gt1) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfxVR(i1,gt1)*cplcFxvFxeHpL(i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfxVL(i1,gt1))*cplcFxvFxeHpR(i1)
End Do


! External Field 2 
ZcoupT1L = ZcoupT1L + 0.5_dp*Zfed*cplcFxvFxeHpL(gt1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(Zfeu)*cplcFxvFxeHpR(gt1)


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfHp*cplcFxvFxeHpL(gt1)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfHp*cplcFxvFxeHpR(gt1)


! Getting the amplitude 
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt1) = AmpC 
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FxvToFxeHp


Subroutine Amplitude_VERTEX_SDdiracDM_FxvToFxeHp(MAh,MFe,MFv,MFxe,MFxv,               & 
& Mhh,MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,             & 
& MVWp2,MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhHpcVWp,cplcFvFeHpL,cplcFvFeHpR,            & 
& cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,   & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,              & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhHpcHp,     & 
& cplhhHpcVWp,cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,cplHpcVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),          & 
& MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplAhHpcVWp,cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),  & 
& cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),           & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxeFxeVZL,         & 
& cplcFxeFxeVZR,cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFxvFxvhhL(2,2,2),            & 
& cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),cplhhHpcHp(2),              & 
& cplhhHpcVWp(2),cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,cplHpcVWpVZ

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
Amp(:,gt1) = 0._dp 
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxe 
Mex3 = MHp 


! {Fxe, conj[Hp], VP}
If ((Include_in_loopFxe).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MFxe 
ML2 = MHp 
ML3 = MVP 
coup1L = cplcFxvFxeHpL(gt1)
coup1R = cplcFxvFxeHpR(gt1)
coup2L = cplcFxeFxeVPL
coup2R = cplcFxeFxeVPR
coup3 = -cplHpcHpVP
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[VWp], VP}
If ((Include_in_loopFxe).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
ML1 = MFxe 
ML2 = MVWp 
ML3 = MVP 
coup1L = cplcFxvFxeVWpL(gt1)
coup1R = cplcFxvFxeVWpR(gt1)
coup2L = cplcFxeFxeVPL
coup2R = cplcFxeFxeVPR
coup3 = cplHpcVWpVP
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[Hp], VZ}
If ((Include_in_loopFxe).and.(Include_in_loopHp).and.(Include_in_loopVZ)) Then 
ML1 = MFxe 
ML2 = MHp 
ML3 = MVZ 
coup1L = cplcFxvFxeHpL(gt1)
coup1R = cplcFxvFxeHpR(gt1)
coup2L = cplcFxeFxeVZL
coup2R = cplcFxeFxeVZR
coup3 = -cplHpcHpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[VWp], VZ}
If ((Include_in_loopFxe).and.(Include_in_loopVWp).and.(Include_in_loopVZ)) Then 
ML1 = MFxe 
ML2 = MVWp 
ML3 = MVZ 
coup1L = cplcFxvFxeVWpL(gt1)
coup1R = cplcFxvFxeVWpR(gt1)
coup2L = cplcFxeFxeVZL
coup2R = cplcFxeFxeVZR
coup3 = cplHpcVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Fxv, hh, Hp}
If ((Include_in_loopFxv).and.(Include_in_loophh).and.(Include_in_loopHp)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MFxv(i1) 
ML2 = Mhh(i2) 
ML3 = MHp 
coup1L = cplcFxvFxvhhL(gt1,i1,i2)
coup1R = cplcFxvFxvhhR(gt1,i1,i2)
coup2L = cplcFxvFxeHpL(i1)
coup2R = cplcFxvFxeHpR(i1)
coup3 = cplhhHpcHp(i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {Fxv, VZ, Hp}
If ((Include_in_loopFxv).and.(Include_in_loopVZ).and.(Include_in_loopHp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MVZ 
ML3 = MHp 
coup1L = -cplcFxvFxvVZR(gt1,i1)
coup1R = -cplcFxvFxvVZL(gt1,i1)
coup2L = cplcFxvFxeHpL(i1)
coup2R = cplcFxvFxeHpR(i1)
coup3 = -cplHpcHpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, Ah, VWp}
If ((Include_in_loopFxv).and.(Include_in_loopAh).and.(Include_in_loopVWp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MAh 
ML3 = MVWp 
coup1L = cplcFxvFxvAhL(gt1,i1)
coup1R = cplcFxvFxvAhR(gt1,i1)
coup2L = cplcFxvFxeVWpL(i1)
coup2R = cplcFxvFxeVWpR(i1)
coup3 = cplAhHpcVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, hh, VWp}
If ((Include_in_loopFxv).and.(Include_in_loophh).and.(Include_in_loopVWp)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MFxv(i1) 
ML2 = Mhh(i2) 
ML3 = MVWp 
coup1L = cplcFxvFxvhhL(gt1,i1,i2)
coup1R = cplcFxvFxvhhR(gt1,i1,i2)
coup2L = cplcFxvFxeVWpL(i1)
coup2R = cplcFxvFxeVWpR(i1)
coup3 = cplhhHpcVWp(i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {Fxv, VZ, VWp}
If ((Include_in_loopFxv).and.(Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MVZ 
ML3 = MVWp 
coup1L = -cplcFxvFxvVZR(gt1,i1)
coup1R = -cplcFxvFxvVZL(gt1,i1)
coup2L = cplcFxvFxeVWpL(i1)
coup2R = cplcFxvFxeVWpR(i1)
coup3 = cplHpcVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Ssc, bar[Fv], bar[Fe]}
If ((Include_in_loopSsc).and.(Include_in_loopFv).and.(Include_in_loopFe)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = MSsc(i1) 
ML2 = MFv(i2) 
ML3 = MFe(i3) 
coup1L = cplcFxvFvSscL(gt1,i2,i1)
coup1R = cplcFxvFvSscR(gt1,i2,i1)
coup2L = cplcFeFxecSscL(i3,i1)
coup2R = cplcFeFxecSscR(i3,i1)
coup3L = cplcFvFeHpL(i2,i3)
coup3R = cplcFvFeHpR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {VZ, bar[Fxv], bar[Fxe]}
If ((Include_in_loopVZ).and.(Include_in_loopFxv).and.(Include_in_loopFxe)) Then 
  Do i2=1,2
ML1 = MVZ 
ML2 = MFxv(i2) 
ML3 = MFxe 
coup1L = -cplcFxvFxvVZR(gt1,i2)
coup1R = -cplcFxvFxvVZL(gt1,i2)
coup2L = cplcFxeFxeVZL
coup2R = cplcFxeFxeVZR
coup3L = cplcFxvFxeHpL(i2)
coup3R = cplcFxvFxeHpR(i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End if 
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_FxvToFxeHp


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxvToFxeHp(MAh,MFe,MFv,MFxe,MFxv,            & 
& Mhh,MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,             & 
& MVWp2,MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhHpcVWp,cplcFvFeHpL,cplcFvFeHpR,            & 
& cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,   & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,              & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhHpcHp,     & 
& cplhhHpcVWp,cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,cplHpcVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),          & 
& MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplAhHpcVWp,cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),  & 
& cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),           & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxeFxeVZL,         & 
& cplcFxeFxeVZR,cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFxvFxvhhL(2,2,2),            & 
& cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),cplhhHpcHp(2),              & 
& cplhhHpcVWp(2),cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,cplHpcVWpVZ

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
Amp(:,gt1) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxe 
Mex3 = MHp 


! {Fxe, conj[Hp], VP}
If ((Include_in_loopFxe).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MFxe 
ML2 = MHp 
ML3 = MVP 
coup1L = cplcFxvFxeHpL(gt1)
coup1R = cplcFxvFxeHpR(gt1)
coup2L = cplcFxeFxeVPL
coup2R = cplcFxeFxeVPR
coup3 = -cplHpcHpVP
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[VWp], VP}
If ((Include_in_loopFxe).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
ML1 = MFxe 
ML2 = MVWp 
ML3 = MVP 
coup1L = cplcFxvFxeVWpL(gt1)
coup1R = cplcFxvFxeVWpR(gt1)
coup2L = cplcFxeFxeVPL
coup2R = cplcFxeFxeVPR
coup3 = cplHpcVWpVP
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxvToFxeHp


Subroutine Amplitude_Tree_SDdiracDM_FxvToFxeVWp(cplcFxvFxeVWpL,cplcFxvFxeVWpR,        & 
& MFxe,MFxv,MVWp,MFxe2,MFxv2,MVWp2,Amp)

Implicit None

Real(dp), Intent(in) :: MFxe,MFxv(2),MVWp,MFxe2,MFxv2(2),MVWp2

Complex(dp), Intent(in) :: cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2)

Complex(dp) :: Amp(4,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxe 
Mex3 = MVWp 
! Tree-Level Vertex 
coupT1L = cplcFxvFxeVWpL(gt1)
coupT1R = cplcFxvFxeVWpR(gt1)
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,coupT1R,coupT1L,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1) = -AmpC 
End Do
End Subroutine Amplitude_Tree_SDdiracDM_FxvToFxeVWp


Subroutine Gamma_Real_SDdiracDM_FxvToFxeVWp(MLambda,em,gs,cplcFxvFxeVWpL,             & 
& cplcFxvFxeVWpR,MFxe,MFxv,MVWp,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2)

Real(dp), Intent(in) :: MFxe,MFxv(2),MVWp

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2), GammarealGluon(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,2
CoupL = cplcFxvFxeVWpL(i1)
CoupR = cplcFxvFxeVWpR(i1)
Mex1 = MFxv(i1)
Mex2 = MFxe
Mex3 = MVWp
If (Mex1.gt.(Mex2+Mex3)) Then 
  Call hardphotonFFW(Mex1,Mex2,Mex3,MLambda,0._dp,-1._dp,CoupL,CoupR,(0,1)*em,GammaRealPhoton(i1),kont)
  GammarealGluon(i1) = 0._dp 
Else 
  GammarealGluon(i1) = 0._dp 
  GammarealPhoton(i1) = 0._dp 

End if 
End Do
End Subroutine Gamma_Real_SDdiracDM_FxvToFxeVWp


Subroutine Amplitude_WAVE_SDdiracDM_FxvToFxeVWp(cplcFxvFxeVWpL,cplcFxvFxeVWpR,        & 
& ctcplcFxvFxeVWpL,ctcplcFxvFxeVWpR,MFxe,MFxe2,MFxv,MFxv2,MVWp,MVWp2,Zfed,               & 
& Zfeu,ZfVWp,ZfxVL,ZfxVR,Amp)

Implicit None

Real(dp), Intent(in) :: MFxe,MFxe2,MFxv(2),MFxv2(2),MVWp,MVWp2

Complex(dp), Intent(in) :: cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2)

Complex(dp), Intent(in) :: ctcplcFxvFxeVWpL(2),ctcplcFxvFxeVWpR(2)

Complex(dp), Intent(in) :: Zfed,Zfeu,ZfVWp,ZfxVL(2,2),ZfxVR(2,2)

Complex(dp), Intent(out) :: Amp(4,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxe 
Mex3 = MVWp 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFxvFxeVWpL(gt1) 
ZcoupT1R = ctcplcFxvFxeVWpR(gt1)
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*Conjg(ZfxVL(i1,gt1))*cplcFxvFxeVWpL(i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfxVR(i1,gt1)*cplcFxvFxeVWpR(i1)
End Do


! External Field 2 
ZcoupT1L = ZcoupT1L + 0.5_dp*Zfed*cplcFxvFxeVWpL(gt1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(Zfeu)*cplcFxvFxeVWpR(gt1)


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVWp*cplcFxvFxeVWpL(gt1)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfVWp*cplcFxvFxeVWpR(gt1)


! Getting the amplitude 
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,ZcoupT1R,ZcoupT1L,AmpC) 
Amp(:,gt1) = -AmpC 
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FxvToFxeVWp


Subroutine Amplitude_VERTEX_SDdiracDM_FxvToFxeVWp(MAh,MFe,MFv,MFxe,MFxv,              & 
& Mhh,MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,             & 
& MVWp2,MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhcHpVWp,cplcFvFeVWpL,cplcFvFeVWpR,          & 
& cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,   & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,              & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhcHpVWp,    & 
& cplhhcVWpVWp,cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),          & 
& MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplAhcHpVWp,cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),& 
& cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),           & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxeFxeVZL,         & 
& cplcFxeFxeVZR,cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFxvFxvhhL(2,2,2),            & 
& cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),cplhhcHpVWp(2),             & 
& cplhhcVWpVWp(2),cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ

Complex(dp), Intent(out) :: Amp(4,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
Amp(:,gt1) = 0._dp 
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxe 
Mex3 = MVWp 


! {Fxe, conj[Hp], VP}
If ((Include_in_loopFxe).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MFxe 
ML2 = MHp 
ML3 = MVP 
coup1L = cplcFxvFxeHpL(gt1)
coup1R = cplcFxvFxeHpR(gt1)
coup2L = cplcFxeFxeVPL
coup2R = cplcFxeFxeVPR
coup3 = cplcHpVPVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[VWp], VP}
If ((Include_in_loopFxe).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
ML1 = MFxe 
ML2 = MVWp 
ML3 = MVP 
coup1L = cplcFxvFxeVWpL(gt1)
coup1R = cplcFxvFxeVWpR(gt1)
coup2L = cplcFxeFxeVPL
coup2R = cplcFxeFxeVPR
coup3 = cplcVWpVPVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[Hp], VZ}
If ((Include_in_loopFxe).and.(Include_in_loopHp).and.(Include_in_loopVZ)) Then 
ML1 = MFxe 
ML2 = MHp 
ML3 = MVZ 
coup1L = cplcFxvFxeHpL(gt1)
coup1R = cplcFxvFxeHpR(gt1)
coup2L = cplcFxeFxeVZL
coup2R = cplcFxeFxeVZR
coup3 = cplcHpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[VWp], VZ}
If ((Include_in_loopFxe).and.(Include_in_loopVWp).and.(Include_in_loopVZ)) Then 
ML1 = MFxe 
ML2 = MVWp 
ML3 = MVZ 
coup1L = cplcFxvFxeVWpL(gt1)
coup1R = cplcFxvFxeVWpR(gt1)
coup2L = cplcFxeFxeVZL
coup2R = cplcFxeFxeVZR
coup3 = -cplcVWpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Fxv, Ah, Hp}
If ((Include_in_loopFxv).and.(Include_in_loopAh).and.(Include_in_loopHp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MAh 
ML3 = MHp 
coup1L = cplcFxvFxvAhL(gt1,i1)
coup1R = cplcFxvFxvAhR(gt1,i1)
coup2L = cplcFxvFxeHpL(i1)
coup2R = cplcFxvFxeHpR(i1)
coup3 = -cplAhcHpVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, hh, Hp}
If ((Include_in_loopFxv).and.(Include_in_loophh).and.(Include_in_loopHp)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MFxv(i1) 
ML2 = Mhh(i2) 
ML3 = MHp 
coup1L = cplcFxvFxvhhL(gt1,i1,i2)
coup1R = cplcFxvFxvhhR(gt1,i1,i2)
coup2L = cplcFxvFxeHpL(i1)
coup2R = cplcFxvFxeHpR(i1)
coup3 = -cplhhcHpVWp(i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {Fxv, VZ, Hp}
If ((Include_in_loopFxv).and.(Include_in_loopVZ).and.(Include_in_loopHp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MVZ 
ML3 = MHp 
coup1L = cplcFxvFxvVZL(gt1,i1)
coup1R = cplcFxvFxvVZR(gt1,i1)
coup2L = cplcFxvFxeHpL(i1)
coup2R = cplcFxvFxeHpR(i1)
coup3 = cplcHpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, hh, VWp}
If ((Include_in_loopFxv).and.(Include_in_loophh).and.(Include_in_loopVWp)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MFxv(i1) 
ML2 = Mhh(i2) 
ML3 = MVWp 
coup1L = cplcFxvFxvhhL(gt1,i1,i2)
coup1R = cplcFxvFxvhhR(gt1,i1,i2)
coup2L = cplcFxvFxeVWpL(i1)
coup2R = cplcFxvFxeVWpR(i1)
coup3 = cplhhcVWpVWp(i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {Fxv, VZ, VWp}
If ((Include_in_loopFxv).and.(Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MVZ 
ML3 = MVWp 
coup1L = cplcFxvFxvVZL(gt1,i1)
coup1R = cplcFxvFxvVZR(gt1,i1)
coup2L = cplcFxvFxeVWpL(i1)
coup2R = cplcFxvFxeVWpR(i1)
coup3 = cplcVWpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Ssc, bar[Fv], bar[Fe]}
If ((Include_in_loopSsc).and.(Include_in_loopFv).and.(Include_in_loopFe)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = MSsc(i1) 
ML2 = MFv(i2) 
ML3 = MFe(i3) 
coup1L = cplcFxvFvSscL(gt1,i2,i1)
coup1R = cplcFxvFvSscR(gt1,i2,i1)
coup2L = cplcFeFxecSscL(i3,i1)
coup2R = cplcFeFxecSscR(i3,i1)
coup3L = cplcFvFeVWpL(i2,i3)
coup3R = cplcFvFeVWpR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {VZ, bar[Fxv], bar[Fxe]}
If ((Include_in_loopVZ).and.(Include_in_loopFxv).and.(Include_in_loopFxe)) Then 
  Do i2=1,2
ML1 = MVZ 
ML2 = MFxv(i2) 
ML3 = MFxe 
coup1L = cplcFxvFxvVZL(gt1,i2)
coup1R = cplcFxvFxvVZR(gt1,i2)
coup2L = cplcFxeFxeVZL
coup2R = cplcFxeFxeVZR
coup3L = cplcFxvFxeVWpL(i2)
coup3R = cplcFxvFxeVWpR(i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End if 
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_FxvToFxeVWp


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxvToFxeVWp(MAh,MFe,MFv,MFxe,MFxv,           & 
& Mhh,MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,             & 
& MVWp2,MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhcHpVWp,cplcFvFeVWpL,cplcFvFeVWpR,          & 
& cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,   & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,              & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplhhcHpVWp,    & 
& cplhhcVWpVWp,cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),          & 
& MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplAhcHpVWp,cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),& 
& cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),           & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxeFxeVZL,         & 
& cplcFxeFxeVZR,cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFxvFxvhhL(2,2,2),            & 
& cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),cplhhcHpVWp(2),             & 
& cplhhcVWpVWp(2),cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ

Complex(dp), Intent(out) :: Amp(4,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
Amp(:,gt1) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxe 
Mex3 = MVWp 


! {Fxe, conj[Hp], VP}
If ((Include_in_loopFxe).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MFxe 
ML2 = MHp 
ML3 = MVP 
coup1L = cplcFxvFxeHpL(gt1)
coup1R = cplcFxvFxeHpR(gt1)
coup2L = cplcFxeFxeVPL
coup2R = cplcFxeFxeVPR
coup3 = cplcHpVPVWp
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[VWp], VP}
If ((Include_in_loopFxe).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
ML1 = MFxe 
ML2 = MVWp 
ML3 = MVP 
coup1L = cplcFxvFxeVWpL(gt1)
coup1R = cplcFxvFxeVWpR(gt1)
coup2L = cplcFxeFxeVPL
coup2R = cplcFxeFxeVPR
coup3 = cplcVWpVPVWp
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxvToFxeVWp


Subroutine Amplitude_Tree_SDdiracDM_FxvToFxvhh(cplcFxvFxvhhL,cplcFxvFxvhhR,           & 
& MFxv,Mhh,MFxv2,Mhh2,Amp)

Implicit None

Real(dp), Intent(in) :: MFxv(2),Mhh(2),MFxv2(2),Mhh2(2)

Complex(dp), Intent(in) :: cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2)

Complex(dp) :: Amp(2,2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
  Do gt2=1,2
    Do gt3=1,2
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = Mhh(gt3) 
! Tree-Level Vertex 
coupT1L = cplcFxvFxvhhL(gt1,gt2,gt3)
coupT1R = cplcFxvFxvhhR(gt1,gt2,gt3)
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_FxvToFxvhh


Subroutine Gamma_Real_SDdiracDM_FxvToFxvhh(MLambda,em,gs,cplcFxvFxvhhL,               & 
& cplcFxvFxvhhR,MFxv,Mhh,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2)

Real(dp), Intent(in) :: MFxv(2),Mhh(2)

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2,2,2), GammarealGluon(2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
CoupL = cplcFxvFxvhhL(i1,i2,i3)
CoupR = cplcFxvFxvhhR(i1,i2,i3)
Mex1 = MFxv(i1)
Mex2 = MFxv(i2)
Mex3 = Mhh(i3)
If (Mex1.gt.(Mex2+Mex3)) Then 
 Gammarealphoton(i1,i2,i3) = 0._dp 
  GammarealGluon(i1,i2,i3) = 0._dp 
Else 
  GammarealGluon(i1,i2,i3) = 0._dp 
  GammarealPhoton(i1,i2,i3) = 0._dp 

End if 
    End Do
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_FxvToFxvhh


Subroutine Amplitude_WAVE_SDdiracDM_FxvToFxvhh(cplcFxvFxvhhL,cplcFxvFxvhhR,           & 
& ctcplcFxvFxvhhL,ctcplcFxvFxvhhR,MFxv,MFxv2,Mhh,Mhh2,Zfhh,ZfxVL,ZfxVR,Amp)

Implicit None

Real(dp), Intent(in) :: MFxv(2),MFxv2(2),Mhh(2),Mhh2(2)

Complex(dp), Intent(in) :: cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2)

Complex(dp), Intent(in) :: ctcplcFxvFxvhhL(2,2,2),ctcplcFxvFxvhhR(2,2,2)

Complex(dp), Intent(in) :: Zfhh(2,2),ZfxVL(2,2),ZfxVR(2,2)

Complex(dp), Intent(out) :: Amp(2,2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
  Do gt2=1,2
    Do gt3=1,2
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = Mhh(gt3) 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFxvFxvhhL(gt1,gt2,gt3) 
ZcoupT1R = ctcplcFxvFxvhhR(gt1,gt2,gt3) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfxVR(i1,gt1)*cplcFxvFxvhhL(i1,gt2,gt3)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfxVL(i1,gt1))*cplcFxvFxvhhR(i1,gt2,gt3)
End Do


! External Field 2 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfxVL(i1,gt2)*cplcFxvFxvhhL(gt1,i1,gt3)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfxVR(i1,gt2))*cplcFxvFxvhhR(gt1,i1,gt3)
End Do


! External Field 3 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*Zfhh(i1,gt3)*cplcFxvFxvhhL(gt1,gt2,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Zfhh(i1,gt3)*cplcFxvFxvhhR(gt1,gt2,i1)
End Do


! Getting the amplitude 
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FxvToFxvhh


Subroutine Amplitude_VERTEX_SDdiracDM_FxvToFxvhh(MAh,MFv,MFxe,MFxv,Mhh,               & 
& MHp,MSsc,MVWp,MVZ,MAh2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplAhAhhh,          & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,       & 
& cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,               & 
& cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,              & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,       & 
& cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVWp,MVZ,MAh2,MFv2(3),MFxe2,               & 
& MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplAhhhVZ(2),cplcFxvFvSscL(2,3,2), & 
& cplcFxvFvSscR(2,3,2),cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxvFxeVWpL(2),              & 
& cplcFxvFxeVWpR(2),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),        & 
& cplcFxvFxvVZR(2,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),cplcFxeFxvcHpL(2),      & 
& cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplhhhhhh(2,2,2),              & 
& cplhhHpcHp(2),cplhhHpcVWp(2),cplhhSsccSsc(2,2,2),cplhhcHpVWp(2),cplhhcVWpVWp(2),       & 
& cplhhVZVZ(2)

Complex(dp), Intent(out) :: Amp(2,2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,2
    Do gt3=1,2
Amp(:,gt1, gt2, gt3) = 0._dp 
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = Mhh(gt3) 


! {Ah, bar[Fxv], bar[Fxv]}
If ((Include_in_loopAh).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
  Do i2=1,2
    Do i3=1,2
ML1 = MAh 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxvFxvAhL(gt1,i2)
coup1R = cplcFxvFxvAhR(gt1,i2)
coup2L = cplcFxvFxvAhL(i3,gt2)
coup2R = cplcFxvFxvAhR(i3,gt2)
coup3L = cplcFxvFxvhhL(i2,i3,gt3)
coup3R = cplcFxvFxvhhR(i2,i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 


! {Fv, conj[Ssc], conj[Ssc]}
If ((Include_in_loopFv).and.(Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
Do i1=1,3
  Do i2=1,2
    Do i3=1,2
ML1 = MFv(i1) 
ML2 = MSsc(i2) 
ML3 = MSsc(i3) 
coup1L = cplcFxvFvSscL(gt1,i1,i2)
coup1R = cplcFxvFvSscR(gt1,i1,i2)
coup2L = cplcFvFxvcSscL(i1,gt2,i3)
coup2R = cplcFvFxvcSscR(i1,gt2,i3)
coup3 = cplhhSsccSsc(gt3,i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fxe, conj[Hp], conj[Hp]}
If ((Include_in_loopFxe).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MFxe 
ML2 = MHp 
ML3 = MHp 
coup1L = cplcFxvFxeHpL(gt1)
coup1R = cplcFxvFxeHpR(gt1)
coup2L = cplcFxeFxvcHpL(gt2)
coup2R = cplcFxeFxvcHpR(gt2)
coup3 = cplhhHpcHp(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[VWp], conj[Hp]}
If ((Include_in_loopFxe).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MFxe 
ML2 = MVWp 
ML3 = MHp 
coup1L = cplcFxvFxeVWpL(gt1)
coup1R = cplcFxvFxeVWpR(gt1)
coup2L = cplcFxeFxvcHpL(gt2)
coup2R = cplcFxeFxvcHpR(gt2)
coup3 = -cplhhHpcVWp(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[Hp], conj[VWp]}
If ((Include_in_loopFxe).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MFxe 
ML2 = MHp 
ML3 = MVWp 
coup1L = cplcFxvFxeHpL(gt1)
coup1R = cplcFxvFxeHpR(gt1)
coup2L = cplcFxeFxvcVWpL(gt2)
coup2R = cplcFxeFxvcVWpR(gt2)
coup3 = -cplhhcHpVWp(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[VWp], conj[VWp]}
If ((Include_in_loopFxe).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MFxe 
ML2 = MVWp 
ML3 = MVWp 
coup1L = cplcFxvFxeVWpL(gt1)
coup1R = cplcFxvFxeVWpR(gt1)
coup2L = cplcFxeFxvcVWpL(gt2)
coup2R = cplcFxeFxvcVWpR(gt2)
coup3 = cplhhcVWpVWp(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {Fxv, Ah, Ah}
If ((Include_in_loopFxv).and.(Include_in_loopAh).and.(Include_in_loopAh)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MAh 
ML3 = MAh 
coup1L = cplcFxvFxvAhL(gt1,i1)
coup1R = cplcFxvFxvAhR(gt1,i1)
coup2L = cplcFxvFxvAhL(i1,gt2)
coup2R = cplcFxvFxvAhR(i1,gt2)
coup3 = cplAhAhhh(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, VZ, Ah}
If ((Include_in_loopFxv).and.(Include_in_loopVZ).and.(Include_in_loopAh)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MVZ 
ML3 = MAh 
coup1L = -cplcFxvFxvVZR(gt1,i1)
coup1R = -cplcFxvFxvVZL(gt1,i1)
coup2L = cplcFxvFxvAhL(i1,gt2)
coup2R = cplcFxvFxvAhR(i1,gt2)
coup3 = cplAhhhVZ(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, hh, hh}
If ((Include_in_loopFxv).and.(Include_in_loophh).and.(Include_in_loophh)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = Mhh(i2) 
ML3 = Mhh(i3) 
coup1L = cplcFxvFxvhhL(gt1,i1,i2)
coup1R = cplcFxvFxvhhR(gt1,i1,i2)
coup2L = cplcFxvFxvhhL(i1,gt2,i3)
coup2R = cplcFxvFxvhhR(i1,gt2,i3)
coup3 = cplhhhhhh(gt3,i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fxv, Ah, VZ}
If ((Include_in_loopFxv).and.(Include_in_loopAh).and.(Include_in_loopVZ)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MAh 
ML3 = MVZ 
coup1L = cplcFxvFxvAhL(gt1,i1)
coup1R = cplcFxvFxvAhR(gt1,i1)
coup2L = -cplcFxvFxvVZR(i1,gt2)
coup2R = -cplcFxvFxvVZL(i1,gt2)
coup3 = cplAhhhVZ(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, VZ, VZ}
If ((Include_in_loopFxv).and.(Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MVZ 
ML3 = MVZ 
coup1L = -cplcFxvFxvVZR(gt1,i1)
coup1R = -cplcFxvFxvVZL(gt1,i1)
coup2L = -cplcFxvFxvVZR(i1,gt2)
coup2R = -cplcFxvFxvVZL(i1,gt2)
coup3 = cplhhVZVZ(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End Do
End if 


! {hh, bar[Fxv], bar[Fxv]}
If ((Include_in_loophh).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = Mhh(i1) 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxvFxvhhL(gt1,i2,i1)
coup1R = cplcFxvFxvhhR(gt1,i2,i1)
coup2L = cplcFxvFxvhhL(i3,gt2,i1)
coup2R = cplcFxvFxvhhR(i3,gt2,i1)
coup3L = cplcFxvFxvhhL(i2,i3,gt3)
coup3R = cplcFxvFxvhhR(i2,i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {VZ, bar[Fxv], bar[Fxv]}
If ((Include_in_loopVZ).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
  Do i2=1,2
    Do i3=1,2
ML1 = MVZ 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = -cplcFxvFxvVZR(gt1,i2)
coup1R = -cplcFxvFxvVZL(gt1,i2)
coup2L = -cplcFxvFxvVZR(i3,gt2)
coup2R = -cplcFxvFxvVZL(i3,gt2)
coup3L = cplcFxvFxvhhL(i2,i3,gt3)
coup3R = cplcFxvFxvhhR(i2,i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
    End Do
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_FxvToFxvhh


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvhh(MAh,MFv,MFxe,MFxv,Mhh,            & 
& MHp,MSsc,MVWp,MVZ,MAh2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplAhAhhh,          & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeHpL,       & 
& cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxvFxvhhL,cplcFxvFxvhhR,               & 
& cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,              & 
& cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,       & 
& cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVWp,MVZ,MAh2,MFv2(3),MFxe2,               & 
& MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplAhhhVZ(2),cplcFxvFvSscL(2,3,2), & 
& cplcFxvFvSscR(2,3,2),cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxvFxeVWpL(2),              & 
& cplcFxvFxeVWpR(2),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),        & 
& cplcFxvFxvVZR(2,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),cplcFxeFxvcHpL(2),      & 
& cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplhhhhhh(2,2,2),              & 
& cplhhHpcHp(2),cplhhHpcVWp(2),cplhhSsccSsc(2,2,2),cplhhcHpVWp(2),cplhhcVWpVWp(2),       & 
& cplhhVZVZ(2)

Complex(dp), Intent(out) :: Amp(2,2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,2
    Do gt3=1,2
Amp(:,gt1, gt2, gt3) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = Mhh(gt3) 
    End Do
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvhh


Subroutine Amplitude_Tree_SDdiracDM_FxvToFxvVZ(cplcFxvFxvVZL,cplcFxvFxvVZR,           & 
& MFxv,MVZ,MFxv2,MVZ2,Amp)

Implicit None

Real(dp), Intent(in) :: MFxv(2),MVZ,MFxv2(2),MVZ2

Complex(dp), Intent(in) :: cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2)

Complex(dp) :: Amp(4,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
  Do gt2=1,2
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = MVZ 
! Tree-Level Vertex 
coupT1L = cplcFxvFxvVZL(gt1,gt2)
coupT1R = cplcFxvFxvVZR(gt1,gt2)
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,coupT1R,coupT1L,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2) = -AmpC 
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_FxvToFxvVZ


Subroutine Gamma_Real_SDdiracDM_FxvToFxvVZ(MLambda,em,gs,cplcFxvFxvVZL,               & 
& cplcFxvFxvVZR,MFxv,MVZ,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2)

Real(dp), Intent(in) :: MFxv(2),MVZ

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2,2), GammarealGluon(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,2
  Do i2=1,2
CoupL = cplcFxvFxvVZL(i1,i2)
CoupR = cplcFxvFxvVZR(i1,i2)
Mex1 = MFxv(i1)
Mex2 = MFxv(i2)
Mex3 = MVZ
If (Mex1.gt.(Mex2+Mex3)) Then 
  GammarealPhoton(i1,i2) = 0._dp 

  GammarealGluon(i1,i2) = 0._dp 
Else 
  GammarealGluon(i1,i2) = 0._dp 
  GammarealPhoton(i1,i2) = 0._dp 

End if 
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_FxvToFxvVZ


Subroutine Amplitude_WAVE_SDdiracDM_FxvToFxvVZ(cplcFxvFxvVZL,cplcFxvFxvVZR,           & 
& ctcplcFxvFxvVZL,ctcplcFxvFxvVZR,MFxv,MFxv2,MVZ,MVZ2,ZfVZ,ZfxVL,ZfxVR,Amp)

Implicit None

Real(dp), Intent(in) :: MFxv(2),MFxv2(2),MVZ,MVZ2

Complex(dp), Intent(in) :: cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2)

Complex(dp), Intent(in) :: ctcplcFxvFxvVZL(2,2),ctcplcFxvFxvVZR(2,2)

Complex(dp), Intent(in) :: ZfVZ,ZfxVL(2,2),ZfxVR(2,2)

Complex(dp), Intent(out) :: Amp(4,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
  Do gt2=1,2
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = MVZ 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFxvFxvVZL(gt1,gt2) 
ZcoupT1R = ctcplcFxvFxvVZR(gt1,gt2)
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*Conjg(ZfxVL(i1,gt1))*cplcFxvFxvVZL(i1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfxVR(i1,gt1)*cplcFxvFxvVZR(i1,gt2)
End Do


! External Field 2 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfxVL(i1,gt2)*cplcFxvFxvVZL(gt1,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfxVR(i1,gt2))*cplcFxvFxvVZR(gt1,i1)
End Do


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVZ*cplcFxvFxvVZL(gt1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfVZ*cplcFxvFxvVZR(gt1,gt2)


! Getting the amplitude 
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,ZcoupT1R,ZcoupT1L,AmpC) 
Amp(:,gt1, gt2) = -AmpC 
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FxvToFxvVZ


Subroutine Amplitude_VERTEX_SDdiracDM_FxvToFxvVZ(MAh,MFv,MFxe,MFxv,Mhh,               & 
& MHp,MSsc,MVWp,MVZ,MAh2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFxvFxvAhL,      & 
& cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,           & 
& cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,               & 
& cplcFxeFxeVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,  & 
& cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,          & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVWp,MVZ,MAh2,MFv2(3),MFxe2,               & 
& MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplAhhhVZ(2),cplcFxvFvSscL(2,3,2),              & 
& cplcFxvFvSscR(2,3,2),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFxvFxeHpL(2),               & 
& cplcFxvFxeHpR(2),cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxeFxeVZL,cplcFxeFxeVZR,      & 
& cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),       & 
& cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),       & 
& cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplhhVZVZ(2),cplHpcHpVZ,cplHpcVWpVZ,             & 
& cplcHpVWpVZ,cplcVWpVWpVZ

Complex(dp), Intent(out) :: Amp(4,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,2
Amp(:,gt1, gt2) = 0._dp 
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = MVZ 


! {Ah, bar[Fxv], bar[Fxv]}
If ((Include_in_loopAh).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
  Do i2=1,2
    Do i3=1,2
ML1 = MAh 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxvFxvAhL(gt1,i2)
coup1R = cplcFxvFxvAhR(gt1,i2)
coup2L = cplcFxvFxvAhL(i3,gt2)
coup2R = cplcFxvFxvAhR(i3,gt2)
coup3L = cplcFxvFxvVZL(i2,i3)
coup3R = cplcFxvFxvVZR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 


! {Fxe, conj[Hp], conj[Hp]}
If ((Include_in_loopFxe).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MFxe 
ML2 = MHp 
ML3 = MHp 
coup1L = cplcFxvFxeHpL(gt1)
coup1R = cplcFxvFxeHpR(gt1)
coup2L = cplcFxeFxvcHpL(gt2)
coup2R = cplcFxeFxvcHpR(gt2)
coup3 = cplHpcHpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[VWp], conj[Hp]}
If ((Include_in_loopFxe).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MFxe 
ML2 = MVWp 
ML3 = MHp 
coup1L = cplcFxvFxeVWpL(gt1)
coup1R = cplcFxvFxeVWpR(gt1)
coup2L = cplcFxeFxvcHpL(gt2)
coup2R = cplcFxeFxvcHpR(gt2)
coup3 = cplHpcVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[Hp], conj[VWp]}
If ((Include_in_loopFxe).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MFxe 
ML2 = MHp 
ML3 = MVWp 
coup1L = cplcFxvFxeHpL(gt1)
coup1R = cplcFxvFxeHpR(gt1)
coup2L = cplcFxeFxvcVWpL(gt2)
coup2R = cplcFxeFxvcVWpR(gt2)
coup3 = cplcHpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[VWp], conj[VWp]}
If ((Include_in_loopFxe).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MFxe 
ML2 = MVWp 
ML3 = MVWp 
coup1L = cplcFxvFxeVWpL(gt1)
coup1R = cplcFxvFxeVWpR(gt1)
coup2L = cplcFxeFxvcVWpL(gt2)
coup2R = cplcFxeFxvcVWpR(gt2)
coup3 = cplcVWpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxv, hh, Ah}
If ((Include_in_loopFxv).and.(Include_in_loophh).and.(Include_in_loopAh)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MFxv(i1) 
ML2 = Mhh(i2) 
ML3 = MAh 
coup1L = cplcFxvFxvhhL(gt1,i1,i2)
coup1R = cplcFxvFxvhhR(gt1,i1,i2)
coup2L = cplcFxvFxvAhL(i1,gt2)
coup2R = cplcFxvFxvAhR(i1,gt2)
coup3 = cplAhhhVZ(i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {Fxv, Ah, hh}
If ((Include_in_loopFxv).and.(Include_in_loopAh).and.(Include_in_loophh)) Then 
Do i1=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = MAh 
ML3 = Mhh(i3) 
coup1L = cplcFxvFxvAhL(gt1,i1)
coup1R = cplcFxvFxvAhR(gt1,i1)
coup2L = cplcFxvFxvhhL(i1,gt2,i3)
coup2R = cplcFxvFxvhhR(i1,gt2,i3)
coup3 = -cplAhhhVZ(i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
End Do
End if 


! {Fxv, VZ, hh}
If ((Include_in_loopFxv).and.(Include_in_loopVZ).and.(Include_in_loophh)) Then 
Do i1=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = MVZ 
ML3 = Mhh(i3) 
coup1L = cplcFxvFxvVZL(gt1,i1)
coup1R = cplcFxvFxvVZR(gt1,i1)
coup2L = cplcFxvFxvhhL(i1,gt2,i3)
coup2R = cplcFxvFxvhhR(i1,gt2,i3)
coup3 = cplhhVZVZ(i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
End Do
End if 


! {Fxv, hh, VZ}
If ((Include_in_loopFxv).and.(Include_in_loophh).and.(Include_in_loopVZ)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MFxv(i1) 
ML2 = Mhh(i2) 
ML3 = MVZ 
coup1L = cplcFxvFxvhhL(gt1,i1,i2)
coup1R = cplcFxvFxvhhR(gt1,i1,i2)
coup2L = cplcFxvFxvVZL(i1,gt2)
coup2R = cplcFxvFxvVZR(i1,gt2)
coup3 = cplhhVZVZ(i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {hh, bar[Fxv], bar[Fxv]}
If ((Include_in_loophh).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = Mhh(i1) 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxvFxvhhL(gt1,i2,i1)
coup1R = cplcFxvFxvhhR(gt1,i2,i1)
coup2L = cplcFxvFxvhhL(i3,gt2,i1)
coup2R = cplcFxvFxvhhR(i3,gt2,i1)
coup3L = cplcFxvFxvVZL(i2,i3)
coup3R = cplcFxvFxvVZR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {Hp, bar[Fxe], bar[Fxe]}
If ((Include_in_loopHp).and.(Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
ML1 = MHp 
ML2 = MFxe 
ML3 = MFxe 
coup1L = cplcFxvFxeHpL(gt1)
coup1R = cplcFxvFxeHpR(gt1)
coup2L = cplcFxeFxvcHpL(gt2)
coup2R = cplcFxeFxvcHpR(gt2)
coup3L = cplcFxeFxeVZL
coup3R = cplcFxeFxeVZR
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Ssc, bar[Fv], bar[Fv]}
If ((Include_in_loopSsc).and.(Include_in_loopFv).and.(Include_in_loopFv)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = MSsc(i1) 
ML2 = MFv(i2) 
ML3 = MFv(i3) 
coup1L = cplcFxvFvSscL(gt1,i2,i1)
coup1R = cplcFxvFvSscR(gt1,i2,i1)
coup2L = cplcFvFxvcSscL(i3,gt2,i1)
coup2R = cplcFvFxvcSscR(i3,gt2,i1)
coup3L = cplcFvFvVZL(i2,i3)
coup3R = cplcFvFvVZR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {VWp, bar[Fxe], bar[Fxe]}
If ((Include_in_loopVWp).and.(Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
ML1 = MVWp 
ML2 = MFxe 
ML3 = MFxe 
coup1L = cplcFxvFxeVWpL(gt1)
coup1R = cplcFxvFxeVWpR(gt1)
coup2L = cplcFxeFxvcVWpL(gt2)
coup2R = cplcFxeFxvcVWpR(gt2)
coup3L = cplcFxeFxeVZL
coup3R = cplcFxeFxeVZR
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {VZ, bar[Fxv], bar[Fxv]}
If ((Include_in_loopVZ).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
  Do i2=1,2
    Do i3=1,2
ML1 = MVZ 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxvFxvVZL(gt1,i2)
coup1R = cplcFxvFxvVZR(gt1,i2)
coup2L = cplcFxvFxvVZL(i3,gt2)
coup2R = cplcFxvFxvVZR(i3,gt2)
coup3L = cplcFxvFxvVZL(i2,i3)
coup3R = cplcFxvFxvVZR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_FxvToFxvVZ


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvVZ(MAh,MFv,MFxe,MFxv,Mhh,            & 
& MHp,MSsc,MVWp,MVZ,MAh2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFxvFxvAhL,      & 
& cplcFxvFxvAhR,cplAhhhVZ,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,           & 
& cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,               & 
& cplcFxeFxeVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,  & 
& cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,          & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVWp,MVZ,MAh2,MFv2(3),MFxe2,               & 
& MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplAhhhVZ(2),cplcFxvFvSscL(2,3,2),              & 
& cplcFxvFvSscR(2,3,2),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFxvFxeHpL(2),               & 
& cplcFxvFxeHpR(2),cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxeFxeVZL,cplcFxeFxeVZR,      & 
& cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),       & 
& cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),       & 
& cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplhhVZVZ(2),cplHpcHpVZ,cplHpcVWpVZ,             & 
& cplcHpVWpVZ,cplcVWpVWpVZ

Complex(dp), Intent(out) :: Amp(4,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,2
Amp(:,gt1, gt2) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = MVZ 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvVZ


Subroutine Amplitude_WAVE_SDdiracDM_FxvToFxvVP(cplcFxvFxvVZL,cplcFxvFxvVZR,           & 
& ctcplcFxvFxvVZL,ctcplcFxvFxvVZR,MFxv,MFxv2,MVP,MVP2,MVZ,MVZ2,ZfVP,ZfVZVP,              & 
& ZfxVL,ZfxVR,Amp)

Implicit None

Real(dp), Intent(in) :: MFxv(2),MFxv2(2),MVP,MVP2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2)

Complex(dp), Intent(in) :: ctcplcFxvFxvVZL(2,2),ctcplcFxvFxvVZR(2,2)

Complex(dp), Intent(in) :: ZfVP,ZfVZVP,ZfxVL(2,2),ZfxVR(2,2)

Complex(dp), Intent(out) :: Amp(4,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
  Do gt2=1,2
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = MVP 
ZcoupT1L = 0._dp 
ZcoupT1R = 0._dp 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
! Vanishing 


! External Field 2 
! Vanishing 


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVZVP*cplcFxvFxvVZL(gt1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfVZVP*cplcFxvFxvVZR(gt1,gt2)


! Getting the amplitude 
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,ZcoupT1R,ZcoupT1L,AmpC) 
Amp(:,gt1, gt2) = -AmpC 
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FxvToFxvVP


Subroutine Amplitude_VERTEX_SDdiracDM_FxvToFxvVP(MFxe,MFxv,MHp,MVP,MVWp,              & 
& MFxe2,MFxv2,MHp2,MVP2,MVWp2,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,   & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,           & 
& cplcFxeFxvcVWpR,cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,Amp)

Implicit None

Real(dp), Intent(in) :: MFxe,MFxv(2),MHp,MVP,MVWp,MFxe2,MFxv2(2),MHp2,MVP2,MVWp2

Complex(dp), Intent(in) :: cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL(2),      & 
& cplcFxvFxeVWpR(2),cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),              & 
& cplcFxeFxvcVWpR(2),cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp

Complex(dp), Intent(out) :: Amp(4,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,2
Amp(:,gt1, gt2) = 0._dp 
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = MVP 


! {Fxe, conj[Hp], conj[Hp]}
If ((Include_in_loopFxe).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MFxe 
ML2 = MHp 
ML3 = MHp 
coup1L = cplcFxvFxeHpL(gt1)
coup1R = cplcFxvFxeHpR(gt1)
coup2L = cplcFxeFxvcHpL(gt2)
coup2R = cplcFxeFxvcHpR(gt2)
coup3 = cplHpcHpVP
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[VWp], conj[Hp]}
If ((Include_in_loopFxe).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MFxe 
ML2 = MVWp 
ML3 = MHp 
coup1L = cplcFxvFxeVWpL(gt1)
coup1R = cplcFxvFxeVWpR(gt1)
coup2L = cplcFxeFxvcHpL(gt2)
coup2R = cplcFxeFxvcHpR(gt2)
coup3 = cplHpcVWpVP
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[Hp], conj[VWp]}
If ((Include_in_loopFxe).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MFxe 
ML2 = MHp 
ML3 = MVWp 
coup1L = cplcFxvFxeHpL(gt1)
coup1R = cplcFxvFxeHpR(gt1)
coup2L = cplcFxeFxvcVWpL(gt2)
coup2R = cplcFxeFxvcVWpR(gt2)
coup3 = cplcHpVPVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, conj[VWp], conj[VWp]}
If ((Include_in_loopFxe).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MFxe 
ML2 = MVWp 
ML3 = MVWp 
coup1L = cplcFxvFxeVWpL(gt1)
coup1R = cplcFxvFxeVWpR(gt1)
coup2L = cplcFxeFxvcVWpL(gt2)
coup2R = cplcFxeFxvcVWpR(gt2)
coup3 = -cplcVWpVPVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Hp, bar[Fxe], bar[Fxe]}
If ((Include_in_loopHp).and.(Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
ML1 = MHp 
ML2 = MFxe 
ML3 = MFxe 
coup1L = cplcFxvFxeHpL(gt1)
coup1R = cplcFxvFxeHpR(gt1)
coup2L = cplcFxeFxvcHpL(gt2)
coup2R = cplcFxeFxvcHpR(gt2)
coup3L = cplcFxeFxeVPL
coup3R = cplcFxeFxeVPR
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {VWp, bar[Fxe], bar[Fxe]}
If ((Include_in_loopVWp).and.(Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
ML1 = MVWp 
ML2 = MFxe 
ML3 = MFxe 
coup1L = cplcFxvFxeVWpL(gt1)
coup1R = cplcFxvFxeVWpR(gt1)
coup2L = cplcFxeFxvcVWpL(gt2)
coup2R = cplcFxeFxvcVWpR(gt2)
coup3L = cplcFxeFxeVPL
coup3R = cplcFxeFxeVPR
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_FxvToFxvVP


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvVP(MFxe,MFxv,MHp,MVP,MVWp,           & 
& MFxe2,MFxv2,MHp2,MVP2,MVWp2,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,   & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,           & 
& cplcFxeFxvcVWpR,cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,Amp)

Implicit None

Real(dp), Intent(in) :: MFxe,MFxv(2),MHp,MVP,MVWp,MFxe2,MFxv2(2),MHp2,MVP2,MVWp2

Complex(dp), Intent(in) :: cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL(2),      & 
& cplcFxvFxeVWpR(2),cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),              & 
& cplcFxeFxvcVWpR(2),cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp

Complex(dp), Intent(out) :: Amp(4,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,2
Amp(:,gt1, gt2) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFxv(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = MVP 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxvToFxvVP



End Module OneLoopDecay_Fxv_SDdiracDM
