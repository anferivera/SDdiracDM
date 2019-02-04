! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:21 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module OneLoopDecay_hh_SDdiracDM
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

Subroutine Amplitude_Tree_SDdiracDM_hhToAhAh(cplAhAhhh,MAh,Mhh,MAh2,Mhh2,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,Mhh(2),MAh2,Mhh2(2)

Complex(dp), Intent(in) :: cplAhAhhh(2)

Complex(dp) :: Amp(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MAh 
Mex3 = MAh 
! Tree-Level Vertex 
coupT1 = cplAhAhhh(gt1)
Call TreeAmp_StoSS(Mex1,Mex2,Mex3,coupT1,AmpC) 
! Colour and symmetry factor 
Amp(gt1) = AmpC 
End Do
End Subroutine Amplitude_Tree_SDdiracDM_hhToAhAh


Subroutine Gamma_Real_SDdiracDM_hhToAhAh(MLambda,em,gs,cplAhAhhh,MAh,Mhh,             & 
& GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplAhAhhh(2)

Real(dp), Intent(in) :: MAh,Mhh(2)

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2), GammarealGluon(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: Coup 
 
Do i1=1,2
Coup = cplAhAhhh(i1)
Mex1 = Mhh(i1)
Mex2 = MAh
Mex3 = MAh
If (Mex1.gt.(Mex2+Mex3)) Then 
 Gammarealphoton(i1) = 0._dp 
  GammarealGluon(i1) = 0._dp 
Else 
  GammarealGluon(i1) = 0._dp 
  GammarealPhoton(i1) = 0._dp 

End if 
End Do
End Subroutine Gamma_Real_SDdiracDM_hhToAhAh


Subroutine Amplitude_WAVE_SDdiracDM_hhToAhAh(cplAhAhhh,ctcplAhAhhh,MAh,               & 
& MAh2,Mhh,Mhh2,ZfAh,Zfhh,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MAh2,Mhh(2),Mhh2(2)

Complex(dp), Intent(in) :: cplAhAhhh(2)

Complex(dp), Intent(in) :: ctcplAhAhhh(2)

Complex(dp), Intent(in) :: ZfAh,Zfhh(2,2)

Complex(dp), Intent(out) :: Amp(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MAh 
Mex3 = MAh 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1 = ctcplAhAhhh(gt1) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1 = ZcoupT1 + 0.5_dp*Zfhh(i1,gt1)*cplAhAhhh(i1)
End Do


! External Field 2 
ZcoupT1 = ZcoupT1 + 0.5_dp*ZfAh*cplAhAhhh(gt1)


! External Field 3 
ZcoupT1 = ZcoupT1 + 0.5_dp*ZfAh*cplAhAhhh(gt1)


! Getting the amplitude 
Call TreeAmp_StoSS(Mex1,Mex2,Mex3,ZcoupT1,AmpC) 
Amp(gt1) = AmpC 
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhToAhAh


Subroutine Amplitude_VERTEX_SDdiracDM_hhToAhAh(MAh,MFd,MFe,MFu,MFxv,Mhh,              & 
& MHp,MSsc,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplAhAhhh,      & 
& cplcFdFdAhL,cplcFdFdAhR,cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,               & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ,cplAhHpcVWp,           & 
& cplAhcHpVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,               & 
& cplcFuFuhhR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcgWpgWphh,cplcgWCgWChh,cplhhhhhh,           & 
& cplhhHpcHp,cplhhSsccSsc,cplhhcVWpVWp,cplhhVZVZ,cplAhAhAhAh1,cplAhAhhhhh1,              & 
& cplAhAhHpcHp1,cplAhAhSsccSsc1,cplAhAhcVWpVWp1,cplAhAhVZVZ1,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),MFxv(2),Mhh(2),MHp,MSsc(2),MVWp,MVZ,MAh2,MFd2(3),            & 
& MFe2(3),MFu2(3),MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),     & 
& cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),               & 
& cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ(2),cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL(3,3,2),     & 
& cplcFdFdhhR(3,3,2),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFuFuhhL(3,3,2),           & 
& cplcFuFuhhR(3,3,2),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcgWpgWphh(2),          & 
& cplcgWCgWChh(2),cplhhhhhh(2,2,2),cplhhHpcHp(2),cplhhSsccSsc(2,2,2),cplhhcVWpVWp(2),    & 
& cplhhVZVZ(2),cplAhAhAhAh1,cplAhAhhhhh1(2,2),cplAhAhHpcHp1,cplAhAhSsccSsc1(2,2),        & 
& cplAhAhcVWpVWp1,cplAhAhVZVZ1

Complex(dp), Intent(out) :: Amp(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
Amp(gt1) = 0._dp 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MAh 
Mex3 = MAh 


! {Ah, Ah, hh}
If ((Include_in_loopAh).and.(Include_in_loopAh).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MAh 
ML2 = MAh 
ML3 = Mhh(i3) 
coup1 = cplAhAhhh(gt1)
coup2 = cplAhAhhh(i3)
coup3 = cplAhAhhh(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {Ah, VZ, hh}
If ((Include_in_loopAh).and.(Include_in_loopVZ).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MAh 
ML2 = MVZ 
ML3 = Mhh(i3) 
coup1 = cplAhhhVZ(gt1)
coup2 = cplAhAhhh(i3)
coup3 = cplAhhhVZ(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {Fd, Fd, Fd}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFdFdhhL(i2,i1,gt1)
coup1R = cplcFdFdhhR(i2,i1,gt1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFdFdAhL(i3,i2)
coup3R = cplcFdFdAhR(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fe, Fe, Fe}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFehhL(i2,i1,gt1)
coup1R = cplcFeFehhR(i2,i1,gt1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFeFeAhL(i3,i2)
coup3R = cplcFeFeAhR(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(2)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fu, Fu, Fu}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuhhL(i2,i1,gt1)
coup1R = cplcFuFuhhR(i2,i1,gt1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFuFuAhL(i3,i2)
coup3R = cplcFuFuAhR(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fxv, Fxv, Fxv}
If ((Include_in_loopFxv).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxvFxvhhL(i2,i1,gt1)
coup1R = cplcFxvFxvhhR(i2,i1,gt1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFxvAhL(i3,i2)
coup3R = cplcFxvFxvAhR(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(2)*AmpC 
    End Do
  End Do
End Do
End if 


! {gWp, gWp, gWp}
If ((Include_in_loopgWp).and.(Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWpgWphh(gt1)
coup2 = cplcgWpgWpAh
coup3 = cplcgWpgWpAh
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(2)*AmpC 
End if 


! {gWpC, gWpC, gWpC}
If ((Include_in_loopgWC).and.(Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWCgWChh(gt1)
coup2 = cplcgWCgWCAh
coup3 = cplcgWCgWCAh
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(2)*AmpC 
End if 


! {hh, hh, Ah}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loopAh)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = MAh 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = cplAhAhhh(i1)
coup3 = cplAhAhhh(i2)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {hh, hh, VZ}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loopVZ)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = MVZ 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = -cplAhhhVZ(i1)
coup3 = cplAhhhVZ(i2)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {Hp, Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = -cplAhcHpVWp
coup3 = cplAhHpcVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = -cplAhHpcVWp
coup3 = cplAhcHpVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {VZ, Ah, hh}
If ((Include_in_loopVZ).and.(Include_in_loopAh).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MVZ 
ML2 = MAh 
ML3 = Mhh(i3) 
coup1 = cplAhhhVZ(gt1)
coup2 = -cplAhhhVZ(i3)
coup3 = cplAhAhhh(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VZ, VZ, hh}
If ((Include_in_loopVZ).and.(Include_in_loopVZ).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MVZ 
ML2 = MVZ 
ML3 = Mhh(i3) 
coup1 = cplhhVZVZ(gt1)
coup2 = -cplAhhhVZ(i3)
coup3 = cplAhhhVZ(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[Hp], conj[Hp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = -cplAhHpcVWp
coup3 = cplAhcHpVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = -cplAhcHpVWp
coup3 = cplAhHpcVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {Ah, Ah}
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
ML1 = MAh 
ML2 = MAh 
coup1 = cplAhAhhh(gt1)
coup2 = cplAhAhAhAh1
Call Amp_VERTEX_StoSS_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1._dp/2._dp)*AmpC 
End if 


! {hh, hh}
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = cplAhAhhhhh1(i1,i2)
Call Amp_VERTEX_StoSS_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1._dp/2._dp)*AmpC 
  End Do
End Do
End if 


! {Hp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MHp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplAhAhHpcHp1
Call Amp_VERTEX_StoSS_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {Ssc, Ssc}
If ((Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MSsc(i1) 
ML2 = MSsc(i2) 
coup1 = cplhhSsccSsc(gt1,i1,i2)
coup2 = cplAhAhSsccSsc1(i2,i1)
Call Amp_VERTEX_StoSS_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {VWp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplAhAhcVWpVWp1
Call Amp_VERTEX_StoSS_Topology2_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {VZ, VZ}
If ((Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
ML1 = MVZ 
ML2 = MVZ 
coup1 = cplhhVZVZ(gt1)
coup2 = cplAhAhVZVZ1
Call Amp_VERTEX_StoSS_Topology2_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1._dp/2._dp)*AmpC 
End if 


! {Ah, hh}
If ((Include_in_loopAh).and.(Include_in_loophh)) Then 
  Do i2=1,2
ML1 = MAh 
ML2 = Mhh(i2) 
coup1 = cplAhAhhhhh1(gt1,i2)
coup2 = cplAhAhhh(i2)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology3_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 

  End Do
End if 


! {Ah, hh}
If ((Include_in_loopAh).and.(Include_in_loophh)) Then 
  Do i2=1,2
ML1 = MAh 
ML2 = Mhh(i2) 
coup1 = cplAhAhhhhh1(gt1,i2)
coup2 = cplAhAhhh(i2)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology4_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 

  End Do
End if 
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhToAhAh


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToAhAh(MAh,MFd,MFe,MFu,MFxv,               & 
& Mhh,MHp,MSsc,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,            & 
& cplAhAhhh,cplcFdFdAhL,cplcFdFdAhR,cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,     & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ,cplAhHpcVWp,           & 
& cplAhcHpVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,               & 
& cplcFuFuhhR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcgWpgWphh,cplcgWCgWChh,cplhhhhhh,           & 
& cplhhHpcHp,cplhhSsccSsc,cplhhcVWpVWp,cplhhVZVZ,cplAhAhAhAh1,cplAhAhhhhh1,              & 
& cplAhAhHpcHp1,cplAhAhSsccSsc1,cplAhAhcVWpVWp1,cplAhAhVZVZ1,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),MFxv(2),Mhh(2),MHp,MSsc(2),MVWp,MVZ,MAh2,MFd2(3),            & 
& MFe2(3),MFu2(3),MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),     & 
& cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),               & 
& cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ(2),cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL(3,3,2),     & 
& cplcFdFdhhR(3,3,2),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFuFuhhL(3,3,2),           & 
& cplcFuFuhhR(3,3,2),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcgWpgWphh(2),          & 
& cplcgWCgWChh(2),cplhhhhhh(2,2,2),cplhhHpcHp(2),cplhhSsccSsc(2,2,2),cplhhcVWpVWp(2),    & 
& cplhhVZVZ(2),cplAhAhAhAh1,cplAhAhhhhh1(2,2),cplAhAhHpcHp1,cplAhAhSsccSsc1(2,2),        & 
& cplAhAhcVWpVWp1,cplAhAhVZVZ1

Complex(dp), Intent(out) :: Amp(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
Amp(gt1) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MAh 
Mex3 = MAh 
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToAhAh


Subroutine Amplitude_Tree_SDdiracDM_hhToAhVZ(cplAhhhVZ,MAh,Mhh,MVZ,MAh2,              & 
& Mhh2,MVZ2,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,Mhh(2),MVZ,MAh2,Mhh2(2),MVZ2

Complex(dp), Intent(in) :: cplAhhhVZ(2)

Complex(dp) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MAh 
Mex3 = MVZ 
! Tree-Level Vertex 
coupT1 = -cplAhhhVZ(gt1)
Call TreeAmp_StoSV(Mex1,Mex2,Mex3,coupT1,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1) = AmpC 
End Do
End Subroutine Amplitude_Tree_SDdiracDM_hhToAhVZ


Subroutine Gamma_Real_SDdiracDM_hhToAhVZ(MLambda,em,gs,cplAhhhVZ,MAh,Mhh,             & 
& MVZ,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplAhhhVZ(2)

Real(dp), Intent(in) :: MAh,Mhh(2),MVZ

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2), GammarealGluon(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: Coup 
 
Do i1=1,2
Coup = cplAhhhVZ(i1)
Mex1 = Mhh(i1)
Mex2 = MAh
Mex3 = MVZ
If (Mex1.gt.(Mex2+Mex3)) Then 
 Gammarealphoton(i1) = 0._dp 
 GammarealGluon(i1) = 0._dp 
Else 
  GammarealGluon(i1) = 0._dp 
  GammarealPhoton(i1) = 0._dp 

End if 
End Do
End Subroutine Gamma_Real_SDdiracDM_hhToAhVZ


Subroutine Amplitude_WAVE_SDdiracDM_hhToAhVZ(cplAhhhVZ,ctcplAhhhVZ,MAh,               & 
& MAh2,Mhh,Mhh2,MVZ,MVZ2,ZfAh,Zfhh,ZfVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MAh2,Mhh(2),Mhh2(2),MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhhhVZ(2)

Complex(dp), Intent(in) :: ctcplAhhhVZ(2)

Complex(dp), Intent(in) :: ZfAh,Zfhh(2,2),ZfVZ

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MAh 
Mex3 = MVZ 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1 = ctcplAhhhVZ(gt1) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1 = ZcoupT1 + 0.5_dp*Zfhh(i1,gt1)*cplAhhhVZ(i1)
End Do


! External Field 2 
ZcoupT1 = ZcoupT1 + 0.5_dp*ZfAh*cplAhhhVZ(gt1)


! External Field 3 
ZcoupT1 = ZcoupT1 + 0.5_dp*ZfVZ*cplAhhhVZ(gt1)


! Getting the amplitude 
Call TreeAmp_StoSV(Mex1,Mex2,Mex3,-ZcoupT1,AmpC) 
Amp(:,gt1) = AmpC 
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhToAhVZ


Subroutine Amplitude_VERTEX_SDdiracDM_hhToAhVZ(MAh,MFd,MFe,MFu,MFxv,Mhh,              & 
& MHp,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MVWp2,MVZ2,cplAhAhhh,cplcFdFdAhL,     & 
& cplcFdFdAhR,cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplcFxvFxvAhL,             & 
& cplcFxvFxvAhR,cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,             & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWphh,cplcgWpgWpVZ,     & 
& cplcgWCgWChh,cplcgWCgWCVZ,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,   & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ1,cplAhHpcVWpVZ1, & 
& cplAhcHpVWpVZ1,cplhhhhVZVZ1,cplhhHpcVWpVZ1,cplhhcHpVWpVZ1,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),MFxv(2),Mhh(2),MHp,MVWp,MVZ,MAh2,MFd2(3),MFe2(3),            & 
& MFu2(3),MFxv2(2),Mhh2(2),MHp2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),     & 
& cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),               & 
& cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ(2),cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL(3,3,2),     & 
& cplcFdFdhhR(3,3,2),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFehhL(3,3,2),               & 
& cplcFeFehhR(3,3,2),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFuFuhhL(3,3,2),               & 
& cplcFuFuhhR(3,3,2),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFxvFxvhhL(2,2,2),             & 
& cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),cplcgWpgWphh(2),            & 
& cplcgWpgWpVZ,cplcgWCgWChh(2),cplcgWCgWCVZ,cplhhhhhh(2,2,2),cplhhHpcHp(2),              & 
& cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplhhVZVZ(2),cplHpcHpVZ,cplHpcVWpVZ,     & 
& cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ1,cplAhHpcVWpVZ1,cplAhcHpVWpVZ1,cplhhhhVZVZ1(2,2), & 
& cplhhHpcVWpVZ1(2),cplhhcHpVWpVZ1(2)

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
Mex1 = Mhh(gt1) 
Mex2 = MAh 
Mex3 = MVZ 


! {Ah, Ah, hh}
If ((Include_in_loopAh).and.(Include_in_loopAh).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MAh 
ML2 = MAh 
ML3 = Mhh(i3) 
coup1 = cplAhAhhh(gt1)
coup2 = cplAhAhhh(i3)
coup3 = -cplAhhhVZ(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {Ah, VZ, hh}
If ((Include_in_loopAh).and.(Include_in_loopVZ).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MAh 
ML2 = MVZ 
ML3 = Mhh(i3) 
coup1 = cplAhhhVZ(gt1)
coup2 = cplAhAhhh(i3)
coup3 = cplhhVZVZ(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {Fd, Fd, Fd}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFdFdhhL(i2,i1,gt1)
coup1R = cplcFdFdhhR(i2,i1,gt1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = -cplcFdFdVZR(i3,i2)
coup3R = -cplcFdFdVZL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fe, Fe, Fe}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFehhL(i2,i1,gt1)
coup1R = cplcFeFehhR(i2,i1,gt1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = -cplcFeFeVZR(i3,i2)
coup3R = -cplcFeFeVZL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fu, Fu, Fu}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuhhL(i2,i1,gt1)
coup1R = cplcFuFuhhR(i2,i1,gt1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = -cplcFuFuVZR(i3,i2)
coup3R = -cplcFuFuVZL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fxv, Fxv, Fxv}
If ((Include_in_loopFxv).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxvFxvhhL(i2,i1,gt1)
coup1R = cplcFxvFxvhhR(i2,i1,gt1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = -cplcFxvFxvVZR(i3,i2)
coup3R = -cplcFxvFxvVZL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
    End Do
  End Do
End Do
End if 


! {gWp, gWp, gWp}
If ((Include_in_loopgWp).and.(Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWpgWphh(gt1)
coup2 = cplcgWpgWpAh
coup3 = cplcgWpgWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
End if 


! {gWpC, gWpC, gWpC}
If ((Include_in_loopgWC).and.(Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWCgWChh(gt1)
coup2 = cplcgWCgWCAh
coup3 = cplcgWCgWCVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
End if 


! {hh, hh, Ah}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loopAh)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = MAh 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = cplAhAhhh(i1)
coup3 = cplAhhhVZ(i2)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {hh, hh, VZ}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loopVZ)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = MVZ 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = cplAhhhVZ(i1)
coup3 = cplhhVZVZ(i2)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {Hp, Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplAhcHpVWp
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplAhcHpVWp
coup3 = -cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplAhHpcVWp
coup3 = -cplHpcHpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplAhHpcVWp
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VZ, Ah, hh}
If ((Include_in_loopVZ).and.(Include_in_loopAh).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MVZ 
ML2 = MAh 
ML3 = Mhh(i3) 
coup1 = cplAhhhVZ(gt1)
coup2 = cplAhhhVZ(i3)
coup3 = -cplAhhhVZ(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VZ, VZ, hh}
If ((Include_in_loopVZ).and.(Include_in_loopVZ).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MVZ 
ML2 = MVZ 
ML3 = Mhh(i3) 
coup1 = cplhhVZVZ(gt1)
coup2 = cplAhhhVZ(i3)
coup3 = cplhhVZVZ(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[Hp], conj[Hp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplAhHpcVWp
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[VWp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplAhHpcVWp
coup3 = cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplAhcHpVWp
coup3 = cplHpcHpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplAhcHpVWp
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Ah, VZ}
If ((Include_in_loopAh).and.(Include_in_loopVZ)) Then 
ML1 = MAh 
ML2 = MVZ 
coup1 = -cplAhhhVZ(gt1)
coup2 = cplAhAhVZVZ1
Call Amp_VERTEX_StoSV_Topology2_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
coup1 = cplhhHpcVWp(gt1)
coup2 = cplAhcHpVWpVZ1
Call Amp_VERTEX_StoSV_Topology2_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplAhHpcVWpVZ1
Call Amp_VERTEX_StoSV_Topology2_VS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {hh, VZ}
If ((Include_in_loophh).and.(Include_in_loopVZ)) Then 
Do i1=1,2
ML1 = Mhh(i1) 
ML2 = MVZ 
coup1 = cplhhhhVZVZ1(gt1,i1)
coup2 = cplAhhhVZ(i1)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology3_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End Do
End if 


! {Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
coup1 = cplhhHpcVWpVZ1(gt1)
coup2 = cplAhcHpVWp
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology3_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 


! {VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
coup1 = cplhhcHpVWpVZ1(gt1)
coup2 = cplAhHpcVWp
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology3_VS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhToAhVZ


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToAhVZ(MAh,MFd,MFe,MFu,MFxv,               & 
& Mhh,MHp,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MVWp2,MVZ2,cplAhAhhh,             & 
& cplcFdFdAhL,cplcFdFdAhR,cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,               & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ,cplAhHpcVWp,           & 
& cplAhcHpVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,               & 
& cplcFeFehhR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,               & 
& cplcFuFuVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWphh,      & 
& cplcgWpgWpVZ,cplcgWCgWChh,cplcgWCgWCVZ,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,               & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,    & 
& cplAhAhVZVZ1,cplAhHpcVWpVZ1,cplAhcHpVWpVZ1,cplhhhhVZVZ1,cplhhHpcVWpVZ1,cplhhcHpVWpVZ1,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),MFxv(2),Mhh(2),MHp,MVWp,MVZ,MAh2,MFd2(3),MFe2(3),            & 
& MFu2(3),MFxv2(2),Mhh2(2),MHp2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),     & 
& cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),               & 
& cplcgWpgWpAh,cplcgWCgWCAh,cplAhhhVZ(2),cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL(3,3,2),     & 
& cplcFdFdhhR(3,3,2),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFehhL(3,3,2),               & 
& cplcFeFehhR(3,3,2),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFuFuhhL(3,3,2),               & 
& cplcFuFuhhR(3,3,2),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFxvFxvhhL(2,2,2),             & 
& cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),cplcgWpgWphh(2),            & 
& cplcgWpgWpVZ,cplcgWCgWChh(2),cplcgWCgWCVZ,cplhhhhhh(2,2,2),cplhhHpcHp(2),              & 
& cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplhhVZVZ(2),cplHpcHpVZ,cplHpcVWpVZ,     & 
& cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ1,cplAhHpcVWpVZ1,cplAhcHpVWpVZ1,cplhhhhVZVZ1(2,2), & 
& cplhhHpcVWpVZ1(2),cplhhcHpVWpVZ1(2)

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
Mex1 = Mhh(gt1) 
Mex2 = MAh 
Mex3 = MVZ 
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToAhVZ


Subroutine Amplitude_Tree_SDdiracDM_hhTocFdFd(cplcFdFdhhL,cplcFdFdhhR,MFd,            & 
& Mhh,MFd2,Mhh2,Amp)

Implicit None

Real(dp), Intent(in) :: MFd(3),Mhh(2),MFd2(3),Mhh2(2)

Complex(dp), Intent(in) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2)

Complex(dp) :: Amp(2,2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,3
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MFd(gt2) 
Mex3 = MFd(gt3) 
! Tree-Level Vertex 
coupT1L = cplcFdFdhhL(gt2,gt3,gt1)
coupT1R = cplcFdFdhhR(gt2,gt3,gt1)
Call TreeAmp_StoFF(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_hhTocFdFd


Subroutine Gamma_Real_SDdiracDM_hhTocFdFd(MLambda,em,gs,cplcFdFdhhL,cplcFdFdhhR,      & 
& MFd,Mhh,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2)

Real(dp), Intent(in) :: MFd(3),Mhh(2)

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2,3,3), GammarealGluon(2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
CoupL = cplcFdFdhhL(i2,i3,i1)
CoupR = cplcFdFdhhR(i2,i3,i1)
Mex1 = Mhh(i1)
Mex2 = MFd(i2)
Mex3 = MFd(i3)
If (Mex1.gt.(Mex2+Mex3)) Then 
 Call hardradiationSFF(Mex1,Mex2,Mex3,MLambda,em,0._dp,0._dp,0._dp,1._dp/3._dp,-1._dp/3._dp,1._dp/3._dp,CoupL,CoupR,Gammarealphoton(i1,i2,i3),kont)
 Call hardradiationSFF(Mex1,Mex2,Mex3,MLambda,gs,0._dp,0._dp,0._dp,4._dp,-4._dp,4._dp,CoupL,CoupR,Gammarealgluon(i1,i2,i3),kont)
Else 
  GammarealGluon(i1,i2,i3) = 0._dp 
  GammarealPhoton(i1,i2,i3) = 0._dp 

End if 
    End Do
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_hhTocFdFd


Subroutine Amplitude_WAVE_SDdiracDM_hhTocFdFd(cplcFdFdhhL,cplcFdFdhhR,ctcplcFdFdhhL,  & 
& ctcplcFdFdhhR,MFd,MFd2,Mhh,Mhh2,ZfDL,ZfDR,Zfhh,Amp)

Implicit None

Real(dp), Intent(in) :: MFd(3),MFd2(3),Mhh(2),Mhh2(2)

Complex(dp), Intent(in) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2)

Complex(dp), Intent(in) :: ctcplcFdFdhhL(3,3,2),ctcplcFdFdhhR(3,3,2)

Complex(dp), Intent(in) :: ZfDL(3,3),ZfDR(3,3),Zfhh(2,2)

Complex(dp), Intent(out) :: Amp(2,2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,3
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MFd(gt2) 
Mex3 = MFd(gt3) 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFdFdhhL(gt2,gt3,gt1) 
ZcoupT1R = ctcplcFdFdhhR(gt2,gt3,gt1) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*Zfhh(i1,gt1)*cplcFdFdhhL(gt2,gt3,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Zfhh(i1,gt1)*cplcFdFdhhR(gt2,gt3,i1)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfDR(i1,gt2)*cplcFdFdhhL(i1,gt3,gt1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfDL(i1,gt2))*cplcFdFdhhR(i1,gt3,gt1)
End Do


! External Field 3 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfDL(i1,gt3)*cplcFdFdhhL(gt2,i1,gt1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfDR(i1,gt3))*cplcFdFdhhR(gt2,i1,gt1)
End Do


! Getting the amplitude 
Call TreeAmp_StoFF(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhTocFdFd


Subroutine Amplitude_VERTEX_SDdiracDM_hhTocFdFd(MAh,MFd,MFu,Mhh,MHp,MVG,              & 
& MVP,MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFdFdAhL,      & 
& cplcFdFdAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,     & 
& cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,             & 
& cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,               & 
& cplhhVZVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFu(3),Mhh(2),MHp,MVG,MVP,MVWp,MVZ,MAh2,MFd2(3),MFu2(3),Mhh2(2),           & 
& MHp2,MVG2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplAhhhVZ(2),cplcFdFdhhL(3,3,2),       & 
& cplcFdFdhhR(3,3,2),cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),& 
& cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFdFdVZL(3,3),& 
& cplcFdFdVZR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFdFucHpL(3,3),              & 
& cplcFdFucHpR(3,3),cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),cplhhhhhh(2,2,2),              & 
& cplhhHpcHp(2),cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplhhVZVZ(2)

Complex(dp), Intent(out) :: Amp(2,2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,3
Amp(:,gt1, gt2, gt3) = 0._dp 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MFd(gt2) 
Mex3 = MFd(gt3) 


! {Ah, Ah, Fd}
If ((Include_in_loopAh).and.(Include_in_loopAh).and.(Include_in_loopFd)) Then 
    Do i3=1,3
ML1 = MAh 
ML2 = MAh 
ML3 = MFd(i3) 
coup1 = cplAhAhhh(gt1)
coup2L = cplcFdFdAhL(gt2,i3)
coup2R = cplcFdFdAhR(gt2,i3)
coup3L = cplcFdFdAhL(i3,gt3)
coup3R = cplcFdFdAhR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {Ah, VZ, Fd}
If ((Include_in_loopAh).and.(Include_in_loopVZ).and.(Include_in_loopFd)) Then 
    Do i3=1,3
ML1 = MAh 
ML2 = MVZ 
ML3 = MFd(i3) 
coup1 = -cplAhhhVZ(gt1)
coup2L = cplcFdFdAhL(gt2,i3)
coup2R = cplcFdFdAhR(gt2,i3)
coup3L = cplcFdFdVZL(i3,gt3)
coup3R = cplcFdFdVZR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {hh, hh, Fd}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loopFd)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,3
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = MFd(i3) 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2L = cplcFdFdhhL(gt2,i3,i1)
coup2R = cplcFdFdhhR(gt2,i3,i1)
coup3L = cplcFdFdhhL(i3,gt3,i2)
coup3R = cplcFdFdhhR(i3,gt3,i2)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {Hp, Hp, Fu}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopFu)) Then 
    Do i3=1,3
ML1 = MHp 
ML2 = MHp 
ML3 = MFu(i3) 
coup1 = cplhhHpcHp(gt1)
coup2L = cplcFdFucHpL(gt2,i3)
coup2R = cplcFdFucHpR(gt2,i3)
coup3L = cplcFuFdHpL(i3,gt3)
coup3R = cplcFuFdHpR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {Hp, VWp, Fu}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopFu)) Then 
    Do i3=1,3
ML1 = MHp 
ML2 = MVWp 
ML3 = MFu(i3) 
coup1 = cplhhHpcVWp(gt1)
coup2L = cplcFdFucHpL(gt2,i3)
coup2R = cplcFdFucHpR(gt2,i3)
coup3L = cplcFuFdVWpL(i3,gt3)
coup3R = cplcFuFdVWpR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VWp, Hp, Fu}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopFu)) Then 
    Do i3=1,3
ML1 = MVWp 
ML2 = MHp 
ML3 = MFu(i3) 
coup1 = cplhhcHpVWp(gt1)
coup2L = cplcFdFucVWpL(gt2,i3)
coup2R = cplcFdFucVWpR(gt2,i3)
coup3L = cplcFuFdHpL(i3,gt3)
coup3R = cplcFuFdHpR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VWp, VWp, Fu}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopFu)) Then 
    Do i3=1,3
ML1 = MVWp 
ML2 = MVWp 
ML3 = MFu(i3) 
coup1 = cplhhcVWpVWp(gt1)
coup2L = cplcFdFucVWpL(gt2,i3)
coup2R = cplcFdFucVWpR(gt2,i3)
coup3L = cplcFuFdVWpL(i3,gt3)
coup3R = cplcFuFdVWpR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VZ, Ah, Fd}
If ((Include_in_loopVZ).and.(Include_in_loopAh).and.(Include_in_loopFd)) Then 
    Do i3=1,3
ML1 = MVZ 
ML2 = MAh 
ML3 = MFd(i3) 
coup1 = -cplAhhhVZ(gt1)
coup2L = cplcFdFdVZL(gt2,i3)
coup2R = cplcFdFdVZR(gt2,i3)
coup3L = cplcFdFdAhL(i3,gt3)
coup3R = cplcFdFdAhR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VZ, VZ, Fd}
If ((Include_in_loopVZ).and.(Include_in_loopVZ).and.(Include_in_loopFd)) Then 
    Do i3=1,3
ML1 = MVZ 
ML2 = MVZ 
ML3 = MFd(i3) 
coup1 = cplhhVZVZ(gt1)
coup2L = cplcFdFdVZL(gt2,i3)
coup2R = cplcFdFdVZR(gt2,i3)
coup3L = cplcFdFdVZL(i3,gt3)
coup3R = cplcFdFdVZR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {bar[Fd], bar[Fd], Ah}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopAh)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MAh 
coup1L = cplcFdFdhhL(i1,i2,gt1)
coup1R = cplcFdFdhhR(i1,i2,gt1)
coup2L = cplcFdFdAhL(gt2,i1)
coup2R = cplcFdFdAhR(gt2,i1)
coup3L = cplcFdFdAhL(i2,gt3)
coup3R = cplcFdFdAhR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {bar[Fd], bar[Fd], hh}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loophh)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,2
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = Mhh(i3) 
coup1L = cplcFdFdhhL(i1,i2,gt1)
coup1R = cplcFdFdhhR(i1,i2,gt1)
coup2L = cplcFdFdhhL(gt2,i1,i3)
coup2R = cplcFdFdhhR(gt2,i1,i3)
coup3L = cplcFdFdhhL(i2,gt3,i3)
coup3R = cplcFdFdhhR(i2,gt3,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {bar[Fd], bar[Fd], VG}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopVG)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MVG 
coup1L = cplcFdFdhhL(i1,i2,gt1)
coup1R = cplcFdFdhhR(i1,i2,gt1)
coup2L = cplcFdFdVGL(gt2,i1)
coup2R = cplcFdFdVGR(gt2,i1)
coup3L = cplcFdFdVGL(i2,gt3)
coup3R = cplcFdFdVGR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(4._dp/3._dp)*AmpC 
  End Do
End Do
End if 


! {bar[Fd], bar[Fd], VP}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopVP)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MVP 
coup1L = cplcFdFdhhL(i1,i2,gt1)
coup1R = cplcFdFdhhR(i1,i2,gt1)
coup2L = cplcFdFdVPL(gt2,i1)
coup2R = cplcFdFdVPR(gt2,i1)
coup3L = cplcFdFdVPL(i2,gt3)
coup3R = cplcFdFdVPR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {bar[Fd], bar[Fd], VZ}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopVZ)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MVZ 
coup1L = cplcFdFdhhL(i1,i2,gt1)
coup1R = cplcFdFdhhR(i1,i2,gt1)
coup2L = cplcFdFdVZL(gt2,i1)
coup2R = cplcFdFdVZR(gt2,i1)
coup3L = cplcFdFdVZL(i2,gt3)
coup3R = cplcFdFdVZR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {bar[Fu], bar[Fu], conj[Hp]}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopHp)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MHp 
coup1L = cplcFuFuhhL(i1,i2,gt1)
coup1R = cplcFuFuhhR(i1,i2,gt1)
coup2L = cplcFdFucHpL(gt2,i1)
coup2R = cplcFdFucHpR(gt2,i1)
coup3L = cplcFuFdHpL(i2,gt3)
coup3R = cplcFuFdHpR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {bar[Fu], bar[Fu], conj[VWp]}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopVWp)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MVWp 
coup1L = cplcFuFuhhL(i1,i2,gt1)
coup1R = cplcFuFuhhR(i1,i2,gt1)
coup2L = cplcFdFucVWpL(gt2,i1)
coup2R = cplcFdFucVWpR(gt2,i1)
coup3L = cplcFuFdVWpL(i2,gt3)
coup3R = cplcFuFdVWpR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
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
End Subroutine Amplitude_VERTEX_SDdiracDM_hhTocFdFd


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTocFdFd(MAh,MFd,MFu,Mhh,MHp,               & 
& MVG,MVP,MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,              & 
& cplcFdFdAhL,cplcFdFdAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,     & 
& cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,             & 
& cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucHpL,cplcFdFucHpR,             & 
& cplcFdFucVWpL,cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,              & 
& cplhhcVWpVWp,cplhhVZVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFu(3),Mhh(2),MHp,MVG,MVP,MVWp,MVZ,MAh2,MFd2(3),MFu2(3),Mhh2(2),           & 
& MHp2,MVG2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplAhhhVZ(2),cplcFdFdhhL(3,3,2),       & 
& cplcFdFdhhR(3,3,2),cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),& 
& cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFdFdVZL(3,3),& 
& cplcFdFdVZR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFdFucHpL(3,3),              & 
& cplcFdFucHpR(3,3),cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),cplhhhhhh(2,2,2),              & 
& cplhhHpcHp(2),cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplhhVZVZ(2)

Complex(dp), Intent(out) :: Amp(2,2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,3
Amp(:,gt1, gt2, gt3) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MFd(gt2) 
Mex3 = MFd(gt3) 


! {bar[Fd], bar[Fd], VG}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopVG)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MVG 
coup1L = cplcFdFdhhL(i1,i2,gt1)
coup1R = cplcFdFdhhR(i1,i2,gt1)
coup2L = cplcFdFdVGL(gt2,i1)
coup2R = cplcFdFdVGR(gt2,i1)
coup3L = cplcFdFdVGL(i2,gt3)
coup3R = cplcFdFdVGR(i2,gt3)
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(4._dp/3._dp)*AmpC 
  End Do
End Do
End if 


! {bar[Fd], bar[Fd], VP}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopVP)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MVP 
coup1L = cplcFdFdhhL(i1,i2,gt1)
coup1R = cplcFdFdhhR(i1,i2,gt1)
coup2L = cplcFdFdVPL(gt2,i1)
coup2R = cplcFdFdVPR(gt2,i1)
coup3L = cplcFdFdVPL(i2,gt3)
coup3R = cplcFdFdVPR(i2,gt3)
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 
    End Do
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTocFdFd


Subroutine Amplitude_Tree_SDdiracDM_hhTocFeFe(cplcFeFehhL,cplcFeFehhR,MFe,            & 
& Mhh,MFe2,Mhh2,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),Mhh(2),MFe2(3),Mhh2(2)

Complex(dp), Intent(in) :: cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2)

Complex(dp) :: Amp(2,2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,3
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MFe(gt3) 
! Tree-Level Vertex 
coupT1L = cplcFeFehhL(gt2,gt3,gt1)
coupT1R = cplcFeFehhR(gt2,gt3,gt1)
Call TreeAmp_StoFF(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_hhTocFeFe


Subroutine Gamma_Real_SDdiracDM_hhTocFeFe(MLambda,em,gs,cplcFeFehhL,cplcFeFehhR,      & 
& MFe,Mhh,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2)

Real(dp), Intent(in) :: MFe(3),Mhh(2)

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2,3,3), GammarealGluon(2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
CoupL = cplcFeFehhL(i2,i3,i1)
CoupR = cplcFeFehhR(i2,i3,i1)
Mex1 = Mhh(i1)
Mex2 = MFe(i2)
Mex3 = MFe(i3)
If (Mex1.gt.(Mex2+Mex3)) Then 
 Call hardradiationSFF(Mex1,Mex2,Mex3,MLambda,em,0._dp,0._dp,0._dp,1._dp,-1._dp,1._dp,CoupL,CoupR,Gammarealphoton(i1,i2,i3),kont)
  GammarealGluon(i1,i2,i3) = 0._dp 
Else 
  GammarealGluon(i1,i2,i3) = 0._dp 
  GammarealPhoton(i1,i2,i3) = 0._dp 

End if 
    End Do
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_hhTocFeFe


Subroutine Amplitude_WAVE_SDdiracDM_hhTocFeFe(cplcFeFehhL,cplcFeFehhR,ctcplcFeFehhL,  & 
& ctcplcFeFehhR,MFe,MFe2,Mhh,Mhh2,ZfEL,ZfER,Zfhh,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFe2(3),Mhh(2),Mhh2(2)

Complex(dp), Intent(in) :: cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2)

Complex(dp), Intent(in) :: ctcplcFeFehhL(3,3,2),ctcplcFeFehhR(3,3,2)

Complex(dp), Intent(in) :: ZfEL(3,3),ZfER(3,3),Zfhh(2,2)

Complex(dp), Intent(out) :: Amp(2,2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,3
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MFe(gt3) 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFeFehhL(gt2,gt3,gt1) 
ZcoupT1R = ctcplcFeFehhR(gt2,gt3,gt1) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*Zfhh(i1,gt1)*cplcFeFehhL(gt2,gt3,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Zfhh(i1,gt1)*cplcFeFehhR(gt2,gt3,i1)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfER(i1,gt2)*cplcFeFehhL(i1,gt3,gt1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfEL(i1,gt2))*cplcFeFehhR(i1,gt3,gt1)
End Do


! External Field 3 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfEL(i1,gt3)*cplcFeFehhL(gt2,i1,gt1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfER(i1,gt3))*cplcFeFehhR(gt2,i1,gt1)
End Do


! Getting the amplitude 
Call TreeAmp_StoFF(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhTocFeFe


Subroutine Amplitude_VERTEX_SDdiracDM_hhTocFeFe(MAh,MFe,MFv,MFxe,Mhh,MHp,             & 
& MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplAhAhhh,      & 
& cplcFeFeAhL,cplcFeFeAhR,cplAhhhVZ,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,     & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,         & 
& cplcFeFxecSscL,cplcFeFxecSscR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,           & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),MFv2(3),          & 
& MFxe2,Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplAhhhVZ(2),cplcFeFehhL(3,3,2),       & 
& cplcFeFehhR(3,3,2),cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),cplcFxeFeSscL(3,2),               & 
& cplcFxeFeSscR(3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),& 
& cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),& 
& cplcFeFvcVWpR(3,3),cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplhhhhhh(2,2,2),           & 
& cplhhHpcHp(2),cplhhHpcVWp(2),cplhhSsccSsc(2,2,2),cplhhcHpVWp(2),cplhhcVWpVWp(2),       & 
& cplhhVZVZ(2)

Complex(dp), Intent(out) :: Amp(2,2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,3
Amp(:,gt1, gt2, gt3) = 0._dp 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MFe(gt3) 


! {Ah, Ah, Fe}
If ((Include_in_loopAh).and.(Include_in_loopAh).and.(Include_in_loopFe)) Then 
    Do i3=1,3
ML1 = MAh 
ML2 = MAh 
ML3 = MFe(i3) 
coup1 = cplAhAhhh(gt1)
coup2L = cplcFeFeAhL(gt2,i3)
coup2R = cplcFeFeAhR(gt2,i3)
coup3L = cplcFeFeAhL(i3,gt3)
coup3R = cplcFeFeAhR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {Ah, VZ, Fe}
If ((Include_in_loopAh).and.(Include_in_loopVZ).and.(Include_in_loopFe)) Then 
    Do i3=1,3
ML1 = MAh 
ML2 = MVZ 
ML3 = MFe(i3) 
coup1 = -cplAhhhVZ(gt1)
coup2L = cplcFeFeAhL(gt2,i3)
coup2R = cplcFeFeAhR(gt2,i3)
coup3L = cplcFeFeVZL(i3,gt3)
coup3R = cplcFeFeVZR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {hh, hh, Fe}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loopFe)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,3
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = MFe(i3) 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2L = cplcFeFehhL(gt2,i3,i1)
coup2R = cplcFeFehhR(gt2,i3,i1)
coup3L = cplcFeFehhL(i3,gt3,i2)
coup3R = cplcFeFehhR(i3,gt3,i2)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {Hp, Hp, Fv}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopFv)) Then 
    Do i3=1,3
ML1 = MHp 
ML2 = MHp 
ML3 = MFv(i3) 
coup1 = cplhhHpcHp(gt1)
coup2L = cplcFeFvcHpL(gt2,i3)
coup2R = cplcFeFvcHpR(gt2,i3)
coup3L = cplcFvFeHpL(i3,gt3)
coup3R = cplcFvFeHpR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {Hp, VWp, Fv}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopFv)) Then 
    Do i3=1,3
ML1 = MHp 
ML2 = MVWp 
ML3 = MFv(i3) 
coup1 = cplhhHpcVWp(gt1)
coup2L = cplcFeFvcHpL(gt2,i3)
coup2R = cplcFeFvcHpR(gt2,i3)
coup3L = cplcFvFeVWpL(i3,gt3)
coup3R = cplcFvFeVWpR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {Ssc, Ssc, Fxe}
If ((Include_in_loopSsc).and.(Include_in_loopSsc).and.(Include_in_loopFxe)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MSsc(i1) 
ML2 = MSsc(i2) 
ML3 = MFxe 
coup1 = cplhhSsccSsc(gt1,i1,i2)
coup2L = cplcFeFxecSscL(gt2,i1)
coup2R = cplcFeFxecSscR(gt2,i1)
coup3L = cplcFxeFeSscL(gt3,i2)
coup3R = cplcFxeFeSscR(gt3,i2)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {VWp, Hp, Fv}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopFv)) Then 
    Do i3=1,3
ML1 = MVWp 
ML2 = MHp 
ML3 = MFv(i3) 
coup1 = cplhhcHpVWp(gt1)
coup2L = cplcFeFvcVWpL(gt2,i3)
coup2R = cplcFeFvcVWpR(gt2,i3)
coup3L = cplcFvFeHpL(i3,gt3)
coup3R = cplcFvFeHpR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VWp, VWp, Fv}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopFv)) Then 
    Do i3=1,3
ML1 = MVWp 
ML2 = MVWp 
ML3 = MFv(i3) 
coup1 = cplhhcVWpVWp(gt1)
coup2L = cplcFeFvcVWpL(gt2,i3)
coup2R = cplcFeFvcVWpR(gt2,i3)
coup3L = cplcFvFeVWpL(i3,gt3)
coup3R = cplcFvFeVWpR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VZ, Ah, Fe}
If ((Include_in_loopVZ).and.(Include_in_loopAh).and.(Include_in_loopFe)) Then 
    Do i3=1,3
ML1 = MVZ 
ML2 = MAh 
ML3 = MFe(i3) 
coup1 = -cplAhhhVZ(gt1)
coup2L = cplcFeFeVZL(gt2,i3)
coup2R = cplcFeFeVZR(gt2,i3)
coup3L = cplcFeFeAhL(i3,gt3)
coup3R = cplcFeFeAhR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VZ, VZ, Fe}
If ((Include_in_loopVZ).and.(Include_in_loopVZ).and.(Include_in_loopFe)) Then 
    Do i3=1,3
ML1 = MVZ 
ML2 = MVZ 
ML3 = MFe(i3) 
coup1 = cplhhVZVZ(gt1)
coup2L = cplcFeFeVZL(gt2,i3)
coup2R = cplcFeFeVZR(gt2,i3)
coup3L = cplcFeFeVZL(i3,gt3)
coup3R = cplcFeFeVZR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {bar[Fe], bar[Fe], Ah}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopAh)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MAh 
coup1L = cplcFeFehhL(i1,i2,gt1)
coup1R = cplcFeFehhR(i1,i2,gt1)
coup2L = cplcFeFeAhL(gt2,i1)
coup2R = cplcFeFeAhR(gt2,i1)
coup3L = cplcFeFeAhL(i2,gt3)
coup3R = cplcFeFeAhR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {bar[Fe], bar[Fe], hh}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loophh)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,2
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = Mhh(i3) 
coup1L = cplcFeFehhL(i1,i2,gt1)
coup1R = cplcFeFehhR(i1,i2,gt1)
coup2L = cplcFeFehhL(gt2,i1,i3)
coup2R = cplcFeFehhR(gt2,i1,i3)
coup3L = cplcFeFehhL(i2,gt3,i3)
coup3R = cplcFeFehhR(i2,gt3,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {bar[Fe], bar[Fe], VP}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopVP)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MVP 
coup1L = cplcFeFehhL(i1,i2,gt1)
coup1R = cplcFeFehhR(i1,i2,gt1)
coup2L = cplcFeFeVPL(gt2,i1)
coup2R = cplcFeFeVPR(gt2,i1)
coup3L = cplcFeFeVPL(i2,gt3)
coup3R = cplcFeFeVPR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {bar[Fe], bar[Fe], VZ}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopVZ)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MVZ 
coup1L = cplcFeFehhL(i1,i2,gt1)
coup1R = cplcFeFehhR(i1,i2,gt1)
coup2L = cplcFeFeVZL(gt2,i1)
coup2R = cplcFeFeVZR(gt2,i1)
coup3L = cplcFeFeVZL(i2,gt3)
coup3R = cplcFeFeVZR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
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
End Subroutine Amplitude_VERTEX_SDdiracDM_hhTocFeFe


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTocFeFe(MAh,MFe,MFv,MFxe,Mhh,              & 
& MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,            & 
& cplAhAhhh,cplcFeFeAhL,cplcFeFeAhR,cplAhhhVZ,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,       & 
& cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,          & 
& cplcFvFeVWpR,cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,          & 
& cplcFeFvcVWpR,cplcFeFxecSscL,cplcFeFxecSscR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,          & 
& cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),MFv2(3),          & 
& MFxe2,Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplAhhhVZ(2),cplcFeFehhL(3,3,2),       & 
& cplcFeFehhR(3,3,2),cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),cplcFxeFeSscL(3,2),               & 
& cplcFxeFeSscR(3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),& 
& cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),& 
& cplcFeFvcVWpR(3,3),cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplhhhhhh(2,2,2),           & 
& cplhhHpcHp(2),cplhhHpcVWp(2),cplhhSsccSsc(2,2,2),cplhhcHpVWp(2),cplhhcVWpVWp(2),       & 
& cplhhVZVZ(2)

Complex(dp), Intent(out) :: Amp(2,2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,3
Amp(:,gt1, gt2, gt3) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MFe(gt3) 


! {bar[Fe], bar[Fe], VP}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopVP)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MVP 
coup1L = cplcFeFehhL(i1,i2,gt1)
coup1R = cplcFeFehhR(i1,i2,gt1)
coup2L = cplcFeFeVPL(gt2,i1)
coup2R = cplcFeFeVPR(gt2,i1)
coup3L = cplcFeFeVPL(i2,gt3)
coup3R = cplcFeFeVPR(i2,gt3)
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 
    End Do
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTocFeFe


Subroutine Amplitude_Tree_SDdiracDM_hhTocFuFu(cplcFuFuhhL,cplcFuFuhhR,MFu,            & 
& Mhh,MFu2,Mhh2,Amp)

Implicit None

Real(dp), Intent(in) :: MFu(3),Mhh(2),MFu2(3),Mhh2(2)

Complex(dp), Intent(in) :: cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2)

Complex(dp) :: Amp(2,2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,3
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MFu(gt3) 
! Tree-Level Vertex 
coupT1L = cplcFuFuhhL(gt2,gt3,gt1)
coupT1R = cplcFuFuhhR(gt2,gt3,gt1)
Call TreeAmp_StoFF(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_hhTocFuFu


Subroutine Gamma_Real_SDdiracDM_hhTocFuFu(MLambda,em,gs,cplcFuFuhhL,cplcFuFuhhR,      & 
& MFu,Mhh,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2)

Real(dp), Intent(in) :: MFu(3),Mhh(2)

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2,3,3), GammarealGluon(2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
CoupL = cplcFuFuhhL(i2,i3,i1)
CoupR = cplcFuFuhhR(i2,i3,i1)
Mex1 = Mhh(i1)
Mex2 = MFu(i2)
Mex3 = MFu(i3)
If (Mex1.gt.(Mex2+Mex3)) Then 
 Call hardradiationSFF(Mex1,Mex2,Mex3,MLambda,em,0._dp,0._dp,0._dp,4._dp/3._dp,-4._dp/3._dp,4._dp/3._dp,CoupL,CoupR,Gammarealphoton(i1,i2,i3),kont)
 Call hardradiationSFF(Mex1,Mex2,Mex3,MLambda,gs,0._dp,0._dp,0._dp,4._dp,-4._dp,4._dp,CoupL,CoupR,Gammarealgluon(i1,i2,i3),kont)
Else 
  GammarealGluon(i1,i2,i3) = 0._dp 
  GammarealPhoton(i1,i2,i3) = 0._dp 

End if 
    End Do
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_hhTocFuFu


Subroutine Amplitude_WAVE_SDdiracDM_hhTocFuFu(cplcFuFuhhL,cplcFuFuhhR,ctcplcFuFuhhL,  & 
& ctcplcFuFuhhR,MFu,MFu2,Mhh,Mhh2,Zfhh,ZfUL,ZfUR,Amp)

Implicit None

Real(dp), Intent(in) :: MFu(3),MFu2(3),Mhh(2),Mhh2(2)

Complex(dp), Intent(in) :: cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2)

Complex(dp), Intent(in) :: ctcplcFuFuhhL(3,3,2),ctcplcFuFuhhR(3,3,2)

Complex(dp), Intent(in) :: Zfhh(2,2),ZfUL(3,3),ZfUR(3,3)

Complex(dp), Intent(out) :: Amp(2,2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,3
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MFu(gt3) 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFuFuhhL(gt2,gt3,gt1) 
ZcoupT1R = ctcplcFuFuhhR(gt2,gt3,gt1) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*Zfhh(i1,gt1)*cplcFuFuhhL(gt2,gt3,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Zfhh(i1,gt1)*cplcFuFuhhR(gt2,gt3,i1)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfUR(i1,gt2)*cplcFuFuhhL(i1,gt3,gt1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfUL(i1,gt2))*cplcFuFuhhR(i1,gt3,gt1)
End Do


! External Field 3 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfUL(i1,gt3)*cplcFuFuhhL(gt2,i1,gt1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfUR(i1,gt3))*cplcFuFuhhR(gt2,i1,gt1)
End Do


! Getting the amplitude 
Call TreeAmp_StoFF(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhTocFuFu


Subroutine Amplitude_VERTEX_SDdiracDM_hhTocFuFu(MAh,MFd,MFu,Mhh,MHp,MVG,              & 
& MVP,MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFuFuAhL,      & 
& cplcFuFuAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,    & 
& cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,              & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,               & 
& cplhhVZVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFu(3),Mhh(2),MHp,MVG,MVP,MVWp,MVZ,MAh2,MFd2(3),MFu2(3),Mhh2(2),           & 
& MHp2,MVG2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplAhhhVZ(2),cplcFdFdhhL(3,3,2),       & 
& cplcFdFdhhR(3,3,2),cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),& 
& cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),               & 
& cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFdFucHpL(3,3), & 
& cplcFdFucHpR(3,3),cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),cplhhhhhh(2,2,2),              & 
& cplhhHpcHp(2),cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplhhVZVZ(2)

Complex(dp), Intent(out) :: Amp(2,2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,3
Amp(:,gt1, gt2, gt3) = 0._dp 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MFu(gt3) 


! {Ah, Ah, Fu}
If ((Include_in_loopAh).and.(Include_in_loopAh).and.(Include_in_loopFu)) Then 
    Do i3=1,3
ML1 = MAh 
ML2 = MAh 
ML3 = MFu(i3) 
coup1 = cplAhAhhh(gt1)
coup2L = cplcFuFuAhL(gt2,i3)
coup2R = cplcFuFuAhR(gt2,i3)
coup3L = cplcFuFuAhL(i3,gt3)
coup3R = cplcFuFuAhR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {Ah, VZ, Fu}
If ((Include_in_loopAh).and.(Include_in_loopVZ).and.(Include_in_loopFu)) Then 
    Do i3=1,3
ML1 = MAh 
ML2 = MVZ 
ML3 = MFu(i3) 
coup1 = -cplAhhhVZ(gt1)
coup2L = cplcFuFuAhL(gt2,i3)
coup2R = cplcFuFuAhR(gt2,i3)
coup3L = cplcFuFuVZL(i3,gt3)
coup3R = cplcFuFuVZR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {hh, hh, Fu}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loopFu)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,3
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = MFu(i3) 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2L = cplcFuFuhhL(gt2,i3,i1)
coup2R = cplcFuFuhhR(gt2,i3,i1)
coup3L = cplcFuFuhhL(i3,gt3,i2)
coup3R = cplcFuFuhhR(i3,gt3,i2)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {VZ, Ah, Fu}
If ((Include_in_loopVZ).and.(Include_in_loopAh).and.(Include_in_loopFu)) Then 
    Do i3=1,3
ML1 = MVZ 
ML2 = MAh 
ML3 = MFu(i3) 
coup1 = -cplAhhhVZ(gt1)
coup2L = cplcFuFuVZL(gt2,i3)
coup2R = cplcFuFuVZR(gt2,i3)
coup3L = cplcFuFuAhL(i3,gt3)
coup3R = cplcFuFuAhR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VZ, VZ, Fu}
If ((Include_in_loopVZ).and.(Include_in_loopVZ).and.(Include_in_loopFu)) Then 
    Do i3=1,3
ML1 = MVZ 
ML2 = MVZ 
ML3 = MFu(i3) 
coup1 = cplhhVZVZ(gt1)
coup2L = cplcFuFuVZL(gt2,i3)
coup2R = cplcFuFuVZR(gt2,i3)
coup3L = cplcFuFuVZL(i3,gt3)
coup3R = cplcFuFuVZR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {bar[Fd], bar[Fd], Hp}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopHp)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MHp 
coup1L = cplcFdFdhhL(i1,i2,gt1)
coup1R = cplcFdFdhhR(i1,i2,gt1)
coup2L = cplcFuFdHpL(gt2,i1)
coup2R = cplcFuFdHpR(gt2,i1)
coup3L = cplcFdFucHpL(i2,gt3)
coup3R = cplcFdFucHpR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {bar[Fd], bar[Fd], VWp}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopVWp)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MVWp 
coup1L = cplcFdFdhhL(i1,i2,gt1)
coup1R = cplcFdFdhhR(i1,i2,gt1)
coup2L = cplcFuFdVWpL(gt2,i1)
coup2R = cplcFuFdVWpR(gt2,i1)
coup3L = cplcFdFucVWpL(i2,gt3)
coup3R = cplcFdFucVWpR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {bar[Fu], bar[Fu], Ah}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopAh)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MAh 
coup1L = cplcFuFuhhL(i1,i2,gt1)
coup1R = cplcFuFuhhR(i1,i2,gt1)
coup2L = cplcFuFuAhL(gt2,i1)
coup2R = cplcFuFuAhR(gt2,i1)
coup3L = cplcFuFuAhL(i2,gt3)
coup3R = cplcFuFuAhR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {bar[Fu], bar[Fu], hh}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loophh)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,2
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = Mhh(i3) 
coup1L = cplcFuFuhhL(i1,i2,gt1)
coup1R = cplcFuFuhhR(i1,i2,gt1)
coup2L = cplcFuFuhhL(gt2,i1,i3)
coup2R = cplcFuFuhhR(gt2,i1,i3)
coup3L = cplcFuFuhhL(i2,gt3,i3)
coup3R = cplcFuFuhhR(i2,gt3,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {bar[Fu], bar[Fu], VG}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopVG)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MVG 
coup1L = cplcFuFuhhL(i1,i2,gt1)
coup1R = cplcFuFuhhR(i1,i2,gt1)
coup2L = cplcFuFuVGL(gt2,i1)
coup2R = cplcFuFuVGR(gt2,i1)
coup3L = cplcFuFuVGL(i2,gt3)
coup3R = cplcFuFuVGR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(4._dp/3._dp)*AmpC 
  End Do
End Do
End if 


! {bar[Fu], bar[Fu], VP}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopVP)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MVP 
coup1L = cplcFuFuhhL(i1,i2,gt1)
coup1R = cplcFuFuhhR(i1,i2,gt1)
coup2L = cplcFuFuVPL(gt2,i1)
coup2R = cplcFuFuVPR(gt2,i1)
coup3L = cplcFuFuVPL(i2,gt3)
coup3R = cplcFuFuVPR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {bar[Fu], bar[Fu], VZ}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopVZ)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MVZ 
coup1L = cplcFuFuhhL(i1,i2,gt1)
coup1R = cplcFuFuhhR(i1,i2,gt1)
coup2L = cplcFuFuVZL(gt2,i1)
coup2R = cplcFuFuVZR(gt2,i1)
coup3L = cplcFuFuVZL(i2,gt3)
coup3R = cplcFuFuVZR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {conj[Hp], conj[Hp], Fd}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopFd)) Then 
    Do i3=1,3
ML1 = MHp 
ML2 = MHp 
ML3 = MFd(i3) 
coup1 = cplhhHpcHp(gt1)
coup2L = cplcFuFdHpL(gt2,i3)
coup2R = cplcFuFdHpR(gt2,i3)
coup3L = cplcFdFucHpL(i3,gt3)
coup3R = cplcFdFucHpR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[Hp], conj[VWp], Fd}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopFd)) Then 
    Do i3=1,3
ML1 = MHp 
ML2 = MVWp 
ML3 = MFd(i3) 
coup1 = cplhhcHpVWp(gt1)
coup2L = cplcFuFdHpL(gt2,i3)
coup2R = cplcFuFdHpR(gt2,i3)
coup3L = cplcFdFucVWpL(i3,gt3)
coup3R = cplcFdFucVWpR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[VWp], conj[Hp], Fd}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopFd)) Then 
    Do i3=1,3
ML1 = MVWp 
ML2 = MHp 
ML3 = MFd(i3) 
coup1 = cplhhHpcVWp(gt1)
coup2L = cplcFuFdVWpL(gt2,i3)
coup2R = cplcFuFdVWpR(gt2,i3)
coup3L = cplcFdFucHpL(i3,gt3)
coup3R = cplcFdFucHpR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[VWp], conj[VWp], Fd}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopFd)) Then 
    Do i3=1,3
ML1 = MVWp 
ML2 = MVWp 
ML3 = MFd(i3) 
coup1 = cplhhcVWpVWp(gt1)
coup2L = cplcFuFdVWpL(gt2,i3)
coup2R = cplcFuFdVWpR(gt2,i3)
coup3L = cplcFdFucVWpL(i3,gt3)
coup3R = cplcFdFucVWpR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 
    End Do
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhTocFuFu


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTocFuFu(MAh,MFd,MFu,Mhh,MHp,               & 
& MVG,MVP,MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,              & 
& cplcFuFuAhL,cplcFuFuAhR,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,     & 
& cplcFuFdVWpL,cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,             & 
& cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,             & 
& cplcFdFucVWpL,cplcFdFucVWpR,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,              & 
& cplhhcVWpVWp,cplhhVZVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFu(3),Mhh(2),MHp,MVG,MVP,MVWp,MVZ,MAh2,MFd2(3),MFu2(3),Mhh2(2),           & 
& MHp2,MVG2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplAhhhVZ(2),cplcFdFdhhL(3,3,2),       & 
& cplcFdFdhhR(3,3,2),cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),& 
& cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),               & 
& cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFdFucHpL(3,3), & 
& cplcFdFucHpR(3,3),cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),cplhhhhhh(2,2,2),              & 
& cplhhHpcHp(2),cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplhhVZVZ(2)

Complex(dp), Intent(out) :: Amp(2,2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,3
Amp(:,gt1, gt2, gt3) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MFu(gt3) 


! {bar[Fu], bar[Fu], VG}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopVG)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MVG 
coup1L = cplcFuFuhhL(i1,i2,gt1)
coup1R = cplcFuFuhhR(i1,i2,gt1)
coup2L = cplcFuFuVGL(gt2,i1)
coup2R = cplcFuFuVGR(gt2,i1)
coup3L = cplcFuFuVGL(i2,gt3)
coup3R = cplcFuFuVGR(i2,gt3)
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(4._dp/3._dp)*AmpC 
  End Do
End Do
End if 


! {bar[Fu], bar[Fu], VP}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopVP)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MVP 
coup1L = cplcFuFuhhL(i1,i2,gt1)
coup1R = cplcFuFuhhR(i1,i2,gt1)
coup2L = cplcFuFuVPL(gt2,i1)
coup2R = cplcFuFuVPR(gt2,i1)
coup3L = cplcFuFuVPL(i2,gt3)
coup3R = cplcFuFuVPR(i2,gt3)
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 
    End Do
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTocFuFu


Subroutine Amplitude_Tree_SDdiracDM_hhTocFxvFxv(cplcFxvFxvhhL,cplcFxvFxvhhR,          & 
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
Mex1 = Mhh(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = MFxv(gt3) 
! Tree-Level Vertex 
coupT1L = cplcFxvFxvhhL(gt2,gt3,gt1)
coupT1R = cplcFxvFxvhhR(gt2,gt3,gt1)
Call TreeAmp_StoFF(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_hhTocFxvFxv


Subroutine Gamma_Real_SDdiracDM_hhTocFxvFxv(MLambda,em,gs,cplcFxvFxvhhL,              & 
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
CoupL = cplcFxvFxvhhL(i2,i3,i1)
CoupR = cplcFxvFxvhhR(i2,i3,i1)
Mex1 = Mhh(i1)
Mex2 = MFxv(i2)
Mex3 = MFxv(i3)
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
End Subroutine Gamma_Real_SDdiracDM_hhTocFxvFxv


Subroutine Amplitude_WAVE_SDdiracDM_hhTocFxvFxv(cplcFxvFxvhhL,cplcFxvFxvhhR,          & 
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
Mex1 = Mhh(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = MFxv(gt3) 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFxvFxvhhL(gt2,gt3,gt1) 
ZcoupT1R = ctcplcFxvFxvhhR(gt2,gt3,gt1) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*Zfhh(i1,gt1)*cplcFxvFxvhhL(gt2,gt3,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Zfhh(i1,gt1)*cplcFxvFxvhhR(gt2,gt3,i1)
End Do


! External Field 2 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfxVR(i1,gt2)*cplcFxvFxvhhL(i1,gt3,gt1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfxVL(i1,gt2))*cplcFxvFxvhhR(i1,gt3,gt1)
End Do


! External Field 3 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfxVL(i1,gt3)*cplcFxvFxvhhL(gt2,i1,gt1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfxVR(i1,gt3))*cplcFxvFxvhhR(gt2,i1,gt1)
End Do


! Getting the amplitude 
Call TreeAmp_StoFF(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhTocFxvFxv


Subroutine Amplitude_VERTEX_SDdiracDM_hhTocFxvFxv(MAh,MFv,MFxe,MFxv,Mhh,              & 
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
Mex1 = Mhh(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = MFxv(gt3) 


! {Ah, Ah, Fxv}
If ((Include_in_loopAh).and.(Include_in_loopAh).and.(Include_in_loopFxv)) Then 
    Do i3=1,2
ML1 = MAh 
ML2 = MAh 
ML3 = MFxv(i3) 
coup1 = cplAhAhhh(gt1)
coup2L = cplcFxvFxvAhL(gt2,i3)
coup2R = cplcFxvFxvAhR(gt2,i3)
coup3L = cplcFxvFxvAhL(i3,gt3)
coup3R = cplcFxvFxvAhR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {Ah, VZ, Fxv}
If ((Include_in_loopAh).and.(Include_in_loopVZ).and.(Include_in_loopFxv)) Then 
    Do i3=1,2
ML1 = MAh 
ML2 = MVZ 
ML3 = MFxv(i3) 
coup1 = -cplAhhhVZ(gt1)
coup2L = cplcFxvFxvAhL(gt2,i3)
coup2R = cplcFxvFxvAhR(gt2,i3)
coup3L = cplcFxvFxvVZL(i3,gt3)
coup3R = cplcFxvFxvVZR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {hh, hh, Fxv}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loopFxv)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = MFxv(i3) 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2L = cplcFxvFxvhhL(gt2,i3,i1)
coup2R = cplcFxvFxvhhR(gt2,i3,i1)
coup3L = cplcFxvFxvhhL(i3,gt3,i2)
coup3R = cplcFxvFxvhhR(i3,gt3,i2)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {VZ, Ah, Fxv}
If ((Include_in_loopVZ).and.(Include_in_loopAh).and.(Include_in_loopFxv)) Then 
    Do i3=1,2
ML1 = MVZ 
ML2 = MAh 
ML3 = MFxv(i3) 
coup1 = -cplAhhhVZ(gt1)
coup2L = cplcFxvFxvVZL(gt2,i3)
coup2R = cplcFxvFxvVZR(gt2,i3)
coup3L = cplcFxvFxvAhL(i3,gt3)
coup3R = cplcFxvFxvAhR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VZ, VZ, Fxv}
If ((Include_in_loopVZ).and.(Include_in_loopVZ).and.(Include_in_loopFxv)) Then 
    Do i3=1,2
ML1 = MVZ 
ML2 = MVZ 
ML3 = MFxv(i3) 
coup1 = cplhhVZVZ(gt1)
coup2L = cplcFxvFxvVZL(gt2,i3)
coup2R = cplcFxvFxvVZR(gt2,i3)
coup3L = cplcFxvFxvVZL(i3,gt3)
coup3R = cplcFxvFxvVZR(i3,gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {bar[Fxv], bar[Fxv], Ah}
If ((Include_in_loopFxv).and.(Include_in_loopFxv).and.(Include_in_loopAh)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MFxv(i1) 
ML2 = MFxv(i2) 
ML3 = MAh 
coup1L = cplcFxvFxvhhL(i1,i2,gt1)
coup1R = cplcFxvFxvhhR(i1,i2,gt1)
coup2L = cplcFxvFxvAhL(gt2,i1)
coup2R = cplcFxvFxvAhR(gt2,i1)
coup3L = cplcFxvFxvAhL(i2,gt3)
coup3R = cplcFxvFxvAhR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {bar[Fxv], bar[Fxv], hh}
If ((Include_in_loopFxv).and.(Include_in_loopFxv).and.(Include_in_loophh)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = MFxv(i2) 
ML3 = Mhh(i3) 
coup1L = cplcFxvFxvhhL(i1,i2,gt1)
coup1R = cplcFxvFxvhhR(i1,i2,gt1)
coup2L = cplcFxvFxvhhL(gt2,i1,i3)
coup2R = cplcFxvFxvhhR(gt2,i1,i3)
coup3L = cplcFxvFxvhhL(i2,gt3,i3)
coup3R = cplcFxvFxvhhR(i2,gt3,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {bar[Fxv], bar[Fxv], VZ}
If ((Include_in_loopFxv).and.(Include_in_loopFxv).and.(Include_in_loopVZ)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MFxv(i1) 
ML2 = MFxv(i2) 
ML3 = MVZ 
coup1L = cplcFxvFxvhhL(i1,i2,gt1)
coup1R = cplcFxvFxvhhR(i1,i2,gt1)
coup2L = cplcFxvFxvVZL(gt2,i1)
coup2R = cplcFxvFxvVZR(gt2,i1)
coup3L = cplcFxvFxvVZL(i2,gt3)
coup3R = cplcFxvFxvVZR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {conj[Hp], conj[Hp], Fxe}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopFxe)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MFxe 
coup1 = cplhhHpcHp(gt1)
coup2L = cplcFxvFxeHpL(gt2)
coup2R = cplcFxvFxeHpR(gt2)
coup3L = cplcFxeFxvcHpL(gt3)
coup3R = cplcFxeFxvcHpR(gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[VWp], Fxe}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopFxe)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MFxe 
coup1 = cplhhcHpVWp(gt1)
coup2L = cplcFxvFxeHpL(gt2)
coup2R = cplcFxvFxeHpR(gt2)
coup3L = cplcFxeFxvcVWpL(gt3)
coup3R = cplcFxeFxvcVWpR(gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {conj[Ssc], conj[Ssc], Fv}
If ((Include_in_loopSsc).and.(Include_in_loopSsc).and.(Include_in_loopFv)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,3
ML1 = MSsc(i1) 
ML2 = MSsc(i2) 
ML3 = MFv(i3) 
coup1 = cplhhSsccSsc(gt1,i2,i1)
coup2L = cplcFxvFvSscL(gt2,i3,i1)
coup2R = cplcFxvFvSscR(gt2,i3,i1)
coup3L = cplcFvFxvcSscL(i3,gt3,i2)
coup3R = cplcFvFxvcSscR(i3,gt3,i2)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {conj[VWp], conj[Hp], Fxe}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopFxe)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MFxe 
coup1 = cplhhHpcVWp(gt1)
coup2L = cplcFxvFxeVWpL(gt2)
coup2R = cplcFxvFxeVWpR(gt2)
coup3L = cplcFxeFxvcHpL(gt3)
coup3R = cplcFxeFxvcHpR(gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], Fxe}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopFxe)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MFxe 
coup1 = cplhhcVWpVWp(gt1)
coup2L = cplcFxvFxeVWpL(gt2)
coup2R = cplcFxvFxeVWpR(gt2)
coup3L = cplcFxeFxvcVWpL(gt3)
coup3R = cplcFxeFxvcVWpR(gt3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 
    End Do
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhTocFxvFxv


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTocFxvFxv(MAh,MFv,MFxe,MFxv,               & 
& Mhh,MHp,MSsc,MVWp,MVZ,MAh2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplAhAhhh,      & 
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
Mex1 = Mhh(gt1) 
Mex2 = MFxv(gt2) 
Mex3 = MFxv(gt3) 
    End Do
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTocFxvFxv


Subroutine Amplitude_Tree_SDdiracDM_hhTohhhh(cplhhhhhh,Mhh,Mhh2,Amp)

Implicit None

Real(dp), Intent(in) :: Mhh(2),Mhh2(2)

Complex(dp), Intent(in) :: cplhhhhhh(2,2,2)

Complex(dp) :: Amp(2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
  Do gt2=1,2
    Do gt3=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = Mhh(gt2) 
Mex3 = Mhh(gt3) 
! Tree-Level Vertex 
coupT1 = cplhhhhhh(gt1,gt2,gt3)
Call TreeAmp_StoSS(Mex1,Mex2,Mex3,coupT1,AmpC) 
! Colour and symmetry factor 
Amp(gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_hhTohhhh


Subroutine Gamma_Real_SDdiracDM_hhTohhhh(MLambda,em,gs,cplhhhhhh,Mhh,GammarealPhoton, & 
& GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplhhhhhh(2,2,2)

Real(dp), Intent(in) :: Mhh(2)

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2,2,2), GammarealGluon(2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: Coup 
 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
Coup = cplhhhhhh(i1,i2,i3)
Mex1 = Mhh(i1)
Mex2 = Mhh(i2)
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
End Subroutine Gamma_Real_SDdiracDM_hhTohhhh


Subroutine Amplitude_WAVE_SDdiracDM_hhTohhhh(cplhhhhhh,ctcplhhhhhh,Mhh,               & 
& Mhh2,Zfhh,Amp)

Implicit None

Real(dp), Intent(in) :: Mhh(2),Mhh2(2)

Complex(dp), Intent(in) :: cplhhhhhh(2,2,2)

Complex(dp), Intent(in) :: ctcplhhhhhh(2,2,2)

Complex(dp), Intent(in) :: Zfhh(2,2)

Complex(dp), Intent(out) :: Amp(2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
  Do gt2=1,2
    Do gt3=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = Mhh(gt2) 
Mex3 = Mhh(gt3) 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1 = ctcplhhhhhh(gt1,gt2,gt3) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1 = ZcoupT1 + 0.5_dp*Zfhh(i1,gt1)*cplhhhhhh(i1,gt2,gt3)
End Do


! External Field 2 
Do i1=1,2
ZcoupT1 = ZcoupT1 + 0.5_dp*Zfhh(i1,gt2)*cplhhhhhh(gt1,i1,gt3)
End Do


! External Field 3 
Do i1=1,2
ZcoupT1 = ZcoupT1 + 0.5_dp*Zfhh(i1,gt3)*cplhhhhhh(gt1,gt2,i1)
End Do


! Getting the amplitude 
Call TreeAmp_StoSS(Mex1,Mex2,Mex3,ZcoupT1,AmpC) 
Amp(gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhTohhhh


Subroutine Amplitude_VERTEX_SDdiracDM_hhTohhhh(MAh,MFd,MFe,MFu,MFxv,Mhh,              & 
& MHp,MSsc,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplAhAhhh,      & 
& cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,     & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcgWpgWphh,cplcgWCgWChh,cplcgZgZhh,cplhhhhhh,            & 
& cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,cplAhAhhhhh1,   & 
& cplhhhhhhhh1,cplhhhhHpcHp1,cplhhhhSsccSsc1,cplhhhhcVWpVWp1,cplhhhhVZVZ1,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),MFxv(2),Mhh(2),MHp,MSsc(2),MVWp,MVZ,MAh2,MFd2(3),            & 
& MFe2(3),MFu2(3),MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplAhhhVZ(2),cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFeFehhL(3,3,2),   & 
& cplcFeFehhR(3,3,2),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFxvFxvhhL(2,2,2),         & 
& cplcFxvFxvhhR(2,2,2),cplcgWpgWphh(2),cplcgWCgWChh(2),cplcgZgZhh(2),cplhhhhhh(2,2,2),   & 
& cplhhHpcHp(2),cplhhHpcVWp(2),cplhhSsccSsc(2,2,2),cplhhcHpVWp(2),cplhhcVWpVWp(2),       & 
& cplhhVZVZ(2),cplAhAhhhhh1(2,2),cplhhhhhhhh1(2,2,2,2),cplhhhhHpcHp1(2,2),               & 
& cplhhhhSsccSsc1(2,2,2,2),cplhhhhcVWpVWp1(2,2),cplhhhhVZVZ1(2,2)

Complex(dp), Intent(out) :: Amp(2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,2
    Do gt3=1,2
Amp(gt1, gt2, gt3) = 0._dp 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = Mhh(gt2) 
Mex3 = Mhh(gt3) 


! {Ah, Ah, Ah}
If ((Include_in_loopAh).and.(Include_in_loopAh).and.(Include_in_loopAh)) Then 
ML1 = MAh 
ML2 = MAh 
ML3 = MAh 
coup1 = cplAhAhhh(gt1)
coup2 = cplAhAhhh(gt2)
coup3 = cplAhAhhh(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {Ah, VZ, Ah}
If ((Include_in_loopAh).and.(Include_in_loopVZ).and.(Include_in_loopAh)) Then 
ML1 = MAh 
ML2 = MVZ 
ML3 = MAh 
coup1 = cplAhhhVZ(gt1)
coup2 = cplAhAhhh(gt2)
coup3 = -cplAhhhVZ(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {Ah, Ah, VZ}
If ((Include_in_loopAh).and.(Include_in_loopAh).and.(Include_in_loopVZ)) Then 
ML1 = MAh 
ML2 = MAh 
ML3 = MVZ 
coup1 = cplAhAhhh(gt1)
coup2 = cplAhhhVZ(gt2)
coup3 = -cplAhhhVZ(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {Ah, VZ, VZ}
If ((Include_in_loopAh).and.(Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
ML1 = MAh 
ML2 = MVZ 
ML3 = MVZ 
coup1 = cplAhhhVZ(gt1)
coup2 = cplAhhhVZ(gt2)
coup3 = cplhhVZVZ(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {Fd, Fd, Fd}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFdFdhhL(i2,i1,gt1)
coup1R = cplcFdFdhhR(i2,i1,gt1)
coup2L = cplcFdFdhhL(i1,i3,gt2)
coup2R = cplcFdFdhhR(i1,i3,gt2)
coup3L = cplcFdFdhhL(i3,i2,gt3)
coup3R = cplcFdFdhhR(i3,i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fe, Fe, Fe}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFehhL(i2,i1,gt1)
coup1R = cplcFeFehhR(i2,i1,gt1)
coup2L = cplcFeFehhL(i1,i3,gt2)
coup2R = cplcFeFehhR(i1,i3,gt2)
coup3L = cplcFeFehhL(i3,i2,gt3)
coup3R = cplcFeFehhR(i3,i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(2)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fu, Fu, Fu}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuhhL(i2,i1,gt1)
coup1R = cplcFuFuhhR(i2,i1,gt1)
coup2L = cplcFuFuhhL(i1,i3,gt2)
coup2R = cplcFuFuhhR(i1,i3,gt2)
coup3L = cplcFuFuhhL(i3,i2,gt3)
coup3R = cplcFuFuhhR(i3,i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fxv, Fxv, Fxv}
If ((Include_in_loopFxv).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxvFxvhhL(i2,i1,gt1)
coup1R = cplcFxvFxvhhR(i2,i1,gt1)
coup2L = cplcFxvFxvhhL(i1,i3,gt2)
coup2R = cplcFxvFxvhhR(i1,i3,gt2)
coup3L = cplcFxvFxvhhL(i3,i2,gt3)
coup3R = cplcFxvFxvhhR(i3,i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(2)*AmpC 
    End Do
  End Do
End Do
End if 


! {gWp, gWp, gWp}
If ((Include_in_loopgWp).and.(Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWpgWphh(gt1)
coup2 = cplcgWpgWphh(gt2)
coup3 = cplcgWpgWphh(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(2)*AmpC 
End if 


! {gWpC, gWpC, gWpC}
If ((Include_in_loopgWC).and.(Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWCgWChh(gt1)
coup2 = cplcgWCgWChh(gt2)
coup3 = cplcgWCgWChh(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(2)*AmpC 
End if 


! {gZ, gZ, gZ}
If ((Include_in_loopgZ).and.(Include_in_loopgZ).and.(Include_in_loopgZ)) Then 
ML1 = MVZ 
ML2 = MVZ 
ML3 = MVZ 
coup1 = cplcgZgZhh(gt1)
coup2 = cplcgZgZhh(gt2)
coup3 = cplcgZgZhh(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(2)*AmpC 
End if 


! {hh, hh, hh}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loophh)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = Mhh(i3) 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = cplhhhhhh(gt2,i1,i3)
coup3 = cplhhhhhh(gt3,i3,i2)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {Hp, Hp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MHp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplhhHpcHp(gt2)
coup3 = cplhhHpcHp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(2)*AmpC 
End if 


! {Hp, VWp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MHp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplhhHpcHp(gt2)
coup3 = cplhhcHpVWp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {Hp, Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = -cplhhcHpVWp(gt2)
coup3 = cplhhHpcVWp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = -cplhhcHpVWp(gt2)
coup3 = cplhhcVWpVWp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {Ssc, Ssc, Ssc}
If ((Include_in_loopSsc).and.(Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = MSsc(i1) 
ML2 = MSsc(i2) 
ML3 = MSsc(i3) 
coup1 = cplhhSsccSsc(gt1,i1,i2)
coup2 = cplhhSsccSsc(gt2,i3,i1)
coup3 = cplhhSsccSsc(gt3,i2,i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(2)*AmpC 
    End Do
  End Do
End Do
End if 


! {VWp, Hp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = -cplhhHpcVWp(gt2)
coup3 = cplhhHpcHp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = -cplhhHpcVWp(gt2)
coup3 = cplhhcHpVWp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVWp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplhhcVWpVWp(gt2)
coup3 = cplhhHpcVWp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplhhcVWpVWp(gt2)
coup3 = cplhhcVWpVWp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(2)*AmpC 
End if 


! {VZ, Ah, Ah}
If ((Include_in_loopVZ).and.(Include_in_loopAh).and.(Include_in_loopAh)) Then 
ML1 = MVZ 
ML2 = MAh 
ML3 = MAh 
coup1 = cplAhhhVZ(gt1)
coup2 = cplAhhhVZ(gt2)
coup3 = cplAhAhhh(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {VZ, VZ, Ah}
If ((Include_in_loopVZ).and.(Include_in_loopVZ).and.(Include_in_loopAh)) Then 
ML1 = MVZ 
ML2 = MVZ 
ML3 = MAh 
coup1 = cplhhVZVZ(gt1)
coup2 = cplAhhhVZ(gt2)
coup3 = -cplAhhhVZ(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {VZ, Ah, VZ}
If ((Include_in_loopVZ).and.(Include_in_loopAh).and.(Include_in_loopVZ)) Then 
ML1 = MVZ 
ML2 = MAh 
ML3 = MVZ 
coup1 = cplAhhhVZ(gt1)
coup2 = cplhhVZVZ(gt2)
coup3 = -cplAhhhVZ(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {VZ, VZ, VZ}
If ((Include_in_loopVZ).and.(Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
ML1 = MVZ 
ML2 = MVZ 
ML3 = MVZ 
coup1 = cplhhVZVZ(gt1)
coup2 = cplhhVZVZ(gt2)
coup3 = cplhhVZVZ(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[VWp], conj[Hp]}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MHp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplhhHpcHp(gt2)
coup3 = cplhhHpcVWp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[Hp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = -cplhhHpcVWp(gt2)
coup3 = cplhhcHpVWp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[VWp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = -cplhhHpcVWp(gt2)
coup3 = cplhhcVWpVWp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = -cplhhcHpVWp(gt2)
coup3 = cplhhHpcHp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = -cplhhcHpVWp(gt2)
coup3 = cplhhHpcVWp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], conj[VWp]}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVWp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplhhcVWpVWp(gt2)
coup3 = cplhhcHpVWp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {Ah, Ah}
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
ML1 = MAh 
ML2 = MAh 
coup1 = cplAhAhhh(gt1)
coup2 = cplAhAhhhhh1(gt2,gt3)
Call Amp_VERTEX_StoSS_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1._dp/2._dp)*AmpC 
End if 


! {hh, hh}
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = cplhhhhhhhh1(gt2,gt3,i1,i2)
Call Amp_VERTEX_StoSS_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1._dp/2._dp)*AmpC 
  End Do
End Do
End if 


! {Hp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MHp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplhhhhHpcHp1(gt2,gt3)
Call Amp_VERTEX_StoSS_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {Ssc, Ssc}
If ((Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MSsc(i1) 
ML2 = MSsc(i2) 
coup1 = cplhhSsccSsc(gt1,i1,i2)
coup2 = cplhhhhSsccSsc1(gt2,gt3,i2,i1)
Call Amp_VERTEX_StoSS_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {VWp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplhhhhcVWpVWp1(gt2,gt3)
Call Amp_VERTEX_StoSS_Topology2_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {VZ, VZ}
If ((Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
ML1 = MVZ 
ML2 = MVZ 
coup1 = cplhhVZVZ(gt1)
coup2 = cplhhhhVZVZ1(gt2,gt3)
Call Amp_VERTEX_StoSS_Topology2_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1._dp/2._dp)*AmpC 
End if 


! {Ah, Ah}
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
ML1 = MAh 
ML2 = MAh 
coup1 = cplAhAhhhhh1(gt1,gt3)
coup2 = cplAhAhhh(gt2)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology3_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1._dp/2._dp)*AmpC 

End if 


! {hh, hh}
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
coup1 = cplhhhhhhhh1(gt1,gt3,i1,i2)
coup2 = cplhhhhhh(gt2,i1,i2)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology3_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1._dp/2._dp)*AmpC 

  End Do
End Do
End if 


! {Hp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MHp 
coup1 = cplhhhhHpcHp1(gt1,gt3)
coup2 = cplhhHpcHp(gt2)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology3_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 

End if 


! {Ssc, Ssc}
If ((Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MSsc(i1) 
ML2 = MSsc(i2) 
coup1 = cplhhhhSsccSsc1(gt1,gt3,i1,i2)
coup2 = cplhhSsccSsc(gt2,i2,i1)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology3_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 

  End Do
End Do
End if 


! {VWp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
coup1 = cplhhhhcVWpVWp1(gt1,gt3)
coup2 = cplhhcVWpVWp(gt2)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology3_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 

End if 


! {VZ, VZ}
If ((Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
ML1 = MVZ 
ML2 = MVZ 
coup1 = cplhhhhVZVZ1(gt1,gt3)
coup2 = cplhhVZVZ(gt2)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology3_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1._dp/2._dp)*AmpC 

End if 


! {Ah, Ah}
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
ML1 = MAh 
ML2 = MAh 
coup1 = cplAhAhhhhh1(gt1,gt2)
coup2 = cplAhAhhh(gt3)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology4_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1._dp/2._dp)*AmpC 

End if 


! {hh, hh}
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
coup1 = cplhhhhhhhh1(gt1,gt2,i1,i2)
coup2 = cplhhhhhh(gt3,i1,i2)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology4_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1._dp/2._dp)*AmpC 

  End Do
End Do
End if 


! {Hp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MHp 
coup1 = cplhhhhHpcHp1(gt1,gt2)
coup2 = cplhhHpcHp(gt3)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology4_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 

End if 


! {Ssc, Ssc}
If ((Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MSsc(i1) 
ML2 = MSsc(i2) 
coup1 = cplhhhhSsccSsc1(gt1,gt2,i1,i2)
coup2 = cplhhSsccSsc(gt3,i2,i1)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology4_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 

  End Do
End Do
End if 


! {VWp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
coup1 = cplhhhhcVWpVWp1(gt1,gt2)
coup2 = cplhhcVWpVWp(gt3)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology4_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 

End if 


! {VZ, VZ}
If ((Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
ML1 = MVZ 
ML2 = MVZ 
coup1 = cplhhhhVZVZ1(gt1,gt2)
coup2 = cplhhVZVZ(gt3)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology4_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1._dp/2._dp)*AmpC 

End if 
    End Do
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhTohhhh


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTohhhh(MAh,MFd,MFe,MFu,MFxv,               & 
& Mhh,MHp,MSsc,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,            & 
& cplAhAhhh,cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,       & 
& cplcFuFuhhR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcgWpgWphh,cplcgWCgWChh,cplcgZgZhh,          & 
& cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,      & 
& cplAhAhhhhh1,cplhhhhhhhh1,cplhhhhHpcHp1,cplhhhhSsccSsc1,cplhhhhcVWpVWp1,               & 
& cplhhhhVZVZ1,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),MFxv(2),Mhh(2),MHp,MSsc(2),MVWp,MVZ,MAh2,MFd2(3),            & 
& MFe2(3),MFu2(3),MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplAhhhVZ(2),cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFeFehhL(3,3,2),   & 
& cplcFeFehhR(3,3,2),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFxvFxvhhL(2,2,2),         & 
& cplcFxvFxvhhR(2,2,2),cplcgWpgWphh(2),cplcgWCgWChh(2),cplcgZgZhh(2),cplhhhhhh(2,2,2),   & 
& cplhhHpcHp(2),cplhhHpcVWp(2),cplhhSsccSsc(2,2,2),cplhhcHpVWp(2),cplhhcVWpVWp(2),       & 
& cplhhVZVZ(2),cplAhAhhhhh1(2,2),cplhhhhhhhh1(2,2,2,2),cplhhhhHpcHp1(2,2),               & 
& cplhhhhSsccSsc1(2,2,2,2),cplhhhhcVWpVWp1(2,2),cplhhhhVZVZ1(2,2)

Complex(dp), Intent(out) :: Amp(2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,2
    Do gt3=1,2
Amp(gt1, gt2, gt3) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = Mhh(gt2) 
Mex3 = Mhh(gt3) 
    End Do
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTohhhh


Subroutine Amplitude_Tree_SDdiracDM_hhTocHpHp(cplhhHpcHp,Mhh,MHp,Mhh2,MHp2,Amp)

Implicit None

Real(dp), Intent(in) :: Mhh(2),MHp,Mhh2(2),MHp2

Complex(dp), Intent(in) :: cplhhHpcHp(2)

Complex(dp) :: Amp(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MHp 
Mex3 = MHp 
! Tree-Level Vertex 
coupT1 = cplhhHpcHp(gt1)
Call TreeAmp_StoSS(Mex1,Mex2,Mex3,coupT1,AmpC) 
! Colour and symmetry factor 
Amp(gt1) = AmpC 
End Do
End Subroutine Amplitude_Tree_SDdiracDM_hhTocHpHp


Subroutine Gamma_Real_SDdiracDM_hhTocHpHp(MLambda,em,gs,cplhhHpcHp,Mhh,               & 
& MHp,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplhhHpcHp(2)

Real(dp), Intent(in) :: Mhh(2),MHp

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2), GammarealGluon(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: Coup 
 
Do i1=1,2
Coup = cplhhHpcHp(i1)
Mex1 = Mhh(i1)
Mex2 = MHp
Mex3 = MHp
If (Mex1.gt.(Mex2+Mex3)) Then 
 Call hardradiationSSS(Mex1,Mex2,Mex3,MLambda,em,0._dp,0._dp,0._dp,1._dp,-1._dp,1._dp,Coup,Gammarealphoton(i1),kont)
  GammarealGluon(i1) = 0._dp 
Else 
  GammarealGluon(i1) = 0._dp 
  GammarealPhoton(i1) = 0._dp 

End if 
End Do
End Subroutine Gamma_Real_SDdiracDM_hhTocHpHp


Subroutine Amplitude_WAVE_SDdiracDM_hhTocHpHp(cplhhHpcHp,ctcplhhHpcHp,Mhh,            & 
& Mhh2,MHp,MHp2,Zfhh,ZfHp,Amp)

Implicit None

Real(dp), Intent(in) :: Mhh(2),Mhh2(2),MHp,MHp2

Complex(dp), Intent(in) :: cplhhHpcHp(2)

Complex(dp), Intent(in) :: ctcplhhHpcHp(2)

Complex(dp), Intent(in) :: Zfhh(2,2),ZfHp

Complex(dp), Intent(out) :: Amp(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MHp 
Mex3 = MHp 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1 = ctcplhhHpcHp(gt1) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1 = ZcoupT1 + 0.5_dp*Zfhh(i1,gt1)*cplhhHpcHp(i1)
End Do


! External Field 2 
ZcoupT1 = ZcoupT1 + 0.5_dp*Conjg(ZfHp)*cplhhHpcHp(gt1)


! External Field 3 
ZcoupT1 = ZcoupT1 + 0.5_dp*ZfHp*cplhhHpcHp(gt1)


! Getting the amplitude 
Call TreeAmp_StoSS(Mex1,Mex2,Mex3,ZcoupT1,AmpC) 
Amp(gt1) = AmpC 
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhTocHpHp


Subroutine Amplitude_VERTEX_SDdiracDM_hhTocHpHp(MAh,MFd,MFe,MFu,MFv,MFxe,             & 
& MFxv,Mhh,MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFv2,MFxe2,MFxv2,Mhh2,              & 
& MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL,    & 
& cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,               & 
& cplcFvFeHpR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucHpL,cplcFdFucHpR,cplcFeFvcHpL,            & 
& cplcFeFvcHpR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxeFxvcHpL,   & 
& cplcFxeFxvcHpR,cplcgWpgWphh,cplcgZgWpcHp,cplcgWCgWChh,cplcgZgWCHp,cplcgZgZhh,          & 
& cplcgWpgZHp,cplcgWCgZcHp,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,    & 
& cplhhcVWpVWp,cplhhVZVZ,cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVPVWp,      & 
& cplcHpVWpVZ,cplAhAhHpcHp1,cplhhhhHpcHp1,cplhhHpcVWpVP1,cplhhHpcVWpVZ1,cplhhcHpVPVWp1,  & 
& cplhhcHpVWpVZ1,cplHpHpcHpcHp1,cplHpSsccHpcSsc1,cplHpcHpcVWpVWp1,cplHpcHpVZVZ1,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,             & 
& MVZ,MAh2,MFd2(3),MFe2(3),MFu2(3),MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),         & 
& MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplAhhhVZ(2),cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),& 
& cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),               & 
& cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),               & 
& cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),               & 
& cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),           & 
& cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcgWpgWphh(2),cplcgZgWpcHp,cplcgWCgWChh(2),      & 
& cplcgZgWCHp,cplcgZgZhh(2),cplcgWpgZHp,cplcgWCgZcHp,cplhhhhhh(2,2,2),cplhhHpcHp(2),     & 
& cplhhHpcVWp(2),cplhhSsccSsc(2,2,2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplhhVZVZ(2),        & 
& cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplAhAhHpcHp1,   & 
& cplhhhhHpcHp1(2,2),cplhhHpcVWpVP1(2),cplhhHpcVWpVZ1(2),cplhhcHpVPVWp1(2),              & 
& cplhhcHpVWpVZ1(2),cplHpHpcHpcHp1,cplHpSsccHpcSsc1(2,2),cplHpcHpcVWpVWp1,               & 
& cplHpcHpVZVZ1

Complex(dp), Intent(out) :: Amp(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
Amp(gt1) = 0._dp 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MHp 
Mex3 = MHp 


! {Ah, Ah, VWp}
If ((Include_in_loopAh).and.(Include_in_loopAh).and.(Include_in_loopVWp)) Then 
ML1 = MAh 
ML2 = MAh 
ML3 = MVWp 
coup1 = cplAhAhhh(gt1)
coup2 = cplAhcHpVWp
coup3 = -cplAhHpcVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {Ah, VZ, VWp}
If ((Include_in_loopAh).and.(Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
ML1 = MAh 
ML2 = MVZ 
ML3 = MVWp 
coup1 = cplAhhhVZ(gt1)
coup2 = cplAhcHpVWp
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {Fd, Fd, Fu}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopFu)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MFu(i3) 
coup1L = cplcFdFdhhL(i2,i1,gt1)
coup1R = cplcFdFdhhR(i2,i1,gt1)
coup2L = cplcFdFucHpL(i1,i3)
coup2R = cplcFdFucHpR(i1,i3)
coup3L = cplcFuFdHpL(i3,i2)
coup3R = cplcFuFdHpR(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(3)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fe, Fe, Fv}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopFv)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MFv(i3) 
coup1L = cplcFeFehhL(i2,i1,gt1)
coup1R = cplcFeFehhR(i2,i1,gt1)
coup2L = cplcFeFvcHpL(i1,i3)
coup2R = cplcFeFvcHpR(i1,i3)
coup3L = cplcFvFeHpL(i3,i2)
coup3R = cplcFvFeHpR(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {gWpC, gWpC, gZ}
If ((Include_in_loopgWC).and.(Include_in_loopgWC).and.(Include_in_loopgZ)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVZ 
coup1 = cplcgWCgWChh(gt1)
coup2 = cplcgWCgZcHp
coup3 = cplcgZgWCHp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {gZ, gZ, gWp}
If ((Include_in_loopgZ).and.(Include_in_loopgZ).and.(Include_in_loopgWp)) Then 
ML1 = MVZ 
ML2 = MVZ 
ML3 = MVWp 
coup1 = cplcgZgZhh(gt1)
coup2 = cplcgZgWpcHp
coup3 = cplcgWpgZHp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {hh, hh, Hp}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loopHp)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = MHp 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = cplhhHpcHp(i1)
coup3 = cplhhHpcHp(i2)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {hh, hh, VWp}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loopVWp)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = MVWp 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = cplhhcHpVWp(i1)
coup3 = -cplhhHpcVWp(i2)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {VZ, VZ, Hp}
If ((Include_in_loopVZ).and.(Include_in_loopVZ).and.(Include_in_loopHp)) Then 
ML1 = MVZ 
ML2 = MVZ 
ML3 = MHp 
coup1 = cplhhVZVZ(gt1)
coup2 = cplHpcHpVZ
coup3 = cplHpcHpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {VZ, Ah, VWp}
If ((Include_in_loopVZ).and.(Include_in_loopAh).and.(Include_in_loopVWp)) Then 
ML1 = MVZ 
ML2 = MAh 
ML3 = MVWp 
coup1 = cplAhhhVZ(gt1)
coup2 = cplcHpVWpVZ
coup3 = -cplAhHpcVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {VZ, VZ, VWp}
If ((Include_in_loopVZ).and.(Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
ML1 = MVZ 
ML2 = MVZ 
ML3 = MVWp 
coup1 = cplhhVZVZ(gt1)
coup2 = cplcHpVWpVZ
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {bar[Fu], bar[Fu], bar[Fd]}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopFd)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFuhhL(i1,i2,gt1)
coup1R = cplcFuFuhhR(i1,i2,gt1)
coup2L = cplcFdFucHpL(i3,i1)
coup2R = cplcFdFucHpR(i3,i1)
coup3L = cplcFuFdHpL(i2,i3)
coup3R = cplcFuFdHpR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(3)*AmpC 
    End Do
  End Do
End Do
End if 


! {bar[Fxv], bar[Fxv], bar[Fxe]}
If ((Include_in_loopFxv).and.(Include_in_loopFxv).and.(Include_in_loopFxe)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MFxv(i1) 
ML2 = MFxv(i2) 
ML3 = MFxe 
coup1L = cplcFxvFxvhhL(i1,i2,gt1)
coup1R = cplcFxvFxvhhR(i1,i2,gt1)
coup2L = cplcFxeFxvcHpL(i1)
coup2R = cplcFxeFxvcHpR(i1)
coup3L = cplcFxvFxeHpL(i2)
coup3R = cplcFxvFxeHpR(i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {bar[gWp], bar[gWp], bar[gZ]}
If ((Include_in_loopgWp).and.(Include_in_loopgWp).and.(Include_in_loopgZ)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVZ 
coup1 = cplcgWpgWphh(gt1)
coup2 = cplcgZgWpcHp
coup3 = cplcgWpgZHp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {bar[gZ], bar[gZ], bar[gWpC]}
If ((Include_in_loopgZ).and.(Include_in_loopgZ).and.(Include_in_loopgWC)) Then 
ML1 = MVZ 
ML2 = MVZ 
ML3 = MVWp 
coup1 = cplcgZgZhh(gt1)
coup2 = cplcgWCgZcHp
coup3 = cplcgZgWCHp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[Hp], hh}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MHp 
ML2 = MHp 
ML3 = Mhh(i3) 
coup1 = cplhhHpcHp(gt1)
coup2 = cplhhHpcHp(i3)
coup3 = cplhhHpcHp(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[Hp], conj[VWp], hh}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MHp 
ML2 = MVWp 
ML3 = Mhh(i3) 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplhhHpcHp(i3)
coup3 = -cplhhHpcVWp(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[Hp], conj[Hp], VP}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVP 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpcHpVP
coup3 = cplHpcHpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[VWp], VP}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVP 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplHpcHpVP
coup3 = cplHpcVWpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[Hp], VZ}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVZ)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVZ 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpcHpVZ
coup3 = cplHpcHpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[VWp], VZ}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVZ)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVZ 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplHpcHpVZ
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], Ah}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopAh)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MAh 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplAhcHpVWp
coup3 = -cplAhHpcVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], hh}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MVWp 
ML2 = MHp 
ML3 = Mhh(i3) 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplhhcHpVWp(i3)
coup3 = cplhhHpcHp(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[VWp], conj[VWp], hh}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MVWp 
ML2 = MVWp 
ML3 = Mhh(i3) 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplhhcHpVWp(i3)
coup3 = -cplhhHpcVWp(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[VWp], conj[Hp], VP}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVP 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplcHpVPVWp
coup3 = cplHpcHpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], VP}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVP 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplcHpVPVWp
coup3 = cplHpcVWpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], VZ}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVZ)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVZ 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplcHpVWpVZ
coup3 = cplHpcHpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], VZ}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopVZ)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVZ 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplcHpVWpVZ
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {Ah, Ah}
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
ML1 = MAh 
ML2 = MAh 
coup1 = cplAhAhhh(gt1)
coup2 = cplAhAhHpcHp1
Call Amp_VERTEX_StoSS_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1._dp/2._dp)*AmpC 
End if 


! {hh, hh}
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = cplhhhhHpcHp1(i1,i2)
Call Amp_VERTEX_StoSS_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1._dp/2._dp)*AmpC 
  End Do
End Do
End if 


! {Hp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MHp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpHpcHpcHp1
Call Amp_VERTEX_StoSS_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {Ssc, Ssc}
If ((Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MSsc(i1) 
ML2 = MSsc(i2) 
coup1 = cplhhSsccSsc(gt1,i1,i2)
coup2 = cplHpSsccHpcSsc1(i2,i1)
Call Amp_VERTEX_StoSS_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {VWp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplHpcHpcVWpVWp1
Call Amp_VERTEX_StoSS_Topology2_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {VZ, VZ}
If ((Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
ML1 = MVZ 
ML2 = MVZ 
coup1 = cplhhVZVZ(gt1)
coup2 = cplHpcHpVZVZ1
Call Amp_VERTEX_StoSS_Topology2_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1._dp/2._dp)*AmpC 
End if 


! {hh, Hp}
If ((Include_in_loophh).and.(Include_in_loopHp)) Then 
Do i1=1,2
ML1 = Mhh(i1) 
ML2 = MHp 
coup1 = cplhhhhHpcHp1(gt1,i1)
coup2 = cplhhHpcHp(i1)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology3_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 

End Do
End if 


! {VP, VWp}
If ((Include_in_loopVP).and.(Include_in_loopVWp)) Then 
ML1 = MVP 
ML2 = MVWp 
coup1 = cplhhHpcVWpVP1(gt1)
coup2 = cplcHpVPVWp
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology3_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 

End if 


! {VZ, VWp}
If ((Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
ML1 = MVZ 
ML2 = MVWp 
coup1 = cplhhHpcVWpVZ1(gt1)
coup2 = cplcHpVWpVZ
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology3_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 

End if 


! {hh, conj[Hp]}
If ((Include_in_loophh).and.(Include_in_loopHp)) Then 
Do i1=1,2
ML1 = Mhh(i1) 
ML2 = MHp 
coup1 = cplhhhhHpcHp1(gt1,i1)
coup2 = cplhhHpcHp(i1)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology4_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 

End Do
End if 


! {VP, conj[VWp]}
If ((Include_in_loopVP).and.(Include_in_loopVWp)) Then 
ML1 = MVP 
ML2 = MVWp 
coup1 = cplhhcHpVPVWp1(gt1)
coup2 = cplHpcVWpVP
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology4_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 

End if 


! {VWp, VZ}
If ((Include_in_loopVWp).and.(Include_in_loopVZ)) Then 
ML1 = MVWp 
ML2 = MVZ 
coup1 = cplhhcHpVWpVZ1(gt1)
coup2 = cplHpcVWpVZ
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology4_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 

End if 
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhTocHpHp


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTocHpHp(MAh,MFd,MFe,MFu,MFv,               & 
& MFxe,MFxv,Mhh,MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFv2,MFxe2,MFxv2,              & 
& Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,           & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFeFehhL,cplcFeFehhR,               & 
& cplcFvFeHpL,cplcFvFeHpR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucHpL,cplcFdFucHpR,             & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxvhhL,cplcFxvFxvhhR,     & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcgWpgWphh,cplcgZgWpcHp,cplcgWCgWChh,cplcgZgWCHp,      & 
& cplcgZgZhh,cplcgWpgZHp,cplcgWCgZcHp,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,     & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,cplHpcVWpVZ,      & 
& cplcHpVPVWp,cplcHpVWpVZ,cplAhAhHpcHp1,cplhhhhHpcHp1,cplhhHpcVWpVP1,cplhhHpcVWpVZ1,     & 
& cplhhcHpVPVWp1,cplhhcHpVWpVZ1,cplHpHpcHpcHp1,cplHpSsccHpcSsc1,cplHpcHpcVWpVWp1,        & 
& cplHpcHpVZVZ1,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,             & 
& MVZ,MAh2,MFd2(3),MFe2(3),MFu2(3),MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),         & 
& MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplAhhhVZ(2),cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),& 
& cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),               & 
& cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),               & 
& cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),               & 
& cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),           & 
& cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcgWpgWphh(2),cplcgZgWpcHp,cplcgWCgWChh(2),      & 
& cplcgZgWCHp,cplcgZgZhh(2),cplcgWpgZHp,cplcgWCgZcHp,cplhhhhhh(2,2,2),cplhhHpcHp(2),     & 
& cplhhHpcVWp(2),cplhhSsccSsc(2,2,2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplhhVZVZ(2),        & 
& cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVPVWp,cplcHpVWpVZ,cplAhAhHpcHp1,   & 
& cplhhhhHpcHp1(2,2),cplhhHpcVWpVP1(2),cplhhHpcVWpVZ1(2),cplhhcHpVPVWp1(2),              & 
& cplhhcHpVWpVZ1(2),cplHpHpcHpcHp1,cplHpSsccHpcSsc1(2,2),cplHpcHpcVWpVWp1,               & 
& cplHpcHpVZVZ1

Complex(dp), Intent(out) :: Amp(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
Amp(gt1) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MHp 
Mex3 = MHp 


! {conj[Hp], conj[Hp], VP}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVP 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpcHpVP
coup3 = cplHpcHpVP
Call Amp_VERTEX_StoSS_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[VWp], VP}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVP 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplHpcHpVP
coup3 = cplHpcVWpVP
Call Amp_VERTEX_StoSS_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], VP}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVP 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplcHpVPVWp
coup3 = cplHpcHpVP
Call Amp_VERTEX_StoSS_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], VP}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVP 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplcHpVPVWp
coup3 = cplHpcVWpVP
Call Amp_VERTEX_StoSS_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 
End if 


! {VP, VWp}
If ((Include_in_loopVP).and.(Include_in_loopVWp)) Then 
ML1 = MVP 
ML2 = MVWp 
coup1 = cplhhHpcVWpVP1(gt1)
coup2 = cplcHpVPVWp
Call Amp_VERTEX_StoSS_Topology3_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 

End if 


! {VP, conj[VWp]}
If ((Include_in_loopVP).and.(Include_in_loopVWp)) Then 
ML1 = MVP 
ML2 = MVWp 
coup1 = cplhhcHpVPVWp1(gt1)
coup2 = cplHpcVWpVP
Call Amp_VERTEX_StoSS_Topology4_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1) = Amp(gt1) + oo16pi2*(1)*AmpC 

End if 
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTocHpHp


Subroutine Amplitude_Tree_SDdiracDM_hhToHpcVWp(cplhhHpcVWp,Mhh,MHp,MVWp,              & 
& Mhh2,MHp2,MVWp2,Amp)

Implicit None

Real(dp), Intent(in) :: Mhh(2),MHp,MVWp,Mhh2(2),MHp2,MVWp2

Complex(dp), Intent(in) :: cplhhHpcVWp(2)

Complex(dp) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MHp 
Mex3 = MVWp 
! Tree-Level Vertex 
coupT1 = cplhhHpcVWp(gt1)
Call TreeAmp_StoSV(Mex1,Mex2,Mex3,coupT1,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1) = AmpC 
End Do
End Subroutine Amplitude_Tree_SDdiracDM_hhToHpcVWp


Subroutine Gamma_Real_SDdiracDM_hhToHpcVWp(MLambda,em,gs,cplhhHpcVWp,Mhh,             & 
& MHp,MVWp,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplhhHpcVWp(2)

Real(dp), Intent(in) :: Mhh(2),MHp,MVWp

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2), GammarealGluon(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: Coup 
 
Do i1=1,2
Coup = cplhhHpcVWp(i1)
Mex1 = Mhh(i1)
Mex2 = MHp
Mex3 = MVWp
If (Mex1.gt.(Mex2+Mex3)) Then 
 Call hardradiationSSV(Mex1,Mex2,Mex3,MLambda,em,0._dp,1._dp,-1._dp,Coup,Gammarealphoton(i1),kont)
 GammarealGluon(i1) = 0._dp 
Else 
  GammarealGluon(i1) = 0._dp 
  GammarealPhoton(i1) = 0._dp 

End if 
End Do
End Subroutine Gamma_Real_SDdiracDM_hhToHpcVWp


Subroutine Amplitude_WAVE_SDdiracDM_hhToHpcVWp(cplhhHpcVWp,ctcplhhHpcVWp,             & 
& Mhh,Mhh2,MHp,MHp2,MVWp,MVWp2,Zfhh,ZfHp,ZfVWp,Amp)

Implicit None

Real(dp), Intent(in) :: Mhh(2),Mhh2(2),MHp,MHp2,MVWp,MVWp2

Complex(dp), Intent(in) :: cplhhHpcVWp(2)

Complex(dp), Intent(in) :: ctcplhhHpcVWp(2)

Complex(dp), Intent(in) :: Zfhh(2,2),ZfHp,ZfVWp

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MHp 
Mex3 = MVWp 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1 = ctcplhhHpcVWp(gt1) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1 = ZcoupT1 + 0.5_dp*Zfhh(i1,gt1)*cplhhHpcVWp(i1)
End Do


! External Field 2 
ZcoupT1 = ZcoupT1 + 0.5_dp*ZfHp*cplhhHpcVWp(gt1)


! External Field 3 
ZcoupT1 = ZcoupT1 + 0.5_dp*ZfVWp*cplhhHpcVWp(gt1)


! Getting the amplitude 
Call TreeAmp_StoSV(Mex1,Mex2,Mex3,ZcoupT1,AmpC) 
Amp(:,gt1) = AmpC 
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhToHpcVWp


Subroutine Amplitude_VERTEX_SDdiracDM_hhToHpcVWp(MAh,MFd,MFe,MFu,MFv,MFxe,            & 
& MFxv,Mhh,MHp,MVP,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,              & 
& MVP2,MVWp2,MVZ2,cplAhhhVZ,cplAhHpcVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,             & 
& cplcFuFdHpR,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFuFuhhL,               & 
& cplcFuFuhhR,cplcFdFucVWpL,cplcFdFucVWpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,     & 
& cplcFxvFxeHpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,             & 
& cplcgZgAhh,cplcgWpgAHp,cplcgWpgWphh,cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgWChh,          & 
& cplcgZgWCHp,cplcgZgZhh,cplcgWpgZHp,cplcgWCgZcVWp,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,     & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,cplHpcVWpVZ,      & 
& cplcVWpVPVWp,cplcVWpVWpVZ,cplAhHpcVWpVZ1,cplhhhhcVWpVWp1,cplhhHpcVWpVP1,               & 
& cplhhHpcVWpVZ1,cplHpcHpcVWpVWp1,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MVP,MVWp,MVZ,MAh2,            & 
& MFd2(3),MFe2(3),MFu2(3),MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhhhVZ(2),cplAhHpcVWp,cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFuFdHpL(3,3),      & 
& cplcFuFdHpR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFvFeHpL(3,3),               & 
& cplcFvFeHpR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFdFucVWpL(3,3),             & 
& cplcFdFucVWpR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFxvFxeHpL(2),             & 
& cplcFxvFxeHpR(2),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxeFxvcVWpL(2),         & 
& cplcFxeFxvcVWpR(2),cplcgZgAhh(2),cplcgWpgAHp,cplcgWpgWphh(2),cplcgAgWpcVWp,            & 
& cplcgZgWpcVWp,cplcgWCgWChh(2),cplcgZgWCHp,cplcgZgZhh(2),cplcgWpgZHp,cplcgWCgZcVWp,     & 
& cplhhhhhh(2,2,2),cplhhHpcHp(2),cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),          & 
& cplhhVZVZ(2),cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,cplHpcVWpVZ,cplcVWpVPVWp,               & 
& cplcVWpVWpVZ,cplAhHpcVWpVZ1,cplhhhhcVWpVWp1(2,2),cplhhHpcVWpVP1(2),cplhhHpcVWpVZ1(2),  & 
& cplHpcHpcVWpVWp1

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
Mex1 = Mhh(gt1) 
Mex2 = MHp 
Mex3 = MVWp 


! {Ah, VZ, conj[VWp]}
If ((Include_in_loopAh).and.(Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
ML1 = MAh 
ML2 = MVZ 
ML3 = MVWp 
coup1 = cplAhhhVZ(gt1)
coup2 = -cplAhHpcVWp
coup3 = -cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Fu, Fu, Fd}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopFd)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFuhhL(i2,i1,gt1)
coup1R = cplcFuFuhhR(i2,i1,gt1)
coup2L = cplcFuFdHpL(i1,i3)
coup2R = cplcFuFdHpR(i1,i3)
coup3L = -cplcFdFucVWpR(i3,i2)
coup3R = -cplcFdFucVWpL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(3)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fxv, Fxv, Fxe}
If ((Include_in_loopFxv).and.(Include_in_loopFxv).and.(Include_in_loopFxe)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MFxv(i1) 
ML2 = MFxv(i2) 
ML3 = MFxe 
coup1L = cplcFxvFxvhhL(i2,i1,gt1)
coup1R = cplcFxvFxvhhR(i2,i1,gt1)
coup2L = cplcFxvFxeHpL(i1)
coup2R = cplcFxvFxeHpR(i1)
coup3L = cplcFxeFxvcVWpL(i2)
coup3R = cplcFxeFxvcVWpR(i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {gWp, gWp, gP}
If ((Include_in_loopgWp).and.(Include_in_loopgWp).and.(Include_in_loopgA)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVP 
coup1 = cplcgWpgWphh(gt1)
coup2 = cplcgWpgAHp
coup3 = cplcgAgWpcVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {gWp, gWp, gZ}
If ((Include_in_loopgWp).and.(Include_in_loopgWp).and.(Include_in_loopgZ)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVZ 
coup1 = cplcgWpgWphh(gt1)
coup2 = cplcgWpgZHp
coup3 = cplcgZgWpcVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {gZ, gZ, gWpC}
If ((Include_in_loopgZ).and.(Include_in_loopgZ).and.(Include_in_loopgWC)) Then 
ML1 = MVZ 
ML2 = MVZ 
ML3 = MVWp 
coup1 = cplcgZgZhh(gt1)
coup2 = cplcgZgWCHp
coup3 = cplcgWCgZcVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {hh, hh, conj[Hp]}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loopHp)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = MHp 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = cplhhHpcHp(i1)
coup3 = -cplhhHpcVWp(i2)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {hh, hh, conj[VWp]}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loopVWp)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = MVWp 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = -cplhhHpcVWp(i1)
coup3 = cplhhcVWpVWp(i2)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {Hp, Hp, hh}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MHp 
ML2 = MHp 
ML3 = Mhh(i3) 
coup1 = cplhhHpcHp(gt1)
coup2 = cplhhHpcHp(i3)
coup3 = cplhhHpcVWp(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {Hp, VWp, hh}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MHp 
ML2 = MVWp 
ML3 = Mhh(i3) 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplhhHpcHp(i3)
coup3 = cplhhcVWpVWp(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {Hp, Hp, VP}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVP 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpcHpVP
coup3 = cplHpcVWpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp, VP}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVP 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplHpcHpVP
coup3 = -cplcVWpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, Hp, VZ}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVZ)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVZ 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpcHpVZ
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp, VZ}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVZ)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVZ 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplHpcHpVZ
coup3 = cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, Ah}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopAh)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MAh 
coup1 = -cplhhcHpVWp(gt1)
coup2 = -cplAhHpcVWp
coup3 = cplAhHpcVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, hh}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MVWp 
ML2 = MHp 
ML3 = Mhh(i3) 
coup1 = -cplhhcHpVWp(gt1)
coup2 = -cplhhHpcVWp(i3)
coup3 = cplhhHpcVWp(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VWp, VWp, hh}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MVWp 
ML2 = MVWp 
ML3 = Mhh(i3) 
coup1 = cplhhcVWpVWp(gt1)
coup2 = -cplhhHpcVWp(i3)
coup3 = cplhhcVWpVWp(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VWp, Hp, VP}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVP 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplHpcVWpVP
coup3 = cplHpcVWpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, VP}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVP 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplHpcVWpVP
coup3 = -cplcVWpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, VZ}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVZ)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVZ 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplHpcVWpVZ
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, VZ}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopVZ)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVZ 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplHpcVWpVZ
coup3 = cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VZ, Ah, conj[Hp]}
If ((Include_in_loopVZ).and.(Include_in_loopAh).and.(Include_in_loopHp)) Then 
ML1 = MVZ 
ML2 = MAh 
ML3 = MHp 
coup1 = cplAhhhVZ(gt1)
coup2 = cplHpcHpVZ
coup3 = -cplAhHpcVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VZ, VZ, conj[Hp]}
If ((Include_in_loopVZ).and.(Include_in_loopVZ).and.(Include_in_loopHp)) Then 
ML1 = MVZ 
ML2 = MVZ 
ML3 = MHp 
coup1 = cplhhVZVZ(gt1)
coup2 = cplHpcHpVZ
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VZ, VZ, conj[VWp]}
If ((Include_in_loopVZ).and.(Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
ML1 = MVZ 
ML2 = MVZ 
ML3 = MVWp 
coup1 = cplhhVZVZ(gt1)
coup2 = cplHpcVWpVZ
coup3 = -cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {bar[Fd], bar[Fd], bar[Fu]}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopFu)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MFu(i3) 
coup1L = cplcFdFdhhL(i1,i2,gt1)
coup1R = cplcFdFdhhR(i1,i2,gt1)
coup2L = cplcFuFdHpL(i3,i1)
coup2R = cplcFuFdHpR(i3,i1)
coup3L = cplcFdFucVWpL(i2,i3)
coup3R = cplcFdFucVWpR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(3)*AmpC 
    End Do
  End Do
End Do
End if 


! {bar[Fe], bar[Fe], bar[Fv]}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopFv)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MFv(i3) 
coup1L = cplcFeFehhL(i1,i2,gt1)
coup1R = cplcFeFehhR(i1,i2,gt1)
coup2L = cplcFvFeHpL(i3,i1)
coup2R = cplcFvFeHpR(i3,i1)
coup3L = cplcFeFvcVWpL(i2,i3)
coup3R = cplcFeFvcVWpR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {bar[gWpC], bar[gWpC], bar[gZ]}
If ((Include_in_loopgWC).and.(Include_in_loopgWC).and.(Include_in_loopgZ)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVZ 
coup1 = cplcgWCgWChh(gt1)
coup2 = cplcgZgWCHp
coup3 = cplcgWCgZcVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {bar[gZ], bar[gP], bar[gWp]}
If ((Include_in_loopgZ).and.(Include_in_loopgA).and.(Include_in_loopgWp)) Then 
ML1 = MVZ 
ML2 = MVP 
ML3 = MVWp 
coup1 = cplcgZgAhh(gt1)
coup2 = cplcgWpgZHp
coup3 = cplcgAgWpcVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {bar[gZ], bar[gZ], bar[gWp]}
If ((Include_in_loopgZ).and.(Include_in_loopgZ).and.(Include_in_loopgWp)) Then 
ML1 = MVZ 
ML2 = MVZ 
ML3 = MVWp 
coup1 = cplcgZgZhh(gt1)
coup2 = cplcgWpgZHp
coup3 = cplcgZgWpcVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Ah, VZ}
If ((Include_in_loopAh).and.(Include_in_loopVZ)) Then 
ML1 = MAh 
ML2 = MVZ 
coup1 = -cplAhhhVZ(gt1)
coup2 = cplAhHpcVWpVZ1
Call Amp_VERTEX_StoSV_Topology2_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
coup1 = cplhhHpcVWp(gt1)
coup2 = cplHpcHpcVWpVWp1
Call Amp_VERTEX_StoSV_Topology2_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {hh, conj[VWp]}
If ((Include_in_loophh).and.(Include_in_loopVWp)) Then 
Do i1=1,2
ML1 = Mhh(i1) 
ML2 = MVWp 
coup1 = cplhhhhcVWpVWp1(gt1,i1)
coup2 = -cplhhHpcVWp(i1)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology3_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End Do
End if 


! {Hp, VP}
If ((Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MHp 
ML2 = MVP 
coup1 = cplhhHpcVWpVP1(gt1)
coup2 = cplHpcHpVP
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology3_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 


! {Hp, VZ}
If ((Include_in_loopHp).and.(Include_in_loopVZ)) Then 
ML1 = MHp 
ML2 = MVZ 
coup1 = cplhhHpcVWpVZ1(gt1)
coup2 = cplHpcHpVZ
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology3_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhToHpcVWp


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToHpcVWp(MAh,MFd,MFe,MFu,MFv,              & 
& MFxe,MFxv,Mhh,MHp,MVP,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFv2,MFxe2,MFxv2,Mhh2,              & 
& MHp2,MVP2,MVWp2,MVZ2,cplAhhhVZ,cplAhHpcVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,        & 
& cplcFuFdHpR,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFuFuhhL,               & 
& cplcFuFuhhR,cplcFdFucVWpL,cplcFdFucVWpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,     & 
& cplcFxvFxeHpR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,             & 
& cplcgZgAhh,cplcgWpgAHp,cplcgWpgWphh,cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgWChh,          & 
& cplcgZgWCHp,cplcgZgZhh,cplcgWpgZHp,cplcgWCgZcVWp,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,     & 
& cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,cplHpcVWpVZ,      & 
& cplcVWpVPVWp,cplcVWpVWpVZ,cplAhHpcVWpVZ1,cplhhhhcVWpVWp1,cplhhHpcVWpVP1,               & 
& cplhhHpcVWpVZ1,cplHpcHpcVWpVWp1,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MVP,MVWp,MVZ,MAh2,            & 
& MFd2(3),MFe2(3),MFu2(3),MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhhhVZ(2),cplAhHpcVWp,cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFuFdHpL(3,3),      & 
& cplcFuFdHpR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFvFeHpL(3,3),               & 
& cplcFvFeHpR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFdFucVWpL(3,3),             & 
& cplcFdFucVWpR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFxvFxeHpL(2),             & 
& cplcFxvFxeHpR(2),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxeFxvcVWpL(2),         & 
& cplcFxeFxvcVWpR(2),cplcgZgAhh(2),cplcgWpgAHp,cplcgWpgWphh(2),cplcgAgWpcVWp,            & 
& cplcgZgWpcVWp,cplcgWCgWChh(2),cplcgZgWCHp,cplcgZgZhh(2),cplcgWpgZHp,cplcgWCgZcVWp,     & 
& cplhhhhhh(2,2,2),cplhhHpcHp(2),cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),          & 
& cplhhVZVZ(2),cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,cplHpcVWpVZ,cplcVWpVPVWp,               & 
& cplcVWpVWpVZ,cplAhHpcVWpVZ1,cplhhhhcVWpVWp1(2,2),cplhhHpcVWpVP1(2),cplhhHpcVWpVZ1(2),  & 
& cplHpcHpcVWpVWp1

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
Mex1 = Mhh(gt1) 
Mex2 = MHp 
Mex3 = MVWp 


! {Hp, Hp, VP}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVP 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpcHpVP
coup3 = cplHpcVWpVP
Call Amp_VERTEX_StoSV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp, VP}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVP 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplHpcHpVP
coup3 = -cplcVWpVPVWp
Call Amp_VERTEX_StoSV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, VP}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVP 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplHpcVWpVP
coup3 = cplHpcVWpVP
Call Amp_VERTEX_StoSV_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, VP}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVP 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplHpcVWpVP
coup3 = -cplcVWpVPVWp
Call Amp_VERTEX_StoSV_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VP}
If ((Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MHp 
ML2 = MVP 
coup1 = cplhhHpcVWpVP1(gt1)
coup2 = cplHpcHpVP
Call Amp_VERTEX_StoSV_Topology3_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToHpcVWp


Subroutine Amplitude_Tree_SDdiracDM_hhTocSscSsc(cplhhSsccSsc,Mhh,MSsc,Mhh2,           & 
& MSsc2,Amp)

Implicit None

Real(dp), Intent(in) :: Mhh(2),MSsc(2),Mhh2(2),MSsc2(2)

Complex(dp), Intent(in) :: cplhhSsccSsc(2,2,2)

Complex(dp) :: Amp(2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
  Do gt2=1,2
    Do gt3=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MSsc(gt2) 
Mex3 = MSsc(gt3) 
! Tree-Level Vertex 
coupT1 = cplhhSsccSsc(gt1,gt3,gt2)
Call TreeAmp_StoSS(Mex1,Mex2,Mex3,coupT1,AmpC) 
! Colour and symmetry factor 
Amp(gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_hhTocSscSsc


Subroutine Gamma_Real_SDdiracDM_hhTocSscSsc(MLambda,em,gs,cplhhSsccSsc,               & 
& Mhh,MSsc,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplhhSsccSsc(2,2,2)

Real(dp), Intent(in) :: Mhh(2),MSsc(2)

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2,2,2), GammarealGluon(2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: Coup 
 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
Coup = cplhhSsccSsc(i1,i3,i2)
Mex1 = Mhh(i1)
Mex2 = MSsc(i2)
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
End Subroutine Gamma_Real_SDdiracDM_hhTocSscSsc


Subroutine Amplitude_WAVE_SDdiracDM_hhTocSscSsc(cplhhSsccSsc,ctcplhhSsccSsc,          & 
& Mhh,Mhh2,MSsc,MSsc2,Zfhh,ZfSsc,Amp)

Implicit None

Real(dp), Intent(in) :: Mhh(2),Mhh2(2),MSsc(2),MSsc2(2)

Complex(dp), Intent(in) :: cplhhSsccSsc(2,2,2)

Complex(dp), Intent(in) :: ctcplhhSsccSsc(2,2,2)

Complex(dp), Intent(in) :: Zfhh(2,2),ZfSsc(2,2)

Complex(dp), Intent(out) :: Amp(2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
  Do gt2=1,2
    Do gt3=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MSsc(gt2) 
Mex3 = MSsc(gt3) 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1 = ctcplhhSsccSsc(gt1,gt3,gt2) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1 = ZcoupT1 + 0.5_dp*Zfhh(i1,gt1)*cplhhSsccSsc(i1,gt3,gt2)
End Do


! External Field 2 
Do i1=1,2
ZcoupT1 = ZcoupT1 + 0.5_dp*Conjg(ZfSsc(i1,gt2))*cplhhSsccSsc(gt1,gt3,i1)
End Do


! External Field 3 
Do i1=1,2
ZcoupT1 = ZcoupT1 + 0.5_dp*ZfSsc(i1,gt3)*cplhhSsccSsc(gt1,i1,gt2)
End Do


! Getting the amplitude 
Call TreeAmp_StoSS(Mex1,Mex2,Mex3,ZcoupT1,AmpC) 
Amp(gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhTocSscSsc


Subroutine Amplitude_VERTEX_SDdiracDM_hhTocSscSsc(MAh,MFe,MFv,MFxe,MFxv,              & 
& Mhh,MHp,MSsc,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,cplAhAhhh,cplcFeFehhL,         & 
& cplcFeFehhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFeFxecSscL,    & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFvFxvcSscL,cplcFvFxvcSscR,              & 
& cplhhhhhh,cplhhHpcHp,cplhhSsccSsc,cplAhAhSsccSsc1,cplhhhhSsccSsc1,cplHpSsccHpcSsc1,    & 
& cplSscSsccSsccSsc1,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MAh2,MFe2(3),MFv2(3),               & 
& MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2)

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),& 
& cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),     & 
& cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2), & 
& cplhhhhhh(2,2,2),cplhhHpcHp(2),cplhhSsccSsc(2,2,2),cplAhAhSsccSsc1(2,2),               & 
& cplhhhhSsccSsc1(2,2,2,2),cplHpSsccHpcSsc1(2,2),cplSscSsccSsccSsc1(2,2,2,2)

Complex(dp), Intent(out) :: Amp(2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,2
    Do gt3=1,2
Amp(gt1, gt2, gt3) = 0._dp 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MSsc(gt2) 
Mex3 = MSsc(gt3) 


! {Fe, Fe, Fxe}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopFxe)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MFxe 
coup1L = cplcFeFehhL(i2,i1,gt1)
coup1R = cplcFeFehhR(i2,i1,gt1)
coup2L = cplcFeFxecSscL(i1,gt2)
coup2R = cplcFeFxecSscR(i1,gt2)
coup3L = cplcFxeFeSscL(i2,gt3)
coup3R = cplcFxeFeSscR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {hh, hh, Ssc}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loopSsc)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = MSsc(i3) 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = cplhhSsccSsc(i1,i3,gt2)
coup3 = cplhhSsccSsc(i2,gt3,i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {bar[Fxv], bar[Fxv], bar[Fv]}
If ((Include_in_loopFxv).and.(Include_in_loopFxv).and.(Include_in_loopFv)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,3
ML1 = MFxv(i1) 
ML2 = MFxv(i2) 
ML3 = MFv(i3) 
coup1L = cplcFxvFxvhhL(i1,i2,gt1)
coup1R = cplcFxvFxvhhR(i1,i2,gt1)
coup2L = cplcFvFxvcSscL(i3,i1,gt2)
coup2R = cplcFvFxvcSscR(i3,i1,gt2)
coup3L = cplcFxvFvSscL(i2,i3,gt3)
coup3R = cplcFxvFvSscR(i2,i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {conj[Ssc], conj[Ssc], hh}
If ((Include_in_loopSsc).and.(Include_in_loopSsc).and.(Include_in_loophh)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = MSsc(i1) 
ML2 = MSsc(i2) 
ML3 = Mhh(i3) 
coup1 = cplhhSsccSsc(gt1,i2,i1)
coup2 = cplhhSsccSsc(i3,i1,gt2)
coup3 = cplhhSsccSsc(i3,gt3,i2)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {Ah, Ah}
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
ML1 = MAh 
ML2 = MAh 
coup1 = cplAhAhhh(gt1)
coup2 = cplAhAhSsccSsc1(gt3,gt2)
Call Amp_VERTEX_StoSS_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1._dp/2._dp)*AmpC 
End if 


! {hh, hh}
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = cplhhhhSsccSsc1(i1,i2,gt3,gt2)
Call Amp_VERTEX_StoSS_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1._dp/2._dp)*AmpC 
  End Do
End Do
End if 


! {Hp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MHp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpSsccHpcSsc1(gt3,gt2)
Call Amp_VERTEX_StoSS_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End if 


! {Ssc, Ssc}
If ((Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MSsc(i1) 
ML2 = MSsc(i2) 
coup1 = cplhhSsccSsc(gt1,i1,i2)
coup2 = cplSscSsccSsccSsc1(gt3,i2,gt2,i1)
Call Amp_VERTEX_StoSS_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {hh, Ssc}
If ((Include_in_loophh).and.(Include_in_loopSsc)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = MSsc(i2) 
coup1 = cplhhhhSsccSsc1(gt1,i1,gt3,i2)
coup2 = cplhhSsccSsc(i1,i2,gt2)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology3_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 

  End Do
End Do
End if 


! {hh, conj[Ssc]}
If ((Include_in_loophh).and.(Include_in_loopSsc)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = MSsc(i2) 
coup1 = cplhhhhSsccSsc1(gt1,i1,i2,gt2)
coup2 = cplhhSsccSsc(i1,gt3,i2)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology4_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt2, gt3) = Amp(gt1, gt2, gt3) + oo16pi2*(1)*AmpC 

  End Do
End Do
End if 
    End Do
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhTocSscSsc


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTocSscSsc(MAh,MFe,MFv,MFxe,MFxv,           & 
& Mhh,MHp,MSsc,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,cplAhAhhh,cplcFeFehhL,         & 
& cplcFeFehhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFeFxecSscL,    & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFvFxvcSscL,cplcFvFxvcSscR,              & 
& cplhhhhhh,cplhhHpcHp,cplhhSsccSsc,cplAhAhSsccSsc1,cplhhhhSsccSsc1,cplHpSsccHpcSsc1,    & 
& cplSscSsccSsccSsc1,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MAh2,MFe2(3),MFv2(3),               & 
& MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2)

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),& 
& cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),     & 
& cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2), & 
& cplhhhhhh(2,2,2),cplhhHpcHp(2),cplhhSsccSsc(2,2,2),cplAhAhSsccSsc1(2,2),               & 
& cplhhhhSsccSsc1(2,2,2,2),cplHpSsccHpcSsc1(2,2),cplSscSsccSsccSsc1(2,2,2,2)

Complex(dp), Intent(out) :: Amp(2,2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,2
    Do gt3=1,2
Amp(gt1, gt2, gt3) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MSsc(gt2) 
Mex3 = MSsc(gt3) 
    End Do
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTocSscSsc


Subroutine Amplitude_Tree_SDdiracDM_hhTocVWpVWp(cplhhcVWpVWp,Mhh,MVWp,Mhh2,           & 
& MVWp2,Amp)

Implicit None

Real(dp), Intent(in) :: Mhh(2),MVWp,Mhh2(2),MVWp2

Complex(dp), Intent(in) :: cplhhcVWpVWp(2)

Complex(dp) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MVWp 
Mex3 = MVWp 
! Tree-Level Vertex 
coupT1 = cplhhcVWpVWp(gt1)
Call TreeAmp_StoVV(Mex1,Mex2,Mex3,coupT1,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1) = AmpC 
End Do
End Subroutine Amplitude_Tree_SDdiracDM_hhTocVWpVWp


Subroutine Gamma_Real_SDdiracDM_hhTocVWpVWp(MLambda,em,gs,cplhhcVWpVWp,               & 
& Mhh,MVWp,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplhhcVWpVWp(2)

Real(dp), Intent(in) :: Mhh(2),MVWp

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2), GammarealGluon(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: Coup 
 
Do i1=1,2
Coup = cplhhcVWpVWp(i1)
Mex1 = Mhh(i1)
Mex2 = MVWp
Mex3 = MVWp
If (Mex1.gt.(Mex2+Mex3)) Then 
  GammarealGluon(i1) = 0._dp 
 Call hardphotonSVV(Mex1,Mex2,Mex3,MLambda,em,0._dp,-1._dp,1._dp,Coup,Gammarealphoton(i1),kont)
Else 
  GammarealGluon(i1) = 0._dp 
  GammarealPhoton(i1) = 0._dp 

End if 
End Do
End Subroutine Gamma_Real_SDdiracDM_hhTocVWpVWp


Subroutine Amplitude_WAVE_SDdiracDM_hhTocVWpVWp(cplhhcVWpVWp,ctcplhhcVWpVWp,          & 
& Mhh,Mhh2,MVWp,MVWp2,Zfhh,ZfVWp,Amp)

Implicit None

Real(dp), Intent(in) :: Mhh(2),Mhh2(2),MVWp,MVWp2

Complex(dp), Intent(in) :: cplhhcVWpVWp(2)

Complex(dp), Intent(in) :: ctcplhhcVWpVWp(2)

Complex(dp), Intent(in) :: Zfhh(2,2),ZfVWp

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MVWp 
Mex3 = MVWp 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1 = ctcplhhcVWpVWp(gt1) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1 = ZcoupT1 + 0.5_dp*Zfhh(i1,gt1)*cplhhcVWpVWp(i1)
End Do


! External Field 2 
ZcoupT1 = ZcoupT1 + 0.5_dp*Conjg(ZfVWp)*cplhhcVWpVWp(gt1)


! External Field 3 
ZcoupT1 = ZcoupT1 + 0.5_dp*ZfVWp*cplhhcVWpVWp(gt1)


! Getting the amplitude 
Call TreeAmp_StoVV(Mex1,Mex2,Mex3,ZcoupT1,AmpC) 
Amp(:,gt1) = AmpC 
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhTocVWpVWp


Subroutine Amplitude_VERTEX_SDdiracDM_hhTocVWpVWp(MAh,MFd,MFe,MFu,MFv,MFxe,           & 
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
& cplcVWpVWpVZVZ1Q,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MVP,MVWp,MVZ,MAh2,            & 
& MFd2(3),MFe2(3),MFu2(3),MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplAhhhVZ(2),cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),& 
& cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),             & 
& cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),             & 
& cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),           & 
& cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),         & 
& cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplcgZgAhh(2),cplcgWpgAVWp,cplcgWCgAcVWp,        & 
& cplcgWpgWphh(2),cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgWChh(2),cplcgAgWCVWp,              & 
& cplcgZgWCVWp,cplcgZgZhh(2),cplcgWpgZVWp,cplcgWCgZcVWp,cplhhhhhh(2,2,2),cplhhHpcHp(2),  & 
& cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplhhVZVZ(2),cplHpcVWpVP,cplHpcVWpVZ,    & 
& cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhcVWpVWp1,cplhhhhcVWpVWp1(2,2),& 
& cplhhHpcVWpVP1(2),cplhhHpcVWpVZ1(2),cplhhcHpVPVWp1(2),cplhhcHpVWpVZ1(2),               & 
& cplHpcHpcVWpVWp1,cplcVWpcVWpVWpVWp1Q,cplcVWpcVWpVWpVWp2Q,cplcVWpcVWpVWpVWp3Q,          & 
& cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q,cplcVWpVWpVZVZ1Q

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
Mex1 = Mhh(gt1) 
Mex2 = MVWp 
Mex3 = MVWp 


! {Ah, Ah, Hp}
If ((Include_in_loopAh).and.(Include_in_loopAh).and.(Include_in_loopHp)) Then 
ML1 = MAh 
ML2 = MAh 
ML3 = MHp 
coup1 = cplAhAhhh(gt1)
coup2 = cplAhHpcVWp
coup3 = -cplAhcHpVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Ah, VZ, Hp}
If ((Include_in_loopAh).and.(Include_in_loopVZ).and.(Include_in_loopHp)) Then 
ML1 = MAh 
ML2 = MVZ 
ML3 = MHp 
coup1 = -cplAhhhVZ(gt1)
coup2 = cplAhHpcVWp
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Fd, Fd, Fu}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopFu)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MFu(i3) 
coup1L = cplcFdFdhhL(i2,i1,gt1)
coup1R = cplcFdFdhhR(i2,i1,gt1)
coup2L = -cplcFdFucVWpR(i1,i3)
coup2R = -cplcFdFucVWpL(i1,i3)
coup3L = -cplcFuFdVWpR(i3,i2)
coup3R = -cplcFuFdVWpL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(3)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fe, Fe, Fv}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopFv)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MFv(i3) 
coup1L = cplcFeFehhL(i2,i1,gt1)
coup1R = cplcFeFehhR(i2,i1,gt1)
coup2L = -cplcFeFvcVWpR(i1,i3)
coup2R = -cplcFeFvcVWpL(i1,i3)
coup3L = -cplcFvFeVWpR(i3,i2)
coup3R = -cplcFvFeVWpL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {gP, gZ, gWp}
If ((Include_in_loopgA).and.(Include_in_loopgZ).and.(Include_in_loopgWp)) Then 
ML1 = MVP 
ML2 = MVZ 
ML3 = MVWp 
coup1 = cplcgZgAhh(gt1)
coup2 = cplcgAgWpcVWp
coup3 = cplcgWpgZVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {gWpC, gWpC, gP}
If ((Include_in_loopgWC).and.(Include_in_loopgWC).and.(Include_in_loopgA)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVP 
coup1 = cplcgWCgWChh(gt1)
coup2 = cplcgWCgAcVWp
coup3 = cplcgAgWCVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {gWpC, gWpC, gZ}
If ((Include_in_loopgWC).and.(Include_in_loopgWC).and.(Include_in_loopgZ)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVZ 
coup1 = cplcgWCgWChh(gt1)
coup2 = cplcgWCgZcVWp
coup3 = cplcgZgWCVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {gZ, gZ, gWp}
If ((Include_in_loopgZ).and.(Include_in_loopgZ).and.(Include_in_loopgWp)) Then 
ML1 = MVZ 
ML2 = MVZ 
ML3 = MVWp 
coup1 = cplcgZgZhh(gt1)
coup2 = cplcgZgWpcVWp
coup3 = cplcgWpgZVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {hh, hh, Hp}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loopHp)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = MHp 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = cplhhHpcVWp(i1)
coup3 = -cplhhcHpVWp(i2)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {hh, hh, VWp}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loopVWp)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = MVWp 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = cplhhcVWpVWp(i1)
coup3 = cplhhcVWpVWp(i2)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {VZ, Ah, Hp}
If ((Include_in_loopVZ).and.(Include_in_loopAh).and.(Include_in_loopHp)) Then 
ML1 = MVZ 
ML2 = MAh 
ML3 = MHp 
coup1 = -cplAhhhVZ(gt1)
coup2 = cplHpcVWpVZ
coup3 = -cplAhcHpVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VZ, VZ, Hp}
If ((Include_in_loopVZ).and.(Include_in_loopVZ).and.(Include_in_loopHp)) Then 
ML1 = MVZ 
ML2 = MVZ 
ML3 = MHp 
coup1 = cplhhVZVZ(gt1)
coup2 = cplHpcVWpVZ
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VZ, VZ, VWp}
If ((Include_in_loopVZ).and.(Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
ML1 = MVZ 
ML2 = MVZ 
ML3 = MVWp 
coup1 = cplhhVZVZ(gt1)
coup2 = cplcVWpVWpVZ
coup3 = cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {bar[Fu], bar[Fu], bar[Fd]}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopFd)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFuhhL(i1,i2,gt1)
coup1R = cplcFuFuhhR(i1,i2,gt1)
coup2L = cplcFdFucVWpL(i3,i1)
coup2R = cplcFdFucVWpR(i3,i1)
coup3L = cplcFuFdVWpL(i2,i3)
coup3R = cplcFuFdVWpR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(3)*AmpC 
    End Do
  End Do
End Do
End if 


! {bar[Fxv], bar[Fxv], bar[Fxe]}
If ((Include_in_loopFxv).and.(Include_in_loopFxv).and.(Include_in_loopFxe)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MFxv(i1) 
ML2 = MFxv(i2) 
ML3 = MFxe 
coup1L = cplcFxvFxvhhL(i1,i2,gt1)
coup1R = cplcFxvFxvhhR(i1,i2,gt1)
coup2L = cplcFxeFxvcVWpL(i1)
coup2R = cplcFxeFxvcVWpR(i1)
coup3L = cplcFxvFxeVWpL(i2)
coup3R = cplcFxvFxeVWpR(i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {bar[gWp], bar[gWp], bar[gP]}
If ((Include_in_loopgWp).and.(Include_in_loopgWp).and.(Include_in_loopgA)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVP 
coup1 = cplcgWpgWphh(gt1)
coup2 = cplcgAgWpcVWp
coup3 = cplcgWpgAVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {bar[gWp], bar[gWp], bar[gZ]}
If ((Include_in_loopgWp).and.(Include_in_loopgWp).and.(Include_in_loopgZ)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVZ 
coup1 = cplcgWpgWphh(gt1)
coup2 = cplcgZgWpcVWp
coup3 = cplcgWpgZVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {bar[gZ], bar[gP], bar[gWpC]}
If ((Include_in_loopgZ).and.(Include_in_loopgA).and.(Include_in_loopgWC)) Then 
ML1 = MVZ 
ML2 = MVP 
ML3 = MVWp 
coup1 = cplcgZgAhh(gt1)
coup2 = cplcgWCgZcVWp
coup3 = cplcgAgWCVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {bar[gZ], bar[gZ], bar[gWpC]}
If ((Include_in_loopgZ).and.(Include_in_loopgZ).and.(Include_in_loopgWC)) Then 
ML1 = MVZ 
ML2 = MVZ 
ML3 = MVWp 
coup1 = cplcgZgZhh(gt1)
coup2 = cplcgWCgZcVWp
coup3 = cplcgZgWCVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[Hp], Ah}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopAh)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MAh 
coup1 = cplhhHpcHp(gt1)
coup2 = -cplAhHpcVWp
coup3 = cplAhcHpVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[Hp], hh}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MHp 
ML2 = MHp 
ML3 = Mhh(i3) 
coup1 = cplhhHpcHp(gt1)
coup2 = -cplhhHpcVWp(i3)
coup3 = cplhhcHpVWp(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[Hp], conj[VWp], hh}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MHp 
ML2 = MVWp 
ML3 = Mhh(i3) 
coup1 = cplhhcHpVWp(gt1)
coup2 = -cplhhHpcVWp(i3)
coup3 = cplhhcVWpVWp(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[Hp], conj[Hp], VP}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVP 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpcVWpVP
coup3 = cplcHpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[VWp], VP}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVP 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplHpcVWpVP
coup3 = cplcVWpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[Hp], VZ}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVZ)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVZ 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpcVWpVZ
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[VWp], VZ}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVZ)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVZ 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplHpcVWpVZ
coup3 = -cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], hh}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MVWp 
ML2 = MHp 
ML3 = Mhh(i3) 
coup1 = cplhhHpcVWp(gt1)
coup2 = cplhhcVWpVWp(i3)
coup3 = cplhhcHpVWp(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[VWp], conj[VWp], hh}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MVWp 
ML2 = MVWp 
ML3 = Mhh(i3) 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplhhcVWpVWp(i3)
coup3 = cplhhcVWpVWp(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[VWp], conj[Hp], VP}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVP 
coup1 = cplhhHpcVWp(gt1)
coup2 = cplcVWpVPVWp
coup3 = cplcHpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], VP}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVP 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplcVWpVPVWp
coup3 = cplcVWpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], VZ}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVZ)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVZ 
coup1 = cplhhHpcVWp(gt1)
coup2 = -cplcVWpVWpVZ
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], VZ}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopVZ)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVZ 
coup1 = cplhhcVWpVWp(gt1)
coup2 = -cplcVWpVWpVZ
coup3 = -cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Ah, Ah}
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
ML1 = MAh 
ML2 = MAh 
coup1 = cplAhAhhh(gt1)
coup2 = cplAhAhcVWpVWp1
Call Amp_VERTEX_StoVV_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1._dp/2._dp)*AmpC 
End if 


! {hh, hh}
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = cplhhhhcVWpVWp1(i1,i2)
Call Amp_VERTEX_StoVV_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1._dp/2._dp)*AmpC 
  End Do
End Do
End if 


! {Hp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MHp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpcHpcVWpVWp1
Call Amp_VERTEX_StoVV_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplcVWpcVWpVWpVWp1Q
coup2b = coup2 
coup2 = cplcVWpcVWpVWpVWp2Q
coup2a = coup2 
coup2 = cplcVWpcVWpVWpVWp3Q
coup2c = coup2 
If (Abs(coup1)*(Abs(coup2a)+Abs(coup2b)+Abs(coup2c)) .gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology2_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2a,coup2b,coup2c,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VZ, VZ}
If ((Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
ML1 = MVZ 
ML2 = MVZ 
coup1 = cplhhVZVZ(gt1)
coup2 = cplcVWpVWpVZVZ1Q
coup2a = coup2 
coup2 = cplcVWpVWpVZVZ2Q
coup2b = coup2 
coup2 = cplcVWpVWpVZVZ3Q
coup2c = coup2 
If (Abs(coup1)*(Abs(coup2a)+Abs(coup2b)+Abs(coup2c)) .gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology2_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2a,coup2b,coup2c,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1._dp/2._dp)*AmpC 
End if 


! {hh, VWp}
If ((Include_in_loophh).and.(Include_in_loopVWp)) Then 
Do i1=1,2
ML1 = Mhh(i1) 
ML2 = MVWp 
coup1 = cplhhhhcVWpVWp1(gt1,i1)
coup2 = cplhhcVWpVWp(i1)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology3_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End Do
End if 


! {VP, Hp}
If ((Include_in_loopVP).and.(Include_in_loopHp)) Then 
ML1 = MVP 
ML2 = MHp 
coup1 = cplhhcHpVPVWp1(gt1)
coup2 = cplHpcVWpVP
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology3_VS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 


! {VZ, Hp}
If ((Include_in_loopVZ).and.(Include_in_loopHp)) Then 
ML1 = MVZ 
ML2 = MHp 
coup1 = cplhhcHpVWpVZ1(gt1)
coup2 = cplHpcVWpVZ
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology3_VS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 


! {hh, conj[VWp]}
If ((Include_in_loophh).and.(Include_in_loopVWp)) Then 
Do i1=1,2
ML1 = Mhh(i1) 
ML2 = MVWp 
coup1 = cplhhhhcVWpVWp1(gt1,i1)
coup2 = cplhhcVWpVWp(i1)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology4_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End Do
End if 


! {Hp, VP}
If ((Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MHp 
ML2 = MVP 
coup1 = cplhhHpcVWpVP1(gt1)
coup2 = cplcHpVPVWp
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology4_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 


! {Hp, VZ}
If ((Include_in_loopHp).and.(Include_in_loopVZ)) Then 
ML1 = MHp 
ML2 = MVZ 
coup1 = cplhhHpcVWpVZ1(gt1)
coup2 = cplcHpVWpVZ
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology4_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhTocVWpVWp


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTocVWpVWp(MAh,MFd,MFe,MFu,MFv,             & 
& MFxe,MFxv,Mhh,MHp,MVP,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFv2,MFxe2,MFxv2,Mhh2,              & 
& MHp2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL,          & 
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
& cplcVWpVWpVZVZ1Q,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MVP,MVWp,MVZ,MAh2,            & 
& MFd2(3),MFe2(3),MFu2(3),MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplAhhhVZ(2),cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),& 
& cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),             & 
& cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),             & 
& cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),           & 
& cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),         & 
& cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplcgZgAhh(2),cplcgWpgAVWp,cplcgWCgAcVWp,        & 
& cplcgWpgWphh(2),cplcgAgWpcVWp,cplcgZgWpcVWp,cplcgWCgWChh(2),cplcgAgWCVWp,              & 
& cplcgZgWCVWp,cplcgZgZhh(2),cplcgWpgZVWp,cplcgWCgZcVWp,cplhhhhhh(2,2,2),cplhhHpcHp(2),  & 
& cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplhhVZVZ(2),cplHpcVWpVP,cplHpcVWpVZ,    & 
& cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhcVWpVWp1,cplhhhhcVWpVWp1(2,2),& 
& cplhhHpcVWpVP1(2),cplhhHpcVWpVZ1(2),cplhhcHpVPVWp1(2),cplhhcHpVWpVZ1(2),               & 
& cplHpcHpcVWpVWp1,cplcVWpcVWpVWpVWp1Q,cplcVWpcVWpVWpVWp2Q,cplcVWpcVWpVWpVWp3Q,          & 
& cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q,cplcVWpVWpVZVZ1Q

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
Mex1 = Mhh(gt1) 
Mex2 = MVWp 
Mex3 = MVWp 


! {conj[Hp], conj[Hp], VP}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVP 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpcVWpVP
coup3 = cplcHpVPVWp
Call Amp_VERTEX_StoVV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[VWp], VP}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVP 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplHpcVWpVP
coup3 = cplcVWpVPVWp
Call Amp_VERTEX_StoVV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], VP}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVP 
coup1 = cplhhHpcVWp(gt1)
coup2 = cplcVWpVPVWp
coup3 = cplcHpVPVWp
Call Amp_VERTEX_StoVV_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], VP}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVP 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplcVWpVPVWp
coup3 = cplcVWpVPVWp
Call Amp_VERTEX_StoVV_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VP, Hp}
If ((Include_in_loopVP).and.(Include_in_loopHp)) Then 
ML1 = MVP 
ML2 = MHp 
coup1 = cplhhcHpVPVWp1(gt1)
coup2 = cplHpcVWpVP
Call Amp_VERTEX_StoVV_Topology3_VS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 


! {Hp, VP}
If ((Include_in_loopHp).and.(Include_in_loopVP)) Then 
ML1 = MHp 
ML2 = MVP 
coup1 = cplhhHpcVWpVP1(gt1)
coup2 = cplcHpVPVWp
Call Amp_VERTEX_StoVV_Topology4_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTocVWpVWp


Subroutine Amplitude_Tree_SDdiracDM_hhToVZVZ(cplhhVZVZ,Mhh,MVZ,Mhh2,MVZ2,Amp)

Implicit None

Real(dp), Intent(in) :: Mhh(2),MVZ,Mhh2(2),MVZ2

Complex(dp), Intent(in) :: cplhhVZVZ(2)

Complex(dp) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MVZ 
Mex3 = MVZ 
! Tree-Level Vertex 
coupT1 = cplhhVZVZ(gt1)
Call TreeAmp_StoVV(Mex1,Mex2,Mex3,coupT1,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1) = AmpC 
End Do
End Subroutine Amplitude_Tree_SDdiracDM_hhToVZVZ


Subroutine Gamma_Real_SDdiracDM_hhToVZVZ(MLambda,em,gs,cplhhVZVZ,Mhh,MVZ,             & 
& GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplhhVZVZ(2)

Real(dp), Intent(in) :: Mhh(2),MVZ

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2), GammarealGluon(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: Coup 
 
Do i1=1,2
Coup = cplhhVZVZ(i1)
Mex1 = Mhh(i1)
Mex2 = MVZ
Mex3 = MVZ
If (Mex1.gt.(Mex2+Mex3)) Then 
  GammarealGluon(i1) = 0._dp 
 Gammarealphoton(i1) = 0._dp 
Else 
  GammarealGluon(i1) = 0._dp 
  GammarealPhoton(i1) = 0._dp 

End if 
End Do
End Subroutine Gamma_Real_SDdiracDM_hhToVZVZ


Subroutine Amplitude_WAVE_SDdiracDM_hhToVZVZ(cplhhVZVZ,ctcplhhVZVZ,Mhh,               & 
& Mhh2,MVZ,MVZ2,Zfhh,ZfVZ,Amp)

Implicit None

Real(dp), Intent(in) :: Mhh(2),Mhh2(2),MVZ,MVZ2

Complex(dp), Intent(in) :: cplhhVZVZ(2)

Complex(dp), Intent(in) :: ctcplhhVZVZ(2)

Complex(dp), Intent(in) :: Zfhh(2,2),ZfVZ

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MVZ 
Mex3 = MVZ 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1 = ctcplhhVZVZ(gt1) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1 = ZcoupT1 + 0.5_dp*Zfhh(i1,gt1)*cplhhVZVZ(i1)
End Do


! External Field 2 
ZcoupT1 = ZcoupT1 + 0.5_dp*ZfVZ*cplhhVZVZ(gt1)


! External Field 3 
ZcoupT1 = ZcoupT1 + 0.5_dp*ZfVZ*cplhhVZVZ(gt1)


! Getting the amplitude 
Call TreeAmp_StoVV(Mex1,Mex2,Mex3,ZcoupT1,AmpC) 
Amp(:,gt1) = AmpC 
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhToVZVZ


Subroutine Amplitude_VERTEX_SDdiracDM_hhToVZVZ(MAh,MFd,MFe,MFu,MFxv,Mhh,              & 
& MHp,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MVWp2,MVZ2,cplAhAhhh,cplAhhhVZ,       & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,               & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWphh,cplcgWpgWpVZ,     & 
& cplcgWCgWChh,cplcgWCgWCVZ,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,   & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ1,cplhhhhVZVZ1,   & 
& cplhhHpcVWpVZ1,cplhhcHpVWpVZ1,cplHpcHpVZVZ1,cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q,         & 
& cplcVWpVWpVZVZ1Q,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),MFxv(2),Mhh(2),MHp,MVWp,MVZ,MAh2,MFd2(3),MFe2(3),            & 
& MFu2(3),MFxv2(2),Mhh2(2),MHp2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplAhhhVZ(2),cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVZL(3,3),     & 
& cplcFdFdVZR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFeFeVZL(3,3),               & 
& cplcFeFeVZR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFuFuVZL(3,3),               & 
& cplcFuFuVZR(3,3),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),         & 
& cplcFxvFxvVZR(2,2),cplcgWpgWphh(2),cplcgWpgWpVZ,cplcgWCgWChh(2),cplcgWCgWCVZ,          & 
& cplhhhhhh(2,2,2),cplhhHpcHp(2),cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),          & 
& cplhhVZVZ(2),cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ1,             & 
& cplhhhhVZVZ1(2,2),cplhhHpcVWpVZ1(2),cplhhcHpVWpVZ1(2),cplHpcHpVZVZ1,cplcVWpVWpVZVZ2Q,  & 
& cplcVWpVWpVZVZ3Q,cplcVWpVWpVZVZ1Q

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
Mex1 = Mhh(gt1) 
Mex2 = MVZ 
Mex3 = MVZ 


! {Ah, Ah, hh}
If ((Include_in_loopAh).and.(Include_in_loopAh).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MAh 
ML2 = MAh 
ML3 = Mhh(i3) 
coup1 = cplAhAhhh(gt1)
coup2 = cplAhhhVZ(i3)
coup3 = -cplAhhhVZ(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {Ah, VZ, hh}
If ((Include_in_loopAh).and.(Include_in_loopVZ).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MAh 
ML2 = MVZ 
ML3 = Mhh(i3) 
coup1 = -cplAhhhVZ(gt1)
coup2 = cplAhhhVZ(i3)
coup3 = cplhhVZVZ(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {Fd, Fd, Fd}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFdFdhhL(i2,i1,gt1)
coup1R = cplcFdFdhhR(i2,i1,gt1)
coup2L = -cplcFdFdVZR(i1,i3)
coup2R = -cplcFdFdVZL(i1,i3)
coup3L = -cplcFdFdVZR(i3,i2)
coup3R = -cplcFdFdVZL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fe, Fe, Fe}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFehhL(i2,i1,gt1)
coup1R = cplcFeFehhR(i2,i1,gt1)
coup2L = -cplcFeFeVZR(i1,i3)
coup2R = -cplcFeFeVZL(i1,i3)
coup3L = -cplcFeFeVZR(i3,i2)
coup3R = -cplcFeFeVZL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fu, Fu, Fu}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuhhL(i2,i1,gt1)
coup1R = cplcFuFuhhR(i2,i1,gt1)
coup2L = -cplcFuFuVZR(i1,i3)
coup2R = -cplcFuFuVZL(i1,i3)
coup3L = -cplcFuFuVZR(i3,i2)
coup3R = -cplcFuFuVZL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fxv, Fxv, Fxv}
If ((Include_in_loopFxv).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxvFxvhhL(i2,i1,gt1)
coup1R = cplcFxvFxvhhR(i2,i1,gt1)
coup2L = -cplcFxvFxvVZR(i1,i3)
coup2R = -cplcFxvFxvVZL(i1,i3)
coup3L = -cplcFxvFxvVZR(i3,i2)
coup3R = -cplcFxvFxvVZL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
    End Do
  End Do
End Do
End if 


! {gWp, gWp, gWp}
If ((Include_in_loopgWp).and.(Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWpgWphh(gt1)
coup2 = cplcgWpgWpVZ
coup3 = cplcgWpgWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
End if 


! {gWpC, gWpC, gWpC}
If ((Include_in_loopgWC).and.(Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWCgWChh(gt1)
coup2 = cplcgWCgWCVZ
coup3 = cplcgWCgWCVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
End if 


! {hh, hh, Ah}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loopAh)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = MAh 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = -cplAhhhVZ(i1)
coup3 = cplAhhhVZ(i2)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {hh, hh, VZ}
If ((Include_in_loophh).and.(Include_in_loophh).and.(Include_in_loopVZ)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
ML3 = MVZ 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = cplhhVZVZ(i1)
coup3 = cplhhVZVZ(i2)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {Hp, Hp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MHp 
coup1 = cplhhHpcHp(gt1)
coup2 = -cplHpcHpVZ
coup3 = -cplHpcHpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
End if 


! {Hp, VWp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhHpcVWp(gt1)
coup2 = -cplHpcHpVZ
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplcHpVWpVZ
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplhhHpcVWp(gt1)
coup2 = cplcHpVWpVZ
coup3 = -cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplHpcVWpVZ
coup3 = -cplHpcHpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplHpcVWpVZ
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhcHpVWp(gt1)
coup2 = -cplcVWpVWpVZ
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = -cplcVWpVWpVZ
coup3 = -cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
End if 


! {VZ, Ah, hh}
If ((Include_in_loopVZ).and.(Include_in_loopAh).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MVZ 
ML2 = MAh 
ML3 = Mhh(i3) 
coup1 = -cplAhhhVZ(gt1)
coup2 = cplhhVZVZ(i3)
coup3 = -cplAhhhVZ(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VZ, VZ, hh}
If ((Include_in_loopVZ).and.(Include_in_loopVZ).and.(Include_in_loophh)) Then 
    Do i3=1,2
ML1 = MVZ 
ML2 = MVZ 
ML3 = Mhh(i3) 
coup1 = cplhhVZVZ(gt1)
coup2 = cplhhVZVZ(i3)
coup3 = cplhhVZVZ(i3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[Hp], conj[VWp], conj[Hp]}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplHpcHpVZ
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[Hp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpcVWpVZ
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[VWp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplHpcVWpVZ
coup3 = cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = cplhhHpcVWp(gt1)
coup2 = cplcHpVWpVZ
coup3 = cplHpcHpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplcHpVWpVZ
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], conj[VWp]}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcVWp(gt1)
coup2 = cplcVWpVWpVZ
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Ah, Ah}
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
ML1 = MAh 
ML2 = MAh 
coup1 = cplAhAhhh(gt1)
coup2 = cplAhAhVZVZ1
Call Amp_VERTEX_StoVV_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1._dp/2._dp)*AmpC 
End if 


! {hh, hh}
If ((Include_in_loophh).and.(Include_in_loophh)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = Mhh(i2) 
coup1 = cplhhhhhh(gt1,i1,i2)
coup2 = cplhhhhVZVZ1(i1,i2)
Call Amp_VERTEX_StoVV_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1._dp/2._dp)*AmpC 
  End Do
End Do
End if 


! {Hp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MHp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpcHpVZVZ1
Call Amp_VERTEX_StoVV_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplcVWpVWpVZVZ1Q
coup2a = coup2 
coup2 = cplcVWpVWpVZVZ2Q
coup2b = coup2 
coup2 = cplcVWpVWpVZVZ3Q
coup2c = coup2 
If (Abs(coup1)*(Abs(coup2a)+Abs(coup2b)+Abs(coup2c)) .gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology2_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2a,coup2b,coup2c,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {hh, VZ}
If ((Include_in_loophh).and.(Include_in_loopVZ)) Then 
Do i1=1,2
ML1 = Mhh(i1) 
ML2 = MVZ 
coup1 = cplhhhhVZVZ1(gt1,i1)
coup2 = cplhhVZVZ(i1)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology3_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End Do
End if 


! {Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
coup1 = cplhhHpcVWpVZ1(gt1)
coup2 = cplcHpVWpVZ
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology3_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 


! {VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
coup1 = cplhhcHpVWpVZ1(gt1)
coup2 = cplHpcVWpVZ
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology3_VS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 


! {hh, VZ}
If ((Include_in_loophh).and.(Include_in_loopVZ)) Then 
Do i1=1,2
ML1 = Mhh(i1) 
ML2 = MVZ 
coup1 = cplhhhhVZVZ1(gt1,i1)
coup2 = cplhhVZVZ(i1)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology4_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End Do
End if 


! {Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
coup1 = cplhhHpcVWpVZ1(gt1)
coup2 = cplcHpVWpVZ
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology4_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 


! {VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
coup1 = cplhhcHpVWpVZ1(gt1)
coup2 = cplHpcVWpVZ
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology4_VS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhToVZVZ


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToVZVZ(MAh,MFd,MFe,MFu,MFxv,               & 
& Mhh,MHp,MVWp,MVZ,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MVWp2,MVZ2,cplAhAhhh,             & 
& cplAhhhVZ,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,     & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWphh,cplcgWpgWpVZ,     & 
& cplcgWCgWChh,cplcgWCgWCVZ,cplhhhhhh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,   & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ1,cplhhhhVZVZ1,   & 
& cplhhHpcVWpVZ1,cplhhcHpVWpVZ1,cplHpcHpVZVZ1,cplcVWpVWpVZVZ2Q,cplcVWpVWpVZVZ3Q,         & 
& cplcVWpVWpVZVZ1Q,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),MFxv(2),Mhh(2),MHp,MVWp,MVZ,MAh2,MFd2(3),MFe2(3),            & 
& MFu2(3),MFxv2(2),Mhh2(2),MHp2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplAhhhVZ(2),cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVZL(3,3),     & 
& cplcFdFdVZR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFeFeVZL(3,3),               & 
& cplcFeFeVZR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFuFuVZL(3,3),               & 
& cplcFuFuVZR(3,3),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),         & 
& cplcFxvFxvVZR(2,2),cplcgWpgWphh(2),cplcgWpgWpVZ,cplcgWCgWChh(2),cplcgWCgWCVZ,          & 
& cplhhhhhh(2,2,2),cplhhHpcHp(2),cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),          & 
& cplhhVZVZ(2),cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,cplAhAhVZVZ1,             & 
& cplhhhhVZVZ1(2,2),cplhhHpcVWpVZ1(2),cplhhcHpVWpVZ1(2),cplHpcHpVZVZ1,cplcVWpVWpVZVZ2Q,  & 
& cplcVWpVWpVZVZ3Q,cplcVWpVWpVZVZ1Q

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
Mex1 = Mhh(gt1) 
Mex2 = MVZ 
Mex3 = MVZ 
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToVZVZ


Subroutine Amplitude_WAVE_SDdiracDM_hhToAhhh(MAh,MAh2,Mhh,Mhh2,ZfAh,Zfhh,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MAh2,Mhh(2),Mhh2(2)

Complex(dp), Intent(in) :: ZfAh,Zfhh(2,2)

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
    Do gt3=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MAh 
Mex3 = Mhh(gt3) 
ZcoupT1 = 0._dp 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
! Vanishing 


! External Field 2 
! Vanishing 


! External Field 3 
! Vanishing 
Amp(gt1, gt3) = 0._dp
    End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhToAhhh


Subroutine Amplitude_VERTEX_SDdiracDM_hhToAhhh(MAh,MFd,MFe,MFu,MFxv,Mhh,              & 
& MHp,MVWp,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MVWp2,cplcFdFdAhL,cplcFdFdAhR,            & 
& cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplcFxvFxvAhL,cplcFxvFxvAhR,           & 
& cplcgWpgWpAh,cplcgWCgWCAh,cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL,cplcFdFdhhR,             & 
& cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,cplcFxvFxvhhL,cplcFxvFxvhhR,           & 
& cplcgWpgWphh,cplcgWCgWChh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),MFxv(2),Mhh(2),MHp,MVWp,MAh2,MFd2(3),MFe2(3),MFu2(3),        & 
& MFxv2(2),Mhh2(2),MHp2,MVWp2

Complex(dp), Intent(in) :: cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFuFuAhL(3,3), & 
& cplcFuFuAhR(3,3),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplcgWpgWpAh,cplcgWCgWCAh,      & 
& cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFeFehhL(3,3,2),      & 
& cplcFeFehhR(3,3,2),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFxvFxvhhL(2,2,2),         & 
& cplcFxvFxvhhR(2,2,2),cplcgWpgWphh(2),cplcgWCgWChh(2),cplhhHpcHp(2),cplhhHpcVWp(2),     & 
& cplhhcHpVWp(2),cplhhcVWpVWp(2)

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
    Do gt3=1,2
Amp(gt1, gt3) = 0._dp 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MAh 
Mex3 = Mhh(gt3) 


! {Fd, Fd, Fd}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFdFdhhL(i2,i1,gt1)
coup1R = cplcFdFdhhR(i2,i1,gt1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFdFdhhL(i3,i2,gt3)
coup3R = cplcFdFdhhR(i3,i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt3) = Amp(gt1, gt3) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fe, Fe, Fe}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFehhL(i2,i1,gt1)
coup1R = cplcFeFehhR(i2,i1,gt1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFeFehhL(i3,i2,gt3)
coup3R = cplcFeFehhR(i3,i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt3) = Amp(gt1, gt3) + oo16pi2*(2)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fu, Fu, Fu}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuhhL(i2,i1,gt1)
coup1R = cplcFuFuhhR(i2,i1,gt1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFuFuhhL(i3,i2,gt3)
coup3R = cplcFuFuhhR(i3,i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt3) = Amp(gt1, gt3) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fxv, Fxv, Fxv}
If ((Include_in_loopFxv).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxvFxvhhL(i2,i1,gt1)
coup1R = cplcFxvFxvhhR(i2,i1,gt1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFxvhhL(i3,i2,gt3)
coup3R = cplcFxvFxvhhR(i3,i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt3) = Amp(gt1, gt3) + oo16pi2*(2)*AmpC 
    End Do
  End Do
End Do
End if 


! {gWp, gWp, gWp}
If ((Include_in_loopgWp).and.(Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWpgWphh(gt1)
coup2 = cplcgWpgWpAh
coup3 = cplcgWpgWphh(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt3) = Amp(gt1, gt3) + oo16pi2*(2)*AmpC 
End if 


! {gWpC, gWpC, gWpC}
If ((Include_in_loopgWC).and.(Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWCgWChh(gt1)
coup2 = cplcgWCgWCAh
coup3 = cplcgWCgWChh(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt3) = Amp(gt1, gt3) + oo16pi2*(2)*AmpC 
End if 


! {Hp, Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = -cplAhcHpVWp
coup3 = cplhhHpcVWp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt3) = Amp(gt1, gt3) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = -cplAhcHpVWp
coup3 = cplhhcVWpVWp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt3) = Amp(gt1, gt3) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = -cplAhHpcVWp
coup3 = cplhhHpcHp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt3) = Amp(gt1, gt3) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = -cplAhHpcVWp
coup3 = cplhhcHpVWp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt3) = Amp(gt1, gt3) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[Hp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = -cplAhHpcVWp
coup3 = cplhhcHpVWp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt3) = Amp(gt1, gt3) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[VWp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = -cplAhHpcVWp
coup3 = cplhhcVWpVWp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt3) = Amp(gt1, gt3) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = -cplAhcHpVWp
coup3 = cplhhHpcHp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt3) = Amp(gt1, gt3) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = -cplAhcHpVWp
coup3 = cplhhHpcVWp(gt3)
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt3) = Amp(gt1, gt3) + oo16pi2*(1)*AmpC 
End if 
    End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhToAhhh


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToAhhh(MAh,MFd,MFe,MFu,MFxv,               & 
& Mhh,MHp,MVWp,MAh2,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MVWp2,cplcFdFdAhL,cplcFdFdAhR,        & 
& cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplcFxvFxvAhL,cplcFxvFxvAhR,           & 
& cplcgWpgWpAh,cplcgWCgWCAh,cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL,cplcFdFdhhR,             & 
& cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,cplcFxvFxvhhL,cplcFxvFxvhhR,           & 
& cplcgWpgWphh,cplcgWCgWChh,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),MFxv(2),Mhh(2),MHp,MVWp,MAh2,MFd2(3),MFe2(3),MFu2(3),        & 
& MFxv2(2),Mhh2(2),MHp2,MVWp2

Complex(dp), Intent(in) :: cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFuFuAhL(3,3), & 
& cplcFuFuAhR(3,3),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplcgWpgWpAh,cplcgWCgWCAh,      & 
& cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFeFehhL(3,3,2),      & 
& cplcFeFehhR(3,3,2),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFxvFxvhhL(2,2,2),         & 
& cplcFxvFxvhhR(2,2,2),cplcgWpgWphh(2),cplcgWCgWChh(2),cplhhHpcHp(2),cplhhHpcVWp(2),     & 
& cplhhcHpVWp(2),cplhhcVWpVWp(2)

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
    Do gt3=1,2
Amp(gt1, gt3) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MAh 
Mex3 = Mhh(gt3) 
    End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToAhhh


Subroutine Amplitude_WAVE_SDdiracDM_hhToAhVP(cplAhhhVZ,ctcplAhhhVZ,MAh,               & 
& MAh2,Mhh,Mhh2,MVP,MVP2,MVZ,MVZ2,ZfAh,Zfhh,ZfVP,ZfVZVP,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MAh2,Mhh(2),Mhh2(2),MVP,MVP2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplAhhhVZ(2)

Complex(dp), Intent(in) :: ctcplAhhhVZ(2)

Complex(dp), Intent(in) :: ZfAh,Zfhh(2,2),ZfVP,ZfVZVP

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MAh 
Mex3 = MVP 
ZcoupT1 = 0._dp 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
! Vanishing 


! External Field 2 
! Vanishing 


! External Field 3 
ZcoupT1 = ZcoupT1 + 0.5_dp*ZfVZVP*cplAhhhVZ(gt1)


! Getting the amplitude 
Call TreeAmp_StoSV(Mex1,Mex2,Mex3,ZcoupT1,AmpC) 
Amp(:,gt1) = AmpC 
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhToAhVP


Subroutine Amplitude_VERTEX_SDdiracDM_hhToAhVP(MAh,MFd,MFe,MFu,Mhh,MHp,               & 
& MVP,MVWp,MAh2,MFd2,MFe2,MFu2,Mhh2,MHp2,MVP2,MVWp2,cplcFdFdAhL,cplcFdFdAhR,             & 
& cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplcgWpgWpAh,cplcgWCgWCAh,             & 
& cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVPL,cplcFdFdVPR,               & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuhhL,cplcFuFuhhR,               & 
& cplcFuFuVPL,cplcFuFuVPR,cplcgWpgWphh,cplcgWpgWpVP,cplcgWCgWChh,cplcgWCgWCVP,           & 
& cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,    & 
& cplcVWpVPVWp,cplAhHpcVWpVP1,cplAhcHpVPVWp1,cplhhHpcVWpVP1,cplhhcHpVPVWp1,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),Mhh(2),MHp,MVP,MVWp,MAh2,MFd2(3),MFe2(3),MFu2(3),            & 
& Mhh2(2),MHp2,MVP2,MVWp2

Complex(dp), Intent(in) :: cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFuFuAhL(3,3), & 
& cplcFuFuAhR(3,3),cplcgWpgWpAh,cplcgWCgWCAh,cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL(3,3,2), & 
& cplcFdFdhhR(3,3,2),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFeFehhL(3,3,2),               & 
& cplcFeFehhR(3,3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFuFuhhL(3,3,2),               & 
& cplcFuFuhhR(3,3,2),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcgWpgWphh(2),cplcgWpgWpVP,     & 
& cplcgWCgWChh(2),cplcgWCgWCVP,cplhhHpcHp(2),cplhhHpcVWp(2),cplhhcHpVWp(2),              & 
& cplhhcVWpVWp(2),cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,cplAhHpcVWpVP1,        & 
& cplAhcHpVPVWp1,cplhhHpcVWpVP1(2),cplhhcHpVPVWp1(2)

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
Mex1 = Mhh(gt1) 
Mex2 = MAh 
Mex3 = MVP 


! {Fd, Fd, Fd}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFdFdhhL(i2,i1,gt1)
coup1R = cplcFdFdhhR(i2,i1,gt1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = -cplcFdFdVPR(i3,i2)
coup3R = -cplcFdFdVPL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fe, Fe, Fe}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFehhL(i2,i1,gt1)
coup1R = cplcFeFehhR(i2,i1,gt1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = -cplcFeFeVPR(i3,i2)
coup3R = -cplcFeFeVPL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fu, Fu, Fu}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuhhL(i2,i1,gt1)
coup1R = cplcFuFuhhR(i2,i1,gt1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = -cplcFuFuVPR(i3,i2)
coup3R = -cplcFuFuVPL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {gWp, gWp, gWp}
If ((Include_in_loopgWp).and.(Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWpgWphh(gt1)
coup2 = cplcgWpgWpAh
coup3 = cplcgWpgWpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
End if 


! {gWpC, gWpC, gWpC}
If ((Include_in_loopgWC).and.(Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWCgWChh(gt1)
coup2 = cplcgWCgWCAh
coup3 = cplcgWCgWCVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
End if 


! {Hp, Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplAhcHpVWp
coup3 = cplHpcVWpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplAhcHpVWp
coup3 = cplcVWpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplAhHpcVWp
coup3 = -cplHpcHpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplAhHpcVWp
coup3 = cplcHpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[Hp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplAhHpcVWp
coup3 = cplcHpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[VWp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplAhHpcVWp
coup3 = -cplcVWpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplAhcHpVWp
coup3 = cplHpcHpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplAhcHpVWp
coup3 = cplHpcVWpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
coup1 = cplhhHpcVWp(gt1)
coup2 = cplAhcHpVPVWp1
Call Amp_VERTEX_StoSV_Topology2_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplAhHpcVWpVP1
Call Amp_VERTEX_StoSV_Topology2_VS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
coup1 = cplhhHpcVWpVP1(gt1)
coup2 = cplAhcHpVWp
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology3_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 


! {VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
coup1 = cplhhcHpVPVWp1(gt1)
coup2 = cplAhHpcVWp
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology3_VS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhToAhVP


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToAhVP(MAh,MFd,MFe,MFu,Mhh,MHp,            & 
& MVP,MVWp,MAh2,MFd2,MFe2,MFu2,Mhh2,MHp2,MVP2,MVWp2,cplcFdFdAhL,cplcFdFdAhR,             & 
& cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplcgWpgWpAh,cplcgWCgWCAh,             & 
& cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVPL,cplcFdFdVPR,               & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuhhL,cplcFuFuhhR,               & 
& cplcFuFuVPL,cplcFuFuVPR,cplcgWpgWphh,cplcgWpgWpVP,cplcgWCgWChh,cplcgWCgWCVP,           & 
& cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,    & 
& cplcVWpVPVWp,cplAhHpcVWpVP1,cplAhcHpVPVWp1,cplhhHpcVWpVP1,cplhhcHpVPVWp1,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFe(3),MFu(3),Mhh(2),MHp,MVP,MVWp,MAh2,MFd2(3),MFe2(3),MFu2(3),            & 
& Mhh2(2),MHp2,MVP2,MVWp2

Complex(dp), Intent(in) :: cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFuFuAhL(3,3), & 
& cplcFuFuAhR(3,3),cplcgWpgWpAh,cplcgWCgWCAh,cplAhHpcVWp,cplAhcHpVWp,cplcFdFdhhL(3,3,2), & 
& cplcFdFdhhR(3,3,2),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFeFehhL(3,3,2),               & 
& cplcFeFehhR(3,3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFuFuhhL(3,3,2),               & 
& cplcFuFuhhR(3,3,2),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcgWpgWphh(2),cplcgWpgWpVP,     & 
& cplcgWCgWChh(2),cplcgWCgWCVP,cplhhHpcHp(2),cplhhHpcVWp(2),cplhhcHpVWp(2),              & 
& cplhhcVWpVWp(2),cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,cplAhHpcVWpVP1,        & 
& cplAhcHpVPVWp1,cplhhHpcVWpVP1(2),cplhhcHpVPVWp1(2)

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
Mex1 = Mhh(gt1) 
Mex2 = MAh 
Mex3 = MVP 
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToAhVP


Subroutine Amplitude_WAVE_SDdiracDM_hhToFvcFv(MFv,MFv2,Mhh,Mhh2,Zfhh,ZfVL,ZfVR,Amp)

Implicit None

Real(dp), Intent(in) :: MFv(3),MFv2(3),Mhh(2),Mhh2(2)

Complex(dp), Intent(in) :: Zfhh(2,2),ZfVL(3,3),ZfVR(3,3)

Complex(dp), Intent(out) :: Amp(2,2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,3
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MFv(gt3) 
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
! Vanishing 
Amp(:,gt1, gt2, gt3) = 0._dp
    End Do
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhToFvcFv


Subroutine Amplitude_VERTEX_SDdiracDM_hhToFvcFv(MFe,MFv,MFxv,Mhh,MHp,MSsc,            & 
& MVWp,MVZ,MFe2,MFv2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFeFehhL,cplcFeFehhR,           & 
& cplcFvFeHpL,cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFxvFvSscL,cplcFxvFvSscR,         & 
& cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,         & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplhhHpcHp,cplhhHpcVWp,      & 
& cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFv(3),MFxv(2),Mhh(2),MHp,MSsc(2),MVWp,MVZ,MFe2(3),MFv2(3),MFxv2(2),           & 
& Mhh2(2),MHp2,MSsc2(2),MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),              & 
& cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),         & 
& cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),& 
& cplcFeFvcVWpR(3,3),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFvFxvcSscL(3,2,2),    & 
& cplcFvFxvcSscR(3,2,2),cplhhHpcHp(2),cplhhHpcVWp(2),cplhhSsccSsc(2,2,2),cplhhcHpVWp(2), & 
& cplhhcVWpVWp(2),cplhhVZVZ(2)

Complex(dp), Intent(out) :: Amp(2,2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,3
Amp(:,gt1, gt2, gt3) = 0._dp 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MFv(gt3) 


! {Fe, Fe, conj[Hp]}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopHp)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MHp 
coup1L = cplcFeFehhL(i2,i1,gt1)
coup1R = cplcFeFehhR(i2,i1,gt1)
coup2L = cplcFeFvcHpL(i1,gt2)
coup2R = cplcFeFvcHpR(i1,gt2)
coup3L = cplcFvFeHpL(gt3,i2)
coup3R = cplcFvFeHpR(gt3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {Fe, Fe, conj[VWp]}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopVWp)) Then 
Do i1=1,3
  Do i2=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MVWp 
coup1L = cplcFeFehhL(i2,i1,gt1)
coup1R = cplcFeFehhR(i2,i1,gt1)
coup2L = -cplcFeFvcVWpR(i1,gt2)
coup2R = -cplcFeFvcVWpL(i1,gt2)
coup3L = -cplcFvFeVWpR(gt3,i2)
coup3R = -cplcFvFeVWpL(gt3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {Fxv, Fxv, Ssc}
If ((Include_in_loopFxv).and.(Include_in_loopFxv).and.(Include_in_loopSsc)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = MFxv(i2) 
ML3 = MSsc(i3) 
coup1L = cplcFxvFxvhhL(i2,i1,gt1)
coup1R = cplcFxvFxvhhR(i2,i1,gt1)
coup2L = cplcFxvFvSscL(i1,gt2,i3)
coup2R = cplcFxvFvSscR(i1,gt2,i3)
coup3L = cplcFvFxvcSscL(gt3,i2,i3)
coup3R = cplcFvFxvcSscR(gt3,i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {Hp, Hp, bar[Fe]}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopFe)) Then 
    Do i3=1,3
ML1 = MHp 
ML2 = MHp 
ML3 = MFe(i3) 
coup1 = cplhhHpcHp(gt1)
coup2L = cplcFeFvcHpL(i3,gt2)
coup2R = cplcFeFvcHpR(i3,gt2)
coup3L = cplcFvFeHpL(gt3,i3)
coup3R = cplcFvFeHpR(gt3,i3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {Hp, VWp, bar[Fe]}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopFe)) Then 
    Do i3=1,3
ML1 = MHp 
ML2 = MVWp 
ML3 = MFe(i3) 
coup1 = cplhhHpcVWp(gt1)
coup2L = cplcFeFvcHpL(i3,gt2)
coup2R = cplcFeFvcHpR(i3,gt2)
coup3L = -cplcFvFeVWpR(gt3,i3)
coup3R = -cplcFvFeVWpL(gt3,i3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VWp, Hp, bar[Fe]}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopFe)) Then 
    Do i3=1,3
ML1 = MVWp 
ML2 = MHp 
ML3 = MFe(i3) 
coup1 = cplhhcHpVWp(gt1)
coup2L = -cplcFeFvcVWpR(i3,gt2)
coup2R = -cplcFeFvcVWpL(i3,gt2)
coup3L = cplcFvFeHpL(gt3,i3)
coup3R = cplcFvFeHpR(gt3,i3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VWp, VWp, bar[Fe]}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopFe)) Then 
    Do i3=1,3
ML1 = MVWp 
ML2 = MVWp 
ML3 = MFe(i3) 
coup1 = cplhhcVWpVWp(gt1)
coup2L = -cplcFeFvcVWpR(i3,gt2)
coup2R = -cplcFeFvcVWpL(i3,gt2)
coup3L = -cplcFvFeVWpR(gt3,i3)
coup3R = -cplcFvFeVWpL(gt3,i3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VZ, VZ, bar[Fv]}
If ((Include_in_loopVZ).and.(Include_in_loopVZ).and.(Include_in_loopFv)) Then 
    Do i3=1,3
ML1 = MVZ 
ML2 = MVZ 
ML3 = MFv(i3) 
coup1 = cplhhVZVZ(gt1)
coup2L = -cplcFvFvVZR(i3,gt2)
coup2R = -cplcFvFvVZL(i3,gt2)
coup3L = -cplcFvFvVZR(gt3,i3)
coup3R = -cplcFvFvVZL(gt3,i3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[Ssc], conj[Ssc], bar[Fxv]}
If ((Include_in_loopSsc).and.(Include_in_loopSsc).and.(Include_in_loopFxv)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = MSsc(i1) 
ML2 = MSsc(i2) 
ML3 = MFxv(i3) 
coup1 = cplhhSsccSsc(gt1,i2,i1)
coup2L = cplcFxvFvSscL(i3,gt2,i1)
coup2R = cplcFxvFvSscR(i3,gt2,i1)
coup3L = cplcFvFxvcSscL(gt3,i3,i2)
coup3R = cplcFvFxvcSscR(gt3,i3,i2)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 
    End Do
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhToFvcFv


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToFvcFv(MFe,MFv,MFxv,Mhh,MHp,              & 
& MSsc,MVWp,MVZ,MFe2,MFv2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFeFehhL,cplcFeFehhR,      & 
& cplcFvFeHpL,cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFxvFvSscL,cplcFxvFvSscR,         & 
& cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,         & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFvFxvcSscL,cplcFvFxvcSscR,cplhhHpcHp,cplhhHpcVWp,      & 
& cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFv(3),MFxv(2),Mhh(2),MHp,MSsc(2),MVWp,MVZ,MFe2(3),MFv2(3),MFxv2(2),           & 
& Mhh2(2),MHp2,MSsc2(2),MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),              & 
& cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),         & 
& cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),& 
& cplcFeFvcVWpR(3,3),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFvFxvcSscL(3,2,2),    & 
& cplcFvFxvcSscR(3,2,2),cplhhHpcHp(2),cplhhHpcVWp(2),cplhhSsccSsc(2,2,2),cplhhcHpVWp(2), & 
& cplhhcVWpVWp(2),cplhhVZVZ(2)

Complex(dp), Intent(out) :: Amp(2,2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,3
Amp(:,gt1, gt2, gt3) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MFv(gt3) 
    End Do
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToFvcFv


Subroutine Amplitude_WAVE_SDdiracDM_hhToFxecFxe(MFxe,MFxe2,Mhh,Mhh2,Zfed,             & 
& Zfeu,Zfhh,Amp)

Implicit None

Real(dp), Intent(in) :: MFxe,MFxe2,Mhh(2),Mhh2(2)

Complex(dp), Intent(in) :: Zfed,Zfeu,Zfhh(2,2)

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MFxe 
Mex3 = MFxe 
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
! Vanishing 
Amp(:,gt1) = 0._dp
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhToFxecFxe


Subroutine Amplitude_VERTEX_SDdiracDM_hhToFxecFxe(MFe,MFxe,MFxv,Mhh,MHp,              & 
& MSsc,MVWp,MVZ,MFe2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFeFehhL,cplcFeFehhR,     & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR, & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,               & 
& cplcFxvFxvhhR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,           & 
& cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,cplhhVZVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVWp,MVZ,MFe2(3),MFxe2,MFxv2(2),               & 
& Mhh2(2),MHp2,MSsc2(2),MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),          & 
& cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxeFxeVZL,   & 
& cplcFxeFxeVZR,cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFxvFxvhhL(2,2,2),            & 
& cplcFxvFxvhhR(2,2,2),cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),           & 
& cplcFxeFxvcVWpR(2),cplhhHpcHp(2),cplhhHpcVWp(2),cplhhSsccSsc(2,2,2),cplhhcHpVWp(2),    & 
& cplhhcVWpVWp(2),cplhhVZVZ(2)

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
Mex1 = Mhh(gt1) 
Mex2 = MFxe 
Mex3 = MFxe 


! {Fe, Fe, conj[Ssc]}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopSsc)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,2
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MSsc(i3) 
coup1L = cplcFeFehhL(i2,i1,gt1)
coup1R = cplcFeFehhR(i2,i1,gt1)
coup2L = cplcFeFxecSscL(i1,i3)
coup2R = cplcFeFxecSscR(i1,i3)
coup3L = cplcFxeFeSscL(i2,i3)
coup3R = cplcFxeFeSscR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fxv, Fxv, Hp}
If ((Include_in_loopFxv).and.(Include_in_loopFxv).and.(Include_in_loopHp)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MFxv(i1) 
ML2 = MFxv(i2) 
ML3 = MHp 
coup1L = cplcFxvFxvhhL(i2,i1,gt1)
coup1R = cplcFxvFxvhhR(i2,i1,gt1)
coup2L = cplcFxvFxeHpL(i1)
coup2R = cplcFxvFxeHpR(i1)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {Fxv, Fxv, VWp}
If ((Include_in_loopFxv).and.(Include_in_loopFxv).and.(Include_in_loopVWp)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = MFxv(i1) 
ML2 = MFxv(i2) 
ML3 = MVWp 
coup1L = cplcFxvFxvhhL(i2,i1,gt1)
coup1R = cplcFxvFxvhhR(i2,i1,gt1)
coup2L = cplcFxvFxeVWpL(i1)
coup2R = cplcFxvFxeVWpR(i1)
coup3L = cplcFxeFxvcVWpL(i2)
coup3R = cplcFxeFxvcVWpR(i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {Ssc, Ssc, bar[Fe]}
If ((Include_in_loopSsc).and.(Include_in_loopSsc).and.(Include_in_loopFe)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,3
ML1 = MSsc(i1) 
ML2 = MSsc(i2) 
ML3 = MFe(i3) 
coup1 = cplhhSsccSsc(gt1,i1,i2)
coup2L = cplcFeFxecSscL(i3,i1)
coup2R = cplcFeFxecSscR(i3,i1)
coup3L = cplcFxeFeSscL(i3,i2)
coup3R = cplcFxeFeSscR(i3,i2)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {VZ, VZ, bar[Fxe]}
If ((Include_in_loopVZ).and.(Include_in_loopVZ).and.(Include_in_loopFxe)) Then 
ML1 = MVZ 
ML2 = MVZ 
ML3 = MFxe 
coup1 = cplhhVZVZ(gt1)
coup2L = cplcFxeFxeVZL
coup2R = cplcFxeFxeVZR
coup3L = cplcFxeFxeVZL
coup3R = cplcFxeFxeVZR
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[Hp], bar[Fxv]}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopFxv)) Then 
    Do i3=1,2
ML1 = MHp 
ML2 = MHp 
ML3 = MFxv(i3) 
coup1 = cplhhHpcHp(gt1)
coup2L = cplcFxvFxeHpL(i3)
coup2R = cplcFxvFxeHpR(i3)
coup3L = cplcFxeFxvcHpL(i3)
coup3R = cplcFxeFxvcHpR(i3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[Hp], conj[VWp], bar[Fxv]}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopFxv)) Then 
    Do i3=1,2
ML1 = MHp 
ML2 = MVWp 
ML3 = MFxv(i3) 
coup1 = cplhhcHpVWp(gt1)
coup2L = cplcFxvFxeHpL(i3)
coup2R = cplcFxvFxeHpR(i3)
coup3L = cplcFxeFxvcVWpL(i3)
coup3R = cplcFxeFxvcVWpR(i3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[VWp], conj[Hp], bar[Fxv]}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopFxv)) Then 
    Do i3=1,2
ML1 = MVWp 
ML2 = MHp 
ML3 = MFxv(i3) 
coup1 = cplhhHpcVWp(gt1)
coup2L = cplcFxvFxeVWpL(i3)
coup2R = cplcFxvFxeVWpR(i3)
coup3L = cplcFxeFxvcHpL(i3)
coup3R = cplcFxeFxvcHpR(i3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[VWp], conj[VWp], bar[Fxv]}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopFxv)) Then 
    Do i3=1,2
ML1 = MVWp 
ML2 = MVWp 
ML3 = MFxv(i3) 
coup1 = cplhhcVWpVWp(gt1)
coup2L = cplcFxvFxeVWpL(i3)
coup2R = cplcFxvFxeVWpR(i3)
coup3L = cplcFxeFxvcVWpL(i3)
coup3R = cplcFxeFxvcVWpR(i3)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_VVF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
End if 
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhToFxecFxe


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToFxecFxe(MFe,MFxe,MFxv,Mhh,               & 
& MHp,MSsc,MVWp,MVZ,MFe2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFeFehhL,             & 
& cplcFeFehhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,    & 
& cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,              & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,             & 
& cplcFxeFxvcVWpR,cplhhHpcHp,cplhhHpcVWp,cplhhSsccSsc,cplhhcHpVWp,cplhhcVWpVWp,          & 
& cplhhVZVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVWp,MVZ,MFe2(3),MFxe2,MFxv2(2),               & 
& Mhh2(2),MHp2,MSsc2(2),MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),          & 
& cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxeFxeVZL,   & 
& cplcFxeFxeVZR,cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFxvFxvhhL(2,2,2),            & 
& cplcFxvFxvhhR(2,2,2),cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),           & 
& cplcFxeFxvcVWpR(2),cplhhHpcHp(2),cplhhHpcVWp(2),cplhhSsccSsc(2,2,2),cplhhcHpVWp(2),    & 
& cplhhcVWpVWp(2),cplhhVZVZ(2)

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
Mex1 = Mhh(gt1) 
Mex2 = MFxe 
Mex3 = MFxe 
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToFxecFxe


Subroutine Amplitude_WAVE_SDdiracDM_hhTohhVP(Mhh,Mhh2,MVP,MVP2,Zfhh,ZfVP,Amp)

Implicit None

Real(dp), Intent(in) :: Mhh(2),Mhh2(2),MVP,MVP2

Complex(dp), Intent(in) :: Zfhh(2,2),ZfVP

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
Mex1 = Mhh(gt1) 
Mex2 = Mhh(gt2) 
Mex3 = MVP 
ZcoupT1 = 0._dp 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
! Vanishing 


! External Field 2 
! Vanishing 


! External Field 3 
Amp(:,gt1, gt2) = 0._dp
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhTohhVP


Subroutine Amplitude_VERTEX_SDdiracDM_hhTohhVP(MFd,MFe,MFu,Mhh,MHp,MVP,               & 
& MVWp,MFd2,MFe2,MFu2,Mhh2,MHp2,MVP2,MVWp2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVPL,          & 
& cplcFdFdVPR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuhhL,               & 
& cplcFuFuhhR,cplcFuFuVPL,cplcFuFuVPR,cplcgWpgWphh,cplcgWpgWpVP,cplcgWCgWChh,            & 
& cplcgWCgWCVP,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplHpcHpVP,               & 
& cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,cplhhHpcVWpVP1,cplhhcHpVPVWp1,Amp)

Implicit None

Real(dp), Intent(in) :: MFd(3),MFe(3),MFu(3),Mhh(2),MHp,MVP,MVWp,MFd2(3),MFe2(3),MFu2(3),Mhh2(2),             & 
& MHp2,MVP2,MVWp2

Complex(dp), Intent(in) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),              & 
& cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),               & 
& cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),               & 
& cplcgWpgWphh(2),cplcgWpgWpVP,cplcgWCgWChh(2),cplcgWCgWCVP,cplhhHpcHp(2),               & 
& cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,      & 
& cplcVWpVPVWp,cplhhHpcVWpVP1(2),cplhhcHpVPVWp1(2)

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
Mex1 = Mhh(gt1) 
Mex2 = Mhh(gt2) 
Mex3 = MVP 


! {Fd, Fd, Fd}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFdFdhhL(i2,i1,gt1)
coup1R = cplcFdFdhhR(i2,i1,gt1)
coup2L = cplcFdFdhhL(i1,i3,gt2)
coup2R = cplcFdFdhhR(i1,i3,gt2)
coup3L = -cplcFdFdVPR(i3,i2)
coup3R = -cplcFdFdVPL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fe, Fe, Fe}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFehhL(i2,i1,gt1)
coup1R = cplcFeFehhR(i2,i1,gt1)
coup2L = cplcFeFehhL(i1,i3,gt2)
coup2R = cplcFeFehhR(i1,i3,gt2)
coup3L = -cplcFeFeVPR(i3,i2)
coup3R = -cplcFeFeVPL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(2)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fu, Fu, Fu}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuhhL(i2,i1,gt1)
coup1R = cplcFuFuhhR(i2,i1,gt1)
coup2L = cplcFuFuhhL(i1,i3,gt2)
coup2R = cplcFuFuhhR(i1,i3,gt2)
coup3L = -cplcFuFuVPR(i3,i2)
coup3R = -cplcFuFuVPL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {gWp, gWp, gWp}
If ((Include_in_loopgWp).and.(Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWpgWphh(gt1)
coup2 = cplcgWpgWphh(gt2)
coup3 = cplcgWpgWpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(2)*AmpC 
End if 


! {gWpC, gWpC, gWpC}
If ((Include_in_loopgWC).and.(Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWCgWChh(gt1)
coup2 = cplcgWCgWChh(gt2)
coup3 = cplcgWCgWCVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(2)*AmpC 
End if 


! {Hp, Hp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MHp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplhhHpcHp(gt2)
coup3 = -cplHpcHpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(2)*AmpC 
End if 


! {Hp, VWp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MHp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplhhHpcHp(gt2)
coup3 = cplcHpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Hp, Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplhhcHpVWp(gt2)
coup3 = cplHpcVWpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplhhcHpVWp(gt2)
coup3 = cplcVWpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplhhHpcVWp(gt2)
coup3 = -cplHpcHpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplhhHpcVWp(gt2)
coup3 = cplcHpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVWp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplhhcVWpVWp(gt2)
coup3 = cplHpcVWpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplhhcVWpVWp(gt2)
coup3 = cplcVWpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(2)*AmpC 
End if 


! {conj[Hp], conj[VWp], conj[Hp]}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MHp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplhhHpcHp(gt2)
coup3 = cplHpcVWpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[Hp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplhhHpcVWp(gt2)
coup3 = cplcHpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[VWp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplhhHpcVWp(gt2)
coup3 = -cplcVWpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplhhcHpVWp(gt2)
coup3 = cplHpcHpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplhhcHpVWp(gt2)
coup3 = cplHpcVWpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], conj[VWp]}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVWp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplhhcVWpVWp(gt2)
coup3 = cplcHpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
coup1 = cplhhHpcVWp(gt1)
coup2 = cplhhcHpVPVWp1(gt2)
Call Amp_VERTEX_StoSV_Topology2_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplhhHpcVWpVP1(gt2)
Call Amp_VERTEX_StoSV_Topology2_VS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
coup1 = cplhhHpcVWpVP1(gt1)
coup2 = cplhhcHpVWp(gt2)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology3_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 

End if 


! {VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
coup1 = cplhhcHpVPVWp1(gt1)
coup2 = cplhhHpcVWp(gt2)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology3_VS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 

End if 
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhTohhVP


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTohhVP(MFd,MFe,MFu,Mhh,MHp,MVP,            & 
& MVWp,MFd2,MFe2,MFu2,Mhh2,MHp2,MVP2,MVWp2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVPL,          & 
& cplcFdFdVPR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuhhL,               & 
& cplcFuFuhhR,cplcFuFuVPL,cplcFuFuVPR,cplcgWpgWphh,cplcgWpgWpVP,cplcgWCgWChh,            & 
& cplcgWCgWCVP,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplHpcHpVP,               & 
& cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,cplhhHpcVWpVP1,cplhhcHpVPVWp1,Amp)

Implicit None

Real(dp), Intent(in) :: MFd(3),MFe(3),MFu(3),Mhh(2),MHp,MVP,MVWp,MFd2(3),MFe2(3),MFu2(3),Mhh2(2),             & 
& MHp2,MVP2,MVWp2

Complex(dp), Intent(in) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),              & 
& cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),               & 
& cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),               & 
& cplcgWpgWphh(2),cplcgWpgWpVP,cplcgWCgWChh(2),cplcgWCgWCVP,cplhhHpcHp(2),               & 
& cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,      & 
& cplcVWpVPVWp,cplhhHpcVWpVP1(2),cplhhcHpVPVWp1(2)

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
Mex1 = Mhh(gt1) 
Mex2 = Mhh(gt2) 
Mex3 = MVP 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTohhVP


Subroutine Amplitude_WAVE_SDdiracDM_hhTohhVZ(Mhh,Mhh2,MVZ,MVZ2,Zfhh,ZfVZ,Amp)

Implicit None

Real(dp), Intent(in) :: Mhh(2),Mhh2(2),MVZ,MVZ2

Complex(dp), Intent(in) :: Zfhh(2,2),ZfVZ

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
Mex1 = Mhh(gt1) 
Mex2 = Mhh(gt2) 
Mex3 = MVZ 
ZcoupT1 = 0._dp 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
! Vanishing 


! External Field 2 
! Vanishing 


! External Field 3 
Amp(:,gt1, gt2) = 0._dp
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhTohhVZ


Subroutine Amplitude_VERTEX_SDdiracDM_hhTohhVZ(MFd,MFe,MFu,MFxv,Mhh,MHp,              & 
& MVWp,MVZ,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MVWp2,MVZ2,cplcFdFdhhL,cplcFdFdhhR,            & 
& cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVZL,cplcFeFeVZR,               & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,           & 
& cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWphh,cplcgWpgWpVZ,cplcgWCgWChh,cplcgWCgWCVZ,       & 
& cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,    & 
& cplcVWpVWpVZ,cplhhHpcVWpVZ1,cplhhcHpVWpVZ1,Amp)

Implicit None

Real(dp), Intent(in) :: MFd(3),MFe(3),MFu(3),MFxv(2),Mhh(2),MHp,MVWp,MVZ,MFd2(3),MFe2(3),MFu2(3),             & 
& MFxv2(2),Mhh2(2),MHp2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),              & 
& cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),               & 
& cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),               & 
& cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),       & 
& cplcgWpgWphh(2),cplcgWpgWpVZ,cplcgWCgWChh(2),cplcgWCgWCVZ,cplhhHpcHp(2),               & 
& cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,      & 
& cplcVWpVWpVZ,cplhhHpcVWpVZ1(2),cplhhcHpVWpVZ1(2)

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
Mex1 = Mhh(gt1) 
Mex2 = Mhh(gt2) 
Mex3 = MVZ 


! {Fd, Fd, Fd}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFdFdhhL(i2,i1,gt1)
coup1R = cplcFdFdhhR(i2,i1,gt1)
coup2L = cplcFdFdhhL(i1,i3,gt2)
coup2R = cplcFdFdhhR(i1,i3,gt2)
coup3L = -cplcFdFdVZR(i3,i2)
coup3R = -cplcFdFdVZL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fe, Fe, Fe}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFehhL(i2,i1,gt1)
coup1R = cplcFeFehhR(i2,i1,gt1)
coup2L = cplcFeFehhL(i1,i3,gt2)
coup2R = cplcFeFehhR(i1,i3,gt2)
coup3L = -cplcFeFeVZR(i3,i2)
coup3R = -cplcFeFeVZL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(2)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fu, Fu, Fu}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuhhL(i2,i1,gt1)
coup1R = cplcFuFuhhR(i2,i1,gt1)
coup2L = cplcFuFuhhL(i1,i3,gt2)
coup2R = cplcFuFuhhR(i1,i3,gt2)
coup3L = -cplcFuFuVZR(i3,i2)
coup3R = -cplcFuFuVZL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fxv, Fxv, Fxv}
If ((Include_in_loopFxv).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxvFxvhhL(i2,i1,gt1)
coup1R = cplcFxvFxvhhR(i2,i1,gt1)
coup2L = cplcFxvFxvhhL(i1,i3,gt2)
coup2R = cplcFxvFxvhhR(i1,i3,gt2)
coup3L = -cplcFxvFxvVZR(i3,i2)
coup3R = -cplcFxvFxvVZL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(2)*AmpC 
    End Do
  End Do
End Do
End if 


! {gWp, gWp, gWp}
If ((Include_in_loopgWp).and.(Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWpgWphh(gt1)
coup2 = cplcgWpgWphh(gt2)
coup3 = cplcgWpgWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(2)*AmpC 
End if 


! {gWpC, gWpC, gWpC}
If ((Include_in_loopgWC).and.(Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWCgWChh(gt1)
coup2 = cplcgWCgWChh(gt2)
coup3 = cplcgWCgWCVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(2)*AmpC 
End if 


! {Hp, Hp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MHp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplhhHpcHp(gt2)
coup3 = -cplHpcHpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(2)*AmpC 
End if 


! {Hp, VWp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MHp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplhhHpcHp(gt2)
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Hp, Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplhhcHpVWp(gt2)
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplhhcHpVWp(gt2)
coup3 = -cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplhhHpcVWp(gt2)
coup3 = -cplHpcHpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplhhHpcVWp(gt2)
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVWp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplhhcVWpVWp(gt2)
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplhhcVWpVWp(gt2)
coup3 = -cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(2)*AmpC 
End if 


! {conj[Hp], conj[VWp], conj[Hp]}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MHp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplhhHpcHp(gt2)
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[Hp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplhhHpcVWp(gt2)
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[VWp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = -cplhhcHpVWp(gt1)
coup2 = cplhhHpcVWp(gt2)
coup3 = cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplhhcHpVWp(gt2)
coup3 = cplHpcHpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplhhcHpVWp(gt2)
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], conj[VWp]}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVWp 
coup1 = -cplhhHpcVWp(gt1)
coup2 = cplhhcVWpVWp(gt2)
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
coup1 = cplhhHpcVWp(gt1)
coup2 = cplhhcHpVWpVZ1(gt2)
Call Amp_VERTEX_StoSV_Topology2_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplhhHpcVWpVZ1(gt2)
Call Amp_VERTEX_StoSV_Topology2_VS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
coup1 = cplhhHpcVWpVZ1(gt1)
coup2 = cplhhcHpVWp(gt2)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology3_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 

End if 


! {VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
coup1 = cplhhcHpVWpVZ1(gt1)
coup2 = cplhhHpcVWp(gt2)
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology3_VS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 

End if 
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhTohhVZ


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTohhVZ(MFd,MFe,MFu,MFxv,Mhh,               & 
& MHp,MVWp,MVZ,MFd2,MFe2,MFu2,MFxv2,Mhh2,MHp2,MVWp2,MVZ2,cplcFdFdhhL,cplcFdFdhhR,        & 
& cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVZL,cplcFeFeVZR,               & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVZL,cplcFuFuVZR,cplcFxvFxvhhL,cplcFxvFxvhhR,           & 
& cplcFxvFxvVZL,cplcFxvFxvVZR,cplcgWpgWphh,cplcgWpgWpVZ,cplcgWCgWChh,cplcgWCgWCVZ,       & 
& cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,    & 
& cplcVWpVWpVZ,cplhhHpcVWpVZ1,cplhhcHpVWpVZ1,Amp)

Implicit None

Real(dp), Intent(in) :: MFd(3),MFe(3),MFu(3),MFxv(2),Mhh(2),MHp,MVWp,MVZ,MFd2(3),MFe2(3),MFu2(3),             & 
& MFxv2(2),Mhh2(2),MHp2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),              & 
& cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),               & 
& cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),               & 
& cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),       & 
& cplcgWpgWphh(2),cplcgWpgWpVZ,cplcgWCgWChh(2),cplcgWCgWCVZ,cplhhHpcHp(2),               & 
& cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,      & 
& cplcVWpVWpVZ,cplhhHpcVWpVZ1(2),cplhhcHpVWpVZ1(2)

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
Mex1 = Mhh(gt1) 
Mex2 = Mhh(gt2) 
Mex3 = MVZ 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhTohhVZ


Subroutine Amplitude_WAVE_SDdiracDM_hhToVGVG(Mhh,Mhh2,MVG,MVG2,Zfhh,ZfVG,Amp)

Implicit None

Real(dp), Intent(in) :: Mhh(2),Mhh2(2),MVG,MVG2

Complex(dp), Intent(in) :: Zfhh(2,2),ZfVG

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MVG 
Mex3 = MVG 
ZcoupT1 = 0._dp 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
! Vanishing 


! External Field 2 
! Vanishing 


! External Field 3 
Amp(:,gt1) = 0._dp
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhToVGVG


Subroutine Amplitude_VERTEX_SDdiracDM_hhToVGVG(MFd,MFu,Mhh,MVG,MFd2,MFu2,             & 
& Mhh2,MVG2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVGL,cplcFdFdVGR,cplcFuFuhhL,cplcFuFuhhR,     & 
& cplcFuFuVGL,cplcFuFuVGR,Amp)

Implicit None

Real(dp), Intent(in) :: MFd(3),MFu(3),Mhh(2),MVG,MFd2(3),MFu2(3),Mhh2(2),MVG2

Complex(dp), Intent(in) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),              & 
& cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3)

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
Mex1 = Mhh(gt1) 
Mex2 = MVG 
Mex3 = MVG 


! {Fd, Fd, Fd}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFdFdhhL(i2,i1,gt1)
coup1R = cplcFdFdhhR(i2,i1,gt1)
coup2L = -cplcFdFdVGR(i1,i3)
coup2R = -cplcFdFdVGL(i1,i3)
coup3L = -cplcFdFdVGR(i3,i2)
coup3R = -cplcFdFdVGL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fu, Fu, Fu}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuhhL(i2,i1,gt1)
coup1R = cplcFuFuhhR(i2,i1,gt1)
coup2L = -cplcFuFuVGR(i1,i3)
coup2R = -cplcFuFuVGL(i1,i3)
coup3L = -cplcFuFuVGR(i3,i2)
coup3R = -cplcFuFuVGL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhToVGVG


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToVGVG(MFd,MFu,Mhh,MVG,MFd2,               & 
& MFu2,Mhh2,MVG2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVGL,cplcFdFdVGR,cplcFuFuhhL,            & 
& cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,Amp)

Implicit None

Real(dp), Intent(in) :: MFd(3),MFu(3),Mhh(2),MVG,MFd2(3),MFu2(3),Mhh2(2),MVG2

Complex(dp), Intent(in) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),              & 
& cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3)

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
Mex1 = Mhh(gt1) 
Mex2 = MVG 
Mex3 = MVG 
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToVGVG


Subroutine Amplitude_WAVE_SDdiracDM_hhToVPVP(cplhhVZVZ,ctcplhhVZVZ,Mhh,               & 
& Mhh2,MVP,MVP2,MVZ,MVZ2,Zfhh,ZfVP,ZfVPVZ,ZfVZVP,Amp)

Implicit None

Real(dp), Intent(in) :: Mhh(2),Mhh2(2),MVP,MVP2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplhhVZVZ(2)

Complex(dp), Intent(in) :: ctcplhhVZVZ(2)

Complex(dp), Intent(in) :: Zfhh(2,2),ZfVP,ZfVPVZ,ZfVZVP

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MVP 
Mex3 = MVP 
ZcoupT1 = 0._dp 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
! Vanishing 


! External Field 2 
! Vanishing 


! External Field 3 
Amp(:,gt1) = 0._dp
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhToVPVP


Subroutine Amplitude_VERTEX_SDdiracDM_hhToVPVP(MFd,MFe,MFu,Mhh,MHp,MVP,               & 
& MVWp,MFd2,MFe2,MFu2,Mhh2,MHp2,MVP2,MVWp2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVPL,          & 
& cplcFdFdVPR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuhhL,               & 
& cplcFuFuhhR,cplcFuFuVPL,cplcFuFuVPR,cplcgWpgWphh,cplcgWpgWpVP,cplcgWCgWChh,            & 
& cplcgWCgWCVP,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplHpcHpVP,               & 
& cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,cplhhHpcVWpVP1,cplhhcHpVPVWp1,cplHpcHpVPVP1,      & 
& cplcVWpVPVPVWp3Q,cplcVWpVPVPVWp1Q,cplcVWpVPVPVWp2Q,Amp)

Implicit None

Real(dp), Intent(in) :: MFd(3),MFe(3),MFu(3),Mhh(2),MHp,MVP,MVWp,MFd2(3),MFe2(3),MFu2(3),Mhh2(2),             & 
& MHp2,MVP2,MVWp2

Complex(dp), Intent(in) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),              & 
& cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),               & 
& cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),               & 
& cplcgWpgWphh(2),cplcgWpgWpVP,cplcgWCgWChh(2),cplcgWCgWCVP,cplhhHpcHp(2),               & 
& cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,      & 
& cplcVWpVPVWp,cplhhHpcVWpVP1(2),cplhhcHpVPVWp1(2),cplHpcHpVPVP1,cplcVWpVPVPVWp3Q,       & 
& cplcVWpVPVPVWp1Q,cplcVWpVPVPVWp2Q

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
Mex1 = Mhh(gt1) 
Mex2 = MVP 
Mex3 = MVP 


! {Fd, Fd, Fd}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFdFdhhL(i2,i1,gt1)
coup1R = cplcFdFdhhR(i2,i1,gt1)
coup2L = -cplcFdFdVPR(i1,i3)
coup2R = -cplcFdFdVPL(i1,i3)
coup3L = -cplcFdFdVPR(i3,i2)
coup3R = -cplcFdFdVPL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fe, Fe, Fe}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFehhL(i2,i1,gt1)
coup1R = cplcFeFehhR(i2,i1,gt1)
coup2L = -cplcFeFeVPR(i1,i3)
coup2R = -cplcFeFeVPL(i1,i3)
coup3L = -cplcFeFeVPR(i3,i2)
coup3R = -cplcFeFeVPL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fu, Fu, Fu}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuhhL(i2,i1,gt1)
coup1R = cplcFuFuhhR(i2,i1,gt1)
coup2L = -cplcFuFuVPR(i1,i3)
coup2R = -cplcFuFuVPL(i1,i3)
coup3L = -cplcFuFuVPR(i3,i2)
coup3R = -cplcFuFuVPL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {gWp, gWp, gWp}
If ((Include_in_loopgWp).and.(Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWpgWphh(gt1)
coup2 = cplcgWpgWpVP
coup3 = cplcgWpgWpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
End if 


! {gWpC, gWpC, gWpC}
If ((Include_in_loopgWC).and.(Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWCgWChh(gt1)
coup2 = cplcgWCgWCVP
coup3 = cplcgWCgWCVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
End if 


! {Hp, Hp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MHp 
coup1 = cplhhHpcHp(gt1)
coup2 = -cplHpcHpVP
coup3 = -cplHpcHpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
End if 


! {Hp, VWp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhHpcVWp(gt1)
coup2 = -cplHpcHpVP
coup3 = cplcHpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplcHpVPVWp
coup3 = cplHpcVWpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplhhHpcVWp(gt1)
coup2 = cplcHpVPVWp
coup3 = cplcVWpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplHpcVWpVP
coup3 = -cplHpcHpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplHpcVWpVP
coup3 = cplcHpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplcVWpVPVWp
coup3 = cplHpcVWpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplcVWpVPVWp
coup3 = cplcVWpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
End if 


! {conj[Hp], conj[VWp], conj[Hp]}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplHpcHpVP
coup3 = cplHpcVWpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[Hp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpcVWpVP
coup3 = cplcHpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[VWp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplHpcVWpVP
coup3 = -cplcVWpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = cplhhHpcVWp(gt1)
coup2 = cplcHpVPVWp
coup3 = cplHpcHpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplcHpVPVWp
coup3 = cplHpcVWpVP
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], conj[VWp]}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcVWp(gt1)
coup2 = -cplcVWpVPVWp
coup3 = cplcHpVPVWp
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MHp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpcHpVPVP1
Call Amp_VERTEX_StoVV_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplcVWpVPVPVWp1Q
coup2b = coup2 
coup2 = cplcVWpVPVPVWp2Q
coup2c = coup2 
coup2 = cplcVWpVPVPVWp3Q
coup2a = coup2 
If (Abs(coup1)*(Abs(coup2a)+Abs(coup2b)+Abs(coup2c)) .gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology2_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2a,coup2b,coup2c,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
coup1 = cplhhHpcVWpVP1(gt1)
coup2 = cplcHpVPVWp
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology3_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 


! {VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
coup1 = cplhhcHpVPVWp1(gt1)
coup2 = cplHpcVWpVP
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology3_VS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 


! {Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
coup1 = cplhhHpcVWpVP1(gt1)
coup2 = cplcHpVPVWp
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology4_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 


! {VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
coup1 = cplhhcHpVPVWp1(gt1)
coup2 = cplHpcVWpVP
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology4_VS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhToVPVP


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToVPVP(MFd,MFe,MFu,Mhh,MHp,MVP,            & 
& MVWp,MFd2,MFe2,MFu2,Mhh2,MHp2,MVP2,MVWp2,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVPL,          & 
& cplcFdFdVPR,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuhhL,               & 
& cplcFuFuhhR,cplcFuFuVPL,cplcFuFuVPR,cplcgWpgWphh,cplcgWpgWpVP,cplcgWCgWChh,            & 
& cplcgWCgWCVP,cplhhHpcHp,cplhhHpcVWp,cplhhcHpVWp,cplhhcVWpVWp,cplHpcHpVP,               & 
& cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,cplhhHpcVWpVP1,cplhhcHpVPVWp1,cplHpcHpVPVP1,      & 
& cplcVWpVPVPVWp3Q,cplcVWpVPVPVWp1Q,cplcVWpVPVPVWp2Q,Amp)

Implicit None

Real(dp), Intent(in) :: MFd(3),MFe(3),MFu(3),Mhh(2),MHp,MVP,MVWp,MFd2(3),MFe2(3),MFu2(3),Mhh2(2),             & 
& MHp2,MVP2,MVWp2

Complex(dp), Intent(in) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),              & 
& cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),               & 
& cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),               & 
& cplcgWpgWphh(2),cplcgWpgWpVP,cplcgWCgWChh(2),cplcgWCgWCVP,cplhhHpcHp(2),               & 
& cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,      & 
& cplcVWpVPVWp,cplhhHpcVWpVP1(2),cplhhcHpVPVWp1(2),cplHpcHpVPVP1,cplcVWpVPVPVWp3Q,       & 
& cplcVWpVPVPVWp1Q,cplcVWpVPVPVWp2Q

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
Mex1 = Mhh(gt1) 
Mex2 = MVP 
Mex3 = MVP 
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToVPVP


Subroutine Amplitude_WAVE_SDdiracDM_hhToVPVZ(cplhhVZVZ,ctcplhhVZVZ,Mhh,               & 
& Mhh2,MVP,MVP2,MVZ,MVZ2,Zfhh,ZfVP,ZfVZ,Amp)

Implicit None

Real(dp), Intent(in) :: Mhh(2),Mhh2(2),MVP,MVP2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplhhVZVZ(2)

Complex(dp), Intent(in) :: ctcplhhVZVZ(2)

Complex(dp), Intent(in) :: Zfhh(2,2),ZfVP,ZfVZ

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
! External masses 
Mex1 = Mhh(gt1) 
Mex2 = MVP 
Mex3 = MVZ 
ZcoupT1 = 0._dp 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
! Vanishing 


! External Field 2 
! Vanishing 


! External Field 3 
Amp(:,gt1) = 0._dp
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_hhToVPVZ


Subroutine Amplitude_VERTEX_SDdiracDM_hhToVPVZ(MFd,MFe,MFu,Mhh,MHp,MVP,               & 
& MVWp,MVZ,MFd2,MFe2,MFu2,Mhh2,MHp2,MVP2,MVWp2,MVZ2,cplcFdFdhhL,cplcFdFdhhR,             & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,               & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,               & 
& cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcgWpgWphh,cplcgWpgWpVP,             & 
& cplcgWpgWpVZ,cplcgWCgWChh,cplcgWCgWCVP,cplcgWCgWCVZ,cplhhHpcHp,cplhhHpcVWp,            & 
& cplhhcHpVWp,cplhhcVWpVWp,cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVPVWp,    & 
& cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,cplhhHpcVWpVP1,cplhhHpcVWpVZ1,cplhhcHpVPVWp1,    & 
& cplhhcHpVWpVZ1,cplHpcHpVPVZ1,cplcVWpVPVWpVZ3Q,cplcVWpVPVWpVZ2Q,cplcVWpVPVWpVZ1Q,Amp)

Implicit None

Real(dp), Intent(in) :: MFd(3),MFe(3),MFu(3),Mhh(2),MHp,MVP,MVWp,MVZ,MFd2(3),MFe2(3),MFu2(3),Mhh2(2),         & 
& MHp2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),              & 
& cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),               & 
& cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFuFuhhL(3,3,2),& 
& cplcFuFuhhR(3,3,2),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),& 
& cplcgWpgWphh(2),cplcgWpgWpVP,cplcgWpgWpVZ,cplcgWCgWChh(2),cplcgWCgWCVP,cplcgWCgWCVZ,   & 
& cplhhHpcHp(2),cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplHpcHpVP,cplHpcVWpVP,    & 
& cplHpcHpVZ,cplHpcVWpVZ,cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,              & 
& cplhhHpcVWpVP1(2),cplhhHpcVWpVZ1(2),cplhhcHpVPVWp1(2),cplhhcHpVWpVZ1(2),               & 
& cplHpcHpVPVZ1,cplcVWpVPVWpVZ3Q,cplcVWpVPVWpVZ2Q,cplcVWpVPVWpVZ1Q

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
Mex1 = Mhh(gt1) 
Mex2 = MVP 
Mex3 = MVZ 


! {Fd, Fd, Fd}
If ((Include_in_loopFd).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFd(i1) 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFdFdhhL(i2,i1,gt1)
coup1R = cplcFdFdhhR(i2,i1,gt1)
coup2L = -cplcFdFdVPR(i1,i3)
coup2R = -cplcFdFdVPL(i1,i3)
coup3L = -cplcFdFdVZR(i3,i2)
coup3R = -cplcFdFdVZL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fe, Fe, Fe}
If ((Include_in_loopFe).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFe(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFehhL(i2,i1,gt1)
coup1R = cplcFeFehhR(i2,i1,gt1)
coup2L = -cplcFeFeVPR(i1,i3)
coup2R = -cplcFeFeVPL(i1,i3)
coup3L = -cplcFeFeVZR(i3,i2)
coup3R = -cplcFeFeVZL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fu, Fu, Fu}
If ((Include_in_loopFu).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
Do i1=1,3
  Do i2=1,3
    Do i3=1,3
ML1 = MFu(i1) 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuhhL(i2,i1,gt1)
coup1R = cplcFuFuhhR(i2,i1,gt1)
coup2L = -cplcFuFuVPR(i1,i3)
coup2R = -cplcFuFuVPL(i1,i3)
coup3L = -cplcFuFuVZR(i3,i2)
coup3R = -cplcFuFuVZL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(6)*AmpC 
    End Do
  End Do
End Do
End if 


! {gWp, gWp, gWp}
If ((Include_in_loopgWp).and.(Include_in_loopgWp).and.(Include_in_loopgWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWpgWphh(gt1)
coup2 = cplcgWpgWpVP
coup3 = cplcgWpgWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
End if 


! {gWpC, gWpC, gWpC}
If ((Include_in_loopgWC).and.(Include_in_loopgWC).and.(Include_in_loopgWC)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplcgWCgWChh(gt1)
coup2 = cplcgWCgWCVP
coup3 = cplcgWCgWCVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_UUU(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
End if 


! {Hp, Hp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MHp 
coup1 = cplhhHpcHp(gt1)
coup2 = -cplHpcHpVP
coup3 = -cplHpcHpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
End if 


! {Hp, VWp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhHpcVWp(gt1)
coup2 = -cplHpcHpVP
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplcHpVPVWp
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplhhHpcVWp(gt1)
coup2 = cplcHpVPVWp
coup3 = -cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplHpcVWpVP
coup3 = -cplHpcHpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplHpcVWpVP
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, Hp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplcVWpVPVWp
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplcVWpVPVWp
coup3 = -cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(2)*AmpC 
End if 


! {conj[Hp], conj[VWp], conj[Hp]}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplHpcHpVP
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[Hp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpcVWpVP
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], conj[VWp], conj[VWp]}
If ((Include_in_loopHp).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
ML3 = MVWp 
coup1 = cplhhcHpVWp(gt1)
coup2 = cplHpcVWpVP
coup3 = cplcVWpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_SVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MHp 
coup1 = cplhhHpcVWp(gt1)
coup2 = cplcHpVPVWp
coup3 = cplHpcHpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[VWp], conj[Hp]}
If ((Include_in_loopVWp).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MVWp 
ML3 = MHp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplcHpVPVWp
coup3 = cplHpcVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {conj[VWp], conj[Hp], conj[VWp]}
If ((Include_in_loopVWp).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MHp 
ML3 = MVWp 
coup1 = cplhhHpcVWp(gt1)
coup2 = -cplcVWpVPVWp
coup3 = cplcHpVWpVZ
If ((Abs(coup1))*(Abs(coup2))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology1_VSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, Hp}
If ((Include_in_loopHp).and.(Include_in_loopHp)) Then 
ML1 = MHp 
ML2 = MHp 
coup1 = cplhhHpcHp(gt1)
coup2 = cplHpcHpVPVZ1
Call Amp_VERTEX_StoVV_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {VWp, VWp}
If ((Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
ML1 = MVWp 
ML2 = MVWp 
coup1 = cplhhcVWpVWp(gt1)
coup2 = cplcVWpVPVWpVZ1Q
coup2b = coup2 
coup2 = cplcVWpVPVWpVZ2Q
coup2a = coup2 
coup2 = cplcVWpVPVWpVZ3Q
coup2c = coup2 
If (Abs(coup1)*(Abs(coup2a)+Abs(coup2b)+Abs(coup2c)) .gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology2_VV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2a,coup2b,coup2c,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 
End if 


! {Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
coup1 = cplhhHpcVWpVZ1(gt1)
coup2 = cplcHpVPVWp
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology3_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 


! {VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
coup1 = cplhhcHpVWpVZ1(gt1)
coup2 = cplHpcVWpVP
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology3_VS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 


! {Hp, VWp}
If ((Include_in_loopHp).and.(Include_in_loopVWp)) Then 
ML1 = MHp 
ML2 = MVWp 
coup1 = cplhhHpcVWpVP1(gt1)
coup2 = cplcHpVWpVZ
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology4_SV(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 


! {VWp, Hp}
If ((Include_in_loopVWp).and.(Include_in_loopHp)) Then 
ML1 = MVWp 
ML2 = MHp 
coup1 = cplhhcHpVPVWp1(gt1)
coup2 = cplHpcVWpVZ
If (Abs(coup1)*Abs(coup2).gt.epsCoup) Then 
Call Amp_VERTEX_StoVV_Topology4_VS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1) = Amp(:,gt1) + oo16pi2*(1)*AmpC 

End if 
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_hhToVPVZ


Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToVPVZ(MFd,MFe,MFu,Mhh,MHp,MVP,            & 
& MVWp,MVZ,MFd2,MFe2,MFu2,Mhh2,MHp2,MVP2,MVWp2,MVZ2,cplcFdFdhhL,cplcFdFdhhR,             & 
& cplcFdFdVPL,cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFeFehhL,cplcFeFehhR,               & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuhhL,cplcFuFuhhR,               & 
& cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcgWpgWphh,cplcgWpgWpVP,             & 
& cplcgWpgWpVZ,cplcgWCgWChh,cplcgWCgWCVP,cplcgWCgWCVZ,cplhhHpcHp,cplhhHpcVWp,            & 
& cplhhcHpVWp,cplhhcVWpVWp,cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVPVWp,    & 
& cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,cplhhHpcVWpVP1,cplhhHpcVWpVZ1,cplhhcHpVPVWp1,    & 
& cplhhcHpVWpVZ1,cplHpcHpVPVZ1,cplcVWpVPVWpVZ3Q,cplcVWpVPVWpVZ2Q,cplcVWpVPVWpVZ1Q,Amp)

Implicit None

Real(dp), Intent(in) :: MFd(3),MFe(3),MFu(3),Mhh(2),MHp,MVP,MVWp,MVZ,MFd2(3),MFe2(3),MFu2(3),Mhh2(2),         & 
& MHp2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),              & 
& cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),               & 
& cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFuFuhhL(3,3,2),& 
& cplcFuFuhhR(3,3,2),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),& 
& cplcgWpgWphh(2),cplcgWpgWpVP,cplcgWpgWpVZ,cplcgWCgWChh(2),cplcgWCgWCVP,cplcgWCgWCVZ,   & 
& cplhhHpcHp(2),cplhhHpcVWp(2),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplHpcHpVP,cplHpcVWpVP,    & 
& cplHpcHpVZ,cplHpcVWpVZ,cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,              & 
& cplhhHpcVWpVP1(2),cplhhHpcVWpVZ1(2),cplhhcHpVPVWp1(2),cplhhcHpVWpVZ1(2),               & 
& cplHpcHpVPVZ1,cplcVWpVPVWpVZ3Q,cplcVWpVPVWpVZ2Q,cplcVWpVPVWpVZ1Q

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
Mex1 = Mhh(gt1) 
Mex2 = MVP 
Mex3 = MVZ 
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_hhToVPVZ



End Module OneLoopDecay_hh_SDdiracDM
