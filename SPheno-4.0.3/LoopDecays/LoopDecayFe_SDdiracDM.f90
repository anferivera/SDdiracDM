! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:21 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module OneLoopDecay_Fe_SDdiracDM
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

Subroutine Amplitude_Tree_SDdiracDM_FeToFeAh(cplcFeFeAhL,cplcFeFeAhR,MAh,             & 
& MFe,MAh2,MFe2,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MAh2,MFe2(3)

Complex(dp), Intent(in) :: cplcFeFeAhL(3,3),cplcFeFeAhR(3,3)

Complex(dp) :: Amp(2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,3
  Do gt2=1,3
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MAh 
! Tree-Level Vertex 
coupT1L = cplcFeFeAhL(gt1,gt2)
coupT1R = cplcFeFeAhR(gt1,gt2)
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2) = AmpC 
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_FeToFeAh


Subroutine Gamma_Real_SDdiracDM_FeToFeAh(MLambda,em,gs,cplcFeFeAhL,cplcFeFeAhR,       & 
& MAh,MFe,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFeFeAhL(3,3),cplcFeFeAhR(3,3)

Real(dp), Intent(in) :: MAh,MFe(3)

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(3,3), GammarealGluon(3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,3
  Do i2=1,3
CoupL = cplcFeFeAhL(i1,i2)
CoupR = cplcFeFeAhR(i1,i2)
Mex1 = MFe(i1)
Mex2 = MFe(i2)
Mex3 = MAh
If (Mex1.gt.(Mex2+Mex3)) Then 
 Call hardradiationFFS(Mex1,Mex2,Mex3,MLambda,em,1._dp,1._dp,0._dp,1._dp,0._dp,0._dp,CoupL,CoupR,Gammarealphoton(i1,i2),kont)
  GammarealGluon(i1,i2) = 0._dp 
Else 
  GammarealGluon(i1,i2) = 0._dp 
  GammarealPhoton(i1,i2) = 0._dp 

End if 
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_FeToFeAh


Subroutine Amplitude_WAVE_SDdiracDM_FeToFeAh(cplcFeFeAhL,cplcFeFeAhR,ctcplcFeFeAhL,   & 
& ctcplcFeFeAhR,MAh,MAh2,MFe,MFe2,ZfAh,ZfEL,ZfER,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MAh2,MFe(3),MFe2(3)

Complex(dp), Intent(in) :: cplcFeFeAhL(3,3),cplcFeFeAhR(3,3)

Complex(dp), Intent(in) :: ctcplcFeFeAhL(3,3),ctcplcFeFeAhR(3,3)

Complex(dp), Intent(in) :: ZfAh,ZfEL(3,3),ZfER(3,3)

Complex(dp), Intent(out) :: Amp(2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,3
  Do gt2=1,3
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MAh 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFeFeAhL(gt1,gt2) 
ZcoupT1R = ctcplcFeFeAhR(gt1,gt2) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfER(i1,gt1)*cplcFeFeAhL(i1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfEL(i1,gt1))*cplcFeFeAhR(i1,gt2)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfEL(i1,gt2)*cplcFeFeAhL(gt1,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfER(i1,gt2))*cplcFeFeAhR(gt1,i1)
End Do


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfAh*cplcFeFeAhL(gt1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfAh*cplcFeFeAhR(gt1,gt2)


! Getting the amplitude 
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt1, gt2) = AmpC 
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FeToFeAh


Subroutine Amplitude_VERTEX_SDdiracDM_FeToFeAh(MAh,MFe,MFv,Mhh,MHp,MVP,               & 
& MVWp,MVZ,MAh2,MFe2,MFv2,Mhh2,MHp2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFeFeAhL,               & 
& cplcFeFeAhR,cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,     & 
& cplcFvFeHpR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,             & 
& cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),Mhh(2),MHp,MVP,MVWp,MVZ,MAh2,MFe2(3),MFv2(3),Mhh2(2),               & 
& MHp2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplAhhhVZ(2),cplAhHpcVWp,              & 
& cplAhcHpVWp,cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),   & 
& cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFeFeVZL(3,3),& 
& cplcFeFeVZR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),               & 
& cplcFeFvcVWpR(3,3)

Complex(dp), Intent(out) :: Amp(2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,3
  Do gt2=1,3
Amp(:,gt1, gt2) = 0._dp 
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MAh 


! {Ah, bar[Fe], bar[Fe]}
If ((Include_in_loopAh).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MAh 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFeAhL(gt1,i2)
coup1R = cplcFeFeAhR(gt1,i2)
coup2L = cplcFeFeAhL(i3,gt2)
coup2R = cplcFeFeAhR(i3,gt2)
coup3L = cplcFeFeAhL(i2,i3)
coup3R = cplcFeFeAhR(i2,i3)
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


! {Fe, hh, Ah}
If ((Include_in_loopFe).and.(Include_in_loophh).and.(Include_in_loopAh)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFe(i1) 
ML2 = Mhh(i2) 
ML3 = MAh 
coup1L = cplcFeFehhL(gt1,i1,i2)
coup1R = cplcFeFehhR(gt1,i1,i2)
coup2L = cplcFeFeAhL(i1,gt2)
coup2R = cplcFeFeAhR(i1,gt2)
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


! {Fe, Ah, hh}
If ((Include_in_loopFe).and.(Include_in_loopAh).and.(Include_in_loophh)) Then 
Do i1=1,3
    Do i3=1,2
ML1 = MFe(i1) 
ML2 = MAh 
ML3 = Mhh(i3) 
coup1L = cplcFeFeAhL(gt1,i1)
coup1R = cplcFeFeAhR(gt1,i1)
coup2L = cplcFeFehhL(i1,gt2,i3)
coup2R = cplcFeFehhR(i1,gt2,i3)
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


! {Fe, VZ, hh}
If ((Include_in_loopFe).and.(Include_in_loopVZ).and.(Include_in_loophh)) Then 
Do i1=1,3
    Do i3=1,2
ML1 = MFe(i1) 
ML2 = MVZ 
ML3 = Mhh(i3) 
coup1L = -cplcFeFeVZR(gt1,i1)
coup1R = -cplcFeFeVZL(gt1,i1)
coup2L = cplcFeFehhL(i1,gt2,i3)
coup2R = cplcFeFehhR(i1,gt2,i3)
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


! {Fe, hh, VZ}
If ((Include_in_loopFe).and.(Include_in_loophh).and.(Include_in_loopVZ)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFe(i1) 
ML2 = Mhh(i2) 
ML3 = MVZ 
coup1L = cplcFeFehhL(gt1,i1,i2)
coup1R = cplcFeFehhR(gt1,i1,i2)
coup2L = -cplcFeFeVZR(i1,gt2)
coup2R = -cplcFeFeVZL(i1,gt2)
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


! {Fv, VWp, Hp}
If ((Include_in_loopFv).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MVWp 
ML3 = MHp 
coup1L = -cplcFeFvcVWpR(gt1,i1)
coup1R = -cplcFeFvcVWpL(gt1,i1)
coup2L = cplcFvFeHpL(i1,gt2)
coup2R = cplcFvFeHpR(i1,gt2)
coup3 = -cplAhcHpVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fv, Hp, VWp}
If ((Include_in_loopFv).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MHp 
ML3 = MVWp 
coup1L = cplcFeFvcHpL(gt1,i1)
coup1R = cplcFeFvcHpR(gt1,i1)
coup2L = -cplcFvFeVWpR(i1,gt2)
coup2R = -cplcFvFeVWpL(i1,gt2)
coup3 = -cplAhHpcVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {hh, bar[Fe], bar[Fe]}
If ((Include_in_loophh).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = Mhh(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFehhL(gt1,i2,i1)
coup1R = cplcFeFehhR(gt1,i2,i1)
coup2L = cplcFeFehhL(i3,gt2,i1)
coup2R = cplcFeFehhR(i3,gt2,i1)
coup3L = cplcFeFeAhL(i2,i3)
coup3R = cplcFeFeAhR(i2,i3)
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


! {VP, bar[Fe], bar[Fe]}
If ((Include_in_loopVP).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = -cplcFeFeVPR(gt1,i2)
coup1R = -cplcFeFeVPL(gt1,i2)
coup2L = -cplcFeFeVPR(i3,gt2)
coup2R = -cplcFeFeVPL(i3,gt2)
coup3L = cplcFeFeAhL(i2,i3)
coup3R = cplcFeFeAhR(i2,i3)
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


! {VZ, bar[Fe], bar[Fe]}
If ((Include_in_loopVZ).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVZ 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = -cplcFeFeVZR(gt1,i2)
coup1R = -cplcFeFeVZL(gt1,i2)
coup2L = -cplcFeFeVZR(i3,gt2)
coup2R = -cplcFeFeVZL(i3,gt2)
coup3L = cplcFeFeAhL(i2,i3)
coup3R = cplcFeFeAhR(i2,i3)
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
End Subroutine Amplitude_VERTEX_SDdiracDM_FeToFeAh


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FeToFeAh(MAh,MFe,MFv,Mhh,MHp,MVP,            & 
& MVWp,MVZ,MAh2,MFe2,MFv2,Mhh2,MHp2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFeFeAhL,               & 
& cplcFeFeAhR,cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,     & 
& cplcFvFeHpR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,             & 
& cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),Mhh(2),MHp,MVP,MVWp,MVZ,MAh2,MFe2(3),MFv2(3),Mhh2(2),               & 
& MHp2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplAhhhVZ(2),cplAhHpcVWp,              & 
& cplAhcHpVWp,cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),   & 
& cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFeFeVZL(3,3),& 
& cplcFeFeVZR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),               & 
& cplcFeFvcVWpR(3,3)

Complex(dp), Intent(out) :: Amp(2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,3
  Do gt2=1,3
Amp(:,gt1, gt2) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MAh 


! {VP, bar[Fe], bar[Fe]}
If ((Include_in_loopVP).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = -cplcFeFeVPR(gt1,i2)
coup1R = -cplcFeFeVPL(gt1,i2)
coup2L = -cplcFeFeVPR(i3,gt2)
coup2R = -cplcFeFeVPL(i3,gt2)
coup3L = cplcFeFeAhL(i2,i3)
coup3R = cplcFeFeAhR(i2,i3)
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FeToFeAh


Subroutine Amplitude_Tree_SDdiracDM_FeToFehh(cplcFeFehhL,cplcFeFehhR,MFe,             & 
& Mhh,MFe2,Mhh2,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),Mhh(2),MFe2(3),Mhh2(2)

Complex(dp), Intent(in) :: cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2)

Complex(dp) :: Amp(2,3,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,2
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFe(gt2) 
Mex3 = Mhh(gt3) 
! Tree-Level Vertex 
coupT1L = cplcFeFehhL(gt1,gt2,gt3)
coupT1R = cplcFeFehhR(gt1,gt2,gt3)
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_FeToFehh


Subroutine Gamma_Real_SDdiracDM_FeToFehh(MLambda,em,gs,cplcFeFehhL,cplcFeFehhR,       & 
& MFe,Mhh,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2)

Real(dp), Intent(in) :: MFe(3),Mhh(2)

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(3,3,2), GammarealGluon(3,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,3
  Do i2=1,3
    Do i3=1,2
CoupL = cplcFeFehhL(i1,i2,i3)
CoupR = cplcFeFehhR(i1,i2,i3)
Mex1 = MFe(i1)
Mex2 = MFe(i2)
Mex3 = Mhh(i3)
If (Mex1.gt.(Mex2+Mex3)) Then 
 Call hardradiationFFS(Mex1,Mex2,Mex3,MLambda,em,1._dp,1._dp,0._dp,1._dp,0._dp,0._dp,CoupL,CoupR,Gammarealphoton(i1,i2,i3),kont)
  GammarealGluon(i1,i2,i3) = 0._dp 
Else 
  GammarealGluon(i1,i2,i3) = 0._dp 
  GammarealPhoton(i1,i2,i3) = 0._dp 

End if 
    End Do
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_FeToFehh


Subroutine Amplitude_WAVE_SDdiracDM_FeToFehh(cplcFeFehhL,cplcFeFehhR,ctcplcFeFehhL,   & 
& ctcplcFeFehhR,MFe,MFe2,Mhh,Mhh2,ZfEL,ZfER,Zfhh,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFe2(3),Mhh(2),Mhh2(2)

Complex(dp), Intent(in) :: cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2)

Complex(dp), Intent(in) :: ctcplcFeFehhL(3,3,2),ctcplcFeFehhR(3,3,2)

Complex(dp), Intent(in) :: ZfEL(3,3),ZfER(3,3),Zfhh(2,2)

Complex(dp), Intent(out) :: Amp(2,3,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,2
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFe(gt2) 
Mex3 = Mhh(gt3) 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFeFehhL(gt1,gt2,gt3) 
ZcoupT1R = ctcplcFeFehhR(gt1,gt2,gt3) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfER(i1,gt1)*cplcFeFehhL(i1,gt2,gt3)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfEL(i1,gt1))*cplcFeFehhR(i1,gt2,gt3)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfEL(i1,gt2)*cplcFeFehhL(gt1,i1,gt3)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfER(i1,gt2))*cplcFeFehhR(gt1,i1,gt3)
End Do


! External Field 3 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*Zfhh(i1,gt3)*cplcFeFehhL(gt1,gt2,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Zfhh(i1,gt3)*cplcFeFehhR(gt1,gt2,i1)
End Do


! Getting the amplitude 
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FeToFehh


Subroutine Amplitude_VERTEX_SDdiracDM_FeToFehh(MAh,MFe,MFv,MFxe,Mhh,MHp,              & 
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

Complex(dp), Intent(out) :: Amp(2,3,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,2
Amp(:,gt1, gt2, gt3) = 0._dp 
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFe(gt2) 
Mex3 = Mhh(gt3) 


! {Ah, bar[Fe], bar[Fe]}
If ((Include_in_loopAh).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MAh 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFeAhL(gt1,i2)
coup1R = cplcFeFeAhR(gt1,i2)
coup2L = cplcFeFeAhL(i3,gt2)
coup2R = cplcFeFeAhR(i3,gt2)
coup3L = cplcFeFehhL(i2,i3,gt3)
coup3R = cplcFeFehhR(i2,i3,gt3)
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


! {Fe, Ah, Ah}
If ((Include_in_loopFe).and.(Include_in_loopAh).and.(Include_in_loopAh)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MAh 
ML3 = MAh 
coup1L = cplcFeFeAhL(gt1,i1)
coup1R = cplcFeFeAhR(gt1,i1)
coup2L = cplcFeFeAhL(i1,gt2)
coup2R = cplcFeFeAhR(i1,gt2)
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


! {Fe, VZ, Ah}
If ((Include_in_loopFe).and.(Include_in_loopVZ).and.(Include_in_loopAh)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MVZ 
ML3 = MAh 
coup1L = -cplcFeFeVZR(gt1,i1)
coup1R = -cplcFeFeVZL(gt1,i1)
coup2L = cplcFeFeAhL(i1,gt2)
coup2R = cplcFeFeAhR(i1,gt2)
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


! {Fe, hh, hh}
If ((Include_in_loopFe).and.(Include_in_loophh).and.(Include_in_loophh)) Then 
Do i1=1,3
  Do i2=1,2
    Do i3=1,2
ML1 = MFe(i1) 
ML2 = Mhh(i2) 
ML3 = Mhh(i3) 
coup1L = cplcFeFehhL(gt1,i1,i2)
coup1R = cplcFeFehhR(gt1,i1,i2)
coup2L = cplcFeFehhL(i1,gt2,i3)
coup2R = cplcFeFehhR(i1,gt2,i3)
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


! {Fe, Ah, VZ}
If ((Include_in_loopFe).and.(Include_in_loopAh).and.(Include_in_loopVZ)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MAh 
ML3 = MVZ 
coup1L = cplcFeFeAhL(gt1,i1)
coup1R = cplcFeFeAhR(gt1,i1)
coup2L = -cplcFeFeVZR(i1,gt2)
coup2R = -cplcFeFeVZL(i1,gt2)
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


! {Fe, VZ, VZ}
If ((Include_in_loopFe).and.(Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MVZ 
ML3 = MVZ 
coup1L = -cplcFeFeVZR(gt1,i1)
coup1R = -cplcFeFeVZL(gt1,i1)
coup2L = -cplcFeFeVZR(i1,gt2)
coup2R = -cplcFeFeVZL(i1,gt2)
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


! {Fv, Hp, Hp}
If ((Include_in_loopFv).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MHp 
ML3 = MHp 
coup1L = cplcFeFvcHpL(gt1,i1)
coup1R = cplcFeFvcHpR(gt1,i1)
coup2L = cplcFvFeHpL(i1,gt2)
coup2R = cplcFvFeHpR(i1,gt2)
coup3 = cplhhHpcHp(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fv, VWp, Hp}
If ((Include_in_loopFv).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MVWp 
ML3 = MHp 
coup1L = -cplcFeFvcVWpR(gt1,i1)
coup1R = -cplcFeFvcVWpL(gt1,i1)
coup2L = cplcFvFeHpL(i1,gt2)
coup2R = cplcFvFeHpR(i1,gt2)
coup3 = -cplhhcHpVWp(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fv, Hp, VWp}
If ((Include_in_loopFv).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MHp 
ML3 = MVWp 
coup1L = cplcFeFvcHpL(gt1,i1)
coup1R = cplcFeFvcHpR(gt1,i1)
coup2L = -cplcFvFeVWpR(i1,gt2)
coup2R = -cplcFvFeVWpL(i1,gt2)
coup3 = -cplhhHpcVWp(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fv, VWp, VWp}
If ((Include_in_loopFv).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MVWp 
ML3 = MVWp 
coup1L = -cplcFeFvcVWpR(gt1,i1)
coup1R = -cplcFeFvcVWpL(gt1,i1)
coup2L = -cplcFvFeVWpR(i1,gt2)
coup2R = -cplcFvFeVWpL(i1,gt2)
coup3 = cplhhcVWpVWp(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxe, Ssc, Ssc}
If ((Include_in_loopFxe).and.(Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
  Do i2=1,2
    Do i3=1,2
ML1 = MFxe 
ML2 = MSsc(i2) 
ML3 = MSsc(i3) 
coup1L = cplcFeFxecSscL(gt1,i2)
coup1R = cplcFeFxecSscR(gt1,i2)
coup2L = cplcFxeFeSscL(gt2,i3)
coup2R = cplcFxeFeSscR(gt2,i3)
coup3 = cplhhSsccSsc(gt3,i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 


! {hh, bar[Fe], bar[Fe]}
If ((Include_in_loophh).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = Mhh(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFehhL(gt1,i2,i1)
coup1R = cplcFeFehhR(gt1,i2,i1)
coup2L = cplcFeFehhL(i3,gt2,i1)
coup2R = cplcFeFehhR(i3,gt2,i1)
coup3L = cplcFeFehhL(i2,i3,gt3)
coup3R = cplcFeFehhR(i2,i3,gt3)
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


! {VP, bar[Fe], bar[Fe]}
If ((Include_in_loopVP).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = -cplcFeFeVPR(gt1,i2)
coup1R = -cplcFeFeVPL(gt1,i2)
coup2L = -cplcFeFeVPR(i3,gt2)
coup2R = -cplcFeFeVPL(i3,gt2)
coup3L = cplcFeFehhL(i2,i3,gt3)
coup3R = cplcFeFehhR(i2,i3,gt3)
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


! {VZ, bar[Fe], bar[Fe]}
If ((Include_in_loopVZ).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVZ 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = -cplcFeFeVZR(gt1,i2)
coup1R = -cplcFeFeVZL(gt1,i2)
coup2L = -cplcFeFeVZR(i3,gt2)
coup2R = -cplcFeFeVZL(i3,gt2)
coup3L = cplcFeFehhL(i2,i3,gt3)
coup3R = cplcFeFehhR(i2,i3,gt3)
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
End Subroutine Amplitude_VERTEX_SDdiracDM_FeToFehh


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FeToFehh(MAh,MFe,MFv,MFxe,Mhh,               & 
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

Complex(dp), Intent(out) :: Amp(2,3,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,2
Amp(:,gt1, gt2, gt3) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFe(gt2) 
Mex3 = Mhh(gt3) 


! {VP, bar[Fe], bar[Fe]}
If ((Include_in_loopVP).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = -cplcFeFeVPR(gt1,i2)
coup1R = -cplcFeFeVPL(gt1,i2)
coup2L = -cplcFeFeVPR(i3,gt2)
coup2R = -cplcFeFeVPL(i3,gt2)
coup3L = cplcFeFehhL(i2,i3,gt3)
coup3R = cplcFeFehhR(i2,i3,gt3)
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
    End Do
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FeToFehh


Subroutine Amplitude_Tree_SDdiracDM_FeToFeVZ(cplcFeFeVZL,cplcFeFeVZR,MFe,             & 
& MVZ,MFe2,MVZ2,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MVZ,MFe2(3),MVZ2

Complex(dp), Intent(in) :: cplcFeFeVZL(3,3),cplcFeFeVZR(3,3)

Complex(dp) :: Amp(4,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,3
  Do gt2=1,3
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MVZ 
! Tree-Level Vertex 
coupT1L = cplcFeFeVZL(gt1,gt2)
coupT1R = cplcFeFeVZR(gt1,gt2)
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,coupT1R,coupT1L,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2) = -AmpC 
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_FeToFeVZ


Subroutine Gamma_Real_SDdiracDM_FeToFeVZ(MLambda,em,gs,cplcFeFeVZL,cplcFeFeVZR,       & 
& MFe,MVZ,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFeFeVZL(3,3),cplcFeFeVZR(3,3)

Real(dp), Intent(in) :: MFe(3),MVZ

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(3,3), GammarealGluon(3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,3
  Do i2=1,3
CoupL = cplcFeFeVZL(i1,i2)
CoupR = cplcFeFeVZR(i1,i2)
Mex1 = MFe(i1)
Mex2 = MFe(i2)
Mex3 = MVZ
If (Mex1.gt.(Mex2+Mex3)) Then 
  Call hardphotonFFZ(Mex1,Mex2,Mex3,MLambda,-1._dp,-1._dp,CoupL,CoupR,(0,1)*em,GammaRealPhoton(i1,i2),kont)
  GammarealGluon(i1,i2) = 0._dp 
Else 
  GammarealGluon(i1,i2) = 0._dp 
  GammarealPhoton(i1,i2) = 0._dp 

End if 
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_FeToFeVZ


Subroutine Amplitude_WAVE_SDdiracDM_FeToFeVZ(cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,     & 
& cplcFeFeVZR,ctcplcFeFeVPL,ctcplcFeFeVPR,ctcplcFeFeVZL,ctcplcFeFeVZR,MFe,               & 
& MFe2,MVP,MVP2,MVZ,MVZ2,ZfEL,ZfER,ZfVPVZ,ZfVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFe2(3),MVP,MVP2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3)

Complex(dp), Intent(in) :: ctcplcFeFeVPL(3,3),ctcplcFeFeVPR(3,3),ctcplcFeFeVZL(3,3),ctcplcFeFeVZR(3,3)

Complex(dp), Intent(in) :: ZfEL(3,3),ZfER(3,3),ZfVPVZ,ZfVZ

Complex(dp), Intent(out) :: Amp(4,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,3
  Do gt2=1,3
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MVZ 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFeFeVZL(gt1,gt2) 
ZcoupT1R = ctcplcFeFeVZR(gt1,gt2)
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*Conjg(ZfEL(i1,gt1))*cplcFeFeVZL(i1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfER(i1,gt1)*cplcFeFeVZR(i1,gt2)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfEL(i1,gt2)*cplcFeFeVZL(gt1,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfER(i1,gt2))*cplcFeFeVZR(gt1,i1)
End Do


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVPVZ*cplcFeFeVPL(gt1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfVPVZ*cplcFeFeVPR(gt1,gt2)
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVZ*cplcFeFeVZL(gt1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfVZ*cplcFeFeVZR(gt1,gt2)


! Getting the amplitude 
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,ZcoupT1R,ZcoupT1L,AmpC) 
Amp(:,gt1, gt2) = -AmpC 
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FeToFeVZ


Subroutine Amplitude_VERTEX_SDdiracDM_FeToFeVZ(MAh,MFe,MFv,MFxe,Mhh,MHp,              & 
& MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFeFeAhL,    & 
& cplcFeFeAhR,cplAhhhVZ,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,   & 
& cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,           & 
& cplcFeFeVZR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,           & 
& cplcFeFvcVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,               & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),MFv2(3),          & 
& MFxe2,Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplAhhhVZ(2),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2), & 
& cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),               & 
& cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFeFeVZL(3,3),& 
& cplcFeFeVZR(3,3),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),& 
& cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL(3,2), & 
& cplcFeFxecSscR(3,2),cplhhVZVZ(2),cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ

Complex(dp), Intent(out) :: Amp(4,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,3
  Do gt2=1,3
Amp(:,gt1, gt2) = 0._dp 
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MVZ 


! {Ah, bar[Fe], bar[Fe]}
If ((Include_in_loopAh).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MAh 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFeAhL(gt1,i2)
coup1R = cplcFeFeAhR(gt1,i2)
coup2L = cplcFeFeAhL(i3,gt2)
coup2R = cplcFeFeAhR(i3,gt2)
coup3L = cplcFeFeVZL(i2,i3)
coup3R = cplcFeFeVZR(i2,i3)
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


! {Fe, hh, Ah}
If ((Include_in_loopFe).and.(Include_in_loophh).and.(Include_in_loopAh)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFe(i1) 
ML2 = Mhh(i2) 
ML3 = MAh 
coup1L = cplcFeFehhL(gt1,i1,i2)
coup1R = cplcFeFehhR(gt1,i1,i2)
coup2L = cplcFeFeAhL(i1,gt2)
coup2R = cplcFeFeAhR(i1,gt2)
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


! {Fe, Ah, hh}
If ((Include_in_loopFe).and.(Include_in_loopAh).and.(Include_in_loophh)) Then 
Do i1=1,3
    Do i3=1,2
ML1 = MFe(i1) 
ML2 = MAh 
ML3 = Mhh(i3) 
coup1L = cplcFeFeAhL(gt1,i1)
coup1R = cplcFeFeAhR(gt1,i1)
coup2L = cplcFeFehhL(i1,gt2,i3)
coup2R = cplcFeFehhR(i1,gt2,i3)
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


! {Fe, VZ, hh}
If ((Include_in_loopFe).and.(Include_in_loopVZ).and.(Include_in_loophh)) Then 
Do i1=1,3
    Do i3=1,2
ML1 = MFe(i1) 
ML2 = MVZ 
ML3 = Mhh(i3) 
coup1L = cplcFeFeVZL(gt1,i1)
coup1R = cplcFeFeVZR(gt1,i1)
coup2L = cplcFeFehhL(i1,gt2,i3)
coup2R = cplcFeFehhR(i1,gt2,i3)
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


! {Fe, hh, VZ}
If ((Include_in_loopFe).and.(Include_in_loophh).and.(Include_in_loopVZ)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFe(i1) 
ML2 = Mhh(i2) 
ML3 = MVZ 
coup1L = cplcFeFehhL(gt1,i1,i2)
coup1R = cplcFeFehhR(gt1,i1,i2)
coup2L = cplcFeFeVZL(i1,gt2)
coup2R = cplcFeFeVZR(i1,gt2)
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


! {Fv, Hp, Hp}
If ((Include_in_loopFv).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MHp 
ML3 = MHp 
coup1L = cplcFeFvcHpL(gt1,i1)
coup1R = cplcFeFvcHpR(gt1,i1)
coup2L = cplcFvFeHpL(i1,gt2)
coup2R = cplcFvFeHpR(i1,gt2)
coup3 = -cplHpcHpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fv, VWp, Hp}
If ((Include_in_loopFv).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MVWp 
ML3 = MHp 
coup1L = cplcFeFvcVWpL(gt1,i1)
coup1R = cplcFeFvcVWpR(gt1,i1)
coup2L = cplcFvFeHpL(i1,gt2)
coup2R = cplcFvFeHpR(i1,gt2)
coup3 = cplcHpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fv, Hp, VWp}
If ((Include_in_loopFv).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MHp 
ML3 = MVWp 
coup1L = cplcFeFvcHpL(gt1,i1)
coup1R = cplcFeFvcHpR(gt1,i1)
coup2L = cplcFvFeVWpL(i1,gt2)
coup2R = cplcFvFeVWpR(i1,gt2)
coup3 = cplHpcVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fv, VWp, VWp}
If ((Include_in_loopFv).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MVWp 
ML3 = MVWp 
coup1L = cplcFeFvcVWpL(gt1,i1)
coup1R = cplcFeFvcVWpR(gt1,i1)
coup2L = cplcFvFeVWpL(i1,gt2)
coup2R = cplcFvFeVWpR(i1,gt2)
coup3 = -cplcVWpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {hh, bar[Fe], bar[Fe]}
If ((Include_in_loophh).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = Mhh(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFehhL(gt1,i2,i1)
coup1R = cplcFeFehhR(gt1,i2,i1)
coup2L = cplcFeFehhL(i3,gt2,i1)
coup2R = cplcFeFehhR(i3,gt2,i1)
coup3L = cplcFeFeVZL(i2,i3)
coup3R = cplcFeFeVZR(i2,i3)
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


! {VP, bar[Fe], bar[Fe]}
If ((Include_in_loopVP).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFeVPL(gt1,i2)
coup1R = cplcFeFeVPR(gt1,i2)
coup2L = cplcFeFeVPL(i3,gt2)
coup2R = cplcFeFeVPR(i3,gt2)
coup3L = cplcFeFeVZL(i2,i3)
coup3R = cplcFeFeVZR(i2,i3)
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


! {VZ, bar[Fe], bar[Fe]}
If ((Include_in_loopVZ).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVZ 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFeVZL(gt1,i2)
coup1R = cplcFeFeVZR(gt1,i2)
coup2L = cplcFeFeVZL(i3,gt2)
coup2R = cplcFeFeVZR(i3,gt2)
coup3L = cplcFeFeVZL(i2,i3)
coup3R = cplcFeFeVZR(i2,i3)
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


! {conj[Hp], bar[Fv], bar[Fv]}
If ((Include_in_loopHp).and.(Include_in_loopFv).and.(Include_in_loopFv)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MHp 
ML2 = MFv(i2) 
ML3 = MFv(i3) 
coup1L = cplcFeFvcHpL(gt1,i2)
coup1R = cplcFeFvcHpR(gt1,i2)
coup2L = cplcFvFeHpL(i3,gt2)
coup2R = cplcFvFeHpR(i3,gt2)
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
End if 


! {conj[Ssc], bar[Fxe], bar[Fxe]}
If ((Include_in_loopSsc).and.(Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
Do i1=1,2
ML1 = MSsc(i1) 
ML2 = MFxe 
ML3 = MFxe 
coup1L = cplcFeFxecSscL(gt1,i1)
coup1R = cplcFeFxecSscR(gt1,i1)
coup2L = cplcFxeFeSscL(gt2,i1)
coup2R = cplcFxeFeSscR(gt2,i1)
coup3L = cplcFxeFxeVZL
coup3R = cplcFxeFxeVZR
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {conj[VWp], bar[Fv], bar[Fv]}
If ((Include_in_loopVWp).and.(Include_in_loopFv).and.(Include_in_loopFv)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVWp 
ML2 = MFv(i2) 
ML3 = MFv(i3) 
coup1L = cplcFeFvcVWpL(gt1,i2)
coup1R = cplcFeFvcVWpR(gt1,i2)
coup2L = cplcFvFeVWpL(i3,gt2)
coup2R = cplcFvFeVWpR(i3,gt2)
coup3L = cplcFvFvVZL(i2,i3)
coup3R = cplcFvFvVZR(i2,i3)
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
End Subroutine Amplitude_VERTEX_SDdiracDM_FeToFeVZ


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FeToFeVZ(MAh,MFe,MFv,MFxe,Mhh,               & 
& MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,            & 
& cplcFeFeAhL,cplcFeFeAhR,cplAhhhVZ,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,     & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,             & 
& cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR, & 
& cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),MFv2(3),          & 
& MFxe2,Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplAhhhVZ(2),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2), & 
& cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),               & 
& cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFeFeVZL(3,3),& 
& cplcFeFeVZR(3,3),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),& 
& cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL(3,2), & 
& cplcFeFxecSscR(3,2),cplhhVZVZ(2),cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ

Complex(dp), Intent(out) :: Amp(4,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,3
  Do gt2=1,3
Amp(:,gt1, gt2) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MVZ 


! {VP, bar[Fe], bar[Fe]}
If ((Include_in_loopVP).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFeVPL(gt1,i2)
coup1R = cplcFeFeVPR(gt1,i2)
coup2L = cplcFeFeVPL(i3,gt2)
coup2R = cplcFeFeVPR(i3,gt2)
coup3L = cplcFeFeVZL(i2,i3)
coup3R = cplcFeFeVZR(i2,i3)
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FeToFeVZ


Subroutine Amplitude_Tree_SDdiracDM_FeToFvcHp(cplcFeFvcHpL,cplcFeFvcHpR,              & 
& MFe,MFv,MHp,MFe2,MFv2,MHp2,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFv(3),MHp,MFe2(3),MFv2(3),MHp2

Complex(dp), Intent(in) :: cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3)

Complex(dp) :: Amp(2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,3
  Do gt2=1,3
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MHp 
! Tree-Level Vertex 
coupT1L = cplcFeFvcHpL(gt1,gt2)
coupT1R = cplcFeFvcHpR(gt1,gt2)
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2) = AmpC 
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_FeToFvcHp


Subroutine Gamma_Real_SDdiracDM_FeToFvcHp(MLambda,em,gs,cplcFeFvcHpL,cplcFeFvcHpR,    & 
& MFe,MFv,MHp,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3)

Real(dp), Intent(in) :: MFe(3),MFv(3),MHp

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(3,3), GammarealGluon(3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,3
  Do i2=1,3
CoupL = cplcFeFvcHpL(i1,i2)
CoupR = cplcFeFvcHpR(i1,i2)
Mex1 = MFe(i1)
Mex2 = MFv(i2)
Mex3 = MHp
If (Mex1.gt.(Mex2+Mex3)) Then 
 Call hardradiationFFS(Mex1,Mex2,Mex3,MLambda,em,1._dp,0._dp,1._dp,0._dp,0._dp,1._dp,CoupL,CoupR,Gammarealphoton(i1,i2),kont)
  GammarealGluon(i1,i2) = 0._dp 
Else 
  GammarealGluon(i1,i2) = 0._dp 
  GammarealPhoton(i1,i2) = 0._dp 

End if 
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_FeToFvcHp


Subroutine Amplitude_WAVE_SDdiracDM_FeToFvcHp(cplcFeFvcHpL,cplcFeFvcHpR,              & 
& ctcplcFeFvcHpL,ctcplcFeFvcHpR,MFe,MFe2,MFv,MFv2,MHp,MHp2,ZfEL,ZfER,ZfHp,               & 
& ZfVL,ZfVR,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFe2(3),MFv(3),MFv2(3),MHp,MHp2

Complex(dp), Intent(in) :: cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3)

Complex(dp), Intent(in) :: ctcplcFeFvcHpL(3,3),ctcplcFeFvcHpR(3,3)

Complex(dp), Intent(in) :: ZfEL(3,3),ZfER(3,3),ZfHp,ZfVL(3,3),ZfVR(3,3)

Complex(dp), Intent(out) :: Amp(2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,3
  Do gt2=1,3
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MHp 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFeFvcHpL(gt1,gt2) 
ZcoupT1R = ctcplcFeFvcHpR(gt1,gt2) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfER(i1,gt1)*cplcFeFvcHpL(i1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfEL(i1,gt1))*cplcFeFvcHpR(i1,gt2)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVL(i1,gt2)*cplcFeFvcHpL(gt1,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfVR(i1,gt2))*cplcFeFvcHpR(gt1,i1)
End Do


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*Conjg(ZfHp)*cplcFeFvcHpL(gt1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfHp)*cplcFeFvcHpR(gt1,gt2)


! Getting the amplitude 
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt1, gt2) = AmpC 
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FeToFvcHp


Subroutine Amplitude_VERTEX_SDdiracDM_FeToFvcHp(MAh,MFe,MFv,MFxe,MFxv,Mhh,            & 
& MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,           & 
& MVZ2,cplcFeFeAhL,cplcFeFeAhR,cplAhcHpVWp,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,          & 
& cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,           & 
& cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,      & 
& cplcFeFxecSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplhhHpcHp,cplhhcHpVWp,cplHpcHpVP,        & 
& cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),          & 
& MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplAhcHpVWp,cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),  & 
& cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFxvFvSscL(2,3,2),& 
& cplcFxvFvSscR(2,3,2),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFeFvcHpL(3,3),              & 
& cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFeFxecSscL(3,2),           & 
& cplcFeFxecSscR(3,2),cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplhhHpcHp(2),cplhhcHpVWp(2),  & 
& cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ

Complex(dp), Intent(out) :: Amp(2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,3
  Do gt2=1,3
Amp(:,gt1, gt2) = 0._dp 
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MHp 


! {Fe, hh, conj[Hp]}
If ((Include_in_loopFe).and.(Include_in_loophh).and.(Include_in_loopHp)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFe(i1) 
ML2 = Mhh(i2) 
ML3 = MHp 
coup1L = cplcFeFehhL(gt1,i1,i2)
coup1R = cplcFeFehhR(gt1,i1,i2)
coup2L = cplcFeFvcHpL(i1,gt2)
coup2R = cplcFeFvcHpR(i1,gt2)
coup3 = cplhhHpcHp(i2)
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


! {Fe, VP, conj[Hp]}
If ((Include_in_loopFe).and.(Include_in_loopVP).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MVP 
ML3 = MHp 
coup1L = -cplcFeFeVPR(gt1,i1)
coup1R = -cplcFeFeVPL(gt1,i1)
coup2L = cplcFeFvcHpL(i1,gt2)
coup2R = cplcFeFvcHpR(i1,gt2)
coup3 = cplHpcHpVP
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fe, VZ, conj[Hp]}
If ((Include_in_loopFe).and.(Include_in_loopVZ).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MVZ 
ML3 = MHp 
coup1L = -cplcFeFeVZR(gt1,i1)
coup1R = -cplcFeFeVZL(gt1,i1)
coup2L = cplcFeFvcHpL(i1,gt2)
coup2R = cplcFeFvcHpR(i1,gt2)
coup3 = cplHpcHpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fe, Ah, conj[VWp]}
If ((Include_in_loopFe).and.(Include_in_loopAh).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MAh 
ML3 = MVWp 
coup1L = cplcFeFeAhL(gt1,i1)
coup1R = cplcFeFeAhR(gt1,i1)
coup2L = -cplcFeFvcVWpR(i1,gt2)
coup2R = -cplcFeFvcVWpL(i1,gt2)
coup3 = cplAhcHpVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fe, hh, conj[VWp]}
If ((Include_in_loopFe).and.(Include_in_loophh).and.(Include_in_loopVWp)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFe(i1) 
ML2 = Mhh(i2) 
ML3 = MVWp 
coup1L = cplcFeFehhL(gt1,i1,i2)
coup1R = cplcFeFehhR(gt1,i1,i2)
coup2L = -cplcFeFvcVWpR(i1,gt2)
coup2R = -cplcFeFvcVWpL(i1,gt2)
coup3 = cplhhcHpVWp(i2)
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


! {Fe, VP, conj[VWp]}
If ((Include_in_loopFe).and.(Include_in_loopVP).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MVP 
ML3 = MVWp 
coup1L = -cplcFeFeVPR(gt1,i1)
coup1R = -cplcFeFeVPL(gt1,i1)
coup2L = -cplcFeFvcVWpR(i1,gt2)
coup2R = -cplcFeFvcVWpL(i1,gt2)
coup3 = cplcHpVPVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fe, VZ, conj[VWp]}
If ((Include_in_loopFe).and.(Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MVZ 
ML3 = MVWp 
coup1L = -cplcFeFeVZR(gt1,i1)
coup1R = -cplcFeFeVZL(gt1,i1)
coup2L = -cplcFeFvcVWpR(i1,gt2)
coup2R = -cplcFeFvcVWpL(i1,gt2)
coup3 = cplcHpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fv, Hp, VZ}
If ((Include_in_loopFv).and.(Include_in_loopHp).and.(Include_in_loopVZ)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MHp 
ML3 = MVZ 
coup1L = cplcFeFvcHpL(gt1,i1)
coup1R = cplcFeFvcHpR(gt1,i1)
coup2L = -cplcFvFvVZR(i1,gt2)
coup2R = -cplcFvFvVZL(i1,gt2)
coup3 = cplHpcHpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fv, VWp, VZ}
If ((Include_in_loopFv).and.(Include_in_loopVWp).and.(Include_in_loopVZ)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MVWp 
ML3 = MVZ 
coup1L = -cplcFeFvcVWpR(gt1,i1)
coup1R = -cplcFeFvcVWpL(gt1,i1)
coup2L = -cplcFvFvVZR(i1,gt2)
coup2R = -cplcFvFvVZL(i1,gt2)
coup3 = cplcHpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {VZ, bar[Fe], bar[Fv]}
If ((Include_in_loopVZ).and.(Include_in_loopFe).and.(Include_in_loopFv)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVZ 
ML2 = MFe(i2) 
ML3 = MFv(i3) 
coup1L = -cplcFeFeVZR(gt1,i2)
coup1R = -cplcFeFeVZL(gt1,i2)
coup2L = -cplcFvFvVZR(i3,gt2)
coup2R = -cplcFvFvVZL(i3,gt2)
coup3L = cplcFeFvcHpL(i2,i3)
coup3R = cplcFeFvcHpR(i2,i3)
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


! {conj[Ssc], bar[Fxe], bar[Fxv]}
If ((Include_in_loopSsc).and.(Include_in_loopFxe).and.(Include_in_loopFxv)) Then 
Do i1=1,2
    Do i3=1,2
ML1 = MSsc(i1) 
ML2 = MFxe 
ML3 = MFxv(i3) 
coup1L = cplcFeFxecSscL(gt1,i1)
coup1R = cplcFeFxecSscR(gt1,i1)
coup2L = cplcFxvFvSscL(i3,gt2,i1)
coup2R = cplcFxvFvSscR(i3,gt2,i1)
coup3L = cplcFxeFxvcHpL(i3)
coup3R = cplcFxeFxvcHpR(i3)
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
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_FeToFvcHp


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FeToFvcHp(MAh,MFe,MFv,MFxe,MFxv,             & 
& Mhh,MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,             & 
& MVWp2,MVZ2,cplcFeFeAhL,cplcFeFeAhR,cplAhcHpVWp,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,    & 
& cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,           & 
& cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,      & 
& cplcFeFxecSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplhhHpcHp,cplhhcHpVWp,cplHpcHpVP,        & 
& cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),          & 
& MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplAhcHpVWp,cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),  & 
& cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFxvFvSscL(2,3,2),& 
& cplcFxvFvSscR(2,3,2),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFeFvcHpL(3,3),              & 
& cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFeFxecSscL(3,2),           & 
& cplcFeFxecSscR(3,2),cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplhhHpcHp(2),cplhhcHpVWp(2),  & 
& cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ

Complex(dp), Intent(out) :: Amp(2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,3
  Do gt2=1,3
Amp(:,gt1, gt2) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MHp 


! {Fe, VP, conj[Hp]}
If ((Include_in_loopFe).and.(Include_in_loopVP).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MVP 
ML3 = MHp 
coup1L = -cplcFeFeVPR(gt1,i1)
coup1R = -cplcFeFeVPL(gt1,i1)
coup2L = cplcFeFvcHpL(i1,gt2)
coup2R = cplcFeFvcHpR(i1,gt2)
coup3 = cplHpcHpVP
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fe, VP, conj[VWp]}
If ((Include_in_loopFe).and.(Include_in_loopVP).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MVP 
ML3 = MVWp 
coup1L = -cplcFeFeVPR(gt1,i1)
coup1R = -cplcFeFeVPL(gt1,i1)
coup2L = -cplcFeFvcVWpR(i1,gt2)
coup2R = -cplcFeFvcVWpL(i1,gt2)
coup3 = cplcHpVPVWp
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FeToFvcHp


Subroutine Amplitude_Tree_SDdiracDM_FeToFvcVWp(cplcFeFvcVWpL,cplcFeFvcVWpR,           & 
& MFe,MFv,MVWp,MFe2,MFv2,MVWp2,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFv(3),MVWp,MFe2(3),MFv2(3),MVWp2

Complex(dp), Intent(in) :: cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3)

Complex(dp) :: Amp(4,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,3
  Do gt2=1,3
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MVWp 
! Tree-Level Vertex 
coupT1L = cplcFeFvcVWpL(gt1,gt2)
coupT1R = cplcFeFvcVWpR(gt1,gt2)
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,coupT1R,coupT1L,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2) = -AmpC 
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_FeToFvcVWp


Subroutine Gamma_Real_SDdiracDM_FeToFvcVWp(MLambda,em,gs,cplcFeFvcVWpL,               & 
& cplcFeFvcVWpR,MFe,MFv,MVWp,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3)

Real(dp), Intent(in) :: MFe(3),MFv(3),MVWp

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(3,3), GammarealGluon(3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,3
  Do i2=1,3
CoupL = cplcFeFvcVWpL(i1,i2)
CoupR = cplcFeFvcVWpR(i1,i2)
Mex1 = MFe(i1)
Mex2 = MFv(i2)
Mex3 = MVWp
If (Mex1.gt.(Mex2+Mex3)) Then 
  Call hardphotonFFW(Mex1,Mex2,Mex3,MLambda,-1._dp,0._dp,CoupL,CoupR,(0,1)*em,GammaRealPhoton(i1,i2),kont)
  GammarealGluon(i1,i2) = 0._dp 
Else 
  GammarealGluon(i1,i2) = 0._dp 
  GammarealPhoton(i1,i2) = 0._dp 

End if 
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_FeToFvcVWp


Subroutine Amplitude_WAVE_SDdiracDM_FeToFvcVWp(cplcFeFvcVWpL,cplcFeFvcVWpR,           & 
& ctcplcFeFvcVWpL,ctcplcFeFvcVWpR,MFe,MFe2,MFv,MFv2,MVWp,MVWp2,ZfEL,ZfER,ZfVL,           & 
& ZfVR,ZfVWp,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFe2(3),MFv(3),MFv2(3),MVWp,MVWp2

Complex(dp), Intent(in) :: cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3)

Complex(dp), Intent(in) :: ctcplcFeFvcVWpL(3,3),ctcplcFeFvcVWpR(3,3)

Complex(dp), Intent(in) :: ZfEL(3,3),ZfER(3,3),ZfVL(3,3),ZfVR(3,3),ZfVWp

Complex(dp), Intent(out) :: Amp(4,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,3
  Do gt2=1,3
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MVWp 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFeFvcVWpL(gt1,gt2) 
ZcoupT1R = ctcplcFeFvcVWpR(gt1,gt2)
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*Conjg(ZfEL(i1,gt1))*cplcFeFvcVWpL(i1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfER(i1,gt1)*cplcFeFvcVWpR(i1,gt2)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVL(i1,gt2)*cplcFeFvcVWpL(gt1,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfVR(i1,gt2))*cplcFeFvcVWpR(gt1,i1)
End Do


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVWp*cplcFeFvcVWpL(gt1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfVWp*cplcFeFvcVWpR(gt1,gt2)


! Getting the amplitude 
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,ZcoupT1R,ZcoupT1L,AmpC) 
Amp(:,gt1, gt2) = -AmpC 
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FeToFvcVWp


Subroutine Amplitude_VERTEX_SDdiracDM_FeToFvcVWp(MAh,MFe,MFv,MFxe,MFxv,               & 
& Mhh,MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,             & 
& MVWp2,MVZ2,cplcFeFeAhL,cplcFeFeAhR,cplAhHpcVWp,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,    & 
& cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,           & 
& cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,      & 
& cplcFeFxecSscR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcVWp,cplhhcVWpVWp,               & 
& cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),          & 
& MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplAhHpcVWp,cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),  & 
& cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFxvFvSscL(2,3,2),& 
& cplcFxvFvSscR(2,3,2),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFeFvcHpL(3,3),              & 
& cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFeFxecSscL(3,2),           & 
& cplcFeFxecSscR(3,2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplhhHpcVWp(2),              & 
& cplhhcVWpVWp(2),cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ

Complex(dp), Intent(out) :: Amp(4,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,3
  Do gt2=1,3
Amp(:,gt1, gt2) = 0._dp 
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MVWp 


! {Fe, Ah, conj[Hp]}
If ((Include_in_loopFe).and.(Include_in_loopAh).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MAh 
ML3 = MHp 
coup1L = cplcFeFeAhL(gt1,i1)
coup1R = cplcFeFeAhR(gt1,i1)
coup2L = cplcFeFvcHpL(i1,gt2)
coup2R = cplcFeFvcHpR(i1,gt2)
coup3 = -cplAhHpcVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fe, hh, conj[Hp]}
If ((Include_in_loopFe).and.(Include_in_loophh).and.(Include_in_loopHp)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFe(i1) 
ML2 = Mhh(i2) 
ML3 = MHp 
coup1L = cplcFeFehhL(gt1,i1,i2)
coup1R = cplcFeFehhR(gt1,i1,i2)
coup2L = cplcFeFvcHpL(i1,gt2)
coup2R = cplcFeFvcHpR(i1,gt2)
coup3 = -cplhhHpcVWp(i2)
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


! {Fe, VP, conj[Hp]}
If ((Include_in_loopFe).and.(Include_in_loopVP).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MVP 
ML3 = MHp 
coup1L = cplcFeFeVPL(gt1,i1)
coup1R = cplcFeFeVPR(gt1,i1)
coup2L = cplcFeFvcHpL(i1,gt2)
coup2R = cplcFeFvcHpR(i1,gt2)
coup3 = cplHpcVWpVP
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fe, VZ, conj[Hp]}
If ((Include_in_loopFe).and.(Include_in_loopVZ).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MVZ 
ML3 = MHp 
coup1L = cplcFeFeVZL(gt1,i1)
coup1R = cplcFeFeVZR(gt1,i1)
coup2L = cplcFeFvcHpL(i1,gt2)
coup2R = cplcFeFvcHpR(i1,gt2)
coup3 = cplHpcVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fe, hh, conj[VWp]}
If ((Include_in_loopFe).and.(Include_in_loophh).and.(Include_in_loopVWp)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFe(i1) 
ML2 = Mhh(i2) 
ML3 = MVWp 
coup1L = cplcFeFehhL(gt1,i1,i2)
coup1R = cplcFeFehhR(gt1,i1,i2)
coup2L = cplcFeFvcVWpL(i1,gt2)
coup2R = cplcFeFvcVWpR(i1,gt2)
coup3 = cplhhcVWpVWp(i2)
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


! {Fe, VP, conj[VWp]}
If ((Include_in_loopFe).and.(Include_in_loopVP).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MVP 
ML3 = MVWp 
coup1L = cplcFeFeVPL(gt1,i1)
coup1R = cplcFeFeVPR(gt1,i1)
coup2L = cplcFeFvcVWpL(i1,gt2)
coup2R = cplcFeFvcVWpR(i1,gt2)
coup3 = cplcVWpVPVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fe, VZ, conj[VWp]}
If ((Include_in_loopFe).and.(Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MVZ 
ML3 = MVWp 
coup1L = cplcFeFeVZL(gt1,i1)
coup1R = cplcFeFeVZR(gt1,i1)
coup2L = cplcFeFvcVWpL(i1,gt2)
coup2R = cplcFeFvcVWpR(i1,gt2)
coup3 = -cplcVWpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fv, Hp, VZ}
If ((Include_in_loopFv).and.(Include_in_loopHp).and.(Include_in_loopVZ)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MHp 
ML3 = MVZ 
coup1L = cplcFeFvcHpL(gt1,i1)
coup1R = cplcFeFvcHpR(gt1,i1)
coup2L = cplcFvFvVZL(i1,gt2)
coup2R = cplcFvFvVZR(i1,gt2)
coup3 = cplHpcVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fv, VWp, VZ}
If ((Include_in_loopFv).and.(Include_in_loopVWp).and.(Include_in_loopVZ)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MVWp 
ML3 = MVZ 
coup1L = cplcFeFvcVWpL(gt1,i1)
coup1R = cplcFeFvcVWpR(gt1,i1)
coup2L = cplcFvFvVZL(i1,gt2)
coup2R = cplcFvFvVZR(i1,gt2)
coup3 = cplcVWpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {VZ, bar[Fe], bar[Fv]}
If ((Include_in_loopVZ).and.(Include_in_loopFe).and.(Include_in_loopFv)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVZ 
ML2 = MFe(i2) 
ML3 = MFv(i3) 
coup1L = cplcFeFeVZL(gt1,i2)
coup1R = cplcFeFeVZR(gt1,i2)
coup2L = cplcFvFvVZL(i3,gt2)
coup2R = cplcFvFvVZR(i3,gt2)
coup3L = cplcFeFvcVWpL(i2,i3)
coup3R = cplcFeFvcVWpR(i2,i3)
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


! {conj[Ssc], bar[Fxe], bar[Fxv]}
If ((Include_in_loopSsc).and.(Include_in_loopFxe).and.(Include_in_loopFxv)) Then 
Do i1=1,2
    Do i3=1,2
ML1 = MSsc(i1) 
ML2 = MFxe 
ML3 = MFxv(i3) 
coup1L = cplcFeFxecSscL(gt1,i1)
coup1R = cplcFeFxecSscR(gt1,i1)
coup2L = cplcFxvFvSscL(i3,gt2,i1)
coup2R = cplcFxvFvSscR(i3,gt2,i1)
coup3L = cplcFxeFxvcVWpL(i3)
coup3R = cplcFxeFxvcVWpR(i3)
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
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_FeToFvcVWp


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FeToFvcVWp(MAh,MFe,MFv,MFxe,MFxv,            & 
& Mhh,MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,             & 
& MVWp2,MVZ2,cplcFeFeAhL,cplcFeFeAhR,cplAhHpcVWp,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,    & 
& cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,           & 
& cplcFvFvVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFeFxecSscL,      & 
& cplcFeFxecSscR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcVWp,cplhhcVWpVWp,               & 
& cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),          & 
& MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplAhHpcVWp,cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),  & 
& cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFxvFvSscL(2,3,2),& 
& cplcFxvFvSscR(2,3,2),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),cplcFeFvcHpL(3,3),              & 
& cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFeFxecSscL(3,2),           & 
& cplcFeFxecSscR(3,2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplhhHpcVWp(2),              & 
& cplhhcVWpVWp(2),cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ

Complex(dp), Intent(out) :: Amp(4,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,3
  Do gt2=1,3
Amp(:,gt1, gt2) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MVWp 


! {Fe, VP, conj[Hp]}
If ((Include_in_loopFe).and.(Include_in_loopVP).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MVP 
ML3 = MHp 
coup1L = cplcFeFeVPL(gt1,i1)
coup1R = cplcFeFeVPR(gt1,i1)
coup2L = cplcFeFvcHpL(i1,gt2)
coup2R = cplcFeFvcHpR(i1,gt2)
coup3 = cplHpcVWpVP
Call Amp_VERTEX_FtoFV_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fe, VP, conj[VWp]}
If ((Include_in_loopFe).and.(Include_in_loopVP).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MVP 
ML3 = MVWp 
coup1L = cplcFeFeVPL(gt1,i1)
coup1R = cplcFeFeVPR(gt1,i1)
coup2L = cplcFeFvcVWpL(i1,gt2)
coup2R = cplcFeFvcVWpR(i1,gt2)
coup3 = cplcVWpVPVWp
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FeToFvcVWp


Subroutine Amplitude_Tree_SDdiracDM_FeToFxecSsc(cplcFeFxecSscL,cplcFeFxecSscR,        & 
& MFe,MFxe,MSsc,MFe2,MFxe2,MSsc2,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFxe,MSsc(2),MFe2(3),MFxe2,MSsc2(2)

Complex(dp), Intent(in) :: cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2)

Complex(dp) :: Amp(2,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,3
    Do gt3=1,2
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFxe 
Mex3 = MSsc(gt3) 
! Tree-Level Vertex 
coupT1L = cplcFeFxecSscL(gt1,gt3)
coupT1R = cplcFeFxecSscR(gt1,gt3)
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt3) = AmpC 
    End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_FeToFxecSsc


Subroutine Gamma_Real_SDdiracDM_FeToFxecSsc(MLambda,em,gs,cplcFeFxecSscL,             & 
& cplcFeFxecSscR,MFe,MFxe,MSsc,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2)

Real(dp), Intent(in) :: MFe(3),MFxe,MSsc(2)

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(3,2), GammarealGluon(3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,3
    Do i3=1,2
CoupL = cplcFeFxecSscL(i1,i3)
CoupR = cplcFeFxecSscR(i1,i3)
Mex1 = MFe(i1)
Mex2 = MFxe
Mex3 = MSsc(i3)
If (Mex1.gt.(Mex2+Mex3)) Then 
 Call hardradiationFFS(Mex1,Mex2,Mex3,MLambda,em,1._dp,1._dp,0._dp,1._dp,0._dp,0._dp,CoupL,CoupR,Gammarealphoton(i1,i3),kont)
  GammarealGluon(i1,i3) = 0._dp 
Else 
  GammarealGluon(i1,i3) = 0._dp 
  GammarealPhoton(i1,i3) = 0._dp 

End if 
    End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_FeToFxecSsc


Subroutine Amplitude_WAVE_SDdiracDM_FeToFxecSsc(cplcFeFxecSscL,cplcFeFxecSscR,        & 
& ctcplcFeFxecSscL,ctcplcFeFxecSscR,MFe,MFe2,MFxe,MFxe2,MSsc,MSsc2,Zfed,ZfEL,            & 
& ZfER,Zfeu,ZfSsc,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFe2(3),MFxe,MFxe2,MSsc(2),MSsc2(2)

Complex(dp), Intent(in) :: cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2)

Complex(dp), Intent(in) :: ctcplcFeFxecSscL(3,2),ctcplcFeFxecSscR(3,2)

Complex(dp), Intent(in) :: Zfed,ZfEL(3,3),ZfER(3,3),Zfeu,ZfSsc(2,2)

Complex(dp), Intent(out) :: Amp(2,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,3
    Do gt3=1,2
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFxe 
Mex3 = MSsc(gt3) 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFeFxecSscL(gt1,gt3) 
ZcoupT1R = ctcplcFeFxecSscR(gt1,gt3) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfER(i1,gt1)*cplcFeFxecSscL(i1,gt3)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfEL(i1,gt1))*cplcFeFxecSscR(i1,gt3)
End Do


! External Field 2 
ZcoupT1L = ZcoupT1L + 0.5_dp*Zfed*cplcFeFxecSscL(gt1,gt3)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(Zfeu)*cplcFeFxecSscR(gt1,gt3)


! External Field 3 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*Conjg(ZfSsc(i1,gt3))*cplcFeFxecSscL(gt1,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfSsc(i1,gt3))*cplcFeFxecSscR(gt1,i1)
End Do


! Getting the amplitude 
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt1, gt3) = AmpC 
    End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FeToFxecSsc


Subroutine Amplitude_VERTEX_SDdiracDM_FeToFxecSsc(MFe,MFv,MFxe,MFxv,Mhh,              & 
& MHp,MSsc,MVP,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,           & 
& cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,               & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,cplcFxvFxeHpR,     & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,               & 
& cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFvFxvcSscL,cplcFvFxvcSscR,             & 
& cplhhSsccSsc,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MFe2(3),MFv2(3),           & 
& MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),              & 
& cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),& 
& cplcFeFvcVWpR(3,3),cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxeFxeVPL,cplcFxeFxeVPR,      & 
& cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL(3,2),   & 
& cplcFeFxecSscR(3,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),cplhhSsccSsc(2,2,2)

Complex(dp), Intent(out) :: Amp(2,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,3
    Do gt3=1,2
Amp(:,gt1, gt3) = 0._dp 
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFxe 
Mex3 = MSsc(gt3) 


! {Fe, hh, conj[Ssc]}
If ((Include_in_loopFe).and.(Include_in_loophh).and.(Include_in_loopSsc)) Then 
Do i1=1,3
  Do i2=1,2
    Do i3=1,2
ML1 = MFe(i1) 
ML2 = Mhh(i2) 
ML3 = MSsc(i3) 
coup1L = cplcFeFehhL(gt1,i1,i2)
coup1R = cplcFeFehhR(gt1,i1,i2)
coup2L = cplcFeFxecSscL(i1,i3)
coup2R = cplcFeFxecSscR(i1,i3)
coup3 = cplhhSsccSsc(i2,i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt3) = Amp(:,gt1, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {VP, bar[Fe], bar[Fxe]}
If ((Include_in_loopVP).and.(Include_in_loopFe).and.(Include_in_loopFxe)) Then 
  Do i2=1,3
ML1 = MVP 
ML2 = MFe(i2) 
ML3 = MFxe 
coup1L = -cplcFeFeVPR(gt1,i2)
coup1R = -cplcFeFeVPL(gt1,i2)
coup2L = cplcFxeFxeVPL
coup2R = cplcFxeFxeVPR
coup3L = cplcFeFxecSscL(i2,gt3)
coup3R = cplcFeFxecSscR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt3) = Amp(:,gt1, gt3) + oo16pi2*(1)*AmpC 
  End Do
End if 


! {VZ, bar[Fe], bar[Fxe]}
If ((Include_in_loopVZ).and.(Include_in_loopFe).and.(Include_in_loopFxe)) Then 
  Do i2=1,3
ML1 = MVZ 
ML2 = MFe(i2) 
ML3 = MFxe 
coup1L = -cplcFeFeVZR(gt1,i2)
coup1R = -cplcFeFeVZL(gt1,i2)
coup2L = cplcFxeFxeVZL
coup2R = cplcFxeFxeVZR
coup3L = cplcFeFxecSscL(i2,gt3)
coup3R = cplcFeFxecSscR(i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt3) = Amp(:,gt1, gt3) + oo16pi2*(1)*AmpC 
  End Do
End if 


! {conj[Hp], bar[Fv], bar[Fxv]}
If ((Include_in_loopHp).and.(Include_in_loopFv).and.(Include_in_loopFxv)) Then 
  Do i2=1,3
    Do i3=1,2
ML1 = MHp 
ML2 = MFv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFeFvcHpL(gt1,i2)
coup1R = cplcFeFvcHpR(gt1,i2)
coup2L = cplcFxvFxeHpL(i3)
coup2R = cplcFxvFxeHpR(i3)
coup3L = cplcFvFxvcSscL(i2,i3,gt3)
coup3R = cplcFvFxvcSscR(i2,i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt3) = Amp(:,gt1, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 


! {conj[VWp], bar[Fv], bar[Fxv]}
If ((Include_in_loopVWp).and.(Include_in_loopFv).and.(Include_in_loopFxv)) Then 
  Do i2=1,3
    Do i3=1,2
ML1 = MVWp 
ML2 = MFv(i2) 
ML3 = MFxv(i3) 
coup1L = -cplcFeFvcVWpR(gt1,i2)
coup1R = -cplcFeFvcVWpL(gt1,i2)
coup2L = cplcFxvFxeVWpL(i3)
coup2R = cplcFxvFxeVWpR(i3)
coup3L = cplcFvFxvcSscL(i2,i3,gt3)
coup3R = cplcFvFxvcSscR(i2,i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt3) = Amp(:,gt1, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
    End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_FeToFxecSsc


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FeToFxecSsc(MFe,MFv,MFxe,MFxv,               & 
& Mhh,MHp,MSsc,MVP,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,            & 
& MVZ2,cplcFeFehhL,cplcFeFehhR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,          & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxvFxeHpL,cplcFxvFxeHpR,     & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,               & 
& cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFvFxvcSscL,cplcFvFxvcSscR,             & 
& cplhhSsccSsc,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MFe2(3),MFv2(3),           & 
& MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),              & 
& cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),& 
& cplcFeFvcVWpR(3,3),cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxeFxeVPL,cplcFxeFxeVPR,      & 
& cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL(3,2),   & 
& cplcFeFxecSscR(3,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),cplhhSsccSsc(2,2,2)

Complex(dp), Intent(out) :: Amp(2,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,3
    Do gt3=1,2
Amp(:,gt1, gt3) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFxe 
Mex3 = MSsc(gt3) 


! {VP, bar[Fe], bar[Fxe]}
If ((Include_in_loopVP).and.(Include_in_loopFe).and.(Include_in_loopFxe)) Then 
  Do i2=1,3
ML1 = MVP 
ML2 = MFe(i2) 
ML3 = MFxe 
coup1L = -cplcFeFeVPR(gt1,i2)
coup1R = -cplcFeFeVPL(gt1,i2)
coup2L = cplcFxeFxeVPL
coup2R = cplcFxeFxeVPR
coup3L = cplcFeFxecSscL(i2,gt3)
coup3R = cplcFeFxecSscR(i2,gt3)
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt3) = Amp(:,gt1, gt3) + oo16pi2*(1)*AmpC 
  End Do
End if 
    End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FeToFxecSsc


Subroutine Amplitude_WAVE_SDdiracDM_FeToFeVP(cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,     & 
& cplcFeFeVZR,ctcplcFeFeVPL,ctcplcFeFeVPR,ctcplcFeFeVZL,ctcplcFeFeVZR,MFe,               & 
& MFe2,MVP,MVP2,ZfEL,ZfER,ZfVP,ZfVZVP,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFe2(3),MVP,MVP2

Complex(dp), Intent(in) :: cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3)

Complex(dp), Intent(in) :: ctcplcFeFeVPL(3,3),ctcplcFeFeVPR(3,3),ctcplcFeFeVZL(3,3),ctcplcFeFeVZR(3,3)

Complex(dp), Intent(in) :: ZfEL(3,3),ZfER(3,3),ZfVP,ZfVZVP

Complex(dp), Intent(out) :: Amp(4,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,3
  Do gt2=1,3
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MVP 
ZcoupT1L = 0._dp 
ZcoupT1R = 0._dp 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*Conjg(ZfEL(i1,gt1))*cplcFeFeVPL(i1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfER(i1,gt1)*cplcFeFeVPR(i1,gt2)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfEL(i1,gt2)*cplcFeFeVPL(gt1,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfER(i1,gt2))*cplcFeFeVPR(gt1,i1)
End Do


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVZVP*cplcFeFeVZL(gt1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfVZVP*cplcFeFeVZR(gt1,gt2)


! Getting the amplitude 
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,ZcoupT1R,ZcoupT1L,AmpC) 
Amp(:,gt1, gt2) = -AmpC 
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FeToFeVP


Subroutine Amplitude_VERTEX_SDdiracDM_FeToFeVP(MAh,MFe,MFv,MFxe,Mhh,MHp,              & 
& MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFeFeAhL,    & 
& cplcFeFeAhR,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,             & 
& cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,           & 
& cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxeVPL,       & 
& cplcFxeFxeVPR,cplcFeFxecSscL,cplcFeFxecSscR,cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,        & 
& cplcVWpVPVWp,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),MFv2(3),          & 
& MFxe2,Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),              & 
& cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),               & 
& cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFeFeVZL(3,3),& 
& cplcFeFeVZR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),               & 
& cplcFeFvcVWpR(3,3),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),& 
& cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp

Complex(dp), Intent(out) :: Amp(4,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,3
  Do gt2=1,3
Amp(:,gt1, gt2) = 0._dp 
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MVP 


! {Ah, bar[Fe], bar[Fe]}
If ((Include_in_loopAh).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MAh 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFeAhL(gt1,i2)
coup1R = cplcFeFeAhR(gt1,i2)
coup2L = cplcFeFeAhL(i3,gt2)
coup2R = cplcFeFeAhR(i3,gt2)
coup3L = cplcFeFeVPL(i2,i3)
coup3R = cplcFeFeVPR(i2,i3)
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


! {Fv, Hp, Hp}
If ((Include_in_loopFv).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MHp 
ML3 = MHp 
coup1L = cplcFeFvcHpL(gt1,i1)
coup1R = cplcFeFvcHpR(gt1,i1)
coup2L = cplcFvFeHpL(i1,gt2)
coup2R = cplcFvFeHpR(i1,gt2)
coup3 = -cplHpcHpVP
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fv, VWp, Hp}
If ((Include_in_loopFv).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MVWp 
ML3 = MHp 
coup1L = cplcFeFvcVWpL(gt1,i1)
coup1R = cplcFeFvcVWpR(gt1,i1)
coup2L = cplcFvFeHpL(i1,gt2)
coup2R = cplcFvFeHpR(i1,gt2)
coup3 = cplcHpVPVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fv, Hp, VWp}
If ((Include_in_loopFv).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MHp 
ML3 = MVWp 
coup1L = cplcFeFvcHpL(gt1,i1)
coup1R = cplcFeFvcHpR(gt1,i1)
coup2L = cplcFvFeVWpL(i1,gt2)
coup2R = cplcFvFeVWpR(i1,gt2)
coup3 = cplHpcVWpVP
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fv, VWp, VWp}
If ((Include_in_loopFv).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFv(i1) 
ML2 = MVWp 
ML3 = MVWp 
coup1L = cplcFeFvcVWpL(gt1,i1)
coup1R = cplcFeFvcVWpR(gt1,i1)
coup2L = cplcFvFeVWpL(i1,gt2)
coup2R = cplcFvFeVWpR(i1,gt2)
coup3 = cplcVWpVPVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {hh, bar[Fe], bar[Fe]}
If ((Include_in_loophh).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = Mhh(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFehhL(gt1,i2,i1)
coup1R = cplcFeFehhR(gt1,i2,i1)
coup2L = cplcFeFehhL(i3,gt2,i1)
coup2R = cplcFeFehhR(i3,gt2,i1)
coup3L = cplcFeFeVPL(i2,i3)
coup3R = cplcFeFeVPR(i2,i3)
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


! {VP, bar[Fe], bar[Fe]}
If ((Include_in_loopVP).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFeVPL(gt1,i2)
coup1R = cplcFeFeVPR(gt1,i2)
coup2L = cplcFeFeVPL(i3,gt2)
coup2R = cplcFeFeVPR(i3,gt2)
coup3L = cplcFeFeVPL(i2,i3)
coup3R = cplcFeFeVPR(i2,i3)
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


! {VZ, bar[Fe], bar[Fe]}
If ((Include_in_loopVZ).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVZ 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFeVZL(gt1,i2)
coup1R = cplcFeFeVZR(gt1,i2)
coup2L = cplcFeFeVZL(i3,gt2)
coup2R = cplcFeFeVZR(i3,gt2)
coup3L = cplcFeFeVPL(i2,i3)
coup3R = cplcFeFeVPR(i2,i3)
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


! {conj[Ssc], bar[Fxe], bar[Fxe]}
If ((Include_in_loopSsc).and.(Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
Do i1=1,2
ML1 = MSsc(i1) 
ML2 = MFxe 
ML3 = MFxe 
coup1L = cplcFeFxecSscL(gt1,i1)
coup1R = cplcFeFxecSscR(gt1,i1)
coup2L = cplcFxeFeSscL(gt2,i1)
coup2R = cplcFxeFeSscR(gt2,i1)
coup3L = cplcFxeFxeVPL
coup3R = cplcFxeFxeVPR
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_FeToFeVP


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FeToFeVP(MAh,MFe,MFv,MFxe,Mhh,               & 
& MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,            & 
& cplcFeFeAhL,cplcFeFeAhR,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,               & 
& cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,         & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFeFvcVWpL,cplcFeFvcVWpR,         & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFeFxecSscL,cplcFeFxecSscR,cplHpcHpVP,cplHpcVWpVP,      & 
& cplcHpVPVWp,cplcVWpVPVWp,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),MFv2(3),          & 
& MFxe2,Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),              & 
& cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),               & 
& cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFeFeVZL(3,3),& 
& cplcFeFeVZR(3,3),cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFeFvcVWpL(3,3),               & 
& cplcFeFvcVWpR(3,3),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),& 
& cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp

Complex(dp), Intent(out) :: Amp(4,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,3
  Do gt2=1,3
Amp(:,gt1, gt2) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFe(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MVP 


! {VP, bar[Fe], bar[Fe]}
If ((Include_in_loopVP).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFeVPL(gt1,i2)
coup1R = cplcFeFeVPR(gt1,i2)
coup2L = cplcFeFeVPL(i3,gt2)
coup2R = cplcFeFeVPR(i3,gt2)
coup3L = cplcFeFeVPL(i2,i3)
coup3R = cplcFeFeVPR(i2,i3)
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FeToFeVP



End Module OneLoopDecay_Fe_SDdiracDM
