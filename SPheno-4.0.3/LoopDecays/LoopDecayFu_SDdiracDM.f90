! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:21 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module OneLoopDecay_Fu_SDdiracDM
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

Subroutine Amplitude_Tree_SDdiracDM_FuToFuAh(cplcFuFuAhL,cplcFuFuAhR,MAh,             & 
& MFu,MAh2,MFu2,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFu(3),MAh2,MFu2(3)

Complex(dp), Intent(in) :: cplcFuFuAhL(3,3),cplcFuFuAhR(3,3)

Complex(dp) :: Amp(2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,3
  Do gt2=1,3
! External masses 
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MAh 
! Tree-Level Vertex 
coupT1L = cplcFuFuAhL(gt1,gt2)
coupT1R = cplcFuFuAhR(gt1,gt2)
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2) = AmpC 
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_FuToFuAh


Subroutine Gamma_Real_SDdiracDM_FuToFuAh(MLambda,em,gs,cplcFuFuAhL,cplcFuFuAhR,       & 
& MAh,MFu,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFuFuAhL(3,3),cplcFuFuAhR(3,3)

Real(dp), Intent(in) :: MAh,MFu(3)

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(3,3), GammarealGluon(3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,3
  Do i2=1,3
CoupL = cplcFuFuAhL(i1,i2)
CoupR = cplcFuFuAhR(i1,i2)
Mex1 = MFu(i1)
Mex2 = MFu(i2)
Mex3 = MAh
If (Mex1.gt.(Mex2+Mex3)) Then 
 Call hardradiationFFS(Mex1,Mex2,Mex3,MLambda,em,4._dp/9._dp,4._dp/9._dp,0._dp,4._dp/9._dp,0._dp,0._dp,CoupL,CoupR,Gammarealphoton(i1,i2),kont)
 Call hardradiationFFS(Mex1,Mex2,Mex3,MLambda,gs,4._dp/3._dp,4._dp/3._dp,0._dp,4._dp/3._dp,0._dp,0._dp,CoupL,CoupR,Gammarealgluon(i1,i2),kont)
Else 
  GammarealGluon(i1,i2) = 0._dp 
  GammarealPhoton(i1,i2) = 0._dp 

End if 
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_FuToFuAh


Subroutine Amplitude_WAVE_SDdiracDM_FuToFuAh(cplcFuFuAhL,cplcFuFuAhR,ctcplcFuFuAhL,   & 
& ctcplcFuFuAhR,MAh,MAh2,MFu,MFu2,ZfAh,ZfUL,ZfUR,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MAh2,MFu(3),MFu2(3)

Complex(dp), Intent(in) :: cplcFuFuAhL(3,3),cplcFuFuAhR(3,3)

Complex(dp), Intent(in) :: ctcplcFuFuAhL(3,3),ctcplcFuFuAhR(3,3)

Complex(dp), Intent(in) :: ZfAh,ZfUL(3,3),ZfUR(3,3)

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
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MAh 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFuFuAhL(gt1,gt2) 
ZcoupT1R = ctcplcFuFuAhR(gt1,gt2) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfUR(i1,gt1)*cplcFuFuAhL(i1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfUL(i1,gt1))*cplcFuFuAhR(i1,gt2)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfUL(i1,gt2)*cplcFuFuAhL(gt1,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfUR(i1,gt2))*cplcFuFuAhR(gt1,i1)
End Do


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfAh*cplcFuFuAhL(gt1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfAh*cplcFuFuAhR(gt1,gt2)


! Getting the amplitude 
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt1, gt2) = AmpC 
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FuToFuAh


Subroutine Amplitude_VERTEX_SDdiracDM_FuToFuAh(MAh,MFd,MFu,Mhh,MHp,MVG,               & 
& MVP,MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFdFdAhL,      & 
& cplcFdFdAhR,cplcFuFuAhL,cplcFuFuAhR,cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,cplcFuFdHpL,     & 
& cplcFuFdHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,             & 
& cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,              & 
& cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFu(3),Mhh(2),MHp,MVG,MVP,MVWp,MVZ,MAh2,MFd2(3),MFu2(3),Mhh2(2),           & 
& MHp2,MVG2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),     & 
& cplAhhhVZ(2),cplAhHpcVWp,cplAhcHpVWp,cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),cplcFuFdVWpL(3,3),& 
& cplcFuFdVWpR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFuFuVGL(3,3),              & 
& cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),  & 
& cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3)

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
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MAh 


! {Ah, bar[Fu], bar[Fu]}
If ((Include_in_loopAh).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MAh 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuAhL(gt1,i2)
coup1R = cplcFuFuAhR(gt1,i2)
coup2L = cplcFuFuAhL(i3,gt2)
coup2R = cplcFuFuAhR(i3,gt2)
coup3L = cplcFuFuAhL(i2,i3)
coup3R = cplcFuFuAhR(i2,i3)
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


! {Fd, conj[VWp], conj[Hp]}
If ((Include_in_loopFd).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MVWp 
ML3 = MHp 
coup1L = -cplcFuFdVWpR(gt1,i1)
coup1R = -cplcFuFdVWpL(gt1,i1)
coup2L = cplcFdFucHpL(i1,gt2)
coup2R = cplcFdFucHpR(i1,gt2)
coup3 = -cplAhHpcVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fd, conj[Hp], conj[VWp]}
If ((Include_in_loopFd).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MHp 
ML3 = MVWp 
coup1L = cplcFuFdHpL(gt1,i1)
coup1R = cplcFuFdHpR(gt1,i1)
coup2L = -cplcFdFucVWpR(i1,gt2)
coup2R = -cplcFdFucVWpL(i1,gt2)
coup3 = -cplAhcHpVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fu, hh, Ah}
If ((Include_in_loopFu).and.(Include_in_loophh).and.(Include_in_loopAh)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFu(i1) 
ML2 = Mhh(i2) 
ML3 = MAh 
coup1L = cplcFuFuhhL(gt1,i1,i2)
coup1R = cplcFuFuhhR(gt1,i1,i2)
coup2L = cplcFuFuAhL(i1,gt2)
coup2R = cplcFuFuAhR(i1,gt2)
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


! {Fu, Ah, hh}
If ((Include_in_loopFu).and.(Include_in_loopAh).and.(Include_in_loophh)) Then 
Do i1=1,3
    Do i3=1,2
ML1 = MFu(i1) 
ML2 = MAh 
ML3 = Mhh(i3) 
coup1L = cplcFuFuAhL(gt1,i1)
coup1R = cplcFuFuAhR(gt1,i1)
coup2L = cplcFuFuhhL(i1,gt2,i3)
coup2R = cplcFuFuhhR(i1,gt2,i3)
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


! {Fu, VZ, hh}
If ((Include_in_loopFu).and.(Include_in_loopVZ).and.(Include_in_loophh)) Then 
Do i1=1,3
    Do i3=1,2
ML1 = MFu(i1) 
ML2 = MVZ 
ML3 = Mhh(i3) 
coup1L = -cplcFuFuVZR(gt1,i1)
coup1R = -cplcFuFuVZL(gt1,i1)
coup2L = cplcFuFuhhL(i1,gt2,i3)
coup2R = cplcFuFuhhR(i1,gt2,i3)
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


! {Fu, hh, VZ}
If ((Include_in_loopFu).and.(Include_in_loophh).and.(Include_in_loopVZ)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFu(i1) 
ML2 = Mhh(i2) 
ML3 = MVZ 
coup1L = cplcFuFuhhL(gt1,i1,i2)
coup1R = cplcFuFuhhR(gt1,i1,i2)
coup2L = -cplcFuFuVZR(i1,gt2)
coup2R = -cplcFuFuVZL(i1,gt2)
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


! {hh, bar[Fu], bar[Fu]}
If ((Include_in_loophh).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = Mhh(i1) 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuhhL(gt1,i2,i1)
coup1R = cplcFuFuhhR(gt1,i2,i1)
coup2L = cplcFuFuhhL(i3,gt2,i1)
coup2R = cplcFuFuhhR(i3,gt2,i1)
coup3L = cplcFuFuAhL(i2,i3)
coup3R = cplcFuFuAhR(i2,i3)
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


! {Hp, bar[Fd], bar[Fd]}
If ((Include_in_loopHp).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MHp 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFdHpL(gt1,i2)
coup1R = cplcFuFdHpR(gt1,i2)
coup2L = cplcFdFucHpL(i3,gt2)
coup2R = cplcFdFucHpR(i3,gt2)
coup3L = cplcFdFdAhL(i2,i3)
coup3R = cplcFdFdAhR(i2,i3)
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


! {VG, bar[Fu], bar[Fu]}
If ((Include_in_loopVG).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVG 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = -cplcFuFuVGR(gt1,i2)
coup1R = -cplcFuFuVGL(gt1,i2)
coup2L = -cplcFuFuVGR(i3,gt2)
coup2R = -cplcFuFuVGL(i3,gt2)
coup3L = cplcFuFuAhL(i2,i3)
coup3R = cplcFuFuAhR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(4._dp/3._dp)*AmpC 
    End Do
  End Do
End if 


! {VP, bar[Fu], bar[Fu]}
If ((Include_in_loopVP).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = -cplcFuFuVPR(gt1,i2)
coup1R = -cplcFuFuVPL(gt1,i2)
coup2L = -cplcFuFuVPR(i3,gt2)
coup2R = -cplcFuFuVPL(i3,gt2)
coup3L = cplcFuFuAhL(i2,i3)
coup3R = cplcFuFuAhR(i2,i3)
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


! {VWp, bar[Fd], bar[Fd]}
If ((Include_in_loopVWp).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVWp 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = -cplcFuFdVWpR(gt1,i2)
coup1R = -cplcFuFdVWpL(gt1,i2)
coup2L = -cplcFdFucVWpR(i3,gt2)
coup2R = -cplcFdFucVWpL(i3,gt2)
coup3L = cplcFdFdAhL(i2,i3)
coup3R = cplcFdFdAhR(i2,i3)
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


! {VZ, bar[Fu], bar[Fu]}
If ((Include_in_loopVZ).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVZ 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = -cplcFuFuVZR(gt1,i2)
coup1R = -cplcFuFuVZL(gt1,i2)
coup2L = -cplcFuFuVZR(i3,gt2)
coup2R = -cplcFuFuVZL(i3,gt2)
coup3L = cplcFuFuAhL(i2,i3)
coup3R = cplcFuFuAhR(i2,i3)
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
End Subroutine Amplitude_VERTEX_SDdiracDM_FuToFuAh


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FuToFuAh(MAh,MFd,MFu,Mhh,MHp,MVG,            & 
& MVP,MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplAhAhhh,cplcFdFdAhL,      & 
& cplcFdFdAhR,cplcFuFuAhL,cplcFuFuAhR,cplAhhhVZ,cplAhHpcVWp,cplAhcHpVWp,cplcFuFdHpL,     & 
& cplcFuFdHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,             & 
& cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,              & 
& cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFu(3),Mhh(2),MHp,MVG,MVP,MVWp,MVZ,MAh2,MFd2(3),MFu2(3),Mhh2(2),           & 
& MHp2,MVG2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplAhAhhh(2),cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),     & 
& cplAhhhVZ(2),cplAhHpcVWp,cplAhcHpVWp,cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),cplcFuFdVWpL(3,3),& 
& cplcFuFdVWpR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFuFuVGL(3,3),              & 
& cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),  & 
& cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3)

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
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MAh 


! {VG, bar[Fu], bar[Fu]}
If ((Include_in_loopVG).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVG 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = -cplcFuFuVGR(gt1,i2)
coup1R = -cplcFuFuVGL(gt1,i2)
coup2L = -cplcFuFuVGR(i3,gt2)
coup2R = -cplcFuFuVGL(i3,gt2)
coup3L = cplcFuFuAhL(i2,i3)
coup3R = cplcFuFuAhR(i2,i3)
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(4._dp/3._dp)*AmpC 
    End Do
  End Do
End if 


! {VP, bar[Fu], bar[Fu]}
If ((Include_in_loopVP).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = -cplcFuFuVPR(gt1,i2)
coup1R = -cplcFuFuVPL(gt1,i2)
coup2L = -cplcFuFuVPR(i3,gt2)
coup2R = -cplcFuFuVPL(i3,gt2)
coup3L = cplcFuFuAhL(i2,i3)
coup3R = cplcFuFuAhR(i2,i3)
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FuToFuAh


Subroutine Amplitude_Tree_SDdiracDM_FuToFdHp(cplcFuFdHpL,cplcFuFdHpR,MFd,             & 
& MFu,MHp,MFd2,MFu2,MHp2,Amp)

Implicit None

Real(dp), Intent(in) :: MFd(3),MFu(3),MHp,MFd2(3),MFu2(3),MHp2

Complex(dp), Intent(in) :: cplcFuFdHpL(3,3),cplcFuFdHpR(3,3)

Complex(dp) :: Amp(2,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,3
  Do gt2=1,3
! External masses 
Mex1 = MFu(gt1) 
Mex2 = MFd(gt2) 
Mex3 = MHp 
! Tree-Level Vertex 
coupT1L = cplcFuFdHpL(gt1,gt2)
coupT1R = cplcFuFdHpR(gt1,gt2)
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2) = AmpC 
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_FuToFdHp


Subroutine Gamma_Real_SDdiracDM_FuToFdHp(MLambda,em,gs,cplcFuFdHpL,cplcFuFdHpR,       & 
& MFd,MFu,MHp,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFuFdHpL(3,3),cplcFuFdHpR(3,3)

Real(dp), Intent(in) :: MFd(3),MFu(3),MHp

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(3,3), GammarealGluon(3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,3
  Do i2=1,3
CoupL = cplcFuFdHpL(i1,i2)
CoupR = cplcFuFdHpR(i1,i2)
Mex1 = MFu(i1)
Mex2 = MFd(i2)
Mex3 = MHp
If (Mex1.gt.(Mex2+Mex3)) Then 
 Call hardradiationFFS(Mex1,Mex2,Mex3,MLambda,em,4._dp/9._dp,-2._dp/9._dp,2._dp/3._dp,1._dp/9._dp,-1._dp/3._dp,1._dp,CoupL,CoupR,Gammarealphoton(i1,i2),kont)
 Call hardradiationFFS(Mex1,Mex2,Mex3,MLambda,gs,4._dp/3._dp,4._dp/3._dp,0._dp,4._dp/3._dp,0._dp,0._dp,CoupL,CoupR,Gammarealgluon(i1,i2),kont)
Else 
  GammarealGluon(i1,i2) = 0._dp 
  GammarealPhoton(i1,i2) = 0._dp 

End if 
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_FuToFdHp


Subroutine Amplitude_WAVE_SDdiracDM_FuToFdHp(cplcFuFdHpL,cplcFuFdHpR,ctcplcFuFdHpL,   & 
& ctcplcFuFdHpR,MFd,MFd2,MFu,MFu2,MHp,MHp2,ZfDL,ZfDR,ZfHp,ZfUL,ZfUR,Amp)

Implicit None

Real(dp), Intent(in) :: MFd(3),MFd2(3),MFu(3),MFu2(3),MHp,MHp2

Complex(dp), Intent(in) :: cplcFuFdHpL(3,3),cplcFuFdHpR(3,3)

Complex(dp), Intent(in) :: ctcplcFuFdHpL(3,3),ctcplcFuFdHpR(3,3)

Complex(dp), Intent(in) :: ZfDL(3,3),ZfDR(3,3),ZfHp,ZfUL(3,3),ZfUR(3,3)

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
Mex1 = MFu(gt1) 
Mex2 = MFd(gt2) 
Mex3 = MHp 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFuFdHpL(gt1,gt2) 
ZcoupT1R = ctcplcFuFdHpR(gt1,gt2) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfUR(i1,gt1)*cplcFuFdHpL(i1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfUL(i1,gt1))*cplcFuFdHpR(i1,gt2)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfDL(i1,gt2)*cplcFuFdHpL(gt1,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfDR(i1,gt2))*cplcFuFdHpR(gt1,i1)
End Do


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfHp*cplcFuFdHpL(gt1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfHp*cplcFuFdHpR(gt1,gt2)


! Getting the amplitude 
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt1, gt2) = AmpC 
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FuToFdHp


Subroutine Amplitude_VERTEX_SDdiracDM_FuToFdHp(MAh,MFd,MFu,Mhh,MHp,MVG,               & 
& MVP,MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,cplcFdFdAhR,    & 
& cplcFuFuAhL,cplcFuFuAhR,cplAhHpcVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,               & 
& cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,              & 
& cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,              & 
& cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplhhHpcHp,cplhhHpcVWp,    & 
& cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,cplHpcVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFu(3),Mhh(2),MHp,MVG,MVP,MVWp,MVZ,MAh2,MFd2(3),MFu2(3),Mhh2(2),           & 
& MHp2,MVG2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplAhHpcVWp,      & 
& cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),               & 
& cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFuFdVWpL(3,3), & 
& cplcFuFdVWpR(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),& 
& cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),  & 
& cplcFuFuVZR(3,3),cplhhHpcHp(2),cplhhHpcVWp(2),cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,       & 
& cplHpcVWpVZ

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
Mex1 = MFu(gt1) 
Mex2 = MFd(gt2) 
Mex3 = MHp 


! {Ah, bar[Fu], bar[Fd]}
If ((Include_in_loopAh).and.(Include_in_loopFu).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MAh 
ML2 = MFu(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFuAhL(gt1,i2)
coup1R = cplcFuFuAhR(gt1,i2)
coup2L = cplcFdFdAhL(i3,gt2)
coup2R = cplcFdFdAhR(i3,gt2)
coup3L = cplcFuFdHpL(i2,i3)
coup3R = cplcFuFdHpR(i2,i3)
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


! {Fd, conj[VWp], Ah}
If ((Include_in_loopFd).and.(Include_in_loopVWp).and.(Include_in_loopAh)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MVWp 
ML3 = MAh 
coup1L = -cplcFuFdVWpR(gt1,i1)
coup1R = -cplcFuFdVWpL(gt1,i1)
coup2L = cplcFdFdAhL(i1,gt2)
coup2R = cplcFdFdAhR(i1,gt2)
coup3 = cplAhHpcVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fd, conj[Hp], hh}
If ((Include_in_loopFd).and.(Include_in_loopHp).and.(Include_in_loophh)) Then 
Do i1=1,3
    Do i3=1,2
ML1 = MFd(i1) 
ML2 = MHp 
ML3 = Mhh(i3) 
coup1L = cplcFuFdHpL(gt1,i1)
coup1R = cplcFuFdHpR(gt1,i1)
coup2L = cplcFdFdhhL(i1,gt2,i3)
coup2R = cplcFdFdhhR(i1,gt2,i3)
coup3 = cplhhHpcHp(i3)
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


! {Fd, conj[VWp], hh}
If ((Include_in_loopFd).and.(Include_in_loopVWp).and.(Include_in_loophh)) Then 
Do i1=1,3
    Do i3=1,2
ML1 = MFd(i1) 
ML2 = MVWp 
ML3 = Mhh(i3) 
coup1L = -cplcFuFdVWpR(gt1,i1)
coup1R = -cplcFuFdVWpL(gt1,i1)
coup2L = cplcFdFdhhL(i1,gt2,i3)
coup2R = cplcFdFdhhR(i1,gt2,i3)
coup3 = cplhhHpcVWp(i3)
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


! {Fd, conj[Hp], VP}
If ((Include_in_loopFd).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MHp 
ML3 = MVP 
coup1L = cplcFuFdHpL(gt1,i1)
coup1R = cplcFuFdHpR(gt1,i1)
coup2L = -cplcFdFdVPR(i1,gt2)
coup2R = -cplcFdFdVPL(i1,gt2)
coup3 = -cplHpcHpVP
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fd, conj[VWp], VP}
If ((Include_in_loopFd).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MVWp 
ML3 = MVP 
coup1L = -cplcFuFdVWpR(gt1,i1)
coup1R = -cplcFuFdVWpL(gt1,i1)
coup2L = -cplcFdFdVPR(i1,gt2)
coup2R = -cplcFdFdVPL(i1,gt2)
coup3 = cplHpcVWpVP
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fd, conj[Hp], VZ}
If ((Include_in_loopFd).and.(Include_in_loopHp).and.(Include_in_loopVZ)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MHp 
ML3 = MVZ 
coup1L = cplcFuFdHpL(gt1,i1)
coup1R = cplcFuFdHpR(gt1,i1)
coup2L = -cplcFdFdVZR(i1,gt2)
coup2R = -cplcFdFdVZL(i1,gt2)
coup3 = -cplHpcHpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fd, conj[VWp], VZ}
If ((Include_in_loopFd).and.(Include_in_loopVWp).and.(Include_in_loopVZ)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MVWp 
ML3 = MVZ 
coup1L = -cplcFuFdVWpR(gt1,i1)
coup1R = -cplcFuFdVWpL(gt1,i1)
coup2L = -cplcFdFdVZR(i1,gt2)
coup2R = -cplcFdFdVZL(i1,gt2)
coup3 = cplHpcVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fu, hh, Hp}
If ((Include_in_loopFu).and.(Include_in_loophh).and.(Include_in_loopHp)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFu(i1) 
ML2 = Mhh(i2) 
ML3 = MHp 
coup1L = cplcFuFuhhL(gt1,i1,i2)
coup1R = cplcFuFuhhR(gt1,i1,i2)
coup2L = cplcFuFdHpL(i1,gt2)
coup2R = cplcFuFdHpR(i1,gt2)
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


! {Fu, VP, Hp}
If ((Include_in_loopFu).and.(Include_in_loopVP).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MVP 
ML3 = MHp 
coup1L = -cplcFuFuVPR(gt1,i1)
coup1R = -cplcFuFuVPL(gt1,i1)
coup2L = cplcFuFdHpL(i1,gt2)
coup2R = cplcFuFdHpR(i1,gt2)
coup3 = -cplHpcHpVP
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fu, VZ, Hp}
If ((Include_in_loopFu).and.(Include_in_loopVZ).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MVZ 
ML3 = MHp 
coup1L = -cplcFuFuVZR(gt1,i1)
coup1R = -cplcFuFuVZL(gt1,i1)
coup2L = cplcFuFdHpL(i1,gt2)
coup2R = cplcFuFdHpR(i1,gt2)
coup3 = -cplHpcHpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fu, Ah, VWp}
If ((Include_in_loopFu).and.(Include_in_loopAh).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MAh 
ML3 = MVWp 
coup1L = cplcFuFuAhL(gt1,i1)
coup1R = cplcFuFuAhR(gt1,i1)
coup2L = -cplcFuFdVWpR(i1,gt2)
coup2R = -cplcFuFdVWpL(i1,gt2)
coup3 = cplAhHpcVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fu, hh, VWp}
If ((Include_in_loopFu).and.(Include_in_loophh).and.(Include_in_loopVWp)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFu(i1) 
ML2 = Mhh(i2) 
ML3 = MVWp 
coup1L = cplcFuFuhhL(gt1,i1,i2)
coup1R = cplcFuFuhhR(gt1,i1,i2)
coup2L = -cplcFuFdVWpR(i1,gt2)
coup2R = -cplcFuFdVWpL(i1,gt2)
coup3 = cplhhHpcVWp(i2)
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


! {Fu, VP, VWp}
If ((Include_in_loopFu).and.(Include_in_loopVP).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MVP 
ML3 = MVWp 
coup1L = -cplcFuFuVPR(gt1,i1)
coup1R = -cplcFuFuVPL(gt1,i1)
coup2L = -cplcFuFdVWpR(i1,gt2)
coup2R = -cplcFuFdVWpL(i1,gt2)
coup3 = cplHpcVWpVP
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fu, VZ, VWp}
If ((Include_in_loopFu).and.(Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MVZ 
ML3 = MVWp 
coup1L = -cplcFuFuVZR(gt1,i1)
coup1R = -cplcFuFuVZL(gt1,i1)
coup2L = -cplcFuFdVWpR(i1,gt2)
coup2R = -cplcFuFdVWpL(i1,gt2)
coup3 = cplHpcVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {hh, bar[Fu], bar[Fd]}
If ((Include_in_loophh).and.(Include_in_loopFu).and.(Include_in_loopFd)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = Mhh(i1) 
ML2 = MFu(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFuhhL(gt1,i2,i1)
coup1R = cplcFuFuhhR(gt1,i2,i1)
coup2L = cplcFdFdhhL(i3,gt2,i1)
coup2R = cplcFdFdhhR(i3,gt2,i1)
coup3L = cplcFuFdHpL(i2,i3)
coup3R = cplcFuFdHpR(i2,i3)
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


! {VG, bar[Fu], bar[Fd]}
If ((Include_in_loopVG).and.(Include_in_loopFu).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVG 
ML2 = MFu(i2) 
ML3 = MFd(i3) 
coup1L = -cplcFuFuVGR(gt1,i2)
coup1R = -cplcFuFuVGL(gt1,i2)
coup2L = -cplcFdFdVGR(i3,gt2)
coup2R = -cplcFdFdVGL(i3,gt2)
coup3L = cplcFuFdHpL(i2,i3)
coup3R = cplcFuFdHpR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(4._dp/3._dp)*AmpC 
    End Do
  End Do
End if 


! {VP, bar[Fu], bar[Fd]}
If ((Include_in_loopVP).and.(Include_in_loopFu).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFu(i2) 
ML3 = MFd(i3) 
coup1L = -cplcFuFuVPR(gt1,i2)
coup1R = -cplcFuFuVPL(gt1,i2)
coup2L = -cplcFdFdVPR(i3,gt2)
coup2R = -cplcFdFdVPL(i3,gt2)
coup3L = cplcFuFdHpL(i2,i3)
coup3R = cplcFuFdHpR(i2,i3)
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


! {VZ, bar[Fu], bar[Fd]}
If ((Include_in_loopVZ).and.(Include_in_loopFu).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVZ 
ML2 = MFu(i2) 
ML3 = MFd(i3) 
coup1L = -cplcFuFuVZR(gt1,i2)
coup1R = -cplcFuFuVZL(gt1,i2)
coup2L = -cplcFdFdVZR(i3,gt2)
coup2R = -cplcFdFdVZL(i3,gt2)
coup3L = cplcFuFdHpL(i2,i3)
coup3R = cplcFuFdHpR(i2,i3)
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
End Subroutine Amplitude_VERTEX_SDdiracDM_FuToFdHp


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FuToFdHp(MAh,MFd,MFu,Mhh,MHp,MVG,            & 
& MVP,MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,cplcFdFdAhR,    & 
& cplcFuFuAhL,cplcFuFuAhR,cplAhHpcVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,               & 
& cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,              & 
& cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,              & 
& cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplhhHpcHp,cplhhHpcVWp,    & 
& cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,cplHpcVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFu(3),Mhh(2),MHp,MVG,MVP,MVWp,MVZ,MAh2,MFd2(3),MFu2(3),Mhh2(2),           & 
& MHp2,MVG2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplAhHpcVWp,      & 
& cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),               & 
& cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFuFdVWpL(3,3), & 
& cplcFuFdVWpR(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),& 
& cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),  & 
& cplcFuFuVZR(3,3),cplhhHpcHp(2),cplhhHpcVWp(2),cplHpcHpVP,cplHpcVWpVP,cplHpcHpVZ,       & 
& cplHpcVWpVZ

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
Mex1 = MFu(gt1) 
Mex2 = MFd(gt2) 
Mex3 = MHp 


! {Fd, conj[Hp], VP}
If ((Include_in_loopFd).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MHp 
ML3 = MVP 
coup1L = cplcFuFdHpL(gt1,i1)
coup1R = cplcFuFdHpR(gt1,i1)
coup2L = -cplcFdFdVPR(i1,gt2)
coup2R = -cplcFdFdVPL(i1,gt2)
coup3 = -cplHpcHpVP
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fd, conj[VWp], VP}
If ((Include_in_loopFd).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MVWp 
ML3 = MVP 
coup1L = -cplcFuFdVWpR(gt1,i1)
coup1R = -cplcFuFdVWpL(gt1,i1)
coup2L = -cplcFdFdVPR(i1,gt2)
coup2R = -cplcFdFdVPL(i1,gt2)
coup3 = cplHpcVWpVP
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fu, VP, Hp}
If ((Include_in_loopFu).and.(Include_in_loopVP).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MVP 
ML3 = MHp 
coup1L = -cplcFuFuVPR(gt1,i1)
coup1R = -cplcFuFuVPL(gt1,i1)
coup2L = cplcFuFdHpL(i1,gt2)
coup2R = cplcFuFdHpR(i1,gt2)
coup3 = -cplHpcHpVP
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fu, VP, VWp}
If ((Include_in_loopFu).and.(Include_in_loopVP).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MVP 
ML3 = MVWp 
coup1L = -cplcFuFuVPR(gt1,i1)
coup1R = -cplcFuFuVPL(gt1,i1)
coup2L = -cplcFuFdVWpR(i1,gt2)
coup2R = -cplcFuFdVWpL(i1,gt2)
coup3 = cplHpcVWpVP
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {VG, bar[Fu], bar[Fd]}
If ((Include_in_loopVG).and.(Include_in_loopFu).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVG 
ML2 = MFu(i2) 
ML3 = MFd(i3) 
coup1L = -cplcFuFuVGR(gt1,i2)
coup1R = -cplcFuFuVGL(gt1,i2)
coup2L = -cplcFdFdVGR(i3,gt2)
coup2R = -cplcFdFdVGL(i3,gt2)
coup3L = cplcFuFdHpL(i2,i3)
coup3R = cplcFuFdHpR(i2,i3)
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(4._dp/3._dp)*AmpC 
    End Do
  End Do
End if 


! {VP, bar[Fu], bar[Fd]}
If ((Include_in_loopVP).and.(Include_in_loopFu).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFu(i2) 
ML3 = MFd(i3) 
coup1L = -cplcFuFuVPR(gt1,i2)
coup1R = -cplcFuFuVPL(gt1,i2)
coup2L = -cplcFdFdVPR(i3,gt2)
coup2R = -cplcFdFdVPL(i3,gt2)
coup3L = cplcFuFdHpL(i2,i3)
coup3R = cplcFuFdHpR(i2,i3)
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FuToFdHp


Subroutine Amplitude_Tree_SDdiracDM_FuToFdVWp(cplcFuFdVWpL,cplcFuFdVWpR,              & 
& MFd,MFu,MVWp,MFd2,MFu2,MVWp2,Amp)

Implicit None

Real(dp), Intent(in) :: MFd(3),MFu(3),MVWp,MFd2(3),MFu2(3),MVWp2

Complex(dp), Intent(in) :: cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3)

Complex(dp) :: Amp(4,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,3
  Do gt2=1,3
! External masses 
Mex1 = MFu(gt1) 
Mex2 = MFd(gt2) 
Mex3 = MVWp 
! Tree-Level Vertex 
coupT1L = cplcFuFdVWpL(gt1,gt2)
coupT1R = cplcFuFdVWpR(gt1,gt2)
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,coupT1R,coupT1L,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2) = -AmpC 
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_FuToFdVWp


Subroutine Gamma_Real_SDdiracDM_FuToFdVWp(MLambda,em,gs,cplcFuFdVWpL,cplcFuFdVWpR,    & 
& MFd,MFu,MVWp,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3)

Real(dp), Intent(in) :: MFd(3),MFu(3),MVWp

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(3,3), GammarealGluon(3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,3
  Do i2=1,3
CoupL = cplcFuFdVWpL(i1,i2)
CoupR = cplcFuFdVWpR(i1,i2)
Mex1 = MFu(i1)
Mex2 = MFd(i2)
Mex3 = MVWp
If (Mex1.gt.(Mex2+Mex3)) Then 
  Call hardphotonFFW(Mex1,Mex2,Mex3,MLambda,2._dp/3._dp,-1._dp/3._dp,CoupL,CoupR,(0,1)*em,GammaRealPhoton(i1,i2),kont)
  Call hardgluonFFZW(Mex1,Mex2,Mex3,MLambda,4._dp/3._dp,gs,CoupL,CoupR,Gammarealgluon(i1,i2),kont)
Else 
  GammarealGluon(i1,i2) = 0._dp 
  GammarealPhoton(i1,i2) = 0._dp 

End if 
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_FuToFdVWp


Subroutine Amplitude_WAVE_SDdiracDM_FuToFdVWp(cplcFuFdVWpL,cplcFuFdVWpR,              & 
& ctcplcFuFdVWpL,ctcplcFuFdVWpR,MFd,MFd2,MFu,MFu2,MVWp,MVWp2,ZfDL,ZfDR,ZfUL,             & 
& ZfUR,ZfVWp,Amp)

Implicit None

Real(dp), Intent(in) :: MFd(3),MFd2(3),MFu(3),MFu2(3),MVWp,MVWp2

Complex(dp), Intent(in) :: cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3)

Complex(dp), Intent(in) :: ctcplcFuFdVWpL(3,3),ctcplcFuFdVWpR(3,3)

Complex(dp), Intent(in) :: ZfDL(3,3),ZfDR(3,3),ZfUL(3,3),ZfUR(3,3),ZfVWp

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
Mex1 = MFu(gt1) 
Mex2 = MFd(gt2) 
Mex3 = MVWp 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFuFdVWpL(gt1,gt2) 
ZcoupT1R = ctcplcFuFdVWpR(gt1,gt2)
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*Conjg(ZfUL(i1,gt1))*cplcFuFdVWpL(i1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfUR(i1,gt1)*cplcFuFdVWpR(i1,gt2)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfDL(i1,gt2)*cplcFuFdVWpL(gt1,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfDR(i1,gt2))*cplcFuFdVWpR(gt1,i1)
End Do


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVWp*cplcFuFdVWpL(gt1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfVWp*cplcFuFdVWpR(gt1,gt2)


! Getting the amplitude 
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,ZcoupT1R,ZcoupT1L,AmpC) 
Amp(:,gt1, gt2) = -AmpC 
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FuToFdVWp


Subroutine Amplitude_VERTEX_SDdiracDM_FuToFdVWp(MAh,MFd,MFu,Mhh,MHp,MVG,              & 
& MVP,MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,cplcFdFdAhR,    & 
& cplcFuFuAhL,cplcFuFuAhR,cplAhcHpVWp,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdHpL,               & 
& cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,              & 
& cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,              & 
& cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplhhcHpVWp,               & 
& cplhhcVWpVWp,cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFu(3),Mhh(2),MHp,MVG,MVP,MVWp,MVZ,MAh2,MFd2(3),MFu2(3),Mhh2(2),           & 
& MHp2,MVG2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplAhcHpVWp,      & 
& cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),               & 
& cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFuFdVWpL(3,3), & 
& cplcFuFdVWpR(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),& 
& cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),  & 
& cplcFuFuVZR(3,3),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplcHpVPVWp,cplcVWpVPVWp,              & 
& cplcHpVWpVZ,cplcVWpVWpVZ

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
Mex1 = MFu(gt1) 
Mex2 = MFd(gt2) 
Mex3 = MVWp 


! {Ah, bar[Fu], bar[Fd]}
If ((Include_in_loopAh).and.(Include_in_loopFu).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MAh 
ML2 = MFu(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFuAhL(gt1,i2)
coup1R = cplcFuFuAhR(gt1,i2)
coup2L = cplcFdFdAhL(i3,gt2)
coup2R = cplcFdFdAhR(i3,gt2)
coup3L = cplcFuFdVWpL(i2,i3)
coup3R = cplcFuFdVWpR(i2,i3)
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


! {Fd, conj[Hp], Ah}
If ((Include_in_loopFd).and.(Include_in_loopHp).and.(Include_in_loopAh)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MHp 
ML3 = MAh 
coup1L = cplcFuFdHpL(gt1,i1)
coup1R = cplcFuFdHpR(gt1,i1)
coup2L = cplcFdFdAhL(i1,gt2)
coup2R = cplcFdFdAhR(i1,gt2)
coup3 = cplAhcHpVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fd, conj[Hp], hh}
If ((Include_in_loopFd).and.(Include_in_loopHp).and.(Include_in_loophh)) Then 
Do i1=1,3
    Do i3=1,2
ML1 = MFd(i1) 
ML2 = MHp 
ML3 = Mhh(i3) 
coup1L = cplcFuFdHpL(gt1,i1)
coup1R = cplcFuFdHpR(gt1,i1)
coup2L = cplcFdFdhhL(i1,gt2,i3)
coup2R = cplcFdFdhhR(i1,gt2,i3)
coup3 = cplhhcHpVWp(i3)
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


! {Fd, conj[VWp], hh}
If ((Include_in_loopFd).and.(Include_in_loopVWp).and.(Include_in_loophh)) Then 
Do i1=1,3
    Do i3=1,2
ML1 = MFd(i1) 
ML2 = MVWp 
ML3 = Mhh(i3) 
coup1L = cplcFuFdVWpL(gt1,i1)
coup1R = cplcFuFdVWpR(gt1,i1)
coup2L = cplcFdFdhhL(i1,gt2,i3)
coup2R = cplcFdFdhhR(i1,gt2,i3)
coup3 = cplhhcVWpVWp(i3)
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


! {Fd, conj[Hp], VP}
If ((Include_in_loopFd).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MHp 
ML3 = MVP 
coup1L = cplcFuFdHpL(gt1,i1)
coup1R = cplcFuFdHpR(gt1,i1)
coup2L = cplcFdFdVPL(i1,gt2)
coup2R = cplcFdFdVPR(i1,gt2)
coup3 = cplcHpVPVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fd, conj[VWp], VP}
If ((Include_in_loopFd).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MVWp 
ML3 = MVP 
coup1L = cplcFuFdVWpL(gt1,i1)
coup1R = cplcFuFdVWpR(gt1,i1)
coup2L = cplcFdFdVPL(i1,gt2)
coup2R = cplcFdFdVPR(i1,gt2)
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


! {Fd, conj[Hp], VZ}
If ((Include_in_loopFd).and.(Include_in_loopHp).and.(Include_in_loopVZ)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MHp 
ML3 = MVZ 
coup1L = cplcFuFdHpL(gt1,i1)
coup1R = cplcFuFdHpR(gt1,i1)
coup2L = cplcFdFdVZL(i1,gt2)
coup2R = cplcFdFdVZR(i1,gt2)
coup3 = cplcHpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fd, conj[VWp], VZ}
If ((Include_in_loopFd).and.(Include_in_loopVWp).and.(Include_in_loopVZ)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MVWp 
ML3 = MVZ 
coup1L = cplcFuFdVWpL(gt1,i1)
coup1R = cplcFuFdVWpR(gt1,i1)
coup2L = cplcFdFdVZL(i1,gt2)
coup2R = cplcFdFdVZR(i1,gt2)
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


! {Fu, Ah, Hp}
If ((Include_in_loopFu).and.(Include_in_loopAh).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MAh 
ML3 = MHp 
coup1L = cplcFuFuAhL(gt1,i1)
coup1R = cplcFuFuAhR(gt1,i1)
coup2L = cplcFuFdHpL(i1,gt2)
coup2R = cplcFuFdHpR(i1,gt2)
coup3 = -cplAhcHpVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fu, hh, Hp}
If ((Include_in_loopFu).and.(Include_in_loophh).and.(Include_in_loopHp)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFu(i1) 
ML2 = Mhh(i2) 
ML3 = MHp 
coup1L = cplcFuFuhhL(gt1,i1,i2)
coup1R = cplcFuFuhhR(gt1,i1,i2)
coup2L = cplcFuFdHpL(i1,gt2)
coup2R = cplcFuFdHpR(i1,gt2)
coup3 = -cplhhcHpVWp(i2)
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


! {Fu, VP, Hp}
If ((Include_in_loopFu).and.(Include_in_loopVP).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MVP 
ML3 = MHp 
coup1L = cplcFuFuVPL(gt1,i1)
coup1R = cplcFuFuVPR(gt1,i1)
coup2L = cplcFuFdHpL(i1,gt2)
coup2R = cplcFuFdHpR(i1,gt2)
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


! {Fu, VZ, Hp}
If ((Include_in_loopFu).and.(Include_in_loopVZ).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MVZ 
ML3 = MHp 
coup1L = cplcFuFuVZL(gt1,i1)
coup1R = cplcFuFuVZR(gt1,i1)
coup2L = cplcFuFdHpL(i1,gt2)
coup2R = cplcFuFdHpR(i1,gt2)
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


! {Fu, hh, VWp}
If ((Include_in_loopFu).and.(Include_in_loophh).and.(Include_in_loopVWp)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFu(i1) 
ML2 = Mhh(i2) 
ML3 = MVWp 
coup1L = cplcFuFuhhL(gt1,i1,i2)
coup1R = cplcFuFuhhR(gt1,i1,i2)
coup2L = cplcFuFdVWpL(i1,gt2)
coup2R = cplcFuFdVWpR(i1,gt2)
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


! {Fu, VP, VWp}
If ((Include_in_loopFu).and.(Include_in_loopVP).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MVP 
ML3 = MVWp 
coup1L = cplcFuFuVPL(gt1,i1)
coup1R = cplcFuFuVPR(gt1,i1)
coup2L = cplcFuFdVWpL(i1,gt2)
coup2R = cplcFuFdVWpR(i1,gt2)
coup3 = -cplcVWpVPVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fu, VZ, VWp}
If ((Include_in_loopFu).and.(Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MVZ 
ML3 = MVWp 
coup1L = cplcFuFuVZL(gt1,i1)
coup1R = cplcFuFuVZR(gt1,i1)
coup2L = cplcFuFdVWpL(i1,gt2)
coup2R = cplcFuFdVWpR(i1,gt2)
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


! {hh, bar[Fu], bar[Fd]}
If ((Include_in_loophh).and.(Include_in_loopFu).and.(Include_in_loopFd)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = Mhh(i1) 
ML2 = MFu(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFuhhL(gt1,i2,i1)
coup1R = cplcFuFuhhR(gt1,i2,i1)
coup2L = cplcFdFdhhL(i3,gt2,i1)
coup2R = cplcFdFdhhR(i3,gt2,i1)
coup3L = cplcFuFdVWpL(i2,i3)
coup3R = cplcFuFdVWpR(i2,i3)
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


! {VG, bar[Fu], bar[Fd]}
If ((Include_in_loopVG).and.(Include_in_loopFu).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVG 
ML2 = MFu(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFuVGL(gt1,i2)
coup1R = cplcFuFuVGR(gt1,i2)
coup2L = cplcFdFdVGL(i3,gt2)
coup2R = cplcFdFdVGR(i3,gt2)
coup3L = cplcFuFdVWpL(i2,i3)
coup3R = cplcFuFdVWpR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(4._dp/3._dp)*AmpC 
    End Do
  End Do
End if 


! {VP, bar[Fu], bar[Fd]}
If ((Include_in_loopVP).and.(Include_in_loopFu).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFu(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFuVPL(gt1,i2)
coup1R = cplcFuFuVPR(gt1,i2)
coup2L = cplcFdFdVPL(i3,gt2)
coup2R = cplcFdFdVPR(i3,gt2)
coup3L = cplcFuFdVWpL(i2,i3)
coup3R = cplcFuFdVWpR(i2,i3)
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


! {VZ, bar[Fu], bar[Fd]}
If ((Include_in_loopVZ).and.(Include_in_loopFu).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVZ 
ML2 = MFu(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFuVZL(gt1,i2)
coup1R = cplcFuFuVZR(gt1,i2)
coup2L = cplcFdFdVZL(i3,gt2)
coup2R = cplcFdFdVZR(i3,gt2)
coup3L = cplcFuFdVWpL(i2,i3)
coup3R = cplcFuFdVWpR(i2,i3)
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
End Subroutine Amplitude_VERTEX_SDdiracDM_FuToFdVWp


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FuToFdVWp(MAh,MFd,MFu,Mhh,MHp,               & 
& MVG,MVP,MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFdFdAhL,            & 
& cplcFdFdAhR,cplcFuFuAhL,cplcFuFuAhR,cplAhcHpVWp,cplcFdFdhhL,cplcFdFdhhR,               & 
& cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,               & 
& cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,             & 
& cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,               & 
& cplhhcHpVWp,cplhhcVWpVWp,cplcHpVPVWp,cplcVWpVPVWp,cplcHpVWpVZ,cplcVWpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFu(3),Mhh(2),MHp,MVG,MVP,MVWp,MVZ,MAh2,MFd2(3),MFu2(3),Mhh2(2),           & 
& MHp2,MVG2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplAhcHpVWp,      & 
& cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),               & 
& cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFuFdVWpL(3,3), & 
& cplcFuFdVWpR(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),& 
& cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),  & 
& cplcFuFuVZR(3,3),cplhhcHpVWp(2),cplhhcVWpVWp(2),cplcHpVPVWp,cplcVWpVPVWp,              & 
& cplcHpVWpVZ,cplcVWpVWpVZ

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
Mex1 = MFu(gt1) 
Mex2 = MFd(gt2) 
Mex3 = MVWp 


! {Fd, conj[Hp], VP}
If ((Include_in_loopFd).and.(Include_in_loopHp).and.(Include_in_loopVP)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MHp 
ML3 = MVP 
coup1L = cplcFuFdHpL(gt1,i1)
coup1R = cplcFuFdHpR(gt1,i1)
coup2L = cplcFdFdVPL(i1,gt2)
coup2R = cplcFdFdVPR(i1,gt2)
coup3 = cplcHpVPVWp
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fd, conj[VWp], VP}
If ((Include_in_loopFd).and.(Include_in_loopVWp).and.(Include_in_loopVP)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MVWp 
ML3 = MVP 
coup1L = cplcFuFdVWpL(gt1,i1)
coup1R = cplcFuFdVWpR(gt1,i1)
coup2L = cplcFdFdVPL(i1,gt2)
coup2R = cplcFdFdVPR(i1,gt2)
coup3 = cplcVWpVPVWp
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fu, VP, Hp}
If ((Include_in_loopFu).and.(Include_in_loopVP).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MVP 
ML3 = MHp 
coup1L = cplcFuFuVPL(gt1,i1)
coup1R = cplcFuFuVPR(gt1,i1)
coup2L = cplcFuFdHpL(i1,gt2)
coup2R = cplcFuFdHpR(i1,gt2)
coup3 = cplcHpVPVWp
Call Amp_VERTEX_FtoFV_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fu, VP, VWp}
If ((Include_in_loopFu).and.(Include_in_loopVP).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MVP 
ML3 = MVWp 
coup1L = cplcFuFuVPL(gt1,i1)
coup1R = cplcFuFuVPR(gt1,i1)
coup2L = cplcFuFdVWpL(i1,gt2)
coup2R = cplcFuFdVWpR(i1,gt2)
coup3 = -cplcVWpVPVWp
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {VG, bar[Fu], bar[Fd]}
If ((Include_in_loopVG).and.(Include_in_loopFu).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVG 
ML2 = MFu(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFuVGL(gt1,i2)
coup1R = cplcFuFuVGR(gt1,i2)
coup2L = cplcFdFdVGL(i3,gt2)
coup2R = cplcFdFdVGR(i3,gt2)
coup3L = cplcFuFdVWpL(i2,i3)
coup3R = cplcFuFdVWpR(i2,i3)
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(4._dp/3._dp)*AmpC 
    End Do
  End Do
End if 


! {VP, bar[Fu], bar[Fd]}
If ((Include_in_loopVP).and.(Include_in_loopFu).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFu(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFuVPL(gt1,i2)
coup1R = cplcFuFuVPR(gt1,i2)
coup2L = cplcFdFdVPL(i3,gt2)
coup2R = cplcFdFdVPR(i3,gt2)
coup3L = cplcFuFdVWpL(i2,i3)
coup3R = cplcFuFdVWpR(i2,i3)
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FuToFdVWp


Subroutine Amplitude_Tree_SDdiracDM_FuToFuhh(cplcFuFuhhL,cplcFuFuhhR,MFu,             & 
& Mhh,MFu2,Mhh2,Amp)

Implicit None

Real(dp), Intent(in) :: MFu(3),Mhh(2),MFu2(3),Mhh2(2)

Complex(dp), Intent(in) :: cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2)

Complex(dp) :: Amp(2,3,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,2
! External masses 
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = Mhh(gt3) 
! Tree-Level Vertex 
coupT1L = cplcFuFuhhL(gt1,gt2,gt3)
coupT1R = cplcFuFuhhR(gt1,gt2,gt3)
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_FuToFuhh


Subroutine Gamma_Real_SDdiracDM_FuToFuhh(MLambda,em,gs,cplcFuFuhhL,cplcFuFuhhR,       & 
& MFu,Mhh,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2)

Real(dp), Intent(in) :: MFu(3),Mhh(2)

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(3,3,2), GammarealGluon(3,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,3
  Do i2=1,3
    Do i3=1,2
CoupL = cplcFuFuhhL(i1,i2,i3)
CoupR = cplcFuFuhhR(i1,i2,i3)
Mex1 = MFu(i1)
Mex2 = MFu(i2)
Mex3 = Mhh(i3)
If (Mex1.gt.(Mex2+Mex3)) Then 
 Call hardradiationFFS(Mex1,Mex2,Mex3,MLambda,em,4._dp/9._dp,4._dp/9._dp,0._dp,4._dp/9._dp,0._dp,0._dp,CoupL,CoupR,Gammarealphoton(i1,i2,i3),kont)
 Call hardradiationFFS(Mex1,Mex2,Mex3,MLambda,gs,4._dp/3._dp,4._dp/3._dp,0._dp,4._dp/3._dp,0._dp,0._dp,CoupL,CoupR,Gammarealgluon(i1,i2,i3),kont)
Else 
  GammarealGluon(i1,i2,i3) = 0._dp 
  GammarealPhoton(i1,i2,i3) = 0._dp 

End if 
    End Do
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_FuToFuhh


Subroutine Amplitude_WAVE_SDdiracDM_FuToFuhh(cplcFuFuhhL,cplcFuFuhhR,ctcplcFuFuhhL,   & 
& ctcplcFuFuhhR,MFu,MFu2,Mhh,Mhh2,Zfhh,ZfUL,ZfUR,Amp)

Implicit None

Real(dp), Intent(in) :: MFu(3),MFu2(3),Mhh(2),Mhh2(2)

Complex(dp), Intent(in) :: cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2)

Complex(dp), Intent(in) :: ctcplcFuFuhhL(3,3,2),ctcplcFuFuhhR(3,3,2)

Complex(dp), Intent(in) :: Zfhh(2,2),ZfUL(3,3),ZfUR(3,3)

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
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = Mhh(gt3) 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFuFuhhL(gt1,gt2,gt3) 
ZcoupT1R = ctcplcFuFuhhR(gt1,gt2,gt3) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfUR(i1,gt1)*cplcFuFuhhL(i1,gt2,gt3)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfUL(i1,gt1))*cplcFuFuhhR(i1,gt2,gt3)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfUL(i1,gt2)*cplcFuFuhhL(gt1,i1,gt3)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfUR(i1,gt2))*cplcFuFuhhR(gt1,i1,gt3)
End Do


! External Field 3 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*Zfhh(i1,gt3)*cplcFuFuhhL(gt1,gt2,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Zfhh(i1,gt3)*cplcFuFuhhR(gt1,gt2,i1)
End Do


! Getting the amplitude 
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FuToFuhh


Subroutine Amplitude_VERTEX_SDdiracDM_FuToFuhh(MAh,MFd,MFu,Mhh,MHp,MVG,               & 
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
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = Mhh(gt3) 


! {Ah, bar[Fu], bar[Fu]}
If ((Include_in_loopAh).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MAh 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuAhL(gt1,i2)
coup1R = cplcFuFuAhR(gt1,i2)
coup2L = cplcFuFuAhL(i3,gt2)
coup2R = cplcFuFuAhR(i3,gt2)
coup3L = cplcFuFuhhL(i2,i3,gt3)
coup3R = cplcFuFuhhR(i2,i3,gt3)
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


! {Fd, conj[Hp], conj[Hp]}
If ((Include_in_loopFd).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MHp 
ML3 = MHp 
coup1L = cplcFuFdHpL(gt1,i1)
coup1R = cplcFuFdHpR(gt1,i1)
coup2L = cplcFdFucHpL(i1,gt2)
coup2R = cplcFdFucHpR(i1,gt2)
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


! {Fd, conj[VWp], conj[Hp]}
If ((Include_in_loopFd).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MVWp 
ML3 = MHp 
coup1L = -cplcFuFdVWpR(gt1,i1)
coup1R = -cplcFuFdVWpL(gt1,i1)
coup2L = cplcFdFucHpL(i1,gt2)
coup2R = cplcFdFucHpR(i1,gt2)
coup3 = -cplhhHpcVWp(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fd, conj[Hp], conj[VWp]}
If ((Include_in_loopFd).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MHp 
ML3 = MVWp 
coup1L = cplcFuFdHpL(gt1,i1)
coup1R = cplcFuFdHpR(gt1,i1)
coup2L = -cplcFdFucVWpR(i1,gt2)
coup2R = -cplcFdFucVWpL(i1,gt2)
coup3 = -cplhhcHpVWp(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fd, conj[VWp], conj[VWp]}
If ((Include_in_loopFd).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MVWp 
ML3 = MVWp 
coup1L = -cplcFuFdVWpR(gt1,i1)
coup1R = -cplcFuFdVWpL(gt1,i1)
coup2L = -cplcFdFucVWpR(i1,gt2)
coup2R = -cplcFdFucVWpL(i1,gt2)
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


! {Fu, Ah, Ah}
If ((Include_in_loopFu).and.(Include_in_loopAh).and.(Include_in_loopAh)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MAh 
ML3 = MAh 
coup1L = cplcFuFuAhL(gt1,i1)
coup1R = cplcFuFuAhR(gt1,i1)
coup2L = cplcFuFuAhL(i1,gt2)
coup2R = cplcFuFuAhR(i1,gt2)
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


! {Fu, VZ, Ah}
If ((Include_in_loopFu).and.(Include_in_loopVZ).and.(Include_in_loopAh)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MVZ 
ML3 = MAh 
coup1L = -cplcFuFuVZR(gt1,i1)
coup1R = -cplcFuFuVZL(gt1,i1)
coup2L = cplcFuFuAhL(i1,gt2)
coup2R = cplcFuFuAhR(i1,gt2)
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


! {Fu, hh, hh}
If ((Include_in_loopFu).and.(Include_in_loophh).and.(Include_in_loophh)) Then 
Do i1=1,3
  Do i2=1,2
    Do i3=1,2
ML1 = MFu(i1) 
ML2 = Mhh(i2) 
ML3 = Mhh(i3) 
coup1L = cplcFuFuhhL(gt1,i1,i2)
coup1R = cplcFuFuhhR(gt1,i1,i2)
coup2L = cplcFuFuhhL(i1,gt2,i3)
coup2R = cplcFuFuhhR(i1,gt2,i3)
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


! {Fu, Ah, VZ}
If ((Include_in_loopFu).and.(Include_in_loopAh).and.(Include_in_loopVZ)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MAh 
ML3 = MVZ 
coup1L = cplcFuFuAhL(gt1,i1)
coup1R = cplcFuFuAhR(gt1,i1)
coup2L = -cplcFuFuVZR(i1,gt2)
coup2R = -cplcFuFuVZL(i1,gt2)
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


! {Fu, VZ, VZ}
If ((Include_in_loopFu).and.(Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MVZ 
ML3 = MVZ 
coup1L = -cplcFuFuVZR(gt1,i1)
coup1R = -cplcFuFuVZL(gt1,i1)
coup2L = -cplcFuFuVZR(i1,gt2)
coup2R = -cplcFuFuVZL(i1,gt2)
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


! {hh, bar[Fu], bar[Fu]}
If ((Include_in_loophh).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = Mhh(i1) 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuhhL(gt1,i2,i1)
coup1R = cplcFuFuhhR(gt1,i2,i1)
coup2L = cplcFuFuhhL(i3,gt2,i1)
coup2R = cplcFuFuhhR(i3,gt2,i1)
coup3L = cplcFuFuhhL(i2,i3,gt3)
coup3R = cplcFuFuhhR(i2,i3,gt3)
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


! {Hp, bar[Fd], bar[Fd]}
If ((Include_in_loopHp).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MHp 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFdHpL(gt1,i2)
coup1R = cplcFuFdHpR(gt1,i2)
coup2L = cplcFdFucHpL(i3,gt2)
coup2R = cplcFdFucHpR(i3,gt2)
coup3L = cplcFdFdhhL(i2,i3,gt3)
coup3R = cplcFdFdhhR(i2,i3,gt3)
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


! {VG, bar[Fu], bar[Fu]}
If ((Include_in_loopVG).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVG 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = -cplcFuFuVGR(gt1,i2)
coup1R = -cplcFuFuVGL(gt1,i2)
coup2L = -cplcFuFuVGR(i3,gt2)
coup2R = -cplcFuFuVGL(i3,gt2)
coup3L = cplcFuFuhhL(i2,i3,gt3)
coup3R = cplcFuFuhhR(i2,i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(4._dp/3._dp)*AmpC 
    End Do
  End Do
End if 


! {VP, bar[Fu], bar[Fu]}
If ((Include_in_loopVP).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = -cplcFuFuVPR(gt1,i2)
coup1R = -cplcFuFuVPL(gt1,i2)
coup2L = -cplcFuFuVPR(i3,gt2)
coup2R = -cplcFuFuVPL(i3,gt2)
coup3L = cplcFuFuhhL(i2,i3,gt3)
coup3R = cplcFuFuhhR(i2,i3,gt3)
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


! {VWp, bar[Fd], bar[Fd]}
If ((Include_in_loopVWp).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVWp 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = -cplcFuFdVWpR(gt1,i2)
coup1R = -cplcFuFdVWpL(gt1,i2)
coup2L = -cplcFdFucVWpR(i3,gt2)
coup2R = -cplcFdFucVWpL(i3,gt2)
coup3L = cplcFdFdhhL(i2,i3,gt3)
coup3R = cplcFdFdhhR(i2,i3,gt3)
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


! {VZ, bar[Fu], bar[Fu]}
If ((Include_in_loopVZ).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVZ 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = -cplcFuFuVZR(gt1,i2)
coup1R = -cplcFuFuVZL(gt1,i2)
coup2L = -cplcFuFuVZR(i3,gt2)
coup2R = -cplcFuFuVZL(i3,gt2)
coup3L = cplcFuFuhhL(i2,i3,gt3)
coup3R = cplcFuFuhhR(i2,i3,gt3)
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
End Subroutine Amplitude_VERTEX_SDdiracDM_FuToFuhh


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FuToFuhh(MAh,MFd,MFu,Mhh,MHp,MVG,            & 
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
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = Mhh(gt3) 


! {VG, bar[Fu], bar[Fu]}
If ((Include_in_loopVG).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVG 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = -cplcFuFuVGR(gt1,i2)
coup1R = -cplcFuFuVGL(gt1,i2)
coup2L = -cplcFuFuVGR(i3,gt2)
coup2R = -cplcFuFuVGL(i3,gt2)
coup3L = cplcFuFuhhL(i2,i3,gt3)
coup3R = cplcFuFuhhR(i2,i3,gt3)
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(4._dp/3._dp)*AmpC 
    End Do
  End Do
End if 


! {VP, bar[Fu], bar[Fu]}
If ((Include_in_loopVP).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = -cplcFuFuVPR(gt1,i2)
coup1R = -cplcFuFuVPL(gt1,i2)
coup2L = -cplcFuFuVPR(i3,gt2)
coup2R = -cplcFuFuVPL(i3,gt2)
coup3L = cplcFuFuhhL(i2,i3,gt3)
coup3R = cplcFuFuhhR(i2,i3,gt3)
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
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FuToFuhh


Subroutine Amplitude_Tree_SDdiracDM_FuToFuVZ(cplcFuFuVZL,cplcFuFuVZR,MFu,             & 
& MVZ,MFu2,MVZ2,Amp)

Implicit None

Real(dp), Intent(in) :: MFu(3),MVZ,MFu2(3),MVZ2

Complex(dp), Intent(in) :: cplcFuFuVZL(3,3),cplcFuFuVZR(3,3)

Complex(dp) :: Amp(4,3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,3
  Do gt2=1,3
! External masses 
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MVZ 
! Tree-Level Vertex 
coupT1L = cplcFuFuVZL(gt1,gt2)
coupT1R = cplcFuFuVZR(gt1,gt2)
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,coupT1R,coupT1L,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2) = -AmpC 
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_FuToFuVZ


Subroutine Gamma_Real_SDdiracDM_FuToFuVZ(MLambda,em,gs,cplcFuFuVZL,cplcFuFuVZR,       & 
& MFu,MVZ,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFuFuVZL(3,3),cplcFuFuVZR(3,3)

Real(dp), Intent(in) :: MFu(3),MVZ

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(3,3), GammarealGluon(3,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,3
  Do i2=1,3
CoupL = cplcFuFuVZL(i1,i2)
CoupR = cplcFuFuVZR(i1,i2)
Mex1 = MFu(i1)
Mex2 = MFu(i2)
Mex3 = MVZ
If (Mex1.gt.(Mex2+Mex3)) Then 
  Call hardphotonFFZ(Mex1,Mex2,Mex3,MLambda,2._dp/3._dp,2._dp/3._dp,CoupL,CoupR,(0,1)*em,GammaRealPhoton(i1,i2),kont)
  Call hardgluonFFZW(Mex1,Mex2,Mex3,MLambda,4._dp/3._dp,gs,CoupL,CoupR,Gammarealgluon(i1,i2),kont)
Else 
  GammarealGluon(i1,i2) = 0._dp 
  GammarealPhoton(i1,i2) = 0._dp 

End if 
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_FuToFuVZ


Subroutine Amplitude_WAVE_SDdiracDM_FuToFuVZ(cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,     & 
& cplcFuFuVZR,ctcplcFuFuVPL,ctcplcFuFuVPR,ctcplcFuFuVZL,ctcplcFuFuVZR,MFu,               & 
& MFu2,MVP,MVP2,MVZ,MVZ2,ZfUL,ZfUR,ZfVPVZ,ZfVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MFu(3),MFu2(3),MVP,MVP2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3)

Complex(dp), Intent(in) :: ctcplcFuFuVPL(3,3),ctcplcFuFuVPR(3,3),ctcplcFuFuVZL(3,3),ctcplcFuFuVZR(3,3)

Complex(dp), Intent(in) :: ZfUL(3,3),ZfUR(3,3),ZfVPVZ,ZfVZ

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
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MVZ 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFuFuVZL(gt1,gt2) 
ZcoupT1R = ctcplcFuFuVZR(gt1,gt2)
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*Conjg(ZfUL(i1,gt1))*cplcFuFuVZL(i1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfUR(i1,gt1)*cplcFuFuVZR(i1,gt2)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfUL(i1,gt2)*cplcFuFuVZL(gt1,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfUR(i1,gt2))*cplcFuFuVZR(gt1,i1)
End Do


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVPVZ*cplcFuFuVPL(gt1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfVPVZ*cplcFuFuVPR(gt1,gt2)
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVZ*cplcFuFuVZL(gt1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfVZ*cplcFuFuVZR(gt1,gt2)


! Getting the amplitude 
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,ZcoupT1R,ZcoupT1L,AmpC) 
Amp(:,gt1, gt2) = -AmpC 
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FuToFuVZ


Subroutine Amplitude_VERTEX_SDdiracDM_FuToFuVZ(MAh,MFd,MFu,Mhh,MHp,MVG,               & 
& MVP,MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFuFuAhL,cplcFuFuAhR,    & 
& cplAhhhVZ,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,               & 
& cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,               & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFu(3),Mhh(2),MHp,MVG,MVP,MVWp,MVZ,MAh2,MFd2(3),MFu2(3),Mhh2(2),           & 
& MHp2,MVG2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplAhhhVZ(2),cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),     & 
& cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFuFuhhL(3,3,2),& 
& cplcFuFuhhR(3,3,2),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),& 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFdFucVWpL(3,3),& 
& cplcFdFucVWpR(3,3),cplhhVZVZ(2),cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ

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
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MVZ 


! {Ah, bar[Fu], bar[Fu]}
If ((Include_in_loopAh).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MAh 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuAhL(gt1,i2)
coup1R = cplcFuFuAhR(gt1,i2)
coup2L = cplcFuFuAhL(i3,gt2)
coup2R = cplcFuFuAhR(i3,gt2)
coup3L = cplcFuFuVZL(i2,i3)
coup3R = cplcFuFuVZR(i2,i3)
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


! {Fd, conj[Hp], conj[Hp]}
If ((Include_in_loopFd).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MHp 
ML3 = MHp 
coup1L = cplcFuFdHpL(gt1,i1)
coup1R = cplcFuFdHpR(gt1,i1)
coup2L = cplcFdFucHpL(i1,gt2)
coup2R = cplcFdFucHpR(i1,gt2)
coup3 = cplHpcHpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fd, conj[VWp], conj[Hp]}
If ((Include_in_loopFd).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MVWp 
ML3 = MHp 
coup1L = cplcFuFdVWpL(gt1,i1)
coup1R = cplcFuFdVWpR(gt1,i1)
coup2L = cplcFdFucHpL(i1,gt2)
coup2R = cplcFdFucHpR(i1,gt2)
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


! {Fd, conj[Hp], conj[VWp]}
If ((Include_in_loopFd).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MHp 
ML3 = MVWp 
coup1L = cplcFuFdHpL(gt1,i1)
coup1R = cplcFuFdHpR(gt1,i1)
coup2L = cplcFdFucVWpL(i1,gt2)
coup2R = cplcFdFucVWpR(i1,gt2)
coup3 = cplcHpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fd, conj[VWp], conj[VWp]}
If ((Include_in_loopFd).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MVWp 
ML3 = MVWp 
coup1L = cplcFuFdVWpL(gt1,i1)
coup1R = cplcFuFdVWpR(gt1,i1)
coup2L = cplcFdFucVWpL(i1,gt2)
coup2R = cplcFdFucVWpR(i1,gt2)
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


! {Fu, hh, Ah}
If ((Include_in_loopFu).and.(Include_in_loophh).and.(Include_in_loopAh)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFu(i1) 
ML2 = Mhh(i2) 
ML3 = MAh 
coup1L = cplcFuFuhhL(gt1,i1,i2)
coup1R = cplcFuFuhhR(gt1,i1,i2)
coup2L = cplcFuFuAhL(i1,gt2)
coup2R = cplcFuFuAhR(i1,gt2)
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


! {Fu, Ah, hh}
If ((Include_in_loopFu).and.(Include_in_loopAh).and.(Include_in_loophh)) Then 
Do i1=1,3
    Do i3=1,2
ML1 = MFu(i1) 
ML2 = MAh 
ML3 = Mhh(i3) 
coup1L = cplcFuFuAhL(gt1,i1)
coup1R = cplcFuFuAhR(gt1,i1)
coup2L = cplcFuFuhhL(i1,gt2,i3)
coup2R = cplcFuFuhhR(i1,gt2,i3)
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


! {Fu, VZ, hh}
If ((Include_in_loopFu).and.(Include_in_loopVZ).and.(Include_in_loophh)) Then 
Do i1=1,3
    Do i3=1,2
ML1 = MFu(i1) 
ML2 = MVZ 
ML3 = Mhh(i3) 
coup1L = cplcFuFuVZL(gt1,i1)
coup1R = cplcFuFuVZR(gt1,i1)
coup2L = cplcFuFuhhL(i1,gt2,i3)
coup2R = cplcFuFuhhR(i1,gt2,i3)
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


! {Fu, hh, VZ}
If ((Include_in_loopFu).and.(Include_in_loophh).and.(Include_in_loopVZ)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFu(i1) 
ML2 = Mhh(i2) 
ML3 = MVZ 
coup1L = cplcFuFuhhL(gt1,i1,i2)
coup1R = cplcFuFuhhR(gt1,i1,i2)
coup2L = cplcFuFuVZL(i1,gt2)
coup2R = cplcFuFuVZR(i1,gt2)
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


! {hh, bar[Fu], bar[Fu]}
If ((Include_in_loophh).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = Mhh(i1) 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuhhL(gt1,i2,i1)
coup1R = cplcFuFuhhR(gt1,i2,i1)
coup2L = cplcFuFuhhL(i3,gt2,i1)
coup2R = cplcFuFuhhR(i3,gt2,i1)
coup3L = cplcFuFuVZL(i2,i3)
coup3R = cplcFuFuVZR(i2,i3)
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


! {Hp, bar[Fd], bar[Fd]}
If ((Include_in_loopHp).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MHp 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFdHpL(gt1,i2)
coup1R = cplcFuFdHpR(gt1,i2)
coup2L = cplcFdFucHpL(i3,gt2)
coup2R = cplcFdFucHpR(i3,gt2)
coup3L = cplcFdFdVZL(i2,i3)
coup3R = cplcFdFdVZR(i2,i3)
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


! {VG, bar[Fu], bar[Fu]}
If ((Include_in_loopVG).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVG 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuVGL(gt1,i2)
coup1R = cplcFuFuVGR(gt1,i2)
coup2L = cplcFuFuVGL(i3,gt2)
coup2R = cplcFuFuVGR(i3,gt2)
coup3L = cplcFuFuVZL(i2,i3)
coup3R = cplcFuFuVZR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(4._dp/3._dp)*AmpC 
    End Do
  End Do
End if 


! {VP, bar[Fu], bar[Fu]}
If ((Include_in_loopVP).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuVPL(gt1,i2)
coup1R = cplcFuFuVPR(gt1,i2)
coup2L = cplcFuFuVPL(i3,gt2)
coup2R = cplcFuFuVPR(i3,gt2)
coup3L = cplcFuFuVZL(i2,i3)
coup3R = cplcFuFuVZR(i2,i3)
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


! {VWp, bar[Fd], bar[Fd]}
If ((Include_in_loopVWp).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVWp 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFdVWpL(gt1,i2)
coup1R = cplcFuFdVWpR(gt1,i2)
coup2L = cplcFdFucVWpL(i3,gt2)
coup2R = cplcFdFucVWpR(i3,gt2)
coup3L = cplcFdFdVZL(i2,i3)
coup3R = cplcFdFdVZR(i2,i3)
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


! {VZ, bar[Fu], bar[Fu]}
If ((Include_in_loopVZ).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVZ 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuVZL(gt1,i2)
coup1R = cplcFuFuVZR(gt1,i2)
coup2L = cplcFuFuVZL(i3,gt2)
coup2R = cplcFuFuVZR(i3,gt2)
coup3L = cplcFuFuVZL(i2,i3)
coup3R = cplcFuFuVZR(i2,i3)
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
End Subroutine Amplitude_VERTEX_SDdiracDM_FuToFuVZ


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FuToFuVZ(MAh,MFd,MFu,Mhh,MHp,MVG,            & 
& MVP,MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFuFuAhL,cplcFuFuAhR,    & 
& cplAhhhVZ,cplcFuFdHpL,cplcFuFdHpR,cplcFuFdVWpL,cplcFuFdVWpR,cplcFdFdVZL,               & 
& cplcFdFdVZR,cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,               & 
& cplcFuFuVPR,cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,           & 
& cplcFdFucVWpR,cplhhVZVZ,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFu(3),Mhh(2),MHp,MVG,MVP,MVWp,MVZ,MAh2,MFd2(3),MFu2(3),Mhh2(2),           & 
& MHp2,MVG2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplAhhhVZ(2),cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),     & 
& cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFuFuhhL(3,3,2),& 
& cplcFuFuhhR(3,3,2),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),& 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFdFucVWpL(3,3),& 
& cplcFdFucVWpR(3,3),cplhhVZVZ(2),cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ

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
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MVZ 


! {VG, bar[Fu], bar[Fu]}
If ((Include_in_loopVG).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVG 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuVGL(gt1,i2)
coup1R = cplcFuFuVGR(gt1,i2)
coup2L = cplcFuFuVGL(i3,gt2)
coup2R = cplcFuFuVGR(i3,gt2)
coup3L = cplcFuFuVZL(i2,i3)
coup3R = cplcFuFuVZR(i2,i3)
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(4._dp/3._dp)*AmpC 
    End Do
  End Do
End if 


! {VP, bar[Fu], bar[Fu]}
If ((Include_in_loopVP).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuVPL(gt1,i2)
coup1R = cplcFuFuVPR(gt1,i2)
coup2L = cplcFuFuVPL(i3,gt2)
coup2R = cplcFuFuVPR(i3,gt2)
coup3L = cplcFuFuVZL(i2,i3)
coup3R = cplcFuFuVZR(i2,i3)
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FuToFuVZ


Subroutine Amplitude_WAVE_SDdiracDM_FuToFuVG(cplcFuFuVGL,cplcFuFuVGR,ctcplcFuFuVGL,   & 
& ctcplcFuFuVGR,MFu,MFu2,MVG,MVG2,ZfUL,ZfUR,ZfVG,Amp)

Implicit None

Real(dp), Intent(in) :: MFu(3),MFu2(3),MVG,MVG2

Complex(dp), Intent(in) :: cplcFuFuVGL(3,3),cplcFuFuVGR(3,3)

Complex(dp), Intent(in) :: ctcplcFuFuVGL(3,3),ctcplcFuFuVGR(3,3)

Complex(dp), Intent(in) :: ZfUL(3,3),ZfUR(3,3),ZfVG

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
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MVG 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFuFuVGL(gt1,gt2) 
ZcoupT1R = ctcplcFuFuVGR(gt1,gt2)
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*Conjg(ZfUL(i1,gt1))*cplcFuFuVGL(i1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfUR(i1,gt1)*cplcFuFuVGR(i1,gt2)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfUL(i1,gt2)*cplcFuFuVGL(gt1,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfUR(i1,gt2))*cplcFuFuVGR(gt1,i1)
End Do


! External Field 3 


! Getting the amplitude 
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,ZcoupT1R,ZcoupT1L,AmpC) 
Amp(:,gt1, gt2) = -AmpC 
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FuToFuVG


Subroutine Amplitude_VERTEX_SDdiracDM_FuToFuVG(MAh,MFd,MFu,Mhh,MHp,MVG,               & 
& MVP,MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFuFuAhL,cplcFuFuAhR,    & 
& cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFuFdVWpL,cplcFuFdVWpR,             & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,               & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,         & 
& cplVGVGVG,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFu(3),Mhh(2),MHp,MVG,MVP,MVWp,MVZ,MAh2,MFd2(3),MFu2(3),Mhh2(2),           & 
& MHp2,MVG2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),cplcFdFdVGL(3,3), & 
& cplcFdFdVGR(3,3),cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFuFuhhL(3,3,2),               & 
& cplcFuFuhhR(3,3,2),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),& 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFdFucVWpL(3,3),& 
& cplcFdFucVWpR(3,3),cplVGVGVG

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
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MVG 


! {Ah, bar[Fu], bar[Fu]}
If ((Include_in_loopAh).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MAh 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuAhL(gt1,i2)
coup1R = cplcFuFuAhR(gt1,i2)
coup2L = cplcFuFuAhL(i3,gt2)
coup2R = cplcFuFuAhR(i3,gt2)
coup3L = cplcFuFuVGL(i2,i3)
coup3R = cplcFuFuVGR(i2,i3)
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


! {Fu, VG, VG}
If ((Include_in_loopFu).and.(Include_in_loopVG).and.(Include_in_loopVG)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MVG 
ML3 = MVG 
coup1L = cplcFuFuVGL(gt1,i1)
coup1R = cplcFuFuVGR(gt1,i1)
coup2L = cplcFuFuVGL(i1,gt2)
coup2R = cplcFuFuVGR(i1,gt2)
coup3 = cplVGVGVG
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(-3._dp/2._dp*(0.,1._dp))*AmpC 
End Do
End if 


! {hh, bar[Fu], bar[Fu]}
If ((Include_in_loophh).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = Mhh(i1) 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuhhL(gt1,i2,i1)
coup1R = cplcFuFuhhR(gt1,i2,i1)
coup2L = cplcFuFuhhL(i3,gt2,i1)
coup2R = cplcFuFuhhR(i3,gt2,i1)
coup3L = cplcFuFuVGL(i2,i3)
coup3R = cplcFuFuVGR(i2,i3)
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


! {Hp, bar[Fd], bar[Fd]}
If ((Include_in_loopHp).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MHp 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFdHpL(gt1,i2)
coup1R = cplcFuFdHpR(gt1,i2)
coup2L = cplcFdFucHpL(i3,gt2)
coup2R = cplcFdFucHpR(i3,gt2)
coup3L = cplcFdFdVGL(i2,i3)
coup3R = cplcFdFdVGR(i2,i3)
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


! {VG, bar[Fu], bar[Fu]}
If ((Include_in_loopVG).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVG 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuVGL(gt1,i2)
coup1R = cplcFuFuVGR(gt1,i2)
coup2L = cplcFuFuVGL(i3,gt2)
coup2R = cplcFuFuVGR(i3,gt2)
coup3L = cplcFuFuVGL(i2,i3)
coup3R = cplcFuFuVGR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(-1._dp/6._dp)*AmpC 
    End Do
  End Do
End if 


! {VP, bar[Fu], bar[Fu]}
If ((Include_in_loopVP).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuVPL(gt1,i2)
coup1R = cplcFuFuVPR(gt1,i2)
coup2L = cplcFuFuVPL(i3,gt2)
coup2R = cplcFuFuVPR(i3,gt2)
coup3L = cplcFuFuVGL(i2,i3)
coup3R = cplcFuFuVGR(i2,i3)
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


! {VWp, bar[Fd], bar[Fd]}
If ((Include_in_loopVWp).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVWp 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFdVWpL(gt1,i2)
coup1R = cplcFuFdVWpR(gt1,i2)
coup2L = cplcFdFucVWpL(i3,gt2)
coup2R = cplcFdFucVWpR(i3,gt2)
coup3L = cplcFdFdVGL(i2,i3)
coup3R = cplcFdFdVGR(i2,i3)
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


! {VZ, bar[Fu], bar[Fu]}
If ((Include_in_loopVZ).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVZ 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuVZL(gt1,i2)
coup1R = cplcFuFuVZR(gt1,i2)
coup2L = cplcFuFuVZL(i3,gt2)
coup2R = cplcFuFuVZR(i3,gt2)
coup3L = cplcFuFuVGL(i2,i3)
coup3R = cplcFuFuVGR(i2,i3)
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
End Subroutine Amplitude_VERTEX_SDdiracDM_FuToFuVG


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FuToFuVG(MAh,MFd,MFu,Mhh,MHp,MVG,            & 
& MVP,MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFuFuAhL,cplcFuFuAhR,    & 
& cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVGL,cplcFdFdVGR,cplcFuFdVWpL,cplcFuFdVWpR,             & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,               & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,         & 
& cplVGVGVG,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFu(3),Mhh(2),MHp,MVG,MVP,MVWp,MVZ,MAh2,MFd2(3),MFu2(3),Mhh2(2),           & 
& MHp2,MVG2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),cplcFdFdVGL(3,3), & 
& cplcFdFdVGR(3,3),cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFuFuhhL(3,3,2),               & 
& cplcFuFuhhR(3,3,2),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),& 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFdFucVWpL(3,3),& 
& cplcFdFucVWpR(3,3),cplVGVGVG

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
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MVG 


! {Fu, VG, VG}
If ((Include_in_loopFu).and.(Include_in_loopVG).and.(Include_in_loopVG)) Then 
Do i1=1,3
ML1 = MFu(i1) 
ML2 = MVG 
ML3 = MVG 
coup1L = cplcFuFuVGL(gt1,i1)
coup1R = cplcFuFuVGR(gt1,i1)
coup2L = cplcFuFuVGL(i1,gt2)
coup2R = cplcFuFuVGR(i1,gt2)
coup3 = cplVGVGVG
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(-3._dp/2._dp*(0.,1._dp))*AmpC 
End Do
End if 


! {VG, bar[Fu], bar[Fu]}
If ((Include_in_loopVG).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVG 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuVGL(gt1,i2)
coup1R = cplcFuFuVGR(gt1,i2)
coup2L = cplcFuFuVGL(i3,gt2)
coup2R = cplcFuFuVGR(i3,gt2)
coup3L = cplcFuFuVGL(i2,i3)
coup3R = cplcFuFuVGR(i2,i3)
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(-1._dp/6._dp)*AmpC 
    End Do
  End Do
End if 


! {VP, bar[Fu], bar[Fu]}
If ((Include_in_loopVP).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuVPL(gt1,i2)
coup1R = cplcFuFuVPR(gt1,i2)
coup2L = cplcFuFuVPL(i3,gt2)
coup2R = cplcFuFuVPR(i3,gt2)
coup3L = cplcFuFuVGL(i2,i3)
coup3R = cplcFuFuVGR(i2,i3)
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FuToFuVG


Subroutine Amplitude_WAVE_SDdiracDM_FuToFuVP(cplcFuFuVPL,cplcFuFuVPR,cplcFuFuVZL,     & 
& cplcFuFuVZR,ctcplcFuFuVPL,ctcplcFuFuVPR,ctcplcFuFuVZL,ctcplcFuFuVZR,MFu,               & 
& MFu2,MVP,MVP2,ZfUL,ZfUR,ZfVP,ZfVZVP,Amp)

Implicit None

Real(dp), Intent(in) :: MFu(3),MFu2(3),MVP,MVP2

Complex(dp), Intent(in) :: cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3)

Complex(dp), Intent(in) :: ctcplcFuFuVPL(3,3),ctcplcFuFuVPR(3,3),ctcplcFuFuVZL(3,3),ctcplcFuFuVZR(3,3)

Complex(dp), Intent(in) :: ZfUL(3,3),ZfUR(3,3),ZfVP,ZfVZVP

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
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MVP 
ZcoupT1L = 0._dp 
ZcoupT1R = 0._dp 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*Conjg(ZfUL(i1,gt1))*cplcFuFuVPL(i1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfUR(i1,gt1)*cplcFuFuVPR(i1,gt2)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfUL(i1,gt2)*cplcFuFuVPL(gt1,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfUR(i1,gt2))*cplcFuFuVPR(gt1,i1)
End Do


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVZVP*cplcFuFuVZL(gt1,gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfVZVP*cplcFuFuVZR(gt1,gt2)


! Getting the amplitude 
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,ZcoupT1R,ZcoupT1L,AmpC) 
Amp(:,gt1, gt2) = -AmpC 
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FuToFuVP


Subroutine Amplitude_VERTEX_SDdiracDM_FuToFuVP(MAh,MFd,MFu,Mhh,MHp,MVG,               & 
& MVP,MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFuFuAhL,cplcFuFuAhR,    & 
& cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,             & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,               & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,         & 
& cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFu(3),Mhh(2),MHp,MVG,MVP,MVWp,MVZ,MAh2,MFd2(3),MFu2(3),Mhh2(2),           & 
& MHp2,MVG2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),cplcFdFdVPL(3,3), & 
& cplcFdFdVPR(3,3),cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFuFuhhL(3,3,2),               & 
& cplcFuFuhhR(3,3,2),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),& 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFdFucVWpL(3,3),& 
& cplcFdFucVWpR(3,3),cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp

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
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MVP 


! {Ah, bar[Fu], bar[Fu]}
If ((Include_in_loopAh).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MAh 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuAhL(gt1,i2)
coup1R = cplcFuFuAhR(gt1,i2)
coup2L = cplcFuFuAhL(i3,gt2)
coup2R = cplcFuFuAhR(i3,gt2)
coup3L = cplcFuFuVPL(i2,i3)
coup3R = cplcFuFuVPR(i2,i3)
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


! {Fd, conj[Hp], conj[Hp]}
If ((Include_in_loopFd).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MHp 
ML3 = MHp 
coup1L = cplcFuFdHpL(gt1,i1)
coup1R = cplcFuFdHpR(gt1,i1)
coup2L = cplcFdFucHpL(i1,gt2)
coup2R = cplcFdFucHpR(i1,gt2)
coup3 = cplHpcHpVP
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fd, conj[VWp], conj[Hp]}
If ((Include_in_loopFd).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MVWp 
ML3 = MHp 
coup1L = cplcFuFdVWpL(gt1,i1)
coup1R = cplcFuFdVWpR(gt1,i1)
coup2L = cplcFdFucHpL(i1,gt2)
coup2R = cplcFdFucHpR(i1,gt2)
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


! {Fd, conj[Hp], conj[VWp]}
If ((Include_in_loopFd).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MHp 
ML3 = MVWp 
coup1L = cplcFuFdHpL(gt1,i1)
coup1R = cplcFuFdHpR(gt1,i1)
coup2L = cplcFdFucVWpL(i1,gt2)
coup2R = cplcFdFucVWpR(i1,gt2)
coup3 = cplcHpVPVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fd, conj[VWp], conj[VWp]}
If ((Include_in_loopFd).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFd(i1) 
ML2 = MVWp 
ML3 = MVWp 
coup1L = cplcFuFdVWpL(gt1,i1)
coup1R = cplcFuFdVWpR(gt1,i1)
coup2L = cplcFdFucVWpL(i1,gt2)
coup2R = cplcFdFucVWpR(i1,gt2)
coup3 = -cplcVWpVPVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {hh, bar[Fu], bar[Fu]}
If ((Include_in_loophh).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = Mhh(i1) 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuhhL(gt1,i2,i1)
coup1R = cplcFuFuhhR(gt1,i2,i1)
coup2L = cplcFuFuhhL(i3,gt2,i1)
coup2R = cplcFuFuhhR(i3,gt2,i1)
coup3L = cplcFuFuVPL(i2,i3)
coup3R = cplcFuFuVPR(i2,i3)
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


! {Hp, bar[Fd], bar[Fd]}
If ((Include_in_loopHp).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MHp 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFdHpL(gt1,i2)
coup1R = cplcFuFdHpR(gt1,i2)
coup2L = cplcFdFucHpL(i3,gt2)
coup2R = cplcFdFucHpR(i3,gt2)
coup3L = cplcFdFdVPL(i2,i3)
coup3R = cplcFdFdVPR(i2,i3)
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


! {VG, bar[Fu], bar[Fu]}
If ((Include_in_loopVG).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVG 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuVGL(gt1,i2)
coup1R = cplcFuFuVGR(gt1,i2)
coup2L = cplcFuFuVGL(i3,gt2)
coup2R = cplcFuFuVGR(i3,gt2)
coup3L = cplcFuFuVPL(i2,i3)
coup3R = cplcFuFuVPR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(4._dp/3._dp)*AmpC 
    End Do
  End Do
End if 


! {VP, bar[Fu], bar[Fu]}
If ((Include_in_loopVP).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuVPL(gt1,i2)
coup1R = cplcFuFuVPR(gt1,i2)
coup2L = cplcFuFuVPL(i3,gt2)
coup2R = cplcFuFuVPR(i3,gt2)
coup3L = cplcFuFuVPL(i2,i3)
coup3R = cplcFuFuVPR(i2,i3)
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


! {VWp, bar[Fd], bar[Fd]}
If ((Include_in_loopVWp).and.(Include_in_loopFd).and.(Include_in_loopFd)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVWp 
ML2 = MFd(i2) 
ML3 = MFd(i3) 
coup1L = cplcFuFdVWpL(gt1,i2)
coup1R = cplcFuFdVWpR(gt1,i2)
coup2L = cplcFdFucVWpL(i3,gt2)
coup2R = cplcFdFucVWpR(i3,gt2)
coup3L = cplcFdFdVPL(i2,i3)
coup3R = cplcFdFdVPR(i2,i3)
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


! {VZ, bar[Fu], bar[Fu]}
If ((Include_in_loopVZ).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVZ 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuVZL(gt1,i2)
coup1R = cplcFuFuVZR(gt1,i2)
coup2L = cplcFuFuVZL(i3,gt2)
coup2R = cplcFuFuVZR(i3,gt2)
coup3L = cplcFuFuVPL(i2,i3)
coup3R = cplcFuFuVPR(i2,i3)
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
End Subroutine Amplitude_VERTEX_SDdiracDM_FuToFuVP


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FuToFuVP(MAh,MFd,MFu,Mhh,MHp,MVG,            & 
& MVP,MVWp,MVZ,MAh2,MFd2,MFu2,Mhh2,MHp2,MVG2,MVP2,MVWp2,MVZ2,cplcFuFuAhL,cplcFuFuAhR,    & 
& cplcFuFdHpL,cplcFuFdHpR,cplcFdFdVPL,cplcFdFdVPR,cplcFuFdVWpL,cplcFuFdVWpR,             & 
& cplcFuFuhhL,cplcFuFuhhR,cplcFuFuVGL,cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,               & 
& cplcFuFuVZL,cplcFuFuVZR,cplcFdFucHpL,cplcFdFucHpR,cplcFdFucVWpL,cplcFdFucVWpR,         & 
& cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFd(3),MFu(3),Mhh(2),MHp,MVG,MVP,MVWp,MVZ,MAh2,MFd2(3),MFu2(3),Mhh2(2),           & 
& MHp2,MVG2,MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFuFuAhL(3,3),cplcFuFuAhR(3,3),cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),cplcFdFdVPL(3,3), & 
& cplcFdFdVPR(3,3),cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFuFuhhL(3,3,2),               & 
& cplcFuFuhhR(3,3,2),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),& 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplcFdFucHpL(3,3),cplcFdFucHpR(3,3),cplcFdFucVWpL(3,3),& 
& cplcFdFucVWpR(3,3),cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp

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
Mex1 = MFu(gt1) 
Mex2 = MFu(gt2) 
Mex3 = MVP 


! {VG, bar[Fu], bar[Fu]}
If ((Include_in_loopVG).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVG 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuVGL(gt1,i2)
coup1R = cplcFuFuVGR(gt1,i2)
coup2L = cplcFuFuVGL(i3,gt2)
coup2R = cplcFuFuVGR(i3,gt2)
coup3L = cplcFuFuVPL(i2,i3)
coup3R = cplcFuFuVPR(i2,i3)
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(4._dp/3._dp)*AmpC 
    End Do
  End Do
End if 


! {VP, bar[Fu], bar[Fu]}
If ((Include_in_loopVP).and.(Include_in_loopFu).and.(Include_in_loopFu)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MVP 
ML2 = MFu(i2) 
ML3 = MFu(i3) 
coup1L = cplcFuFuVPL(gt1,i2)
coup1R = cplcFuFuVPR(gt1,i2)
coup2L = cplcFuFuVPL(i3,gt2)
coup2R = cplcFuFuVPR(i3,gt2)
coup3L = cplcFuFuVPL(i2,i3)
coup3R = cplcFuFuVPR(i2,i3)
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FuToFuVP



End Module OneLoopDecay_Fu_SDdiracDM
