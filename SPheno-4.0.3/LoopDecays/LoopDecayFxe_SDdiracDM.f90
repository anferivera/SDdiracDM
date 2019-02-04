! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:21 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module OneLoopDecay_Fxe_SDdiracDM
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

Subroutine Amplitude_Tree_SDdiracDM_FxeToFeSsc(cplcFxeFeSscL,cplcFxeFeSscR,           & 
& MFe,MFxe,MSsc,MFe2,MFxe2,MSsc2,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFxe,MSsc(2),MFe2(3),MFxe2,MSsc2(2)

Complex(dp), Intent(in) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2)

Complex(dp) :: Amp(2,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

  Do gt2=1,3
    Do gt3=1,2
! External masses 
Mex1 = MFxe 
Mex2 = MFe(gt2) 
Mex3 = MSsc(gt3) 
! Tree-Level Vertex 
coupT1L = cplcFxeFeSscL(gt2,gt3)
coupT1R = cplcFxeFeSscR(gt2,gt3)
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt2, gt3) = AmpC 
    End Do
  End Do
End Subroutine Amplitude_Tree_SDdiracDM_FxeToFeSsc


Subroutine Gamma_Real_SDdiracDM_FxeToFeSsc(MLambda,em,gs,cplcFxeFeSscL,               & 
& cplcFxeFeSscR,MFe,MFxe,MSsc,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2)

Real(dp), Intent(in) :: MFe(3),MFxe,MSsc(2)

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(3,2), GammarealGluon(3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
  Do i2=1,3
    Do i3=1,2
CoupL = cplcFxeFeSscL(i2,i3)
CoupR = cplcFxeFeSscR(i2,i3)
Mex1 = MFxe
Mex2 = MFe(i2)
Mex3 = MSsc(i3)
If (Mex1.gt.(Mex2+Mex3)) Then 
 Call hardradiationFFS(Mex1,Mex2,Mex3,MLambda,em,1._dp,1._dp,0._dp,1._dp,0._dp,0._dp,CoupL,CoupR,Gammarealphoton(i2,i3),kont)
  GammarealGluon(i2,i3) = 0._dp 
Else 
  GammarealGluon(i2,i3) = 0._dp 
  GammarealPhoton(i2,i3) = 0._dp 

End if 
    End Do
  End Do
End Subroutine Gamma_Real_SDdiracDM_FxeToFeSsc


Subroutine Amplitude_WAVE_SDdiracDM_FxeToFeSsc(cplcFxeFeSscL,cplcFxeFeSscR,           & 
& ctcplcFxeFeSscL,ctcplcFxeFeSscR,MFe,MFe2,MFxe,MFxe2,MSsc,MSsc2,Zfed,ZfEL,              & 
& ZfER,Zfeu,ZfSsc,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFe2(3),MFxe,MFxe2,MSsc(2),MSsc2(2)

Complex(dp), Intent(in) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2)

Complex(dp), Intent(in) :: ctcplcFxeFeSscL(3,2),ctcplcFxeFeSscR(3,2)

Complex(dp), Intent(in) :: Zfed,ZfEL(3,3),ZfER(3,3),Zfeu,ZfSsc(2,2)

Complex(dp), Intent(out) :: Amp(2,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

  Do gt2=1,3
    Do gt3=1,2
! External masses 
Mex1 = MFxe 
Mex2 = MFe(gt2) 
Mex3 = MSsc(gt3) 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFxeFeSscL(gt2,gt3) 
ZcoupT1R = ctcplcFxeFeSscR(gt2,gt3) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
ZcoupT1L = ZcoupT1L + 0.5_dp*Zfeu*cplcFxeFeSscL(gt2,gt3)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(Zfed)*cplcFxeFeSscR(gt2,gt3)


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfEL(i1,gt2)*cplcFxeFeSscL(i1,gt3)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfER(i1,gt2))*cplcFxeFeSscR(i1,gt3)
End Do


! External Field 3 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfSsc(i1,gt3)*cplcFxeFeSscL(gt2,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfSsc(i1,gt3)*cplcFxeFeSscR(gt2,i1)
End Do


! Getting the amplitude 
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt2, gt3) = AmpC 
    End Do
  End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FxeToFeSsc


Subroutine Amplitude_VERTEX_SDdiracDM_FxeToFeSsc(MFe,MFv,MFxe,MFxv,Mhh,               & 
& MHp,MSsc,MVP,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,           & 
& cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,           & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,cplcFeFeVZR,             & 
& cplcFxvFvSscL,cplcFxvFvSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,   & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhSsccSsc,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MFe2(3),MFv2(3),           & 
& MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),              & 
& cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),               & 
& cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFxvFvSscL(2,3,2),& 
& cplcFxvFvSscR(2,3,2),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,          & 
& cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),             & 
& cplhhSsccSsc(2,2,2)

Complex(dp), Intent(out) :: Amp(2,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
  Do gt2=1,3
    Do gt3=1,2
Amp(:,gt2, gt3) = 0._dp 
! External masses 
Mex1 = MFxe 
Mex2 = MFe(gt2) 
Mex3 = MSsc(gt3) 


! {Fe, conj[Ssc], hh}
If ((Include_in_loopFe).and.(Include_in_loopSsc).and.(Include_in_loophh)) Then 
Do i1=1,3
  Do i2=1,2
    Do i3=1,2
ML1 = MFe(i1) 
ML2 = MSsc(i2) 
ML3 = Mhh(i3) 
coup1L = cplcFxeFeSscL(i1,i2)
coup1R = cplcFxeFeSscR(i1,i2)
coup2L = cplcFeFehhL(i1,gt2,i3)
coup2R = cplcFeFehhR(i1,gt2,i3)
coup3 = cplhhSsccSsc(i3,gt3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2, gt3) = Amp(:,gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {VP, bar[Fxe], bar[Fe]}
If ((Include_in_loopVP).and.(Include_in_loopFxe).and.(Include_in_loopFe)) Then 
    Do i3=1,3
ML1 = MVP 
ML2 = MFxe 
ML3 = MFe(i3) 
coup1L = cplcFxeFxeVPL
coup1R = cplcFxeFxeVPR
coup2L = -cplcFeFeVPR(i3,gt2)
coup2R = -cplcFeFeVPL(i3,gt2)
coup3L = cplcFxeFeSscL(i3,gt3)
coup3R = cplcFxeFeSscR(i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2, gt3) = Amp(:,gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {VZ, bar[Fxe], bar[Fe]}
If ((Include_in_loopVZ).and.(Include_in_loopFxe).and.(Include_in_loopFe)) Then 
    Do i3=1,3
ML1 = MVZ 
ML2 = MFxe 
ML3 = MFe(i3) 
coup1L = cplcFxeFxeVZL
coup1R = cplcFxeFxeVZR
coup2L = -cplcFeFeVZR(i3,gt2)
coup2R = -cplcFeFeVZL(i3,gt2)
coup3L = cplcFxeFeSscL(i3,gt3)
coup3R = cplcFxeFeSscR(i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2, gt3) = Amp(:,gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 


! {conj[Hp], bar[Fxv], bar[Fv]}
If ((Include_in_loopHp).and.(Include_in_loopFxv).and.(Include_in_loopFv)) Then 
  Do i2=1,2
    Do i3=1,3
ML1 = MHp 
ML2 = MFxv(i2) 
ML3 = MFv(i3) 
coup1L = cplcFxeFxvcHpL(i2)
coup1R = cplcFxeFxvcHpR(i2)
coup2L = cplcFvFeHpL(i3,gt2)
coup2R = cplcFvFeHpR(i3,gt2)
coup3L = cplcFxvFvSscL(i2,i3,gt3)
coup3R = cplcFxvFvSscR(i2,i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2, gt3) = Amp(:,gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 


! {conj[VWp], bar[Fxv], bar[Fv]}
If ((Include_in_loopVWp).and.(Include_in_loopFxv).and.(Include_in_loopFv)) Then 
  Do i2=1,2
    Do i3=1,3
ML1 = MVWp 
ML2 = MFxv(i2) 
ML3 = MFv(i3) 
coup1L = cplcFxeFxvcVWpL(i2)
coup1R = cplcFxeFxvcVWpR(i2)
coup2L = -cplcFvFeVWpR(i3,gt2)
coup2R = -cplcFvFeVWpL(i3,gt2)
coup3L = cplcFxvFvSscL(i2,i3,gt3)
coup3R = cplcFxvFvSscR(i2,i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2, gt3) = Amp(:,gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
    End Do
  End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_FxeToFeSsc


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxeToFeSsc(MFe,MFv,MFxe,MFxv,Mhh,            & 
& MHp,MSsc,MVP,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,           & 
& cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,           & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFeFeVZL,cplcFeFeVZR,             & 
& cplcFxvFvSscL,cplcFxvFvSscR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,   & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhSsccSsc,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MFe2(3),MFv2(3),           & 
& MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),              & 
& cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),               & 
& cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFxvFvSscL(2,3,2),& 
& cplcFxvFvSscR(2,3,2),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,          & 
& cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),             & 
& cplhhSsccSsc(2,2,2)

Complex(dp), Intent(out) :: Amp(2,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
  Do gt2=1,3
    Do gt3=1,2
Amp(:,gt2, gt3) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFxe 
Mex2 = MFe(gt2) 
Mex3 = MSsc(gt3) 


! {VP, bar[Fxe], bar[Fe]}
If ((Include_in_loopVP).and.(Include_in_loopFxe).and.(Include_in_loopFe)) Then 
    Do i3=1,3
ML1 = MVP 
ML2 = MFxe 
ML3 = MFe(i3) 
coup1L = cplcFxeFxeVPL
coup1R = cplcFxeFxeVPR
coup2L = -cplcFeFeVPR(i3,gt2)
coup2R = -cplcFeFeVPL(i3,gt2)
coup3L = cplcFxeFeSscL(i3,gt3)
coup3R = cplcFxeFeSscR(i3,gt3)
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt2, gt3) = Amp(:,gt2, gt3) + oo16pi2*(1)*AmpC 
    End Do
End if 
    End Do
  End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxeToFeSsc


Subroutine Amplitude_Tree_SDdiracDM_FxeToFxeVZ(cplcFxeFxeVZL,cplcFxeFxeVZR,           & 
& MFxe,MVZ,MFxe2,MVZ2,Amp)

Implicit None

Real(dp), Intent(in) :: MFxe,MVZ,MFxe2,MVZ2

Complex(dp), Intent(in) :: cplcFxeFxeVZL,cplcFxeFxeVZR

Complex(dp) :: Amp(4) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

! External masses 
Mex1 = MFxe 
Mex2 = MFxe 
Mex3 = MVZ 
! Tree-Level Vertex 
coupT1L = cplcFxeFxeVZL
coupT1R = cplcFxeFxeVZR
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,coupT1R,coupT1L,AmpC) 
! Colour and symmetry factor 
Amp(:) = -AmpC 
End Subroutine Amplitude_Tree_SDdiracDM_FxeToFxeVZ


Subroutine Gamma_Real_SDdiracDM_FxeToFxeVZ(MLambda,em,gs,cplcFxeFxeVZL,               & 
& cplcFxeFxeVZR,MFxe,MVZ,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFxeFxeVZL,cplcFxeFxeVZR

Real(dp), Intent(in) :: MFxe,MVZ

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton, GammarealGluon 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
CoupL = cplcFxeFxeVZL
CoupR = cplcFxeFxeVZR
Mex1 = MFxe
Mex2 = MFxe
Mex3 = MVZ
If (Mex1.gt.(Mex2+Mex3)) Then 
  Call hardphotonFFZ(Mex1,Mex2,Mex3,MLambda,-1._dp,-1._dp,CoupL,CoupR,(0,1)*em,GammaRealPhoton,kont)
  GammarealGluon = 0._dp 
Else 
  GammarealGluon = 0._dp 
  GammarealPhoton = 0._dp 

End if 
End Subroutine Gamma_Real_SDdiracDM_FxeToFxeVZ


Subroutine Amplitude_WAVE_SDdiracDM_FxeToFxeVZ(cplcFxeFxeVPL,cplcFxeFxeVPR,           & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,ctcplcFxeFxeVPL,ctcplcFxeFxeVPR,ctcplcFxeFxeVZL,           & 
& ctcplcFxeFxeVZR,MFxe,MFxe2,MVP,MVP2,MVZ,MVZ2,Zfed,Zfeu,ZfVPVZ,ZfVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MFxe,MFxe2,MVP,MVP2,MVZ,MVZ2

Complex(dp), Intent(in) :: cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR

Complex(dp), Intent(in) :: ctcplcFxeFxeVPL,ctcplcFxeFxeVPR,ctcplcFxeFxeVZL,ctcplcFxeFxeVZR

Complex(dp), Intent(in) :: Zfed,Zfeu,ZfVPVZ,ZfVZ

Complex(dp), Intent(out) :: Amp(4) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

! External masses 
Mex1 = MFxe 
Mex2 = MFxe 
Mex3 = MVZ 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFxeFxeVZL 
ZcoupT1R = ctcplcFxeFxeVZR
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
ZcoupT1L = ZcoupT1L + 0.5_dp*Conjg(Zfed)*cplcFxeFxeVZL
ZcoupT1R = ZcoupT1R + 0.5_dp*Zfeu*cplcFxeFxeVZR


! External Field 2 
ZcoupT1L = ZcoupT1L + 0.5_dp*Zfed*cplcFxeFxeVZL
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(Zfeu)*cplcFxeFxeVZR


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVPVZ*cplcFxeFxeVPL
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfVPVZ*cplcFxeFxeVPR
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVZ*cplcFxeFxeVZL
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfVZ*cplcFxeFxeVZR


! Getting the amplitude 
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,ZcoupT1R,ZcoupT1L,AmpC) 
Amp(:) = -AmpC 
End Subroutine Amplitude_WAVE_SDdiracDM_FxeToFxeVZ


Subroutine Amplitude_VERTEX_SDdiracDM_FxeToFxeVZ(MFe,MFxe,MFxv,MHp,MSsc,              & 
& MVP,MVWp,MVZ,MFe2,MFxe2,MFxv2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFxeFeSscL,cplcFxeFeSscR,  & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,       & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,              & 
& cplcFeFxecSscR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,              & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFxe,MFxv(2),MHp,MSsc(2),MVP,MVWp,MVZ,MFe2(3),MFxe2,MFxv2(2),MHp2,             & 
& MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),              & 
& cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL(2),       & 
& cplcFxvFxeVWpR(2),cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2), & 
& cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),             & 
& cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,              & 
& cplcVWpVWpVZ

Complex(dp), Intent(out) :: Amp(4) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Amp(:) = 0._dp 
! External masses 
Mex1 = MFxe 
Mex2 = MFxe 
Mex3 = MVZ 


! {Fxv, Hp, Hp}
If ((Include_in_loopFxv).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MHp 
ML3 = MHp 
coup1L = cplcFxeFxvcHpL(i1)
coup1R = cplcFxeFxvcHpR(i1)
coup2L = cplcFxvFxeHpL(i1)
coup2R = cplcFxvFxeHpR(i1)
coup3 = -cplHpcHpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, VWp, Hp}
If ((Include_in_loopFxv).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MVWp 
ML3 = MHp 
coup1L = cplcFxeFxvcVWpL(i1)
coup1R = cplcFxeFxvcVWpR(i1)
coup2L = cplcFxvFxeHpL(i1)
coup2R = cplcFxvFxeHpR(i1)
coup3 = cplcHpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, Hp, VWp}
If ((Include_in_loopFxv).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MHp 
ML3 = MVWp 
coup1L = cplcFxeFxvcHpL(i1)
coup1R = cplcFxeFxvcHpR(i1)
coup2L = cplcFxvFxeVWpL(i1)
coup2R = cplcFxvFxeVWpR(i1)
coup3 = cplHpcVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, VWp, VWp}
If ((Include_in_loopFxv).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MVWp 
ML3 = MVWp 
coup1L = cplcFxeFxvcVWpL(i1)
coup1R = cplcFxeFxvcVWpR(i1)
coup2L = cplcFxvFxeVWpL(i1)
coup2R = cplcFxvFxeVWpR(i1)
coup3 = -cplcVWpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Ssc, bar[Fe], bar[Fe]}
If ((Include_in_loopSsc).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = MSsc(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFxeFeSscL(i2,i1)
coup1R = cplcFxeFeSscR(i2,i1)
coup2L = cplcFeFxecSscL(i3,i1)
coup2R = cplcFeFxecSscR(i3,i1)
coup3L = cplcFeFeVZL(i2,i3)
coup3R = cplcFeFeVZR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {VP, bar[Fxe], bar[Fxe]}
If ((Include_in_loopVP).and.(Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
ML1 = MVP 
ML2 = MFxe 
ML3 = MFxe 
coup1L = cplcFxeFxeVPL
coup1R = cplcFxeFxeVPR
coup2L = cplcFxeFxeVPL
coup2R = cplcFxeFxeVPR
coup3L = cplcFxeFxeVZL
coup3R = cplcFxeFxeVZR
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
End if 


! {VZ, bar[Fxe], bar[Fxe]}
If ((Include_in_loopVZ).and.(Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
ML1 = MVZ 
ML2 = MFxe 
ML3 = MFxe 
coup1L = cplcFxeFxeVZL
coup1R = cplcFxeFxeVZR
coup2L = cplcFxeFxeVZL
coup2R = cplcFxeFxeVZR
coup3L = cplcFxeFxeVZL
coup3R = cplcFxeFxeVZR
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
End if 


! {conj[Hp], bar[Fxv], bar[Fxv]}
If ((Include_in_loopHp).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
  Do i2=1,2
    Do i3=1,2
ML1 = MHp 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxeFxvcHpL(i2)
coup1R = cplcFxeFxvcHpR(i2)
coup2L = cplcFxvFxeHpL(i3)
coup2R = cplcFxvFxeHpR(i3)
coup3L = cplcFxvFxvVZL(i2,i3)
coup3R = cplcFxvFxvVZR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 


! {conj[VWp], bar[Fxv], bar[Fxv]}
If ((Include_in_loopVWp).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
  Do i2=1,2
    Do i3=1,2
ML1 = MVWp 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxeFxvcVWpL(i2)
coup1R = cplcFxeFxvcVWpR(i2)
coup2L = cplcFxvFxeVWpL(i3)
coup2R = cplcFxvFxeVWpR(i3)
coup3L = cplcFxvFxvVZL(i2,i3)
coup3R = cplcFxvFxvVZR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
End Subroutine Amplitude_VERTEX_SDdiracDM_FxeToFxeVZ


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxeToFxeVZ(MFe,MFxe,MFxv,MHp,MSsc,           & 
& MVP,MVWp,MVZ,MFe2,MFxe2,MFxv2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFxeFeSscL,cplcFxeFeSscR,  & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,       & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,              & 
& cplcFeFxecSscR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,              & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,cplcVWpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFxe,MFxv(2),MHp,MSsc(2),MVP,MVWp,MVZ,MFe2(3),MFxe2,MFxv2(2),MHp2,             & 
& MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),              & 
& cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL(2),       & 
& cplcFxvFxeVWpR(2),cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2), & 
& cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),             & 
& cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplHpcHpVZ,cplHpcVWpVZ,cplcHpVWpVZ,              & 
& cplcVWpVWpVZ

Complex(dp), Intent(out) :: Amp(4) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Amp(:) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFxe 
Mex2 = MFxe 
Mex3 = MVZ 


! {VP, bar[Fxe], bar[Fxe]}
If ((Include_in_loopVP).and.(Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
ML1 = MVP 
ML2 = MFxe 
ML3 = MFxe 
coup1L = cplcFxeFxeVPL
coup1R = cplcFxeFxeVPR
coup2L = cplcFxeFxeVPL
coup2R = cplcFxeFxeVPR
coup3L = cplcFxeFxeVZL
coup3R = cplcFxeFxeVZR
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
End if 

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxeToFxeVZ


Subroutine Amplitude_Tree_SDdiracDM_FxeToFxvcHp(cplcFxeFxvcHpL,cplcFxeFxvcHpR,        & 
& MFxe,MFxv,MHp,MFxe2,MFxv2,MHp2,Amp)

Implicit None

Real(dp), Intent(in) :: MFxe,MFxv(2),MHp,MFxe2,MFxv2(2),MHp2

Complex(dp), Intent(in) :: cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2)

Complex(dp) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

  Do gt2=1,2
! External masses 
Mex1 = MFxe 
Mex2 = MFxv(gt2) 
Mex3 = MHp 
! Tree-Level Vertex 
coupT1L = cplcFxeFxvcHpL(gt2)
coupT1R = cplcFxeFxvcHpR(gt2)
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt2) = AmpC 
  End Do
End Subroutine Amplitude_Tree_SDdiracDM_FxeToFxvcHp


Subroutine Gamma_Real_SDdiracDM_FxeToFxvcHp(MLambda,em,gs,cplcFxeFxvcHpL,             & 
& cplcFxeFxvcHpR,MFxe,MFxv,MHp,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2)

Real(dp), Intent(in) :: MFxe,MFxv(2),MHp

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2), GammarealGluon(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
  Do i2=1,2
CoupL = cplcFxeFxvcHpL(i2)
CoupR = cplcFxeFxvcHpR(i2)
Mex1 = MFxe
Mex2 = MFxv(i2)
Mex3 = MHp
If (Mex1.gt.(Mex2+Mex3)) Then 
 Call hardradiationFFS(Mex1,Mex2,Mex3,MLambda,em,1._dp,0._dp,1._dp,0._dp,0._dp,1._dp,CoupL,CoupR,Gammarealphoton(i2),kont)
  GammarealGluon(i2) = 0._dp 
Else 
  GammarealGluon(i2) = 0._dp 
  GammarealPhoton(i2) = 0._dp 

End if 
  End Do
End Subroutine Gamma_Real_SDdiracDM_FxeToFxvcHp


Subroutine Amplitude_WAVE_SDdiracDM_FxeToFxvcHp(cplcFxeFxvcHpL,cplcFxeFxvcHpR,        & 
& ctcplcFxeFxvcHpL,ctcplcFxeFxvcHpR,MFxe,MFxe2,MFxv,MFxv2,MHp,MHp2,Zfed,Zfeu,            & 
& ZfHp,ZfxVL,ZfxVR,Amp)

Implicit None

Real(dp), Intent(in) :: MFxe,MFxe2,MFxv(2),MFxv2(2),MHp,MHp2

Complex(dp), Intent(in) :: cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2)

Complex(dp), Intent(in) :: ctcplcFxeFxvcHpL(2),ctcplcFxeFxvcHpR(2)

Complex(dp), Intent(in) :: Zfed,Zfeu,ZfHp,ZfxVL(2,2),ZfxVR(2,2)

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

  Do gt2=1,2
! External masses 
Mex1 = MFxe 
Mex2 = MFxv(gt2) 
Mex3 = MHp 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFxeFxvcHpL(gt2) 
ZcoupT1R = ctcplcFxeFxvcHpR(gt2) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
ZcoupT1L = ZcoupT1L + 0.5_dp*Zfeu*cplcFxeFxvcHpL(gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(Zfed)*cplcFxeFxvcHpR(gt2)


! External Field 2 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfxVL(i1,gt2)*cplcFxeFxvcHpL(i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfxVR(i1,gt2))*cplcFxeFxvcHpR(i1)
End Do


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*Conjg(ZfHp)*cplcFxeFxvcHpL(gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfHp)*cplcFxeFxvcHpR(gt2)


! Getting the amplitude 
Call TreeAmp_FtoFS(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt2) = AmpC 
  End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FxeToFxvcHp


Subroutine Amplitude_VERTEX_SDdiracDM_FxeToFxvcHp(MAh,MFe,MFv,MFxe,MFxv,              & 
& Mhh,MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,             & 
& MVWp2,MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhcHpVWp,cplcFxeFeSscL,cplcFxeFeSscR,        & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,     & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR, & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcHp,              & 
& cplhhcHpVWp,cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),          & 
& MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplAhcHpVWp,cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),& 
& cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,         & 
& cplcFxeFxeVZR,cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),            & 
& cplcFxvFxvVZR(2,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),cplcFxeFxvcHpL(2),      & 
& cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplhhHpcHp(2),cplhhcHpVWp(2),  & 
& cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
  Do gt2=1,2
Amp(:,gt2) = 0._dp 
! External masses 
Mex1 = MFxe 
Mex2 = MFxv(gt2) 
Mex3 = MHp 


! {Fxe, VP, conj[Hp]}
If ((Include_in_loopFxe).and.(Include_in_loopVP).and.(Include_in_loopHp)) Then 
ML1 = MFxe 
ML2 = MVP 
ML3 = MHp 
coup1L = cplcFxeFxeVPL
coup1R = cplcFxeFxeVPR
coup2L = cplcFxeFxvcHpL(gt2)
coup2R = cplcFxeFxvcHpR(gt2)
coup3 = cplHpcHpVP
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, VZ, conj[Hp]}
If ((Include_in_loopFxe).and.(Include_in_loopVZ).and.(Include_in_loopHp)) Then 
ML1 = MFxe 
ML2 = MVZ 
ML3 = MHp 
coup1L = cplcFxeFxeVZL
coup1R = cplcFxeFxeVZR
coup2L = cplcFxeFxvcHpL(gt2)
coup2R = cplcFxeFxvcHpR(gt2)
coup3 = cplHpcHpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, VP, conj[VWp]}
If ((Include_in_loopFxe).and.(Include_in_loopVP).and.(Include_in_loopVWp)) Then 
ML1 = MFxe 
ML2 = MVP 
ML3 = MVWp 
coup1L = cplcFxeFxeVPL
coup1R = cplcFxeFxeVPR
coup2L = cplcFxeFxvcVWpL(gt2)
coup2R = cplcFxeFxvcVWpR(gt2)
coup3 = cplcHpVPVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, VZ, conj[VWp]}
If ((Include_in_loopFxe).and.(Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
ML1 = MFxe 
ML2 = MVZ 
ML3 = MVWp 
coup1L = cplcFxeFxeVZL
coup1R = cplcFxeFxeVZR
coup2L = cplcFxeFxvcVWpL(gt2)
coup2R = cplcFxeFxvcVWpR(gt2)
coup3 = cplcHpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxv, VWp, Ah}
If ((Include_in_loopFxv).and.(Include_in_loopVWp).and.(Include_in_loopAh)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MVWp 
ML3 = MAh 
coup1L = cplcFxeFxvcVWpL(i1)
coup1R = cplcFxeFxvcVWpR(i1)
coup2L = cplcFxvFxvAhL(i1,gt2)
coup2R = cplcFxvFxvAhR(i1,gt2)
coup3 = cplAhcHpVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, Hp, hh}
If ((Include_in_loopFxv).and.(Include_in_loopHp).and.(Include_in_loophh)) Then 
Do i1=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = MHp 
ML3 = Mhh(i3) 
coup1L = cplcFxeFxvcHpL(i1)
coup1R = cplcFxeFxvcHpR(i1)
coup2L = cplcFxvFxvhhL(i1,gt2,i3)
coup2R = cplcFxvFxvhhR(i1,gt2,i3)
coup3 = cplhhHpcHp(i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
    End Do
End Do
End if 


! {Fxv, VWp, hh}
If ((Include_in_loopFxv).and.(Include_in_loopVWp).and.(Include_in_loophh)) Then 
Do i1=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = MVWp 
ML3 = Mhh(i3) 
coup1L = cplcFxeFxvcVWpL(i1)
coup1R = cplcFxeFxvcVWpR(i1)
coup2L = cplcFxvFxvhhL(i1,gt2,i3)
coup2R = cplcFxvFxvhhR(i1,gt2,i3)
coup3 = cplhhcHpVWp(i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
    End Do
End Do
End if 


! {Fxv, Hp, VZ}
If ((Include_in_loopFxv).and.(Include_in_loopHp).and.(Include_in_loopVZ)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MHp 
ML3 = MVZ 
coup1L = cplcFxeFxvcHpL(i1)
coup1R = cplcFxeFxvcHpR(i1)
coup2L = -cplcFxvFxvVZR(i1,gt2)
coup2R = -cplcFxvFxvVZL(i1,gt2)
coup3 = cplHpcHpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, VWp, VZ}
If ((Include_in_loopFxv).and.(Include_in_loopVWp).and.(Include_in_loopVZ)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MVWp 
ML3 = MVZ 
coup1L = cplcFxeFxvcVWpL(i1)
coup1R = cplcFxeFxvcVWpR(i1)
coup2L = -cplcFxvFxvVZR(i1,gt2)
coup2R = -cplcFxvFxvVZL(i1,gt2)
coup3 = cplcHpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Ssc, bar[Fe], bar[Fv]}
If ((Include_in_loopSsc).and.(Include_in_loopFe).and.(Include_in_loopFv)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = MSsc(i1) 
ML2 = MFe(i2) 
ML3 = MFv(i3) 
coup1L = cplcFxeFeSscL(i2,i1)
coup1R = cplcFxeFeSscR(i2,i1)
coup2L = cplcFvFxvcSscL(i3,gt2,i1)
coup2R = cplcFvFxvcSscR(i3,gt2,i1)
coup3L = cplcFeFvcHpL(i2,i3)
coup3R = cplcFeFvcHpR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {VZ, bar[Fxe], bar[Fxv]}
If ((Include_in_loopVZ).and.(Include_in_loopFxe).and.(Include_in_loopFxv)) Then 
    Do i3=1,2
ML1 = MVZ 
ML2 = MFxe 
ML3 = MFxv(i3) 
coup1L = cplcFxeFxeVZL
coup1R = cplcFxeFxeVZR
coup2L = -cplcFxvFxvVZR(i3,gt2)
coup2R = -cplcFxvFxvVZL(i3,gt2)
coup3L = cplcFxeFxvcHpL(i3)
coup3R = cplcFxeFxvcHpR(i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
    End Do
End if 
  End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_FxeToFxvcHp


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxeToFxvcHp(MAh,MFe,MFv,MFxe,MFxv,           & 
& Mhh,MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,             & 
& MVWp2,MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhcHpVWp,cplcFxeFeSscL,cplcFxeFeSscR,        & 
& cplcFeFvcHpL,cplcFeFvcHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,     & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR, & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcHp,              & 
& cplhhcHpVWp,cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),          & 
& MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplAhcHpVWp,cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),& 
& cplcFeFvcHpL(3,3),cplcFeFvcHpR(3,3),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,         & 
& cplcFxeFxeVZR,cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),            & 
& cplcFxvFxvVZR(2,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),cplcFxeFxvcHpL(2),      & 
& cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplhhHpcHp(2),cplhhcHpVWp(2),  & 
& cplHpcHpVP,cplHpcHpVZ,cplcHpVPVWp,cplcHpVWpVZ

Complex(dp), Intent(out) :: Amp(2,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
  Do gt2=1,2
Amp(:,gt2) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFxe 
Mex2 = MFxv(gt2) 
Mex3 = MHp 


! {Fxe, VP, conj[Hp]}
If ((Include_in_loopFxe).and.(Include_in_loopVP).and.(Include_in_loopHp)) Then 
ML1 = MFxe 
ML2 = MVP 
ML3 = MHp 
coup1L = cplcFxeFxeVPL
coup1R = cplcFxeFxeVPR
coup2L = cplcFxeFxvcHpL(gt2)
coup2R = cplcFxeFxvcHpR(gt2)
coup3 = cplHpcHpVP
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, VP, conj[VWp]}
If ((Include_in_loopFxe).and.(Include_in_loopVP).and.(Include_in_loopVWp)) Then 
ML1 = MFxe 
ML2 = MVP 
ML3 = MVWp 
coup1L = cplcFxeFxeVPL
coup1R = cplcFxeFxeVPR
coup2L = cplcFxeFxvcVWpL(gt2)
coup2R = cplcFxeFxvcVWpR(gt2)
coup3 = cplcHpVPVWp
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End if 
  End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxeToFxvcHp


Subroutine Amplitude_Tree_SDdiracDM_FxeToFxvcVWp(cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,     & 
& MFxe,MFxv,MVWp,MFxe2,MFxv2,MVWp2,Amp)

Implicit None

Real(dp), Intent(in) :: MFxe,MFxv(2),MVWp,MFxe2,MFxv2(2),MVWp2

Complex(dp), Intent(in) :: cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2)

Complex(dp) :: Amp(4,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

  Do gt2=1,2
! External masses 
Mex1 = MFxe 
Mex2 = MFxv(gt2) 
Mex3 = MVWp 
! Tree-Level Vertex 
coupT1L = cplcFxeFxvcVWpL(gt2)
coupT1R = cplcFxeFxvcVWpR(gt2)
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,coupT1R,coupT1L,AmpC) 
! Colour and symmetry factor 
Amp(:,gt2) = -AmpC 
  End Do
End Subroutine Amplitude_Tree_SDdiracDM_FxeToFxvcVWp


Subroutine Gamma_Real_SDdiracDM_FxeToFxvcVWp(MLambda,em,gs,cplcFxeFxvcVWpL,           & 
& cplcFxeFxvcVWpR,MFxe,MFxv,MVWp,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2)

Real(dp), Intent(in) :: MFxe,MFxv(2),MVWp

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2), GammarealGluon(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
  Do i2=1,2
CoupL = cplcFxeFxvcVWpL(i2)
CoupR = cplcFxeFxvcVWpR(i2)
Mex1 = MFxe
Mex2 = MFxv(i2)
Mex3 = MVWp
If (Mex1.gt.(Mex2+Mex3)) Then 
  Call hardphotonFFW(Mex1,Mex2,Mex3,MLambda,-1._dp,0._dp,CoupL,CoupR,(0,1)*em,GammaRealPhoton(i2),kont)
  GammarealGluon(i2) = 0._dp 
Else 
  GammarealGluon(i2) = 0._dp 
  GammarealPhoton(i2) = 0._dp 

End if 
  End Do
End Subroutine Gamma_Real_SDdiracDM_FxeToFxvcVWp


Subroutine Amplitude_WAVE_SDdiracDM_FxeToFxvcVWp(cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,     & 
& ctcplcFxeFxvcVWpL,ctcplcFxeFxvcVWpR,MFxe,MFxe2,MFxv,MFxv2,MVWp,MVWp2,Zfed,             & 
& Zfeu,ZfVWp,ZfxVL,ZfxVR,Amp)

Implicit None

Real(dp), Intent(in) :: MFxe,MFxe2,MFxv(2),MFxv2(2),MVWp,MVWp2

Complex(dp), Intent(in) :: cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2)

Complex(dp), Intent(in) :: ctcplcFxeFxvcVWpL(2),ctcplcFxeFxvcVWpR(2)

Complex(dp), Intent(in) :: Zfed,Zfeu,ZfVWp,ZfxVL(2,2),ZfxVR(2,2)

Complex(dp), Intent(out) :: Amp(4,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

  Do gt2=1,2
! External masses 
Mex1 = MFxe 
Mex2 = MFxv(gt2) 
Mex3 = MVWp 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFxeFxvcVWpL(gt2) 
ZcoupT1R = ctcplcFxeFxvcVWpR(gt2)
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
ZcoupT1L = ZcoupT1L + 0.5_dp*Conjg(Zfed)*cplcFxeFxvcVWpL(gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*Zfeu*cplcFxeFxvcVWpR(gt2)


! External Field 2 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfxVL(i1,gt2)*cplcFxeFxvcVWpL(i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfxVR(i1,gt2))*cplcFxeFxvcVWpR(i1)
End Do


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVWp*cplcFxeFxvcVWpL(gt2)
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfVWp*cplcFxeFxvcVWpR(gt2)


! Getting the amplitude 
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,ZcoupT1R,ZcoupT1L,AmpC) 
Amp(:,gt2) = -AmpC 
  End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FxeToFxvcVWp


Subroutine Amplitude_VERTEX_SDdiracDM_FxeToFxvcVWp(MAh,MFe,MFv,MFxe,MFxv,             & 
& Mhh,MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVP2,             & 
& MVWp2,MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhHpcVWp,cplcFxeFeSscL,cplcFxeFeSscR,        & 
& cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,   & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR, & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcVWp,             & 
& cplhhcVWpVWp,cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),          & 
& MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplAhHpcVWp,cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),& 
& cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,       & 
& cplcFxeFxeVZR,cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),            & 
& cplcFxvFxvVZR(2,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),cplcFxeFxvcHpL(2),      & 
& cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplhhHpcVWp(2),cplhhcVWpVWp(2),& 
& cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ

Complex(dp), Intent(out) :: Amp(4,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
  Do gt2=1,2
Amp(:,gt2) = 0._dp 
! External masses 
Mex1 = MFxe 
Mex2 = MFxv(gt2) 
Mex3 = MVWp 


! {Fxe, VP, conj[Hp]}
If ((Include_in_loopFxe).and.(Include_in_loopVP).and.(Include_in_loopHp)) Then 
ML1 = MFxe 
ML2 = MVP 
ML3 = MHp 
coup1L = cplcFxeFxeVPL
coup1R = cplcFxeFxeVPR
coup2L = cplcFxeFxvcHpL(gt2)
coup2R = cplcFxeFxvcHpR(gt2)
coup3 = cplHpcVWpVP
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, VZ, conj[Hp]}
If ((Include_in_loopFxe).and.(Include_in_loopVZ).and.(Include_in_loopHp)) Then 
ML1 = MFxe 
ML2 = MVZ 
ML3 = MHp 
coup1L = cplcFxeFxeVZL
coup1R = cplcFxeFxeVZR
coup2L = cplcFxeFxvcHpL(gt2)
coup2R = cplcFxeFxvcHpR(gt2)
coup3 = cplHpcVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, VP, conj[VWp]}
If ((Include_in_loopFxe).and.(Include_in_loopVP).and.(Include_in_loopVWp)) Then 
ML1 = MFxe 
ML2 = MVP 
ML3 = MVWp 
coup1L = cplcFxeFxeVPL
coup1R = cplcFxeFxeVPR
coup2L = cplcFxeFxvcVWpL(gt2)
coup2R = cplcFxeFxvcVWpR(gt2)
coup3 = cplcVWpVPVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, VZ, conj[VWp]}
If ((Include_in_loopFxe).and.(Include_in_loopVZ).and.(Include_in_loopVWp)) Then 
ML1 = MFxe 
ML2 = MVZ 
ML3 = MVWp 
coup1L = cplcFxeFxeVZL
coup1R = cplcFxeFxeVZR
coup2L = cplcFxeFxvcVWpL(gt2)
coup2R = cplcFxeFxvcVWpR(gt2)
coup3 = -cplcVWpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxv, Hp, Ah}
If ((Include_in_loopFxv).and.(Include_in_loopHp).and.(Include_in_loopAh)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MHp 
ML3 = MAh 
coup1L = cplcFxeFxvcHpL(i1)
coup1R = cplcFxeFxvcHpR(i1)
coup2L = cplcFxvFxvAhL(i1,gt2)
coup2R = cplcFxvFxvAhR(i1,gt2)
coup3 = cplAhHpcVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, Hp, hh}
If ((Include_in_loopFxv).and.(Include_in_loopHp).and.(Include_in_loophh)) Then 
Do i1=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = MHp 
ML3 = Mhh(i3) 
coup1L = cplcFxeFxvcHpL(i1)
coup1R = cplcFxeFxvcHpR(i1)
coup2L = cplcFxvFxvhhL(i1,gt2,i3)
coup2R = cplcFxvFxvhhR(i1,gt2,i3)
coup3 = cplhhHpcVWp(i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
    End Do
End Do
End if 


! {Fxv, VWp, hh}
If ((Include_in_loopFxv).and.(Include_in_loopVWp).and.(Include_in_loophh)) Then 
Do i1=1,2
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = MVWp 
ML3 = Mhh(i3) 
coup1L = cplcFxeFxvcVWpL(i1)
coup1R = cplcFxeFxvcVWpR(i1)
coup2L = cplcFxvFxvhhL(i1,gt2,i3)
coup2R = cplcFxvFxvhhR(i1,gt2,i3)
coup3 = cplhhcVWpVWp(i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
    End Do
End Do
End if 


! {Fxv, Hp, VZ}
If ((Include_in_loopFxv).and.(Include_in_loopHp).and.(Include_in_loopVZ)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MHp 
ML3 = MVZ 
coup1L = cplcFxeFxvcHpL(i1)
coup1R = cplcFxeFxvcHpR(i1)
coup2L = cplcFxvFxvVZL(i1,gt2)
coup2R = cplcFxvFxvVZR(i1,gt2)
coup3 = cplHpcVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, VWp, VZ}
If ((Include_in_loopFxv).and.(Include_in_loopVWp).and.(Include_in_loopVZ)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MVWp 
ML3 = MVZ 
coup1L = cplcFxeFxvcVWpL(i1)
coup1R = cplcFxeFxvcVWpR(i1)
coup2L = cplcFxvFxvVZL(i1,gt2)
coup2R = cplcFxvFxvVZR(i1,gt2)
coup3 = cplcVWpVWpVZ
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Ssc, bar[Fe], bar[Fv]}
If ((Include_in_loopSsc).and.(Include_in_loopFe).and.(Include_in_loopFv)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = MSsc(i1) 
ML2 = MFe(i2) 
ML3 = MFv(i3) 
coup1L = cplcFxeFeSscL(i2,i1)
coup1R = cplcFxeFeSscR(i2,i1)
coup2L = cplcFvFxvcSscL(i3,gt2,i1)
coup2R = cplcFvFxvcSscR(i3,gt2,i1)
coup3L = cplcFeFvcVWpL(i2,i3)
coup3R = cplcFeFvcVWpR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {VZ, bar[Fxe], bar[Fxv]}
If ((Include_in_loopVZ).and.(Include_in_loopFxe).and.(Include_in_loopFxv)) Then 
    Do i3=1,2
ML1 = MVZ 
ML2 = MFxe 
ML3 = MFxv(i3) 
coup1L = cplcFxeFxeVZL
coup1R = cplcFxeFxeVZR
coup2L = cplcFxvFxvVZL(i3,gt2)
coup2R = cplcFxvFxvVZR(i3,gt2)
coup3L = cplcFxeFxvcVWpL(i3)
coup3R = cplcFxeFxvcVWpR(i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
    End Do
End if 
  End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_FxeToFxvcVWp


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxeToFxvcVWp(MAh,MFe,MFv,MFxe,               & 
& MFxv,Mhh,MHp,MSsc,MVP,MVWp,MVZ,MAh2,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,             & 
& MVP2,MVWp2,MVZ2,cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhHpcVWp,cplcFxeFeSscL,cplcFxeFeSscR,   & 
& cplcFeFvcVWpL,cplcFeFvcVWpR,cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR,   & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL,cplcFvFxvcSscR, & 
& cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,cplhhHpcVWp,             & 
& cplhhcVWpVWp,cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVP,MVWp,MVZ,MAh2,MFe2(3),          & 
& MFv2(3),MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplAhHpcVWp,cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),& 
& cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,       & 
& cplcFxeFxeVZR,cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),            & 
& cplcFxvFxvVZR(2,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),cplcFxeFxvcHpL(2),      & 
& cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),cplhhHpcVWp(2),cplhhcVWpVWp(2),& 
& cplHpcVWpVP,cplHpcVWpVZ,cplcVWpVPVWp,cplcVWpVWpVZ

Complex(dp), Intent(out) :: Amp(4,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
  Do gt2=1,2
Amp(:,gt2) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFxe 
Mex2 = MFxv(gt2) 
Mex3 = MVWp 


! {Fxe, VP, conj[Hp]}
If ((Include_in_loopFxe).and.(Include_in_loopVP).and.(Include_in_loopHp)) Then 
ML1 = MFxe 
ML2 = MVP 
ML3 = MHp 
coup1L = cplcFxeFxeVPL
coup1R = cplcFxeFxeVPR
coup2L = cplcFxeFxvcHpL(gt2)
coup2R = cplcFxeFxvcHpR(gt2)
coup3 = cplHpcVWpVP
Call Amp_VERTEX_FtoFV_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End if 


! {Fxe, VP, conj[VWp]}
If ((Include_in_loopFxe).and.(Include_in_loopVP).and.(Include_in_loopVWp)) Then 
ML1 = MFxe 
ML2 = MVP 
ML3 = MVWp 
coup1L = cplcFxeFxeVPL
coup1R = cplcFxeFxeVPR
coup2L = cplcFxeFxvcVWpL(gt2)
coup2R = cplcFxeFxvcVWpR(gt2)
coup3 = cplcVWpVPVWp
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt2) = Amp(:,gt2) + oo16pi2*(1)*AmpC 
End if 
  End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxeToFxvcVWp


Subroutine Amplitude_WAVE_SDdiracDM_FxeToFxeAh(MAh,MAh2,MFxe,MFxe2,ZfAh,              & 
& Zfed,Zfeu,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MAh2,MFxe,MFxe2

Complex(dp), Intent(in) :: ZfAh,Zfed,Zfeu

Complex(dp), Intent(out) :: Amp(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

! External masses 
Mex1 = MFxe 
Mex2 = MFxe 
Mex3 = MAh 
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
Amp(:) = 0._dp
End Subroutine Amplitude_WAVE_SDdiracDM_FxeToFxeAh


Subroutine Amplitude_VERTEX_SDdiracDM_FxeToFxeAh(MAh,MFe,MFxe,MFxv,MHp,               & 
& MSsc,MVWp,MAh2,MFe2,MFxe2,MFxv2,MHp2,MSsc2,MVWp2,cplcFeFeAhL,cplcFeFeAhR,              & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhHpcVWp,cplAhcHpVWp,cplcFxeFeSscL,cplcFxeFeSscR,       & 
& cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFeFxecSscL,              & 
& cplcFeFxecSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFxe,MFxv(2),MHp,MSsc(2),MVWp,MAh2,MFe2(3),MFxe2,MFxv2(2),MHp2,            & 
& MSsc2(2),MVWp2

Complex(dp), Intent(in) :: cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),              & 
& cplAhHpcVWp,cplAhcHpVWp,cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFxvFxeHpL(2),        & 
& cplcFxvFxeHpR(2),cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFeFxecSscL(3,2),              & 
& cplcFeFxecSscR(3,2),cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),            & 
& cplcFxeFxvcVWpR(2)

Complex(dp), Intent(out) :: Amp(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Amp(:) = 0._dp 
! External masses 
Mex1 = MFxe 
Mex2 = MFxe 
Mex3 = MAh 


! {Fxv, VWp, Hp}
If ((Include_in_loopFxv).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MVWp 
ML3 = MHp 
coup1L = cplcFxeFxvcVWpL(i1)
coup1R = cplcFxeFxvcVWpR(i1)
coup2L = cplcFxvFxeHpL(i1)
coup2R = cplcFxvFxeHpR(i1)
coup3 = -cplAhcHpVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, Hp, VWp}
If ((Include_in_loopFxv).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MHp 
ML3 = MVWp 
coup1L = cplcFxeFxvcHpL(i1)
coup1R = cplcFxeFxvcHpR(i1)
coup2L = cplcFxvFxeVWpL(i1)
coup2R = cplcFxvFxeVWpR(i1)
coup3 = -cplAhHpcVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Ssc, bar[Fe], bar[Fe]}
If ((Include_in_loopSsc).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = MSsc(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFxeFeSscL(i2,i1)
coup1R = cplcFxeFeSscR(i2,i1)
coup2L = cplcFeFxecSscL(i3,i1)
coup2R = cplcFeFxecSscR(i3,i1)
coup3L = cplcFeFeAhL(i2,i3)
coup3R = cplcFeFeAhR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {conj[Hp], bar[Fxv], bar[Fxv]}
If ((Include_in_loopHp).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
  Do i2=1,2
    Do i3=1,2
ML1 = MHp 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxeFxvcHpL(i2)
coup1R = cplcFxeFxvcHpR(i2)
coup2L = cplcFxvFxeHpL(i3)
coup2R = cplcFxvFxeHpR(i3)
coup3L = cplcFxvFxvAhL(i2,i3)
coup3R = cplcFxvFxvAhR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 


! {conj[VWp], bar[Fxv], bar[Fxv]}
If ((Include_in_loopVWp).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
  Do i2=1,2
    Do i3=1,2
ML1 = MVWp 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxeFxvcVWpL(i2)
coup1R = cplcFxeFxvcVWpR(i2)
coup2L = cplcFxvFxeVWpL(i3)
coup2R = cplcFxvFxeVWpR(i3)
coup3L = cplcFxvFxvAhL(i2,i3)
coup3R = cplcFxvFxvAhR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
End Subroutine Amplitude_VERTEX_SDdiracDM_FxeToFxeAh


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxeToFxeAh(MAh,MFe,MFxe,MFxv,MHp,            & 
& MSsc,MVWp,MAh2,MFe2,MFxe2,MFxv2,MHp2,MSsc2,MVWp2,cplcFeFeAhL,cplcFeFeAhR,              & 
& cplcFxvFxvAhL,cplcFxvFxvAhR,cplAhHpcVWp,cplAhcHpVWp,cplcFxeFeSscL,cplcFxeFeSscR,       & 
& cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFeFxecSscL,              & 
& cplcFeFxecSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFxe,MFxv(2),MHp,MSsc(2),MVWp,MAh2,MFe2(3),MFxe2,MFxv2(2),MHp2,            & 
& MSsc2(2),MVWp2

Complex(dp), Intent(in) :: cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),              & 
& cplAhHpcVWp,cplAhcHpVWp,cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFxvFxeHpL(2),        & 
& cplcFxvFxeHpR(2),cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),cplcFeFxecSscL(3,2),              & 
& cplcFeFxecSscR(3,2),cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),            & 
& cplcFxeFxvcVWpR(2)

Complex(dp), Intent(out) :: Amp(2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Amp(:) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFxe 
Mex2 = MFxe 
Mex3 = MAh 

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxeToFxeAh


Subroutine Amplitude_WAVE_SDdiracDM_FxeToFxehh(MFxe,MFxe2,Mhh,Mhh2,Zfed,              & 
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

    Do gt3=1,2
! External masses 
Mex1 = MFxe 
Mex2 = MFxe 
Mex3 = Mhh(gt3) 
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
Amp(:,gt3) = 0._dp
    End Do
End Subroutine Amplitude_WAVE_SDdiracDM_FxeToFxehh


Subroutine Amplitude_VERTEX_SDdiracDM_FxeToFxehh(MFe,MFxe,MFxv,Mhh,MHp,               & 
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
    Do gt3=1,2
Amp(:,gt3) = 0._dp 
! External masses 
Mex1 = MFxe 
Mex2 = MFxe 
Mex3 = Mhh(gt3) 


! {Fe, conj[Ssc], conj[Ssc]}
If ((Include_in_loopFe).and.(Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
Do i1=1,3
  Do i2=1,2
    Do i3=1,2
ML1 = MFe(i1) 
ML2 = MSsc(i2) 
ML3 = MSsc(i3) 
coup1L = cplcFxeFeSscL(i1,i2)
coup1R = cplcFxeFeSscR(i1,i2)
coup2L = cplcFeFxecSscL(i1,i3)
coup2R = cplcFeFxecSscR(i1,i3)
coup3 = cplhhSsccSsc(gt3,i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt3) = Amp(:,gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {Fxe, VZ, VZ}
If ((Include_in_loopFxe).and.(Include_in_loopVZ).and.(Include_in_loopVZ)) Then 
ML1 = MFxe 
ML2 = MVZ 
ML3 = MVZ 
coup1L = cplcFxeFxeVZL
coup1R = cplcFxeFxeVZR
coup2L = cplcFxeFxeVZL
coup2R = cplcFxeFxeVZR
coup3 = cplhhVZVZ(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt3) = Amp(:,gt3) + oo16pi2*(1)*AmpC 
End if 


! {Fxv, Hp, Hp}
If ((Include_in_loopFxv).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MHp 
ML3 = MHp 
coup1L = cplcFxeFxvcHpL(i1)
coup1R = cplcFxeFxvcHpR(i1)
coup2L = cplcFxvFxeHpL(i1)
coup2R = cplcFxvFxeHpR(i1)
coup3 = cplhhHpcHp(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt3) = Amp(:,gt3) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, VWp, Hp}
If ((Include_in_loopFxv).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MVWp 
ML3 = MHp 
coup1L = cplcFxeFxvcVWpL(i1)
coup1R = cplcFxeFxvcVWpR(i1)
coup2L = cplcFxvFxeHpL(i1)
coup2R = cplcFxvFxeHpR(i1)
coup3 = -cplhhcHpVWp(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt3) = Amp(:,gt3) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, Hp, VWp}
If ((Include_in_loopFxv).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MHp 
ML3 = MVWp 
coup1L = cplcFxeFxvcHpL(i1)
coup1R = cplcFxeFxvcHpR(i1)
coup2L = cplcFxvFxeVWpL(i1)
coup2R = cplcFxvFxeVWpR(i1)
coup3 = -cplhhHpcVWp(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt3) = Amp(:,gt3) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, VWp, VWp}
If ((Include_in_loopFxv).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MVWp 
ML3 = MVWp 
coup1L = cplcFxeFxvcVWpL(i1)
coup1R = cplcFxeFxvcVWpR(i1)
coup2L = cplcFxvFxeVWpL(i1)
coup2R = cplcFxvFxeVWpR(i1)
coup3 = cplhhcVWpVWp(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt3) = Amp(:,gt3) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Ssc, bar[Fe], bar[Fe]}
If ((Include_in_loopSsc).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = MSsc(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFxeFeSscL(i2,i1)
coup1R = cplcFxeFeSscR(i2,i1)
coup2L = cplcFeFxecSscL(i3,i1)
coup2R = cplcFeFxecSscR(i3,i1)
coup3L = cplcFeFehhL(i2,i3,gt3)
coup3R = cplcFeFehhR(i2,i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt3) = Amp(:,gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {conj[Hp], bar[Fxv], bar[Fxv]}
If ((Include_in_loopHp).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
  Do i2=1,2
    Do i3=1,2
ML1 = MHp 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxeFxvcHpL(i2)
coup1R = cplcFxeFxvcHpR(i2)
coup2L = cplcFxvFxeHpL(i3)
coup2R = cplcFxvFxeHpR(i3)
coup3L = cplcFxvFxvhhL(i2,i3,gt3)
coup3R = cplcFxvFxvhhR(i2,i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt3) = Amp(:,gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 


! {conj[VWp], bar[Fxv], bar[Fxv]}
If ((Include_in_loopVWp).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
  Do i2=1,2
    Do i3=1,2
ML1 = MVWp 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFxeFxvcVWpL(i2)
coup1R = cplcFxeFxvcVWpR(i2)
coup2L = cplcFxvFxeVWpL(i3)
coup2R = cplcFxvFxeVWpR(i3)
coup3L = cplcFxvFxvhhL(i2,i3,gt3)
coup3R = cplcFxvFxvhhR(i2,i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFS_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt3) = Amp(:,gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 
    End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_FxeToFxehh


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxeToFxehh(MFe,MFxe,MFxv,Mhh,MHp,            & 
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
    Do gt3=1,2
Amp(:,gt3) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFxe 
Mex2 = MFxe 
Mex3 = Mhh(gt3) 
    End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxeToFxehh


Subroutine Amplitude_WAVE_SDdiracDM_FxeToFxeVP(cplcFxeFxeVPL,cplcFxeFxeVPR,           & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,ctcplcFxeFxeVPL,ctcplcFxeFxeVPR,ctcplcFxeFxeVZL,           & 
& ctcplcFxeFxeVZR,MFxe,MFxe2,MVP,MVP2,Zfed,Zfeu,ZfVP,ZfVZVP,Amp)

Implicit None

Real(dp), Intent(in) :: MFxe,MFxe2,MVP,MVP2

Complex(dp), Intent(in) :: cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxeFxeVZL,cplcFxeFxeVZR

Complex(dp), Intent(in) :: ctcplcFxeFxeVPL,ctcplcFxeFxeVPR,ctcplcFxeFxeVZL,ctcplcFxeFxeVZR

Complex(dp), Intent(in) :: Zfed,Zfeu,ZfVP,ZfVZVP

Complex(dp), Intent(out) :: Amp(4) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

! External masses 
Mex1 = MFxe 
Mex2 = MFxe 
Mex3 = MVP 
ZcoupT1L = 0._dp 
ZcoupT1R = 0._dp 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
ZcoupT1L = ZcoupT1L + 0.5_dp*Conjg(Zfed)*cplcFxeFxeVPL
ZcoupT1R = ZcoupT1R + 0.5_dp*Zfeu*cplcFxeFxeVPR


! External Field 2 
ZcoupT1L = ZcoupT1L + 0.5_dp*Zfed*cplcFxeFxeVPL
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(Zfeu)*cplcFxeFxeVPR


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVZVP*cplcFxeFxeVZL
ZcoupT1R = ZcoupT1R + 0.5_dp*ZfVZVP*cplcFxeFxeVZR


! Getting the amplitude 
Call TreeAmp_FtoFV(Mex1,Mex2,Mex3,ZcoupT1R,ZcoupT1L,AmpC) 
Amp(:) = -AmpC 
End Subroutine Amplitude_WAVE_SDdiracDM_FxeToFxeVP


Subroutine Amplitude_VERTEX_SDdiracDM_FxeToFxeVP(MFe,MFxe,MFxv,MHp,MSsc,              & 
& MVP,MVWp,MVZ,MFe2,MFxe2,MFxv2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFxeFeSscL,cplcFxeFeSscR,  & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,       & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,              & 
& cplcFeFxecSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,          & 
& cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFxe,MFxv(2),MHp,MSsc(2),MVP,MVWp,MVZ,MFe2(3),MFxe2,MFxv2(2),MHp2,             & 
& MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),              & 
& cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL(2),       & 
& cplcFxvFxeVWpR(2),cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2), & 
& cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),             & 
& cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp

Complex(dp), Intent(out) :: Amp(4) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Amp(:) = 0._dp 
! External masses 
Mex1 = MFxe 
Mex2 = MFxe 
Mex3 = MVP 


! {Fxv, Hp, Hp}
If ((Include_in_loopFxv).and.(Include_in_loopHp).and.(Include_in_loopHp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MHp 
ML3 = MHp 
coup1L = cplcFxeFxvcHpL(i1)
coup1R = cplcFxeFxvcHpR(i1)
coup2L = cplcFxvFxeHpL(i1)
coup2R = cplcFxvFxeHpR(i1)
coup3 = -cplHpcHpVP
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, VWp, Hp}
If ((Include_in_loopFxv).and.(Include_in_loopVWp).and.(Include_in_loopHp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MVWp 
ML3 = MHp 
coup1L = cplcFxeFxvcVWpL(i1)
coup1R = cplcFxeFxvcVWpR(i1)
coup2L = cplcFxvFxeHpL(i1)
coup2R = cplcFxvFxeHpR(i1)
coup3 = cplcHpVPVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, Hp, VWp}
If ((Include_in_loopFxv).and.(Include_in_loopHp).and.(Include_in_loopVWp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MHp 
ML3 = MVWp 
coup1L = cplcFxeFxvcHpL(i1)
coup1R = cplcFxeFxvcHpR(i1)
coup2L = cplcFxvFxeVWpL(i1)
coup2R = cplcFxvFxeVWpR(i1)
coup3 = cplHpcVWpVP
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FSV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Fxv, VWp, VWp}
If ((Include_in_loopFxv).and.(Include_in_loopVWp).and.(Include_in_loopVWp)) Then 
Do i1=1,2
ML1 = MFxv(i1) 
ML2 = MVWp 
ML3 = MVWp 
coup1L = cplcFxeFxvcVWpL(i1)
coup1R = cplcFxeFxvcVWpR(i1)
coup2L = cplcFxvFxeVWpL(i1)
coup2R = cplcFxvFxeVWpR(i1)
coup3 = cplcVWpVPVWp
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_FVV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
End Do
End if 


! {Ssc, bar[Fe], bar[Fe]}
If ((Include_in_loopSsc).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = MSsc(i1) 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFxeFeSscL(i2,i1)
coup1R = cplcFxeFeSscR(i2,i1)
coup2L = cplcFeFxecSscL(i3,i1)
coup2R = cplcFeFxecSscR(i3,i1)
coup3L = cplcFeFeVPL(i2,i3)
coup3R = cplcFeFeVPR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_SFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {VP, bar[Fxe], bar[Fxe]}
If ((Include_in_loopVP).and.(Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
ML1 = MVP 
ML2 = MFxe 
ML3 = MFxe 
coup1L = cplcFxeFxeVPL
coup1R = cplcFxeFxeVPR
coup2L = cplcFxeFxeVPL
coup2R = cplcFxeFxeVPR
coup3L = cplcFxeFxeVPL
coup3R = cplcFxeFxeVPR
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
End if 


! {VZ, bar[Fxe], bar[Fxe]}
If ((Include_in_loopVZ).and.(Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
ML1 = MVZ 
ML2 = MFxe 
ML3 = MFxe 
coup1L = cplcFxeFxeVZL
coup1R = cplcFxeFxeVZR
coup2L = cplcFxeFxeVZL
coup2R = cplcFxeFxeVZR
coup3L = cplcFxeFxeVPL
coup3R = cplcFxeFxeVPR
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
End if 
End Subroutine Amplitude_VERTEX_SDdiracDM_FxeToFxeVP


Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxeToFxeVP(MFe,MFxe,MFxv,MHp,MSsc,           & 
& MVP,MVWp,MVZ,MFe2,MFxe2,MFxv2,MHp2,MSsc2,MVP2,MVWp2,MVZ2,cplcFxeFeSscL,cplcFxeFeSscR,  & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFxvFxeHpL,cplcFxvFxeHpR,cplcFxeFxeVPL,cplcFxeFxeVPR,       & 
& cplcFxvFxeVWpL,cplcFxvFxeVWpR,cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL,              & 
& cplcFeFxecSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,          & 
& cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFxe,MFxv(2),MHp,MSsc(2),MVP,MVWp,MVZ,MFe2(3),MFxe2,MFxv2(2),MHp2,             & 
& MSsc2(2),MVP2,MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),              & 
& cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFxvFxeVWpL(2),       & 
& cplcFxvFxeVWpR(2),cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2), & 
& cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),cplcFxeFxvcVWpR(2),             & 
& cplHpcHpVP,cplHpcVWpVP,cplcHpVPVWp,cplcVWpVPVWp

Complex(dp), Intent(out) :: Amp(4) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(4) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Amp(:) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MFxe 
Mex2 = MFxe 
Mex3 = MVP 


! {VP, bar[Fxe], bar[Fxe]}
If ((Include_in_loopVP).and.(Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
ML1 = MVP 
ML2 = MFxe 
ML3 = MFxe 
coup1L = cplcFxeFxeVPL
coup1R = cplcFxeFxeVPR
coup2L = cplcFxeFxeVPL
coup2R = cplcFxeFxeVPR
coup3L = cplcFxeFxeVPL
coup3R = cplcFxeFxeVPR
Call Amp_VERTEX_FtoFV_Topology1_VFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:) = Amp(:) + oo16pi2*(1)*AmpC 
End if 

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_FxeToFxeVP



End Module OneLoopDecay_Fxe_SDdiracDM
