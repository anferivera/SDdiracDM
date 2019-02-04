! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:21 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module OneLoopDecay_Ssc_SDdiracDM
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

Subroutine Amplitude_Tree_SDdiracDM_SscTocFeFxe(cplcFeFxecSscL,cplcFeFxecSscR,        & 
& MFe,MFxe,MSsc,MFe2,MFxe2,MSsc2,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFxe,MSsc(2),MFe2(3),MFxe2,MSsc2(2)

Complex(dp), Intent(in) :: cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2)

Complex(dp) :: Amp(2,2,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
  Do gt2=1,3
! External masses 
Mex1 = MSsc(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MFxe 
! Tree-Level Vertex 
coupT1L = cplcFeFxecSscL(gt2,gt1)
coupT1R = cplcFeFxecSscR(gt2,gt1)
Call TreeAmp_StoFF(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2) = AmpC 
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_SscTocFeFxe


Subroutine Gamma_Real_SDdiracDM_SscTocFeFxe(MLambda,em,gs,cplcFeFxecSscL,             & 
& cplcFeFxecSscR,MFe,MFxe,MSsc,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2)

Real(dp), Intent(in) :: MFe(3),MFxe,MSsc(2)

Real(dp), Intent(in) :: MLambda, em, gs 

Real(dp), Intent(out) :: GammarealPhoton(2,3), GammarealGluon(2,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3, kont 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 

Real(dp) :: Mloop1, Mloop2, Mloop3 
Complex(dp) :: CoupL, CoupR 
 
Do i1=1,2
  Do i2=1,3
CoupL = cplcFeFxecSscL(i2,i1)
CoupR = cplcFeFxecSscR(i2,i1)
Mex1 = MSsc(i1)
Mex2 = MFe(i2)
Mex3 = MFxe
If (Mex1.gt.(Mex2+Mex3)) Then 
 Call hardradiationSFF(Mex1,Mex2,Mex3,MLambda,em,0._dp,0._dp,0._dp,1._dp,-1._dp,1._dp,CoupL,CoupR,Gammarealphoton(i1,i2),kont)
  GammarealGluon(i1,i2) = 0._dp 
Else 
  GammarealGluon(i1,i2) = 0._dp 
  GammarealPhoton(i1,i2) = 0._dp 

End if 
  End Do
End Do
End Subroutine Gamma_Real_SDdiracDM_SscTocFeFxe


Subroutine Amplitude_WAVE_SDdiracDM_SscTocFeFxe(cplcFeFxecSscL,cplcFeFxecSscR,        & 
& ctcplcFeFxecSscL,ctcplcFeFxecSscR,MFe,MFe2,MFxe,MFxe2,MSsc,MSsc2,Zfed,ZfEL,            & 
& ZfER,Zfeu,ZfSsc,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFe2(3),MFxe,MFxe2,MSsc(2),MSsc2(2)

Complex(dp), Intent(in) :: cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2)

Complex(dp), Intent(in) :: ctcplcFeFxecSscL(3,2),ctcplcFeFxecSscR(3,2)

Complex(dp), Intent(in) :: Zfed,ZfEL(3,3),ZfER(3,3),Zfeu,ZfSsc(2,2)

Complex(dp), Intent(out) :: Amp(2,2,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 
Complex(dp) :: TcoupT1, TcoupT1L, TcoupT1R 
Complex(dp) :: ZcoupT1, ZcoupT1L, ZcoupT1R 

Do gt1=1,2
  Do gt2=1,3
! External masses 
Mex1 = MSsc(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MFxe 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFeFxecSscL(gt2,gt1) 
ZcoupT1R = ctcplcFeFxecSscR(gt2,gt1) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*Conjg(ZfSsc(i1,gt1))*cplcFeFxecSscL(gt2,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfSsc(i1,gt1))*cplcFeFxecSscR(gt2,i1)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfER(i1,gt2)*cplcFeFxecSscL(i1,gt1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfEL(i1,gt2))*cplcFeFxecSscR(i1,gt1)
End Do


! External Field 3 
ZcoupT1L = ZcoupT1L + 0.5_dp*Zfed*cplcFeFxecSscL(gt2,gt1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(Zfeu)*cplcFeFxecSscR(gt2,gt1)


! Getting the amplitude 
Call TreeAmp_StoFF(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt1, gt2) = AmpC 
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_SscTocFeFxe


Subroutine Amplitude_VERTEX_SDdiracDM_SscTocFeFxe(MFe,MFv,MFxe,MFxv,Mhh,              & 
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

Complex(dp), Intent(out) :: Amp(2,2,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,3
Amp(:,gt1, gt2) = 0._dp 
! External masses 
Mex1 = MSsc(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MFxe 


! {hh, conj[Ssc], Fe}
If ((Include_in_loophh).and.(Include_in_loopSsc).and.(Include_in_loopFe)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,3
ML1 = Mhh(i1) 
ML2 = MSsc(i2) 
ML3 = MFe(i3) 
coup1 = cplhhSsccSsc(i1,i2,gt1)
coup2L = cplcFeFehhL(gt2,i3,i1)
coup2R = cplcFeFehhR(gt2,i3,i1)
coup3L = cplcFeFxecSscL(i3,i2)
coup3R = cplcFeFxecSscR(i3,i2)
If ((Abs(coup1))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_SSF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {bar[Fe], bar[Fxe], VP}
If ((Include_in_loopFe).and.(Include_in_loopFxe).and.(Include_in_loopVP)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MFxe 
ML3 = MVP 
coup1L = cplcFeFxecSscL(i1,gt1)
coup1R = cplcFeFxecSscR(i1,gt1)
coup2L = cplcFeFeVPL(gt2,i1)
coup2R = cplcFeFeVPR(gt2,i1)
coup3L = cplcFxeFxeVPL
coup3R = cplcFxeFxeVPR
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {bar[Fe], bar[Fxe], VZ}
If ((Include_in_loopFe).and.(Include_in_loopFxe).and.(Include_in_loopVZ)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MFxe 
ML3 = MVZ 
coup1L = cplcFeFxecSscL(i1,gt1)
coup1R = cplcFeFxecSscR(i1,gt1)
coup2L = cplcFeFeVZL(gt2,i1)
coup2R = cplcFeFeVZR(gt2,i1)
coup3L = cplcFxeFxeVZL
coup3R = cplcFxeFxeVZR
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {bar[Fv], bar[Fxv], conj[Hp]}
If ((Include_in_loopFv).and.(Include_in_loopFxv).and.(Include_in_loopHp)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFv(i1) 
ML2 = MFxv(i2) 
ML3 = MHp 
coup1L = cplcFvFxvcSscL(i1,i2,gt1)
coup1R = cplcFvFxvcSscR(i1,i2,gt1)
coup2L = cplcFeFvcHpL(gt2,i1)
coup2R = cplcFeFvcHpR(gt2,i1)
coup3L = cplcFxvFxeHpL(i2)
coup3R = cplcFxvFxeHpR(i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
  End Do
End Do
End if 


! {bar[Fv], bar[Fxv], conj[VWp]}
If ((Include_in_loopFv).and.(Include_in_loopFxv).and.(Include_in_loopVWp)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFv(i1) 
ML2 = MFxv(i2) 
ML3 = MVWp 
coup1L = cplcFvFxvcSscL(i1,i2,gt1)
coup1R = cplcFvFxvcSscR(i1,i2,gt1)
coup2L = cplcFeFvcVWpL(gt2,i1)
coup2R = cplcFeFvcVWpR(gt2,i1)
coup3L = cplcFxvFxeVWpL(i2)
coup3R = cplcFxvFxeVWpR(i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
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
End Subroutine Amplitude_VERTEX_SDdiracDM_SscTocFeFxe


Subroutine Amplitude_IR_VERTEX_SDdiracDM_SscTocFeFxe(MFe,MFv,MFxe,MFxv,               & 
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

Complex(dp), Intent(out) :: Amp(2,2,3) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Real(dp) :: ML1, ML2, ML3 
Complex(dp) :: coupT1, coupT1L, coupT1R, coup1, coup1L, coup1R 
Complex(dp) :: coup2, coup2L, coup2R, coup3, coup3L, coup3R, coup2a,coup2b,coup2c 
Do gt1=1,2
  Do gt2=1,3
Amp(:,gt1, gt2) = 0._dp 
IRdivOnly =.true. 
! External masses 
Mex1 = MSsc(gt1) 
Mex2 = MFe(gt2) 
Mex3 = MFxe 


! {bar[Fe], bar[Fxe], VP}
If ((Include_in_loopFe).and.(Include_in_loopFxe).and.(Include_in_loopVP)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MFxe 
ML3 = MVP 
coup1L = cplcFeFxecSscL(i1,gt1)
coup1R = cplcFeFxecSscR(i1,gt1)
coup2L = cplcFeFeVPL(gt2,i1)
coup2R = cplcFeFeVPR(gt2,i1)
coup3L = cplcFxeFxeVPL
coup3R = cplcFxeFxeVPR
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_SscTocFeFxe


Subroutine Amplitude_Tree_SDdiracDM_SscTocFvFxv(cplcFvFxvcSscL,cplcFvFxvcSscR,        & 
& MFv,MFxv,MSsc,MFv2,MFxv2,MSsc2,Amp)

Implicit None

Real(dp), Intent(in) :: MFv(3),MFxv(2),MSsc(2),MFv2(3),MFxv2(2),MSsc2(2)

Complex(dp), Intent(in) :: cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2)

Complex(dp) :: Amp(2,2,3,2) 

Integer :: i1, i2, i3, gt1, gt2, gt3 
Complex(dp) :: AmpC(2) 
Real(dp) :: Mex1, Mex2, Mex3, ExtRMsq 
Complex(dp) :: coupT1, coupT1L, coupT1R 

Do gt1=1,2
  Do gt2=1,3
    Do gt3=1,2
! External masses 
Mex1 = MSsc(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MFxv(gt3) 
! Tree-Level Vertex 
coupT1L = cplcFvFxvcSscL(gt2,gt3,gt1)
coupT1R = cplcFvFxvcSscR(gt2,gt3,gt1)
Call TreeAmp_StoFF(Mex1,Mex2,Mex3,coupT1L,coupT1R,AmpC) 
! Colour and symmetry factor 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_SscTocFvFxv


Subroutine Gamma_Real_SDdiracDM_SscTocFvFxv(MLambda,em,gs,cplcFvFxvcSscL,             & 
& cplcFvFxvcSscR,MFv,MFxv,MSsc,GammarealPhoton,GammarealGluon)

Implicit None

Complex(dp), Intent(in) :: cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2)

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
CoupL = cplcFvFxvcSscL(i2,i3,i1)
CoupR = cplcFvFxvcSscR(i2,i3,i1)
Mex1 = MSsc(i1)
Mex2 = MFv(i2)
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
End Subroutine Gamma_Real_SDdiracDM_SscTocFvFxv


Subroutine Amplitude_WAVE_SDdiracDM_SscTocFvFxv(cplcFvFxvcSscL,cplcFvFxvcSscR,        & 
& ctcplcFvFxvcSscL,ctcplcFvFxvcSscR,MFv,MFv2,MFxv,MFxv2,MSsc,MSsc2,ZfSsc,ZfVL,           & 
& ZfVR,ZfxVL,ZfxVR,Amp)

Implicit None

Real(dp), Intent(in) :: MFv(3),MFv2(3),MFxv(2),MFxv2(2),MSsc(2),MSsc2(2)

Complex(dp), Intent(in) :: cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2)

Complex(dp), Intent(in) :: ctcplcFvFxvcSscL(3,2,2),ctcplcFvFxvcSscR(3,2,2)

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
Mex1 = MSsc(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MFxv(gt3) 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1L = ctcplcFvFxvcSscL(gt2,gt3,gt1) 
ZcoupT1R = ctcplcFvFxvcSscR(gt2,gt3,gt1) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*Conjg(ZfSsc(i1,gt1))*cplcFvFxvcSscL(gt2,gt3,i1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfSsc(i1,gt1))*cplcFvFxvcSscR(gt2,gt3,i1)
End Do


! External Field 2 
Do i1=1,3
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfVR(i1,gt2)*cplcFvFxvcSscL(i1,gt3,gt1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfVL(i1,gt2))*cplcFvFxvcSscR(i1,gt3,gt1)
End Do


! External Field 3 
Do i1=1,2
ZcoupT1L = ZcoupT1L + 0.5_dp*ZfxVL(i1,gt3)*cplcFvFxvcSscL(gt2,i1,gt1)
ZcoupT1R = ZcoupT1R + 0.5_dp*Conjg(ZfxVR(i1,gt3))*cplcFvFxvcSscR(gt2,i1,gt1)
End Do


! Getting the amplitude 
Call TreeAmp_StoFF(Mex1,Mex2,Mex3,ZcoupT1L,ZcoupT1R,AmpC) 
Amp(:,gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_SscTocFvFxv


Subroutine Amplitude_VERTEX_SDdiracDM_SscTocFvFxv(MFe,MFv,MFxe,MFxv,Mhh,              & 
& MHp,MSsc,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFvFeHpL,        & 
& cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFxecSscL,          & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL, & 
& cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,          & 
& cplhhSsccSsc,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVWp,MVZ,MFe2(3),MFv2(3),               & 
& MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFvFvVZL(3,3),& 
& cplcFvFvVZR(3,3),cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFxvFxvhhL(2,2,2),         & 
& cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),cplcFvFxvcSscL(3,2,2),      & 
& cplcFvFxvcSscR(3,2,2),cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),          & 
& cplcFxeFxvcVWpR(2),cplhhSsccSsc(2,2,2)

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
Mex1 = MSsc(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MFxv(gt3) 


! {Ssc, hh, Fxv}
If ((Include_in_loopSsc).and.(Include_in_loophh).and.(Include_in_loopFxv)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = MSsc(i1) 
ML2 = Mhh(i2) 
ML3 = MFxv(i3) 
coup1 = cplhhSsccSsc(i2,i1,gt1)
coup2L = cplcFvFxvcSscL(gt2,i3,i1)
coup2R = cplcFvFxvcSscR(gt2,i3,i1)
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


! {bar[Fe], bar[Fxe], Hp}
If ((Include_in_loopFe).and.(Include_in_loopFxe).and.(Include_in_loopHp)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MFxe 
ML3 = MHp 
coup1L = cplcFeFxecSscL(i1,gt1)
coup1R = cplcFeFxecSscR(i1,gt1)
coup2L = cplcFvFeHpL(gt2,i1)
coup2R = cplcFvFeHpR(gt2,i1)
coup3L = cplcFxeFxvcHpL(gt3)
coup3R = cplcFxeFxvcHpR(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFS(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End Do
End if 


! {bar[Fe], bar[Fxe], VWp}
If ((Include_in_loopFe).and.(Include_in_loopFxe).and.(Include_in_loopVWp)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MFxe 
ML3 = MVWp 
coup1L = cplcFeFxecSscL(i1,gt1)
coup1R = cplcFeFxecSscR(i1,gt1)
coup2L = cplcFvFeVWpL(gt2,i1)
coup2R = cplcFvFeVWpR(gt2,i1)
coup3L = cplcFxeFxvcVWpL(gt3)
coup3R = cplcFxeFxvcVWpR(gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoFF_Topology1_FFV(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2, gt3) = Amp(:,gt1, gt2, gt3) + oo16pi2*(1)*AmpC 
End Do
End if 


! {bar[Fv], bar[Fxv], VZ}
If ((Include_in_loopFv).and.(Include_in_loopFxv).and.(Include_in_loopVZ)) Then 
Do i1=1,3
  Do i2=1,2
ML1 = MFv(i1) 
ML2 = MFxv(i2) 
ML3 = MVZ 
coup1L = cplcFvFxvcSscL(i1,i2,gt1)
coup1R = cplcFvFxvcSscR(i1,i2,gt1)
coup2L = cplcFvFvVZL(gt2,i1)
coup2R = cplcFvFvVZR(gt2,i1)
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
    End Do
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_SscTocFvFxv


Subroutine Amplitude_IR_VERTEX_SDdiracDM_SscTocFvFxv(MFe,MFv,MFxe,MFxv,               & 
& Mhh,MHp,MSsc,MVWp,MVZ,MFe2,MFv2,MFxe2,MFxv2,Mhh2,MHp2,MSsc2,MVWp2,MVZ2,cplcFvFeHpL,    & 
& cplcFvFeHpR,cplcFvFeVWpL,cplcFvFeVWpR,cplcFvFvVZL,cplcFvFvVZR,cplcFeFxecSscL,          & 
& cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,cplcFvFxvcSscL, & 
& cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,          & 
& cplhhSsccSsc,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFv(3),MFxe,MFxv(2),Mhh(2),MHp,MSsc(2),MVWp,MVZ,MFe2(3),MFv2(3),               & 
& MFxe2,MFxv2(2),Mhh2(2),MHp2,MSsc2(2),MVWp2,MVZ2

Complex(dp), Intent(in) :: cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),cplcFvFeVWpL(3,3),cplcFvFeVWpR(3,3),cplcFvFvVZL(3,3),& 
& cplcFvFvVZR(3,3),cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFxvFxvhhL(2,2,2),         & 
& cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2),cplcFvFxvcSscL(3,2,2),      & 
& cplcFvFxvcSscR(3,2,2),cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFxeFxvcVWpL(2),          & 
& cplcFxeFxvcVWpR(2),cplhhSsccSsc(2,2,2)

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
Mex1 = MSsc(gt1) 
Mex2 = MFv(gt2) 
Mex3 = MFxv(gt3) 
    End Do
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_SscTocFvFxv


Subroutine Amplitude_Tree_SDdiracDM_SscToSschh(cplhhSsccSsc,Mhh,MSsc,Mhh2,            & 
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
Mex1 = MSsc(gt1) 
Mex2 = MSsc(gt2) 
Mex3 = Mhh(gt3) 
! Tree-Level Vertex 
coupT1 = cplhhSsccSsc(gt3,gt2,gt1)
Call TreeAmp_StoSS(Mex1,Mex2,Mex3,coupT1,AmpC) 
! Colour and symmetry factor 
Amp(gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_Tree_SDdiracDM_SscToSschh


Subroutine Gamma_Real_SDdiracDM_SscToSschh(MLambda,em,gs,cplhhSsccSsc,Mhh,            & 
& MSsc,GammarealPhoton,GammarealGluon)

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
Coup = cplhhSsccSsc(i3,i2,i1)
Mex1 = MSsc(i1)
Mex2 = MSsc(i2)
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
End Subroutine Gamma_Real_SDdiracDM_SscToSschh


Subroutine Amplitude_WAVE_SDdiracDM_SscToSschh(cplhhSsccSsc,ctcplhhSsccSsc,           & 
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
Mex1 = MSsc(gt1) 
Mex2 = MSsc(gt2) 
Mex3 = Mhh(gt3) 
!----------------------------- 
! Coupling counter term 
!----------------------------- 
ZcoupT1 = ctcplhhSsccSsc(gt3,gt2,gt1) 
!----------------------------- 
! Multiply Z-factors 
!----------------------------- 
! External Field 1 
Do i1=1,2
ZcoupT1 = ZcoupT1 + 0.5_dp*Conjg(ZfSsc(i1,gt1))*cplhhSsccSsc(gt3,gt2,i1)
End Do


! External Field 2 
Do i1=1,2
ZcoupT1 = ZcoupT1 + 0.5_dp*ZfSsc(i1,gt2)*cplhhSsccSsc(gt3,i1,gt1)
End Do


! External Field 3 
Do i1=1,2
ZcoupT1 = ZcoupT1 + 0.5_dp*Zfhh(i1,gt3)*cplhhSsccSsc(i1,gt2,gt1)
End Do


! Getting the amplitude 
Call TreeAmp_StoSS(Mex1,Mex2,Mex3,ZcoupT1,AmpC) 
Amp(gt1, gt2, gt3) = AmpC 
    End Do
  End Do
End Do
End Subroutine Amplitude_WAVE_SDdiracDM_SscToSschh


Subroutine Amplitude_VERTEX_SDdiracDM_SscToSschh(MAh,MFe,MFv,MFxe,MFxv,               & 
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
Mex1 = MSsc(gt1) 
Mex2 = MSsc(gt2) 
Mex3 = Mhh(gt3) 


! {Fxe, Fe, Fe}
If ((Include_in_loopFxe).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MFxe 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFxecSscL(i2,gt1)
coup1R = cplcFeFxecSscR(i2,gt1)
coup2L = cplcFxeFeSscL(i3,gt2)
coup2R = cplcFxeFeSscR(i3,gt2)
coup3L = cplcFeFehhL(i3,i2,gt3)
coup3R = cplcFeFehhR(i3,i2,gt3)
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


! {hh, conj[Ssc], conj[Ssc]}
If ((Include_in_loophh).and.(Include_in_loopSsc).and.(Include_in_loopSsc)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = Mhh(i1) 
ML2 = MSsc(i2) 
ML3 = MSsc(i3) 
coup1 = cplhhSsccSsc(i1,i2,gt1)
coup2 = cplhhSsccSsc(i1,gt2,i3)
coup3 = cplhhSsccSsc(gt3,i3,i2)
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


! {Ssc, hh, hh}
If ((Include_in_loopSsc).and.(Include_in_loophh).and.(Include_in_loophh)) Then 
Do i1=1,2
  Do i2=1,2
    Do i3=1,2
ML1 = MSsc(i1) 
ML2 = Mhh(i2) 
ML3 = Mhh(i3) 
coup1 = cplhhSsccSsc(i2,i1,gt1)
coup2 = cplhhSsccSsc(i3,gt2,i1)
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


! {bar[Fv], bar[Fxv], bar[Fxv]}
If ((Include_in_loopFv).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
Do i1=1,3
  Do i2=1,2
    Do i3=1,2
ML1 = MFv(i1) 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFvFxvcSscL(i1,i2,gt1)
coup1R = cplcFvFxvcSscR(i1,i2,gt1)
coup2L = cplcFxvFvSscL(i3,i1,gt2)
coup2R = cplcFxvFvSscR(i3,i1,gt2)
coup3L = cplcFxvFxvhhL(i2,i3,gt3)
coup3R = cplcFxvFxvhhR(i2,i3,gt3)
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


! {hh, conj[Ssc]}
If ((Include_in_loophh).and.(Include_in_loopSsc)) Then 
Do i1=1,2
  Do i2=1,2
ML1 = Mhh(i1) 
ML2 = MSsc(i2) 
coup1 = cplhhSsccSsc(i1,i2,gt1)
coup2 = cplhhhhSsccSsc1(gt3,i1,gt2,i2)
Call Amp_VERTEX_StoSS_Topology2_SS(Mex1,Mex2,Mex3,ML1,ML2,coup1,coup2,AmpC) 
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
coup1 = cplhhhhSsccSsc1(gt3,i1,i2,gt1)
coup2 = cplhhSsccSsc(i1,gt2,i2)
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


! {Ah, Ah}
If ((Include_in_loopAh).and.(Include_in_loopAh)) Then 
ML1 = MAh 
ML2 = MAh 
coup1 = cplAhAhSsccSsc1(gt2,gt1)
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
coup1 = cplhhhhSsccSsc1(i1,i2,gt2,gt1)
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
coup1 = cplHpSsccHpcSsc1(gt2,gt1)
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
coup1 = cplSscSsccSsccSsc1(gt2,i1,gt1,i2)
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
    End Do
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_SscToSschh


Subroutine Amplitude_IR_VERTEX_SDdiracDM_SscToSschh(MAh,MFe,MFv,MFxe,MFxv,            & 
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
Mex1 = MSsc(gt1) 
Mex2 = MSsc(gt2) 
Mex3 = Mhh(gt3) 
    End Do
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_SscToSschh


Subroutine Amplitude_WAVE_SDdiracDM_SscToAhSsc(MAh,MAh2,MSsc,MSsc2,ZfAh,              & 
& ZfSsc,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MAh2,MSsc(2),MSsc2(2)

Complex(dp), Intent(in) :: ZfAh,ZfSsc(2,2)

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
Mex1 = MSsc(gt1) 
Mex2 = MAh 
Mex3 = MSsc(gt3) 
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
End Subroutine Amplitude_WAVE_SDdiracDM_SscToAhSsc


Subroutine Amplitude_VERTEX_SDdiracDM_SscToAhSsc(MAh,MFe,MFv,MFxe,MFxv,               & 
& MSsc,MAh2,MFe2,MFv2,MFxe2,MFxv2,MSsc2,cplcFeFeAhL,cplcFeFeAhR,cplcFxvFxvAhL,           & 
& cplcFxvFxvAhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFeFxecSscL,  & 
& cplcFeFxecSscR,cplcFvFxvcSscL,cplcFvFxvcSscR,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,MFxv(2),MSsc(2),MAh2,MFe2(3),MFv2(3),MFxe2,MFxv2(2),MSsc2(2)

Complex(dp), Intent(in) :: cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),              & 
& cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),       & 
& cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2)

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
Mex1 = MSsc(gt1) 
Mex2 = MAh 
Mex3 = MSsc(gt3) 


! {Fxv, Fv, Fxv}
If ((Include_in_loopFxv).and.(Include_in_loopFv).and.(Include_in_loopFxv)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,2
ML1 = MFxv(i1) 
ML2 = MFv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFvFxvcSscL(i2,i1,gt1)
coup1R = cplcFvFxvcSscR(i2,i1,gt1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFvSscL(i3,i2,gt3)
coup3R = cplcFxvFvSscR(i3,i2,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt3) = Amp(gt1, gt3) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {bar[Fe], bar[Fxe], bar[Fe]}
If ((Include_in_loopFe).and.(Include_in_loopFxe).and.(Include_in_loopFe)) Then 
Do i1=1,3
    Do i3=1,3
ML1 = MFe(i1) 
ML2 = MFxe 
ML3 = MFe(i3) 
coup1L = cplcFeFxecSscL(i1,gt1)
coup1R = cplcFeFxecSscR(i1,gt1)
coup2L = cplcFeFeAhL(i3,i1)
coup2R = cplcFeFeAhR(i3,i1)
coup3L = cplcFxeFeSscL(i3,gt3)
coup3R = cplcFxeFeSscR(i3,gt3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSS_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(gt1, gt3) = Amp(gt1, gt3) + oo16pi2*(1)*AmpC 
    End Do
End Do
End if 
    End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_SscToAhSsc


Subroutine Amplitude_IR_VERTEX_SDdiracDM_SscToAhSsc(MAh,MFe,MFv,MFxe,MFxv,            & 
& MSsc,MAh2,MFe2,MFv2,MFxe2,MFxv2,MSsc2,cplcFeFeAhL,cplcFeFeAhR,cplcFxvFxvAhL,           & 
& cplcFxvFxvAhR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFeFxecSscL,  & 
& cplcFeFxecSscR,cplcFvFxvcSscL,cplcFvFxvcSscR,Amp)

Implicit None

Real(dp), Intent(in) :: MAh,MFe(3),MFv(3),MFxe,MFxv(2),MSsc(2),MAh2,MFe2(3),MFv2(3),MFxe2,MFxv2(2),MSsc2(2)

Complex(dp), Intent(in) :: cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),              & 
& cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),       & 
& cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2)

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
Mex1 = MSsc(gt1) 
Mex2 = MAh 
Mex3 = MSsc(gt3) 
    End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_SscToAhSsc


Subroutine Amplitude_WAVE_SDdiracDM_SscToSscVP(MSsc,MSsc2,MVP,MVP2,ZfSsc,ZfVP,Amp)

Implicit None

Real(dp), Intent(in) :: MSsc(2),MSsc2(2),MVP,MVP2

Complex(dp), Intent(in) :: ZfSsc(2,2),ZfVP

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
Mex1 = MSsc(gt1) 
Mex2 = MSsc(gt2) 
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
End Subroutine Amplitude_WAVE_SDdiracDM_SscToSscVP


Subroutine Amplitude_VERTEX_SDdiracDM_SscToSscVP(MFe,MFxe,MSsc,MVP,MFe2,              & 
& MFxe2,MSsc2,MVP2,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFxeFxeVPL,    & 
& cplcFxeFxeVPR,cplcFeFxecSscL,cplcFeFxecSscR,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFxe,MSsc(2),MVP,MFe2(3),MFxe2,MSsc2(2),MVP2

Complex(dp), Intent(in) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),              & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2)

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
Mex1 = MSsc(gt1) 
Mex2 = MSsc(gt2) 
Mex3 = MVP 


! {Fxe, Fe, Fe}
If ((Include_in_loopFxe).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MFxe 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFxecSscL(i2,gt1)
coup1R = cplcFeFxecSscR(i2,gt1)
coup2L = cplcFxeFeSscL(i3,gt2)
coup2R = cplcFxeFeSscR(i3,gt2)
coup3L = -cplcFeFeVPR(i3,i2)
coup3R = -cplcFeFeVPL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 


! {bar[Fe], bar[Fxe], bar[Fxe]}
If ((Include_in_loopFe).and.(Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MFxe 
ML3 = MFxe 
coup1L = cplcFeFxecSscL(i1,gt1)
coup1R = cplcFeFxecSscR(i1,gt1)
coup2L = cplcFxeFeSscL(i1,gt2)
coup2R = cplcFxeFeSscR(i1,gt2)
coup3L = cplcFxeFxeVPL
coup3R = cplcFxeFxeVPR
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_SscToSscVP


Subroutine Amplitude_IR_VERTEX_SDdiracDM_SscToSscVP(MFe,MFxe,MSsc,MVP,MFe2,           & 
& MFxe2,MSsc2,MVP2,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVPL,cplcFeFeVPR,cplcFxeFxeVPL,    & 
& cplcFxeFxeVPR,cplcFeFxecSscL,cplcFeFxecSscR,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFxe,MSsc(2),MVP,MFe2(3),MFxe2,MSsc2(2),MVP2

Complex(dp), Intent(in) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),              & 
& cplcFxeFxeVPL,cplcFxeFxeVPR,cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2)

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
Mex1 = MSsc(gt1) 
Mex2 = MSsc(gt2) 
Mex3 = MVP 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_SscToSscVP


Subroutine Amplitude_WAVE_SDdiracDM_SscToSscVZ(MSsc,MSsc2,MVZ,MVZ2,ZfSsc,ZfVZ,Amp)

Implicit None

Real(dp), Intent(in) :: MSsc(2),MSsc2(2),MVZ,MVZ2

Complex(dp), Intent(in) :: ZfSsc(2,2),ZfVZ

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
Mex1 = MSsc(gt1) 
Mex2 = MSsc(gt2) 
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
End Subroutine Amplitude_WAVE_SDdiracDM_SscToSscVZ


Subroutine Amplitude_VERTEX_SDdiracDM_SscToSscVZ(MFe,MFv,MFxe,MFxv,MSsc,              & 
& MVZ,MFe2,MFv2,MFxe2,MFxv2,MSsc2,MVZ2,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVZL,          & 
& cplcFeFeVZR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,cplcFxeFxeVZL,         & 
& cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvVZL,cplcFxvFxvVZR,               & 
& cplcFvFxvcSscL,cplcFvFxvcSscR,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFv(3),MFxe,MFxv(2),MSsc(2),MVZ,MFe2(3),MFv2(3),MFxe2,MFxv2(2),MSsc2(2),MVZ2

Complex(dp), Intent(in) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),              & 
& cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),           & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFxvFxvVZL(2,2),& 
& cplcFxvFxvVZR(2,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2)

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
Mex1 = MSsc(gt1) 
Mex2 = MSsc(gt2) 
Mex3 = MVZ 


! {Fxe, Fe, Fe}
If ((Include_in_loopFxe).and.(Include_in_loopFe).and.(Include_in_loopFe)) Then 
  Do i2=1,3
    Do i3=1,3
ML1 = MFxe 
ML2 = MFe(i2) 
ML3 = MFe(i3) 
coup1L = cplcFeFxecSscL(i2,gt1)
coup1R = cplcFeFxecSscR(i2,gt1)
coup2L = cplcFxeFeSscL(i3,gt2)
coup2R = cplcFxeFeSscR(i3,gt2)
coup3L = -cplcFeFeVZR(i3,i2)
coup3R = -cplcFeFeVZL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End if 


! {Fxv, Fv, Fv}
If ((Include_in_loopFxv).and.(Include_in_loopFv).and.(Include_in_loopFv)) Then 
Do i1=1,2
  Do i2=1,3
    Do i3=1,3
ML1 = MFxv(i1) 
ML2 = MFv(i2) 
ML3 = MFv(i3) 
coup1L = cplcFvFxvcSscL(i2,i1,gt1)
coup1R = cplcFvFxvcSscR(i2,i1,gt1)
coup2L = cplcFxvFvSscL(i1,i3,gt2)
coup2R = cplcFxvFvSscR(i1,i3,gt2)
coup3L = -cplcFvFvVZR(i3,i2)
coup3R = -cplcFvFvVZL(i3,i2)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 


! {bar[Fe], bar[Fxe], bar[Fxe]}
If ((Include_in_loopFe).and.(Include_in_loopFxe).and.(Include_in_loopFxe)) Then 
Do i1=1,3
ML1 = MFe(i1) 
ML2 = MFxe 
ML3 = MFxe 
coup1L = cplcFeFxecSscL(i1,gt1)
coup1R = cplcFeFxecSscR(i1,gt1)
coup2L = cplcFxeFeSscL(i1,gt2)
coup2R = cplcFxeFeSscR(i1,gt2)
coup3L = cplcFxeFxeVZL
coup3R = cplcFxeFxeVZR
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
End Do
End if 


! {bar[Fv], bar[Fxv], bar[Fxv]}
If ((Include_in_loopFv).and.(Include_in_loopFxv).and.(Include_in_loopFxv)) Then 
Do i1=1,3
  Do i2=1,2
    Do i3=1,2
ML1 = MFv(i1) 
ML2 = MFxv(i2) 
ML3 = MFxv(i3) 
coup1L = cplcFvFxvcSscL(i1,i2,gt1)
coup1R = cplcFvFxvcSscR(i1,i2,gt1)
coup2L = cplcFxvFvSscL(i3,i1,gt2)
coup2R = cplcFxvFvSscR(i3,i1,gt2)
coup3L = cplcFxvFxvVZL(i2,i3)
coup3R = cplcFxvFxvVZR(i2,i3)
If ((Abs(coup1L)+Abs(coup1R))*(Abs(coup2L)+Abs(coup2R))*(Abs(coup3L)+Abs(coup3R)).gt.epsCoup) Then 
Call Amp_VERTEX_StoSV_Topology1_FFF(Mex1,Mex2,Mex3,ML1,ML2,ML3,coup1L,coup1R,coup2L,coup2R,coup3L,coup3R,AmpC) 
Else
 AmpC = 0._dp
End if
! Colour and symmetry Factor 
Amp(:,gt1, gt2) = Amp(:,gt1, gt2) + oo16pi2*(1)*AmpC 
    End Do
  End Do
End Do
End if 
  End Do
End Do
End Subroutine Amplitude_VERTEX_SDdiracDM_SscToSscVZ


Subroutine Amplitude_IR_VERTEX_SDdiracDM_SscToSscVZ(MFe,MFv,MFxe,MFxv,MSsc,           & 
& MVZ,MFe2,MFv2,MFxe2,MFxv2,MSsc2,MVZ2,cplcFxeFeSscL,cplcFxeFeSscR,cplcFeFeVZL,          & 
& cplcFeFeVZR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFvFvVZL,cplcFvFvVZR,cplcFxeFxeVZL,         & 
& cplcFxeFxeVZR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvVZL,cplcFxvFxvVZR,               & 
& cplcFvFxvcSscL,cplcFvFxvcSscR,Amp)

Implicit None

Real(dp), Intent(in) :: MFe(3),MFv(3),MFxe,MFxv(2),MSsc(2),MVZ,MFe2(3),MFv2(3),MFxe2,MFxv2(2),MSsc2(2),MVZ2

Complex(dp), Intent(in) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),              & 
& cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),cplcFvFvVZL(3,3),cplcFvFvVZR(3,3),           & 
& cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFxvFxvVZL(2,2),& 
& cplcFxvFxvVZR(2,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2)

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
Mex1 = MSsc(gt1) 
Mex2 = MSsc(gt2) 
Mex3 = MVZ 
  End Do
End Do

IRdivOnly =.false. 
End Subroutine Amplitude_IR_VERTEX_SDdiracDM_SscToSscVZ



End Module OneLoopDecay_Ssc_SDdiracDM
