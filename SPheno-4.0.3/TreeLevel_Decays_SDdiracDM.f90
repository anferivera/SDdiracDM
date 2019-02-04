! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:21 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module TreeLevelDecays_SDdiracDM
 
Use Control 
Use DecayFunctions 
Use Settings 
Use LoopCouplings_SDdiracDM 
Use CouplingsForDecays_SDdiracDM 
Use Model_Data_SDdiracDM 
Use Mathematics, Only: Li2 
 
 Contains 
 
  
Subroutine FuTwoBodyDecay(i_in,deltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,            & 
& MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,            & 
& MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,             & 
& Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,               & 
& MS12,MS22,mP2,vvSM,vS,gPartial,gT,BR)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS,MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),          & 
& MFu2(3),MFv(3),MFv2(3),MFxe,MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,            & 
& MSsc(2),MSsc2(2),MVWp,MVWp2,MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp),Intent(in) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3),ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),             & 
& ZUL(3,3),UV(3,3),UVR(3,3),XU(2,2),XV(2,2),ZW(2,2)

Complex(dp) :: cplcFuFdVWpL(3,3),cplcFuFdVWpR(3,3),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),            & 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3)

Integer, Intent(in) :: i_in 
Real(dp), Intent(inout) :: gPartial(:,:), gT(:) 
Real(dp), Intent(in) :: deltaM 
Real(dp), Optional, Intent(inout) :: BR(:,:) 
Integer :: i1, i2, i3, i4, i_start, i_end, i_count, gt1, gt2, gt3, gt4 
Real(dp) :: gam, m_in, m1out, m2out, coupReal 
Complex(dp) :: coupC, coupR, coupL, coup 
 
Iname = Iname + 1 
NameOfUnit(Iname) = 'FuTwoBodyDecay'
 
If (i_in.Lt.0) Then 
  i_start = 1 
  i_end = 3
  gT = 0._dp 
  gPartial = 0._dp 
Else If ( (i_in.Ge.1).And.(i_in.Le.3) ) Then 
  i_start = i_in 
  i_end = i_in 
  gT(i_in) = 0._dp 
  gPartial(i_in,:) = 0._dp 
Else 
  If (ErrorLevel.Ge.-1) Then 
     Write(ErrCan,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,3

     Write(*,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(*,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,3

  End If 
  If (ErrorLevel.Gt.0) Call TerminateProgram 
  If (Present(BR)) BR = 0._dp 
  Iname = Iname -1 
  Return 
End If 
 
Do i1=i_start,i_end 
m_in = MFu(i1) 
If (m_in.Eq.0._dp) Cycle 
Call CouplingsFor_Fu_decays_2B(m_in,i1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,           & 
& MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,            & 
& MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,             & 
& Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,               & 
& MS12,MS22,mP2,vvSM,vS,cplcFuFdVWpL,cplcFuFdVWpR,cplcFuFuhhL,cplcFuFuhhR,               & 
& cplcFuFuVZL,cplcFuFuVZR,deltaM)

i_count = 1 

 
! ----------------------------------------------
! Fd, VWp
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = MFd(gt1)
m2out = MVWp
coupL = cplcFuFdVWpL(i1,gt1)
coupR = cplcFuFdVWpR(i1,gt1)
Call FermionToFermionVectorBoson(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
If (gt1.eq.3) Then 
  BR_tWb = gPartial(i1,i_count) 
End if 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! Fu, hh
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 2
m1out = MFu(gt1)
m2out = Mhh(gt2)
coupL = cplcFuFuhhL(i1,gt1,gt2)
coupR = cplcFuFuhhR(i1,gt1,gt2)
Call FermionToFermionScalar(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Fu, VZ
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = MFu(gt1)
m2out = MVZ
coupL = cplcFuFuVZL(i1,gt1)
coupR = cplcFuFuVZR(i1,gt1)
Call FermionToFermionVectorBoson(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! Fu, VG
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = MFu(gt1)
m2out = 0.
gam = 0._dp 
gPartial(i1,i_count) = 0._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! Fu, VP
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = MFu(gt1)
m2out = 0.
gam = 0._dp 
gPartial(i1,i_count) = 0._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
If ((Present(BR)).And.(gT(i1).Eq.0)) Then 
  BR(i1,:) = 0._dp 
Else If (Present(BR)) Then 
  BR(i1,:) = gPartial(i1,:)/gT(i1) 
End if 
 
End Do 
 
Iname = Iname - 1 
 
End Subroutine FuTwoBodyDecay
 
 
Subroutine FeTwoBodyDecay(i_in,deltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,            & 
& MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,            & 
& MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,             & 
& Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,               & 
& MS12,MS22,mP2,vvSM,vS,gPartial,gT,BR)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS,MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),          & 
& MFu2(3),MFv(3),MFv2(3),MFxe,MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,            & 
& MSsc(2),MSsc2(2),MVWp,MVWp2,MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp),Intent(in) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3),ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),             & 
& ZUL(3,3),UV(3,3),UVR(3,3),XU(2,2),XV(2,2),ZW(2,2)

Complex(dp) :: cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),              & 
& cplcFeFvcVWpL(3,3),cplcFeFvcVWpR(3,3),cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2)

Integer, Intent(in) :: i_in 
Real(dp), Intent(inout) :: gPartial(:,:), gT(:) 
Real(dp), Intent(in) :: deltaM 
Real(dp), Optional, Intent(inout) :: BR(:,:) 
Integer :: i1, i2, i3, i4, i_start, i_end, i_count, gt1, gt2, gt3, gt4 
Real(dp) :: gam, m_in, m1out, m2out, coupReal 
Complex(dp) :: coupC, coupR, coupL, coup 
 
Iname = Iname + 1 
NameOfUnit(Iname) = 'FeTwoBodyDecay'
 
If (i_in.Lt.0) Then 
  i_start = 1 
  i_end = 3
  gT = 0._dp 
  gPartial = 0._dp 
Else If ( (i_in.Ge.1).And.(i_in.Le.3) ) Then 
  i_start = i_in 
  i_end = i_in 
  gT(i_in) = 0._dp 
  gPartial(i_in,:) = 0._dp 
Else 
  If (ErrorLevel.Ge.-1) Then 
     Write(ErrCan,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,3

     Write(*,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(*,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,3

  End If 
  If (ErrorLevel.Gt.0) Call TerminateProgram 
  If (Present(BR)) BR = 0._dp 
  Iname = Iname -1 
  Return 
End If 
 
Do i1=i_start,i_end 
m_in = MFe(i1) 
If (m_in.Eq.0._dp) Cycle 
Call CouplingsFor_Fe_decays_2B(m_in,i1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,           & 
& MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,            & 
& MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,             & 
& Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,               & 
& MS12,MS22,mP2,vvSM,vS,cplcFeFehhL,cplcFeFehhR,cplcFeFeVZL,cplcFeFeVZR,cplcFeFvcVWpL,   & 
& cplcFeFvcVWpR,cplcFeFxecSscL,cplcFeFxecSscR,deltaM)

i_count = 1 

 
! ----------------------------------------------
! Fe, hh
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 2
m1out = MFe(gt1)
m2out = Mhh(gt2)
coupL = cplcFeFehhL(i1,gt1,gt2)
coupR = cplcFeFehhR(i1,gt1,gt2)
Call FermionToFermionScalar(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Fe, VZ
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = MFe(gt1)
m2out = MVZ
coupL = cplcFeFeVZL(i1,gt1)
coupR = cplcFeFeVZR(i1,gt1)
Call FermionToFermionVectorBoson(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! Fv, conj[VWp]
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = MFv(gt1)
m2out = MVWp
coupL = cplcFeFvcVWpL(i1,gt1)
coupR = cplcFeFvcVWpR(i1,gt1)
Call FermionToFermionVectorBoson(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! Fxe, conj[Ssc]
! ----------------------------------------------

 
  Do gt2=1, 2
m1out = MFxe
m2out = MSsc(gt2)
coupL = cplcFeFxecSscL(i1,gt2)
coupR = cplcFeFxecSscR(i1,gt2)
Call FermionToFermionScalar(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
End Do 
 

 
! ----------------------------------------------
! Fe, VP
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = MFe(gt1)
m2out = 0.
gam = 0._dp 
gPartial(i1,i_count) = 0._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
If ((Present(BR)).And.(gT(i1).Eq.0)) Then 
  BR(i1,:) = 0._dp 
Else If (Present(BR)) Then 
  BR(i1,:) = gPartial(i1,:)/gT(i1) 
End if 
 
End Do 
 
Iname = Iname - 1 
 
End Subroutine FeTwoBodyDecay
 
 
Subroutine FdTwoBodyDecay(i_in,deltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,            & 
& MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,            & 
& MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,             & 
& Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,               & 
& MS12,MS22,mP2,vvSM,vS,gPartial,gT,BR)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS,MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),          & 
& MFu2(3),MFv(3),MFv2(3),MFxe,MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,            & 
& MSsc(2),MSsc2(2),MVWp,MVWp2,MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp),Intent(in) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3),ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),             & 
& ZUL(3,3),UV(3,3),UVR(3,3),XU(2,2),XV(2,2),ZW(2,2)

Complex(dp) :: cplcFdFdhhL(3,3,2),cplcFdFdhhR(3,3,2),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),              & 
& cplcFdFucVWpL(3,3),cplcFdFucVWpR(3,3)

Integer, Intent(in) :: i_in 
Real(dp), Intent(inout) :: gPartial(:,:), gT(:) 
Real(dp), Intent(in) :: deltaM 
Real(dp), Optional, Intent(inout) :: BR(:,:) 
Integer :: i1, i2, i3, i4, i_start, i_end, i_count, gt1, gt2, gt3, gt4 
Real(dp) :: gam, m_in, m1out, m2out, coupReal 
Complex(dp) :: coupC, coupR, coupL, coup 
 
Iname = Iname + 1 
NameOfUnit(Iname) = 'FdTwoBodyDecay'
 
If (i_in.Lt.0) Then 
  i_start = 1 
  i_end = 3
  gT = 0._dp 
  gPartial = 0._dp 
Else If ( (i_in.Ge.1).And.(i_in.Le.3) ) Then 
  i_start = i_in 
  i_end = i_in 
  gT(i_in) = 0._dp 
  gPartial(i_in,:) = 0._dp 
Else 
  If (ErrorLevel.Ge.-1) Then 
     Write(ErrCan,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,3

     Write(*,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(*,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,3

  End If 
  If (ErrorLevel.Gt.0) Call TerminateProgram 
  If (Present(BR)) BR = 0._dp 
  Iname = Iname -1 
  Return 
End If 
 
Do i1=i_start,i_end 
m_in = MFd(i1) 
If (m_in.Eq.0._dp) Cycle 
Call CouplingsFor_Fd_decays_2B(m_in,i1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,           & 
& MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,            & 
& MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,             & 
& Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,               & 
& MS12,MS22,mP2,vvSM,vS,cplcFdFdhhL,cplcFdFdhhR,cplcFdFdVZL,cplcFdFdVZR,cplcFdFucVWpL,   & 
& cplcFdFucVWpR,deltaM)

i_count = 1 

 
! ----------------------------------------------
! Fd, hh
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 2
m1out = MFd(gt1)
m2out = Mhh(gt2)
coupL = cplcFdFdhhL(i1,gt1,gt2)
coupR = cplcFdFdhhR(i1,gt1,gt2)
Call FermionToFermionScalar(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Fd, VZ
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = MFd(gt1)
m2out = MVZ
coupL = cplcFdFdVZL(i1,gt1)
coupR = cplcFdFdVZR(i1,gt1)
Call FermionToFermionVectorBoson(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! Fu, conj[VWp]
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = MFu(gt1)
m2out = MVWp
coupL = cplcFdFucVWpL(i1,gt1)
coupR = cplcFdFucVWpR(i1,gt1)
Call FermionToFermionVectorBoson(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! Fd, VG
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = MFd(gt1)
m2out = 0.
gam = 0._dp 
gPartial(i1,i_count) = 0._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! Fd, VP
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = MFd(gt1)
m2out = 0.
gam = 0._dp 
gPartial(i1,i_count) = 0._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
If ((Present(BR)).And.(gT(i1).Eq.0)) Then 
  BR(i1,:) = 0._dp 
Else If (Present(BR)) Then 
  BR(i1,:) = gPartial(i1,:)/gT(i1) 
End if 
 
End Do 
 
Iname = Iname - 1 
 
End Subroutine FdTwoBodyDecay
 
 
Subroutine hhTwoBodyDecay(i_in,deltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,            & 
& MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,            & 
& MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,             & 
& Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,               & 
& MS12,MS22,mP2,vvSM,vS,gPartial,gT,BR)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS,MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),          & 
& MFu2(3),MFv(3),MFv2(3),MFxe,MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,            & 
& MSsc(2),MSsc2(2),MVWp,MVWp2,MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp),Intent(in) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3),ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),             & 
& ZUL(3,3),UV(3,3),UVR(3,3),XU(2,2),XV(2,2),ZW(2,2)

Complex(dp) :: cplHiggsPP(2),cplHiggsGG(2),cplHiggsZZvirt(2),cplHiggsWWvirt(2),cplcFdFdhhL(3,3,2),   & 
& cplcFdFdhhR(3,3,2),cplcFeFehhL(3,3,2),cplcFeFehhR(3,3,2),cplcFuFuhhL(3,3,2),           & 
& cplcFuFuhhR(3,3,2),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplhhhhhh(2,2,2),         & 
& cplhhSsccSsc(2,2,2),cplhhcVWpVWp(2),cplhhVZVZ(2)

Integer, Intent(in) :: i_in 
Real(dp), Intent(inout) :: gPartial(:,:), gT(:) 
Real(dp), Intent(in) :: deltaM 
Real(dp), Optional, Intent(inout) :: BR(:,:) 
Integer :: i1, i2, i3, i4, i_start, i_end, i_count, gt1, gt2, gt3, gt4 
Real(dp) :: gam, m_in, m1out, m2out, coupReal 
Complex(dp) :: coupC, coupR, coupL, coup 
 
Real(dp) :: alpha3 
Iname = Iname + 1 
NameOfUnit(Iname) = 'hhTwoBodyDecay'
 
If (i_in.Lt.0) Then 
  i_start = 1 
  i_end = 2
  gT = 0._dp 
  gPartial = 0._dp 
Else If ( (i_in.Ge.1).And.(i_in.Le.2) ) Then 
  i_start = i_in 
  i_end = i_in 
  gT(i_in) = 0._dp 
  gPartial(i_in,:) = 0._dp 
Else 
  If (ErrorLevel.Ge.-1) Then 
     Write(ErrCan,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,2

     Write(*,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(*,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,2

  End If 
  If (ErrorLevel.Gt.0) Call TerminateProgram 
  If (Present(BR)) BR = 0._dp 
  Iname = Iname -1 
  Return 
End If 
 
Do i1=i_start,i_end 
m_in = Mhh(i1) 
If (m_in.Eq.0._dp) Cycle 
Call CouplingsFor_hh_decays_2B(m_in,i1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,           & 
& MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,            & 
& MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,             & 
& Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,               & 
& MS12,MS22,mP2,vvSM,vS,cplHiggsPP,cplHiggsGG,cplHiggsZZvirt,cplHiggsWWvirt,             & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,               & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplhhhhhh,cplhhSsccSsc,cplhhcVWpVWp,cplhhVZVZ,deltaM)

!alpha3 = AlphaSDR(m_in,MFd,MFu) 
alpha3 = g3running**2/(4._dp*Pi) 
i_count = 1 

 
! ----------------------------------------------
! VP, VP
! ----------------------------------------------

 
m1out = 0.
m2out = 0.
gam = G_F * m_in**3 * oosqrt2 * oo128pi3 * Abs(cplHiggsPP(i1))**2 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! VG, VG
! ----------------------------------------------

 
m1out = 0.
m2out = 0.
gam = G_F * m_in**3 * oosqrt2 * oo36pi3 * Abs(cplHiggsGG(i1))**2 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! VZ, VZ
! ----------------------------------------------

 
m1out = MVZ
m2out = MVZ
If (m_in.le.2._dp*m1out) Then 
coupReal = cplHiggsZZvirt(i1)
Call ScalarToVectorBosonsVR(m_in,m1out,coupReal,gam) 
Else 
gam = 0._dp 
End if 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! VWp, VWp
! ----------------------------------------------

 
m1out = MVWp
m2out = MVWp
If (m_in.le.2._dp*m1out) Then 
coupReal = cplHiggsWWvirt(i1)
Call ScalarToVectorBosonsVR(m_in,m1out,coupReal,gam) 
Else 
gam = 0._dp 
End if 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! bar[Fd], Fd
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFd(gt1)
m2out = MFd(gt2)
coupL = cplcFdFdhhL(gt1,gt2,i1)
coupR = cplcFdFdhhR(gt1,gt2,i1)
Call ScalarToTwoFermions(m_in,m1out,m2out,coupL,coupR,gam) 
gam = gam * FFqcd(m1out,m_in,alpha3) 
gPartial(i1,i_count) = 3*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! bar[Fe], Fe
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFe(gt1)
m2out = MFe(gt2)
coupL = cplcFeFehhL(gt1,gt2,i1)
coupR = cplcFeFehhR(gt1,gt2,i1)
Call ScalarToTwoFermions(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! bar[Fu], Fu
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFu(gt1)
m2out = MFu(gt2)
coupL = cplcFuFuhhL(gt1,gt2,i1)
coupR = cplcFuFuhhR(gt1,gt2,i1)
Call ScalarToTwoFermions(m_in,m1out,m2out,coupL,coupR,gam) 
gam = gam * FFqcd(m1out,m_in,alpha3) 
gPartial(i1,i_count) = 3*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! bar[Fxv], Fxv
! ----------------------------------------------

 
Do gt1= 1, 2
  Do gt2=1, 2
m1out = MFxv(gt1)
m2out = MFxv(gt2)
coupL = cplcFxvFxvhhL(gt1,gt2,i1)
coupR = cplcFxvFxvhhR(gt1,gt2,i1)
Call ScalarToTwoFermions(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! hh, hh
! ----------------------------------------------

 
Do gt1= 1, 2
  Do gt2= gt1, 2
m1out = Mhh(gt1)
m2out = Mhh(gt2)
coup = cplhhhhhh(i1,gt1,gt2)
Call ScalarToTwoScalars(m_in,m1out,m2out,coup,gam) 
If (gt1.ne.gt2) gam = 2._dp*gam 
gPartial(i1,i_count) = 1._dp/2._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
If (gt1.eq.gt2) Then 
  BRHHH(i1,gt1) = gPartial(i1,i_count) 
End if 
  BRHHHijk(i1,gt1,gt2) = gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! conj[Ssc], Ssc
! ----------------------------------------------

 
Do gt1= 1, 2
  Do gt2=1, 2
m1out = MSsc(gt1)
m2out = MSsc(gt2)
coup = cplhhSsccSsc(i1,gt2,gt1)
Call ScalarToTwoScalars(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! VWp, conj[VWp]
! ----------------------------------------------

 
m1out = MVWp
m2out = MVWp
coup = cplhhcVWpVWp(i1)
Call ScalarToTwoVectorBosons(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! VZ, VZ
! ----------------------------------------------

 
m1out = MVZ
m2out = MVZ
coup = cplhhVZVZ(i1)
Call ScalarToTwoVectorBosons(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1._dp/2._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! Fv, bar[Fv]
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 3
m1out = MFv(gt1)
m2out = MFv(gt2)
gam = 0._dp 
gPartial(i1,i_count) = 0._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Fxe, bar[Fxe]
! ----------------------------------------------

 
m1out = MFxe
m2out = MFxe
gam = 0._dp 
gPartial(i1,i_count) = 0._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! hh, VP
! ----------------------------------------------

 
Do gt1= 1, 2
m1out = Mhh(gt1)
m2out = 0.
gam = 0._dp 
gPartial(i1,i_count) = 0._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! hh, VZ
! ----------------------------------------------

 
Do gt1= 1, 2
m1out = Mhh(gt1)
m2out = MVZ
gam = 0._dp 
gPartial(i1,i_count) = 0._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
  BRHHZ(i1,gt1) = gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! VG, VG
! ----------------------------------------------

 
m1out = 0.
m2out = 0.
gam = 0._dp 
gPartial(i1,i_count) = 0._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! VP, VP
! ----------------------------------------------

 
m1out = 0.
m2out = 0.
gam = 0._dp 
gPartial(i1,i_count) = 0._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! VZ, VP
! ----------------------------------------------

 
m1out = MVZ
m2out = 0.
gam = 0._dp 
gPartial(i1,i_count) = 0._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
If ((Present(BR)).And.(gT(i1).Eq.0)) Then 
  BR(i1,:) = 0._dp 
Else If (Present(BR)) Then 
  BR(i1,:) = gPartial(i1,:)/gT(i1) 
End if 
 
End Do 
 
Iname = Iname - 1 
 
Contains 
 
  Real(dp) Function FFqcd(mf, mA, alpha_s)
  Implicit None
   Real(dp) , Intent(in) :: mf, mA, alpha_s
   Real(dp) :: fac, beta, beta2, ratio, R_beta_1, Ln_R_beta_1, Ln_beta

   FFqcd = 0._dp
   ratio = mf / mA
   If (ratio.Ge.0.5_dp) Return ! decay is kinematically forbitten

   If (ratio.Ge.0.495_dp) Return ! Coloumb singularity

    beta2 = 1._dp - 4._dp * ratio**2
    beta = Sqrt(beta2)
    
    R_beta_1 = (1. - beta) / (1._dp + beta)
    Ln_beta = Log(beta)
    Ln_R_beta_1 = Log(R_beta_1)

    fac = (3._dp + 34._dp * beta2 - 13._dp * beta**4) / (16._dp * beta**3)  &
      &     * (-Ln_R_beta_1)                                                &
      & + 0.375_dp * (7._dp - beta2) - 3._dp * Log(4._dp/(1._dp - beta**2)) &
      & - 4._dp * Ln_beta                                                   &
      & + (1._dp + beta**2)                                                 &
      &       * ( 4._dp * Li2(R_beta_1) + 2._dp * Li2(- R_beta_1)           &
      &         + Ln_R_beta_1 * ( 3._dp * Log(2._dp/(1._dp + beta))         &
      &                         + 2._dp * Ln_beta )  ) / beta

    fac = fac - 3._dp * Log(ratio)  ! absorb large logarithms in mass

    FFqcd = 1._dp + 5._dp * alpha_s * fac * oo3pi 

  End  Function FFqcd
End Subroutine hhTwoBodyDecay
 
 
Subroutine SscTwoBodyDecay(i_in,deltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,           & 
& MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,            & 
& MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,             & 
& Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,               & 
& MS12,MS22,mP2,vvSM,vS,gPartial,gT,BR)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS,MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),          & 
& MFu2(3),MFv(3),MFv2(3),MFxe,MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,            & 
& MSsc(2),MSsc2(2),MVWp,MVWp2,MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp),Intent(in) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3),ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),             & 
& ZUL(3,3),UV(3,3),UVR(3,3),XU(2,2),XV(2,2),ZW(2,2)

Complex(dp) :: cplcFeFxecSscL(3,2),cplcFeFxecSscR(3,2),cplcFvFxvcSscL(3,2,2),cplcFvFxvcSscR(3,2,2),  & 
& cplhhSsccSsc(2,2,2)

Integer, Intent(in) :: i_in 
Real(dp), Intent(inout) :: gPartial(:,:), gT(:) 
Real(dp), Intent(in) :: deltaM 
Real(dp), Optional, Intent(inout) :: BR(:,:) 
Integer :: i1, i2, i3, i4, i_start, i_end, i_count, gt1, gt2, gt3, gt4 
Real(dp) :: gam, m_in, m1out, m2out, coupReal 
Complex(dp) :: coupC, coupR, coupL, coup 
 
Iname = Iname + 1 
NameOfUnit(Iname) = 'SscTwoBodyDecay'
 
If (i_in.Lt.0) Then 
  i_start = 1 
  i_end = 2
  gT = 0._dp 
  gPartial = 0._dp 
Else If ( (i_in.Ge.1).And.(i_in.Le.2) ) Then 
  i_start = i_in 
  i_end = i_in 
  gT(i_in) = 0._dp 
  gPartial(i_in,:) = 0._dp 
Else 
  If (ErrorLevel.Ge.-1) Then 
     Write(ErrCan,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,2

     Write(*,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(*,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,2

  End If 
  If (ErrorLevel.Gt.0) Call TerminateProgram 
  If (Present(BR)) BR = 0._dp 
  Iname = Iname -1 
  Return 
End If 
 
Do i1=i_start,i_end 
m_in = MSsc(i1) 
If (m_in.Eq.0._dp) Cycle 
Call CouplingsFor_Ssc_decays_2B(m_in,i1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,               & 
& MFu2,MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,           & 
& MVZ,MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,               & 
& g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,              & 
& m2SM,MS12,MS22,mP2,vvSM,vS,cplcFeFxecSscL,cplcFeFxecSscR,cplcFvFxvcSscL,               & 
& cplcFvFxvcSscR,cplhhSsccSsc,deltaM)

i_count = 1 

 
! ----------------------------------------------
! bar[Fe], Fxe
! ----------------------------------------------

 
Do gt1= 1, 3
m1out = MFe(gt1)
m2out = MFxe
coupL = cplcFeFxecSscL(gt1,i1)
coupR = cplcFeFxecSscR(gt1,i1)
Call ScalarToTwoFermions(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! bar[Fv], Fxv
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 2
m1out = MFv(gt1)
m2out = MFxv(gt2)
coupL = cplcFvFxvcSscL(gt1,gt2,i1)
coupR = cplcFvFxvcSscR(gt1,gt2,i1)
Call ScalarToTwoFermions(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Ssc, hh
! ----------------------------------------------

 
Do gt1= 1, 2
  Do gt2=1, 2
m1out = MSsc(gt1)
m2out = Mhh(gt2)
coup = cplhhSsccSsc(gt2,gt1,i1)
Call ScalarToTwoScalars(m_in,m1out,m2out,coup,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Ssc, VP
! ----------------------------------------------

 
Do gt1= 1, 2
m1out = MSsc(gt1)
m2out = 0.
gam = 0._dp 
gPartial(i1,i_count) = 0._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! Ssc, VZ
! ----------------------------------------------

 
Do gt1= 1, 2
m1out = MSsc(gt1)
m2out = MVZ
gam = 0._dp 
gPartial(i1,i_count) = 0._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
If ((Present(BR)).And.(gT(i1).Eq.0)) Then 
  BR(i1,:) = 0._dp 
Else If (Present(BR)) Then 
  BR(i1,:) = gPartial(i1,:)/gT(i1) 
End if 
 
End Do 
 
Iname = Iname - 1 
 
End Subroutine SscTwoBodyDecay
 
 
Subroutine FxeTwoBodyDecay(i_in,deltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,           & 
& MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,            & 
& MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,             & 
& Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,               & 
& MS12,MS22,mP2,vvSM,vS,gPartial,gT,BR)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS,MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),          & 
& MFu2(3),MFv(3),MFv2(3),MFxe,MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,            & 
& MSsc(2),MSsc2(2),MVWp,MVWp2,MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp),Intent(in) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3),ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),             & 
& ZUL(3,3),UV(3,3),UVR(3,3),XU(2,2),XV(2,2),ZW(2,2)

Complex(dp) :: cplcFxeFeSscL(3,2),cplcFxeFeSscR(3,2),cplcFxeFxeVZL,cplcFxeFxeVZR,cplcFxeFxvcVWpL(2), & 
& cplcFxeFxvcVWpR(2)

Integer, Intent(in) :: i_in 
Real(dp), Intent(inout) :: gPartial(:,:), gT 
Real(dp), Intent(in) :: deltaM 
Real(dp), Optional, Intent(inout) :: BR(:,:) 
Integer :: i1, i2, i3, i4, i_start, i_end, i_count, gt1, gt2, gt3, gt4 
Real(dp) :: gam, m_in, m1out, m2out, coupReal 
Complex(dp) :: coupC, coupR, coupL, coup 
 
Iname = Iname + 1 
NameOfUnit(Iname) = 'FxeTwoBodyDecay'
 
If (i_in.Lt.0) Then 
  i_start = 1 
  i_end = 1 
  gT = 0._dp 
  gPartial = 0._dp 
Else 
  If (ErrorLevel.Ge.-1) Then 
     Write(ErrCan,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,1

     Write(*,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(*,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,1

  End If 
  If (ErrorLevel.Gt.0) Call TerminateProgram 
  If (Present(BR)) BR = 0._dp 
  Iname = Iname -1 
  Return 
End If 
 
i1=1 
m_in = MFxe 
Call CouplingsFor_Fxe_decays_2B(m_in,i1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,               & 
& MFu2,MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,           & 
& MVZ,MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,               & 
& g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,              & 
& m2SM,MS12,MS22,mP2,vvSM,vS,cplcFxeFeSscL,cplcFxeFeSscR,cplcFxeFxeVZL,cplcFxeFxeVZR,    & 
& cplcFxeFxvcVWpL,cplcFxeFxvcVWpR,deltaM)

i_count = 1 

 
! ----------------------------------------------
! Fe, Ssc
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 2
m1out = MFe(gt1)
m2out = MSsc(gt2)
coupL = cplcFxeFeSscL(gt1,gt2)
coupR = cplcFxeFeSscR(gt1,gt2)
Call FermionToFermionScalar(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(1,i_count) = 1*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Fxe, VZ
! ----------------------------------------------

 
m1out = MFxe
m2out = MVZ
coupL = cplcFxeFxeVZL
coupR = cplcFxeFxeVZR
Call FermionToFermionVectorBoson(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(1,i_count) = 1*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! Fxv, conj[VWp]
! ----------------------------------------------

 
Do gt1= 1, 2
m1out = MFxv(gt1)
m2out = MVWp
coupL = cplcFxeFxvcVWpL(gt1)
coupR = cplcFxeFxvcVWpR(gt1)
Call FermionToFermionVectorBoson(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(1,i_count) = 1*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! Fxe, hh
! ----------------------------------------------

 
  Do gt2=1, 2
m1out = MFxe
m2out = Mhh(gt2)
gam = 0._dp 
gPartial(1,i_count) = 0._dp*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
End Do 
 

 
! ----------------------------------------------
! Fxe, VP
! ----------------------------------------------

 
m1out = MFxe
m2out = 0.
gam = 0._dp 
gPartial(1,i_count) = 0._dp*gam 
gT = gT + gPartial(1,i_count) 
i_count = i_count +1 
If ((Present(BR)).And.(gT.Eq.0)) Then 
  BR(1,:) = 0._dp 
Else If (Present(BR)) Then 
  BR(1,:) = gPartial(1,:)/gT 
End if 
 
Iname = Iname - 1 
 
End Subroutine FxeTwoBodyDecay
 
 
Subroutine FxvTwoBodyDecay(i_in,deltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,           & 
& MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,            & 
& MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,             & 
& Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,               & 
& MS12,MS22,mP2,vvSM,vS,gPartial,gT,BR)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS,MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),          & 
& MFu2(3),MFv(3),MFv2(3),MFxe,MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,            & 
& MSsc(2),MSsc2(2),MVWp,MVWp2,MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp),Intent(in) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3),ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),             & 
& ZUL(3,3),UV(3,3),UVR(3,3),XU(2,2),XV(2,2),ZW(2,2)

Complex(dp) :: cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),cplcFxvFxeVWpL(2),cplcFxvFxeVWpR(2),        & 
& cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFxvFxvVZL(2,2),cplcFxvFxvVZR(2,2)

Integer, Intent(in) :: i_in 
Real(dp), Intent(inout) :: gPartial(:,:), gT(:) 
Real(dp), Intent(in) :: deltaM 
Real(dp), Optional, Intent(inout) :: BR(:,:) 
Integer :: i1, i2, i3, i4, i_start, i_end, i_count, gt1, gt2, gt3, gt4 
Real(dp) :: gam, m_in, m1out, m2out, coupReal 
Complex(dp) :: coupC, coupR, coupL, coup 
 
Iname = Iname + 1 
NameOfUnit(Iname) = 'FxvTwoBodyDecay'
 
If (i_in.Lt.0) Then 
  i_start = 1 
  i_end = 2
  gT = 0._dp 
  gPartial = 0._dp 
Else If ( (i_in.Ge.1).And.(i_in.Le.2) ) Then 
  i_start = i_in 
  i_end = i_in 
  gT(i_in) = 0._dp 
  gPartial(i_in,:) = 0._dp 
Else 
  If (ErrorLevel.Ge.-1) Then 
     Write(ErrCan,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,2

     Write(*,*) 'Problem in subroutine '//NameOfUnit(Iname) 
     Write(*,*) 'Value of i_in out of range, (i_in,i_max) = ', i_in,2

  End If 
  If (ErrorLevel.Gt.0) Call TerminateProgram 
  If (Present(BR)) BR = 0._dp 
  Iname = Iname -1 
  Return 
End If 
 
Do i1=i_start,i_end 
m_in = MFxv(i1) 
If (m_in.Eq.0._dp) Cycle 
Call CouplingsFor_Fxv_decays_2B(m_in,i1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,               & 
& MFu2,MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,           & 
& MVZ,MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,               & 
& g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,              & 
& m2SM,MS12,MS22,mP2,vvSM,vS,cplcFxvFvSscL,cplcFxvFvSscR,cplcFxvFxeVWpL,cplcFxvFxeVWpR,  & 
& cplcFxvFxvhhL,cplcFxvFxvhhR,cplcFxvFxvVZL,cplcFxvFxvVZR,deltaM)

i_count = 1 

 
! ----------------------------------------------
! Fv, Ssc
! ----------------------------------------------

 
Do gt1= 1, 3
  Do gt2=1, 2
m1out = MFv(gt1)
m2out = MSsc(gt2)
coupL = cplcFxvFvSscL(i1,gt1,gt2)
coupR = cplcFxvFvSscR(i1,gt1,gt2)
Call FermionToFermionScalar(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Fxe, VWp
! ----------------------------------------------

 
m1out = MFxe
m2out = MVWp
coupL = cplcFxvFxeVWpL(i1)
coupR = cplcFxvFxeVWpR(i1)
Call FermionToFermionVectorBoson(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 

 
! ----------------------------------------------
! Fxv, hh
! ----------------------------------------------

 
Do gt1= 1, 2
  Do gt2=1, 2
m1out = MFxv(gt1)
m2out = Mhh(gt2)
coupL = cplcFxvFxvhhL(i1,gt1,gt2)
coupR = cplcFxvFxvhhR(i1,gt1,gt2)
Call FermionToFermionScalar(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
End Do 
 

 
! ----------------------------------------------
! Fxv, VZ
! ----------------------------------------------

 
Do gt1= 1, 2
m1out = MFxv(gt1)
m2out = MVZ
coupL = cplcFxvFxvVZL(i1,gt1)
coupR = cplcFxvFxvVZR(i1,gt1)
Call FermionToFermionVectorBoson(m_in,m1out,m2out,coupL,coupR,gam) 
gPartial(i1,i_count) = 1*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 

 
! ----------------------------------------------
! Fxv, VP
! ----------------------------------------------

 
Do gt1= 1, 2
m1out = MFxv(gt1)
m2out = 0.
gam = 0._dp 
gPartial(i1,i_count) = 0._dp*gam 
gT(i1) = gT(i1) + gPartial(i1,i_count) 
i_count = i_count +1 
  End Do 
If ((Present(BR)).And.(gT(i1).Eq.0)) Then 
  BR(i1,:) = 0._dp 
Else If (Present(BR)) Then 
  BR(i1,:) = gPartial(i1,:)/gT(i1) 
End if 
 
End Do 
 
Iname = Iname - 1 
 
End Subroutine FxvTwoBodyDecay
 
 
Subroutine ScalarToTwoVectorbosonsNew(mS,mV1,mV2,coup,width)
  implicit none
   real(dp), intent(in) :: mS,mV1,mV2
   real(dp), intent(out) :: width
   complex(dp), intent(in) :: coup

   real(dp) :: mSsq,mV1sq, mV2sq,kappa

   if ( abs(mS).le.( abs(mV1)+abs(mV2) ) ) then
    width = 0._dp

   elseif (mV1.eq.0._dp) then
    write(ErrCan,*) 'Server warning, in subroutine ScalarToTwoVectorbosons'
    write(ErrCan,*) 'm_V1 = 0, setting width to 0'
    width = 0._dp

   elseif (mV2.eq.0._dp) then
    write(ErrCan,*) 'Server warning, in subroutine ScalarToTwoVectorbosons'
    write(ErrCan,*) 'm_V2 = 0, setting width to 0'
    width = 0._dp


   elseif (Abs(coup).eq.0._dp) then
    width = 0._dp

   else
    mSsq = mS * mS
    mV1sq = mV1**2
    mV2sq = mV2**2
    kappa = Sqrt( (mSsq-mV1sq-mV2sq)**2 - 4._dp * mV1sq*mV2sq )/(mS**3)
    width = coup**2/(4._dp*mV1sq*mV2sq)*(mV1sq**2 + 10._dp*mV1sq*mV2sq - &
             & 2._dp*(mV1sq+mV2sq)*mSsq + mV2sq**2 +mSsq**2)
    width = oo16Pi*width*kappa

   endif

  End Subroutine ScalarToTwoVectorbosonsNew

Subroutine FermionToFermionVectorBosonMassless(mFin,mFout,mV,coupL,coupR,width)
  implicit none
   real(dp), intent(in) :: mFin,mFout,mV
   real(dp), intent(out) :: width
   complex(dp), intent(in) :: coupL, coupR


   if ( abs(mFin).le.( abs(mFout)+abs(mV) ) ) then
    width = 0._dp

   else

    width = 0.5_dp*oo16Pi*(Abs(coupL)**2 + Abs(coupR)**2)*mFin**3


   endif

  End Subroutine FermionToFermionVectorBosonMassless

End Module TreeLevelDecays_SDdiracDM 
 
