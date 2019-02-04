! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:20 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module TreeLevelMasses_SDdiracDM 
 
Use Control 
Use Mathematics 
Use MathematicsQP 
Use Settings 
Use Model_Data_SDdiracDM 
!Use StandardModel 
 
 
Logical :: SignOfMassChanged =.False.  
Logical :: SignOfMuChanged =.False.  
Contains 
 
Subroutine TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,              & 
& MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,              & 
& ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,vvSM,vS,g1,g2,g3,Lam,             & 
& LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,              & 
& MS22,mP2,GenerationMixing,kont)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2

Complex(dp),Intent(in) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Real(dp),Intent(out) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp),Intent(out) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Real(dp),Intent(in) :: vvSM,vS

Logical, Intent(in) :: GenerationMixing 
Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,kontSave 
Complex(dp) :: MFxeC 
Iname = Iname + 1 
NameOfUnit(Iname) = 'TreeMassesSDdiracDM'
 
kont = 0 
Call CalculateVPVZ(g1,g2,vvSM,ZZ,MVZ,MVZ2,TW,kont)

Call CalculateVWp(g2,vvSM,ZW,MVWp,MVWp2,kont)

! ------------------------------- 
! Mass of Fxe 
MFxeC = MDF 
If (RotateNegativeFermionMasses) Then 
MFxe = Abs(MFxeC) 
MFxe2 = MFxe**2 
Else 
MFxe = Real(MFxeC,dp) 
MFxe2 = MFxe**2 
End if
! ------------------------------- 
MHp2 = (-4._dp*(m2SM) + 2*Lam*vvSM**2 + 2*LSPH*vS**2 + g2**2*vvSM**2*RXiWp)/4._dp 
  If (MHp2.ne.MHp2) Then 
      Write(*,*) 'NaN appearing in MHp2' 
      Call TerminateProgram 
    End If 
  If (MHp2.Ge.0._dp) Then 
MHp = sqrt(MHp2) 
  Else 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from MHp2' 
      Write(10,*) 'mass squarred is negative: ',MHp2 
    End If 
     Write(ErrCan,*) 'Warning from routine MHp2' 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) MHp2 
     Write(*,*) 'Warning from routine MHp2' 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) MHp2 
        MHp= 1._dp 
        MHp2= 1._dp 
   SignOfMassChanged = .True. 
 End if 


! ------------------------------- 
MAh2 = (2*(-2._dp*(m2SM) + Lam*vvSM**2 + LSPH*vS**2) + vvSM**2*RXiZ*(g2*Cos(TW)       & 
&  + g1*Sin(TW))**2)/4._dp 
  If (MAh2.ne.MAh2) Then 
      Write(*,*) 'NaN appearing in MAh2' 
      Call TerminateProgram 
    End If 
  If (MAh2.Ge.0._dp) Then 
MAh = sqrt(MAh2) 
  Else 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from MAh2' 
      Write(10,*) 'mass squarred is negative: ',MAh2 
    End If 
     Write(ErrCan,*) 'Warning from routine MAh2' 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) MAh2 
     Write(*,*) 'Warning from routine MAh2' 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) MAh2 
        MAh= 1._dp 
        MAh2= 1._dp 
   SignOfMassChanged = .True. 
 End if 


! ------------------------------- 
Call CalculateMhh(m2SM,Lam,mP2,LSP,LSPH,vvSM,vS,ZH,Mhh,Mhh2,kont)

Call CalculateMFd(Yd,vvSM,ZDL,ZDR,MFd,kont)

MFd2 = MFd**2 
Call CalculateMFu(Yu,vvSM,ZUL,ZUR,MFu,kont)

MFu2 = MFu**2 
Call CalculateMFe(Ye,vvSM,ZEL,ZER,MFe,kont)

MFe2 = MFe**2 
Call CalculateMFv(UV,UVR,MFv,kont)

MFv2 = MFv**2 
Call CalculateMFxv(MDF,YRD,YRC,vvSM,vS,XV,XU,MFxv,kont)

MFxv2 = MFxv**2 
Call CalculateMSsc(MS12,MS22,LS1H,LS2H,vvSM,VSs,MSsc,MSsc2,kont)


 
 Call SortGoldstones(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,          & 
& MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,            & 
& ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,kont)

If (SignOfMassChanged) Then  
 If (.Not.IgnoreNegativeMasses) Then 
  Write(*,*) " Stopping calculation because of negative mass squared." 
  Call TerminateProgram 
 Else 
  SignOfMassChanged= .False. 
  kont=0  
 End If 
End If 
If (SignOfMuChanged) Then 
 If (.Not.IgnoreMuSignFlip) Then 
  Write(*,*) " Stopping calculation because of negative mass squared in tadpoles." 
  Call TerminateProgram 
 Else 
  SignOfMuChanged= .False. 
  kont=0 
 End If 
End If 

 ! -------------------------------- 
! Setting Goldstone masses 
! -------------------------------- 
 
MAh=MVZ*sqrt(RXiZ)
MAh2=MVZ2*RXiZ
MHp=MVWp*sqrt(RXiWp)
MHp2=MVWp2*RXiWp
TW = ACos(Abs(ZZ(1,1)))
alphaH = ATan(ZH(2,2)/ZH(1,2))
Iname = Iname - 1 
 
End Subroutine  TreeMasses 
 
 
Subroutine RunningFermionMasses(MFeIN,MFe2IN,MFdIN,MFd2IN,MFuIN,MFu2IN,               & 
& vvSM,vS,g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,            & 
& YRA2,MDF,m2SM,MS12,MS22,mP2,kont)

Implicit None 
 
Integer, Intent(inout) :: kont 
Real(dp),Intent(in) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2

Complex(dp),Intent(in) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Real(dp),Intent(in) :: vvSM,vS

Real(dp),Intent(inout) :: MFeIN(3),MFe2IN(3),MFdIN(3),MFd2IN(3),MFuIN(3),MFu2IN(3)

Real(dp) :: MFe(3),MFe2(3),MFd(3),MFd2(3),MFu(3),MFu2(3)

Call CalculateMFe(Ye,vvSM,ZEL,ZER,MFe,kont)

MFe2 = MFe**2 
MFeIN(1:2) = MFe(1:2) 
MFe2IN(1:2) = MFe2(1:2) 
Call CalculateMFd(Yd,vvSM,ZDL,ZDR,MFd,kont)

MFd2 = MFd**2 
MFdIN(1:2) = MFd(1:2) 
MFd2IN(1:2) = MFd2(1:2) 
Call CalculateMFu(Yu,vvSM,ZUL,ZUR,MFu,kont)

MFu2 = MFu**2 
MFuIN(1:2) = MFu(1:2) 
MFu2IN(1:2) = MFu2(1:2) 
End Subroutine RunningFermionMasses 

Subroutine TreeMassesEffPot(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,             & 
& MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,             & 
& ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,vvSM,vS,g1,g2,g3,             & 
& Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,               & 
& MS12,MS22,mP2,GenerationMixing,kont)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2

Complex(dp),Intent(in) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Real(dp),Intent(out) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp),Intent(out) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Real(dp),Intent(in) :: vvSM,vS

Logical, Intent(in) :: GenerationMixing 
Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,kontSave 
Complex(dp) :: MFxeC 
Iname = Iname + 1 
NameOfUnit(Iname) = 'TreeMassesSDdiracDM'
 
kont = 0 
Call CalculateVPVZEffPot(g1,g2,vvSM,ZZ,MVZ,MVZ2,TW,kont)

Call CalculateVWpEffPot(g2,vvSM,ZW,MVWp,MVWp2,kont)

! ------------------------------- 
! Mass of Fxe 
MFxeC = MDF 
If (RotateNegativeFermionMasses) Then 
MFxe = Abs(MFxeC) 
MFxe2 = MFxe**2 
Else 
MFxe = Real(MFxeC,dp) 
MFxe2 = MFxe**2 
End if
! ------------------------------- 
MHp2 = (-4._dp*(m2SM) + 2*Lam*vvSM**2 + 2*LSPH*vS**2 + g2**2*vvSM**2*RXiWp)/4._dp 
  If (MHp2.ne.MHp2) Then 
      Write(*,*) 'NaN appearing in MHp2' 
      Call TerminateProgram 
    End If 
  If (MHp2.Ge.0._dp) Then 
MHp = sqrt(MHp2) 
  Else 
 End if 


! ------------------------------- 
MAh2 = (2*(-2._dp*(m2SM) + Lam*vvSM**2 + LSPH*vS**2) + vvSM**2*RXiZ*(g2*Cos(TW)       & 
&  + g1*Sin(TW))**2)/4._dp 
  If (MAh2.ne.MAh2) Then 
      Write(*,*) 'NaN appearing in MAh2' 
      Call TerminateProgram 
    End If 
  If (MAh2.Ge.0._dp) Then 
MAh = sqrt(MAh2) 
  Else 
 End if 


! ------------------------------- 
Call CalculateMhhEffPot(m2SM,Lam,mP2,LSP,LSPH,vvSM,vS,ZH,Mhh,Mhh2,kont)

Call CalculateMFdEffPot(Yd,vvSM,ZDL,ZDR,MFd,kont)

MFd2 = MFd**2 
Call CalculateMFuEffPot(Yu,vvSM,ZUL,ZUR,MFu,kont)

MFu2 = MFu**2 
Call CalculateMFeEffPot(Ye,vvSM,ZEL,ZER,MFe,kont)

MFe2 = MFe**2 
Call CalculateMFvEffPot(UV,UVR,MFv,kont)

MFv2 = MFv**2 
Call CalculateMFxvEffPot(MDF,YRD,YRC,vvSM,vS,XV,XU,MFxv,kont)

MFxv2 = MFxv**2 
Call CalculateMSscEffPot(MS12,MS22,LS1H,LS2H,vvSM,VSs,MSsc,MSsc2,kont)


 
 If (SignOfMassChanged) Then  
 If (.Not.IgnoreNegativeMasses) Then 
  Write(*,*) " Stopping calculation because of negative mass squared." 
  Call TerminateProgram 
 Else 
  SignOfMassChanged= .False. 
  kont=0  
 End If 
End If 
If (SignOfMuChanged) Then 
 If (.Not.IgnoreMuSignFlip) Then 
  Write(*,*) " Stopping calculation because of negative mass squared in tadpoles." 
  Call TerminateProgram 
 Else 
  SignOfMuChanged= .False. 
  kont=0 
 End If 
End If 
Iname = Iname - 1 
 
End Subroutine  TreeMassesEffPot 
 
 
Subroutine CalculateMhh(m2SM,Lam,mP2,LSP,LSPH,vvSM,vS,ZH,Mhh,Mhh2,kont)

Real(dp), Intent(in) :: m2SM,mP2,LSP,LSPH,vvSM,vS

Complex(dp), Intent(in) :: Lam

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4, pos 
Real(dp), Intent(out) :: Mhh(2), Mhh2(2) 
Real(dp), Intent(out) :: ZH(2,2) 
 
Real(dp) :: mat(2,2)  

Real(dp) ::  test(2) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMhh'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)-1._dp*(m2SM)
mat(1,1) = mat(1,1)+(3*Lam*vvSM**2)/2._dp
mat(1,1) = mat(1,1)+(LSPH*vS**2)/2._dp
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+LSPH*vvSM*vS
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+mP2/2._dp
mat(2,2) = mat(2,2)+(LSPH*vvSM**2)/2._dp
mat(2,2) = mat(2,2)+(3*LSP*vS**2)/2._dp

 
 Do i1=2,2
  Do i2 = 1, i1-1 
  mat(i1,i2) = mat(i2,i1) 
  End do 
End do 

 
Call EigenSystem(mat,Mhh2,ZH,ierr,test) 
 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,2
  If (Abs(Mhh2(i1)).Le.MaxMassNumericalZero) Mhh2(i1) = 1.E-10_dp 
  If (Mhh2(i1).ne.Mhh2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Mhh2(i1).Ge.0._dp) Then 
  Mhh(i1)=Sqrt(Mhh2(i1) ) 
  Else 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,Mhh2(i1) 
    End If 
  Mhh(i1) = 1._dp 
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,Mhh2(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,Mhh2(i1) 
  Mhh2(i1) = 1._dp 
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
Iname = Iname - 1 
 
End Subroutine CalculateMhh 

Subroutine CalculateMFd(Yd,vvSM,ZDL,ZDR,MFd,kont)

Real(dp),Intent(in) :: vvSM

Complex(dp),Intent(in) :: Yd(3,3)

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MFd(3) 
 Complex(dp), Intent(out) :: ZDL(3,3), ZDR(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZDL2(3,3), ZDR2(3,3) 
 
 Real(dp) :: ZDL1(3,3), ZDR1(3,3), test(2), MFd2(3) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFd'
 
MFd = 0._dp 
ZDL = 0._dp 
ZDR = 0._dp 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(vvSM*Yd(1,1))/sqrt(2._dp)
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+(vvSM*Yd(2,1))/sqrt(2._dp)
mat(1,3) = 0._dp 
mat(1,3) = mat(1,3)+(vvSM*Yd(3,1))/sqrt(2._dp)
mat(2,1) = 0._dp 
mat(2,1) = mat(2,1)+(vvSM*Yd(1,2))/sqrt(2._dp)
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(vvSM*Yd(2,2))/sqrt(2._dp)
mat(2,3) = 0._dp 
mat(2,3) = mat(2,3)+(vvSM*Yd(3,2))/sqrt(2._dp)
mat(3,1) = 0._dp 
mat(3,1) = mat(3,1)+(vvSM*Yd(1,3))/sqrt(2._dp)
mat(3,2) = 0._dp 
mat(3,2) = mat(3,2)+(vvSM*Yd(2,3))/sqrt(2._dp)
mat(3,3) = 0._dp 
mat(3,3) = mat(3,3)+(vvSM*Yd(3,3))/sqrt(2._dp)

 
mat2 = Matmul(Transpose(Conjg(mat)),mat) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFd2,ZDR1,ierr,test) 
ZDR2 = ZDR1 
Else 
Call EigenSystem(mat2,MFd2,ZDR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(mat,Transpose(Conjg(mat))) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem (Real(mat2,dp),MFd2,ZDL1,ierr,test) 
                  
                  
ZDL2 = ZDL1 
Else 
Call EigenSystem(mat2,MFd2,ZDL2,ierr,test) 
 
 
End If 
ZDL2 = Conjg(ZDL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZDL2),mat),Transpose( Conjg(ZDR2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZDR2(i1,:) = phaseM *ZDR2(i1,:) 
 End if 
End Do 
 
Do i1=1,3
If (Abs(ZDR2(i1,i1)).gt.0._dp) Then 
phaseM = ZDR2(i1,i1) / Abs(ZDR2(i1,i1)) 
ZDR2(i1,:) = Conjg(phaseM) *ZDR2(i1,:) 
 ZDL2(i1,:) = phaseM *ZDL2(i1,:) 
 End if 
  If (MFd2(i1).ne.MFd2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Abs(MFd2(i1)).Le.MaxMassNumericalZero) MFd2(i1) = Abs(MFd2(i1))+1.E-10_dp 
  If (MFd2(i1).Le.0._dp) Then 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,MFd2(i1) 
      Write(*,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(*,*) 'a mass squarred is negative: ',i1,MFd2(i1) 
      Call TerminateProgram 
    End If 
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,MFd2(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,MFd2(i1) 
  MFd2(i1) = 1._dp 
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFd, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


MFd = Sqrt( MFd2 ) 
ZDL = ZDL2 
ZDR = ZDR2 
Iname = Iname - 1 
 
End Subroutine CalculateMFd 

Subroutine CalculateMFu(Yu,vvSM,ZUL,ZUR,MFu,kont)

Real(dp),Intent(in) :: vvSM

Complex(dp),Intent(in) :: Yu(3,3)

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MFu(3) 
 Complex(dp), Intent(out) :: ZUL(3,3), ZUR(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZUL2(3,3), ZUR2(3,3) 
 
 Real(dp) :: ZUL1(3,3), ZUR1(3,3), test(2), MFu2(3) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFu'
 
MFu = 0._dp 
ZUL = 0._dp 
ZUR = 0._dp 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(vvSM*Yu(1,1))/sqrt(2._dp)
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+(vvSM*Yu(2,1))/sqrt(2._dp)
mat(1,3) = 0._dp 
mat(1,3) = mat(1,3)+(vvSM*Yu(3,1))/sqrt(2._dp)
mat(2,1) = 0._dp 
mat(2,1) = mat(2,1)+(vvSM*Yu(1,2))/sqrt(2._dp)
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(vvSM*Yu(2,2))/sqrt(2._dp)
mat(2,3) = 0._dp 
mat(2,3) = mat(2,3)+(vvSM*Yu(3,2))/sqrt(2._dp)
mat(3,1) = 0._dp 
mat(3,1) = mat(3,1)+(vvSM*Yu(1,3))/sqrt(2._dp)
mat(3,2) = 0._dp 
mat(3,2) = mat(3,2)+(vvSM*Yu(2,3))/sqrt(2._dp)
mat(3,3) = 0._dp 
mat(3,3) = mat(3,3)+(vvSM*Yu(3,3))/sqrt(2._dp)

 
mat2 = Matmul(Transpose(Conjg(mat)),mat) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFu2,ZUR1,ierr,test) 
ZUR2 = ZUR1 
Else 
Call EigenSystem(mat2,MFu2,ZUR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(mat,Transpose(Conjg(mat))) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem (Real(mat2,dp),MFu2,ZUL1,ierr,test) 
                  
                  
ZUL2 = ZUL1 
Else 
Call EigenSystem(mat2,MFu2,ZUL2,ierr,test) 
 
 
End If 
ZUL2 = Conjg(ZUL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZUL2),mat),Transpose( Conjg(ZUR2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZUR2(i1,:) = phaseM *ZUR2(i1,:) 
 End if 
End Do 
 
Do i1=1,3
If (Abs(ZUR2(i1,i1)).gt.0._dp) Then 
phaseM = ZUR2(i1,i1) / Abs(ZUR2(i1,i1)) 
ZUR2(i1,:) = Conjg(phaseM) *ZUR2(i1,:) 
 ZUL2(i1,:) = phaseM *ZUL2(i1,:) 
 End if 
  If (MFu2(i1).ne.MFu2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Abs(MFu2(i1)).Le.MaxMassNumericalZero) MFu2(i1) = Abs(MFu2(i1))+1.E-10_dp 
  If (MFu2(i1).Le.0._dp) Then 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,MFu2(i1) 
      Write(*,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(*,*) 'a mass squarred is negative: ',i1,MFu2(i1) 
      Call TerminateProgram 
    End If 
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,MFu2(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,MFu2(i1) 
  MFu2(i1) = 1._dp 
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFu, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


MFu = Sqrt( MFu2 ) 
ZUL = ZUL2 
ZUR = ZUR2 
Iname = Iname - 1 
 
End Subroutine CalculateMFu 

Subroutine CalculateMFe(Ye,vvSM,ZEL,ZER,MFe,kont)

Real(dp),Intent(in) :: vvSM

Complex(dp),Intent(in) :: Ye(3,3)

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MFe(3) 
 Complex(dp), Intent(out) :: ZEL(3,3), ZER(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZEL2(3,3), ZER2(3,3) 
 
 Real(dp) :: ZEL1(3,3), ZER1(3,3), test(2), MFe2(3) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFe'
 
MFe = 0._dp 
ZEL = 0._dp 
ZER = 0._dp 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(vvSM*Ye(1,1))/sqrt(2._dp)
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+(vvSM*Ye(2,1))/sqrt(2._dp)
mat(1,3) = 0._dp 
mat(1,3) = mat(1,3)+(vvSM*Ye(3,1))/sqrt(2._dp)
mat(2,1) = 0._dp 
mat(2,1) = mat(2,1)+(vvSM*Ye(1,2))/sqrt(2._dp)
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(vvSM*Ye(2,2))/sqrt(2._dp)
mat(2,3) = 0._dp 
mat(2,3) = mat(2,3)+(vvSM*Ye(3,2))/sqrt(2._dp)
mat(3,1) = 0._dp 
mat(3,1) = mat(3,1)+(vvSM*Ye(1,3))/sqrt(2._dp)
mat(3,2) = 0._dp 
mat(3,2) = mat(3,2)+(vvSM*Ye(2,3))/sqrt(2._dp)
mat(3,3) = 0._dp 
mat(3,3) = mat(3,3)+(vvSM*Ye(3,3))/sqrt(2._dp)

 
mat2 = Matmul(Transpose(Conjg(mat)),mat) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFe2,ZER1,ierr,test) 
ZER2 = ZER1 
Else 
Call EigenSystem(mat2,MFe2,ZER2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(mat,Transpose(Conjg(mat))) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem (Real(mat2,dp),MFe2,ZEL1,ierr,test) 
                  
                  
ZEL2 = ZEL1 
Else 
Call EigenSystem(mat2,MFe2,ZEL2,ierr,test) 
 
 
End If 
ZEL2 = Conjg(ZEL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZEL2),mat),Transpose( Conjg(ZER2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZER2(i1,:) = phaseM *ZER2(i1,:) 
 End if 
End Do 
 
Do i1=1,3
If (Abs(ZER2(i1,i1)).gt.0._dp) Then 
phaseM = ZER2(i1,i1) / Abs(ZER2(i1,i1)) 
ZER2(i1,:) = Conjg(phaseM) *ZER2(i1,:) 
 ZEL2(i1,:) = phaseM *ZEL2(i1,:) 
 End if 
  If (MFe2(i1).ne.MFe2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Abs(MFe2(i1)).Le.MaxMassNumericalZero) MFe2(i1) = Abs(MFe2(i1))+1.E-10_dp 
  If (MFe2(i1).Le.0._dp) Then 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,MFe2(i1) 
      Write(*,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(*,*) 'a mass squarred is negative: ',i1,MFe2(i1) 
      Call TerminateProgram 
    End If 
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,MFe2(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,MFe2(i1) 
  MFe2(i1) = 1._dp 
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFe, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


MFe = Sqrt( MFe2 ) 
ZEL = ZEL2 
ZER = ZER2 
Iname = Iname - 1 
 
End Subroutine CalculateMFe 

Subroutine CalculateMFv(UV,UVR,MFv,kont)

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MFv(3) 
 Complex(dp), Intent(out) :: UV(3,3), UVR(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: UV2(3,3), UVR2(3,3) 
 
 Real(dp) :: UV1(3,3), UVR1(3,3), test(2), MFv2(3) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFv'
 
MFv = 0._dp 
UV = 0._dp 
UVR = 0._dp 
mat(1,1) = 0._dp 
mat(1,2) = 0._dp 
mat(1,3) = 0._dp 
mat(2,1) = 0._dp 
mat(2,2) = 0._dp 
mat(2,3) = 0._dp 
mat(3,1) = 0._dp 
mat(3,2) = 0._dp 
mat(3,3) = 0._dp 

 
mat2 = Matmul(Transpose(Conjg(mat)),mat) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFv2,UVR1,ierr,test) 
UVR2 = UVR1 
Else 
Call EigenSystem(mat2,MFv2,UVR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(mat,Transpose(Conjg(mat))) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem (Real(mat2,dp),MFv2,UV1,ierr,test) 
                  
                  
UV2 = UV1 
Else 
Call EigenSystem(mat2,MFv2,UV2,ierr,test) 
 
 
End If 
UV2 = Conjg(UV2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(UV2),mat),Transpose( Conjg(UVR2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
UVR2(i1,:) = phaseM *UVR2(i1,:) 
 End if 
End Do 
 
Do i1=1,3
If (Abs(UVR2(i1,i1)).gt.0._dp) Then 
phaseM = UVR2(i1,i1) / Abs(UVR2(i1,i1)) 
UVR2(i1,:) = Conjg(phaseM) *UVR2(i1,:) 
 UV2(i1,:) = phaseM *UV2(i1,:) 
 End if 
  If (MFv2(i1).ne.MFv2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Abs(MFv2(i1)).Le.MaxMassNumericalZero) MFv2(i1) = Abs(MFv2(i1))+1.E-10_dp 
  If (MFv2(i1).Le.0._dp) Then 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,MFv2(i1) 
      Write(*,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(*,*) 'a mass squarred is negative: ',i1,MFv2(i1) 
      Call TerminateProgram 
    End If 
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,MFv2(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,MFv2(i1) 
  MFv2(i1) = 1._dp 
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFv, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


MFv = Sqrt( MFv2 ) 
UV = UV2 
UVR = UVR2 
Iname = Iname - 1 
 
End Subroutine CalculateMFv 

Subroutine CalculateMFxv(MDF,YRD,YRC,vvSM,vS,XV,XU,MFxv,kont)

Real(dp),Intent(in) :: MDF,YRD,YRC,vvSM,vS

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MFxv(2) 
 Complex(dp), Intent(out) :: XV(2,2), XU(2,2) 
 
 Complex(dp) :: mat(2,2)=0._dp, mat2(2,2)=0._dp, phaseM 

Complex(dp) :: XV2(2,2), XU2(2,2) 
 
 Real(dp) :: XV1(2,2), XU1(2,2), test(2), MFxv2(2) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFxv'
 
MFxv = 0._dp 
XV = 0._dp 
XU = 0._dp 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+MDF
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+(vvSM*YRD)/sqrt(2._dp)
mat(2,1) = 0._dp 
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(vS*YRC)/sqrt(2._dp)

 
mat2 = Matmul(Transpose(Conjg(mat)),mat) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFxv2,XU1,ierr,test) 
XU2 = XU1 
Else 
Call EigenSystem(mat2,MFxv2,XU2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(mat,Transpose(Conjg(mat))) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem (Real(mat2,dp),MFxv2,XV1,ierr,test) 
                  
                  
XV2 = XV1 
Else 
Call EigenSystem(mat2,MFxv2,XV2,ierr,test) 
 
 
End If 
XV2 = Conjg(XV2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(XV2),mat),Transpose( Conjg(XU2))) 
Do i1=1,2
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
XU2(i1,:) = phaseM *XU2(i1,:) 
 End if 
End Do 
 
Do i1=1,2
If (Abs(XU2(i1,i1)).gt.0._dp) Then 
phaseM = XU2(i1,i1) / Abs(XU2(i1,i1)) 
XU2(i1,:) = Conjg(phaseM) *XU2(i1,:) 
 XV2(i1,:) = phaseM *XV2(i1,:) 
 End if 
  If (MFxv2(i1).ne.MFxv2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Abs(MFxv2(i1)).Le.MaxMassNumericalZero) MFxv2(i1) = Abs(MFxv2(i1))+1.E-10_dp 
  If (MFxv2(i1).Le.0._dp) Then 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,MFxv2(i1) 
      Write(*,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(*,*) 'a mass squarred is negative: ',i1,MFxv2(i1) 
      Call TerminateProgram 
    End If 
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,MFxv2(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,MFxv2(i1) 
  MFxv2(i1) = 1._dp 
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFxv, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


MFxv = Sqrt( MFxv2 ) 
XV = XV2 
XU = XU2 
Iname = Iname - 1 
 
End Subroutine CalculateMFxv 

Subroutine CalculateMSsc(MS12,MS22,LS1H,LS2H,vvSM,VSs,MSsc,MSsc2,kont)

Real(dp), Intent(in) :: MS12,MS22,LS1H,LS2H,vvSM

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4, pos 
Real(dp), Intent(out) :: MSsc(2), MSsc2(2) 
Real(dp), Intent(out) :: VSs(2,2) 
 
Real(dp) :: mat(2,2)  

Real(dp) ::  test(2) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMSsc'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+MS12
mat(1,1) = mat(1,1)+(LS1H*vvSM**2)/2._dp
mat(1,2) = 0._dp 
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+MS22
mat(2,2) = mat(2,2)+(LS2H*vvSM**2)/2._dp

 
 Do i1=2,2
  Do i2 = 1, i1-1 
  mat(i1,i2) = mat(i2,i1) 
  End do 
End do 

 
Call EigenSystem(mat,MSsc2,VSs,ierr,test) 
 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,2
  If (Abs(MSsc2(i1)).Le.MaxMassNumericalZero) MSsc2(i1) = 1.E-10_dp 
  If (MSsc2(i1).ne.MSsc2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (MSsc2(i1).Ge.0._dp) Then 
  MSsc(i1)=Sqrt(MSsc2(i1) ) 
  Else 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,MSsc2(i1) 
    End If 
  MSsc(i1) = 1._dp 
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,MSsc2(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,MSsc2(i1) 
  MSsc2(i1) = 1._dp 
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
Iname = Iname - 1 
 
End Subroutine CalculateMSsc 

Subroutine CalculateVPVZ(g1,g2,vvSM,ZZ,MVZ,MVZ2,TW,kont)

Real(dp), Intent(in) :: g1,g2,vvSM

Real(dp), Intent(out) :: TW

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MVZ, MVZ2
Real(dp) :: VPVZ2(2),VPVZ(2)  

Real(dp), Intent(out) :: ZZ(2,2) 
 
Real(dp) :: mat(2,2)  

Real(dp) ::  test(2) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateVPVZ'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(g1**2*vvSM**2)/4._dp
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)-(g1*g2*vvSM**2)/4._dp
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(g2**2*vvSM**2)/4._dp

 
 Do i1=2,2
  Do i2 = 1, i1-1 
  mat(i1,i2) = mat(i2,i1) 
  End do 
End do 

 
Call EigenSystem(mat,VPVZ2,ZZ,ierr,test) 
 
 
ZZ = Transpose(ZZ) 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,2
  If (Abs(VPVZ2(i1)).Le.1.E-10_dp*(Maxval(VPVZ2))) VPVZ2(i1) = 1.E-10_dp 
  If (VPVZ2(i1).ne.VPVZ2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (VPVZ2(i1).Ge.0._dp) Then 
  VPVZ(i1) =Sqrt(VPVZ2(i1) ) 
  Else 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,VPVZ2(i1) 
    End If 
  VPVZ(i1)= 1._dp 
  VPVZ2(i1)= 1._dp  
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,VPVZ2(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,VPVZ2(i1) 
  VPVZ(i1)= 1._dp 
  VPVZ2(i1) = 1._dp  
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
 
MVZ = VPVZ(2) 
MVZ2 = VPVZ2(2) 
TW = ACos(Abs(ZZ(1,1)))

 Iname = Iname - 1 
 
End Subroutine CalculateVPVZ 

Subroutine CalculateVWp(g2,vvSM,ZW,MVWp,MVWp2,kont)

Real(dp), Intent(in) :: g2,vvSM

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MVWp, MVWp2
Real(dp) :: VWp2(2),VWp(2)  

Complex(dp), Intent(out) :: ZW(2,2) 
 
Complex(dp) :: mat(2,2)  

Real(dp) ::  test(2) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateVWp'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(g2**2*vvSM**2)/4._dp
mat(1,2) = 0._dp 
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(g2**2*vvSM**2)/4._dp

 
 Do i1=2,2
  Do i2 = 1, i1-1 
  mat(i1,i2) = Conjg(mat(i2,i1)) 
  End do 
End do 

 
Call EigenSystem(mat,VWp2,ZW,ierr,test) 
 
 
ZW = Transpose(ZW) 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,2
  If (Abs(VWp2(i1)).Le.1.E-10_dp*(Maxval(VWp2))) VWp2(i1) = 1.E-10_dp 
  If (VWp2(i1).ne.VWp2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (VWp2(i1).Ge.0._dp) Then 
  VWp(i1) =Sqrt(VWp2(i1) ) 
  Else 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,VWp2(i1) 
    End If 
  VWp(i1)= 1._dp 
  VWp2(i1)= 1._dp  
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,VWp2(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,VWp2(i1) 
  VWp(i1)= 1._dp 
  VWp2(i1) = 1._dp  
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
 
MVWp = VWp(1) 
MVWp2 = VWp2(1) 

 Iname = Iname - 1 
 
End Subroutine CalculateVWp 

Subroutine CalculateMhhEffPot(m2SM,Lam,mP2,LSP,LSPH,vvSM,vS,ZH,Mhh,Mhh2,kont)

Real(dp), Intent(in) :: m2SM,mP2,LSP,LSPH,vvSM,vS

Complex(dp), Intent(in) :: Lam

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4, pos 
Real(dp), Intent(out) :: Mhh(2), Mhh2(2) 
Real(dp), Intent(out) :: ZH(2,2) 
 
Real(dp) :: mat(2,2)  

Real(dp) :: Mhh2temp(2), Q2 
Real(dp) :: ZHtemp(2,2),ZHtemp2(2,2) 
 
Real(dp) ::  test(2) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMhh'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)-1._dp*(m2SM)
mat(1,1) = mat(1,1)+(3*Lam*vvSM**2)/2._dp
mat(1,1) = mat(1,1)+(LSPH*vS**2)/2._dp
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+LSPH*vvSM*vS
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+mP2/2._dp
mat(2,2) = mat(2,2)+(LSPH*vvSM**2)/2._dp
mat(2,2) = mat(2,2)+(3*LSP*vS**2)/2._dp

 
 Do i1=2,2
  Do i2 = 1, i1-1 
  mat(i1,i2) = mat(i2,i1) 
  End do 
End do 

 
Call EigenSystem(mat,Mhh2,ZH,ierr,test) 
 
 
! Fix phases
Do i1=1,2
  pos=Maxloc(Abs(ZH(i1,:)),1)
  If (Real(ZH(i1,pos),dp).lt.0._dp) Then
    ZH(i1,:)=-ZH(i1,:)
  End if
End do
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,2
  If (Mhh2(i1).ne.Mhh2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Mhh2(i1).Ge.0._dp) Then 
  Mhh(i1)=Sqrt(Mhh2(i1) ) 
  Else 
  Mhh(i1) = 1._dp 
! kont = -104 
 End if 
End Do 
Iname = Iname - 1 
 
End Subroutine CalculateMhhEffPot 

Subroutine CalculateMFdEffPot(Yd,vvSM,ZDL,ZDR,MFd,kont)

Real(dp),Intent(in) :: vvSM

Complex(dp),Intent(in) :: Yd(3,3)

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MFd(3) 
 Complex(dp), Intent(out) :: ZDL(3,3), ZDR(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZDL2(3,3), ZDR2(3,3) 
 
 Real(dp) :: ZDL1(3,3), ZDR1(3,3), test(2), MFd2(3) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFd'
 
MFd = 0._dp 
ZDL = 0._dp 
ZDR = 0._dp 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(vvSM*Yd(1,1))/sqrt(2._dp)
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+(vvSM*Yd(2,1))/sqrt(2._dp)
mat(1,3) = 0._dp 
mat(1,3) = mat(1,3)+(vvSM*Yd(3,1))/sqrt(2._dp)
mat(2,1) = 0._dp 
mat(2,1) = mat(2,1)+(vvSM*Yd(1,2))/sqrt(2._dp)
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(vvSM*Yd(2,2))/sqrt(2._dp)
mat(2,3) = 0._dp 
mat(2,3) = mat(2,3)+(vvSM*Yd(3,2))/sqrt(2._dp)
mat(3,1) = 0._dp 
mat(3,1) = mat(3,1)+(vvSM*Yd(1,3))/sqrt(2._dp)
mat(3,2) = 0._dp 
mat(3,2) = mat(3,2)+(vvSM*Yd(2,3))/sqrt(2._dp)
mat(3,3) = 0._dp 
mat(3,3) = mat(3,3)+(vvSM*Yd(3,3))/sqrt(2._dp)

 
mat2 = Matmul(Transpose(Conjg(mat)),mat) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFd2,ZDR1,ierr,test) 
ZDR2 = ZDR1 
Else 
Call EigenSystem(mat2,MFd2,ZDR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(mat,Transpose(Conjg(mat))) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem (Real(mat2,dp),MFd2,ZDL1,ierr,test) 
                  
                  
ZDL2 = ZDL1 
Else 
Call EigenSystem(mat2,MFd2,ZDL2,ierr,test) 
 
 
End If 
ZDL2 = Conjg(ZDL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZDL2),mat),Transpose( Conjg(ZDR2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZDR2(i1,:) = phaseM *ZDR2(i1,:) 
 End if 
End Do 
 
Do i1=1,3
If (Abs(ZDR2(i1,i1)).gt.0._dp) Then 
phaseM = ZDR2(i1,i1) / Abs(ZDR2(i1,i1)) 
ZDR2(i1,:) = Conjg(phaseM) *ZDR2(i1,:) 
 ZDL2(i1,:) = phaseM *ZDL2(i1,:) 
 End if 
  If (MFd2(i1).ne.MFd2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Abs(MFd2(i1)).Le.MaxMassNumericalZero) MFd2(i1) = Abs(MFd2(i1))+1.E-10_dp 
  If (MFd2(i1).Le.0._dp) Then 
! kont = -104 
 End if 
End Do 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFd, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


MFd = Sqrt( MFd2 ) 
ZDL = ZDL2 
ZDR = ZDR2 
Iname = Iname - 1 
 
End Subroutine CalculateMFdEffPot 

Subroutine CalculateMFuEffPot(Yu,vvSM,ZUL,ZUR,MFu,kont)

Real(dp),Intent(in) :: vvSM

Complex(dp),Intent(in) :: Yu(3,3)

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MFu(3) 
 Complex(dp), Intent(out) :: ZUL(3,3), ZUR(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZUL2(3,3), ZUR2(3,3) 
 
 Real(dp) :: ZUL1(3,3), ZUR1(3,3), test(2), MFu2(3) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFu'
 
MFu = 0._dp 
ZUL = 0._dp 
ZUR = 0._dp 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(vvSM*Yu(1,1))/sqrt(2._dp)
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+(vvSM*Yu(2,1))/sqrt(2._dp)
mat(1,3) = 0._dp 
mat(1,3) = mat(1,3)+(vvSM*Yu(3,1))/sqrt(2._dp)
mat(2,1) = 0._dp 
mat(2,1) = mat(2,1)+(vvSM*Yu(1,2))/sqrt(2._dp)
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(vvSM*Yu(2,2))/sqrt(2._dp)
mat(2,3) = 0._dp 
mat(2,3) = mat(2,3)+(vvSM*Yu(3,2))/sqrt(2._dp)
mat(3,1) = 0._dp 
mat(3,1) = mat(3,1)+(vvSM*Yu(1,3))/sqrt(2._dp)
mat(3,2) = 0._dp 
mat(3,2) = mat(3,2)+(vvSM*Yu(2,3))/sqrt(2._dp)
mat(3,3) = 0._dp 
mat(3,3) = mat(3,3)+(vvSM*Yu(3,3))/sqrt(2._dp)

 
mat2 = Matmul(Transpose(Conjg(mat)),mat) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFu2,ZUR1,ierr,test) 
ZUR2 = ZUR1 
Else 
Call EigenSystem(mat2,MFu2,ZUR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(mat,Transpose(Conjg(mat))) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem (Real(mat2,dp),MFu2,ZUL1,ierr,test) 
                  
                  
ZUL2 = ZUL1 
Else 
Call EigenSystem(mat2,MFu2,ZUL2,ierr,test) 
 
 
End If 
ZUL2 = Conjg(ZUL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZUL2),mat),Transpose( Conjg(ZUR2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZUR2(i1,:) = phaseM *ZUR2(i1,:) 
 End if 
End Do 
 
Do i1=1,3
If (Abs(ZUR2(i1,i1)).gt.0._dp) Then 
phaseM = ZUR2(i1,i1) / Abs(ZUR2(i1,i1)) 
ZUR2(i1,:) = Conjg(phaseM) *ZUR2(i1,:) 
 ZUL2(i1,:) = phaseM *ZUL2(i1,:) 
 End if 
  If (MFu2(i1).ne.MFu2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Abs(MFu2(i1)).Le.MaxMassNumericalZero) MFu2(i1) = Abs(MFu2(i1))+1.E-10_dp 
  If (MFu2(i1).Le.0._dp) Then 
! kont = -104 
 End if 
End Do 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFu, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


MFu = Sqrt( MFu2 ) 
ZUL = ZUL2 
ZUR = ZUR2 
Iname = Iname - 1 
 
End Subroutine CalculateMFuEffPot 

Subroutine CalculateMFeEffPot(Ye,vvSM,ZEL,ZER,MFe,kont)

Real(dp),Intent(in) :: vvSM

Complex(dp),Intent(in) :: Ye(3,3)

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MFe(3) 
 Complex(dp), Intent(out) :: ZEL(3,3), ZER(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZEL2(3,3), ZER2(3,3) 
 
 Real(dp) :: ZEL1(3,3), ZER1(3,3), test(2), MFe2(3) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFe'
 
MFe = 0._dp 
ZEL = 0._dp 
ZER = 0._dp 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(vvSM*Ye(1,1))/sqrt(2._dp)
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+(vvSM*Ye(2,1))/sqrt(2._dp)
mat(1,3) = 0._dp 
mat(1,3) = mat(1,3)+(vvSM*Ye(3,1))/sqrt(2._dp)
mat(2,1) = 0._dp 
mat(2,1) = mat(2,1)+(vvSM*Ye(1,2))/sqrt(2._dp)
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(vvSM*Ye(2,2))/sqrt(2._dp)
mat(2,3) = 0._dp 
mat(2,3) = mat(2,3)+(vvSM*Ye(3,2))/sqrt(2._dp)
mat(3,1) = 0._dp 
mat(3,1) = mat(3,1)+(vvSM*Ye(1,3))/sqrt(2._dp)
mat(3,2) = 0._dp 
mat(3,2) = mat(3,2)+(vvSM*Ye(2,3))/sqrt(2._dp)
mat(3,3) = 0._dp 
mat(3,3) = mat(3,3)+(vvSM*Ye(3,3))/sqrt(2._dp)

 
mat2 = Matmul(Transpose(Conjg(mat)),mat) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFe2,ZER1,ierr,test) 
ZER2 = ZER1 
Else 
Call EigenSystem(mat2,MFe2,ZER2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(mat,Transpose(Conjg(mat))) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem (Real(mat2,dp),MFe2,ZEL1,ierr,test) 
                  
                  
ZEL2 = ZEL1 
Else 
Call EigenSystem(mat2,MFe2,ZEL2,ierr,test) 
 
 
End If 
ZEL2 = Conjg(ZEL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZEL2),mat),Transpose( Conjg(ZER2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZER2(i1,:) = phaseM *ZER2(i1,:) 
 End if 
End Do 
 
Do i1=1,3
If (Abs(ZER2(i1,i1)).gt.0._dp) Then 
phaseM = ZER2(i1,i1) / Abs(ZER2(i1,i1)) 
ZER2(i1,:) = Conjg(phaseM) *ZER2(i1,:) 
 ZEL2(i1,:) = phaseM *ZEL2(i1,:) 
 End if 
  If (MFe2(i1).ne.MFe2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Abs(MFe2(i1)).Le.MaxMassNumericalZero) MFe2(i1) = Abs(MFe2(i1))+1.E-10_dp 
  If (MFe2(i1).Le.0._dp) Then 
! kont = -104 
 End if 
End Do 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFe, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


MFe = Sqrt( MFe2 ) 
ZEL = ZEL2 
ZER = ZER2 
Iname = Iname - 1 
 
End Subroutine CalculateMFeEffPot 

Subroutine CalculateMFvEffPot(UV,UVR,MFv,kont)

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MFv(3) 
 Complex(dp), Intent(out) :: UV(3,3), UVR(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: UV2(3,3), UVR2(3,3) 
 
 Real(dp) :: UV1(3,3), UVR1(3,3), test(2), MFv2(3) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFv'
 
MFv = 0._dp 
UV = 0._dp 
UVR = 0._dp 
mat(1,1) = 0._dp 
mat(1,2) = 0._dp 
mat(1,3) = 0._dp 
mat(2,1) = 0._dp 
mat(2,2) = 0._dp 
mat(2,3) = 0._dp 
mat(3,1) = 0._dp 
mat(3,2) = 0._dp 
mat(3,3) = 0._dp 

 
mat2 = Matmul(Transpose(Conjg(mat)),mat) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFv2,UVR1,ierr,test) 
UVR2 = UVR1 
Else 
Call EigenSystem(mat2,MFv2,UVR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(mat,Transpose(Conjg(mat))) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem (Real(mat2,dp),MFv2,UV1,ierr,test) 
                  
                  
UV2 = UV1 
Else 
Call EigenSystem(mat2,MFv2,UV2,ierr,test) 
 
 
End If 
UV2 = Conjg(UV2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(UV2),mat),Transpose( Conjg(UVR2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
UVR2(i1,:) = phaseM *UVR2(i1,:) 
 End if 
End Do 
 
Do i1=1,3
If (Abs(UVR2(i1,i1)).gt.0._dp) Then 
phaseM = UVR2(i1,i1) / Abs(UVR2(i1,i1)) 
UVR2(i1,:) = Conjg(phaseM) *UVR2(i1,:) 
 UV2(i1,:) = phaseM *UV2(i1,:) 
 End if 
  If (MFv2(i1).ne.MFv2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Abs(MFv2(i1)).Le.MaxMassNumericalZero) MFv2(i1) = Abs(MFv2(i1))+1.E-10_dp 
  If (MFv2(i1).Le.0._dp) Then 
! kont = -104 
 End if 
End Do 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFv, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


MFv = Sqrt( MFv2 ) 
UV = UV2 
UVR = UVR2 
Iname = Iname - 1 
 
End Subroutine CalculateMFvEffPot 

Subroutine CalculateMFxvEffPot(MDF,YRD,YRC,vvSM,vS,XV,XU,MFxv,kont)

Real(dp),Intent(in) :: MDF,YRD,YRC,vvSM,vS

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MFxv(2) 
 Complex(dp), Intent(out) :: XV(2,2), XU(2,2) 
 
 Complex(dp) :: mat(2,2)=0._dp, mat2(2,2)=0._dp, phaseM 

Complex(dp) :: XV2(2,2), XU2(2,2) 
 
 Real(dp) :: XV1(2,2), XU1(2,2), test(2), MFxv2(2) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFxv'
 
MFxv = 0._dp 
XV = 0._dp 
XU = 0._dp 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+MDF
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+(vvSM*YRD)/sqrt(2._dp)
mat(2,1) = 0._dp 
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(vS*YRC)/sqrt(2._dp)

 
mat2 = Matmul(Transpose(Conjg(mat)),mat) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFxv2,XU1,ierr,test) 
XU2 = XU1 
Else 
Call EigenSystem(mat2,MFxv2,XU2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(mat,Transpose(Conjg(mat))) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem (Real(mat2,dp),MFxv2,XV1,ierr,test) 
                  
                  
XV2 = XV1 
Else 
Call EigenSystem(mat2,MFxv2,XV2,ierr,test) 
 
 
End If 
XV2 = Conjg(XV2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(XV2),mat),Transpose( Conjg(XU2))) 
Do i1=1,2
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
XU2(i1,:) = phaseM *XU2(i1,:) 
 End if 
End Do 
 
Do i1=1,2
If (Abs(XU2(i1,i1)).gt.0._dp) Then 
phaseM = XU2(i1,i1) / Abs(XU2(i1,i1)) 
XU2(i1,:) = Conjg(phaseM) *XU2(i1,:) 
 XV2(i1,:) = phaseM *XV2(i1,:) 
 End if 
  If (MFxv2(i1).ne.MFxv2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Abs(MFxv2(i1)).Le.MaxMassNumericalZero) MFxv2(i1) = Abs(MFxv2(i1))+1.E-10_dp 
  If (MFxv2(i1).Le.0._dp) Then 
! kont = -104 
 End if 
End Do 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFxv, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


MFxv = Sqrt( MFxv2 ) 
XV = XV2 
XU = XU2 
Iname = Iname - 1 
 
End Subroutine CalculateMFxvEffPot 

Subroutine CalculateMSscEffPot(MS12,MS22,LS1H,LS2H,vvSM,VSs,MSsc,MSsc2,kont)

Real(dp), Intent(in) :: MS12,MS22,LS1H,LS2H,vvSM

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4, pos 
Real(dp), Intent(out) :: MSsc(2), MSsc2(2) 
Real(dp), Intent(out) :: VSs(2,2) 
 
Real(dp) :: mat(2,2)  

Real(dp) :: MSsc2temp(2), Q2 
Real(dp) :: VSstemp(2,2),VSstemp2(2,2) 
 
Real(dp) ::  test(2) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMSsc'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+MS12
mat(1,1) = mat(1,1)+(LS1H*vvSM**2)/2._dp
mat(1,2) = 0._dp 
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+MS22
mat(2,2) = mat(2,2)+(LS2H*vvSM**2)/2._dp

 
 Do i1=2,2
  Do i2 = 1, i1-1 
  mat(i1,i2) = mat(i2,i1) 
  End do 
End do 

 
Call EigenSystem(mat,MSsc2,VSs,ierr,test) 
 
 
! Fix phases
Do i1=1,2
  pos=Maxloc(Abs(VSs(i1,:)),1)
  If (Real(VSs(i1,pos),dp).lt.0._dp) Then
    VSs(i1,:)=-VSs(i1,:)
  End if
End do
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,2
  If (MSsc2(i1).ne.MSsc2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (MSsc2(i1).Ge.0._dp) Then 
  MSsc(i1)=Sqrt(MSsc2(i1) ) 
  Else 
  MSsc(i1) = 1._dp 
! kont = -104 
 End if 
End Do 
Iname = Iname - 1 
 
End Subroutine CalculateMSscEffPot 

Subroutine CalculateVPVZEffPot(g1,g2,vvSM,ZZ,MVZ,MVZ2,TW,kont)

Real(dp), Intent(in) :: g1,g2,vvSM

Real(dp), Intent(out) :: TW

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MVZ, MVZ2
Real(dp) :: VPVZ2(2),VPVZ(2)  

Real(dp), Intent(out) :: ZZ(2,2) 
 
Real(dp) :: mat(2,2)  

Real(dp) ::  test(2) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateVPVZ'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(g1**2*vvSMFix**2)/4._dp
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)-(g1*g2*vvSMFix**2)/4._dp
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(g2**2*vvSMFix**2)/4._dp

 
 Do i1=2,2
  Do i2 = 1, i1-1 
  mat(i1,i2) = mat(i2,i1) 
  End do 
End do 

 
Call EigenSystem(mat,VPVZ2,ZZ,ierr,test) 
 
 
ZZ = Transpose(ZZ) 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,2
  If (Abs(VPVZ2(i1)).Le.1.E-10_dp*(Maxval(VPVZ2))) VPVZ2(i1) = 1.E-10_dp 
  If (VPVZ2(i1).ne.VPVZ2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (VPVZ2(i1).Ge.0._dp) Then 
  VPVZ(i1) =Sqrt(VPVZ2(i1) ) 
  Else 
  VPVZ(i1)= 1._dp 
  VPVZ(i1)= 1._dp 
! kont = -104 
 End if 
End Do 
 
MVZ = VPVZ(2) 
MVZ2 = VPVZ2(2) 

 Iname = Iname - 1 
 
End Subroutine CalculateVPVZEffPot 

Subroutine CalculateVWpEffPot(g2,vvSM,ZW,MVWp,MVWp2,kont)

Real(dp), Intent(in) :: g2,vvSM

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MVWp, MVWp2
Real(dp) :: VWp2(2),VWp(2)  

Complex(dp), Intent(out) :: ZW(2,2) 
 
Complex(dp) :: mat(2,2)  

Real(dp) ::  test(2) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateVWp'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(g2**2*vvSMFix**2)/4._dp
mat(1,2) = 0._dp 
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(g2**2*vvSMFix**2)/4._dp

 
 Do i1=2,2
  Do i2 = 1, i1-1 
  mat(i1,i2) = Conjg(mat(i2,i1)) 
  End do 
End do 

 
Call EigenSystem(mat,VWp2,ZW,ierr,test) 
 
 
ZW = Transpose(ZW) 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,2
  If (Abs(VWp2(i1)).Le.1.E-10_dp*(Maxval(VWp2))) VWp2(i1) = 1.E-10_dp 
  If (VWp2(i1).ne.VWp2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (VWp2(i1).Ge.0._dp) Then 
  VWp(i1) =Sqrt(VWp2(i1) ) 
  Else 
  VWp(i1)= 1._dp 
  VWp(i1)= 1._dp 
! kont = -104 
 End if 
End Do 
 
MVWp = VWp(1) 
MVWp2 = VWp2(1) 

 Iname = Iname - 1 
 
End Subroutine CalculateVWpEffPot 

Subroutine TreeMassesSM(MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MVWp,MVWp2,               & 
& MVZ,MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,ZUL,UV,UVR,ZW,ZZ,vvSM,vS,g1,g2,g3,Lam,LS1H,            & 
& LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,              & 
& mP2,GenerationMixing,kont)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2

Complex(dp),Intent(in) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Real(dp),Intent(out) :: MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MVWp,MVWp2,               & 
& MVZ,MVZ2,TW,ZZ(2,2)

Complex(dp),Intent(out) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),ZW(2,2)

Real(dp),Intent(in) :: vvSM,vS

Logical, Intent(in) :: GenerationMixing 
Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,kontSave 
Iname = Iname + 1 
NameOfUnit(Iname) = 'TreeMassesSDdiracDM'
 
kont = 0 
Call CalculateMFd(Yd,vvSM,ZDL,ZDR,MFd,kont)

MFd2 = MFd**2 
Call CalculateMFu(Yu,vvSM,ZUL,ZUR,MFu,kont)

MFu2 = MFu**2 
Call CalculateMFe(Ye,vvSM,ZEL,ZER,MFe,kont)

MFe2 = MFe**2 
Call CalculateMFv(UV,UVR,MFv,kont)

MFv2 = MFv**2 

 
 Call CalculateVPVZ(g1,g2,vvSM,ZZ,MVZ,MVZ2,TW,kont)

Call CalculateVWp(g2,vvSM,ZW,MVWp,MVWp2,kont)

Iname = Iname - 1 
 
End Subroutine  TreeMassesSM 
 
 
Subroutine SortGoldstones(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,               & 
& MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,             & 
& ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,kont)

Real(dp),Intent(inout) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp),Intent(inout) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Integer, Intent(inout) :: kont 
Integer :: i1, i2, pos 


End subroutine SortGoldstones 


End Module TreeLevelMasses_SDdiracDM 
 
