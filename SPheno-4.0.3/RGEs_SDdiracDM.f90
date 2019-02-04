! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:20 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module RGEs_SDdiracDM 
 
Use Control 
Use Settings 
Use Model_Data_SDdiracDM 
Use Mathematics 
 
Logical,Private,Save::OnlyDiagonal

Real(dp),Parameter::id3R(3,3)=& 
   & Reshape(Source=(/& 
   & 1,0,0,& 
 &0,1,0,& 
 &0,0,1& 
 &/),shape=(/3,3/)) 
Contains 


Subroutine GToParameters83(g,g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,             & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2)

Implicit None 
Real(dp), Intent(in) :: g(83) 
Real(dp),Intent(out) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2

Complex(dp),Intent(out) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Integer i1, i2, i3, i4, SumI 
 
Iname = Iname +1 
NameOfUnit(Iname) = 'GToParameters83' 
 
g1= g(1) 
g2= g(2) 
g3= g(3) 
Lam= Cmplx(g(4),g(5),dp) 
LS1H= g(6) 
LS= g(7) 
LS2H= g(8) 
LSP= g(9) 
LSPH= g(10) 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Yu(i1,i2) = Cmplx( g(SumI+11), g(SumI+12), dp) 
End Do 
 End Do 
 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Yd(i1,i2) = Cmplx( g(SumI+29), g(SumI+30), dp) 
End Do 
 End Do 
 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Ye(i1,i2) = Cmplx( g(SumI+47), g(SumI+48), dp) 
End Do 
 End Do 
 
YRD= g(65) 
Do i1 = 1,3
SumI = (i1-1) 
YRB1(i1) =  g(SumI+66) 
End Do 
 
Do i1 = 1,3
SumI = (i1-1) 
YRB2(i1) =  g(SumI+69) 
End Do 
 
YRC= g(72) 
Do i1 = 1,3
SumI = (i1-1) 
YRA1(i1) =  g(SumI+73) 
End Do 
 
Do i1 = 1,3
SumI = (i1-1) 
YRA2(i1) =  g(SumI+76) 
End Do 
 
MDF= g(79) 
m2SM= g(80) 
MS12= g(81) 
MS22= g(82) 
mP2= g(83) 
Do i1=1,83 
If (g(i1).ne.g(i1)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Write(*,*) "At position ", i1 
 Call TerminateProgram 
End if 
End do 
Iname = Iname - 1 
 
End Subroutine GToParameters83

Subroutine ParametersToG83(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,               & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,g)

Implicit None 
Real(dp), Intent(out) :: g(83) 
Real(dp), Intent(in) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2

Complex(dp), Intent(in) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Integer i1, i2, i3, i4, SumI 
 
Iname = Iname +1 
NameOfUnit(Iname) = 'ParametersToG83' 
 
g(1) = g1  
g(2) = g2  
g(3) = g3  
g(4) = Real(Lam,dp)  
g(5) = Aimag(Lam)  
g(6) = LS1H  
g(7) = LS  
g(8) = LS2H  
g(9) = LSP  
g(10) = LSPH  
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+11) = Real(Yu(i1,i2), dp) 
g(SumI+12) = Aimag(Yu(i1,i2)) 
End Do 
End Do 

Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+29) = Real(Yd(i1,i2), dp) 
g(SumI+30) = Aimag(Yd(i1,i2)) 
End Do 
End Do 

Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+47) = Real(Ye(i1,i2), dp) 
g(SumI+48) = Aimag(Ye(i1,i2)) 
End Do 
End Do 

g(65) = YRD  
Do i1 = 1,3
SumI = (i1-1) 
g(SumI+66) = YRB1(i1) 
End Do 

Do i1 = 1,3
SumI = (i1-1) 
g(SumI+69) = YRB2(i1) 
End Do 

g(72) = YRC  
Do i1 = 1,3
SumI = (i1-1) 
g(SumI+73) = YRA1(i1) 
End Do 

Do i1 = 1,3
SumI = (i1-1) 
g(SumI+76) = YRA2(i1) 
End Do 

g(79) = MDF  
g(80) = m2SM  
g(81) = MS12  
g(82) = MS22  
g(83) = mP2  
Iname = Iname - 1 
 
End Subroutine ParametersToG83

Subroutine rge83(len, T, GY, F) 
Implicit None 
Integer, Intent(in) :: len 
Real(dp), Intent(in) :: T, GY(len) 
Real(dp), Intent(out) :: F(len) 
Integer :: i1,i2,i3,i4 
Integer :: j1,j2,j3,j4,j5,j6,j7 
Real(dp) :: q 
Real(dp) :: g1,betag11,betag12,Dg1,g2,betag21,betag22,Dg2,g3,betag31,betag32,         & 
& Dg3,LS1H,betaLS1H1,betaLS1H2,DLS1H,LS,betaLS1,betaLS2,DLS,LS2H,betaLS2H1,              & 
& betaLS2H2,DLS2H,LSP,betaLSP1,betaLSP2,DLSP,LSPH,betaLSPH1,betaLSPH2,DLSPH,             & 
& YRD,betaYRD1,betaYRD2,DYRD,YRB1(3),betaYRB11(3),betaYRB12(3),DYRB1(3),YRB2(3)          & 
& ,betaYRB21(3),betaYRB22(3),DYRB2(3),YRC,betaYRC1,betaYRC2,DYRC,YRA1(3),betaYRA11(3)    & 
& ,betaYRA12(3),DYRA1(3),YRA2(3),betaYRA21(3),betaYRA22(3),DYRA2(3),MDF,betaMDF1,        & 
& betaMDF2,DMDF,m2SM,betam2SM1,betam2SM2,Dm2SM,MS12,betaMS121,betaMS122,DMS12,           & 
& MS22,betaMS221,betaMS222,DMS22,mP2,betamP21,betamP22,DmP2
Complex(dp) :: Lam,betaLam1,betaLam2,DLam,Yu(3,3),betaYu1(3,3),betaYu2(3,3)           & 
& ,DYu(3,3),adjYu(3,3),Yd(3,3),betaYd1(3,3),betaYd2(3,3),DYd(3,3),adjYd(3,3)             & 
& ,Ye(3,3),betaYe1(3,3),betaYe2(3,3),DYe(3,3),adjYe(3,3)
Complex(dp) :: YdadjYd(3,3),YeYRA1(3),YeYRA2(3),YeadjYe(3,3),YuadjYu(3,3),adjYdYd(3,3),              & 
& adjYeYe(3,3),adjYuYu(3,3),CYeYRA1(3),CYeYRA2(3),YdadjYdYd(3,3),YdadjYuYu(3,3),         & 
& YeadjYeYe(3,3),YuadjYdYd(3,3),YuadjYuYu(3,3),adjYdYdadjYd(3,3),adjYeYeYRA1(3),         & 
& adjYeYeYRA2(3),adjYeYeadjYe(3,3),adjYuYuadjYu(3,3),TpYeCYeYRA1(3),TpYeCYeYRA2(3),      & 
& YdadjYdYdadjYd(3,3),YeadjYeYeadjYe(3,3),YuadjYuYuadjYu(3,3)

Complex(dp) :: TrYdadjYd,TrYeadjYe,TrYuadjYu,TrYdadjYdYdadjYd,TrYeadjYeYeadjYe,TrYuadjYuYuadjYu

Real(dp) :: SPYRA1xxYRA1,SPYRA1xxYRA2,SPYRA1xxadjYeYeYRA1,SPYRA2xxYRA1,SPYRA2xxYRA2,              & 
& SPYRA2xxadjYeYeYRA2,SPYRB1xxYRB1,SPYRB1xxYRB2,SPYRB2xxYRB1,SPYRB2xxYRB2

Real(dp) :: g1p2,g1p3,g1p4,g2p2,g2p3,g2p4,g3p2,g3p3,LSp2,LS1Hp2,LS2Hp2,LSPp2,LSPHp2,              & 
& MDFp2,YRCp2,YRCp4,YRDp2,YRDp4

Complex(dp) :: Lamp2,SPYRA1xxYRA1p2,SPYRB1xxYRB1p2

Complex(dp) :: DyYeYRA1i1YRA1i2(3,3),DyYeYRA2i1YRA2i2(3,3)

Iname = Iname +1 
NameOfUnit(Iname) = 'rge83' 
 
OnlyDiagonal = .Not.GenerationMixing 
q = t 
 
Call GToParameters83(gy,g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,              & 
& YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2)

Call Adjungate(Yu,adjYu)
Call Adjungate(Yd,adjYd)
Call Adjungate(Ye,adjYe)
 YdadjYd = Matmul(Yd,adjYd) 
Forall(i2=1:3)  YdadjYd(i2,i2) =  Real(YdadjYd(i2,i2),dp) 
 YeYRA1 = Matmul(Ye,YRA1) 
 YeYRA2 = Matmul(Ye,YRA2) 
 YeadjYe = Matmul(Ye,adjYe) 
Forall(i2=1:3)  YeadjYe(i2,i2) =  Real(YeadjYe(i2,i2),dp) 
 YuadjYu = Matmul(Yu,adjYu) 
Forall(i2=1:3)  YuadjYu(i2,i2) =  Real(YuadjYu(i2,i2),dp) 
 adjYdYd = Matmul(adjYd,Yd) 
Forall(i2=1:3)  adjYdYd(i2,i2) =  Real(adjYdYd(i2,i2),dp) 
 adjYeYe = Matmul(adjYe,Ye) 
Forall(i2=1:3)  adjYeYe(i2,i2) =  Real(adjYeYe(i2,i2),dp) 
 adjYuYu = Matmul(adjYu,Yu) 
Forall(i2=1:3)  adjYuYu(i2,i2) =  Real(adjYuYu(i2,i2),dp) 
 CYeYRA1 = Matmul(Conjg(Ye),YRA1) 
 CYeYRA2 = Matmul(Conjg(Ye),YRA2) 
 YdadjYdYd = Matmul(Yd,adjYdYd) 
 YdadjYuYu = Matmul(Yd,adjYuYu) 
 YeadjYeYe = Matmul(Ye,adjYeYe) 
 YuadjYdYd = Matmul(Yu,adjYdYd) 
 YuadjYuYu = Matmul(Yu,adjYuYu) 
 adjYdYdadjYd = Matmul(adjYd,YdadjYd) 
 adjYeYeYRA1 = Matmul(adjYe,YeYRA1) 
 adjYeYeYRA2 = Matmul(adjYe,YeYRA2) 
 adjYeYeadjYe = Matmul(adjYe,YeadjYe) 
 adjYuYuadjYu = Matmul(adjYu,YuadjYu) 
 TpYeCYeYRA1 = Matmul(Transpose(Ye),CYeYRA1) 
 TpYeCYeYRA2 = Matmul(Transpose(Ye),CYeYRA2) 
 YdadjYdYdadjYd = Matmul(Yd,adjYdYdadjYd) 
Forall(i2=1:3)  YdadjYdYdadjYd(i2,i2) =  Real(YdadjYdYdadjYd(i2,i2),dp) 
 YeadjYeYeadjYe = Matmul(Ye,adjYeYeadjYe) 
Forall(i2=1:3)  YeadjYeYeadjYe(i2,i2) =  Real(YeadjYeYeadjYe(i2,i2),dp) 
 YuadjYuYuadjYu = Matmul(Yu,adjYuYuadjYu) 
Forall(i2=1:3)  YuadjYuYuadjYu(i2,i2) =  Real(YuadjYuYuadjYu(i2,i2),dp) 
 TrYdadjYd = Real(cTrace(YdadjYd),dp) 
 TrYeadjYe = Real(cTrace(YeadjYe),dp) 
 TrYuadjYu = Real(cTrace(YuadjYu),dp) 
 TrYdadjYdYdadjYd = Real(cTrace(YdadjYdYdadjYd),dp) 
 TrYeadjYeYeadjYe = Real(cTrace(YeadjYeYeadjYe),dp) 
 TrYuadjYuYuadjYu = Real(cTrace(YuadjYuYuadjYu),dp) 
 SPYRA1xxYRA1 = DOT_PRODUCT(YRA1,YRA1) 
 SPYRA1xxYRA2 = DOT_PRODUCT(YRA1,YRA2) 
 SPYRA1xxadjYeYeYRA1 = DOT_PRODUCT(YRA1,adjYeYeYRA1) 
 SPYRA2xxYRA1 = DOT_PRODUCT(YRA2,YRA1) 
 SPYRA2xxYRA2 = DOT_PRODUCT(YRA2,YRA2) 
 SPYRA2xxadjYeYeYRA2 = DOT_PRODUCT(YRA2,adjYeYeYRA2) 
 SPYRB1xxYRB1 = DOT_PRODUCT(YRB1,YRB1) 
 SPYRB1xxYRB2 = DOT_PRODUCT(YRB1,YRB2) 
 SPYRB2xxYRB1 = DOT_PRODUCT(YRB2,YRB1) 
 SPYRB2xxYRB2 = DOT_PRODUCT(YRB2,YRB2) 
 g1p2 =g1**2 
 g1p3 =g1**3 
 g1p4 =g1**4 
 g2p2 =g2**2 
 g2p3 =g2**3 
 g2p4 =g2**4 
 g3p2 =g3**2 
 g3p3 =g3**3 
 Lamp2 =Lam**2 
 LSp2 =LS**2 
 LS1Hp2 =LS1H**2 
 LS2Hp2 =LS2H**2 
 LSPp2 =LSP**2 
 LSPHp2 =LSPH**2 
 MDFp2 =MDF**2 
 YRCp2 =YRC**2 
 YRCp4 =YRC**4 
 YRDp2 =YRD**2 
 YRDp4 =YRD**4 
 SPYRA1xxYRA1p2 =SPYRA1xxYRA1**2 
 SPYRB1xxYRB1p2 =SPYRB1xxYRB1**2 
Do i1=1,3
  Do i2=1,3
DyYeYRA1i1YRA1i2(i1,i2) = YeYRA1(i1)*YRA1(i2) 
  End Do 
End Do 
Do i1=1,3
  Do i2=1,3
DyYeYRA2i1YRA2i2(i1,i2) = YeYRA2(i1)*YRA2(i2) 
  End Do 
End Do 


If (TwoLoopRGE) Then 
End If 
 
 
!-------------------- 
! g1 
!-------------------- 
 
betag11  = 9._dp*(g1p3)/2._dp

 
 
If (TwoLoopRGE) Then 
betag12 = 0

 
Dg1 = oo16pi2*( betag11 + oo16pi2 * betag12 ) 

 
Else 
Dg1 = oo16pi2* betag11 
End If 
 
 
!-------------------- 
! g2 
!-------------------- 
 
betag21  = -5._dp*(g2p3)/2._dp

 
 
If (TwoLoopRGE) Then 
betag22 = 0

 
Dg2 = oo16pi2*( betag21 + oo16pi2 * betag22 ) 

 
Else 
Dg2 = oo16pi2* betag21 
End If 
 
 
!-------------------- 
! g3 
!-------------------- 
 
betag31  = -7._dp*(g3p3)

 
 
If (TwoLoopRGE) Then 
betag32 = 0

 
Dg3 = oo16pi2*( betag31 + oo16pi2 * betag32 ) 

 
Else 
Dg3 = oo16pi2* betag31 
End If 
 
 
!-------------------- 
! Lam 
!-------------------- 
 
betaLam1  = 27._dp*(g1p4)/100._dp + 9._dp*(g2p4)/4._dp + (9*g1p2*(g2p2 -              & 
&  2._dp*(Lam)))/10._dp - 9*g2p2*Lam + 12._dp*(Lamp2) + 2._dp*(LS1Hp2) + 2._dp*(LS2Hp2)  & 
&  + 4._dp*(LSPHp2) + 12*Lam*TrYdadjYd - 12._dp*(TrYdadjYdYdadjYd) + 4*Lam*TrYeadjYe -   & 
&  4._dp*(TrYeadjYeYeadjYe) + 12*Lam*TrYuadjYu - 12._dp*(TrYuadjYuYuadjYu)               & 
&  + 4*Lam*YRDp2 - 4._dp*(YRDp4)

 
 
If (TwoLoopRGE) Then 
betaLam2 = 0

 
DLam = oo16pi2*( betaLam1 + oo16pi2 * betaLam2 ) 

 
Else 
DLam = oo16pi2* betaLam1 
End If 
 
 
Call Chop(DLam) 

!-------------------- 
! LS1H 
!-------------------- 
 
betaLS1H1  = (-9*g1p2*LS1H)/10._dp - (9*g2p2*LS1H)/2._dp + 2*(3*Lam*LS1H +            & 
&  2*LS*LS1H + 2._dp*(LS1Hp2) - 2._dp*(SPYRA1xxadjYeYeYRA1) + 2*LS1H*SPYRA1xxYRA1 +      & 
&  LS1H*SPYRB1xxYRB1 + 3*LS1H*TrYdadjYd + LS1H*TrYeadjYe + 3*LS1H*TrYuadjYu +            & 
&  LS1H*YRDp2)

 
 
If (TwoLoopRGE) Then 
betaLS1H2 = 0

 
DLS1H = oo16pi2*( betaLS1H1 + oo16pi2 * betaLS1H2 ) 

 
Else 
DLS1H = oo16pi2* betaLS1H1 
End If 
 
 
!-------------------- 
! LS 
!-------------------- 
 
betaLS1  = 4._dp*(LS1Hp2) + 10._dp*(LSp2) + 8*LS*SPYRA1xxYRA1 - 8._dp*(SPYRA1xxYRA1p2)& 
&  + 4*LS*SPYRB1xxYRB1 - 4._dp*(SPYRB1xxYRB1p2)

 
 
If (TwoLoopRGE) Then 
betaLS2 = 0

 
DLS = oo16pi2*( betaLS1 + oo16pi2 * betaLS2 ) 

 
Else 
DLS = oo16pi2* betaLS1 
End If 
 
 
!-------------------- 
! LS2H 
!-------------------- 
 
betaLS2H1  = (-9*g1p2*LS2H)/10._dp - (9*g2p2*LS2H)/2._dp + 2*(3*Lam*LS2H +            & 
&  2*LS*LS2H + 2._dp*(LS2Hp2) - 2._dp*(SPYRA2xxadjYeYeYRA2) + 2*LS2H*SPYRA2xxYRA2 +      & 
&  LS2H*SPYRB2xxYRB2 + 3*LS2H*TrYdadjYd + LS2H*TrYeadjYe + 3*LS2H*TrYuadjYu +            & 
&  LS2H*YRDp2)

 
 
If (TwoLoopRGE) Then 
betaLS2H2 = 0

 
DLS2H = oo16pi2*( betaLS2H1 + oo16pi2 * betaLS2H2 ) 

 
Else 
DLS2H = oo16pi2* betaLS2H1 
End If 
 
 
!-------------------- 
! LSP 
!-------------------- 
 
betaLSP1  = 4*(LSPHp2 + 9._dp*(LSPp2) + 2*LSP*YRCp2 - YRCp4)

 
 
If (TwoLoopRGE) Then 
betaLSP2 = 0

 
DLSP = oo16pi2*( betaLSP1 + oo16pi2 * betaLSP2 ) 

 
Else 
DLSP = oo16pi2* betaLSP1 
End If 
 
 
!-------------------- 
! LSPH 
!-------------------- 
 
betaLSPH1  = (-9*g1p2*LSPH)/10._dp - (9*g2p2*LSPH)/2._dp + 2*(3*Lam*LSPH +            & 
&  6*LSP*LSPH + 4._dp*(LSPHp2) + 3*LSPH*TrYdadjYd + LSPH*TrYeadjYe + 3*LSPH*TrYuadjYu +  & 
&  2*LSPH*YRCp2 + LSPH*YRDp2 - 2*YRCp2*YRDp2)

 
 
If (TwoLoopRGE) Then 
betaLSPH2 = 0

 
DLSPH = oo16pi2*( betaLSPH1 + oo16pi2 * betaLSPH2 ) 

 
Else 
DLSPH = oo16pi2* betaLSPH1 
End If 
 
 
!-------------------- 
! Yu 
!-------------------- 
 
betaYu1  = (-17._dp*(g1p2)/20._dp - 9._dp*(g2p2)/4._dp - 8._dp*(g3p2) +               & 
&  3._dp*(TrYdadjYd) + TrYeadjYe + 3._dp*(TrYuadjYu) + YRDp2)*Yu - (3*(YuadjYdYd -       & 
&  YuadjYuYu))/2._dp

 
 
If (TwoLoopRGE) Then 
betaYu2 = 0

 
DYu = oo16pi2*( betaYu1 + oo16pi2 * betaYu2 ) 

 
Else 
DYu = oo16pi2* betaYu1 
End If 
 
 
Call Chop(DYu) 

!-------------------- 
! Yd 
!-------------------- 
 
betaYd1  = (3*(YdadjYdYd - YdadjYuYu))/2._dp + Yd*(-1._dp*(g1p2)/4._dp -              & 
&  9._dp*(g2p2)/4._dp - 8._dp*(g3p2) + 3._dp*(TrYdadjYd) + TrYeadjYe + 3._dp*(TrYuadjYu) + YRDp2)

 
 
If (TwoLoopRGE) Then 
betaYd2 = 0

 
DYd = oo16pi2*( betaYd1 + oo16pi2 * betaYd2 ) 

 
Else 
DYd = oo16pi2* betaYd1 
End If 
 
 
Call Chop(DYd) 

!-------------------- 
! Ye 
!-------------------- 
 
betaYe1  = (2._dp*(DyYeYRA1i1YRA1i2) + 2._dp*(DyYeYRA2i1YRA2i2) - 9*g1p2*Ye -         & 
&  9*g2p2*Ye + 12*TrYdadjYd*Ye + 4*TrYeadjYe*Ye + 12*TrYuadjYu*Ye + 6._dp*(YeadjYeYe)    & 
&  + 4*Ye*YRDp2)/4._dp

 
 
If (TwoLoopRGE) Then 
betaYe2 = 0

 
DYe = oo16pi2*( betaYe1 + oo16pi2 * betaYe2 ) 

 
Else 
DYe = oo16pi2* betaYe1 
End If 
 
 
Call Chop(DYe) 

!-------------------- 
! YRD 
!-------------------- 
 
betaYRD1  = (YRD*(-9._dp*(g1p2) + 5*(-9._dp*(g2p2) + 2*(6._dp*(TrYdadjYd)             & 
&  + 2._dp*(TrYeadjYe) + 6._dp*(TrYuadjYu) + YRCp2 + 5*YRD**2))))/20._dp

 
 
If (TwoLoopRGE) Then 
betaYRD2 = 0

 
DYRD = oo16pi2*( betaYRD1 + oo16pi2 * betaYRD2 ) 

 
Else 
DYRD = oo16pi2* betaYRD1 
End If 
 
 
!-------------------- 
! YRB1 
!-------------------- 
 
betaYRB11  = ((4._dp*(SPYRA2xxYRA1) + 3._dp*(SPYRB1xxYRB2))*YRB2 + YRB1*(4._dp*(SPYRA1xxYRA1)& 
&  + 4._dp*(SPYRB1xxYRB1) + SPYRB2xxYRB2 + YRCp2))/2._dp

 
 
If (TwoLoopRGE) Then 
betaYRB12 = 0

 
DYRB1 = oo16pi2*( betaYRB11 + oo16pi2 * betaYRB12 ) 

 
Else 
DYRB1 = oo16pi2* betaYRB11 
End If 
 
 
!-------------------- 
! YRB2 
!-------------------- 
 
betaYRB21  = ((4._dp*(SPYRA1xxYRA2) + 3._dp*(SPYRB2xxYRB1))*YRB1 + YRB2*(4._dp*(SPYRA2xxYRA2)& 
&  + SPYRB1xxYRB1 + 4._dp*(SPYRB2xxYRB2) + YRCp2))/2._dp

 
 
If (TwoLoopRGE) Then 
betaYRB22 = 0

 
DYRB2 = oo16pi2*( betaYRB21 + oo16pi2 * betaYRB22 ) 

 
Else 
DYRB2 = oo16pi2* betaYRB21 
End If 
 
 
!-------------------- 
! YRC 
!-------------------- 
 
betaYRC1  = (YRC*(SPYRB1xxYRB1 + SPYRB2xxYRB2 + 2*(5*YRC**2 + YRDp2)))/2._dp

 
 
If (TwoLoopRGE) Then 
betaYRC2 = 0

 
DYRC = oo16pi2*( betaYRC1 + oo16pi2 * betaYRC2 ) 

 
Else 
DYRC = oo16pi2* betaYRC1 
End If 
 
 
!-------------------- 
! YRA1 
!-------------------- 
 
betaYRA11  = TpYeCYeYRA1/2._dp + (-9._dp*(g1p2)/10._dp - 9._dp*(g2p2)/2._dp +         & 
&  3._dp*(SPYRA1xxYRA1) + SPYRA2xxYRA2/2._dp + SPYRB1xxYRB1)*YRA1 + (5*SPYRA1xxYRA2*YRA2)& 
& /2._dp + SPYRB2xxYRB1*YRA2

 
 
If (TwoLoopRGE) Then 
betaYRA12 = 0

 
DYRA1 = oo16pi2*( betaYRA11 + oo16pi2 * betaYRA12 ) 

 
Else 
DYRA1 = oo16pi2* betaYRA11 
End If 
 
 
!-------------------- 
! YRA2 
!-------------------- 
 
betaYRA21  = TpYeCYeYRA2/2._dp + (5._dp*(SPYRA2xxYRA1)/2._dp + SPYRB1xxYRB2)          & 
& *YRA1 + ((-9._dp*(g1p2) + 5*(-9._dp*(g2p2) + SPYRA1xxYRA1 + 6._dp*(SPYRA2xxYRA2)       & 
&  + 2._dp*(SPYRB2xxYRB2)))*YRA2)/10._dp

 
 
If (TwoLoopRGE) Then 
betaYRA22 = 0

 
DYRA2 = oo16pi2*( betaYRA21 + oo16pi2 * betaYRA22 ) 

 
Else 
DYRA2 = oo16pi2* betaYRA21 
End If 
 
 
!-------------------- 
! MDF 
!-------------------- 
 
betaMDF1  = (MDF*(-9._dp*(g1p2) + 5*(-9._dp*(g2p2) + SPYRA1xxYRA1 + SPYRA2xxYRA2 +    & 
&  YRDp2)))/10._dp

 
 
If (TwoLoopRGE) Then 
betaMDF2 = 0

 
DMDF = oo16pi2*( betaMDF1 + oo16pi2 * betaMDF2 ) 

 
Else 
DMDF = oo16pi2* betaMDF1 
End If 
 
 
!-------------------- 
! m2SM 
!-------------------- 
 
betam2SM1  = -2*LSPH*mP2 - 2*LS1H*MS12 - 2*LS2H*MS22 - (9*g1p2*m2SM)/10._dp -         & 
&  (9*g2p2*m2SM)/2._dp + 6*Lam*m2SM + 6*m2SM*TrYdadjYd + 2*m2SM*TrYeadjYe +              & 
&  6*m2SM*TrYuadjYu + 4*MDFp2*YRDp2 + 2*m2SM*YRDp2

 
 
If (TwoLoopRGE) Then 
betam2SM2 = 0

 
Dm2SM = oo16pi2*( betam2SM1 + oo16pi2 * betam2SM2 ) 

 
Else 
Dm2SM = oo16pi2* betam2SM1 
End If 
 
 
!-------------------- 
! MS12 
!-------------------- 
 
betaMS121  = 4*LS*MS12 - 4*LS1H*m2SM - 8*MDFp2*SPYRA1xxYRA1 + 4*MS12*SPYRA1xxYRA1 +   & 
&  2*MS12*SPYRB1xxYRB1

 
 
If (TwoLoopRGE) Then 
betaMS122 = 0

 
DMS12 = oo16pi2*( betaMS121 + oo16pi2 * betaMS122 ) 

 
Else 
DMS12 = oo16pi2* betaMS121 
End If 
 
 
!-------------------- 
! MS22 
!-------------------- 
 
betaMS221  = 4*LS*MS22 - 4*LS2H*m2SM - 8*MDFp2*SPYRA2xxYRA2 + 4*MS22*SPYRA2xxYRA2 +   & 
&  2*MS22*SPYRB2xxYRB2

 
 
If (TwoLoopRGE) Then 
betaMS222 = 0

 
DMS22 = oo16pi2*( betaMS221 + oo16pi2 * betaMS222 ) 

 
Else 
DMS22 = oo16pi2* betaMS221 
End If 
 
 
!-------------------- 
! mP2 
!-------------------- 
 
betamP21  = 4*(3*LSP*mP2 - 2*LSPH*m2SM + mP2*YRCp2)

 
 
If (TwoLoopRGE) Then 
betamP22 = 0

 
DmP2 = oo16pi2*( betamP21 + oo16pi2 * betamP22 ) 

 
Else 
DmP2 = oo16pi2* betamP21 
End If 
 
 
Call ParametersToG83(Dg1,Dg2,Dg3,DLam,DLS1H,DLS,DLS2H,DLSP,DLSPH,DYu,DYd,             & 
& DYe,DYRD,DYRB1,DYRB2,DYRC,DYRA1,DYRA2,DMDF,Dm2SM,DMS12,DMS22,DmP2,f)

Iname = Iname - 1 
 
End Subroutine rge83  

Subroutine GToParameters85(g,g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,             & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS)

Implicit None 
Real(dp), Intent(in) :: g(85) 
Real(dp),Intent(out) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS

Complex(dp),Intent(out) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Integer i1, i2, i3, i4, SumI 
 
Iname = Iname +1 
NameOfUnit(Iname) = 'GToParameters85' 
 
g1= g(1) 
g2= g(2) 
g3= g(3) 
Lam= Cmplx(g(4),g(5),dp) 
LS1H= g(6) 
LS= g(7) 
LS2H= g(8) 
LSP= g(9) 
LSPH= g(10) 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Yu(i1,i2) = Cmplx( g(SumI+11), g(SumI+12), dp) 
End Do 
 End Do 
 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Yd(i1,i2) = Cmplx( g(SumI+29), g(SumI+30), dp) 
End Do 
 End Do 
 
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
Ye(i1,i2) = Cmplx( g(SumI+47), g(SumI+48), dp) 
End Do 
 End Do 
 
YRD= g(65) 
Do i1 = 1,3
SumI = (i1-1) 
YRB1(i1) =  g(SumI+66) 
End Do 
 
Do i1 = 1,3
SumI = (i1-1) 
YRB2(i1) =  g(SumI+69) 
End Do 
 
YRC= g(72) 
Do i1 = 1,3
SumI = (i1-1) 
YRA1(i1) =  g(SumI+73) 
End Do 
 
Do i1 = 1,3
SumI = (i1-1) 
YRA2(i1) =  g(SumI+76) 
End Do 
 
MDF= g(79) 
m2SM= g(80) 
MS12= g(81) 
MS22= g(82) 
mP2= g(83) 
vvSM= g(84) 
vS= g(85) 
Do i1=1,85 
If (g(i1).ne.g(i1)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Write(*,*) "At position ", i1 
 Call TerminateProgram 
End if 
End do 
Iname = Iname - 1 
 
End Subroutine GToParameters85

Subroutine ParametersToG85(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,               & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,g)

Implicit None 
Real(dp), Intent(out) :: g(85) 
Real(dp), Intent(in) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS

Complex(dp), Intent(in) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Integer i1, i2, i3, i4, SumI 
 
Iname = Iname +1 
NameOfUnit(Iname) = 'ParametersToG85' 
 
g(1) = g1  
g(2) = g2  
g(3) = g3  
g(4) = Real(Lam,dp)  
g(5) = Aimag(Lam)  
g(6) = LS1H  
g(7) = LS  
g(8) = LS2H  
g(9) = LSP  
g(10) = LSPH  
Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+11) = Real(Yu(i1,i2), dp) 
g(SumI+12) = Aimag(Yu(i1,i2)) 
End Do 
End Do 

Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+29) = Real(Yd(i1,i2), dp) 
g(SumI+30) = Aimag(Yd(i1,i2)) 
End Do 
End Do 

Do i1 = 1,3
Do i2 = 1,3
SumI = (i2-1) + (i1-1)*3
SumI = SumI*2 
g(SumI+47) = Real(Ye(i1,i2), dp) 
g(SumI+48) = Aimag(Ye(i1,i2)) 
End Do 
End Do 

g(65) = YRD  
Do i1 = 1,3
SumI = (i1-1) 
g(SumI+66) = YRB1(i1) 
End Do 

Do i1 = 1,3
SumI = (i1-1) 
g(SumI+69) = YRB2(i1) 
End Do 

g(72) = YRC  
Do i1 = 1,3
SumI = (i1-1) 
g(SumI+73) = YRA1(i1) 
End Do 

Do i1 = 1,3
SumI = (i1-1) 
g(SumI+76) = YRA2(i1) 
End Do 

g(79) = MDF  
g(80) = m2SM  
g(81) = MS12  
g(82) = MS22  
g(83) = mP2  
g(84) = vvSM  
g(85) = vS  
Iname = Iname - 1 
 
End Subroutine ParametersToG85

Subroutine rge85(len, T, GY, F) 
Implicit None 
Integer, Intent(in) :: len 
Real(dp), Intent(in) :: T, GY(len) 
Real(dp), Intent(out) :: F(len) 
Integer :: i1,i2,i3,i4 
Integer :: j1,j2,j3,j4,j5,j6,j7 
Real(dp) :: q 
Real(dp) :: g1,betag11,betag12,Dg1,g2,betag21,betag22,Dg2,g3,betag31,betag32,         & 
& Dg3,LS1H,betaLS1H1,betaLS1H2,DLS1H,LS,betaLS1,betaLS2,DLS,LS2H,betaLS2H1,              & 
& betaLS2H2,DLS2H,LSP,betaLSP1,betaLSP2,DLSP,LSPH,betaLSPH1,betaLSPH2,DLSPH,             & 
& YRD,betaYRD1,betaYRD2,DYRD,YRB1(3),betaYRB11(3),betaYRB12(3),DYRB1(3),YRB2(3)          & 
& ,betaYRB21(3),betaYRB22(3),DYRB2(3),YRC,betaYRC1,betaYRC2,DYRC,YRA1(3),betaYRA11(3)    & 
& ,betaYRA12(3),DYRA1(3),YRA2(3),betaYRA21(3),betaYRA22(3),DYRA2(3),MDF,betaMDF1,        & 
& betaMDF2,DMDF,m2SM,betam2SM1,betam2SM2,Dm2SM,MS12,betaMS121,betaMS122,DMS12,           & 
& MS22,betaMS221,betaMS222,DMS22,mP2,betamP21,betamP22,DmP2,vvSM,betavvSM1,              & 
& betavvSM2,DvvSM,vS,betavS1,betavS2,DvS
Complex(dp) :: Lam,betaLam1,betaLam2,DLam,Yu(3,3),betaYu1(3,3),betaYu2(3,3)           & 
& ,DYu(3,3),adjYu(3,3),Yd(3,3),betaYd1(3,3),betaYd2(3,3),DYd(3,3),adjYd(3,3)             & 
& ,Ye(3,3),betaYe1(3,3),betaYe2(3,3),DYe(3,3),adjYe(3,3)
Complex(dp) :: YdadjYd(3,3),YeYRA1(3),YeYRA2(3),YeadjYe(3,3),YuadjYu(3,3),adjYdYd(3,3),              & 
& adjYeYe(3,3),adjYuYu(3,3),CYeYRA1(3),CYeYRA2(3),YdadjYdYd(3,3),YdadjYuYu(3,3),         & 
& YeadjYeYe(3,3),YuadjYdYd(3,3),YuadjYuYu(3,3),adjYdYdadjYd(3,3),adjYeYeYRA1(3),         & 
& adjYeYeYRA2(3),adjYeYeadjYe(3,3),adjYuYuadjYu(3,3),TpYeCYeYRA1(3),TpYeCYeYRA2(3),      & 
& YdadjYdYdadjYd(3,3),YeadjYeYeadjYe(3,3),YuadjYuYuadjYu(3,3)

Complex(dp) :: TrYdadjYd,TrYeadjYe,TrYuadjYu,TrYdadjYdYdadjYd,TrYeadjYeYeadjYe,TrYuadjYuYuadjYu

Real(dp) :: SPYRA1xxYRA1,SPYRA1xxYRA2,SPYRA1xxadjYeYeYRA1,SPYRA2xxYRA1,SPYRA2xxYRA2,              & 
& SPYRA2xxadjYeYeYRA2,SPYRB1xxYRB1,SPYRB1xxYRB2,SPYRB2xxYRB1,SPYRB2xxYRB2

Real(dp) :: g1p2,g1p3,g1p4,g2p2,g2p3,g2p4,g3p2,g3p3,LSp2,LS1Hp2,LS2Hp2,LSPp2,LSPHp2,              & 
& MDFp2,YRCp2,YRCp4,YRDp2,YRDp4

Complex(dp) :: Lamp2,SPYRA1xxYRA1p2,SPYRB1xxYRB1p2

Complex(dp) :: DyYeYRA1i1YRA1i2(3,3),DyYeYRA2i1YRA2i2(3,3)

Iname = Iname +1 
NameOfUnit(Iname) = 'rge85' 
 
OnlyDiagonal = .Not.GenerationMixing 
q = t 
 
Call GToParameters85(gy,g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,              & 
& YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS)

Call Adjungate(Yu,adjYu)
Call Adjungate(Yd,adjYd)
Call Adjungate(Ye,adjYe)
 YdadjYd = Matmul(Yd,adjYd) 
Forall(i2=1:3)  YdadjYd(i2,i2) =  Real(YdadjYd(i2,i2),dp) 
 YeYRA1 = Matmul(Ye,YRA1) 
 YeYRA2 = Matmul(Ye,YRA2) 
 YeadjYe = Matmul(Ye,adjYe) 
Forall(i2=1:3)  YeadjYe(i2,i2) =  Real(YeadjYe(i2,i2),dp) 
 YuadjYu = Matmul(Yu,adjYu) 
Forall(i2=1:3)  YuadjYu(i2,i2) =  Real(YuadjYu(i2,i2),dp) 
 adjYdYd = Matmul(adjYd,Yd) 
Forall(i2=1:3)  adjYdYd(i2,i2) =  Real(adjYdYd(i2,i2),dp) 
 adjYeYe = Matmul(adjYe,Ye) 
Forall(i2=1:3)  adjYeYe(i2,i2) =  Real(adjYeYe(i2,i2),dp) 
 adjYuYu = Matmul(adjYu,Yu) 
Forall(i2=1:3)  adjYuYu(i2,i2) =  Real(adjYuYu(i2,i2),dp) 
 CYeYRA1 = Matmul(Conjg(Ye),YRA1) 
 CYeYRA2 = Matmul(Conjg(Ye),YRA2) 
 YdadjYdYd = Matmul(Yd,adjYdYd) 
 YdadjYuYu = Matmul(Yd,adjYuYu) 
 YeadjYeYe = Matmul(Ye,adjYeYe) 
 YuadjYdYd = Matmul(Yu,adjYdYd) 
 YuadjYuYu = Matmul(Yu,adjYuYu) 
 adjYdYdadjYd = Matmul(adjYd,YdadjYd) 
 adjYeYeYRA1 = Matmul(adjYe,YeYRA1) 
 adjYeYeYRA2 = Matmul(adjYe,YeYRA2) 
 adjYeYeadjYe = Matmul(adjYe,YeadjYe) 
 adjYuYuadjYu = Matmul(adjYu,YuadjYu) 
 TpYeCYeYRA1 = Matmul(Transpose(Ye),CYeYRA1) 
 TpYeCYeYRA2 = Matmul(Transpose(Ye),CYeYRA2) 
 YdadjYdYdadjYd = Matmul(Yd,adjYdYdadjYd) 
Forall(i2=1:3)  YdadjYdYdadjYd(i2,i2) =  Real(YdadjYdYdadjYd(i2,i2),dp) 
 YeadjYeYeadjYe = Matmul(Ye,adjYeYeadjYe) 
Forall(i2=1:3)  YeadjYeYeadjYe(i2,i2) =  Real(YeadjYeYeadjYe(i2,i2),dp) 
 YuadjYuYuadjYu = Matmul(Yu,adjYuYuadjYu) 
Forall(i2=1:3)  YuadjYuYuadjYu(i2,i2) =  Real(YuadjYuYuadjYu(i2,i2),dp) 
 TrYdadjYd = Real(cTrace(YdadjYd),dp) 
 TrYeadjYe = Real(cTrace(YeadjYe),dp) 
 TrYuadjYu = Real(cTrace(YuadjYu),dp) 
 TrYdadjYdYdadjYd = Real(cTrace(YdadjYdYdadjYd),dp) 
 TrYeadjYeYeadjYe = Real(cTrace(YeadjYeYeadjYe),dp) 
 TrYuadjYuYuadjYu = Real(cTrace(YuadjYuYuadjYu),dp) 
 SPYRA1xxYRA1 = DOT_PRODUCT(YRA1,YRA1) 
 SPYRA1xxYRA2 = DOT_PRODUCT(YRA1,YRA2) 
 SPYRA1xxadjYeYeYRA1 = DOT_PRODUCT(YRA1,adjYeYeYRA1) 
 SPYRA2xxYRA1 = DOT_PRODUCT(YRA2,YRA1) 
 SPYRA2xxYRA2 = DOT_PRODUCT(YRA2,YRA2) 
 SPYRA2xxadjYeYeYRA2 = DOT_PRODUCT(YRA2,adjYeYeYRA2) 
 SPYRB1xxYRB1 = DOT_PRODUCT(YRB1,YRB1) 
 SPYRB1xxYRB2 = DOT_PRODUCT(YRB1,YRB2) 
 SPYRB2xxYRB1 = DOT_PRODUCT(YRB2,YRB1) 
 SPYRB2xxYRB2 = DOT_PRODUCT(YRB2,YRB2) 
 g1p2 =g1**2 
 g1p3 =g1**3 
 g1p4 =g1**4 
 g2p2 =g2**2 
 g2p3 =g2**3 
 g2p4 =g2**4 
 g3p2 =g3**2 
 g3p3 =g3**3 
 Lamp2 =Lam**2 
 LSp2 =LS**2 
 LS1Hp2 =LS1H**2 
 LS2Hp2 =LS2H**2 
 LSPp2 =LSP**2 
 LSPHp2 =LSPH**2 
 MDFp2 =MDF**2 
 YRCp2 =YRC**2 
 YRCp4 =YRC**4 
 YRDp2 =YRD**2 
 YRDp4 =YRD**4 
 SPYRA1xxYRA1p2 =SPYRA1xxYRA1**2 
 SPYRB1xxYRB1p2 =SPYRB1xxYRB1**2 
Do i1=1,3
  Do i2=1,3
DyYeYRA1i1YRA1i2(i1,i2) = YeYRA1(i1)*YRA1(i2) 
  End Do 
End Do 
Do i1=1,3
  Do i2=1,3
DyYeYRA2i1YRA2i2(i1,i2) = YeYRA2(i1)*YRA2(i2) 
  End Do 
End Do 


If (TwoLoopRGE) Then 
End If 
 
 
!-------------------- 
! g1 
!-------------------- 
 
betag11  = 9._dp*(g1p3)/2._dp

 
 
If (TwoLoopRGE) Then 
betag12 = 0

 
Dg1 = oo16pi2*( betag11 + oo16pi2 * betag12 ) 

 
Else 
Dg1 = oo16pi2* betag11 
End If 
 
 
!-------------------- 
! g2 
!-------------------- 
 
betag21  = -5._dp*(g2p3)/2._dp

 
 
If (TwoLoopRGE) Then 
betag22 = 0

 
Dg2 = oo16pi2*( betag21 + oo16pi2 * betag22 ) 

 
Else 
Dg2 = oo16pi2* betag21 
End If 
 
 
!-------------------- 
! g3 
!-------------------- 
 
betag31  = -7._dp*(g3p3)

 
 
If (TwoLoopRGE) Then 
betag32 = 0

 
Dg3 = oo16pi2*( betag31 + oo16pi2 * betag32 ) 

 
Else 
Dg3 = oo16pi2* betag31 
End If 
 
 
!-------------------- 
! Lam 
!-------------------- 
 
betaLam1  = 27._dp*(g1p4)/100._dp + 9._dp*(g2p4)/4._dp + (9*g1p2*(g2p2 -              & 
&  2._dp*(Lam)))/10._dp - 9*g2p2*Lam + 12._dp*(Lamp2) + 2._dp*(LS1Hp2) + 2._dp*(LS2Hp2)  & 
&  + 4._dp*(LSPHp2) + 12*Lam*TrYdadjYd - 12._dp*(TrYdadjYdYdadjYd) + 4*Lam*TrYeadjYe -   & 
&  4._dp*(TrYeadjYeYeadjYe) + 12*Lam*TrYuadjYu - 12._dp*(TrYuadjYuYuadjYu)               & 
&  + 4*Lam*YRDp2 - 4._dp*(YRDp4)

 
 
If (TwoLoopRGE) Then 
betaLam2 = 0

 
DLam = oo16pi2*( betaLam1 + oo16pi2 * betaLam2 ) 

 
Else 
DLam = oo16pi2* betaLam1 
End If 
 
 
Call Chop(DLam) 

!-------------------- 
! LS1H 
!-------------------- 
 
betaLS1H1  = (-9*g1p2*LS1H)/10._dp - (9*g2p2*LS1H)/2._dp + 2*(3*Lam*LS1H +            & 
&  2*LS*LS1H + 2._dp*(LS1Hp2) - 2._dp*(SPYRA1xxadjYeYeYRA1) + 2*LS1H*SPYRA1xxYRA1 +      & 
&  LS1H*SPYRB1xxYRB1 + 3*LS1H*TrYdadjYd + LS1H*TrYeadjYe + 3*LS1H*TrYuadjYu +            & 
&  LS1H*YRDp2)

 
 
If (TwoLoopRGE) Then 
betaLS1H2 = 0

 
DLS1H = oo16pi2*( betaLS1H1 + oo16pi2 * betaLS1H2 ) 

 
Else 
DLS1H = oo16pi2* betaLS1H1 
End If 
 
 
!-------------------- 
! LS 
!-------------------- 
 
betaLS1  = 4._dp*(LS1Hp2) + 10._dp*(LSp2) + 8*LS*SPYRA1xxYRA1 - 8._dp*(SPYRA1xxYRA1p2)& 
&  + 4*LS*SPYRB1xxYRB1 - 4._dp*(SPYRB1xxYRB1p2)

 
 
If (TwoLoopRGE) Then 
betaLS2 = 0

 
DLS = oo16pi2*( betaLS1 + oo16pi2 * betaLS2 ) 

 
Else 
DLS = oo16pi2* betaLS1 
End If 
 
 
!-------------------- 
! LS2H 
!-------------------- 
 
betaLS2H1  = (-9*g1p2*LS2H)/10._dp - (9*g2p2*LS2H)/2._dp + 2*(3*Lam*LS2H +            & 
&  2*LS*LS2H + 2._dp*(LS2Hp2) - 2._dp*(SPYRA2xxadjYeYeYRA2) + 2*LS2H*SPYRA2xxYRA2 +      & 
&  LS2H*SPYRB2xxYRB2 + 3*LS2H*TrYdadjYd + LS2H*TrYeadjYe + 3*LS2H*TrYuadjYu +            & 
&  LS2H*YRDp2)

 
 
If (TwoLoopRGE) Then 
betaLS2H2 = 0

 
DLS2H = oo16pi2*( betaLS2H1 + oo16pi2 * betaLS2H2 ) 

 
Else 
DLS2H = oo16pi2* betaLS2H1 
End If 
 
 
!-------------------- 
! LSP 
!-------------------- 
 
betaLSP1  = 4*(LSPHp2 + 9._dp*(LSPp2) + 2*LSP*YRCp2 - YRCp4)

 
 
If (TwoLoopRGE) Then 
betaLSP2 = 0

 
DLSP = oo16pi2*( betaLSP1 + oo16pi2 * betaLSP2 ) 

 
Else 
DLSP = oo16pi2* betaLSP1 
End If 
 
 
!-------------------- 
! LSPH 
!-------------------- 
 
betaLSPH1  = (-9*g1p2*LSPH)/10._dp - (9*g2p2*LSPH)/2._dp + 2*(3*Lam*LSPH +            & 
&  6*LSP*LSPH + 4._dp*(LSPHp2) + 3*LSPH*TrYdadjYd + LSPH*TrYeadjYe + 3*LSPH*TrYuadjYu +  & 
&  2*LSPH*YRCp2 + LSPH*YRDp2 - 2*YRCp2*YRDp2)

 
 
If (TwoLoopRGE) Then 
betaLSPH2 = 0

 
DLSPH = oo16pi2*( betaLSPH1 + oo16pi2 * betaLSPH2 ) 

 
Else 
DLSPH = oo16pi2* betaLSPH1 
End If 
 
 
!-------------------- 
! Yu 
!-------------------- 
 
betaYu1  = (-17._dp*(g1p2)/20._dp - 9._dp*(g2p2)/4._dp - 8._dp*(g3p2) +               & 
&  3._dp*(TrYdadjYd) + TrYeadjYe + 3._dp*(TrYuadjYu) + YRDp2)*Yu - (3*(YuadjYdYd -       & 
&  YuadjYuYu))/2._dp

 
 
If (TwoLoopRGE) Then 
betaYu2 = 0

 
DYu = oo16pi2*( betaYu1 + oo16pi2 * betaYu2 ) 

 
Else 
DYu = oo16pi2* betaYu1 
End If 
 
 
Call Chop(DYu) 

!-------------------- 
! Yd 
!-------------------- 
 
betaYd1  = (3*(YdadjYdYd - YdadjYuYu))/2._dp + Yd*(-1._dp*(g1p2)/4._dp -              & 
&  9._dp*(g2p2)/4._dp - 8._dp*(g3p2) + 3._dp*(TrYdadjYd) + TrYeadjYe + 3._dp*(TrYuadjYu) + YRDp2)

 
 
If (TwoLoopRGE) Then 
betaYd2 = 0

 
DYd = oo16pi2*( betaYd1 + oo16pi2 * betaYd2 ) 

 
Else 
DYd = oo16pi2* betaYd1 
End If 
 
 
Call Chop(DYd) 

!-------------------- 
! Ye 
!-------------------- 
 
betaYe1  = (2._dp*(DyYeYRA1i1YRA1i2) + 2._dp*(DyYeYRA2i1YRA2i2) - 9*g1p2*Ye -         & 
&  9*g2p2*Ye + 12*TrYdadjYd*Ye + 4*TrYeadjYe*Ye + 12*TrYuadjYu*Ye + 6._dp*(YeadjYeYe)    & 
&  + 4*Ye*YRDp2)/4._dp

 
 
If (TwoLoopRGE) Then 
betaYe2 = 0

 
DYe = oo16pi2*( betaYe1 + oo16pi2 * betaYe2 ) 

 
Else 
DYe = oo16pi2* betaYe1 
End If 
 
 
Call Chop(DYe) 

!-------------------- 
! YRD 
!-------------------- 
 
betaYRD1  = (YRD*(-9._dp*(g1p2) + 5*(-9._dp*(g2p2) + 2*(6._dp*(TrYdadjYd)             & 
&  + 2._dp*(TrYeadjYe) + 6._dp*(TrYuadjYu) + YRCp2 + 5*YRD**2))))/20._dp

 
 
If (TwoLoopRGE) Then 
betaYRD2 = 0

 
DYRD = oo16pi2*( betaYRD1 + oo16pi2 * betaYRD2 ) 

 
Else 
DYRD = oo16pi2* betaYRD1 
End If 
 
 
!-------------------- 
! YRB1 
!-------------------- 
 
betaYRB11  = ((4._dp*(SPYRA2xxYRA1) + 3._dp*(SPYRB1xxYRB2))*YRB2 + YRB1*(4._dp*(SPYRA1xxYRA1)& 
&  + 4._dp*(SPYRB1xxYRB1) + SPYRB2xxYRB2 + YRCp2))/2._dp

 
 
If (TwoLoopRGE) Then 
betaYRB12 = 0

 
DYRB1 = oo16pi2*( betaYRB11 + oo16pi2 * betaYRB12 ) 

 
Else 
DYRB1 = oo16pi2* betaYRB11 
End If 
 
 
!-------------------- 
! YRB2 
!-------------------- 
 
betaYRB21  = ((4._dp*(SPYRA1xxYRA2) + 3._dp*(SPYRB2xxYRB1))*YRB1 + YRB2*(4._dp*(SPYRA2xxYRA2)& 
&  + SPYRB1xxYRB1 + 4._dp*(SPYRB2xxYRB2) + YRCp2))/2._dp

 
 
If (TwoLoopRGE) Then 
betaYRB22 = 0

 
DYRB2 = oo16pi2*( betaYRB21 + oo16pi2 * betaYRB22 ) 

 
Else 
DYRB2 = oo16pi2* betaYRB21 
End If 
 
 
!-------------------- 
! YRC 
!-------------------- 
 
betaYRC1  = (YRC*(SPYRB1xxYRB1 + SPYRB2xxYRB2 + 2*(5*YRC**2 + YRDp2)))/2._dp

 
 
If (TwoLoopRGE) Then 
betaYRC2 = 0

 
DYRC = oo16pi2*( betaYRC1 + oo16pi2 * betaYRC2 ) 

 
Else 
DYRC = oo16pi2* betaYRC1 
End If 
 
 
!-------------------- 
! YRA1 
!-------------------- 
 
betaYRA11  = TpYeCYeYRA1/2._dp + (-9._dp*(g1p2)/10._dp - 9._dp*(g2p2)/2._dp +         & 
&  3._dp*(SPYRA1xxYRA1) + SPYRA2xxYRA2/2._dp + SPYRB1xxYRB1)*YRA1 + (5*SPYRA1xxYRA2*YRA2)& 
& /2._dp + SPYRB2xxYRB1*YRA2

 
 
If (TwoLoopRGE) Then 
betaYRA12 = 0

 
DYRA1 = oo16pi2*( betaYRA11 + oo16pi2 * betaYRA12 ) 

 
Else 
DYRA1 = oo16pi2* betaYRA11 
End If 
 
 
!-------------------- 
! YRA2 
!-------------------- 
 
betaYRA21  = TpYeCYeYRA2/2._dp + (5._dp*(SPYRA2xxYRA1)/2._dp + SPYRB1xxYRB2)          & 
& *YRA1 + ((-9._dp*(g1p2) + 5*(-9._dp*(g2p2) + SPYRA1xxYRA1 + 6._dp*(SPYRA2xxYRA2)       & 
&  + 2._dp*(SPYRB2xxYRB2)))*YRA2)/10._dp

 
 
If (TwoLoopRGE) Then 
betaYRA22 = 0

 
DYRA2 = oo16pi2*( betaYRA21 + oo16pi2 * betaYRA22 ) 

 
Else 
DYRA2 = oo16pi2* betaYRA21 
End If 
 
 
!-------------------- 
! MDF 
!-------------------- 
 
betaMDF1  = (MDF*(-9._dp*(g1p2) + 5*(-9._dp*(g2p2) + SPYRA1xxYRA1 + SPYRA2xxYRA2 +    & 
&  YRDp2)))/10._dp

 
 
If (TwoLoopRGE) Then 
betaMDF2 = 0

 
DMDF = oo16pi2*( betaMDF1 + oo16pi2 * betaMDF2 ) 

 
Else 
DMDF = oo16pi2* betaMDF1 
End If 
 
 
!-------------------- 
! m2SM 
!-------------------- 
 
betam2SM1  = -2*LSPH*mP2 - 2*LS1H*MS12 - 2*LS2H*MS22 - (9*g1p2*m2SM)/10._dp -         & 
&  (9*g2p2*m2SM)/2._dp + 6*Lam*m2SM + 6*m2SM*TrYdadjYd + 2*m2SM*TrYeadjYe +              & 
&  6*m2SM*TrYuadjYu + 4*MDFp2*YRDp2 + 2*m2SM*YRDp2

 
 
If (TwoLoopRGE) Then 
betam2SM2 = 0

 
Dm2SM = oo16pi2*( betam2SM1 + oo16pi2 * betam2SM2 ) 

 
Else 
Dm2SM = oo16pi2* betam2SM1 
End If 
 
 
!-------------------- 
! MS12 
!-------------------- 
 
betaMS121  = 4*LS*MS12 - 4*LS1H*m2SM - 8*MDFp2*SPYRA1xxYRA1 + 4*MS12*SPYRA1xxYRA1 +   & 
&  2*MS12*SPYRB1xxYRB1

 
 
If (TwoLoopRGE) Then 
betaMS122 = 0

 
DMS12 = oo16pi2*( betaMS121 + oo16pi2 * betaMS122 ) 

 
Else 
DMS12 = oo16pi2* betaMS121 
End If 
 
 
!-------------------- 
! MS22 
!-------------------- 
 
betaMS221  = 4*LS*MS22 - 4*LS2H*m2SM - 8*MDFp2*SPYRA2xxYRA2 + 4*MS22*SPYRA2xxYRA2 +   & 
&  2*MS22*SPYRB2xxYRB2

 
 
If (TwoLoopRGE) Then 
betaMS222 = 0

 
DMS22 = oo16pi2*( betaMS221 + oo16pi2 * betaMS222 ) 

 
Else 
DMS22 = oo16pi2* betaMS221 
End If 
 
 
!-------------------- 
! mP2 
!-------------------- 
 
betamP21  = 4*(3*LSP*mP2 - 2*LSPH*m2SM + mP2*YRCp2)

 
 
If (TwoLoopRGE) Then 
betamP22 = 0

 
DmP2 = oo16pi2*( betamP21 + oo16pi2 * betamP22 ) 

 
Else 
DmP2 = oo16pi2* betamP21 
End If 
 
 
!-------------------- 
! vvSM 
!-------------------- 
 
betavvSM1  = (vvSM*(3*g1p2*(3 + Xi) + 5*(3*g2p2*(3 + Xi) - 4*(3._dp*(TrYdadjYd)       & 
&  + TrYeadjYe + 3._dp*(TrYuadjYu) + YRDp2))))/20._dp

 
 
If (TwoLoopRGE) Then 
betavvSM2 = 0

 
DvvSM = oo16pi2*( betavvSM1 + oo16pi2 * betavvSM2 ) 

 
Else 
DvvSM = oo16pi2* betavvSM1 
End If 
 
 
!-------------------- 
! vS 
!-------------------- 
 
betavS1  = -2*vS*YRCp2

 
 
If (TwoLoopRGE) Then 
betavS2 = 0

 
DvS = oo16pi2*( betavS1 + oo16pi2 * betavS2 ) 

 
Else 
DvS = oo16pi2* betavS1 
End If 
 
 
Call ParametersToG85(Dg1,Dg2,Dg3,DLam,DLS1H,DLS,DLS2H,DLSP,DLSPH,DYu,DYd,             & 
& DYe,DYRD,DYRB1,DYRB2,DYRC,DYRA1,DYRA2,DMDF,Dm2SM,DMS12,DMS22,DmP2,DvvSM,DvS,f)

Iname = Iname - 1 
 
End Subroutine rge85  

End Module RGEs_SDdiracDM 
 
