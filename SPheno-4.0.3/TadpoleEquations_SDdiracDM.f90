! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:23 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module Tadpoles_SDdiracDM 
 
Use Model_Data_SDdiracDM 
Use TreeLevelMasses_SDdiracDM 
Use Control 
Use Settings 
Use Mathematics 

Contains 


Subroutine SolveTadpoleEquations(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,               & 
& Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,Tad1Loop)

Implicit None
Real(dp),Intent(inout) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS

Complex(dp),Intent(inout) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Complex(dp), Intent(in) :: Tad1Loop(2)

! For numerical routines 
Real(dp) :: gC(85)
logical :: broycheck 
Real(dp) :: broyx(2)

If (HighScaleModel.Eq."LOW") Then 
m2SM = (Lam*vvSM**3 + LSPH*vvSM*vS**2 - 2*Tad1Loop(1))/(2._dp*vvSM)
mP2 = -(LSPH*vvSM**2) - LSP*vS**2 + (2*Tad1Loop(2))/vS

 ! ----------- Check solutions for consistency  -------- 

 ! Check for NaNs 
If (m2SM.ne.m2SM) Then 
   Write(*,*) "NaN appearing in solution of tadpole equations for m2SM" 
   Call TerminateProgram  
 End If 
 If (mP2.ne.mP2) Then 
   Write(*,*) "NaN appearing in solution of tadpole equations for mP2" 
   Call TerminateProgram  
 End If 
 Else 
m2SM = (Lam*vvSM**3 + LSPH*vvSM*vS**2 - 2*Tad1Loop(1))/(2._dp*vvSM)
mP2 = -(LSPH*vvSM**2) - LSP*vS**2 + (2*Tad1Loop(2))/vS

 ! ----------- Check solutions for consistency  -------- 

 ! Check for NaNs 
If (m2SM.ne.m2SM) Then 
   Write(*,*) "NaN appearing in solution of tadpole equations for m2SM" 
   Call TerminateProgram  
 End If 
 If (mP2.ne.mP2) Then 
   Write(*,*) "NaN appearing in solution of tadpole equations for mP2" 
   Call TerminateProgram  
 End If 
 End if 
End Subroutine SolveTadpoleEquations

Subroutine CalculateTadpoles(g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,             & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,vvSM,vS,Tad1Loop,TadpoleValues)

Real(dp),Intent(in) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2,vvSM,vS

Complex(dp),Intent(in) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Complex(dp), Intent(in) :: Tad1Loop(2)

Real(dp), Intent(out) :: TadpoleValues(2)

TadpoleValues(1) = Real((vvSM*(-2._dp*(m2SM) + Lam*vvSM**2 + LSPH*vS**2))             & 
& /2._dp - Tad1Loop(1),dp) 
TadpoleValues(2) = Real((vS*(mP2 + LSPH*vvSM**2 + LSP*vS**2))/2._dp - Tad1Loop(2),dp) 
End Subroutine CalculateTadpoles 

End Module Tadpoles_SDdiracDM 
 
