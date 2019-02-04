! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.12.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 14:22 on 29.1.2019   
! ----------------------------------------------------------------------  
 
 
Module BranchingRatios_SDdiracDM 
 
Use Control 
Use Settings 
Use Couplings_SDdiracDM 
Use Model_Data_SDdiracDM 
Use LoopCouplings_SDdiracDM 
Use Fu3Decays_SDdiracDM 
Use Fe3Decays_SDdiracDM 
Use Fd3Decays_SDdiracDM 
Use TreeLevelDecays_SDdiracDM 
Use OneLoopDecays_SDdiracDM


 Contains 
 
Subroutine CalculateBR(CTBD,fac3,epsI,deltaM,kont,MAh,MAh2,MFd,MFd2,MFe,              & 
& MFe2,MFu,MFu2,MFv,MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,             & 
& MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,              & 
& alphaH,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,              & 
& YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,gPFu,gTFu,BRFu,gPFe,gTFe,BRFe,gPFd,               & 
& gTFd,BRFd,gPhh,gThh,BRhh,gPSsc,gTSsc,BRSsc,gPFxe,gTFxe,BRFxe,gPFxv,gTFxv,BRFxv)

Real(dp), Intent(in) :: epsI, deltaM, fac3 
Integer, Intent(inout) :: kont 
Logical, Intent(in) :: CTBD 
Real(dp),Intent(inout) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2

Complex(dp),Intent(inout) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Real(dp),Intent(in) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp),Intent(in) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Real(dp),Intent(inout) :: vvSM,vS

Real(dp),Intent(inout) :: gPFu(3,174),gTFu(3),BRFu(3,174),gPFe(3,173),gTFe(3),BRFe(3,173),gPFd(3,174),          & 
& gTFd(3),BRFd(3,174),gPhh(2,61),gThh(2),BRhh(2,61),gPSsc(2,17),gTSsc(2),BRSsc(2,17),    & 
& gPFxe(1,12),gTFxe,BRFxe(1,12),gPFxv(2,15),gTFxv(2),BRFxv(2,15)

Complex(dp) :: cplHiggsPP(2),cplHiggsGG(2),cplPseudoHiggsPP,cplPseudoHiggsGG,cplHiggsZZvirt(2),      & 
& cplHiggsWWvirt(2)

Real(dp) :: gTAh 
Real(dp) :: gFuFucFdFd(3,3,3,3),gFuFdcFeFv(3,3,3,3),gFuFdcFxeFxv(3,3,1,2),gFuFucFeFe(3,3,3,3),    & 
& gFuFucFuFu(3,3,3,3),gFuFucFxvFxv(3,3,2,2),gFuFucFvFv(3,3,3,3),gFuFucFxeFxe(3,3,1,1),   & 
& gFeFecFdFd(3,3,3,3),gFeFecFeFe(3,3,3,3),gFeFecFuFu(3,3,3,3),gFeFecFxvFxv(3,3,2,2),     & 
& gFeFecFvFv(3,3,3,3),gFeFecFxeFxe(3,3,1,1),gFeFvcFuFd(3,3,3,3),gFeFvcFxvFxe(3,3,2,1),   & 
& gFdFdcFdFd(3,3,3,3),gFdFdcFeFe(3,3,3,3),gFdFdcFuFu(3,3,3,3),gFdFdcFxvFxv(3,3,2,2),     & 
& gFdFdcFvFv(3,3,3,3),gFdFdcFxeFxe(3,3,1,1),gFdFucFvFe(3,3,3,3),gFdFucFxvFxe(3,3,2,1)

Complex(dp) :: coup 
Real(dp) :: vev 
Real(dp) :: gTVZ,gTVWp

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateBR'
 
Write(*,*) "Calculating branching ratios and decay widths" 
gTVWp = gamW 
gTVZ = gamZ 
! One-Loop Decays 
If (OneLoopDecays) Then 
Call CalculateOneLoopDecays(gP1LFu,gP1LFe,gP1LFd,gP1Lhh,gP1LSsc,gP1LFxe,              & 
& gP1LFxv,Mhh,Mhh2,MFd,MFd2,MFu,MFu2,MFe,MFe2,MFv,MFv2,MFxv,MFxv2,MSsc,MSsc2,            & 
& MFxe,MFxe2,MHp,MHp2,MAh,MAh2,MVZ,MVZ2,MVWp,MVWp2,ZH,ZDL,ZDR,ZUL,ZUR,ZEL,               & 
& ZER,UV,UVR,XV,XU,VSs,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,              & 
& YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,epsI,deltaM,kont)

End if 


gPFu = 0._dp 
gTFu = 0._dp 
BRFu = 0._dp 
Call FuTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,           & 
& MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,             & 
& ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,            & 
& LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,              & 
& mP2,vvSM,vS,gPFu(:,1:18),gTFu,BRFu(:,1:18))

Do i1=1,3
gTFu(i1) =Sum(gPFu(i1,:)) 
If (gTFu(i1).Gt.0._dp) BRFu(i1,: ) =gPFu(i1,:)/gTFu(i1) 
If (OneLoopDecays) Then 
gT1LFu(i1) =Sum(gP1LFu(i1,:)) 
If (gT1LFu(i1).Gt.0._dp) BR1LFu(i1,: ) =gP1LFu(i1,:)/gT1LFu(i1) 
End if
End Do 
 

gPFe = 0._dp 
gTFe = 0._dp 
BRFe = 0._dp 
Call FeTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,           & 
& MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,             & 
& ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,            & 
& LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,              & 
& mP2,vvSM,vS,gPFe(:,1:17),gTFe,BRFe(:,1:17))

Do i1=1,3
gTFe(i1) =Sum(gPFe(i1,:)) 
If (gTFe(i1).Gt.0._dp) BRFe(i1,: ) =gPFe(i1,:)/gTFe(i1) 
If (OneLoopDecays) Then 
gT1LFe(i1) =Sum(gP1LFe(i1,:)) 
If (gT1LFe(i1).Gt.0._dp) BR1LFe(i1,: ) =gP1LFe(i1,:)/gT1LFe(i1) 
End if
End Do 
 

gPFd = 0._dp 
gTFd = 0._dp 
BRFd = 0._dp 
Call FdTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,           & 
& MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,             & 
& ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,            & 
& LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,              & 
& mP2,vvSM,vS,gPFd(:,1:18),gTFd,BRFd(:,1:18))

Do i1=1,3
gTFd(i1) =Sum(gPFd(i1,:)) 
If (gTFd(i1).Gt.0._dp) BRFd(i1,: ) =gPFd(i1,:)/gTFd(i1) 
If (OneLoopDecays) Then 
gT1LFd(i1) =Sum(gP1LFd(i1,:)) 
If (gT1LFd(i1).Gt.0._dp) BR1LFd(i1,: ) =gP1LFd(i1,:)/gT1LFd(i1) 
End if
End Do 
 

gPhh = 0._dp 
gThh = 0._dp 
BRhh = 0._dp 
Call hhTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,           & 
& MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,             & 
& ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,            & 
& LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,              & 
& mP2,vvSM,vS,gPhh,gThh,BRhh)

Do i1=1,2
gThh(i1) =Sum(gPhh(i1,:)) 
If (gThh(i1).Gt.0._dp) BRhh(i1,: ) =gPhh(i1,:)/gThh(i1) 
If (OneLoopDecays) Then 
gT1Lhh(i1) =Sum(gP1Lhh(i1,:)) 
If (gT1Lhh(i1).Gt.0._dp) BR1Lhh(i1,: ) =gP1Lhh(i1,:)/gT1Lhh(i1) 
End if
End Do 
 

gPSsc = 0._dp 
gTSsc = 0._dp 
BRSsc = 0._dp 
Call SscTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,               & 
& MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,           & 
& TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,              & 
& LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,              & 
& MS22,mP2,vvSM,vS,gPSsc,gTSsc,BRSsc)

Do i1=1,2
gTSsc(i1) =Sum(gPSsc(i1,:)) 
If (gTSsc(i1).Gt.0._dp) BRSsc(i1,: ) =gPSsc(i1,:)/gTSsc(i1) 
If (OneLoopDecays) Then 
gT1LSsc(i1) =Sum(gP1LSsc(i1,:)) 
If (gT1LSsc(i1).Gt.0._dp) BR1LSsc(i1,: ) =gP1LSsc(i1,:)/gT1LSsc(i1) 
End if
End Do 
 

gPFxe = 0._dp 
gTFxe = 0._dp 
BRFxe = 0._dp 
Call FxeTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,               & 
& MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,           & 
& TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,              & 
& LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,              & 
& MS22,mP2,vvSM,vS,gPFxe,gTFxe,BRFxe)

Do i1=1,1
gTFxe =Sum(gPFxe(i1,:)) 
If (gTFxe.Gt.0._dp) BRFxe(i1,: ) =gPFxe(i1,:)/gTFxe 
If (OneLoopDecays) Then 
gT1LFxe =Sum(gP1LFxe(i1,:)) 
If (gT1LFxe.Gt.0._dp) BR1LFxe(i1,: ) =gP1LFxe(i1,:)/gT1LFxe 
End if
End Do 
 

gPFxv = 0._dp 
gTFxv = 0._dp 
BRFxv = 0._dp 
Call FxvTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,               & 
& MFv2,MFxe,MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,           & 
& TW,ZDR,ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,              & 
& LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,              & 
& MS22,mP2,vvSM,vS,gPFxv,gTFxv,BRFxv)

Do i1=1,2
gTFxv(i1) =Sum(gPFxv(i1,:)) 
If (gTFxv(i1).Gt.0._dp) BRFxv(i1,: ) =gPFxv(i1,:)/gTFxv(i1) 
If (OneLoopDecays) Then 
gT1LFxv(i1) =Sum(gP1LFxv(i1,:)) 
If (gT1LFxv(i1).Gt.0._dp) BR1LFxv(i1,: ) =gP1LFxv(i1,:)/gT1LFxv(i1) 
End if
End Do 
 

If (.Not.CTBD) Then 
If ((Enable3BDecaysF).and.(Calc3BodyDecay_Fu)) Then 
If (MaxVal(gTFu).Lt.MaxVal(fac3*Abs(MFu))) Then 
Call FuThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,           & 
& MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,              & 
& ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& vvSM,vS,gThh,gTVWp,gTVZ,gFuFucFdFd,gFuFdcFeFv,gFuFdcFxeFxv,gFuFucFeFe,gFuFucFuFu,      & 
& gFuFucFxvFxv,gFuFucFvFv,gFuFucFxeFxe,epsI,deltaM,.False.,gTFu,gPFu(:,19:174)           & 
& ,BRFu(:,19:174))

Else 
Call FuThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,           & 
& MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,              & 
& ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& vvSM,vS,gThh,gTVWp,gTVZ,gFuFucFdFd,gFuFdcFeFv,gFuFdcFxeFxv,gFuFucFeFe,gFuFucFuFu,      & 
& gFuFucFxvFxv,gFuFucFvFv,gFuFucFxeFxe,epsI,deltaM,.True.,gTFu,gPFu(:,19:174)            & 
& ,BRFu(:,19:174))

End If 
 
End If 
Else 
Call FuThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,           & 
& MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,              & 
& ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& vvSM,vS,gThh,gTVWp,gTVZ,gFuFucFdFd,gFuFdcFeFv,gFuFdcFxeFxv,gFuFucFeFe,gFuFucFuFu,      & 
& gFuFucFxvFxv,gFuFucFvFv,gFuFucFxeFxe,epsI,deltaM,.False.,gTFu,gPFu(:,19:174)           & 
& ,BRFu(:,19:174))

End If 
Do i1=1,3
gTFu(i1) =Sum(gPFu(i1,:)) 
If (gTFu(i1).Gt.0._dp) BRFu(i1,: ) =gPFu(i1,:)/gTFu(i1) 
End Do 
 

If (.Not.CTBD) Then 
If ((Enable3BDecaysF).and.(Calc3BodyDecay_Fe)) Then 
If (MaxVal(gTFe).Lt.MaxVal(fac3*Abs(MFe))) Then 
Call FeThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,           & 
& MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,              & 
& ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& vvSM,vS,gThh,gTSsc,gTVWp,gTVZ,gFeFecFdFd,gFeFecFeFe,gFeFecFuFu,gFeFecFxvFxv,           & 
& gFeFecFvFv,gFeFecFxeFxe,gFeFvcFuFd,gFeFvcFxvFxe,epsI,deltaM,.False.,gTFe,              & 
& gPFe(:,18:173),BRFe(:,18:173))

Else 
Call FeThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,           & 
& MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,              & 
& ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& vvSM,vS,gThh,gTSsc,gTVWp,gTVZ,gFeFecFdFd,gFeFecFeFe,gFeFecFuFu,gFeFecFxvFxv,           & 
& gFeFecFvFv,gFeFecFxeFxe,gFeFvcFuFd,gFeFvcFxvFxe,epsI,deltaM,.True.,gTFe,               & 
& gPFe(:,18:173),BRFe(:,18:173))

End If 
 
End If 
Else 
Call FeThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,           & 
& MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,              & 
& ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& vvSM,vS,gThh,gTSsc,gTVWp,gTVZ,gFeFecFdFd,gFeFecFeFe,gFeFecFuFu,gFeFecFxvFxv,           & 
& gFeFecFvFv,gFeFecFxeFxe,gFeFvcFuFd,gFeFvcFxvFxe,epsI,deltaM,.False.,gTFe,              & 
& gPFe(:,18:173),BRFe(:,18:173))

End If 
Do i1=1,3
gTFe(i1) =Sum(gPFe(i1,:)) 
If (gTFe(i1).Gt.0._dp) BRFe(i1,: ) =gPFe(i1,:)/gTFe(i1) 
End Do 
 

If (.Not.CTBD) Then 
If ((Enable3BDecaysF).and.(Calc3BodyDecay_Fd)) Then 
If (MaxVal(gTFd).Lt.MaxVal(fac3*Abs(MFd))) Then 
Call FdThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,           & 
& MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,              & 
& ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& vvSM,vS,gThh,gTVWp,gTVZ,gFdFdcFdFd,gFdFdcFeFe,gFdFdcFuFu,gFdFdcFxvFxv,gFdFdcFvFv,      & 
& gFdFdcFxeFxe,gFdFucFvFe,gFdFucFxvFxe,epsI,deltaM,.False.,gTFd,gPFd(:,19:174)           & 
& ,BRFd(:,19:174))

Else 
Call FdThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,           & 
& MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,              & 
& ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& vvSM,vS,gThh,gTVWp,gTVZ,gFdFdcFdFd,gFdFdcFeFe,gFdFdcFuFu,gFdFdcFxvFxv,gFdFdcFvFv,      & 
& gFdFdcFxeFxe,gFdFucFvFe,gFdFucFxvFxe,epsI,deltaM,.True.,gTFd,gPFd(:,19:174)            & 
& ,BRFd(:,19:174))

End If 
 
End If 
Else 
Call FdThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,           & 
& MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,              & 
& ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,g1,g2,g3,Lam,LS1H,LS,             & 
& LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,             & 
& vvSM,vS,gThh,gTVWp,gTVZ,gFdFdcFdFd,gFdFdcFeFe,gFdFdcFuFu,gFdFdcFxvFxv,gFdFdcFvFv,      & 
& gFdFdcFxeFxe,gFdFucFvFe,gFdFucFxvFxe,epsI,deltaM,.False.,gTFd,gPFd(:,19:174)           & 
& ,BRFd(:,19:174))

End If 
Do i1=1,3
gTFd(i1) =Sum(gPFd(i1,:)) 
If (gTFd(i1).Gt.0._dp) BRFd(i1,: ) =gPFd(i1,:)/gTFd(i1) 
End Do 
 

! No 3-body decays for hh  
! No 3-body decays for Ssc  
! No 3-body decays for Fxe  
! No 3-body decays for Fxv  
Iname = Iname - 1 
 
End Subroutine CalculateBR 
End Module BranchingRatios_SDdiracDM 
 