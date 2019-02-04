Module Pole2L_SDdiracDM 
 
Use Control 
Use Settings 
Use Couplings_SDdiracDM 
Use AddLoopFunctions 
Use LoopFunctions 
Use Mathematics 
Use MathematicsQP 
Use Model_Data_SDdiracDM 
Use StandardModel 
Use TreeLevelMasses_SDdiracDM 
Use Pole2LFunctions
Contains 
 
Subroutine CalculatePi2S(p2,vvSM,vS,g1,g2,g3,Lam,LS1H,LS,LS2H,LSP,LSPH,               & 
& Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,MS22,mP2,kont,tad2L,Pi2S,Pi2P)

Implicit None 
Real(dp),Intent(in) :: g1,g2,g3,LS1H,LS,LS2H,LSP,LSPH,YRD,YRB1(3),YRB2(3),YRC,YRA1(3),YRA2(3),               & 
& MDF,m2SM,MS12,MS22,mP2

Complex(dp),Intent(in) :: Lam,Yu(3,3),Yd(3,3),Ye(3,3)

Real(dp),Intent(in) :: vvSM,vS

Real(dp) :: MAh,MAh2,MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(3),MFv2(3),MFxe,            & 
& MFxe2,MFxv(2),MFxv2(2),Mhh(2),Mhh2(2),MHp,MHp2,MSsc(2),MSsc2(2),MVWp,MVWp2,            & 
& MVZ,MVZ2,TW,VSs(2,2),ZH(2,2),ZZ(2,2),alphaH

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),UV(3,3),UVR(3,3),               & 
& XU(2,2),XV(2,2),ZW(2,2)

Complex(dp) :: cplAhAhhh(2),cplhhhhhh(2,2,2),cplhhHpcHp(2),cplhhSsccSsc(2,2,2),cplVGVGVG,            & 
& cplcFdFdAhL(3,3),cplcFdFdAhR(3,3),cplcFeFeAhL(3,3),cplcFeFeAhR(3,3),cplcFuFuAhL(3,3),  & 
& cplcFuFuAhR(3,3),cplcFxvFxvAhL(2,2),cplcFxvFxvAhR(2,2),cplcFdFdhhL(3,3,2),             & 
& cplcFdFdhhR(3,3,2),cplcFuFdHpL(3,3),cplcFuFdHpR(3,3),cplcFeFehhL(3,3,2),               & 
& cplcFeFehhR(3,3,2),cplcFvFeHpL(3,3),cplcFvFeHpR(3,3),cplcFxeFeSscL(3,2),               & 
& cplcFxeFeSscR(3,2),cplcFuFuhhL(3,3,2),cplcFuFuhhR(3,3,2),cplcFdFucHpL(3,3),            & 
& cplcFdFucHpR(3,3),cplcFxvFvSscL(2,3,2),cplcFxvFvSscR(2,3,2),cplcFeFvcHpL(3,3),         & 
& cplcFeFvcHpR(3,3),cplcFxvFxeHpL(2),cplcFxvFxeHpR(2),cplcFeFxecSscL(3,2),               & 
& cplcFeFxecSscR(3,2),cplcFxvFxvhhL(2,2,2),cplcFxvFxvhhR(2,2,2),cplcFvFxvcSscL(3,2,2),   & 
& cplcFvFxvcSscR(3,2,2),cplcFxeFxvcHpL(2),cplcFxeFxvcHpR(2),cplcFdFdVGL(3,3),            & 
& cplcFdFdVGR(3,3),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3)

Complex(dp) :: cplAhAhAhAh,cplAhAhhhhh(2,2),cplAhAhHpcHp,cplAhAhSsccSsc(2,2),cplhhhhhhhh(2,2,2,2),   & 
& cplhhhhHpcHp(2,2),cplhhhhSsccSsc(2,2,2,2),cplHpHpcHpcHp,cplHpSsccHpcSsc(2,2),          & 
& cplSscSsccSsccSsc(2,2,2,2)

Real(dp), Intent(in) :: p2
Integer, Intent(inout):: kont
Integer :: gE1,gE2,i,i1,i2,i3,i4,i5 
Real(dp) :: Qscale,prefactor,funcvalue
complex(dp) :: cplxprefactor,A0m
Real(dp)  :: temptad(2)
Real(dp)  :: tempcont(2,2)
Real(dp)  :: tempcontah(1,1)
Real(dp)  :: runningval(2,2)
Real(dp), Intent(out) :: tad2l(2)
Real(dp), Intent(out) :: Pi2S(2,2)
Real(dp), Intent(out) :: Pi2P(1,1)
complex(dp) :: coup1,coup2,coup3,coup4
complex(dp) :: coup1L,coup1R,coup2l,coup2r,coup3l,coup3r,coup4l,coup4r
real(dp) :: epsFmass
real(dp) :: epscouplings
Real(dp)  :: tempcouplingvector(2)
Real(dp)  :: tempcouplingmatrix(2,2)
Real(dp)  :: tempcouplingmatrixah(1,1)
logical :: nonzerocoupling
real(dp) :: delta2Ltadpoles(2)
real(dp)  :: delta2Lmasses(2,2)
real(dp)  :: delta2Lmassesah(1,1)
Real(dp)  :: tad1LG(2)
complex(dp) :: tad1Lmatrixhh(2,2)
complex(dp) :: tad1LmatrixHp(1,1)
complex(dp) :: tad1LmatrixAh(1,1)


tad2l(:)=0
Pi2S(:,:)=0
Pi2P(:,:)=0
Qscale=getrenormalizationscale()
epsfmass=0._dp
epscouplings=1.0E-6_dp
Call TreeMassesEffPot(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MFxe,              & 
& MFxe2,MFxv,MFxv2,Mhh,Mhh2,MHp,MHp2,MSsc,MSsc2,MVWp,MVWp2,MVZ,MVZ2,TW,ZDR,              & 
& ZER,ZUR,ZDL,ZEL,VSs,ZUL,UV,UVR,XU,XV,ZH,ZW,ZZ,alphaH,vvSM,vS,g1,g2,g3,Lam,             & 
& LS1H,LS,LS2H,LSP,LSPH,Yu,Yd,Ye,YRD,YRB1,YRB2,YRC,YRA1,YRA2,MDF,m2SM,MS12,              & 
& MS22,mP2,.True.,kont)

If (Abs(MHp2/Qscale).lt.TwoLoopRegulatorMass) MHp2=Qscale*TwoLoopRegulatorMass
If (Abs(MAh2/Qscale).lt.TwoLoopRegulatorMass) MAh2=Qscale*TwoLoopRegulatorMass
Where (Abs(Mhh2/Qscale).lt.TwoLoopRegulatorMass )Mhh2=Qscale*TwoLoopRegulatorMass
Where (Abs(MSsc2/Qscale).lt.TwoLoopRegulatorMass )MSsc2=Qscale*TwoLoopRegulatorMass
Call CouplingsFor2LPole3(Lam,LSPH,vvSM,vS,ZH,LSP,LS1H,LS2H,VSs,g3,Yd,ZDL,             & 
& ZDR,Ye,ZEL,ZER,Yu,ZUL,ZUR,YRD,XV,XU,UV,YRA1,YRA2,YRB1,YRB2,UVR,YRC,cplAhAhhh,          & 
& cplhhhhhh,cplhhHpcHp,cplhhSsccSsc,cplVGVGVG,cplcFdFdAhL,cplcFdFdAhR,cplcFeFeAhL,       & 
& cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplcFxvFxvAhL,cplcFxvFxvAhR,cplcFdFdhhL,           & 
& cplcFdFdhhR,cplcFuFdHpL,cplcFuFdHpR,cplcFeFehhL,cplcFeFehhR,cplcFvFeHpL,               & 
& cplcFvFeHpR,cplcFxeFeSscL,cplcFxeFeSscR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFucHpL,          & 
& cplcFdFucHpR,cplcFxvFvSscL,cplcFxvFvSscR,cplcFeFvcHpL,cplcFeFvcHpR,cplcFxvFxeHpL,      & 
& cplcFxvFxeHpR,cplcFeFxecSscL,cplcFeFxecSscR,cplcFxvFxvhhL,cplcFxvFxvhhR,               & 
& cplcFvFxvcSscL,cplcFvFxvcSscR,cplcFxeFxvcHpL,cplcFxeFxvcHpR,cplcFdFdVGL,               & 
& cplcFdFdVGR,cplcFuFuVGL,cplcFuFuVGR)

Call CouplingsFor2LPole4(Lam,LSPH,ZH,LS1H,LS2H,VSs,LSP,LS,cplAhAhAhAh,cplAhAhhhhh,    & 
& cplAhAhHpcHp,cplAhAhSsccSsc,cplhhhhhhhh,cplhhhhHpcHp,cplhhhhSsccSsc,cplHpHpcHpcHp,     & 
& cplHpSsccHpcSsc,cplSscSsccSsccSsc)

! ----------------------------------
! ------- 1L GAUGELESS TADPOLE DIAGRAMS --------
! ----------------------------------
delta2Ltadpoles(:)=0._dp
delta2Lmasses(:,:)=0._dp
delta2LmassesAh(:,:)=0._dp
tad1LG(:)=0._dp
if(include1l2lshift) then
temptad(:) = 0._dp 
A0m = 1._dp/2._dp*(-J0(MAh2,qscale)) 
  Do gE1 = 1, 2
coup1 = cplAhAhhh(gE1)
   temptad(gE1) = temptad(gE1)-real(coup1*A0m,dp) 
  End Do 
  Do i1 = 1, 3
A0m = 3._dp*(-J0(MFd2(i1),qscale)) 
  Do gE1 = 1, 2
coup1L = cplcFdFdhhL(i1,i1,gE1)
coup1R = cplcFdFdhhR(i1,i1,gE1)
  temptad(gE1)  = temptad(gE1)+ 2._dp*MFd(i1)*real((coup1L+coup1R)*A0m,dp) 
  End Do 
  End do 

  Do i1 = 1, 3
A0m = 1._dp*(-J0(MFe2(i1),qscale)) 
  Do gE1 = 1, 2
coup1L = cplcFeFehhL(i1,i1,gE1)
coup1R = cplcFeFehhR(i1,i1,gE1)
  temptad(gE1)  = temptad(gE1)+ 2._dp*MFe(i1)*real((coup1L+coup1R)*A0m,dp) 
  End Do 
  End do 

  Do i1 = 1, 3
A0m = 3._dp*(-J0(MFu2(i1),qscale)) 
  Do gE1 = 1, 2
coup1L = cplcFuFuhhL(i1,i1,gE1)
coup1R = cplcFuFuhhR(i1,i1,gE1)
  temptad(gE1)  = temptad(gE1)+ 2._dp*MFu(i1)*real((coup1L+coup1R)*A0m,dp) 
  End Do 
  End do 

  Do i1 = 1, 2
A0m = 1._dp*(-J0(MFxv2(i1),qscale)) 
  Do gE1 = 1, 2
coup1L = cplcFxvFxvhhL(i1,i1,gE1)
coup1R = cplcFxvFxvhhR(i1,i1,gE1)
  temptad(gE1)  = temptad(gE1)+ 2._dp*MFxv(i1)*real((coup1L+coup1R)*A0m,dp) 
  End Do 
  End do 

  Do i1 = 1, 2
A0m = 1._dp/2._dp*(-J0(Mhh2(i1),qscale)) 
  Do gE1 = 1, 2
coup1 = cplhhhhhh(gE1,i1,i1)
   temptad(gE1) = temptad(gE1)-real(coup1*A0m,dp) 
  End Do 
  End do 

A0m = 1._dp*(-J0(MHp2,qscale)) 
  Do gE1 = 1, 2
coup1 = cplhhHpcHp(gE1)
   temptad(gE1) = temptad(gE1)-real(coup1*A0m,dp) 
  End Do 
  Do i1 = 1, 2
A0m = 1._dp*(-J0(MSsc2(i1),qscale)) 
  Do gE1 = 1, 2
coup1 = cplhhSsccSsc(gE1,i1,i1)
   temptad(gE1) = temptad(gE1)-real(coup1*A0m,dp) 
  End Do 
  End do 

tad1LG=matmul(temptad*oo16Pi2,ZH)
! ----------------------------
! ----------------------------------
! ------- 1L2L SHIFTS --------
! ----------------------------------
tad1Lmatrixhh=0._dp
tad1Lmatrixhh(1,1)=tad1Lmatrixhh(1,1)+1/vvSM*tad1LG(1)
tad1Lmatrixhh(2,2)=tad1Lmatrixhh(2,2)+1/vS*tad1LG(2)
tad1Lmatrixhh=matmul(ZH,matmul(tad1Lmatrixhh,transpose(ZH)))
do i1=1,2
do i2=1,2
 funcvalue= tad1Lmatrixhh(i2,i1)*BB(Mhh2(i1),Mhh2(i2),qscale)
do gE1=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
delta2Ltadpoles(gE1)=delta2Ltadpoles(gE1)+real(0.5_dp*coup1*1._dp*funcvalue,dp)
end do
do gE1=1,2
do gE2=1,2
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
delta2Lmasses(gE1,gE2)=delta2Lmasses(gE1,gE2)+real(0.5_dp*coup1*1._dp*funcvalue,dp)
end do
 end do
end do 
 end do
do i1=1,2
do i2=1,2
do i3=1,2
 funcvalue= tad1Lmatrixhh(i2,i3)*CCtilde(Mhh2(i1),Mhh2(i2),Mhh2(i3),qscale)
do gE1=1,2
do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i3,i1)
delta2Lmasses(gE1,gE2)=delta2Lmasses(gE1,gE2)+real(coup1*coup2*1._dp*funcvalue,dp)
end do
 end do
end do 
 end do
 end do
do i1=1,1
do i2=1,2
do i3=1,2
 funcvalue= tad1Lmatrixhh(i2,i3)*CCtilde(MAh2,Mhh2(i2),Mhh2(i3),Qscale)
do gE1=1,1
do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i3)
delta2LmassesAh(gE1,gE2)=delta2LmassesAh(gE1,gE2)+real(coup1*coup2*1._dp*funcvalue,dp)
end do
 end do
end do 
 end do
 end do
tad1LmatrixHp=0._dp
tad1LmatrixHp(1,1)=tad1LmatrixHp(1,1)+1/vvSM*tad1LG(1)
do i1=1,1
do i2=1,1
 funcvalue= tad1LmatrixHp(i2,i1)*BB(MHp2,MHp2,qscale)
do gE1=1,2
coup1 = cplhhHpcHp(gE1)
delta2Ltadpoles(gE1)=delta2Ltadpoles(gE1)+real(0.5_dp*coup1*2._dp*funcvalue,dp)
end do
do gE1=1,2
do gE2=1,2
coup1 = cplhhhhHpcHp(gE1,gE2)
delta2Lmasses(gE1,gE2)=delta2Lmasses(gE1,gE2)+real(0.5_dp*coup1*2._dp*funcvalue,dp)
end do
 end do
end do 
 end do
do i1=1,1
do i2=1,1
do i3=1,1
 funcvalue= tad1LmatrixHp(i2,i3)*CCtilde(MHp2,MHp2,MHp2,qscale)
do gE1=1,2
do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhHpcHp(gE2)
delta2Lmasses(gE1,gE2)=delta2Lmasses(gE1,gE2)+real(coup1*coup2*2._dp*funcvalue,dp)
end do
 end do
end do 
 end do
 end do
tad1LmatrixAh=0._dp
tad1LmatrixAh(1,1)=tad1LmatrixAh(1,1)+1/vvSM*tad1LG(1)
do i1=1,1
do i2=1,1
 funcvalue= tad1LmatrixAh(i2,i1)*BB(MAh2,MAh2,qscale)
do gE1=1,2
coup1 = cplAhAhhh(gE1)
delta2Ltadpoles(gE1)=delta2Ltadpoles(gE1)+real(0.5_dp*coup1*1._dp*funcvalue,dp)
end do
do gE1=1,2
do gE2=1,2
coup1 = cplAhAhhhhh(gE1,gE2)
delta2Lmasses(gE1,gE2)=delta2Lmasses(gE1,gE2)+real(0.5_dp*coup1*1._dp*funcvalue,dp)
end do
 end do
end do 
 end do
do i1=1,1
do i2=1,1
do i3=1,1
 funcvalue= tad1LmatrixAh(i2,i3)*CCtilde(MAh2,MAh2,MAh2,qscale)
do gE1=1,2
do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhh(gE2)
delta2Lmasses(gE1,gE2)=delta2Lmasses(gE1,gE2)+real(coup1*coup2*1._dp*funcvalue,dp)
end do
 end do
end do 
 end do
 end do
do i1=1,2
do i2=1,1
do i3=1,1
 funcvalue= tad1LmatrixAh(i2,i3)*CCtilde(Mhh2(i1),MAh2,MAh2,Qscale)
do gE1=1,1
do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2 = cplAhAhhh(i1)
delta2LmassesAh(gE1,gE2)=delta2LmassesAh(gE1,gE2)+real(coup1*coup2*1._dp*funcvalue,dp)
end do
 end do
end do 
 end do
 end do
delta2Ltadpoles=delta2Ltadpoles*oo16Pi2
delta2Lmasses=delta2Lmasses*oo16Pi2
delta2LmassesAh=delta2LmassesAh*oo16Pi2
! ----------------------------
end if ! include1l2lshift
! ----------------------------------
! ------- TADPOLE DIAGRAMS --------
! ----------------------------------
temptad(:)=0._dp
tempcouplingvector(:)=0._dp
! ---- Topology ToSSS
! ---- Ah,Ah,hh ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplAhAhhhhh(gE1,i3)
coup2 = cplAhAhhh(i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*TfSSS(MAh2,MAh2,Mhh2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
! ---- hh,hh,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhhhhh(gE1,i1,i2,i3)
coup2 = cplhhhhhh(i1,i2,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/6._dp*TfSSS(Mhh2(i1),Mhh2(i2),Mhh2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- hh,Hp,conj[Hp] ----
Do i1=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhHpcHp(gE1,i1)
coup2 = cplhhHpcHp(i1)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp*TfSSS(Mhh2(i1),MHp2,MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
! ---- hh,Ssc,conj[Ssc] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhSsccSsc(gE1,i1,i2,i3)
coup2 = cplhhSsccSsc(i1,i3,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp*TfSSS(Mhh2(i1),MSsc2(i2),MSsc2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Topology ToSS
! ---- Ah,Ah,Ah ----
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhAhAh
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*TfSS(MAh2,MAh2,MAh2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
! ---- Ah,Ah,hh ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhhhh(i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*TfSS(MAh2,MAh2,Mhh2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
! ---- Ah,Ah,Hp ----
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhHpcHp
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*TfSS(MAh2,MAh2,MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
! ---- Ah,Ah,Ssc ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhSsccSsc(i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*TfSS(MAh2,MAh2,MSsc2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
! ---- hh,hh,Ah ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplAhAhhhhh(i1,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*TfSS(Mhh2(i1),Mhh2(i2),MAh2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
! ---- hh,hh,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhhhh(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*TfSS(Mhh2(i1),Mhh2(i2),Mhh2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- hh,hh,Hp ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhHpcHp(i1,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*TfSS(Mhh2(i1),Mhh2(i2),MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
! ---- hh,hh,Ssc ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhSsccSsc(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*TfSS(Mhh2(i1),Mhh2(i2),MSsc2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Hp,conj[Hp],Ah ----
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplAhAhHpcHp
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*TfSS(MHp2,MHp2,MAh2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
! ---- Hp,conj[Hp],hh ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhhhHpcHp(i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*TfSS(MHp2,MHp2,Mhh2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
! ---- Hp,conj[Hp],Hp ----
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplHpHpcHpcHp
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp*TfSS(MHp2,MHp2,MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
! ---- Hp,conj[Hp],Ssc ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplHpSsccHpcSsc(i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp*TfSS(MHp2,MHp2,MSsc2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
! ---- Ssc,conj[Ssc],Ah ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2 = cplAhAhSsccSsc(i2,i1)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*TfSS(MSsc2(i1),MSsc2(i2),MAh2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
! ---- Ssc,conj[Ssc],hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2 = cplhhhhSsccSsc(i3,i3,i2,i1)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*TfSS(MSsc2(i1),MSsc2(i2),Mhh2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Ssc,conj[Ssc],Hp ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2 = cplHpSsccHpcSsc(i2,i1)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp*TfSS(MSsc2(i1),MSsc2(i2),MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
! ---- Ssc,conj[Ssc],Ssc ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2 = cplSscSsccSsccSsc(i2,i3,i1,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 1._dp*TfSS(MSsc2(i1),MSsc2(i2),MSsc2(i3),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
! ---- Topology ToSSSS
! ---- Ah,Ah,Ah,hh ----
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhh(i4)
coup3 = cplAhAhhh(i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*TfSSSS(MAh2,MAh2,MAh2,Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
! ---- hh,hh,Ah,Ah ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplAhAhhh(i1)
coup3 = cplAhAhhh(i2)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp/4._dp*TfSSSS(Mhh2(i1),Mhh2(i2),MAh2,MAh2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
! ---- hh,hh,hh,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(i1,i3,i4)
coup3 = cplhhhhhh(i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp/4._dp*TfSSSS(Mhh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Hp,conj[Hp] ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhHpcHp(i1)
coup3 = cplhhHpcHp(i2)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*TfSSSS(Mhh2(i1),Mhh2(i2),MHp2,MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
! ---- hh,hh,Ssc,conj[Ssc] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhSsccSsc(i1,i3,i4)
coup3 = cplhhSsccSsc(i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*TfSSSS(Mhh2(i1),Mhh2(i2),MSsc2(i3),MSsc2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
end do
! ---- Hp,conj[Hp],hh,Hp ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhHpcHp(i3)
coup3 = cplhhHpcHp(i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp*TfSSSS(MHp2,MHp2,Mhh2(i3),MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
! ---- Ssc,conj[Ssc],hh,Ssc ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2 = cplhhSsccSsc(i3,i4,i1)
coup3 = cplhhSsccSsc(i3,i2,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp*TfSSSS(MSsc2(i1),MSsc2(i2),Mhh2(i3),MSsc2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end do
end do
end do
end do
! ---- Topology ToSSFF
! ---- Ah,Ah,Fd,bar[Fd] ----
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFdFdAhL(i4,i3)
coup2R = cplcFdFdAhR(i4,i3)
coup3L = cplcFdFdAhL(i3,i4)
coup3R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*TfSSFF(MAh2,MAh2,MFd2(i3),MFd2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFdFdAhL(i4,i3)
coup2R = cplcFdFdAhR(i4,i3)
coup3L = cplcFdFdAhL(i3,i4)
coup3R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i3)*MFd(i4)*TfSSFbFb(MAh2,MAh2,MFd2(i3),MFd2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
! ---- Ah,Ah,Fe,bar[Fe] ----
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFeFeAhL(i4,i3)
coup2R = cplcFeFeAhR(i4,i3)
coup3L = cplcFeFeAhL(i3,i4)
coup3R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*TfSSFF(MAh2,MAh2,MFe2(i3),MFe2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFeFeAhL(i4,i3)
coup2R = cplcFeFeAhR(i4,i3)
coup3L = cplcFeFeAhL(i3,i4)
coup3R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= MFe(i3)*MFe(i4)*TfSSFbFb(MAh2,MAh2,MFe2(i3),MFe2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
! ---- Ah,Ah,Fu,bar[Fu] ----
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFuFuAhL(i4,i3)
coup2R = cplcFuFuAhR(i4,i3)
coup3L = cplcFuFuAhL(i3,i4)
coup3R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*TfSSFF(MAh2,MAh2,MFu2(i3),MFu2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFuFuAhL(i4,i3)
coup2R = cplcFuFuAhR(i4,i3)
coup3L = cplcFuFuAhL(i3,i4)
coup3R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i3)*MFu(i4)*TfSSFbFb(MAh2,MAh2,MFu2(i3),MFu2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
! ---- Ah,Ah,Fxv,bar[Fxv] ----
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFxvFxvAhL(i4,i3)
coup2R = cplcFxvFxvAhR(i4,i3)
coup3L = cplcFxvFxvAhL(i3,i4)
coup3R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*TfSSFF(MAh2,MAh2,MFxv2(i3),MFxv2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFxvFxvAhL(i4,i3)
coup2R = cplcFxvFxvAhR(i4,i3)
coup3L = cplcFxvFxvAhL(i3,i4)
coup3R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= MFxv(i3)*MFxv(i4)*TfSSFbFb(MAh2,MAh2,MFxv2(i3),MFxv2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
! ---- hh,hh,Fd,bar[Fd] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2L = cplcFdFdhhL(i4,i3,i1)
coup2R = cplcFdFdhhR(i4,i3,i1)
coup3L = cplcFdFdhhL(i3,i4,i2)
coup3R = cplcFdFdhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*TfSSFF(Mhh2(i1),Mhh2(i2),MFd2(i3),MFd2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2L = cplcFdFdhhL(i4,i3,i1)
coup2R = cplcFdFdhhR(i4,i3,i1)
coup3L = cplcFdFdhhL(i3,i4,i2)
coup3R = cplcFdFdhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i3)*MFd(i4)*TfSSFbFb(Mhh2(i1),Mhh2(i2),MFd2(i3),MFd2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- hh,hh,Fe,bar[Fe] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2L = cplcFeFehhL(i4,i3,i1)
coup2R = cplcFeFehhR(i4,i3,i1)
coup3L = cplcFeFehhL(i3,i4,i2)
coup3R = cplcFeFehhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*TfSSFF(Mhh2(i1),Mhh2(i2),MFe2(i3),MFe2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2L = cplcFeFehhL(i4,i3,i1)
coup2R = cplcFeFehhR(i4,i3,i1)
coup3L = cplcFeFehhL(i3,i4,i2)
coup3R = cplcFeFehhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= MFe(i3)*MFe(i4)*TfSSFbFb(Mhh2(i1),Mhh2(i2),MFe2(i3),MFe2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- hh,hh,Fu,bar[Fu] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2L = cplcFuFuhhL(i4,i3,i1)
coup2R = cplcFuFuhhR(i4,i3,i1)
coup3L = cplcFuFuhhL(i3,i4,i2)
coup3R = cplcFuFuhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*TfSSFF(Mhh2(i1),Mhh2(i2),MFu2(i3),MFu2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2L = cplcFuFuhhL(i4,i3,i1)
coup2R = cplcFuFuhhR(i4,i3,i1)
coup3L = cplcFuFuhhL(i3,i4,i2)
coup3R = cplcFuFuhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i3)*MFu(i4)*TfSSFbFb(Mhh2(i1),Mhh2(i2),MFu2(i3),MFu2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- hh,hh,Fxv,bar[Fxv] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2L = cplcFxvFxvhhL(i4,i3,i1)
coup2R = cplcFxvFxvhhR(i4,i3,i1)
coup3L = cplcFxvFxvhhL(i3,i4,i2)
coup3R = cplcFxvFxvhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*TfSSFF(Mhh2(i1),Mhh2(i2),MFxv2(i3),MFxv2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2L = cplcFxvFxvhhL(i4,i3,i1)
coup2R = cplcFxvFxvhhR(i4,i3,i1)
coup3L = cplcFxvFxvhhL(i3,i4,i2)
coup3R = cplcFxvFxvhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= MFxv(i3)*MFxv(i4)*TfSSFbFb(Mhh2(i1),Mhh2(i2),MFxv2(i3),MFxv2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Hp,conj[Hp],Fu,bar[Fd] ----
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFdFucHpL(i4,i3)
coup2R = cplcFdFucHpR(i4,i3)
coup3L = cplcFuFdHpL(i3,i4)
coup3R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -3._dp*TfSSFF(MHp2,MHp2,MFu2(i3),MFd2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFd(i4) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFdFucHpL(i4,i3)
coup2R = cplcFdFucHpR(i4,i3)
coup3L = cplcFuFdHpL(i3,i4)
coup3R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i3)*MFd(i4)*TfSSFbFb(MHp2,MHp2,MFu2(i3),MFd2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
! ---- Hp,conj[Hp],Fv,bar[Fe] ----
Do i3=1,3
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFeFvcHpL(i4,i3)
coup2R = cplcFeFvcHpR(i4,i3)
coup3L = cplcFvFeHpL(i3,i4)
coup3R = cplcFvFeHpR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp*TfSSFF(MHp2,MHp2,MFv2(i3),MFe2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFe(i4) .gt. epsfmass) .and. (MFv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFeFvcHpL(i4,i3)
coup2R = cplcFeFvcHpR(i4,i3)
coup3L = cplcFvFeHpL(i3,i4)
coup3R = cplcFvFeHpR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFv(i3)*MFe(i4)*TfSSFbFb(MHp2,MHp2,MFv2(i3),MFe2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
! ---- Hp,conj[Hp],Fxv,bar[Fxe] ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFxeFxvcHpL(i3)
coup2R = cplcFxeFxvcHpR(i3)
coup3L = cplcFxvFxeHpL(i3)
coup3R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp*TfSSFF(MHp2,MHp2,MFxv2(i3),MFxe2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFxe .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFxeFxvcHpL(i3)
coup2R = cplcFxeFxvcHpR(i3)
coup3L = cplcFxvFxeHpL(i3)
coup3R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MFxv(i3)*TfSSFbFb(MHp2,MHp2,MFxv2(i3),MFxe2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
! ---- Ssc,conj[Ssc],Fxe,bar[Fe] ----
Do i1=1,2
Do i2=1,2
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2L = cplcFeFxecSscL(i4,i1)
coup2R = cplcFeFxecSscR(i4,i1)
coup3L = cplcFxeFeSscL(i4,i2)
coup3R = cplcFxeFeSscR(i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp*TfSSFF(MSsc2(i1),MSsc2(i2),MFxe2,MFe2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFe(i4) .gt. epsfmass) .and. (MFxe .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2L = cplcFeFxecSscL(i4,i1)
coup2R = cplcFeFxecSscR(i4,i1)
coup3L = cplcFxeFeSscL(i4,i2)
coup3R = cplcFxeFeSscR(i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MFe(i4)*TfSSFbFb(MSsc2(i1),MSsc2(i2),MFxe2,MFe2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
! ---- Ssc,conj[Ssc],Fxv,bar[Fv] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2L = cplcFvFxvcSscL(i4,i3,i1)
coup2R = cplcFvFxvcSscR(i4,i3,i1)
coup3L = cplcFxvFvSscL(i3,i4,i2)
coup3R = cplcFxvFvSscR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -1._dp*TfSSFF(MSsc2(i1),MSsc2(i2),MFxv2(i3),MFv2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
if((MFv(i4) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2L = cplcFvFxvcSscL(i4,i3,i1)
coup2R = cplcFvFxvcSscR(i4,i3,i1)
coup3L = cplcFxvFvSscL(i3,i4,i2)
coup3R = cplcFxvFvSscR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i3)*MFv(i4)*TfSSFbFb(MSsc2(i1),MSsc2(i2),MFxv2(i3),MFv2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Topology ToFFFS
! ---- Fd,bar[Fd],Fd,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
if((MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFdFdAhL(i3,i2)
coup3R = cplcFdFdAhR(i3,i2)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFd(i3)*TfFFFbS(MFd2(i1),MFd2(i2),MFd2(i3),MAh2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFdFdAhL(i3,i2)
coup3R = cplcFdFdAhR(i3,i2)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFd(i2)*TfFFbFS(MFd2(i1),MFd2(i2),MFd2(i3),MAh2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFdFdAhL(i3,i2)
coup3R = cplcFdFdAhR(i3,i2)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i3)*MFd(i2)*TfFbFbFbS(MFd2(i1),MFd2(i2),MFd2(i3),MAh2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
! ---- Fd,bar[Fd],Fd,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,2
if((MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,i4)
coup2R = cplcFdFdhhR(i1,i3,i4)
coup3L = cplcFdFdhhL(i3,i2,i4)
coup3R = cplcFdFdhhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFd(i3)*TfFFFbS(MFd2(i1),MFd2(i2),MFd2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,i4)
coup2R = cplcFdFdhhR(i1,i3,i4)
coup3L = cplcFdFdhhL(i3,i2,i4)
coup3R = cplcFdFdhhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFd(i2)*TfFFbFS(MFd2(i1),MFd2(i2),MFd2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,i4)
coup2R = cplcFdFdhhR(i1,i3,i4)
coup3L = cplcFdFdhhL(i3,i2,i4)
coup3R = cplcFdFdhhR(i3,i2,i4)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i3)*MFd(i2)*TfFbFbFbS(MFd2(i1),MFd2(i2),MFd2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Fd,bar[Fd],Fu,conj[Hp] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
if((MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFucHpL(i1,i3)
coup2R = cplcFdFucHpR(i1,i3)
coup3L = cplcFuFdHpL(i3,i2)
coup3R = cplcFuFdHpR(i3,i2)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFu(i3)*TfFFFbS(MFd2(i1),MFd2(i2),MFu2(i3),MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFucHpL(i1,i3)
coup2R = cplcFdFucHpR(i1,i3)
coup3L = cplcFuFdHpL(i3,i2)
coup3R = cplcFuFdHpR(i3,i2)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFd(i2)*TfFFbFS(MFd2(i1),MFd2(i2),MFu2(i3),MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFucHpL(i1,i3)
coup2R = cplcFdFucHpR(i1,i3)
coup3L = cplcFuFdHpL(i3,i2)
coup3R = cplcFuFdHpR(i3,i2)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFu(i3)*MFd(i2)*TfFbFbFbS(MFd2(i1),MFd2(i2),MFu2(i3),MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
! ---- Fe,bar[Fe],Fe,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
if((MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFeFeAhL(i3,i2)
coup3R = cplcFeFeAhR(i3,i2)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFe(i3)*TfFFFbS(MFe2(i1),MFe2(i2),MFe2(i3),MAh2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFeFeAhL(i3,i2)
coup3R = cplcFeFeAhR(i3,i2)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFe(i2)*TfFFbFS(MFe2(i1),MFe2(i2),MFe2(i3),MAh2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFeFeAhL(i3,i2)
coup3R = cplcFeFeAhR(i3,i2)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i3)*MFe(i2)*TfFbFbFbS(MFe2(i1),MFe2(i2),MFe2(i3),MAh2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
! ---- Fe,bar[Fe],Fe,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,2
if((MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,i4)
coup2R = cplcFeFehhR(i1,i3,i4)
coup3L = cplcFeFehhL(i3,i2,i4)
coup3R = cplcFeFehhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFe(i3)*TfFFFbS(MFe2(i1),MFe2(i2),MFe2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,i4)
coup2R = cplcFeFehhR(i1,i3,i4)
coup3L = cplcFeFehhL(i3,i2,i4)
coup3R = cplcFeFehhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFe(i2)*TfFFbFS(MFe2(i1),MFe2(i2),MFe2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,i4)
coup2R = cplcFeFehhR(i1,i3,i4)
coup3L = cplcFeFehhL(i3,i2,i4)
coup3R = cplcFeFehhR(i3,i2,i4)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i3)*MFe(i2)*TfFbFbFbS(MFe2(i1),MFe2(i2),MFe2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Fe,bar[Fe],Fv,conj[Hp] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
if((MFv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFvcHpL(i1,i3)
coup2R = cplcFeFvcHpR(i1,i3)
coup3L = cplcFvFeHpL(i3,i2)
coup3R = cplcFvFeHpR(i3,i2)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFv(i3)*TfFFFbS(MFe2(i1),MFe2(i2),MFv2(i3),MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFvcHpL(i1,i3)
coup2R = cplcFeFvcHpR(i1,i3)
coup3L = cplcFvFeHpL(i3,i2)
coup3R = cplcFvFeHpR(i3,i2)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFe(i2)*TfFFbFS(MFe2(i1),MFe2(i2),MFv2(i3),MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFvcHpL(i1,i3)
coup2R = cplcFeFvcHpR(i1,i3)
coup3L = cplcFvFeHpL(i3,i2)
coup3R = cplcFvFeHpR(i3,i2)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFv(i3)*MFe(i2)*TfFbFbFbS(MFe2(i1),MFe2(i2),MFv2(i3),MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
! ---- Fe,bar[Fe],Fxe,conj[Ssc] ----
Do i1=1,3
Do i2=1,3
Do i4=1,2
if((MFxe .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFxecSscL(i1,i4)
coup2R = cplcFeFxecSscR(i1,i4)
coup3L = cplcFxeFeSscL(i2,i4)
coup3R = cplcFxeFeSscR(i2,i4)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFxe*TfFFFbS(MFe2(i1),MFe2(i2),MFxe2,MSsc2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFxecSscL(i1,i4)
coup2R = cplcFeFxecSscR(i1,i4)
coup3L = cplcFxeFeSscL(i2,i4)
coup3R = cplcFxeFeSscR(i2,i4)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFe(i2)*TfFFbFS(MFe2(i1),MFe2(i2),MFxe2,MSsc2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFxe .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFxecSscL(i1,i4)
coup2R = cplcFeFxecSscR(i1,i4)
coup3L = cplcFxeFeSscL(i2,i4)
coup3R = cplcFxeFeSscR(i2,i4)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MFe(i1)*MFe(i2)*TfFbFbFbS(MFe2(i1),MFe2(i2),MFxe2,MSsc2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
! ---- Fu,bar[Fu],Fu,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
if((MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFuFuAhL(i3,i2)
coup3R = cplcFuFuAhR(i3,i2)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFu(i3)*TfFFFbS(MFu2(i1),MFu2(i2),MFu2(i3),MAh2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFuFuAhL(i3,i2)
coup3R = cplcFuFuAhR(i3,i2)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFu(i2)*TfFFbFS(MFu2(i1),MFu2(i2),MFu2(i3),MAh2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFuFuAhL(i3,i2)
coup3R = cplcFuFuAhR(i3,i2)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i3)*MFu(i2)*TfFbFbFbS(MFu2(i1),MFu2(i2),MFu2(i3),MAh2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
! ---- Fu,bar[Fu],Fd,Hp ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
if((MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFdHpL(i1,i3)
coup2R = cplcFuFdHpR(i1,i3)
coup3L = cplcFdFucHpL(i3,i2)
coup3R = cplcFdFucHpR(i3,i2)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFd(i3)*TfFFFbS(MFu2(i1),MFu2(i2),MFd2(i3),MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFdHpL(i1,i3)
coup2R = cplcFuFdHpR(i1,i3)
coup3L = cplcFdFucHpL(i3,i2)
coup3R = cplcFdFucHpR(i3,i2)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFu(i2)*TfFFbFS(MFu2(i1),MFu2(i2),MFd2(i3),MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFdHpL(i1,i3)
coup2R = cplcFuFdHpR(i1,i3)
coup3L = cplcFdFucHpL(i3,i2)
coup3R = cplcFdFucHpR(i3,i2)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFu(i1)*MFu(i2)*TfFbFbFbS(MFu2(i1),MFu2(i2),MFd2(i3),MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
! ---- Fu,bar[Fu],Fu,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,2
if((MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,i4)
coup2R = cplcFuFuhhR(i1,i3,i4)
coup3L = cplcFuFuhhL(i3,i2,i4)
coup3R = cplcFuFuhhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFu(i3)*TfFFFbS(MFu2(i1),MFu2(i2),MFu2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,i4)
coup2R = cplcFuFuhhR(i1,i3,i4)
coup3L = cplcFuFuhhL(i3,i2,i4)
coup3R = cplcFuFuhhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -6._dp*MFu(i2)*TfFFbFS(MFu2(i1),MFu2(i2),MFu2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,i4)
coup2R = cplcFuFuhhR(i1,i3,i4)
coup3L = cplcFuFuhhL(i3,i2,i4)
coup3R = cplcFuFuhhR(i3,i2,i4)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i3)*MFu(i2)*TfFbFbFbS(MFu2(i1),MFu2(i2),MFu2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Fxv,bar[Fxv],Fxv,Ah ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
if((MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFxvAhL(i3,i2)
coup3R = cplcFxvFxvAhR(i3,i2)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFxv(i3)*TfFFFbS(MFxv2(i1),MFxv2(i2),MFxv2(i3),MAh2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFxvAhL(i3,i2)
coup3R = cplcFxvFxvAhR(i3,i2)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFxv(i2)*TfFFbFS(MFxv2(i1),MFxv2(i2),MFxv2(i3),MAh2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFxvAhL(i3,i2)
coup3R = cplcFxvFxvAhR(i3,i2)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i1)*MFxv(i3)*MFxv(i2)*TfFbFbFbS(MFxv2(i1),MFxv2(i2),MFxv2(i3),MAh2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
! ---- Fxv,bar[Fxv],Fv,Ssc ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,2
if((MFv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFvSscL(i1,i3,i4)
coup2R = cplcFxvFvSscR(i1,i3,i4)
coup3L = cplcFvFxvcSscL(i3,i2,i4)
coup3R = cplcFvFxvcSscR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFv(i3)*TfFFFbS(MFxv2(i1),MFxv2(i2),MFv2(i3),MSsc2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFvSscL(i1,i3,i4)
coup2R = cplcFxvFvSscR(i1,i3,i4)
coup3L = cplcFvFxvcSscL(i3,i2,i4)
coup3R = cplcFvFxvcSscR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFxv(i2)*TfFFbFS(MFxv2(i1),MFxv2(i2),MFv2(i3),MSsc2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFv(i3) .gt. epsfmass) .and. (MFxv(i1) .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFvSscL(i1,i3,i4)
coup2R = cplcFxvFvSscR(i1,i3,i4)
coup3L = cplcFvFxvcSscL(i3,i2,i4)
coup3R = cplcFvFxvcSscR(i3,i2,i4)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFv(i3)*MFxv(i1)*MFxv(i2)*TfFbFbFbS(MFxv2(i1),MFxv2(i2),MFv2(i3),MSsc2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Fxv,bar[Fxv],Fxe,Hp ----
Do i1=1,2
Do i2=1,2
if((MFxe .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxeHpL(i1)
coup2R = cplcFxvFxeHpR(i1)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFxe*TfFFFbS(MFxv2(i1),MFxv2(i2),MFxe2,MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxeHpL(i1)
coup2R = cplcFxvFxeHpR(i1)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFxv(i2)*TfFFbFS(MFxv2(i1),MFxv2(i2),MFxe2,MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFxe .gt. epsfmass) .and. (MFxv(i1) .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxeHpL(i1)
coup2R = cplcFxvFxeHpR(i1)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MFxv(i1)*MFxv(i2)*TfFbFbFbS(MFxv2(i1),MFxv2(i2),MFxe2,MHp2,Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
! ---- Fxv,bar[Fxv],Fxv,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
if((MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,i4)
coup2R = cplcFxvFxvhhR(i1,i3,i4)
coup3L = cplcFxvFxvhhL(i3,i2,i4)
coup3R = cplcFxvFxvhhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3R+coup1R*coup2L*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFxv(i3)*TfFFFbS(MFxv2(i1),MFxv2(i2),MFxv2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,i4)
coup2R = cplcFxvFxvhhR(i1,i3,i4)
coup3L = cplcFxvFxvhhL(i3,i2,i4)
coup3R = cplcFxvFxvhhR(i3,i2,i4)
prefactor=Real(coup1L*coup2R*coup3L+coup1R*coup2L*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -2._dp*MFxv(i2)*TfFFbFS(MFxv2(i1),MFxv2(i2),MFxv2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,i4)
coup2R = cplcFxvFxvhhR(i1,i3,i4)
coup3L = cplcFxvFxvhhL(i3,i2,i4)
coup3R = cplcFxvFxvhhR(i3,i2,i4)
prefactor=Real(coup1L*coup2L*coup3L+coup1R*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i1)*MFxv(i3)*MFxv(i2)*TfFbFbFbS(MFxv2(i1),MFxv2(i2),MFxv2(i3),Mhh2(i4),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Topology ToFV
! ---- Fd ----
Do i1=1,3
if((MFd(i1) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFdFdhhL(i1,i1,gE1)
coup1R = cplcFdFdhhR(i1,i1,gE1)
coup2 = g3
coup3 = g3
prefactor=Real(coup1L*coup2*coup3+coup1R*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -4._dp*MFd(i1)*TfFV(MFd2(i1),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
! ---- Fu ----
Do i1=1,3
if((MFu(i1) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
coup1L = cplcFuFuhhL(i1,i1,gE1)
coup1R = cplcFuFuhhR(i1,i1,gE1)
coup2 = g3
coup3 = g3
prefactor=Real(coup1L*coup2*coup3+coup1R*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingvector(gE1)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingvector(gE1)= 0._dp
 end if
   End Do
if(nonzerocoupling) then 
 funcvalue= -4._dp*MFu(i1)*TfFV(MFu2(i1),Qscale)
 temptad=temptad+tempcouplingvector*funcvalue
end if
end if

end do
! ----------------------------
! ---- Final tadpole result --
temptad=(temptad*oo16Pi2*oo16Pi2)+delta2ltadpoles
tad2L=matmul(temptad,ZH)
! ----------------------------

! ------------------------------------
! ------- CP EVEN MASS DIAGRAMS ------
! ------------------------------------
tempcont(:,:)=0._dp
tempcouplingmatrix(:,:)=0._dp
! ---- Topology WoSSSS
! ---- Ah,Ah,Ah,hh ----
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhhhh(gE1,gE2)
coup2 = cplAhAhhh(i4)
coup3 = cplAhAhhh(i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSSS(p2,MAh2,MAh2,MAh2,Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- hh,hh,Ah,Ah ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2 = cplAhAhhh(i1)
coup3 = cplAhAhhh(i2)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/4._dp*WfSSSS(p2,Mhh2(i1),Mhh2(i2),MAh2,MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- hh,hh,hh,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2 = cplhhhhhh(i1,i3,i4)
coup3 = cplhhhhhh(i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/4._dp*WfSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Hp,conj[Hp] ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2 = cplhhHpcHp(i1)
coup3 = cplhhHpcHp(i2)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSSS(p2,Mhh2(i1),Mhh2(i2),MHp2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- hh,hh,Ssc,conj[Ssc] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2 = cplhhSsccSsc(i1,i3,i4)
coup3 = cplhhSsccSsc(i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSSS(p2,Mhh2(i1),Mhh2(i2),MSsc2(i3),MSsc2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hp,conj[Hp],hh,Hp ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhHpcHp(gE1,gE2)
coup2 = cplhhHpcHp(i3)
coup3 = cplhhHpcHp(i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSSS(p2,MHp2,MHp2,Mhh2(i3),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- Ssc,conj[Ssc],hh,Ssc ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhSsccSsc(gE1,gE2,i1,i2)
coup2 = cplhhSsccSsc(i3,i4,i1)
coup3 = cplhhSsccSsc(i3,i2,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSSS(p2,MSsc2(i1),MSsc2(i2),Mhh2(i3),MSsc2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Topology XoSSS
! ---- Ah,Ah,Ah ----
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhhhh(gE1,gE2)
coup2 = cplAhAhAhAh
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*XfSSS(p2,MAh2,MAh2,MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
! ---- Ah,Ah,hh ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhhhh(gE1,gE2)
coup2 = cplAhAhhhhh(i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*XfSSS(p2,MAh2,MAh2,Mhh2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- Ah,Ah,Hp ----
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhhhh(gE1,gE2)
coup2 = cplAhAhHpcHp
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MAh2,MAh2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
! ---- Ah,Ah,Ssc ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhhhh(gE1,gE2)
coup2 = cplAhAhSsccSsc(i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MAh2,MAh2,MSsc2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- hh,hh,Ah ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2 = cplAhAhhhhh(i1,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*XfSSS(p2,Mhh2(i1),Mhh2(i2),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- hh,hh,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2 = cplhhhhhhhh(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*XfSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- hh,hh,Hp ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2 = cplhhhhHpcHp(i1,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,Mhh2(i1),Mhh2(i2),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- hh,hh,Ssc ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2 = cplhhhhSsccSsc(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,Mhh2(i1),Mhh2(i2),MSsc2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Hp,conj[Hp],Ah ----
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhHpcHp(gE1,gE2)
coup2 = cplAhAhHpcHp
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MHp2,MHp2,MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
! ---- Hp,conj[Hp],hh ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhHpcHp(gE1,gE2)
coup2 = cplhhhhHpcHp(i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MHp2,MHp2,Mhh2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- Hp,conj[Hp],Hp ----
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhHpcHp(gE1,gE2)
coup2 = cplHpHpcHpcHp
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*XfSSS(p2,MHp2,MHp2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
! ---- Hp,conj[Hp],Ssc ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhHpcHp(gE1,gE2)
coup2 = cplHpSsccHpcSsc(i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*XfSSS(p2,MHp2,MHp2,MSsc2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- Ssc,conj[Ssc],Ah ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhSsccSsc(gE1,gE2,i1,i2)
coup2 = cplAhAhSsccSsc(i2,i1)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MSsc2(i1),MSsc2(i2),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- Ssc,conj[Ssc],hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhSsccSsc(gE1,gE2,i1,i2)
coup2 = cplhhhhSsccSsc(i3,i3,i2,i1)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MSsc2(i1),MSsc2(i2),Mhh2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Ssc,conj[Ssc],Hp ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhSsccSsc(gE1,gE2,i1,i2)
coup2 = cplHpSsccHpcSsc(i2,i1)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*XfSSS(p2,MSsc2(i1),MSsc2(i2),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- Ssc,conj[Ssc],Ssc ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhSsccSsc(gE1,gE2,i1,i2)
coup2 = cplSscSsccSsccSsc(i2,i3,i1,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*XfSSS(p2,MSsc2(i1),MSsc2(i2),MSsc2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Topology YoSSSS
! ---- Ah,Ah,Ah,Ah ----
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhh(gE2)
coup3 = cplAhAhAhAh
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*YfSSSS(p2,MAh2,MAh2,MAh2,MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
! ---- Ah,Ah,Ah,hh ----
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhh(gE2)
coup3 = cplAhAhhhhh(i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*YfSSSS(p2,MAh2,MAh2,MAh2,Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- Ah,Ah,Ah,Hp ----
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhh(gE2)
coup3 = cplAhAhHpcHp
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,MAh2,MAh2,MAh2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
! ---- Ah,Ah,Ah,Ssc ----
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhh(gE2)
coup3 = cplAhAhSsccSsc(i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,MAh2,MAh2,MAh2,MSsc2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- hh,hh,hh,Ah ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3 = cplAhAhhhhh(i2,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*YfSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- hh,hh,hh,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3 = cplhhhhhhhh(i2,i3,i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*YfSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,hh,Hp ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3 = cplhhhhHpcHp(i2,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- hh,hh,hh,Ssc ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3 = cplhhhhSsccSsc(i2,i3,i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MSsc2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hp,conj[Hp],Hp,Ah ----
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhHpcHp(gE2)
coup3 = cplAhAhHpcHp
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,MHp2,MHp2,MHp2,MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
! ---- Hp,conj[Hp],Hp,hh ----
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhHpcHp(gE2)
coup3 = cplhhhhHpcHp(i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,MHp2,MHp2,MHp2,Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- Hp,conj[Hp],Hp,Hp ----
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhHpcHp(gE2)
coup3 = cplHpHpcHpcHp
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*YfSSSS(p2,MHp2,MHp2,MHp2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
! ---- Hp,conj[Hp],Hp,Ssc ----
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhHpcHp(gE2)
coup3 = cplHpSsccHpcSsc(i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*YfSSSS(p2,MHp2,MHp2,MHp2,MSsc2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- Ssc,conj[Ssc],Ssc,Ah ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2 = cplhhSsccSsc(gE2,i3,i1)
coup3 = cplAhAhSsccSsc(i2,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,MSsc2(i1),MSsc2(i2),MSsc2(i3),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Ssc,conj[Ssc],Ssc,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2 = cplhhSsccSsc(gE2,i3,i1)
coup3 = cplhhhhSsccSsc(i4,i4,i2,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,MSsc2(i1),MSsc2(i2),MSsc2(i3),Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Ssc,conj[Ssc],Ssc,Hp ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2 = cplhhSsccSsc(gE2,i3,i1)
coup3 = cplHpSsccHpcSsc(i2,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*YfSSSS(p2,MSsc2(i1),MSsc2(i2),MSsc2(i3),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Ssc,conj[Ssc],Ssc,Ssc ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2 = cplhhSsccSsc(gE2,i3,i1)
coup3 = cplSscSsccSsccSsc(i2,i4,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*YfSSSS(p2,MSsc2(i1),MSsc2(i2),MSsc2(i3),MSsc2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Topology ZoSSSS
! ---- Ah,Ah,Ah,Ah ----
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhh(gE2)
coup3 = cplAhAhAhAh
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/4._dp*ZfSSSS(p2,MAh2,MAh2,MAh2,MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
! ---- Ah,Ah,hh,hh ----
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplhhhhhh(gE2,i3,i4)
coup3 = cplAhAhhhhh(i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*ZfSSSS(p2,MAh2,MAh2,Mhh2(i3),Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- Ah,Ah,Hp,conj[Hp] ----
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplhhHpcHp(gE2)
coup3 = cplAhAhHpcHp
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*ZfSSSS(p2,MAh2,MAh2,MHp2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
! ---- Ah,Ah,Ssc,conj[Ssc] ----
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplhhSsccSsc(gE2,i3,i4)
coup3 = cplAhAhSsccSsc(i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*ZfSSSS(p2,MAh2,MAh2,MSsc2(i3),MSsc2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- hh,hh,hh,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i3,i4)
coup3 = cplhhhhhhhh(i1,i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/4._dp*ZfSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Hp,conj[Hp] ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhHpcHp(gE2)
coup3 = cplhhhhHpcHp(i1,i2)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*ZfSSSS(p2,Mhh2(i1),Mhh2(i2),MHp2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- hh,hh,Ssc,conj[Ssc] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhSsccSsc(gE2,i3,i4)
coup3 = cplhhhhSsccSsc(i1,i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*ZfSSSS(p2,Mhh2(i1),Mhh2(i2),MSsc2(i3),MSsc2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hp,conj[Hp],Hp,conj[Hp] ----
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhHpcHp(gE2)
coup3 = cplHpHpcHpcHp
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*ZfSSSS(p2,MHp2,MHp2,MHp2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
! ---- Hp,conj[Hp],Ssc,conj[Ssc] ----
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhSsccSsc(gE2,i3,i4)
coup3 = cplHpSsccHpcSsc(i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*ZfSSSS(p2,MHp2,MHp2,MSsc2(i3),MSsc2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- Ssc,conj[Ssc],Ssc,conj[Ssc] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2 = cplhhSsccSsc(gE2,i3,i4)
coup3 = cplSscSsccSsccSsc(i2,i4,i1,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*ZfSSSS(p2,MSsc2(i1),MSsc2(i2),MSsc2(i3),MSsc2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Topology SoSSS
! ---- Ah,Ah,hh ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhhhh(gE1,i3)
coup2 = cplAhAhhhhh(gE2,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*SfSSS(p2,MAh2,MAh2,Mhh2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- hh,hh,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhhhh(gE1,i1,i2,i3)
coup2 = cplhhhhhhhh(gE2,i1,i2,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/6._dp*SfSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- hh,Hp,conj[Hp] ----
Do i1=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhHpcHp(gE1,i1)
coup2 = cplhhhhHpcHp(gE2,i1)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*SfSSS(p2,Mhh2(i1),MHp2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- hh,Ssc,conj[Ssc] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhSsccSsc(gE1,i1,i2,i3)
coup2 = cplhhhhSsccSsc(gE2,i1,i3,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*SfSSS(p2,Mhh2(i1),MSsc2(i2),MSsc2(i3),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Topology UoSSSS
! ---- Ah,Ah,Ah,hh ----
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhhhh(gE2,i4)
coup3 = cplAhAhhh(i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*UfSSSS(p2,MAh2,MAh2,MAh2,Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- hh,hh,Ah,Ah ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplAhAhhhhh(gE2,i1)
coup3 = cplAhAhhh(i2)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*UfSSSS(p2,Mhh2(i1),Mhh2(i2),MAh2,MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- hh,hh,hh,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhhhh(gE2,i1,i3,i4)
coup3 = cplhhhhhh(i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*UfSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Hp,conj[Hp] ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhHpcHp(gE2,i1)
coup3 = cplhhHpcHp(i2)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*UfSSSS(p2,Mhh2(i1),Mhh2(i2),MHp2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- hh,hh,Ssc,conj[Ssc] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhSsccSsc(gE2,i1,i3,i4)
coup3 = cplhhSsccSsc(i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*UfSSSS(p2,Mhh2(i1),Mhh2(i2),MSsc2(i3),MSsc2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hp,conj[Hp],hh,Hp ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhhhHpcHp(gE2,i3)
coup3 = cplhhHpcHp(i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -4._dp*UfSSSS(p2,MHp2,MHp2,Mhh2(i3),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- Ssc,conj[Ssc],hh,Ssc ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2 = cplhhhhSsccSsc(gE2,i3,i4,i1)
coup3 = cplhhSsccSsc(i3,i2,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -4._dp*UfSSSS(p2,MSsc2(i1),MSsc2(i2),Mhh2(i3),MSsc2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Topology VoSSSSS
! ---- Ah,Ah,Ah,Ah,hh ----
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhh(gE2)
coup3 = cplAhAhhh(i5)
coup4 = cplAhAhhh(i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSSS(p2,MAh2,MAh2,MAh2,MAh2,Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- hh,hh,hh,Ah,Ah ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3 = cplAhAhhh(i2)
coup4 = cplAhAhhh(i3)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*VfSSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MAh2,MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- hh,hh,hh,hh,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3 = cplhhhhhh(i2,i4,i5)
coup4 = cplhhhhhh(i3,i4,i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*VfSSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,hh,hh,Hp,conj[Hp] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3 = cplhhHpcHp(i2)
coup4 = cplhhHpcHp(i3)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MHp2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- hh,hh,hh,Ssc,conj[Ssc] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3 = cplhhSsccSsc(i2,i4,i5)
coup4 = cplhhSsccSsc(i3,i5,i4)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MSsc2(i4),MSsc2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Hp,conj[Hp],Hp,hh,conj[Hp] ----
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhHpcHp(gE2)
coup3 = cplhhHpcHp(i4)
coup4 = cplhhHpcHp(i4)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfSSSSS(p2,MHp2,MHp2,MHp2,Mhh2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- Ssc,conj[Ssc],Ssc,hh,conj[Ssc] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2 = cplhhSsccSsc(gE2,i3,i1)
coup3 = cplhhSsccSsc(i4,i2,i5)
coup4 = cplhhSsccSsc(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfSSSSS(p2,MSsc2(i1),MSsc2(i2),MSsc2(i3),Mhh2(i4),MSsc2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Topology MoSSSSS
! ---- Ah,Ah,Ah,Ah,hh ----
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhh(gE2)
coup3 = cplAhAhhh(i5)
coup4 = cplAhAhhh(i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*MfSSSSS(p2,MAh2,MAh2,MAh2,MAh2,Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- Ah,hh,Ah,hh,Ah ----
Do i2=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplhhhhhh(gE2,i2,i4)
coup3 = cplAhAhhh(i2)
coup4 = cplAhAhhh(i4)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfSSSSS(p2,MAh2,Mhh2(i2),MAh2,Mhh2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- hh,hh,hh,hh,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i3)
coup2 = cplhhhhhh(gE2,i2,i4)
coup3 = cplhhhhhh(i1,i2,i5)
coup4 = cplhhhhhh(i3,i4,i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*MfSSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,Hp,hh,conj[Hp],Hp ----
Do i1=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i3)
coup2 = cplhhHpcHp(gE2)
coup3 = cplhhHpcHp(i1)
coup4 = cplhhHpcHp(i3)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MfSSSSS(p2,Mhh2(i1),MHp2,Mhh2(i3),MHp2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- hh,Ssc,hh,conj[Ssc],Ssc ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i3)
coup2 = cplhhSsccSsc(gE2,i2,i4)
coup3 = cplhhSsccSsc(i1,i5,i2)
coup4 = cplhhSsccSsc(i3,i4,i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MfSSSSS(p2,Mhh2(i1),MSsc2(i2),Mhh2(i3),MSsc2(i4),MSsc2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Hp,conj[Hp],conj[Hp],Hp,hh ----
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhHpcHp(gE2)
coup3 = cplhhHpcHp(i5)
coup4 = cplhhHpcHp(i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfSSSSS(p2,MHp2,MHp2,MHp2,MHp2,Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- Ssc,conj[Ssc],conj[Ssc],Ssc,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i3)
coup2 = cplhhSsccSsc(gE2,i4,i2)
coup3 = cplhhSsccSsc(i5,i2,i1)
coup4 = cplhhSsccSsc(i5,i3,i4)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfSSSSS(p2,MSsc2(i1),MSsc2(i2),MSsc2(i3),MSsc2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Topology WoSSFF
! ---- Ah,Ah,Fd,bar[Fd] ----
Do i3=1,3
Do i4=1,3
if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhhhh(gE1,gE2)
coup2L = cplcFdFdAhL(i4,i3)
coup2R = cplcFdFdAhR(i4,i3)
coup3L = cplcFdFdAhL(i3,i4)
coup3R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*MFd(i3)*MFd(i4)*WfSSFbFb(p2,MAh2,MAh2,MFd2(i3),MFd2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhhhh(gE1,gE2)
coup2L = cplcFdFdAhL(i4,i3)
coup2R = cplcFdFdAhR(i4,i3)
coup3L = cplcFdFdAhL(i3,i4)
coup3R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*WfSSFF(p2,MAh2,MAh2,MFd2(i3),MFd2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- Ah,Ah,Fe,bar[Fe] ----
Do i3=1,3
Do i4=1,3
if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhhhh(gE1,gE2)
coup2L = cplcFeFeAhL(i4,i3)
coup2R = cplcFeFeAhR(i4,i3)
coup3L = cplcFeFeAhL(i3,i4)
coup3R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*MFe(i3)*MFe(i4)*WfSSFbFb(p2,MAh2,MAh2,MFe2(i3),MFe2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhhhh(gE1,gE2)
coup2L = cplcFeFeAhL(i4,i3)
coup2R = cplcFeFeAhR(i4,i3)
coup3L = cplcFeFeAhL(i3,i4)
coup3R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSFF(p2,MAh2,MAh2,MFe2(i3),MFe2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- Ah,Ah,Fu,bar[Fu] ----
Do i3=1,3
Do i4=1,3
if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhhhh(gE1,gE2)
coup2L = cplcFuFuAhL(i4,i3)
coup2R = cplcFuFuAhR(i4,i3)
coup3L = cplcFuFuAhL(i3,i4)
coup3R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*MFu(i3)*MFu(i4)*WfSSFbFb(p2,MAh2,MAh2,MFu2(i3),MFu2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhhhh(gE1,gE2)
coup2L = cplcFuFuAhL(i4,i3)
coup2R = cplcFuFuAhR(i4,i3)
coup3L = cplcFuFuAhL(i3,i4)
coup3R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*WfSSFF(p2,MAh2,MAh2,MFu2(i3),MFu2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- Ah,Ah,Fxv,bar[Fxv] ----
Do i3=1,2
Do i4=1,2
if((MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhhhh(gE1,gE2)
coup2L = cplcFxvFxvAhL(i4,i3)
coup2R = cplcFxvFxvAhR(i4,i3)
coup3L = cplcFxvFxvAhL(i3,i4)
coup3R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*MFxv(i3)*MFxv(i4)*WfSSFbFb(p2,MAh2,MAh2,MFxv2(i3),MFxv2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhhhh(gE1,gE2)
coup2L = cplcFxvFxvAhL(i4,i3)
coup2R = cplcFxvFxvAhR(i4,i3)
coup3L = cplcFxvFxvAhL(i3,i4)
coup3R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSFF(p2,MAh2,MAh2,MFxv2(i3),MFxv2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- hh,hh,Fd,bar[Fd] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,3
if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFdFdhhL(i4,i3,i1)
coup2R = cplcFdFdhhR(i4,i3,i1)
coup3L = cplcFdFdhhL(i3,i4,i2)
coup3R = cplcFdFdhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*MFd(i3)*MFd(i4)*WfSSFbFb(p2,Mhh2(i1),Mhh2(i2),MFd2(i3),MFd2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFdFdhhL(i4,i3,i1)
coup2R = cplcFdFdhhR(i4,i3,i1)
coup3L = cplcFdFdhhL(i3,i4,i2)
coup3R = cplcFdFdhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*WfSSFF(p2,Mhh2(i1),Mhh2(i2),MFd2(i3),MFd2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Fe,bar[Fe] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,3
if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFeFehhL(i4,i3,i1)
coup2R = cplcFeFehhR(i4,i3,i1)
coup3L = cplcFeFehhL(i3,i4,i2)
coup3R = cplcFeFehhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*MFe(i3)*MFe(i4)*WfSSFbFb(p2,Mhh2(i1),Mhh2(i2),MFe2(i3),MFe2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFeFehhL(i4,i3,i1)
coup2R = cplcFeFehhR(i4,i3,i1)
coup3L = cplcFeFehhL(i3,i4,i2)
coup3R = cplcFeFehhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSFF(p2,Mhh2(i1),Mhh2(i2),MFe2(i3),MFe2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Fu,bar[Fu] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,3
if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFuFuhhL(i4,i3,i1)
coup2R = cplcFuFuhhR(i4,i3,i1)
coup3L = cplcFuFuhhL(i3,i4,i2)
coup3R = cplcFuFuhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*MFu(i3)*MFu(i4)*WfSSFbFb(p2,Mhh2(i1),Mhh2(i2),MFu2(i3),MFu2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFuFuhhL(i4,i3,i1)
coup2R = cplcFuFuhhR(i4,i3,i1)
coup3L = cplcFuFuhhL(i3,i4,i2)
coup3R = cplcFuFuhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*WfSSFF(p2,Mhh2(i1),Mhh2(i2),MFu2(i3),MFu2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Fxv,bar[Fxv] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
if((MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFxvFxvhhL(i4,i3,i1)
coup2R = cplcFxvFxvhhR(i4,i3,i1)
coup3L = cplcFxvFxvhhL(i3,i4,i2)
coup3R = cplcFxvFxvhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*MFxv(i3)*MFxv(i4)*WfSSFbFb(p2,Mhh2(i1),Mhh2(i2),MFxv2(i3),MFxv2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhhhh(gE1,gE2,i1,i2)
coup2L = cplcFxvFxvhhL(i4,i3,i1)
coup2R = cplcFxvFxvhhR(i4,i3,i1)
coup3L = cplcFxvFxvhhL(i3,i4,i2)
coup3R = cplcFxvFxvhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSFF(p2,Mhh2(i1),Mhh2(i2),MFxv2(i3),MFxv2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Hp,conj[Hp],Fu,bar[Fd] ----
Do i3=1,3
Do i4=1,3
if((MFd(i4) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhHpcHp(gE1,gE2)
coup2L = cplcFdFucHpL(i4,i3)
coup2R = cplcFdFucHpR(i4,i3)
coup3L = cplcFuFdHpL(i3,i4)
coup3R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp*MFu(i3)*MFd(i4)*WfSSFbFb(p2,MHp2,MHp2,MFu2(i3),MFd2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhHpcHp(gE1,gE2)
coup2L = cplcFdFucHpL(i4,i3)
coup2R = cplcFdFucHpR(i4,i3)
coup3L = cplcFuFdHpL(i3,i4)
coup3R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp*WfSSFF(p2,MHp2,MHp2,MFu2(i3),MFd2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- Hp,conj[Hp],Fv,bar[Fe] ----
Do i3=1,3
Do i4=1,3
if((MFe(i4) .gt. epsfmass) .and. (MFv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhHpcHp(gE1,gE2)
coup2L = cplcFeFvcHpL(i4,i3)
coup2R = cplcFeFvcHpR(i4,i3)
coup3L = cplcFvFeHpL(i3,i4)
coup3R = cplcFvFeHpR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*MFv(i3)*MFe(i4)*WfSSFbFb(p2,MHp2,MHp2,MFv2(i3),MFe2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhHpcHp(gE1,gE2)
coup2L = cplcFeFvcHpL(i4,i3)
coup2R = cplcFeFvcHpR(i4,i3)
coup3L = cplcFvFeHpL(i3,i4)
coup3R = cplcFvFeHpR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSFF(p2,MHp2,MHp2,MFv2(i3),MFe2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- Hp,conj[Hp],Fxv,bar[Fxe] ----
Do i3=1,2
if((MFxe .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhHpcHp(gE1,gE2)
coup2L = cplcFxeFxvcHpL(i3)
coup2R = cplcFxeFxvcHpR(i3)
coup3L = cplcFxvFxeHpL(i3)
coup3R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*MFxe*MFxv(i3)*WfSSFbFb(p2,MHp2,MHp2,MFxv2(i3),MFxe2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhHpcHp(gE1,gE2)
coup2L = cplcFxeFxvcHpL(i3)
coup2R = cplcFxeFxvcHpR(i3)
coup3L = cplcFxvFxeHpL(i3)
coup3R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSFF(p2,MHp2,MHp2,MFxv2(i3),MFxe2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- Ssc,conj[Ssc],Fxe,bar[Fe] ----
Do i1=1,2
Do i2=1,2
Do i4=1,3
if((MFe(i4) .gt. epsfmass) .and. (MFxe .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhSsccSsc(gE1,gE2,i1,i2)
coup2L = cplcFeFxecSscL(i4,i1)
coup2R = cplcFeFxecSscR(i4,i1)
coup3L = cplcFxeFeSscL(i4,i2)
coup3R = cplcFxeFeSscR(i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*MFxe*MFe(i4)*WfSSFbFb(p2,MSsc2(i1),MSsc2(i2),MFxe2,MFe2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhSsccSsc(gE1,gE2,i1,i2)
coup2L = cplcFeFxecSscL(i4,i1)
coup2R = cplcFeFxecSscR(i4,i1)
coup3L = cplcFxeFeSscL(i4,i2)
coup3R = cplcFxeFeSscR(i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSFF(p2,MSsc2(i1),MSsc2(i2),MFxe2,MFe2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Ssc,conj[Ssc],Fxv,bar[Fv] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
if((MFv(i4) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhSsccSsc(gE1,gE2,i1,i2)
coup2L = cplcFvFxvcSscL(i4,i3,i1)
coup2R = cplcFvFxvcSscR(i4,i3,i1)
coup3L = cplcFxvFvSscL(i3,i4,i2)
coup3R = cplcFxvFvSscR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*MFxv(i3)*MFv(i4)*WfSSFbFb(p2,MSsc2(i1),MSsc2(i2),MFxv2(i3),MFv2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhSsccSsc(gE1,gE2,i1,i2)
coup2L = cplcFvFxvcSscL(i4,i3,i1)
coup2R = cplcFvFxvcSscR(i4,i3,i1)
coup3L = cplcFxvFvSscL(i3,i4,i2)
coup3R = cplcFxvFvSscR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSFF(p2,MSsc2(i1),MSsc2(i2),MFxv2(i3),MFv2(i4),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Topology MoFFFFS
! ---- Fd,bar[Fd],bar[Fd],Fd,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdAhL(i1,i2)
coup3R = cplcFdFdAhR(i1,i2)
coup4L = cplcFdFdAhL(i4,i3)
coup4R = cplcFdFdAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i1)*MFd(i4)*MFd(i2)*MFd(i3)*MfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdAhL(i1,i2)
coup3R = cplcFdFdAhR(i1,i2)
coup4L = cplcFdFdAhL(i4,i3)
coup4R = cplcFdFdAhR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFd(i3)*MfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdAhL(i1,i2)
coup3R = cplcFdFdAhR(i1,i2)
coup4L = cplcFdFdAhL(i4,i3)
coup4R = cplcFdFdAhR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i2)*MfFFbFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdAhL(i1,i2)
coup3R = cplcFdFdAhR(i1,i2)
coup4L = cplcFdFdAhL(i4,i3)
coup4R = cplcFdFdAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i3)*MfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdAhL(i1,i2)
coup3R = cplcFdFdAhR(i1,i2)
coup4L = cplcFdFdAhL(i4,i3)
coup4R = cplcFdFdAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Fd,bar[Fd],bar[Fd],Fd,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i1)*MFd(i4)*MFd(i2)*MFd(i3)*MfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFd(i3)*MfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i2)*MfFFbFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i3)*MfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fd,bar[Fu],bar[Fd],Fu,conj[Hp] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i1) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFdFucHpL(i1,i2)
coup3R = cplcFdFucHpR(i1,i2)
coup4L = cplcFuFdHpL(i4,i3)
coup4R = cplcFuFdHpR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i1)*MFu(i4)*MFd(i3)*MFu(i2)*MfFbFbFbFbS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFdFucHpL(i1,i2)
coup3R = cplcFdFucHpR(i1,i2)
coup4L = cplcFuFdHpL(i4,i3)
coup4R = cplcFuFdHpR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFu(i2)*MfFFbFbFS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFdFucHpL(i1,i2)
coup3R = cplcFdFucHpR(i1,i2)
coup4L = cplcFuFdHpL(i4,i3)
coup4R = cplcFuFdHpR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i2)*MfFFbFFbS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFdFucHpL(i1,i2)
coup3R = cplcFdFucHpR(i1,i2)
coup4L = cplcFuFdHpL(i4,i3)
coup4R = cplcFuFdHpR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFd(i3)*MfFFFbFbS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i3,i1,gE1)
coup1R = cplcFdFdhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFdFucHpL(i1,i2)
coup3R = cplcFdFucHpR(i1,i2)
coup4L = cplcFuFdHpL(i4,i3)
coup4R = cplcFuFdHpR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Fe,bar[Fe],bar[Fe],Fe,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFeAhL(i1,i2)
coup3R = cplcFeFeAhR(i1,i2)
coup4L = cplcFeFeAhL(i4,i3)
coup4R = cplcFeFeAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFe(i1)*MFe(i4)*MFe(i2)*MFe(i3)*MfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFeAhL(i1,i2)
coup3R = cplcFeFeAhR(i1,i2)
coup4L = cplcFeFeAhL(i4,i3)
coup4R = cplcFeFeAhR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i2)*MFe(i3)*MfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFeAhL(i1,i2)
coup3R = cplcFeFeAhR(i1,i2)
coup4L = cplcFeFeAhL(i4,i3)
coup4R = cplcFeFeAhR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i4)*MFe(i2)*MfFFbFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFeAhL(i1,i2)
coup3R = cplcFeFeAhR(i1,i2)
coup4L = cplcFeFeAhL(i4,i3)
coup4R = cplcFeFeAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i4)*MFe(i3)*MfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFeAhL(i1,i2)
coup3R = cplcFeFeAhR(i1,i2)
coup4L = cplcFeFeAhL(i4,i3)
coup4R = cplcFeFeAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Fe,bar[Fe],bar[Fe],Fe,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFe(i1)*MFe(i4)*MFe(i2)*MFe(i3)*MfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i2)*MFe(i3)*MfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i4)*MFe(i2)*MfFFbFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i4)*MFe(i3)*MfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i3,i1,gE1)
coup1R = cplcFeFehhR(i3,i1,gE1)
coup2L = cplcFeFehhL(i2,i4,gE2)
coup2R = cplcFeFehhR(i2,i4,gE2)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fu,bar[Fd],bar[Fu],Fd,Hp ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass) .and. (MFu(i1) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFuFdHpL(i1,i2)
coup3R = cplcFuFdHpR(i1,i2)
coup4L = cplcFdFucHpL(i4,i3)
coup4R = cplcFdFucHpR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i4)*MFu(i1)*MFd(i2)*MFu(i3)*MfFbFbFbFbS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFuFdHpL(i1,i2)
coup3R = cplcFuFdHpR(i1,i2)
coup4L = cplcFdFucHpL(i4,i3)
coup4R = cplcFdFucHpR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFu(i3)*MfFFbFbFS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFuFdHpL(i1,i2)
coup3R = cplcFuFdHpR(i1,i2)
coup4L = cplcFdFucHpL(i4,i3)
coup4R = cplcFdFucHpR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i2)*MfFFbFFbS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFuFdHpL(i1,i2)
coup3R = cplcFuFdHpR(i1,i2)
coup4L = cplcFdFucHpL(i4,i3)
coup4R = cplcFdFucHpR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFu(i3)*MfFFFbFbS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFdFdhhL(i2,i4,gE2)
coup2R = cplcFdFdhhR(i2,i4,gE2)
coup3L = cplcFuFdHpL(i1,i2)
coup3R = cplcFuFdHpR(i1,i2)
coup4L = cplcFdFucHpL(i4,i3)
coup4R = cplcFdFucHpR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Fu,bar[Fu],bar[Fu],Fu,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuAhL(i1,i2)
coup3R = cplcFuFuAhR(i1,i2)
coup4L = cplcFuFuAhL(i4,i3)
coup4R = cplcFuFuAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i1)*MFu(i4)*MFu(i2)*MFu(i3)*MfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuAhL(i1,i2)
coup3R = cplcFuFuAhR(i1,i2)
coup4L = cplcFuFuAhL(i4,i3)
coup4R = cplcFuFuAhR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i2)*MFu(i3)*MfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuAhL(i1,i2)
coup3R = cplcFuFuAhR(i1,i2)
coup4L = cplcFuFuAhL(i4,i3)
coup4R = cplcFuFuAhR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i2)*MfFFbFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuAhL(i1,i2)
coup3R = cplcFuFuAhR(i1,i2)
coup4L = cplcFuFuAhL(i4,i3)
coup4R = cplcFuFuAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i3)*MfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuAhL(i1,i2)
coup3R = cplcFuFuAhR(i1,i2)
coup4L = cplcFuFuAhL(i4,i3)
coup4R = cplcFuFuAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Fu,bar[Fu],bar[Fu],Fu,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i1)*MFu(i4)*MFu(i2)*MFu(i3)*MfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i2)*MFu(i3)*MfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i2)*MfFFbFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i3)*MfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i3,i1,gE1)
coup1R = cplcFuFuhhR(i3,i1,gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fxv,bar[Fxv],bar[Fxv],Fxv,Ah ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i3,i1,gE1)
coup1R = cplcFxvFxvhhR(i3,i1,gE1)
coup2L = cplcFxvFxvhhL(i2,i4,gE2)
coup2R = cplcFxvFxvhhR(i2,i4,gE2)
coup3L = cplcFxvFxvAhL(i1,i2)
coup3R = cplcFxvFxvAhR(i1,i2)
coup4L = cplcFxvFxvAhL(i4,i3)
coup4R = cplcFxvFxvAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFxv(i1)*MFxv(i4)*MFxv(i2)*MFxv(i3)*MfFbFbFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i3,i1,gE1)
coup1R = cplcFxvFxvhhR(i3,i1,gE1)
coup2L = cplcFxvFxvhhL(i2,i4,gE2)
coup2R = cplcFxvFxvhhR(i2,i4,gE2)
coup3L = cplcFxvFxvAhL(i1,i2)
coup3R = cplcFxvFxvAhR(i1,i2)
coup4L = cplcFxvFxvAhL(i4,i3)
coup4R = cplcFxvFxvAhR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i2)*MFxv(i3)*MfFFbFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i3,i1,gE1)
coup1R = cplcFxvFxvhhR(i3,i1,gE1)
coup2L = cplcFxvFxvhhL(i2,i4,gE2)
coup2R = cplcFxvFxvhhR(i2,i4,gE2)
coup3L = cplcFxvFxvAhL(i1,i2)
coup3R = cplcFxvFxvAhR(i1,i2)
coup4L = cplcFxvFxvAhL(i4,i3)
coup4R = cplcFxvFxvAhR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i4)*MFxv(i2)*MfFFbFFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i3,i1,gE1)
coup1R = cplcFxvFxvhhR(i3,i1,gE1)
coup2L = cplcFxvFxvhhL(i2,i4,gE2)
coup2R = cplcFxvFxvhhR(i2,i4,gE2)
coup3L = cplcFxvFxvAhL(i1,i2)
coup3R = cplcFxvFxvAhR(i1,i2)
coup4L = cplcFxvFxvAhL(i4,i3)
coup4R = cplcFxvFxvAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i4)*MFxv(i3)*MfFFFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i3,i1,gE1)
coup1R = cplcFxvFxvhhR(i3,i1,gE1)
coup2L = cplcFxvFxvhhL(i2,i4,gE2)
coup2R = cplcFxvFxvhhR(i2,i4,gE2)
coup3L = cplcFxvFxvAhL(i1,i2)
coup3R = cplcFxvFxvAhR(i1,i2)
coup4L = cplcFxvFxvAhL(i4,i3)
coup4R = cplcFxvFxvAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfFFFFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Fxv,bar[Fxv],bar[Fxv],Fxv,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,2
if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i3,i1,gE1)
coup1R = cplcFxvFxvhhR(i3,i1,gE1)
coup2L = cplcFxvFxvhhL(i2,i4,gE2)
coup2R = cplcFxvFxvhhR(i2,i4,gE2)
coup3L = cplcFxvFxvhhL(i1,i2,i5)
coup3R = cplcFxvFxvhhR(i1,i2,i5)
coup4L = cplcFxvFxvhhL(i4,i3,i5)
coup4R = cplcFxvFxvhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFxv(i1)*MFxv(i4)*MFxv(i2)*MFxv(i3)*MfFbFbFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i3,i1,gE1)
coup1R = cplcFxvFxvhhR(i3,i1,gE1)
coup2L = cplcFxvFxvhhL(i2,i4,gE2)
coup2R = cplcFxvFxvhhR(i2,i4,gE2)
coup3L = cplcFxvFxvhhL(i1,i2,i5)
coup3R = cplcFxvFxvhhR(i1,i2,i5)
coup4L = cplcFxvFxvhhL(i4,i3,i5)
coup4R = cplcFxvFxvhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i2)*MFxv(i3)*MfFFbFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i3,i1,gE1)
coup1R = cplcFxvFxvhhR(i3,i1,gE1)
coup2L = cplcFxvFxvhhL(i2,i4,gE2)
coup2R = cplcFxvFxvhhR(i2,i4,gE2)
coup3L = cplcFxvFxvhhL(i1,i2,i5)
coup3R = cplcFxvFxvhhR(i1,i2,i5)
coup4L = cplcFxvFxvhhL(i4,i3,i5)
coup4R = cplcFxvFxvhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i4)*MFxv(i2)*MfFFbFFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i3,i1,gE1)
coup1R = cplcFxvFxvhhR(i3,i1,gE1)
coup2L = cplcFxvFxvhhL(i2,i4,gE2)
coup2R = cplcFxvFxvhhR(i2,i4,gE2)
coup3L = cplcFxvFxvhhL(i1,i2,i5)
coup3R = cplcFxvFxvhhR(i1,i2,i5)
coup4L = cplcFxvFxvhhL(i4,i3,i5)
coup4R = cplcFxvFxvhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i4)*MFxv(i3)*MfFFFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i3,i1,gE1)
coup1R = cplcFxvFxvhhR(i3,i1,gE1)
coup2L = cplcFxvFxvhhL(i2,i4,gE2)
coup2R = cplcFxvFxvhhR(i2,i4,gE2)
coup3L = cplcFxvFxvhhL(i1,i2,i5)
coup3R = cplcFxvFxvhhR(i1,i2,i5)
coup4L = cplcFxvFxvhhL(i4,i3,i5)
coup4R = cplcFxvFxvhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfFFFFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Topology MoSFSFF
! ---- Ah,Fd,Ah,bar[Fd],Fd ----
Do i2=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass) .and. (MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFdFdhhL(i4,i2,gE2)
coup2R = cplcFdFdhhR(i4,i2,gE2)
coup3L = cplcFdFdAhL(i2,i5)
coup3R = cplcFdFdAhR(i2,i5)
coup4L = cplcFdFdAhL(i5,i4)
coup4R = cplcFdFdAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFd(i5)*MFd(i4)*MfSFbSFbFb(p2,MAh2,MFd2(i2),MAh2,MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFdFdhhL(i4,i2,gE2)
coup2R = cplcFdFdhhR(i4,i2,gE2)
coup3L = cplcFdFdAhL(i2,i5)
coup3R = cplcFdFdAhR(i2,i5)
coup4L = cplcFdFdAhL(i5,i4)
coup4R = cplcFdFdAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i4)*MfSFSFbF(p2,MAh2,MFd2(i2),MAh2,MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFdFdhhL(i4,i2,gE2)
coup2R = cplcFdFdhhR(i4,i2,gE2)
coup3L = cplcFdFdAhL(i2,i5)
coup3R = cplcFdFdAhR(i2,i5)
coup4L = cplcFdFdAhL(i5,i4)
coup4R = cplcFdFdAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i5)*MfSFSFFb(p2,MAh2,MFd2(i2),MAh2,MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
! ---- Ah,Fe,Ah,bar[Fe],Fe ----
Do i2=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass) .and. (MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFeAhL(i2,i5)
coup3R = cplcFeFeAhR(i2,i5)
coup4L = cplcFeFeAhL(i5,i4)
coup4R = cplcFeFeAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i2)*MFe(i5)*MFe(i4)*MfSFbSFbFb(p2,MAh2,MFe2(i2),MAh2,MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFeAhL(i2,i5)
coup3R = cplcFeFeAhR(i2,i5)
coup4L = cplcFeFeAhL(i5,i4)
coup4R = cplcFeFeAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i4)*MfSFSFbF(p2,MAh2,MFe2(i2),MAh2,MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFeAhL(i2,i5)
coup3R = cplcFeFeAhR(i2,i5)
coup4L = cplcFeFeAhL(i5,i4)
coup4R = cplcFeFeAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i5)*MfSFSFFb(p2,MAh2,MFe2(i2),MAh2,MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
! ---- Ah,Fu,Ah,bar[Fu],Fu ----
Do i2=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFuFuhhL(i4,i2,gE2)
coup2R = cplcFuFuhhR(i4,i2,gE2)
coup3L = cplcFuFuAhL(i2,i5)
coup3R = cplcFuFuAhR(i2,i5)
coup4L = cplcFuFuAhL(i5,i4)
coup4R = cplcFuFuAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i2)*MFu(i5)*MFu(i4)*MfSFbSFbFb(p2,MAh2,MFu2(i2),MAh2,MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFuFuhhL(i4,i2,gE2)
coup2R = cplcFuFuhhR(i4,i2,gE2)
coup3L = cplcFuFuAhL(i2,i5)
coup3R = cplcFuFuAhR(i2,i5)
coup4L = cplcFuFuAhL(i5,i4)
coup4R = cplcFuFuAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i4)*MfSFSFbF(p2,MAh2,MFu2(i2),MAh2,MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFuFuhhL(i4,i2,gE2)
coup2R = cplcFuFuhhR(i4,i2,gE2)
coup3L = cplcFuFuAhL(i2,i5)
coup3R = cplcFuFuAhR(i2,i5)
coup4L = cplcFuFuAhL(i5,i4)
coup4R = cplcFuFuAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i5)*MfSFSFFb(p2,MAh2,MFu2(i2),MAh2,MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
! ---- Ah,Fxv,Ah,bar[Fxv],Fxv ----
Do i2=1,2
Do i4=1,2
Do i5=1,2
if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass) .and. (MFxv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFxvFxvhhL(i4,i2,gE2)
coup2R = cplcFxvFxvhhR(i4,i2,gE2)
coup3L = cplcFxvFxvAhL(i2,i5)
coup3R = cplcFxvFxvAhR(i2,i5)
coup4L = cplcFxvFxvAhL(i5,i4)
coup4R = cplcFxvFxvAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i2)*MFxv(i5)*MFxv(i4)*MfSFbSFbFb(p2,MAh2,MFxv2(i2),MAh2,MFxv2(i4),MFxv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFxvFxvhhL(i4,i2,gE2)
coup2R = cplcFxvFxvhhR(i4,i2,gE2)
coup3L = cplcFxvFxvAhL(i2,i5)
coup3R = cplcFxvFxvAhR(i2,i5)
coup4L = cplcFxvFxvAhL(i5,i4)
coup4R = cplcFxvFxvAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i4)*MfSFSFbF(p2,MAh2,MFxv2(i2),MAh2,MFxv2(i4),MFxv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2L = cplcFxvFxvhhL(i4,i2,gE2)
coup2R = cplcFxvFxvhhR(i4,i2,gE2)
coup3L = cplcFxvFxvAhL(i2,i5)
coup3R = cplcFxvFxvAhR(i2,i5)
coup4L = cplcFxvFxvAhL(i5,i4)
coup4R = cplcFxvFxvAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i5)*MfSFSFFb(p2,MAh2,MFxv2(i2),MAh2,MFxv2(i4),MFxv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
! ---- hh,Fd,hh,bar[Fd],Fd ----
Do i1=1,2
Do i2=1,3
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass) .and. (MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFdFdhhL(i4,i2,gE2)
coup2R = cplcFdFdhhR(i4,i2,gE2)
coup3L = cplcFdFdhhL(i2,i5,i1)
coup3R = cplcFdFdhhR(i2,i5,i1)
coup4L = cplcFdFdhhL(i5,i4,i3)
coup4R = cplcFdFdhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFd(i5)*MFd(i4)*MfSFbSFbFb(p2,Mhh2(i1),MFd2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFdFdhhL(i4,i2,gE2)
coup2R = cplcFdFdhhR(i4,i2,gE2)
coup3L = cplcFdFdhhL(i2,i5,i1)
coup3R = cplcFdFdhhR(i2,i5,i1)
coup4L = cplcFdFdhhL(i5,i4,i3)
coup4R = cplcFdFdhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i4)*MfSFSFbF(p2,Mhh2(i1),MFd2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFdFdhhL(i4,i2,gE2)
coup2R = cplcFdFdhhR(i4,i2,gE2)
coup3L = cplcFdFdhhL(i2,i5,i1)
coup3R = cplcFdFdhhR(i2,i5,i1)
coup4L = cplcFdFdhhL(i5,i4,i3)
coup4R = cplcFdFdhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i5)*MfSFSFFb(p2,Mhh2(i1),MFd2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- hh,Fe,hh,bar[Fe],Fe ----
Do i1=1,2
Do i2=1,3
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass) .and. (MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFehhL(i2,i5,i1)
coup3R = cplcFeFehhR(i2,i5,i1)
coup4L = cplcFeFehhL(i5,i4,i3)
coup4R = cplcFeFehhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i2)*MFe(i5)*MFe(i4)*MfSFbSFbFb(p2,Mhh2(i1),MFe2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFehhL(i2,i5,i1)
coup3R = cplcFeFehhR(i2,i5,i1)
coup4L = cplcFeFehhL(i5,i4,i3)
coup4R = cplcFeFehhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i4)*MfSFSFbF(p2,Mhh2(i1),MFe2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFehhL(i2,i5,i1)
coup3R = cplcFeFehhR(i2,i5,i1)
coup4L = cplcFeFehhL(i5,i4,i3)
coup4R = cplcFeFehhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i5)*MfSFSFFb(p2,Mhh2(i1),MFe2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- hh,Fu,hh,bar[Fu],Fu ----
Do i1=1,2
Do i2=1,3
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFuFuhhL(i4,i2,gE2)
coup2R = cplcFuFuhhR(i4,i2,gE2)
coup3L = cplcFuFuhhL(i2,i5,i1)
coup3R = cplcFuFuhhR(i2,i5,i1)
coup4L = cplcFuFuhhL(i5,i4,i3)
coup4R = cplcFuFuhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i2)*MFu(i5)*MFu(i4)*MfSFbSFbFb(p2,Mhh2(i1),MFu2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFuFuhhL(i4,i2,gE2)
coup2R = cplcFuFuhhR(i4,i2,gE2)
coup3L = cplcFuFuhhL(i2,i5,i1)
coup3R = cplcFuFuhhR(i2,i5,i1)
coup4L = cplcFuFuhhL(i5,i4,i3)
coup4R = cplcFuFuhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i4)*MfSFSFbF(p2,Mhh2(i1),MFu2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFuFuhhL(i4,i2,gE2)
coup2R = cplcFuFuhhR(i4,i2,gE2)
coup3L = cplcFuFuhhL(i2,i5,i1)
coup3R = cplcFuFuhhR(i2,i5,i1)
coup4L = cplcFuFuhhL(i5,i4,i3)
coup4R = cplcFuFuhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i5)*MfSFSFFb(p2,Mhh2(i1),MFu2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- hh,Fxv,hh,bar[Fxv],Fxv ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,2
if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass) .and. (MFxv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFxvFxvhhL(i4,i2,gE2)
coup2R = cplcFxvFxvhhR(i4,i2,gE2)
coup3L = cplcFxvFxvhhL(i2,i5,i1)
coup3R = cplcFxvFxvhhR(i2,i5,i1)
coup4L = cplcFxvFxvhhL(i5,i4,i3)
coup4R = cplcFxvFxvhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i2)*MFxv(i5)*MFxv(i4)*MfSFbSFbFb(p2,Mhh2(i1),MFxv2(i2),Mhh2(i3),MFxv2(i4),MFxv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFxvFxvhhL(i4,i2,gE2)
coup2R = cplcFxvFxvhhR(i4,i2,gE2)
coup3L = cplcFxvFxvhhL(i2,i5,i1)
coup3R = cplcFxvFxvhhR(i2,i5,i1)
coup4L = cplcFxvFxvhhL(i5,i4,i3)
coup4R = cplcFxvFxvhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i4)*MfSFSFbF(p2,Mhh2(i1),MFxv2(i2),Mhh2(i3),MFxv2(i4),MFxv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i3)
coup2L = cplcFxvFxvhhL(i4,i2,gE2)
coup2R = cplcFxvFxvhhR(i4,i2,gE2)
coup3L = cplcFxvFxvhhL(i2,i5,i1)
coup3R = cplcFxvFxvhhR(i2,i5,i1)
coup4L = cplcFxvFxvhhL(i5,i4,i3)
coup4R = cplcFxvFxvhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i5)*MfSFSFFb(p2,Mhh2(i1),MFxv2(i2),Mhh2(i3),MFxv2(i4),MFxv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- Hp,Fd,conj[Hp],bar[Fd],Fu ----
Do i2=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFdFdhhL(i4,i2,gE2)
coup2R = cplcFdFdhhR(i4,i2,gE2)
coup3L = cplcFdFucHpL(i2,i5)
coup3R = cplcFdFucHpR(i2,i5)
coup4L = cplcFuFdHpL(i5,i4)
coup4R = cplcFuFdHpR(i5,i4)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFu(i5)*MFd(i4)*MfSFbSFbFb(p2,MHp2,MFd2(i2),MHp2,MFd2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFdFdhhL(i4,i2,gE2)
coup2R = cplcFdFdhhR(i4,i2,gE2)
coup3L = cplcFdFucHpL(i2,i5)
coup3R = cplcFdFucHpR(i2,i5)
coup4L = cplcFuFdHpL(i5,i4)
coup4R = cplcFuFdHpR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i4)*MfSFSFbF(p2,MHp2,MFd2(i2),MHp2,MFd2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFdFdhhL(i4,i2,gE2)
coup2R = cplcFdFdhhR(i4,i2,gE2)
coup3L = cplcFdFucHpL(i2,i5)
coup3R = cplcFdFucHpR(i2,i5)
coup4L = cplcFuFdHpL(i5,i4)
coup4R = cplcFuFdHpR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i5)*MfSFSFFb(p2,MHp2,MFd2(i2),MHp2,MFd2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
! ---- Hp,Fe,conj[Hp],bar[Fe],Fv ----
Do i2=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass) .and. (MFv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFvcHpL(i2,i5)
coup3R = cplcFeFvcHpR(i2,i5)
coup4L = cplcFvFeHpL(i5,i4)
coup4R = cplcFvFeHpR(i5,i4)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i2)*MFv(i5)*MFe(i4)*MfSFbSFbFb(p2,MHp2,MFe2(i2),MHp2,MFe2(i4),MFv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFvcHpL(i2,i5)
coup3R = cplcFeFvcHpR(i2,i5)
coup4L = cplcFvFeHpL(i5,i4)
coup4R = cplcFvFeHpR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i4)*MfSFSFbF(p2,MHp2,MFe2(i2),MHp2,MFe2(i4),MFv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFvcHpL(i2,i5)
coup3R = cplcFeFvcHpR(i2,i5)
coup4L = cplcFvFeHpL(i5,i4)
coup4R = cplcFvFeHpR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFv(i5)*MfSFSFFb(p2,MHp2,MFe2(i2),MHp2,MFe2(i4),MFv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
! ---- Hp,bar[Fu],conj[Hp],Fu,bar[Fd] ----
Do i2=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i5) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFdFucHpL(i5,i2)
coup3R = cplcFdFucHpR(i5,i2)
coup4L = cplcFuFdHpL(i4,i5)
coup4R = cplcFuFdHpR(i4,i5)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFd(i5)*MFu(i2)*MfSFbSFbFb(p2,MHp2,MFu2(i2),MHp2,MFu2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFdFucHpL(i5,i2)
coup3R = cplcFdFucHpR(i5,i2)
coup4L = cplcFuFdHpL(i4,i5)
coup4R = cplcFuFdHpR(i4,i5)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i4)*MfSFSFbF(p2,MHp2,MFu2(i2),MHp2,MFu2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFuFuhhL(i2,i4,gE2)
coup2R = cplcFuFuhhR(i2,i4,gE2)
coup3L = cplcFdFucHpL(i5,i2)
coup3R = cplcFdFucHpR(i5,i2)
coup4L = cplcFuFdHpL(i4,i5)
coup4R = cplcFuFdHpR(i4,i5)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i5)*MfSFSFFb(p2,MHp2,MFu2(i2),MHp2,MFu2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
! ---- Hp,bar[Fxv],conj[Hp],Fxv,bar[Fxe] ----
Do i2=1,2
Do i4=1,2
if((MFxe .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFxvFxvhhL(i2,i4,gE2)
coup2R = cplcFxvFxvhhR(i2,i4,gE2)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
coup4L = cplcFxvFxeHpL(i4)
coup4R = cplcFxvFxeHpR(i4)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MFxv(i4)*MFxv(i2)*MfSFbSFbFb(p2,MHp2,MFxv2(i2),MHp2,MFxv2(i4),MFxe2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFxvFxvhhL(i2,i4,gE2)
coup2R = cplcFxvFxvhhR(i2,i4,gE2)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
coup4L = cplcFxvFxeHpL(i4)
coup4R = cplcFxvFxeHpR(i4)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i4)*MfSFSFbF(p2,MHp2,MFxv2(i2),MHp2,MFxv2(i4),MFxe2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxe .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2L = cplcFxvFxvhhL(i2,i4,gE2)
coup2R = cplcFxvFxvhhR(i2,i4,gE2)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
coup4L = cplcFxvFxeHpL(i4)
coup4R = cplcFxvFxeHpR(i4)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MfSFSFFb(p2,MHp2,MFxv2(i2),MHp2,MFxv2(i4),MFxe2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
! ---- Ssc,Fe,conj[Ssc],bar[Fe],Fxe ----
Do i1=1,2
Do i2=1,3
Do i3=1,2
Do i4=1,3
if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass) .and. (MFxe .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i3)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFxecSscL(i2,i1)
coup3R = cplcFeFxecSscR(i2,i1)
coup4L = cplcFxeFeSscL(i4,i3)
coup4R = cplcFxeFeSscR(i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MFe(i2)*MFe(i4)*MfSFbSFbFb(p2,MSsc2(i1),MFe2(i2),MSsc2(i3),MFe2(i4),MFxe2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i3)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFxecSscL(i2,i1)
coup3R = cplcFeFxecSscR(i2,i1)
coup4L = cplcFxeFeSscL(i4,i3)
coup4R = cplcFxeFeSscR(i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i4)*MfSFSFbF(p2,MSsc2(i1),MFe2(i2),MSsc2(i3),MFe2(i4),MFxe2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxe .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i3)
coup2L = cplcFeFehhL(i4,i2,gE2)
coup2R = cplcFeFehhR(i4,i2,gE2)
coup3L = cplcFeFxecSscL(i2,i1)
coup3R = cplcFeFxecSscR(i2,i1)
coup4L = cplcFxeFeSscL(i4,i3)
coup4R = cplcFxeFeSscR(i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MfSFSFFb(p2,MSsc2(i1),MFe2(i2),MSsc2(i3),MFe2(i4),MFxe2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Ssc,bar[Fxv],conj[Ssc],Fxv,bar[Fv] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,3
if((MFv(i5) .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i3)
coup2L = cplcFxvFxvhhL(i2,i4,gE2)
coup2R = cplcFxvFxvhhR(i2,i4,gE2)
coup3L = cplcFvFxvcSscL(i5,i2,i1)
coup3R = cplcFvFxvcSscR(i5,i2,i1)
coup4L = cplcFxvFvSscL(i4,i5,i3)
coup4R = cplcFxvFvSscR(i4,i5,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i4)*MFv(i5)*MFxv(i2)*MfSFbSFbFb(p2,MSsc2(i1),MFxv2(i2),MSsc2(i3),MFxv2(i4),MFv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i3)
coup2L = cplcFxvFxvhhL(i2,i4,gE2)
coup2R = cplcFxvFxvhhR(i2,i4,gE2)
coup3L = cplcFvFxvcSscL(i5,i2,i1)
coup3R = cplcFvFxvcSscR(i5,i2,i1)
coup4L = cplcFxvFvSscL(i4,i5,i3)
coup4R = cplcFxvFvSscR(i4,i5,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i4)*MfSFSFbF(p2,MSsc2(i1),MFxv2(i2),MSsc2(i3),MFxv2(i4),MFv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i3)
coup2L = cplcFxvFxvhhL(i2,i4,gE2)
coup2R = cplcFxvFxvhhR(i2,i4,gE2)
coup3L = cplcFvFxvcSscL(i5,i2,i1)
coup3R = cplcFvFxvcSscR(i5,i2,i1)
coup4L = cplcFxvFvSscL(i4,i5,i3)
coup4R = cplcFxvFvSscR(i4,i5,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFv(i5)*MfSFSFFb(p2,MSsc2(i1),MFxv2(i2),MSsc2(i3),MFxv2(i4),MFv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
end do
end do
end do
! ---- Topology VoSSSFF
! ---- Ah,Ah,Ah,Fd,bar[Fd] ----
Do i4=1,3
Do i5=1,3
if((MFd(i4) .gt. epsfmass) .and. (MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhh(gE2)
coup3L = cplcFdFdAhL(i5,i4)
coup3R = cplcFdFdAhR(i5,i4)
coup4L = cplcFdFdAhL(i4,i5)
coup4R = cplcFdFdAhR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i4)*MFd(i5)*VfSSSFbFb(p2,MAh2,MAh2,MAh2,MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhh(gE2)
coup3L = cplcFdFdAhL(i5,i4)
coup3R = cplcFdFdAhR(i5,i4)
coup4L = cplcFdFdAhL(i4,i5)
coup4R = cplcFdFdAhR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*VfSSSFF(p2,MAh2,MAh2,MAh2,MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- Ah,Ah,Ah,Fe,bar[Fe] ----
Do i4=1,3
Do i5=1,3
if((MFe(i4) .gt. epsfmass) .and. (MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhh(gE2)
coup3L = cplcFeFeAhL(i5,i4)
coup3R = cplcFeFeAhR(i5,i4)
coup4L = cplcFeFeAhL(i4,i5)
coup4R = cplcFeFeAhR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFe(i4)*MFe(i5)*VfSSSFbFb(p2,MAh2,MAh2,MAh2,MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhh(gE2)
coup3L = cplcFeFeAhL(i5,i4)
coup3R = cplcFeFeAhR(i5,i4)
coup4L = cplcFeFeAhL(i4,i5)
coup4R = cplcFeFeAhR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSFF(p2,MAh2,MAh2,MAh2,MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- Ah,Ah,Ah,Fu,bar[Fu] ----
Do i4=1,3
Do i5=1,3
if((MFu(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhh(gE2)
coup3L = cplcFuFuAhL(i5,i4)
coup3R = cplcFuFuAhR(i5,i4)
coup4L = cplcFuFuAhL(i4,i5)
coup4R = cplcFuFuAhR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i4)*MFu(i5)*VfSSSFbFb(p2,MAh2,MAh2,MAh2,MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhh(gE2)
coup3L = cplcFuFuAhL(i5,i4)
coup3R = cplcFuFuAhR(i5,i4)
coup4L = cplcFuFuAhL(i4,i5)
coup4R = cplcFuFuAhR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*VfSSSFF(p2,MAh2,MAh2,MAh2,MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- Ah,Ah,Ah,Fxv,bar[Fxv] ----
Do i4=1,2
Do i5=1,2
if((MFxv(i4) .gt. epsfmass) .and. (MFxv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhh(gE2)
coup3L = cplcFxvFxvAhL(i5,i4)
coup3R = cplcFxvFxvAhR(i5,i4)
coup4L = cplcFxvFxvAhL(i4,i5)
coup4R = cplcFxvFxvAhR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFxv(i4)*MFxv(i5)*VfSSSFbFb(p2,MAh2,MAh2,MAh2,MFxv2(i4),MFxv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplAhAhhh(gE1)
coup2 = cplAhAhhh(gE2)
coup3L = cplcFxvFxvAhL(i5,i4)
coup3R = cplcFxvFxvAhR(i5,i4)
coup4L = cplcFxvFxvAhL(i4,i5)
coup4R = cplcFxvFxvAhR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSFF(p2,MAh2,MAh2,MAh2,MFxv2(i4),MFxv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- hh,hh,hh,Fd,bar[Fd] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFd(i4) .gt. epsfmass) .and. (MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3L = cplcFdFdhhL(i5,i4,i2)
coup3R = cplcFdFdhhR(i5,i4,i2)
coup4L = cplcFdFdhhL(i4,i5,i3)
coup4R = cplcFdFdhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i4)*MFd(i5)*VfSSSFbFb(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3L = cplcFdFdhhL(i5,i4,i2)
coup3R = cplcFdFdhhR(i5,i4,i2)
coup4L = cplcFdFdhhL(i4,i5,i3)
coup4R = cplcFdFdhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*VfSSSFF(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,hh,hh,Fe,bar[Fe] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFe(i4) .gt. epsfmass) .and. (MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3L = cplcFeFehhL(i5,i4,i2)
coup3R = cplcFeFehhR(i5,i4,i2)
coup4L = cplcFeFehhL(i4,i5,i3)
coup4R = cplcFeFehhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFe(i4)*MFe(i5)*VfSSSFbFb(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3L = cplcFeFehhL(i5,i4,i2)
coup3R = cplcFeFehhR(i5,i4,i2)
coup4L = cplcFeFehhL(i4,i5,i3)
coup4R = cplcFeFehhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSFF(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,hh,hh,Fu,bar[Fu] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFu(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3L = cplcFuFuhhL(i5,i4,i2)
coup3R = cplcFuFuhhR(i5,i4,i2)
coup4L = cplcFuFuhhL(i4,i5,i3)
coup4R = cplcFuFuhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i4)*MFu(i5)*VfSSSFbFb(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3L = cplcFuFuhhL(i5,i4,i2)
coup3R = cplcFuFuhhR(i5,i4,i2)
coup4L = cplcFuFuhhL(i4,i5,i3)
coup4R = cplcFuFuhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*VfSSSFF(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- hh,hh,hh,Fxv,bar[Fxv] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,2
if((MFxv(i4) .gt. epsfmass) .and. (MFxv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3L = cplcFxvFxvhhL(i5,i4,i2)
coup3R = cplcFxvFxvhhR(i5,i4,i2)
coup4L = cplcFxvFxvhhL(i4,i5,i3)
coup4R = cplcFxvFxvhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFxv(i4)*MFxv(i5)*VfSSSFbFb(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MFxv2(i4),MFxv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhhhhh(gE1,i1,i2)
coup2 = cplhhhhhh(gE2,i1,i3)
coup3L = cplcFxvFxvhhL(i5,i4,i2)
coup3R = cplcFxvFxvhhR(i5,i4,i2)
coup4L = cplcFxvFxvhhL(i4,i5,i3)
coup4R = cplcFxvFxvhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSFF(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),MFxv2(i4),MFxv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Hp,conj[Hp],Hp,Fd,bar[Fu] ----
Do i4=1,3
Do i5=1,3
if((MFd(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhHpcHp(gE2)
coup3L = cplcFuFdHpL(i5,i4)
coup3R = cplcFuFdHpR(i5,i4)
coup4L = cplcFdFucHpL(i4,i5)
coup4R = cplcFdFucHpR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFu(i5)*VfSSSFbFb(p2,MHp2,MHp2,MHp2,MFd2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhHpcHp(gE2)
coup3L = cplcFuFdHpL(i5,i4)
coup3R = cplcFuFdHpR(i5,i4)
coup4L = cplcFdFucHpL(i4,i5)
coup4R = cplcFdFucHpR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfSSSFF(p2,MHp2,MHp2,MHp2,MFd2(i4),MFu2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- Hp,conj[Hp],Hp,Fe,bar[Fv] ----
Do i4=1,3
Do i5=1,3
if((MFe(i4) .gt. epsfmass) .and. (MFv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhHpcHp(gE2)
coup3L = cplcFvFeHpL(i5,i4)
coup3R = cplcFvFeHpR(i5,i4)
coup4L = cplcFeFvcHpL(i4,i5)
coup4R = cplcFeFvcHpR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i4)*MFv(i5)*VfSSSFbFb(p2,MHp2,MHp2,MHp2,MFe2(i4),MFv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhHpcHp(gE2)
coup3L = cplcFvFeHpL(i5,i4)
coup3R = cplcFvFeHpR(i5,i4)
coup4L = cplcFeFvcHpL(i4,i5)
coup4R = cplcFeFvcHpR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfSSSFF(p2,MHp2,MHp2,MHp2,MFe2(i4),MFv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
! ---- Hp,conj[Hp],Hp,Fxe,bar[Fxv] ----
Do i5=1,2
if((MFxe .gt. epsfmass) .and. (MFxv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhHpcHp(gE2)
coup3L = cplcFxvFxeHpL(i5)
coup3R = cplcFxvFxeHpR(i5)
coup4L = cplcFxeFxvcHpL(i5)
coup4R = cplcFxeFxvcHpR(i5)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MFxv(i5)*VfSSSFbFb(p2,MHp2,MHp2,MHp2,MFxe2,MFxv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhHpcHp(gE1)
coup2 = cplhhHpcHp(gE2)
coup3L = cplcFxvFxeHpL(i5)
coup3R = cplcFxvFxeHpR(i5)
coup4L = cplcFxeFxvcHpL(i5)
coup4R = cplcFxeFxvcHpR(i5)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfSSSFF(p2,MHp2,MHp2,MHp2,MFxe2,MFxv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
! ---- Ssc,conj[Ssc],Ssc,Fe,bar[Fxe] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
if((MFe(i4) .gt. epsfmass) .and. (MFxe .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2 = cplhhSsccSsc(gE2,i3,i1)
coup3L = cplcFxeFeSscL(i4,i2)
coup3R = cplcFxeFeSscR(i4,i2)
coup4L = cplcFeFxecSscL(i4,i3)
coup4R = cplcFeFxecSscR(i4,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MFe(i4)*VfSSSFbFb(p2,MSsc2(i1),MSsc2(i2),MSsc2(i3),MFe2(i4),MFxe2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2 = cplhhSsccSsc(gE2,i3,i1)
coup3L = cplcFxeFeSscL(i4,i2)
coup3R = cplcFxeFeSscR(i4,i2)
coup4L = cplcFeFxecSscL(i4,i3)
coup4R = cplcFeFxecSscR(i4,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfSSSFF(p2,MSsc2(i1),MSsc2(i2),MSsc2(i3),MFe2(i4),MFxe2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Ssc,conj[Ssc],Ssc,Fv,bar[Fxv] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
Do i5=1,2
if((MFv(i4) .gt. epsfmass) .and. (MFxv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2 = cplhhSsccSsc(gE2,i3,i1)
coup3L = cplcFxvFvSscL(i5,i4,i2)
coup3R = cplcFxvFvSscR(i5,i4,i2)
coup4L = cplcFvFxvcSscL(i4,i5,i3)
coup4R = cplcFvFxvcSscR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFv(i4)*MFxv(i5)*VfSSSFbFb(p2,MSsc2(i1),MSsc2(i2),MSsc2(i3),MFv2(i4),MFxv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1 = cplhhSsccSsc(gE1,i1,i2)
coup2 = cplhhSsccSsc(gE2,i3,i1)
coup3L = cplcFxvFvSscL(i5,i4,i2)
coup3R = cplcFxvFvSscR(i5,i4,i2)
coup4L = cplcFvFxvcSscL(i4,i5,i3)
coup4R = cplcFvFxvcSscR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfSSSFF(p2,MSsc2(i1),MSsc2(i2),MSsc2(i3),MFv2(i4),MFxv2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Topology VoFFFFS
! ---- Fd,bar[Fd],Fd,bar[Fd],Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2)
coup3R = cplcFdFdAhR(i4,i2)
coup4L = cplcFdFdAhL(i3,i4)
coup4R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i3)*MFd(i2)*MFd(i4)*VfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2)
coup3R = cplcFdFdAhR(i4,i2)
coup4L = cplcFdFdAhL(i3,i4)
coup4R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i1)*MFd(i3)*VfFbFFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2)
coup3R = cplcFdFdAhR(i4,i2)
coup4L = cplcFdFdAhL(i3,i4)
coup4R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i4)*VfFbFFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2)
coup3R = cplcFdFdAhR(i4,i2)
coup4L = cplcFdFdAhL(i3,i4)
coup4R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFd(i2)*VfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2)
coup3R = cplcFdFdAhR(i4,i2)
coup4L = cplcFdFdAhL(i3,i4)
coup4R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i3)*MFd(i4)*VfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdAhL(i4,i2)
coup3R = cplcFdFdAhR(i4,i2)
coup4L = cplcFdFdAhL(i3,i4)
coup4R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Fd,bar[Fd],Fd,bar[Fd],hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i3)*MFd(i2)*MFd(i4)*VfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i1)*MFd(i3)*VfFbFFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i4)*VfFbFFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFd(i2)*VfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i3)*MFd(i4)*VfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fd,bar[Fd],Fd,bar[Fu],Hp ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFuFdHpL(i4,i2)
coup3R = cplcFuFdHpR(i4,i2)
coup4L = cplcFdFucHpL(i3,i4)
coup4R = cplcFdFucHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i3)*MFd(i2)*MFu(i4)*VfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFuFdHpL(i4,i2)
coup3R = cplcFuFdHpR(i4,i2)
coup4L = cplcFdFucHpL(i3,i4)
coup4R = cplcFdFucHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i1)*MFd(i3)*VfFbFFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFuFdHpL(i4,i2)
coup3R = cplcFuFdHpR(i4,i2)
coup4L = cplcFdFucHpL(i3,i4)
coup4R = cplcFdFucHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFu(i4)*VfFbFFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFuFdHpL(i4,i2)
coup3R = cplcFuFdHpR(i4,i2)
coup4L = cplcFdFucHpL(i3,i4)
coup4R = cplcFdFucHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFd(i2)*VfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFuFdHpL(i4,i2)
coup3R = cplcFuFdHpR(i4,i2)
coup4L = cplcFdFucHpL(i3,i4)
coup4R = cplcFdFucHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i3)*MFu(i4)*VfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i3,gE2)
coup2R = cplcFdFdhhR(i1,i3,gE2)
coup3L = cplcFuFdHpL(i4,i2)
coup3R = cplcFuFdHpR(i4,i2)
coup4L = cplcFdFucHpL(i3,i4)
coup4R = cplcFdFucHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fe],Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2)
coup3R = cplcFeFeAhR(i4,i2)
coup4L = cplcFeFeAhL(i3,i4)
coup4R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i3)*MFe(i2)*MFe(i4)*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2)
coup3R = cplcFeFeAhR(i4,i2)
coup4L = cplcFeFeAhL(i3,i4)
coup4R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2)
coup3R = cplcFeFeAhR(i4,i2)
coup4L = cplcFeFeAhL(i3,i4)
coup4R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i4)*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2)
coup3R = cplcFeFeAhR(i4,i2)
coup4L = cplcFeFeAhL(i3,i4)
coup4R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2)
coup3R = cplcFeFeAhR(i4,i2)
coup4L = cplcFeFeAhL(i3,i4)
coup4R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i3)*MFe(i4)*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFeAhL(i4,i2)
coup3R = cplcFeFeAhR(i4,i2)
coup4L = cplcFeFeAhL(i3,i4)
coup4R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fe],hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i3)*MFe(i2)*MFe(i4)*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i4)*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i3)*MFe(i4)*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fv],Hp ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFvFeHpL(i4,i2)
coup3R = cplcFvFeHpR(i4,i2)
coup4L = cplcFeFvcHpL(i3,i4)
coup4R = cplcFeFvcHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i3)*MFe(i2)*MFv(i4)*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFv2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFvFeHpL(i4,i2)
coup3R = cplcFvFeHpR(i4,i2)
coup4L = cplcFeFvcHpL(i3,i4)
coup4R = cplcFeFvcHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFv2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFvFeHpL(i4,i2)
coup3R = cplcFvFeHpR(i4,i2)
coup4L = cplcFeFvcHpL(i3,i4)
coup4R = cplcFeFvcHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFv(i4)*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFv2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFvFeHpL(i4,i2)
coup3R = cplcFvFeHpR(i4,i2)
coup4L = cplcFeFvcHpL(i3,i4)
coup4R = cplcFeFvcHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFv2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFvFeHpL(i4,i2)
coup3R = cplcFvFeHpR(i4,i2)
coup4L = cplcFeFvcHpL(i3,i4)
coup4R = cplcFeFvcHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i3)*MFv(i4)*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFv2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFvFeHpL(i4,i2)
coup3R = cplcFvFeHpR(i4,i2)
coup4L = cplcFeFvcHpL(i3,i4)
coup4R = cplcFeFvcHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFv2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fxe],Ssc ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i5=1,2
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFxe .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFxeFeSscL(i2,i5)
coup3R = cplcFxeFeSscR(i2,i5)
coup4L = cplcFeFxecSscL(i3,i5)
coup4R = cplcFeFxecSscR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MFe(i1)*MFe(i3)*MFe(i2)*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFxe2,MSsc2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFxeFeSscL(i2,i5)
coup3R = cplcFxeFeSscR(i2,i5)
coup4L = cplcFeFxecSscL(i3,i5)
coup4R = cplcFeFxecSscR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFxe2,MSsc2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFxe .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFxeFeSscL(i2,i5)
coup3R = cplcFxeFeSscR(i2,i5)
coup4L = cplcFeFxecSscL(i3,i5)
coup4R = cplcFeFxecSscR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MFe(i1)*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFxe2,MSsc2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFxeFeSscL(i2,i5)
coup3R = cplcFxeFeSscR(i2,i5)
coup4L = cplcFeFxecSscL(i3,i5)
coup4R = cplcFeFxecSscR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFxe2,MSsc2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFxe .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFxeFeSscL(i2,i5)
coup3R = cplcFxeFeSscR(i2,i5)
coup4L = cplcFeFxecSscL(i3,i5)
coup4R = cplcFeFxecSscR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxe*MFe(i3)*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFxe2,MSsc2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFeFehhL(i2,i1,gE1)
coup1R = cplcFeFehhR(i2,i1,gE1)
coup2L = cplcFeFehhL(i1,i3,gE2)
coup2R = cplcFeFehhR(i1,i3,gE2)
coup3L = cplcFxeFeSscL(i2,i5)
coup3R = cplcFxeFeSscR(i2,i5)
coup4L = cplcFeFxecSscL(i3,i5)
coup4R = cplcFeFxecSscR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFxe2,MSsc2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Fu,bar[Fu],Fu,bar[Fu],Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2)
coup3R = cplcFuFuAhR(i4,i2)
coup4L = cplcFuFuAhL(i3,i4)
coup4R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i3)*MFu(i2)*MFu(i4)*VfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2)
coup3R = cplcFuFuAhR(i4,i2)
coup4L = cplcFuFuAhL(i3,i4)
coup4R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i1)*MFu(i3)*VfFbFFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2)
coup3R = cplcFuFuAhR(i4,i2)
coup4L = cplcFuFuAhL(i3,i4)
coup4R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i4)*VfFbFFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2)
coup3R = cplcFuFuAhR(i4,i2)
coup4L = cplcFuFuAhL(i3,i4)
coup4R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i3)*MFu(i2)*VfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2)
coup3R = cplcFuFuAhR(i4,i2)
coup4L = cplcFuFuAhL(i3,i4)
coup4R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i3)*MFu(i4)*VfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuAhL(i4,i2)
coup3R = cplcFuFuAhR(i4,i2)
coup4L = cplcFuFuAhL(i3,i4)
coup4R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Fu,bar[Fu],Fu,bar[Fu],hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i3)*MFu(i2)*MFu(i4)*VfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i1)*MFu(i3)*VfFbFFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i4)*VfFbFFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i3)*MFu(i2)*VfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i3)*MFu(i4)*VfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fu,bar[Fu],Fu,bar[Fd],conj[Hp] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i4) .gt. epsfmass) .and. (MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFdFucHpL(i4,i2)
coup3R = cplcFdFucHpR(i4,i2)
coup4L = cplcFuFdHpL(i3,i4)
coup4R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i3)*MFd(i4)*MFu(i2)*VfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFdFucHpL(i4,i2)
coup3R = cplcFdFucHpR(i4,i2)
coup4L = cplcFuFdHpL(i3,i4)
coup4R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i1)*MFu(i3)*VfFbFFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass) .and. (MFu(i1) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFdFucHpL(i4,i2)
coup3R = cplcFdFucHpR(i4,i2)
coup4L = cplcFuFdHpL(i3,i4)
coup4R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFd(i4)*VfFbFFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFdFucHpL(i4,i2)
coup3R = cplcFdFucHpR(i4,i2)
coup4L = cplcFuFdHpL(i3,i4)
coup4R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i3)*MFu(i2)*VfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFdFucHpL(i4,i2)
coup3R = cplcFdFucHpR(i4,i2)
coup4L = cplcFuFdHpL(i3,i4)
coup4R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i3)*MFd(i4)*VfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i3,gE2)
coup2R = cplcFuFuhhR(i1,i3,gE2)
coup3L = cplcFdFucHpL(i4,i2)
coup3R = cplcFdFucHpR(i4,i2)
coup4L = cplcFuFdHpL(i3,i4)
coup4R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Fxv,bar[Fxv],Fxv,bar[Fxv],Ah ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxvFxvAhL(i4,i2)
coup3R = cplcFxvFxvAhR(i4,i2)
coup4L = cplcFxvFxvAhL(i3,i4)
coup4R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i1)*MFxv(i3)*MFxv(i2)*MFxv(i4)*VfFbFbFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxvFxvAhL(i4,i2)
coup3R = cplcFxvFxvAhR(i4,i2)
coup4L = cplcFxvFxvAhL(i3,i4)
coup4R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i1)*MFxv(i3)*VfFbFFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxvFxvAhL(i4,i2)
coup3R = cplcFxvFxvAhR(i4,i2)
coup4L = cplcFxvFxvAhL(i3,i4)
coup4R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i1)*MFxv(i4)*VfFbFFFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxvFxvAhL(i4,i2)
coup3R = cplcFxvFxvAhR(i4,i2)
coup4L = cplcFxvFxvAhL(i3,i4)
coup4R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i3)*MFxv(i2)*VfFFbFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxvFxvAhL(i4,i2)
coup3R = cplcFxvFxvAhR(i4,i2)
coup4L = cplcFxvFxvAhL(i3,i4)
coup4R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i3)*MFxv(i4)*VfFFFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxvFxvAhL(i4,i2)
coup3R = cplcFxvFxvAhR(i4,i2)
coup4L = cplcFxvFxvAhL(i3,i4)
coup4R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
! ---- Fxv,bar[Fxv],Fxv,bar[Fxv],hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,2
if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxvFxvhhL(i4,i2,i5)
coup3R = cplcFxvFxvhhR(i4,i2,i5)
coup4L = cplcFxvFxvhhL(i3,i4,i5)
coup4R = cplcFxvFxvhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i1)*MFxv(i3)*MFxv(i2)*MFxv(i4)*VfFbFbFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxvFxvhhL(i4,i2,i5)
coup3R = cplcFxvFxvhhR(i4,i2,i5)
coup4L = cplcFxvFxvhhL(i3,i4,i5)
coup4R = cplcFxvFxvhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i1)*MFxv(i3)*VfFbFFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxvFxvhhL(i4,i2,i5)
coup3R = cplcFxvFxvhhR(i4,i2,i5)
coup4L = cplcFxvFxvhhL(i3,i4,i5)
coup4R = cplcFxvFxvhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i1)*MFxv(i4)*VfFbFFFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxvFxvhhL(i4,i2,i5)
coup3R = cplcFxvFxvhhR(i4,i2,i5)
coup4L = cplcFxvFxvhhL(i3,i4,i5)
coup4R = cplcFxvFxvhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i3)*MFxv(i2)*VfFFbFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxvFxvhhL(i4,i2,i5)
coup3R = cplcFxvFxvhhR(i4,i2,i5)
coup4L = cplcFxvFxvhhL(i3,i4,i5)
coup4R = cplcFxvFxvhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i3)*MFxv(i4)*VfFFFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxvFxvhhL(i4,i2,i5)
coup3R = cplcFxvFxvhhR(i4,i2,i5)
coup4L = cplcFxvFxvhhL(i3,i4,i5)
coup4R = cplcFxvFxvhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fxv,bar[Fxv],Fxv,bar[Fv],conj[Ssc] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
Do i5=1,2
if((MFv(i4) .gt. epsfmass) .and. (MFxv(i1) .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFvFxvcSscL(i4,i2,i5)
coup3R = cplcFvFxvcSscR(i4,i2,i5)
coup4L = cplcFxvFvSscL(i3,i4,i5)
coup4R = cplcFxvFvSscR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i1)*MFxv(i3)*MFv(i4)*MFxv(i2)*VfFbFbFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFv2(i4),MSsc2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFvFxvcSscL(i4,i2,i5)
coup3R = cplcFvFxvcSscR(i4,i2,i5)
coup4L = cplcFxvFvSscL(i3,i4,i5)
coup4R = cplcFxvFvSscR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i1)*MFxv(i3)*VfFbFFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFv2(i4),MSsc2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFv(i4) .gt. epsfmass) .and. (MFxv(i1) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFvFxvcSscL(i4,i2,i5)
coup3R = cplcFvFxvcSscR(i4,i2,i5)
coup4L = cplcFxvFvSscL(i3,i4,i5)
coup4R = cplcFxvFvSscR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i1)*MFv(i4)*VfFbFFFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFv2(i4),MSsc2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFvFxvcSscL(i4,i2,i5)
coup3R = cplcFvFxvcSscR(i4,i2,i5)
coup4L = cplcFxvFvSscL(i3,i4,i5)
coup4R = cplcFxvFvSscR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i3)*MFxv(i2)*VfFFbFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFv2(i4),MSsc2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFv(i4) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFvFxvcSscL(i4,i2,i5)
coup3R = cplcFvFxvcSscR(i4,i2,i5)
coup4L = cplcFxvFvSscL(i3,i4,i5)
coup4R = cplcFxvFvSscR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i3)*MFv(i4)*VfFFFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFv2(i4),MSsc2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFvFxvcSscL(i4,i2,i5)
coup3R = cplcFvFxvcSscR(i4,i2,i5)
coup4L = cplcFxvFvSscL(i3,i4,i5)
coup4R = cplcFxvFvSscR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFv2(i4),MSsc2(i5),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fxv,bar[Fxv],Fxv,bar[Fxe],conj[Hp] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
if((MFxe .gt. epsfmass) .and. (MFxv(i1) .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
coup4L = cplcFxvFxeHpL(i3)
coup4R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MFxv(i1)*MFxv(i3)*MFxv(i2)*VfFbFbFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxe2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
coup4L = cplcFxvFxeHpL(i3)
coup4R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i1)*MFxv(i3)*VfFbFFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxe2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxe .gt. epsfmass) .and. (MFxv(i1) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
coup4L = cplcFxvFxeHpL(i3)
coup4R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MFxv(i1)*VfFbFFFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxe2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
coup4L = cplcFxvFxeHpL(i3)
coup4R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i3)*MFxv(i2)*VfFFbFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxe2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

if((MFxe .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
coup4L = cplcFxvFxeHpL(i3)
coup4R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxe*MFxv(i3)*VfFFFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxe2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFxvFxvhhL(i2,i1,gE1)
coup1R = cplcFxvFxvhhR(i2,i1,gE1)
coup2L = cplcFxvFxvhhL(i1,i3,gE2)
coup2R = cplcFxvFxvhhR(i1,i3,gE2)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
coup4L = cplcFxvFxeHpL(i3)
coup4R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxe2,MHp2,Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end do
end do
end do
! ---- Topology GoFFFFV
! ---- Fd,bar[Fd] ----
Do i1=1,3
Do i2=1,3
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i2,gE2)
coup2R = cplcFdFdhhR(i1,i2,gE2)
coup3 = g3
coup4 = g3
prefactor=Real(coup1L*coup2R*coup3*coup4+coup1R*coup2L*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*GfFFV(p2,MFd2(i1),MFd2(i2),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFdFdhhL(i2,i1,gE1)
coup1R = cplcFdFdhhR(i2,i1,gE1)
coup2L = cplcFdFdhhL(i1,i2,gE2)
coup2R = cplcFdFdhhR(i1,i2,gE2)
coup3 = g3
coup4 = g3
prefactor=Real(coup1L*coup2L*coup3*coup4+coup1R*coup2R*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFd(i1)*MFd(i2)*GfFbFbV(p2,MFd2(i1),MFd2(i2),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
! ---- Fu,bar[Fu] ----
Do i1=1,3
Do i2=1,3
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i2,gE2)
coup2R = cplcFuFuhhR(i1,i2,gE2)
coup3 = g3
coup4 = g3
prefactor=Real(coup1L*coup2R*coup3*coup4+coup1R*coup2L*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*GfFFV(p2,MFu2(i1),MFu2(i2),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,2
   Do gE2=1,2
coup1L = cplcFuFuhhL(i2,i1,gE1)
coup1R = cplcFuFuhhR(i2,i1,gE1)
coup2L = cplcFuFuhhL(i1,i2,gE2)
coup2R = cplcFuFuhhR(i1,i2,gE2)
coup3 = g3
coup4 = g3
prefactor=Real(coup1L*coup2L*coup3*coup4+coup1R*coup2R*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrix(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrix(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFu(i1)*MFu(i2)*GfFbFbV(p2,MFu2(i1),MFu2(i2),Qscale)
 tempcont=tempcont+tempcouplingmatrix*funcvalue
end if
end if

end do
end do
do gE1=1,2
Pi2S(gE1,gE1)=Pi2S(gE1,gE1)+tempcont(gE1,gE1)*oo16Pi2*oo16Pi2
do gE2=1,gE1-1
Pi2S(gE1,gE2)=Pi2S(gE1,gE2)+0.5_dp*(tempcont(gE1,gE2)+tempcont(gE2,gE1))*oo16Pi2*oo16Pi2
Pi2S(gE2,gE1)=Pi2S(gE1,gE2)
End do
End do
Pi2S=Pi2S+delta2lmasses
Pi2S = Matmul(Pi2S,ZH)
Pi2S = Matmul(Transpose(ZH),Pi2S)

! -----------------------------------
! ------- CP ODD MASS DIAGRAMS ------
! -----------------------------------
tempcontah(:,:)=0._dp
tempcouplingmatrixah(:,:)=0._dp
! ---- Topology WoSSSS
! ---- Ah,Ah,Ah,hh ----
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhAhAh
coup2 = cplAhAhhh(i4)
coup3 = cplAhAhhh(i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSSS(p2,MAh2,MAh2,MAh2,Mhh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
! ---- hh,hh,Ah,Ah ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhhhh(i1,i2)
coup2 = cplAhAhhh(i1)
coup3 = cplAhAhhh(i2)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/4._dp*WfSSSS(p2,Mhh2(i1),Mhh2(i2),MAh2,MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- hh,hh,hh,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhhhh(i1,i2)
coup2 = cplhhhhhh(i1,i3,i4)
coup3 = cplhhhhhh(i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/4._dp*WfSSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Mhh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Hp,conj[Hp] ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhhhh(i1,i2)
coup2 = cplhhHpcHp(i1)
coup3 = cplhhHpcHp(i2)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSSS(p2,Mhh2(i1),Mhh2(i2),MHp2,MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- hh,hh,Ssc,conj[Ssc] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhhhh(i1,i2)
coup2 = cplhhSsccSsc(i1,i3,i4)
coup3 = cplhhSsccSsc(i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSSS(p2,Mhh2(i1),Mhh2(i2),MSsc2(i3),MSsc2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Hp,conj[Hp],hh,Hp ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhHpcHp
coup2 = cplhhHpcHp(i3)
coup3 = cplhhHpcHp(i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSSS(p2,MHp2,MHp2,Mhh2(i3),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
! ---- Ssc,conj[Ssc],hh,Ssc ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhSsccSsc(i1,i2)
coup2 = cplhhSsccSsc(i3,i4,i1)
coup3 = cplhhSsccSsc(i3,i2,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSSS(p2,MSsc2(i1),MSsc2(i2),Mhh2(i3),MSsc2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Topology XoSSS
! ---- Ah,Ah,Ah ----
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhAhAh
coup2 = cplAhAhAhAh
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*XfSSS(p2,MAh2,MAh2,MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
! ---- Ah,Ah,hh ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhAhAh
coup2 = cplAhAhhhhh(i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*XfSSS(p2,MAh2,MAh2,Mhh2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
! ---- Ah,Ah,Hp ----
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhAhAh
coup2 = cplAhAhHpcHp
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MAh2,MAh2,MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
! ---- Ah,Ah,Ssc ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhAhAh
coup2 = cplAhAhSsccSsc(i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MAh2,MAh2,MSsc2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
! ---- hh,hh,Ah ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhhhh(i1,i2)
coup2 = cplAhAhhhhh(i1,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*XfSSS(p2,Mhh2(i1),Mhh2(i2),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- hh,hh,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhhhh(i1,i2)
coup2 = cplhhhhhhhh(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/4._dp*XfSSS(p2,Mhh2(i1),Mhh2(i2),Mhh2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- hh,hh,Hp ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhhhh(i1,i2)
coup2 = cplhhhhHpcHp(i1,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,Mhh2(i1),Mhh2(i2),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- hh,hh,Ssc ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhhhh(i1,i2)
coup2 = cplhhhhSsccSsc(i1,i2,i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,Mhh2(i1),Mhh2(i2),MSsc2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Hp,conj[Hp],Ah ----
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhHpcHp
coup2 = cplAhAhHpcHp
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MHp2,MHp2,MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
! ---- Hp,conj[Hp],hh ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhHpcHp
coup2 = cplhhhhHpcHp(i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MHp2,MHp2,Mhh2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
! ---- Hp,conj[Hp],Hp ----
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhHpcHp
coup2 = cplHpHpcHpcHp
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*XfSSS(p2,MHp2,MHp2,MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
! ---- Hp,conj[Hp],Ssc ----
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhHpcHp
coup2 = cplHpSsccHpcSsc(i3,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*XfSSS(p2,MHp2,MHp2,MSsc2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
! ---- Ssc,conj[Ssc],Ah ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhSsccSsc(i1,i2)
coup2 = cplAhAhSsccSsc(i2,i1)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MSsc2(i1),MSsc2(i2),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Ssc,conj[Ssc],hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhSsccSsc(i1,i2)
coup2 = cplhhhhSsccSsc(i3,i3,i2,i1)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*XfSSS(p2,MSsc2(i1),MSsc2(i2),Mhh2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Ssc,conj[Ssc],Hp ----
Do i1=1,2
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhSsccSsc(i1,i2)
coup2 = cplHpSsccHpcSsc(i2,i1)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*XfSSS(p2,MSsc2(i1),MSsc2(i2),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Ssc,conj[Ssc],Ssc ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhSsccSsc(i1,i2)
coup2 = cplSscSsccSsccSsc(i2,i3,i1,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*XfSSS(p2,MSsc2(i1),MSsc2(i2),MSsc2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Topology YoSSSS
! ---- Ah,hh,hh,Ah ----
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i3)
coup3 = cplAhAhhhhh(i2,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*YfSSSS(p2,MAh2,Mhh2(i2),Mhh2(i3),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Ah,hh,hh,hh ----
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i3)
coup3 = cplhhhhhhhh(i2,i3,i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*YfSSSS(p2,MAh2,Mhh2(i2),Mhh2(i3),Mhh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Ah,hh,hh,Hp ----
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i3)
coup3 = cplhhhhHpcHp(i2,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,MAh2,Mhh2(i2),Mhh2(i3),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Ah,hh,hh,Ssc ----
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i3)
coup3 = cplhhhhSsccSsc(i2,i3,i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,MAh2,Mhh2(i2),Mhh2(i3),MSsc2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- hh,Ah,Ah,Ah ----
Do i1=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2 = cplAhAhhh(i1)
coup3 = cplAhAhAhAh
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*YfSSSS(p2,Mhh2(i1),MAh2,MAh2,MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
! ---- hh,Ah,Ah,hh ----
Do i1=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2 = cplAhAhhh(i1)
coup3 = cplAhAhhhhh(i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*YfSSSS(p2,Mhh2(i1),MAh2,MAh2,Mhh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- hh,Ah,Ah,Hp ----
Do i1=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2 = cplAhAhhh(i1)
coup3 = cplAhAhHpcHp
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,Mhh2(i1),MAh2,MAh2,MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
! ---- hh,Ah,Ah,Ssc ----
Do i1=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2 = cplAhAhhh(i1)
coup3 = cplAhAhSsccSsc(i4,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*YfSSSS(p2,Mhh2(i1),MAh2,MAh2,MSsc2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Topology ZoSSSS
! ---- Ah,hh,Ah,hh ----
Do i2=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i4)
coup3 = cplAhAhhhhh(i2,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*ZfSSSS(p2,MAh2,Mhh2(i2),MAh2,Mhh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Topology SoSSS
! ---- Ah,Ah,Ah ----
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhAhAh
coup2 = cplAhAhAhAh
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/6._dp*SfSSS(p2,MAh2,MAh2,MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
! ---- Ah,hh,hh ----
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhhhh(i2,i3)
coup2 = cplAhAhhhhh(i2,i3)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*SfSSS(p2,MAh2,Mhh2(i2),Mhh2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Ah,Hp,conj[Hp] ----
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhHpcHp
coup2 = cplAhAhHpcHp
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*SfSSS(p2,MAh2,MHp2,MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
! ---- Ah,Ssc,conj[Ssc] ----
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhSsccSsc(i2,i3)
coup2 = cplAhAhSsccSsc(i3,i2)
prefactor=Real(coup1*coup2,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*SfSSS(p2,MAh2,MSsc2(i2),MSsc2(i3),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Topology UoSSSS
! ---- Ah,hh,Ah,Ah ----
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhAhAh
coup3 = cplAhAhhh(i2)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*UfSSSS(p2,MAh2,Mhh2(i2),MAh2,MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
! ---- Ah,hh,hh,hh ----
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhhhh(i3,i4)
coup3 = cplhhhhhh(i2,i3,i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*UfSSSS(p2,MAh2,Mhh2(i2),Mhh2(i3),Mhh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Ah,hh,Hp,conj[Hp] ----
Do i2=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhHpcHp
coup3 = cplhhHpcHp(i2)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*UfSSSS(p2,MAh2,Mhh2(i2),MHp2,MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
! ---- Ah,hh,Ssc,conj[Ssc] ----
Do i2=1,2
Do i3=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhSsccSsc(i3,i4)
coup3 = cplhhSsccSsc(i2,i4,i3)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*UfSSSS(p2,MAh2,Mhh2(i2),MSsc2(i3),MSsc2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- hh,Ah,Ah,hh ----
Do i1=1,2
Do i4=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2 = cplAhAhhhhh(i1,i4)
coup3 = cplAhAhhh(i4)
prefactor=Real(coup1*coup2*coup3,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -2._dp*UfSSSS(p2,Mhh2(i1),MAh2,MAh2,Mhh2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Topology VoSSSSS
! ---- Ah,hh,hh,Ah,Ah ----
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i3)
coup3 = cplAhAhhh(i2)
coup4 = cplAhAhhh(i3)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*VfSSSSS(p2,MAh2,Mhh2(i2),Mhh2(i3),MAh2,MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Ah,hh,hh,hh,hh ----
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i3)
coup3 = cplhhhhhh(i2,i4,i5)
coup4 = cplhhhhhh(i3,i4,i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp/2._dp*VfSSSSS(p2,MAh2,Mhh2(i2),Mhh2(i3),Mhh2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,hh,hh,Hp,conj[Hp] ----
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i3)
coup3 = cplhhHpcHp(i2)
coup4 = cplhhHpcHp(i3)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSSS(p2,MAh2,Mhh2(i2),Mhh2(i3),MHp2,MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Ah,hh,hh,Ssc,conj[Ssc] ----
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i3)
coup3 = cplhhSsccSsc(i2,i4,i5)
coup4 = cplhhSsccSsc(i3,i5,i4)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSSS(p2,MAh2,Mhh2(i2),Mhh2(i3),MSsc2(i4),MSsc2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- hh,Ah,Ah,Ah,hh ----
Do i1=1,2
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2 = cplAhAhhh(i1)
coup3 = cplAhAhhh(i5)
coup4 = cplAhAhhh(i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSSS(p2,Mhh2(i1),MAh2,MAh2,MAh2,Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Topology MoSSSSS
! ---- Ah,Ah,hh,hh,hh ----
Do i3=1,2
Do i4=1,2
Do i5=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i3)
coup2 = cplAhAhhh(i4)
coup3 = cplAhAhhh(i5)
coup4 = cplhhhhhh(i3,i4,i5)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfSSSSS(p2,MAh2,MAh2,Mhh2(i3),Mhh2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Ah,hh,hh,Ah,Ah ----
Do i2=1,2
Do i3=1,2
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i3)
coup2 = cplAhAhhh(i2)
coup3 = cplAhAhhh(i2)
coup4 = cplAhAhhh(i3)
prefactor=Real(coup1*coup2*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfSSSSS(p2,MAh2,Mhh2(i2),Mhh2(i3),MAh2,MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Topology WoSSFF
! ---- Ah,Ah,Fd,bar[Fd] ----
Do i3=1,3
Do i4=1,3
if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhAhAh
coup2L = cplcFdFdAhL(i4,i3)
coup2R = cplcFdFdAhR(i4,i3)
coup3L = cplcFdFdAhL(i3,i4)
coup3R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*MFd(i3)*MFd(i4)*WfSSFbFb(p2,MAh2,MAh2,MFd2(i3),MFd2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhAhAh
coup2L = cplcFdFdAhL(i4,i3)
coup2R = cplcFdFdAhR(i4,i3)
coup3L = cplcFdFdAhL(i3,i4)
coup3R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*WfSSFF(p2,MAh2,MAh2,MFd2(i3),MFd2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Ah,Ah,Fe,bar[Fe] ----
Do i3=1,3
Do i4=1,3
if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhAhAh
coup2L = cplcFeFeAhL(i4,i3)
coup2R = cplcFeFeAhR(i4,i3)
coup3L = cplcFeFeAhL(i3,i4)
coup3R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*MFe(i3)*MFe(i4)*WfSSFbFb(p2,MAh2,MAh2,MFe2(i3),MFe2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhAhAh
coup2L = cplcFeFeAhL(i4,i3)
coup2R = cplcFeFeAhR(i4,i3)
coup3L = cplcFeFeAhL(i3,i4)
coup3R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSFF(p2,MAh2,MAh2,MFe2(i3),MFe2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Ah,Ah,Fu,bar[Fu] ----
Do i3=1,3
Do i4=1,3
if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhAhAh
coup2L = cplcFuFuAhL(i4,i3)
coup2R = cplcFuFuAhR(i4,i3)
coup3L = cplcFuFuAhL(i3,i4)
coup3R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*MFu(i3)*MFu(i4)*WfSSFbFb(p2,MAh2,MAh2,MFu2(i3),MFu2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhAhAh
coup2L = cplcFuFuAhL(i4,i3)
coup2R = cplcFuFuAhR(i4,i3)
coup3L = cplcFuFuAhL(i3,i4)
coup3R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*WfSSFF(p2,MAh2,MAh2,MFu2(i3),MFu2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Ah,Ah,Fxv,bar[Fxv] ----
Do i3=1,2
Do i4=1,2
if((MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhAhAh
coup2L = cplcFxvFxvAhL(i4,i3)
coup2R = cplcFxvFxvAhR(i4,i3)
coup3L = cplcFxvFxvAhL(i3,i4)
coup3R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*MFxv(i3)*MFxv(i4)*WfSSFbFb(p2,MAh2,MAh2,MFxv2(i3),MFxv2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhAhAh
coup2L = cplcFxvFxvAhL(i4,i3)
coup2R = cplcFxvFxvAhR(i4,i3)
coup3L = cplcFxvFxvAhL(i3,i4)
coup3R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSFF(p2,MAh2,MAh2,MFxv2(i3),MFxv2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- hh,hh,Fd,bar[Fd] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,3
if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhhhh(i1,i2)
coup2L = cplcFdFdhhL(i4,i3,i1)
coup2R = cplcFdFdhhR(i4,i3,i1)
coup3L = cplcFdFdhhL(i3,i4,i2)
coup3R = cplcFdFdhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*MFd(i3)*MFd(i4)*WfSSFbFb(p2,Mhh2(i1),Mhh2(i2),MFd2(i3),MFd2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhhhh(i1,i2)
coup2L = cplcFdFdhhL(i4,i3,i1)
coup2R = cplcFdFdhhR(i4,i3,i1)
coup3L = cplcFdFdhhL(i3,i4,i2)
coup3R = cplcFdFdhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*WfSSFF(p2,Mhh2(i1),Mhh2(i2),MFd2(i3),MFd2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Fe,bar[Fe] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,3
if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhhhh(i1,i2)
coup2L = cplcFeFehhL(i4,i3,i1)
coup2R = cplcFeFehhR(i4,i3,i1)
coup3L = cplcFeFehhL(i3,i4,i2)
coup3R = cplcFeFehhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*MFe(i3)*MFe(i4)*WfSSFbFb(p2,Mhh2(i1),Mhh2(i2),MFe2(i3),MFe2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhhhh(i1,i2)
coup2L = cplcFeFehhL(i4,i3,i1)
coup2R = cplcFeFehhR(i4,i3,i1)
coup3L = cplcFeFehhL(i3,i4,i2)
coup3R = cplcFeFehhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSFF(p2,Mhh2(i1),Mhh2(i2),MFe2(i3),MFe2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Fu,bar[Fu] ----
Do i1=1,2
Do i2=1,2
Do i3=1,3
Do i4=1,3
if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhhhh(i1,i2)
coup2L = cplcFuFuhhL(i4,i3,i1)
coup2R = cplcFuFuhhR(i4,i3,i1)
coup3L = cplcFuFuhhL(i3,i4,i2)
coup3R = cplcFuFuhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*MFu(i3)*MFu(i4)*WfSSFbFb(p2,Mhh2(i1),Mhh2(i2),MFu2(i3),MFu2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhhhh(i1,i2)
coup2L = cplcFuFuhhL(i4,i3,i1)
coup2R = cplcFuFuhhR(i4,i3,i1)
coup3L = cplcFuFuhhL(i3,i4,i2)
coup3R = cplcFuFuhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp/2._dp*WfSSFF(p2,Mhh2(i1),Mhh2(i2),MFu2(i3),MFu2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- hh,hh,Fxv,bar[Fxv] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
if((MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhhhh(i1,i2)
coup2L = cplcFxvFxvhhL(i4,i3,i1)
coup2R = cplcFxvFxvhhR(i4,i3,i1)
coup3L = cplcFxvFxvhhL(i3,i4,i2)
coup3R = cplcFxvFxvhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*MFxv(i3)*MFxv(i4)*WfSSFbFb(p2,Mhh2(i1),Mhh2(i2),MFxv2(i3),MFxv2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhhhh(i1,i2)
coup2L = cplcFxvFxvhhL(i4,i3,i1)
coup2R = cplcFxvFxvhhR(i4,i3,i1)
coup3L = cplcFxvFxvhhL(i3,i4,i2)
coup3R = cplcFxvFxvhhR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp/2._dp*WfSSFF(p2,Mhh2(i1),Mhh2(i2),MFxv2(i3),MFxv2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Hp,conj[Hp],Fu,bar[Fd] ----
Do i3=1,3
Do i4=1,3
if((MFd(i4) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhHpcHp
coup2L = cplcFdFucHpL(i4,i3)
coup2R = cplcFdFucHpR(i4,i3)
coup3L = cplcFuFdHpL(i3,i4)
coup3R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp*MFu(i3)*MFd(i4)*WfSSFbFb(p2,MHp2,MHp2,MFu2(i3),MFd2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhHpcHp
coup2L = cplcFdFucHpL(i4,i3)
coup2R = cplcFdFucHpR(i4,i3)
coup3L = cplcFuFdHpL(i3,i4)
coup3R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -3._dp*WfSSFF(p2,MHp2,MHp2,MFu2(i3),MFd2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Hp,conj[Hp],Fv,bar[Fe] ----
Do i3=1,3
Do i4=1,3
if((MFe(i4) .gt. epsfmass) .and. (MFv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhHpcHp
coup2L = cplcFeFvcHpL(i4,i3)
coup2R = cplcFeFvcHpR(i4,i3)
coup3L = cplcFvFeHpL(i3,i4)
coup3R = cplcFvFeHpR(i3,i4)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*MFv(i3)*MFe(i4)*WfSSFbFb(p2,MHp2,MHp2,MFv2(i3),MFe2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhHpcHp
coup2L = cplcFeFvcHpL(i4,i3)
coup2R = cplcFeFvcHpR(i4,i3)
coup3L = cplcFvFeHpL(i3,i4)
coup3R = cplcFvFeHpR(i3,i4)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSFF(p2,MHp2,MHp2,MFv2(i3),MFe2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
! ---- Hp,conj[Hp],Fxv,bar[Fxe] ----
Do i3=1,2
if((MFxe .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhHpcHp
coup2L = cplcFxeFxvcHpL(i3)
coup2R = cplcFxeFxvcHpR(i3)
coup3L = cplcFxvFxeHpL(i3)
coup3R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*MFxe*MFxv(i3)*WfSSFbFb(p2,MHp2,MHp2,MFxv2(i3),MFxe2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhHpcHp
coup2L = cplcFxeFxvcHpL(i3)
coup2R = cplcFxeFxvcHpR(i3)
coup3L = cplcFxvFxeHpL(i3)
coup3R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSFF(p2,MHp2,MHp2,MFxv2(i3),MFxe2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
! ---- Ssc,conj[Ssc],Fxe,bar[Fe] ----
Do i1=1,2
Do i2=1,2
Do i4=1,3
if((MFe(i4) .gt. epsfmass) .and. (MFxe .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhSsccSsc(i1,i2)
coup2L = cplcFeFxecSscL(i4,i1)
coup2R = cplcFeFxecSscR(i4,i1)
coup3L = cplcFxeFeSscL(i4,i2)
coup3R = cplcFxeFeSscR(i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*MFxe*MFe(i4)*WfSSFbFb(p2,MSsc2(i1),MSsc2(i2),MFxe2,MFe2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhSsccSsc(i1,i2)
coup2L = cplcFeFxecSscL(i4,i1)
coup2R = cplcFeFxecSscR(i4,i1)
coup3L = cplcFxeFeSscL(i4,i2)
coup3R = cplcFxeFeSscR(i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSFF(p2,MSsc2(i1),MSsc2(i2),MFxe2,MFe2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Ssc,conj[Ssc],Fxv,bar[Fv] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
if((MFv(i4) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhSsccSsc(i1,i2)
coup2L = cplcFvFxvcSscL(i4,i3,i1)
coup2R = cplcFvFxvcSscR(i4,i3,i1)
coup3L = cplcFxvFvSscL(i3,i4,i2)
coup3R = cplcFxvFvSscR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3L+coup1*coup2R*coup3R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*MFxv(i3)*MFv(i4)*WfSSFbFb(p2,MSsc2(i1),MSsc2(i2),MFxv2(i3),MFv2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhSsccSsc(i1,i2)
coup2L = cplcFvFxvcSscL(i4,i3,i1)
coup2R = cplcFvFxvcSscR(i4,i3,i1)
coup3L = cplcFxvFvSscL(i3,i4,i2)
coup3R = cplcFxvFvSscR(i3,i4,i2)
prefactor=Real(coup1*coup2L*coup3R+coup1*coup2R*coup3L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= -1._dp*WfSSFF(p2,MSsc2(i1),MSsc2(i2),MFxv2(i3),MFv2(i4),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Topology MoFFFFS
! ---- Fd,bar[Fd],bar[Fd],Fd,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i3,i1)
coup1R = cplcFdFdAhR(i3,i1)
coup2L = cplcFdFdAhL(i2,i4)
coup2R = cplcFdFdAhR(i2,i4)
coup3L = cplcFdFdAhL(i1,i2)
coup3R = cplcFdFdAhR(i1,i2)
coup4L = cplcFdFdAhL(i4,i3)
coup4R = cplcFdFdAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i1)*MFd(i4)*MFd(i2)*MFd(i3)*MfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i3,i1)
coup1R = cplcFdFdAhR(i3,i1)
coup2L = cplcFdFdAhL(i2,i4)
coup2R = cplcFdFdAhR(i2,i4)
coup3L = cplcFdFdAhL(i1,i2)
coup3R = cplcFdFdAhR(i1,i2)
coup4L = cplcFdFdAhL(i4,i3)
coup4R = cplcFdFdAhR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFd(i3)*MfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i3,i1)
coup1R = cplcFdFdAhR(i3,i1)
coup2L = cplcFdFdAhL(i2,i4)
coup2R = cplcFdFdAhR(i2,i4)
coup3L = cplcFdFdAhL(i1,i2)
coup3R = cplcFdFdAhR(i1,i2)
coup4L = cplcFdFdAhL(i4,i3)
coup4R = cplcFdFdAhR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i2)*MfFFbFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i3,i1)
coup1R = cplcFdFdAhR(i3,i1)
coup2L = cplcFdFdAhL(i2,i4)
coup2R = cplcFdFdAhR(i2,i4)
coup3L = cplcFdFdAhL(i1,i2)
coup3R = cplcFdFdAhR(i1,i2)
coup4L = cplcFdFdAhL(i4,i3)
coup4R = cplcFdFdAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i3)*MfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i3,i1)
coup1R = cplcFdFdAhR(i3,i1)
coup2L = cplcFdFdAhL(i2,i4)
coup2R = cplcFdFdAhR(i2,i4)
coup3L = cplcFdFdAhL(i1,i2)
coup3R = cplcFdFdAhR(i1,i2)
coup4L = cplcFdFdAhL(i4,i3)
coup4R = cplcFdFdAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Fd,bar[Fd],bar[Fd],Fd,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i3,i1)
coup1R = cplcFdFdAhR(i3,i1)
coup2L = cplcFdFdAhL(i2,i4)
coup2R = cplcFdFdAhR(i2,i4)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i1)*MFd(i4)*MFd(i2)*MFd(i3)*MfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i3,i1)
coup1R = cplcFdFdAhR(i3,i1)
coup2L = cplcFdFdAhL(i2,i4)
coup2R = cplcFdFdAhR(i2,i4)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFd(i3)*MfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i3,i1)
coup1R = cplcFdFdAhR(i3,i1)
coup2L = cplcFdFdAhL(i2,i4)
coup2R = cplcFdFdAhR(i2,i4)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i2)*MfFFbFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i3,i1)
coup1R = cplcFdFdAhR(i3,i1)
coup2L = cplcFdFdAhL(i2,i4)
coup2R = cplcFdFdAhR(i2,i4)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i3)*MfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i3,i1)
coup1R = cplcFdFdAhR(i3,i1)
coup2L = cplcFdFdAhL(i2,i4)
coup2R = cplcFdFdAhR(i2,i4)
coup3L = cplcFdFdhhL(i1,i2,i5)
coup3R = cplcFdFdhhR(i1,i2,i5)
coup4L = cplcFdFdhhL(i4,i3,i5)
coup4R = cplcFdFdhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fd,bar[Fu],bar[Fd],Fu,conj[Hp] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i1) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i3,i1)
coup1R = cplcFdFdAhR(i3,i1)
coup2L = cplcFuFuAhL(i2,i4)
coup2R = cplcFuFuAhR(i2,i4)
coup3L = cplcFdFucHpL(i1,i2)
coup3R = cplcFdFucHpR(i1,i2)
coup4L = cplcFuFdHpL(i4,i3)
coup4R = cplcFuFdHpR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i1)*MFu(i4)*MFd(i3)*MFu(i2)*MfFbFbFbFbS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i3,i1)
coup1R = cplcFdFdAhR(i3,i1)
coup2L = cplcFuFuAhL(i2,i4)
coup2R = cplcFuFuAhR(i2,i4)
coup3L = cplcFdFucHpL(i1,i2)
coup3R = cplcFdFucHpR(i1,i2)
coup4L = cplcFuFdHpL(i4,i3)
coup4R = cplcFuFdHpR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFu(i2)*MfFFbFbFS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i3,i1)
coup1R = cplcFdFdAhR(i3,i1)
coup2L = cplcFuFuAhL(i2,i4)
coup2R = cplcFuFuAhR(i2,i4)
coup3L = cplcFdFucHpL(i1,i2)
coup3R = cplcFdFucHpR(i1,i2)
coup4L = cplcFuFdHpL(i4,i3)
coup4R = cplcFuFdHpR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i2)*MfFFbFFbS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i3,i1)
coup1R = cplcFdFdAhR(i3,i1)
coup2L = cplcFuFuAhL(i2,i4)
coup2R = cplcFuFuAhR(i2,i4)
coup3L = cplcFdFucHpL(i1,i2)
coup3R = cplcFdFucHpR(i1,i2)
coup4L = cplcFuFdHpL(i4,i3)
coup4R = cplcFuFdHpR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFd(i3)*MfFFFbFbS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i3,i1)
coup1R = cplcFdFdAhR(i3,i1)
coup2L = cplcFuFuAhL(i2,i4)
coup2R = cplcFuFuAhR(i2,i4)
coup3L = cplcFdFucHpL(i1,i2)
coup3R = cplcFdFucHpR(i1,i2)
coup4L = cplcFuFdHpL(i4,i3)
coup4R = cplcFuFdHpR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFd2(i1),MFu2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Fe,bar[Fe],bar[Fe],Fe,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i3,i1)
coup1R = cplcFeFeAhR(i3,i1)
coup2L = cplcFeFeAhL(i2,i4)
coup2R = cplcFeFeAhR(i2,i4)
coup3L = cplcFeFeAhL(i1,i2)
coup3R = cplcFeFeAhR(i1,i2)
coup4L = cplcFeFeAhL(i4,i3)
coup4R = cplcFeFeAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFe(i1)*MFe(i4)*MFe(i2)*MFe(i3)*MfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i3,i1)
coup1R = cplcFeFeAhR(i3,i1)
coup2L = cplcFeFeAhL(i2,i4)
coup2R = cplcFeFeAhR(i2,i4)
coup3L = cplcFeFeAhL(i1,i2)
coup3R = cplcFeFeAhR(i1,i2)
coup4L = cplcFeFeAhL(i4,i3)
coup4R = cplcFeFeAhR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i2)*MFe(i3)*MfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i3,i1)
coup1R = cplcFeFeAhR(i3,i1)
coup2L = cplcFeFeAhL(i2,i4)
coup2R = cplcFeFeAhR(i2,i4)
coup3L = cplcFeFeAhL(i1,i2)
coup3R = cplcFeFeAhR(i1,i2)
coup4L = cplcFeFeAhL(i4,i3)
coup4R = cplcFeFeAhR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i4)*MFe(i2)*MfFFbFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i3,i1)
coup1R = cplcFeFeAhR(i3,i1)
coup2L = cplcFeFeAhL(i2,i4)
coup2R = cplcFeFeAhR(i2,i4)
coup3L = cplcFeFeAhL(i1,i2)
coup3R = cplcFeFeAhR(i1,i2)
coup4L = cplcFeFeAhL(i4,i3)
coup4R = cplcFeFeAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i4)*MFe(i3)*MfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i3,i1)
coup1R = cplcFeFeAhR(i3,i1)
coup2L = cplcFeFeAhL(i2,i4)
coup2R = cplcFeFeAhR(i2,i4)
coup3L = cplcFeFeAhL(i1,i2)
coup3R = cplcFeFeAhR(i1,i2)
coup4L = cplcFeFeAhL(i4,i3)
coup4R = cplcFeFeAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Fe,bar[Fe],bar[Fe],Fe,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i3,i1)
coup1R = cplcFeFeAhR(i3,i1)
coup2L = cplcFeFeAhL(i2,i4)
coup2R = cplcFeFeAhR(i2,i4)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFe(i1)*MFe(i4)*MFe(i2)*MFe(i3)*MfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i3,i1)
coup1R = cplcFeFeAhR(i3,i1)
coup2L = cplcFeFeAhL(i2,i4)
coup2R = cplcFeFeAhR(i2,i4)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i2)*MFe(i3)*MfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i3,i1)
coup1R = cplcFeFeAhR(i3,i1)
coup2L = cplcFeFeAhL(i2,i4)
coup2R = cplcFeFeAhR(i2,i4)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i4)*MFe(i2)*MfFFbFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i3,i1)
coup1R = cplcFeFeAhR(i3,i1)
coup2L = cplcFeFeAhL(i2,i4)
coup2R = cplcFeFeAhR(i2,i4)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i4)*MFe(i3)*MfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i3,i1)
coup1R = cplcFeFeAhR(i3,i1)
coup2L = cplcFeFeAhL(i2,i4)
coup2R = cplcFeFeAhR(i2,i4)
coup3L = cplcFeFehhL(i1,i2,i5)
coup3R = cplcFeFehhR(i1,i2,i5)
coup4L = cplcFeFehhL(i4,i3,i5)
coup4R = cplcFeFehhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fu,bar[Fd],bar[Fu],Fd,Hp ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass) .and. (MFu(i1) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i3,i1)
coup1R = cplcFuFuAhR(i3,i1)
coup2L = cplcFdFdAhL(i2,i4)
coup2R = cplcFdFdAhR(i2,i4)
coup3L = cplcFuFdHpL(i1,i2)
coup3R = cplcFuFdHpR(i1,i2)
coup4L = cplcFdFucHpL(i4,i3)
coup4R = cplcFdFucHpR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i4)*MFu(i1)*MFd(i2)*MFu(i3)*MfFbFbFbFbS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i3,i1)
coup1R = cplcFuFuAhR(i3,i1)
coup2L = cplcFdFdAhL(i2,i4)
coup2R = cplcFdFdAhR(i2,i4)
coup3L = cplcFuFdHpL(i1,i2)
coup3R = cplcFuFdHpR(i1,i2)
coup4L = cplcFdFucHpL(i4,i3)
coup4R = cplcFdFucHpR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFu(i3)*MfFFbFbFS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i3,i1)
coup1R = cplcFuFuAhR(i3,i1)
coup2L = cplcFdFdAhL(i2,i4)
coup2R = cplcFdFdAhR(i2,i4)
coup3L = cplcFuFdHpL(i1,i2)
coup3R = cplcFuFdHpR(i1,i2)
coup4L = cplcFdFucHpL(i4,i3)
coup4R = cplcFdFucHpR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFd(i2)*MfFFbFFbS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i3,i1)
coup1R = cplcFuFuAhR(i3,i1)
coup2L = cplcFdFdAhL(i2,i4)
coup2R = cplcFdFdAhR(i2,i4)
coup3L = cplcFuFdHpL(i1,i2)
coup3R = cplcFuFdHpR(i1,i2)
coup4L = cplcFdFucHpL(i4,i3)
coup4R = cplcFdFucHpR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i4)*MFu(i3)*MfFFFbFbS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i3,i1)
coup1R = cplcFuFuAhR(i3,i1)
coup2L = cplcFdFdAhL(i2,i4)
coup2R = cplcFdFdAhR(i2,i4)
coup3L = cplcFuFdHpL(i1,i2)
coup3R = cplcFuFdHpR(i1,i2)
coup4L = cplcFdFucHpL(i4,i3)
coup4R = cplcFdFucHpR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFu2(i1),MFd2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Fu,bar[Fu],bar[Fu],Fu,Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i3,i1)
coup1R = cplcFuFuAhR(i3,i1)
coup2L = cplcFuFuAhL(i2,i4)
coup2R = cplcFuFuAhR(i2,i4)
coup3L = cplcFuFuAhL(i1,i2)
coup3R = cplcFuFuAhR(i1,i2)
coup4L = cplcFuFuAhL(i4,i3)
coup4R = cplcFuFuAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i1)*MFu(i4)*MFu(i2)*MFu(i3)*MfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i3,i1)
coup1R = cplcFuFuAhR(i3,i1)
coup2L = cplcFuFuAhL(i2,i4)
coup2R = cplcFuFuAhR(i2,i4)
coup3L = cplcFuFuAhL(i1,i2)
coup3R = cplcFuFuAhR(i1,i2)
coup4L = cplcFuFuAhL(i4,i3)
coup4R = cplcFuFuAhR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i2)*MFu(i3)*MfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i3,i1)
coup1R = cplcFuFuAhR(i3,i1)
coup2L = cplcFuFuAhL(i2,i4)
coup2R = cplcFuFuAhR(i2,i4)
coup3L = cplcFuFuAhL(i1,i2)
coup3R = cplcFuFuAhR(i1,i2)
coup4L = cplcFuFuAhL(i4,i3)
coup4R = cplcFuFuAhR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i2)*MfFFbFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i3,i1)
coup1R = cplcFuFuAhR(i3,i1)
coup2L = cplcFuFuAhL(i2,i4)
coup2R = cplcFuFuAhR(i2,i4)
coup3L = cplcFuFuAhL(i1,i2)
coup3R = cplcFuFuAhR(i1,i2)
coup4L = cplcFuFuAhL(i4,i3)
coup4R = cplcFuFuAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i3)*MfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i3,i1)
coup1R = cplcFuFuAhR(i3,i1)
coup2L = cplcFuFuAhL(i2,i4)
coup2R = cplcFuFuAhR(i2,i4)
coup3L = cplcFuFuAhL(i1,i2)
coup3R = cplcFuFuAhR(i1,i2)
coup4L = cplcFuFuAhL(i4,i3)
coup4R = cplcFuFuAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Fu,bar[Fu],bar[Fu],Fu,hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i3,i1)
coup1R = cplcFuFuAhR(i3,i1)
coup2L = cplcFuFuAhL(i2,i4)
coup2R = cplcFuFuAhR(i2,i4)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i1)*MFu(i4)*MFu(i2)*MFu(i3)*MfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i3,i1)
coup1R = cplcFuFuAhR(i3,i1)
coup2L = cplcFuFuAhL(i2,i4)
coup2R = cplcFuFuAhR(i2,i4)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i2)*MFu(i3)*MfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i3,i1)
coup1R = cplcFuFuAhR(i3,i1)
coup2L = cplcFuFuAhL(i2,i4)
coup2R = cplcFuFuAhR(i2,i4)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i2)*MfFFbFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i3,i1)
coup1R = cplcFuFuAhR(i3,i1)
coup2L = cplcFuFuAhL(i2,i4)
coup2R = cplcFuFuAhR(i2,i4)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i4)*MFu(i3)*MfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i3,i1)
coup1R = cplcFuFuAhR(i3,i1)
coup2L = cplcFuFuAhL(i2,i4)
coup2R = cplcFuFuAhR(i2,i4)
coup3L = cplcFuFuhhL(i1,i2,i5)
coup3R = cplcFuFuhhR(i1,i2,i5)
coup4L = cplcFuFuhhL(i4,i3,i5)
coup4R = cplcFuFuhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fxv,bar[Fxv],bar[Fxv],Fxv,Ah ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i3,i1)
coup1R = cplcFxvFxvAhR(i3,i1)
coup2L = cplcFxvFxvAhL(i2,i4)
coup2R = cplcFxvFxvAhR(i2,i4)
coup3L = cplcFxvFxvAhL(i1,i2)
coup3R = cplcFxvFxvAhR(i1,i2)
coup4L = cplcFxvFxvAhL(i4,i3)
coup4R = cplcFxvFxvAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFxv(i1)*MFxv(i4)*MFxv(i2)*MFxv(i3)*MfFbFbFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i3,i1)
coup1R = cplcFxvFxvAhR(i3,i1)
coup2L = cplcFxvFxvAhL(i2,i4)
coup2R = cplcFxvFxvAhR(i2,i4)
coup3L = cplcFxvFxvAhL(i1,i2)
coup3R = cplcFxvFxvAhR(i1,i2)
coup4L = cplcFxvFxvAhL(i4,i3)
coup4R = cplcFxvFxvAhR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i2)*MFxv(i3)*MfFFbFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i3,i1)
coup1R = cplcFxvFxvAhR(i3,i1)
coup2L = cplcFxvFxvAhL(i2,i4)
coup2R = cplcFxvFxvAhR(i2,i4)
coup3L = cplcFxvFxvAhL(i1,i2)
coup3R = cplcFxvFxvAhR(i1,i2)
coup4L = cplcFxvFxvAhL(i4,i3)
coup4R = cplcFxvFxvAhR(i4,i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i4)*MFxv(i2)*MfFFbFFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i3,i1)
coup1R = cplcFxvFxvAhR(i3,i1)
coup2L = cplcFxvFxvAhL(i2,i4)
coup2R = cplcFxvFxvAhR(i2,i4)
coup3L = cplcFxvFxvAhL(i1,i2)
coup3R = cplcFxvFxvAhR(i1,i2)
coup4L = cplcFxvFxvAhL(i4,i3)
coup4R = cplcFxvFxvAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i4)*MFxv(i3)*MfFFFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i3,i1)
coup1R = cplcFxvFxvAhR(i3,i1)
coup2L = cplcFxvFxvAhL(i2,i4)
coup2R = cplcFxvFxvAhR(i2,i4)
coup3L = cplcFxvFxvAhL(i1,i2)
coup3R = cplcFxvFxvAhR(i1,i2)
coup4L = cplcFxvFxvAhL(i4,i3)
coup4R = cplcFxvFxvAhR(i4,i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfFFFFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Fxv,bar[Fxv],bar[Fxv],Fxv,hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,2
if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i3,i1)
coup1R = cplcFxvFxvAhR(i3,i1)
coup2L = cplcFxvFxvAhL(i2,i4)
coup2R = cplcFxvFxvAhR(i2,i4)
coup3L = cplcFxvFxvhhL(i1,i2,i5)
coup3R = cplcFxvFxvhhR(i1,i2,i5)
coup4L = cplcFxvFxvhhL(i4,i3,i5)
coup4R = cplcFxvFxvhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFxv(i1)*MFxv(i4)*MFxv(i2)*MFxv(i3)*MfFbFbFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i3,i1)
coup1R = cplcFxvFxvAhR(i3,i1)
coup2L = cplcFxvFxvAhL(i2,i4)
coup2R = cplcFxvFxvAhR(i2,i4)
coup3L = cplcFxvFxvhhL(i1,i2,i5)
coup3R = cplcFxvFxvhhR(i1,i2,i5)
coup4L = cplcFxvFxvhhL(i4,i3,i5)
coup4R = cplcFxvFxvhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i2)*MFxv(i3)*MfFFbFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i3,i1)
coup1R = cplcFxvFxvAhR(i3,i1)
coup2L = cplcFxvFxvAhL(i2,i4)
coup2R = cplcFxvFxvAhR(i2,i4)
coup3L = cplcFxvFxvhhL(i1,i2,i5)
coup3R = cplcFxvFxvhhR(i1,i2,i5)
coup4L = cplcFxvFxvhhL(i4,i3,i5)
coup4R = cplcFxvFxvhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i4)*MFxv(i2)*MfFFbFFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i3,i1)
coup1R = cplcFxvFxvAhR(i3,i1)
coup2L = cplcFxvFxvAhL(i2,i4)
coup2R = cplcFxvFxvAhR(i2,i4)
coup3L = cplcFxvFxvhhL(i1,i2,i5)
coup3R = cplcFxvFxvhhR(i1,i2,i5)
coup4L = cplcFxvFxvhhL(i4,i3,i5)
coup4R = cplcFxvFxvhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i4)*MFxv(i3)*MfFFFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i3,i1)
coup1R = cplcFxvFxvAhR(i3,i1)
coup2L = cplcFxvFxvAhL(i2,i4)
coup2R = cplcFxvFxvAhR(i2,i4)
coup3L = cplcFxvFxvhhL(i1,i2,i5)
coup3R = cplcFxvFxvhhR(i1,i2,i5)
coup4L = cplcFxvFxvhhL(i4,i3,i5)
coup4R = cplcFxvFxvhhR(i4,i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*MfFFFFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Topology MoSFSFF
! ---- Ah,Fd,hh,bar[Fd],Fd ----
Do i2=1,3
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass) .and. (MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i3)
coup2L = cplcFdFdAhL(i4,i2)
coup2R = cplcFdFdAhR(i4,i2)
coup3L = cplcFdFdAhL(i2,i5)
coup3R = cplcFdFdAhR(i2,i5)
coup4L = cplcFdFdhhL(i5,i4,i3)
coup4R = cplcFdFdhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFd(i5)*MFd(i4)*MfSFbSFbFb(p2,MAh2,MFd2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i3)
coup2L = cplcFdFdAhL(i4,i2)
coup2R = cplcFdFdAhR(i4,i2)
coup3L = cplcFdFdAhL(i2,i5)
coup3R = cplcFdFdAhR(i2,i5)
coup4L = cplcFdFdhhL(i5,i4,i3)
coup4R = cplcFdFdhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i4)*MfSFSFbF(p2,MAh2,MFd2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i3)
coup2L = cplcFdFdAhL(i4,i2)
coup2R = cplcFdFdAhR(i4,i2)
coup3L = cplcFdFdAhL(i2,i5)
coup3R = cplcFdFdAhR(i2,i5)
coup4L = cplcFdFdhhL(i5,i4,i3)
coup4R = cplcFdFdhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i5)*MfSFSFFb(p2,MAh2,MFd2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Ah,Fe,hh,bar[Fe],Fe ----
Do i2=1,3
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass) .and. (MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i3)
coup2L = cplcFeFeAhL(i4,i2)
coup2R = cplcFeFeAhR(i4,i2)
coup3L = cplcFeFeAhL(i2,i5)
coup3R = cplcFeFeAhR(i2,i5)
coup4L = cplcFeFehhL(i5,i4,i3)
coup4R = cplcFeFehhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i2)*MFe(i5)*MFe(i4)*MfSFbSFbFb(p2,MAh2,MFe2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i3)
coup2L = cplcFeFeAhL(i4,i2)
coup2R = cplcFeFeAhR(i4,i2)
coup3L = cplcFeFeAhL(i2,i5)
coup3R = cplcFeFeAhR(i2,i5)
coup4L = cplcFeFehhL(i5,i4,i3)
coup4R = cplcFeFehhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i4)*MfSFSFbF(p2,MAh2,MFe2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i3)
coup2L = cplcFeFeAhL(i4,i2)
coup2R = cplcFeFeAhR(i4,i2)
coup3L = cplcFeFeAhL(i2,i5)
coup3R = cplcFeFeAhR(i2,i5)
coup4L = cplcFeFehhL(i5,i4,i3)
coup4R = cplcFeFehhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i5)*MfSFSFFb(p2,MAh2,MFe2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Ah,Fu,hh,bar[Fu],Fu ----
Do i2=1,3
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i3)
coup2L = cplcFuFuAhL(i4,i2)
coup2R = cplcFuFuAhR(i4,i2)
coup3L = cplcFuFuAhL(i2,i5)
coup3R = cplcFuFuAhR(i2,i5)
coup4L = cplcFuFuhhL(i5,i4,i3)
coup4R = cplcFuFuhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i2)*MFu(i5)*MFu(i4)*MfSFbSFbFb(p2,MAh2,MFu2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i3)
coup2L = cplcFuFuAhL(i4,i2)
coup2R = cplcFuFuAhR(i4,i2)
coup3L = cplcFuFuAhL(i2,i5)
coup3R = cplcFuFuAhR(i2,i5)
coup4L = cplcFuFuhhL(i5,i4,i3)
coup4R = cplcFuFuhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i4)*MfSFSFbF(p2,MAh2,MFu2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i3)
coup2L = cplcFuFuAhL(i4,i2)
coup2R = cplcFuFuAhR(i4,i2)
coup3L = cplcFuFuAhL(i2,i5)
coup3R = cplcFuFuAhR(i2,i5)
coup4L = cplcFuFuhhL(i5,i4,i3)
coup4R = cplcFuFuhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i5)*MfSFSFFb(p2,MAh2,MFu2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Ah,Fxv,hh,bar[Fxv],Fxv ----
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,2
if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass) .and. (MFxv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i3)
coup2L = cplcFxvFxvAhL(i4,i2)
coup2R = cplcFxvFxvAhR(i4,i2)
coup3L = cplcFxvFxvAhL(i2,i5)
coup3R = cplcFxvFxvAhR(i2,i5)
coup4L = cplcFxvFxvhhL(i5,i4,i3)
coup4R = cplcFxvFxvhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i2)*MFxv(i5)*MFxv(i4)*MfSFbSFbFb(p2,MAh2,MFxv2(i2),Mhh2(i3),MFxv2(i4),MFxv2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i3)
coup2L = cplcFxvFxvAhL(i4,i2)
coup2R = cplcFxvFxvAhR(i4,i2)
coup3L = cplcFxvFxvAhL(i2,i5)
coup3R = cplcFxvFxvAhR(i2,i5)
coup4L = cplcFxvFxvhhL(i5,i4,i3)
coup4R = cplcFxvFxvhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i4)*MfSFSFbF(p2,MAh2,MFxv2(i2),Mhh2(i3),MFxv2(i4),MFxv2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i3)
coup2L = cplcFxvFxvAhL(i4,i2)
coup2R = cplcFxvFxvAhR(i4,i2)
coup3L = cplcFxvFxvAhL(i2,i5)
coup3R = cplcFxvFxvAhR(i2,i5)
coup4L = cplcFxvFxvhhL(i5,i4,i3)
coup4R = cplcFxvFxvhhR(i5,i4,i3)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i5)*MfSFSFFb(p2,MAh2,MFxv2(i2),Mhh2(i3),MFxv2(i4),MFxv2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
end do
end do
! ---- hh,Fd,Ah,bar[Fd],Fd ----
Do i1=1,2
Do i2=1,3
Do i4=1,3
Do i5=1,3
if((MFd(i2) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass) .and. (MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2L = cplcFdFdAhL(i4,i2)
coup2R = cplcFdFdAhR(i4,i2)
coup3L = cplcFdFdhhL(i2,i5,i1)
coup3R = cplcFdFdhhR(i2,i5,i1)
coup4L = cplcFdFdAhL(i5,i4)
coup4R = cplcFdFdAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i2)*MFd(i5)*MFd(i4)*MfSFbSFbFb(p2,Mhh2(i1),MFd2(i2),MAh2,MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2L = cplcFdFdAhL(i4,i2)
coup2R = cplcFdFdAhR(i4,i2)
coup3L = cplcFdFdhhL(i2,i5,i1)
coup3R = cplcFdFdhhR(i2,i5,i1)
coup4L = cplcFdFdAhL(i5,i4)
coup4R = cplcFdFdAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i4)*MfSFSFbF(p2,Mhh2(i1),MFd2(i2),MAh2,MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2L = cplcFdFdAhL(i4,i2)
coup2R = cplcFdFdAhR(i4,i2)
coup3L = cplcFdFdhhL(i2,i5,i1)
coup3R = cplcFdFdhhR(i2,i5,i1)
coup4L = cplcFdFdAhL(i5,i4)
coup4R = cplcFdFdAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i5)*MfSFSFFb(p2,Mhh2(i1),MFd2(i2),MAh2,MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
end do
end do
! ---- hh,Fe,Ah,bar[Fe],Fe ----
Do i1=1,2
Do i2=1,3
Do i4=1,3
Do i5=1,3
if((MFe(i2) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass) .and. (MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2L = cplcFeFeAhL(i4,i2)
coup2R = cplcFeFeAhR(i4,i2)
coup3L = cplcFeFehhL(i2,i5,i1)
coup3R = cplcFeFehhR(i2,i5,i1)
coup4L = cplcFeFeAhL(i5,i4)
coup4R = cplcFeFeAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i2)*MFe(i5)*MFe(i4)*MfSFbSFbFb(p2,Mhh2(i1),MFe2(i2),MAh2,MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2L = cplcFeFeAhL(i4,i2)
coup2R = cplcFeFeAhR(i4,i2)
coup3L = cplcFeFehhL(i2,i5,i1)
coup3R = cplcFeFehhR(i2,i5,i1)
coup4L = cplcFeFeAhL(i5,i4)
coup4R = cplcFeFeAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i4)*MfSFSFbF(p2,Mhh2(i1),MFe2(i2),MAh2,MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2L = cplcFeFeAhL(i4,i2)
coup2R = cplcFeFeAhR(i4,i2)
coup3L = cplcFeFehhL(i2,i5,i1)
coup3R = cplcFeFehhR(i2,i5,i1)
coup4L = cplcFeFeAhL(i5,i4)
coup4R = cplcFeFeAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i5)*MfSFSFFb(p2,Mhh2(i1),MFe2(i2),MAh2,MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
end do
end do
! ---- hh,Fu,Ah,bar[Fu],Fu ----
Do i1=1,2
Do i2=1,3
Do i4=1,3
Do i5=1,3
if((MFu(i2) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2L = cplcFuFuAhL(i4,i2)
coup2R = cplcFuFuAhR(i4,i2)
coup3L = cplcFuFuhhL(i2,i5,i1)
coup3R = cplcFuFuhhR(i2,i5,i1)
coup4L = cplcFuFuAhL(i5,i4)
coup4R = cplcFuFuAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i2)*MFu(i5)*MFu(i4)*MfSFbSFbFb(p2,Mhh2(i1),MFu2(i2),MAh2,MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2L = cplcFuFuAhL(i4,i2)
coup2R = cplcFuFuAhR(i4,i2)
coup3L = cplcFuFuhhL(i2,i5,i1)
coup3R = cplcFuFuhhR(i2,i5,i1)
coup4L = cplcFuFuAhL(i5,i4)
coup4R = cplcFuFuAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i4)*MfSFSFbF(p2,Mhh2(i1),MFu2(i2),MAh2,MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2L = cplcFuFuAhL(i4,i2)
coup2R = cplcFuFuAhR(i4,i2)
coup3L = cplcFuFuhhL(i2,i5,i1)
coup3R = cplcFuFuhhR(i2,i5,i1)
coup4L = cplcFuFuAhL(i5,i4)
coup4R = cplcFuFuAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i5)*MfSFSFFb(p2,Mhh2(i1),MFu2(i2),MAh2,MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
end do
end do
! ---- hh,Fxv,Ah,bar[Fxv],Fxv ----
Do i1=1,2
Do i2=1,2
Do i4=1,2
Do i5=1,2
if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass) .and. (MFxv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2L = cplcFxvFxvAhL(i4,i2)
coup2R = cplcFxvFxvAhR(i4,i2)
coup3L = cplcFxvFxvhhL(i2,i5,i1)
coup3R = cplcFxvFxvhhR(i2,i5,i1)
coup4L = cplcFxvFxvAhL(i5,i4)
coup4R = cplcFxvFxvAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3L*coup4L+coup1*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i2)*MFxv(i5)*MFxv(i4)*MfSFbSFbFb(p2,Mhh2(i1),MFxv2(i2),MAh2,MFxv2(i4),MFxv2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2L = cplcFxvFxvAhL(i4,i2)
coup2R = cplcFxvFxvAhR(i4,i2)
coup3L = cplcFxvFxvhhL(i2,i5,i1)
coup3R = cplcFxvFxvhhR(i2,i5,i1)
coup4L = cplcFxvFxvAhL(i5,i4)
coup4R = cplcFxvFxvAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4L+coup1*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i4)*MfSFSFbF(p2,Mhh2(i1),MFxv2(i2),MAh2,MFxv2(i4),MFxv2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2L = cplcFxvFxvAhL(i4,i2)
coup2R = cplcFxvFxvAhR(i4,i2)
coup3L = cplcFxvFxvhhL(i2,i5,i1)
coup3R = cplcFxvFxvhhR(i2,i5,i1)
coup4L = cplcFxvFxvAhL(i5,i4)
coup4R = cplcFxvFxvAhR(i5,i4)
prefactor=Real(coup1*coup2L*coup3R*coup4R+coup1*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i5)*MfSFSFFb(p2,Mhh2(i1),MFxv2(i2),MAh2,MFxv2(i4),MFxv2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
end do
end do
! ---- Topology VoSSSFF
! ---- Ah,hh,hh,Fd,bar[Fd] ----
Do i2=1,2
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFd(i4) .gt. epsfmass) .and. (MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i3)
coup3L = cplcFdFdhhL(i5,i4,i2)
coup3R = cplcFdFdhhR(i5,i4,i2)
coup4L = cplcFdFdhhL(i4,i5,i3)
coup4R = cplcFdFdhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i4)*MFd(i5)*VfSSSFbFb(p2,MAh2,Mhh2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i3)
coup3L = cplcFdFdhhL(i5,i4,i2)
coup3R = cplcFdFdhhR(i5,i4,i2)
coup4L = cplcFdFdhhL(i4,i5,i3)
coup4R = cplcFdFdhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*VfSSSFF(p2,MAh2,Mhh2(i2),Mhh2(i3),MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,hh,hh,Fe,bar[Fe] ----
Do i2=1,2
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFe(i4) .gt. epsfmass) .and. (MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i3)
coup3L = cplcFeFehhL(i5,i4,i2)
coup3R = cplcFeFehhR(i5,i4,i2)
coup4L = cplcFeFehhL(i4,i5,i3)
coup4R = cplcFeFehhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFe(i4)*MFe(i5)*VfSSSFbFb(p2,MAh2,Mhh2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i3)
coup3L = cplcFeFehhL(i5,i4,i2)
coup3R = cplcFeFehhR(i5,i4,i2)
coup4L = cplcFeFehhL(i4,i5,i3)
coup4R = cplcFeFehhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSFF(p2,MAh2,Mhh2(i2),Mhh2(i3),MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,hh,hh,Fu,bar[Fu] ----
Do i2=1,2
Do i3=1,2
Do i4=1,3
Do i5=1,3
if((MFu(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i3)
coup3L = cplcFuFuhhL(i5,i4,i2)
coup3R = cplcFuFuhhR(i5,i4,i2)
coup4L = cplcFuFuhhL(i4,i5,i3)
coup4R = cplcFuFuhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i4)*MFu(i5)*VfSSSFbFb(p2,MAh2,Mhh2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i3)
coup3L = cplcFuFuhhL(i5,i4,i2)
coup3R = cplcFuFuhhR(i5,i4,i2)
coup4L = cplcFuFuhhL(i4,i5,i3)
coup4R = cplcFuFuhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*VfSSSFF(p2,MAh2,Mhh2(i2),Mhh2(i3),MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Ah,hh,hh,Fxv,bar[Fxv] ----
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,2
if((MFxv(i4) .gt. epsfmass) .and. (MFxv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i3)
coup3L = cplcFxvFxvhhL(i5,i4,i2)
coup3R = cplcFxvFxvhhR(i5,i4,i2)
coup4L = cplcFxvFxvhhL(i4,i5,i3)
coup4R = cplcFxvFxvhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFxv(i4)*MFxv(i5)*VfSSSFbFb(p2,MAh2,Mhh2(i2),Mhh2(i3),MFxv2(i4),MFxv2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i2)
coup2 = cplAhAhhh(i3)
coup3L = cplcFxvFxvhhL(i5,i4,i2)
coup3R = cplcFxvFxvhhR(i5,i4,i2)
coup4L = cplcFxvFxvhhL(i4,i5,i3)
coup4R = cplcFxvFxvhhR(i4,i5,i3)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSFF(p2,MAh2,Mhh2(i2),Mhh2(i3),MFxv2(i4),MFxv2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- hh,Ah,Ah,Fd,bar[Fd] ----
Do i1=1,2
Do i4=1,3
Do i5=1,3
if((MFd(i4) .gt. epsfmass) .and. (MFd(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2 = cplAhAhhh(i1)
coup3L = cplcFdFdAhL(i5,i4)
coup3R = cplcFdFdAhR(i5,i4)
coup4L = cplcFdFdAhL(i4,i5)
coup4R = cplcFdFdAhR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFd(i4)*MFd(i5)*VfSSSFbFb(p2,Mhh2(i1),MAh2,MAh2,MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2 = cplAhAhhh(i1)
coup3L = cplcFdFdAhL(i5,i4)
coup3R = cplcFdFdAhR(i5,i4)
coup4L = cplcFdFdAhL(i4,i5)
coup4R = cplcFdFdAhR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*VfSSSFF(p2,Mhh2(i1),MAh2,MAh2,MFd2(i4),MFd2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- hh,Ah,Ah,Fe,bar[Fe] ----
Do i1=1,2
Do i4=1,3
Do i5=1,3
if((MFe(i4) .gt. epsfmass) .and. (MFe(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2 = cplAhAhhh(i1)
coup3L = cplcFeFeAhL(i5,i4)
coup3R = cplcFeFeAhR(i5,i4)
coup4L = cplcFeFeAhL(i4,i5)
coup4R = cplcFeFeAhR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFe(i4)*MFe(i5)*VfSSSFbFb(p2,Mhh2(i1),MAh2,MAh2,MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2 = cplAhAhhh(i1)
coup3L = cplcFeFeAhL(i5,i4)
coup3R = cplcFeFeAhR(i5,i4)
coup4L = cplcFeFeAhL(i4,i5)
coup4R = cplcFeFeAhR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSFF(p2,Mhh2(i1),MAh2,MAh2,MFe2(i4),MFe2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- hh,Ah,Ah,Fu,bar[Fu] ----
Do i1=1,2
Do i4=1,3
Do i5=1,3
if((MFu(i4) .gt. epsfmass) .and. (MFu(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2 = cplAhAhhh(i1)
coup3L = cplcFuFuAhL(i5,i4)
coup3R = cplcFuFuAhR(i5,i4)
coup4L = cplcFuFuAhL(i4,i5)
coup4R = cplcFuFuAhR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*MFu(i4)*MFu(i5)*VfSSSFbFb(p2,Mhh2(i1),MAh2,MAh2,MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2 = cplAhAhhh(i1)
coup3L = cplcFuFuAhL(i5,i4)
coup3R = cplcFuFuAhR(i5,i4)
coup4L = cplcFuFuAhL(i4,i5)
coup4R = cplcFuFuAhR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 3._dp*VfSSSFF(p2,Mhh2(i1),MAh2,MAh2,MFu2(i4),MFu2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- hh,Ah,Ah,Fxv,bar[Fxv] ----
Do i1=1,2
Do i4=1,2
Do i5=1,2
if((MFxv(i4) .gt. epsfmass) .and. (MFxv(i5) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2 = cplAhAhhh(i1)
coup3L = cplcFxvFxvAhL(i5,i4)
coup3R = cplcFxvFxvAhR(i5,i4)
coup4L = cplcFxvFxvAhL(i4,i5)
coup4R = cplcFxvFxvAhR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4L+coup1*coup2*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= MFxv(i4)*MFxv(i5)*VfSSSFbFb(p2,Mhh2(i1),MAh2,MAh2,MFxv2(i4),MFxv2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1 = cplAhAhhh(i1)
coup2 = cplAhAhhh(i1)
coup3L = cplcFxvFxvAhL(i5,i4)
coup3R = cplcFxvFxvAhR(i5,i4)
coup4L = cplcFxvFxvAhL(i4,i5)
coup4R = cplcFxvFxvAhR(i4,i5)
prefactor=Real(coup1*coup2*coup3L*coup4R+coup1*coup2*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 1._dp*VfSSSFF(p2,Mhh2(i1),MAh2,MAh2,MFxv2(i4),MFxv2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Topology VoFFFFS
! ---- Fd,bar[Fd],Fd,bar[Fd],Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFdFdAhL(i4,i2)
coup3R = cplcFdFdAhR(i4,i2)
coup4L = cplcFdFdAhL(i3,i4)
coup4R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i3)*MFd(i2)*MFd(i4)*VfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFdFdAhL(i4,i2)
coup3R = cplcFdFdAhR(i4,i2)
coup4L = cplcFdFdAhL(i3,i4)
coup4R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i1)*MFd(i3)*VfFbFFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFdFdAhL(i4,i2)
coup3R = cplcFdFdAhR(i4,i2)
coup4L = cplcFdFdAhL(i3,i4)
coup4R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i4)*VfFbFFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFdFdAhL(i4,i2)
coup3R = cplcFdFdAhR(i4,i2)
coup4L = cplcFdFdAhL(i3,i4)
coup4R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFd(i2)*VfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFdFdAhL(i4,i2)
coup3R = cplcFdFdAhR(i4,i2)
coup4L = cplcFdFdAhL(i3,i4)
coup4R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i3)*MFd(i4)*VfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFdFdAhL(i4,i2)
coup3R = cplcFdFdAhR(i4,i2)
coup4L = cplcFdFdAhL(i3,i4)
coup4R = cplcFdFdAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Fd,bar[Fd],Fd,bar[Fd],hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i3)*MFd(i2)*MFd(i4)*VfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i1)*MFd(i3)*VfFbFFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i4)*VfFbFFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFd(i2)*VfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFd(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i3)*MFd(i4)*VfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFdFdhhL(i4,i2,i5)
coup3R = cplcFdFdhhR(i4,i2,i5)
coup4L = cplcFdFdhhL(i3,i4,i5)
coup4R = cplcFdFdhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFd2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fd,bar[Fd],Fd,bar[Fu],Hp ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFuFdHpL(i4,i2)
coup3R = cplcFuFdHpR(i4,i2)
coup4L = cplcFdFucHpL(i3,i4)
coup4R = cplcFdFucHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFd(i3)*MFd(i2)*MFu(i4)*VfFbFbFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFuFdHpL(i4,i2)
coup3R = cplcFuFdHpR(i4,i2)
coup4L = cplcFdFucHpL(i3,i4)
coup4R = cplcFdFucHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i1)*MFd(i3)*VfFbFFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i1) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFuFdHpL(i4,i2)
coup3R = cplcFuFdHpR(i4,i2)
coup4L = cplcFdFucHpL(i3,i4)
coup4R = cplcFdFucHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i1)*MFu(i4)*VfFbFFFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i2) .gt. epsfmass) .and. (MFd(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFuFdHpL(i4,i2)
coup3R = cplcFuFdHpR(i4,i2)
coup4L = cplcFdFucHpL(i3,i4)
coup4R = cplcFdFucHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFd(i3)*MFd(i2)*VfFFbFbFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFuFdHpL(i4,i2)
coup3R = cplcFuFdHpR(i4,i2)
coup4L = cplcFdFucHpL(i3,i4)
coup4R = cplcFdFucHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFd(i3)*MFu(i4)*VfFFFbFbS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i3)
coup2R = cplcFdFdAhR(i1,i3)
coup3L = cplcFuFdHpL(i4,i2)
coup3R = cplcFuFdHpR(i4,i2)
coup4L = cplcFdFucHpL(i3,i4)
coup4R = cplcFdFucHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFd2(i1),MFd2(i2),MFd2(i3),MFu2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fe],Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFeFeAhL(i4,i2)
coup3R = cplcFeFeAhR(i4,i2)
coup4L = cplcFeFeAhL(i3,i4)
coup4R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i3)*MFe(i2)*MFe(i4)*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFeFeAhL(i4,i2)
coup3R = cplcFeFeAhR(i4,i2)
coup4L = cplcFeFeAhL(i3,i4)
coup4R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFeFeAhL(i4,i2)
coup3R = cplcFeFeAhR(i4,i2)
coup4L = cplcFeFeAhL(i3,i4)
coup4R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i4)*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFeFeAhL(i4,i2)
coup3R = cplcFeFeAhR(i4,i2)
coup4L = cplcFeFeAhL(i3,i4)
coup4R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFeFeAhL(i4,i2)
coup3R = cplcFeFeAhR(i4,i2)
coup4L = cplcFeFeAhL(i3,i4)
coup4R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i3)*MFe(i4)*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFeFeAhL(i4,i2)
coup3R = cplcFeFeAhR(i4,i2)
coup4L = cplcFeFeAhL(i3,i4)
coup4R = cplcFeFeAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fe],hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i3)*MFe(i2)*MFe(i4)*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i4)*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFe(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i3)*MFe(i4)*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFeFehhL(i4,i2,i5)
coup3R = cplcFeFehhR(i4,i2,i5)
coup4L = cplcFeFehhL(i3,i4,i5)
coup4R = cplcFeFehhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFe2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fv],Hp ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFvFeHpL(i4,i2)
coup3R = cplcFvFeHpR(i4,i2)
coup4L = cplcFeFvcHpL(i3,i4)
coup4R = cplcFeFvcHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFe(i3)*MFe(i2)*MFv(i4)*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFv2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFvFeHpL(i4,i2)
coup3R = cplcFvFeHpR(i4,i2)
coup4L = cplcFeFvcHpL(i3,i4)
coup4R = cplcFeFvcHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFv2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFvFeHpL(i4,i2)
coup3R = cplcFvFeHpR(i4,i2)
coup4L = cplcFeFvcHpL(i3,i4)
coup4R = cplcFeFvcHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i1)*MFv(i4)*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFv2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFvFeHpL(i4,i2)
coup3R = cplcFvFeHpR(i4,i2)
coup4L = cplcFeFvcHpL(i3,i4)
coup4R = cplcFeFvcHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFv2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFvFeHpL(i4,i2)
coup3R = cplcFvFeHpR(i4,i2)
coup4L = cplcFeFvcHpL(i3,i4)
coup4R = cplcFeFvcHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i3)*MFv(i4)*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFv2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFvFeHpL(i4,i2)
coup3R = cplcFvFeHpR(i4,i2)
coup4L = cplcFeFvcHpL(i3,i4)
coup4R = cplcFeFvcHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFv2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Fe,bar[Fe],Fe,bar[Fxe],Ssc ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i5=1,2
if((MFe(i1) .gt. epsfmass) .and. (MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass) .and. (MFxe .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFxeFeSscL(i2,i5)
coup3R = cplcFxeFeSscR(i2,i5)
coup4L = cplcFeFxecSscL(i3,i5)
coup4R = cplcFeFxecSscR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MFe(i1)*MFe(i3)*MFe(i2)*VfFbFbFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFxe2,MSsc2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFxeFeSscL(i2,i5)
coup3R = cplcFxeFeSscR(i2,i5)
coup4L = cplcFeFxecSscL(i3,i5)
coup4R = cplcFeFxecSscR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFe(i1)*MFe(i3)*VfFbFFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFxe2,MSsc2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i1) .gt. epsfmass) .and. (MFxe .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFxeFeSscL(i2,i5)
coup3R = cplcFxeFeSscR(i2,i5)
coup4L = cplcFeFxecSscL(i3,i5)
coup4R = cplcFeFxecSscR(i3,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MFe(i1)*VfFbFFFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFxe2,MSsc2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i2) .gt. epsfmass) .and. (MFe(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFxeFeSscL(i2,i5)
coup3R = cplcFxeFeSscR(i2,i5)
coup4L = cplcFeFxecSscL(i3,i5)
coup4R = cplcFeFxecSscR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFe(i3)*MFe(i2)*VfFFbFbFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFxe2,MSsc2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFe(i3) .gt. epsfmass) .and. (MFxe .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFxeFeSscL(i2,i5)
coup3R = cplcFxeFeSscR(i2,i5)
coup4L = cplcFeFxecSscL(i3,i5)
coup4R = cplcFeFxecSscR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxe*MFe(i3)*VfFFFbFbS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFxe2,MSsc2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFeFeAhL(i2,i1)
coup1R = cplcFeFeAhR(i2,i1)
coup2L = cplcFeFeAhL(i1,i3)
coup2R = cplcFeFeAhR(i1,i3)
coup3L = cplcFxeFeSscL(i2,i5)
coup3R = cplcFxeFeSscR(i2,i5)
coup4L = cplcFeFxecSscL(i3,i5)
coup4R = cplcFeFxecSscR(i3,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFe2(i1),MFe2(i2),MFe2(i3),MFxe2,MSsc2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Fu,bar[Fu],Fu,bar[Fu],Ah ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFuFuAhL(i4,i2)
coup3R = cplcFuFuAhR(i4,i2)
coup4L = cplcFuFuAhL(i3,i4)
coup4R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i3)*MFu(i2)*MFu(i4)*VfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFuFuAhL(i4,i2)
coup3R = cplcFuFuAhR(i4,i2)
coup4L = cplcFuFuAhL(i3,i4)
coup4R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i1)*MFu(i3)*VfFbFFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFuFuAhL(i4,i2)
coup3R = cplcFuFuAhR(i4,i2)
coup4L = cplcFuFuAhL(i3,i4)
coup4R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i4)*VfFbFFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFuFuAhL(i4,i2)
coup3R = cplcFuFuAhR(i4,i2)
coup4L = cplcFuFuAhL(i3,i4)
coup4R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i3)*MFu(i2)*VfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFuFuAhL(i4,i2)
coup3R = cplcFuFuAhR(i4,i2)
coup4L = cplcFuFuAhL(i3,i4)
coup4R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i3)*MFu(i4)*VfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFuFuAhL(i4,i2)
coup3R = cplcFuFuAhR(i4,i2)
coup4L = cplcFuFuAhL(i3,i4)
coup4R = cplcFuFuAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Fu,bar[Fu],Fu,bar[Fu],hh ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
Do i5=1,2
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i3)*MFu(i2)*MFu(i4)*VfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i1)*MFu(i3)*VfFbFFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i4)*VfFbFFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i3)*MFu(i2)*VfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i3) .gt. epsfmass) .and. (MFu(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i3)*MFu(i4)*VfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFuFuhhL(i4,i2,i5)
coup3R = cplcFuFuhhR(i4,i2,i5)
coup4L = cplcFuFuhhL(i3,i4,i5)
coup4R = cplcFuFuhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFu2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fu,bar[Fu],Fu,bar[Fd],conj[Hp] ----
Do i1=1,3
Do i2=1,3
Do i3=1,3
Do i4=1,3
if((MFd(i4) .gt. epsfmass) .and. (MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFdFucHpL(i4,i2)
coup3R = cplcFdFucHpR(i4,i2)
coup4L = cplcFuFdHpL(i3,i4)
coup4R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFu(i3)*MFd(i4)*MFu(i2)*VfFbFbFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i1) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFdFucHpL(i4,i2)
coup3R = cplcFdFucHpR(i4,i2)
coup4L = cplcFuFdHpL(i3,i4)
coup4R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i1)*MFu(i3)*VfFbFFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass) .and. (MFu(i1) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFdFucHpL(i4,i2)
coup3R = cplcFdFucHpR(i4,i2)
coup4L = cplcFuFdHpL(i3,i4)
coup4R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i1)*MFd(i4)*VfFbFFFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFu(i2) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFdFucHpL(i4,i2)
coup3R = cplcFdFucHpR(i4,i2)
coup4L = cplcFuFdHpL(i3,i4)
coup4R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*MFu(i3)*MFu(i2)*VfFFbFbFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFd(i4) .gt. epsfmass) .and. (MFu(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFdFucHpL(i4,i2)
coup3R = cplcFdFucHpR(i4,i2)
coup4L = cplcFuFdHpL(i3,i4)
coup4R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 12._dp*MFu(i3)*MFd(i4)*VfFFFbFbS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i3)
coup2R = cplcFuFuAhR(i1,i3)
coup3L = cplcFdFucHpL(i4,i2)
coup3R = cplcFdFucHpR(i4,i2)
coup4L = cplcFuFdHpL(i3,i4)
coup4R = cplcFuFdHpR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 6._dp*VfFFFFS(p2,MFu2(i1),MFu2(i2),MFu2(i3),MFd2(i4),MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Fxv,bar[Fxv],Fxv,bar[Fxv],Ah ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFxvAhL(i4,i2)
coup3R = cplcFxvFxvAhR(i4,i2)
coup4L = cplcFxvFxvAhL(i3,i4)
coup4R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i1)*MFxv(i3)*MFxv(i2)*MFxv(i4)*VfFbFbFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFxvAhL(i4,i2)
coup3R = cplcFxvFxvAhR(i4,i2)
coup4L = cplcFxvFxvAhL(i3,i4)
coup4R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i1)*MFxv(i3)*VfFbFFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFxvAhL(i4,i2)
coup3R = cplcFxvFxvAhR(i4,i2)
coup4L = cplcFxvFxvAhL(i3,i4)
coup4R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i1)*MFxv(i4)*VfFbFFFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFxvAhL(i4,i2)
coup3R = cplcFxvFxvAhR(i4,i2)
coup4L = cplcFxvFxvAhL(i3,i4)
coup4R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i3)*MFxv(i2)*VfFFbFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFxvAhL(i4,i2)
coup3R = cplcFxvFxvAhR(i4,i2)
coup4L = cplcFxvFxvAhL(i3,i4)
coup4R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i3)*MFxv(i4)*VfFFFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFxvAhL(i4,i2)
coup3R = cplcFxvFxvAhR(i4,i2)
coup4L = cplcFxvFxvAhL(i3,i4)
coup4R = cplcFxvFxvAhR(i3,i4)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),MAh2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
! ---- Fxv,bar[Fxv],Fxv,bar[Fxv],hh ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,2
Do i5=1,2
if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFxvhhL(i4,i2,i5)
coup3R = cplcFxvFxvhhR(i4,i2,i5)
coup4L = cplcFxvFxvhhL(i3,i4,i5)
coup4R = cplcFxvFxvhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i1)*MFxv(i3)*MFxv(i2)*MFxv(i4)*VfFbFbFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFxvhhL(i4,i2,i5)
coup3R = cplcFxvFxvhhR(i4,i2,i5)
coup4L = cplcFxvFxvhhL(i3,i4,i5)
coup4R = cplcFxvFxvhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i1)*MFxv(i3)*VfFbFFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFxvhhL(i4,i2,i5)
coup3R = cplcFxvFxvhhR(i4,i2,i5)
coup4L = cplcFxvFxvhhL(i3,i4,i5)
coup4R = cplcFxvFxvhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i1)*MFxv(i4)*VfFbFFFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFxvhhL(i4,i2,i5)
coup3R = cplcFxvFxvhhR(i4,i2,i5)
coup4L = cplcFxvFxvhhL(i3,i4,i5)
coup4R = cplcFxvFxvhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i3)*MFxv(i2)*VfFFbFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i3) .gt. epsfmass) .and. (MFxv(i4) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFxvhhL(i4,i2,i5)
coup3R = cplcFxvFxvhhR(i4,i2,i5)
coup4L = cplcFxvFxvhhL(i3,i4,i5)
coup4R = cplcFxvFxvhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i3)*MFxv(i4)*VfFFFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxvFxvhhL(i4,i2,i5)
coup3R = cplcFxvFxvhhR(i4,i2,i5)
coup4L = cplcFxvFxvhhL(i3,i4,i5)
coup4R = cplcFxvFxvhhR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxv2(i4),Mhh2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fxv,bar[Fxv],Fxv,bar[Fv],conj[Ssc] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
Do i4=1,3
Do i5=1,2
if((MFv(i4) .gt. epsfmass) .and. (MFxv(i1) .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFvFxvcSscL(i4,i2,i5)
coup3R = cplcFvFxvcSscR(i4,i2,i5)
coup4L = cplcFxvFvSscL(i3,i4,i5)
coup4R = cplcFxvFvSscR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i1)*MFxv(i3)*MFv(i4)*MFxv(i2)*VfFbFbFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFv2(i4),MSsc2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFvFxvcSscL(i4,i2,i5)
coup3R = cplcFvFxvcSscR(i4,i2,i5)
coup4L = cplcFxvFvSscL(i3,i4,i5)
coup4R = cplcFxvFvSscR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i1)*MFxv(i3)*VfFbFFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFv2(i4),MSsc2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFv(i4) .gt. epsfmass) .and. (MFxv(i1) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFvFxvcSscL(i4,i2,i5)
coup3R = cplcFvFxvcSscR(i4,i2,i5)
coup4L = cplcFxvFvSscL(i3,i4,i5)
coup4R = cplcFxvFvSscR(i3,i4,i5)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i1)*MFv(i4)*VfFbFFFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFv2(i4),MSsc2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFvFxvcSscL(i4,i2,i5)
coup3R = cplcFvFxvcSscR(i4,i2,i5)
coup4L = cplcFxvFvSscL(i3,i4,i5)
coup4R = cplcFxvFvSscR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i3)*MFxv(i2)*VfFFbFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFv2(i4),MSsc2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFv(i4) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFvFxvcSscL(i4,i2,i5)
coup3R = cplcFvFxvcSscR(i4,i2,i5)
coup4L = cplcFxvFvSscL(i3,i4,i5)
coup4R = cplcFxvFvSscR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i3)*MFv(i4)*VfFFFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFv2(i4),MSsc2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFvFxvcSscL(i4,i2,i5)
coup3R = cplcFvFxvcSscR(i4,i2,i5)
coup4L = cplcFxvFvSscL(i3,i4,i5)
coup4R = cplcFxvFvSscR(i3,i4,i5)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFv2(i4),MSsc2(i5),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
end do
end do
! ---- Fxv,bar[Fxv],Fxv,bar[Fxe],conj[Hp] ----
Do i1=1,2
Do i2=1,2
Do i3=1,2
if((MFxe .gt. epsfmass) .and. (MFxv(i1) .gt. epsfmass) .and. (MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
coup4L = cplcFxvFxeHpL(i3)
coup4R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1L*coup2L*coup3L*coup4L+coup1R*coup2R*coup3R*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MFxv(i1)*MFxv(i3)*MFxv(i2)*VfFbFbFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxe2,MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i1) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
coup4L = cplcFxvFxeHpL(i3)
coup4R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4L+coup1R*coup2R*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxv(i1)*MFxv(i3)*VfFbFFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxe2,MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxe .gt. epsfmass) .and. (MFxv(i1) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
coup4L = cplcFxvFxeHpL(i3)
coup4R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1L*coup2L*coup3R*coup4R+coup1R*coup2R*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxe*MFxv(i1)*VfFbFFFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxe2,MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxv(i2) .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
coup4L = cplcFxvFxeHpL(i3)
coup4R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1L*coup2R*coup3L*coup4R+coup1R*coup2L*coup3R*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*MFxv(i3)*MFxv(i2)*VfFFbFbFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxe2,MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

if((MFxe .gt. epsfmass) .and. (MFxv(i3) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
coup4L = cplcFxvFxeHpL(i3)
coup4R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4R+coup1R*coup2L*coup3L*coup4L,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFxe*MFxv(i3)*VfFFFbFbS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxe2,MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFxvFxvAhL(i2,i1)
coup1R = cplcFxvFxvAhR(i2,i1)
coup2L = cplcFxvFxvAhL(i1,i3)
coup2R = cplcFxvFxvAhR(i1,i3)
coup3L = cplcFxeFxvcHpL(i2)
coup3R = cplcFxeFxvcHpR(i2)
coup4L = cplcFxvFxeHpL(i3)
coup4R = cplcFxvFxeHpR(i3)
prefactor=Real(coup1L*coup2R*coup3R*coup4L+coup1R*coup2L*coup3L*coup4R,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 2._dp*VfFFFFS(p2,MFxv2(i1),MFxv2(i2),MFxv2(i3),MFxe2,MHp2,Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end do
end do
end do
! ---- Topology GoFFFFV
! ---- Fd,bar[Fd] ----
Do i1=1,3
Do i2=1,3
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i2)
coup2R = cplcFdFdAhR(i1,i2)
coup3 = g3
coup4 = g3
prefactor=Real(coup1L*coup2R*coup3*coup4+coup1R*coup2L*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*GfFFV(p2,MFd2(i1),MFd2(i2),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
if((MFd(i1) .gt. epsfmass) .and. (MFd(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFdFdAhL(i2,i1)
coup1R = cplcFdFdAhR(i2,i1)
coup2L = cplcFdFdAhL(i1,i2)
coup2R = cplcFdFdAhR(i1,i2)
coup3 = g3
coup4 = g3
prefactor=Real(coup1L*coup2L*coup3*coup4+coup1R*coup2R*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFd(i1)*MFd(i2)*GfFbFbV(p2,MFd2(i1),MFd2(i2),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
! ---- Fu,bar[Fu] ----
Do i1=1,3
Do i2=1,3
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i2)
coup2R = cplcFuFuAhR(i1,i2)
coup3 = g3
coup4 = g3
prefactor=Real(coup1L*coup2R*coup3*coup4+coup1R*coup2L*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*GfFFV(p2,MFu2(i1),MFu2(i2),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
if((MFu(i1) .gt. epsfmass) .and. (MFu(i2) .gt. epsfmass)) then 
nonzerocoupling=.false.
  Do gE1=1,1
   Do gE2=1,1
coup1L = cplcFuFuAhL(i2,i1)
coup1R = cplcFuFuAhR(i2,i1)
coup2L = cplcFuFuAhL(i1,i2)
coup2R = cplcFuFuAhR(i1,i2)
coup3 = g3
coup4 = g3
prefactor=Real(coup1L*coup2L*coup3*coup4+coup1R*coup2R*coup3*coup4,dp)
if(abs(prefactor) .gt. epscouplings) then
 tempcouplingmatrixah(gE1,gE2)=prefactor
 nonzerocoupling=.true.
 else
 tempcouplingmatrixah(gE1,gE2)= 0._dp
 end if
   End Do
  End do
if(nonzerocoupling) then 
 funcvalue= 4._dp*MFu(i1)*MFu(i2)*GfFbFbV(p2,MFu2(i1),MFu2(i2),Qscale)
 tempcontah=tempcontah+tempcouplingmatrixah*funcvalue
end if
end if

end do
end do
Pi2P(1,1)=tempcontah(1,1)*oo16Pi2*oo16Pi2+delta2lmassesah(1,1)
End Subroutine CalculatePi2S
End Module Pole2L_SDdiracDM 
 
