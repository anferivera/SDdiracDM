#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../../CalcHEP_src/include/extern.h"
#include "../../CalcHEP_src/include/VandP.h"
#include "autoprot.h"
extern int  FError;
/*  Special model functions  */

int nModelParticles=23;
static ModelPrtclsStr ModelPrtcls_[23]=
{
  {"h1","h1", 25, "Mh1","Wh1",0,1,0}
, {"h2","h2", 35, "Mh2","Wh2",0,1,0}
, {"~Ssc1","~ssc1", 635, "MSsc1","WSsc1",0,1,0}
, {"~Ssc2","~ssc2", 636, "MSsc2","WSsc2",0,1,0}
, {"g","g", 21, "0","0",2,8,0}
, {"A","A", 22, "0","0",2,1,0}
, {"Z","Z", 23, "MZ","WZ",2,1,0}
, {"Wp","Wm", 24, "MWp","WWp",2,1,3}
, {"~Cha","~cha", 210004, "MCha","WCha",1,1,-3}
, {"d1","D1", 1, "Md1","Wd1",1,3,-1}
, {"d2","D2", 3, "Md2","Wd2",1,3,-1}
, {"d3","D3", 5, "Md3","Wd3",1,3,-1}
, {"u1","U1", 2, "Mu1","Wu1",1,3,2}
, {"u2","U2", 4, "Mu2","Wu2",1,3,2}
, {"u3","U3", 6, "Mu3","Wu3",1,3,2}
, {"e1","E1", 11, "Me1","We1",1,1,-3}
, {"e2","E2", 13, "Me2","We2",1,1,-3}
, {"e3","E3", 15, "Me3","We3",1,1,-3}
, {"nu1","Nu1", 12, "Mnu1","Wnu1",1,1,0}
, {"nu2","Nu2", 14, "Mnu2","Wnu2",1,1,0}
, {"nu3","Nu3", 16, "Mnu3","Wnu3",1,1,0}
, {"~Chi01","~chi01", 210001, "MChi01","WChi01",1,1,0}
, {"~Chi02","~chi02", 210002, "MChi02","WChi02",1,1,0}
};
ModelPrtclsStr *ModelPrtcls=ModelPrtcls_; 
int nModelVars=24;
int nModelFunc=180;
static char*varNames_[204]={
 "WSsc1","WSsc2","WZ","WWp","Wd1","Wd2","Wd3","Wu1","Wu2","Wu3"
,"We1","We2","We3","Wnu1","Wnu2","Wnu3","Maux","sqrt2","Pi","Q"
,"alfSMZ","aS","aEWinv","Gf","rd","Mh1","Mh2","MSsc1","MSsc2","MZ"
,"MCha","Md1","Md2","Md3","Mu1","Mu2","Mu3","Me1","Me2","Me3"
,"Mnu1","Mnu2","Mnu3","MChi01","MChi02","YRD","LS1H","LS","LS2H","LSP"
,"LSPH","YRB11","YRB12","YRB13","YRB21","YRB22","YRB23","YRC","YRA11","YRA12"
,"YRA13","YRA21","YRA22","YRA23","YNU11","YNU12","YNU13","YNU21","YNU22","YNU23"
,"YNU31","YNU32","YNU33","vS","ZH11","ZH12","ZH21","ZH22","ZDL11","ZDL12"
,"ZDL13","ZDL21","ZDL22","ZDL23","ZDL31","ZDL32","ZDL33","ZDR11","ZDR12","ZDR13"
,"ZDR21","ZDR22","ZDR23","ZDR31","ZDR32","ZDR33","ZUL11","ZUL12","ZUL13","ZUL21"
,"ZUL22","ZUL23","ZUL31","ZUL32","ZUL33","ZUR11","ZUR12","ZUR13","ZUR21","ZUR22"
,"ZUR23","ZUR31","ZUR32","ZUR33","ZEL11","ZEL12","ZEL13","ZEL21","ZEL22","ZEL23"
,"ZEL31","ZEL32","ZEL33","ZER11","ZER12","ZER13","ZER21","ZER22","ZER23","ZER31"
,"ZER32","ZER33","UV11","UV12","UV13","UV21","UV22","UV23","UV31","UV32"
,"UV33","UVR11","UVR12","UVR13","UVR21","UVR22","UVR23","UVR31","UVR32","UVR33"
,"XV11","XV12","XV21","XV22","XU11","XU12","XU21","XU22","VSs11","VSs12"
,"VSs21","VSs22","HPP1","HGG1","HPP2","HGG2","QCDok","g3","el","MWp"
,"TW","STW","CTW","TTW","g1","g2","vvSM","Yd11","Yd12","Yd13"
,"Yd21","Yd22","Yd23","Yd31","Yd32","Yd33","Ye11","Ye12","Ye13","Ye21"
,"Ye22","Ye23","Ye31","Ye32","Ye33","Yu11","Yu12","Yu13","Yu21","Yu22"
,"Yu23","Yu31","Yu32","Yu33"};
char**varNames=varNames_;
static REAL varValues_[204]={
   0.000000E+00,  0.000000E+00,  2.495200E+00,  2.141000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00,  1.510000E+00
,  0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00,  0.000000E+00,  1.000000E+00,  1.414214E+00,  3.141593E+00,  1.000000E+02
,  1.172000E-01,  1.190000E-01,  1.370360E+02,  1.166390E-05};
REAL*varValues=varValues_;
int calcMainFunc(void)
{
   int i;
   static REAL * VV=NULL;
   static int iQ=-1;
   static int cErr=1;
   REAL *V=varValues;
   FError=0;
   if(VV && cErr==0)
   { for(i=0;i<nModelVars;i++) if(i!=iQ && VV[i]!=V[i]) break;
     if(i==nModelVars)      {if(iQ>=0 && VV[iQ]!=V[iQ]) goto FirstQ; else return 0;} 
   }
  cErr=1;
   V[24]=slhaRead("SPheno.spc.SDdiracDM",0);
   if(!isfinite(V[24]) || FError) return 24;
 FirstQ:
 cErr=1;
   V[25]=slhaVal("MASS",V[19],1,25);
   if(!isfinite(V[25]) || FError) return 25;
   V[26]=slhaVal("MASS",V[19],1,35);
   if(!isfinite(V[26]) || FError) return 26;
   V[27]=slhaVal("MASS",V[19],1,635);
   if(!isfinite(V[27]) || FError) return 27;
   V[28]=slhaVal("MASS",V[19],1,636);
   if(!isfinite(V[28]) || FError) return 28;
   V[29]=slhaVal("MASS",V[19],1,23);
   if(!isfinite(V[29]) || FError) return 29;
   V[30]=slhaVal("MASS",V[19],1,210004);
   if(!isfinite(V[30]) || FError) return 30;
   V[31]=slhaVal("MASS",V[19],1,1);
   if(!isfinite(V[31]) || FError) return 31;
   V[32]=slhaVal("MASS",V[19],1,3);
   if(!isfinite(V[32]) || FError) return 32;
   V[33]=slhaVal("MASS",V[19],1,5);
   if(!isfinite(V[33]) || FError) return 33;
   V[34]=slhaVal("MASS",V[19],1,2);
   if(!isfinite(V[34]) || FError) return 34;
   V[35]=slhaVal("MASS",V[19],1,4);
   if(!isfinite(V[35]) || FError) return 35;
   V[36]=slhaVal("MASS",V[19],1,6);
   if(!isfinite(V[36]) || FError) return 36;
   V[37]=slhaVal("MASS",V[19],1,11);
   if(!isfinite(V[37]) || FError) return 37;
   V[38]=slhaVal("MASS",V[19],1,13);
   if(!isfinite(V[38]) || FError) return 38;
   V[39]=slhaVal("MASS",V[19],1,15);
   if(!isfinite(V[39]) || FError) return 39;
   V[40]=slhaVal("MASS",V[19],1,12);
   if(!isfinite(V[40]) || FError) return 40;
   V[41]=slhaVal("MASS",V[19],1,14);
   if(!isfinite(V[41]) || FError) return 41;
   V[42]=slhaVal("MASS",V[19],1,16);
   if(!isfinite(V[42]) || FError) return 42;
   V[43]=slhaVal("MASS",V[19],1,210001);
   if(!isfinite(V[43]) || FError) return 43;
   V[44]=slhaVal("MASS",V[19],1,210002);
   if(!isfinite(V[44]) || FError) return 44;
   V[45]=slhaVal("YRD",V[19],1,1);
   if(!isfinite(V[45]) || FError) return 45;
   V[46]=slhaVal("LS1H",V[19],1,1);
   if(!isfinite(V[46]) || FError) return 46;
   V[47]=slhaVal("LS",V[19],1,1);
   if(!isfinite(V[47]) || FError) return 47;
   V[48]=slhaVal("LS2H",V[19],1,1);
   if(!isfinite(V[48]) || FError) return 48;
   V[49]=slhaVal("LSP",V[19],1,1);
   if(!isfinite(V[49]) || FError) return 49;
   V[50]=slhaVal("LSPH",V[19],1,1);
   if(!isfinite(V[50]) || FError) return 50;
   V[51]=slhaVal("YRB1",V[19],1,1);
   if(!isfinite(V[51]) || FError) return 51;
   V[52]=slhaVal("YRB1",V[19],1,2);
   if(!isfinite(V[52]) || FError) return 52;
   V[53]=slhaVal("YRB1",V[19],1,3);
   if(!isfinite(V[53]) || FError) return 53;
   V[54]=slhaVal("YRB2",V[19],1,1);
   if(!isfinite(V[54]) || FError) return 54;
   V[55]=slhaVal("YRB2",V[19],1,2);
   if(!isfinite(V[55]) || FError) return 55;
   V[56]=slhaVal("YRB2",V[19],1,3);
   if(!isfinite(V[56]) || FError) return 56;
   V[57]=slhaVal("YRC",V[19],1,1);
   if(!isfinite(V[57]) || FError) return 57;
   V[58]=slhaVal("YRA1",V[19],1,1);
   if(!isfinite(V[58]) || FError) return 58;
   V[59]=slhaVal("YRA1",V[19],1,2);
   if(!isfinite(V[59]) || FError) return 59;
   V[60]=slhaVal("YRA1",V[19],1,3);
   if(!isfinite(V[60]) || FError) return 60;
   V[61]=slhaVal("YRA2",V[19],1,1);
   if(!isfinite(V[61]) || FError) return 61;
   V[62]=slhaVal("YRA2",V[19],1,2);
   if(!isfinite(V[62]) || FError) return 62;
   V[63]=slhaVal("YRA2",V[19],1,3);
   if(!isfinite(V[63]) || FError) return 63;
   V[64]=slhaVal("YNU",V[19],2,1,1);
   if(!isfinite(V[64]) || FError) return 64;
   V[65]=slhaVal("YNU",V[19],2,1,2);
   if(!isfinite(V[65]) || FError) return 65;
   V[66]=slhaVal("YNU",V[19],2,1,3);
   if(!isfinite(V[66]) || FError) return 66;
   V[67]=slhaVal("YNU",V[19],2,2,1);
   if(!isfinite(V[67]) || FError) return 67;
   V[68]=slhaVal("YNU",V[19],2,2,2);
   if(!isfinite(V[68]) || FError) return 68;
   V[69]=slhaVal("YNU",V[19],2,2,3);
   if(!isfinite(V[69]) || FError) return 69;
   V[70]=slhaVal("YNU",V[19],2,3,1);
   if(!isfinite(V[70]) || FError) return 70;
   V[71]=slhaVal("YNU",V[19],2,3,2);
   if(!isfinite(V[71]) || FError) return 71;
   V[72]=slhaVal("YNU",V[19],2,3,3);
   if(!isfinite(V[72]) || FError) return 72;
   V[73]=slhaVal("VS",V[19],1,1);
   if(!isfinite(V[73]) || FError) return 73;
   V[74]=slhaVal("SCALARMIX",V[19],2,1,1);
   if(!isfinite(V[74]) || FError) return 74;
   V[75]=slhaVal("SCALARMIX",V[19],2,1,2);
   if(!isfinite(V[75]) || FError) return 75;
   V[76]=slhaVal("SCALARMIX",V[19],2,2,1);
   if(!isfinite(V[76]) || FError) return 76;
   V[77]=slhaVal("SCALARMIX",V[19],2,2,2);
   if(!isfinite(V[77]) || FError) return 77;
   V[78]=slhaVal("UDLMIX",V[19],2,1,1);
   if(!isfinite(V[78]) || FError) return 78;
   V[79]=slhaVal("UDLMIX",V[19],2,1,2);
   if(!isfinite(V[79]) || FError) return 79;
   V[80]=slhaVal("UDLMIX",V[19],2,1,3);
   if(!isfinite(V[80]) || FError) return 80;
   V[81]=slhaVal("UDLMIX",V[19],2,2,1);
   if(!isfinite(V[81]) || FError) return 81;
   V[82]=slhaVal("UDLMIX",V[19],2,2,2);
   if(!isfinite(V[82]) || FError) return 82;
   V[83]=slhaVal("UDLMIX",V[19],2,2,3);
   if(!isfinite(V[83]) || FError) return 83;
   V[84]=slhaVal("UDLMIX",V[19],2,3,1);
   if(!isfinite(V[84]) || FError) return 84;
   V[85]=slhaVal("UDLMIX",V[19],2,3,2);
   if(!isfinite(V[85]) || FError) return 85;
   V[86]=slhaVal("UDLMIX",V[19],2,3,3);
   if(!isfinite(V[86]) || FError) return 86;
   V[87]=slhaVal("UDRMIX",V[19],2,1,1);
   if(!isfinite(V[87]) || FError) return 87;
   V[88]=slhaVal("UDRMIX",V[19],2,1,2);
   if(!isfinite(V[88]) || FError) return 88;
   V[89]=slhaVal("UDRMIX",V[19],2,1,3);
   if(!isfinite(V[89]) || FError) return 89;
   V[90]=slhaVal("UDRMIX",V[19],2,2,1);
   if(!isfinite(V[90]) || FError) return 90;
   V[91]=slhaVal("UDRMIX",V[19],2,2,2);
   if(!isfinite(V[91]) || FError) return 91;
   V[92]=slhaVal("UDRMIX",V[19],2,2,3);
   if(!isfinite(V[92]) || FError) return 92;
   V[93]=slhaVal("UDRMIX",V[19],2,3,1);
   if(!isfinite(V[93]) || FError) return 93;
   V[94]=slhaVal("UDRMIX",V[19],2,3,2);
   if(!isfinite(V[94]) || FError) return 94;
   V[95]=slhaVal("UDRMIX",V[19],2,3,3);
   if(!isfinite(V[95]) || FError) return 95;
   V[96]=slhaVal("UULMIX",V[19],2,1,1);
   if(!isfinite(V[96]) || FError) return 96;
   V[97]=slhaVal("UULMIX",V[19],2,1,2);
   if(!isfinite(V[97]) || FError) return 97;
   V[98]=slhaVal("UULMIX",V[19],2,1,3);
   if(!isfinite(V[98]) || FError) return 98;
   V[99]=slhaVal("UULMIX",V[19],2,2,1);
   if(!isfinite(V[99]) || FError) return 99;
   V[100]=slhaVal("UULMIX",V[19],2,2,2);
   if(!isfinite(V[100]) || FError) return 100;
   V[101]=slhaVal("UULMIX",V[19],2,2,3);
   if(!isfinite(V[101]) || FError) return 101;
   V[102]=slhaVal("UULMIX",V[19],2,3,1);
   if(!isfinite(V[102]) || FError) return 102;
   V[103]=slhaVal("UULMIX",V[19],2,3,2);
   if(!isfinite(V[103]) || FError) return 103;
   V[104]=slhaVal("UULMIX",V[19],2,3,3);
   if(!isfinite(V[104]) || FError) return 104;
   V[105]=slhaVal("UURMIX",V[19],2,1,1);
   if(!isfinite(V[105]) || FError) return 105;
   V[106]=slhaVal("UURMIX",V[19],2,1,2);
   if(!isfinite(V[106]) || FError) return 106;
   V[107]=slhaVal("UURMIX",V[19],2,1,3);
   if(!isfinite(V[107]) || FError) return 107;
   V[108]=slhaVal("UURMIX",V[19],2,2,1);
   if(!isfinite(V[108]) || FError) return 108;
   V[109]=slhaVal("UURMIX",V[19],2,2,2);
   if(!isfinite(V[109]) || FError) return 109;
   V[110]=slhaVal("UURMIX",V[19],2,2,3);
   if(!isfinite(V[110]) || FError) return 110;
   V[111]=slhaVal("UURMIX",V[19],2,3,1);
   if(!isfinite(V[111]) || FError) return 111;
   V[112]=slhaVal("UURMIX",V[19],2,3,2);
   if(!isfinite(V[112]) || FError) return 112;
   V[113]=slhaVal("UURMIX",V[19],2,3,3);
   if(!isfinite(V[113]) || FError) return 113;
   V[114]=slhaVal("UELMIX",V[19],2,1,1);
   if(!isfinite(V[114]) || FError) return 114;
   V[115]=slhaVal("UELMIX",V[19],2,1,2);
   if(!isfinite(V[115]) || FError) return 115;
   V[116]=slhaVal("UELMIX",V[19],2,1,3);
   if(!isfinite(V[116]) || FError) return 116;
   V[117]=slhaVal("UELMIX",V[19],2,2,1);
   if(!isfinite(V[117]) || FError) return 117;
   V[118]=slhaVal("UELMIX",V[19],2,2,2);
   if(!isfinite(V[118]) || FError) return 118;
   V[119]=slhaVal("UELMIX",V[19],2,2,3);
   if(!isfinite(V[119]) || FError) return 119;
   V[120]=slhaVal("UELMIX",V[19],2,3,1);
   if(!isfinite(V[120]) || FError) return 120;
   V[121]=slhaVal("UELMIX",V[19],2,3,2);
   if(!isfinite(V[121]) || FError) return 121;
   V[122]=slhaVal("UELMIX",V[19],2,3,3);
   if(!isfinite(V[122]) || FError) return 122;
   V[123]=slhaVal("UERMIX",V[19],2,1,1);
   if(!isfinite(V[123]) || FError) return 123;
   V[124]=slhaVal("UERMIX",V[19],2,1,2);
   if(!isfinite(V[124]) || FError) return 124;
   V[125]=slhaVal("UERMIX",V[19],2,1,3);
   if(!isfinite(V[125]) || FError) return 125;
   V[126]=slhaVal("UERMIX",V[19],2,2,1);
   if(!isfinite(V[126]) || FError) return 126;
   V[127]=slhaVal("UERMIX",V[19],2,2,2);
   if(!isfinite(V[127]) || FError) return 127;
   V[128]=slhaVal("UERMIX",V[19],2,2,3);
   if(!isfinite(V[128]) || FError) return 128;
   V[129]=slhaVal("UERMIX",V[19],2,3,1);
   if(!isfinite(V[129]) || FError) return 129;
   V[130]=slhaVal("UERMIX",V[19],2,3,2);
   if(!isfinite(V[130]) || FError) return 130;
   V[131]=slhaVal("UERMIX",V[19],2,3,3);
   if(!isfinite(V[131]) || FError) return 131;
   V[132]=slhaVal("UVMIX",V[19],2,1,1);
   if(!isfinite(V[132]) || FError) return 132;
   V[133]=slhaVal("UVMIX",V[19],2,1,2);
   if(!isfinite(V[133]) || FError) return 133;
   V[134]=slhaVal("UVMIX",V[19],2,1,3);
   if(!isfinite(V[134]) || FError) return 134;
   V[135]=slhaVal("UVMIX",V[19],2,2,1);
   if(!isfinite(V[135]) || FError) return 135;
   V[136]=slhaVal("UVMIX",V[19],2,2,2);
   if(!isfinite(V[136]) || FError) return 136;
   V[137]=slhaVal("UVMIX",V[19],2,2,3);
   if(!isfinite(V[137]) || FError) return 137;
   V[138]=slhaVal("UVMIX",V[19],2,3,1);
   if(!isfinite(V[138]) || FError) return 138;
   V[139]=slhaVal("UVMIX",V[19],2,3,2);
   if(!isfinite(V[139]) || FError) return 139;
   V[140]=slhaVal("UVMIX",V[19],2,3,3);
   if(!isfinite(V[140]) || FError) return 140;
   V[141]=slhaVal("UVRMIX",V[19],2,1,1);
   if(!isfinite(V[141]) || FError) return 141;
   V[142]=slhaVal("UVRMIX",V[19],2,1,2);
   if(!isfinite(V[142]) || FError) return 142;
   V[143]=slhaVal("UVRMIX",V[19],2,1,3);
   if(!isfinite(V[143]) || FError) return 143;
   V[144]=slhaVal("UVRMIX",V[19],2,2,1);
   if(!isfinite(V[144]) || FError) return 144;
   V[145]=slhaVal("UVRMIX",V[19],2,2,2);
   if(!isfinite(V[145]) || FError) return 145;
   V[146]=slhaVal("UVRMIX",V[19],2,2,3);
   if(!isfinite(V[146]) || FError) return 146;
   V[147]=slhaVal("UVRMIX",V[19],2,3,1);
   if(!isfinite(V[147]) || FError) return 147;
   V[148]=slhaVal("UVRMIX",V[19],2,3,2);
   if(!isfinite(V[148]) || FError) return 148;
   V[149]=slhaVal("UVRMIX",V[19],2,3,3);
   if(!isfinite(V[149]) || FError) return 149;
   V[150]=slhaVal("XV",V[19],2,1,1);
   if(!isfinite(V[150]) || FError) return 150;
   V[151]=slhaVal("XV",V[19],2,1,2);
   if(!isfinite(V[151]) || FError) return 151;
   V[152]=slhaVal("XV",V[19],2,2,1);
   if(!isfinite(V[152]) || FError) return 152;
   V[153]=slhaVal("XV",V[19],2,2,2);
   if(!isfinite(V[153]) || FError) return 153;
   V[154]=slhaVal("XU",V[19],2,1,1);
   if(!isfinite(V[154]) || FError) return 154;
   V[155]=slhaVal("XU",V[19],2,1,2);
   if(!isfinite(V[155]) || FError) return 155;
   V[156]=slhaVal("XU",V[19],2,2,1);
   if(!isfinite(V[156]) || FError) return 156;
   V[157]=slhaVal("XU",V[19],2,2,2);
   if(!isfinite(V[157]) || FError) return 157;
   V[158]=slhaVal("VSS",V[19],2,1,1);
   if(!isfinite(V[158]) || FError) return 158;
   V[159]=slhaVal("VSS",V[19],2,1,2);
   if(!isfinite(V[159]) || FError) return 159;
   V[160]=slhaVal("VSS",V[19],2,2,1);
   if(!isfinite(V[160]) || FError) return 160;
   V[161]=slhaVal("VSS",V[19],2,2,2);
   if(!isfinite(V[161]) || FError) return 161;
   V[162]=slhaVal("EFFHIGGSCOUPLINGS",V[19],3,25,22,22);
   if(!isfinite(V[162]) || FError) return 162;
   V[163]=slhaVal("EFFHIGGSCOUPLINGS",V[19],3,25,21,21);
   if(!isfinite(V[163]) || FError) return 163;
   V[164]=slhaVal("EFFHIGGSCOUPLINGS",V[19],3,35,22,22);
   if(!isfinite(V[164]) || FError) return 164;
   V[165]=slhaVal("EFFHIGGSCOUPLINGS",V[19],3,35,21,21);
   if(!isfinite(V[165]) || FError) return 165;
   V[166]=initQCD(V[20],V[35],V[33],V[36]);
   if(!isfinite(V[166]) || FError) return 166;
   V[167]=sqrt(alphaQCD(V[19])*4*3.1415927)*1;
   if(!isfinite(V[167]) || FError) return 167;
   V[168]=2*sqrt(1/(V[22]))*sqrt(V[18]);
   if(!isfinite(V[168]) || FError) return 168;
   V[169]=sqrt(pow(V[29],2)/(2.)+sqrt(pow(V[29],4)/(4.)-pow(V[29],2)*V[18]/(V[17]*V[22]*V[23])));
   if(!isfinite(V[169]) || FError) return 169;
   V[170]=asin(sqrt(1-pow(V[169],2)/(pow(V[29],2))));
   if(!isfinite(V[170]) || FError) return 170;
   V[171]=sin(V[170]);
   if(!isfinite(V[171]) || FError) return 171;
   V[172]=cos(V[170]);
   if(!isfinite(V[172]) || FError) return 172;
   V[173]=tan(V[170]);
   if(!isfinite(V[173]) || FError) return 173;
   V[174]=V[168]*1/(cos(V[170]));
   if(!isfinite(V[174]) || FError) return 174;
   V[175]=V[168]*1/(sin(V[170]));
   if(!isfinite(V[175]) || FError) return 175;
   V[176]=2*sqrt(pow(V[169],2)/(pow(V[175],2)));
   if(!isfinite(V[176]) || FError) return 176;
   V[177]=V[17]*V[31]/(V[176]);
   if(!isfinite(V[177]) || FError) return 177;
   V[178]=0;

   V[179]=0;

   V[180]=0;

   V[181]=V[17]*V[32]/(V[176]);
   if(!isfinite(V[181]) || FError) return 181;
   V[182]=0;

   V[183]=0;

   V[184]=0;

   V[185]=V[17]*MbEff(V[19])*1/(V[176]);
   if(!isfinite(V[185]) || FError) return 185;
   V[186]=V[17]*V[37]/(V[176]);
   if(!isfinite(V[186]) || FError) return 186;
   V[187]=0;

   V[188]=0;

   V[189]=0;

   V[190]=V[17]*V[38]/(V[176]);
   if(!isfinite(V[190]) || FError) return 190;
   V[191]=0;

   V[192]=0;

   V[193]=0;

   V[194]=V[17]*V[39]/(V[176]);
   if(!isfinite(V[194]) || FError) return 194;
   V[195]=V[17]*V[34]/(V[176]);
   if(!isfinite(V[195]) || FError) return 195;
   V[196]=0;

   V[197]=0;

   V[198]=0;

   V[199]=V[17]*McEff(V[19])*1/(V[176]);
   if(!isfinite(V[199]) || FError) return 199;
   V[200]=0;

   V[201]=0;

   V[202]=0;

   V[203]=V[17]*MtEff(V[19])*1/(V[176]);
   if(!isfinite(V[203]) || FError) return 203;
   if(VV==NULL) 
   {  VV=malloc(sizeof(REAL)*nModelVars);
      for(i=0;i<nModelVars;i++) if(strcmp(varNames[i],"Q")==0) iQ=i;
   }
   for(i=0;i<nModelVars;i++) VV[i]=V[i];
   cErr=0;
   return 0;
}
