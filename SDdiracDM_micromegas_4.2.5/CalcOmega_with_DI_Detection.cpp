#include "../sources/micromegas.h"
#include"../sources/micromegas_aux.h"
#include "lib/pmodel.h"
#include <string>

using namespace std;

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* MAIN PROGRAM (by F.Staub, last change 28.07.2017)			     		    */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

int main(int argc, char** argv)
{  
		int err, i;
	   	char lspname[10], nlspname[10];
		double Omega=-1, Xf=-1;
		double w;
		double cut = 0.01;		// cut-off for channel output								
		int fast = 1;			/* 0 = best accuracy, 1 = "fast option" accuracy ~1% 	     */
 		double Beps = 1.E-5;  		/* Criteqrium for including co-annihilations (1 = no coann.) */
 		double vsigmaefectiva;
 		VZdecay=0; VWdecay=0; cleanDecayTable();
		ForceUG=1; 		
			err = sortOddParticles(lspname);	
			printMasses(stdout,1);				
	 		Omega = darkOmega(&Xf,fast,Beps);
			printf("\n");
			
			/* Returns sigma*v effective in cm^3/sec.*/
			//vsigmaefectiva=vSigma(Mcdm/Xf,Beps,fast,NULL)*2.98962*1E-26; 
			vsigmaefectiva=vSigma(Mcdm/Xf,Beps,fast)*2.98962*1E-26; 
			printf("vsigmaeff = %.3e\n",vsigmaefectiva);
			
			printf("Xf=%.2e Omega h^2=%.2e\n",Xf,Omega);
//   			printChannels(Xf,cut,Beps,1,stdout);
			printf("\n");
			printChannels(Xf,cut,Beps,1,stdout);
			FILE *omega = fopen("omg.out","w");
			fprintf(omega,"%i %6.6lf # relic density \n",1,Omega);
			w = 1.;
			i = 0;
			while (w>cut) 
			{
			    fprintf(omega,"%i %6.6lf # %s %s -> %s %s\n",100+i,omegaCh[i].weight,omegaCh[i].prtcl[0],omegaCh[i].prtcl[1],omegaCh[i].prtcl[2],omegaCh[i].prtcl[3]);
			    i++;
			    w = omegaCh[i].weight;
			}
				FILE *channels = fopen("channels.out","w");
			w = 1.;
			i = 0;
			while (w>cut) 
			{
			fprintf(channels,"%li %li %li %li %6.6lf # %s %s -> %s %s\n",pNum(omegaCh[i].prtcl[0]),pNum(omegaCh[i].prtcl[1]),pNum(omegaCh[i].prtcl[2]),pNum(omegaCh[i].prtcl[3]),omegaCh[i].weight,omegaCh[i].prtcl[0],omegaCh[i].prtcl[1],omegaCh[i].prtcl[2],omegaCh[i].prtcl[3]);
			    i++;
			    w = omegaCh[i].weight;
			}




{ double pA0[2],pA5[2],nA0[2],nA5[2];
  double Nmass=0.939; /*nucleon mass*/
  double SCcoeff;  
  
printf("\n======== Direct Detection ========\n");

printf("\n==== Calculation of CDM-nucleons amplitudes  =====\n");   
printf("         TREE LEVEL\n");

    nucleonAmplitudes(CDM1, pA0,pA5,nA0,nA5);
    printf("CDM-nucleon micrOMEGAs amplitudes:\n");
    printf("proton:  SI  %.3E  SD  %.3E\n",pA0[0],pA5[0]);
    printf("neutron: SI  %.3E  SD  %.3E\n",nA0[0],nA5[0]); 


printf("         BOX DIAGRAMS\n");  

   
    nucleonAmplitudes(CDM1,  pA0,pA5,nA0,nA5);
    printf("CDM-nucleon micrOMEGAs amplitudes:\n");
    printf("proton:  SI  %.3E  SD  %.3E\n",pA0[0],pA5[0]);
    printf("neutron: SI  %.3E  SD  %.3E\n",nA0[0],nA5[0]); 

  SCcoeff=4/M_PI*3.8937966E8*pow(Nmass*Mcdm/(Nmass+ Mcdm),2.);
    printf("CDM-nucleon cross sections[pb]:\n");
    printf(" proton  SI %.3E  SD %.3E\n",SCcoeff*pA0[0]*pA0[0],3*SCcoeff*pA5[0]*pA5[0]);
    printf(" neutron SI %.3E  SD %.3E\n",SCcoeff*nA0[0]*nA0[0],3*SCcoeff*nA5[0]*nA5[0]);
    
    
    fprintf(omega,"201 %6.15lf #\n",SCcoeff*pA0[0]*pA0[0]);
    fprintf(omega,"202 %6.15lf #\n",3*SCcoeff*pA5[0]*pA5[0]);
    fprintf(omega,"203 %6.15lf #\n",SCcoeff*nA0[0]*nA0[0]);
    fprintf(omega,"204 %6.15lf # \n",3*SCcoeff*nA5[0]*nA5[0]);
    
    
}
  
{ 
  int err,i;
  double Emin=1,/* Energy cut  in GeV   */  sigmaV;
  double vcs_gz,vcs_gg;
  char txt[100];
  double SpA[NZ],SpE[NZ],SpP[NZ];
  double FluxA[NZ],FluxE[NZ],FluxP[NZ];
  double * SpNe=NULL,*SpNm=NULL,*SpNl=NULL;
  double Etest=Mcdm/2;
  
printf("\n==== Indirect detection =======\n");  

  sigmaV=calcSpectrum(1+2+4,SpA,SpE,SpP,SpNe,SpNm,SpNl ,&err);
    /* Returns sigma*v in cm^3/sec.     SpX - calculated spectra of annihilation.
       Use SpectdNdE(E, SpX) to calculate energy distribution in  1/GeV units.
       
       First parameter 1-includes W/Z polarization
                       2-includes gammas for 2->2+gamma
                       4-print cross sections             
    */

}  

       fclose(channels);
       fclose(omega);

  	return 0;
}

