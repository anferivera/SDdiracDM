#!/usr/bin/env python

#Created by Andres Rivera. File for red LSHAFiles

import pyslha

def buildSLHAinFile():

	########################### CREATED THE LHAFILE #######################
	#Read the blocks, but do not read all its entries.
	LHA = pyslha.readSLHAFile('LesHouches.in.SDdiracDM_low', ignorenomass=True)

	#Add entries with coments (some of them were vacum)
	LHA.blocks['MODSEL'].entries[1]='0               #  1/0: High/low scale input' 
	LHA.blocks['MODSEL'].entries[2]='1              # Boundary Condition'
	LHA.blocks['MODSEL'].entries[6]='1               # Generation Mixing'
	LHA.blocks['MODSEL'].entries[12]='173.5         # Renormalization scale'  

	LHA.blocks['SMINPUTS'].entries[2]='1.166370E-05    # G_F,Fermi constant' 
	LHA.blocks['SMINPUTS'].entries[3]='1.187000E-01    # alpha_s(MZ) SM MSbar' 
	LHA.blocks['SMINPUTS'].entries[4]='9.118870E+01    # Z-boson pole mass' 
	LHA.blocks['SMINPUTS'].entries[5]='4.180000E+00    # m_b(mb) SM MSbar' 
	LHA.blocks['SMINPUTS'].entries[6]='1.735000E+02    # m_top(pole)' 
	LHA.blocks['SMINPUTS'].entries[7]='1.776690E+00    # m_tau(pole)' 

	LHA.blocks['MINPAR'].entries[1]='2.6000000E-01    # LambdaIN'
	LHA.blocks['MINPAR'].entries[2]='3.0000000E+02    # MDFInput'
	LHA.blocks['MINPAR'].entries[3]='1.0000000E+06    # MS12Input'
	LHA.blocks['MINPAR'].entries[4]='4.0000000E+06    # MS22Input'
	LHA.blocks['MINPAR'].entries[5]='1.0000000E-02    # LamS1HInput'
	LHA.blocks['MINPAR'].entries[6]='1.0000000E-04    # LamS2HInput'
	LHA.blocks['MINPAR'].entries[7]='1.0000000E-04    # LamSPHInput'
	LHA.blocks['MINPAR'].entries[8]='1.0000000E-04    # LamSInput'
	LHA.blocks['MINPAR'].entries[9]='3.0000000E-01    # LamSPInput'
	LHA.blocks['MINPAR'].entries[10]='5.0000000E+02    # vSInput'
	LHA.blocks['MINPAR'].entries[11]='1.0000000E-03    # YRDInput'
	LHA.blocks['MINPAR'].entries[12]='5.0000000E-01    # YRCInput'

	LHA.blocks['SPHENOINPUT'].entries[1]='-1              # error level' 
	LHA.blocks['SPHENOINPUT'].entries[2]='  0              # SPA conventions' 
	LHA.blocks['SPHENOINPUT'].entries[7]='  1              # Skip 2-loop Higgs corrections '
	LHA.blocks['SPHENOINPUT'].entries[8]='  3              # Method used for two-loop calculation' 
	LHA.blocks['SPHENOINPUT'].entries[9]='  1              # Gaugeless limit used at two-loop' 
	LHA.blocks['SPHENOINPUT'].entries[10]='  0              # safe-mode used at two-loop '
	LHA.blocks['SPHENOINPUT'].entries[11]=' 1               # calculate branching ratios '
	LHA.blocks['SPHENOINPUT'].entries[13]=' 1               # 3-Body decays: none (0), fermion (1), scalar (2), both (3) '
	LHA.blocks['SPHENOINPUT'].entries[14]=' 0               # Run couplings to scale of decaying particle '
	LHA.blocks['SPHENOINPUT'].entries[12]=' 1.000E-04       # write only branching ratios larger than this value' 
	LHA.blocks['SPHENOINPUT'].entries[15]=' 1.000E-30       # write only decay if width larger than this value '
	LHA.blocks['SPHENOINPUT'].entries[16]=' 0              # One-loop decays '
	LHA.blocks['SPHENOINPUT'].entries[31]=' -1              # fixed GUT scale (-1: dynamical GUT scale)' 
	LHA.blocks['SPHENOINPUT'].entries[32]=' 0               # Strict unification '
	LHA.blocks['SPHENOINPUT'].entries[34]=' 1.000E-04       # Precision of mass calculation' 
	LHA.blocks['SPHENOINPUT'].entries[35]=' 40              # Maximal number of iterations'
	LHA.blocks['SPHENOINPUT'].entries[36]=' 5               # Minimal number of iterations before discarding points'
	LHA.blocks['SPHENOINPUT'].entries[37]=' 1               # Set Yukawa scheme  '
	LHA.blocks['SPHENOINPUT'].entries[38]=' 1               # 1- or 2-Loop RGEs '
	LHA.blocks['SPHENOINPUT'].entries[50]=' 0               # Majorana phases: use only positive masses (put 0 to use file with CalcHep/Micromegas!) '
	LHA.blocks['SPHENOINPUT'].entries[51]=' 0               # Write Output in CKM basis '
	LHA.blocks['SPHENOINPUT'].entries[52]=' 0               # Write spectrum in case of tachyonic states' 
	LHA.blocks['SPHENOINPUT'].entries[55]=' 0               # Calculate loop corrected masses '
	LHA.blocks['SPHENOINPUT'].entries[61]=' 0               # Running SM parameters'
	LHA.blocks['SPHENOINPUT'].entries[57]=' 1               # Calculate low energy constraints '
	LHA.blocks['SPHENOINPUT'].entries[65]=' 1               # Solution tadpole equation '
	LHA.blocks['SPHENOINPUT'].entries[66]=' 1               # Two-Scale Matching '
	LHA.blocks['SPHENOINPUT'].entries[67]=' 1               # effective Higgs mass calculation '
	LHA.blocks['SPHENOINPUT'].entries[75]=' 0               # Write WHIZARD files '
	LHA.blocks['SPHENOINPUT'].entries[76]=' 0               # Write HiggsBounds file'   
	LHA.blocks['SPHENOINPUT'].entries[77]=' 1               # Output for MicrOmegas (running masses for light quarks; real mixing matrices)'   
	LHA.blocks['SPHENOINPUT'].entries[78]=' 1               # Output for MadGraph (writes also vanishing blocks)'
	LHA.blocks['SPHENOINPUT'].entries[79]=' 1               # Write WCXF files (exchange format for Wilson coefficients)' 
	LHA.blocks['SPHENOINPUT'].entries[86]=' 0.              # Maximal width to be counted as invisible in Higgs decays; -1: only LSP '
	LHA.blocks['SPHENOINPUT'].entries[510]=' 0.              # Write tree level values for tadpole solutions '
	LHA.blocks['SPHENOINPUT'].entries[515]=' 0               # Write parameter values at GUT scale '
	LHA.blocks['SPHENOINPUT'].entries[520]=' 1.              # Write effective Higgs couplings (HiggsBounds blocks): put 0 to use file with MadGraph!' 
	LHA.blocks['SPHENOINPUT'].entries[521]=' 1.              # Diphoton/Digluon widths including higher order '
	LHA.blocks['SPHENOINPUT'].entries[525]=' 0.              # Write loop contributions to diphoton decay of Higgs' 
	LHA.blocks['SPHENOINPUT'].entries[530]=' 0               # Write Blocks for Vevacious '
	LHA.blocks['SPHENOINPUT'].entries[1101]=' 1             # Include Fxe in 1.loop corrections' 
	LHA.blocks['SPHENOINPUT'].entries[1102]=' 1             # Include Fd in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1103]=' 1             # Include Fu in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1104]=' 1             # Include Fe in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1105]=' 1             # Include Fv in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1106]=' 1             # Include Fxv in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1201]=' 1             # Include Hp in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1202]=' 1             # Include Ah in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1203]=' 1             # Include hh in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1204]=' 1             # Include Ssc in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1301]=' 1             # Include VG in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1302]=' 1             # Include VP in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1303]=' 1             # Include VZ in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1304]=' 1             # Include VWp in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1401]=' 1             # Include gG in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1402]=' 1             # Include gA in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1403]=' 1             # Include gZ in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1404]=' 1             # Include gWp in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1405]=' 1             # Include gWC in 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1500]=' 1               # Include Wave diagrams 1.loop corrections '
	LHA.blocks['SPHENOINPUT'].entries[1501]=' 1               # Include Penguin diagrams 1.loop corrections' 
	LHA.blocks['SPHENOINPUT'].entries[1502]=' 1               # Include Box diagrams 1.loop corrections '

	LHA.blocks['DECAYOPTIONS'].entries[1]='1     # Calc 3-Body decays of Fu '
	LHA.blocks['DECAYOPTIONS'].entries[2]='1     # Calc 3-Body decays of Fe '
	LHA.blocks['DECAYOPTIONS'].entries[3]='1     # Calc 3-Body decays of Fd '
	LHA.blocks['DECAYOPTIONS'].entries[1001]='1     # Loop Decay of Fu '
	LHA.blocks['DECAYOPTIONS'].entries[1002]='1     # Loop Decay of Fe '
	LHA.blocks['DECAYOPTIONS'].entries[1003]='1     # Loop Decay of Fd '
	LHA.blocks['DECAYOPTIONS'].entries[1004]='1     # Loop Decay of hh '
	LHA.blocks['DECAYOPTIONS'].entries[1005]='1     # Loop Decay of Ssc '
	LHA.blocks['DECAYOPTIONS'].entries[1006]='0     # Loop Decay of Fxe '
	LHA.blocks['DECAYOPTIONS'].entries[1007]='0     # Loop Decay of Fxv '
	LHA.blocks['DECAYOPTIONS'].entries[1114]='1.     # U-factors (0: off, 1:p2_i=m2_i, 2:p2=0, p3:p2_i=m2_1) '
	LHA.blocks['DECAYOPTIONS'].entries[1115]='1.     # Use loop-corrected masses for external states '
	LHA.blocks['DECAYOPTIONS'].entries[1116]='0.     # OS values for W,Z and fermions (0: off, 1:g1,g2,v 2:g1,g2,v,Y_i) '
	LHA.blocks['DECAYOPTIONS'].entries[1117]='0.     # Use defined counter-terms '
	LHA.blocks['DECAYOPTIONS'].entries[1118]='1.     # Use everywhere loop-corrected masses for loop-induced decays '

	LHA.blocks['YNUIN'].entries[1,1]=' 0.000000E-00         # YNU(1,1)'
	LHA.blocks['YNUIN'].entries[1,2]=' 0.000000E-00         # YNU(1,2)'
	LHA.blocks['YNUIN'].entries[1,3]=' 0.000000E-00         # YNU(1,3)'
	LHA.blocks['YNUIN'].entries[2,1]=' 0.000000E-00         # YNU(2,1)'
	LHA.blocks['YNUIN'].entries[2,2]=' 0.000000E-00         # YNU(2,2)'
	LHA.blocks['YNUIN'].entries[2,3]=' 0.000000E-00         # YNU(2,3)'
	LHA.blocks['YNUIN'].entries[3,1]=' 0.000000E-00         # YNU(3,1)'
	LHA.blocks['YNUIN'].entries[3,2]=' 0.000000E-00         # YNU(3,2)'
	LHA.blocks['YNUIN'].entries[3,3]=' 0.000000E-00         # YNU(3,3)'

	LHA.blocks['YRA1IN'].entries[1]='1.000000E-02         # YRA1(1)'
	LHA.blocks['YRA1IN'].entries[2]='2.000000E-02         # YRA1(2)'
	LHA.blocks['YRA1IN'].entries[3]='3.000000E-03         # YRA1(3)'

	LHA.blocks['YRA2IN'].entries[1]='1.100000E-02         # YRA2(1)'
	LHA.blocks['YRA2IN'].entries[2]='3.200000E-03         # YRA2(2)'
	LHA.blocks['YRA2IN'].entries[3]='2.100000E-03         # YRA2(3)'

	LHA.blocks['YRB1IN'].entries[1]='3.100000E-03         # YRB1(1)'
	LHA.blocks['YRB1IN'].entries[2]='2.100000E-03         # YRB1(2)'
	LHA.blocks['YRB1IN'].entries[3]='1.100000E-03         # YRB1(3)'

	LHA.blocks['YRB2IN'].entries[1]='3.000000E-02         # YRB2(1)'
	LHA.blocks['YRB2IN'].entries[2]='2.000000E-03         # YRB2(2)'
	LHA.blocks['YRB2IN'].entries[3]='1.000000E-03         # YRB2(3)'
	################################################################
	
	return LHA

# Example: Modific the LesHouches_low   
#LHA.blocks['MINPAR'].entries[2]='%.7E    # MDFInput' %222
#xdict['MINPAR'].entries[3]='%.6E    # MS12Input' %MS12      

#Write the file
#pyslha.writeSLHAFile('InputFile',LHA)
 
