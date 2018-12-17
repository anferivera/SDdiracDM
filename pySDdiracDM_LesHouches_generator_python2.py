#!/usr/bin/env python
'''
Modules to run and analyse SPheno. Current implementation icludes:
* Gneration of general LesHouches dictionary.
* Change RVKAPPAIN and RVSNVEVIN from a six component vector
* Gneration of general LesHouches.in file.
* Filter Decays Block.
* ranlog: for generate random numbers in ranges of several orders
  of magnitude
* Funtions to analyse neutrino solutions
'''
import pyslha

def buildLHAinFile():
    """Usage of pyslha.Block to generate one LesHouches.in file"""
    #Initialize dictionary of block clases
    xdict={}
    #define block class
    #====================
    MODSEL=pyslha.Block('MODSEL') #
    #Add entries to the class
    MODSEL.entries[1]='0               #  1/0: High/low scale input' 
    MODSEL.entries[2]='1              # Boundary Condition'
    MODSEL.entries[6]='1               # Generation Mixing'
    MODSEL.entries[12]='2000.5         # Renormalization scale'    
    #Add class to dictionary
    xdict['MODSEL']=MODSEL
    #====================    
    SMINPUTS=pyslha.Block('SMINPUTS')    # Standard Model inputs 
    SMINPUTS.entries[2]='1.166370E-05    # G_F,Fermi constantt'
    SMINPUTS.entries[3]='1.187000E-01    # alpha_s(MZ) SM MSbar'
    SMINPUTS.entries[4]='9.118870E+01    # Z-boson pole mass'
    SMINPUTS.entries[5]='4.180000E+00    # m_b(mb) SM MSbar'
    SMINPUTS.entries[6]='1.735000E+02    # m_top(pole)'
    SMINPUTS.entries[7]='1.776690E+00    # m_tau(pole)'
    xdict['SMINPUTS']=SMINPUTS
    #====================
    MINPAR=pyslha.Block('MINPAR')      # Input parameters 
    MINPAR.entries[1]='   2.7000000E-01    # LambdaIN'
    MINPAR.entries[2]='   2.0000000E+02    # MDFInput'
    MINPAR.entries[3]='   3.0000000E+06    # MS12Input'
    MINPAR.entries[4]='   4.0000000E+06    # MS22Input'
    MINPAR.entries[5]='   0.0000000E-04    # LamS1HInput'
    MINPAR.entries[6]='   0.0000000E-04    # LamS2HInput'
    MINPAR.entries[7]='   0.0000000E-04    # LamSPHInput'
    MINPAR.entries[8]='   1.0000000E-04    # LamSInput'
    MINPAR.entries[9]='   3.0000000E-01    # LamSPInput'
    MINPAR.entries[10]='   5.0000000E+02    # vSInput'
    MINPAR.entries[11]='   1.0000000E-03    # YRDInput'
    MINPAR.entries[12]='   5.0000000E-01    # YRCInput'
    xdict['MINPAR']=MINPAR
    #====================
    SPhenoInput=pyslha.Block('SPhenoInput') # SPheno specific input 
    SPhenoInput.entries[1]=' -1              # error level' 
    SPhenoInput.entries[2]='  0              # SPA conventions' 
    SPhenoInput.entries[7]='  1              # Skip 2-loop Higgs corrections '
    SPhenoInput.entries[8]='  3              # Method used for two-loop calculation' 
    SPhenoInput.entries[9]='  1              # Gaugeless limit used at two-loop '
    SPhenoInput.entries[10]='  0              # safe-mode used at two-loop '
    SPhenoInput.entries[11]=' 1               # calculate branching ratios '
    SPhenoInput.entries[13]=' 1               # 3-Body decays: none (0), fermion (1), scalar (2), both (3)' 
    SPhenoInput.entries[14]=' 0               # Run couplings to scale of decaying particle '
    SPhenoInput.entries[12]=' 1.000E-04       # write only branching ratios larger than this value' 
    SPhenoInput.entries[15]=' 1.000E-30       # write only decay if width larger than this value '
    SPhenoInput.entries[16]=' 0              # One-loop decays '
    SPhenoInput.entries[31]=' -1              # fixed GUT scale (-1: dynamical GUT scale)' 
    SPhenoInput.entries[32]=' 0               # Strict unification '
    SPhenoInput.entries[34]=' 1.000E-04       # Precision of mass calculation' 
    SPhenoInput.entries[35]=' 40              # Maximal number of iterations'
    SPhenoInput.entries[36]=' 5               # Minimal number of iterations before discarding points'
    SPhenoInput.entries[37]=' 1               # Set Yukawa scheme  '
    SPhenoInput.entries[38]=' 1               # 1- or 2-Loop RGEs '
    SPhenoInput.entries[50]=' 0               # Majorana phases: use only positive masses (put 0 to use file with CalcHep/Micromegas!)' 
    SPhenoInput.entries[51]=' 0               # Write Output in CKM basis '
    SPhenoInput.entries[52]=' 0               # Write spectrum in case of tachyonic states' 
    SPhenoInput.entries[55]=' 0               # Calculate loop corrected masses '
    SPhenoInput.entries[61]=' 0               # Running SM parameters'
    SPhenoInput.entries[57]=' 1               # Calculate low energy constraints '
    SPhenoInput.entries[65]=' 1               # Solution tadpole equation '
    SPhenoInput.entries[66]=' 1               # Two-Scale Matching '
    SPhenoInput.entries[67]=' 1               # effective Higgs mass calculation' 
    SPhenoInput.entries[75]=' 0               # Write WHIZARD files '
    SPhenoInput.entries[76]=' 0               # Write HiggsBounds file'   
    SPhenoInput.entries[77]=' 1               # Output for MicrOmegas (running masses for light quarks; real mixing matrices)   '
    SPhenoInput.entries[78]=' 1               # Output for MadGraph (writes also vanishing blocks)   '
    SPhenoInput.entries[79]=' 1               # Write WCXF files (exchange format for Wilson coefficients)' 
    SPhenoInput.entries[86]=' 0.              # Maximal width to be counted as invisible in Higgs decays; -1: only LSP '
    SPhenoInput.entries[510]=' 0.              # Write tree level values for tadpole solutions '
    SPhenoInput.entries[515]=' 0               # Write parameter values at GUT scale '
    SPhenoInput.entries[520]=' 0              # Write effective Higgs couplings (HiggsBounds blocks): put 0 to use file with MadGraph! '
    SPhenoInput.entries[521]=' 1.              # Diphoton/Digluon widths including higher order '
    SPhenoInput.entries[525]=' 0.              # Write loop contributions to diphoton decay of Higgs' 
    SPhenoInput.entries[530]=' 0               # Write Blocks for Vevacious '
    SPhenoInput.entries[1101]=' 1             # Include Fxe in 1.loop corrections '
    SPhenoInput.entries[1102]=' 1             # Include Fd in 1.loop corrections '
    SPhenoInput.entries[1103]=' 1             # Include Fu in 1.loop corrections '
    SPhenoInput.entries[1104]=' 1             # Include Fe in 1.loop corrections '
    SPhenoInput.entries[1105]=' 1             # Include Fv in 1.loop corrections '
    SPhenoInput.entries[1106]=' 1             # Include Fxv in 1.loop corrections '
    SPhenoInput.entries[1201]=' 1             # Include Hp in 1.loop corrections '
    SPhenoInput.entries[1202]=' 1             # Include Ah in 1.loop corrections '
    SPhenoInput.entries[1203]=' 1             # Include hh in 1.loop corrections '
    SPhenoInput.entries[1204]=' 1             # Include Ssc in 1.loop corrections '
    SPhenoInput.entries[1301]=' 1             # Include VG in 1.loop corrections '
    SPhenoInput.entries[1302]=' 1             # Include VP in 1.loop corrections '
    SPhenoInput.entries[1303]=' 1             # Include VZ in 1.loop corrections '
    SPhenoInput.entries[1304]=' 1             # Include VWp in 1.loop corrections '
    SPhenoInput.entries[1401]=' 1             # Include gG in 1.loop corrections '
    SPhenoInput.entries[1402]=' 1             # Include gA in 1.loop corrections '
    SPhenoInput.entries[1403]=' 1             # Include gZ in 1.loop corrections '
    SPhenoInput.entries[1404]=' 1             # Include gWp in 1.loop corrections '
    SPhenoInput.entries[1405]=' 1             # Include gWC in 1.loop corrections '
    SPhenoInput.entries[1500]=' 1               # Include Wave diagrams 1.loop corrections' 
    SPhenoInput.entries[1501]=' 1               # Include Penguin diagrams 1.loop corrections' 
    SPhenoInput.entries[1502]=' 1               # Include Box diagrams 1.loop corrections '
    xdict['SPhenoInput']=SPhenoInput
    #====================
    DECAYOPTIONS=pyslha.Block('DECAYOPTIONS') # Options to turn on/off specific decays 
    DECAYOPTIONS.entries[1]='    1     # Calc 3-Body decays of Fu '
    DECAYOPTIONS.entries[2]='    1     # Calc 3-Body decays of Fe '
    DECAYOPTIONS.entries[3]='    1     # Calc 3-Body decays of Fd '
    DECAYOPTIONS.entries[1001]=' 1     # Loop Decay of Fu '
    DECAYOPTIONS.entries[1002]=' 1     # Loop Decay of Fe '
    DECAYOPTIONS.entries[1003]=' 1     # Loop Decay of Fd '
    DECAYOPTIONS.entries[1004]=' 1     # Loop Decay of hh '
    DECAYOPTIONS.entries[1005]=' 1     # Loop Decay of Ssc '
    DECAYOPTIONS.entries[1006]=' 1     # Loop Decay of Fxe '
    DECAYOPTIONS.entries[1007]=' 1     # Loop Decay of Fxv '
    DECAYOPTIONS.entries[1114]=' 1.     # U-factors (0: off, 1:p2_i=m2_i, 2:p2=0, p3:p2_i=m2_1) '
    DECAYOPTIONS.entries[1115]=' 1.     # Use loop-corrected masses for external states '
    DECAYOPTIONS.entries[1116]=' 0.     # OS values for W,Z and fermions (0: off, 1:g1,g2,v 2:g1,g2,v,Y_i)' 
    DECAYOPTIONS.entries[1117]=' 0.     # Use defined counter-terms '
    DECAYOPTIONS.entries[1118]=' 1.     # Use everywhere loop-corrected masses for loop-induced decays '
    xdict['DECAYOPTIONS']=DECAYOPTIONS
    #====================
    YNUIN=pyslha.Block('YNUIN')
    YNUIN.entries[1]={}
    YNUIN.entries[1][1]=' 0.000000E-00         # YNU(1,1)'
    YNUIN.entries[1][2]=' 0.000000E-00         # YNU(1,2)'
    YNUIN.entries[1][3]=' 0.000000E-00         # YNU(1,3)'
    YNUIN.entries[2]={}
    YNUIN.entries[2][1]=' 0.000000E-00         # YNU(2,1)'
    YNUIN.entries[2][2]=' 0.000000E-00         # YNU(2,2)'
    YNUIN.entries[2][3]=' 0.000000E-00         # YNU(2,3)'
    YNUIN.entries[3]={}
    YNUIN.entries[3][1]=' 0.000000E-00         # YNU(3,1)'
    YNUIN.entries[3][2]=' 0.000000E-00         # YNU(3,2)'
    YNUIN.entries[3][3]=' 0.000000E-00         # YNU(3,3)'
    xdict['YNUIN']=YNUIN
    #====================
    YRA1IN=pyslha.Block('YRA1IN')
    YRA1IN.entries[1]='   1.500000E-02         # YRA1(1)'
    YRA1IN.entries[2]='   2.000000E-03         # YRA1(2)'
    YRA1IN.entries[3]='   1.000000E-02         # YRA1(3)'
    xdict['YRA1IN']=YRA1IN    
    #====================
    YRA2IN=pyslha.Block('YRA2IN')
    YRA2IN.entries[1]='   3.500000E-03         # YRA2(1)'
    YRA2IN.entries[2]='   2.000000E-03         # YRA2(2)'
    YRA2IN.entries[3]='   1.000000E-03         # YRA2(3)'
    xdict['YRA2IN']=YRA2IN    
    #====================    
    YRB1IN=pyslha.Block('YRB1IN')
    YRB1IN.entries[1]='   1.100000E-04         # YRB1(1)'
    YRB1IN.entries[2]='   2.000000E-03         # YRB1(2)'
    YRB1IN.entries[3]='   1.000000E-03         # YRB1(3)'
    xdict['YRB1IN']=YRB1IN
    #====================
    YRB2IN=pyslha.Block('YRB2IN')
    YRB2IN.entries[1]='   1.500000E-04         # YRB2(1)'
    YRB2IN.entries[2]='   3.000000E-03         # YRB2(2)'
    YRB2IN.entries[3]='   1.100000E-03         # YRB2(3)'
    xdict['YRB2IN']=YRB2IN
    #====================    
        
    return xdict 

def writeLHAinFile(xdict,lhinfile='LesHouches.in.SDdiracDM_low'):
    '''To write LesHouches.in in the right order.'''
    LesHouches={}
    LesHouches['AMODSEL']=xdict['MODSEL']
    LesHouches['BSMINPUTS']=xdict['SMINPUTS']
    LesHouches['CMINPAR']=xdict['MINPAR']
    LesHouches['DSPhenoInput']=xdict['SPhenoInput']   
    LesHouches['EDECAYOPTIONS']=xdict['DECAYOPTIONS']   
    LesHouches['FYNUIN']=xdict['YNUIN']
    LesHouches['GYRA1IN']=xdict['YRA1IN']
    LesHouches['HYRA2IN']=xdict['YRA2IN']
    LesHouches['IYRB1IN']=xdict['YRB1IN']
    LesHouches['JYRB2IN']=xdict['YRB2IN']
   
    #pyslha.writeSLHAFile(lhinfile,LesHouches,{})
    pyslha.writeSLHAFile(lhinfile,LesHouches)

"""
def filterParticle(p1,pdaug,sign=False):
    '''Find decays channels with PDG PI:
         pdaug
    for Particle class:
        p1
    Returns filtered Particles class:
        p2
    '''
    p2 = pyslha.Particle(p1.pid, p1.totalwidth, p1.mass)
    if sign:
        p2.decays = [d for d in p1.decays if pdaug in d.ids]
    else:
        p2.decays = [d for d in p1.decays if pdaug in map(abs, d.ids)]
    return p2
"""
if __name__ == '__main__':
    print ('''Check pyspheno_random.py and pysphenofull.py for examples of use''')

