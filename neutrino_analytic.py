###+++++++++ python rutine to get the anlytical neutrino eigenvalues +++++++++++++++++++++++++++++

import numpy as np


#Loop factor
def Fk(mNk,mSk,Vk2,Uk1):
    
    mk = 1./(16.*np.pi**2)*Vk2*Uk1*(mNk**3/(mNk**2-mSk**2))*np.log(mNk**2/mSk**2)
    
    return mk   

#Mab matrix. sum over i and k is expanded
def Mab(YB1b,YB2b,YA1a,YA2a,m1,m2,ms1,ms2,V12,V22,U11,U21):
    sumS1= (Fk(m1, ms1, V12, U11)+Fk(m2, ms1, V22, U21))*(YB1b*YA1a) 
    
    sumS2= (Fk(m1, ms2, V12, U11)+Fk(m2, ms2, V22, U21))*(YB2b*YA2a) 
    
    return sumS1 + sumS2

def MATRIXDIAG(YB11,YB12,YB13,YB21,YB22,YB23,YA11,YA12,YA13,YA21,YA22,YA23,m1,m2,mS1,mS2,V12,V22,U11,U21):

    #Matrix elements
    M11 = Mab(YB11,YB21,YA11,YA21,m1,m2,mS1,mS2,V12,V22,U11,U21)
    M12 = Mab(YB11,YB21,YA12,YA22,m1,m2,mS1,mS2,V12,V22,U11,U21)
    M13 = Mab(YB11,YB21,YA13,YA23,m1,m2,mS1,mS2,V12,V22,U11,U21)
    M21 = Mab(YB12,YB22,YA11,YA21,m1,m2,mS1,mS2,V12,V22,U11,U21)
    M22 = Mab(YB12,YB22,YA12,YA22,m1,m2,mS1,mS2,V12,V22,U11,U21)
    M23 = Mab(YB12,YB22,YA13,YA23,m1,m2,mS1,mS2,V12,V22,U11,U21)
    M31 = Mab(YB13,YB23,YA11,YA21,m1,m2,mS1,mS2,V12,V22,U11,U21)
    M32 = Mab(YB13,YB23,YA12,YA22,m1,m2,mS1,mS2,V12,V22,U11,U21)
    M33 = Mab(YB13,YB23,YA13,YA23,m1,m2,mS1,mS2,V12,V22,U11,U21)


    Mvij = np.matrix( [[M11, M12, M13],
                       [M21, M22, M23],
                       [M31, M32, M33]] )

    #eigenvalues e eigenvectors
    (Mdiag2,V)=np.linalg.eig(Mvij*np.transpose(Mvij))
    
    #took eigenvalues
    MX1 = np.sqrt(np.abs(Mdiag2[0]))
    MX2 = np.sqrt(np.abs(Mdiag2[1]))
    MX3 = np.sqrt(np.abs(Mdiag2[2]))
    
    ## reorganize the eigenvalues (neutrino masses)
    mn1 = 0.
    mn2 = 0.
    mn3 = 0.

    if MX1 < MX2 and MX1 < MX3:
        mn1 = MX1
        #print "Hola1"

        if MX2 < MX3:
            mn2 = MX2
            mn3 = MX3
        else:
            mn2 = MX3
            mn3 = MX2  

    if MX2 < MX1 and MX2 < MX3:
        mn1 = MX2
        #print "Hola2" 

        if MX1 < MX3:
            mn2 = MX1
            mn3 = MX3
        else:
            mn2 = MX3
            mn3 = MX1   

    if MX3 < MX1 and MX3 < MX2:
        mn1 = MX3
        #print "Hola3"  

        if MX1 < MX2:
            mn2 = MX1
            mn3 = MX2
        else:
            mn2 = MX2
            mn3 = MX1

    #print("Theoretical values found:")        
    #print(mn1, mn2,mn3)   

    return mn1, mn2, mn3

#run all dataframe
MatrixDiag_new=np.vectorize(MATRIXDIAG,excluded={'vev':246.2,'sw':np.sqrt(0.23),'gg':0.653,'LAMBDA':1E16},\
                      doc='Input for pyfunc below: ')