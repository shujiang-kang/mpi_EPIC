      SUBROUTINE NDNITCI
!     EPIC1102
      USE PARM 
      ! XKN5,XKN3,XKN1/2.8,0.5,0.14/ *affinity coeff in gasdn0607                                                                        
      ! XKN5,XKN3,XKN1/10.0,0.5,0.0005/ *affinity coeff in epic0810v5                                                                        
      ! XKN5,XKN3,XKN1/10.0,2.5,2.5/ *as per Grant et al. (1993)                                                                   
      ! XKN5,XKN3,XKN1/10.0,2.5,2.5/ *latest approximation (2011/4/5)                                                        
      ! XKN5,XKN3,XKN1/0.077,0.126,0.057/ *latest approximation (2011/4/26)
      ! XKN5,XKN3,XKN1/10.,.5,.0005/,                                                                                                                               
      DATA B1,B2,B3/70.,140.,720./,COX/.032/,DAO2/7.2E-6/,O2MW/32./,&
      XN/2.58368E15/,FD/.19/,RGF,RMF/.547,.01785/,VU/1./,VL/-.15/,&
      DIAM1/3.27E-7/,STN/.0728/,A1/.61821/,T1/3.27536/,FCMP/3.06/,&
      VWCR/.03/,WPMP/153.06/,A,B,C/.002,1.4,.5/
      !DATA VALUE CHANGES September 2011:
      !XN ; current=2.07E16,new=2.58368E15 ; no significant change
      !VWCR ; current=0.0, new=0.03 ; no significant change
      !DAO2 ; current=2.E-5, new=7.2E-6 ; no significant change
      !B ; current=1.04, new=1.4 ; no significant change      
      ! THIS LOOP CALCULATES THE PRODUCTION AND CONSUMPTION OF O2, CO2, AND
      ! N2O BY SOIL LAYER WITHIN ONE HOUR. IT ALSO UPDATES POOLS OF NO3 AND
      ! NO2 FACTOR TO CONVERT MASS (kg/ha FOR A GIVEN SOIL LAYER) TO 
      ! GAS CONCENTRATION (G/M3 SOIL)
	  AD1=0.
      AD2=0.
      AD3=0.
      XFCM=LOG10(FCMP)
      XMP=XFCM-LOG10(WPMP)
      XSTN=2.*STN
      DO J=1,NBCL
          AD1=AD1+WNO3(J)+WNO2(J)+WN2O(J)
    !     EA=TOTAL ELECTRONS ACCEPTED BY O2 AND N OXIDES
          EA=0.
          EAO2=0.
          EAO2R=0.
          ESRR=0.
          WN2G=0.
          ESMR=RSPC(J)/B3
          X4=SOT(J)+273.15
          IF(IDN==3)THEN
              VWCE=MIN(.99,(VWC(J)-VWCR)/(TPOR(J)-VWCR))
              X3=.001*((VWCE**(-1./C)-1.)**(1./B))/A
              DW=MAX(1.01E-6,1.E-6+8.E-6*X3**(-0.945703126))
          ELSE
              DIAM2=1.E-6*(T1-(1./A1)*LOG((VU-VL)/(CBVT-VL)-1.))
              XFC=LOG10(VFC(J))
              BX=XMP/(LOG10(VWP(J))-XFC)
              A2=10.**(BX*XFC+XFCM)
              CONSTA=9.82*A2/XSTN
              CONSTB=1./BX
              PVC=(CONSTA*DIAM2)**CONSTB
              VEDW=VWC(J)-PVC
              HX=.9549*PVC/((.5*DIAM1)**2+(.5*DIAM2)**2+DIAM1*DIAM2/4.)
              XL=SQRT(HX**2+(.5*(DIAM2-DIAM1))**2)
              SA=1.5708*(DIAM1+DIAM2)*XL
              DW=MAX(1.1E-6,VEDW/SA)
          END IF
          DAO2TC=DAO2*(X4/293.15)**6
          XKT=1.5708E-10*XN*WBMC(J)*DAO2TC*DW/(DW-1.E-6)
          IF(XKT>0.)THEN
              QTB=XKT*(CLO2(J)-COX)-ESMR
              QTC=XKT*COX*CLO2(J)
              O2M=(QTB+SQRT(QTB*QTB+4.*XKT*QTC))/(2.*XKT)
              EAO2=ESMR*O2M/(O2M+COX)
          ELSE
              EAO2=0.
          END IF
          IF(RWTZ(J)>0.)THEN
              ! NEW DERIVATION FOR ELECTRON SUPPLY DUE TO ROOT RESPIRATION
              ! SEE MODEL DOCUMENTATION
              ! ROOT RESPIRED C (KG C HA-1 D-1)
              X1=(RGF/(1.-RGF))*MAX(0.,DRWX(J))+RMF*RWTZ(J)
              RRTC=.42*X1
	          ! ESRR=MOLE E- M-2 H-1 FROM ROOT RESPIRATION - USE ESRR
              ESRR=5.833E-4*X1
	  	      RRC=ESRR*B3 
              ! XKTR=54.573*DAO2TC*RWTZ(J)
              X2=LOG(DW/.001)
	          IF(X2<=0.)THEN
	              X2=1.
	          END IF
	          XKTR=125.664*DAO2TC*RWTZ(J)/X2
	          QTBR=XKTR*(CLO2(J)-COX)-ESRR
	          QTCR=XKTR*COX*CLO2(J)            
              ! SOLVE QUADRATIC EQN FOR O2M AND O2MR
              O2R=(QTBR+SQRT(QTBR*QTBR+4.*XKTR*QTCR))/(2.*XKTR)
              ! ELECTRONS FROM MICROBE AND ROOT RESPIRATION ACCEPTED BY O2
              EAO2R=ESRR*O2R/(O2R+COX)
          END IF	
          SUM=EAO2+EAO2R
          ESD=ESMR+ESRR-SUM
          ! ELECTRONS AVAILABLE FOR DENITRIFICATION
          ESD=FD*ESD
          ! COMPETITION FOR ELECTRONS AMONG OXIDES OF N
          ! CALCULATE WEIGHTING FACTORS FIRST
          X1=DZ10*VWC(J)
          CNO3=MAX(1.E-5,WNO3(J)/X1)
          CNO2=MAX(1.E-5,WNO2(J)/X1)
          WN5=5.*CNO3/(XKN5+CNO3)
          WN3=3.*CNO2/(XKN3*(1.+CNO3/XKN5)+CNO2)
          WN1=CLN2O(J)/(XKN1*(1.+CNO2/XKN3)+CLN2O(J))
          !     CALCULATE THE RATES OF REDUCTION OF OXIDES OF N
          X2=ESD/(WN1+WN3+WN5)
          X1=MAX(1.E-10,WNO3(J)/B1)
          EAN5=MIN(X1,X2*WN5)
          X1=MAX(1.E-10,WNO2(J)/B1)
          EAN3=MIN(X1,X2*WN3)
          IF(WN2O(J)>0.)THEN
              X1=WN2O(J)/B2
              EAN1=MIN(X1,X2*WN1)
          ELSE
              EAN1=0.
          END IF
              ! THESE ARE THE RESULTS BY LAYER AT THE END OF ONE HOUR
              ! IF NOT ALL ELECTRONS CAN BE ACCEPTED BY O2 (ESD>0.)
              ! TOTAL ELECTRONS ACCEPTED AND TRANSFORMATIONS OF N OXIDES
              EA=EA+EAO2+EAO2R+EAN5+EAN3+EAN1
              SMEA(J)=SMEA(J)+EA
              SMES(J)=SMES(J)+ESMR+ESRR
              ! EAD=TOTAL DEFICIT OF ELECTRONS
              EAD=EAN5+EAN3+EAN1
              !	LIQUID POOLS
              WNO3(J)=WNO3(J)-EAN5*B1
              WNO2(J)=WNO2(J)-(EAN3-EAN5)*B1
              !	GAS POOLS
              !	NITROUS OXIDE AND DINITROGEN
              !	GENN2O CALCULATES HOW MUCH N2O IS GENERATED (kg/ha/h per layer)
	          GENN2O=EAN3*B1-EAN1*B2
              ! DN2OG(J) ACCUMULATES N2O GENERATED DURING A DAY (kg/ha)
              ! IN LAYER J
	          DN2OG(J)=DN2OG(J)+GENN2O
              ! WN2O(J) UPDATES THE N2O POOL (kg/ha). *WN2O(J)=DN2O(J)*
              WN2O(J)=WN2O(J)+GENN2O
              ! AN2OC(J) CONVERTS MASS OF N2O INTO CONCENTRATION (G/M3)
	          AN2OC(J)=WN2O(J)/DZ10
              ! WN2G CALCULATES THE MASS OF N2 GENERATED (kg/ha)
              WN2G=EAN1*B2
 !             IF(WN2O(J)<0.)WRITE(KW(1),'(1X,A,4I4,10E16.6)')'~~~~~',IY,MO,&
 !             KDA,J,WN2O(J),GENN2O,WN2G
              ! DN2G(J) ACCUMULATES N2 GENERATED DURING A DAY (kg/ha)
              ! IN LAYER J
              DN2G(J)=DN2G(J)+WN2G
              ! OXYGEN          
              ! O2CONS= O2 CONSUMED (kg/ha)
              ! (MOL E/M2*1/4*MOL O2/MOL E*32 G O2/MOL O2*10.)
              ! FACTOR 10. ABOVE IS TO CONVERT G/M2 TO kg/ha
              O2CONS=2.5*SUM*O2MW
              ! DO2CONS ACCUMULATES O2 CONSUMED DAILY 
              ! IN LAYER J (kg/ha)
              DO2CONS(J)=DO2CONS(J)+O2CONS
              ! AO2C(J) RECALCULATES O2 CONCENTRATION IN LAYER (G/M3)
              AO2C(J)=MAX(0.,AO2C(J)-O2CONS/DZ10)
              ! CARBON DIOXIDE
              ! CO2GEN IS CO2 GENERATED (kg/ha)
              ! (MOL E/M2*1/4*MOL C/MOL E*12 G C/MOL C*10.)
              ! FACTOR 10. ABOVE IS TO CONVERT G/M2 TO kg/ha
              CO2GEN=EA*30.  !accounts for all ways to generate CO2 (WBM & RCI)
              ! DCO2GEN(J) ACCUMULATES CO2 GENERATED DAILY 
              ! IN LAYER J (kg/ha)
              DCO2GEN(J)=DCO2GEN(J)+CO2GEN
              ! ACO2C(J) RECALCULATES CO2 CONCENTRATION IN LAYER (G/M3)
              ACO2C(J)=MAX(0.,ACO2C(J)+CO2GEN/DZ10)
              AD2=AD2+WNO3(J)+WNO2(J)+WN2O(J)
              AD3=AD3+WN2G
      END DO
      DF=AD2+AD3-AD1
!      IF(ABS(DF/AD1)>1.E-5)WRITE(KW(1),1)IY,MO,KDA,AD1,AD2,AD3,DF   !skang
    1 FORMAT(1X,'NDNITCI',3I4,4E16.6)      
      RETURN
      END