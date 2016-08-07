      SUBROUTINE GASDF3
!     EPIC1102
!     THIS PROGRAM SOLVES THE GAS DIFFUSION EQUATION
      USE PARM
      DIMENSION YTP(30),XTP1(30),XTP2(30),XTP3(30),XTP4(30),XTP5(30),&
      XTP6(30),XTP7(30),XTP8(30),XTP9(30),XTP10(30),XTP11(30),XTP12(30),&
      XTP13(30),XTP14(30),XTP15(30),XTP16(30),XTP17(30),XTP18(30)
!     WBM & RCI REVISED GAS CONC. IN AIR (g/m3) (8/25/2011)      
!     PARTIAL PRESSURE AT SEA LEVEL (0 m)
!     O2=299.2957143;
!     CO2=.766071429; CO2-C=.208928571
!     N2O=.000616786; N2O-N=.0003925
!     Original calculations in GasDiffusion_inAir.xls
      REAL :: FLXO2, FLXCO2, FLXN2O
      DATA GASC/0.08205783/,DCAO,DCAC,DCAN/.06132,.05556,.05148/,CUPO,CUPC,&
      CUPN/299.,.2089,.0003925/

      DZX=1000.*DZ
      DTG10=10.*DTG
      SM1=0.
      AD1=0.
      AD2=0.
      AD3=0.
      TOT1=0.
      DO J=1,NBSL
          L=LID(J)
          YTP(L)=RSPC(L)
          XTP1(L)=WNO3(L)
          XTP2(L)=WNO2(L)
          XTP3(L)=WN2O(L)
          XTP4(L)=WBMC(L)
          XTP5(L)=RWT(L,JJK)
          XTP6(L)=MAX(1.E-10,DRWX(L))
          XTP7(L)=WCO2L(L)
          XTP8(L)=WCO2G(L)
          XTP9(L)=WN2OL(L)
          XTP10(L)=WN2OG(L)
          XTP11(L)=WO2L(L)
          XTP12(L)=WO2G(L)
          XTP13(L)=SSFCO2(L)
          XTP14(L)=SSFN2O(L)
          XTP15(L)=SSFO2(L)
          XTP16(L)=VCO2(L)
          XTP17(L)=VN2O(L)
          XTP18(L)=VO2(L)
          SM1=SM1+WNO3(L)+WNO2(L)+WN2O(L)
          AD1=AD1+WN2O(L)
          AD2=AD2+WN2OG(L)
          AD3=AD3+WN2OL(L)
          TOT1=TOT1+WBMC(L)
      END DO
      CALL AINTRI(PO,TPOR,NBSL,NBCL)
      CALL AINTRI(ST,VWC,NBSL,NBCL)
      CALL AINTRI(S15,VWP,NBSL,NBCL)
      CALL AINTRIC(STMP,SOT,NBSL,NBCL)
      CALL AINTRI(YTP,RSPC,NBSL,NBCL)
      CALL AINTRI(XTP1,WNO3,NBSL,NBCL)
      CALL AINTRI(XTP2,WNO2,NBSL,NBCL)
      CALL AINTRI(XTP3,WN2O,NBSL,NBCL)
      CALL AINTRI(XTP4,WBMC,NBSL,NBCL)
      CALL AINTRI(XTP5,RWTZ,NBSL,NBCL)
      CALL AINTRI(XTP6,DRWX,NBSL,NBCL)
      CALL AINTRI(XTP7,WCO2L,NBSL,NBCL)
      CALL AINTRI(XTP8,WCO2G,NBSL,NBCL)
      CALL AINTRI(XTP9,WN2OL,NBSL,NBCL)
      CALL AINTRI(XTP10,WN2OG,NBSL,NBCL)
      CALL AINTRI(XTP11,WO2L,NBSL,NBCL)
      CALL AINTRI(XTP12,WO2G,NBSL,NBCL)
      CALL AINTRI(XTP13,SSFCO2,NBSL,NBCL)
      CALL AINTRI(XTP14,SSFN2O,NBSL,NBCL)
      CALL AINTRI(XTP15,SSFO2,NBSL,NBCL)
      CALL AINTRI(XTP16,VCO2,NBSL,NBCL)
      CALL AINTRI(XTP17,VN2O,NBSL,NBCL)
      CALL AINTRI(XTP18,VO2,NBSL,NBCL)
      CALL AINTRI(FC,VFC,NBSL,NBCL)
      AD4=0.
      AD5=0.
      AD6=0.
      TOT2=0.
      DO I=1,NBCL
          AD4=AD4+WN2O(I)
          AD5=AD5+WN2OG(I)
          AD6=AD6+WN2OL(I)
          TOT2=TOT2+WBMC(I)
      END DO
      ZZ=DZX
      DO I=1,NBCL
         IF(I==NBCL)ZZ=1000.*(Z(LID(NBSL))-ZC(NBCL-1))
         TPOR(I)=TPOR(I)/ZZ
         VWC(I)=VWC(I)/ZZ
         VFC(I)=VFC(I)/ZZ
         VWP(I)=VWP(I)/ZZ
         ABST=SOT(I)+273.15
         AFP(I)=MAX(1.E-5,TPOR(I)-VWC(I))
         ! HERE WE OBTAIN HENRYS CONST AS A FUNCT OF SOIL T DIRECTLY
         ! UNITS ARE kpa M3 MOL-1
         HKF     = .018/(GASC*ABST)

         !! Oxygen
         X1=.01*ABST
         XXO=EXP(-66.7354+87.4755/X1+24.4526*LOG(X1))
         HKPO(I)=HKF*(1./XXO-1.)

         !! Carbon dioxide
         XXC=EXP(-8.286*LOG(ABST)+46.742)*1.E-03
         HKPC(I)=HKF*(1./XXC-1.)

         !! Nitrogen
         XXN=EXP(-60.7467+88.828/X1+21.2531*LOG(X1))
         HKPN(I)=HKF*(1./XXN-1.)
      END DO
      DO I=1,NBCL
          IF(I < NBCL) THEN
             !! Estimate soil properties at grid cell boundaries using
             !! linear interpolation (NB: someday we might want to do
             !! a spline interpolation here)
             ABST  = 0.5*(SOT(I)  + SOT(I+1)) + 273.15
             XTPOR = 0.5*(TPOR(I) + TPOR(I+1))
             XVWC  = 0.5*(VWC(I)  + VWC(I+1))
             XVFC  = 0.5*(VFC(I)  + VFC(I+1))
             XAFP  = 0.5*(AFP(I)  + AFP(I+1))
          ELSE
             !! for the last grid cell we don't have a value to
             !! interpolate with, so just use the cell-center value.
             ABST  = SOT(I)+273.15
             XTPOR = TPOR(I)
             XVWC  = VWC(I)
             XVFC  = VFC(I)
             XAFP  = AFP(I)
          END IF
          HKF=.018/(GASC*ABST)
          ! THIS IS THE MILLINGTON-QUIRCK COEFF
          EPS=XAFP**3.333/XTPOR**2
          ! DIFFUSION COEFFICIENT IN SOIL
          ! First, for O2
          DCSO    = DCAO*EPS
          X1      = .01*ABST
          XXO     = EXP(-66.7354+87.4755/X1+24.4526*LOG(X1))
          XHKPO   = HKF*(1./XXO-1.)
          DPRO(I) = DCSO*XAFP/(XTPOR+XVWC*(1./XHKPO-1.))*(ABST/273.15)**2
          !    FOR CO2
          DCSC    = DCAC*EPS
          ! HERE WE OBTAIN HENRYS CONST AS A FUNCT OF SOIL T DIRECTLY
          ! UNITS ARE kpa M-3 MOL-1
          XXC     = EXP(-8.286*LOG(ABST)+46.742)*1.E-03 
          XHKPC   = HKF*(1./XXC-1.)
          DPRC(I) = DCSC*XAFP/(XTPOR+XVWC*(1./XHKPC-1.))*(ABST/273.15)**1.75
          !    FOR N2O
          DCSN    = DCAN*EPS
          XXN     = EXP(-60.7467+88.828/X1+21.2531*LOG(X1))
          XHKPN   = HKF*(1./XXN-1.)
          DPRN(I) = DCSN*XAFP/(XTPOR+XVWC*(1./XHKPN-1.))*(ABST/273.15)**1.75
      END DO
      IF(IY==1.AND.IDA==IBD)CALL CCONI
      ! SET TO 0. LAYER ARRAYS OF DAILY PRODUCTION AND CONSUMPTION
      ! OF GASES (O2, CO2, N2O, N2 AND N2O+N2 [DDENIT])
      DO I=1,NBCL
          DO2CONS(I)=0.
          DCO2GEN(I)=0.
          DN2OG(I)=0.
          DN2G(I)=0.
      END DO
!     Initialize daily gas fluxes (WBM & RCI, 8/25/11)
      SMEA=0.
      SMES=0.
      DFO2S=0.
      DFCO2S=0.
      DFN2OS=0.
      DFO2B=0.
      DFCO2B=0.
      DFN2OB=0.
      DFO2T=0.
      DFCO2T=0.
      DFN2OT=0.
      TIME=0.
!	  TIME LOOP TO CALCULATE GENERATION AND CONSUMPTION OF
!	  GASES, GAS TRANSPORT, AND FLUX AT THE SURFACE
      DO IT=1,NBDT
          TIME=TIME+DTG
    !     CALCULATE GENERATION AND CONSUMPTION OF GASES
          CALL NDNITCI
    !	  RE-CALCULATE GAS CONCENTRATIONS IN LIQUID AND AIR PHASES
    !	  PRODUCTION AND CONSUMPTION OF GASES
          CALL NCCONC
          WO2GB=0.
          WCO2GB=0.
          WN2OGB=0.   
          WO2GA=0.
          WCO2GA=0.
          WN2OGA=0.
          DO J=1,NBCL
              X1=AFP(J)*DZ10
              WO2GB=WO2GB+CGO2(J)*X1
              WCO2GB=WCO2GB+CGCO2(J)*X1
              WN2OGB=WN2OGB+CGN2O(J)*X1
          END DO
    !     MOVE O2 AND STORE VALUES OF GAS CONC. IN 2-DIM ARRAY
          CALL GASTRANS2(CGO2,DPRO,CUPO,DCAO,FLXO2)
    !     MOVE CO2 AND STORE VALUES OF GAS CONC. IN 2-DIM ARRAY
          CALL GASTRANS2(CGCO2,DPRC,CUPC,DCAC,FLXCO2)    
    !     MOVE N2O AND STORE VALUES OF GAS CONC. IN 2-DIM ARRAY
          CALL GASTRANS2(CGN2O,DPRN,CUPN,DCAN,FLXN2O)
    !     RECALCULATE GAS CONCENTRATIONS IN LIQUID AND GAS PHASES
          DO J=1,NBCL
              AO2C(J)=CGO2(J)*AFP(J)+CLO2(J)*VWC(J)
              ACO2C(J)=CGCO2(J)*AFP(J)+CLCO2(J)*VWC(J)
              AN2OC(J)=CGN2O(J)*AFP(J)+CLN2O(J)*VWC(J)
			  WN2O(J)=AN2OC(J)*DZ10
              X1=AFP(J)*DZ10
              WO2GA=WO2GA+CGO2(J)*X1
              WCO2GA=WCO2GA+CGCO2(J)*X1
              WN2OGA=WN2OGA+CGN2O(J)*X1
          END DO
          DFO2T = DFO2T  + (WO2GA-WO2GB) * DTG
          DFCO2T= DFCO2T + (WCO2GA-WCO2GB) * DTG
          DFN2OT= DFN2OT + (WN2OGA-WN2OGB) * DTG
          DFO2S = DFO2S  + FLXO2 *  DTG10 ! factor of 10 = g/m^2 -> kg/ha
          DFCO2S= DFCO2S + FLXCO2 * DTG10
          DFN2OS= DFN2OS + FLXN2O * DTG10
          DFO2B=DFO2T-DFO2S
          DFCO2B=DFCO2T-DFCO2S
          DFN2OB=DFN2OT-DFN2OS
          CALL NCCONC
      END DO
      SN2O=0.
      SN2=0.
      SDN=0.
      AD7=0.
      AD8=0.
      AD9=0.
      TOT3=0.
      DO J=1,NBCL
          IF(SMES(J)>0.)THEN
              EAR(J)=SMEA(J)/SMES(J)
          ELSE
              EAR(J)=1.
          END IF
          X1=AFP(J)*DZ10
          WO2G(J)=CGO2(J)*X1
          WCO2G(J)=CGCO2(J)*X1
          WN2OG(J)=CGN2O(J)*X1
          X1=VWC(J)*DZ10
          WO2L(J)=CLO2(J)*X1
          WCO2L(J)=CLCO2(J)*X1
          WN2OL(J)=CLN2O(J)*X1
          !IF(VWC(J)>VFC(J).AND.SOT(J)>0.)THEN
          SN2=SN2+DN2G(J)
          SN2O=SN2O+DN2OG(J)
          SDN=SDN+DN2OG(J)+DN2G(J)
          !END IF
          XTP1(J)=WNO3(J)
          XTP2(J)=WNO2(J)
          XTP3(J)=WN2O(J)
          XTP4(J)=WBMC(J)
          XTP5(J)=EAR(J)
          XTP6(J)=WN2OG(J)
          XTP7(J)=WN2OL(J)
          XTP8(J)=DN2G(J)
          XTP9(J)=DN2OG(J)
          XTP10(J)=RSPC(J)
          XTP14(J)=SSFN2O(J)
          XTP17(J)=VN2O(J)
          AD7=AD7+WN2O(J)
          AD8=AD8+WN2OG(J)
          AD9=AD9+WN2OL(J)
          TOT3=TOT3+WBMC(J)
      END DO
      IF(KFL(30)>0)WRITE(KW(30),19)IYR,MO,KDA,VAR(4),(VWC(K),K=1,NBCL),&
      (AFP(K),K=1,NBCL),(SMEA(K),K=1,NBCL),(SMES(K),K=1,NBCL),(EAR(K),&
      K=1,NBCL),(WNO3(K),K=1,NBCL),(WNO2(K),K=1,NBCL),(WO2L(K),K=1,NBCL),&
      (WO2G(K),K=1,NBCL),(DO2CONS(K),K=1,NBCL),DFO2S,DFO2B,DFO2T,QO2,(SSFO2(K),&
      K=1,NBCL),(VO2(K),K=1,NBCL),(WCO2L(K),K=1,NBCL),(WCO2G(K),K=1,NBCL),&
      (DCO2GEN(K),K=1,NBCL),DFCO2S,DFCO2B,DFCO2T,QCO2,(SSFCO2(K),K=1,NBCL),(VCO2&
      (K),K=1,NBCL),(WN2OL(K),K=1,NBCL),(WN2OG(K),K=1,NBCL),DFN2OS,DFN2OB,&
      DFN2OT,QN2O,(SSFN2O(K),K=1,NBCL),(VN2O(K),K=1,NBCL),(DN2OG(K),K=1,NBCL),&
      (DN2G(K),K=1,NBCL)
      CALL AINTRO(XTP1,WNO3,NBSL,NBCL)
      CALL AINTRO(XTP2,WNO2,NBSL,NBCL)
      CALL AINTRO(XTP3,WN2O,NBSL,NBCL)
      CALL AINTRO(XTP4,WBMC,NBSL,NBCL)
      CALL AINTRX(XTP5,EAR,NBSL,NBCL)
      CALL AINTRO(XTP6,WN2OG,NBSL,NBCL)
      CALL AINTRO(XTP7,WN2OL,NBSL,NBCL)
      CALL AINTRO(XTP14,SSFN2O,NBSL,NBCL)
      CALL AINTRO(XTP17,VN2O,NBSL,NBCL)
      CALL AINTRO(XTP10,RSPC,NBSL,NBCL)
      SM2=0.
      AD10=0.
      AD11=0.
      AD12=0.
      TOT4=0.
      DO J=1,NBSL
          L=LID(J)
          SM2=SM2+WNO3(L)+WNO2(L)+WN2OG(L)+WN2OL(L)
          AD10=AD10+WN2O(L)
          AD11=AD11+WN2OG(L)
          AD12=AD12+WN2OL(L)
          TOT4=TOT4+WBMC(L)
      END DO
 !     IF(ABS(AD1-AD4)>1.E-5)WRITE(KW(1),'(1X,3I4,A,E13.6,A,E13.6)')IY,MO,KDA,'^^^^^ A1=',AD1,'  A4=',AD4
 !     IF(ABS(AD2-AD5)>1.E-5)WRITE(KW(1),'(1X,3I4,A,E13.6,A,E13.6)')IY,MO,KDA,'^^^^^ A2=',AD2,'  A5=',AD5
 !     IF(ABS(AD3-AD6)>1.E-5)WRITE(KW(1),'(1X,3I4,A,E13.6,A,E13.6)')IY,MO,KDA,'^^^^^ A3=',AD3,'  A6=',AD6
 !     IF(ABS(AD7-AD10)>1.E-5)WRITE(KW(1),'(1X,3I4,A,E13.6,A,E13.6)')IY,MO,KDA,'^^^^^ A7=',AD7,'  A10=',AD10
 !     IF(ABS(AD8-AD11)>1.E-5)WRITE(KW(1),'(1X,3I4,A,E13.6,A,E13.6)')IY,MO,KDA,'^^^^^ A8=',AD8,'  A11=',AD11
 !     IF(ABS(AD9-AD12)>1.E-5)WRITE(KW(1),'(1X,3I4,A,E13.6,A,E13.6)')IY,MO,KDA,'^^^^^ A9=',AD9,'  A12=',AD12
 !     IF(ABS((TOT1-TOT2)/TOT1)>1.E-5)WRITE(KW(1),4)IY,MO,KDA,'^^^^^ TOT1=',TOT1,' TOT2=',TOT2
 !     IF(ABS((TOT3-TOT4)/TOT3)>1.E-5)WRITE(KW(1),4)IY,MO,KDA,'^^^^^ TOT3=',TOT3,' TOT4=',TOT4
      DF=SM1-SM2-SN2+DFN2OT
 !     IF(ABS(DF/SM1)>1.E-3)THEN
 !         WRITE(KW(1),3)IY,MO1,KDA,SM1,SDN,SM2,SN2O,SN2,DFN2OT,DF
 !     END IF
      RETURN
    1 FORMAT(1X,'GASDF31',3I4,3E16.6)            
    2 FORMAT(1X,'GASDF32',3I4,4E16.6)
    3 FORMAT(4X,'GAS XBAL ',3I4,2X,'BTOT=',F11.6,2X,'DNIT=',F11.6,2X,'FTOT=',&
      F11.6,2X,'SN2O=',F11.6,2X,'SN2=',F11.6,2X,'DFN2OT=',F11.6,2X,'DF=',F11.6)      
    4 FORMAT(1X,3I4,A12,E13.6,A6,E13.6)          
   10 FORMAT(1X,'!!!!!',3I4,9E12.5)
   11 FORMAT(1X,'$$$$$',3I4,10E12.5)
   12 FORMAT(1X,'#####',3I4,9E12.5)
   18 FORMAT(9F10.3)
   19 FORMAT(1X,3I4,F9.1,1000E13.5) 
      END
