      SUBROUTINE HEVP
!     EPIC1102
!     THIS SUBPROGRAM ESTIMATES DAILY EVAPOTRANSPIRATION.  THERE ARE
!     FOUR OPTIONS FOR COMPUTING POTENTIAL EVAP(PENMAN-MONTEITH, PENMAN,
!     PRIESTLEY-TAYLOR, & HARGREAVES)
      USE PARM
      SUM=.01
      EPP=0.
      NN=NBC(IRO)
      CHMX=0.
      DO K=1,NN
          K1=JE(K)
          IF(K1>MNC)CYCLE
          SUM=SUM+SLAI(K1)
          IF(CHT(K1)>CHMX)CHMX=CHT(K1)
          EP(K1)=0.
      END DO
      X1=MAX(.4*SUM,PRMT(41)*(CV+.1))
      EAJ=EXP(-X1)
      IF(SNO>5.)THEN
          ALB=.6
          EAJ=.5
      ELSE
	      IF(ICV>0)THEN
	          X1=1.-EAJ
	          ALB=(.23*X1+PALB*FCV)/(FCV+X1)
	          EAJ=MIN(EAJ,1.-FCV)
	      ELSE
	          ALB=.23*(1.-EAJ)+SALB*EAJ
          END IF
      END IF          
      TK=TX+273.
      RSO=RAMX
      XL=2.501-2.2E-3*TX
      EA=ASVP(TK)
      ED=EA*RHD
      VPD=EA-ED
      SMM(9,MO)=SMM(9,MO)+VPD
      VAR(9)=VPD
      RALB1=SRAD*(1.-ALB)
      DLT=EA*(6790.5/TK-5.029)/TK
      XX=DLT+GMA
      TK4=TK**4
      RBO=(.34-.14*SQRT(ED))*4.9E-9*TK4
      RTO=MIN(.99,SRAD/(RSO+.1))
      RN=RALB1-RBO*(.9*RTO+.1)
      X2=RN*DLT
      SELECT CASE(IET)
          CASE(5)
    !         BAIER-ROBERTSON PET METHOD
              EO=MAX(0.,.288*TMX-.144*TMN+.139*RSO-4.931)
          CASE(4)
    !         HARGREAVES PET METHOD
              RAMM=RSO/XL
              EO=MAX(0.,PRMT(38)*RAMM*(TX+17.8)*(TMX-TMN)**PRMT(13))
	          EO=MIN(9.,EO)
          CASE(3)
    !         PRIESTLEY-TAYLOR PET METHOD
              RAMM=RALB1/XL
              EO=1.28*RAMM*DLT/XX
          CASE(2)
    !         PENMAN PET METHOD
              FWV=2.7+1.63*U10
              X3=GMA*FWV*VPD
              X1=X2/XL+X3
              EO=X1/XX
          CASE(1)
    !         PENMAN-MONTEITH PET METHOD
              RHO=.01276*PB/(1.+.00367*TX)
              IF(IGO==0)GO TO 12
              IF(CHMX<8.)THEN
                  UZZ=U10
                  ZZ=10.
              ELSE
                  ZZ=CHMX+2.
                  UZZ=U10*LOG(ZZ/.0005)/9.9035
              END IF
              X1=LOG10(CHMX+.01)
              Z0=10.**(.997*X1-.883)
              ZD=10.**(.979*X1-.154)
              RV=6.25*(LOG((ZZ-ZD)/Z0))**2/UZZ
              X3=VPD-VPTH(JJK)
              IF(X3>0.)THEN
                  FVPD=MAX(.1,1.-VPD2(JJK)*X3)
              ELSE
                  FVPD=1.
              END IF
              G1=GSI(JJK)*FVPD
              RC=PRMT(1)/((SUM+.01)*G1*EXP(.00155*(330.-CO2)))
              EPP=PRMT(74)*(X2+86.66*RHO*VPD/RV)/(XL*(DLT+GMA*(1.+RC/RV)))
       12     RV=350./U10
              EO=PRMT(74)*(X2+86.66*RHO*VPD/RV)/(XL*XX)
              IF(EPP>EO)EO=EPP
          CASE DEFAULT              
          !         HARGREAVES PET METHOD
              RAMM=RSO/XL
              EO=MAX(0.,PRMT(38)*RAMM*(TX+17.8)*(TMX-TMN)**PRMT(13))
	          EO=MIN(9.,EO)    
      END SELECT
      IF(IET>1)EPP=MIN(EO,EO*SUM/3.)
      IF(IGO>0)THEN
          XX=EPP/SUM
          DO K=1,NN
              K1=JE(K)
              IF(K1>MNC)CYCLE
              EP(K1)=SLAI(K1)*XX
          END DO
      END IF
      ES=EO*EAJ
      ST0=RALB1
      ES=MIN(ES,ES*EO/(ES+EPP+1.E-10))
      IF(SNO>=ES)GO TO 21
      XX=ES-SNO
      ES=SNO
      SNO=0.
      TOT=0.
      DO J=1,NBSL
          ISL=LID(J)
          RTO=1000.*Z(ISL)
          SUM=XX*RTO/(RTO+EXP(SCRP(2,1)-SCRP(2,2)*RTO))
          XZ=FC(ISL)-S15(ISL)
          IF(ST(ISL)<FC(ISL))THEN
              F=EXP(PRMT(12)*(ST(ISL)-FC(ISL))/XZ)
          ELSE
              F=1.
          END IF
          ZZ=SUM-PRMT(61)*TOT-(1.-PRMT(61))*ES
          SEV(ISL)=ZZ*F
          XY=PRMT(5)*S15(ISL)
          IF(Z(ISL)>.2)GO TO 20
          IF(ST(ISL)-SEV(ISL)<XY)SEV(ISL)=ST(ISL)-XY-1.E-5
          ES=ES+SEV(ISL)
          ST(ISL)=ST(ISL)-SEV(ISL)
          TOT=SUM
      END DO
      J=NBSL
   20 NEV=J
      Z1=Z(LID(J-1))
      RTO=(.2-Z1)/(Z(ISL)-Z1)
      X1=RTO*ST(ISL)
      X2=RTO*XY
      IF(X1-SEV(ISL)<X2)SEV(ISL)=X1-X2
      ES=ES+SEV(ISL)
      ST(ISL)=MAX(1.E-5,ST(ISL)-SEV(ISL))
      GO TO 23
   21 SNO=SNO-ES
      NEV=1
   23 XX=MAX(0.,EO-ES)
      IF(EPP>XX)THEN
          X1=XX/EPP
          EPP=XX
          DO K=1,NN
              K1=JE(K)
              IF(K1>MNC)CYCLE
              EP(K1)=EP(K1)*X1
          END DO
      END IF
      VAR(12)=EPP
      SMM(12,MO)=SMM(12,MO)+EPP
      RETURN
      END