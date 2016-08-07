      SUBROUTINE EYSED
!     EPIC1102
!     THIS SUBPROGRAM PREDICTS DAILY SOIL LOSS CAUSED BY WATER EROSION
!     AND ESTIMATES THE NUTRIENT ENRICHMENT RATIO.
      USE PARM
	  CALL ERSC
      CALL EYCC
      CVX=CVF
      IF(ICF==0)CVX=SLR
      F=1.
      XX=STMP(LID(2))
      IF(XX<=0.)THEN
          XX=273.+XX
          F=XX/(XX+EXP(SCRP(18,1)-SCRP(18,2)*XX))
      END IF
      XX=CVX*USL*F
      YLM=100.*RFV
      IF(RFV>12.7)THEN
          YSD(3)=MIN(YLM,EI*XX*1.292)
          SMM(29,MO)=SMM(29,MO)+CVF*EI
          SMM(28,MO)=SMM(28,MO)+EI
	      X1=SLR*EI
          SMM(37,MO)=SMM(37,MO)+X1
          YSD(7)=MIN(YLM,X1*RSLK)
	      IF(YSD(7)<1.E-10)GO TO 2
	      B=BETA
	      RXM=B/(1.+B)
	      RLFX=UPSX**RXM
	      SMM(90,MO)=SMM(90,MO)+RLFX*RSF*EI
	      SMM(91,MO)=SMM(91,MO)+REK*EI
	      YI=MIN(YLM,.5*X1*RSK)
	      SUM=PSZ(1)*SAN(LD1)
	      SUM=SUM+PSZ(2)*SIL(LD1)
	      SUM=SUM+PSZ(3)*CLA(LD1)
	      SUM=.01*PRMT(71)*SUM/(QPR+1.E-5)
	      T2=PRMT(72)*QPR*UPS
	      IF(T2>YI)THEN
	          YSD(8)=1.5*X1*RLFX*RSK
	          RUSM(1)=RUSM(1)+YSD(8)
	      ELSE
	          YSD(8)=MAX(0.,YI+SUM*(T2-YI))
	          RUSM(2)=RUSM(2)+YSD(8)
	          RUSM(3)=RUSM(3)+YI
	      END IF
	  END IF
    2 IF(QD<1.)RETURN
      REP=REP*(QD/RWO)**.1
      YSD(2)=MIN(YLM,(.646*EI+.45*QD*QP**.3333)*XX)
      QQ=QD*QP
      YSD(4)=MIN(YLM,YSW*QQ**.65*XX)
      YSD(1)=MIN(YLM,2.5*SQRT(QQ)*XX)
      YSD(6)=MIN(YLM,BUS(1)*QD**BUS(2)*QP**BUS(3)*XX)
      CX(MO)=CX(MO)+1.
      TAL(MO)=TAL(MO)+AL5
      YSD(5)=MIN(YLM,WSA1*QQ**.56*XX)
      DR=SQRT(QP/REP)
      CY=.1*YSD(4)/QD+1.E-10
      IF(IERT>0)THEN
          ER=.78*CY**(-.2468)
      ELSE
          DR1=1./DR
          B2=-LOG10(DR1)/2.699
          B1=1./.1**B2
          ER=MAX(1.,B1*(CY+1.E-4)**B2)
          ER=MIN(ER,3.)
      END IF
      SMM(58,MO)=SMM(58,MO)+ER
      RETURN
      END