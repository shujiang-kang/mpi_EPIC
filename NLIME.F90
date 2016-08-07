      SUBROUTINE NLIME(TLX)
!     EPIC1102
!     THIS SUBPROGRAM APPLIES LIME WHEN THE SUM OF THE SOIL LIME REQUIRE
!     MENT AND ACCUMULATED LIME REQUIREMENT CAUSED BY N FERTILIZER EXCEED
!     4 t/ha.
      USE PARM
      DATA SMFN/0./
      OC=0.
      TOT=0.
      XZ=0.
      ZZ=0.
      XX=0.
      DO J=1,NBSL
          ISL=LID(J)
          IF(Z(ISL)>BIG)EXIT
          XY=WT(ISL)
          OC=OC+.1*WOC(ISL)
          XZ=XZ+CEC(ISL)*XY
          TOT=TOT+PH(ISL)*XY
          ZZ=ZZ+SMB(ISL)*XY
          XX=XX+XY
      END DO
      IF(J>NBSL)THEN
          J=NBSL
          ISL=LID(NBSL)
      ELSE
          L1=LID(J-1)
          W3=Z(ISL)-Z(L1)
          W2=BIG-Z(L1)
          RTO=W2*WT(ISL)/W3
          X1=.1*WOC(ISL)/WT(ISL)
          OC=OC+RTO*X1
          TOT=TOT+RTO*PH(ISL)
          ZZ=ZZ+RTO*SMB(ISL)
          XZ=XZ+RTO*CEC(ISL)
          XX=XX+RTO
      END IF
      XZ=XZ/XX
      OC=OC/XX
      TOT=TOT/XX
      ZZ=ZZ/XX
      XY=.001*XX
      X1=SMY(60)+SMY(61)
      DSB=.036*(SMY(50)+X1-SMFN)/XX
      SMFN=X1
      BS=100./XZ
      TOT=TOT-.05*DSB*BS
      CALL NLIMA(ZZ,DSB,BS,TOT,ALSX,OC,BSA)
      IF(TOT>6.5.AND.TLX<1.E-5)GO TO 6
      IF(IDSP==4)THEN
          EAL=.01*ALSX*XZ
          IF(TLX>0.)THEN
              TLZ=TLX
          ELSE
              TLZ=EAL*XY
              IF(TLZ<1.)GO TO 6
          END IF
          TOT=5.4
          CALL NLIMA(ZZ,-EAL,BS,TOT,ALSX,OC,BSA)
          GO TO 7
      END IF
      DBS=MIN((6.5-TOT)/.023,90.-BSA)
      ALN=0.
      RTO=1.
      IF(TLX>0.)THEN
          TLZ=TLX
      ELSE          
          TLZ=DBS*XY/BS
          IF(TLZ>2.)THEN
              RTO=2./TLZ
              TLZ=2.
          ELSE
	          IF(TOT>5.)GO TO 6
	      END IF
	  END IF
      PHN=(6.5-TOT)*RTO+TOT
      DBS=MIN((PHN-TOT)/.023,90.-BSA)
      BSA=(BSA+DBS)/BS
      GO TO 8
    6 TLZ=0.
    7 ALN=ALSX
      PHN=TOT
      BSA=ZZ
    8 TLA=TLZ  
      DO K=1,J
          ISL=LID(K)
          TOT=SMB(ISL)
          XZ=PH(ISL)
          ALSX=ALS(ISL)
          SMB(ISL)=BSA
          PH(ISL)=PHN
          ALS(ISL)=ALN
      END DO
      IF(J==NBSL)RETURN
      ISL=LID(J)
      W1=Z(ISL)-BIG
      SMB(ISL)=(W1*TOT+W2*SMB(ISL))/W3
      PH(ISL)=(W1*XZ+W2*PH(ISL))/W3
      ALS(ISL)=MAX(.001,(W1*ALSX+W2*ALS(ISL))/W3)
      RETURN
      END