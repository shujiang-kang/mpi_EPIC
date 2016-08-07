      SUBROUTINE ERSC
!     EPCM1102.FOR
!     THIS SUBPROGRAM ESTIMATES THE RUSLE C FACTOR DAILY.
      USE PARM
      NN=NBC(IRO)
      SUM=0.
      TOT=0.
      DO I=2,NBSL
          L=LID(I)
          IF(Z(L)>PMX)GO TO 3
          IF(I>1)TOT=TOT+RSD(L)
          DO K=1,NN
              IF(JE(K)>MNC)CYCLE
              SUM=SUM+RWT(L,JE(K))
          END DO
      END DO
      GO TO 4
    3 KK=LID(I-1)
      RTO=(PMX-Z(KK))/(Z(L)-Z(KK))
      TOT=TOT+RTO*RSD(L)
      DO K=1,NN
          IF(JE(K)>MNC)CYCLE
          SUM=SUM+RTO*RWT(L,JE(K))
      END DO
    4 SUM=SUM/PMX
      TOT=TOT/(PMX-Z(LD1))
	  PLU=.951*RCF*EXP(-.0451*SUM-.00943*TOT/SQRT(RCF))            
      FRUF=MIN(1.,EXP(-.026*(RRUF-6.1)))
      IF(CVRS<15.)THEN
          FRSD=EXP(-PRMT(23)*CVRS)
      ELSE
          FRSD=.0001
      END IF    
      FBIO=1.-FGC*EXP(-PRMT(26)*CHT(JJK))
      SLR=MAX(1.E-10,FRSD*FBIO*FRUF)
      RETURN
      END