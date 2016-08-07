      SUBROUTINE PSTCY
!     EPIC1102
!     THIS SUBPROGRAM SIMULATES PESTICIDE TRANSPORT & DEGRADATION
      USE PARM
      DIMENSION NXP(90),NY(5)
      DATA NXP/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,&
     &23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,&
     &44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,&
     &65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,&
     &86,87,88,89,90/,NY/1,4,21,60,90/
      II=NXP(90)
      QQ=QD+SST
      Y2=YSD(NDRV)
      SQB(1)=QQ
      SYB(1)=Y2
      DO I=2,5
          SQB(I)=SQB(I)+QQ-VQ(NXP(NY(I)))
          SYB(I)=SYB(I)+Y2-VY(NXP(NY(I)))
      END DO
      DO 9 K=1,NDP
      ADD=0.
      SUM=0.
      TOT=0.
      X3=0.
      PQ=0.
      PY=0.
      Y1=PSTZ(K,LD1)
      IF(IGO>0)THEN
          IF(PFOL(K)>.01)THEN
              IF(RWO>2.54)THEN
!                 COMPUTE PESTICIDE WASH OFF FROM FOLIAGE
                  WO=PWOF(K)*PFOL(K)  
                  PFOL(K)=PFOL(K)-WO
                  Y1=Y1+WO
              END IF
  !           COMPUTE PESTICIDE DEGRADATION FROM FOLIAGE
              DGF=PFOL(K)*PHLF(K)
              PFOL(K)=PFOL(K)-DGF
              SMMP(6,K,MO)=SMMP(6,K,MO)+DGF
              VARP(6,K)=DGF
          ELSE
              PFOL(K)=0.
          END IF
      END IF
!     COMPUTE PESTICIDE LOSS FROM TOP SOIL LAYER IN RUNOFF,
!     LATERAL SUBSURFACE FLOW, & PERCOLATION
      IF(Y1>.01)THEN
          DK=.0001*PKOC(K)*WOC(LD1)
          X1=PO(LD1)-S15(LD1)
          XX=X1+DK
          V=QD+SSF(LD1)+PKRZ(LD1)
          IF(V>0.)THEN
              VPST=Y1*(1.-EXP(-V/XX))
              CO=MIN(PSOL(K),VPST/(PKRZ(LD1)+PRMT(18)*(QD+SSF(LD1))))
              CS=PRMT(18)*CO
              X3=CO*PKRZ(LD1)
              PQ=CS*QD
              SMMP(2,K,MO)=SMMP(2,K,MO)+PQ
              VARP(2,K)=PQ
              SUM=CS*SSF(LD1)
	          Y1=Y1-X3-PQ-SUM
  !           WRITE(KW(1),3)IYR,MO,KDA,QD,SSF(LD1),Y1,VPST,CO,SUM,PQ
!             COMPUTE PESTICIDE LOSS WITH SEDIMENT
              IF(YEW>0.)THEN
                  CS=DK*Y1/XX
                  PY=YEW*CS
                  SMMP(5,K,MO)=SMMP(5,K,MO)+PY
                  VARP(5,K)=PY
                  Y1=Y1-PY
              END IF
          END IF
!         COMPUTE PESTICIDE DEGRADATION IN TOP SOIL LAYER
          DGS=Y1*PHLS(K)
          Y1=Y1-DGS
          TOT=DGS
          ADD=Y1
      ELSE
          Y1=0.
      END IF
      PSTZ(K,LD1)=Y1
!     COMPUTE PESTICIDE MOVEMENT THRU SOIL LAYERS BY LATERAL
!     SUBSURFACE FLOW & PERCOLATION
!     COMPUTE PESTICIDE MOVEMENT THRU SOIL LAYERS BY LATERAL
      X2=0.
      DO L1=2,NBSL
          ISL=LID(L1)
          Y1=PSTZ(K,ISL)
          Y1=Y1+X3
          X3=0.
          IF(Y1>.01)THEN
              V=PKRZ(ISL)+SSF(ISL)
              IF(V>0.)THEN
                  VPST=Y1*(1.-EXP(-V/(PO(ISL)-S15(ISL)+.0001*PKOC(K)*WOC&
                  &(ISL))))
                  CO=MIN(PSOL(K),VPST/(PKRZ(ISL)+PRMT(18)*SSF(ISL)))
	              CS=PRMT(18)*CO
                  X4=CS*SSF(ISL)
                  IF(ISL==IDR)THEN
                      SMMP(10,K,MO)=SMMP(10,K,MO)+X4
                      VARP(10,K)=X4
                  END IF
                  SUM=SUM+X4
                  X3=CO*PKRZ(ISL)
                  IF(L1==NBSL)X2=X3
                  Y1=Y1-X4-X3
              ELSE
!             COMPUTE PESTICIDE DEGRADATION IN SOIL LAYERS
                  DGS=Y1*PHLS(K)
                  Y1=Y1-DGS
                  TOT=TOT+DGS
                  ADD=ADD+Y1
              END IF
          ELSE
              Y1=0.
          END IF
          PSTZ(K,ISL)=Y1
      END DO
      SMMP(3,K,MO)=SMMP(3,K,MO)+X2
      VARP(3,K)=X2
      SMMP(4,K,MO)=SMMP(4,K,MO)+SUM
      VARP(4,K)=SUM
      SMMP(7,K,MO)=SMMP(7,K,MO)+TOT
      VARP(7,K)=TOT
      SMMP(9,K,MO)=ADD
      VARP(9,K)=ADD
      PLCH(K)=X2
      SSPS(K)=SUM
      PQST=PQ+SUM
      SPQ(1,K)=PQST
      SPY(1,K)=PY
      DO I=2,5
          SPQ(I,K)=SPQ(I,K)+PQST-PVQ(K,NXP(NY(I)))
	      IF(SPQ(I,K)<1.E-3.OR.SQB(I)<1.E-3)THEN
	          SPQC(I,K)=0.
          ELSE
	          SPQC(I,K)=100.*SPQ(I,K)/SQB(I)
	      END IF
	      SPY(I,K)=SPY(I,K)+PY-PVY(K,NXP(NY(I)))
      END DO
      DO I=1,5
          IF(APQ(I,K,IY)<=SPQ(I,K))THEN
              APQ(I,K,IY)=SPQ(I,K)
              AQB(I,K,IY)=SQB(I)
          END IF
          IF(APQC(I,K,IY)<SPQC(I,K))APQC(I,K,IY)=SPQC(I,K)
          IF(APY(I,K,IY)>SPY(I,K))CYCLE
          APY(I,K,IY)=SPY(I,K)
          AYB(I,K,IY)=SYB(I)
      END DO
      PVQ(K,II)=PQST
      PVY(K,II)=PY
      VQ(II)=QQ
      VY(II)=Y2
    9 CONTINUE
      DO I=90,2,-1
          I1=I-1
          NXP(I)=NXP(I1)
      END DO
      NXP(1)=II
      RETURN
    4 FORMAT(25I4)
   11 FORMAT(1X,10E13.5)
      END