      SUBROUTINE NAJN(UU,AN,DMD,SUPL,AJF,IAJ)
!     EPIC1102
!     THIS SUBPROGRAM COMPUTES ACTUAL N PLANT UPTAKE FROM EACH
!     LAYER (UPTAKE = MINIMUM OF PLANT DEMAND AND SOIL SUPPLY).
      USE PARM
      DIMENSION UU(15),AN(15)
      IF(IAJ==0)THEN
          SUM=0.
          X2=DMD/(SUPL+1.E-20)
          IF(X2>1..OR.DMD<0.)GO TO 4
          DO J=1,LRD
              K=LID(J)
              UU(K)=UU(K)*X2
              SUM=SUM+UU(K)
          END DO
          SUPL=SUM
          RETURN
      END IF
      SUM=0.
    4 X2=AJF*(DMD-SUPL)
      X21=X2
      DO J=1,LRD
          K=LID(J)
          XX=UU(K)+X2
          X1=AN(K)-.001*PRMT(27)*WT(K)
          IF(XX<X1)GO TO 6
          IF(X1>0.)THEN
              X2=X2-X1+UU(K)
              UU(K)=X1
              SUM=SUM+UU(K)
          ELSE
              UU(K)=0.
          END IF
      END DO
      SUPL=SUM
      RETURN
    6 UU(K)=XX
      SUPL=SUPL+X21
      RETURN
      END