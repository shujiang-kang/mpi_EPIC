      SUBROUTINE HTR55
!     EPIC1102
!     THIS SUBPROGRAM ESTIMATES PEAK RUNOFF RATES USING THE SCS TR55
!     EXTENDED METHOD.
      USE PARM
      DIMENSION PIAF(18)
      DATA PIAF/0.0,0.1,0.2,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75&
     &,0.8,0.85,0.9,0.95,1.0/
      IF(QP>.35)THEN
          XOX=(QP-.35)/.05+5.
      ELSE
          XOX=QP/.1+1.
      END IF
      INT=XOX
      INT1=INT+1
      RTO=(QP-PIAF(INT))/(PIAF(INT1)-PIAF(INT))
      X1=LOG(TC)
      Y1=HQP(X1,CQP,ITYP,INT)
      IF(INT<17)THEN
          Y2=HQP(X1,CQP,ITYP,INT1)
          Y=Y1+(Y2-Y1)*RTO
          Y=EXP(Y)
      ELSE
          Y=EXP(Y1)*(1.0-RTO)
      END IF
      QP=Y*RFV
      RETURN
      END