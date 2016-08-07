      SUBROUTINE NFIX
!     EPIC1102
!     THIS SUBPROGRAM ESTIMATES N FIXATION FOR LEGUMES.
      USE PARM
      IF(HUI(JJK)<.15.OR.HUI(JJK)>.75)GO TO 8
      SUM=0.
      TOT=0.
      ADD=0.
      DO J=1,NBSL
          ISL=LID(J)
          IF(Z(ISL)>.3)GO TO 3
          SUM=SUM+ST(ISL)-S15(ISL)
          TOT=TOT+FC(ISL)-S15(ISL)
      END DO
      GO TO 4
    3 L1=LID(J-1)
      RTO=(.3-Z(L1))/(Z(ISL)-Z(L1))
      SUM=SUM+(ST(ISL)-S15(ISL))*RTO
      TOT=TOT+(FC(ISL)-S15(ISL))*RTO
    4 X1=SUM/TOT
      IF(X1<=.25)GO TO 8
      DO J=1,NBSL
          ISL=LID(J)
          IF(Z(ISL)>RD(JJK))GO TO 6
          ADD=ADD+WNO3(ISL)
      END DO
      GO TO 7
    6 L1=LID(J-1)
      RTO=(RD(JJK)-Z(L1))/(Z(ISL)-Z(L1))
      ADD=ADD+WNO3(ISL)*RTO
    7 FXN=1.5-.005*ADD/RD(JJK)
      IF(FXN>0.)THEN
          FXW=1.333*X1-.333
          FXG=(HUI(JJK)-.1)*5.
          FXS=4.-5.*HUI(JJK)
          FXP=MIN(FXG,FXS,1.)
          FIXR=MIN(FXW,FXN,1.)*FXP
          WFX=FIXR*UNO3
      END IF
    8 WFX=MAX(0.,PRMT(7)*WFX+(1.-PRMT(7))*UNO3)
      IF(WFX>PRMT(68))WFX=PRMT(68)
      SMM(50,MO)=SMM(50,MO)+WFX
      DFX=DFX+WFX
      UNO3=UNO3-WFX
      RETURN
      END