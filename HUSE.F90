      SUBROUTINE HUSE
!     EPIC1102
!     THIS SUBPROGRAM IS THE MASTER WATER AND NUTRIENT USE SUBPROGRAM.
!     CALLS HSWU AND NUPPO FOR EACH SOIL LAYER.
      USE PARM
	  LRD=1
      IAR=0
      UX=0.
      SEP=0.
      TOT=0.
      CPWU=1.
      RGS=1.
      DO J=1,NBSL
          ISL=LID(J)
          IF(Z(ISL)<1.)GO TO 5
          IF(IAR>0)GO TO 6
          IAR=1
          X3=1.-SEP
          X4=Z(ISL)-SEP
          RTO=X3/X4
          IF(WTBL<=Z(ISL))THEN
              X1=PO(ISL)*(Z(ISL)-WTBL)/X4
              X2=ST(ISL)-X1
              IF(WTBL>1.)THEN
                  SAT=SAT+X2*X3/(WTBL-SEP)
              ELSE
                  SAT=SAT+X2+PO(ISL)*(1.-WTBL)/X4
              END IF
              GO TO 4
          END IF
          SAT=SAT+RTO*ST(ISL)
        4 TOT=TOT+RTO*PO(ISL)
          GO TO 6
        5 SAT=SAT+ST(ISL)
          TOT=TOT+PO(ISL)
          SEP=Z(ISL)
        6 IF(LRD>1)CYCLE
          IF(RD(JJK)>Z(ISL))THEN
              GX=Z(ISL)
          ELSE
              GX=RD(JJK)
              LRD=J
          END IF
          CALL HSWU(CPWU,RGS)
          SU=SU+U(ISL)
      END DO
      IF(LRD==0)LRD=NBSL
      IF(RZSW>PAW)THEN
          RTO=MIN(1.,SAT/TOT)
          F=100.*(RTO-CAF(JJK))/(1.0001-CAF(JJK))
          IF(F>0.)THEN
              SAT=1.-F/(F+EXP(SCRP(7,1)-SCRP(7,2)*F))
              RETURN
          END IF
      ELSE
          SAT=1.
      END IF
      RETURN
      END