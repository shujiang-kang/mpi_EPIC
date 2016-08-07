      SUBROUTINE HWTBL
!     EPIC1102
!     THIS SUBPROGRAM SIMULATES WATER TABLE DYNAMICS AS A FUNCTION
!     OF RAIN AND EVAP.
      USE PARM
      RTO=100.*GWST/GWMX
      WTBL=WTMX+(WTMN-WTMX)*RTO/(RTO+EXP(SCRP(19,1)-SCRP(19,2)*RTO))
      SUM=0.
      TOT=0.
      IF(WTBL<=Z(LID(NBSL)))THEN
          XX=0.
          NN=0
          DO K=1,NBSL
              ISL=LID(K)
              SUM=SUM+ST(ISL)
              IF(WTBL<=Z(ISL))THEN
                  IF(NN>0)THEN
                      ST(ISL)=PO(ISL)
                  ELSE
                      NN=1
                      IF(ST(ISL)>FC(ISL))ST(ISL)=FC(ISL)
                      ST(ISL)=(ST(ISL)*(WTBL-XX)+PO(ISL)*(Z(ISL)-WTBL))/&
                      &(Z(ISL)-XX)
                  END IF
              END IF
              TOT=TOT+ST(ISL)
              XX=Z(ISL)
          END DO
      END IF
      XX=TOT-SUM
      QIN(MO)=QIN(MO)+XX
      SMM(20,MO)=SMM(20,MO)+XX
      VAR(20)=XX
      RETURN
      END