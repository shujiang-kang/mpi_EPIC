      FUNCTION CAHU(J,K,BASE,NHS)
!     EPIC1102
!     THIS SUBPROGRAM ACCUMULATES HEAT UNITS FOR USE IN CPTHU.
      USE PARM
      CAHU=0.
      MO=1
      DO JDA=J,K
          CALL AXMON(JDA,MO)
          IF(JDHU<=366)THEN
              CALL WHRL
              IF(HRLT<WDRM.AND.NHS==0)CYCLE
          END IF
          TA=ARALT(TAV,XX)
          TGX=TA-BASE
          IF(TGX>0.)CAHU=CAHU+TGX
      END DO
      RETURN
      END