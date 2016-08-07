      SUBROUTINE SAJBD
!     EPIC1102
!     THIS SUBPROGRAM SIMULATES THE CHANGE IN BULK DENSITY WITHIN THE
!     PLOW LAYER CAUSED BY INFILTRATION SETTLING.
      USE PARM
      XX=RFV-QD
	  DO J=1,NBSL
          ISL=LID(J)
          IF(XX>0..AND.BDP(ISL)<BD(ISL))THEN
              XX=XX*.2*(1.+2.*SAN(ISL)/(SAN(ISL)+EXP(8.597-.075*SAN(ISL))))&
              &/Z(ISL)**1.6
              IF(XX<200.)THEN
                  F=XX/(XX+EXP(SCRP(6,1)-SCRP(6,2)*XX))
              ELSE
                  F=1.
              END IF
              BDP(ISL)=BDP(ISL)+F*(BD(ISL)-BDP(ISL))
          END IF
          XX=PKRZ(ISL)
          IF(Z(ISL)>BIG)EXIT
      END DO
      RETURN
      END