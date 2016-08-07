      SUBROUTINE WNDIR
!     EPIC1102
!     THIS SUBPROGRAM SIMULATES DAILY WIND DIRECTION
      USE PARM
      FX=AUNIF(IDG(6))
      DO J=1,16
          J1=J-1
          IF(DIR(MO,J)>FX)GO TO 3
      END DO
      J=16
    3 IF(J==1)THEN
          G=FX/DIR(MO,J)
      ELSE
          G=(FX-DIR(MO,J1))/(DIR(MO,J)-DIR(MO,J1))
      END IF
      XJ1=J1
      TH=PI2*(G+XJ1-.5)/16.
      IF(TH<0.)TH=PI2+TH
      RETURN
      END