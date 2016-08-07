      SUBROUTINE SWTN
!     EPIC1102
      USE PARM
      DO J=1,NBSL
          ISL=LID(J)
          IF(Z(ISL)>=.15)GO TO 1
      END DO
      ISL=LID(NBSL)
    1 XX=LOG10(S15(ISL))
      X1=MAX(.1,ST(ISL))
      WTN=MAX(5.,10.**(3.1761-1.6576*((LOG10(X1)-XX)/(LOG10(FC(ISL))-XX))))
      RETURN
      END