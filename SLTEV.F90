      SUBROUTINE SLTEV
!     EPIC1102
!     THIS SUBPROGRAM ESTIMATES UPWARD SALT MOVEMENT CAUSED BY SOIL
!     EVAPORATION
      USE PARM
      IF(NEV==1)RETURN
      J=NEV
      SUM=0.
      DO J=NEV,2,-1
          L=LID(J)
          X1=WSLT(L)
          IF(X1<=1.E-5)CYCLE
          XX=MIN(.05*X1,SEV(L)*X1/(ST(L)+SEV(L)))
          SUM=SUM+XX
          WSLT(L)=WSLT(L)-XX
      END DO
      WSLT(LD1)=WSLT(LD1)+SUM
      RETURN
      END