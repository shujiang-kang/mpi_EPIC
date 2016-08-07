      SUBROUTINE NUSE
!     EPIC1102
!     THIS SUBPROGRAM CALCULATES THE DAILY POTENTIAL SOIL SUPPLY OF P
!     FOR EACH LAYER.
      USE PARM
      IF(RW(JJK)>0.)THEN
          XX=1.5*UPP/RW(JJK)
      ELSE
          XX=1.
      END IF
      DO J=1,LRD
          ISL=LID(J)
          UN(ISL)=MAX(0.,(WNO3(ISL)-.001*PRMT(27)*WT(ISL))*U(ISL)/(ST(ISL)&
          +.001))
          SUN=SUN+UN(ISL)
          UK(ISL)=SOLK(ISL)*U(ISL)/(ST(ISL)+.001)
          SUK=SUK+UK(ISL)
          F=1000.*AP(ISL)/WT(ISL)
          IF(F>30.)THEN
              F=1.
          ELSE
              F=F/(F+EXP(8.0065-.3604*F))
          END IF
          UP(ISL)=XX*F*RWT(ISL,JJK)
          IF(UP(ISL)>=AP(ISL))UP(ISL)=.9*AP(ISL)
          SUP=SUP+UP(ISL)
      END DO
      RETURN
      END