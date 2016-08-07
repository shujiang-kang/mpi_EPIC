      SUBROUTINE HGAWY(A,PT,Q1,RX)
!     EPIC1102
!     THIS SUBPROGRAM SOLVES THE GREEN & AMPT INFILTRATION EQ ITERATIVELY
!     TO OBTAIN RESULT AT DT/2
      USE PARM
      F1=PT-QD
      ZI=SATK*(SCN/F1+1.)
      Q1=0.
      IF(RX<=ZI)GO TO 3
      QL=A*(RX-ZI)/RX
      GB=QL
      GL=0.
      FF=F1-GB
      ZI=SATK*(SCN/FF+1.)
      QB=A*(RX-ZI)/RX
      DO I=1,10
          B2=(QL-QB)/(GL-GB)
          B1=QL-B2*GL
          G1=B1/(1.-B2)
          FF=F1-G1
          ZI=SATK*(SCN/FF+1.)
          Q1=A*(RX-ZI)/RX
          GQ=Q1-G1
          IF(ABS(GQ/G1)<.001)EXIT
          IF(GQ>0.)THEN
              GL=G1
              QL=Q1
          ELSE
              GB=G1
              QB=Q1
          END IF
      END DO
    3 QD=QD+Q1
!      WRITE(KW(1),2)I,SATK,RX,ZI,F1,Q1,PT,QD !skang
      RETURN
    2 FORMAT(1X,I3,10F10.3)
      END