      SUBROUTINE ACOUT(XX,QQ,GKG)
!     EPIC1102
      REAL*8 XX,X1,X2,X3
      IF(XX<1.E-10)RETURN
      X1=.1*GKG*XX/(QQ+1.E-10)
      IF(X1<1000.)THEN
	      DO I=1,4
              N2=X1
              IF(N2>0)EXIT
              X1=X1*1.D3
          END DO
          I=MIN(I,4)
          XI=I
	  ELSE
          XI=0.
          X1=.001*X1
      END IF
      N2=XX+.5
      X2=N2
      N1=X1+.5
      X1=N1
      X3=1.D-3*X1+1.D-4*XI
      XX=X2+X3
      RETURN
      END      
      