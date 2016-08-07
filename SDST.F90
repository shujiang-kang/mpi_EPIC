      SUBROUTINE SDST(X,DG,DG1,X1,X2,I,ISL)
!     EPIC1102
      DIMENSION X(ISL)
      IF(X(I)>0.)RETURN
      IF(I==1)THEN
          X(1)=X1
          RETURN
      ELSE
          XX=X2*DG
          IF(XX>10.)THEN
              RTO=.0001
          ELSE
	          RTO=DG*EXP(-XX)/DG1
          END IF
	      X(I)=X(I-1)*RTO
      END IF 
      RETURN
      END