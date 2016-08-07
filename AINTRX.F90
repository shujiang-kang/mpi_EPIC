      SUBROUTINE AINTRX(X,Y,N1,N2)
!     EPIC1102
!     THIS SUBPROGRAM INTERPOLATES SOIL PROPERTIES FROM LAYERS WITH   
!     EQUAL THICKNESS (OUTPUT FROM DIF EQ SOLN OF GAS DIFFUSION EQS) TO
!     LAYERS OF UNEQUAL THICKNESS (INPUT SOIL LAYERS).
      USE PARM
      DIMENSION X(30),Y(30)
      Z1=0.
      TOT=0.
      Y=0.
      J=1
      DO K=1,N2
          DO WHILE(J<=N1)
              L=LID(J)
              IF(Z(L)>ZC(K))EXIT
              Y(L)=TOT+X(K)*(Z(L)-Z1)
              Z1=Z(L)
              J=J+1
              TOT=0.
          END DO
          IF(J<=N1)THEN
              TOT=TOT+X(K)*(ZC(K)-Z1)
              Z1=ZC(K)
          ELSE
              EXIT  
          END IF
      END DO
      Z1=0.
      DO J=1,N1
          L=LID(J)     
          Y(L)=Y(L)/(Z(L)-Z1)
          Z1=Z(L)
      END DO
      RETURN
      END