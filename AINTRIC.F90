      SUBROUTINE AINTRIC(X,Y,N1,N2)
!     EPIC1102
!     THIS SUBPROGRAM INTERPOLATES CONCENTRATIONS FROM LAYERS WITH UN
!     EQUAL THICKNESS TO LAYERS OF EQUAL THICKNESS USED IN DIFFERENTIAL
!     EQUATIONS OF GAS DIFFUSION.
      USE PARM
      DIMENSION X(15),Y(N2)
      ZZ=0.
      Z1=0.
      TOT=0.
      J=1
      DO K=1,N1
          L=LID(K)
          DO 
              IF(ZC(J)>Z(L))EXIT
              Y(J)=TOT+X(L)*(ZC(J)-ZZ)
              ZZ=ZC(J)
              J=J+1
              IF(J>N2)RETURN
              TOT=0.
          END DO
          IF(J>N2)RETURN
          TOT=TOT+X(L)*(Z(L)-ZZ)
          Z1=Z(L) 
          ZZ=Z(L)
      END DO
      DO I=1,N2-1
          Y(I)=Y(I)/DZ
      END DO 
      Y(N2)=MAX(X(LID(N1)),TOT/DZ)
      RETURN
      END