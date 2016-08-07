      SUBROUTINE AINTRI(X,Y,N1,N2)
!     EPIC1102
!     THIS SUBPROGRAM INTERPOLATES SOIL PROPERTIES FROM LAYERS WITH UN
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
              Y(J)=TOT+X(L)*(ZC(J)-ZZ)/(Z(L)-Z1)
              ZZ=ZC(J)
              J=J+1
              IF(J>N2)EXIT
              TOT=0.
          END DO
          TOT=TOT+X(L)*(Z(L)-ZZ)/(Z(L)-Z1)
          IF(J>N2)EXIT
          Z1=Z(L) 
          ZZ=Z(L)
      END DO 
      I=MIN(J,N2)
      Y(I)=TOT
      RETURN
      END