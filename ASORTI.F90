      SUBROUTINE ASORTI(NZ,NY,M)
!     EPIC1102
!     THIS SUBPROGRAM SORTS INTEGERS INTO ASCENDING ORDER USING
!     RIPPLE SORT
      USE PARM
      DIMENSION NY(M),NZ(M)
      NB=M-1
      J=M
      DO I=1,NB
          J=J-1
          MK=0
          DO K=1,J
              K1=K+1
              IF(NZ(NY(K))<=NZ(NY(K1)))CYCLE
              N1=NY(K1)
              NY(K1)=NY(K)
              NY(K)=N1
              MK=1
          END DO
          IF(MK==0)EXIT
      END DO
      RETURN
      END