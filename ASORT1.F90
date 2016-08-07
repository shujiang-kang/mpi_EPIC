      SUBROUTINE ASORT1(X,NX,M)
!     THIS SUBPROGRAM SORTS REAL NUMBERS INTO ASCENDING ORDER USING
!     RIPPLE SORT      
      DIMENSION X(200),NX(200)
      NB=M-1
      J=M
      DO I=1,NB
          J=J-1
          MK=0
          DO K=1,J
              KP1=K+1
              IF(X(NX(K))>=X(NX(KP1)))CYCLE
              N1=NX(KP1)
              NX(KP1)=NX(K)
              NX(K)=N1
              MK=1
          END DO
          IF(MK==0)EXIT
      END DO
      RETURN
      END