      SUBROUTINE AISHFL
!     EPIC1102
!     THIS SUBPROGRAM SHUFFLES DATA RANDOMLY. (BRATLEY,FOX,SCHRAGE,P.34)
      USE PARM
      DO I=20,2,-1
          II=IDG(I)
          RN=AUNIF(21)
          K=I*RN+1
          IDG(I)=IDG(K)
          IDG(K)=II
      END DO
      RETURN
      END