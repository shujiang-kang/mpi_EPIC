      SUBROUTINE NFALL(ZZ)
!     EPIC1102
!     THIS SUBPROGRAM SIMULATES THE CONVERSION OF STANDING DEAD CROP
!     RESIDUE TO FLAT RESIDUE.
      USE PARM
      SUM=0.
      TOT=0.
      DO K=1,LC
          IF(STD(K)<.001)CYCLE
          X1=ZZ*STD(K)
          STD(K)=STD(K)-X1
          SUM=SUM+X1
          X2=ZZ*STDN(K)
          STDN(K)=STDN(K)-X2
          TOT=TOT+X2
          ZS=ZZ*STDP
          FOP(LD1)=FOP(LD1)+ZS
          STDP=STDP-ZS
          ZS=ZZ*STDK
          SOLK(LD1)=SOLK(LD1)+ZS
          STDK=MAX(1.E-5,STDK-ZS)
          ZS=ZZ*STDL
          STDL=STDL-ZS
      END DO
      ZZ=MIN(1.,ZZ*10.)
      ZS=ZZ*STDO
      STDO=MAX(1.E-5,STDO-ZS)
      SUM=SUM+ZS
      ZS=ZZ*STDON
      TOT=TOT+ZS
      CALL NCNSTD(SUM,TOT,0)
      STDON=MAX(1.E-5,STDON-ZS)
      ZS=ZZ*STDOP
      FOP(LD1)=FOP(LD1)+ZS
      STDOP=MAX(1.E-5,STDOP-ZS)
      ZS=ZZ*STDOK
      SOLK(LD1)=SOLK(LD1)+ZS
      STDOK=MAX(1.E-5,STDOK-ZS)
      RETURN
      END