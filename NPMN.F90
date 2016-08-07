      SUBROUTINE NPMN(CS)
!     EPIC1102
!     THIS SUBPROGRAM SIMULATES MINERALIZATION P USING PAPRAN EQS
      USE PARM
      HMP=DHN(ISL)*WP(ISL)/(WHSN(ISL)+WHPN(ISL))
      TKG=RSD(ISL)*1000.      
      R4=.58*TKG
      X1=MAX(1.,FOP(ISL)+AP(ISL))
      CPR=MIN(2000.,R4/X1)
      CPRF=EXP(-.693*(CPR-200.)/200.)
      DECR=MAX(.01,.05*CPRF*CS)
      RMP=DECR*FOP(ISL)
      IF(FOP(ISL)>1.E-5)THEN
          FOP(ISL)=FOP(ISL)-RMP
      ELSE
          RMP=FOP(ISL)
          FOP(ISL)=0.
      END IF
      WMP=.8*RMP+HMP
      X1=AP(ISL)+WMP
      IF(X1<0.)THEN
          WMP=AP(ISL)
          HMP=WMP-.8*RMP
          AP(ISL)=1.E-5
      ELSE
          AP(ISL)=AP(ISL)+WMP
      END IF
      WP(ISL)=WP(ISL)-HMP+.2*RMP
      RETURN
      END