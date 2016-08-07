      SUBROUTINE HLGOON(JRT)
!     EPIC1102
!     THIS SUBPROGRAM ESTIMATES INFLOW, STORAGE, & IRRIGATION FROM
!     LAGOONS. RUNOFF IS ESTIMATED WITH 90 CN.
      USE PARM
      X2=10.*DALG
      DP=.1677*VLG**.3333
      TW=18.*DP
      SA=.0001*TW*TW
      EV=6.*EO*SA
      VLG=VLG-EV+COWW
      EV=EV/X2
      SMM(21,MO)=SMM(21,MO)+EV
      VAR(21)=EV
      XX=COWW/X2
      SMM(22,MO)=SMM(22,MO)+XX
      VAR(22)=XX
      X1=RWO-5.64
      IF(X1>0.)THEN
          QLG=X1*X1/(RWO+22.6)
          QLG=10.*(QLG*(DALG-SA)+RWO*SA)
          VLG=VLG+QLG
          QLG=QLG/X2
          SMM(23,MO)=SMM(23,MO)+QLG
          VAR(23)=QLG
      END IF
      IF(VLG<=VLGN)THEN
          JRT=1
          RETURN
      END IF
      IF(VLG>VLGM)THEN
          X1=(VLG-VLGM)/X2
!          WRITE(KW(1),4)IYR,MO,KDA,X1
          SMM(24,MO)=SMM(24,MO)+X1
          VAR(24)=X1
          VLG=VLGM
      END IF
      IF(RZSW>=PAW)THEN
          JRT=1
          RETURN
      END IF
      XX=10.*WSA*VLGI
      X1=XX/X2
      SMM(25,MO)=SMM(25,MO)+X1
      VAR(25)=X1
      VLG=MAX(1.E-5,VLG-XX)
      JRT=0
      RETURN
    4 FORMAT(T10,'***** LAGOON OVERFLOWED ',3I4,F4.0,' MM')
      END