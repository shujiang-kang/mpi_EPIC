      SUBROUTINE WRWD(IEX)
!     EPIC1102
      USE PARM
      IF(IEX==0)THEN
          RN=1.-AUNIF(IDG(1))
          IF(RN>PRWM+.001)THEN
              RFV=0.
              LW=1
              RETURN
          END IF
      END IF
      V4=AUNIF(IDG(3))
      IF(ICDP==0)THEN
          R6=RFSK/6.
          ZZ=ADSTN(V3,V4)
          RFV=WRAIN(R6,ZZ,RFSD,RFSK,RFVM)*PCF(NWI,MO)
          V3=V4
      ELSE
          RFV=RFVM*(-LOG(V4))**EXPK
      END IF
      LW=2
      RETURN
      END