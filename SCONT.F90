      SUBROUTINE SCONT(KWX)
!     THIS SUBPROGRAM ADDS SOIL LAYER CONTENTS OF WATER & NUTRIENTS
      USE PARM
      DIMENSION ATX(4),SWZ(4)
      SW=0.
      APB=0.
      OCPD=0.
      RZSW=0.
      PAW=0.
      SWRZ=0.
      TEK=0.
      TFK=0.
      TFOP=0.
      ZN2O=0.
      ZN2OG=0.
      ZN2OL=0.
      ZNO2=0.
      ZNO3=0.
      ZNH3=0.
      TAP=0.
      TP=0.
      TMP=0.
      TRSD=0.
      TSK=0.
      TWN=0.
      TOC=0.
      ZLS=0.
      ZLM=0.
      ZLSL=0.
      ZLSC=0.
      ZLMC=0.
      ZLSLC=0.
      ZLSLNC=0.
      ZBMC=0.
      ZHSC=0.
      ZHPC=0.
      ZLSN=0.
      ZLMN=0.
      ZBMN=0.
      ZHSN=0.
      ZHPN=0.
      TOP=0.
      TNOR=0.
      TSLT=0.
      TSRZ=0.
      XX=0.
      PDSW=0.
      FCSW=0.
      IF(BIG>0.)CALL SAJBD
      SUM=0.
      S1=0.
      S2=0.
      NTX=0 
      BL=ZCS(1)
      DO J=1,NBSL
          L=LID(J)
          ZN2O=ZN2O+WN2O(L)
          ZNO2=ZNO2+WNO2(L)
          ZN2OG=ZN2OG+WN2OG(L)
          ZN2OL=ZN2OL+WN2OL(L)
          ZNO3=ZNO3+WNO3(L)
          ZNH3=ZNH3+WNH3(L)
          TEK=TEK+EXCK(L)
          TFK=TFK+FIXK(L)
          TSK=TSK+SOLK(L)
          TSLT=TSLT+WSLT(L)
          WOC(L)=WBMC(L)+WHPC(L)+WHSC(L)+WLMC(L)+WLSC(L)
          ECND(L)=.15625*WSLT(L)/ST(L)
          IF(Z(L)<=PMX)THEN
              PDSW=PDSW+ST(L)-S15(L)
              FCSW=FCSW+FC(L)-S15(L)
              APB=APB+AP(L)
              OCPD=OCPD+WOC(L)
              SUM=SUM+WT(L)
              K1=J
              K2=L
          END IF
          IF(Z(L)<=RZ)THEN
              SWRZ=SWRZ+ST(L)
              TNOR=TNOR+WNO3(L)
              TSRZ=TSRZ+WSLT(L)
              LZ=L
              L1=J
              RZSW=RZSW+ST(L)-S15(L)
              PAW=PAW+FC(L)-S15(L)
          END IF
          TAP=TAP+AP(L)
          TOP=TOP+OP(L)
          TMP=TMP+PMN(L)
          TP=TP+WP(L)
          TRSD=TRSD+RSD(L)
          SW=SW+ST(L)
          TFOP=TFOP+FOP(L)
          ZLS=ZLS+WLS(L)
          ZLM=ZLM+WLM(L)
          ZLSL=ZLSL+WLSL(L)
          ZLSC=ZLSC+WLSC(L)
          ZLMC=ZLMC+WLMC(L)
          ZLSLC=ZLSLC+WLSLC(L)
          ZLSLNC=ZLSLNC+WLSLNC(L)
          ZBMC=ZBMC+WBMC(L)
          ZHSC=ZHSC+WHSC(L)
          ZHPC=ZHPC+WHPC(L)
          ZLSN=ZLSN+WLSN(L)
          ZLMN=ZLMN+WLMN(L)
          ZBMN=ZBMN+WBMN(L)
          ZHSN=ZHSN+WHSN(L)
          ZHPN=ZHPN+WHPN(L)
          WON(L)=WBMN(L)+WHPN(L)+WHSN(L)+WLMN(L)+WLSN(L)
          IF(KFL(18)==0.OR.KWX>0)CYCLE
    !     DETERMINE SOIL TEMP AT .1, .2, .5, 1. m
          RTO=.001*(ST(L)-S15(L))/(Z(L)-XX)
          IF(XX<.1.AND.Z(L)>=.1)THEN
              ATX(1)=STMP(L)
              SWZ(1)=RTO
          END IF
          IF(XX<.2.AND.Z(L)>=.2)THEN
              ATX(2)=STMP(L)
              SWZ(2)=RTO
          END IF
          IF(XX<.5.AND.Z(L)>=.5)THEN
              ATX(3)=STMP(L)
              SWZ(3)=RTO
          END IF
          IF(XX<1..AND.Z(L)>=1.)THEN
              ATX(4)=STMP(L)
              SWZ(4)=RTO
          END IF 
          RTO=1000.*RTO
          !     COMPUTE CONTROL SECTION WATER CONTENT
          IF(Z(L)>ZCS(1).AND.BL<ZCS(3))THEN
              IF(Z(L)<=ZCS(2))THEN
                  S1=S1+RTO*(Z(L)-BL)
              ELSE
                  IF(BL<ZCS(2))THEN      
                      S1=S1+RTO*(ZCS(2)-BL)
                      BL=ZCS(2)
                      S2=S2+RTO*(Z(L)-BL)        
                  ELSE
                      IF(Z(L)<=ZCS(3))THEN
                          S2=S2+RTO*(Z(L)-BL)
                      ELSE
                          S2=S2+RTO*(ZCS(3)-BL)
                      END IF                                                    
                  END IF
              END IF
              BL=Z(L)
          END IF        
          XX=Z(L)
      END DO
      IF(KFL(18)>0.AND.KWX==0)WRITE(KW(18),554)IY,IYR,MO,KDA,S1,S2,SWZ,ATX
      TWN=ZLSN+ZLMN+ZBMN+ZHSN+ZHPN
      TOC=ZLSC+ZLMC+ZBMC+ZHSC+ZHPC
	  VAR(85)=TWN0-TWN-YN
	  SMM(85,MO)=SMM(85,MO)+TWN0-TWN-YN
	  TWN0=TWN
      IF(LZ/=L)THEN
          ZZ=RZ-Z(LZ)
          L1=LID(L1+1)
          RTO=ZZ/(Z(L1)-Z(LZ))
          RZSW=RZSW+(ST(L1)-S15(L1))*RTO
          PAW=PAW+RTO*(FC(L1)-S15(L1))
          SWRZ=SWRZ+ST(L1)*RTO
          TNOR=TNOR+WNO3(L1)*RTO
          TSRZ=TSRZ+WSLT(L1)*RTO
      END IF
      SSW=SSW+RZSW
      IF(K1/=NBSL)THEN
          KK=LID(K1+1)
          RTO=(PMX-Z(K2))/(Z(KK)-Z(K2))
          PDSW=PDSW+RTO*(ST(KK)-S15(KK))
          FCSW=FCSW+RTO*(FC(KK)-S15(KK))
          APB=APB+RTO*AP(KK)
          OCPD=OCPD+RTO*WOC(KK)
          SUM=SUM+RTO*WT(KK)
      END IF
      APBC=1000.*APB/SUM
      OCPD=.001*OCPD
      RETURN
  554 FORMAT(I4,1X,3I4,2F10.1,4F10.4,4F10.2)
      END