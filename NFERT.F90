      SUBROUTINE NFERT(IRC,JFT)
!     EPIC1102
!     THIS SUBPROGRAM APPLIES N AND P FERTILIZER AT SPECIFIED DATES,
!     RATES, AND DEPTH.
      USE PARM
      IF(ANA(JJK)>=FNMX(JJK))RETURN
      X1=0.
      I=LD1
      ZFT=TLD(JFT)
      SELECT CASE(IRC)
          CASE(1)
              KF=LFT(IRO,KT)
              X1=WFA(IRO,KT)
          CASE(2)
              KF=IDFT(1)
              X1=FNP
          CASE(3)
              IF(IRR/=4)THEN
                  KF=IDFT(1)
                  X1=FNP
              ELSE
                  KF=IDFT(2)
              END IF
          CASE(4)
              KF=IDFT(2)
              X3=BFT-TNOR
              GO TO 1
          CASE(5)
              KF=IDFP
              X1=APMU
              X3=APMU*FN(KF)
              GO TO 13              
      END SELECT
      DO J=1,NBSL
          I=LID(J)
          IF(ZFT<Z(I))EXIT
      END DO
      IF(X1>0.)THEN
          IF(FN(KF)>0.)THEN
              X3=X1*FN(KF)
          ELSE
              X3=0.
              GO TO 13
          END IF
      ELSE
          X1=MAX(PRMT(28)*UNA(JJK)-TNOR,0.)
          X3=X1
          IF(FN(KF)<1.E-10)RETURN
      END IF
    1 X3=MIN(X3,FNMX(JJK)-ANA(JJK))
      X1=X3/FN(KF)
   13 X2=X1*FP(KF)
      X4=X3*FNH3(KF)
      X5=X1*FNO(KF)
      X6=X1*FPO(KF)
      X7=X3-X4
      X8=X1*FOC(KF)
      SMM(65,MO)=SMM(65,MO)+X8
      VAR(65)=X8
      IF(FOC(KF)>.5)THEN
          ZZ=0.
          DO I=1,NBSL
          ISL=LID(I)
              IF(Z(ISL)<=.15)THEN
                  SOM=.172*WOC(ISL)/WT(ISL)
                  DZB=(Z(ISL)-ZZ)/.15
                  BC=.1*X1*DZB/WT(ISL)
                  BD(ISL)=100./(SOM/.244+(100.-SOM-BC)/BDM(ISL)+BC/.55)
                  CECM=(CEC(ISL)+BC*187.)/(1.+BC)
                  ADD=CECM-CEC(ISL)
                  OM=(SOM+BC)*WT(ISL)*1.E4
                  CECA=10.*ADD/OM
                  CEC(ISL)=CECM 
                  X=6.6-(LOG(3.8056/(PH(ISL)-3.495))-1.)/1.08
                  XN=X+CECA
                  PH(ISL)=3.8056/(1.+EXP(1.08*(6.6-XN)))+3.495
                  W1=X8*DZB
                  WLM(ISL)=WLM(ISL)+.02*X1*DZB
                  WLMC(ISL)=WLMC(ISL)+.02*W1
                  WHSC(ISL)=WHSC(ISL)+.6*W1
                  WHPC(ISL)=WHPC(ISL)+.38*W1                                      
              ELSE
                  EXIT          
              END IF
              ZZ=Z(ISL)  
          END DO
          IF(I<NBSL)THEN
              SOM=.172*WOC(ISL)/WT(ISL)
              DZB=(.15-ZZ)/.15
              BC=.1*X1*DZB/WT(ISL)
              BD(ISL)=100./(SOM/.244+(100.-SOM-BC)/BDM(ISL)+BC/.55)
              CECM=(CEC(ISL)+BC*187.)/(1.+BC)
              ADD=CECM-CEC(ISL)
              OM=(SOM+BC)*WT(ISL)*1.E4
              CECA=10.*ADD/OM
              CEC(ISL)=CECM
              X=6.6-(LOG(3.8056/(PH(ISL)-3.495))-1.)/1.08
              XN=X+CECA
              PH(ISL)=3.8056/(1.+EXP(1.08*(6.6-XN)))+3.495
              W1=X8*DZB
              WLM(ISL)=WLM(ISL)+.02*X1*DZB
              WLMC(ISL)=WLMC(ISL)+.02*W1
              WHSC(ISL)=WHSC(ISL)+.6*W1
              WHPC(ISL)=WHPC(ISL)+.38*W1                                                     
          END IF      
	  ELSE
          IF(X8>.1)THEN
              RLN=.175*X8/(X3+X5)
              X10=.85-.018*RLN
              IF(X10<.01)THEN
                  X10=.01
              ELSE
                  IF(X10>.7)X10=.7
              END IF
              XX=X8*X10
              WLMC(I)=WLMC(I)+XX
              YY=X1*X10
              WLM(I)=WLM(I)+YY
              ZZ=X5*X10
              WLMN(I)=WLMN(I)+ZZ
              WLSN(I)=WLSN(I)+X5-ZZ
              XZ=X8-XX
              WLSC(I)=WLSC(I)+XZ
              WLSLC(I)=WLSLC(I)+XZ*.175
              WLSLNC(I)=WLSC(I)-WLSLC(I)
              YZ=X1-YY
              WLS(I)=WLS(I)+YZ
              WLSL(I)=WLSL(I)+YZ*.175
          END IF
      END IF
      X9=X1*FK(KF)
      X11=X1*FSLT(KF)
      WNH3(I)=WNH3(I)+X4
      AP(I)=AP(I)+X2
      WP(I)=WP(I)+X6
      SMM(59,MO)=SMM(59,MO)+X5
      VAR(59)=X5
      SMM(62,MO)=SMM(62,MO)+X6
      VAR(62)=X6
      SMM(61,MO)=SMM(61,MO)+X4
      VAR(61)=X4
      SMM(63,MO)=SMM(63,MO)+X2
      VAR(63)=X2
      WNO3(I)=WNO3(I)+X7
      ANA(JJK)=ANA(JJK)+X3
      SMM(60,MO)=SMM(60,MO)+X7
      VAR(60)=VAR(60)+X7
      SOLK(I)=SOLK(I)+X9
      SMM(64,MO)=SMM(64,MO)+X9
      VAR(64)=X9
      WSLT(I)=WSLT(I)+X11
      SMM(72,MO)=SMM(72,MO)+X11
      VAR(72)=X11
	  N1=MAX(1,NCP(JJK))
      FRTN(N1,JJK)=FRTN(N1,JJK)+X3+X5
      FRTP(N1,JJK)=FRTP(N1,JJK)+X2+X6
      FRTK(N1,JJK)=FRTK(N1,JJK)+X9
      XX=X1*FCST(KF)
      COST=COST+XX
      SMM(96,MO)=SMM(96,MO)+FCEM(KF)*X1
      IF(IRC==3)THEN
          Y1=COTL(JFT)
          Y2=Y1-COOP(JFT)
          COST=COST+Y1
          CSFX=CSFX+Y2
      END IF
      IF(KFL(20)>0)THEN
          WRITE(KW(20),18)IYR,MO,KDA,FTNM(KF),KDC(JJK),KDF(KF),IHC(JFT),&
              NBE(JFT),NBT(JFT),XX,XX,X1
          IF(IRC==3)THEN
	          WRITE(KW(20),50)IYR,MO,KDA,TIL(JFT),KDC(JJK),IHC(JFT),NBE&
                  (JFT),NBT(JFT),Y1,Y2,FULU(JFT)
          END IF
      END IF
!      IF(NOP>0)WRITE(KW(1),90)IYR,MO,KDA,FTNM(KF),X1,ZFT,X3,X4,X5,X2,&
!      X6,X9,XHSM,XX     !skang
      NFA=0
      RETURN
   18 FORMAT(1X,3I4,2X,A8,8X,I6,2X,4I4,F10.2,10X,2F10.2,10X,F10.2)
   50 FORMAT(1X,3I4,2X,A8,8X,I6,6X,3I4,2F10.2,20X,F10.2)
   90 FORMAT(1X,3I4,2X,A8,2X,'RATE=',F6.0,'kg/ha',1X,'DPTH=',F4.2,'m'&
      ,1X,'MN=',F4.0,1X,'NH3=',F4.0,1X,'ON=',F4.0,1X,'MP=',F4.0,1X,'OP='&
      ,F4.0,1X,'MK=',F4.0,1X,'HUSC=',F4.2,2X,'COST=',F7.0,'$/ha')
      END