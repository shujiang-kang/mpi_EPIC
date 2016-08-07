      SUBROUTINE TMIX(EE,DMX,NMIX)
!     EPIC1102
!     THIS SUBPROGRAM MIXES N,P, AND CROP RESIDUE WITHIN THE PLOW DEPTH
!     ACCORDING TO THE MIXING EFFICIENCY OF THE IMPLEMENT, CALCULATES
!     THE CHANGE IN BULK DENSITY, CONVERTS STANDING RESIDUE TO FLAT
!     RESIDUE, AND ESTIMATES THE IMPLEMENT'S EFFECT ON RIDGE HEIGHT AND
!     INTERVAL.
      USE PARM
      DIMENSION TST(58),DUM(15),XTP(8),YTP(8)
      DATA ISM/58/
      AD1=0.
      ZLSC1=0.
      ZLMC1=0.
      ZBMC1=0.
      ZHSC1=0.
      ZHPC1=0.
      DO I=1,NBSL
          ISL=LID(I)
          ZLSC1=ZLSC1+WLSC(ISL)                                                              
          ZLMC1=ZLMC1+WLMC(ISL)                                                              
          ZBMC1=ZBMC1+WBMC(ISL)                                                              
          ZHSC1=ZHSC1+WHSC(ISL)                                                              
          ZHPC1=ZHPC1+WHPC(ISL)                                                              
          AD1=AD1+WBMC(ISL)+WHPC(ISL)+WHSC(ISL)+WLMC(ISL)+WLSC(ISL)
      END DO
      IF(NMIX==0)THEN
	      IF(DMX<0.)THEN
	      !     MOW, SHRED, ETC
		      DO J=1,LC
			      IF(IDC(JJK)==NDC(7).OR.IDC(JJK)==NDC(8).OR.IDC(JJK)==NDC(10))CYCLE                     
			      IF(CHT(J)+DMX<0.)CYCLE
			      XX=(CHT(J)+DMX)/CHT(J)
			      ZZ=XX*STD(J)
			      ZO=XX*STDO
			      ZL=XX*STL(J)
			      STD(J)=STD(J)-ZZ
			      STDO=STDO-ZO
			      STL(J)=STL(J)-ZL
			      X1=1.-XX
			      SLAI(J)=SLAI(J)*X1
			      HU(J)=HU(J)*X1
			      STDL=STDL*X1
			      DX=MIN(.99,ZL/(DM(J)+1.E-10))
			      X1=ZZ+ZL+ZO
			      ZZ=XX*STDN(J)
			      ZON=XX*STDON
			      ZN=DX*UN1(J)
			      STDN(J)=STDN(J)-ZZ
			      STDON=STDON-ZON
			      X2=ZZ+ZN+ZON
			      CALL NCNSTD(X1,X2,0)
			      ZZ=XX*STDP
			      STDP=STDP-ZZ
			      ZOP=XX*STDOP
			      STDOP=STDOP-ZOP
			      ZP=DX*UP1(J)
			      FOP(LD1)=FOP(LD1)+ZZ+ZP+ZOP
			      ZZ=XX*STDK
			      STDK=STDK-ZZ
			      ZOK=XX*STDOK
			      STDOK=STDOK-ZOK
			      ZK=DX*UK1(J)
			      SOLK(LD1)=SOLK(LD1)+ZZ+ZK+ZOK
			      DM(J)=DM(J)-ZL
			      UN1(J)=MAX(1.E-5,UN1(J)-ZN)
			      UP1(J)=UP1(J)-ZP
			      UK1(J)=MAX(1.E-5,UK1(J)-ZK)
			      CHT(J)=-DMX
		      END DO
		      RETURN
	      END IF
	      IF(Z(LD1)>=DMX)RETURN
	      RFSM=0.
	      RCF=1.
	      IF(RHT(JT1)<RHT(JT2))THEN
		      RHTT=RHT(JT1)+(RHT(JT2)-RHT(JT1))*EXP(-DMX/TLD(JT2))
	      ELSE
		      RHTT=RHT(JT1)
		      RGIN=RIN(JT1)
	      END IF
	      F=1.-EXP(-56.9*DMX*EE)
	      SUM=0.
	      TOT=0.
	      DO K=1,LC
		      X1=STD(K)*F
		      SUM=SUM+X1
		      STD(K)=MAX(1.E-10,STD(K)-X1)
		      XX=F*STDN(K)
		      TOT=TOT+XX
		      STDN(K)=MAX(1.E-10,STDN(K)-XX)
		      XX=F*STDP
		      STDP=MAX(1.E-10,STDP-XX)
		      FOP(LD1)=FOP(LD1)+XX
		      XX=F*STDK
		      STDK=MAX(1.E-10,STDK-XX)
		      SOLK(LD1)=SOLK(LD1)+XX
		      XX=F*STDL
		      STDL=MAX(.1*STD(K),STDL-XX)
	      END DO
	      XX=STDO*F
	      STDO=MAX(1.E-5,STDO-XX)
	      X1=SUM+XX
	      ZON=F*STDON
	      STDON=MAX(1.E-5,STDON-ZON)
	      X2=TOT+ZON
		  AD1=AD1+420.*X1
	      CALL NCNSTD(X1,X2,0)
	      XX=F*STDOP
	      STDOP=MAX(1.E-5,STDOP-XX)
	      FOP(LD1)=FOP(LD1)+XX
	      XX=F*STDOK
	      STDOK=MAX(1.E-5,STDOK-XX)
	      SOLK(LD1)=SOLK(LD1)+XX
	      RRUF=MAX(1.,RR(JT1))
	      TLMF=0.
	  END IF
      DO I=1,ISM
	      TST(I)=0.
      END DO
      XX=0.
      XTP(1)=WLS(LD1)
      XTP(2)=WLM(LD1)
      XTP(3)=WLSL(LD1)
      XTP(4)=WLSC(LD1)
      XTP(5)=WLMC(LD1)
      XTP(6)=WLSLC(LD1)
      XTP(7)=WLSN(LD1)
      XTP(8)=WLMN(LD1)
      DO J=1,NBSL
	      ISL=LID(J)
	      UN(ISL)=ROK(ISL)
	      ZZ=Z(ISL)-XX
	      IF(Z(ISL)>=DMX)EXIT
	      IF(NMIX<=0)THEN
		      BDP(ISL)=BDP(ISL)-(BDP(ISL)-.6667*BD(ISL))*EE
		      CLA(ISL)=CLA(ISL)*ZZ
		      SIL(ISL)=SIL(ISL)*ZZ
		      ROK(ISL)=ROK(ISL)*ZZ
	      END IF
	      PMA=PMN(ISL)+AP(ISL)
	      DUM(ISL)=PSP(ISL)*PMA
	      UP(ISL)=PMA-DUM(ISL)
    !     EXTRACT THE FRACTION OF MATERIAL TO BE MIXED AND PLACE IN TST
    !     STORAGE
	      TST(1)=EAJL(WNO3(ISL),EE)+TST(1)
    !     TST(2)=EAJL(WHPN(ISL),EE)+TST(2)
    !     TST(3)=EAJL(WHSN(ISL),EE)+TST(3)
	      TST(4)=EAJL(WBMN(ISL),EE)+TST(4)
	      TST(5)=EAJL(WLSN(ISL),EE)+TST(5)
	      TST(6)=EAJL(WLMN(ISL),EE)+TST(6)
    !     TST(7)=EAJL(WHPC(ISL),EE)+TST(7)
    !     TST(8)=EAJL(WHSC(ISL),EE)+TST(8)
	      TST(9)=EAJL(WBMC(ISL),EE)+TST(9)
	      TST(14)=EAJL(WLS(ISL),EE)+TST(14)
	      TST(15)=EAJL(WLM(ISL),EE)+TST(15)
	      TST(16)=EAJL(WLSL(ISL),EE)+TST(16)
	      TST(10)=EAJL(WLSC(ISL),EE)+TST(10)
	      TST(11)=EAJL(WLMC(ISL),EE)+TST(11)
	      TST(12)=EAJL(WLSLC(ISL),EE)+TST(12)
	      IF(J==1)THEN
		      YTP(1)=WLS(LD1)
		      YTP(2)=WLM(LD1)
		      YTP(3)=WLSL(LD1)
		      YTP(4)=WLSC(LD1)
		      YTP(5)=WLMC(LD1)
		      YTP(6)=WLSLC(LD1)
		      YTP(7)=WLSN(LD1)
		      YTP(8)=WLMN(LD1)
	      END IF
	      TST(17)=EAJL(WP(ISL),EE)+TST(17)
	      TST(19)=EAJL(AP(ISL),EE)+TST(19)
	      TST(20)=EAJL(PMN(ISL),EE)+TST(20)
	      TST(21)=EAJL(FOP(ISL),EE)+TST(21)
	      TST(22)=EAJL(OP(ISL),EE)+TST(22)
	      IF(NMIX==0)THEN
		      TST(23)=EAJL(CLA(ISL),EE)+TST(23)
		      TST(24)=EAJL(SIL(ISL),EE)+TST(24)
		      TST(27)=EAJL(ROK(ISL),EE)+TST(27)
	      END IF
	      TST(25)=EAJL(DUM(ISL),EE)+TST(25)
	      TST(26)=EAJL(UP(ISL),EE)+TST(26)
	      TST(28)=EAJL(WNH3(ISL),EE)+TST(28)
	      I1=29
	      DO I=1,NDP
		      TST(I1)=EAJL(PSTZ(I,ISL),EE)+TST(I1)
		      I1=I1+1
	      END DO 
	      XX=Z(ISL)
      END DO          
      IF(J<=NBSL)THEN
	      RTO=(DMX-XX)/ZZ
          RE=RTO*EE
          IF(NMIX==0)THEN
	          BDP(ISL)=BDP(ISL)-(BDP(ISL)-.6667*BD(ISL))*RE
	          CLA(ISL)=CLA(ISL)*ZZ
	          SIL(ISL)=SIL(ISL)*ZZ
	          ROK(ISL)=ROK(ISL)*ZZ
          END IF
          PMA=PMN(ISL)+AP(ISL)
          DUM(ISL)=PSP(ISL)*PMA
          UP(ISL)=PMA-DUM(ISL)
          TST(1)=EAJL(WNO3(ISL),RE)+TST(1)
	          ! TST(2)=EAJL(WHPN(ISL),RE)+TST(2)
	          ! TST(3)=EAJL(WHSN(ISL),RE)+TST(3)
          TST(4)=EAJL(WBMN(ISL),RE)+TST(4)
          TST(5)=EAJL(WLSN(ISL),RE)+TST(5)
          TST(6)=EAJL(WLMN(ISL),RE)+TST(6)
	          ! TST(7)=EAJL(WHPC(ISL),RE)+TST(7)
	          ! TST(8)=EAJL(WHSC(ISL),RE)+TST(8)
          TST(9)=EAJL(WBMC(ISL),RE)+TST(9)
          TST(10)=EAJL(WLSC(ISL),RE)+TST(10)
          TST(11)=EAJL(WLMC(ISL),RE)+TST(11)
          TST(12)=EAJL(WLSLC(ISL),RE)+TST(12)
          TST(14)=EAJL(WLS(ISL),RE)+TST(14)
          TST(15)=EAJL(WLM(ISL),RE)+TST(15)
          TST(16)=EAJL(WLSL(ISL),RE)+TST(16)
          TST(17)=EAJL(WP(ISL),RE)+TST(17)
          TST(19)=EAJL(AP(ISL),RE)+TST(19)
          TST(20)=EAJL(PMN(ISL),RE)+TST(20)
          TST(21)=EAJL(FOP(ISL),RE)+TST(21)
          TST(22)=EAJL(OP(ISL),RE)+TST(22)
          IF(NMIX==0)THEN
	          TST(23)=EAJL(CLA(ISL),RE)+TST(23)
	          TST(24)=EAJL(SIL(ISL),RE)+TST(24)
	          TST(27)=EAJL(ROK(ISL),RE)+TST(27)
          END IF
          TST(25)=EAJL(DUM(ISL),RE)+TST(25)
          TST(26)=EAJL(UP(ISL),RE)+TST(26)
          TST(28)=EAJL(WNH3(ISL),RE)+TST(28)
          I1=29
          DO I=1,NDP
	          TST(I1)=EAJL(PSTZ(I,ISL),RE)+TST(I1)
	          I1=I1+1
          END DO
      ELSE
	      J=NBSL
	      DMX=Z(LID(NBSL))
      END IF
      J1=J-1
      ! COMPUTE MATERIAL PER DEPTH (kg/ha/m)
      DO I=1,ISM
	      TST(I)=TST(I)/DMX
      END DO
      XX=0.
      DO J=1,J1
	      LL=LID(J)
	      ZZ=Z(LL)-XX
    !     DISTRIBUTE MIXED MATERIAL UNIFORMLY THRU PLOW DEPTH
	      WNO3(LL)=TST(1)*ZZ+WNO3(LL)
    !     WHPN(LL)=TST(2)*ZZ+WHPN(LL)
    !     WHSN(LL)=TST(3)*ZZ+WHSN(LL)
	      WBMN(LL)=TST(4)*ZZ+WBMN(LL)
	      WLSN(LL)=TST(5)*ZZ+WLSN(LL)
	      WLMN(LL)=TST(6)*ZZ+WLMN(LL)
    !     WHPC(LL)=TST(7)*ZZ+WHPC(LL)
    !     WHSC(LL)=TST(8)*ZZ+WHSC(LL)
	      WBMC(LL)=TST(9)*ZZ+WBMC(LL)
	      WLSC(LL)=TST(10)*ZZ+WLSC(LL)
	      WLMC(LL)=TST(11)*ZZ+WLMC(LL)
	      WLSLC(LL)=TST(12)*ZZ+WLSLC(LL)
	      WLS(LL)=TST(14)*ZZ+WLS(LL)
	      WLM(LL)=TST(15)*ZZ+WLM(LL)
	      WLSL(LL)=TST(16)*ZZ+WLSL(LL)
	      ! IF(J==1)THEN
	      !    IF(WLS(LL)>XTP(1))CALL TMXL1(DMX,TST(14),WLS(LL),XTP(1),YTP(1))
	      !    IF(WLM(LL)>XTP(2))CALL TMXL1(DMX,TST(15),WLM(LL),XTP(2),YTP(2))
	      !    IF(WLSL(LL)>XTP(3))CALL TMXL1(DMX,TST(16),WLSL(LL),XTP(3),YTP(3))
	      !    IF(WLSC(LL)>XTP(4))CALL TMXL1(DMX,TST(10),WLSC(LL),XTP(4),YTP(4))
	      !    IF(WLMC(LL)>XTP(5))CALL TMXL1(DMX,TST(11),WLMC(LL),XTP(5),YTP(5))
	      !    IF(WLSLC(LL)>XTP(6))CALL TMXL1(DMX,TST(12),WLSLC(LL),XTP(6),YTP(6))
	      !    IF(WLSN(LL)>XTP(7))CALL TMXL1(DMX,TST(5),WLSN(LL),XTP(7),YTP(7))
	      !    IF(WLMN(LL)>XTP(8))CALL TMXL1(DMX,TST(6),WLMN(LL),XTP(8),YTP(8))
	      ! END IF
	      WLSLNC(LL)=WLSC(LL)-WLSLC(LL)
	      RSD(LL)=.001*(WLS(LL)+WLM(LL))
	      WP(LL)=TST(17)*ZZ+WP(LL)
	      AP(LL)=TST(19)*ZZ+AP(LL)
	      PMN(LL)=TST(20)*ZZ+PMN(LL)
	      FOP(LL)=TST(21)*ZZ+FOP(LL)
	      OP(LL)=TST(22)*ZZ+OP(LL)
	      DUM(LL)=TST(25)*ZZ+DUM(LL)
	      UP(LL)=TST(26)*ZZ+UP(LL)
	      IF(NMIX==0)THEN
		      ROK(LL)=TST(27)+ROK(LL)/ZZ
		      CLA(LL)=TST(23)+CLA(LL)/ZZ
		      SIL(LL)=TST(24)+SIL(LL)/ZZ
	      END IF
	      WNH3(LL)=TST(28)*ZZ+WNH3(LL)
	      I1=29
	      DO I=1,NDP
		      PSTZ(I,LL)=TST(I1)*ZZ+PSTZ(I,LL)
		      I1=I1+1
	      END DO
	      PSP(LL)=DUM(LL)/(UP(LL)+DUM(LL))
	      RX=MIN(1.,(100.-ROK(LL))/(100.-UN(LL)))
	      FC(LL)=FC(LL)*RX
	      S15(LL)=S15(LL)*RX
	      PO(LL)=PO(LL)*RX
	      CALL SPOFC(LL)
	      SAN(LL)=100.-CLA(LL)-SIL(LL)
	      WT(LL)=BD(LL)*ZZ*1.E4
	      XX=Z(LL)
      END DO
      XX=DMX-Z(LID(J1))
      WNO3(ISL)=WNO3(ISL)+TST(1)*XX
!     WHPN(ISL)=WHPN(ISL)+TST(2)*XX
!     WHSN(ISL)=WHSN(ISL)+TST(3)*XX
      WBMN(ISL)=WBMN(ISL)+TST(4)*XX
      WLSN(ISL)=WLSN(ISL)+TST(5)*XX
      WLMN(ISL)=WLMN(ISL)+TST(6)*XX
!     WHPC(ISL)=WHPC(ISL)+TST(7)*XX
!     WHSC(ISL)=WHSC(ISL)+TST(8)*XX
      WBMC(ISL)=WBMC(ISL)+TST(9)*XX
      WLSC(ISL)=WLSC(ISL)+TST(10)*XX
      WLMC(ISL)=WLMC(ISL)+TST(11)*XX
      WLSLC(ISL)=WLSLC(ISL)+TST(12)*XX
      WLS(ISL)=WLS(ISL)+TST(14)*XX
      WLM(ISL)=WLM(ISL)+TST(15)*XX
      WLSL(ISL)=WLSL(ISL)+TST(16)*XX
      WLSLNC(ISL)=WLSC(ISL)-WLSLC(ISL)
      RSD(ISL)=.001*(WLS(ISL)+WLM(ISL))
      WP(ISL)=WP(ISL)+TST(17)*XX
      AP(ISL)=AP(ISL)+TST(19)*XX
      PMN(ISL)=PMN(ISL)+TST(20)*XX
      FOP(ISL)=FOP(ISL)+TST(21)*XX
      OP(ISL)=OP(ISL)+TST(22)*XX
      DUM(ISL)=DUM(ISL)+TST(25)*XX
      UP(ISL)=UP(ISL)+TST(26)*XX
      IF(NMIX==0)THEN
          ROK(ISL)=ROK(ISL)+TST(27)*XX
          CLA(ISL)=CLA(ISL)+TST(23)*XX
          SIL(ISL)=SIL(ISL)+TST(24)*XX
      END IF
      WNH3(ISL)=WNH3(ISL)+TST(28)*XX
!     CALL NCONT(VAR(77),VAR(75),VAR(76),VAR(65),VAR(73))
      I1=29
      DO I=1,NDP
          PSTZ(I,ISL)=PSTZ(I,ISL)+TST(I1)*XX
          I1=I1+1
      END DO
      PSP(ISL)=DUM(ISL)/(UP(ISL)+DUM(ISL))
      ZZ=Z(ISL)-Z(LID(J1))
      IF(NMIX==0)THEN
          ROK(ISL)=ROK(ISL)/ZZ
          CLA(ISL)=CLA(ISL)/ZZ
          SIL(ISL)=SIL(ISL)/ZZ
      END IF
      IF(UN(ISL)>0.)THEN
          RX=MIN(1.,(100.-ROK(ISL))/(100.-UN(ISL)))
          FC(ISL)=FC(ISL)*RX
          S15(ISL)=S15(ISL)*RX
          PO(ISL)=PO(ISL)*RX
          CALL SPOFC(ISL)
      END IF
      SAN(ISL)=100.-CLA(ISL)-SIL(ISL)
      WT(ISL)=BD(ISL)*ZZ*1.E4
      AD2=0.
      ZLSC=0.
      ZLMC=0.
      ZBMC=0.
      ZHSC=0.
      ZHPC=0.
      DO I=1,NBSL
          ISL=LID(I)
          ZLSC=ZLSC+WLSC(ISL)                                                              
          ZLMC=ZLMC+WLMC(ISL)                                                              
          ZBMC=ZBMC+WBMC(ISL)                                                              
          ZHSC=ZHSC+WHSC(ISL)                                                              
          ZHPC=ZHPC+WHPC(ISL)                                                              
          AD2=AD2+WBMC(ISL)+WHPC(ISL)+WHSC(ISL)+WLMC(ISL)+WLSC(ISL)
      END DO
      DF=AD1-AD2
!      IF(ABS(DF)>.1)WRITE(KW(1),1)IYR,MO,KDA,ZLSC1,ZLSC,ZLMC1,ZLMC,ZBMC1,&
!      ZBMC,ZHSC1,ZHSC,ZHPC1,ZHPC,AD1,AD2,DF
      IF(EE<.95)RETURN
      LD2=LID(2)
      X1=STL(JJK)+STD(JJK)+STDO
      DM(JJK)=DM(JJK)-STL(JJK)
      XX=STL(JJK)/(DM(JJK)+1.E-10)
      X2=XX*UN1(JJK)
      X3=XX*UP1(JJK)
      W1=XX*UK1(JJK)
      X4=STDN(JJK)+STDON+X2
      CALL NCNSTD(X1,X4,0)
      WLMN(LD2)=WLMN(LD2)+WLMN(LD1)
      WLMN(LD1)=0.
      WLSN(LD2)=WLSN(LD2)+WLSN(LD1)
      WLSN(LD1)=0.
      WLS(LD2)=WLS(LD2)+WLS(LD1)
      WLS(LD1)=0.
      WLM(LD2)=WLM(LD2)+WLM(LD1)
      WLM(LD1)=0.
      WLSL(LD2)=WLSL(LD2)+WLSL(LD1)
      WLSL(LD1)=0.
      WLSC(LD2)=WLSC(LD2)+WLSC(LD1)
      WLSC(LD1)=0.
      WLMC(LD2)=WLMC(LD2)+WLMC(LD1)
      WLMC(LD1)=0.
      WLSLC(LD2)=WLSLC(LD2)+WLSLC(LD1)
      WLSLC(LD1)=0.
      WLSLNC(LD2)=WLSLNC(LD2)+WLSLNC(LD1)
      WLSLNC(LD1)=0.
      FOP(LD2)=FOP(LD2)+FOP(LD1)+STDP+STDOP+X3
      FOP(LD1)=0.
      AP(LD2)=AP(LD2)+AP(LD1)
      AP(LD1)=0.
      SOLK(LD2)=SOLK(LD2)+STDK+STDOK+W1
      RSD(LD1)=0.
      STL(JJK)=0.
      STDO=0.
      STD(JJK)=0.
      STDN(JJK)=0.
      STDON=0.
      STDL=0.
      STDP=0.
      STDOP=0.
      STDK=0.
      STDOK=0.
      UN1(JJK)=MAX(1.E-5,UN1(JJK)-X2)
      UP1(JJK)=UP1(JJK)-X3
      UK1(JJK)=MAX(1.E-5,UK1(JJK)-W1)
      RETURN
    1 FORMAT(1X,'&&&&&',3I4,20E16.6)  
      END