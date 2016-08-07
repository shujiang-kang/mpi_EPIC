      SUBROUTINE TLOP(CSTX,COX,JRT)
!     EPIC1102
!     THIS SUBPROGRAM CONTROLS ALL TILLAGE OPERATIONS INCLUDING PLANTING
!     HARVESTING, AND AUTOMATIC FERTILIZER APPLICATIONS AT PLANTING.
      USE PARM
	  DIMENSION YTP(16)
      FNPP(X)=DMLA(JJK)*X/(X+EXP(PPCF(1,JJK)-PPCF(2,JJK)*X))
      JRT=0
      II=IHC(JT1)
      NN=NBC(IRO)
      N1=MAX(1,NCP(JJK))
      X1=CND(IRO,KT)
      IF(ABS(X1-CN0)>0.)THEN
          X2=SMX
          CALL HCNSLP(X1,X3)
          CN0=X1
          CN2=X1
          SCI=SMX*SCI/X2
      END IF
      IF(II==NHC(1).OR.II==NHC(2).OR.II==NHC(3).OR.II==NHC(19)&
      .OR.II==NHC(22))GO TO 10
      IF(II==NHC(5))GO TO 61
	  IF(II==NHC(6))GO TO 53
	  IF(II==NHC(7).OR.II==NHC(8))GO TO 57
	  IF(II==NHC(10))GO TO 51
	  IF(II==NHC(11))GO TO 52
	  IF(II==NHC(12).OR.II==NHC(13))GO TO 56
	  IF(II==NHC(14))GO TO 59
	  IF(II==NHC(23))GO TO 63
	  IF(II==NHC(24))GO TO 64
	  GO TO 6
   51 CSTX=-CSTX*YLD1(N1,JJK)/(1.-WCY(JJK))
      COX=CSTX
      GO TO 57
   52 CSTX=-CSTX*YLD(JJK)/(1.-WCY(JJK))
      COX=CSTX
      GO TO 57
   56 IF(ICUS(JT1)==0)GO TO 57
      CSTX=-CSTX*YLD(JJK)/(1.-WCY(JJK))
      COX=CSTX
      GO TO 57
   53 IDRL=1
   61 ISL=LID(2)
      DO K=1,NN
          I2=LY(IRO,K)
          IF(JH(IRO,KT)==KDC(I2))GO TO 27
      END DO
      GO TO 26
   27 IF(KG(I2)>0)GO TO 26
      IF(STMP(ISL)<TBSC(I2)+2..AND.MO<12)THEN
          KOMP(KT)=0
          JRT=1
          RETURN
      END IF
      AWC=RZSW
      AQV=0.
      ARF=0.
	  IGO=IGO+1
      DO KC=1,NN
          IF(JE(KC)>MNC)EXIT
      END DO
      JE(KC)=I2
      JJK=I2
      KG(JJK)=1
      JP(JJK)=0
	  IYH(JJK)=1
	  GSEP=0.
	  GSVP=0.
	  SRA=0.
      SWH(JJK)=0.
      SWP(JJK)=0.
      ACET(JJK)=0.
      XDLAI(JJK)=DLAI(JJK)
	  XDLA0(JJK)=0.
      WCYD=.3
      STDO=STDO+STD(JJK)
      STDOK=STDOK+STDK
      STDON=STDON+STDN(JJK)
      STDOP=STDOP+STDP
      STD(JJK)=0.
      STDK=0.
      STDL=0.
      STDN(JJK)=0.
      STDP=0.
      RD(JJK)=TLD(JT1)
      ROSP=RIN(JT1)
      HU(JJK)=0.
      DM(JJK)=SDW(JJK)*5.E-4
      DM1(JJK)=DM(JJK)
      SM99=420.*DM(JJK)
      VAR(99)=VAR(99)+SM99
      SMM(99,MO)=SMM(99,MO)+SM99                                                                             
      RW(JJK)=.4*DM(JJK)
      RWT(ISL,JJK)=RW(JJK)
      CHT(JJK)=0.
      PPL0(JJK)=POP(JJK,IHU(JJK))
      XLAI(JJK)=FNPP(PPL0(JJK))
	  DMLX(JJK)=XLAI(JJK)
      X1=SDW(JJK)*CSTS(JJK)
      COST=COST+X1
      SMM(96,MO)=SMM(96,MO)+CCEM(JJK)*SDW(JJK)
      LRD=2
      JPL(JJK)=1
	  IF(NCP(JJK)==0)NCP(JJK)=1
      N1=MAX(1,NCP(JJK))
      IPLD(N1,JJK)=IYR*10000+MO*100+KDA
	  IHVD(N1,JJK)=0
!      IF(NOP>0)WRITE(KW(1),32)IYR,MO,KDA,CPNM(JJK),CV,X1
      IF(KFL(20)>0)WRITE(KW(20),49)IYR,MO,KDA,TIL(JT1),KDC(JJK),II,&
     &NBE(JT1),NBT(JT1),X1,X1,SDW(JJK)
    6 EE=EMX(JT1)
      PPL0(JJK)=(1.-FPOP(JT1))*PPL0(JJK)
      XLAI(JJK)=FNPP(PPL0(JJK))
	  DMLX(JJK)=XLAI(JJK)
      DMX=TLD(JT1)
	  IF(II/=NHC(19).AND.II/=NHC(2))CALL TMIX(EE,DMX,0)
      IF(DMX>BIG)TLD(JT1)=BIG
	  IF(II==NHC(15))THEN
	      SATC(LID(2))=PRMT(33)
	  ELSE
	      IF(II==NHC(16))SATC(LID(2))=SATK
	  END IF
   57 IF(IDR>0)THEN
          IF(II==NHC(25))THEN
              HCL(IDR)=HCLN
          ELSE
              IF(II==NHC(26))HCL(IDR)=HCLD
          END IF
      END IF
      IF(KFL(20)>0)WRITE(KW(20),50)IYR,MO,KDA,TIL(JT1),KDC(JJK),II,&
      NBE(JT1),NBT(JT1),CSTX,COX,FULU(JT1)
	  SMM(92,MO)=SMM(92,MO)+FULU(JT1) 
	  IF(II==NHC(2).OR.II==NHC(3).OR.II==NHC(19))GO TO 26
    7 XX=TLD(JT1)*1000.
!      IF(NOP>0)WRITE(KW(1),28)IYR,MO,KDA,TIL(JT1),XX,XHSM,CSTX
      IF(II/=NHC(17).AND.II/=NHC(18))GO TO 26
      IF(II/=NHC(18))THEN
          DHT=DKH(JT1)
          DKHL=DHT
          DKIN=DKI(JT1)
	      IFD=1
      ELSE
          IFD=0
      END IF
!      IF(NOP>0)WRITE(KW(1),30)DHT,DKIN,XHSM
      GO TO 26
   59 CALL TBURN
      GO TO 7
   63 ICV=1
      GO TO 57
   64 ICV=0
      GO TO 57
   10 DO K=1,NN
          IF(JE(K)>MNC)CYCLE
	      IF(JH(IRO,KT)==KDC(JE(K)))EXIT
	  END DO
	  IF(K>NN)THEN
          CSTX=0.
          COX=0.
          JRT=1
          RETURN
      END IF
      JJK=JE(K)
      IF(IDC(JJK)==NDC(7).OR.IDC(JJK)==NDC(8).OR.IDC(JJK)==NDC(10))THEN
          IF(IYH(JJK)/=LYR(IRO,KT).AND.LYR(IRO,KT)/=1)THEN
 	          KOMP(KT)=1
	          RETURN
	      END IF
	  END IF
      N1=MAX(1,NCP(JJK))
      JHV=K
      KHV=1
      IF(II==NHC(1))GO TO 22
	  IF(II/=NHC(2).AND.II/=NHC(19).AND.II/=NHC(22))THEN
          IF(IHT(JT1)>0)GO TO 26
          IHT(JT1)=1
      END IF
      HVWC=HWC(IRO,KT)                                                                 
      IF(HVWC>0..AND.WCYD>HVWC.AND.MO<MOFX)THEN
          JRT=1
          KOMP(KT)=0
          RETURN
      END IF
      IF(JP(JJK)==0)THEN
          JP(JJK)=1
          IF(II/=NHC(3))NCR(JJK)=NCR(JJK)+1
      END IF
      HUF(JJK)=MAX(HUF(JJK),HU(JJK))
      DMF(N1,JJK)=DM1(JJK)
      TRA(JJK)=SRA+TRA(JJK)
      IF(RD(JJK)>RDF(JJK))RDF(JJK)=RD(JJK)
      X9=DM(JJK)+.001
      X2=MAX(BN(3,JJK),UN1(JJK)/X9)
      X7=.001*X2
      X3=UP1(JJK)/X9
      X8=UK1(JJK)/X9
	  XX=STD(JJK)+1.E-10
      RNR=STDN(JJK)/XX
      RPR=STDP/XX
      RKR=STDK/XX
      STDL=CLG*XX
      RLR=MIN(.8,STDL/(STD(JJK)+1.E-5))
      IF(ORHI(JT1)<1.E-10)THEN
	      IF(IDC(JJK)==NDC(8))THEN
	          F=1.
	      ELSE
              XX=100.*SWH(JJK)/(SWP(JJK)+1.E-5)
	          F=XX/(XX+EXP(SCRP(10,1)-SCRP(10,2)*XX))
	      END IF
          XX=MAX(AJHI(JJK)-WSYF(JJK),0.)
	      FT=MAX(.1,1.+PRMT(50)*(IYR-2000))
          X1=MIN(F*XX+WSYF(JJK),.9*DM(JJK)/(STL(JJK)+1.E-10))*FT
	      IF(IDC(JJK)==NDC(8))THEN
	          X2=PRMT(76)/AWC
	          X1=MIN(HI(JJK),X1*X2)
	      END IF
          X2=1000.*CNY(JJK)*(X7/BN(3,JJK))**.1
          X3=1000.*CPY(JJK)*(.001*X3/BP(3,JJK))**.1
          GO TO 17
      END IF
      IF(II/=NHC(19).AND.II/=NHC(22))GO TO 16
      IF(IDC(JJK)==NDC(7).OR.IDC(JJK)==NDC(8).OR.IDC(JJK)==NDC(10))THEN
          KTT=0
          GO TO 26
      END IF
      KOMP(KT)=0
      KTT=KT
	  IF(II==NHC(22))THEN
	      XX=CHT(JJK)-HMO(JT1)
          IF(XX<.001.OR.NMW<IMW)RETURN
          X1=XX/CHT(JJK)
          ZZ=HMO(JT1)/CHT(JJK)
          YZ=ZZ*STD(JJK)
          NMW=0
          GO TO 45
      END IF
      IF(AGPM<GZLM)RETURN
      GCOW=WSA/RSTK(IRO,KT)
      XX=GCOW*ORHI(JT1)/(WSA*HE(JT1))
      X1=MIN(XX/AGPM,.9)
      GO TO 17
   16 X1=ORHI(JT1)
      IF(TLD(JT1)<=0.)GO TO 17
      CALL THVRT(YY,X3,X1,X6,X7,N1)
      GO TO 25
   17 ZZ=MAX(.01,1.-X1)
      YZ=X1*STD(JJK)
   45 XZ=X1*STL(JJK)
      HIF(N1,JJK)=X1
      AJHI(JJK)=0.
      CHT(JJK)=CHT(JJK)*ZZ
 	  HU(JJK)=HU(JJK)*PRMT(69)
      SLAI(JJK)=SLAI(JJK)*ZZ
      STL(JJK)=STL(JJK)*ZZ
      STD(JJK)=MAX(1.E-10,STD(JJK)-YZ)
      STDL=STDL*ZZ
      CALL PESTF
      TPSF(N1,JJK)=TPSF(N1,JJK)+PSTF(JJK)
      NPSF(N1,JJK)=NPSF(N1,JJK)+1
      YLD(JJK)=XZ*HE(JT1)*PSTF(JJK)
      YLSD=YZ*HE(JT1)
      Y4=YZ*RNR
      Y5=YZ*RPR
      Y6=YZ*RKR
      STDN(JJK)=MAX(1.E-5,STDN(JJK)-Y4)
      STDP=MAX(1.E-5,STDP-Y5)
      STDK=MAX(1.E-5,STDK-Y6)
      STDL=MAX(STDL-YZ*RLR,.1*STD(JJK))
      X6=PSTF(JJK)
      X4=MIN(XZ*X2,UN1(JJK))
      X5=MIN(XZ*X3,UP1(JJK))
      X11=XZ-YLD(JJK)+YZ-YLSD
      Z2=YLSD*RNR
      Z3=YLSD*RPR
      Z4=YLSD*RKR
      YLN=MIN(.9*(UN1(JJK)+STDN(JJK)),YLD(JJK)*X2+Z2)
      YLP=MIN(.9*(UP1(JJK)+STDP),YLD(JJK)*X3+Z3)
      X10=X4-YLN+Y4
      CALL NCNSTD(X11,X10,0)
      FOP(LD1)=FOP(LD1)+X5-YLP+Y5
      YY=YLD(JJK)+YLSD
      YLD(JJK)=YY
      YLC=.42*YY
      IF(ORHI(JT1)>0.)THEN
          YLD2(N1,JJK)=YLD2(N1,JJK)+YY
          X11=YY*PRYF(JJK)
      ELSE
          IF(IDC(JJK)==NDC(9))THEN
              YLD1(N1,JJK)=YLD1(N1,JJK)+FTO(JJK)*YY
              YLD2(N1,JJK)=YLD2(N1,JJK)+YLD1(N1,JJK)*(1./FLT(JJK)-1.)
          ELSE
              YLD1(N1,JJK)=YLD1(N1,JJK)+YY
              X10=YY*PRYG(JJK)
          END IF            
      END IF
      X3=RW(JJK)
      RWF(N1,JJK)=X3
      JD=JJK
	  SRAF=SRA
	  SRA=0.
      UN1(JJK)=MAX(1.E-5,UN1(JJK)-X4)
      UP1(JJK)=UP1(JJK)-X5
      DM(JJK)=DM(JJK)-XZ
      YLNF(N1,JJK)=YLNF(N1,JJK)+YLN
      YLPF(N1,JJK)=YLPF(N1,JJK)+YLP
      YLKF(N1,JJK)=YLKF(N1,JJK)+YLK
      YLCF(N1,JJK)=YLCF(N1,JJK)+YLC
	  GO TO 25
   22 IF(IPD==5)THEN
          CALL SPRNT(YTP)
!          WRITE(KW(1),'(T5,A)')'SOIL DATA'
!          CALL SOLIO(YTP,1)   !skang
      END IF
      CALL TRDST
      NCP(JJK)=MIN(NBCX(IRO,JJK),NCP(JJK)+1)
	  IF(YLD1(N1,JJK)+YLD2(N1,JJK)<1.E-10)NCR(JJK)=NCR(JJK)+1
	  JE(JHV)=MNC+1
      GSEF=GSEP
	  SMGS(4)=SMGS(4)+GSEF
	  GSEP=0.
	  GSVF=GSVP
	  SMGS(6)=SMGS(6)+GSVF
	  GSVP=0.
      IGO=MAX(0,IGO-1)
      KG(JJK)=0
	  IYH(JJK)=0
	  JPL(JJK)=0
      HU(JJK)=0.
      HUI(JJK)=0.
      HSM=0.
      SLAI(JJK)=0.
      WLV(JJK)=0.
      ANA(JJK)=0.
      NFA=0
      NII=IRI
      CSTF(N1,JJK)=COST
      CSOF(N1,JJK)=COST-CSFX
      COST=0.
      CSFX=0.
      IHU(JJK)=IHU(JJK)+1
      IF(IHU(JJK)>NHU(JJK))IHU(JJK)=1
      CAW(N1,JJK)=AWC
      CQV(N1,JJK)=AQV
      CRF(N1,JJK)=ARF 
      ETG(N1,JJK)=ACET(JJK)+ETG(N1,JJK)
	  PSTS=0.
      IPST=0
      WS=1.
	  FGC=0.
	  U=0.
	  UN=0.
      GO TO 6
   25 TYN=TYN+YLN
      TYP=TYP+YLP
      TYK=TYK+YLK
      TYC=TYC+YLC
      IHVD(N1,JJK)=IYR*10000+MO*100+KDA
      IF(ICUS(JT1)/=0.AND.CSTX<=0.)THEN
          CSTX=-CSTX*YLD(JJK)
          COX=CSTX
      END IF
 !     IF(NOP>0.AND.II/=NHC(19))WRITE(KW(1),29)IYR,MO,KDA,TIL(JT1),&
 !     CPNM(JD),YY,YLSD,AGPM,X9,X3,X1,X6,X7,WCYD,XHSM,CSTX
      GO TO 6
 !  26 IF(NOP>0.AND.II==NHC(19))WRITE(KW(1),62)IYR,MO,KDA,TIL(JT1),&
      !CPNM(JJK),YLD(JJK),YLSD,AGPM,STL(JJK),X3,X1,XHSM
26	  IF(KFL(29)>0.AND.II==NHC(19))WRITE(KW(29),31)IYR,MO,KDA,TIL(JT1),&
      CPNM(JJK),DM(JJK),X3,SLAI(JJK),STL(JJK),AGPM,X1,YLD(JJK),YLSD,XHSM 
      RETURN
   28 FORMAT(1X,3I4,2X,A8,2X,'DPTH = ',F5.0,'mm',2X,'HUSC = ',F4.2,2X&
      ,'COST=',F7.0,'$/ha')
   29 FORMAT(1X,3I4,2X,A8,2X,A4,2X,'YLD=',F7.2,'t/ha',2X,'YLSD=',F7.2,&
      't/ha'2X,'AGPM=',F7.2,'t/ha',2X,'BIOM=',F7.2,'t/ha',2X,'RW=',F5.2,&
      't/ha',2X,'HI=',F6.2,2X,'PSTF=',F4.2,2X,'NCN=',F5.3,'G/G',2X,'WCY=',&
      F4.2,2X,'HUSC=',F4.2,2X,'COST=',F7.0,'$/ha')
   30 FORMAT('+',T45,'DKH=',F5.0,'MM',2X,'DKI=',F6.2,'M',2X,'HUSC=',F4.2)
   31 FORMAT(1X,3I4,2X,A8,2X,A4,9F10.3)
   32 FORMAT(1X,3I4,2X,A4,2X,'RSD = ',F5.1,'T',2X,'COST=',F7.0,'$/ha')
   49 FORMAT(1X,3I4,2X,A8,8X,I6,6X,3I4,F10.2,10X,3F10.2)
   50 FORMAT(1X,3I4,2X,A8,8X,I6,6X,3I4,2F10.2,20X,F10.2)
   62 FORMAT(1X,3I4,2X,A8,2X,A4,2X,'YLD=',F7.4,'t/ha',2X,'YSD=',F7.2,&
        't/ha',2X,'AGPM=',F7.2,'t/ha',2X,'STL=',F7.2,'t/ha',2X,'RWT=',F7.2,&
        't/ha',2X,'HI=',F7.3,2X,'HUSC=',F4.2)
      END
