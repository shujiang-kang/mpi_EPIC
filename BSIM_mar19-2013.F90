      SUBROUTINE BSIM                                                                
!     EPIC1102                                                                       
!     THIS SUBPROGRAM DRIVES THE DAILY SIMULATION FOR ANY                            
!     NUMBER OF YEARS                                                                
      USE PARM
      CHARACTER*2 HDN                                                                                                                                      
      CHARACTER*4 ANMX,HD28                                                               
      CHARACTER*7 HD30,HDG                                                               
	  REAL*8 PPX 
	  DIMENSION HDN(30),HD30(30),HDG(15)
	  DIMENSION HD28(15,10)                                                          
      DIMENSION KDT(12,12),KTP(7),MNST(7)                                            
      DIMENSION XTP(4,12,12),YTP(16)                                                 
      DIMENSION PPX(13)                                                              
      DATA HD28/'  Z1','  Z2','  Z3','  Z4','  Z5','  Z6','  Z7','  Z8',&            
      '  Z9',' Z10',' Z11',' Z12',' Z13',' Z14',' Z15',' SW1',' SW2',&               
      ' SW3',' SW4',' SW5',' SW6',' SW7',' SW8',' SW9','SW10','SW11',&               
      'SW12','SW13','SW14','SW15',' WU1',' WU2',' WU3',' WU4',' WU5',&               
      ' WU6',' WU7',' WU8',' WU9','WU10','WU11','WU12','WU13','WU14',&               
      'WU15',' EV1',' EV2',' EV3',' EV4',' EV5',' EV6',' EV7',' EV8',&               
      ' EV9','EV10','EV11','EV12','EV13','EV14','EV15',' PK1',' PK2',&               
      ' PK3',' PK4',' PK5',' PK6',' PK7',' PK8',' PK9','PK10','PK11',&               
      'PK12','PK13','PK14','PK15',' SF1',' SF2',' SF3',' SF4',' SF5',&               
      ' SF6',' SF7',' SF8',' SF9','SF10','SF11','SF12','SF13','SF14',&               
      'SF15',' N31',' N32',' N33',' N34',' N35',' N36',' N37',' N38',&               
      ' N39','N310','N311','N312','N313','N314','N315',' UN1',' UN2',&               
      ' UN3',' UN4',' UN5',' UN6',' UN7',' UN8',' UN9','UN10','UN11',&               
      'UN12','UN13','UN14','UN15',' LN1',' LN2',' LN3',' LN4',' LN5',&               
      ' LN6',' LN7',' LN8',' LN9','LN10','LN11','LN12','LN13','LN14',&               
      'LN15','  T1','  T2','  T3','  T4','  T5','  T6','  T7','  T8',&               
      '  T9',' T10',' T11',' T12',' T13',' T14',' T15'/
      DATA HDG/'   GFO2','  GFCO2','  GFN2O','  DFO2S',' DBFO2B','  DFO2T',&
      '    QO2',' DFCO2S',' DFCO2B',' DFCO2T','   QCO2',' DFN2OS',' DFN2OB',&
      ' DFN2OT','   QN2O'/
      DATA HD30/'     ZC','    VWC','    AFP','   HKPO','   DPRO','   HKPC',&
      '   DPRC','   HKPN','   DPRN','   SMEA','   SMES','    EAR','   WNO3',&
      '   WNO2','   WO2L','   WO2G','DO2CONS','  SSFO2','    VO2','  WCO2L',&
      '  WCO2G','DCO2GEN',' SSFCO2','   VCO2','  WN2OL','  WN2OG',' SSFN2O',&
      '   VN2O','  DN2OG','   DN2G'/
      DATA HDN/'1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 ','10','11',&
      '12','13','14','15','16','17','18','19','20','21','22','23','24',&
      '25','26','27','28','29','30'/ 
      DATA AIR,SAIR,TSMQ,TSMY/4*0./                                             
      DATA MNST/1,2,3,4,5,6,7/,IN0,KTF/2*0/                                                
      PMAV(FMO1,FMO2,X1,X2)=(FMO1*X1+FMO2*X2)/30.
      PLC0=0.                                    
      IF(KFL(5)>0)WRITE(KW(5),471)HEDP                                               
      JP(JJK)=0                                                                      
      IF(KFL(10)>0)WRITE(KW(10),517)                                                 
      TILG=0.                                                                        
      IF(KFL(11)>0)WRITE(KW(11),526)HEDS(6),HED(4),HED(11),HED(14),&                 
      HED(17),HED(16)                                                                
      IF(KFL(12)>0)WRITE(KW(12),527)                                                 
!     IF(KFL(6)>0)WRITE(KW(6),477)(HED(KA(I)),I=1,NKA)                               
	  IF(KFL(6)>0)WRITE(KW(6),477)(HED(IFS(J)),J=1,NFS),(HEDS(I),&                        
      I=4,6)                                                                         
	  IF(KFL(22)>0)WRITE(KW(22),520)HED(4),HED(10),HED(11),HED(13),&                      
      HED(14),(HED(I),I=16,20),(HEDS(I),I=6,8),'RNO3',(HED(I),I=43,46),&             
      HED(49),HED(89),HED(52),HED(85),HED(50),(HED(I),I=59,61),'UNO3',&              
      ' YLN','CPNM',' YLD','TOTN'                                                    
      IF(KFL(27)>0)WRITE(KW(27),516)HED(4),HED(10),HED(11),HED(13),&                  
      HED(14),(HED(I),I=16,20),(HEDS(I),I=6,8)                                       
      IF(KFL(28)>0)WRITE(KW(28),731)HED(4),HED(10),HED(11),(HED(I),&                 
      I=13,20),(HEDS(I),I=6,8),((HD28(I,J),I=1,NBSL),J=1,10)                         
      IF(KFL(30)>0)THEN
          WRITE(KW(30),'(T15,3(A,E16.6))')'XKN1=',XKN1,' XKN3=',XKN3,&
          ' XKN5=',XKN5                                            
          WRITE(KW(30),730)HED(4),((HD30(J),HDN(I),I=1,NBCL),&
             J=2,3),((HD30(J),HDN(I),I=1,NBCL),J=10,17),(HDG(I),'  ',I=4,7),&
             ((HD30(J),HDN(I),I=1,NBCL),J=18,22),(HDG(I),'  ',I=8,11),((HD30(J),&
             HDN(I),I=1,NBCL),J=23,26),(HDG(I),'  ',I=12,15),((HD30(J),HDN(I),&
             I=1,NBCL),J=27,30)                             
      END IF                            
      SILG=AILG                                                                      
      J2=0                                                                           
      J1=1                                                                           
	  IGIS=NGF                                                                            
	  DO 87 IY=1,NBYR                                                                
      JDA=IBD                                                                        
	  KDT=0                                                                               
	  SWGS=0.                                                                             
	  HSM=0.
	 ! IF(IBAT==0)WRITE(*,156)IY,NBYR !Reducing output by Skang 3/19/2013                                                                 
	  IF(ICO2==1)THEN                                                                     
          IF(IYX<25)THEN                                                               
              CO2=280.33                                                               
          ELSE                                                                         
              X1=IYX                                                                   
              CO2=280.33-X1*(.1879-X1*.0077)                                           
	      END IF                                                                            
      END IF
      IRO=IRO+1                                                                      
      IF(IRO>NRO)IRO=1                                                               
      IF(IRO<NRO)THEN                                                                
          IRLX=0                                                                       
      ELSE                                                                           
          IRL=IRL+1                                                                    
          IRLX=IRL                                                                     
      END IF                                                                         
      NN=NBC(IRO)                                                                    
      NN1=NTL(IRO)                                                                   
	  DO I=1,LC                                                                           
     	  WA(I)=100.*CO2/(CO2+EXP(WAC2(1,I)-CO2*WAC2(2,I)))                            
      END DO                                                                         
      IF(KFL(7)>0)WRITE(KW(7),474)IRUN,IRO0,IGN,IY                                   
      DO I=1,MNC                                                                     
          JE(I)=MNC+1                                                                  
          IF(KG(I)==0)CYCLE                                                            
          DO J=1,NN                                                                    
              IF(I==LY(IRO,J))EXIT                                                     
          END DO                                                                       
          JE(J)=I                                                                      
      END DO                                                                         
      SILG=TILG+SILG                                                                 
      AILG=SILG/IY                                                                   
      TILG=0.                                                                        
      TFLG=0.                                                                        
      KOMP=0                                                                         
      IGZ=0                                                                          
      KTT=0                                                                          
      KTMX=1                                                                         
      JJ=IBD-1                                                                       
      JT1=LT(IRO,KT)                                                                 
      CALL APAGE(1)                                                                  
      WRITE(KW(1),582)CO2                                                            
      WRITE(KW(1),94)                                                                
      ND=366-NYD                                                                     
      JP(1)=0                                                                        
      JP(2)=0                                                                        
      IPC=1                                                                          
      IF(IPAT>0)THEN
          IF(APBC<20.)THEN
              APMU=2.25*(30.-APBC)
              JJK=LY(IRO,1)
              IF(APMU>45.)CALL NFERT(5,JT1)
          END IF
      END IF                                                                               
      X1=.2+.3*EXP(-.0256*SAN(LD1)*(1.-.01*SIL(LD1)))                                
      X2=(SIL(LD1)/(CLA(LD1)+SIL(LD1)))**.3                                          
      X5=.1*WOC(LD1)/WT(LD1)                                                         
      IF(X5>5.)THEN                                                                  
          X3=.75                                                                       
      ELSE                                                                           
          X3=1.-.25*X5/(X5+EXP(3.718-2.947*X5))                                        
      END IF                                                                         
      XX=1.-.01*SAN(LD1)                                                             
      X4=1.-.7*XX/(XX+EXP(-5.509+22.899*XX))                                         
      EK=X1*X2*X3*X4                                                                 
      USL=EK*SL*PEC                                                                  
      SUM=(SAN(LD1)*.0247-SIL(LD1)*3.65-CLA(LD1)*6.908)/100.                         
      DG=EXP(SUM)                                                                    
      REK=9.811*(.0034+.0405*EXP(-.5*((LOG10(DG)+1.659)/.7101)**2))                 
	  RSK=REK*PEC*RSF                                                                     
      RSLK=RSK*RLF                                                                   
      IF(ISTA==0.OR.IY==1)CALL EWIK                                                  
      NT1=0                                                                          
      IF(IWP5==0)THEN
          IWIX=1                                                                    
      ELSE
          READ(KR(20),533,IOSTAT=NFL)IWIX
	      IF(NFL/=0)THEN
	          IWIX=1
          ELSE                                       
              DO I=1,12                                                                      
                  IF(IWIX(I)/=0)THEN                                                           
                      IWIX(I)=IWIX(I)+1                                                        
	                  CYCLE                                                                         
	              END IF                                                                            
                  IWIX(I)=1                                                                    
              END DO
          END IF                                                                                       
      END IF
!     BEGIN DAILY SIMULATION.                                                        
      DO 84 IDA=IBD,ND                                                               
      CALL AICL                                                                      
      XDA=KDA                                                                        
	  NMW=NMW+1                                                                           
      NWI=IWIX(MO)                                                                   
      ISIX(NWI)=ISIX(NWI)+1                                                          
      IP15=IDA+15                                                                    
      CALL AXMON(IP15,MOP)
      DFX=0.                                                           
      IF(IP15>ND)IP15=IP15-ND                                                        
      FMO2=MIN(30,IP15-NC(MOP))                                                      
      FMO1=30.-FMO2                                                                  
      I1=MOP-1                                                                       
      IF(I1<1)I1=12                                                                  
      I2=IWIX(I1)                                                                    
      I3=IWIX(MOP)                                                                   
      RFVM=PMAV(FMO1,FMO2,RST(1,I2,I1),RST(1,I3,MOP))                                
      RFSD=PMAV(FMO1,FMO2,RST(2,I2,I1),RST(2,I3,MOP))                                
      RFSK=PMAV(FMO1,FMO2,RST(3,I2,I1),RST(3,I3,MOP))                                
      PRWM=PMAV(FMO1,FMO2,PRW(LW,I2,I1),PRW(LW,I3,MOP))                              
      TMXM=PMAV(FMO1,FMO2,OBMX(I2,I1),OBMX(I3,MOP))                                  
      TMNM=PMAV(FMO1,FMO2,OBMN(I2,I1),OBMN(I3,MOP))                                  
      TXSD=PMAV(FMO1,FMO2,SDTMX(I2,I1),SDTMX(I3,MOP))                                
      TNSD=PMAV(FMO1,FMO2,SDTMN(I2,I1),SDTMN(I3,MOP))                                
      SRAM=PMAV(FMO1,FMO2,OBSL(I2,I1),OBSL(I3,MOP))
      IPC=MAX(IPC,IDA)                                                               
      XDA1=31.-XDA                                                                   
      LNS=LID(NBSL)                                                                  
	  YW=0.                                                                               
      REP=0.                                                                         
      ER=1.                                                                          
	  VAR=0. 
	  CGSF=0.                                                                        
      BCV=1.                                                                         
      RCF=.9997*RCF                                                                  
      IF(CV<10.)BCV=CV/(CV+EXP(SCRP(5,1)-SCRP(5,2)*CV))                              
      SNOF=0.                                                                        
      IF(SNO>0.)THEN                                                                 
          SNOF=SNO/(SNO+EXP(SCRP(17,1)-SCRP(17,2)*SNO))                                
          BCV=MAX(SNOF,BCV)                                                            
      END IF                                                                         
      BCV=BCV*.85                                                                    
      IF(NGN==0)GO TO 11                                                             
      IF(IGSD==0)GO TO 504                                                           
      IF(IY/=IGSD)GO TO 504                                                          
      IF(IDA>NSTP)GO TO 508                                                          
!     READ DAILY WEATHER IF NOT GENERATED                                            
!  1  SRAD   = SOLAR RADIAION(MJ/M2 OR LY)(BLANK TO GENERATE                           
!  2  TMX  = MAX TEMP(C)                                                             
!  3  TMN  = MIN TEMP(C)                                                             
!  4  RFV  = RAINFALL(mm)(999. TO GENERATE OCCURRENCE & AMOUNT;                      
!            -1. TO GENERATE AMOUNT GIVEN OCCURRENCE)                                
!  5  RHD  = RELATIVE HUMIDITY (FRACTION)(BLANK TO GENERATE                          
!  6  U10  = WIND VELOCITY (M/S)(BLANK TO GENERATE                                   
!  7  X1   = ATMOSPHERIC CO2 CONC (ppm)                                              
!  8  EVI  = VEGETATION INDEX                                              
  504 READ(KR(7),103,IOSTAT=NFL)SRAD,TMX,TMN,RFV,RHD,U10,X1,EVI,REP                        
      IF(NFL/=0)GO TO 508 
      IF(X1>0..AND.ICO2==2)CO2=X1                                                           
      SRAD=SRAD*RUNT                                                                     
	  IF(RHD>1.)RHD=.01*RHD                                                               
      III=0                                                                          
      GO TO 507                                                                      
  508 NGN=0                                                                          
      WRITE(KW(1),'(/T10,3I4,A,A)')IYR,MO,KDA,' RAIN, TEMP, RAD, WIND'&            
      ,' SPEED, & REL HUM ARE GENERATED'                                             
      GO TO 11                                                                       
  507 IF(RFV<0.)THEN                                                                 
          CALL WRWD(1)                                                                 
          RFV=RFV*RNCF(MO)                                                             
      END IF                                                                         
      IF(KGN(2)>0)THEN
          IF(ABS(TMX-TMN)>0.)THEN
              IF(TMX>100..OR.TMN>100.)THEN
                  CALL WGN                                                                       
                  CALL WTAIX                                                                     
                  GO TO 18
              ELSE                                               
                  IF(TMX>TMN)GO TO 8
              END IF                      
          END IF                                                                 
      END IF                                                                       
      CALL WGN                                                                       
      CALL WTAIR                                                                     
   18 X1=TMX-TMN                                                                     
      TMX=TMX+TMXF(MO)                                                               
      TMN=TMX-TMNF(MO)*X1                                                            
    8 IF(RFV>900.)CALL WRWD(0)
      IF(SRAD<1.E-5.OR.KGN(3)==0)THEN                                                  
          IF(III==0)CALL WGN                                                           
          CALL WSOLRA                                                                  
      END IF
      IF(RHD<1.E-5.OR.RHD>900..OR.KGN(5)==0)THEN                                                 
          IF(III==0)CALL WGN                                                           
          CALL WRLHUM                                                                  
      END IF                                                                         
      IF(U10>0.AND.U10<900..AND.KGN(4)>0)GO TO 13                                                
      GO TO 12                                                                       
   11 U10=0.                                                                         
      RHD=0.                                                                         
      CALL WRWD(0)                                                                   
      RFV=RFV*RNCF(MO)                                                               
      CALL WGN                                                                       
      CALL WTAIR                                                                     
      X1=TMX-TMN                                                                     
      TMX=TMX+TMXF(MO)                                                               
      TMN=TMX-TMNF(MO)*X1                                                            
      CALL WSOLRA                                                                    
      CALL WRLHUM                                                                    
   12 CALL WNSPD                                                                     
   13 IF(ACW>0.AND.SNO<10.)THEN
          CALL WNDIR                                                                     
          CALL EWER(JRT)                                                                 
          IF(JRT==0)THEN
              YW=YW*ACW                                                                      
              SMM(41,MO)=SMM(41,MO)+RGRF                                                     
              VAR(41)=RGRF
          END IF
      END IF
      CALL WHRL
      CALL WRMX                                                                    
      IHRL(MO)=IHRL(MO)+1                                                            
      THRL(MO)=THRL(MO)+HRLT                                                         
      SRMX(MO)=SRMX(MO)+RAMX                                                                                                                            
      IF(IAZM>0)THEN                                                       
          SRAM=RAMX*MAX(.8,.21*SQRT(TMXM-TMNM))
          CALL WGN
          CALL WSOLRA
      END IF                                                               
      SMM(7,MO)=SMM(7,MO)+U10                                                        
      VAR(7)=U10                                                                     
      SMM(8,MO)=SMM(8,MO)+RHD                                                        
      VAR(8)=RHD                                                                     
      TX=(TMN+TMX)/2.
      IF(TX>0.)HSM=HSM+TX                                                                 
	  CALL SOLT                                                                      
      LD2=LID(2)                                                                     
      SMM(67,MO)=SMM(67,MO)+STMP(LD2)                                                
      VAR(67)=STMP(LD2)                                                              
      YP=0.                                                                          
      YN=0.                                                                          
      QAP=0.                                                                         
      IF(RFV>0.)THEN                                                                 
          SRD(MO)=SRD(MO)+1.                                                           
          SMM(4,MO)=SMM(4,MO)+RFV                                                      
          VAR(4)=RFV                                                                   
          ARF=ARF+RFV                                                                  
          TSNO=0.                                                                      
      END IF                                                                         
      SMM(1,MO)=SMM(1,MO)+TMX                                                        
      VNO3(LD1)=RFNC*RFV                                                                   
 	  RNO3=VNO3(LD1)                                                                           
      VAR(1)=TMX                                                                     
      SMM(2,MO)=SMM(2,MO)+TMN                                                        
      VAR(2)=TMN                                                                     
      SMM(3,MO)=SMM(3,MO)+SRAD                                                         
      VAR(3)=SRAD                                                                      
      SML=0.                                                                         
      YSD=0.                                                                         
      YERO=0.                                                                        
      CVF=0.                                                                         
      QP=0.                                                                          
      QD=0.                                                                          
      EI=0.                                                                          
      RWO=0.                                                                         
	  SNMN=0.                                                                             
      VSLT=0.                                                                        
	  YLN=0.                                                                              
      AL5=.02083                                                                     
      RFRR=0.                                                                        
      CRKF=0.
      SSF=0.                                                                        
      TSNO=TSNO+1.                                                                   
      IF(TX<=0.)THEN
          DSNO=RFV                                                                       
          SNO=SNO+DSNO                                                                   
          SMM(5,MO)=SMM(5,MO)+RFV                                                        
          VAR(5)=RFV                                                                     
          RFV=0.                                                                         
          GO TO 580
      END IF                                                                      
      IF(SNO>0..AND.SRAD>10.)CALL HSNOM                                                
      RWO=RFV+SML                                                                    
      SMM(6,MO)=SMM(6,MO)+SML                                                        
      VAR(6)=SML                                                                     
      IF(RWO<1.E-10)GO TO 580                                                        
      IF(RFV>1.E-10)CALL HRFEI                                                       
      RFV=RWO                                                                        
      VAR(28)=EI                                                                     
      X1=CLA(LD1)                                                                    
      X5=.1*WOC(LD1)/WT(LD1)                                                         
      X2=MAX(50.,63.+62.7*LOG(X5)+15.7*X1-.25*X1*X1)                                
      RFRR=(RFV/X2)**.6                                                              
      CALL HVOLQ                                                                     
      RFSM=RFSM+RFV                                                                  
      JCN=JCN+1                                                                      
      SMM(15,MO)=SMM(15,MO)+CN                                                       
      VAR(15)=CN                                                                     
      IF(QD>1.)THEN
          NQP=NQP+1                                                                      
          PRAV=PRAV+QP                                                                   
          PRSD=PRSD+QP*QP                                                                
          IF(QP>PRB)PRB=QP                                                               
          QPQ=QP/QD                                                                      
          IF(QPQ>QPQB)QPQB=QPQ                                                           
          QPS=QPS+QPQ                                                                    
          TCAV=TCAV+TC                                                                   
          IF(TC>TCMX)THEN
              TCMX=TC
          ELSE           
              IF(TC<TCMN)TCMN=TC                                                             
          END IF
      END IF
!     COMPUTE SEDIMENT YLD                                                           
      IF(ISTA==0.AND.LUN/=35)CALL EYSED                                              
  580 XX=EXP(-RFRR)                                                                  
      RRUF=MAX(1.,RRUF*XX)                                                           
      SMM(40,MO)=SMM(40,MO)+RRUF                                                     
      VAR(40)=RRUF                                                                   
      XX=EXP(-.1*YSD(3))                                                             
      RHTT=MAX(.001,RHTT*XX)                                                         
      DHT=MAX(.001,DHT*XX)                                                           
      IF(DHT/(DKHL+1.E-5)<.7)DHT=DKHL                                                
      SMM(39,MO)=SMM(39,MO)+RHTT                                                     
      VAR(39)=RHTT                                                                   
	  YERO=YSD(NDRV)+YW                                                                   
      X1=.9*WT(LD1)                                                                  
      IF(YERO>X1)THEN                                                                
          RTO=X1/YERO                                                                  
          YSD(NDRV)=YSD(NDRV)*RTO                                                      
          YW=YW*RTO                                                                    
          YERO=X1                                                                      
      END IF                                                                         
      SMM(32,MO)=SMM(32,MO)+YSD(2)                                                   
      VAR(32)=YSD(2)                                                                 
      SMM(33,MO)=SMM(33,MO)+YSD(4)                                                   
      VAR(33)=YSD(4)                                                                 
      SMM(31,MO)=SMM(31,MO)+YSD(5)                                                   
      VAR(31)=YSD(5)                                                                 
      VAR(58)=ER                                                                     
      SMM(83,MO)=SMM(83,MO)+YSD(6)                                                   
      VAR(83)=YSD(6)                                                                 
      CY=1.E5*YSD(NDRV)/(QD+1.E-5)                                                   
      CYAV=CYAV+CY                                                                   
      CYSD=CYSD+CY*CY                                                                
      IF(CY>CYMX)CYMX=CY                                                             
      SMM(30,MO)=SMM(30,MO)+YSD(3)                                                   
      VAR(30)=YSD(3)                                                                 
      VAR(29)=CVF                                                                    
	  SMM(35,MO)=SMM(35,MO)+YSD(8)                                                        
      VAR(35)=YSD(8)                                                                 
      SMM(36,MO)=SMM(36,MO)+YSD(7)                                                   
      VAR(36)=YSD(7)                                                                 
	  SMM(42,MO)=SMM(42,MO)+YW                                                            
      VAR(42)=YW                                                                     
      RFV=RFV+SAIR                                                                   
      GWST=MAX(0.,GWST+RFV-PRMT(15))                                                 
      IF(GWST>GWMX)GWST=GWMX                                                         
      RZSW=RZSW+SAIR                                                                 
      AIR=0.                                                                         
	  SAIR=0.                                                                             
      CALL HPURK                                                                     
      PKRZ(LNS)=PKRZ(LNS)+CRKF                                                       
      SMM(17,MO)=SMM(17,MO)+PKRZ(LNS)                                                
      VAR(17)=PKRZ(LNS)                                                              
      SMM(16,MO)=SMM(16,MO)+SST                                                      
      VAR(16)=SST                                                                    
      CALL HEVP                                                                      
      SMM(10,MO)=SMM(10,MO)+EO                                                       
      VAR(10)=EO                                                                     
      AET=ES                                                                         
      ADRF=(XDA*(SMM(4,MO)-SMM(14,MO))+XDA1*PMORF)/31.                               
      IF(WTMN<Z(LNS))CALL HWTBL                                                      
      IF(IRR==4)THEN
          WTMU=WTMU+FNPI                                                                 
          SMM(26,MO)=SMM(26,MO)+FNPI                                                     
          VAR(26)=FNPI                                                                   
          CALL HLGOON(JRT)                                                               
          IF(RZSW-PAW<BIR.AND.JRT==0)THEN
              FNP=MIN(WTMU,CFNP*VLGI)                                                        
              SMM(27,MO)=SMM(27,MO)+FNP                                                      
              VAR(27)=FNP                                                                    
              WTMU=WTMU-FNP                                                                  
              FNP=FNP/WSA                                                                    
              TFLG=TFLG+FNP                                                                  
              TILG=TILG+VLGI                                                                 
              CFNP=10.*WTMU*WSA/VLG                                                          
              CALL NFERT(2,JT1)                                                              
              AIR=VLGI                                                                       
              CALL HIRG(AIR,1.,0.,JRT,JT1,1)                                                 
	          SAIR=SAIR+AIR
	      END IF
	  END IF                                                                       
      NFA=NFA+1                                                                      
      NII=NII+1                                                                      
      IF(MNU>0.AND.NFA>=IFA)CALL NFERT(2,JT1)                                        
      IRGX=0                                                                         
      IF(NYD==0.AND.IDA==60)THEN
          NT1=1                                                                          
          GO TO 30
      END IF                                                                       
      NB1=KT                                                                         
      KHV=0                                                                          
      DO KT=NB1,NN1                                                               
          IF(KOMP(KT)>0)CYCLE                                                         
          XHSM=HSM/AHSM                                                                  
          IF(KTF==0)KTF=KT                                                               
          DO K=1,LC                                                                      
              IF(JH(IRO,KT)==KDC(K))EXIT
          END DO                                                                         
          IF(K>LC)K=1                                                                            
          JJK=K                                                                          
          IF(IHUS>0.AND.IY>1)GO TO 443                                                   
          IF(IDA<ITL(IRO,KT)+NT1)GO TO 25                                                
      443 IF(KG(JJK)==0.AND.JPL(JJK)==0)THEN                                             
              IF(IY==1.AND.IHC(KT)==NHC(2))GO TO 590                                                           
          ELSE                                                                           
              XHSM=HU(JJK)/PHU(JJK,IHU(JJK))                                               
          END IF                                                                                                                        
          IF(XHSM>=HUSC(IRO,KT))GO TO 590                                                     
          IF(MO<MOFX.OR.IDC(JJK)==NDC(7).OR.IDC(JJK)==NDC(8).OR.IDC&                     
          (JJK)==NDC(10))GO TO 25                                                        
	      GO TO 121                                                                           
      590 IF(PDSW/FCSW<PRMT(59))GO TO 121                                                
	      WRITE(KW(1),589)IY,MO,KDA,PDSW,FCSW                                                 
          IF(MO<MOFX)GO TO 25                                                            
      121 JT1=LT(IRO,KT)                                                                 
          KOMP(KT)=1                                                                     
          IF(KT>KTMX)KTMX=KT                                                             
          CSTX=COTL(JT1)                                                                 
          COX=COOP(JT1)                                                                  
          IF(IHC(JT1)==NHC(8))THEN
              IF(IRGX>0)THEN
                  KOMP(KT)=0                                                                     
                  CYCLE
              END IF                                                             
              AIR=VIRR(IRO,KT)                                                               
              IRY=0                                                                          
              IF(AIR>0.)IRY=1                                                                
              CALL HIRG(AIR,EFM(JT1),TLD(JT1),JRT,JT1,IRY)                                   
              IF(JRT/=0)THEN                                                                 
	              KOMP(KT)=0                                                                        
	              CYCLE                                                                          
	          ELSE                                                                                
                  IRGX=1                                                                       
                  SAIR=SAIR+AIR                                                                
                  GO TO 469                                                                    
              END IF 
          END IF                                                                        
          IF(IHC(JT1)==NHC(9))THEN
              CALL NFERT(1,JT1)                                                              
              IF(IDA/=J1.OR.NBT(JT1)>0.OR.NBE(JT1)/=J2)THEN
                  J1=IDA                                                                         
                  J2=NBE(JT1)                                                                    
                  GO TO 469
              ELSE                                                                                                   
                  CSTX=0.                                                                        
                  COX=0.                                                                         
              END IF
          END IF                                                                       
          IF(IHC(JT1)==NHC(7))THEN
              IF(QD>PRMT(58))THEN
                  WRITE(KW(1),588)IY,MO,KDA,QD
                  KOMP(KT)=0                                                                     
                  CYCLE                    
              ELSE                                                       
	              CALL PSTAPP                                                                         
	              IF(IDA==J1.AND.NBT(JT1)==0.AND.NBE(JT1)==J2)THEN
                      CSTX=0.                                                                        
                      COX=0.
                  END IF
              END IF
          END IF                                                                         
          TLA=0.
          IF(IHC(JT1)==NHC(27))THEN
              TLA=TLMA(IRO,KT)
              CALL NLIME(TLA)
              SMM(66,MO)=SMM(66,MO)+TLA                                                    
              VAR(66)=TLA                                                                    
              X3=TLA*COL                                                                 
              COST=COST+X3
              X1=COTL(JT1)                                                              
              X2=X1-COOP(JT1)                                                           
              COST=COST+X1                                                               
              CSFX=CSFX+X2                                                               
	          SMM(92,MO1)=SMM(92,MO1)+FULU(JT1)                                              
	          SMY(92)=SMY(92)+FULU(JT1)
	          IF(NOP>0)THEN 
	              WRITE(KW(1),63)IYR,MO1,KDA,TIL(JT1),TLA,CPNM(JJK),&
                  X1,X2
              END IF                                                         
              IF(KFL(20)>0)THEN                                                    
                  WRITE(KW(20),567)IYR,MO1,KDA,KDC(JJK),X3,X3,TLA                            
                  WRITE(KW(20),666)IYR,MO1,KDA,TIL(JT1),KDC(JJK),&
                  IHC(JT1),NBE(JT1),NBT(JT1),X1,X2,FULU(JT1)
              END IF                                          
          END IF                                                                                                                                                                
          IF(IHC(JT1)==NHC(20))THEN
              KOMP(NB1)=1                                                                    
	          KTF=KTF+1                                                                           
	          CYCLE
	      END IF                                                                     
          J1=IDA                                                                         
          J2=NBE(JT1)                                                                    
      469 IF(LUN/=35)CALL TLOP(CSTX,COX,JRT)                                             
          IF(JRT>0)GO TO 25
          SMM(96,MO)=SMM(96,MO)+TCEM(JT1)                                                              
          IF(IFD/=0.AND.DKHL>0.)THEN                                                     
              DHT=DKHL                                                                     
              IF(NOP>0)WRITE(KW(1),92)IYR,MO,KDA,DHT,DKIN,XHSM                             
          END IF                                                                         
          COST=COST+CSTX                                                                 
          CSFX=CSFX+COX                                                                  
          JT2=JT1
      END DO                                                                                  
      KTF=NB1                                                                        
   25 IF(KTT>0)IGZ=IGZ+1                                                             
      KT=KTF                                                                         
      BIR=TIR(IRO,KTMX)                                                              
      EFI=QIR(IRO,KTMX)                                                              
	  CFMN=CFRT(IRO,KTMX)
	  JT1=LT(IRO,KT)                                                                 
      KTF=0                                                                          
      IF(ABS(BIR)<1.E-5)GO TO 30                                                     
      IF(BIR>0.)GO TO 122                                                            
      IF(RZSW-PAW>BIR)GO TO 30                                                       
      GO TO 29                                                                       
  122 IF(BIR<1.)THEN
          IF(WS>BIR)GO TO 30                                                             
      ELSE                                                             
          CALL SWTN                                                                      
          IF(WTN<BIR)GO TO 30                                                            
      END IF
   29 IF(IRR>=3.AND.IRR/=5)THEN
          IF(IRR==4.OR.NFA<IFA)GO TO 30                                                  
          CALL NFERT(2,JT1)                                                              
          IF(JRT>0)GO TO 30
      END IF                                                              
      CALL HIRG(AIR,EFM(IAUI),TLD(IAUI),JRT,IAUI,0)                                  
      SAIR=SAIR+AIR                                                                  
   30 IF(LUN/=35)CALL NPCY
      VAR(104)=VN2O(LNS)
      SMM(104,MO)=SMM(104,MO)+VN2O(LNS)
      SMNIM=0.
      IF(IDN>2)CALL GASDF3
      XX=0.
      RSPC=0.
      DO J=1,NBSL
          ISL=LID(J)
          IF(STMP(ISL)>0.)THEN
              Z5=500.*(Z(ISL)+XX)
              SMNIM=SMNIM-WNO3(ISL)
              CALL NCNMI(Z5,CS,EAR(ISL))
              SMNIM=SMNIM+WNO3(ISL)
          END IF
          XX=Z(ISL)
      END DO                                                                 
      SMM(87,MO)=SMM(87,MO)+SMNIM
	  VAR(87)=SMNIM
      IF(IRR==1)RWO=RWO+SAIR                                                         
      YEW=MIN(ER*(YSD(NDRV)+YW)/WT(LD1),.9)	                                         
      CALL NYON                                                                      
      SMM(43,MO)=SMM(43,MO)+YN                                                       
      VAR(43)=YN                                                                     
      CALL NCQYL                                                                     
      IF(NDP>0)CALL PSTCY                                                            
      AGPM=0.                                                                        
      VAC=0.                                                                         
      CVP=0.                                                                         
      CV=RSD(LD1)+STDO                                                        
      CVRS=RSD(LD1)+STDO                                                    
      IN1=0                                                                          
      WS=1.                                                                          
	  N1=1
	  VARS(9)=STDO 
	  STV(9,MO1)=STDO                                                                               
	  DO IN2=1,IGO                                                                    
	      IN1=IN1+1
	      IF(IN1>LC)IN1=1                                                                      
          DO J=IN1,12
              IF(KG(J)>0)EXIT
          END DO
          IF(J>12)GO TO 148                                                                   
          JJK=J
          N1=MAX(1,NCP(J))                                                               
          IF(JPL(JJK)>0)THEN
              HU(JJK)=HU(JJK)+MAX(0.,DST0-TBSC(JJK))                                         
	          IF(PDSW/FCSW<PRMT(11).OR.HU(JJK)<GMHU(JJK).AND.MO<MOFX)CYCLE
              JPL(JJK)=0                                                                     
              IF(NOP>0)WRITE(KW(1),89)IYR,MO,KDA,PDSW,HU(JJK),XHSM,CPNM(JJK)                           
              HU(JJK)=0.                                                                     
              IGMD(N1,JJK)=IYR*10000+MO*100+KDA
          END IF                                              
          AGPM=AGPM+STD(JJK)                                                                  
	  	  CVRS=CVRS+STD(JJK)                                                                       
	      VAC=VAC+BWD(2,JJK)*STD(JJK)                                                         
          CV=CV+DM(JJK)-RW(JJK)+STD(JJK)                                                                           
          XX=PPL0(JJK)                                                                   
          CVP=MAX(CVP,XX/(XX+EXP(SCRP(15,1)-SCRP(15,2)*XX)))                             
          VAC=VAC+BWD(1,JJK)*STL(JJK)                                                    
          AWC=AWC+RFV-QD                                                                 
          AQV=AQV+QD                                                                     
          DO L1=1,NBSL                                                                   
              L=LID(L1)                                                                    
              U(L)=0.                                                                      
              UN(L)=0.                                                                     
              UK(L)=0.                                                                     
              UP(L)=0.                                                                     
          END DO                                                                         
          UNO3=0.                                                                        
          UPP=0.                                                                         
          UN=0.                                                                         
          SU=0.                                                                          
          DDM(JJK)=0.                                                                    
          CALL CGROW(JRT)                                                                
          IF(JRT==0)THEN
              SUN=0.                                                                         
              SUP=0.                                                                         
              SUK=0.                                                                         
              SAT=0.                                                                         
              CALL HUSE                                                                      
              CALL CROP                                                                      
              CALL NUP                                                                       
              CALL NPUP                                                                      
              CALL NUK                                                                       
              CALL NUSE                                                                      
              CALL CSTRS                                                                     
              VAR(50)=WFX                                                                    
              SMM(13,MO)=SMM(13,MO)+SU                                                       
              AET=SU+AET                                                                     
              IF(HUI(JJK)>PRMT(3))THEN                                                       
                  SWH(JJK)=SWH(JJK)+SU                                                       
                  SWP(JJK)=SWP(JJK)+EP(JJK)                                                  
              END IF                                                                         
              VAR(13)=SU                                                                     
              GSEP=GSEP+SU                                                                   
	          ACET(JJK)=ACET(JJK)+SU+ES
	      END IF                                                           
          CALL CAGRO                                                                     
          VARC(1,JJK)=HUI(JJK)                                                           
          VARC(2,JJK)=SLAI(JJK)                                                          
          VARC(3,JJK)=RD(JJK)                                                            
          VARC(4,JJK)=RW(JJK)                                                            
          VARC(5,JJK)=DM(JJK)                                                            
          VARC(6,JJK)=.42*DM(JJK)                                                            
          VARC(7,JJK)=STL(JJK)                                                           
          VARC(8,JJK)=CHT(JJK)                                                           
          VARC(9,JJK)=STD(JJK)                                                           
          VARC(10,JJK)=UN1(JJK)                                                           
          VARC(11,JJK)=UP1(JJK)                                                           
          VARC(12,JJK)=UK1(JJK)                                                          
          IF(IDC(JJK)/=NDC(7).AND.IDC(JJK)/=NDC(8).AND.IDC(JJK)/=NDC&                    
          (10))AGPM=AGPM+STL(JJK)                                                        
          IF(NSTP==IDA.AND.IY==IGSD)CALL REALC 
      END DO                                                    
      IF(KFL(12)>0)THEN
          II=IY                                                                          
          IF(NSTP>0)THEN
              II=IRLX                                                                        
              IF(IY/=IGSD)GO TO 148
          END IF                                                          
          WRITE(KW(12),513)IYR,MO,KDA,II,CPNM(JJK),(CGSF(J,JJK),J=1,7)
      END IF                   
  148 CALL SCONT(0)                                                                          
	  SWGS=SWGS+SW                                                                        
      X1=MAX(AET,EO*EXP(-PRMT(42)*SCI/SMX))                                          
!     X1=EO*EXP(-PRMT(42)*SCI/SMX)                                          
! 	  SCI=SCI+X1-RFV+QD+PKRZ(LNS)+SST                                                    
 	  SCI=SCI+X1-RFV+QD
      SCI=MIN(SCI,PRMT(73)*SMX)                                                      
      X1=(ADRF-PRMT(9))/100.                                                         
      X2=CV-PRMT(10)                                                                 
      IF(IGO>0.AND.NFA>=IFA)THEN
          IF(BFT>=1..AND.TNOR<BFT)CALL NFERT(4,JT1)
      END IF                                      
      IPST=IPST+1                                                                    
      IF(TMN>0.)THEN                                                                 
          IF(X1>0..AND.X2>0.)PSTS=PSTS+RHD*TX                                          
      ELSE                                                                           
          PSTS=PSTS+TMN                                                                
      END IF                                                                         
      IF(NSTP/=IDA.OR.IY/=IGSD)GO TO 200                                             
      IF(ICCD>0)GO TO 559                                                            
      IGSD=IGSD+1                                                                    
      IYR=IYR-1                                                                      
      ICCD=1                                                                         
      GO TO 200                                                                      
  559 CALL REALS                                                                     
      IF(ISTP==1)GO TO 88                                                            
  200 CALL NEVN                                                                      
      CALL NEVP                                                                      
      CALL SLTEV                                                                     
      VAR(47)=SNMN                                                                   
      VAR(48)=SGMN                                                                   
      VAR(49)=SDN                                                                    
	  VAR(89)=SN2
	  VAR(93)=SN2O	  
	  VAR(94)=DFO2S
	  VAR(95)=DFCO2S
	  VAR(101)=DFN2OT
      VAR(56)=SMP                                                                    
      VAR(52)=SVOL                                                                   
      VAR(51)=SNIT                                                                   
      VAR(11)=AET                                                                    
      VAR(14)=QD                                                                     
      VAR(34)=YSD(1)                                                                 
      VAR(44)=QNO3                                                                   
      VAR(106)=QNO2
      VAR(107)=QN2O
      VAR(105)=VNO2(LNS)                                                                   
      VAR(46)=VNO3(LNS)                                                              
      VAR(55)=QAP                                                                    
      VAR(45)=TSFNO3                                                                   
      VAR(71)=TSFS                                                                   
      VAR(80)=TSFK
      VAR(100)=VAR(74)-VAR(99)
      SMM(100,MO)=SMM(100,MO)+VAR(100)                                                                   
      VAR(102)=TSFNO2 
      VAR(103)=TSFN2O                                                                  
      VARS(1)=ZNH3                                                                   
      VARS(2)=ZNO3                                                                   
      VARS(3)=TAP                                                                    
      VARS(4)=TSK                                                                    
      VARS(5)=SNO                                                                    
      VARS(6)=RZSW                                                                   
      VARS(7)=WTBL                                                                   
      VARS(8)=GWST                                                                   
      VARS(11)=OCPD                                                                  
      VARS(12)=TOC                                                                   
      VARS(13)=ZLS                                                                   
      VARS(14)=ZLM                                                                   
      VARS(15)=ZLSL                                                                  
      VARS(16)=ZLSC                                                                  
      VARS(17)=ZLMC                                                                  
      VARS(18)=ZLSLC                                                                 
      VARS(19)=ZLSLNC                                                                
      VARS(20)=ZBMC                                                                  
      VARS(21)=ZHSC                                                                  
      VARS(22)=ZHPC                                                                  
      VARS(23)=ZLSN                                                                  
      VARS(24)=ZLMN                                                                  
      VARS(25)=ZBMN                                                                  
      VARS(26)=ZHSN                                                                  
      VARS(27)=ZHPN                                                                  
      VARS(28)=TWN                                                                   
      VARS(29)=TSLT
      VARS(30)=ZNO2                                                                  
      CALL SPRNT(YTP)                                                                
      DO I=1,NBSL                                                                    
          J=LID(I)                                                                     
          SMS(2,J)=SMS(2,J)+STMP(J)                                                    
      END DO                                                                         
      IF(KFL(15)>0)CALL SOCIOD(KDA)                                                  
      IF(KFL(5)>0.AND.NDP>0)THEN
          II=IY                                                                          
          IF(NSTP/=0)THEN
              II=IRLX                                                                        
              IF(IY/=IGSD)GO TO 561
          END IF                                                          
          DO L=1,NDP                                                                     
              X1=100.*(VARP(2,L)+VARP(4,L))/(QD+SST+1.E-5)                               
              WRITE(KW(5),468)IYR,MO,KDA,II,PSTN(L),(VARP(K,L),K=1,10),QD,&              
              SST,PKRZ(LNS),X1                                                           
          END DO                                                                         
      561 VARP=0.
      END IF                                                                        
      IF(KFL(10)>0)WRITE(KW(10),107)IYR,MO,KDA,DD,DST0,(STMP(LID(K)),&               
      K=1,NBSL)                                                                      
      X1=PDSW/FCSW                                                                   
	  X2=1000.*YLD(JJK)                                                                   
      !    DManowitz - 6/7/10: Always print out STL & STD for now.     	                                                            
      IF(KFL(17)>0)WRITE(KW(17),107)IYR,MO,KDA,X1,(VAR(KD(K)),K=1,NKD),&               
      ZNO3,WNO3(LD1),PKRZ(LD1),VNO3(LD1),ALB,HUI(JJK),SLAI(JJK),DM(JJK),RW(JJK),&
      STL(JJK),STD(JJK),HIX,YLDX,X2,UN1(JJK),VAR(99),VAR(100),(VARS(K),K=23,28)                                                    
  107 FORMAT(1X,I4,2X,I2,2X,I2,100(1X,F10.5))
      IF(KFL(27)>0)WRITE(KW(27),682)IYR,MO,KDA,VAR(4),VAR(10),VAR(11)&               
      ,VAR(13),VAR(14),(VAR(K),K=16,20),(VARS(K),K=6,8)                              
	  CALL SWN1530                                                                        
      IF(KFL(28)>0)WRITE(KW(28),682)IYR,MO,KDA,SW15,SW30,SNN15,SNN30,&               
      SNA15,SNA30,VAR(4),VAR(10),VAR(11),(VAR(K),K=13,20),(VARS(K),K=6,8&            
      ),(Z(LID(K)),K=1,NBSL),(ST(LID(K)),K=1,NBSL),(U(LID(K)),K=1,NBSL),&            
      (SEV(LID(K)),K=1,NBSL),(PKRZ(LID(K)),K=1,NBSL),(SSF(LID(K)),K=1,&              
      NBSL),(WNO3(LID(K)),K=1,NBSL),(UN(LID(K)),K=1,NBSL),(VNO3(LID(K)),&            
      K=1,NBSL),(STMP(LID(K)),K=1,NBSL)                                              
      TNGS=ZNO2+ZNO3+TWN+ZNH3                                                        
      SMGS(2)=SMGS(2)+TNGS                                                                
      IF(KFL(21)>0)THEN                                                              
          WRITE(KW(21),107)IYR,MO,KDA                                                
	      CALL SPRNT(YTP)                                                                 
          CALL SOLIO(YTP,21)                                                         
      END IF                                                                         
      IF(IPD<6)GO TO 44                                                              
      IF(IPD==9)THEN
          IF(IGO==0)GO TO 44                                                             
      END IF
      IF(IDA/=IPC)GO TO 44                                                           
      IF(IPD==8)THEN
          IF(RFV<1.)GO TO 44
      END IF                                                             
      IF(IPD/=7)THEN                                                             
    !     PRINTOUT DAILY                                                                 
          WRITE(KW(1),532)IYR,MO,KDA,(HED(KD(K)),VAR(KD(K)),K=1,NKD)                     
          DO I=1,NN                                                                      
              K1=LY(IRO,I)                                                               
              IF(KG(K1)==0)CYCLE                                                         
              WRITE(KW(1),105)IYR,MO,KDA,CPNM(K1),(HEDC(K),VARC(K,K1),K=1,19)                                                                        
          END DO                                                                         
          WRITE(KW(1),532)IYR,MO,KDA,(HEDS(K),VARS(K),K=1,13)                            
    !     WRITE(KW(1),119)(LID(K),K=1,NBSL)                                              
    !     WRITE(KW(1),100)(Z(LID(K)),K=1,NBSL)                                           
    !     WRITE(KW(1),100)(ST(LID(K)),K=1,NBSL)                                          
    !     WRITE(KW(1),100)(S15(LID(K)),K=1,NBSL)                                         
    !     WRITE(KW(1),100)(FC(LID(K)),K=1,NBSL)                                          
    !     WRITE(KW(1),100)(PO(LID(K)),K=1,NBSL)                                          
    !     WRITE(KW(1),100)(BD(LID(K)),K=1,NBSL)                                          
    !     WRITE(KW(1),100)(WT(LID(K)),K=1,NBSL)                                          
    !     WRITE(KW(1),100)(SAN(LID(K)),K=1,NBSL)                                         
    !     WRITE(KW(1),100)(SIL(LID(K)),K=1,NBSL)                                         
    !     WRITE(KW(1),100)(ROK(LID(K)),K=1,NBSL)                                         
    !     WRITE(KW(1),219)(HK(LID(K)),K=1,NBSL)                                          
    !     WRITE(KW(1),100)(WNO3(LID(K)),K=1,NBSL)                                        
    !     WRITE(KW(1),100)(WNH3(LID(K)),K=1,NBSL)                                        
    !     WRITE(KW(1),100)(AP(LID(K)),K=1,NBSL)                                          
    !     WRITE(KW(1),100)(PMN(LID(K)),K=1,NBSL)                                         
    !     WRITE(KW(1),100)(OP(LID(K)),K=1,NBSL)                                          
    !     WRITE(KW(1),100)(RWT(LID(K),JJK),K=1,NBSL)                                     
    !     WRITE(KW(1),100)(VNO3(LID(K)),K=1,NBSL)                                        
    !     WRITE(KW(1),100)(WON(LID(K)),K=1,NBSL)                                         
    !     WRITE(KW(1),100)(U(LID(K)),K=1,NBSL)                                           
    !     WRITE(KW(1),100)(SEV(LID(K)),K=1,NBSL)                                         
    !     WRITE(KW(1),100)(PKRZ(LID(K)),K=1,NBSL)                                        
    !     WRITE(KW(1),100)(SSF(LID(K)),K=1,NBSL)                                         
    !     WRITE(KW(1),100)(UN(LID(K)),K=1,NBSL)                                          
    !     WRITE(KW(1),100)(UP(LID(K)),K=1,NBSL)                                          
    !     WRITE(KW(1),100)(WP(LID(K)),K=1,NBSL)                                          
    !     WRITE(KW(1),100)(FOP(LID(K)),K=1,NBSL)                                         
    !     WRITE(KW(1),100)(WSLT(LID(K)),K=1,NBSL)                                        
    !     WRITE(KW(1),100)(RSD(LID(K)),K=1,NBSL)                                         
    !     WRITE(KW(1),107)IYR,MO,KDA,DD,DST0,(STMP(LID(K)),K=1,NBSL)                     
      ELSE  
          CALL SPRNT(YTP)                                                                
          WRITE(KW(1),107)IYR,MO,KDA                                                     
          WRITE(KW(1),101)                                                               
          CALL SOLIO(YTP,1)                                                              
      END IF
      IPC=IPC+INP                                                                    
   44 SMM(11,MO)=SMM(11,MO)+AET                                                      
      SMM(47,MO)=SMM(47,MO)+SNMN                                                     
      SMM(48,MO)=SMM(48,MO)+SGMN                                                     
      SMM(49,MO)=SMM(49,MO)+SDN                                                      
	  SMM(89,MO)=SMM(89,MO)+SN2                                                           
      SMM(56,MO)=SMM(56,MO)+SMP                                                      
      SMM(52,MO)=SMM(52,MO)+SVOL                                                     
      SMM(51,MO)=SMM(51,MO)+SNIT                                                     
      SMM(14,MO)=SMM(14,MO)+QD                                                       
      SMM(34,MO)=SMM(34,MO)+YSD(1)                                                   
      SMM(44,MO)=SMM(44,MO)+QNO3                                                     
      SMM(106,MO)=SMM(106,MO)+QNO2
      SMM(107,MO)=SMM(107,MO)+QN2O
      SMM(105,MO)=SMM(105,MO)+VNO2(LNS)                                                     
      SMM(46,MO)=SMM(46,MO)+VNO3(LNS)                                                
      SMM(55,MO)=SMM(55,MO)+QAP                                                      
      SMM(45,MO)=SMM(45,MO)+TSFNO3                                                     
      SMM(80,MO)=SMM(80,MO)+TSFK                                                     
      SMM(71,MO)=SMM(71,MO)+TSFS
      SMM(93,MO)=SMM(93,MO)+SN2O
      SMM(94,MO)=SMM(94,MO)+DFO2S     !modified 8/25/11 (wbm & rci)
      SMM(95,MO)=SMM(95,MO)+DFCO2S    !modified 8/25/11 (wbm & rci)
      SMM(101,MO)=SMM(101,MO)+DFN2OT  !added 8/25/11 (wbm & rci) & modified 10/2/11
      SMM(102,MO)=SMM(102,MO)+TSFNO2
      SMM(103,MO)=SMM(103,MO)+TSFN2O
      CALL NCONT
      IF(IDA==NSTP+NT1.AND.NGN==0)THEN
          IGN=IGN+100                                                                    
          DO KK=1,IGN                                                                    
              DO J=1,20                                                                    
                  XX=AUNIF(21)                                                             
                  IX(J)=IX(21)                                                             
              END DO                                                                       
          END DO                                                                         
      END IF          
      JRT=0                                                                          
      IF(YERO>1.E-5.AND.ISTA==0.AND.Z(LNS)>ZF.AND.NBSL>=3)CALL ESLOS(JRT)                                      
      IF(JRT>0)GO TO 88                                                              
      JDA=IDA+1                                                                      
      CALL AXMON(JDA,MO)                                                             
      IF(MO==MO1)GO TO 84                                                            
      PMOEO=SMM(10,MO1)                                                              
      PMORF=SMM(4,MO1)-SMM(14,MO1)                                                   
      XX=IDA-JJ                                                                      
      STV(1,MO1)=ZNH3                                                                
      STV(2,MO1)=ZNO3                                                                
      STV(4,MO1)=TSK                                                                 
      STV(5,MO1)=SNO                                                                 
      STV(6,MO1)=RZSW                                                                
      STV(7,MO1)=WTBL                                                                
      STV(8,MO1)=GWST                                                                
      STV(11,MO1)=OCPD                                                               
      STV(12,MO1)=.001*TOC                                                           
      STV(13,MO1)=.001*ZLS                                                           
      STV(14,MO1)=.001*ZLM                                                           
      STV(15,MO1)=.001*ZLSL                                                          
      STV(16,MO1)=.001*ZLSC                                                          
      STV(17,MO1)=.001*ZLMC                                                          
      STV(18,MO1)=.001*ZLSLC                                                         
      STV(19,MO1)=.001*ZLSLNC                                                        
      STV(20,MO1)=.001*ZBMC                                                          
      STV(21,MO1)=.001*ZHSC                                                          
      STV(22,MO1)=.001*ZHPC                                                          
      STV(23,MO1)=.001*ZLSN                                                          
      STV(24,MO1)=.001*ZLMN                                                          
      STV(25,MO1)=.001*ZBMN                                                          
      STV(26,MO1)=.001*ZHSN                                                          
      STV(27,MO1)=.001*ZHPN                                                          
      STV(28,MO1)=.001*TWN                                                           
      STV(29,MO1)=TSLT
      STV(30,MO1)=ZNO2                                                               
	  IF(KFL(6)==0)GO TO 472                                                              
      I1=IY                                                                          
      IF(NSTP/=0)THEN
          I1=IRLX                                                                        
          IF(IY/=IGSD)GO TO 472
      END IF                                                          
      WRITE(KW(6),475)IYR,MO1,I1,(SMM(IFS(J),MO1),J=1,NFS),(STV(K,MO1),&             
      K=4,6)                                                                         
  472 DO 118 K=1,NN                                                                  
      K1=LY(IRO,K)                                                                   
      IF(KG(K1)==0)GO TO 50                                                          
      SMMC(1,K1,MO1)=HUI(K1)                                                         
      SMMC(2,K1,MO1)=SLAI(K1)                                                        
      SMMC(3,K1,MO1)=RD(K1)                                                          
      SMMC(4,K1,MO1)=RW(K1)                                                          
      SMMC(5,K1,MO1)=DM(K1)                                                          
      SMMC(6,K1,MO1)=.42*DM(K1)                                                          
      SMMC(7,K1,MO1)=STL(K1)                                                         
      SMMC(8,K1,MO1)=CHT(K1)                                                         
      SMMC(10,K1,MO1)=UN1(K1)                                                         
      SMMC(11,K1,MO1)=UP1(K1)                                                         
      SMMC(12,K1,MO1)=UK1(K1)                                                        
      TSTL(MO1)=TSTL(MO1)+STL(K1)                                                    
      IF(KFL(11)==0)GO TO 50                                                         
      II=IY                                                                          
      IF(NSTP/=0)THEN
          II=IRLX                                                                        
          IF(IY/=IGSD)GO TO 50
      END IF                                                           
      WRITE(KW(11),523)IYR,MO1,II,CPNM(K1),(SFMO(J,K1),J=1,7),RZSW,SMM(4&            
      ,MO1),SMM(11,MO1),SMM(14,MO1),SMM(17,MO1),SMM(16,MO1)                          
   50 IF(KFL(22)>0)THEN                                                              
          RNO3=SMM(4,MO1)*RFNC                                                       
          SUM=ZNO2+ZNO3+ZNH3+TWN+STDN(K1)+STDON+UN1(K1)                                       
	      WRITE(KW(22),565)IYR,MO1,SMM(4,MO1),SMM(10,MO1),SMM(11,MO1),&                   
          SMM(13,MO1),SMM(14,MO1),(SMM(J,MO1),J=16,20),(STV(J,MO1),&                 
          J=6,8),RNO3,(SMM(J,MO1),J=43,46),SMM(49,MO1),SMM(89,MO1),SMM&              
          (52,MO1),SMM(85,MO1),SMM(50,MO1),(SMM(J,MO1),J=59,61),UN1(K1),&            
          YLNF(1,K1),CPNM(K1),YLD1(1,K1),SUM                                         
      END IF                                                                         
      SMMC(9,K1,MO1)=STD(K1)                                                         
      ISM=0                                                                          
      DO J=1,7                                                                       
	      KTP(J)=MIN(31.,SFMO(J,K1)+.5)                                                   
          ISM=ISM+KTP(J)                                                             
          SFMO(J,K1)=0.                                                              
      END DO                                                                         
      IF(ISM==0)GO TO 118                                                            
      CALL ASORTI(KTP,MNST,7)                                                        
      KDT(MO1,K1)=KTP(MNST(5))+100*MNST(5)+1000*KTP(MNST(6))+100000*&                 
      MNST(6)+1000000*KTP(MNST(7))+100000000*MNST(7)                                 
  118 CONTINUE                                                                       
      IF(KFL(25)>0)THEN                                                              
          IF(IY==1.AND.IDA==1)WRITE(KW(25),680)WSA                                   
          YTP(1)=SMM(14,MO1)+SMM(16,MO1)+SMM(18,MO1)                                 
          YTP(2)=SMM(NDVSS,MO1)                                                      
          YTP(3)=SMM(43,MO1)                                                         
          YTP(4)=SMM(54,MO1)                                                         
          YTP(5)=SMM(44,MO1)+SMM(45,MO1)+SMM(53,MO1)                                 
          YTP(6)=SMM(55,MO1)                                                         
          WRITE(KW(25),679)IY,MO1,YTP                                                
      END IF
      STV(10,MO1)=RSD(LD1)                                                                        
      VARS(10)=RSD(LD1)                                                                        
      STV(3,MO1)=1000.*AP(LD1)/WT(LD1)                                               
      TXMX(MO1)=TXMX(MO1)+SMM(1,MO1)                                                 
      TXMN(MO1)=TXMN(MO1)+SMM(2,MO1)                                                 
      TSR(MO1)=TSR(MO1)+SMM(3,MO1)                                                   
      SMM(2,MO1)=SMM(2,MO1)/XX                                                       
      SMM(1,MO1)=SMM(1,MO1)/XX                                                       
      SMM(3,MO1)=SMM(3,MO1)/XX                                                       
      SMM(67,MO1)=SMM(67,MO1)/XX                                                     
	  SMM(68,MO1)=SMM(68,MO1)/XX                                                          
      SMM(7,MO1)=SMM(7,MO1)/XX                                                       
	  TET(MO1)=TET(MO1)+SMM(7,MO1)                                                        
      SMM(8,MO1)=SMM(8,MO1)/XX                                                       
      SMM(9,MO1)=SMM(9,MO1)/XX                                                       
      SMM(39,MO1)=SMM(39,MO1)/XX                                                     
      SMM(40,MO1)=SMM(40,MO1)/XX                                                     
      X1=NWDA-NWD0+1.E-5                                                             
      TAMX(MO1)=TAMX(MO1)+X1                                                         
      SMM(38,MO1)=SMM(38,MO1)/X1                                                     
      SMM(41,MO1)=SMM(41,MO1)/X1                                                     
      NWD0=NWDA                                                                      
      SSW=SSW/XX                                                                     
      ASW(MO1)=ASW(MO1)+SSW                                                          
      SSW=0.                                                                         
      TR(MO1)=TR(MO1)+SMM(4,MO1)                                                     
      TSN(MO1)=TSN(MO1)+SMM(17,MO1)                                                  
      TSY(MO1)=TSY(MO1)+SMM(NDVSS,MO1)                                               
      RSY(MO1)=RSY(MO1)+SMM(36,MO1)                                                  
      TYW(MO1)=TYW(MO1)+SMM(42,MO1)                                                  
      TQ(MO1)=TQ(MO1)+SMM(14,MO1)                                                    
      SET(MO1)=SET(MO1)+SMM(10,MO1)                                                  
      TRHT(MO1)=TRHT(MO1)+SMM(39,MO1)                                                
      JJ=IDA                                                                         
      DO K=1,NSM                                                                     
          SMY(K)=SMY(K)+SMM(K,MO1)                                                     
      END DO                                                                         
      IF(NDP>0)THEN                                                                  
          DO K=1,NDP                                                                   
              SMMP(8,K,MO1)=PFOL(K)                                                    
              DO K1=1,7                                                                
                  SMYP(K1,K)=SMYP(K1,K)+SMMP(K1,K,MO1)                                 
              END DO                                                                   
              SMYP(10,K)=SMYP(10,K)+SMMP(10,K,MO1)                                     
          END DO                                                                       
      END IF                                                                         
      W(MO1)=W(MO1)+SMM(29,MO1)                                                      
      RCM(MO1)=RCM(MO1)+SMM(37,MO1)                                                  
      TEI(MO1)=TEI(MO1)+SMM(28,MO1)                                                  
	  X1=SMM(28,MO1)+1.E-10                                                               
      SMM(29,MO1)=SMM(29,MO1)/X1                                                     
      SMM(37,MO1)=SMM(37,MO1)/X1                                                     
      SMM(83,MO1)=SMM(83,MO1)/X1                                                     
	  SMM(90,MO1)=SMM(90,MO1)/X1                                                          
	  SMM(91,MO1)=SMM(91,MO1)/X1                                                          
      X1=JCN-JCN0                                                                    
	  IF(X1>0.)SMM(15,MO1)=SMM(15,MO1)/X1                                                 
      X1=NQP-NQP0                                                                    
	  IF(X1>0.)SMM(58,MO1)=SMM(58,MO1)/X1                                                 
      NQP0=NQP                                                                       
      JCN0=JCN                                                                       
      IF(MASP>0)THEN
          PPX(MO1)=SMM(44,MO1)                                                           
          CALL ACOUT(PPX(MO1),SMM(14,MO1),1000.)                                         
          SMM(44,MO1)=PPX(MO1)                                                           
          PPX(MO1)=SMM(45,MO1)                                                           
          CALL ACOUT(PPX(MO1),SMM(16,MO1),1000.)                                         
          SMM(45,MO1)=PPX(MO1)                                                           
          PPX(MO1)=SMM(46,MO1)                                                           
          CALL ACOUT(PPX(MO1),SMM(17,MO1),1000.)                                         
          SMM(46,MO1)=PPX(MO1)                                                           
          PPX(MO1)=SMM(55,MO1)                                                           
          CALL ACOUT(PPX(MO1),SMM(14,MO1),1000.)                                         
          SMM(55,MO1)=PPX(MO1)                 
      END IF                                                    
!     WRITE MO VALUES AND SUM YEARLY VALUES                                          
      IF(MO>MO1)GO TO 83                                                             
      IF(LMS==0.AND.TLA<1.E-5)THEN
          CALL NLIME(0.)                                                                     
          IF(TLA>0.)THEN                                                                 
              X3=TLA*COL                                                                 
              COST=COST+X3                                                               
              X1=COTL(IAUL)                                                              
              X2=X1-COOP(IAUL)                                                           
              COST=COST+X1                                                               
              CSFX=CSFX+X2                                                               
	          SMM(92,MO1)=SMM(92,MO1)+FULU(IAUL)                                              
	          SMY(92)=SMY(92)+FULU(IAUL)                                                      
	          IF(NOP>0)THEN 
	              WRITE(KW(1),63)IYR,MO1,KDA,TIL(IAUL),TLA,CPNM(JJK),&
                  X1,X2
              END IF                                                         
              IF(KFL(20)>0)THEN                                                    
                  WRITE(KW(20),567)IYR,MO1,KDA,KDC(JJK),X3,X3,TLA                            
                  WRITE(KW(20),666)IYR,MO1,KDA,TIL(IAUL),KDC(JJK),&
                  IHC(IAUL),NBE(IAUL),NBT(IAUL),X1,X2,FULU(IAUL)
              END IF                                          
          END IF
      END IF                                                                         
      SMM(66,MO1)=SMM(66,MO1)+TLA                                                    
      SMY(66)=SMY(66)+TLA                                                    
      VAR(66)=TLA                                                                    
      SMY(1)=SMY(1)/12.                                                              
      SMY(2)=SMY(2)/12.                                                              
      SMY(3)=SMY(3)/12.                                                              
      SMY(7)=SMY(7)/12.                                                              
      SMY(8)=SMY(8)/12.                                                              
      SMY(9)=SMY(9)/12.                                                              
      SMY(67)=SMY(67)/12.                                                            
	  SMY(68)=SMY(68)/12.                                                                 
      SMY(39)=SMY(39)/12.                                                            
      SMY(40)=SMY(40)/12.                                                            
      SMY(41)=SMY(41)/12.                                                            
      DO K=1,NSM                                                                     
          SM(K)=SM(K)+SMY(K)                                                           
      END DO                                                                         
      IF(NDP>0)THEN                                                                  
          DO K=1,NDP                                                                   
	          DO L=1,7                                                                      
                  SMAP(L,K)=SMAP(L,K)+SMYP(L,K)                                        
              END DO                                                                   
              SMAP(10,K)=SMAP(10,K)+SMYP(10,K)                                         
          END DO                                                                       
      END IF                                                                         
      DMX=MIN(PRMT(24),Z(LNS))                                                 
      IF(DMX>Z(LD1).AND.LUN/=35)CALL TMIX(PRMT(25),DMX,1)                            
	  X1=SMY(28)+1.E-10                                                                   
      SMY(29)=SMY(29)/X1                                                             
      SMY(37)=SMY(37)/X1                                                             
	  SMY(90)=SMY(90)/X1                                                                  
	  SMY(91)=SMY(91)/X1                                                                  
      IF(MASP>0)THEN
          PPX(1)=SMY(44)                                                                 
          PPX(2)=SMY(45)                                                                 
          PPX(3)=SMY(46)                                                                 
          PPX(4)=SMY(49)                                                                 
          CALL ACOUT(PPX(1),SMY(14),1000.)                                               
          CALL ACOUT(PPX(2),SMY(16),1000.)                                               
          CALL ACOUT(PPX(3),SMY(17),1000.)                                               
          CALL ACOUT(PPX(4),SMY(14),1000.)                                               
          SMY(44)=PPX(1)                                                                 
          SMY(45)=PPX(2)                                                                 
          SMY(46)=PPX(3)                                                                 
          SMY(49)=PPX(4)                                                                 
      END IF          
      X1=JCN-JCN1                                                                    
      SMY(15)=SMY(15)/(X1+1.E-20)                                                    
      JCN1=JCN                                                                       
      X1=NQP-NQP1                                                                    
      SMY(58)=SMY(58)/(X1+1.E-20)                                                    
      NQP1=NQP                                                                       
      IF(KFL(7)>0)THEN                                                               
          DO K=1,NDP                                                                   
              WRITE(KW(7),462)PSTN(K)                                                  
              DO L=1,5                                                                 
                  WRITE(KW(7),143)HEDP(L),(SMMP(L,K,J),J=1,12),SMYP(L,K),&             
                  HEDP(L)                                                              
              END DO                                                                   
          END DO                                                                       
      END IF                                                                         
      IF(KFL(26)>0.AND.NDP>0)THEN
          DO J=1,NN
              JJX=LY(IRO,J)                                                                    
              IF(TPAC(JJX,1)>0.)EXIT                                                     
          END DO
          IF(J>NN)THEN
              XPR=0.
              ANMX=' '
          ELSE
              XPR=TPAC(JJX,1)
              ANMX=CPNM(JJX)
          END IF
          WRITE(KW(26),681)IYR,IY,SMY(14),SMY(16),SMY(17),SMY(18),SMY(NDVSS),&
          SMY(77),PSTN(1),ANMX,XPR,(SMYP(J,1),J=2,7),SMYP(10,1),APQC(2,1,IY)
          DO K=2,NDP
              DO J=1,NN
                  JJX=LY(IRO,J)                                                                    
                  IF(TPAC(JJX,K)>0.)EXIT                                                     
              END DO
              IF(J>NN)THEN
                  XPR=0.
                  ANMX=' '
              ELSE
                  XPR=TPAC(JJX,K)
                  ANMX=CPNM(JJX)
              END IF
              WRITE(KW(26),683)PSTN(K),ANMX,XPR,(SMYP(J,K),J=2,7),&
              SMYP(10,K),APQC(2,K,IY)
          END DO
      END IF                                                              
      II=0                                                                           
      IF(IY==IPY)THEN
          II=IPYI                                                                        
          IF(IPD<=2)THEN
              WRITE(KW(1),96)IYR,(HED(KYA(K)),SMY(KYA(K)),K=1,NKYA)                          
          ELSE
	          IF(NDP/=0.AND.MASP>=0)THEN
                  DO K=1,NDP                                                                  
                      IF(K==6.OR.K==1)THEN                                                           
                          CALL APAGE(1)                                                              
                          WRITE(KW(1),112)                                                           
                          WRITE(KW(1),99)IYR,IY                                                      
              END IF                                                                         
              WRITE(KW(1),111)PSTN(K)                                                        
              IF(MASP>0)THEN
                  WRITE(KW(1),113)HEDP(1),(SMMP(1,K,J),J=1,12),&
                        SMYP(1,K),HEDP(1)                 
                  DO L=1,12                                                                      
                      PPX(L)=SMMP(2,K,L)                                                         
                      CALL ACOUT(PPX(L),SMM(14,L),1.)                                            
                  END DO                                                                         
                  PPX(13)=SMYP(2,K)                                                              
                  CALL ACOUT(PPX(13),SMY(14),1.)                                                 
                  WRITE(KW(1),109)HEDP(2),(PPX(J),J=1,13),HEDP(2)                                
                  DO L=1,12                                                                      
                      PPX(L)=SMMP(3,K,L)                                                         
                      CALL ACOUT(PPX(L),SMM(17,L),1.)                                            
                  END DO                                                                         
                  PPX(13)=SMYP(3,K)                                                              
                  CALL ACOUT(PPX(13),SMY(17),1.)                                                 
                  WRITE(KW(1),109)HEDP(3),(PPX(J),J=1,13),HEDP(3)                                
                  DO L=1,12                                                                      
                      PPX(L)=SMMP(4,K,L)                                                         
                      CALL ACOUT(PPX(L),SMM(16,L),1.)                                            
                  END DO                                                                         
                  PPX(13)=SMYP(4,K)                                                              
                  CALL ACOUT(PPX(13),SMY(16),1.)                                                 
                  WRITE(KW(1),109)HEDP(4),(PPX(J),J=1,13),HEDP(4)                                
                  DO L=5,7                                                                       
                      WRITE(KW(1),113)HEDP(L),(SMMP(L,K,J),J=1,12),&
                            SMYP(L,K),HEDP(L)               
                  END DO                                                                         
                  DO L=8,9                                                                       
                      WRITE(KW(1),114)HEDP(L),(SMMP(L,K,J),J=1,12),&
                            HEDP(L)                         
                  END DO                                                                         
                  DO L=1,12                                                                      
                      PPX(L)=SMMP(10,K,L)                                                        
                      CALL ACOUT(PPX(L),SMM(18,L),1.)                                            
                  END DO                                                                         
                  PPX(13)=SMYP(10,K)                                                             
                  CALL ACOUT(PPX(13),SMY(18),1.)                                                 
                  WRITE(KW(1),109)HEDP(10),(PPX(J),J=1,13),HEDP(10)                              
                  CYCLE
              END IF                                                                           
              DO L=1,7                                                                       
    !             PRINTOUT PESTICIDE MONTHLY                                                 
                  WRITE(KW(1),143)HEDP(L),(SMMP(L,K,J),J=1,12),&
                        SMYP(L,K),HEDP(L)             
              END DO                                                                         
              DO L=8,9                                                                       
                  WRITE(KW(1),146)HEDP(L),(SMMP(L,K,J),J=1,12),HEDP(L)                         
              END DO                                                                         
              WRITE(KW(1),143)HEDP(10),(SMMP(10,K,J),J=1,12),&
                    SMYP(10,K),HEDP(10)
          END DO                       
          DO K=1,NDP                                                                     
              WRITE(KW(1),462)PSTN(K)                                                    
              WRITE(KW(1),463)(APQ(I,K,IY),I=1,5)                                        
              WRITE(KW(1),464)(AQB(I,K,IY),I=1,5)                                        
              WRITE(KW(1),465)(APY(I,K,IY),I=1,5)                                        
              WRITE(KW(1),466)(AYB(I,K,IY),I=1,5)                                        
          END DO                                                                         
      END IF                                                                            
      CALL APAGE(1)                                                                  
      WRITE(KW(1),102)IYR,IY                                                         
      IF(NKA>0)THEN                                                                  
          DO J=1,NKA                                                                   
              K=KA(J)                                                                  
              ! PRINTOUT MONTHLY                                                         
              WRITE(KW(1),104)HED(K),(SMM(K,L),L=1,12),SMY(K),HED(K)                   
          END DO                                                                       
          WRITE(KW(1),108)'WIDX',(IWIX(L),L=1,12),'WIDX'                               
      END IF                                                                         
      IF(NJC>0)THEN                                                                  
          DO J=1,NJC                                                                   
              K=JC(J)                                                                  
              WRITE(KW(1),109)HED(K),(SMM(K,L),L=1,12),SMY(K),HED(K)                   
          END DO                                                                       
      END IF                                                                         
      IF(NKS>0)THEN
          DO J=1,NKS                                                                     
              K=KS(J)                                                                      
              WRITE(KW(1),95)HEDS(K),(STV(K,L),L=1,12),HEDS(K)                             
          END DO                                                                         
              END IF                                                                            
          END IF
      END IF
      K=1                                                                            
      N2=NN                                                                          
	  FGIS=0.                                                                             
	  YGIS=0.                                                                             
	  BGIS=0.                                                                             
	  WGIS=0.                                                                             
	  YLNG=0.                                                                             
	  YLPG=0.                                                                             
	  YLKG=0.                                                                             
	  HGIS=0.                                                                             
	  WSGS=0.                                                                             
	  SNGS=0.                                                                             
	  SPGS=0.                                                                             
	  STGS=0.                                                                             
	  SAGS=0.                                                                             
	  SSGS=0.
	  DO WHILE(K<=N2)
          IF(K==1.OR.K==6)THEN                                                           
          CALL APAGE(1)                                                                
          WRITE(KW(1),102)IYR,IY                                                       
      END IF                                                                         
      J=LY(IRO,K)
      IYH(J)=IYH(J)+1                                                                
      N1=MAX(1,NCP(J))                                                               
      DO L=1,20                                                                      
          ! PRINTOUT CROP MONTHLY                                                        
          WRITE(KW(1),95)HEDC(L),(SMMC(L,J,K1),K1=1,12),HEDC(L)                        
          DO K1=1,12                                                                   
              SMMC(L,J,K1)=0.                                                          
          END DO                                                                       
      END DO                                                                         
      DO K1=1,N1                                                                     
          DO L=1,7                                                                     
              TSFC(L,J)=TSFC(L,J)+SF(L,K1,J)                                           
          END DO                                                                       
      END DO                                                                         
      WRITE(KW(1),108)'STRS',(KDT(L,J),L=1,12),'STRS'                                
	  DO J1=N1,1,-1                                                                       
          IF(CAW(J1,J)<1.E-10)THEN                                                     
              CAW(J1,J)=AWC                                                            
              AWC=0.                                                                   
              CRF(J1,J)=ARF                                                            
              ARF=0.                                                                   
              CQV(J1,J)=AQV                                                            
              AQV=0.                                                                   
              JP(J)=0                                                                  
          END IF                                                                       
          TCAW(J)=TCAW(J)+CAW(J1,J)                                                    
          TCQV(J)=TCQV(J)+CQV(J1,J)                                                    
          TCRF(J)=TCRF(J)+CRF(J1,J)                                                    
      END DO
      PMTE=0.                                                                         
      DO J1=1,N1                                                                 
          IF(ETG(J1,J)>0.)THEN                                                           
              ETGS=ETG(J1,J)                                                             
              SMGS(3)=SMGS(3)+ETGS                                                       
          ELSE                                                                           
              ETG(J1,J)=ACET(J)                                                          
	      END IF                                                                              
          PMTE = PMTE + DM(J1) + STD(J1)
          XTP(1,J1,J)=YLD1(J1,J)*PRYG(J)                                                 
          XTP(2,J1,J)=YLD2(J1,J)*PRYF(J)
          XTP(4,J1,J)=WCYD
          VALF1=XTP(1,J1,J)+VALF1+XTP(2,J1,J)                                            
	      IF(ETG(J1,J)>.1)THEN                                                                
	          XTP(3,J1,J)=1000.*(YLD1(J1,J)+YLD2(J1,J))/ETG(J1,J)                               
	      ELSE                                                                                
	          XTP(3,J1,J)=0.                                                                    
	      END IF                                                                              
          XX=NPSF(J1,J)                                                                  
          TPSF(J1,J)=TPSF(J1,J)/(XX+1.E-10)                                              
	      YGIS=MAX(YGIS,YLD1(J1,J))                                                           
	      SMGS(1)=SMGS(1)+YGIS                                                                
	      BGIS=MAX(BGIS,DMF(J1,J))                                                            
	      WGIS=MAX(WGIS,XTP(3,J1,J))                                                          
	      FGIS=MAX(FGIS,FRTN(J1,J))                                                           
	      SMGS(5)=SMGS(5)+FGIS                                                                
	      YLNG=MAX(YLNG,YLNF(J1,J))                                                           
	      YLPG=MAX(YLPG,YLPF(J1,J))                                                           
	      YLKG=MAX(YLKG,YLKF(J1,J))                                                           
	      HGIS=MAX(HGIS,HIF(J1,J))                                                            
	      SMGS(7)=SMGS(7)+HGIS                                                                
	      WSGS=MAX(WSGS,SF(1,J1,J))                                                           
	      SNGS=MAX(SNGS,SF(2,J1,J))                                                           
	      SPGS=MAX(SPGS,SF(3,J1,J))                                                           
	      STGS=MAX(STGS,SF(5,J1,J))                                                           
	      SAGS=MAX(SAGS,SF(6,J1,J))                                                           
	      SSGS=MAX(SSGS,SF(7,J1,J))                                                           
          IF(IY==IPY)THEN                                                                
              IF(CSTF(J1,J)<=0.)THEN                                                       
                  CSTF(J1,J)=COST                                                          
                  CSOF(J1,J)=COST-CSFX                                                     
                  COST=0.                                                                  
                  CSFX=0.                                                                  
              END IF                                                                       
              !         PRINTOUT CROP ANNUAL 
              X1=1000.*YLCF(J1,J)
              IF(IDC(J)==NDC(7).OR.IDC(J)==NDC(8).OR.IDC(J)==NDC(10))THEN
                  X2=.0001*PPL0(J)
              ELSE
                  X2=PPL0(J)
              END IF
              WRITE(KW(1),106)CPNM(J),YLD1(J1,J),YLD2(J1,J),DMF(J1,J),&
              YLNF(J1,J),YLPF(J1,J),YLKF(J1,J),X1,FRTN(J1,J),FRTP(J1,J),&
              FRTK(J1,J),VIR(J1,J),VIL(J1,J),CAW(J1,J),ETG(J1,J),XTP&
              (3,J1,J),X2,TPSF(J1,J),CSTF(J1,J),CSOF(J1,J),XTP(1,J1,J),&
              XTP(2,J1,J),EK,REK,WK
              WRITE(KW(1),97)(SF(L,J1,J),L=1,7)                                            
              TSMQ=0.                                                                      
              TSMY=0.                                                                      
          END IF                                                                         
          TDM(J)=TDM(J)+DMF(J1,J)                                                        
          TYL1(J)=TYL1(J)+YLD1(J1,J)                                                     
          TYL2(J)=TYL2(J)+YLD2(J1,J)                                                     
          TYLN(J)=TYLN(J)+YLNF(J1,J)                                                     
          TYLP(J)=TYLP(J)+YLPF(J1,J)                                                     
          TYLK(J)=TYLK(J)+YLKF(J1,J)
          TYLC(J)=TYLC(J)+YLCF(J1,J)            
          IF(YLNF(J1,J)>0.)THEN                                                          
              NYLN(J)=NYLN(J)+1                                                          
              XX=NYLN(J)                                                                 
              X1=XX+1.                                                                   
              X2=MAX(1.,BLYN(J),1.1*SF(2,J1,J))                                          
              UNA(J)=MIN(1000.,UNA(J)*(PRMT(28)/(1.-SF(2,J1,J)/X2))**2)                  
	          X3=UNA(J)                                                                       
	          ULYN(J)=(ULYN(J)*XX+YLNF(J1,J))/X1                                              
	          UNA(J)=PRMT(46)*UNA(J)+(1.-PRMT(46))*ULYN(J)                                    
	      END IF                                                                              
          TRD(J)=TRD(J)+RDF(J)                                                           
          THU(J)=THU(J)+HUF(J)                                                           
          TETG(J)=TETG(J)+ETG(J1,J)                                                      
          TVIR(J)=TVIR(J)+VIR(J1,J)                                                      
          TIRL(J)=TIRL(J)+VIL(J1,J)                                                      
          CST1=CST1+CSTF(J1,J)                                                           
          CSO1=CSO1+CSOF(J1,J)                                                           
          TFTN(J)=TFTN(J)+FRTN(J1,J)                                                     
          TFTP(J)=TFTP(J)+FRTP(J1,J)                                                     
          TFTK(J)=TFTK(J)+FRTK(J1,J)                                                     
          TCST(J)=TCST(J)+CSTF(J1,J)                                                     
          TCSO(J)=TCSO(J)+CSOF(J1,J)                                                     
          TVAL(J)=TVAL(J)+XTP(1,J1,J)+XTP(2,J1,J)                                        
          PSTM(J)=PSTM(J)+TPSF(J1,J)
      END DO                                                               
      IF(N1>1)N2=N2-1                                                                
      K=K+1                                                                          
      END DO                                                                          
      VIRT=0.                                                                        
      DARF=DARF+SMY(4)*SMY(4)                                                        
      IF(SMY(4)>BARF)BARF=SMY(4)                                                                  
      IF(SMY(4)<SARF)SARF=SMY(4)                                                   
      !     PRINTOUT ANNUAL FILE                                                           
      X1=.001*TOC                                                                    
      IF(NSTP==0)GO TO 556                                                           
	  IF(IY/=IGSD)GO TO 555                                                               
      K=IRTC                                                                         
      L1=LY(IRO,K)                                                                   
      J=MAX(1,NHV(L1))                                                               
	  WRITE(KW(2),498)IYR,IRLX,SMY(4),SMY(10),SMY(11),SMY(14),SMY(16),&                   
      SMY(17),SMY(29),SMY(33),SMY(42),SMY(48),SMY(47),SMY(50),SMY(51),&              
      SMY(52),SMY(49),SMY(43),SMY(44),SMY(45),SMY(46),SMY(56),SMY(54),&              
      SMY(55),SMY(57),TLA,OCPD,X1,APBC,TAP,ZNO3                                      
      IF(KFL(19)==0)GO TO 555                                                        
      WRITE(KW(19),558)IYR,IRLX,CPNM(L1),YLD1(J,L1),YLD2(J,L1),XTP(4,J,&
      L1),HIF(J,L1),DMF(J,L1),RWF(J,L1),YLNF(J,L1),YLPF(J,L1),YLCF(J,L1),&
      FRTN(J,L1),FRTP(J,L1),FRTK(J,L1),VIR(J,L1),VIL(J,L1),XTP(3,J,L1),&
      ETG(J,L1),CAW(J,L1),CRF(J,L1),CQV(J,L1),CSTF(J,L1),CSOF(J,L1),XTP&
      (1,J,L1),XTP(2,J,L1),TPSF(J,L1),(SF(L,J,L1),L=1,7),PPL0(L1),IPLD&
      (J,L1),IGMD(J,L1),IHVD(J,L1)                                                                     
      IF(J==1)GO TO 555                                                              
      J1=J-1                                                                         
      IPLD(J1,L1)=IPLD(J,L1)                                                         
      IGMD(J1,L1)=IGMD(J,L1)                                                         
      GO TO 555                                                                      
  556 IF(KFL(2)>0)WRITE(KW(2),498)IYR,IY,SMY(4),SMY(10),SMY(11),SMY&                 
      (14),SMY(16),SMY(17),SMY(29),SMY(33),SMY(42),SMY(48),SMY(47),SMY&              
      (50),SMY(51),SMY(52),SMY(49),SMY(43),SMY(44),SMY(45),SMY(46),SMY&              
      (56),SMY(54),SMY(55),SMY(57),TLA,OCPD,X1,APBC,TAP,ZNO3                         
      IF(KFL(19)==0)GO TO 555                                                        
	  K=1                                                                                 
	  N2=NN                                                                               
  686 J=LY(IRO,K)                                                                    
      N1=MAX(1,NCP(J))                                                               
      DO J1=1,N1                                                                     
          WRITE(KW(19),558)IYR,IY,CPNM(J),YLD1(J1,J),YLD2(J1,J),XTP(4,J1,&
          J),HIF(J1,J),DMF(J1,J),RWF(J1,J),YLNF(J1,J),YLPF(J1,J),YLCF&
          (J1,J),FRTN(J1,J),FRTP(J1,J),FRTK(J1,J),VIR(J1,J),VIL(J1,J),&
          XTP(3,J1,J),ETG(J1,J),CAW(J1,J),CRF(J1,J),CQV(J1,J),CSTF(J1,J),&
          CSOF(J1,J),XTP(1,J1,J),XTP(2,J1,J),TPSF(J1,J),(SF(L,J1,J),L=1,7),&
          PPL0(J),IPLD(J1,J),IGMD(J1,J),IHVD(J1,J)
          IF(J1==1)CYCLE                                                             
          L1=J1-1                                                                    
          IPLD(L1,J)=IPLD(J1,J)                                                      
          IGMD(L1,J)=IGMD(J1,J)                                                      
      END DO                                                                         
      IF(N1>1)N2=N2-1                                                                
	  K=K+1                                                                               
	  IF(K<=N2)GO TO 686                                                                  
  555 IF(KFL(23)>0)THEN                                                              
          DO K=1,NN                                                                    
              L1=LY(IRO,K)                                                             
              DO J=1,NCP(L1)                                                           
                  WRITE(KW(23),668)IYR,IY,M21,K21,CPNM(L1),DM1(L1),(RWTX&              
                  (LID(I),L1),I=1,NBSL),RWX(L1)                                        
              END DO                                                                   
          END DO                                                                       
      END IF                  
      IF(KFL(22)>0)THEN                                                              
          RNO3=SMY(4)*RFNC                                                       
          WRITE(KW(22),566)IYR,SMY(4),SMY(10),SMY(11),SMY(13),SMY(14),&
	      (SMY(J),J=16,20),RNO3,(SMY(J),J=43,46),SMY(49),SMY(89),SMY(52),&
	      SMY(85),SMY(50),(SMY(J),J=59,61)
      END IF                             
      APBC=.5*(SMY(1)+SMY(2))                                                        
      IF(KFL(8)>0)WRITE(KW(8),293)IRO0,IYR,APBC,PMTE,(SMY(KYA(K)),&
      K=1,NKYA)                                                                          
      RWTX=0.
      DO K=1,LC                                                                       
          J=LY(IRO,K)
          IF(J==0)CYCLE
          IF(KFL(24)>0)THEN                                                              
	          IF(IDC(J)==NDC(7).OR.IDC(J)==NDC(8))WRITE(KW(24),669)IYR,IY,&                   
              CPNM(J),YLD2(K,J),DM(J),RW(J),SLAI(J),STD(J)                                  
          END IF                                                                         
          NCP(J)=1
          IF(KG(J)==0)NCP(J)=0                                                           
          NHV(J)=0                                                                       
	      RWX(J)=0.                                                                           
	      DO I=N1,1,-1                                                                        
              NPSF(I,J)=0                                                                
              FRTN(I,J)=0.                                                               
              FRTP(I,J)=0.                                                               
              FRTK(I,J)=0.                                                               
              YLD1(I,J)=0.                                                               
              YLD2(I,J)=0.                                                               
              YLNF(I,J)=0.                                                               
              YLPF(I,J)=0.                                                               
              YLKF(I,J)=0.
              YLCF(I,J)=0.                                                               
	          DMF(I,J)=0.
	          RWF(I,J)=0.                                                                     
              VIL(I,J)=0.                                                                
              VIR(I,J)=0.                                                                
              CAW(I,J)=0.                                                                
              CQV(I,J)=0.                                                                
              CRF(I,J)=0.                                                                
	          TPSF(I,J)=0.                                                                    
              CSOF(I,J)=0.                                                               
              CSTF(I,J)=0.                                                               
              ETG(I,J)=0.                                                                
          END DO                                                                         
	      HUF(J)=0.                                                                           
          RDF(J)=0.                                                                      
          BLYN(J)=0.                                                                     
          IF(IDC(J)==NDC(3).OR.IDC(J)==NDC(6).OR.IDC(J)==NDC(7).OR.IDC&                  
          (J)==NDC(8).OR.IDC(J)==NDC(10))THEN                                            
              ANA(J)=0.                                                                  
	          SWH(J)=0.                                                                       
	          SWP(J)=0.                                                                       
	          ACET(J)=0.
	          AWC=0.                                                                   
              ARF=0.                                                                   
              AQV=0.                                                                                                                                          
	      END IF                                                                              
          IF(IDC(J)/=NDC(2).AND.IDC(J)/=NDC(5))DM1(J)=0.
      END DO
      IF(KFL(NGF)==0)GO TO 499                                                       
	  X2=SMY(49)+SMY(52)                                                                  
	  X3=SMY(43)+SMY(44)+SMY(45)+SMY(46)                                                  
	  SWGS=SWGS/REAL(ND)                                                                  
!     XLOG      = SITE LONGITUDE                                                     
!     YLAT      = SITE LATITUDE                                                      
!     YGIS      = CROP YLD (t/ha)                                                    
!     WGIS      = CROP YLD/GROWING SEASON ET (kg/mm)                                 
!     ETGS      = GROWING SEASON ET (mm)                                             
!     GSEF      = GROWING SEASON PLANT TRANSPIRATION (mm)                            
!     SMY(19)   = IRRIGATION VOLUME (mm)                                             
!     FGIS      = N FERTILIZER APPLIED (kg/ha)                                       
!     SMY(11)   = ACTUAL ET (mm)                                                     
!     GSVF      = GROWING SEASON VPD (kpa)                                           
!     HGIS      = CROP HARVEST INDEX                                                 
!     SRAF      = GROWING SEASON SOLAR RADIATION (MJ/M^2)                            
!     YLNG      = N CONTENT OF CROP YLD (kg/ha)                                      
!     TNGS      = N CONTENT OF SOIL (kg/ha)                                          
!     X2        = N LOST TO ATMOSPHERE (kg/ha)                                       
!     X3        = N LOST WITH RUNOFF, SUBSURFACE FLOW, AND EROSION                   
!     SMY(42)   = WIND EROSION (t/ha)                                                
!     SMY(NDVSS)= WATER EROSION FOR DRIVING EQ (t/ha)                                
!     SMY(3)    = SOLAR RADIATION (MJ/M^2)                                           
!     BGIS      = BIOMASS PRODUCTION (t/ha)                                          
!     SF(1,     = WATER STRESS (d)                                                   
!     SF(2,     = N STRESS (d)                                                       
!     SF(3,     = P STRESS (d)                                                       
!     SF(4,     = K STRESS (d)                                                       
!     SF(5,     = TEMPERATURE STRESS (d)                                             
!     SF(6,     = AERATION STRESS (d)                                                
!     SF(7,     = SALT STRESS (d)                                                    
!     SMY(46)   = N LEACHED (kg/ha)                                                  
!     SMY(43)   = ORGANIC N LOSS WITH SEDIMENT (kg/ha)                               
!     SMY(47)   = NET MINERALIZATION (kg/ha)                                         
!     SMY(49)   = DENITRIFICATION (kg/ha)                                            
!     SMY(50)   = N FIXATION (kg/ha)                                                 
!     SMY(85)   = N MINERALIZATION FROM HUMUS (kg/ha)                                
!     SMY(51)   = NITRIFICATION (kg/ha)                                              
!     SMY(52)   = VOLATILIZATION (kg/ha)                                             
!     SMY(53)   = N LOSS IN DRAINAGE SYSTEM (kg/ha)                                  
!     SMY(54)   = P LOSS WITH SEDIMENT (kg/ha)                                       
!     SMY(56)   = P MINERALIZATION (kg/ha)                                           
!     SMY(57)   = P LEACHED (kg/ha)                                                  
!     SMY(58)   = ENRICHMENT RATIO                                                   
!     SMY(59)   = N FERT ORGANIC FORM (kg/ha)                                        
!     SMY(60)   = N FERT NO3 FORM (kg/ha)                                            
!     SMY(61)   = N FERT NH3 FORM(kg/ha)                                             
!     SMY(62)   = P FERT ORGANIC FORM (kg/ha)                                        
!     SMY(63)   = P FERT MINERAL FORM (kg/ha)                                        
!     YLPG      = P CONTENT OF CROP YIELD (kg/ha)                                    
!     YLKG      = K CONTENT OF CROP YIELD (kg/ha)                                    
!     SMY(77)   = ORGANIC C LOSS WITH SEDIMENT(kg/ha)                                
!     SMY(4)    = PRECIPITATION (mm)                                                 
!     SMY(17)   = PERCOLATION (mm)                                                   
!     SMY(14)   = RUNOFF (mm)                                                        
!     SMY(5)    = SNOWFALL (WATER EQUIVALENT mm)                                     
!     SMY(6)    = SNOWMELT (WATER EQUIVALENT mm)                                     
!     SMY(10)   = POTENTIAL ET (mm)                                                  
!     SMY(12)   = POTENTIAL PLANT TRANSPIRATION (mm)                                 
!     SMY(13)   = ACTUAL PLANT TRANSPIRATION (mm)                                    
!     SMY(16)   = LATERAL SUBSURFACE FLOW (mm)                                       
!     SMY(18)   = DRAINAGE SYSTEM FLOW (mm)                                          
!     SMY(15)   = SCS CURVE NUMBER                                                   
!     SMY(20)   = EXTERNAL INFLOW TO MAINTAIN WATER TABLE (mm)                       
!     SWGS      = ROOT ZONE SOIL WATER (mm)                                          
!     SMY(68)   = SOIL WATER IN TOP 10 mm OF SOIL (mm)                               
	  WRITE(KW(IGIS),685)XLOG,YLAT,YGIS,WGIS,ETGS,GSEF,SMY(19),FGIS,SMY&                  
      (11),GSVF,HGIS,SRAF,YLNG,TNGS,X2,X3,SMY(42),SMY(NDVSS),SMY(3),BGIS&            
      ,(SF(L,1,1),L=1,7),SMY(46),SMY(43),SMY(47),SMY(49),SMY(50),SMY(85)&            
      ,SMY(51),SMY(52),SMY(53),SMY(54),SMY(56),SMY(57),SMY(58),SMY(59),&             
      SMY(60),SMY(61),SMY(62),SMY(63),YLPG,YLKG,SMY(77),SMY(4),SMY(17),&             
      SMY(14),SMY(5),SMY(6),SMY(10),SMY(12),SMY(13),SMY(16),SMY(18),SMY&             
      (15),SMY(20),SWGS,SMY(68)                                                      
      SMGS(8)=SMGS(8)+SWGS                                                           
  499 WB1=SMY(34)                                                                    
	  WB2=SMY(42)
	  SF=0.                                                                               
      SMM=0.                                                                         
      IF(NDP==0)GO TO 83                                                             
      SMYP=0.                                                                        
      SMMP=0.                                                                        
      TPAC=0.                                                                        
   83 MO1=MO                                                                         
   84 CONTINUE                                                                       
      IBD=1                                                                          
      MO=1                                                                           
      KT=1                                                                           
      KC=0                                                                           
	  IYR1=IYR                                                                            
      IYR=IYR+1                                                                      
      IYX=IYX+1                                                                      
      NYD=1                                                                          
      IPY=IPY+II                                                                     
	  CALL SCONT(1)                                                                          
	  IF(ICOR0>0)THEN                                                                     
	      DO J=1,21                                                                         
              IX(J)=IX0(J)                                                             
              IDG(J)=J                                                                 
          END DO                                                                       
      END IF                                                                         
      IF(ISW<=3.OR.ISW==7)THEN
          XX=0.                                                                          
          K=1                                                                            
          TPAW=0.                                                                        
	      DO I=1,NBSL                                                                
              J=LID(I)                                                                       
	          Y1=.1*WOC(J)/WT(J)                                                                  
	          XZ=.0172*Y1                                                                         
	          ZZ=1.-XZ                                                                            
	          BD(J)=1./(XZ/.224+ZZ/BDM(J))
	          BDX=PRMT(2)+.35+.005*SAN(J)
	          BD(J)=MIN(BD(J),BDX)                                                                                                                                            
	          ZR=1.E-4*WT(J)/BD(J)
	          IF(I/=NBSL)Z(J)=XX+ZR                                                                                                                                            
              X1=1000.*(Z(J)-XX)                                                             
              IF(ISW/=0.AND.ISW/=2)THEN
                  IF(ISW==7)THEN                                                                 
                      CALL SWNN(CLA(J),SAN(J),Y1,X2,X3)                                           
                  ELSE                                                                           
                      ZZ=.5*(XX+Z(J))                                                              
                      CALL SWRTNB(CEM(J),CLA(J),Y1,SAN(J),X2,X3,ZZ)                                
	              END IF
              ELSE                  
                  CALL SWRTNR(CLA(J),SAN(J),Y1,X2,X3)                                            
              END IF               
              XY=1.-ROK(J)*.01                                                               
              XZ=XY*X1                                                                       
              S15(J)=X2*XZ                                                                   
              FC(J)=X3*XZ                                                                    
              CALL SPOFC(J)                                                                  
              IF(K>3)GO TO 670                                                               
              TPAW=TPAW+FC(J)-S15(J)                                                         
          672 IF(TPAW<WCS(K))GO TO 670                                                       
              ZCS(K)=XX+(Z(J)-XX)*((WCS(K)-PZW)/(TPAW-PZW))                                  
              K=K+1                                                                          
              IF(K<4)GO TO 672                                                               
          670 PZW=TPAW                                                                       
              XX=Z(J)
          END DO
          NBCL=Z(LID(NBSL))/DZ+.999
          !ZC(NBCL)=NBCL*DZ
      END IF                                                                                            
      IF(ISTA>0)THEN                                                                 
          DO J=1,NBSL                                                                  
              L=LID(J)                                                                 
	          WHSC(L)=SOL(1,L)                                                              
              WHPC(L)=SOL(2,L)                                                         
              WLSC(L)=SOL(3,L)                                                         
              WLMC(L)=SOL(4,L)                                                         
              WBMC(L)=SOL(5,L)                                                         
              WOC(L)=SOL(6,L)                                                          
              WHSN(L)=SOL(7,L)                                                         
              WHPN(L)=SOL(8,L)                                                         
              WLSN(L)=SOL(9,L)                                                         
              WLMN(L)=SOL(10,L)                                                        
              WBMN(L)=SOL(11,L)                                                        
	          WON(L)=SOL(12,L)                                                              
	          PMN(L)=SOL(13,L)                                                              
              WP(L)=SOL(14,L)                                                          
              OP(L)=SOL(15,L)                                                          
              EXCK(L)=SOL(16,L)                                                        
              FIXK(L)=SOL(17,L)                                                        
!             ST(L)=SOL(18,L)                                                          
	          WLS(L)=SOL(19,L)                                                              
	          WLM(L)=SOL(20,L)                                                              
	          WLSL(L)=SOL(21,L)                                                             
              WLSLC(L)=SOL(22,L)                                                       
              WLSLNC(L)=SOL(23,L)                                                      
          END DO                                                                       
!         CALL SCONT(1)                                                                   
      END IF                                                                         
      CALL SPRNT(YTP)                                                                
	  IF(KFL(14)>0)CALL SOCIOA(IYR1,12,KDA)                                               
	  SMY=0.                                                                         
	  IF(IPD==2.OR.IPD==4)THEN                                                            
          WRITE(KW(1),101)                                                           
          CALL SOLIO(YTP,1)                                                          
      END IF                                                                         
      IF(IGSD/=0.AND.IY==IGSD)THEN                                                   
          REWIND KR(7)                                                               
          CALL WREAD                                                                 
          NGN=NGN0                                                                   
          IYR=IYR0                                                                   
          IGSD=IGSD+NRO                                                              
      END IF                                                                         
      CALL ALPYR(IYR,NYD,LPYR)                                                       
	  IGIS=IGIS+1                                                                         
   87 CONTINUE                                                                       
      IY=NBYR+1                                                                      
   88 RETURN                                                                         
   63 FORMAT(1X,3I4,2X,A8,2X,'LIME RATE=',F8.2,' t/ha',1X,'CROP=',A4,&
      1X,'COTL=',F7.2,' $',1X,'COOP=',1X,F7.2,' $')                                                                         
   89 FORMAT(1X,3I4,2X,'GERMINATION--0.2 m SW = ',F5.1,'mm',2X,'HU = ',&
      F4.0,'c',2X,'HUSC = ',F4.2,2X,A4)                                                               
   90 FORMAT(1X,3I4,2X,'LIME',2X,'RATE=',F6.0,'t/ha',1X,'HUSC=',F4.2,2X,&
      'COST=',F7.0,'$/ha')                                                                                                                 
   92 FORMAT(1X,3I4,2X,'RB FR DK',20X,'DKH=',F5.0,'mm',2X,'DKI=',F6.2&            
      ,'m',2X,'HUSC=',F4.2)                                                          
   94 FORMAT(//T5,'Y M D  OPERATION')                                                
   95 FORMAT(1X,A4,12F9.2,11X,A4)                                                    
   96 FORMAT(//I5,9(2X,A4,F8.2)/(5X,9(2X,A4,F8.2)))                                  
   97 FORMAT(T10,'STRESS DAYS (BIOM)     WATER=',F5.1,2X,'N=',F5.1,2X,&              
      'P=',F5.1,2X,'K=',F5.1,2X,'TEMP=',F5.1,2X,'AIR=',F5.1,2X,'SALT=',&             
      F5.1)                                                                          
   99 FORMAT(45X,'YR=',I4,2X,'YR#=',I4/T11,'JAN',9X,'FEB',9X,'MAR',9X,&              
      'APR',9X,'MAY',9X,'JUN',9X,'JUL',9X,'AUG',9X,'SEP',9X,'OCT',9X,&               
      'NOV',9X,'DEC',9X,' YR')                                                       
  100 FORMAT(10F8.2)                                                                 
  101 FORMAT(T5,'SOIL DATA')                                                         
  102 FORMAT(45X,'YR=',I4,2X,'YR#=',I4/T11,'JAN',6X,'FEB',6X,'MAR',6X&               
      ,'APR',6X,'MAY',6X,'JUN',6X,'JUL',6X,'AUG',6X,'SEP',6X,'OCT',6X,&              
      'NOV',6X,'DEC',6X,' YR')                                                       
  103 FORMAT(14X,9F6.2)                                                              
  104 FORMAT(1X,A4,13F9.2,2X,A4)                                                     
  105 FORMAT(1X,3I4,1X,A4,5(2X,A4,F8.3)/(10X,5(2X,A4,F8.3)))                      
  106 FORMAT(2X,A4,1X,'YLD=',F5.1,'/',F5.1,2X,'BIOM=',F5.1,'t/ha',2X,&               
      'YLN=',F5.0,2X,'YLP=',F5.0,2X,'YLK=',F5.0,2X,'YLC=',F6.0,2X,'FN=',&
      F5.0,2X,'FP=',F5.0,2X,'FK=',F5.0,'kg/ha'/T7,'IRGA=',F5.0,2X,'IRDL=',&
      F5.0,2X,'CAW=',F5.0,'mm',2X,'GSET=',F5.0,'mm',2X,'WUEF=',F6.2,&
      'kg/mm',2X,'POP=',F9.4,'p/m2',2X,'PSTF=',F4.2/T7,'COST=',F7.0,2X,&
      'COOP=',F7.0,2X,'RTRN=',F5.0,'/',F5.0,'$/ha',2X,'EK=',F5.3,2X,&
      'REK=',F5.3,2X,'WK=',F5.3)
  
  108 FORMAT(1X,A4,12I9,11X,A4)                                                      
  109 FORMAT(1X,A4,13F9.4,2X,A4)                                                     
  111 FORMAT(34X,'-------------------------',A8,'-----------------------&            
      --')                                                                           
  112 FORMAT(47X,'PESTICIDE SIMULATION (G/HA)')                                      
  113 FORMAT(1X,A4,13F9.0,2X,A4)                                                     
  114 FORMAT(1X,A4,12F9.0,11X,A4)                                                    
  119 FORMAT(10I8)                                                                   
  143 FORMAT(1X,A4,13F12.5,2X,A4)                                                    
  146 FORMAT(1X,A4,12F12.5,14X,A4)                                                   
  156 FORMAT(/T5,'YEAR ',I4,' OF ',I4,/)                                             
!  293 FORMAT(1X,2I4,42F8.2)                                                          
  293 FORMAT(2(1X,I4),42(1X,F12.2))
  465 FORMAT(8X,'ADSRB',5E13.5)                                                      
  462 FORMAT(5X,A16)                                                                 
  464 FORMAT(8X,'Q+SSF',5E13.5)                                                      
  463 FORMAT(8X,'SOL  ',5E13.5)                                                      
  466 FORMAT(8X,'SED Y',5E13.5)                                                      
  468 FORMAT(1X,3I4,1X,I4,1X,A16,14F10.4)                                         
  471 FORMAT(4X,'Y M D  RT#  PSTN',12X,10(4X,A4,2X),5X,'Q',8X,'SSF',6X,&             
      'PRK',4X,'ROCONC')                                                             
  474 FORMAT(4X,4I4)                                                                 
  475 FORMAT(1X,I4,I2,I4,2X,40F8.1)                                                  
  477 FORMAT(4X,'Y M RT#',43(4X,A4))                                                 
  498 FORMAT(1X,2I4,6F8.1,F8.3,17F8.1,F8.2,F8.1,F8.2,2F8.1,10(A16,F8.0))             
  505 FORMAT(A80)                                                                    
  513 FORMAT(1X,3I4,1X,I4,4(4X,A4,7F8.3))                                         
  516 FORMAT(4X,'Y   M   D  ',13(1X,A4,3X))                                                 
  517 FORMAT(T27,'|----------------------------------------------TEMP(C)&            
      --------------------------------------------------|'/6X,'DATE',8X,&            
      'DAMP',15X,'|_______________________________@ CENTER OF SOIL LAYER&            
      S______________________________________|'/T5,'Y   M   D',3X,'DEPTH&            
      (m)',5X,'SURF',7X,'1',9X,'2',9X,'3',9X,'4',9X,'5',9X,'6',9X,'7',9X&            
      ,'8',9X,'9',8X,'10')
  520 FORMAT(4X,'Y   M  ',32(A4,4X))                                                 
  523 FORMAT(1X,I4,I2,I4,2X,A4,13F8.1)                                               
  526 FORMAT(4X,'Y M RT#  CPNM',6X,'WS',6X,'NS',6X,'PS',6X,'KS',6X,'TS',&            
      6X,'AS',6X,'SS',6(4X,A4))                                                      
  527 FORMAT(4X,'Y M D  RT#',4(4X,'CPNM',3X,'WS',6X,'NS',6X,'PS',6X,'KS'&            
      ,6X,'TS',6X,'AS',6X,'SS'))                                                     
  532 FORMAT(1X,3I4,1X,5(2X,A4,F8.3)/(10X,5(2X,A4,F8.3)))                         
  533 FORMAT(5X,12I4)                                                                
!  558 FORMAT(1X,2I4,1X,A4,6F8.3,6F8.1,2F8.3,4F8.1,4F8.2,9F8.3,3I9,&                  
!     &10(1X,A16,F8.0))                                                               
  558 FORMAT(2(1X,I4),1X,A4,7(1X,F8.3),6(1X,F8.1),2(1X,F8.3),4(1X,F8.1),&
      4(1X,F8.2),9(1X,F8.3),3(1X,I9),10(1X,A16,F8.0))                                                               
  562 FORMAT(1X,I4,I2,2X,60F8.1)                                                     
  565 FORMAT(1X,2I4,28F8.2,4X,A4,F8.2,F8.0)                                          
  566 FORMAT(1X,I4,4X,10F8.2,24X,15F8.2,4X,A4,F8.2,F8.0)                                          
  567 FORMAT(1X,3I4,2X,'LIME',14X,I4,6X,'   9',8X,F10.2,10X,2F10.2)                  
  582 FORMAT(T10,'ATMOS CO2 = ',F5.0)                                                
  588 FORMAT(5X,'!!!!!',3I4,' Q= ',F6.1)                                          
  589 FORMAT(5X,'^^^^^',3I4,' PDSW = ',F6.1,1X,'FCSW = ',F6.1)                    
  666 FORMAT(1X,3I4,2X,A8,8X,I6,6X,3I4,2F10.2,20X,F10.2)                             
  668 FORMAT(1X,4I4,1X,A4,20F8.2)                                                
!  669 FORMAT(1X,2I4,1X,A4,5F8.2)                                                     
  669 FORMAT(2(1X,I4),1X,A4,5(1X,F8.2))
  679 FORMAT(1X,I4,1X,I4,1X,13(1X,F10.3))                                            
  680 FORMAT(10X,'WATERSHED AREA = ',F10.2,' HA'/18X,'Q',10X,'Y',9X,'YN'&            
      ,9X,'YP',9X,'QN',9X,'QP'/2X,'YR    MO',7X,'(mm)',6X,'(t/ha)',3X,&              
      '|-----------------(kg/ha)---------------|')                                   
!  681 FORMAT(1X,I4,1X,I4,6F8.2,1X,A16,1X,A4,8F8.2,E12.5)                           
  681 FORMAT(2(1X,I4),6(1X,F8.2),1X,A16,3X,A4,8(1X,F8.2),1X,E12.5)                           
  682 FORMAT(1X,3I4,F9.1,400F9.5)
!  683 FORMAT(T60,A16,1X,A4,8F8.2,E12.5)                                                                             
  683 FORMAT(T65,A16,3X,A4,8(1X,F8.2),1X,E12.5)                                                                             
  685 FORMAT(1X,4F10.2,5F10.1,2F10.3,F10.0,50F10.2)
  730 FORMAT(4X,'Y',3X,'M',3X,'D',5X,A4,1X,1000(A7,A2,4X))                                                                     
  731 FORMAT(4X,'Y',3X,'M',3X,'D',4X,'SW15',4X,'SW30',3X,'NO315',3X,&                
      'NO330',3X,'NH315',3X,'NH330',3X,200(A4,4X))                                   
      END                                                                            
