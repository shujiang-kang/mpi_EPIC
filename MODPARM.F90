MODULE PARM
                                                                    
      CHARACTER(4)::TITLE(60),HEDS(30),HEDC(20),SID(16),HEDP(10)                     
      CHARACTER(9)::SLOD(3) 
      CHARACTER(46)::ASTN,SOLO                                                             
!     JEFF NICHOLS BASEDIR is the base directory for the program if an alternative is passed on the command line.
      CHARACTER(512):: BASEDIR = ""                                              
      CHARACTER(512):: OUTDIR = ""                                              
      ! David Manowitz: Debug_output signifies if debugging statements are to be printed
      LOGICAL :: DebugOutput = .FALSE.
      ! David Manowitz (3/8/2011): Signifies if EPIC is to be run in "comparison mode", where
      ! certain outputs are changed to make comparing runs easier
      LOGICAL :: ComparisonMode = .FALSE.
      ! David Manowitz (8/22/2011): Get the name of the command as called on the command line.
      CHARACTER(256) :: CommandName = ""
      ! David Manowitz (8/31/2011): Add an explicit build version string.  Start it at 1976.
      CHARACTER(LEN = *), PARAMETER :: VersionStr = "EPIC1102v1, build #1984.5 [choose soil I/O]"

      CHARACTER(512):: OPSCFILE="",SITEFILE="",SOILFILE=""
      CHARACTER(512)::FWTH                                                              
      CHARACTER(4),DIMENSION(:),ALLOCATABLE::CPNM,HED                                
      CHARACTER(8),DIMENSION(:),ALLOCATABLE::ASHZ,FTNM,TIL                                
      CHARACTER(16),DIMENSION(:),ALLOCATABLE::PSTN                                   
      INTEGER::IAC,IAUF,IAUI,IAUL,IAZM,IBAT,IBD,IBDT,ICCD,ICDP,ICF,&
      ICG,ICO2,ICOR0,ICV,IDA,IDAY,IDA0,IDF0,IDFP,IDN,IDR,IDRL,IDS,IDSP,&
      IERT,IET,IEVI,IFA,IFD,IGO,IGN,IGSD,IGS0,IGZ,IHUS,IHV,III,IMON,&
      IMO0,IMW
      INTEGER::INFL,INP,IOX,IPAT,IPD,IPL,IPST,IPY,IPYI,IRI,IRL,IRO,IRO0,&
      IRR,IRUN,IRTC,ISAT,ISCN,ISG,ISL,ISTA,ISTP,ISW,ISX,ITYP,IT1,IT2,&
      IT3,IUN,IWP5,IWTH,IY,IYER,IYR,IYR0,IYX,JCN,JCN0,JCN1,JD,JDA,JDHU,&
      JJK,JT1,JT2,KC,KDA,KF,KHV,KI,KP,KP1,KT             
      INTEGER::KTT,K21,LBP,LC,LD1,LMS,LPYR,LRD,LUN,LW,M21,MASP,MFT,MNC,&
      MNT,MNU,MO,MO1,MOFX,MPS,MRO,MSL,MSO,MXT,MYR,NBCL,NBDT,NBSL,NBYR,&
      ND,NDF,NDP,NDRV,NDT,NDVSS,NEV,NFA,NFS,NGF,NGN,NGN0,NII,NJC,&
      NKA,NKD,NKS,NKYA,NMW,NNE,NOFT,NOP,NQP,NQP0,NQP1,NRO,NSM,NSNN,NSTP,&
      NSX,NUDK,NUPC,NVCN,NWDA,NWD0,NWI,NYD                                                                   
      INTEGER :: NX(43000),KDF1(8000),KDP1(8000),KDC1(800),KW(200),IGY(150),&                
      KDT2(100),KA(100),IFS(40),KD(40),KYA(40),KS(40),KR(30),&                  
      NHC(27),IDG(21),IX(21),IX0(21),NC(13),IHRL(12),IWIX(12),IDC(10),&              
      NDC(10),IYS(8),JX(7),ISIX(6),KGN(5),JC(4),IDFT(2)                              
      INTEGER,DIMENSION(:),ALLOCATABLE::IHU,IYH,JE,JP,JPL,KDC,KFL,KG,&               
      KOMP,LID,LORG,NBC,NBE,NBT,NCP,NCR,NHU,NHV,NPST,NTL,NYLN,ICUS,IHC,&             
      IHT,KDF,KDP                                                                    
      INTEGER,DIMENSION(:,:),ALLOCATABLE::ITL,LFT,LT,LYR,NGZ,LPC,JH,LY,&             
      NBCX,IGMD,IHVD,IPLD,NPSF                                                       
      REAL::ABD,ACW,ADRF,AFN,AFLG,AGP,AGPM,AHSM,AILG,ALB,ALG,ALTC,AL5,&              
      AMSR,ANG,APB,APBC,APMU,AQV,ARF,ARMN,ARMX,AVT,AWC,BARF,BCV,BETA,BFCO2,&
      BFN2O,BFO2,BFT,BIG,BIR,BTK,BTCX,BTN,BTNX,BTP,CAP,CBVT,CEJ,CFE0,CFEM,&
      CFMN,CFNP,CHL,CLF,CLG,CLT,CMN,CN,CNO3I,CN0,CN1,CN2,CN3,COIR,COST,&
      COL,CON,COP,COS1,COWW,CO2,CRKF,CSFX,CSLT,CSO1,CST1,CV,CVF,CVP,CVRS,&
      CYAV,CYMX,CYSD,DALG,DARF,DCFE,DD,DFCO2S,DFCO2T,DFO2S,DFO2T,DFX,DFN2OS,&
      DFN2OT,DHT,DKIN,DKHL
      REAL::DN2,DN2O,DR,DST0,DTG,DUR,DV,DZ,DZ10,DZA,EAO2,ED,EFI,EI,EK,ELEV,&
      EO,ER,ES,EVI,EXNN,EXPK,FCSW,FCV,FDSF,FFED,FGC,FGIS,FL,FMX,FNP,FNPI,&
      FULP,FW,GFCO2,GFN2O,GFO2,GMA,GSEF,GSEP,GSVF,GSVP,GWMX,GWST,GX,GZLM,&
      HCLD,HCLN,HIX,HLMN,HMN,HRLT,HR0,HR1,HSM,OCPD,PALB,PAW,PB,PDSW,PEC,&
      PIT,PI2,PMOEO,PMORF,PMX,PR                       
      REAL::PRAV,PRB,PRSD,PRWM,PSTS,PSTX,QAP,QCO2,QD,QN2O,QNO2,QNO3,QO2,QP,&
      QPQB,QPR,QPS,QVOL,RAMX,RCF,RCN,REK,REP,RFNC,RFSD,RFSK,RFSM,RFTT,&
      RFV,RFVM,RGIN,RGRF,RGSM,RHD,RHM,RHTT,RLF,RLSF,RM,RMNR,RNO3,ROSP,&
      RPR,RRF,RRUF,RSF,RSK,RSLK,RUNT,RWO,RZ,RZSW,SALB,SARF,SAT,SATK,SCI,&
      SCN,SDN,SEP,SGMN,SHM,SHRL,SIM,SIN1,SIP,SK,SL,SLR,SLRY,SLT0,SM99
      REAL::SMER,SML,SMP,SMR,SMX,SN,SN2,SN2O,SNA15,SNA30,SNN15,SNN30,SNIT,&
      SNMN,SNO,SNOF,SNPKT,SP,SQ,SRA,SRAD,SRAF,SRAM,SSFK,SSFNO2,SSFNO3,&
      SSLT,SSST,SST,SSW,STDK,STDL,STDO,STDOK,STDON,STDOP,STDP,STD0,ST0,&
      SU,SUK,SUN,SUP,SVOL,SW,SW15,SW30,SWRZ,SX,S3,TA,TAP,TC,TCAV,TCC,&
      TCMN,TCMX,TCS,TEK,TFK,TFNH3,TFNO,TFNO3,TFOP,TFPL,TFPO,TH,THK,TLA,&
      TLGE,TLGQ                           
      REAL::TLGW,TLMF,TMN,TMNM,TMP,TMX,TMXM,ZNH3,ZN2O,ZN2OG,ZN2OL,ZNO2,&
      ZNO3,TNOR,TNSD,TOC,TOP,TP,TRSD,TRSP,TSFK,TSFN2O,TSFNO2,TSFNO3,TSFS,&
      TSK,TSLT,TSNO,TSRZ,TWN,TWN0,TX,TXSD,TXXM,TYC,TYC1,TYK,TYN,TYN1,TYP,&
      UB1,UNO3,UNT,UOB,UPK,UPP,UPS,UPSL,UPSQ,UPSX,USL,UST,USTRT,USTT,&
      USTW,UX,UXP,U10
      REAL::VAC,VALF1,VALF2,VAP,VF,VIMX,VIRT,VLG,VLGI,VLGM,VLGN,VPD,VSK,&
      VSLT,V1,V3,WAGE,WB,WCF,WCYD,WDN,WDRM,WFX,WIM,WIP,WK,WMP,WS,WSA,WSA1,&
      WTBL,WTMN,WTMU,WTMX,WTN,BRNG,XHSM,XKN1,XKN3,XKN5,XNS,YERO,YEW,YLAT,&
      YLDX,YLK,YLN,YLP,YLTC,YLTS,YN,YP,YSLT,YSW,YTN,YTN1,YW,XLOG,ZBMC,&
      ZBMN,ZF,ZHPC,ZHPN,ZHSC,ZHSN,ZLM,ZLMC,ZLMN,ZLS,ZLSC,ZLSL,ZLSLC,ZLSLNC,&
      ZLSN,ZQT                          
      REAL::VQ(90),VY(90),PRMT(80),VARS(30),ASW(12),CX(12),QIN(12),RCM(12),&              
      RNCF(12),SET(12),RSY(12),SRD(12),SRMX(12),TAL(12),TAMX(12),&              
      TAV(12),TEI(12),TET(12),THRL(12),TMNF(12),TMXF(12),TQ(12),TR(12),&             
      TRHT(12),TSN(12),TSR(12),TSTL(12),TSY(12),TXMN(12),TXMX(12),TYW(12),&               
      U10MX(12),UAVM(12),W(12),OPV(9),SMGS(8),YSD(8),PSZ(5),SQB(5),&            
      SYB(5),BUS(4),RUSM(3),WX(3),XIM(3),WCS(3),XAV(3),XDV(3),XRG(3),ZCS(3)                                   
      REAL::XSP(43000,5),STV(30,12),DIR(12,16),SCRP(30,2),OBMX(6,12),&
      OBMN(6,12),OBSL(6,12),PCF(6,12),RH(6,12),RMO(6,12),SDTMN(6,12),&
      SDTMX(6,12),WFT(6,12),WI(6,12)
      REAL::CQP(8,17,4),RST(3,6,12),PRW(2,6,12)
      REAL,DIMENSION(:), ALLOCATABLE::ACO2C,AFP,AN2OC,AO2C,CGCO2,CGN2O,&
      CGO2,CLCO2,CLN2O,CLO2,DCO2GEN,DHN,DN2G,DN2OG,DO2CONS,DPRC,DPRN,&
      DPRO,EAR,FC,FE26,HKPC,HKPN,HKPO,RSPC,S15,SMEA,SMES,SOT,TPOR,VFC,&
      VO2,VWC,VWP,WBMC,WCO2G,WCO2L,WN2O,WN2OG,WN2OL,WNO2,WNO3,WO2G,WO2L,&
      XN2O,ZC
      REAL,DIMENSION(:),ALLOCATABLE::ALS,AP,BD,BDD,BDM,BDP,BPC,CAC,CDG,&
      CEC,CEM,CLA,CNDS,CN3F,CPRH,CPRV,DRWX,ECND,EQKE,EQKS,EXCK,FIXK,FOP,&
      HCL,HK,PH,OP,PKRZ,PMN,PO,PSP,RNMN,ROK,RSD,RWTZ,SAN,SATC,SEV,SIL,SMB,&            
      SOLK,SSF,SSFCO2,SSFN2O,SSFO2,ST,STD,STDN,STFR,STMP,SUT,U,UK,UN,UP,&
      VCO2,VN2O,VNO2,VNO3,WBMN,WHPC,WHPN,WHSC,WHSN,WLM,WLMC,WLMN,WLS,WLSC,&
      WLSL,WLSLC,WLSLNC,WLSN,WNH3,WOC,WON,WP,WSLT,WT,Z                                                               
      REAL,DIMENSION(:),ALLOCATABLE::ANA,CAF,CKY,CNY,CPY,CSTS,FNMX,HUF,&             
      PST,PSTM,RDF,SDW,TOPC,TFTN,TFTP,WCY                                            
      REAL,DIMENSION(:),ALLOCATABLE::ACET,AJHI,AJWA,ALT,BLYN,CCEM,CHT,&              
      DDM,DLAI,DM,DMLA,DMLX,DM1,EP,FLT,FTO,GMHU,GSI,HI,HMX,HU,HUI,PPL0,&             
      PRYG,PRYF,PSTF,RBMD,RD,RDMX,REG,RLAD,RW,RWX,SLAI,SMSL,STL,SWH,&                
      SWP,TCAW,TCQV,TCRF,TCSO,TCST,TDM                                               
      REAL,DIMENSION(:),ALLOCATABLE::TETG,TFTK,TBSC,THU,TIRL,TRA,TRD,&               
      TVAL,TVIR,TYLC,TYLK,TYLN,TYLP,TYL1,TYL2,UK1,UK2,ULYN,UNA,UN1,UP1,&
      UN2,UP2,VPD2,VPTH,WA,WAVP,WLV,WSYF,WUB,XDLA0,XDLAI,XLAI,XMTU,YLD                   
      REAL,DIMENSION(:),ALLOCATABLE::COOP,COTL,DKH,DKI,EFM,EMX,FCEM,FCST&            
      ,FK,FN,FNH3,FNO,FOC,FP,FPO,FPOP,FRCP,FSLT,FULU,HE,HMO,ORHI,PCEM,&              
      PCST,PFOL,PHLF,PHLS,PKOC,PLCH,PSOL,PWOF,RHT,RIN,SSPS,TCEM,TLD,RR,&             
      SM,SMY,VAR                                                                     
      REAL,DIMENSION(:,:),ALLOCATABLE::PPCF,PPLP,PSTE,PSTR,PVQ,PVY,RSTK,&
      SMM,PHU,POP,PPLA,PSTZ,RWF,RWPC,TPAC,SMAP,SMYP,TLMA,VARP,SMS,SPQ,&
      SPQC,SPY,SOIL,STDA,BK,BN,BP,BLG,BWD                                                          
      REAL,DIMENSION(:,:),ALLOCATABLE::VARC,RWT,RWTX,FRTK,FRTN,FRTP,TPSF&            
      ,CAW,CQV,CRF,CSOF,CSTF,DLAP,DMF,ETG,FRST,HIF,VIL,VIR,WAC2,YLCF,YLD1,&               
      YLD2,YLKF,YLNF,YLPF,SOL,STX,TSFC,CGSF,SFMO                                     
      REAL,DIMENSION(:,:),ALLOCATABLE::CFRT,CND,HUSC,HWC,QIR,TIR,VIRR,WFA                
      REAL,DIMENSION(:,:,:),ALLOCATABLE::APQ,APQC,APY,AQB,AYB,SMMP,SMMC,&            
      SF                                                                             
	  
      !! Some array sizes (we'll add these one by one as we come across them)
      INTEGER,PARAMETER :: MSC=31
      
      !! These variables were added for MPI by SKANG

      integer :: ierr, tasks, this_rank
      integer, parameter :: main_rank = 0
      integer, parameter :: nruns_per_job=5, nparams_per_run=8


      INTERFACE
          ! SUBROUTINE OPENV (NUM, FNAM, ID, NMSO, ArgWritable)
            SUBROUTINE OPENV (NUM, FNAM, NMSO, ArgWritable)
            INTEGER                       :: NUM, NMSO  ! ID
            CHARACTER (LEN=*), INTENT(IN) :: FNAM
            LOGICAL, OPTIONAL, INTENT(IN) :: ArgWritable
          END SUBROUTINE
      END INTERFACE

END MODULE PARM
