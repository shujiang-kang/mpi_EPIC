!     THIS MODEL IS CALLED EPIC (ENVIRONMENTAL POLICY / INTEGRATED                   
!     CLIMATE). 
!     IT IS A COMPREHENSIVE AGRICULTURAL MANAGEMENT MODEL THAT IS USEFUL             
!     IN SOLVING A VARIETY OF PROBLEMS INVOLVING SUSTAINABLE                         
!     AGRICULTURE, WATER QUALITY, WATER SUPPLY, AND GLOBAL CLIMATE                   
!     & CO2 CHANGE.  THE MODEL SIMULATES MOST COMMONLY USED                          
!     MANAGEMENT PRACTICES LIKE TILLAGE, IRRIGATION, FERTILIZATION,                  
!     PESTICIDE APPLICATION, LIMING, AND FURROW DIKING.                              
!     THE MAIN PROGRAM INITIALIZES VARIABLES, ACCUMULATES                            
!     MONTHLY AND ANNUAL VALUES, AND COMPUTES AVERAGE VALUES FOR THE                 
!     SIMULATION PERIOD. 
!     EPIC1102 COMPLETE 20110201                                                     
#ifdef _WIN32
      INCLUDE 'MODPARM.F90'  
#endif
      PROGRAM MAIN                                                                        
	  USE PARM                                                                            
      CHARACTER(1)::ASG 
      CHARACTER(32)::EPICARG  ! added by SKang                                                              
      CHARACTER(2)::RFPT                                                               
      CHARACTER(4)::ANM
      CHARACTER(4), DIMENSION(150) :: AYR
      CHARACTER(8)::AGIX
      CHARACTER(512):: A1="",A2=""
      CHARACTER(512):: FCMOD="",FCROP="",FFERT="",FMLRN="",FOPSC="",FPARM="",&
	      FPEST="",FPRNT="",FSITE="",FSOIL="",FTILL="",FTR55="",FWIDX="",FWIND="",&
	      FWLST="",FWPM1="",FWPM5="",VOID="",WINDFILE="",WPM1FILE="",SOLS=""

      INTEGER :: SoilInputFmtSelect = 0, SoilOutputFmtSelect = 0
      CHARACTER(32)::SoilInputFormat1, SoilInputFormat2, SoilInputFormat3
      CHARACTER(32)::SoilOutputFormat1, SoilOutputFormat2, SoilOutputFormat3

      CHARACTER(80)::AGSM,CM,TITW5
      INTEGER*8 ISIT,INPS,IOPS                                                  
      REAL*8 PPX                                                                     
      DIMENSION CM(9),ASG(4),TITW5(2)                                             
      DIMENSION NXX(3,5),NYY(3,5),IIX(7),IDIR(4),JZ(4),NY(3)                                
      DIMENSION XZP(13,16),XTP(200),XYP(200),SCRX(30,2),ACO2(30),XTP1(30),&
      XTP2(30),YTP(16),SULF(15),UAV0(12),AKX(3,3),COCH(6),RFPT(4),CSTZ(2)
      DIMENSION PPX(13)                                                              
      
! DManowitz - 2/26/10: For help in comparing output between Windows & Linux, changing some outputs.
      CHARACTER(512):: SHORTFILE="",err_str=""
      INTEGER:: POS,Status
                             
	  DATA RFPT/' 1','1A',' 2',' 3'/,ASG/'A','B','C','D'/                                 
	  DATA AKX/1.0307,-3.0921,2.0614,-.040816,4.1224,-4.0816,.010101,&
	  -1.0303,2.0202/
	  CALL ATIMER(0)
	  CALL AHEAD                                                                          
      IRUN=0
      MSO=31
      DO I=1,SIZE(KW)                                                                          
   	      KW(I)=I+50                                                                     
	  END DO
      CALL OPENV (KW(MSO+6), 'WORKSPACE.DAT', -1)
!  1  IBAT = 0 RUNS FROM EPICRUN.DAT
!          > 0 RUNS BATCH MODE FROM RUNALL.BAT                                   	  
      READ(KW(MSO+6),'(I4)')IBAT
      IF(IBAT>0)THEN
          CALL OPENV (KW(MSO), 'EPICERR.DAT', KW(MSO), .TRUE.)
          I=1
          DO
              READ(KW(MSO),'(A80)',IOSTAT=NFL)TITW5
              IF(NFL/=0)EXIT
              I=I+1
          END DO      	                                              
          IF(I==1)THEN
              REWIND KW(MSO)
              WRITE(KW(MSO),621) VersionStr,TRIM (CommandName),IYER,IMON,IDAY,IT1,IT2,IT3
              WRITE(*,621) VersionStr,TRIM (CommandName),IYER,IMON,IDAY,IT1,IT2,IT3
          END IF
      ELSE
	      !JEFF
          CALL COMMAND_LINE()
      END IF

      CALL OPENV(KR(12),'EPICFILE.DAT',KW(MSO))                                        
      CALL OPENV(KR(21),'EPICCONT.DAT',KW(MSO))                                            
      CALL OPENV(KR(28),'AYEAR.DAT',KW(MSO))
      READ(KR(12),509)FSITE,FWPM1,FWPM5,FWIND,FWIDX,FCROP,FTILL,FPEST,&              
      FFERT,FSOIL,FOPSC,FTR55,FPARM,FMLRN,FPRNT,FCMOD,FWLST                          
!  1  NBYR = NUMBER OF YEARS OF SIMULATION                                           
!  2  IYR0 = BEGINNING YEAR OF SIMULATION                                            
!  3  IMO0 = MONTH SIMULATION BEGINS                                                 
!  4  IDA0 = DAY OF MONTH SIMULATION BEGINS                                          
!  5  IPD  = N1 FOR ANNUAL PRINTOUT                     | N YEAR INTERVA             
!          = N2 FOR ANNUAL WITH SOIL TABLE              | N=0 SAME AS                
!          = N3 FOR MONTHLY                             | N=1 EXCEPT                 
!          = N4 FOR MONTHLY WITH SOIL TABLE             | N=0 PRINTS                 
!          = N5 FOR MONTHLY WITH SOIL TABLE AT HARVEST  | OPERATIONS                 
!          = N6 FOR N DAY INTERVAL                                                   
!          = N7 FOR SOIL TABLE ONLY N DAY INTERVAL                                   
!          = N8 FOR N DAY INTERVAL RAINFALL DAYS ONLY                                
!          = N9 FOR N DAY INTERVAL DURING GROWING SEASON                             
!  6  NGN  = ID NUMBER OF WEATHER VARIABLES INPUT.  RAIN=1,  TEMP=2,                 
!            RAD=3,  WIND SPEED=4,  REL HUM=5.  IF ANY VARIABLES ARE INP             
!            RAIN MUST BE INCLUDED.  THUS, IT IS NOT NECESSARY TO SPECIF             
!            ID=1 UNLESS RAIN IS THE ONLY INPUT VARIABLE.                            
!            LEAVE BLANK IF ALL VARIABLES ARE GENERATED.  EXAMPLES                   
!            NGN=1 INPUTS RAIN.                                                      
!            NGN=23 INPUTS RAIN, TEMP, AND RAD.                                      
!            NGN=2345 INPUTS ALL 5 VARIABLES.                                        
!  7  IGN  = NUMBER TIMES RANDOM NUMBER GEN CYCLES BEFORE SIMULATION                 
!            STARTS.                                                                 
!  8  IGS0 = 0 FOR NORMAL OPERATION OF WEATHER MODEL.                                
!          = N NO YRS INPUT WEATHER BEFORE REWINDING (USED FOR REAL                  
!            TIME SIMULATION).                                                       
!  9  LPYR = 0 IF LEAP YEAR IS CONSIDERED                                            
!          = 1 IF LEAP YEAR IS IGNORED                                               
! 10  IET  = PET METHOD CODE                                                         
!          = 1 FOR PENMAN-MONTEITH                                                   
!          = 2 FOR PENMAN                                                            
!          = 3 FOR PRIESTLEY-TAYLOR                                                  
!          = 4 FOR HARGREAVES                                                        
!          = 5 FOR BAIER-ROBERTSON                                                   
! 11  ISCN = 0 FOR STOCHASTIC CURVE NUMBER ESTIMATOR.                                
!          > 0 FOR RIGID CURVE NUMBER ESTIMATOR.                                     
! 12  ITYP = 0 FOR MODIFIED RATIONAL EQ PEAK RATE ESTIMATE.                          
!          > 0 FOR SCS TR55 PEAK RATE ESTIMATE.                                      
!          = 1 FOR TYPE 1 RAINFALL PATTERN                                           
!          = 2     TYPE 1A                                                           
!          = 3     TYPE 2                                                            
!          = 4     TYPE 3                                                            
! 13  ISTA = 0 FOR NORMAL EROSION OF SOIL PROFILE                                    
!          = 1 FOR STATIC SOIL PROFILE                                               
! 14  IHUS = 0 FOR NORMAL OPERATION                                                  
!          = 1 FOR AUTOMATIC HEAT UNIT SCHEDULE(PHU MUST BE INPUT AT                 
!            PLANTING)                                                               
! 15  NDUM = NOT USED (was # cows)                                                                
! 16  NVCN = 0 VARIABLE DAILY CN WITH DEPTH SOIL WATER WEIGHTING                     
!          = 1 VARIABLE DAILY CN WITHOUT DEPTH WEIGHTING                             
!          = 2 VARIABLE DAILY CN LINEAR CN/SW NO DEPTH WEIGHTING                     
!          = 3 NON-VARYING CN--CN2 USED FOR ALL STORMS                               
!          = 4 VARIABLE DAILY CN SMI(SOIL MOISTURE INDEX)                            
! 17  INFL = 0 FOR CN ESTIMATE OF Q                                                  
!          = 1 FOR GREEN & AMPT ESTIMATE OF Q, RF EXP DST, PEAK RF RATE              
!              SIMULATED.                                                            
!          = 2 FOR G&A Q, RF EXP DST, PEAK RF INPUT                                  
!          = 3 FOR G&A Q, RF UNIFORMLY DST, PEAK RF INPUT                            
! 18  MASP < 0 FOR MASS ONLY NO PESTICIDE IN .OUT                                    
!          = 0 FOR MASS ONLY PESTICIDES IN .OUT                                      
!          > 0 FOR PESTICIDE & NUTRIENT OUTPUT IN MASS & CONCENTRATION               
! 19  LBP  = 0 FOR SOL P RUNOFF ESTIMATE USING GLEAMS PESTICIDE APPROACH             
!          > 0 FOR MODIFIED NONLINEAR APPROACH                                       
! 20  NSTP = REAL TIME DAY OF YEAR                                                   
! 21  IGMX = # TIMES GENERATOR SEEDS ARE INITIALIZED FOR A SITE.                     
! 22  IERT = 0 FOR EPIC ENRICHMENT RATIO METHOD                                      
!          = 1 FOR GLEAMS ENRICHMENT RATIO METHOD                                    
! 23  ICG  = CROP GROWTH BIOMASS CONVERSION OPTION                                   
!          = 0 FOR TRADITIONAL EPIC RADIATION TO BIOMASS                             
!          > 0 FOR NEW EXPERIMENTAL WATER USE TO BIOMASS                             
! 24  LMS  = 0 APPLIES LIME                                                          
!          = 1 DOES NOT APPLY LIME                                                   
! 25  ICF  = 0 USES RUSLE C FACTOR FOR ALL EROSION EQS                               
!          > 0 USES EPIC C FACTOR FOR ALL EROSION EQS EXCEPT RUSLE                   
! 26  ISW  = 0 FIELD CAP/WILTING PT EST RAWLS METHOD DYNAMIC.                        
!          = 1 FIELD CAP/WILTING PT EST BAUMER METHOD DYNAMIC.                       
!          = 2 FIELD CAP/WILTING PT INP RAWLS METHOD DYNAMIC.                        
!          = 3 FIELD CAP/WILTING PT INP BAUMER METHOD DYNAMIC.                       
!          = 4 FIELD CAP/WILTING PT EST RAWLS METHOD STATIC.                         
!          = 5 FIELD CAP/WILTING PT EST BAUMER METHOD STATIC.                        
!          = 6 FIELD CAP/WILTING PT INP STATIC.                                      
!          = 7 FIELD CAP/WILTING PT NEAREST NEIGHBOR DYNAMIC                         
!          = 8 FIELD CAP/WILTING PT NEAREST NEIGHBOR STATIC                          
! 27  IRW  = 0 FOR NORMAL RUNS WITH DAILY WEATHER INPUT                              
!          > 0 FOR CONTINUOUS DAILY WEATHER FROM RUN TO RUN(NO REWIND)               
! 28  ICO2 = 0 FOR CONSTANT ATMOSPHERIC CO2                                          
!          = 1 FOR DYNAMIC ATMOSPHERIC CO2                                           
!          = 2 FOR INPUTTING CO2                                                     
! 29  IDUM = 0 FOR READING DATA FROM WORKING DIRECTORY                               
!          > 0 FOR READING FROM WEATDATA DIRECTORY                                   
! 30  ICOR = 0 FOR NORMAL RUN                                                        
!          = DAY OF YEAR WHEN WEATHER CORRECTION TO SIMULATE INPUT MO MEANS          
!            STOPS                                                                   
! 31  IDN  = 1 FOR ORIGINAL EPIC DENITRIFICATION SUBPROGRAM 
!          = 2 FOR ARMEN KEMANIAN DENITRIFICATION SUBPROGRAM
!          = 3 FOR CESAR IZAURRALDE DENITRIFICATION SUBPROGRAM (ORIGINAL DW)
!          = 4 FOR CESAR IZAURRALDE DENITRIFICATION SUBPROGRAM (NEW DW)                         
! 32  NUPC = N AND P PLANT UPTAKE CONCENTRATION CODE                                 
!          = 0 FOR SMITH CURVE                                                       
!          > 0 FOR S CURVE                                                           
! 33  IOX  = 0 FOR ORIGINAL EPIC OXYGEN/DEPTH FUNCTION                               
!          > 0 FOR ARMEN KEMANIAN CARBON/CLAY FUNCTION
! 34  IDI0 = 0 FOR READING DATA FROM WORKING DIRECTORY
!          = 1 FOR READING FROM WEATDATA DIRECTORY
!          = 2 FOR READING FROM WORKING DIRECTORY PLUS 3 OTHER DIRECTORIES
! 35  ISAT = 0 FOR READING SATURATED CONDUCTIVITY IN SOIL FILE
!          > 0 FOR COMPUTING SATURATED CONDUCTIVITY WITH RAWLS METHOD
! 36  IAZM = 0 FOR USING INPUT LATITUDES FOR SUBAREAS
!          > 0 FOR COMPUTING EQUIVALENT LATITUDE BASED ON AZIMUTH 
!            ORIENTATION OF LAND SLOPE.
! 37  IPAT = 0 TURNS OFF AUTO P APPLICATION
!          > 0 FOR AUTO P APPLICATION
! 38  IEVI = 0 PAR DRIVEN BY CROP LAI DEVELOPMENT
!          > 0 PAR DRIVEN BY EVI FROM REMOTE SENSING
!     LINE 1/2                                                                       
      READ(KR(21),300)NBYR,IYR0,IMO0,IDA0,IPD,NGN,IGN,IGS0,LPYR,IET,ISCN&            
      ,ITYP,ISTA,IHUS,NDUM,NVCN,INFL0,MASP,LBP,NSTP,IGMX,IERT,ICG,LMS,&              
      ICF,ISW,IRW,ICO2,IDUM,ICOR,IDN,NUPC,IOX,IDI0,ISAT,IAZM,IPAT,IEVI                                        
      IF(IDN==0)IDN=2                                        
      MYR=NBYR    
                                                                         
      CALL OPENV(KR(6),FMLRN,KW(MSO))                                                           
      CALL OPENV(KR(2),FPARM,KW(MSO))                                                           
      CALL OPENV(KR(5),FPRNT,KW(MSO))                                                      
      CALL OPENV(KR(26),FCMOD,KW(MSO))                                                          
      CALL OPENV(KR(3),FTILL,KW(MSO))                                                     
      CALL OPENV(KR(4),FCROP,KW(MSO))                                                     
      CALL OPENV(KR(8),FPEST,KW(MSO))                                                     
      CALL OPENV(KR(9),FFERT,KW(MSO))                                                
      CALL OPENV(KR(10),FTR55,KW(MSO))                                               
      CALL OPENV(KR(13),FSOIL,KW(MSO))                                               
      CALL OPENV(KR(15),FOPSC,KW(MSO))                                               
      CALL OPENV(KR(17),FWPM1,KW(MSO))                                                    
      CALL OPENV(KR(18),FWPM5,KW(MSO))                                               
      CALL OPENV(KR(19),FWIND,KW(MSO))                                                    
      CALL OPENV(KR(20),FWIDX,KW(MSO))                                               
      CALL OPENV(KR(23),FSITE,KW(MSO))                                                    
      CALL OPENV(KR(27),FWLST,KW(MSO))
      IF(IBAT>0)THEN
         CALL GETCL(CM,KW)
         ASTN=CM(2)
      ELSE
          CALL GETARG(1,EPICARG)
          CALL OPENV(KR(11),EPICARG,KW(MSO))                                             
      END IF		                                                      
!     SCRP = S CURVE SHAPE PARAMETERS (CONSTANTS EXCEPT FOR                          
!            EXPERIMENTAL PURPOSES)                                                  
!     LINE 1/30                                                                      
      READ(KR(2),239)((SCRP(I,J),J=1,2),I=1,30)                                      
!     MISCELLANEOUS PARAMETERS(CONSTANTS EXCEPT FOR EXPERIMENTAL PURPOSES            
!     LINE 31/38                                                                     
      READ(KR(2),303)PRMT                                                            
!     READ ECONOMIC DATA                                                             
!  1  COIR = COST OF IRRIGATION WATER ($/mm)                                         
!  2  COL  = COST OF LIME ($/t)                                                      
!  3  FULP = COST OF FUEL ($/L)                                                      
!  4  WAGE = LABOR COST ($/H)                                                        
!  5  CSTZ = MISCELLANEOUS COST ($/ha)                                               
!  6  1/2                                                                            
!     LINE 39                                                                        
      READ(KR(2),303)COIR,COL,FULP,WAGE,CSTZ(1),CSTZ(2)                              
!     LINE 40      
      READ(KR(2),303)XKN50,XKN30,XKN10,CBVT0                              
      CLOSE(KR(2))                                                                   
      DO I=1,2                                                                       
          DO J=1,30                                                                    
              SCRX(J,I)=SCRP(J,I)                                                      
          END DO                                                                       
	  END DO                                                                              
      DO I=1,29                                                                      
          IF(SCRP(I,1)<1.E-10)CYCLE                                                    
          X1=ASPLT(SCRP(I,1))                                                          
          X2=ASPLT(SCRP(I,2))                                                          
          CALL ASCRV(SCRP(I,1),SCRP(I,2),X1,X2)                                        
	  END DO                                                                              
!     CQP  = COEFS OF 7TH DEG POLY IN TR55 QP EST                                    
      READ(KR(10),396)CQP                                                            
!     COCH = COEFS FOR CHANNEL GEOMETRY X=COCH(N)*WSA**COCH(N+1)                     
!            X=DEPTH FOR COCH(3) & COCH(4)                                           
!            X=LENGTH FOR COCH(5) & COCH(6)                                          
      READ(KR(10),303)COCH                                                           
      CLOSE(KR(10))                                                                  
      IF(COCH(3)<1.E-10)COCH(3)=.0208                                                
      IF(COCH(4)<1.E-10)COCH(4)=.4                                                   
      IF(COCH(5)<1.E-10)COCH(5)=.0803                                                
      IF(COCH(6)<1.E-10)COCH(6)=.6 
                                                  
      READ(KR(28),*,IOSTAT=NFL)AYR                                                        
      IF(NFL/=0)REWIND KR(28)                                                             
      READ(KR(28),*,IOSTAT=NFL)IGY                                                        
      CLOSE(KR(28))

!     LINE 1/5                                                                       
      IF(NFL/=0)READ(KR(5),300)(KDC1(I),I=1,NKA)                                          
!     KA   = OUTPUT VARIABLE ID NO (ACCUMULATED AND AVERAGE VALUES)                  
!     SELECT FROM THIS LIST:(UP TO 100 VARIABLES)                                     
!      1  TMX,  2  TMN,  3  RAD,  4 PRCP,  5 SNOF,  6 SNOM,  7 WSPD,                 
!      8 RHUM,  9  VPD, 10  PET, 11   ET, 12   EP, 13    Q, 14   CN,                 
!     15  SSF, 16  PRK, 17 QDRN, 18 IRGA, 19  QIN, 20 TLGE, 21 TLGW,                 
!     22 TLGQ, 23 TLGF, 24   EI, 25    C, 26 USLE, 27 MUSL, 28  AOF,                 
!     29 MUSS, 30 MUST, 31 MUSI, 32  WK1, 33 RHTT, 34 RRUF, 35 RGRF,                 
!     36   YW, 37  YON, 41  MNN, 42   DN, 43 NFIX, 44  HMN, 45 NITR,                 
!     46 AVOL, 47 DRNN, 48   YP, 50  MNP, 51 PRKP, 52   ER, 53  FNO,                 
!     54 FNO3, 55 FNH3, 56  FPO, 57  FPL, 58 LIME, 59  TMP, 66 SW10                  
      DO I=1,NKA                                                                     
          IF(KDC1(I)<=0)EXIT                                                           
          KA(I)=KDC1(I)                                                                
	  END DO                                                                              
      NKA=I-1                                                                        
!     JC = OUTPUT VARIABLE ID NO (CONCENTRATION VARIABLES)                           
!     SELECT FROM THIS LIST: (UP TO 4 VARIABLES)                                     
!     38 QNO3, 39 SSFN, 40 PRKN, 49  QAP                                             
!     LINE 6                                                                         
      READ(KR(5),300)(KDC1(I),I=1,NJC)                                               
      DO I=1,NJC                                                                     
          IF(KDC1(I)<=0)EXIT                                                           
          JC(I)=KDC1(I)                                                                
	  END DO                                                                              
      NJC=I-1                                                                        
!     KS = OUTPUT VARIABLE ID NO (MO STATE VARIABLES)                                
!     SELECT FROM THIS LIST: (UP TO 27 VARIABLES)                                    
!     1 TNH3, 2 TNO3, 3 PLAB, 4 UNO3, 5  UPP, 6 RZSW, 7 WTBL, 8 GWST,                
!     9  STD, 10 RSD, 11 TOC, 12 SNOA                                                
!     LINE 7/8                                                                       
      READ(KR(5),300)(KDC1(I),I=1,NKS)                                               
      DO I=1,NKS                                                                     
          IF(KDC1(I)<=0)EXIT                                                           
          KS(I)=KDC1(I)                                                                
	  END DO                                                                              
      NKS=I-1                                                                        
!     KD = DAILY OUTPUT VARIABLE ID NO                                               
!     SELECT FROM ACCUMULATED AND AVERAGE LIST (UP TO 40 VARIABLES)                  
!     LINE 9/10                                                                      
      READ(KR(5),300)(KDC1(I),I=1,NKD)                                               
      DO I=1,NKD                                                                     
          IF(KDC1(I)<=0)EXIT                                                           
          KD(I)=KDC1(I)                                                                
      END DO                                                                         
      NKD=I-1                                                                        
!     KYA = ANNUAL OUTPUT TO FILE VARIABLE ID NOS(ACCUMULATED                        
!     AND AVERAGE VALUES)                                                            
!     SELECT FROM THE KA LIST ABOVE: (UP TO 40 VARIABLES)                            
!     LINE 11/12                                                                      
      READ(KR(5),300)(KDC1(I),I=1,NKYA)                                              
      DO I=1,NKYA                                                                    
          IF(KDC1(I)<=0)EXIT                                                           
          KYA(I)=KDC1(I)                                                               
      END DO                                                                         
      NKYA=I-1                                                                       
!     KFS = MONTHLY FLIPSIM VARIABLES.  SELECT FROM THE ACCUMULATED AND              
!     AVERAGE LIST ABOVE (UP TO 40 VARIABLES)                                        
!     LINE 13/14                                                                     
      READ(KR(5),300)(KDC1(I),I=1,NFS)                                               
      DO I=1,NFS                                                                     
          IF(KDC1(I)<=0)EXIT                                                           
          IFS(I)=KDC1(I)                                                               
      END DO                                                                         
      NFS=I-1                                                                        
	  NGN0=NGN                                                                            
	  ICOR0=ICOR                                                                          
!  1  RFN0 = AVE CONC OF N IN RAINFALL (ppm)                                         
!  2  CO20 = CO2 CONCENTRATION IN ATMOSPHERE (ppm)                                   
!  3  CNO30= CONC OF NO3 IN IRRIGATION WATER (ppm)                                   
!  4  CSLT=  CONC OF SALT IN IRRIGATION WATER (ppm)                                  
!  5  PSTX = PEST DAMAGE SCALING FACTOR (0.-10.)--0. SHUTS OFF PEST                  
!            DAMAGE FUNCTION. PEST DAMAGE FUNCTION CAN BE REGULATED FROM             
!            VERY MILD(0.05-0.1) TO VERY SEVERE(1.-10.)                              
!  6  YWI  = NO Y RECORD MAX .5H RAIN (BLANK IF WI IS NOT                            
!            INPUT--LINE 19)                                                         
!  7  BTA  = COEF (0-1)GOVERNING WET-DRY PROBABILITIES GIVEN DAYS                    
!            OF RAIN (BLANK IF UNKNOWN OR IF W|D PROBS ARE                           
!            INPUT--LINES 16 & 17)                                                   
!  8  EXPK = PARAMETER USED TO MODIFY EXPONENTIAL RAINFALL AMOUNT                    
!            DISTRIBUTION (BLANK IF UNKNOWN OR IF ST DEV & SK CF ARE                 
!            INPUT--LINES 14 & 15)                                                   
!  9  FL   = FIELD LENGTH (km)(BLANK IF UNKNOWN                                      
! 10  FW   = FIELD WIDTH (km)(BLANK IF UNKNOWN                                       
! 11  ANG0 = CLOCKWISE ANGLE OF FIELD LENGTH FROM NORTH (deg)(BLANK IF               
!            UNKNOWN)                                                                
! 12  STD0 = STANDING DEAD CROP RESIDUE (t/ha)(BLANK IF UNKNOWN                      
! 13  UXP  = POWER PARAMETER OF MODIFIED EXP DIST OF WIND SPEED (BLANK               
!            IF UNKNOWN)                                                             
! 14  DIAM = SOIL PARTICLE DIAMETER (um)(BLANK IF UNKNOWN                            
! 15  ACW  = WIND EROSION CONTROL FACTOR                                             
!          = 0.0 NO WIND EROSION                                                     
!          = 1.0 FOR NORMAL SIMULATION                                               
!          > 1.0 ACCELERATES WIND EROSION (CONDENSES TIME)                           
! 16  BIR  = IRRIGATION TRIGGER--3 OPTIONS                                           
!            1. PLANT WATER STRESS FACTOR (0-1)                                      
!            2. SOIL WATER TENSION IN TOP 200 mm(> 1 kpa)                            
!            3. PLANT AVAILABLE WATER DEFICIT IN ROOT ZONE (-mm)                     
! 17  EFI  = RUNOFF VOL / VOL IRR WATER APPLIED (BLANK IF IRR=0)                     
! 18  VIMX = MAXIMUM ANNUAL IRRIGATION VOLUME ALLOWED (mm)                           
! 19  ARMN = MINIMUM SINGLE APPLICATION VOLUME ALLOWED (mm)                          
! 20  ARMX = MAXIMUM SINGLE APPLICATION VOLUME ALLOWED (mm)                          
! 21  BFT0 = AUTO FERTILIZER TRIGGER--2 OPTIONS                                      
!            1. PLANT N STRESS FACTOR (0-1)                                          
!            2. SOIL N CONC IN ROOT ZONE (g/t)                                       
! 22  FNP  = FERT APPLICATION VARIABLE--2 MEANINGS                                   
!            1. APPLICATION RATE AUTO/FIXED (kg/ha)                                  
!            2. MANURE INPUT TO LAGOON (KG/COW/D) IRR=4                              
! 23  FMX  = MAXIMUM ANNUAL N FERTILIZER APPLICATION FOR A CROP(kg/ha)               
! 24  DRT  = TIME REQUIRED FOR DRAINAGE SYSTEM TO REDUCE PLANT STRESS(d)             
!            (BLANK IF DRAINAGE NOT USED)                                            
! 25  FDS0 = FURROW DIKE SAFETY FACTOR (0-1.)                                        
! 26  PEC0 = CONSERVATION PRACTICE FACTOR(=0.0 ELIMINATES WATER EROSION)             
! 27  VLGN = LAGOON VOLUME RATIO--NORMAL / MAXIMUM                                   
! 28  COWW = LAGOON INPUT FROM WASH WATER (M**3/COW/D)                               
! 29  DDLG = TIME TO REDUCE LAGOON STORAGE FROM MAX TO NORM (d)                      
! 30  SOLQ = RATIO LIQUID/TOTAL MANURE APPLIED                                       
! 31  GZLM = ABOVE GROUND PLANT MATERIAL GRAZING LIMIT (t/ha)                        
! 32  FFED = FRACTION OF TIME HERD IS IN FEEDING AREA
! 33  DZ   = LAYER THICKNESS FOR DIFFERENTIAL EQ SOLN TO GAS DIFF EQS(m)                                
! 34  DRV  = SPECIFIES WATER EROSION DRIVING EQ.                                     
!            (0=MUST;  1=AOF;  2=USLE;  3=MUSS;  4=MUSL;  5=MUSI;                    
!             6=RUSLE;  7=RUSL2)
! 35  RST0 = BASE STOCKING RATE (ha/hd)                                                     
! 36  BUS  = INPUT PARMS FOR MUSI                                                    
!            YSD(6)=BUS(1)*QD**BUS(2)*QP**BUS(3)*WSA**BUS(4)*KCPLS                   
!     LINE 3/6                                                                       
      READ(KR(21),303)RFN0,CO20,CNO30,CSLT,PSTX,YWI,BTA,EXPK,FL,FW,ANG0,&            
      STD0,UXP,DIAM,ACW,BIR,EFI,VIMX,ARMN,ARMX,BFT0,FNP,FMX,DRT,FDS0,&               
      PEC0,VLGN,COWW,DDLG,SOLQ,GZLM,FFED,DZ,DRV,RST0,BUS
      BUS0=BUS(1)
      IF(RST0<1.E-10)RST0=5.                                                                    
	  ANG=ANG0/CLT
	  IF(DZ<1.E-10)DZ=.1                                                                        
!     READ ECONOMIC DATA                                                             
!  1  COIR = COST OF IRRIGATION WATER ($/m3)                                       
!  2  COL  = COST OF LIME ($/t)                                                      
!  3  FULP = COST OF FUEL ($/gal)                                                    
!  4  WAGE = LABOR COST ($/h)                                                        
!  5  CSTZ = MISCELLANEOUS COST ($/ha)                                               
!  6  1/2                                                                            
!     LINE 7                                                                         
      READ(KR(21),303)(XTP(I),I=1,6)

      ! LINE 8
      ! DManowitz - 2/6/12: Add ability to specify soil input and output formats
      ! 1 specifies new format.  Otherwise, use old format.
      READ(KR(21), *, IOSTAT = NFL) SoilInputFmtSelect, SoilOutputFmtSelect
      ! Assume any error reading should just set these values to old formats.
      IF (NFL /= 0) THEN
         SoilInputFmtSelect = 0
         SoilOutputFmtSelect = 0
      END IF
   
      IF(XTP(1)>0.)COIR=XTP(1)                                                       
      IF(XTP(2)>0.)COL=XTP(2)                                                        
      IF(XTP(3)>0.)FULP=XTP(3)                                                       
      IF(XTP(4)>0.)WAGE=XTP(4)                                                       
      IF(XTP(5)>0.)CSTZ(1)=XTP(5)                                                    
      IF(XTP(6)>0.)CSTZ(2)=XTP(6)                                                    
      IF(ARMX<1.E-10)ARMX=1000.                                                      
      IF(FMX<1.E-10)FMX=200.                                                         
      IF(GZLM<.01)GZLM=.01                                                           
      IF(UXP<1.E-10)UXP=.5                                                           
      IF(DIAM<1.E-10)DIAM=500.                                                       
      USTRT=.0161*SQRT(DIAM)                                                         
      USTT=USTRT*USTRT                                                               
	  DO I=1,3                                                                            
          PSZ(I)=.411*PSZ(I)*PSZ(I)                                                    
      END DO                                                                         
      IF(ISW>6)THEN                                                                                                            
	      CALL OPENV(KR(29),'SOIL38K.DAT',KW(MSO))                                          
	      READ(KR(29),528)XAV,XDV,XRG,BRNG,NSX                                              
	      NSNN=.655*NSX**.493                                                               
	      EXNN=.767*NSX**.049                                                               
          DO I=1,NSX                                                                   
	          READ(KR(29),528)(XSP(I,J),J=1,5)                                              
	          NX(I)=I                                                                       
	      END DO                                                                            
	      CLOSE(KR(29))                                                                     
	  END IF                                                                              
	  CALL ALLOCATE_PARMS                                                                 
	  HED=(/" TMX"," TMN"," RAD","PRCP","SNOF","SNOM","WSPD","RHUM",&                     
      " VPD"," PET","  ET"," PEP","  EP","   Q","  CN"," SSF"," PRK",&               
      "QDRN","IRGA"," QIN","TLGE","TLGW","TLGQ","TLGF","LGIR","LGMI",&               
      "LGMO","  EI"," CVF","USLE","MUSL"," AOF","MUSS","MUST","RUS2",&               
      "RUSL","RUSC"," WK1","RHTT","RRUF","RGRF","  YW"," YON","QNO3",&               
      "SNO3","VNO3"," NMN"," GMN","  DN","NFIX","NITR","AVOL","DRNN",&               
      "  YP"," QAP"," MNP","PRKP","  ER"," FNO","FNO3","FNH3"," FPO",&               
      " FPL"," FSK"," FCO","LIME"," TMP","SW10","SLTI","SLTQ","SLTS",&               
      "SLTF","RSDC","RSPC","CLCH"," CQV"," YOC","YEFK"," QSK"," SSK",&               
      " VSK","SLTV","MUSI","IRDL"," HMN","RNAD","NIMO","FALF"," DN2",&               
      "RLSF"," REK","FULU","DN2O"," FO2","FCO2","CFEM","BURC","BURN",&
      "NPPC"," NEE","FN2O","SNO2","SN2O","VN2O","VNO2","QNO2","QN2O"/)                                    
!	  CALL DATE_AND_TIME(AYMD,AHMS)                                                      
      IF(NSTP>0) CALL OPENV (KW(MSO+3), 'RTSOIL.DAT', KW(MSO), .TRUE.)                                         
!     SELECT OUTPUT FILES--KFL=0(NO OUTPUT); KFL>0(GIVES OUTPUT FOR                 
!     SELECTED FILE)                                                                 
!     1  OUT = STANDARD OUTPUT                                                       
!     2  ACM = ANNUAL CROPMAN                                                        
!     3  SUM = AVE ANNUAL SUMMARY                                                    
!     4  DHY = DAILY HYDROLOGY                                                       
!     5  DPS = DAILY PESTICIDE                                                       
!     6  MFS = MONTHLY FLIPSIM                                                       
!     7  MPS = MONTHLY PESTICIDE                                                     
!     8  ANN = ANNUAL                                                                
!     9  SOT = ENDING SOIL TABLE                                                     
!    10  DTP = DAILY SOIL TEMPERATURE                                                
!    11  MCM = MONTHLY CROPMAN                                                       
!    12  DCS = DAILY CROP STRESS                                                     
!    13  SCO = SUMMARY OPERATION COST                                                
!    14  ACN = ANNUAL SOIL ORGANIC C & N TABLE                                       
!    15  DCN = DAILY SOIL ORGANIC C & N TABLE                                        
!    16  SCN = SUMMARY SOIL ORGANIC C & N TABLE                                      
!    17  DGN = DAILY GENERAL OUTPUT                                                  
!    18  DWT = DAILY SOIL WATER IN CONTROL SECTION AND .5M SOIL T                    
!    19  ACY = ANNUAL CROP YIELD                                                     
!    20  ACO = ANNUAL COST                                                           
!    21  DSL = DAILY SOIL TABLE.                                                     
!    22  MWC = MONTHLY WATER CYCLE + N CYCLE                                         
!    23  ABR = ANNUAL BIOMASS ROOT WEIGHT                                            
!    24  ATG = ANNUAL TREE GROWTH.                                                   
!    25  MSW = MONTHLY OUTPUT TO SWAT.                                               
!    26  APS = ANNUAL PESTICIDE                                                      
!    27  DWC = DAILY WATER CYCLE                                                     
!    28  DHS = DAILY HYDROLOGY/SOIL                                                  
!    29  DGZ = DAILY GRAZING FILE
!    30  DNC = DAILY NITROGEN/CARBON CESAR IZAURRALDE                                                    
!   MSO  ERX = ERROR FILE                                                            
!  MSO+1 RUN1102.SUM = AVE ANNUAL SUMMARY FILE FOR ALL SIMULATIONS IN A              
!        BATCH.                                                                      
!  MSO+2     = RTCROP.DAT                                                            
!  MSO+3     = RTSOIL.DAT                                                            
!  MSO+4 SGI = SUMMARY GIS FILE                                                      
!  MSO+5 ANNUAL FILES FOR GIS                                                        
!     LINE 15/16                                                                     
      READ(KR(5),300)KFL
      IF(KFL(MSO+1)/=0)THEN
          CALL OPENV (KW(MSO+1), 'RUN1102.SUM', KW(MSO), .TRUE.)
          WRITE(KW(MSO+1),621) VersionStr,TRIM (CommandName),IYER,IMON,IDAY,IT1,IT2,IT3                               
          WRITE(KW(MSO+1),693)(HED(KYA(J)),J=1,NKYA)                                   
	  END IF                                                                            
	  IF(NHC(1)==0)THEN                                                                   
	      DO I=1,26                                                                         
  	          NHC(I)=I                                                                    
          END DO                                                                       
      END IF                                                                         
      IF(NDC(1)==0)THEN                                                              
  	      DO I=1,10                                                                       
  	          NDC(I)=I                                                                    
          END DO                                                                       
      END IF                                                                         
      CLOSE(KR(5))                                                                   
      IF(FL<1.E-10)FL=.632                                                           
      IF(FW<1.E-10)FW=.316                                                           
      NDRV=DRV+1.1                                                                   
      IF(YWI<1.E-10)YWI=10.                                                          
      IF(BTA<1.E-10)BTA=.75                                                          
      IF(EXPK<1.E-10)EXPK=1.3                                                        
	  IF(KFL(NGF)>0)THEN
	      DO I=1,150                                                                          
	          IF(IYR0==IGY(I))GO TO 739                                                         
	      END DO                                                                              
          GO TO 519                                                                      
      739 IGYX=I                                                                         
          N1=NGF-1                                                                       
	      AGSM=AYR(I)//"-"//AYR(I+NBYR-1)//".TXT"                                             
	      CALL OPENV (KW(N1), AGSM, KW(MSO), .TRUE.)                                                              
	      IF(KFL(NGF)>=0)THEN
	          DO                                                                                  
	              READ(KW(N1),505,IOSTAT=NFL)WINDFILE                                               
	              IF(NFL/=0)EXIT                                                                    
	          END DO
	      END IF                                                                              
          IGIS=NGF                                                                       
	      DO I=1,NBYR                                                                     
	          AGIX=AYR(IGYX)//".TXT"                                                              
	          IGYX=IGYX+1                                                                         
	          CALL OPENV (KW(IGIS), AGIX, KW(MSO), .TRUE.)
	          IF(KFL(NGF)<0)THEN
	              IGIS=IGIS+1                                                                       
	          ELSE
	              DO
	                  READ(KW(IGIS),505,IOSTAT=NFL)WINDFILE                                         
	                  IF(NFL/=0)EXIT                                                                
	              END DO
              END IF
          END DO
      END IF
    !  1  ASTN = RUN #                                                                   
    !  2  ISIT = SITE #                                                                  
    !  3  IWP1 = WEATHER STA # FROM KR(17) WPM1USEL.DAT                                  
    !  4  IWP5 = WEATHER STA # FROM KR(18) WPM5US.DAT                                  
    !  5  IWND = WIND STA # FROM KR(19) WINDUS.DAT                                     
    !  6  INPS = SOIL # FROM TABLE KR(13)                                                
    !  7  IOPS = OP SCHED # FROM TABLE KR(15)                                            
    !  8  IWTH = DAILY WEATHER STA # FROM KR(27) WDLSTCOM.DAT
  519 DO 740
      IF(IBAT==0)THEN                                                                         
          ! DManowitz - 2/17/12: Reset the XKN parms and CBVT in case they are
          ! not read from the file.  If they had been read on a previous line and
          ! are not reset, they will keep their previous value, rather than
          ! taking on the default values from EPICCONT.
          XKN5 = 0.0; XKN3 = 0.0; XKN1 = 0.0; CBVT = 0.0
          
          ! DManowitz - 2/23/12: It doesn't seem to help that much to allow
          ! reading of XKN values from *BOTH* the PARM file *AND* EPICRUN,
          ! so just read from PARM file.  Change to first commented-out
          ! version to allow reading from EPICRUN file.  However, note that
          ! if this functionality is enabled, but you do *NOT* want to specify
          ! the additional parameters in the EPICRUN file, you must put a
          ! slash ('/') character after the last field in the EPICRUN file,
          ! or the read statement will attempt to read the remainder of the
          ! values from the *NEXT* line!
          READ(KR(11),*,IOSTAT=NFL)ASTN,ISIT,IWP1,IWP5,IWND,INPS,IOPS,IWTH
          ! READ(KR(11),*,IOSTAT=NFL)ASTN,ISIT,IWP1,IWP5,IWND,INPS,IOPS,IWTH,&
          !      XKN5,XKN3,XKN1,CBVT

          IF(NFL/=0)GO TO 219                                                            
          IF(ISIT==0)GO TO 219
      ELSE
          WRITE(KW(MSO+6),'(A80,A8,1X,7A8)')CM
          ASTN=CM(2)
		  REWIND KW(MSO+6)
		  READ(KW(MSO+6),'()')
		  READ(KW(MSO+6),'(88X,7I8)')IIX
		  ISIT=IIX(1)
		  IWP1=IIX(2)
		  IWP5=IIX(3)
		  IWND=IIX(4)
		  INPS=IIX(5)
		  IOPS=IIX(6)
		  IWTH=IIX(7)                                                                     
      END IF

      IF(XKN5<1.E-10)XKN5=XKN50
      IF(XKN3<1.E-10)XKN3=XKN30
      IF(XKN1<1.E-10)XKN1=XKN10
      IF(CBVT<1.E-10)CBVT=CBVT0                                                             
                                         
      WRITE(*,679)TRIM(ASTN),ISIT,IWP1,INPS,IOPS,IWTH
      WRITE(KW(1),679)TRIM(ASTN),ISIT,IWP1,INPS,IOPS,IWTH                                           
      K19=0 
      CALL GETARG(2,OUTDIR) ! Get alternate output directory from command line.  Jeff 2/22/13
      CALL OPENF
      WRITE(KW(1),621) VersionStr,TRIM (CommandName),IYER,IMON,IDAY,IT1,IT2,IT3      

      ! David Manowitz - 9/22/11: With long file names, make sure to trim them
      ! here!
      WRITE(KW(1),508)'FSITE', TRIM(FSITE), 'FWPM1', TRIM(FWPM1), 'FWPM5',&
          TRIM(FWPM5), 'FWIND', TRIM(FWIND), 'FWIDX', TRIM(FWIDX), 'FCROP',&
          TRIM(FCROP), 'FTILL', TRIM(FTILL), 'FPEST', TRIM(FPEST), 'FFERT',&
          TRIM(FFERT), 'FSOIL', TRIM(FSOIL), 'FOPSC', TRIM(FOPSC), 'FTR55',&
          TRIM(FTR55), 'FPARM', TRIM(FPARM), 'FMLRN', TRIM(FMLRN), 'FPRNT',&
          TRIM(FPRNT), 'FCMOD', TRIM(FCMOD), 'FWLST', TRIM(FWLST)
      WPM1FILE=' '                                                                   
      WINDFILE=' '                                                                   
      UAV0=0.                                                                        
      NYD=1                                                                          
      DO I=1,5                                                                       
          KGN(I)=0
      END DO                                                                              
      CALL AINLZ                                                                     
      CALL AINIX                                                                     
	  DO J=1,21                                                                           
          IDG(J)=J
      END DO                                                                         
      NGN=NGN0                                                                       
      IWRT=0                                                                         
      IRO0=1                                                                         
      IRUN=IRUN+1                                                                    
      WRITE(KW(1),'(/1X,A)')'-----VARIABLE NAMES'                                    
      WRITE(KW(1),229)                                                               
      WRITE(KW(1),287)HED                                                            
      WRITE(KW(1),245)                                                               
      DO J=1,30                                                                      
          WRITE(KW(1),242)J,PRMT(J),(SCRX(J,I),I=1,2),(SCRP(J,I),I=1,2)                
      END DO                                                                         
      DO J=31,SIZE(PRMT)                                                                     
          WRITE(KW(1),242)J,PRMT(J)                                                    
      END DO   
      II=-1                                                                      
      DO WHILE(II/=ISIT)                                                                            
          READ(KR(23),*,IOSTAT=NFL)II,SITEFILE                                         
	      IF(NFL/=0)THEN
	          IF(IBAT==0)THEN                                               
	              WRITE(*,*)'SITE NO = ',ISIT,' NOT IN SITE LIST FILE'
#                     if defined(DEBUG) .AND. defined(PAUSE_ENABLE)
	                PAUSE
#                     endif                    
                  STOP 15
              ELSE
                  WRITE(KW(MSO),'(3A,I4,A)')' !!!!! ',TRIM(ASTN),&
                  'SITE NO = ',ISIT,' NOT IN SITE LIST FILE'
                  GO TO 219
              END IF
          END IF
      END DO                                                                              
      REWIND KR(23)                                                                                                               
      CALL OPENV(KR(1),SITEFILE,KW(MSO))                                             
!     TITLE=PROBLEM DESCRIPTION(3 LINES)                                             
!     LINES 1/3                                                                      
      READ(KR(1),299)(TITLE(I),I=1,60)                                               
	  CALL APAGE(0)                                                                       
      INFL=INFL0+1                                                                   
      IF(IRW==0.OR.IRUN==1)IYR=IYR0                                                  
      IBDT=IDA0+100*IMO0+10000*IYR                                                   
      CALL AISPL(IPD,INP)                                                            
      IF(IWP5>0)THEN
          READ(KR(20),'()')                                                           
          READ(KR(20),300)II                                                             
          II=IYR-II                                                                      
          DO I=1,II                                                                      
              READ(KR(20),'()')                                                         
	      END DO
      END IF	                                                                                    
      NOP=1                                                                          
      IF(IPD<=5.AND.INP>0)NOP=0                                                      
      IF(INP==0)INP=1                                                                
      IF(IPD<=5)IPYI=INP                                                             
!  1  YLAT = LATITUDE(deg)                                                           
!  2  XLOG = LONGITUDE(deg)                                                          
!  3  ELEV = ELEVATION OF WATERSHED (m)                                              
!  4  APM  = PEAK RATE - EI ADJUSTMENT FACTOR (BLANK IF UNKNOWN)                     
!  5  CO2X = CO2 CONCENTRATION IN ATMOSPHERE (ppm)--NON ZERO VALUE                   
!            OVERRIDES CO2 INPUT IN EPICCONT.DAT                                     
!  6  CNO3X= CONC OF NO3 IN IRRIGATION WATER (ppm)--NON ZERO VALUE                   
!            OVERRIDES CNO30 INPUT IN EPICCONT.DAT                                   
!  7  RFNX = AVE CONC OF N IN RAINFALL (ppm)                                         
!  8  X1   = DUMMY                                                                   
!  9  X2   = DUMMY                                                                   
! 10  SNO0 = WATER CONTENT OF SNOW ON GROUND AT START OF SIMULATION(mm)
! 11  AZM  = AZIMUTH ORIENTATION OF LAND SLOPE (DEGREES CLOCKWISE FROM NORTH)              
!     LINE 4                                                                         
      READ(KR(1),304)YLAT,XLOG,ELEV,APM,CO2X,CNO3X,RFNX,X1,X2,SNO0,AZM                   
!  1  WSA  = WATERSHED AREA(HA)                                                      
!  2  CHL  = MAINSTEM CHANNEL LENGTH (km)(BLANK IF UNKNOWN                           
!  3  CHS  = MAINSTEM CHANNEL SLOPE (m/m)(BLANK IF UNKNOWN                           
!  4  CHD  = CHANNEL DEPTH (m)                                                       
!  5  CHN  = MANNINGS N FOR CHANNEL (BLANK IF UNKNOWN)                               
!  6  SN   = SURFACE N VALUE (BLANK IF UNKNOWN)                                      
!  7  UPSL = UPLAND SLOPE LENGTH (m)                                                 
!  8  UPS  = UPLAND SLOPE STEEPNESS (m/m)                                            
!  9  PEC  = CONSERVATION PRACTICE FACTOR(=0.0 ELIMINATES WATER EROSION)
! 10  DTG  = TIME INTERVAL FOR GAS DIFF EQS (H)             
!     LINE 5                                                                         
      READ(KR(1),304)WSA,CHL,CHS,CHD,CHN,SN,UPSL,UPS,PEC,DTG                             
	  IF(PEC<1.E-10)PEC=PEC0                                                              
!     READ MANAGEMENT INFORMATION                                                    
!  1  IRR  = N0 FOR DRYLAND AREAS          | N = 0 APPLIES MINIMUM OF                
!          = N1 FROM SPRINKLER IRRIGATION  | VOLUME INPUT, ARMX, & FC-SW             
!          = N2 FOR FURROW IRRIGATION      | N = 1 APPLIES INPUT VOLUME              
!          = N3 FOR IRR WITH FERT ADDED    | OR ARMX                                 
!          = N4 FOR IRR FROM LAGOON        |                                         
!          = N5 FOR DRIP IRR                                                         
!  2  IRI  = N DAY APPLICATION INTERVAL FOR AUTOMATIC IRRIGATION                     
!  3  IFA  = MIN FERT APPL INTERVAL(BLANK FOR USER SPECIFIED)                        
!  4  IFD  = 0 WITHOUT FURROW DIKES                                                  
!            1 WITH FURROW DIKES                                                     
!  5  IDR0 = 0 NO DRAINAGE                                                           
!          = DEPTH OF DRAINAGE SYSTEM(mm)                                            
!  6  IDF0 = FERT # FOR AUTO N FERT & FERTIGATION--BLANK DEFAULTS TO                   
!            ELEMENTAL N
!  7  MNU  = > 0 AUTO DRY MANURE APPL WITHOUT TRIGGER                                
!  8  IMW  = MIN INTERVAL BETWEEN AUTO MOW                                           
!  9  IDFP = FERT # FOR AUTO P FERT--BLANK DEFAULTS TO ELEMENTAL P
!     LINE 6                                                                         
      READ(KR(1),300)IRR,IRI,IFA,IFD,IDR0,IDF0,MNU,IMW,IDFP                               
      CALL AISPL(IRR,IAC)                                                            
	  CO2=CO20                                                                            
      IF(CO2X>0.)CO2=CO2X                                                            
	  CNO3I=CNO30                                                                         
	  IF(CNO3X>0.)CNO3I=CNO3X                                                             
      CNO3I=CNO3I*.01                                                                
      RFNC=RFN0                                                                      
	  IF(RFNX>0.)RFNC=RFNX                                                                
	  RFNC=RFNC*.01                                                                       
      IF(CHN<1.E-10)CHN=.05                                                          
      IF(SN<1.E-10)SN=.15                                                            
      IF(APM<1.E-10)APM=1.
      IF(DTG<1.E-10)DTG=1.
      NBDT=24./DTG+.9 
      XX=YLAT/CLT                                                          
	  SIN1=SIN(XX)
      COS1=COS(XX)
      YLTS=SIN1
      YLTC=COS1
      YTN=TAN(XX)
      YTN1=YTN
      WSX=1.+WSA                                                                     
      YSW=.79*WSX**.009                                                              
      WSA1=1.586*WSX**.12                                                            
      PMX=PRMT(16)                                                                   
	  CFMN=PRMT(32)                                                                       
      SX=SQRT(UPS)                                                                   
      UPSQ=(1.-2.*EXP(-13.86*UPS))/3.                                                
      IF(NGN>0)THEN
          IF(IRW==0.OR.IRUN==1)THEN
              CALL WDOP(IDIR(1))                                                        
              IYR=IYR0                                                                       
          END IF
      END IF
      CALL ALPYR(IYR,NYD,LPYR)
      PB=101.3-ELEV*(.01152-5.44E-7*ELEV)
      IF(YLAT>0.)THEN
          MOFX=12
      ELSE
          MOFX=13
      END IF
      GMA=6.595E-4*PB
      CH=.4349*ABS(YTN)
      IF(CH>=1.)THEN
          H=0.
      ELSE
          H=ACOS(CH)
      END IF
      HLMN=7.72*H
      HR0=HLMN
      SELECT CASE(NDRV)                                                              
          CASE(1)                                                                      
              NDVSS=34                                                                 
          CASE(2)                                                                      
              NDVSS=32                                                                 
          CASE(3)                                                                      
              NDVSS=30                                                                 
          CASE(4)                                                                      
              NDVSS=33                                                                 
          CASE(5)                                                                      
              NDVSS=31                                                                 
          CASE(6)                                                                      
              NDVSS=83                                                                 
          CASE(7)                                                                      
              NDVSS=36                                                                 
          CASE(8)                                                                      
              NDVSS=35                                                                 
      END SELECT                                                                     
      XY2=.5/YWI                                                                     
      IF(IWP5>0)THEN
          I1=IWP5-1                                                                      
          II=67*I1                                                                       
          IF(II>0)THEN
              DO I=1,II                                                                      
                  READ(KR(18),'()',IOSTAT=NFL)
	              IF(NFL/=0)EXIT                                                                    
	          END DO
	          IF(IBAT==0)THEN
	              WRITE(*,*)'WPM5 NO = ',IWP5,' NOT IN WPM5 FILE'
#                     if defined(DEBUG) .AND. defined(PAUSE_ENABLE)
	                PAUSE  
#                     endif                
	              STOP 16
              ELSE
                  WRITE(KW(MSO),'(3A,I4,A)')' !!!!! ',TRIM(ASTN),&
                  &'WPM5 NO = ',IWP5,' NOT IN WPM5 FILE'
                  GO TO 219	              
	          END IF
          END IF	                                                                                          
        !     LINE 1/2                                                                       
          READ(KR(18),505)TITW5
      END IF
      IF(IWP1==0)THEN
          W0=1.E20                                                                       
          DO                                                                             
              READ(KR(17),*,IOSTAT=NFL)II,OPSCFILE,Y,X,ELEX                                     
	          IF(NFL/=0)EXIT                                                                    
	          RY=Y/CLT
              XX=SIN1*SIN(RY)+COS1*COS(RY)*COS((X-XLOG)/CLT)
              D=6378.8*ACOS(XX)
              E=ABS(ELEV-ELEX)
              W1=PRMT(79)*D+(1.-PRMT(79))*E                                                                  
              IF(W1>=W0)CYCLE                                                                
              W0=W1                                                                         
              WPM1FILE=OPSCFILE                                                            
	      END DO                                                                              
      ELSE
          II=-1                                                                            
          DO WHILE(II/=IWP1)                                                                             
              READ(KR(17),*,IOSTAT=NFL)II,WPM1FILE                                         
	          IF(NFL/=0)THEN
	              IF(IBAT==0)THEN
	                  WRITE(*,*)'WPM1 NO = ',IWP1,' NOT IN MO WEATHER LIST FILE'
#                         if defined(DEBUG) .AND. defined(PAUSE_ENABLE)
                            PAUSE 
#                         endif                                                     
	                  STOP 17          
	              ELSE
	                  WRITE(KW(MSO),'(3A,I4,A)')' !!!!! ',TRIM(ASTN),&
	                  ' WPM1 NO = ',IWP1,' NOT IN MO WEATHER LIST FILE'
	                  GO TO 219
                  END IF	                                                                                
	          END IF                                                                            
          END DO
      END IF	                                                                             
      REWIND KR(17)                                                                  
      CALL OPENV(KR(24),WPM1FILE,KW(MSO))                                            
!     LINE 1/2                                                                       
      READ(KR(24),'()')
      READ(KR(24),'()')
      CALL APAGE(0)                                                                  
      WRITE(KW(1),'(//1X,A/)')'____________________WEATHER DATA________________________'
	  IYX=IYR0-1880                                                                       
      IF(ICO2==0)THEN
          WRITE(KW(1),'(T10,A)')'STATIC ATMOSPHERIC CO2'
      ELSE                  
	      IF(ICO2==1)THEN                                                                     
	          IF(IYX<25)THEN                                                                    
	              CO2=280.33                                                                    
	          ELSE                                                                              
                  X1=IYX                                                                   
                  CO2=280.33-X1*(.1879-X1*.0077)                                           
              END IF                                                                       
              WRITE(KW(1),'(T10,A)')'DYNAMIC ATMOSPHERIC CO2'                              
          ELSE                                                                           
	          WRITE(KW(1),'(T10,A)')'INPUT ATMOSPHERIC CO2'                                     
	      END IF                                                                              
      END IF
      WRITE(KW(1),'(T10,A,F6.0,A)')'CO2 CONC ATMOSPHERE = ',CO2,' ppm'               
      WRITE(KW(1),'(T10,A,F4.0,A)')'PERIOD OF RECORD P5MX =',YWI,' Y'                
      IF(NGN>0)THEN                                                                  
          CALL WIGV                                                                    
      ELSE                                                                           
          WRITE(KW(1),'(/T10,A,A)')'**********RAIN, TEMP, RAD, WIND SPEED,',&            
          ' & REL HUM ARE GENERATED**********'                                         
      END IF                                                                         
      SELECT CASE(IET)                                                               
          CASE(1)                                                                      
              WRITE(KW(1),'(/T10,A,A)')'**********PENMAN-MONTEITH  EQ USED',&            
              ' TO EST POT ET**********'                                               
          CASE(2)                                                                      
              WRITE(KW(1),'(/T10,A,A)')'**********PENMAN EQ USED TO EST POT',&            
              ' & ET**********'                                                         
          CASE(3)                                                                      
              WRITE(KW(1),'(/T10,A,A)')'**********PRIESTLEY-TAYLOR EQ USED',&            
              ' TO EST POT ET**********'                                               
          CASE(4)                                                                      
              WRITE(KW(1),'(/T10,A,A)')'**********HARGREAVES EQ USED TO EST',&            
              ' POT ET**********'                                                     
          CASE(5)                                                                      
              WRITE(KW(1),'(/T10,A,A)')'********** BAIER-ROBERTSON EQ USED',&            
              ' TO EST POT ET **********' 
          CASE DEFAULT                                                           
              WRITE(KW(1),'(/T10,A,A)')'**********HARGREAVES EQ USED TO EST',&            
              ' POT ET**********'                                                     
      END SELECT                                                                     
      DO 568 IW=1,6                                                                  
!  3  OBMX   = AV MO MAX TEMP (C)                                                    
!  4  OBMN   = AV MO MIN TEMP (C)                                                    
!  5  SDTMX  = MO STANDARD DEV MAX TEMP (C)OR EXTREME MAXIMUM TEMP (C)               
!              IF STANDARD DEV IS NOT AVAILABLE (BLANK IF TEMP IS INPUT              
!              DAILY)                                                                
!  6  SDTMN  = MO STANDARD DEV MIN TEMP (C)OR EXTREME MIN TEMP (C)                   
!              IF STANDARD DEV IS NOT AVAILABLE (BLANK IF TEMP IS INPUT              
!              DAILY)                                                                
!  7  RMO    = AV MO PRECIPITATION (mm)                                              
!  8  RST(2) = MONTHLY ST DEV OF DAILY RAINFALL (mm)(BLANK IF UNKNOWN                
!              OR IF DAILY PRECIPITATION IS INPUT)                                   
!  9  RST(3) = MONTHLY SKEW COEF OF DAILY RAINFALL (BLANK IF UNKNOWN OR              
!              DAILY PRECIPITATION IS INPUT)                                         
! 10  PRW(1) = MONTHLY PROBABILITY OF WET DAY AFTER DRY DAY (BLANK IF                
!              UNKNOWN OR IF DAILY PRECIPITATION IS INPUT)                           
! 11  PRW(2) = MONTHLY PROBABILITY OF WET DAY AFTER WET DAY (BLANK IF                
!              UNKNOWN OR IF DAILY PRECIPITATION IS INPUT)                           
! 12  UAVM   = AV NO DAYS OF PRECIPITATION/MO (BLANK IF PRECIP IS                    
!              GENERATED AND IF PRW 1&2 ARE INPUT)                                   
! 13  WI     = 3 OPTIONS--(1)MO MAX .5 H RAIN FOR PERIOD = YWI (mm)                  
!                         (2)ALPHA (MEAN .5 H RAIN/MEAN STORM                        
!                             AMOUNT)                                                
!                         (3)BLANK IF UNKNOWN                                        
! 14  OBSL   = AV MO SOL RAD (MJ/M2 OR LY)(BLANK IF UNKNOWN)                         
! 15  RH     = 3 OPTIONS--(1)AV MO RELATIVE HUMIDITY (FRACTION)                      
!                         (2)AV MO DEW POINT TEMP deg C                              
!                         (3)BLANK IF UNKNOWN                                        
!              USED IN PENMAN OR PENMAN-MONTEITH EQS                                 
! 16  UAV0   = AV MO WIND SPEED (M/S)                                                
!     LINES 3/15                                                                     
      IF(IW==1)THEN
          READ(KR(24),310)(OBMX(IW,I),I=1,12)                                            
          READ(KR(24),310)(OBMN(IW,I),I=1,12)                                            
          READ(KR(24),310)(SDTMX(IW,I),I=1,12)                                           
          READ(KR(24),310)(SDTMN(IW,I),I=1,12)                                           
          READ(KR(24),310)(RMO(IW,I),I=1,12)                                             
          READ(KR(24),310)(RST(2,IW,I),I=1,12)                                           
          READ(KR(24),310)(RST(3,IW,I),I=1,12)                                           
          READ(KR(24),310)(PRW(1,IW,I),I=1,12)                                           
          READ(KR(24),310)(PRW(2,IW,I),I=1,12)                                           
          READ(KR(24),310)(UAVM(I),I=1,12)                                               
          READ(KR(24),310)(WI(IW,I),I=1,12)                                              
          READ(KR(24),310)(OBSL(IW,I),I=1,12)                                            
          READ(KR(24),310)(RH(IW,I),I=1,12)                                              
          READ(KR(24),310)(UAV0(I),I=1,12)                                    
	      REWIND KR(24)                                                             
          WRITE(KW(1),295)                                                               
          TAV(1)=.25*(OBMX(IW,12)+OBMN(IW,12)+OBMX(IW,1)+OBMN(IW,1))                     
          JT1=1                                                                          
          IF(OBMN(IW,1)>OBMN(IW,12))JT1=12                                               
          TMN=OBMN(IW,JT1)                                                               
          DO I=2,12                                                                      
              I1=I-1                                                                       
              TAV(I)=.25*(OBMX(IW,I)+OBMN(IW,I)+OBMX(IW,I1)+OBMN(IW,I1))                   
              IF(OBMN(IW,I)>TMN)CYCLE                                                      
              JT1=I                                                                        
              TMN=OBMN(IW,I)                                                               
	      END DO
	  ELSE                                                                              
          READ(KR(18),310)(OBMX(IW,I),I=1,12)                                            
          READ(KR(18),310)(OBMN(IW,I),I=1,12)                                            
          READ(KR(18),310)(SDTMX(IW,I),I=1,12)                                           
          READ(KR(18),310)(SDTMN(IW,I),I=1,12)                                           
          READ(KR(18),310)(RMO(IW,I),I=1,12)                                             
          READ(KR(18),310)(RST(2,IW,I),I=1,12)                                           
          READ(KR(18),310)(RST(3,IW,I),I=1,12)                                           
          READ(KR(18),310)(PRW(1,IW,I),I=1,12)                                           
          READ(KR(18),310)(PRW(2,IW,I),I=1,12)                                           
          READ(KR(18),310)(UAVM(I),I=1,12)                                               
          READ(KR(18),310)(WI(IW,I),I=1,12)                                              
          READ(KR(18),310)(OBSL(IW,I),I=1,12)                                            
          READ(KR(18),310)(RH(IW,I),I=1,12)                                              
	      READ(KR(18),310)(UAV0(I),I=1,12)                                                    
	  END IF
      DO I=1,12                                                                      
          IF(RST(2,IW,I)<1.E-5.OR.RST(3,IW,I)<1.E-5)EXIT
	  END DO
	  IF(I>12)THEN                                                                              
          ICDP=0 
      ELSE                                                                        
          ICDP=1                                                                         
          SUM=0.                                                                         
          DO I=1,10000                                                                   
              XX=AUNIF(IDG(3))                                                             
              SUM=SUM+(-LOG(XX))**EXPK                                                    
          END DO                                                                         
          REXP=10100./SUM                                                                
      END IF
      BIG=0.                                                                         
      V3=AUNIF(IDG(3))                                                               
      DO I=1,12                                                                   
          I1=I+1                                                                         
          XM=NC(I1)-NC(I)                                                                
          JDA=(NC(I1)+NC(I))*.5
          CALL WHRL                                                          
          CALL WRMX                                                                    
          SRMX(I)=RAMX                                                                   
          THRL(I)=HRLT                                                                   
          IF(HRLT>BIG)BIG=HRLT                                                           
          XYP(I)=0.                                                                      
          XX=SDTMX(IW,I)-SDTMN(IW,I)                                                     
          IF(XX>10.)THEN                                                                 
              SDTMX(IW,I)=(SDTMX(IW,I)-OBMX(IW,I))*.25                                     
              SDTMN(IW,I)=(OBMN(IW,I)-SDTMN(IW,I))*.25                                     
	      END IF                                                                              
          IF(PRW(1,IW,I)>0.)THEN                                                         
              UAVM(I)=XM*PRW(1,IW,I)/(1.-PRW(2,IW,I)+PRW(1,IW,I))                          
          ELSE                                                                           
              PRW(1,IW,I)=BTA*(UAVM(I)+.0001)/XM                                           
              PRW(2,IW,I)=1.-BTA+PRW(1,IW,I)                                               
          END IF                                                                         
          RST(1,IW,I)=RMO(IW,I)/(UAVM(I)+.01)                                            
          IF(OBSL(IW,I)<=0.)THEN                                                         
	          X1=MAX(.8,.21*SQRT(OBMX(IW,I)-OBMN(IW,I)))                                        
	          OBSL(IW,I)=X1*RAMX                                                                
	      END IF                                                                              
          IF(ICDP==0)THEN
              SUM=0.                                                                         
              RFVM=RST(1,IW,I)                                                               
              RFSD=RST(2,IW,I)                                                               
              RFSK=RST(3,IW,I)                                                               
              R6=RFSK/6.                                                                     
              DO J=1,1000                                                                    
                  V4=AUNIF(IDG(3))                                                             
                  XX=ADSTN(V3,V4)                                                              
                  V3=V4                                                                        
                  R1=WRAIN(R6,XX,RFSD,RFSK,RFVM)                                               
                  SUM=SUM+R1                                                                   
              END DO                                                                         
              PCF(IW,I)=1010.*RST(1,IW,I)/SUM
          ELSE                             
              RST(1,IW,I)=RST(1,IW,I)*REXP                                                   
              PCF(IW,I)=1.                                                                   
          END IF       
      END DO
      XYP(1)=OBMX(IW,1)                                                              
      BIG=OBSL(IW,1)                                                                 
      UPLM=RH(IW,1)                                                                  
      RFMX=RMO(IW,1)                                                                 
      EXTM=WI(IW,1)                                                                  
      DO I=2,12                                                                      
          IF(OBSL(IW,I)>BIG)BIG=OBSL(IW,I)                                             
          IF(RMO(IW,I)>RFMX)RFMX=RMO(IW,I)                                             
          IF(RH(IW,I)>UPLM)UPLM=RH(IW,I)                                               
          IF(WI(IW,I)>EXTM)EXTM=WI(IW,I)                                               
          XYP(1)=XYP(1)+OBMX(IW,I)                                                     
	  END DO                                                                              
      XYP(1)=XYP(1)/12.                                                              
      RUNT=1.                                                                        
      IF(BIG>100.)RUNT=.04184                                                        
      X3=.3725/(XYP(1)+20.)                                                          
	  SUM=0.                                                                              
      DO I=1,12                                                                   
          XM=NC(I+1)-NC(I)                                                               
          WFT(IW,I)=UAVM(I)/XM                                                           
          XYP(2)=XYP(2)+OBMN(IW,I)                                                       
          XYP(3)=XYP(3)+RMO(IW,I)                                                        
          XYP(4)=XYP(4)+UAVM(I)                                                          
          OBSL(IW,I)=RUNT*OBSL(IW,I)                                                     
          XYP(5)=XYP(5)+OBSL(IW,I)                                                       
          X1=MAX(RMO(IW,I),12.7)                                                         
          TX=.5*(OBMX(IW,I)+OBMN(IW,I))                                                  
          IF(UPLM>1.)THEN
              RH(IW,I)=ASVP(RH(IW,I)+273.)/ASVP(TX+273.)
          ELSE                                                                                                
              IF(RH(IW,I)<1.E-10)THEN
                  XX=OBMX(IW,I)-OBMN(IW,I)                                                       
                  RH(IW,I)=.9-.8*XX/(XX+EXP(5.122-.1269*XX))                                     
              END IF                                                        
          END IF
          X2=MAX(TX,-1.7)                                                                
          XYP(6)=XYP(6)+((X1/25.4)/(1.8*X2+22.))**1.111                                  
          X1=RMO(IW,I)/(UAVM(I)+1.E-10)                                                  
          IF(EXTM<1.)THEN
              IF(EXTM<1.E-10)WI(IW,I)=MAX(.05,APM*X3*(OBMX(IW,I)+20.))                       
              XTP(I)=5.3*X1*WI(IW,I)                                                         
              CYCLE
          END IF
          F=XY2/(UAVM(I)+.01)                                                            
          XTP(I)=WI(IW,I)                                                                
          WI(IW,I)=-XTP(I)/LOG(F)                                                       
          WI(IW,I)=APM*WI(IW,I)/(X1+1.)                                                  
          IF(WI(IW,I)<.1)WI(IW,I)=.1                                                     
          IF(WI(IW,I)>.95)WI(IW,I)=.95                                                   
          X1=1.4-.0778*TX                                                                
          X2=.5+.37*TX                                                                   
          X1=MIN(1.,X1,X2)                                                               
          IF(X1<=0.)CYCLE
          SUM=SUM+X1*XM                                                                  
      END DO
      XYP(2)=XYP(2)/12.                                                              
      XYP(5)=XYP(5)/12.                                                              
      IF(IW>1)THEN                                                                   
	      II=IW-1                                                                           
          WRITE(KW(1),571)II,TITW5(1)                                                  
	  ELSE                                                                                
          WRITE(KW(1),602) TRIM(WPM1FILE)
      END IF                                                                         
      WRITE(KW(1),321)HED(1),(OBMX(IW,I),I=1,12),XYP(1),HED(1)                       
      WRITE(KW(1),321)HED(2),(OBMN(IW,I),I=1,12),XYP(2),HED(2)                       
      WRITE(KW(1),224)'SDMX',(SDTMX(IW,I),I=1,12),'SDMX'                             
      WRITE(KW(1),224)'SDMN',(SDTMN(IW,I),I=1,12),'SDMN'                             
      WRITE(KW(1),243)HED(4),(RMO(IW,I),I=1,12),XYP(3),HED(4)                        
      WRITE(KW(1),223)'SDRF',(RST(2,IW,I),I=1,12),'SDRF'                             
      WRITE(KW(1),224)'SKRF',(RST(3,IW,I),I=1,12),'SKRF'                             
      WRITE(KW(1),225)'PW/D',(PRW(1,IW,I),I=1,12),'PW/D'                             
      WRITE(KW(1),225)'PW/W',(PRW(2,IW,I),I=1,12),'PW/W'                             
      WRITE(KW(1),321)'DAYP',(UAVM(I),I=1,12),XYP(4),'DAYP'                          
      WRITE(KW(1),223)'P5MX',(XTP(I),I=1,12),'P5MX'                                  
      WRITE(KW(1),243)HED(3),(OBSL(IW,I),I=1,12),XYP(5),HED(3)                       
      WRITE(KW(1),223)'RAMX',SRMX,'RAMX'                                             
      WRITE(KW(1),224)'HRLT',THRL,'HRLT'                                             
      WRITE(KW(1),224)'RHUM',(RH(IW,I),I=1,12),'RHUM'                                
      WRITE(KW(1),224)'ALPH',(WI(IW,I),I=1,12),'ALPH'                                
      WRITE(KW(1),224)' PCF',(PCF(IW,I),I=1,12),' PCF'                               
      IF(IWP5==0)EXIT                                                                
  568 CONTINUE                                                                       
      REWIND KR(18)
      IF(IWND==0)THEN
          W0=1.E20                                                                       
          DO                                                                             
              READ(KR(19),*,IOSTAT=NFL)II,OPSCFILE,Y,X,ELEX                                     
	          IF(NFL/=0)EXIT                                                                    
	          RY=Y/CLT
              XX=SIN1*SIN(RY)+COS1*COS(RY)*COS((X-XLOG)/CLT)
              D=6378.8*ACOS(XX)
              E=ABS(ELEV-ELEX)
              W1=PRMT(79)*D+(1.-PRMT(79))*E                                                                  
              IF(W1>=W0)CYCLE                                                                
              W0=W1           
              WINDFILE=OPSCFILE                                                            
	      END DO                                                                              
      ELSE   
          II=-1                                                                         
          DO WHILE(II/=IWND)
              READ(KR(19),*,IOSTAT=NFL)II,WINDFILE                                         
	          IF(NFL/=0)THEN
	              IF(IBAT==0)THEN
	                  WRITE(*,'(T10,A,I8,A)')'WIND NO = ',IWND,' NOT IN'&
	                  ' MO WIND LIST FILE'
#                         if defined(DEBUG) .AND. defined(PAUSE_ENABLE)
                            PAUSE
#			  endif
	                  STOP 18
	              ELSE
	                  WRITE(KW(MSO),'(3A,I4,A)')' !!!!! ',&
	                  TRIM(ASTN),' WIND NO = ',IWND,' NOT IN MO WIND LIST FILE'
	                  GO TO 219
                  END IF	                  
	          END IF                                                                               
	      END DO         
	  END IF                                                                     
      REWIND KR(19)                                                                  
      CALL OPENV(KR(25),WINDFILE,KW(MSO))                                            
!     LINE 1/2                                                                       
      READ(KR(25),'()')
      READ(KR(25),'()')
      SX=SX/SN                                                                       
      IF(CHL<1.E-10)CHL=COCH(5)*WSA**COCH(6)                                         
      IF(CHS<1.E-10)CHS=UPS*WSX**(-.3)                                               
      IF(CHD<1.E-10)CHD=COCH(3)*WSA**COCH(4)                                         
      UPSL=MIN(UPSL,SQRT(10000.*WSA))                                                
      XM=.3*UPS/(UPS+EXP(-1.47-61.09*UPS))+.2                                        
      UPSX=UPSL/22.127                                                               
      SL=UPSX**XM*(UPS*(65.41*UPS+4.56)+.065)                                        
      X1=3.*UPS**.8+.56                                                              
      BETA=UPS/(.0896*X1)                                                            
      RXM=BETA/(1.+BETA)                                                             
	  RLF=UPSX**RXM                                                                       
      IF(UPSL>4.57)THEN
          IF(UPS>.09)THEN
              RSF=16.8*UPS-.5
          ELSE                                                                
              RSF=10.8*UPS+.03                                                               
          END IF
      ELSE
          RSF=X1                                                                         
      END IF  
      IF(ITYP>0)THEN
          IF(CHL>.1)THEN
              SFL=50.
          ELSE                             
              IF(CHL>.05)THEN
                  SFL=100.*(CHL-.05)                                                             
              ELSE                                      
                  SFL=0.                                                                         
              END IF
          END IF
          TSF=SFL/MIN(2160.,17712.*SX*SN)                                                
          X1=MAX(CHL-(UPSL+SFL)*.001,0.)                                                 
          TCC=X1/(3.6*CHD**.66667*SQRT(CHS)/CHN)                                         
          TCS=.0913*(UPSL*SN)**.8/UPS**.4                                                
          TCC=TCC+TSF                                                                    
          TC=TCC+TCS    
      ELSE                                                            
          TCS=.0216*(UPSL*SN)**.75/UPS**.375                                             
          TCC=1.75*CHL*CHN**.75/(WSA**.125*CHS**.375)                                    
          X4=MIN(UPSL/360.,TCS)                                                          
          TC=X4+TCC                                                                      
      END IF
      IF(IAZM>0)THEN
          X1=ASIN(UPS)
          YLAZ=YLAT/CLT
          X2=AZM/CLT
          YLAZ=CLT*ASIN(UPS*COS(X2)*COS(YLAZ)+COS(X1)*SIN(YLAZ))
          WRITE(KW(1),'(T10,A,F8.3)')'EQUIVALENT LATITUDE = ',YLAZ
      ELSE
          YLAZ=YLAT          
      END IF
      XX=YLAZ/CLT
      YLTS=SIN(XX)
      YLTC=COS(XX)
      YTN=TAN(XX)
      JDHU=400                                                                       
      WDRM=HLMN                                                                      
      IF(HLMN<11.)THEN                                                               
	      CALL ADAJ(NC,JDHU,JT1,15,NYD)                                                     
	      WDRM=PRMT(6)+HLMN                                                                 
	  END IF                                                                              
      AAP=XYP(3)                                                                     
      XYP(6)=115.*XYP(6)                                                             
      AVT=(XYP(1)+XYP(2))*.5                                                         
!     UAVM = AV MO WIND SPEED (M/S)(REQUIRED TO SIMULATE WIND                        
!            EROSION--ACW>0 LINE 24  AND POTENTIAL ET IF PENMAN OR                   
!            PENMAN-MONTEITH EQS ARE USED--LINE 4)                                   
!     LINE 3                                                                         
      READ(KR(25),310)UAVM                                                           
      AWV=0.                                                                         
	  WB=0.                                                                               
      DO I=1,12                                                                      
          RNCF(I)=1.                                                                   
          TMNF(I)=1.                                                                   
	      IF(UAVM(I)<1.E-5)UAVM(I)=UAV0(I)                                                  
	      SMY(I)=0.                                                                         
          DO J=1,100                                                                   
              RN2=AUNIF(IDG(5))                                                        
              WV=UAVM(I)*(-LOG(RN2))**UXP                                             
              IF(WV<PRMT(67))CYCLE                                                     
              EV=193.*EXP(1.103*(WV-30.)/(WV+1.))                                      
              SMY(I)=SMY(I)+EV                                                         
          END DO                                                                       
          WB=WB+SMY(I)                                                                 
          AWV=AWV+UAVM(I)                                                              
      END DO                                                                         
      AWV=AWV/12.                                                                    
	  WCF=(3.86*AWV**3/XYP(6)**2)**.3484                                                  
      IF(PRMT(40)>0..AND.IRR==0)THEN                                                 
	      X1=MAX(1.,AVT)                                                                    
          CLF=SQRT(AAP/(X1*PRMT(40)))                                                  
	  ELSE                                                                                
          CLF=1.                                                                       
	  END IF                                                                              
      WRITE(KW(1),'(T10,A,F7.3)')'CLIMATIC FACTOR = ',CLF                            
!     DIR  = AV MO FRACTION OF WIND FROM 16 DIRECTIONS (BLANK UNLESS                 
!            WIND EROSION IS SIMULATED--ACW>0 LINE 24).                              
      DO J=1,16                                                                      
!     LINES 4/19                                                                     
          READ(KR(25),310)(DIR(I,J),I=1,12)                                            
          IF(DIR(1,J)>0.)CYCLE                                                         
          DO I=1,12                                                                    
              DIR(I,J)=1.                                                              
	      END DO                                                                            
	  END DO                                                                              
      REWIND KR(25)                                                                  
      WRITE(KW(1),'(/T20,2A)')'WIND = ', TRIM(WINDFILE)
      WRITE(KW(1),321)HED(7),(UAVM(I),I=1,12),AWV,HED(7)                             
      IF(ICDP==0)THEN                                                                
	      WRITE(KW(1),'(/T10,A)')'RAINFALL DIST IS SKEWED NORMAL'                           
	  ELSE                                                                                
          WRITE(KW(1),'(/T10,A,F5.2)')'RAINFALL DIST IS EXP--PARM = ',EXPK             
          WRITE(KW(1),'(/T10,A,F5.3)')'WET-DRY PROB COEF = ',BTA                       
      END IF                                                                         
      AHSM=CAHU(1,365,0.,1)                                                          
      WRITE(KW(1),221)SUM,AHSM                                                       
      WRITE(KW(1),'(//1X,A)')'-----WIND EROSION DATA'                                
      WRITE(KW(1),285)FL,FW,ANG0,UXP,DIAM,ACW                                        
      DO I=1,12                                                                   
          IF(UAVM(1)>0.)THEN                                                             
              CALL AEXINT(UXP,SUM)                                                         
              UAVM(I)=UAVM(I)/SUM                                                          
	      END IF                                                                              
          DO J=2,16                                                                      
              DIR(I,J)=DIR(I,J)+DIR(I,J-1)                                                 
	      END DO                                                                              
          DO J=1,16                                                                      
              DIR(I,J)=DIR(I,J)/DIR(I,16)                                                  
	      END DO
	  END DO                                                                              
      TX=(OBMX(1,MO)+OBMN(1,MO))/2.                                                  
      ST0=OBSL(1,MO)                                                                 
      DST0=TX                                                                        
      CALL APAGE(0)                                                                  
      WRITE(KW(1),'(//1X,A/)')'____________________GENERAL INFORMATION______________________'
      KRX=KR(16)                                                                     
      IF(NSTP>0)THEN                                                                 
	      WRITE(KW(1),'(T10,A)')'REAL TIME SIMULATION MODE'                                 
	      READ(KW(MSO+3),300)ISTP                                                           
          IF(ISTP>0)THEN                                                               
              KRX=KR(22)                                                               
              CALL OPENV(KR(22),'RTOPSC.DAT',KW(MSO))                                      
	      END IF                                                                            
	  ELSE                                                                                
          WRITE(KW(1),'(T10,A)')'NORMAL SIMULATION MODE'                               
      END IF                                                                         
      IF(PRMT(50)>0.)THEN                                                            
	      WRITE(KW(1),'(T10,A)')'DYNAMIC TECHNOLOGY'                                        
	  ELSE                                                                                
	      WRITE(KW(1),'(T10,A)')'STATIC TECHNOLOGY'                                         
  	  END IF                                                                            
      WRITE(KW(1),301)NBYR,IYR0,IMO0,IDA0                                            
      IF(LPYR>0)THEN                                                                 
	      WRITE(KW(1),'(T10,A)')'LEAP YEAR IGNORED'                                         
	  ELSE                                                                                
          WRITE(KW(1),'(T10,A)')'LEAP YEAR CONSIDERED'                                 
      END IF                                                                         
      WRITE(KW(1),365)WSA,YLAT,XLOG,ELEV,CHL,CHS,CHN,CHD,UPSL,UPS,AZM,SN                 
      WRITE(KW(1),'(T10,A,A4)')'WATER EROSION FACTORS--DRIVING EQ = ',&              
      HED(NDVSS)                                                                     
      IF(NDRV==6)WRITE(KW(1),'(T15,A,4E13.5)')'MUSI COEFS = ',BUS                    
      BUS(1)=BUS0*WSX**BUS(4)                                                        
      WRITE(KW(1),405)SL,RXM,RLF,RSF,TC                                              
      WRITE(KW(1),'(T10,A)')'DAILY RUNOFF ESTIMATION'                                
      SELECT CASE(INFL)                                                              
	      CASE(1)                                                                           
              WRITE(KW(1),'(T15,A)')'NRCS CURVE NUMBER EQ'                             
          CASE(2)                                                                      
              WRITE(KW(1),'(T15,A/T15,A)')'GREEN & AMPT EQ','RF EXP DST--'&            
              'PEAK RF RATE SIM'                                                      
          CASE(3)                                                                      
              WRITE(KW(1),'(T15,A/T15,A)')'GREEN & AMPT EQ','RF EXP DST--'&            
              'PEAK RF RATE INPUT'                                                    
          CASE(4)                                                                      
              WRITE(KW(1),'(T15,A/T15,A)')'GREEN & AMPT EQ','RF UNIF DST-'&            
              '-PEAK RF RATE INP'                                                     
      END SELECT                                                                     
	  SELECT CASE(NVCN+1)                                                                 
	      CASE(1)                                                                           
              WRITE(KW(1),'(T15,A)')'VARIABLE CN DEPTH/SOIL-WATER WEIGHT'                                                                 
          CASE(2)                                                                      
              WRITE(KW(1),'(T15,A)')'VARIABLE CN NO DP/SW WEIGHT'                                                                 
          CASE(3)                                                                      
              WRITE(KW(1),'(T15,A)')'VARIABLE CN LINEAR NO DP/SW WEIGHT'               
          CASE(4)                                                                      
              WRITE(KW(1),'(T15,A)')'CONSTANT CN'                                      
          CASE(5)                                                                      
              WRITE(KW(1),'(T15,A)')'VARIABLE CN RETN PAR INDEX NO DP/SW WEIGHT'             
      END SELECT                                                                     
      IF(ISCN==0)THEN                                                                
	      WRITE(KW(1),'(T15,A)')'DAILY CN--STOCHASTIC'                                      
	  ELSE                                                                                
          WRITE(KW(1),'(T15,A)')'DAILY CN--DETERMINISTIC'                              
      END IF                                                                         
      IF(ITYP>0)THEN                                                                 
	      WRITE(KW(1),'(T10,A,A2)')'PEAK RATE EST WITH TR55--RF TYPE ='&                    
          ,RFPT(ITYP)                                                                  
	  ELSE                                                                                
          WRITE(KW(1),'(T10,A)')'PEAK RATE EST WITH MOD RATIONAL EQ'                   
      END IF                                                                         
      IF(IERT>0)THEN                                                                 
	      WRITE(KW(1),'(T10,A)')'GLEAMS ENRICHMENT RATIO'                                   
	  ELSE                                                                                
          WRITE(KW(1),'(T10,A)')'EPIC ENRICHMENT RATIO'                                
      END IF                                                                         
      IF(ICG>0)THEN                                                                  
          WRITE(KW(1),'(T10,A)')'WATER USE-BIOMASS CONVERSION'                         
      ELSE                                                                           
          WRITE(KW(1),'(T10,A)')'RADIATION-BIOMASS CONVERSION'                         
      END IF                                                                         
      IF(IEVI==0)THEN
          WRITE(KW(1),'(T10,A)')'PAR DRIVEN BY LAI'
      ELSE
          WRITE(KW(1),'(T10,A)')'PAR DRIVEN BY REMOTE SENSING EVI'
      END IF                                                                                 
      IF(ICF==0)THEN                                                                 
	      WRITE(KW(1),'(T10,A)')'RUSLE C FACTOR USED FOR ALL EROS EQS'                      
	  ELSE                                                                                
          WRITE(KW(1),'(T10,A)')'EPIC C FACTOR USED EXCEPT FOR RUSLE'                  
      END IF
      SELECT CASE(IDN)
          CASE(1)
              WRITE(KW(1),'(T10,A)')'EPIC DNIT'
          CASE(2)              
	          WRITE(KW(1),'(T10,A)')'KEMANIAN DNIT'                                             
          CASE(3)	          
              WRITE(KW(1),'(T10,A,F5.2,A)')'IZAURRALDE DNIT DZ=',DZ,' m'
              WRITE(KW(1),'(T15,3(A,E16.6)A)')'XKN1=',XKN1,' XKN3=',XKN3,&
              ' XKN5=',XKN5,' ORIGINAL DW'                                                          
          CASE(4)
              WRITE(KW(1),'(T10,A,F5.2,A)')'IZAURRALDE DNIT DZ=',DZ,' m'
              WRITE(KW(1),'(T15,3(A,E16.6)A)')'XKN1=',XKN1,' XKN3=',XKN3,&
              ' XKN5=',XKN5,' NEW DW'                                                          
      END SELECT
      IF(NUPC>0)THEN                                                                 
	      WRITE(KW(1),'(T10,A)')'N & P UPTAKE CONC S CURVE'                                 
	  ELSE                                                                                
          WRITE(KW(1),'(T10,A)')'N & P UPTAKE CONC SMITH CURVE'                        
      END IF                                                                         
      IF(IOX>0)THEN                                                                  
	      WRITE(KW(1),'(T10,A)')'O2=F(C/CLA)'                                               
	  ELSE                                                                                
          WRITE(KW(1),'(T10,A)')'EPIC O2=F(Z)'                                         
      END IF                                                                         
      WRITE(KW(1),233)APM,SNO0,RFN0,CNO3I,CSLT                                       
      IF(MASP>0)THEN                                                                 
          WRITE(KW(1),'(T10,A)')'NUTRIENT & PESTICIDE OUTPUT (MASS & CONC)'                                   
	  ELSE                                                                                
          WRITE(KW(1),'(T10,A)')'NUTRIENT & PESTICIDE OUTPUT (MASS)'
      END IF                                                                         
      IF(LBP>0)THEN                                                                  
	      WRITE(KW(1),'(T10,A)')'MODIFIED NONLINEAR EQ SOL P RUNOFF'                        
	  ELSE                                                                                
          WRITE(KW(1),'(T10,A)')'GLEAMS PESTICIDE EQ SOL P RUNOFF'                     
      END IF                                                                         
      WRITE(KW(1),330)COL,COIR,FULP,WAGE,CSTZ(1),CSTZ(2)                             
  531 CALL AINLZ                                                                     
      CALL AINIX                                                                     
	  IYX=IYR0-1880                                                                       
      SNO=SNO0                                                                       
      DO J=1,21                                                                      
          IDG(J)=J                                                                     
	  END DO                                                                              
      CALL APAGE(0)                                                                  
!     RANDOM NUMBER GENERATOR ID NUMBERS                                             
!     IDG = 1 DETERMINES WET AND DRY DAYS                                            
!         = 2 RELATES WEATHER VARIABLES TO RAIN                                      
!         = 3 RAINFALL AMOUNT                                                        
!         = 4 RAINFALL ENERGY (EI)- PEAK RUNOFF RATE (QP)                            
!         = 5 WIND SPEED                                                             
!         = 6 WIND DIRECTION                                                         
!         = 7 RELATIVE HUMIDITY                                                      
!         = 8 RUNOFF CURVE NUMBER                                                    
!         = 9 WITHIN DAY WIND SPEED DIST                                             
      IF(IGN>0)THEN
          DO J=1,20                                                                      
              RN=AUNIF(21)                                                                 
              II=100*IGN*RN                                                                
              DO KK=1,II                                                                   
                  XX=AUNIF(21)                                                             
              END DO                                                                       
              IX(J)=IX(21)                                                                 
          END DO                                                                         
          CALL AISHFL 
      END IF                                                                   
      DO J=1,21                                                                      
          IX0(J)=IX(J)                                                                 
      END DO                                                                         
      WRITE(KW(1),297)IGN,(IX(IDG(I)),I=1,10),(IDG(I),I=1,10)                        
	  V3=AUNIF(IDG(3))                                                                    
      V1=AUNIF(IDG(2))                                                               
      BIG=MAX(.2,PRMT(24))                                                           
      CALL ADAJ(NC,IBD,IMO0,IDA0,NYD)                                                
      JDA=IBD                                                                        
      MO=1                                                                           
      CALL AXMON(IBD,MO)                                                             
      MO1=MO                                                                         
	  XX=0.                                                                               
      RZ=2.
      II=-1                                                                          
      DO WHILE(II/=INPS)                                                                             
          READ(KR(13),*,IOSTAT=NFL)II,SOILFILE                                         
	      IF(NFL/=0)THEN
	          IF(IBAT==0)THEN
                  WRITE(*,'(T10,A,I8,A)')'SOIL NO = ',INPS,&
                     ' NOT IN SOIL LIST FILE'
#               if defined(DEBUG) .AND. defined(PAUSE_ENABLE)
                  PAUSE
#		        endif
	              STOP 19
	          ELSE 
	              WRITE(KW(MSO),'(3A,I4,A)')' !!!!! ',TRIM(ASTN),&
	              'SOIL NO = ',INPS,' NOT IN SOIL LIST FILE'
                  GO TO 219
              END IF                      
	      END IF                                                                               
	  END DO                                                                              
      REWIND KR(13)     
                                                             
      CALL OPENV(KR(14),SOILFILE,KW(MSO))                                            
!  1  SOLS = SOIL SERIES NAME
!  2  SOLO = SOIL ORDER                                                  
!     LINE 1                                                  
      READ(KR(14),*)SOLS,SOLO                                                           
!     READ SOIL DATA                                                                 
!  1  SALB = SOIL ALBEDO                                                             
!  2  HSG  = HYDROLOGIC SOIL GROUP--1.=A; 2.=B; 3.=C; 4.=D                           
!  3  FFC  = FRACTION OF FIELD CAP FOR INITAL WATER STORAGE (BLANK IF                
!            UNKNOWN)                                                                
!  4  WTMN = MIN DEPTH TO WATER TABLE(m)(BLANK IF UNKNOWN                            
!  5  WTMX = MAX DEPTH TO WATER TABLE (m)(BLANK IF UNKNOWN                           
!  6  WTBL = INITIAL WATER TABLE HEIGHT(m) (BLANK IF UNKNOWN)                        
!  7  GWST = GROUNDWATER STORAGE (mm)                                                
!  8  GWMX = MAXIMUM GROUNDWATER STORAGE (mm)                                        
!  9  RFT0 = GROUNDWATER RESIDENCE TIME(d)(BLANK IF UNKNOWN)                         
! 10  RFPK = RETURN FLOW / (RETURN FLOW + DEEP PERCOLATION)                          
! 11  TSLA = NUMBER OF SOIL LAYERS AFTER SPLITTING (3-15)                            
!          = 0 NO SPLITTING OCCURS INITIALLY                                         
! 12  XIDP = 0. FOR CALCAREOUS SOILS AND NON CALCAREOUS                              
!               WITHOUT WEATHERING INFORMATION                                       
!          = 1. FOR NON CACO3 SLIGHTLY WEATHERED                                     
!          = 2. NON CACO3 MODERATELY WEATHERED                                       
!          = 3. NON CACO3 HIGHLY WEATHERED                                           
!          = 4. INPUT PSP OR ACTIVE + STABLE MINERAL P (kg/ha)                       
! 13  RTN0 = NUMBER YEARS OF CULTIVATION AT START OF SIMULATION.                     
! 14  XIDK = 1 FOR KAOLINITIC SOIL GROUP                                             
!          = 2 FOR MIXED SOIL GROUP                                                  
!          = 3 FOR SMECTITIC SOIL GROUP                                              
! 15  ZQT  = MINIMUM THICKNESS OF MAXIMUM LAYER (m)(SPLITTING                        
!            STOPS WHEN ZQT IS REACHED)                                              
! 16  ZF   = MINIMUM PROFILE THICKNESS(m)--STOPS SIMULATION.                         
! 17  ZTK  = MINIMUM LAYER THICKNESS FOR BEGINNING SIMULATION LAYER                  
!            SPLITTING--MODEL SPLITS FIRST LAYER WITH THICKNESS GREATER              
!            THAN ZTK(m); IF NONE EXIST THE THICKEST LAYER IS SPLIT.                 
! 18  FBM  = FRACTION OF ORG C IN BIOMASS POOL(.03-.05)                              
! 19  FHP  = FRACTION OF HUMUS IN PASSIVE POOL(.3-.7)                                
! 20  XCC  = CODE WRITTEN AUTOMATICALLY BY .SOT (NOT USER INPUT)                     
!     LINE 2/3       
      IF (SoilInputFmtSelect == 1) THEN
         SoilInputFormat1 = '(10(G10.2,1X))'
         SoilInputFormat2 = '(15(G12.4,1X))'
         SoilInputFormat3 = '(15(A12,1X))'
      ELSE
         SoilInputFormat1 = '(10G8.3)'
         SoilInputFormat2 = '(15G8.3)'
         SoilInputFormat3 = '(15A8)'
      END IF
      READ(KR(14),SoilInputFormat1)SALB,HSG,FFC,WTMN,WTMX,WTBL,GWST,GWMX,RFT0,RFPK,&
           TSLA,XIDP,RTN0,XIDK,ZQT,ZF,ZTK,FBM,FHP,XCC                                     
      NCC=XCC                                                                        
      IF(GWST<1.E-10)GWST=25.                                                        
      IF(GWMX<1.E-10)GWMX=50.                                                        
      IF(WTMX<1.E-5)THEN                                                             
          WTMN=50.                                                                     
          WTMX=100.                                                                    
	      WTBL=75.                                                                          
	  END IF                                                                              
      IDSP=XIDP+1.1                                                                  
      IF(FBM<1.E-10)FBM=.04                                                          
      IF(FHP<1.E-10)FHP=.7-.3*EXP(-.0277*RTN0)                                       
      IDSK=MAX(XIDK,1.)                                                              
      IF(FFC<1.E-10)FFC=AAP/(AAP+EXP(9.043-.002135*AAP))                             
      DO I=1,MSL                                                                  
          WNH3(I)=0.                                                                     
          SEV(I)=0.                                                                      
          U(I)=0.                                                                        
          LORG(I)=I                                                                      
          LID(I)=I                                                                       
          DO J=1,MPS                                                                     
              PSTZ(J,I)=0.                                                                 
	      END DO
	  END DO                                                                             
      PFOL=0.                                                                        
      MXLA=TSLA                                                                      
      IF(ZQT<1.E-10)ZQT=.1                                                           
      IF(ZF<1.E-10)ZF=.1                                                             
      IF(ZTK<1.E-10)ZTK=.15                                                          
      RFTT=RFT0                                                                      
      IF(RFT0<1.E-10)RFTT=10.                                                        
!     THE SOIL IS DIVIDED VERTICALLY INTO LAYERS (MAX OF 10 LAYERS                   
!     OF USER SPECIFIED THICKNESS)                                                   
!  4  Z    = DEPTH TO BOTTOM OF LAYERS(m)                                            
!  5  BD   = BULK DENSITY (T/M**3)                                                   
!  6  U    = SOIL WATER CONTENT AT WILTING POINT (1500 kpa)(m/m)                     
!            (BLANK IF UNKNOWN)                                                      
!  7  FC   = WATER CONTENT AT FIELD CAPACITY (33kpa)(m/m)                            
!            (BLANK IF UNKNOWN)                                                      
!  8  SAN  = % SAND                                                                  
!  9  SIL  = % SILT                                                                  
! 10  WN   = INITIAL ORGANIC N CONC (g/t)        (BLANK IF UNKNOWN)                  
! 11  PH   = SOIL PH                                                                 
! 12  SMB  = SUM OF BASES (cmol/kg)              (BLANK IF UNKNOWN)                  
! 13  WOC  = ORGANIC CARBON CONC(%)                                                  
! 14  CAC  = CALCIUM CARBONATE (%)                                                   
! 15  CEC  = CATION EXCHANGE CAPACITY (cmol/kg) (BLANK IF UNKNOWN)                   
! 16  ROK  = COARSE FRAGMENTS (% VOL)           (BLANK IF UNKNOWN)                   
! 17  CNDS = INITIAL NO3 CONC (g/t)             (BLANK IF UNKNOWN)                   
! 18  PKRZ = INITIAL LABILE P CONC (g/t)        (BLANK IF UNKNOWN)                   
! 19  RSD  = CROP RESIDUE(t/ha)                 (BLANK IF UNKNOWN)                   
! 20  BDD  = BULK DENSITY (OVEN DRY)(T/M**3)    (BLANK IF UNKNOWN)                   
! 21  PSP  = P SORPTION RATIO <1.               (BLANK IF UNKNOWN)                   
!          = ACTIVE & STABLE MINERAL P (kg/ha) >1.                                   
! 22  SATC = SATURATED CONDUCTIVITY (mm/h)      (BLANK IF UNKNOWN)                   
! 23  HCL  = LATERAL HYDRAULIC CONDUCTIVITY(mm/h)(BLANK IF UNKNOWN)                  
! 24  WP   = INITIAL ORGANIC P CONC (g/t)       (BLANK IF UNKNOWN)                   
! 25  EXCK = EXCHANGEABLE K CONC (g/t)                                               
! 26  ECND = ELECTRICAL COND (mmHO/CM)                                               
! 27  STFR = FRACTION OF STORAGE INTERACTING WITH NO3 LEACHING                       
!                                               (BLANK IF UNKNOWN)                   
! 28  ST   = INITIAL SOIL WATER STORAGE (FRACTION OF FIELD CAP)                      
! 29  CPRV = FRACTION INFLOW PARTITIONED TO VERTICLE CRACK OR PIPE FLOW              
! 30  CPRH = FRACTION INFLOW PARTITIONED TO HORIZONTAL CRACK OR PIPE                 
!            FLOW                                                                    
! 31  WLS  = STRUCTURAL LITTER(kg/ha)           (BLANK IF UNKNOWN)                   
! 32  WLM  = METABOLIC LITTER(kg/ha)            (BLANK IF UNKNOWN)                   
! 33  WLSL = LIGNIN CONTENT OF STRUCTURAL LITTER(kg/ha)(B I U)                       
! 34  WLSC = CARBON CONTENT OF STRUCTURAL LITTER(kg/ha)(B I U)                       
! 35  WLMC = C CONTENT OF METABOLIC LITTER(kg/ha)(B I U)                             
! 36  WLSLC= C CONTENT OF LIGNIN OF STRUCTURAL LITTER(kg/ha)(B I U)                  
! 37  WLSLNC=N CONTENT OF LIGNIN OF STRUCTURAL LITTER(kg/ha)(BIU)                    
! 38  WBMC = C CONTENT OF BIOMASS(kg/ha)(BIU)                                        
! 39  WHSC = C CONTENT OF SLOW HUMUS(kg/ha)(BIU)                                     
! 40  WHPC = C CONTENT OF PASSIVE HUMUS(kg/ha)(BIU)                                  
! 41  WLSN = N CONTENT OF STRUCTURAL LITTER(kg/ha)(BIU)                              
! 42  WLMN = N CONTENT OF METABOLIC LITTER(kg/ha)(BIU)                               
! 43  WBMN = N CONTENT OF BIOMASS(kg/ha)(BIU)                                        
! 44  WHSN = N CONTENT OF SLOW HUMUS(kg/ha)(BIU)                                     
! 45  WHPN = N CONTENT OF PASSIVE HUMUS(kg/ha)(BIU)
! 46  FE26 = IRON CONTENT(%)
! 47  SULF = SULFUR CONTENT(%)
! 48  ASHZ = SOIL HORIZON(A,B,C)
! 49  CGO2 = O2 CONC IN GAS PHASE (g/m3 OF SOIL AIR)
! 50  CGCO2= CO2 CONC IN GAS PHASE (g/m3 OF SOIL AIR)
! 51  CGN2O= N2O CONC IN GAS PHASE (g/m3 OF SOIL AIR)                                  

!     LINES 4/47                                                                     
      READ(KR(14),SoilInputFormat2)Z,BD,U,(FC(J),J=1,15),SAN,SIL,WON,PH,SMB,WOC,CAC,&
          CEC,ROK,CNDS,PKRZ,RSD,BDD,PSP,SATC,HCL,WP,EXCK,ECND,STFR,ST,CPRV,&
          CPRH,WLS,WLM,WLSL,WLSC,WLMC,WLSLC,WLSLNC,(WBMC(J),J=1,15),WHSC,WHPC,&
          WLSN,WLMN,WBMN,WHSN,WHPN,FE26,SULF
!     LINE 48      
      READ(KR(14),SoilInputFormat3)ASHZ
!     LINES 49/51      
      IF(IDN>2)THEN
          READ(KR(14),SoilInputFormat2)(CGO2(J),J=1,15),(CGCO2(J),J=1,15),&
              (CGN2O(J),J=1,15)
      ELSE
          CGO2=0.
          CGCO2=0.
          CGN2O=0.
      END IF
      L=1                                                                            
      XCB=.2                                                                         
      SUM=0.                                                                         
      SOCF=0.                                                                        
      LZ=1                                                                           
      K=1                                                                            
      PZW=0.                                                                         
      TPAW=0.                                                                        
      DO J=1,MSL                                                                 
          IF(Z(J)<1.E-10)EXIT
          CLA(J)=100.-SAN(J)-SIL(J)                                                      
          DG=1000.*(Z(J)-XX)                                                             
          CALL SBDSC(BD(J),PRMT(2),F,J,1)                                                
          CALL SDST(RSD,DG,DG1,.01,.01,J,MSL)                                            
          CALL SDST(PKRZ,DG,DG,20.,.001,J,MSL)                                           
          CALL SDST(CNDS,DG,DG,10.,.001,J,MSL)                                           
          CALL SDST(EXCK,DG,DG,10.,.001,J,MSL)                                           
          IF(STFR(J)<1.E-10)STFR(J)=1.                                                   
          TRSD=TRSD+RSD(J)                                                               
          ZD=.25*(XX+Z(J))                                                               
          F=ZD/(ZD+EXP(-.8669-2.0775*ZD))                                                
          STMP(J)=F*(AVT-TX)+TX                                                          
          IF(WOC(J)<1.E-5)WOC(J)=XCB*EXP(-.001*DG)                                       
          XCB=WOC(J)                                                                     
          XZ=WOC(J)*.0172                                                                
          ZZ=1.-XZ                                                                       
          BDM(J)=ZZ/(1./BD(J)-XZ/.224)                                                   
          IF(BDM(J)>2.65)THEN
              BDM(J)=2.65
          ELSE                                                                 
	          IF(BDM(J)<1.)THEN                                                                   
	              BDM(J)=1.                                                                         
	              BD(J)=1./(ZZ+XZ/.224)                                                             
	          END IF                                                                              
	      END IF                                                                                  
          WT(J)=BD(J)*DG*10.                                                             
          DG1=DG                                                                         
          WT1=WT(J)/1000.                                                                
          X1=10.*WOC(J)*WT(J)                                                            
          WOC(J)=X1                                                                      
          IF(WON(J)>0.)THEN                                                              
              WON(J)=WT1*WON(J)                                                            
	          KK=0                                                                              
	      ELSE                                                                                
              WON(J)=.1*WOC(J)                                                             
              KK=1                                                                         
	      END IF                                                                              
          IF(NCC==0)THEN
              WBM=FBM*X1                                                                     
              WBMC(J)=WBM                                                                    
              IF(KK==0)THEN                                                                  
	              RTO=WON(J)/WOC(J)                                                                 
	          ELSE                                                                                
	              RTO=.1                                                                            
	          END IF                                                                              
              WBMN(J)=RTO*WBMC(J)                                                            
              WHP=FHP*(X1-WBM)                                                               
              WHS=X1-WBM-WHP                                                                 
              WHSC(J)=WHS                                                                    
              WHSN(J)=RTO*WHSC(J)                                                            
              WHPC(J)=WHP                                                                    
              WHPN(J)=RTO*WHPC(J)                                                            
              X1=RSD(J)                                                                      
              IF(J==1)X1=X1+STD0                                                              
              WLM(J)=500.*X1                                                                 
              WLS(J)=WLM(J)                                                                  
              WLSL(J)=.8*WLS(J)                                                              
              WLMC(J)=.42*WLM(J)                                                             
              WLMN(J)=.1*WLMC(J)                                                             
              WLSC(J)=.42*WLS(J)                                                             
              WLSLC(J)=.8*WLSC(J)                                                            
              WLSLNC(J)=.2*WLSC(J)                                                           
              WLSN(J)=WLSC(J)/150.                                                           
              WOC(J)=WOC(J)+WLSC(J)+WLMC(J)                                                  
              WON(J)=WON(J)+WLSN(J)+WLMN(J)
          END IF                                                            
          FOP(J)=RSD(J)*1.1                                                              
          SEV(4)=SEV(4)+FOP(J)                                                           
          PMN(J)=0.                                                                      
          IF(WP(J)>0.)THEN                                                               
              WP(J)=WT1*WP(J)                                                              
	      ELSE                                                                                
              WP(J)=.125*WON(J)                                                            
	      END IF                                                                              
          PO(J)=1.-BD(J)/2.65                                                            
          ZZ=.5*(XX+Z(J))                                                                
          X2=.1*WOC(J)/WT(J)                                                             
          X1=MIN(.8*CLA(J),5.+2.428*X2+1.7*ZZ)                                           
          CEC(J)=MAX(CEC(J),X1)                                                          
          SELECT CASE(ISW+1)                                                             
	          CASE(1,5)                                                                         
	              CALL SWRTNR(CLA(J),SAN(J),X2,U(J),FC(J))                                      
	          CASE(2,6)                                                                         
	              CEM(J)=MAX(.1,CEC(J)-2.428*X2-1.7*ZZ)
                  CALL SWRTNB(CEM(J),CLA(J),X2,SAN(J),U(J),FC(J),ZZ)                       
	          CASE(8,9)                                                                         
	              CALL SWNN(CLA(J),SAN(J),X2,U(J),FC(J))                                        
	      END SELECT                                                                          
	      IF(ROK(J)>99.)ROK(J)=90.
          XY=1.-ROK(J)*.01                                                               
          U(J)=U(J)*XY                                                                   
          XY=XY*DG                                                                       
          FC(J)=FC(J)*XY                                                                 
          CAP=CAP+FC(J)                                                                  
          S15(J)=U(J)*DG                                                                 
          PO(J)=PO(J)*XY                                                                 
          CALL SPOFC(J)                                                                  
          IF(ST(J)<1.E-10.AND.NCC==0)ST(J)=FFC                                           
          ST(J)=ST(J)*(FC(J)-S15(J))+S15(J)                                              
          SEV(1)=SEV(1)+XY                                                               
          SEV(3)=SEV(3)+WT(J)                                                            
          IF(HCL(J)<1.E-20)HCL(J)=UPS*SATC(J)                                            
          IF(CEC(J)>0.)THEN
              IF(CAC(J)>0.)SMB(J)=CEC(J)                                                     
              IF(SMB(J)>CEC(J))SMB(J)=CEC(J)                                                 
              BSA=SMB(J)*100./(CEC(J)+1.E-20)                                                
              IF(PH(J)>5.6)THEN
                  ALS(J)=0.
              ELSE
                  X1=.1*WOC(J)/WT(J)                                                             
                  ALS(J)=154.2-1.017*BSA-3.173*X1-14.23*PH(J)                                    
                  IF(ALS(J)<0.)THEN
                      ALS(J)=0.
                  ELSE
                      IF(ALS(J)>95.)ALS(J)=95.
                  END IF
              END IF
          ELSE
              CEC(J)=PH(J)                                                                   
              SMB(J)=CEC(J)                                                                  
              ALS(J)=0.
          END IF                                            
          SELECT CASE(IDSP)                                                              
	          CASE(1)                                                                           
	              IF(CAC(J)>0.)THEN                                                             
                      PSP(J)=.58-.0061*CAC(J)                                              
	                  BPC(J)=.00076                                                             
	              ELSE                                                                          
                      PSP(J)=.5                                                            
	                  BPC(J)=EXP(-1.77*PSP(J)-7.05)                                             
	              END IF                                                                        
              CASE(2)                                                                      
                  PSP(J)=.02+.0104*PKRZ(J)                                                 
                  BPC(J)=EXP(-1.77*PSP(J)-7.05)                                            
              CASE(3)                                                                      
                  PSP(J)=.0054*BSA+.116*PH(J)-.73                                          
                  BPC(J)=EXP(-1.77*PSP(J)-7.05)                                            
              CASE(4)                                                                      
                  PSP(J)=.46-.0916*LOG(CLA(J))                                            
                  BPC(J)=EXP(-1.77*PSP(J)-7.05)                                            
              CASE(5)                                                                      
	              IF(PSP(J)<1.)THEN                                                             
	                  IF(CAC(J)>0.)THEN                                                         
	                      PSP(J)=.58-.0061*CAC(J)                                               
	                      BPC(J)=.00076                                                         
	                  ELSE                                                                      
                          PSP(J)=.5                                                        
	                      BPC(J)=EXP(-1.77*PSP(J)-7.05)                                         
	                  END IF                                                                    
                  ELSE                                                                     
                      PMN(J)=.2*PSP(J)                                                     
                      PSP(J)=PKRZ(J)/(PMN(J)+PKRZ(J))                                      
	              END IF                                                                        
	      END SELECT                                                                          
	      IF(PSP(J)<.05)PSP(J)=.05                                                            
          IF(PSP(J)>.75)PSP(J)=.75                                                       
	      IF(PMN(J)<1.E-5)PMN(J)=PKRZ(J)*(1.-PSP(J))/PSP(J)	                               
          OP(J)=4.*PMN(J)                                                                
          SELECT CASE(IDSK)                                                              
	          CASE(1)                                                                           
	              SOLK(J)=MAX(.05*EXCK(J),.052*EXCK(J)-.12)                                     
                  FIXK(J)=374.+236.*CLA(J)                                                 
	          CASE(2)                                                                           
	              SOLK(J)=.026*EXCK(J)+.5                                                       
                  FIXK(J)=1781.+316.*CLA(J)                                                
	          CASE(3)                                                                           
	              SOLK(J)=.026*EXCK(J)+.5                                                       
                  FIXK(J)=1781.+316.*CLA(J)                                                
	      END SELECT                                                                          
          EQKS(J)=SOLK(J)/EXCK(J)                                                        
          EQKE(J)=EXCK(J)/FIXK(J)                                                        
          PMN(J)=PMN(J)*WT1                                                              
          OP(J)=OP(J)*WT1                                                                
          AP(J)=PKRZ(J)*WT1                                                              
          TAP=TAP+AP(J)                                                                  
          WSLT(J)=6.4*ECND(J)*ST(J)                                                      
          TSLT=TSLT+WSLT(J)                                                              
          TMP=TMP+PMN(J)                                                                 
          EXCK(J)=EXCK(J)*WT1                                                            
          SOLK(J)=SOLK(J)*WT1                                                            
          FIXK(J)=FIXK(J)*WT1                                                            
          WNO3(J)=CNDS(J)*WT1                                                            
          ZNO3=ZNO3+WNO3(J)                                                              
          IF(Z(J)<=RZ)THEN                                                               
              RZSW=RZSW+ST(J)-S15(J)                                                       
              PAW=PAW+FC(J)-S15(J)                                                         
              LZ=J                                                                         
	      END IF                                                                              
          IF(BDD(J)<1.E-5)BDD(J)=BD(J)                                                   
          BDP(J)=BD(J)                                                                   
          BDD(J)=BDD(J)/BD(J)                                                            
          TOP=TOP+OP(J)                                                                  
          TP=TP+WP(J)                                                                    
          TSK=TSK+SOLK(J)                                                                
          TEK=TEK+EXCK(J)                                                                
          TFK=TFK+FIXK(J)                                                                
          ZLS=ZLS+WLS(J)                                                                 
          ZLM=ZLM+WLM(J)                                                                 
          ZLSL=ZLSL+WLSL(J)                                                              
          ZLSC=ZLSC+WLSC(J)                                                              
          ZLMC=ZLMC+WLMC(J)                                                              
          ZLSLC=ZLSLC+WLSLC(J)                                                           
          ZLSLNC=ZLSLNC+WLSLNC(J)                                                        
          ZBMC=ZBMC+WBMC(J)                                                              
          ZHSC=ZHSC+WHSC(J)                                                              
          ZHPC=ZHPC+WHPC(J)                                                              
          ZLSN=ZLSN+WLSN(J)                                                              
          ZLMN=ZLMN+WLMN(J)                                                              
          ZBMN=ZBMN+WBMN(J)                                                              
          ZHSN=ZHSN+WHSN(J)                                                              
          ZHPN=ZHPN+WHPN(J)                                                              
          IF(K<=3)THEN                                                               
              TPAW=TPAW+FC(J)-S15(J)                                                         
              DO K1=K,3
                  IF(TPAW<WCS(K1))EXIT
                  ZCS(K1)=XX+(Z(J)-XX)*((WCS(K1)-PZW)/(TPAW-PZW))                                  
              END DO
              K=K1
          END IF
          PZW=TPAW                                                                       
          XX=Z(J)
      END DO
      IF(J>MSL)THEN
          NBSL=10                                                                        
      ELSE
          L1=LZ+1                                                                        
          NBSL=J-1                                                                       
          IF(L1/=J)THEN
              ZZ=RZ-Z(LZ)                                                                    
              RTO=ZZ/(Z(L1)-Z(LZ))                                                           
              RZSW=RZSW+(ST(L1)-S15(L1))*RTO                                                 
              PAW=PAW+RTO*(FC(L1)-S15(L1))
          END IF                                                   
      END IF
      LRD=NBSL                                                                        
      IF(MXLA<NBSL)MXLA=NBSL                                                         
      LD1=1                                                                          
      IF(Z(1)<.01)THEN
          Z(1)=.01
      ELSE
	      IF(Z(1)>.01)THEN
	          NBSL=NBSL+1                                                                    
              DO J=NBSL,2,-1                                                                         
                  LID(J)=LID(J-1)
              END DO                                                                
              LID(1)=NBSL                                                                    
              LD1=NBSL                                                                       
              LORG(NBSL)=1                                                                   
              RTO=.01/Z(1)                                                                   
              CALL SPLA(1,1,NBSL,0,RTO)                                                      
              Z(NBSL)=.01 
          END IF                                                                   
      END IF
      DO WHILE(NBSL<MXLA)
          L1=LID(1)                                                                      
          ZMX=0.                                                                         
          MXZ=2                                                                          
          DO J=2,NBSL                                                                    
              L=LID(J)                                                                     
              ZZ=Z(L)-Z(L1)                                                                
              IF(ZZ>=ZTK)THEN
                  MXZ=J                                                                        
                  GO TO 130      
              ELSE                                                              
                  IF(ZZ>ZMX+.01)THEN
                      ZMX=ZZ                                                                       
                      MXZ=J
                  END IF                                                                        
              END IF                                                                        
              L1=L
          END DO                                                                         
          L=LID(MXZ)                                                                     
          L1=LID(MXZ-1)                                                                  
      130 NBSL=NBSL+1                                                                    
          CALL SPLA(L,L1,NBSL,1,.5)                                                      
          DO J=NBSL,MXZ,-1                                                               
              LID(J)=LID(J-1)                                                              
          END DO                                                                         
          LID(MXZ)=NBSL                                                                  
          LORG(NBSL)=LORG(L)                                                             
      END DO                                                                      
      DO I=1,13                                                                      
          DO J=1,16                                                                    
              XZP(I,J)=0.                                                              
	      END DO                                                                            
      END DO
      Z1=0.                                                                         
      DO J=1,NBSL                                                                
          L=LID(J)
          ZZ=Z(L)-Z1
          ACO2(L)=CGO2(L)
          XTP1(L)=CGCO2(L)
          XTP2(L)=CGN2O(L)                                                                       
          IF(NCC>0)THEN                                                                  
              WOC(L)=WLSC(L)+WLMC(L)+WBMC(L)+WHSC(L)+WHPC(L)                               
              WON(L)=WLSN(L)+WLMN(L)+WBMN(L)+WHSN(L)+WHPN(L)                               
          ELSE                                                                           
              WLSC(L)=.42*WLS(L)                                                           
              WLMC(L)=.42*WLM(L)                                                           
              WLSLC(L)=.42*WLSL(L)                                                         
              WLSLNC(L)=WLSC(L)-WLSLC(L)                                                   
	      END IF                                                                              
          XZP(1,L)=WHSC(L)                                                               
          XZP(2,L)=WHPC(L)                                                               
          XZP(3,L)=WLSC(L)                                                               
          XZP(4,L)=WLMC(L)                                                               
          XZP(5,L)=WBMC(L)                                                               
          XZP(6,L)=WOC(L)                                                                
          XZP(7,L)=WHSN(L)                                                               
          XZP(8,L)=WHPN(L)                                                               
          XZP(9,L)=WLSN(L)                                                               
          XZP(10,L)=WLMN(L)                                                              
          XZP(11,L)=WBMN(L)                                                              
          XZP(12,L)=WON(L)                                                               
          XZP(13,L)=WOC(L)/WON(L)                                                        
	      SOL(1,L)=WHSC(L)                                                                    
          SOL(2,L)=WHPC(L)                                                               
          SOL(3,L)=WLSC(L)                                                               
          SOL(4,L)=WLMC(L)                                                               
          SOL(5,L)=WBMC(L)                                                               
          SOL(6,L)=WOC(L)                                                                
          SOL(7,L)=WHSN(L)                                                               
          SOL(8,L)=WHPN(L)                                                               
          SOL(9,L)=WLSN(L)                                                               
          SOL(10,L)=WLMN(L)                                                              
          SOL(11,L)=WBMN(L)                                                              
          SOL(12,L)=WON(L)                                                               
	      SOL(13,L)=PMN(L)                                                                    
          SOL(14,L)=WP(L)                                                                
          SOL(15,L)=OP(L)                                                                
          SOL(16,L)=EXCK(L)                                                              
          SOL(17,L)=FIXK(L)                                                              
          SOL(18,L)=ST(L)                                                                
	      SOL(19,L)=WLS(L)                                                                    
	      SOL(20,L)=WLM(L)                                                                    
	      SOL(21,L)=WLSL(L)                                                                   
          SOL(22,L)=WLSLC(L)                                                             
          SOL(23,L)=WLSLNC(L)                                                            
          IF(Z(L)<=PMX)THEN                                                              
              SUM=SUM+WT(L)                                                                
              APB=APB+AP(L)                                                                
              OCPD=OCPD+WOC(L)                                                             
              PDSW=PDSW+ST(L)-S15(L)                                                       
              FCSW=FCSW+FC(L)-S15(L)                                                       
              MXP=J                                                                          
	      END IF
          WNO2(L)=0.
          WN2O(L)=0.
          Z1=Z(L)                                                                         
	  END DO
      IF(MXP==NBSL)THEN                                                                
          PMX=Z(LID(NBSL))                                                             
      ELSE                                                                           
          L1=LID(MXP+1)                                                                  
          X1=0.                                                                        
          IF(MXP>0)X1=Z(LID(MXP))                                                          
          RTO=(PMX-X1)/(Z(L1)-X1)                                                      
          SUM=SUM+WT(L1)*RTO                                                           
          APB=APB+AP(L1)*RTO                                                           
          OCPD=OCPD+WOC(L1)*RTO                                                        
          PDSW=PDSW+RTO*(ST(L1)-S15(L1))                                               
          FCSW=FCSW+RTO*(FC(L1)-S15(L1))                                               
      END IF
      WPMX=.001*SUM                                                                         
!     OCPD=.1*OCPD/SUM                                                               
      APBC=APB/WPMX                                                             
      OCPD=.001*OCPD                                                                 
      ABD=1.E-4*SEV(3)/XX                                                            
      TWN=ZLSN+ZLMN+ZBMN+ZHSN+ZHPN                                                   
      TOC=ZLSC+ZLMC+ZBMC+ZHSC+ZHPC                                                   
	  TWN0=TWN                                                                            
      XZP(1,16)=ZHSC                                                                 
      XZP(2,16)=ZHPC                                                                 
      XZP(3,16)=ZLSC                                                                 
      XZP(4,16)=ZLMC                                                                 
      XZP(5,16)=ZBMC                                                                 
      XZP(6,16)=TOC                                                                  
      XZP(7,16)=ZHSN                                                                 
      XZP(8,16)=ZHPN                                                                 
      XZP(9,16)=ZLSN                                                                 
      XZP(10,16)=ZLMN                                                                
      XZP(11,16)=ZBMN                                                                
      XZP(12,16)=TWN                                                                 
      XZP(13,16)=TOC/TWN
      IF(IDN>2)THEN
          NBCL=Z(LID(NBSL))/DZ+.999
          IF(NBCL>30)THEN
              NBCL=30
              DZ=Z(LID(NBSL))/30.
          ELSE
              IF(NBCL<NBSL)THEN
                  NBCL=NBSL
                  X1=NBCL
                  DZ=Z(LID(NBSL))/X1
              END IF              
          END IF
          DZ10=10.*DZ
          TOT=0.
          DO I=1,NBCL
              TOT=TOT+DZ
              ZC(I)=TOT
          END DO
          CALL AINTRIC(ACO2,CGO2,NBSL,NBCL)
          CALL AINTRIC(XTP1,CGCO2,NBSL,NBCL)
          CALL AINTRIC(XTP2,CGN2O,NBSL,NBCL)
          IUN=NBCL-1                                                             
      END IF                                                             
      CALL SPRNT(YTP)                                                                
      WRITE(KW(1),'(//1X,A/)')'____________________SOIL DATA____________________'
      WRITE(KW(1),'(T10,2A)')'SOIL = ',TRIM(SOILFILE)
      WRITE(KW(1),'(T10,2A)')'SOIL SERIES = ',TRIM(SOLS)
      WRITE(KW(1),'(T10,2A)')'SOIL ORDER = ',TRIM(SOLO)
      WRITE(KW(1),290)SALB,TSLA,ZQT,ZF,ZTK,FBM,FHP,RTN0,XIDP,XIDK,PMX,&              
      OCPD                                                                           
      WRITE(KW(1),351)WTMN,WTMX,WTBL,GWST,GWMX,RFTT                                  
      IF(ISTA>0)WRITE(KW(1),'(T10,A)')'STATIC SOIL PROFILE'                          
      SELECT CASE(ISW+1)                                                             
	      CASE(1)                                                                           
	          WRITE(KW(1),'(T10,A)')'FC/WP EST RAWLS METHOD DYNAMIC'                        
	      CASE(2)                                                                           
	          WRITE(KW(1),'(T10,A)')'FC/WP EST BAUMER METHOD DYNAMIC'                       
	      CASE(3)                                                                           
	          WRITE(KW(1),'(T10,A)')'FC/WP INP RAWLS METHOD DYNAMIC'                        
	      CASE(4)                                                                           
	          WRITE(KW(1),'(T10,A)')'FC/WP INP BAUMER METHOD DYNAMIC'                       
	      CASE(5)                                                                           
	          WRITE(KW(1),'(T10,A)')'FC/WP EST RAWLS METHOD STATIC'                         
	      CASE(6)                                                                           
	          WRITE(KW(1),'(T10,A)')'FC/WP EST BAUMER METHOD STATIC'                        
	      CASE(7)                                                                           
	          WRITE(KW(1),'(T10,A)')'FC/WP INP STATIC'                                      
	      CASE(8)                                                                           
	          WRITE(KW(1),'(T10,A)')'FC/WP INP NEAREST NEIGHBOR METHOD'&                   
              ' DYNAMIC'                                                               
	      CASE(9)                                                                           
	          WRITE(KW(1),'(T10,A)')'FC/WP INP NEAREST NEIGHBOR METHOD'&                   
              ' STATIC'                                                               
	  END SELECT
	  IF(ISAT>0)THEN
	      WRITE(KW(1),'(T10,A)')'SAT COND ESTIMATED WITH RAWLS METHOD'
      ELSE
	      WRITE(KW(1),'(T10,A)')'SAT COND INPUT'
	  END IF                           
      SATK=SATC(LID(2))
      WRITE(KW(1),'(T10,A,F10.3)')'INITIAL STANDING DEAD = ',STD0                                                              
      II=-1
      DO WHILE(II/=IOPS)                                                                             
          READ(KR(15),*,IOSTAT=NFL)II,OPSCFILE                                         
	      IF(NFL/=0)THEN
	          IF(IBAT==0)THEN
	              WRITE(*,*)'OPS NO = ',IOPS,' NOT IN OPSC LIST FILE'
#                     if defined(DEBUG) .AND. defined(PAUSE_ENABLE)
                        PAUSE
#                     endif
		      STOP 20
              	  ELSE
                      WRITE(KW(MSO),'(3A,I4,A)')' !!!!! ',TRIM(ASTN),&
                        &' OPS NO = ',IOPS,' NOT IN OPSC LIST FILE'
                      GO TO 219
                  END IF
              END IF
	  END DO                                                                              
	  REWIND KR(15)                                                                  

      CALL OPENV(KR(16),OPSCFILE,KW(MSO))                                            
!     LINE 1                                                                         
      READ(KR(16),505)VOID                                                           
!     LINE 2                                                                         
      READ(KR(16),300)LUN,IAUI                                                       
      IF(IAUI==0)IAUI=500                                                            
      ISG=HSG                                                                        
      CALL HSGCN                                                                     
      CALL HCNSLP(CN2,X3)                                                            
      WRITE(KW(1),390)ASG(ISG),LUN,CN2,X3,SCRP(30,1),SCRP(30,2),SCRP(4,1),&              
      SCRP(4,2)                                                                      
      CN2=X3                                                                         
      CN0=CN2                                                                        
      CALL SOLIO(YTP,1)                                                              
      REWIND KR(14)                                                                  
      YTP(2)=YTP(2)*XX*1000.                                                         
      SW=YTP(2)                                                                      
      SWW=YTP(2)+SNO                                                                 
      SLT0=TSLT                                                                      
      ! IF(XX<1.)SMX=SMX*SQRT(XX)                                                      
	  SCI=MAX(3.,SMX*(1.-RZSW/PAW))                                                       
      CALL APAGE(1)
      RNMN=0.                                                                  
      IF(IWRT==0)THEN
          IWRT=1                                                                         
          DO I=2,MSO                                                                 
              IF(KFL(I)==0)CYCLE                                                         
              IF(I==9)THEN
                  XCC=1.                                                                         
                  X1=0.                                                                          
                  WRITE(KW(9),523) TRIM(ADJUSTL(SOLS)),TRIM(ADJUSTL(SOLO)),IYER,IMON,IDAY
                  IF (SoilOutputFmtSelect == 1) THEN
                     SoilOutputFormat1 = '(10(G10.2,1X))'
                  ELSE
                     SoilOutputFormat1 = '(10F8.3)'
                  END IF
                  WRITE(KW(9),SoilOutputFormat1) SALB,HSG,FFC,WTMN,WTMX,WTBL,GWST,GWMX,RFT0,&     
                       RFPK,TSLA,XIDP,X1,XIDK,ZQT,ZF,ZTK,FBM,FHP,XCC
                  CYCLE
              END IF                                                                      
                                
              ! DManowitz - 3/1/10: Only report the date (no time) for comparing Win & Linux versions                                              
              ! David Manowitz (3/8/2011): Now making outputs dependent on ComparisonMode flag.
              IF (ComparisonMode .EQV. .FALSE.) THEN
                  WRITE(KW(I),621) VersionStr, TRIM(CommandName),IYER,IMON,IDAY,IT1,IT2,IT3                                     
              ELSE
                  WRITE(KW(I),622) VersionStr
              END IF
              
              WRITE(KW(I),'(T10,A)') TRIM(ASTN)
              WRITE(KW(I),286)IRUN,IRO0,IGN                                                  
              
              ! DManowitz - 3/1/10: Only report the base file name (not the full path)
              ! for comparing Win & Linux versions
              
              ! David Manowitz (3/8/2011): Now making outputs dependent on ComparisonMode flag.
              IF (ComparisonMode .EQV. .TRUE.) THEN
                  ! DManowitz - 3/1/10: Only report the base file name (not the full path) for 
                  ! comparing Win & Linux versions
                  SHORTFILE = ""
                  POS = SCAN (SITEFILE, '/\\', BACK = .TRUE.)
                  IF (POS > 0) THEN
                      SHORTFILE = SITEFILE (POS + 1:)
                  ELSE
                      SHORTFILE = SITEFILE
                  END IF
                  WRITE (KW(I),592) TRIM (SHORTFILE)
              ELSE
                  WRITE (KW(I),592) TRIM (SITEFILE)
              END IF
      
              IF (ComparisonMode .EQV. .TRUE.) THEN
                  ! DManowitz - 3/1/10: Only report the base file name (not the full path) for 
                  ! comparing Win & Linux versions
                  SHORTFILE = ""
                  POS = SCAN (WPM1FILE, '/\\', BACK = .TRUE.)
                  IF (POS > 0) THEN
                      SHORTFILE = WPM1FILE (POS + 1:)
                  ELSE
                      SHORTFILE = WPM1FILE
                  END IF
                  WRITE (KW(I),592) TRIM (SHORTFILE)
              ELSE
                  WRITE (KW(I),592) TRIM (WPM1FILE)
              END IF

              IF (ComparisonMode .EQV. .TRUE.) THEN
                  ! DManowitz - 3/1/10: Only report the base file name (not the full path) for 
                  ! comparing Win & Linux versions
                  SHORTFILE = ""
                  POS = SCAN (WINDFILE, '/\\', BACK = .TRUE.)
                  IF (POS > 0) THEN
                      SHORTFILE = WINDFILE (POS + 1:)
                  ELSE
                      SHORTFILE = WINDFILE
                  END IF
                  WRITE (KW(I),592) TRIM (SHORTFILE)
              ELSE
                  WRITE (KW(I),592) TRIM (WINDFILE)
              END IF
              
              IF (ComparisonMode .EQV. .TRUE.) THEN
                  ! DManowitz - 3/1/10: Only report the base file name (not the full path) for 
                  ! comparing Win & Linux versions
                  SHORTFILE = ""
                  POS = SCAN (SOILFILE, '/\\', BACK = .TRUE.)
                  IF (POS > 0) THEN
                      SHORTFILE = SOILFILE (POS + 1:)
                  ELSE
                      SHORTFILE = SOILFILE
                  END IF
                  WRITE (KW(I),592) TRIM (SHORTFILE) 
              ELSE
                  WRITE (KW(I),592) TRIM (SOILFILE)
              END IF
              
              IF (ComparisonMode .EQV. .TRUE.) THEN
                  ! DManowitz - 3/1/10: Only report the base file name (not the full path) for 
                  ! comparing Win & Linux versions
                  SHORTFILE = ""
                  POS = SCAN (OPSCFILE, '/\\', BACK = .TRUE.)
                  IF (POS > 0) THEN
                      SHORTFILE = OPSCFILE (POS + 1:)
                  ELSE
                      SHORTFILE = OPSCFILE
                  END IF
                  WRITE (KW(I),592) TRIM (SHORTFILE)
              ELSE
                  WRITE (KW(I),592) TRIM (OPSCFILE)
              END IF

              IF(I==14)CALL SOCIOA(IYR0,1,1)                                                 
              IF(I==15)CALL SOCIOD(1)
          END DO
      END IF                                                                  
      IF(KFL(2)>0)WRITE(KW(2),557)HED(4),HED(10),HED(11),HED(14),HED&                
      (16),HED(17),HED(29),HED(33),HED(42),HED(48),HED(47),HED(50),HED&              
      (51),HED(52),HED(49),HED(43),HED(44),HED(45),HED(46),HED(56),HED&              
      (54),HED(55),HED(57),HED(66)                                                   
	  IF(KFL(19)>0.AND.K19==0)WRITE(KW(19),551)                                           
	  K19=1                                                                               
      IF(KFL(8)>0)WRITE(KW(8),558)(HED(KYA(J1)),J1=1,NKYA)                           
      IF(KFL(13)>0)WRITE(KW(13),583)                                                 
!    DManowitz - 6/7/10: Always print out STL & STD for now.      
      IF(KFL(17)>0)WRITE(KW(17),826)(HED(KD(J1)),J1=1,NKD),'ZNO3','NO31',&                  
        'PRK1','LN31',' ALB',' HUI',' LAI','BIOM','  RW',' STL',' STD','  HI',&
	    'YLDX','YLDF','UNO3',' NPP',' NEE',(HEDS(I),I=23,28)                                                                       
      IF(KFL(18)>0)THEN
          IF(NGN>0)THEN
              A1='INPUT WEATHER'
              A2=TRIM(FWTH)
          ELSE
              A1='GENERATED WEATHER'
              A2=' '
          END IF
          WRITE(KW(18),671)TRIM(ASTN),YLAT,YLAZ,UPS,ZCS,A1,A2                              
      END IF
      IF(KFL(4)>0)WRITE(KW(4),669)                                                   
      IF(KFL(20)>0)WRITE(KW(20),583)                                                 
	  IF(KFL(24)>0)WRITE(KW(24),726)                                                      
	  IF(KFL(26)>0)WRITE(KW(26),729)                                                      
	  IF(KFL(29)>0)WRITE(KW(29),727)                                                      
      IF(KFL(23)>0)THEN                                                              
	      WRITE(KW(23),722)(SID(LORG(LID(J))),J=1,NBSL),SID(16)                               
	      WRITE(KW(23),723)'DEPTH(m)',(Z(LID(I)),I=1,NBSL)                                    
	  END IF                                                                              
      WRITE(KW(1),'(//1X,A/)')'____________________MANAGEMENT DATA_____________________'
      WRITE(KW(1),'(T10,2A)')'OPSC = ',TRIM(OPSCFILE)
      NCOW=WSA/RST0                                                                                                                                                                                  
      IF(IRR==0)THEN
          VIMX=0.                                                                        
          WRITE(KW(1),'(T10,A)')'DRYLAND AGRICULTURE'                                    
          GO TO 142
      ELSE
          IF(VIMX<1.E-10)VIMX=2000.                                                      
          IF(IRR==4)THEN
              WRITE(KW(1),331)                                                               
              GO TO 137
          END IF
      END IF                                                                      
      IF(BIR<0.)THEN
          WRITE(KW(1),331)                                                               
          WRITE(KW(1),412)BIR,IRI                                                        
          GO TO 137
      END IF                             
      IF(BIR>0.)THEN
          WRITE(KW(1),331)                                                               
          IF(BIR>1.)THEN                                                                 
              WRITE(KW(1),355)BIR,IRI                                                      
          ELSE                                                                           
              WRITE(KW(1),360)BIR,IRI                                                      
          END IF                                                                         
      ELSE
          WRITE(KW(1),'(T10,A)')'USER SPECIFIED IRRIGATION'
      END IF                              
  137 WRITE(KW(1),354)VIMX,ARMN,ARMX                                                 
      IF(IAC==0)THEN                                                                 
          WRITE(KW(1),'(T15,A)')'VARIABLE APPL VOLUMES'                                
      ELSE                                                                           
          WRITE(KW(1),'(T15,A)')'FIXED APPL VOLUMES'                                   
      END IF
      SELECT CASE(IRR)                                                               
          CASE(1)                                                                      
              WRITE(KW(1),'(T10,A)')'SPRINKLER IRRIGATION'                             
          CASE(2)                                                                      
              WRITE(KW(1),'(T10,A)')'FURROW IRRIGATION'                                
          CASE(3)                                                                      
              WRITE(KW(1),'(T10,A)')'IRRIGATION WITH FERT ADDED'                       
              WRITE(KW(1),'(T15,A,F6.3)')'RUNOFF RATIO = ',EFI                         
          CASE(5)                                                                      
              WRITE(KW(1),'(T10,A)')'DRIP IRRIGATION'                                  
          CASE(4)
              X3=COWW*NCOW                                                             
              X1=RFMX-12.7                                                             
              QLG=X1*X1/(RFMX+50.8)                                                    
              DALG=SOLQ*WSA                                                            
              X1=10.*DALG                                                              
              QWW=30.*X3/X1                                                            
              VLGM=QLG+QWW                                                             
              VLGN=VLGN*VLGM                                                           
              WRITE(KW(1),445)DALG,VLGN,VLGM,DDLG,COWW                                 
              COWW=X3                                                                  
              VLGB=VLGN                                                                
              VLGN=X1*VLGN                                                             
              VLGM=X1*VLGM                                                             
              VLG=VLGN                                                                 
              CFNP=100.                                                                
              WTMU=CFNP*VLG                                                            
              WTMB=WTMU                                                                
              VLGI=(VLGM-VLGN)/(DDLG+1.E-5)                                            
              FNPI=NCOW*FNP*FFED                                                       
              AFLG=365.*FNPI/WSA                                                       
              AILG=.1*(AAP*DALG+365.*X3)/WSA                                           
              COWW=X3                                                                  
              GO TO 433
          CASE DEFAULT                                                                              
      END SELECT                                                                     
  142 IF(BFT0<1.E-10)THEN
          IAUF=0                                                                         
          IF(MNU==0)THEN
              WRITE(KW(1),'(T10,A)')'USER SCHEDULED FERT'                                    
              GO TO 433
          END IF
      END IF                                                                      
      WRITE(KW(1),'(T10,A/T15,A,I3,A)')'AUTO SCHEDULED FERT','MIN APPL I&            
     &NTERVAL = ',IFA,' D'                                                           
      IAUF=1                                                                         
      IF(BFT0>1.)THEN                                                                
          WRITE(KW(1),'(T15,A,F4.0,A)')'SOIL NO3 CONC TRIGGER = ',BFT0,&               
          &' g/t'                                                                       
      ELSE                                                                           
          WRITE(KW(1),'(T15,A,F4.2)')'PLANT STRESS TRIGGER = ',BFT0                    
      END IF                                                                         
      IF(FNP>1.)THEN                                                                 
          WRITE(KW(1),'(T15,A,F8.1,A)')'FIXED RATE = ',FNP,' kg/ha'                    
      ELSE                                                                           
          WRITE(KW(1),'(T15,A)')'VARIABLE RATE'                                        
      END IF                                                                         
  433 WRITE(KW(1),'(T15,A,F7.0,A)')'MAX N FERT/CROP = ',FMX,' kg/ha'
      IF(IPAT>0)THEN
          WRITE(KW(1),'(T10,A)')'AUTO P FERT'
      ELSE
          WRITE(KW(1),'(T10,A)')'MANUAL P FERT'
      END IF                 
      WRITE(KW(1),436)NCOW,GZLM,FFED                                                 
	  NII=IRI                                                                        
      FDSF=FDS0                                                                      
      IF(FDS0<1.E-10)FDSF=.9                                                         
      IDR=IDR0                                                                       
      IF(IDR0>0)THEN
          X1=.001*IDR0                                                                   
          DO I=1,NBSL                                                                    
              L=LID(I)                                                                     
              IF(Z(L)>X1)EXIT                                                              
	      END DO                                                                              
          IDR=L                                                                          
          HCLN=HCL(L)                                                                    
          HCL(L)=MAX(10.*SATC(L),(PO(L)-S15(L))/(24.*DRT))                               
	      HCLD=HCL(L)                                                                         
          WRITE(KW(1),327)L,DRT,HCL(L)                                                   
          IF(DRT<RFTT)RFTT=DRT
      END IF                                                                     
      IF(IFD>0)WRITE(KW(1),'(T10,A,F5.2)')'FURROW DIKE SYSTEM SAFETY FAC&            
     &TOR = ',FDSF                                                                   
      WRITE(KW(1),'(T10,A,F5.3)')'USLE P FACTOR = ',PEC                              
      RFTT=1.-EXP(-1./RFTT)                                                          
      FDSF=FDSF*1000.                                                                
      IF(LMS==0)THEN                                                                 
          WRITE(KW(1),'(T10,A)')'LIME APPLIED AS NEEDED'                               
      ELSE                                                                           
          WRITE(KW(1),'(T10,A)')'NO LIME APPLICATIONS'                                 
      END IF                                                                         
      HU(1)=0.                                                                       
      IBGN=1                                                                         
      JJK=1                                                                          
      I=1                                                                            
      IY1=1                                                                          
      K1=1                                                                           
      WRITE(KW(1),'(/1X,A)')'-----OPERATION SCHEDULE'                                
  189 NCRP=IGO                                                                       
      I1=I-1                                                                         
      K=1                                                                            
	  WRITE(KW(1),'(/T10,A,I2)')'YR',I                                                    
      KI=0                                                                           
      KF=0                                                                           
      KP=0                                                                           
      HU0=0.                                                                         
      IF(JDHU<366)HU(JJK)=0.                                                         
      IF(IGO>0)THEN
          DO J=1,MNC                                                                     
              IF(NHU(J)==0)CYCLE                                                           
              LY(I,K)=NHU(J)                                                               
              K=K+1                                                                        
	      END DO                                                                              
          J=0
      END IF                                                                                      
      WRITE(KW(1),316)
      J=0                                                               
      DO  
          J=J+1
    !     READ OPERATION SCHEDULE                                                        
    !  1  JX(1)= YR OF OPERATION                                                         
    !  2  JX(2)= MO OF OPERATION                                                         
    !  3  JX(3)= DAY OF OPERATION                                                        
    !  4  JX(4)= EQUIPMENT ID NO                                                         
    !  5  JX(5)= TRACTOR ID NO                                                           
    !  6  JX(6)= CROP ID NO                                                              
    !  7  JX(7)= XMTU--TIME FROM PLANTING TO MATURITY (Y)(FOR TREE                       
    !            CROPS AT PLANTING ONLY).                                                
    !          = TIME FROM PLANTING TO HARVEST (Y)(HARVEST ONLY)                         
    !          = PESTICIDE ID NO (FOR PESTICIDE APPLICATION ONLY)                        
    !          = FERTILIZER ID NO (FOR FERTILIZER APPLICATION ONLY)                      
    !  8  OPV1 = POTENTIAL HEAT UNITS FOR PLANTING (BLANK IF UNKNOWN)                    
    !          = APPLICATION VOLUME (mm)FOR IRRIGATION                                   
    !          = FERTILIZER APPLICATION RATE (kg/ha) = 0 FOR VARIABLE RATE               
    !          = PEST CONTROL FACTOR FOR PEST APPLICATION (FRACTION OF PESTS             
    !            CONTROLLED)
    !          = LIME APPLICATION RATE (t/ha)                                                                            
    !  9  OPV2 = LINE NUMBER FOR SCS HYDROLOGIC SOIL GROUP/RUNOFF CURVE                  
    !            NUMBER TABLE                                                            
    !          = SCS CURVE NUMBER (NEGATIVE)                                             
    !          = PESTICIDE APPLICATION RATE (kg/ha)                                      
    !  10 OPV3 = PLANT WATER STRESS FACTOR(0-1); SOIL WATER TENSION(>1kpa);              
    !            OR PLANT AVAILABLE WATER DEFICIT IN ROOT ZONE(-mm)TO                    
    !            TRIGGER AUTO IRR. (0. OR BLANK DOES NOT CHANGE TRIGGER)                 
    !  11 OPV4 = RUNOFF VOL/VOL IRRIGATION WATER APPLIED                                 
    !  12 OPV5 = PLANT POPULATION (PLANTS/M**2 OR PLANTS/HA IF p/m2<1.)                  
    !            (FOR PLANTING ONLY)                                                     
    !  13 OPV6 = MAX ANNUAL N FERTILIZER APPLIED TO A CROP (0. OR BLANK                  
    !            DOES NOT CHANGE FMX; > 0 SETS NEW FMX)(FOR PLANTING ONLY                
    !  14 OPV7 = TIME OF OPERATION AS FRACTION OF GROWING SEASON (ENTER                  
    !            EARLIEST POSSIBLE MO & DAY -- JX(2) & JX(3))
    !  15 OPV8 = MINIMUM USLE C FACTOR
    !  16 OPV9 = MOISTURE CONTENT OF GRAIN REQUIRED FOR HARVEST                            
    !     LINE 3/L                                                                       
          IF(I==1.OR.J>1)READ(KRX,235,IOSTAT=NFL)JX,(OPV(L),L=1,9)
          IF(NFL/=0)EXIT
          IF(I==1.AND.J==1)JJK0=JX(6)                                                    
          IF(JX(1)/=IY1)EXIT
          CALL TILTBL                                                                    
          LT(I,J)=NDT                                                                    
          JH(I,J)=JX(6)                                                                  
          IJ=LT(I,J)                                                                     
          CALL ADAJ(NC,ITL(I,J),JX(2),JX(3),1)                                           
          X4=TLD(IJ)*1000.                                                               
          I3=IHC(IJ)                                                                     
          IF(IBGN<ITL(I,J))GO TO 419                                                     
          IF(IBGN==ITL(I,J))GO TO 422                                                    
          IF(IGO>0)HU(JJK)=HU(JJK)+CAHU(IBGN,365,BASE,0)/&                               
          (PHU(JJK,IHU(JJK))+1.)                                                         
          IBGN=1                                                                         
      419 IF(IGO>0)HU(JJK)=HU(JJK)+CAHU(IBGN,ITL(I,J),BASE,0)/&                          
          (PHU(JJK,IHU(JJK))+1.)                                                         
          HU0=HU0+CAHU(IBGN,ITL(I,J),0.,1)/AHSM                                          
          IBGN=ITL(I,J)                                                                  
      422 IF(OPV(7)>0.)GO TO 420                                                         
          IF(IHUS==0)GO TO 420                                                           
          IF(IGO==0)GO TO 441                                                            
          IF(IDC(JJK)==NDC(1).OR.IDC(JJK)==NDC(2).OR.IDC(JJK)==NDC(4)&                   
          .OR.IDC(JJK)==NDC(5).OR.IDC(JJK)==NDC(9))GO TO 423                             
      441 HUSC(I,J)=HU0                                                                  
          GO TO 421                                                                      
      423 HUSC(I,J)=HU(JJK)                                                              
          GO TO 421                                                                      
      420 HUSC(I,J)=OPV(7)                                                               
      421 CALL INIFP(I3,I,J,JRT)                                                         
          X1=MAX(0.,COOP(IJ))                                                            
          X2=MAX(0.,COTL(IJ))                                                            
    !     PRINTOUT OPERATION SCHEDULE                                                    
          WRITE(KW(1),317)I,JX(2),JX(3),TIL(IJ),I3,JX(4),JX(5),X1,X2,EMX(IJ)&            
          ,RR(IJ),X4,FRCP(IJ),RHT(IJ),RIN(IJ),DKH(IJ),DKI(IJ),HE(IJ),ORHI&               
          (IJ),CN2,BIR,EFI,HUSC(I,J)                                                     
          X1=0.                                                                          
          IF(TLD(IJ)>BIG)BIG=TLD(IJ)                                                     
          IF(I3/=NHC(5).AND.I3/=NHC(6))GO TO 180                                         
          NCRP=NCRP+1                                                                    
          IGO=IGO+1                                                                      
          CALL CPTBL                                                                     
	      NBCX(I,JJK)=NBCX(I,JJK)+1                                                           
          BASE=TBSC(JJK)                                                                 
          IPL=ITL(I,J)+365*I1                                                            
          IPLD(1,JJK)=IPL                                                                
          LY(I,K)=JJK                                                                    
          NHU(JJK)=JJK                                                                   
          K=K+1                                                                          
          X1=SDW(JJK)*CSTS(JJK)                                                          
          X2=X1+X2                                                                       
          WRITE(KW(1),236)X1,CPNM(JJK)                                                   
          GO TO 584                                                                      
      180 IF(I3/=NHC(1).AND.I3/=NHC(2).AND.I3/=NHC(3))GO TO 584                          
          IF(KDC1(JX(6))==0)GO TO 584                                                    
          JJK=KDC1(JX(6))                                                                
          IF(I3==NHC(1))THEN
              IHV=ITL(I,J)+365*I1                                                            
              IHVD(1,JJK)=IHV                                                                
              NHU(JJK)=0                                                                     
              IGO=MAX(0,IGO-1)                                                               
	          IF(NBCX(I,JJK)==0)NBCX(I,JJK)=1
	      END IF                                                     
          HU(JJK)=0.                                                                     
          LYR(I,J)=MAX(1,JX(7))                                                          
          WRITE(KW(1),236)HU(JJK),CPNM(JJK)                                              
      584 IF(KFL(13)>0)WRITE(KW(13),582)I,JX(2),JX(3),TIL(IJ),JX(6),I3,&                 
         &JX(4),JX(5),X2,COOP(IJ),X1
      END DO                                                     
      ITL(I,J)=367                                                                   
      HUSC(I,J)=10.                                                                  
      NBC(I)=NCRP                                                                    
	  J1=J-1                                                                              
      NTL(I)=J1                                                                      
      NPST(I)=KP                                                                     
      LT(I,J)=1                                                                      
      JH(I,J)=0                                                                      
      CND(I,J)=CN2                                                                   
      QIR(I,J)=EFI                                                                   
      TIR(I,J)=BIR                                                                   
	  CFRT(I,J)=CFMN
	  HWC(I,J)=0.                                                                      
      MO=1                                                                           
      IF(KI==0)GO TO 185                                                             
      WRITE(KW(1),247)                                                               
      DO L=1,J1                                                                  
          J2=LT(I,L)                                                                     
          IF(IHC(J2)/=NHC(8))CYCLE
          JDA=ITL(I,L)                                                                   
          IF(NYD==0)JDA=JDA+1                                                            
          CALL AXMON(JDA,MO)                                                             
          CALL AICL                                                                      
          XZ=COIR*VIRR(I,L)                                                              
          XY=MAX(0.,COTL(J2))+XZ                                                         
    !     PRINTOUT IRRIGATION SCHEDULE                                                   
          WRITE(KW(1),230)I,MO,KDA,VIRR(I,L),XY,HUSC(I,L)                                
          IF(KFL(13)>0)WRITE(KW(13),503)I,MO,KDA,JH(I,L),IHC(J2),XY,XZ,&                 
         &VIRR(I,L)
      END DO                                                                              
      MO=1                                                                           
  185 IF(KF==0)GO TO 187                                                             
      WRITE(KW(1),328)                                                               
      JJ=367                                                                         
      KK=0                                                                           
      DO L=1,J1                                                                  
          J2=LT(I,L)                                                                     
          IF(IHC(J2)/=NHC(9))CYCLE
          X1=MAX(0.,COTL(J2))                                                            
          JDA=ITL(I,L)                                                                   
          IF(NYD==0)JDA=JDA+1                                                            
          IF(JDA==JJ.AND.NBT(J2)==0.AND.NBE(J2)==KK)X1=0.
          JJ=JDA                                                                         
          KK=NBE(J2)                                                                     
          CALL AXMON(JDA,MO)                                                             
          CALL AICL                                                                      
          M=LFT(I,L)                                                                     
          XZ=FCST(M)*WFA(I,L)                                                            
          XY=X1+XZ                                                                       
    !     PRINTOUT FERTILIZER SCHEDULE                                                   
          WRITE(KW(1),329)I,MO,KDA,FTNM(M),KDF(M),NBE(J2),NBT(J2),XY,WFA&                
         &(I,L),TLD(J2),FN(M),FNH3(M),FNO(M),FP(M),FPO(M),FK(M),HUSC(I,L)                
          IF(KFL(13)>0)WRITE(KW(13),510)I,MO,KDA,FTNM(M),JH(I,L),KDF(M),&                
         &IHC(J2),NBE(J2),NBT(J2),XY,XZ,WFA(I,L)                                         
      END DO
      MO=1                                                                           
  187 JJ=367                                                                         
      KK=0                                                                           
      IF(KP>0)THEN
          WRITE(KW(1),377)                                                               
          DO L=1,J1                                                                  
              J2=LT(I,L)                                                                     
              IF(IHC(J2)/=NHC(7))CYCLE
              X1=MAX(0.,COTL(J2))                                                            
              JDA=ITL(I,L)                                                                   
              IF(NYD==0)JDA=JDA+1                                                            
              IF(JDA==JJ.AND.NBT(J2)==0.AND.NBE(J2)==KK)X1=0.
              JJ=JDA                                                                         
              KK=NBE(J2)                                                                     
              CALL AXMON(JDA,MO)                                                             
              CALL AICL                                                                      
              M=LPC(I,L)                                                                     
              XZ=PCST(M)*PSTR(I,L)                                                           
              XY=X1+XZ                                                                       
        !     PRINTOUT PESTICIDE SCHEDULE                                                    
              WRITE(KW(1),378)I,MO,KDA,PSTN(M),KDP(M),NBE(J2),NBT(J2),XY,PSTR&               
              (I,L),PSTE(I,L),HUSC(I,L)                                                      
              IF(KFL(13)>0)WRITE(KW(13),501)I,MO,KDA,PSTN(M),JH(I,L),KDP(M),&                
              IHC(J2),NBE(J2),NBT(J2),XY,XZ,PSTR(I,L)                                        
          END DO         
      END IF
      IF(NFL==0.AND.JX(1)>0)THEN
          I=JX(1)                                                                        
          IY1=I                                                                          
          GO TO 189                                                                      
      END IF
      K2=1                                                                           
      REWIND KR(16)                                                                  
      NRO=IY1                                                                        
      IGSD=0                                                                         
      IF(NSTP>0.AND.NSTP<366)IGSD=NRO                                                
      JX(4)=IAUI                                                                     
      JX(5)=0                                                                       
      CALL TILTBL                                                                    
      IAUI=NDT                                                                       
	  WRITE(KW(1),715)TIL(NDT),TLD(NDT)                                                   
!     IF(IAUF==0)GO TO 689                                                           
      JX(4)=261                                                                      
      JX(5)=12                                                                       
      CALL TILTBL                                                                    
      IAUF=NDT                                                                       
      IF(LMS==0)THEN
          JX(4)=267                                                                      
          JX(5)=12                                                                       
          CALL TILTBL                                                                    
          IAUL=NDT                                                                       
	      WRITE(KW(1),714)TIL(NDT),TLD(NDT)
      END IF
      L=1                                                                            
      IF(BFT0>0.)THEN	                                                        
          IDFT(1)=IDF0                                                                   
          IF(IDFT(1)==0)THEN                                                             
              IDFT(1)=52                                                                   
              IDFT(2)=52                                                                   
          ELSE                                                                           
              IDFT(2)=IDF0                                                                 
	      END IF
	      DO K=1,2                                                                       
              JX(7)=IDFT(K)                                                                
              CALL NFTBL(L)                                                                
              IDFT(K)=L                                                                    
          END DO                                                                         
          WRITE(KW(1),716)TIL(IAUF),TLD(IAUF),FTNM(IDFT(1))
      END IF
      IF(IPAT>0)THEN
          IF(IDFP==0)IDFP=53
	      JX(7)=IDFP
	      CALL NFTBL(L)
	      IDFP=L
	  END IF                                          
      DO I=1,NRO                                                                     
          IF(NBC(I)==0)NBC(I)=1                                                        
          IF(LY(I,1)>0)CYCLE                                                           
          I1=I-1                                                                       
          IF(I1==0)I1=NRO                                                              
          LY(I,1)=LY(I1,NBC(I1))                                                       
      END DO                                                                         
      IF(IGO>0)THEN
	      NBCX(1,LY(NRO,NBC(NRO)))=NBCX(1,LY(NRO,NBC(NRO)))+1                                 
          NN=NBC(1)                                                                      
          DO J=1,MNC                                                                 
              IF(NHU(J)==0)CYCLE
              DO I=1,NN                                                                      
                  IF(LY(1,I)==J)EXIT
              END DO
              IF(I<=NN)CYCLE                                                                         
              NBC(1)=NBC(1)+1                                                                
              DO L=NBC(1),2,-1                                                                       
                  L1=L-1                                                                         
                  LY(1,L)=LY(1,L1)                                                               
              END DO                                                               
              LY(1,1)=NHU(J)
          END DO
      END IF                                                                                     
!     ANM = CROP NAME                                                                
!     X1  = GRAIN PRICE ($/t)                                                        
!     X2  = FORAGE PRICE ($/t)                                                       
!     LINE 8/27
      DO                                                                      
          READ(KR(26),630,IOSTAT=NFL)ANM,X1,X2                                                      
          IF(NFL/=0)EXIT
          IF(LEN_TRIM(ANM)==0)EXIT
          DO J=1,LC                                                                      
              IF(ANM==CPNM(J))GO TO 684                                                    
	      END DO                                                                              
          CYCLE
      684 PRYG(J)=X1                                                                     
          PRYF(J)=X2                                                                     
      END DO
      REWIND KR(26)                                                                  
!     PRINTOUT CROP PARAMETERS                                                       
      CALL APAGE(1)                                                                  
      WRITE(KW(1),348)                                                               
      WRITE(KW(1),248)(CPNM(I),I=1,LC)                                               
      WRITE(KW(1),49)'WA  ',(WA(I),I=1,LC)                                           
      WRITE(KW(1),53)'WUB ',(WUB(I),I=1,LC)                                          
      WRITE(KW(1),53)'HI  ',(HI(I),I=1,LC)                                           
      WRITE(KW(1),49)'TOPT',(TOPC(I),I=1,LC)                                         
      WRITE(KW(1),49)'TBAS',(TBSC(I),I=1,LC)                                         
      WRITE(KW(1),35)'GMHU',(GMHU(I),I=1,LC)                                         
      WRITE(KW(1),53)'DMLA',(DMLA(I),I=1,LC)                                         
      WRITE(KW(1),53)'DLAI',(DLAI(I),I=1,LC)                                         
      WRITE(KW(1),63)'LAP1',(DLAP(1,I),I=1,LC)                                       
      WRITE(KW(1),63)'LAP2',(DLAP(2,I),I=1,LC)                                       
      WRITE(KW(1),63)'PPL1',(PPLP(1,I),I=1,LC)                                       
      WRITE(KW(1),63)'PPL2',(PPLP(2,I),I=1,LC)                                       
      WRITE(KW(1),63)'FRS1',(FRST(1,I),I=1,LC)                                       
      WRITE(KW(1),63)'FRS2',(FRST(2,I),I=1,LC)                                       
      WRITE(KW(1),53)'RLAD',(RLAD(I),I=1,LC)                                         
      WRITE(KW(1),53)'RBMD',(RBMD(I),I=1,LC)                                         
      WRITE(KW(1),49)'ALT ',(ALT(I),I=1,LC)                                          
      WRITE(KW(1),53)'CAF ',(CAF(I),I=1,LC)                                          
      WRITE(KW(1),78)'GSI ',(GSI(I),I=1,LC)                                          
      WRITE(KW(1),53)'WAC2',(WAC2(2,I),I=1,LC)                                       
      WRITE(KW(1),49)'WAVP',(WAVP(I),I=1,LC)                                         
      WRITE(KW(1),49)'VPTH',(VPTH(I),I=1,LC)                                         
      WRITE(KW(1),53)'VPD2',(VPD2(I),I=1,LC)                                         
      WRITE(KW(1),49)'SDW ',(SDW(I),I=1,LC)                                          
      WRITE(KW(1),53)'HMX ',(HMX(I),I=1,LC)                                          
      WRITE(KW(1),53)'RDMX',(RDMX(I),I=1,LC)                                         
      WRITE(KW(1),63)'RWP1',(RWPC(1,I),I=1,LC)                                       
      WRITE(KW(1),63)'RWP2',(RWPC(2,I),I=1,LC)                                       
      WRITE(KW(1),78)'CNY ',(CNY(I),I=1,LC)                                          
      WRITE(KW(1),78)'CPY ',(CPY(I),I=1,LC)                                          
      WRITE(KW(1),78)'CKY ',(CKY(I),I=1,LC)                                          
      WRITE(KW(1),78)'WSYF',(WSYF(I),I=1,LC)                                         
      WRITE(KW(1),53)'PST ',(PST(I),I=1,LC)                                          
      WRITE(KW(1),53)'CSTS',(CSTS(I),I=1,LC)                                         
      WRITE(KW(1),53)'PRYG',(PRYG(I),I=1,LC)                                         
      WRITE(KW(1),53)'PRYF',(PRYF(I),I=1,LC)                                         
      WRITE(KW(1),53)'WCYS',(WCY(I),I=1,LC)                                          
      WRITE(KW(1),78)'BN1 ',(BN(1,I),I=1,LC)                                         
      WRITE(KW(1),78)'BN2 ',(BN(2,I),I=1,LC)                                         
      WRITE(KW(1),78)'BN3 ',(BN(3,I),I=1,LC)                                         
      WRITE(KW(1),78)'BP1 ',(BP(1,I),I=1,LC)                                         
      WRITE(KW(1),78)'BP2 ',(BP(2,I),I=1,LC)                                         
      WRITE(KW(1),78)'BP3 ',(BP(3,I),I=1,LC)                                         
      WRITE(KW(1),78)'BK1 ',(BK(1,I),I=1,LC)                                         
      WRITE(KW(1),78)'BK2 ',(BK(2,I),I=1,LC)                                         
      WRITE(KW(1),78)'BK3 ',(BK(3,I),I=1,LC)                                         
      WRITE(KW(1),63)'BW1 ',(BWD(1,I),I=1,LC)                                        
      WRITE(KW(1),63)'BW2 ',(BWD(2,I),I=1,LC)                                        
      WRITE(KW(1),63)'BW3 ',(BWD(3,I),I=1,LC)                                        
      WRITE(KW(1),63)'STX1',(STX(1,I),I=1,LC)                                        
      WRITE(KW(1),63)'STX2',(STX(2,I),I=1,LC)                                        
      WRITE(KW(1),63)'BLG1',(BLG(1,I),I=1,LC)                                        
      WRITE(KW(1),63)'BLG2',(BLG(2,I),I=1,LC)                                        
      WRITE(KW(1),78)'FTO ',(FTO(I),I=1,LC)                                          
      WRITE(KW(1),78)'FLT ',(FLT(I),I=1,LC)                                          
      WRITE(KW(1),281)(IDC(I),I=1,LC)                                                
      IPL=0                                                                          
      LRG=0                                                                          
      DO I=1,LC                                                                  
          NHU(I)=IHU(I)                                                                  
          IF(NHU(I)>LRG)LRG=NHU(I)                                                       
          IF(RDMX(I)>RZ)RZ=RDMX(I)                                                       
          IF(IDC(I)==NDC(7).OR.IDC(I)==NDC(8).OR.IDC(I)==NDC(10))XMTU&                   
         &(I)=(1.-EXP(-FTO(I)/XMTU(I)))/144.                                             
          BLG(3,I)=BLG(2,I)                                                              
          BLG(1,I)=BLG(1,I)/BLG(2,I)                                                     
          BLG(2,I)=.99                                                                   
          CALL ASCRV(BLG(1,I),BLG(2,I),.5,1.)                                            
	      IF(NUPC==0)THEN
	          CALL NCONC(BN(1,I),BN(2,I),BN(3,I),BN(4,I))                                    
              CALL NCONC(BP(1,I),BP(2,I),BP(3,I),BP(4,I))
          ELSE           
              BN(4,I)=BN(1,I)                                                                
              X1=BN(1,I)-BN(3,I)                                                             
              BN(1,I)=1.-(BN(2,I)-BN(3,I))/X1                                                
              BN(2,I)=1.-.00001/X1                                                           
              CALL ASCRV(BN(1,I),BN(2,I),.5,1.)                                              
              BP(4,I)=BP(1,I)                                                                
              X1=BP(1,I)-BP(3,I)                                                             
              BP(1,I)=1.-(BP(2,I)-BP(3,I))/X1                                                
              BP(2,I)=1.-.00001/X1                                                           
              CALL ASCRV(BP(1,I),BP(2,I),.5,1.)                                              
          END IF
          DO K=1,3                                                                       
              XTP(K)=0.                                                                    
              DO J=1,3                                                                     
                  XTP(K)=XTP(K)+BK(J,I)*AKX(K,J)                                           
	          END DO                                                                            
	      END DO                                                                              
          BK(1,I)=XTP(1)                                                                 
          BK(2,I)=XTP(2)                                                                 
          BK(4,I)=XTP(3)                                                                 
          IHU(I)=1                                                                       
          X1=ASPLT(DLAP(1,I))*.01                                                        
          X2=ASPLT(DLAP(2,I))*.01                                                        
          CALL ASCRV(DLAP(1,I),DLAP(2,I),X1,X2)                                          
          X1=ASPLT(FRST(1,I))                                                            
          X2=ASPLT(FRST(2,I))                                                            
          CALL ASCRV(FRST(1,I),FRST(2,I),X1,X2)                                          
          WAC2(1,I)=WA(I)*.01                                                            
          X2=ASPLT(WAC2(2,I))                                                            
          CALL ASCRV(WAC2(1,I),WAC2(2,I),330.,X2)                                        
          X2=ASPLT(VPD2(I))                                                              
          VPD2(I)=(1.-VPD2(I))/(X2-VPTH(I))                                              
          UNA(I)=PRMT(39)*BN(3,I)*WA(I)*PRMT(28)                                         
	      ULYN(I)=UNA(I)                                                                      
          BLYN(I)=0.
      END DO                                                                               
      SMMC=0.                                                                        
	  DO J=1,LRG                                                                          
          WRITE(KW(1),78)'POP ',(POP(I,J),I=1,LC)                                      
          WRITE(KW(1),53)'MXLA',(PPLA(I,J),I=1,LC)                                     
      END DO                                                                         
      DO J=1,LRG                                                                     
          WRITE(KW(1),35)'PHU ',(PHU(I,J),I=1,LC)                                      
	  END DO                                                                              
      CALL APAGE(1)                                                                  
      WRITE(KW(1),348)                                                               
      WRITE(KW(1),248)(CPNM(I),I=1,LC)                                               
      WRITE(KW(1),78)'BN1 ',(BN(1,I),I=1,LC)                                         
      WRITE(KW(1),78)'BN2 ',(BN(2,I),I=1,LC)                                         
      WRITE(KW(1),78)'BN3 ',(BN(3,I),I=1,LC)                                         
      WRITE(KW(1),78)'BN4 ',(BN(4,I),I=1,LC)                                         
      WRITE(KW(1),78)'BP1 ',(BP(1,I),I=1,LC)                                         
      WRITE(KW(1),78)'BP2 ',(BP(2,I),I=1,LC)                                         
      WRITE(KW(1),78)'BP3 ',(BP(3,I),I=1,LC)                                         
      WRITE(KW(1),78)'BP4 ',(BP(4,I),I=1,LC)                                         
      WRITE(KW(1),78)'BK1 ',(BK(1,I),I=1,LC)                                         
      WRITE(KW(1),78)'BK2 ',(BK(2,I),I=1,LC)                                         
      WRITE(KW(1),78)'BK3 ',(BK(3,I),I=1,LC)                                         
      WRITE(KW(1),63)'LAP1',(DLAP(1,I),I=1,LC)                                       
      WRITE(KW(1),63)'LAP2',(DLAP(2,I),I=1,LC)                                       
      WRITE(KW(1),63)'FRS1',(FRST(1,I),I=1,LC)                                       
      WRITE(KW(1),63)'FRS2',(FRST(2,I),I=1,LC)                                       
      WRITE(KW(1),63)'WAC1',(WAC2(1,I),I=1,LC)                                       
      WRITE(KW(1),63)'WAC2',(WAC2(2,I),I=1,LC)                                       
      WRITE(KW(1),63)'PPC1',(PPCF(1,I),I=1,LC)                                       
      WRITE(KW(1),63)'PPC2',(PPCF(2,I),I=1,LC)                                       
      IF(NDP>0)THEN
          WRITE(KW(1),'(//1X,A/)')'____________________PESTICIDE DATA____________________'
          WRITE(KW(1),382)                                                               
    !     PRINTOUT PESTICIDE DATA                                                        
          DO I=1,NDP                                                                     
              WRITE(KW(1),380)PSTN(I),PSOL(I),PHLS(I),PHLF(I),PWOF(I),PKOC(I),&            
              &PCST(I)                                                                      
              PSOL(I)=PSOL(I)*10.                                                          
              PHLS(I)=1.-EXP(-.693/PHLS(I))                                                
              PHLF(I)=1.-EXP(-.693/PHLF(I))                                                
          END DO                                                                         
      END IF          
      JD=LY(NRO,NBC(NRO))                                                            
      ICCD=0                                                                         
      IRTC=1                                                                         
      I=1                                                                            
      IF(NBC(NRO)>1)GO TO 634                                                        
      IF(IHVD(1,JD)==0)GO TO 677                                                     
      IF(IPLD(1,JD)<IHVD(1,JD))GO TO 677                                             
  634 N1=NSTP+365*(NRO-1)                                                            
      DO I=1,NBC(NRO)                                                                
          IF(N1<IHVD(1,LY(NRO,I)))GO TO 677                                            
      END DO                                                                         
      GO TO 678                                                                      
  677 ICCD=1                                                                         
      IRTC=I 
  678 XLC=LC
      X1=STDO/XLC
      SUM=0.                                                                              
      DO J=1,LC
          STDN(J)=8.29*STD0
          SUM=SUM+STDN(J)                                                                  
          STDP=1.04*STD0                                                                  
          STDK=8.29*STD0                                                                  
          STDL=.1*STD0                                                                    
          STD(J)=X1
      END DO
      IF(RZ>XX)RZ=XX                                                                 
      IF(BIG>XX)BIG=XX                                                               
      BTN=TWN+ZNO3+SUM                                                              
      BTNX=BTN                                                                            
      BTC=TOC+420.*STD0
      BTCX=BTC                                                                        
      BTP=TAP+TMP+TOP+TP+SEV(4)+STDP                                                 
      BTK=TSK+TEK+TFK+STDK                                                           
      KK=NTL(1)                                                                      
      KC=1                                                                           
      DO KT=1,KK                                                                     
          IF(ITL(1,KT)>=IBD)GO TO 200                                                  
          II=IHC(LT(1,KT))                                                             
          IF(II==NHC(1))KC=KC+1                                                        
	  END DO                                                                              
      KT=NTL(1)                                                                      
  200 JT2=LT(1,KT)                                                                   
!     IF(NGN>0)CALL WREAD                                                            
      BFT=BFT0                                                                       
      IF(BFT0>1.)BFT=10.*BFT0*ABD*RZ                                                 
      UB1=RZ*PRMT(54)                                                                
      UOB=1.-EXP(-UB1)                                                               
      AWC=0.                                                                         
      AQV=0.                                                                         
      ARF=0.                                                                         
	  ALB=SALB                                                                            
      IF(SNO>5.)ALB=.6                                                               
	  U=0.                                                                                
	  SSF=0.                                                                              
      IGO=0                                                                          
      JJK=KDC1(JJK0)                                                                 
      KC=0                                                                           
	  IPLD=0                                                                              
      MO=MO1                                                                         
      JDA=IBD-1                                                                      
      IF(JDA<=0)JDA=365                                                              
      CALL WHRL
      CALL WRMX                                                                    
      HR0=HRLT                                                                       
!     BEGIN ANNUAL SIMULATION LOOP                                                   
      IRO=IRO0-1                                                                     
  533 CALL BSIM                                                                      
      IF(ISTP==1)GO TO 219                                                           
      IY=MAX(1,IY-1)                                                                 
      XYR=IY                                                                         
      CALL APAGE(1)                                                                  
      IF(KFL(9)>0)THEN
          ! LINES 4/51
          ! DManowitz - 2/6/12: Reuse output format #1
          IF (SoilOutputFmtSelect == 1) THEN
              SoilOutputFormat1 = '(15(G12.4,1X),A)'
              SoilOutputFormat2 = '(15(F12.0,1X),A)'
              SoilOutputFormat3 = '(15(A12,1X),A)'
          ELSE
              SoilOutputFormat1 = '(15F8.2,A)'
              SoilOutputFormat2 = '(15F8.0,A)'
              SoilOutputFormat3 = '(15A8,A)'
          END IF
          WRITE(KW(9),SoilOutputFormat1)(Z(LID(I)),I=1,MSL),'  4 DEPTH(m)        '
          WRITE(KW(9),SoilOutputFormat1)(BD(LID(I)),I=1,MSL),'  5 BD 33kpa(t/m3)  '          
          WRITE(KW(9),SoilOutputFormat1)(SOIL(20,LID(I)),I=1,MSL),'  6 WP SW(m/m)       '
          WRITE(KW(9),SoilOutputFormat1)(SOIL(9,LID(I)),I=1,MSL),'  7 FC SW(m/m)      '          
          WRITE(KW(9),SoilOutputFormat1)(SAN(LID(I)),I=1,MSL),'  8 SAND(%)         '
          WRITE(KW(9),SoilOutputFormat1)(SIL(LID(I)),I=1,MSL),'  9 SILT(%)         '
          WRITE(KW(9),SoilOutputFormat2)(SOIL(6,LID(I)),I=1,MSL),' 10 ORG N(g/t)      '
          WRITE(KW(9),SoilOutputFormat1)(PH(LID(I)),I=1,MSL),' 11 PH              '
          WRITE(KW(9),SoilOutputFormat1)(SMB(LID(I)),I=1,MSL),' 12 SM BS(cmol/kg)  '
          WRITE(KW(9),SoilOutputFormat1)(SOIL(7,LID(I)),I=1,MSL),' 13 ORG C(%)        '
          WRITE(KW(9),SoilOutputFormat1)(CAC(LID(I)),I=1,MSL),' 14 CAC(%)          '
          WRITE(KW(9),SoilOutputFormat1)(CEC(LID(I)),I=1,MSL),' 15 CEC(cmol/kg)    '
          WRITE(KW(9),SoilOutputFormat1)(ROK(LID(I)),I=1,MSL),' 16 ROCK(%)         '
          WRITE(KW(9),SoilOutputFormat1)(SOIL(5,LID(I)),I=1,MSL),' 17 NO3(g/t)        '
          WRITE(KW(9),SoilOutputFormat1)(SOIL(1,LID(I)),I=1,MSL),' 18 LAB P(g/t)      '
          WRITE(KW(9),SoilOutputFormat1)(RSD(LID(I)),I=1,MSL),' 19 CROP RSD(t/ha)  '
          WRITE(KW(9),SoilOutputFormat1)(SOIL(13,LID(I)),I=1,MSL),' 20 BD DRY(t/m3)    '
          WRITE(KW(9),SoilOutputFormat1)(PSP(LID(I)),I=1,MSL),' 21 P SORP RTO       '
          WRITE(KW(9),SoilOutputFormat1)(SATC(LID(I)),I=1,MSL),' 22 SAT COND(mm/h)   '
          WRITE(KW(9),SoilOutputFormat1)(HCL(LID(I)),I=1,MSL),' 23 H SC(mm/h)       '
          WRITE(KW(9),SoilOutputFormat1)(SOIL(4,LID(I)),I=1,MSL),' 24 ORG P(g/t)       '
          WRITE(KW(9),SoilOutputFormat1)(SOIL(15,LID(I)),I=1,MSL),' 25 EXCK(g/t)        '
          WRITE(KW(9),SoilOutputFormat1)(ECND(LID(I)),I=1,MSL),' 26 ECND(mmho/cm)   '
          WRITE(KW(9),SoilOutputFormat1)(STFR(LID(I)),I=1,MSL),' 27 STFR            '
          WRITE(KW(9),SoilOutputFormat1)(SOIL(12,LID(I)),I=1,MSL),' 28 SW(m/m)         '
          WRITE(KW(9),SoilOutputFormat2)(CPRV(LID(I)),I=1,MSL),' 29 FRACT V PIPE F  '
          WRITE(KW(9),SoilOutputFormat2)(CPRH(LID(I)),I=1,MSL),' 30 FRACT H PIPE F  '
          WRITE(KW(9),SoilOutputFormat2)(WLS(LID(I)),I=1,MSL),' 31 WLS(kg/ha)      '
          WRITE(KW(9),SoilOutputFormat2)(WLM(LID(I)),I=1,MSL),' 32 WLM(kg/ha)      '
          WRITE(KW(9),SoilOutputFormat1)(WLSL(LID(I)),I=1,MSL),' 33 WLSL(kg/ha)     '
          WRITE(KW(9),SoilOutputFormat1)(WLSC(LID(I)),I=1,MSL),' 34 WLSC(kg/ha)     '
          WRITE(KW(9),SoilOutputFormat1)(WLMC(LID(I)),I=1,MSL),' 35 WLMC(kg/ha)     '
          WRITE(KW(9),SoilOutputFormat1)(WLSLC(LID(I)),I=1,MSL),' 36 WLSLC(kg/ha)    '
          WRITE(KW(9),SoilOutputFormat1)(WLSLNC(LID(I)),I=1,MSL),' 37 WLSLNC(kg/ha)   '
          WRITE(KW(9),SoilOutputFormat1)(WBMC(LID(I)),I=1,MSL),' 38 WBMC(kg/ha)     '
          WRITE(KW(9),SoilOutputFormat2)(WHSC(LID(I)),I=1,MSL),' 39 WHSC(kg/ha)     '
          WRITE(KW(9),SoilOutputFormat2)(WHPC(LID(I)),I=1,MSL),' 40 WHPC(kg/ha)     '
          WRITE(KW(9),SoilOutputFormat1)(WLSN(LID(I)),I=1,MSL),' 41 WLSN(kg/ha)     '
          WRITE(KW(9),SoilOutputFormat1)(WLMN(LID(I)),I=1,MSL),' 42 WLMN(kg/ha)     '
          WRITE(KW(9),SoilOutputFormat1)(WBMN(LID(I)),I=1,MSL),' 43 WBMN(kg/ha)     '
          WRITE(KW(9),SoilOutputFormat2)(WHSN(LID(I)),I=1,MSL),' 44 WHSN(kg/ha)     '
          WRITE(KW(9),SoilOutputFormat2)(WHPN(LID(I)),I=1,MSL),' 45 WHPN(kg/ha)     '
          WRITE(KW(9),SoilOutputFormat1)(FE26(LID(I)),I=1,MSL),' 46 IRON(%)         '
          WRITE(KW(9),SoilOutputFormat1)(SULF(LID(I)),I=1,MSL),' 47 SULPHUR(%)      '
          WRITE(KW(9),SoilOutputFormat3)(ASHZ(LID(I)),I=1,MSL),' 48 SOIL HORIZON    '
          WRITE(KW(9),SoilOutputFormat1)(CGO2(LID(I)),I=1,MSL),' 49 CGO2(kg/ha)     '
          WRITE(KW(9),SoilOutputFormat1)(CGCO2(LID(I)),I=1,MSL),' 50 CGCO2(kg/ha)    '
          WRITE(KW(9),SoilOutputFormat1)(CGN2O(LID(I)),I=1,MSL),' 51 CGN2O(kg/ha)    '
      END IF
      WRITE(KW(1),'(//1X,A/)')'____________________FINAL SOIL DATA_____________________'
      
      CALL SOLIO(YTP,1)                                                              
	  CALL SCONT(1)                                                                          
      WRITE(KW(1),'(/T10,A,F7.1,A)')'ERODED SOIL THICKNESS = ',THK,' mm'             
      WRITE(KW(1),'(/T10,A,F7.2,A)')'FINAL WATER CONTENT OF SNOW = ',SNO,' mm'
!     PRINTOUT WATER BALANCE                                                         
      CALL HSWBL(SM(4),SM(14),SM(11),SM(16),SM(17),SM(19),SNO,SW,SWW,&               
      SM(20),SM(84),KW,MSO)                                                          
      IF(IRR==4)THEN                                                                 
          VLG=.1*VLG/(DALG+1.E-10)                                                     
          CALL HLGB(SM(23),SM(21),SM(24),SM(78),VLG,VLGB,SM(22),KW,MSO)                
          CALL NLGB(SM(79),SM(80),WTMB,WTMU,KW,MSO)                                    
	  END IF                                                                              
      RNO3=SM(4)*RFNC                                                                
      SUM=0.                                                                         
      TOT=0.                                                                         
      ADD=0.
      AD1=0.
      AD2=0.
      AD3=0.
      AD4=0.                                                                         
      DO J=1,LC                                                                     
          TOT=TOT+UP1(J)                                                               
          ADD=ADD+UK1(J)                                                               
          SUM=SUM+UN1(J)                                                               
          AD1=AD1+STDN(J)
          AD2=AD2+1000.*TYLC(J)
          AD3=AD3+420.*DM(J)
          AD4=AD4+420.*STD(J)
      END DO                                                                         
      FTN=ZN2OG+ZN2OL+ZNO2+ZNO3+TWN+AD1+STDON+SUM+ZNH3
      X1=SM(49)
      IF(IDN>2)X1=SM(89)
      CALL NBL(BTN,RNO3,SM(43),SM(44),SM(45),SM(46),SM(106),SM(102),SM&
      (105),SM(107),SM(103),SM(104),X1,SM(59),TYN,SM(52),SM(60),SM&
      (61),SM(50),SM(98),SM(101),FTN,1,KW,MSO)                                      
      WRITE(KW(1),636)ZN2OG,ZN2OL,ZNO2,ZNO3,ZNH3,TWN,AD1,STDON,SUM
      FTC=TOC+AD3+AD4
      CALL NCBL(BTC,SM(77),SM(75),SM(76),SM(74),SM(65),SM(99),AD2,SM(97),&
      FTC,KW,MSO)
      WRITE(KW(1),637)ZLSC,ZLMC,ZBMC,ZHSC,ZHPC,TOC,AD3,AD4                                     
      FTP=TAP+TOP+TMP+TP+TFOP+STDP+STDOP+TOT                                         
      CALL NBL(BTP,0.,SM(54),SM(55),0.,SM(57),0.,0.,0.,0.,0.,0.,0.,SM(62),&
      TYP,0.,SM(63),0.,0.,0.,0.,FTP,2,KW,MSO)                                                            
      WRITE(KW(1),638)TAP,TOP,TMP,TP,TFOP,STDP,STDOP,TOT                             
      FTK=TSK+TEK+TFK+STDK+STDOK+ADD                                                 
      CALL NBL(BTK,0.,SM(78),SM(79),SM(80),SM(81),0.,0.,0.,0.,0.,0.,0.,&
      0.,TYK,0.,SM(64),0.,0.,0.,0.,FTK,3,KW,MSO)                                                            
      WRITE(KW(1),639)TSK,TEK,TFK,STDK,STDOK,ADD                                     
      CALL SLTB(SM(69),SM(72),SM(82),SM(71),SM(70),SLT0,TSLT,KW,MSO)                 
      XQP=NQP                                                                        
      XCN=JCN                                                                        
      XX=XQP+.01                                                                     
      PRSD=PRSD-PRAV*PRAV/XX                                                         
      PRAV=PRAV/XX                                                                   
      IF(PRSD>0.)PRSD=SQRT(PRSD/XX)                                                  
      CYSD=CYSD-CYAV*CYAV/XX                                                         
      CYAV=CYAV/XX                                                                   
      IF(CYSD>0.)CYSD=SQRT(CYSD/XX)                                                  
      QPS=QPS/XX                                                                     
      TCAV=TCAV/XX                                                                   
      SM(58)=SM(58)/XX                                                               
!     DETERMINE AVE ANNUAL VALUES                                                    
      SMY(13)=0.                                                                     
      SUM=0.                                                                         
      TOT=0.                                                                         
      ADD=0.                                                                         
      AD2=0.                                                                         
      DO I=1,12                                                                  
          X1=IHRL(I)
          IF(X1>0.)THEN
              THRL(I)=THRL(I)/X1                                                             
              SRMX(I)=SRMX(I)/X1
              TXMX(I)=TXMX(I)/X1                                                             
              TXMN(I)=TXMN(I)/X1                                                             
              TSR(I)=TSR(I)/X1
          END IF                                                                                                                            
          TR(I)=TR(I)/XYR                                                                
          TSN(I)=TSN(I)/XYR                                                              
          TSY(I)=TSY(I)/XYR                                                              
          RSY(I)=RSY(I)/XYR                                                              
          TYW(I)=TYW(I)/XYR                                                              
          TQ(I)=TQ(I)/XYR                                                                
          W(I)=W(I)/(TEI(I)+1.E-20)                                                      
          RCM(I)=RCM(I)/(TEI(I)+1.E-20)                                                  
          TAL(I)=TAL(I)/CX(I)                                                            
          CX(I)=CX(I)/XYR                                                                
          TEI(I)=TEI(I)/XYR                                                              
          SMY(I)=SRD(I)/XYR                                                              
          SET(I)=SET(I)/XYR                                                              
          TET(I)=TET(I)/XYR                                                              
          ASW(I)=ASW(I)/XYR                                                              
          QIN(I)=QIN(I)/XYR                                                              
          TSTL(I)=TSTL(I)/XYR                                                            
          TRHT(I)=TRHT(I)/XYR                                                            
          TAMX(I)=TAMX(I)/XYR                                                            
          TOT=TOT+QIN(I)                                                                 
          ADD=ADD+TAMX(I)                                                                
          SUM=SUM+ASW(I)                                                                 
          SMY(13)=SMY(13)+SMY(I)                                                         
          AD2=AD2+CX(I)
      END DO                                                                            
      SM(15)=SM(15)/(XCN+1.E-10)                                                     
	  X1=SM(28)+1.E-10                                                                    
      SM(29)=SM(29)/X1                                                               
      SM(37)=SM(37)/X1                                                               
	  SM(90)=SM(90)/X1                                                                    
	  SM(91)=SM(91)/X1                                                                    
      SRF2=SM(4)*SM(4)                                                               
      SUM=SUM/12.                                                                    
      DO K=1,14                                                                      
          SM(K)=SM(K)/XYR                                                              
      END DO                                                                         
      DO K=16,28                                                                     
          SM(K)=SM(K)/XYR                                                              
      END DO                                                                         
      DO K=30,36                                                                     
          SM(K)=SM(K)/XYR                                                              
      END DO                                                                         
      DO K=38,57                                                                     
          SM(K)=SM(K)/XYR                                                              
      END DO                                                                         
      DO K=59,NSM                                                                  
          SM(K)=SM(K)/XYR                                                              
      END DO                                                                         
      IF(NDP==0.OR.MASP<0.OR.NBYR==1)GO TO 212                                                  
      CALL APAGE(1)                                                                  
      WRITE(KW(1),'(//1X,A/)')'______________PESTICIDE SUMMARY TABLE________________'
      WRITE(KW(1),460)                                                               
      NY(1)=1                                                                        
      NY(2)=.1*XYR+1.5                                                               
      NY(3)=.5*XYR+1.5                                                               
      DO K=1,NDP                                                                 
          WRITE(KW(1),462)PSTN(K)                                                        
          DO I=1,5                                                                   
              DO J=1,NBYR                                                                    
                  NX(J)=J                                                                      
                  XYP(J)=APY(I,K,J)                                                            
                  IF(XYP(J)<=1.E-4)THEN                                                        
                      APY(I,K,J)=0.                                                            
                      AYB(I,K,J)=0.                                                            
	              END IF                                                                            
                  XTP(J)=APQ(I,K,J)                                                            
                  IF(XTP(J)>1.E-4)CYCLE                                                        
                  APQ(I,K,J)=0.                                                                
                  AQB(I,K,J)=0.                                                                
	          END DO                                                                              
              CALL ASORT1(XTP,NX,NBYR)                                                       
              NXX(1,I)=NX(NY(1))                                                             
              NXX(2,I)=NX(NY(2))                                                             
              NXX(3,I)=NX(NY(3))                                                             
              CALL ASORT1(XYP,NX,NBYR)                                                       
              NYY(1,I)=NX(NY(1))                                                             
              NYY(2,I)=NX(NY(2))                                                             
              NYY(3,I)=NX(NY(3))
          END DO                                                             
      !     PRINTOUT PESTICIDE FREQ SUMMARY                                                
          DO N2=1,3                                                                      
              WRITE(KW(1),463)(APQ(I,K,NXX(N2,I)),I=1,5)                                   
              WRITE(KW(1),464)(AQB(I,K,NXX(N2,I)),I=1,5)                                   
              WRITE(KW(1),465)(APY(I,K,NYY(N2,I)),I=1,5)                                   
              WRITE(KW(1),466)(AYB(I,K,NYY(N2,I)),I=1,5)                                   
	          IF(N2==1)WRITE(KW(1),474)                                                         
	          IF(N2==2)WRITE(KW(1),473)                                                         
          END DO
      END DO                                                                                   
      WRITE(KW(1),'(/1X,A)')'-----AVE ANNUAL VALUES (g/ha)'                          
      DO K=1,NDP                                                                     
          DO L=1,7                                                                     
              SMAP(L,K)=SMAP(L,K)/XYR                                                      
          END DO                                                                       
          SMAP(10,K)=SMAP(10,K)/XYR                                                    
      END DO                                                                         
      I1=0                                                                           
      K1=0                                                                           
  418 I1=I1+10                                                                       
      N1=MIN(I1,NDP)                                                                 
      K2=K1+1                                                                        
      N2=MIN(10,NDP-K1)                                                              
!     PRINTOUT PESTICIDE SUMMARY                                                     
      WRITE(KW(1),383)(PSTN(K),K=K2,N1)                                              
      IF(MASP==0)GO TO 467                                                           
      WRITE(KW(1),384)HEDP(1),(SMAP(1,K),K=K2,N1)                                    
      I=1                                                                            
      DO K=K2,N1                                                                     
          PPX(I)=SMAP(2,K)                                                             
          CALL ACOUT(PPX(I),SM(14),1.)                                                 
          I=I+1                                                                        
      END DO                                                                         
      WRITE(KW(1),387)HEDP(2),(PPX(I),I=1,N2)                                        
      I=1                                                                            
      DO K=K2,N1                                                                     
          PPX(I)=SMAP(3,K)                                                             
          CALL ACOUT(PPX(I),SM(17),1.)                                                 
          I=I+1                                                                        
      END DO                                                                         
      WRITE(KW(1),387)HEDP(3),(PPX(I),I=1,N2)                                        
      I=1                                                                            
      DO K=K2,N1                                                                     
          PPX(I)=SMAP(4,K)                                                             
          CALL ACOUT(PPX(I),SM(16),1.)                                                 
          I=I+1                                                                        
      END DO                                                                         
      WRITE(KW(1),387)HEDP(4),(PPX(I),I=1,N2)                                        
      DO L=5,7                                                                       
          WRITE(KW(1),384)HEDP(L),(SMAP(L,K),K=K2,N1)                                  
      END DO                                                                         
      I=1                                                                            
      DO K=K2,N1                                                                     
          PPX(I)=SMAP(10,K)                                                            
          CALL ACOUT(PPX(I),SM(18),1.)                                                 
          I=I+1                                                                        
      END DO                                                                         
      WRITE(KW(1),387)HEDP(10),(PPX(I),I=1,N2)                                       
      GO TO 469                                                                      
  467 DO L=1,7                                                                       
          WRITE(KW(1),470)HEDP(L),(SMAP(L,K),K=K2,N1)                                  
      END DO                                                                         
      WRITE(KW(1),470)HEDP(10),(SMAP(10,K),K=K2,N1)                                  
  469 IF(N1>=NDP)GO TO 212                                                           
      K1=I1                                                                          
      GO TO 418                                                                      
  212 PPX(1)=SM(44)                                                                  
      PPX(2)=SM(45)                                                                  
      PPX(3)=SM(46)                                                                  
      PPX(4)=SM(55)                                                                  
      IF(MASP>0)THEN                                                                 
          CALL ACOUT(PPX(1),SM(14),1000.)                                              
          CALL ACOUT(PPX(2),SM(16),1000.)                                              
          CALL ACOUT(PPX(3),SM(17),1000.)                                              
          CALL ACOUT(PPX(4),SM(14),1000.)                                              
	  END IF                                                                              
      TYN=TYN/XYR                                                                    
      TYP=TYP/XYR                                                                    
      TYC=TYC/XYR                                                                    
      X6=CSTZ(1)+CSTZ(2)                                                             
      X1=CST1/XYR+X6                                                                 
      X4=CSO1/XYR                                                                    
      X2=VALF1/XYR                                                                   
      X3=X2-X1                                                                       
      SM1=0.                                                                         
      DO K=1,6                                                                       
          XTP(K)=ISIX(K)                                                               
          SM1=SM1+XTP(K)                                                               
      END DO                                                                         
      DO K=1,6                                                                       
          XTP(K)=XTP(K)/SM1                                                            
	  END DO                                                                              
      CALL APAGE(1)                                                                  
      WRITE(KW(1),350)                                                               
      WRITE(KW(1),661)(XTP(K),K=1,6)                                                 
      IF(DARF>0..AND.XYR>1.)THEN                                                     
	      DARF=DARF-SRF2/XYR                                                                
          IF(DARF>0.)DARF=SQRT(DARF/(XYR-1.))                                          
	  ELSE                                                                                
          DARF=0.                                                                      
      END IF                                                                         
      WRITE(KW(1),662)BARF,SARF,DARF                                                 
      WRITE(KW(1),323)PRB,PRAV,PRSD,QPQB,QPS,NQP                                     
      IF(TCMN>1.E10)TCMN=0.                                                          
      WRITE(KW(1),417)TCAV,TCMN,TCMX                                                 
      WRITE(KW(1),448)CYAV,CYMX,CYSD                                                 
      AD1=0.                                                                         
      DO K=1,10                                                                      
          AD1=AD1+CNDS(K)                                                              
      END DO                                                                         
      DO K=1,10                                                                      
          CNDS(K)=CNDS(K)/AD1                                                          
      END DO                                                                         
      WRITE(KW(1),234)(CNDS(K),K=1,10)                                               
	  WRITE(KW(1),303)RUSM                                                                
      IF(KFL(16)>0)THEN
      DO J=1,6                                                                       
          XTP(J)=0.                                                                    
          DO I=1,NBSL                                                                  
              ISL=LID(I)                                                               
              SMS(J,ISL)=SMS(J,ISL)/(SMS(11,ISL)+1.E-5)                                
              XTP(J)=XTP(J)+SMS(J,ISL)                                                 
	      END DO                                                                            
      END DO                                                                         
      DO J=7,10                                                                  
          XTP(J)=0.                                                                      
          DO I=1,NBSL                                                                    
              ISL=LID(I)                                                                   
              SMS(J,ISL)=SMS(J,ISL)/XYR                                                    
              XTP(J)=XTP(J)+SMS(J,ISL)                                                     
          END DO
      END DO                                                                                   
      DO I=1,NBSL                                                                    
          ISL=LID(I)                                                                   
          XYP(ISL)=WOC(ISL)-XZP(6,ISL)                                                 
          YTP(ISL)=WON(ISL)-XZP(12,ISL)                                                
	  END DO                                                                              
      XYP(16)=TOC-XZP(6,16)                                                          
      YTP(16)=TWN-XZP(12,16)                                                         
      WRITE(KW(16),635)(SID(J),J=1,MSL),SID(MSL+1)                        
      XNS=NBSL                                                                       
      DO J=1,6                                                                       
          XTP(J)=XTP(J)/XNS                                                            
	  END DO                                                                              
	  MS1=MSL+1                                                                           
      WRITE(KW(16),581)'   Z',(Z(LID(I)),I=1,MSL),Z(LID(MSL))                        
      WRITE(KW(16),581)' SWF',(SMS(1,LID(I)),I=1,MSL),XTP(1)                         
      WRITE(KW(16),581)'TEMP',(SMS(2,LID(I)),I=1,MSL),XTP(2)                         
      WRITE(KW(16),581)'SWTF',(SMS(3,LID(I)),I=1,MSL),XTP(3)                         
      WRITE(KW(16),581)'TLEF',(SMS(4,LID(I)),I=1,MSL),XTP(4)                         
      WRITE(KW(16),581)'SPDM',(SMS(5,LID(I)),I=1,MSL),XTP(5)                         
      WRITE(KW(16),576)'RSDC',(SMS(7,LID(I)),I=1,MSL),XTP(7)                         
      WRITE(KW(16),576)'RSPC',(SMS(8,LID(I)),I=1,MSL),XTP(8)                         
      WRITE(KW(16),576)'RNMN',(SMS(9,LID(I)),I=1,MSL),XTP(9)                         
      WRITE(KW(16),576)'DNO3',(SMS(10,LID(I)),I=1,MSL),XTP(10)                       
      WRITE(KW(16),576)'HSC0',(XZP(1,LID(I)),I=1,MSL),XZP(1,MS1)                     
      WRITE(KW(16),576)'HSCF',(WHSC(LID(I)),I=1,MSL),ZHSC                            
      WRITE(KW(16),576)'HPC0',(XZP(2,LID(I)),I=1,MSL),XZP(2,MS1)                     
      WRITE(KW(16),576)'HPCF',(WHPC(LID(I)),I=1,MSL),ZHPC                            
      WRITE(KW(16),576)'LSC0',(XZP(3,LID(I)),I=1,MSL),XZP(3,MS1)                     
      WRITE(KW(16),576)'LSCF',(WLSC(LID(I)),I=1,MSL),ZLSC                            
      WRITE(KW(16),576)'LMC0',(XZP(4,LID(I)),I=1,MSL),XZP(4,MS1)                     
      WRITE(KW(16),576)'LMCF',(WLMC(LID(I)),I=1,MSL),ZLMC                            
      WRITE(KW(16),576)'BMC0',(XZP(5,LID(I)),I=1,MSL),XZP(5,MS1)                     
      WRITE(KW(16),576)'BMCF',(WBMC(LID(I)),I=1,MSL),ZBMC                            
      WRITE(KW(16),576)'WOC0',(XZP(6,LID(I)),I=1,MSL),XZP(6,MS1)                     
      WRITE(KW(16),576)'WOCF',(WOC(LID(I)),I=1,MSL),TOC                              
      WRITE(KW(16),576)'DWOC',(XYP(LID(I)),I=1,MSL),XYP(MS1)                         
      WRITE(KW(16),576)'HSN0',(XZP(7,LID(I)),I=1,MSL),XZP(7,MS1)                     
      WRITE(KW(16),576)'HSNF',(WHSN(LID(I)),I=1,MSL),ZHSN                            
      WRITE(KW(16),576)'HPN0',(XZP(8,LID(I)),I=1,MSL),XZP(8,MS1)                     
      WRITE(KW(16),576)'HPNF',(WHPN(LID(I)),I=1,MSL),ZHPN                            
      WRITE(KW(16),576)'LSN0',(XZP(9,LID(I)),I=1,MSL),XZP(9,MS1)                     
      WRITE(KW(16),576)'LSNF',(WLSN(LID(I)),I=1,MSL),ZLSN                            
      WRITE(KW(16),576)'LMN0',(XZP(10,LID(I)),I=1,MSL),XZP(10,MS1)                   
      WRITE(KW(16),576)'LMNF',(WLMN(LID(I)),I=1,MSL),ZLMN                            
      WRITE(KW(16),576)'BMN0',(XZP(11,LID(I)),I=1,MSL),XZP(11,MS1)                   
      WRITE(KW(16),576)'BMNF',(WBMN(LID(I)),I=1,MSL),ZBMN                            
      WRITE(KW(16),576)'WON0',(XZP(12,LID(I)),I=1,MSL),XZP(12,MS1)                   
      WRITE(KW(16),576)'WONF',(WON(LID(I)),I=1,MSL),TWN                              
      WRITE(KW(16),576)'DWON',(YTP(LID(I)),I=1,MSL),YTP(MS1)                         
      WRITE(KW(16),581)'C/N0',(XZP(13,LID(I)),I=1,MSL),XZP(13,MS1)                   
      DO I=1,100                                                                     
          XTP(I)=0.                                                                    
	  END DO                                                                              
      DO I=1,NBSL                                                                    
          ISL=LID(I)                                                                   
          XTP(ISL)=WOC(ISL)/WON(ISL)                                                   
	  END DO                                                                              
      XTP(MS1)=TOC/TWN                                                               
      WRITE(KW(16),581)'C/NF',(XTP(LID(I)),I=1,MSL),XTP(MSL+1)                       
      END IF
      CALL APAGE(1)                                                                  
      WRITE(KW(1),350)                                                               
      WRITE(KW(1),295)                                                               
      WRITE(KW(1),319)IY                                                             
!     PRINTOUT SUMMARY MONTHLY                                                       
      WRITE(KW(1),321)HED(1),TXMX,SM(1),HED(1)                                       
      WRITE(KW(1),321)HED(2),TXMN,SM(2),HED(2)                                       
      WRITE(KW(1),243)HED(4),TR,SM(4),HED(4)                                         
      WRITE(KW(1),321)'DAYP',(SMY(I),I=1,13),'DAYP'                                  
      WRITE(KW(1),243)HED(17),TSN,SM(17),HED(17)                                     
      WRITE(KW(1),243)HED(14),TQ,SM(14),HED(14)                                      
      WRITE(KW(1),321)'RZSW',ASW,SUM,'RZSW'                                          
      WRITE(KW(1),311)HED(28),TEI,SM(28),HED(28)                                     
      WRITE(KW(1),224)'ALPH',TAL,'ALPH'                                              
      WRITE(KW(1),321)HED(29),W,SM(29),HED(29)                                       
      WRITE(KW(1),325)HED(37),RCM,SM(37),HED(37)                                     
      WRITE(KW(1),321)HED(NDVSS),TSY,SM(NDVSS),HED(NDVSS)                            
      WRITE(KW(1),321)HED(36),RSY,SM(36),HED(36)                                     
      WRITE(KW(1),321)HED(7),TET,SM(7),HED(7)                                        
      WRITE(KW(1),224)HED(7),U10MX,HED(7)                                            
      WRITE(KW(1),243)'DAYW',TAMX,ADD,'DAYW'                                         
      WRITE(KW(1),243)HED(39),TRHT,SM(39),HED(39)                                    
      WRITE(KW(1),243)'DAYQ',CX,AD2,'DAYQ'                                           
      WRITE(KW(1),224)HEDC(7),TSTL,HEDC(7)                                           
      WRITE(KW(1),311)HED(42),TYW,SM(42),HED(42)                                     
      WRITE(KW(1),321)HED(20),QIN,TOT,HED(20)                                        
      WRITE(KW(1),311)HED(10),SET,SM(10),HED(10)                                     
      WRITE(KW(1),311)HED(3),TSR,SM(3),HED(3)                                        
      WRITE(KW(1),224)'HRLT',THRL,'HRLT'                                             
      WRITE(KW(1),'(/1X,A)')'-----AVE ANNUAL VALUES'                                 
      WRITE(KW(1),293)IY,(HED(KA(K)),SM(KA(K)),K=5,NKA),'COST',X1,'RTRN'&            
      ,X2
      WRITE(KW(1),369)(HED(JC(K)),PPX(K),K=1,NJC)                                    
      SMY(3)=0.                                                                      
      SMY(4)=0.                                                                      
      SMY(1)=SM(59)+SM(60)+SM(61)                                                    
      SMY(2)=SM(62)+SM(63)                                                           
      SMY(5)=1000.*AP(LD1)/WT(LD1)                                                   
      X1=LC                                                                          
      X6=X6/X1                                                                       
      DO K=1,LC                                                                  
          IF(NCR(K)==0)CYCLE
	      XX=MIN(NCR(K),IY)
	      X3=TETG(K)+.01                                                                   
	      TETG(K)=1000.*(TYL1(K)+TYL2(K))/X3                                       
	      TYL1(K)=TYL1(K)/XX                                                                  
          TYL2(K)=TYL2(K)/XX                                                             
          TYLN(K)=TYLN(K)/XX                                                             
          TYLP(K)=TYLP(K)/XX
          TYLC(K)=TYLC(K)/XX                    
          TYLK(K)=TYLK(K)/XX                                                             
          SMY(3)=SMY(3)+TYLN(K)                                                          
          SMY(4)=SMY(4)+TYLP(K)                                                          
          TDM(K)=TDM(K)/XX                                                               
	      THU(K)=THU(K)/XX                                                                    
	      IF(IDC(K)==NDC(3).OR.IDC(K)==NDC(6).OR.IDC(K)==NDC(7).OR.IDC&                       
          (K)==NDC(8).OR.IDC(K)==NDC(10))XX=IY                                           
	      PSTM(K)=PSTM(K)/XX                                                                  
          TCSO(K)=TCSO(K)/XX                                                             
          TCST(K)=TCST(K)/XX+X6                                                          
	      TVAL(K)=TVAL(K)/XX                                                                  
	      TFTK(K)=TFTK(K)/XX                                                                  
          TFTN(K)=TFTN(K)/XX                                                             
          TFTP(K)=TFTP(K)/XX                                                             
          TRA(K)=TRA(K)/XX                                                               
          TRD(K)=TRD(K)/XX                                                               
          TVIR(K)=TVIR(K)/XX                                                             
          TIRL(K)=TIRL(K)/XX                                                             
          TCAW(K)=TCAW(K)/XX                                                             
          TCRF(K)=TCRF(K)/XX                                                             
          TCQV(K)=TCQV(K)/XX 
          X3=X3/XX                                                            
          DO L=1,4                                                                       
              TSFC(L,K)=TSFC(L,K)/XX                                                       
              STDA(L,K)=STDA(L,K)/XX                                                       
          END DO                                                                         
          DO L=5,7                                                                       
              TSFC(L,K)=TSFC(L,K)/XX                                                       
          END DO                                                                         
          X1=TVAL(K)-TCST(K)                                                             
          X2=TVAL(K)-TCSO(K)                                                             
    !     PRINTOUT CROP SUMMARY                                                          
          WRITE(KW(1),326)CPNM(K),TYL1(K),TYL2(K),TDM(K),TYLN(K),TYLP(K),&               
          TYLK(K),TYLC(K),TFTN(K),TFTP(K),TFTK(K),TVIR(K),TIRL(K),TCAW(K),&
          X3,TETG(K),TRA(K),THU(K),PSTM(K),TCST(K),TCSO(K),TVAL(K),X1,X2                           
          WRITE(KW(1),577)(TSFC(L,K),L=1,7),(STDA(L,K),L=1,3)                            
      END DO
  !     PRINTOUT SUMMARY                                                               
      IF(IWP5>0)REWIND KR(20)                                                        
      IF(KFL(3)>0)THEN                                                               
	      WRITE(KW(3),578)(TITLE(I),I=21,35),IYER,IMON,IDAY,IY                              
          WRITE(KW(3),560)HED(4),HED(10),HED(11),HED(14),HED(16),HED(17),&             
          HED(29),HED(NDVSS),HED(42),HED(48),HED(47),HED(50),HED(51),HED&              
          (52),HED(49),HED(43),HED(44),HED(45),HED(46),HED(56),HED(54),HED&            
          (55),HED(57),HED(66),HED(77)                                                 
          WRITE(KW(3),498)SM(4),SM(10),SM(11),SM(14),SM(16),SM(17),SM(29),&            
          SM(NDVSS),SM(42),SM(48),SM(47),SM(50),SM(51),SM(52),SM(49),SM&               
          (43),SM(44),SM(45),SM(46),SM(56),SM(54),SM(55),SM(57),SM(66),SM&             
          (77),(PSTN(K),SMAP(1,K),K=1,10),(CPNM(K),TYL1(K),TYL2(K),TDM(K),&            
          TYLN(K),TYLP(K),TYLC(K),TFTN(K),TFTP(K),TVIR(K),TIRL(K),TETG(K),&
          TCAW(K),TCRF(K),TCQV(K),THU(K),PHU(K,IHU(K)),TCST(K),TCSO(K),&
          TVAL(K),PSTM(K),(TSFC(L,K),L=1,7),(STDA(L,K),L=1,3),K=1,LC)
      END IF                                                                         
      IF(KFL(MSO+1)>0)WRITE(KW(MSO+1),694)ASTN,CPNM(1),TYL1(1),(SM&                  
      (KYA(J)),J=1,NKYA)                                                             
	  IF(KFL(NGF)>0)THEN
	      X2=SM(49)+SM(52)                                                                    
	      X3=SM(43)+SM(44)+SM(45)+SM(46)                                                      
	      DO K=1,8                                                                            
              SMGS(K)=SMGS(K)/XYR                                                          
          END DO                                                                         
          SMGS(2)=SMGS(2)/365.25                                                         
	      WRITE(KW(NGF-1),512)XLOG,YLAT,SMGS(1),TETG(1),SMGS(3),SMGS(4),SM&                   
          (19),SMGS(5),SM(11),SMGS(6),SMGS(7),TRA(1),TYLN(1),SMGS(2),X2,X3,&             
          SM(42),SM(NDVSS),SM(3),TDM(1),(TSFC(L,1),L=1,7),SM(46),SM(43),SM&              
          (47),SM(49),SM(50),SM(85),SM(51),SM(52),SM(53),SM(54),SM(56),SM&               
          (57),SM(58),SM(59),SM(60),SM(61),SM(62),SM(63),TYLP(1),TYLK(1),SM&             
          (77),SM(4),SM(17),SM(14),SM(5),SM(6),SM(10),SM(12),SM(13),SM(16),&             
          SM(18),SM(15),SM(20),SMGS(8),SM(68)
      END IF                                            
      IF(NBSL<3.OR.ZF<1.E-10)GO TO 507                                               
!  1  JZ(1) = NUMBER OF Y FOR SECOND THRU LAST SIMULATION.                           
!  2  JZ(2) = 0 FOR NORMAL EROSION OF SOIL PROFILE                                   
!           = 1 FOR STATIC SOIL PROFILE                                              
!  3  JZ(3) = ID NUMBER OF WEATHER VARIABLES INPUT.  RAIN=1,  TEMP=2,                
!            RAD=3,  WIND SPEED=4,  REL HUM=5.  IF ANY VARIABLES ARE INP             
!            RAIN MUST BE INCLUDED.  THUS, IT IS NOT NECESSARY TO SPECIF             
!            ID=1 UNLESS RAIN IS THE ONLY INPUT VARIABLE.                            
!            LEAVE BLANK IF ALL VARIABLES ARE GENERATED.  EXAMPLES                   
!            NGN=1 INPUTS RAIN.                                                      
!            NGN=23 INPUTS RAIN, TEMP, AND RAD.                                      
!            NGN=2345 INPUTS ALL 5 VARIABLES.                                        
!  4  JZ(4) = DAILY WEATHER STA # FROM KR(27) WTHCOM.DAT                             
      READ(KR(6),300)JZ                                                              
      IF(JZ(1)==0)GO TO 507                                                          
	  NBYR=JZ(N1)                                                                         
      ISTA=JZ(2)                                                                     
      NGN=JZ(3)                                                                      
      IWTH=JZ(4)                                                                     
      IPY=1                                                                          
      IF(ICOR==0)GO TO 203                                                           
      DO K=1,12                                                                      
	      IF(TR(K)>0.)RNCF(K)=RMO(1,K)/TR(K)                                                
	      TMNF(K)=(OBMX(1,K)-OBMN(1,K))/(TXMX(K)-TXMN(K))                                   
          TMXF(K)=OBMX(1,K)-TXMX(K)                                                    
          IF(NGN>0)CYCLE                                                               
          IF(ICOR<=NC(K+1)-NYD)EXIT                                                    
	  END DO                                                                              
      ICOR=0                                                                         
  203 CALL ARESET                                                                    
      IF(NGN==0)THEN
	      WRITE(KW(1),'(/T10,A)')'**********RAIN, TEMP, RAD, WIND SPEED, &
	      REL HUM ARE GENERATED**********'                                                
          GO TO 533
      END IF                                                                      
      IF(IRW==0)THEN
          REWIND KR(7)                                                                   
          CALL WDOP(IDIR(1))                                                        
    !     IYR=IYR0
      END IF                                                                       
      WRITE(KW(1),293)                                                               
      CALL WIGV                                                                      
      GO TO 533                                                                      
  507 IGN=IGN+100                                                                    
      REWIND KR(6)                                                                   
      IF(IGN<IGMX*100)GO TO 538                                                      
      IRO0=IRO0+1                                                                    
      IGN=0                                                                          
      IF(IRO0>NRO.OR.IGMX==0)GO TO 532                                               
  538 REWIND KR(1)                                                                   
      IF(NGN0>0)REWIND KR(7)                                                         
      GO TO 531                                                                      
  532 CLOSE(KR(1))                                                                   
      IF(IRW==0)CLOSE(KR(7))                                                         
	  DO I=2,32                                                                           
          CLOSE(KW(I))                                                                 
	  END DO                                                                              
	  CALL ATIMER(1)                                                                      
      CLOSE(KW(1))
      IF(IBAT>0)GO TO 219
  740 CONTINUE
  219 DO I=1,SIZE(KR)                                                                      
          CLOSE(KR(I))                                                                 
	  END DO
	  DO I=2,SIZE(KW)                                                                         
          CLOSE(KW(I))                                                                 
	  END DO                                                                              
	  STOP                                                                                
   35 FORMAT(T7,A4,11F10.0)                                                          
   49 FORMAT(T7,A4,11F10.1)                                                          
   53 FORMAT(T7,A4,11F10.2)                                                          
   63 FORMAT(T7,A4,11F10.3)                                                          
   78 FORMAT(T7,A4,11F10.4)                                                          
  221 FORMAT (T10,'VERNALIZATION TIME = ',F5.0,' D'/T10,'ANNUAL HEAT UNI&            
      TS = ',F8.0,' C')                                                              
  223 FORMAT(1X,A4,12F9.1,11X,A4)                                                    
  224 FORMAT(1X,A4,12F9.2,11X,A4)                                                    
  225 FORMAT(1X,A4,12F9.3,11X,A4)                                                    
  229 FORMAT(3X,'1',4X,'2',4X,'3',4X,'4',4X,'5',4X,'6',4X,'7',4X,'8',&               
      4X,'9',3X,'10',3X,'11',3X,'12',3X,'13',3X,'14',3X,'15',3X,'16',3X,&            
      '17',3X,'18',3X,'19',3X,'20')                                                  
  230 FORMAT(5X,3I2,F6.0,2F6.2)                                                      
  233 FORMAT(T10,'PEAK RATE-EI ADJ FACTOR = ',F5.3/T10,'INITIAL WATER CO&            
      NTENT OF SNOW = ',F5.1,' mm'/T10,'AVE N CONC IN RAINFALL = ',F4.2,&            
      ' ppm'/T10,'CONC OF NO3 IN IRRIGATION WATER = ',F6.0,' ppm'/T10,&              
      'CONC OF SALT IN IRRIGATION WATER = ',F8.0,' ppm')                             
  234 FORMAT(/1X,'-----CURVE NUMBER DISTRIBUTION'/T10,'>95=',F4.2,3X,'>&             
      90=',F4.2,3X,'>85=',F4.2,3X,'>80=',F4.2,3X,'>75=',F4.2,3X,'>70=',&             
      F4.2,3X,'>65=',F4.2,3X,'>60=',F4.2,3X,'>55=',F4.2,3X,'<55=',F4.2)              
  235 FORMAT(3I3,4I5,10F8.0)                                                         
!  235 FORMAT(3I2,4I5,10F8.0)                                                     
  236 FORMAT('+',T50,F7.2,T118,A4)                                                   
  239 FORMAT(2F8.3)                                                                  
  242 FORMAT(I5,E13.5,2F10.3,2E13.5)                                                 
  243 FORMAT(1X,A4,13F9.1,2X,A4)                                                     
  245 FORMAT(////1X,'-----MISCELLANEOUS PARAMETERS'//T10,'PRMT',9X,'SCRP&            
      1I',4X,'SCRP2I',4X,'SCRP1C',7X,'SCRP2C')                                       
  247 FORMAT(/T10,'IRRIGATION WATER APPLIED'/T7,'DATE',T14,'VOL',3X,'COS&            
      T',3X,'HU'/T7,'Y M D   mm   $/ha   SCD')                                       
  248 FORMAT(10X,11(6X,A4))                                                          
  281 FORMAT(T7,'IDC ',11I10,'  IDC')                                                
  285 FORMAT(T10,'FIELD LENGTH = ',F6.2,' km'/T10,'FIELD WIDTH = ',&                 
      F6.2,' km'/T10,'FIELD ANGLE = ',F4.0,' deg'/T10,'WIND SPEED MOD EX&            
      P POWER PRMT = ',F4.2/T10,'SOIL PARTICLE DIAM = ',F5.0,' UM'/T10,&             
      'ACCELERATE WIND EROS FACTOR = ',F7.3)                                         
  286 FORMAT(5X,3I4)                                                                 
  287 FORMAT(20(1X,A4))                                                              
  290 FORMAT(T10,'SOIL ALBEDO = ',F4.2/T10,'MAX NUMBER SOIL LAYERS = ',&             
      F3.0/T10,'MIN THICKNESS FOR LAYER SPLITTING = ',F4.2,' m'/T10,'MIN&            
       PROFILE THICKNESS--STOPS SIMULATION = ',F4.2,' m'/T10,'SPLITTING'&            
      ,' PRIORITY THICKNESS = ',F4.2,' m'/T10,'BIOMASS/ORG C = ',F5.3/&              
      T10,'PASSIVE HUMUS/TOTAL HUMUS = ',F5.3/T10,'CULTIVATION HISTORY =&            
       ',F5.0,' y'/T10,'WEATHERING CODE = ',F3.0/T10,'SOIL GROUP CODE ='&            
      ,1X,F3.0/T10,'ORG C IN TOP ',F4.2,' m = ',F7.1,' t/ha')                        
  293 FORMAT(//I5,9(2X,A4,F10.2)/(5X,9(2X,A4,F10.2)))                                  
  295 FORMAT(/1X,'-----AVE MO VALUES')                                               
  297 FORMAT(///1X,'-----GENERATOR SEEDS AFTER',I9,' CYCLES'/(5X,10I12))             
  299 FORMAT(20A4)                                                                   
  300 FORMAT(20I4)                                                                   
  301 FORMAT(T10,'SIMULATION DURATION = ',I4,' Y'/T10,'BEGINNING DATE =&             
      ',3I4)                                                                         
  303 FORMAT(10F8.2)                                                                 
  304 FORMAT(20F8.2)                                                                 
  310 FORMAT(12F6.2,8X)                                                              
  311 FORMAT(1X,A4,13F9.0,2X,A4)                                                     
  316 FORMAT(/T10,'TILLAGE OPERATIONS'/6X,'DATE',T21,'OP',4X,'EQ',4X,&               
      'TR',3X,'OCST',3X,'TCST',2X,'SCST',5X,'MX',4X,'RR',4X,'DP',4X,'FR'&            
      ,3X,'RHT',3X,'RIN',3X,'DKH',3X,'DKI',4X,'HV',4X,'HV',8X,'NRCS'4X,&             
      'IRR',5X,'Q/'/6X,'Y M D',3X,'NAME',2X,'CD',4X,'NO',4X,'NO',1X,'___&            
      _____$/ha________',4X,'EF',4X,'mm',4X,'mm',2X,'COMP',4X,'mm',5X,&              
      'm',4X,'mm',5X,'m',4X,'EF',3X,'IDX',2X,'CROP',4X,'CN',3X,'TRGR',3X&            
      ,'VIRR',2X,'HUSC')                                                             
  317 FORMAT(5X,3I2,1X,A8,I2,2I6,2F7.2,7X,F6.2,2F6.0,F6.3,F6.0,F6.2,F6.0&            
      ,2F6.2,F6.3,5X,F7.1,2F7.2,2F6.3)                                               
  319 FORMAT(45X,'YR=',1X,I4,1X,I4/T11,'JAN',6X,'FEB',6X,'MAR',6X,'APR',&            
      6X,'MAY',6X,'JUN',6X,'JUL',6X,'AUG',6X,'SEP',6X,'OCT',6X,'NOV',6X,&            
      'DEC',6X,' YR')                                                                
  321 FORMAT(1X,A4,13F9.2,2X,A4)                                                     
  323 FORMAT(/1X,'-----PEAK FLOW RATE STATS(mm/h)',T70,'UNIT PEAKS(1/h)&             
      '/T10,'MAX = ',F7.2,5X,'MEAN = ',F6.2,5X,'ST DV = ',F6.2,5X,&                  
      'MAX = ',F7.4,5X,'MEAN = ',F6.4,5X,'NO PKS = ',I6)                             
  325 FORMAT(1X,A4,13F9.4,2X,A4)                                                     
  326 FORMAT(/2X,A4,1X,'YLD=',F5.1,'/',F5.1,2X,'BIOM=',F5.1,'t/ha',2X,&              
      'YLN=',F5.0,2X,'YLP=',F5.0,2X,'YLK=',F5.0,2X,'YLC='F5.0,2X,'FN=',&
      F5.0,2X,'FP=',F5.0,2X,'FK=',F5.0,'kg/ha'/T7,'IRGA=',F5.0,2X,'IRDL=',&
      F5.0,2X,'CAW=',F6.0,'mm',2X,'GSET=',F6.0,'mm',2X,'WUEF=',F5.2,&
      'kg/mm',2X,'RAD=',F7.0,'Mj/m2',2X,'HU=',F7.0,2X,'PSTF=',F4.2/T7,&
      'COST=',F7.2,2X,'COOP=',F7.2,2X,'RTRN=',F7.0,2X,'NTRN=',F7.0,2X,&
      'NTRO=',F7.0,'$/ha')                           
  327 FORMAT(T10,'TILE DRAIN IN SOIL LAYER',I3/T15,'DRAIN TIME = ',F5.2,&
      ' D'/T15,'FLOW RATE = ',F5.1,' mm/h')                                        
  328 FORMAT(/T10,'FERTILIZER APPLIED'/T7,'DATE',T25,'FT',4X,'EQ',4X,&               
      'TR',3X,'COST',4X,'WT',2X,'DPTH',1X,'-------------FRACTION OF WT--&            
      ------------'/T7,'Y M D',3X,'NAME',6X,'NO',4X,'NO',4X,'NO',3X,&                
      '$/ha',1X,'kg/ha',5X,'m'4X,'MN',3X,'NH3',4X,'ON',4X,'MP',4X,'OP',&             
      4X,'MK',2X,'HUSC')                                                             
  329 FORMAT(5X,3I2,1X,A8,3I6,F7.2,F6.0,8F6.3)                                       
  330 FORMAT(T10,'COSTS'/T15,'LIME = ',F5.2,' $/t'/T15,'IRR WATER = ',&              
      F4.2,' $/mm'/T15,'FUEL PRICE = ',F5.2,' $/l'/T15,'WAGE PRICE =',&              
      F5.2,' $/h'/T15,'MISCEL COST 1 = ',F7.2,' $/ha'/T15,'MISCEL COST 2 &            
      = ',F7.2,' $/ha')                                                             
  331 FORMAT(T10,'AUTOMATIC IRRIGATION')                                             
  348 FORMAT(//1X,'____________________CROP PARAMETERS_____________________'/)
  350 FORMAT(/1X,'____________________SUMMARY TABLE____________________')                                                                              
  351 FORMAT(T10,'WATER TABLE DEPTH'/T15,'MIN = ',F6.2,' m'/T15,'MAX = '&            
      ,F6.2,' m'/T15,'INITIAL = ',F6.2,' m'/T10,'GROUNDWATER STORAGE = '&            
      ,F5.0,' mm'/T10,'MAX GROUNDWATER STORE = ',F5.0,' mm'/T10,'RETURN'&            
      ,' FLOW TT = ',F7.2,' d')                                                      
  354 FORMAT(T15,'MAX ANNUAL VOL APPL TO A CROP = ',F5.0,' mm'/T15,'MIN &             
      SINGLE APPL VOL = ',F5.0,' mm'/T15,'MAX SINGLE APPL VOL = ',F5.0,&            
      ' mm')                                                                         
  355 FORMAT(T15,'SOIL-WATER TENSION TRIGGER = ',F5.0,' kpa'/T15,'MIN A&             
      PPL INTERVAL = ',I3,' d')                                                      
  360 FORMAT(T15,'PLANT WATER STRESS TRIGGER = ',F4.2/T15,'MIN APPL INTE&            
      RVAL = ',I3,' d')                                                              
  365 FORMAT(T10,'DRAINAGE AREA = ',F9.2,' ha'/T10,'LATITUDE = ',F7.2,' &             
      deg'/T10,'LONGITUDE = ',F7.2,' deg'/T10,'ELEVATION = ',F7.1,' m'/&            
      T10,'CHANNEL'/T15,'LENGTH = ',F6.2,' km',5X,'GRAD = ',F6.4,' m/m',&            
      5X,'MANNINGS N = ',F5.3,5X,'DEPTH = ',F6.3,' m'/T10,'LAND SLOPE'/&             
      T15,'LENGTH = ',F6.1,' m',5X,'GRAD = ',F7.4,' m/m',5X,'AZIMUTH = ',&
      F4.0,' deg',5X,'MANNINGS N = ',F5.3)                                                          
  369 FORMAT(6X,9(1X,A4,F9.4))                                                       
  377 FORMAT(/T10,'PESTICIDES APPLIED'/T7,'DATE',T33,'PS',4X,'EQ',4X,&               
        'TR',3X,'COST',2X,'RATE',2X,'KILL'/T7,'Y M D   NAME',14X,'NO',4X,&              
        'NO',4X,'NO',3X,'$/ha',1X,'kg/ha',4X,'EF',2X,'HUSC')                            
  378 FORMAT(5X,3I2,1X,A16,3I6,F7.2,3F6.2)                                           
  380 FORMAT(10X,A16,3F10.0,F10.2,F10.0,F10.2)                                       
  382 FORMAT(T43,'HALF LIFE(DAYS)  WASH OFF',T83,'COST'/T13,'NAME',11X,&             
      'SOLUBILITY',5X,'SOIL',4X,'FOLIAGE',3X,'FRACTION',3X,'KOC',7X,'($/',&            
      'KG)')                                                                          
  383 FORMAT(/13X,10(A8,4X))                                                         
  384 FORMAT(5X,A4,10F10.0)                                                          
  387 FORMAT(5X,A4,10F10.4)                                                          
  390 FORMAT(T10,'HYDROLOGIC SOIL GROUP = ',A1/T10,'LAND USE NUMBER = ',&
      I3/T10,'RUNOFF CN2 = ',F4.1/T10,'SLP ADJ CN2 = ',F4.1/T10,'CN SCRV &
      SCRP(30)= ',2F6.0/T10,'CN SCRP(4)= ',2E13.5)                                                              
  396 FORMAT(8F10.6)                                                                 
  405 FORMAT(T15,'LS= ',F6.3/T10,'RUSLE XM = ',F5.3/T10,'RUSLE SLP LG FA&            
     &C = ',F5.3/T10,'RUSLE SLP FAC = ',F5.3/T10,'TIME OF FLOW CONC = ',&            
     &F5.2,' h')                                                                     
  412 FORMAT(T15,'PAW DEFICIT TRIGGER = ',F5.0,' mm'/T15,'MIN APPL INTE&             
     &RVAL = ',I3,' d')                                                              
  417 FORMAT(/1X,'-----TIME OF CONCENTRATION STATS(h)'/T10,'MEAN = ',&               
     &F6.2,5X,'MIN = ',F6.2,5X,'MAX = ',F6.2)                                        
  436 FORMAT(T10,'NUMBER OF COWS = ',I8,' hd'/T10,'GRAZING LIMIT = ',&               
     &F6.3,' t/ha'/T10,'FRACTION TIME COWS IN FEED AREA = ',F5.3)                    
  445 FORMAT(T15,'BIG GUN IRRIGATION FROM LAGOON'/T15,'CONFINEMENT AREA&             
     &= ',F7.2,' ha'/T15,'NORMAL LAGOON VOL = ',F5.0,' mm'/T15,'MAX LAGO&            
     &ON VOL = ',F5.0,' mm'/T15,'DRAW DOWN TIME = ',F5.0,' d'/T15,'WASH&             
     &WATER = ',F6.3,' m3/cow/d')                                                  
  448 FORMAT(/1X,'-----SEDIMENT OF CONCENTRATION STATS(g/m3)'/T10,'MEAN&             
     &= ',F10.0,5X,'MAX = ',F10.0,5X,'STDV = ',F10.0)                                
  460 FORMAT(/1X,'-----FREQUENCY & DURATION OF PESTICIDE LOSS(g/ha)'/T35&            
     &,'DURATION(d)'/20X,'1',12X,'4',11X,'21',11X,'60',11X,'90')                     
  462 FORMAT(5X,A16,T35,'MAXIMUM')                                                   
  463 FORMAT(8X,'SOL  ',5E13.5)                                                      
  464 FORMAT(8X,'Q+SSF',5E13.5)                                                      
  465 FORMAT(8X,'ADSRB',5E13.5)                                                      
  466 FORMAT(8X,'SED Y',5E13.5)                                                      
  470 FORMAT(5X,A4,10E12.4)                                                          
  473 FORMAT(T35,'50 % EXCEED')                                                      
  474 FORMAT(T35,'10 % EXCEED')
  498 FORMAT(3F8.0,3F8.1,F8.3,18F8.2,10(1X,A16,F8.0),10(4X,A4,19F8.2,&               
      11F8.1))                                                                                                                                    
  501 FORMAT(1X,3I2,2X,A16,I6,2X,4I4,F7.2,7X,F7.2,F7.2)                              
  503 FORMAT(1X,3I2,2X,'IRGA',12X,I6,6X,I4,8X,F7.2,7X,F7.2,F7.0)                     
  505 FORMAT(A20)                                                                    
!  508 FORMAT(1X,A5,4X,A20)
  508 FORMAT(1X,A5,4X,A)
!  509 FORMAT(10X,A20)
  509 FORMAT(10X,A)
  510 FORMAT(1X,3I2,2X,A8,8X,I6,2X,4I4,F7.2,7X,F7.2,F7.0)                            
  512 FORMAT(1X,4F10.2,5F10.1,2F10.3,F10.0,50F10.2)                                  
  522 FORMAT(T10,A12,3I4)                                                            
!  523 FORMAT(T10,A12,2X,3I4) 
  523 FORMAT(T10,2(A,1X),1X,3I4)                                                        
  528 FORMAT(10F10.0,I10)                                                            
!  551 FORMAT(1X,' YR  RT#',1X,'CPNM',3X,'YLDG',4X,'YLDF',4X,'WCYD',4X,&
!      '  HI',4X,'BIOM',4X,'  RW',4X,' YLN',4X,' YLP',4X,' YLC',4X,' FTN',&
!      4X,' FTP',4X,' FTK',4X,'IRGA',4X,'IRDL',4X,'WUEF',4X,'GSET',4X,' CAW',4X,&
!      ' CRF',4X,' CQV',4X,'COST',4X,'COOP',4X,'RYLG',4X,'RYLF',4X,'PSTF',&
!      4X,'  WS',4X,'  NS',4X,'  PS',4X,'  KS',4X,'  TS',4X,'  AS',4X,&
!      '  SS',4X,'PPOP',5X,'IPLD',5X,'IGMD',5X,'IHVD ')                                     
  551 FORMAT('  YR  RT# CPNM',4X,'YLDG',5X,'YLDF',5X,'WCYD',5X,&
      '  HI',5X,'BIOM',5X,'  RW',5X,' YLN',5X,' YLP',5X,' YLC',5X,' FTN',&
      5X,' FTP',5X,' FTK',5X,'IRGA',5X,'IRDL',5X,'WUEF',5X,'GSET',5X,' CAW',5X,&
      ' CRF',5X,' CQV',5X,'COST',5X,'COOP',5X,'RYLG',5X,'RYLF',5X,'PSTF',&
      5X,'  WS',5X,'  NS',5X,'  PS',5X,'  KS',5X,'  TS',5X,'  AS',5X,&
      '  SS',5X,'PPOP',6X,'IPLD',6X,'IGMD',6X,'IHVD ')                                     
  557 FORMAT(1X,' YR  RT#',24(4X,A4),4X,'OCPD',4X,' TOC',4X,'APBC',4X,&              
     &' TAP',4X,'ZNO3')                                                              
!  558 FORMAT(1X,'RUN   YR',4X,'AP15',41(3X,A4,1X))                                   
  558 FORMAT(' RUN    YR',6X,'AP15',9X,'PMTE',41(6X,A4,3X))
  560 FORMAT(25(4X,A4),10(9X,'PSTN',8X,'APRT'),10(4X,'CPNM',4X,'YLDG',4X,&            
      'YLDF',4X,'BIOM',4X,' YLN',4X,' YLP',4X,' YLC',4X,' FTN',4X,' FTP',&
      4X,'IRGA',4X,'IRDL',4X,'WUEF',4X,' CAW',4X,' CRF',4X,' CQV',4X,' THU',&            
      4X,' PHU',4X,'COST',4X,'COOP',4X,'RETN',4X,'PSTF',4X,'  WS',4X,&              
      '  NS',4X,'  PS',4X,'  KS',4X,'  TS',4X,'  AS',4X,'  SS',4X,'  BD,'&            
      4X,'ALSA',4X,' SRT'))                                                         
  571 FORMAT(/5X,'STAGE =',I2,5X,'WPM5 = ',A80/T11,'JAN',6X,'FEB',6X,&               
      'MAR',6X,'APR',6X,'MAY',6X,'JUN',6X,'JUL',6X,'AUG',6X,'SEP',6X,&               
      'OCT',6X,'NOV',6X,'DEC',6X,' YR')                                              
  576 FORMAT(1X,A4,20F10.0)                                                          
  577 FORMAT(T7,'STRESS (BIOM) WATER=',F5.1,2X,'N=',F5.1,2X,'P=',F5.1,2X&            
      ,'K=',F5.1,2X,'TEMP=',F5.1,2X,'AIR=',F5.1,2X,'SALT=',F5.1,5X,&                 
      '(ROOT) BD=',F5.1,2X,'ALSAT=',F5.1,2X,'TEMP=',F5.1,'D')                        
  578 FORMAT(1X,15A4,1X,3I4,1X,I3)                                                
  581 FORMAT(1X,A4,20F10.3)                                                          
  582 FORMAT(1X,3I2,2X,A8,8X,I6,6X,3I4,3F7.2)                                        
  583 FORMAT(T61,'COTL',6X,'COOP',6X,'MTCO',6X,'MASS',6X,'FUEL'/4X,'Y',&             
      3X,'M',3X,'D',5X,'OP',14X,'CROP',2X,'MT#  HC',2X,'EQ  TR',2X,&                 
      '|----------($/ha)-----------|',2X,'(kg/ha)',4X,'(L/HA)')                      
!  592 FORMAT(T10,A12)                                                                
  592 FORMAT(T10,A)                                                                
  602 FORMAT(19X,'WPM1 = ',A/T11,'JAN',6X,'FEB',6X,'MAR',6X,'APR',6X,&             
      'MAY',6X,'JUN',6X,'JUL',6X,'AUG',6X,'SEP',6X,'OCT',6X,'NOV',6X,&               
      'DEC',6X,' YR')                                                                
  607 FORMAT(2X,I4)                                                                  
!  621 FORMAT(/T5,'EPIC1102',2X,3I4,2X,2(I2,':'),I2/)                      
  621 FORMAT(/T5,A,'(',A,')',2X,3I4,2X,2(I2,':'),I2/)                                        
!    622 FORMAT(/T5,'EPIC1102 - For comparison only!',2X,3I4)                               
  622 FORMAT(/T5,A,' - For comparison!')                               
  ! 625 FORMAT(15F8.2,A20)
  ! 626 FORMAT(15G8.2)
  630 FORMAT(4X,A4,2F8.0)                                                            
  635 FORMAT(///T52,'SOIL LAYER NO'/T5,16(6X,A4))                                    
  636 FORMAT(5X,'FINAL CONTENTS:'/5X,'N2OG=',E13.6,2X,'N2OL=',E13.6,2X,&
      'NO2 =',E13.6,2X,'NO3 =',E13.6,2X,'NH3 =',E13.6,2X,'ORGN=',E13.6/5X,&
      'STDN=',E13.6,2X,'SDON=',E13.6,2X,'UN1 =',E13.6)              
  637 FORMAT(5X,'FINAL CONTENTS:'/5X,'LSC =',E13.6,2X,'LMC =',E13.6,2X,&             
      'BMC =',E13.6,2X,'HSC =',E13.6,2X,'HPC =',E13.6,2X,'TOC =',E13.6/&
      5X,'BIOC=',E13.6,2X,'STD =',E13.6)                               
  638 FORMAT(5X,'FINAL CONTENTS:'/5X,'PLAB=',E13.6,2X,'PMS =',E13.6,2X,&             
      'PMA =',E13.6,2X,'PHUM=',E13.6,2X,'FOP =',E13.6,2X,'STDP=',E13.6/&             
      5X,'STOP=',E13.6,2X,'UP1 =',E13.6)                                             
  639 FORMAT(5X,'FINAL CONTENTS:'/5X,'TSK =',E13.6,2X,'TEK =',E13.6,2X,&             
      'TFK =',E13.6,2X,'STDK=',E13.6,2X,'STOK=',E13.6,2X,'UK1 =',E13.6)              
  661 FORMAT(/1X,'-----SOI STAGE DISTRIBUTION'/T10,'1 = ',F5.3,2X,'2 = '&            
      ,F5.3,2X,'3 = ',F5.3,2X,'4 = ',F5.3,2X,'5 = ',F5.3,2X,'6 = ',F5.3)             
  662 FORMAT(/1X,'-----ANNUAL RAINFALL DISTRIBUTION'/T10,'MAX = ',F6.0,&             
      ' mm',2X,'MIN = ',F6.0,' mm',2X,'STDV = ',F6.0,'mm')                           
  669 FORMAT(/3X,'DATE',8X,'CN',7X,'RAIN',7X,'Q',9X,'TC',8X,'QP',7X,&                
      'DUR',6X,'ALTC',7X,'AL5'/4X,'Y M D',T25,'(mm)',6X,'(mm)',7X,'(H)',&            
      5X,'(mm/h)',6X,'(H)')                                                          
!  671 FORMAT(4X,'RUN #=',A8,2X,'LAT=',F7.2,' deg',2X,'EQUIV LAT=',F7.2,&
!      ' deg',2X,'SLOPE=',F7.4,' m/m'/4X,'CONTROL SECTION DEPTHSm =',&
!      3F6.3,2X,2A20/9X,'DATE',T23,'CONTROL SECTION'/2X,'Y#',4X,'Y   M   D',&
!      5X,'SW1mm',5X,'SW2mm',3X,'SW10m/m',3X,'SW20m/m',3X,'SW50m/m',3X,&
!      'SW100m/m',5X,'T10c',6X,'T20c',6X,'T50c',5X,'T100c')                                 
  671 FORMAT(4X,'RUN #=',A,2X,'LAT=',F7.2,' deg',2X,'EQUIV LAT=',F7.2,&
      ' deg',2X,'SLOPE=',F7.4,' m/m'/5X,'CONTROL SECTION DEPTHSm =',&
      3F6.3,2X,2A20/9X,'DATE',T23,'CONTROL SECTION'/2X,'Y#',4X,'Y   M   D',&
      5X,'SW1mm',5X,'SW2mm',3X,'SW10m/m',3X,'SW20m/m',3X,'SW50m/m',3X,&
      'SW100m/m',5X,'T10c',6X,'T20c',6X,'T50c',5X,'T100c')                                 
!  679 FORMAT(/5X,'RUN= ',A8,2X,'SIT#= ',I8,2X,'WP1#= ',I8,2X,'SOL#= ',&             
!     &I8,2X,'OPS#= ',I8,2X,'WTH#= ',I8)                                                             
  679 FORMAT(/5X,'RUN= ',A,2X,'SIT#= ',I8,2X,'WP1#= ',I8,2X,'SOL#= ',&             
      I8,2X,'OPS#= ',I8,2X,'WTH#= ',I8)                                                             
  693 FORMAT(13X,'CPNM',3X,'YLDG',26(4X,A4),5(4X,'CPNM',4X,'YLDG',4X,&               
      'YLDF',4X,'BIOM',4X,' YLN',4X,' YLP',4X,' YLC',4X,' FTN',4X,' FTP',&
      4X,'IRGA',4X,'IRDL',4X,'WUEF',4X,' CAW',4X,' CRF',4X,' CQV',4X,&
      ' THU',4X,' PHU',4X,'COST',4X,'COOP',4X,'RETN',4X,'PSTF',4X,'  WS',&
      4X,'  NS',4X,'  PS',4X,'  KS',4X,'  TS',4X,'  AS',4X,'  SS',4X,&
      '  BD',4X,'ALSA',4X,' SRT'))                                                             
  694 FORMAT(1X,A8,4X,A4,F8.2,F8.0,F8.1,6F8.2,F8.0,F8.4,2F8.3,10(4X,A4,&             
      10F8.2,5F8.0,4F8.2,10F8.1))                                                    
  ! 695 FORMAT(15F8.0,A20)                                                              
  695 FORMAT(15(F12.0,1X),A)
  714 FORMAT(T10,'AUTO LIME EQUIP = ',A8,2X,'DEPTH = ',F6.3,' M')                    
  715 FORMAT(/T10,'AUTO IRR EQUIP  = ',A8,2X,'DEPTH = ',F6.3,' M')                   
  716 FORMAT(T10,'AUTO FERT EQUIP = ',A8,2X,'DEPTH = ',F6.3,' M',2X,&                
      'FERT = ',A8/)                                                                 
  722 FORMAT(T55,'RWT SOIL LAYER #'/3X,'Y',3X,'Y#',' M D',1X,'CROP',4X,&             
      'BIOM',16(4X,A4))                                                              
  723 FORMAT(10X,A15,1X,16F8.2)                                                      
!  726 FORMAT(3X,'Y',3X,'Y#',1X,'CROP',4X,' YLD',4X,'BIOM',4X,' RWT',4X,&             
!     &' LAI',4X,' STD')                                                              
  726 FORMAT(3X,'Y',4X,'Y#',1X,'CROP',5X,' YLD',5X,'BIOM',5X,' RWT',5X,&             
      ' LAI',5X,' STD')                                                              
  727 FORMAT(4X,'Y',3X,'M',3X,'D',1X,'OPERATION',2X,'CROP',2X,'BIOMt/ha'&            
      ,3X,'RWTt/ha',6X,'LAI',4X,'STLt/ha',2X,'AGPMt/ha',5X,'ORHI',4X,&               
      'YLDt/ha',2X,'YLSDt/ha',6X,'HUSC')                                             
!  729 FORMAT(2X,'YR   YR#',5X,'Q',5X,'SSF',5X,'PRK',4X,'QDRN',7X,'Y',5X,&            
!      'YOC',5X,'PSTN',11X,'CPNM',4X,'PAPL',4X,'PSRO',4X,'PLCH',4X,'PSSF',&
!      4X,'PSED',4X,'PDGF',4X,'PDGS',4X,'PDRN',4X,'CMX4D'/12X,'|---------',&
!      '---(mm)------------|  (t/ha)',1X,'(kg/ha)',17X,'|----------------',&
!      '-------------(g/ha)--------------------------------|',3X,'(ppb)')                                                              
  729 FORMAT('  YR   YR#',6X,'Q',6X,'SSF',6X,'PRK',5X,'QDRN',8X,'Y',6X,&            
      'YOC',5X,'PSTN',13X,'CPNM',5X,'PAPL',5X,'PSRO',5X,'PLCH',5X,'PSSF',&
      5X,'PSED',5X,'PDGF',5X,'PDGS',5X,'PDRN',6X,'CMX4D'/12X,'|---------',&
      '------(mm)---------------|   (t/ha)  (kg/ha)',17X,'|----------------',&
      '------------------(g/ha)--------------------------------|',4X,'(ppb)')                                                              
  730 FORMAT(A80)                                                                    
!  826 FORMAT(4X,'Y   M   D',5X,'PDSW',50(6X,A4))                                     
  826 FORMAT(4X,'Y   M   D',7X,'PDSW',60(7X,A4))                                     
      END                                                                            
