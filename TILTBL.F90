      SUBROUTINE TILTBL
!     EPIC1102
!     THIS SUBPROGRAM READS EQUIPMENT TABLE TO DETERMINE PARAMETERS OF
!     INPUT OPERATIONS AND COMPUTES OPERATION COSTS(EQUIPMENT + TRACTOR)
      USE PARM
      CHARACTER*4 PCAT,PCD
      CHARACTER*8 TILX
      DIMENSION XTP(13),PCAT(5)
      DATA PCAT/'POWE','NON ','SELF','IRRI','CUST'/
      IF(NDT>0)THEN
          DO L=1,MXT
              IF(NBE(L)/=JX(4).OR.NBT(L)/=JX(5))CYCLE
              NDT=L
              RETURN
          END DO
      END IF
      NDT=MXT+1
      MXT=NDT
      NBE(NDT)=JX(4)
      NBT(NDT)=JX(5)
      NN=JX(4)
      COTL(NDT)=0.
      COOP(NDT)=0.
      FLAB=1.
	  JJ=JX(4)
      DO 5 J=1,2
      READ(KR(3),25)TILX
      READ(KR(3),25)TILX
!     READ EQUIPMENT DATA TABLE
!     TIL  = EQUIPMENT NAME
!     PCD  = POWER CODE
!  1  PRIC = PURCHASE PRICE($)--EXCEPTION CUSTOM = COST($/ha)
!  2  XLP  = INITIAL LIST PRICE IN CURRENT($)
!  3  HRY  = ANNUAL USE(H)
!  4  HRL  = LIFE OF EQUIP(h)
!  5  PWR  = POWER OF UNIT(kw)
!  6  WDT  = WIDTH OF PASS(m)
!  7  SPD  = OPERATING SPEED(km/h)
!  8  RC1  = REPAIR COST COEF 1
!  9  RC2  = REPAIR COST COEF 2
! 10  XLB  = LUBRICANT FACTOR
! 11  FCM  = FUEL CONSUMPTION MULTIPLIER
! 12  RFV1 = REMAINING FARM VALUE PARM 1
! 13  RFV2 = REMAINING FARM VALUE PARM 2
! 14  EFM  = MACHINE EFFICIENCY
!          = GRAZING RATE (KG/HD/D)IHC=19
! 15  RTI  = ANNUAL REAL INTEREST RATE($/$)
! 16  EMX  = MIXING EFFICIENCY (0-1)
! 17  RR   = RANDOM SURFACE ROUGHNESS CREATED BY TILLAGE OPERATION (MM)
! 18  TLD  = TILLAGE DEPTH(MM)
! 19  RHT  = RIDGE HEIGHT (MM)
! 20  RIN  = RIDGE INTERVAL (m)
! 21  DKH  = HEIGHT OF FURROW DIKES (MM) (BLANK IF DIKES NOT USED)
! 22  DKI  = DISTANCE BETWEEN FURROW DIKES (m)(BLANK IF DIKES NOT USED
! 23  IHC  = OPERATION CODE
!             1 = KILL CROP
!             2 = HARVEST WITHOUT KILL
!             3 = HARVEST ONCE DURING SIMULATION WITHOUT KILL
!             4 =
!             5 = PLANT IN ROWS
!             6 = PLANT WITH DRILL
!             7 = APPLY PESTICIDE
!             8 = IRRIGATE
!             9 = FERTILIZE
!            10 = BAGGING & TIES (COTTON)
!            11 = GINNING
!            12 = HAULING
!            13 = DRYING
!            14 = BURN
!            15 = PUDDLE
!            16 = DESTROY PUDDLE
!            17 = BUILDS FURROW DIKES
!            18 = DESTROYS FURROW DIKES
!            19 = START GRAZING
!            20 = STOP GRAZING
!            22 = AUTO MOW
!            23 = PLASTIC COVER
!            24 = REMOVE PLASTIC COVER    
!            25 = STOP DRAINAGE SYSTEM FLOW
!            26 = RESUME DRAINAGE FLOW
! 24  HE   = HARVEST EFFICIENCY(0-1)
!          = PESTICIDE APPLICATION EFFICIENCY
! 25  ORHI = OVER RIDES SIMULATED HI IF 0.<ORHI<1.
! 26  FRCP = FRACTION OF SOIL COMPACTED(TIRE WIDTH/TILLAGE WIDTH)
! 27  FPOP = FRACTION PLANT POPULATION REDUCED BY OPERATION
! 28  TCEM = CARBON EMISSION(kg/ha)
      J2=-1
      DO WHILE(J2/=JJ)
          READ(KR(3),18,IOSTAT=NFL)J2,TILX,PCD,PRIC,XLP,HRY,HRL,PWR,WDT,&
          &SPD,RC1,RC2,XLB,FCM,RFV1,RFV2,X1,RTI,(XTP(L),L=1,13)
          IF(NFL/=0)THEN
              IF(IBAT==0)THEN
                  WRITE(*,*)'TILLAGE NO = ',JJ,' NOT IN TILL LIST FILE'
!                 if defined(DEBUG) .AND. defined(PAUSE_ENABLE)
!                   PAUSE
!                 endif
              ELSE
	              WRITE(KW(MSO),'(3A,I4,A)')' !!!!! ',TRIM(ASTN),&
	              &' TILLAGE NO = ',JJ,' NOT IN TILL LIST FILE'
              END IF
              STOP 8
          END IF
      END DO
	  IF(J==1)THEN
          WDTE=WDT
          SPDE=SPD
	      EFM(NDT)=X1
	      TIL(NDT)=TILX
          EMX(NDT)=XTP(1)
          RR(NDT)=XTP(2)
          TLD(NDT)=XTP(3)*.001
          RHT(NDT)=XTP(4)
          RIN(NDT)=XTP(5)
          DKH(NDT)=XTP(6)
          DKI(NDT)=XTP(7)
          IHC(NDT)=XTP(8)
	      HE(NDT)=XTP(9)
          ORHI(NDT)=XTP(10)
          FRCP(NDT)=XTP(11)
          FPOP(NDT)=XTP(12)
          TCEM(NDT)=XTP(13)
          IF(IHC(NDT)==NHC(19))ORHI(NDT)=.001*X1
	      JJ=JX(5)
	      IF(IHC(NDT)==NHC(17))THEN
              RHTT=RHT(NDT)
	          RGIN=RIN(NDT)
	      END IF
          IF(IHC(NDT)==NHC(22))HMO(NDT)=-TLD(NDT)
	      IF(IHC(NDT)==NHC(23))THEN
	          PALB=RR(NDT)
	          FCV=EFM(NDT)
	      END IF
	  END IF
      IF(HRY<1.E-10)THEN
          IF(PCD==PCAT(5))THEN
              COTL(NDT)=PRIC-XLP
              COOP(NDT)=PRIC-XLP
              ICUS(NDT)=1
          END IF
          REWIND KR(3)
          RETURN
      END IF
      YR=MIN(30.,HRL/HRY)
      IF(NBYR>1.AND.NSTP==0.AND.RTI>0.)THEN
          X1=(1.+RTI)**YR
          AMF=RTI*X1/(X1-1.+1.E-10)
      ELSE
          AMF=1./YR
      END IF
      SALV=RFV1*XLP*RFV2**YR
      CSTA=AMF*(PRIC-SALV)/HRY
      CSTI=RTI*SALV/HRY
      CSTO=CSTA+CSTI
      TAR=XLP*RC1*(.001*HRL)**RC2/HRL
      FULU(NDT)=FCM*PWR
      CSFU=FULU(NDT)*FULP
      DO I=1,4
          IF(PCD==PCAT(I))EXIT
      END DO
      K=0
      SELECT CASE(I)
          CASE(1)
              FLAB=1.1
          CASE(2)
              IF(NBT(NDT)==0)THEN
                  COTL(NDT)=CSTO+TAR
                  COOP(NDT)=TAR
                  K=1
              ELSE
                  FLAB=.08
              END IF
          CASE(3)
              FLAB=1.2
          CASE(4)
              COTL(NDT)=CSTO+TAR
              COOP(NDT)=TAR
              K=1 
      END SELECT
      IF(K==0)THEN
          CSTW=WAGE*FLAB
          TOCS=TAR+(1.+XLB)*CSFU+CSTW
          CAPM=10./(SPDE*WDTE*EFM(NDT))
          COOP(NDT)=TOCS*CAPM+COOP(NDT)
          TOCS=TOCS+CSTO
          COTL(NDT)=TOCS*CAPM+COTL(NDT)
	      FULU(NDT)=CAPM*FULU(NDT)
	  END IF
      NN=JX(5)
      REWIND KR(3)
      IF(NN==0)RETURN
    5 CONTINUE
      RETURN
   18 FORMAT(1X,I4,1X,A8,1X,A4,28F8.0)
   25 FORMAT(A8)
      END
