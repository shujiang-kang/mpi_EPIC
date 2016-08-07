      SUBROUTINE HIRG(AIR,EFD,ZX,JRT,IRX,IRY)
!     EPIC1102
!     THIS SUBPROGRAM IS USED TO SIMULATE AUTOMATIC OR USER SPECIFIED
!     IRRIGATION APPLICATIONS.  COMPUTES THE AMOUNT OF IRRIGATION WATER
!     NEEDED TO BRING THE ROOT ZONE WATER CONTENT TO FIELD CAPACITY, FOR
!     AUTOMATIC OPTION.  USER SPECIFIED AMOUNT IS APPLIED FOR
!     MANUAL OPTION.  EROSION AND RUNOFF ARE ESTIMATED.
      USE PARM
      IF(VIRT>=VIMX.OR.NII<IRI)THEN
	      AIR=0.
          JRT=1
          RETURN
      END IF
      X3=COIR*AIR
      N1=MAX(1,NCP(JJK))
	  I=1
      X4=AIR      
      AIR=AIR*EFD
      YX=0.
      IF(IRY==1)THEN
          IF(IAC>0)GO TO 5
          X1=AIR
          XX=(PAW-RZSW)/(1.-EFI)
      ELSE
          X1=10000.
          IF(IAC>0)THEN
              XX=10000.
          ELSE
              XX=(PAW-RZSW)/(1.-EFI)
          END IF
      END IF
      X4=MIN(X1,VIMX-VIRT,XX,ARMX)
      AIR=X4*EFD
      X3=COIR*X4
      IF(AIR>ARMN)GO TO 5
      AIR=0.
      GO TO 10
    5 NII=0
      QXM=AIR*EFI
      IF(IRR/=5)THEN
          IF(QXM<1.E-5)GO TO 7
          QPX=QXM/24.
          IF(IRR/=1)THEN
              CVF=1.
              IF(RHTT<1.E-10.OR.RGIN<1.E-5)GO TO 3
              X1=1000.*RGIN/RHTT
              QPX=2.778E-6*QPX*RGIN*WSA/FW
              DX=(2.*QPX/(SX*X1*(1./(4.+16./(X1*X1)))**.3333))**.375
              X2=DX*X1
              PX=2.*SQRT(DX*DX+.25*X2*X2)
              AX=.5*DX*X2
              VX=(AX/PX)**.6667*SX
              CY=PRMT(36)*VX**PRMT(31)
              YX=10.*QXM*CY*EK
              GO TO 1
          END IF
          CALL EYCC
        3 YX=2.5*CVF*USL*SQRT(QPX*QXM)
          GO TO 1
      END IF
      DO J=1,NBSL
          I=LID(J)
          IF(ZX<Z(I))EXIT
      END DO
      ST(I)=ST(I)+AIR
    1 QD=QD+QXM
      YERO=YERO+YX
      YSD(NDRV)=YSD(NDRV)+YX
    7 X1=RZSW-PAW
      VSLT=.01*AIR*CSLT
      SMM(69,MO)=SMM(69,MO)+VSLT
      VAR(69)=VSLT
	  IF(KG(JJK)>0.OR.JPL(JJK)>0)XHSM=HU(JJK)/PHU(JJK,IHU(JJK))
!      IF(NOP>0)WRITE(KW(1),9)IYR,MO,KDA,AIR,WS,WTN,X1,BIR,QXM,YX,XHSM,X3
      VIRT=VIRT+X4
      COST=COST+X3
      IF(VIRT>0.)THEN
          IF(IRY==0)THEN
              X1=COTL(IRX)
              X2=X1-COOP(IRX)
              COST=COST+X1
              CSFX=CSFX+X2
          END IF
          IF(KFL(20)>0)THEN
              WRITE(KW(20),14)IYR,MO,KDA,TIL(IRX),KDC(JJK),IHC(IRX),NBE(IRX),X3,&
              &X3,AIR
              IF(IRY==0)WRITE(KW(20),50)IYR,MO,KDA,TIL(IRX),KDC(JJK),IHC(IRX),&
              &NBE(IRX),NBT(IRX),X1,X2,FULU(IRX)
          END IF
      END IF
      SMM(19,MO)=SMM(19,MO)+X4
      VAR(19)=X4
	  SMM(92,MO)=SMM(92,MO)+FULU(IRX)
      XEF=X4-AIR
      SMM(84,MO)=SMM(84,MO)+XEF
      VAR(84)=XEF
      AIR=AIR-QXM
	  VIR(N1,JJK)=VIR(N1,JJK)+X4
      VIL(N1,JJK)=VIL(N1,JJK)+XEF
	  AFN=AIR*CNO3I
      ANA(JJK)=ANA(JJK)+AFN
      WNO3(I)=WNO3(I)+AFN
      SMM(60,MO)=SMM(60,MO)+AFN
      VAR(60)=VAR(60)+AFN
!      IF(NOP>0.AND.AFN>0.)WRITE(KW(1),133)IYR,MO,KDA,AFN,AIR,XHSM   !skang
   10 JRT=0
      IF(IRR==5)THEN
	      AWC=AWC+AIR
	      AIR=0.
	  END IF
      RETURN
    9 FORMAT(1X,3I4,2X,'IRRIGATE',10X,'VOL=',F5.0,'MM',2X,'WS=',F4.2,&
     &2X,'WTN=',F8.0,'kpa',2X,'PWDF=',F6.0,'MM',2X,'TRGR=',F7.2,&
     &2X,'Q=',F5.0,'MM',2X,'Y=',F4.1,'t/ha',2X,'HUSC=',F4.2/2X,'COST=',&
     &F7.2,'$/ha')
   14 FORMAT(1X,3I4,2X,A8,8X,I6,6X,2I4,4X,F10.2,10X,2F10.2)
   50 FORMAT(1X,3I4,2X,A8,8X,I6,6X,3I4,2F10.2,20X,F10.2)
  133 FORMAT(1X,3I4,2X,'NO3 FERT = ',F5.0,'kg/ha',2X,'IRR VOL = ',&
     &F5.0,'MM',2X,'HUSC = ',F4.2)
      END