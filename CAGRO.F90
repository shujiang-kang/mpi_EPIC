      SUBROUTINE CAGRO
!     EPIC1102
!     THIS SUBPROGRAM CALCULATES THE DAILY INCREASE IN PLANT BIOMASS,
!     ROOT WEIGHT, AND YIELD BY ADJUSTING THE POTENTIAL VALUES WITH THE
!     ACTIVE STRESS CONSTRAINT.
      USE PARM
      DIMENSION XTP(15)
      XX=REG(JJK)*SHRL
      RWL=RW(JJK)
      RGD=DDM(JJK)*XX
      DRWX=0.
      X1=100.*HUI(JJK)
      AJHI(JJK)=HI(JJK)*X1/(X1+EXP(SCRP(3,1)-SCRP(3,2)*X1))
      XX=100.*SWH(JJK)/(SWP(JJK)+1.E-5)
      F=XX/(XX+EXP(SCRP(10,1)-SCRP(10,2)*XX))
	  XX=MAX(AJHI(JJK)-WSYF(JJK),0.)
	  HIX=F*XX+WSYF(JJK)
	  YLDX=HIX*STL(JJK)
      YX=DM(JJK)-DDM(JJK)
      XX=MAX(1.E-5,YX+RGD)
      X2=RGD*420.
      SM99=SM99+X2
      VAR(99)=VAR(99)+X2
      SMM(99,MO)=SMM(99,MO)+X2
      RF=MAX(.2,RWPC(1,JJK)*(1.-HUI(JJK))+RWPC(2,JJK)*HUI(JJK))
      RF=MIN(RF,.99)
      RW(JJK)=RF*XX
      DRW=RW(JJK)-RWL
      DM(JJK)=XX
      DM1(JJK)=DM1(JJK)+RGD
      STL(JJK)=DM(JJK)-RW(JJK)
      XX=0.
      SUM=0.
      X2=2.*RZ
      DO I=1,LRD
          ISL=LID(I)
          XTP(ISL)=(Z(ISL)-XX)*EXP(-PRMT(56)*Z(ISL)/RZ)
          SUM=SUM+XTP(ISL)
          XX=Z(ISL)
      END DO
      IF(IDC(JJK)==NDC(7).OR.IDC(JJK)==NDC(8).OR.IDC(JJK)==NDC(10))THEN
          X1=MO*MO
          FALF=STL(JJK)*X1*XMTU(JJK)
	      SMM(88,MO)=SMM(88,MO)+FALF
          DM(JJK)=DM(JJK)-FALF
          STL(JJK)=STL(JJK)-FALF
          X1=FALF*1000.
          XZ=CNY(JJK)*X1
          XY=CPY(JJK)*X1
          W1=CKY(JJK)*X1
          CALL NCNSTD(FALF,XZ,0)
          FOP(LD1)=FOP(LD1)+XY
          UN1(JJK)=MAX(1.E-5,UN1(JJK)-XZ)
          UP1(JJK)=UP1(JJK)-XY
          UK1(JJK)=UK1(JJK)-W1
      END IF
      IF(IDC(JJK)==NDC(3).OR.IDC(JJK)==NDC(6))THEN
          ZZ=.01*(HUI(JJK)+.01)**10*STL(JJK)
          STL(JJK)=STL(JJK)-ZZ
          DM(JJK)=DM(JJK)-ZZ
          STD(JJK)=STD(JJK)+ZZ
          STDL=STDL+CLG*ZZ
          XY=ZZ*BN(3,JJK)
          XZ=ZZ*BP(3,JJK)
          XW=ZZ*BK(3,JJK)
          STDK=STDK+XW
          STDN(JJK)=STDN(JJK)+XY
          STDP=STDP+XZ
          UK1(JJK)=UK1(JJK)-XW
          UN1(JJK)=MAX(1.E-5,UN1(JJK)-XY)
          UP1(JJK)=UP1(JJK)-XZ
          IF(HUI(JJK)>.6.AND.STL(JJK)<.1)HU(JJK)=0.
      END IF
      ADD=0.
      DMD=0.
      DO J=1,LRD
          ISL=LID(J)
          WNO3(ISL)=MAX(1.E-5,WNO3(ISL)-UN(ISL))
          IF(DRW<0.)THEN
              UTO=RWT(ISL,JJK)/(RWL+1.E-10)
              DMD=-DRW*UTO+DMD
              SPL=RWT(ISL,JJK)
              IF(DMD>=SPL)THEN
                  X1=SPL
                  DMD=DMD-SPL
              ELSE
                  X1=DMD
                  DMD=0.                      
              END IF
              X2=MIN(UN1(JJK),1000.*BN(3,JJK)*X1)
              CALL NCNSTD(X1,X2,1)
              UN1(JJK)=UN1(JJK)-X2
              DM(JJK)=DM(JJK)-X1
              X1=-X1
          ELSE
              UTO=PRMT(55)*U(ISL)/(SU+1.E-20)+(1.-PRMT(55))*XTP(ISL)/SUM
              X1=DRW*UTO
          END IF
          ST(ISL)=ST(ISL)-U(ISL)
          AP(ISL)=AP(ISL)-UP(ISL)
          SOLK(ISL)=SOLK(ISL)-UK(ISL)
          DRWX(ISL)=X1
          RWT(ISL,JJK)=RWT(ISL,JJK)+DRWX(ISL)
          ADD=ADD+RWT(ISL,JJK)
      END DO
      RW(JJK)=ADD
      IF(RW(JJK)<=RWX(JJK))RETURN
      RWX(JJK)=RW(JJK)
      M21=MO
      K21=KDA
      DO I=1,LRD
          ISL=LID(I)
          RWTX(ISL,JJK)=RWT(ISL,JJK)
      END DO
      RETURN
      END