      SUBROUTINE HVOLQ
!     EPIC1102
!     THIS SUBPROGRAM PREDICTS DAILY RUNOFF VOLUME AND PEAK RUNOFF RATE
!     GIVEN DAILY PRECIPITATION AND SNOW MELT.
      USE PARM
      SUM=0.
      ADD=0.
      IF(LUN==35)THEN
          SCN=25400./CN0-254.
          CN=CN0
          GO TO 20
      END IF
      IF(NVCN>0)GO TO 25
      XX=0.
      DO JJ=1,NBSL
          J=LID(JJ)
          IF(Z(J)>1.)GO TO 3
          ZZ=(Z(J)-XX)/Z(J)
          SUM=SUM+(ST(J)-S15(J))*ZZ/(FC(J)-S15(J))
          ADD=ADD+ZZ
          XX=Z(J)
      END DO
      GO TO 11
    3 ZZ=1.-XX
      SUM=SUM+(ST(J)-S15(J))*ZZ/(FC(J)-S15(J))
      ADD=ADD+ZZ
      GO TO 11
   25 IF(NVCN>1)GO TO 19
      DO JJ=1,NBSL
          L=LID(JJ)
          IF(Z(L)>1.)GO TO 26
          SUM=SUM+ST(L)-S15(L)
          ADD=ADD+FC(L)-S15(L)
          L1=L
      END DO
      GO TO 11
   26 RTO=(1.-Z(L1))/(Z(L)-Z(L1))
      SUM=SUM+(ST(L)-S15(L))*RTO
      ADD=ADD+(FC(L)-S15(L))*RTO
   11 SUM=SUM/ADD
      IF(SUM>0.)GO TO 13
   31 SCN=SMX*(1.-SUM)**2
      GO TO 14
   13 SUM=SUM*100.
      SCN=SMX*(1.-SUM/(SUM+EXP(SCRP(4,1)-SCRP(4,2)*SUM)))
   14 SCN=(SCN-SMX)*EXP(PRMT(75)*(1.-RSD(LD1)))+SMX  
      IF(STMP(LID(2))<-1.)SCN=SCN*PRMT(65)
      IF(ICV>0)SCN=SCN*SQRT(1.-FCV) 
      CN=25400./(SCN+254.)
      IF(ISCN==0)THEN
          UPLM=MIN(99.5,CN+5.)
          BLM=MAX(1.,CN-5.)
          CN=ATRI(BLM,CN,UPLM,8)
      END IF
      SCN=25400./CN-254.
      IF(SCN>3.)GO TO 20
      SCN=3. 
	  CN=25400./(SCN+254.)
      GO TO 20
   19 IF(NVCN>2)GO TO 28
      DO JJ=1,NBSL
          ISL=LID(JJ)
          SUM=SUM+ST(ISL)-S15(ISL)
          ADD=ADD+FC(ISL)-S15(ISL)
          IF(Z(ISL)>1.)EXIT
      END DO
      SUM=SUM/ADD
      IF(SUM<0.)GO TO 31
      RTO=MIN(.98,SUM)
      SCN=SMX*(1.-RTO)
      GO TO 14
   28 IF(NVCN>3)GO TO 33
   33 SCN=MAX(3.,SCI)
      GO TO 14
   20 BB=.2*SCN
      TOT=100.
      DO I=1,9
          TOT=TOT-5.
          IF(CN>TOT)EXIT
      END DO
      CNDS(I)=CNDS(I)+1.
      RTO=MIN(1.,SCN/SMX)
      CRKF=PRMT(17)*RFV*RTO
      RFV=RFV-CRKF
      IF(RWO<1.E-5)GO TO 10
      SELECT CASE(INFL)
          CASE(1)
              X1=RWO-BB
              IF(X1<=0.)GO TO 10
              QD=X1*X1/(RWO+.8*SCN)
          CASE(2,3)
              CALL HREXP
          CASE(4)
              CALL HRUNF
      END SELECT
      IF(IFD>0)THEN
          IF(DHT>.01.AND.RGIN>.01)THEN
              CALL HFURD
	          X1=MAX(0.,ST(LD1)-PO(LD1))
!              IF(NOP>0)WRITE(KW(1),17)IYR,MO,KDA,DHT,RHTT,QD,DV,X1,XHSM
              IF(QD<=DV-X1)THEN
                  QD=0.
                  GO TO 10
              END IF
          END IF
          DHT=0.
          IF(IDRL==0.AND.CHT(JJK)<1.)DHT=DKHL
      END IF
      IF(ITYP==0)THEN
          X2=QD/DUR
          IF(X2>1.)THEN
              X2=X2**.25
          ELSE
              X2=1.
          END IF   
          X4=MIN(UPSL/360.,TCS/X2)
          TC=X4+TCC/X2
          ALTC=1.-EXP(-TC*PR)
	      X1=ALTC*QD
          QP=X1/TC
	      QPR=X1/X4
          GO TO 10
      END IF
      TC=TCC+TCS/SQRT(RWO)
      QP=BB/RWO
      CALL HTR55
   10 IF(KFL(4)>0)WRITE(KW(4),27)IYR,MO,KDA,CN,RWO,QD,TC,QP,DUR,ALTC,AL5
      RETURN
   27 FORMAT(1X,3I4,9F10.2)
   17 FORMAT(1X,3I4,12X,'DHT=',F5.0,'MM',2X,'RHTT=',F5.0,'MM',2X,'Q='&
     &,F5.1,'MM',2X,'DV=',F5.1,'MM',2X,'ST=',F5.1,'MM',2X,'HUSC=',F4.2)
      END