      SUBROUTINE EWEMHKS(JRT)
!     EPIC1102
!     THIS SUBPROGRAM ESTIMATES DAILY SOIL LOSS CAUSED BY WIND EROSION,
!     GIVEN THE AVERAGE WIND SPEED AND DIRECTION.
      USE PARM
      JRT=0
      IF(U10>PRMT(67))GO TO 5
    6 JRT=1
	  RETURN
    5 WD=193.*EXP(1.103*(U10-30.)/(U10+1.))
      BT=PI2/4.+TH-ANG
      ALG=FL*FW/(FL*ABS(COS(BT))+FW*ABS(SIN(BT)))
      IF(RGIN>0.)THEN
          X1=1.+RHTT
          RK=.004*X1*X1/RGIN
          IF(RK<2.27)THEN
              RF=1.
          ELSE
              IF(RK<89.)THEN
                  RF=1.125-.153*LOG(RK)
              ELSE
                  RF=.336*EXP(.00324*RK)
              END IF
          END IF
      ELSE
          RF=1.          
      END IF
      VAC=1000.*(VAC+BWD(3,JD)*RSD(LD1))
      IF(VAC>4000.)GO TO 6
      VF=.2533*VAC**1.363
      BV=1.+VF*(8.9303E-5+VF*(8.5074E-9-VF*1.5888E-13))
      AV=EXP(VF*(-7.5935E-4-VF*(4.7416E-8-VF*2.9476E-13)))
      E2=695.*WK*RF
	  XL=1.5579E6*E2**(-1.2571)*EXP(-.001558*E2)
      AA=ALG*1000./XL
      F=(AA/(AA+EXP(-3.2388-1.6241*AA)))
	  XX=F**.3484+WCF-1.
	  IF(XX<=0.)GO TO 6
      E4=(XX*E2**.3484)**2.8702
      E5=AV*E4**BV
      YW=E5*WD/WB
      RETURN
      END