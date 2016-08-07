      SUBROUTINE NPCY
!     EPIC1102
!     THIS SUBPROGRAM IS THE MASTER NUTRIENT CYCLING SUBPROGRAM.
!     CALLS NPMIN, NYNIT, NLCH, NCNMI, AND NDNIT FOR EACH SOIL
!     LAYER.
      USE PARM
      DATA SDF/0./
      STDX=0.
      DO K=1,LC
          STDX=STDX+STD(K)
      END DO 
      SGMN=0.
      SDN=0.
	  SN2=0.
	  SN2O=0.
      SMP=0.
      SIP=0.
      QNO3=0.
      QNO2=0.
      QN2O=0.
      QCO2=0.
      QO2=0.
      QSK=0.
      VAP=0.
      VNO2=0.
      VN2O=0.
      VCO2=0.
      VO2=0.
      SSFN2O=0.
      SSFCO2=0.
      SSFO2=0.
      SSFK=0.
      SSST=0.
      TSFNO3=0.
      TSFNO2=0.
      TSFN2O=0.
      TSFCO2=0.
      TSFO2=0.
      TSFK=0.
      TSFS=0.
      SVOL=0.
      SNIT=0.
      TRSP=0.
	  XX=0.
	  AD1=0.
	  AD2=0.
	  VNO31=VNO3(LD1)
      DO J=1,NBSL
          ISL=LID(J)
          AD1=AD1+WNO3(ISL)+WNO2(ISL)+WN2OL(ISL)
          RSPC(ISL)=0.
          RNMN(ISL)=0.
          SSFNO3=0.
          SSFNO2=0.
          X1=ST(ISL)-S15(ISL)
	      IF(X1<0.)THEN
	          SUT(ISL)=.1*(ST(ISL)/S15(ISL))**2
	      ELSE
	          SUT(ISL)=MIN(1.,.1+.9*SQRT(X1/(FC(ISL)-S15(ISL))))
          END IF
          CALL NPMIN
          CALL NKMIN
          IF(ISL/=LD1)THEN
              L1=LID(J-1)
              CALL NLCH(L1)
          ELSE
              ZZ=PRMT(66)*(1.+.1*RFV)
              IF(STDX+STDO>.001)CALL NFALL(ZZ)
	          IF(LUN/=35.AND.RFV>0.)THEN
                  CALL NYNIT
                  CALL NYPA
                  SMM(54,MO)=SMM(54,MO)+YP
                  VAR(54)=YP
              END IF
          END IF
          TSFNO3=TSFNO3+SSFNO3
          TSFNO2=TSFNO2+SSFNO2
          TSFN2O=TSFN2O+SSFN2O(ISL)
          TSFCO2=TSFCO2+SSFCO2(ISL)
          TSFO2=TSFO2+SSFO2(ISL)
          TSFK=TSFK+SSFK
          TSFS=TSFS+SSST
          AD2=AD2+WNO3(ISL)+WNO2(ISL)+WN2OL(ISL)
          IF(ISL==IDR)THEN
              VAR(53)=SSFNO3+SSFNO2+SSFN2O(ISL)
              SMM(53,MO)=SMM(53,MO)+VAR(53)              
          END IF
          Z5=500.*(Z(ISL)+XX)
          IF(WNH3(ISL)>.01)CALL NITVOL(Z5)
          IF(STMP(ISL)>0.)THEN
              CDG(ISL)=STMP(ISL)/(STMP(ISL)+EXP(SCRP(14,1)-SCRP(14,2)*STMP(ISL)))
              X1=EXP(PRMT(52)*(BD(ISL)-BDP(ISL)))
              OX=1.-PRMT(53)*Z5/(Z5+EXP(SCRP(20,1)-SCRP(20,2)*Z5))
              CS=MIN(10.,SQRT(CDG(ISL)*SUT(ISL))*PRMT(20)*OX*X1)
	      ! CS=MIN(10.,SQRT(CDG(ISL)*SUT(ISL))*PRMT(20)*X1)
              CALL NRSPC(CS)
              CALL NPMN(CS)
              SMP=SMP+WMP
              IF(IDN<3)THEN
                  DZA=1000.*(Z(ISL)-XX)
                  IF(IDN==2)THEN
                     CALL NDNITAK
                  ELSE
                     WDN=0.
                     DN2=0.
                     DN2O=0.
                     CALL NDNIT
                     SN2=SN2+DN2
                  END IF
                  SDN=SDN+WDN
                  SN2O=SN2O+DN2O
               END IF
          END IF
          XX=Z(ISL)
      END DO
      DF=AD1+VNO31-AD2-QNO3-QNO2-QN2O-VNO3(ISL)-VNO2(ISL)-VN2O(ISL)-TSFNO3&
      -TSFNO2-TSFN2O
      SDF=SDF+DF
!      IF(ABS(DF/AD1)>.001)WRITE(KW(1),1)IY,MO,KDA,AD1,VNO31,AD2,QNO3,&
!      QNO2,QN2O,VNO3(ISL),VNO2(ISL),VN2O(ISL),TSFNO3,TSFNO2,TSFN2O,DF,SDF
      SMM(57,MO)=SMM(57,MO)+VAP
      VAR(57)=VAP
      SMM(81,MO)=SMM(81,MO)+VSK
      VAR(81)=VSK
      SMM(82,MO)=SMM(82,MO)+VSLT
      VAR(82)=VSLT
	  RETURN
    1 FORMAT(1X,'#####',3I4,20E16.6)	  
      END
