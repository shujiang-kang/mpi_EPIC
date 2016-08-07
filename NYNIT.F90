      SUBROUTINE NYNIT
!     EPIC1102
!     THIS SUBPROGRAM ESTIMATES DAILY NO3 LEACHING BY PERCOLATION AND
!     LATERAL SUBSURFACE FLOW AND NO3 LOSS IN RUNOFF FOR THE SURFACE
!     LAYER
      USE PARM
      WNO3(LD1)=WNO3(LD1)+VNO3(LD1)
      WSLT(LD1)=WSLT(LD1)+VSLT
      VNO3(LD1)=0.
      VSK=0.
      VSLT=0.
      V=QD+SSF(LD1)+PKRZ(LD1)
      IF(V<=0.)RETURN
	  X1=WNO3(LD1)-.001*WT(LD1)*PRMT(27)
	  X2=1.-EXP(-V/(STFR(LD1)*PO(LD1)))
	  X4=PKRZ(LD1)+PRMT(14)*(QD+SSF(LD1))
	  IF(X1>0.)THEN
	      X3=X1*X2
          CO=MIN(.01*PRMT(63),X3/X4)
	      CS=PRMT(14)*CO
          QNO3=QD*CS
          SSFNO3=CS*SSF(LD1)
          VNO3(LD1)=CO*PKRZ(LD1)
          WNO3(LD1)=WNO3(LD1)-QNO3-SSFNO3-VNO3(LD1)
      END IF
      IF(WNO2(LD1)>0.)THEN
	      X3=X2*WNO2(LD1)
          CO=X3/X4
	      CS=PRMT(14)*CO
          QNO2=QD*CS
          SSFNO2=CS*SSF(LD1)
          VNO2(LD1)=CO*PKRZ(LD1)
          WNO2(LD1)=WNO2(LD1)-QNO2-SSFNO2-VNO2(LD1)
      END IF
      IF(WN2OL(LD1)>0.)THEN
	      X3=X2*WN2OL(LD1)
          CO=X3/X4
	      CS=PRMT(14)*CO
          QN2O=QD*CS
          SSFN2O(LD1)=CS*SSF(LD1)
          VN2O(LD1)=CO*PKRZ(LD1)
          XX=QN2O+SSFN2O(LD1)+VN2O(LD1)
          WN2OL(LD1)=WN2OL(LD1)-XX
          !WN2O(LD1)=WN2O(LD1)-XX
      END IF
      IF(WCO2L(LD1)>0.)THEN
	      X3=X2*WCO2L(LD1)
          CO=X3/X4
	      CS=PRMT(14)*CO
          QCO2=QD*CS
          SSFCO2(LD1)=CS*SSF(LD1)
          VCO2(LD1)=CO*PKRZ(LD1)
          WCO2L(LD1)=WCO2L(LD1)-QCO2-SSFCO2(LD1)-VCO2(LD1)
      END IF
      IF(WO2L(LD1)>0.)THEN
	      X3=X2*WO2L(LD1)
          CO=X3/X4
	      CS=PRMT(14)*CO
          QO2=QD*CS
          SSFO2(LD1)=CS*SSF(LD1)
          VO2(LD1)=CO*PKRZ(LD1)
          WO2L(LD1)=WO2L(LD1)-QO2-SSFO2(LD1)-VO2(LD1)
      END IF
      IF(SOLK(LD1)>0.)THEN
          X3=SOLK(LD1)*X2
          SOLK(LD1)=SOLK(LD1)-X3
          COK=X3/X4
          CSK=PRMT(14)*COK
          VSK=COK*PKRZ(LD1)
          SSFK=CSK*SSF(LD1)
          QSK=CSK*QD
          SMM(79,MO)=SMM(79,MO)+QSK
          VAR(79)=QSK
      END IF
      IF(WSLT(LD1)>0.)THEN
          X5=(WSLT(LD1)+1.E-5)*X2
          COSL=X5/X4
          CSSL=PRMT(14)*COSL
          SSST=CSSL*SSF(LD1)
          VSLT=COSL*PKRZ(LD1)
          XX=X5-VSLT-SSST
          SMM(70,MO)=SMM(70,MO)+XX
          VAR(70)=XX
          WSLT(LD1)=WSLT(LD1)-X5
      END IF
      RETURN
      END