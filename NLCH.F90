      SUBROUTINE NLCH(L1)
!     EPIC1102
!     THIS SUBPROGRAM ESTIMATES DAILY NO3 LEACHING BY PERCOLATION AND
!     LATERAL SUBSURFACE FLOW FOR ALL LAYERS EXCEPT THE SURFACE LAYER.
      USE PARM
      AP(ISL)=AP(ISL)+VAP
      WNO3(ISL)=WNO3(ISL)+VNO3(L1)
      WNO2(ISL)=WNO2(ISL)+VNO2(L1)
      WN2OL(ISL)=WN2OL(ISL)+VN2O(L1)
      WCO2L(ISL)=WCO2L(ISL)+VCO2(L1)
      WO2L(ISL)=WO2L(ISL)+VO2(L1)
      SOLK(ISL)=SOLK(ISL)+VSK
      WSLT(ISL)=WSLT(ISL)+VSLT
      VAP=0.
      VSK=0.
      VSLT=0.
      VNO3(ISL)=0.
      VNO2(ISL)=0.
      VN2O(ISL)=0.
      VCO2(ISL)=0.
      VO2(ISL)=0.
      V=PKRZ(ISL)+SSF(ISL)+1.E-10
      IF(V<1.E-5)RETURN
      X2=1.-EXP(-V/(STFR(ISL)*PO(ISL)))
      X1=WNO3(ISL)-.001*WT(ISL)*PRMT(27)
      IF(X1>0.)THEN
          X3=X1*X2
          VV=X3/V
          WNO3(ISL)=WNO3(ISL)-X3
          VNO3(ISL)=VV*PKRZ(ISL)
          SSFNO3=VV*SSF(ISL)
      END IF
      IF(WNO2(ISL)>0.)THEN
          VNX=X2*WNO2(ISL)
          VV=VNX/V
          WNO2(ISL)=WNO2(ISL)-VNX
          VNO2(ISL)=VV*PKRZ(ISL)
          SSFNO2=VV*SSF(ISL)
      END IF
      IF(WN2OL(ISL)>0.)THEN
          VNX=X2*WN2OL(ISL)
          VV=VNX/V
          WN2OL(ISL)=WN2OL(ISL)-VNX
          !WN2O(ISL)=WN2O(ISL)-VNX
          VN2O(ISL)=VV*PKRZ(ISL)
          SSFN2O(ISL)=VV*SSF(ISL)
      END IF
      IF(WCO2L(ISL)>0.)THEN
          VNX=X2*WCO2L(ISL)
          VV=VNX/V
          WCO2L(ISL)=WCO2L(ISL)-VNX
          VCO2(ISL)=VV*PKRZ(ISL)
          SSFCO2(ISL)=VV*SSF(ISL)
      END IF
      IF(WO2L(ISL)>0.)THEN
          VNX=X2*WO2L(ISL)
          VV=VNX/V
          WO2L(ISL)=WO2L(ISL)-VNX
          VO2(ISL)=VV*PKRZ(ISL)
          SSFO2(ISL)=VV*SSF(ISL)
      END IF
      IF(SOLK(ISL)>0.)THEN
          X3=SOLK(ISL)*X2
          SOLK(ISL)=SOLK(ISL)-X3
          VSK=X3*PKRZ(ISL)/V
          SSFK=X3-VSK
      END IF
      IF(WSLT(ISL)>1.E-5)THEN
          X3=WSLT(ISL)*X2
          WSLT(ISL)=WSLT(ISL)-X3
          VSLT=X3*PKRZ(ISL)/V
          SSST=X3-VSLT
      END IF
      IF(AP(ISL)<1.E-5)RETURN
      XX=MIN(.75,PKRZ(ISL)/WT(ISL))
      VAP=XX*AP(ISL)
      AP(ISL)=AP(ISL)-VAP
      RETURN
      END