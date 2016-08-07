      SUBROUTINE NCQYL
!     EPIC1102
!     THIS SUBPROGRAM PREDICTS DAILY C LOSS, GIVEN SOIL LOSS AND
!     ENRICHMENT RATIO.
      USE PARM
      Y1=WBMC(LD1)
      Y4=PKRZ(LD1)
      QBC=0.
      VBC=0.
      YBC=0.
      YOC=0.
      TOT=WHPC(LD1)+WHSC(LD1)+WLMC(LD1)+WLSC(LD1)
      X1=1.-YEW
      YOC=YEW*TOT
      WHSC(LD1)=WHSC(LD1)*X1
      WHPC(LD1)=WHPC(LD1)*X1
      WLS(LD1)=WLS(LD1)*X1
      WLM(LD1)=WLM(LD1)*X1
      WLSL(LD1)=WLSL(LD1)*X1
      WLSC(LD1)=WLSC(LD1)*X1
      WLMC(LD1)=WLMC(LD1)*X1
      WLSLC(LD1)=WLSLC(LD1)*X1
      WLSLNC(LD1)=WLSC(LD1)-WLSLC(LD1)
      IF(Y1>0.)THEN
      DK=.0001*PRMT(21)*WOC(LD1)
      X1=PO(LD1)-S15(LD1)
      XX=X1+DK
      V=QD+Y4
      X3=0.
      IF(V>0.)THEN
         X3=Y1*(1.-EXP(-V/XX))
         CO=X3/(Y4+PRMT(44)*QD)
         CS=PRMT(44)*CO
         VBC=CO*Y4
         Y1=Y1-X3
         QBC=CS*QD
          END IF    
          ! COMPUTE WBMC LOSS WITH SEDIMENT
         IF(YEW>0.)THEN
            CS=DK*Y1/XX
            YBC=YEW*CS
              END IF    
          END IF    
      WBMC(LD1)=Y1-YBC      
      XX=0.
      DO L=2,NBSL
          ISL=LID(L)
          Y1=WBMC(ISL)+VBC
          VBC=0.
          IF(Y1>=.01)THEN
              V=PKRZ(ISL)
              IF(V>0.)THEN
                  IF(ASHZ(ISL)=='       B'.OR.ASHZ(ISL)=='       C')THEN
                      DO I=1,3
                          IF(SOLO==SLOD(I))EXIT
                      END DO  
                      IF(I<=3)THEN
                          SELECT CASE(I)
                              CASE(1)
                                  B1=2.662
                                  QMAX=10**(B1+.572*LOG10(CLA(ISL))-.0602*PH(ISL))
                              CASE(2)
                                  B1=-206.452
                                  X2=1000.*WOC(ISL)/WT(ISL)
                                  QMAX=B1+.127*X2-46.88*CLA(ISL)
                              CASE(3)
                                  B1=2.141
                                  X1=10.*FE26(ISL)
                                  QMAX=10**(B1+.403*LOG10(CLA(ISL))+.439*LOG10(X1))
                          END SELECT
                          XK=10**(-.386-.184*PH(ISL))
                          VL=ST(ISL)
                          X2=.1*WBMC(ISL)/(Z(ISL)-XX)
                          XX=Z(ISL)
                          ! C=X2+B1     !ORIGINAL EQUATION
                          C=X2*VL+B1  !CHANGED BY SL, WBM, & RCI - 2Sept11
                          B=XK*(C-QMAX)-VL
                          A=-VL*XK
                          XF=(-B-SQRT(B*B-4.*A*C))/(2.*A)
                          !RE=XK*QMAX*XF/(1.+XK*XF)-B1
                          !SBC=.01*RE*V
                          VBC=.01*XF*V
                          !Y1=Y1-SBC
                          !RTO=WHSC(ISL)/(WHSC(ISL)+WHPC(ISL))
                          !X1=RTO*SBC
                          !X2=SBC-X1
                          !WHSC(ISL)=WHSC(ISL)+X1
                          !WHPC(ISL)=WHPC(ISL)+X2
                      ELSE                                              
                          VBC=Y1*(1.-EXP(-V/(PO(ISL)-S15(ISL)+.0001*PRMT(21)*WOC(ISL))))
                      END IF    
                  ELSE                              
                      VBC=Y1*(1.-EXP(-V/(PO(ISL)-S15(ISL)+.0001*PRMT(21)*WOC(ISL))))
                  END IF    
              END IF
          END IF
          WBMC(ISL)=Y1-VBC
      END DO
      SMM(75,MO)=SMM(75,MO)+VBC
      VAR(75)=VBC
      SMM(76,MO)=SMM(76,MO)+QBC
      VAR(76)=QBC
      YOC=YOC+YBC
      SMM(77,MO)=SMM(77,MO)+YOC
      VAR(77)=YOC
      RETURN
      END