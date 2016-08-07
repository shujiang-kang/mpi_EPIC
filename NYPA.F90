      SUBROUTINE NYPA
!     EPIC1102
!     THIS SUBPROGRAM PREDICTS DAILY P LOSS, GIVEN SOIL LOSS AND
!     ENRICHMENT RATIO.
      USE PARM
      YP=YEW*WP(LD1)
      WP(LD1)=WP(LD1)-YP
      X2=AP(LD1)
      YAP=YEW*X2
      X2=X2-YAP
      X1=EXCK(LD1)*YEW
      EXCK(LD1)=EXCK(LD1)-X1
      X3=FIXK(LD1)*YEW
      FIXK(LD1)=FIXK(LD1)-X3
      VAR(78)=X1+X3
      SMM(78,MO)=SMM(78,MO)+X1+X3
      V=QD+PKRZ(LD1)
      X1=MAX(5.*V,WT(LD1)*PRMT(8))
      IF(QD>0.)THEN
          IF(LBP>0)THEN
              RTO=(10.*WP(LD1)/WT(LD1))**PRMT(34)
              QAP=MIN(.5*X2,X2*QD*RTO/X1)
          ELSE
              QAP=X2*QD/X1
          END IF
          X2=X2-QAP
      END IF
      VAP=MIN(.5*X2,X2*PKRZ(LD1)/X1)
      AP(LD1)=X2-VAP
      YMP=PMN(LD1)*YEW
      PMN(LD1)=PMN(LD1)-YMP
      YP=YP+YMP+YAP
      RETURN
      END