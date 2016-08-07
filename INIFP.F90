      SUBROUTINE INIFP(I3,II,JJ,JRT)
!     EPIC1102
!     THIS SUBPROGRAM ALLOWS INPUT TO OPERATION SCHEDULE FOR IRRIGATION,
!     FERTILIZER, OR PESTICIDE APPLICATION
      USE PARM
      I1=I3-6
      SELECT CASE(I1)
          CASE(1)
              PSTE(II,JJ)=OPV(1)
              PSTR(II,JJ)=OPV(2)
              CALL PSTTBL
              LPC(II,JJ)=KDP1(JX(7))
              KP=KP+1
          CASE(2)
              VIRR(II,JJ)=OPV(1)
              IF(OPV(4)>0.)EFI=OPV(4)
              KI=KI+1
          CASE(3)
              WFA(II,JJ)=OPV(1)
              CALL NFTBL(L)
              LFT(II,JJ)=KDF1(JX(7))
              KF=KF+1
          CASE(13)
              RSTK(II,JJ)=OPV(1)              
          CASE(21)
              TLMA(II,JJ)=OPV(1)                          
          CASE DEFAULT
              GO TO 169
      END SELECT
      TIR(II,JJ)=BIR
      CND(II,JJ)=CN2
      QIR(II,JJ)=EFI
	  JRT=1
      RETURN
  169 IF(OPV(2)<0.)THEN
          CN2=-OPV(2)
      ELSE
          IF(OPV(2)>0.)THEN
              LUN=OPV(2)
              CALL HSGCN
          END IF
      END IF
      CND(II,JJ)=CN2
      IF(ABS(OPV(3))>1.E-5)BIR=OPV(3)
      TIR(II,JJ)=BIR
      IF(OPV(4)>0.)EFI=OPV(4)
      QIR(II,JJ)=EFI
	  IF(OPV(8)>0.)CFMN=OPV(8)
	  CFRT(II,JJ)=CFMN
	  IF(OPV(9)>0.)HWC(II,JJ)=OPV(9)
	  JRT=0
      RETURN
      END