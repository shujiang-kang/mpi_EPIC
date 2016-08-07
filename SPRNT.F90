      SUBROUTINE SPRNT(YTP)
!     EPIC1102
!     THIS SUBPROGRAM PREPARES SOIL DATA TABLE FOR OUTPUT, AND
!     CONVERTS WEIGHTS OF MATERIALS TO CONCENTRATION.
      USE PARM
	  DIMENSION YTP(16)
      XX=0.
      YTP(1)=0.
      YTP(2)=0.
      YTP(3)=0.
      YTP(4)=0.
	  YTP(5)=0.
      DO J=1,NBSL
          I=LID(J)
          WT1=WT(I)/1000.
          SOIL(1,I)=AP(I)/WT1
          SOIL(2,I)=PMN(I)/WT1
          SOIL(3,I)=OP(I)/WT1
          SOIL(4,I)=WP(I)/WT1
          SOIL(5,I)=WNO3(I)/WT1
          SOIL(14,I)=SOLK(I)/WT1
          SOIL(15,I)=EXCK(I)/WT1
          SOIL(16,I)=FIXK(I)/WT1
          SOIL(6,I)=WON(I)/WT1
          SOIL(7,I)=.1*WOC(I)/WT(I)
          DG=(Z(I)-XX)*1000.
          X1=ST(I)-S15(I)
          X2=FC(I)-S15(I)
          SOIL(17,I)=X1/X2
          SOIL(18,I)=X1
          SOIL(19,I)=X2
          ECND(I)=.15625*WSLT(I)/ST(I)
          SOIL(20,I)=S15(I)/DG
          YTP(1)=YTP(1)+FC(I)
          SOIL(9,I)=FC(I)/DG
          SOIL(8,I)=PO(I)/DG
          SOIL(13,I)=BDD(I)*BD(I)
          YTP(2)=YTP(2)+ST(I)
          SOIL(12,I)=ST(I)/DG
          YTP(3)=YTP(3)+S15(I)
          YTP(4)=YTP(4)+PO(I)
	      YTP(5)=YTP(5)+WT(I)
          XX=Z(I)
      END DO
      XX=Z(LID(NBSL))*1000.
      DO I=1,4
          YTP(I)=YTP(I)/XX
      END DO
      RETURN
      END