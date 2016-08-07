      SUBROUTINE SPOFC(I)
!     EPIC1102
      USE PARM
      X1=.9*PO(I)
      IF(FC(I)<X1)RETURN
      X2=FC(I)-S15(I)
      FC(I)=X1
      S15(I)=FC(I)-X2
      IF(S15(I)<=0.)S15(I)=.01*FC(I)
      RETURN
      END