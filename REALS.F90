      SUBROUTINE REALS
!     EPIC1102
!     THIS SUBPROGRAM ALLOWS REAL TIME UPDATES OF SOIL VARIABLES
      USE PARM
      DIMENSION XRTS(7,15)
      DATA IYCD/1/
!      WRITE(KW(1),120)(ST(LID(L)),L=1,NBSL)
!      WRITE(KW(1),120)(STMP(LID(L)),L=1,NBSL)
!      WRITE(KW(1),120)(WNO3(LID(L)),L=1,NBSL)
!      WRITE(KW(1),120)(WNH3(LID(L)),L=1,NBSL)
!      WRITE(KW(1),120)(AP(LID(L)),L=1,NBSL)
!      WRITE(KW(1),120)(EXCK(LID(L)),L=1,NBSL)
!      WRITE(KW(1),120)(WSLT(LID(L)),L=1,NBSL)
      IF(ISTP==2)GO TO 2
      IF(ISTP==0)GO TO 198
      READ(KW(MSO+3),9)(IYS(I),I=2,8)
      DO J=1,NBSL
          L=LID(J)
          READ(KW(MSO+3),6)(XRTS(I,L),I=1,7)
      END DO
      REWIND KW(MSO+3)
      ISTP=0
      WRITE(KW(MSO+3),9)ISTP
      ISTP=2
    2 DO J=1,NBSL
          L=LID(J)
          IF(IYS(2)==1)ST(L)=XRTS(1,L)*(FC(L)-S15(L))+S15(L)
          IF(IYS(3)==1)STMP(L)=XRTS(2,L)
          IF(IYS(4)==1)WNO3(L)=XRTS(3,L)
          IF(IYS(5)==1)WNH3(L)=XRTS(4,L)
          IF(IYS(6)==1)AP(L)=XRTS(5,L)
          IF(IYS(7)==1)EXCK(L)=XRTS(6,L)
          IF(IYS(8)==1)WSLT(L)=XRTS(7,L)
      END DO
      GO TO 196
  198 REWIND KW(MSO+3)
      IF(ISX/=0)THEN
          ISTP=1
          WRITE(KW(MSO+3),9)ISTP
      END IF
      ISX=1
      IF(IYCD/=0)THEN
          DO I=2,8
              IYS(I)=1
          END DO
      END IF
      WRITE(KW(MSO+3),9)(IYS(I),I=2,8)
      RZSW=0.
      PAW=0.
	  Z1=0.
      DO J=1,NBSL
          L=LID(J)
          X1=ST(L)-S15(L)
	      X4=.001*X1/(Z(L)-Z1)
          X2=FC(L)-S15(L)
          X3=X1/X2
          RZSW=RZSW+X1
          PAW=PAW+X2
          WRITE(KW(MSO+3),11)Z(L),X1,X4,X2,X3,STMP(L),WNO3(L),WNH3(L),&
          &AP(L),EXCK(L),WSLT(L)
      END DO
      WRITE(KW(MSO+3),11)RZ,RZSW,PAW
!  196   WRITE(KW(1),120)(ST(LID(L)),L=1,NBSL)
!      WRITE(KW(1),120)(STMP(LID(L)),L=1,NBSL)
!      WRITE(KW(1),120)(WNO3(LID(L)),L=1,NBSL)
!      WRITE(KW(1),120)(WNH3(LID(L)),L=1,NBSL)
!      WRITE(KW(1),120)(AP(LID(L)),L=1,NBSL)
!      WRITE(KW(1),120)(EXCK(LID(L)),L=1,NBSL)
!      WRITE(KW(1),120)(WSLT(LID(L)),L=1,NBSL)
196    RETURN     !move 196 here by Skang
    6 FORMAT(40X,F10.3,F10.2,5F10.0)
    9 FORMAT(20I4)
   11 FORMAT(F10.2,F10.1,F10.3,F10.1,2F10.2,5F10.0)
  120 FORMAT(1X,10F10.2)
      END
