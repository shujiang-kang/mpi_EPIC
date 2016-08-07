      SUBROUTINE REALC
!     EPIC1102
!     THIS SUBPROGRAM ALLOWS REAL TIME UPDATES OF CROP VARIABLES
      USE PARM
      DIMENSION XRTC(8,12)
	  FNPP(X)=DMLA(JJK)*X/(X+EXP(PPCF(1,JJK)-PPCF(2,JJK)*X))
!      WRITE(KW(1),170)IY,MO,KDA
      CALL PESTF
!      WRITE(KW(1),174)CPNM(JJK),WS,SN,SP,SK,VAR(72),SSLT,PSTF(JJK)
!      WRITE(KW(1),191)DM(JJK),SLAI(JJK),PPL0(JJK),UN1(JJK),UP1(JJK),UK1&
!     &(JJK),PRYG(JJK),PRYF(JJK)    !skang
      IF(ISTP==2)THEN
          IF(IYS(1)/=0)THEN
              DM(JJK)=XRTC(1,JJK)
              SLAI(JJK)=XRTC(2,JJK)
	          PPL0(JJK)=XRTC(3,JJK)
	          XLAI(JJK)=FNPP(PPL0(JJK))
              UN1(JJK)=XRTC(4,JJK)
              UP1(JJK)=XRTC(5,JJK)
              UK1(JJK)=XRTC(6,JJK)
              PRYG(JJK)=XRTC(7,JJK)
              PRYF(JJK)=XRTC(8,JJK)
          END IF
!          WRITE(KW(1),191)DM(JJK),SLAI(JJK),PPL0(JJK),UN1(JJK),UP1(JJK),&
!          &UK1(JJK),PRYG(JJK),PRYF(JJK)      
          RETURN
      END IF
      IF(ISTP==0)THEN
          CALL OPENV(KW(MSO+2),'RTCROP.DAT',KW(MSO),.TRUE.)
          WRITE(KW(MSO+2),3)IYS(1)
          WRITE(KW(MSO+2),2)DM(JJK),SLAI(JJK),PPL0(JJK),UN1(JJK),UP1(JJK),&
          &UK1(JJK),PRYG(JJK),PRYF(JJK)
          ISX=1
      ELSE      
          CALL OPENV(KW(MSO+2),'RTCROP.DAT',KW(MSO),.TRUE.)
          READ(KW(MSO+2),3)IYS(1)
          READ(KW(MSO+2),2)(XRTC(I,JJK),I=1,8)
      END IF
      CLOSE(KW(MSO+2))   
      RETURN
    2 FORMAT(10F8.3)
    3 FORMAT(I4)
  170 FORMAT(///'*****UPDATE  YR = ',I2,'  MO = ',I2,'  DA = ',I2)
  174 FORMAT(5X,A4,10F7.2)
  191 FORMAT(5X,'BIOM=',F8.3,' t/ha   LAI=',F5.2,3X,'PPOP=',F6.1,'UN=',&
      F5.1,' kg/ha UP=',F5.1,' kg/ha',3X,'UK=',F5.1,' kg/ha',3X,'PRYG=',&
      F5.0,'$/t',3X,'PRYF=',F5.0,'$/t')
      END