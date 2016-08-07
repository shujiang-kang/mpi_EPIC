      SUBROUTINE WDOP(ID)
!     EPIC1102
!     THIS SUBPROGRAM READS THE DAILY WEATHER LIST AND LOCATES THE 
!     SPECIFIED STATION (IWTH) OR THE NEAREST STATION IF IWTH=0.
      USE PARM
      IF(IWTH==0)THEN
          W0=1.E20
          DO 
              READ(KR(27),*,IOSTAT=NFL)II,OPSCFILE,Y,X,ELEX
	          IF(NFL/=0)EXIT
              RY=Y/CLT
              XX=SIN1*SIN(RY)+COS1*COS(RY)*COS((X-XLOG)/CLT)
              D=6378.8*ACOS(XX)
              E=ABS(ELEV-ELEX)
              W1=PRMT(79)*D+(1.-PRMT(79))*E                                                                  
              IF(W1>=W0)CYCLE                                                                
              W0=W1           
              FWTH=OPSCFILE
	      END DO
      ELSE
          II=-1
          DO WHILE(II/=IWTH)
              READ(KR(27),*,IOSTAT=NFL)II,FWTH
	      IF(NFL/=0)THEN
	          IF(IBAT==0)THEN
	              WRITE(*,*)'FWTH NO = ',IWTH,' NOT IN DAILY WEATHER LIST FILE'
!                     if defined(DEBUG) .AND. defined(PAUSE_ENABLE)
!                         PAUSE
!                     endif
	          ELSE
	    	      WRITE(KW(MSO),'(3A,I4,A)')'!!!!! ',TRIM(ASTN),' FWTH NO = ',&
                         IWTH,' NOT IN DAILY WEATHER LIST FILE'
                  END IF
                  STOP 9
              END IF                  
	  END DO
      END IF	
      REWIND KR(27)
      CALL OPENV(KR(7),FWTH,KW(MSO))
	  CALL WREAD
      RETURN
	  END
