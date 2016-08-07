      SUBROUTINE WREAD
!     EPIC1102
!     THIS SUBPROGRAM READS THE DAILY WEATHER FILE TO THE DAY BEFORE THE
!     SIMULATION BEGINS.
      USE PARM
      DIMENSION MOFD(12),XTP(7)
      DATA MOFD/31,29,31,30,31,30,31,31,30,31,30,31/
      READ(KR(7),2)I3,I2,I1,(XTP(L),L=1,7)
      J3=10000*I3
      J1=100*I2+J3
      II=I1+J1
      K1=I2
	  IF(II>=IBDT)THEN
	      IBDT=II
	      IYR0=I3
	      REWIND KR(7)
	      RETURN
	  ELSE
          DO	
              CALL ALPYR(I3,NT,LPYR)
              DO I2=K1,12
                  N1=MOFD(I2)
                  IF(I2==2)N1=N1-NT
                  J2=100*I2
                  J1=J2+J3
                  DO WHILE(I1<N1)
                      I1=I1+1
                      II=J1+I1
                      IF(II==IBDT)RETURN
                      READ(KR(7),1,IOSTAT=NFL)(XTP(L),L=1,7)
	                  IF(NFL/=0)THEN
	                      WRITE(KW(MSO),'(T10,A)')'START DATE EXCEEDS&
                          & WEATHER FILE--WEATHER GENERATED'
	                      NGN=0
	                      RETURN
	                  END IF
	              END DO
                  I1=0
              END DO
              K1=1
              I3=I3+1
              J3=10000*I3
          END DO
      END IF
      RETURN
    1 FORMAT(14X,7F6.0)
    2 FORMAT(2X,3I4,7F6.0)
      END