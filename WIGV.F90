      SUBROUTINE WIGV
!     EPIC1102
!     THIS SUBPROGRAM WRITES THE NAMES OF THE INPUT WEATHER VARIABLES.
      USE PARM
      K=1
      N1=NGN
!      WRITE(KW(1),673) TRIM (ADJUSTL (FWTH))
      DO J=4,1,-1
          CALL AISPL(N1,N2)
          IF(N1==0)GO TO 1
          KGN(N1)=1
          SELECT CASE(N1)
              CASE(1)
                  GO TO 42
              CASE(2)
                  KDT2(K)=67
              CASE(3)
                  KDT2(K)=3
              CASE(4) 
                  KDT2(K)=7
              CASE(5)
                  KDT2(K)=8
              CASE DEFAULT
          END SELECT
          K=K+1
   42     N1=N2
      END DO
    1 SELECT CASE(K)
          CASE(1)
 !             WRITE(KW(1),222)
          CASE(2)
 !             WRITE(KW(1),356)HED(KDT2(1))
          CASE(3)
 !             WRITE(KW(1),357)(HED(KDT2(J)),J=1,2)
          CASE(4)
 !             WRITE(KW(1),358)(HED(KDT2(J)),J=1,3)
          CASE(5)
 !             WRITE(KW(1),359)(HED(KDT2(J)),J=1,4)
      END SELECT
      RETURN
  222 FORMAT(/T10,'**********RAIN IS INPUT**********')
  356 FORMAT(/T10,'**********RAIN,',1X,A4,', ARE INPUT**********')
  357 FORMAT(/T10,'**********RAIN,',2(1X,A4,','),' ARE INPUT**********')
  358 FORMAT(/T10,'**********RAIN,',3(1X,A4,','),' ARE INPUT**********')
  359 FORMAT(/T10,'**********RAIN,',4(1X,A4,','),' ARE INPUT**********')
  673 FORMAT(T10,'DAILY WEATHER FILE = ',A)
	END