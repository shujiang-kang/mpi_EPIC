      SUBROUTINE EWNINT
!     EPIC1102
!     THIS SUBPROGRAM ESTIMATES DAILY POTENTIAL WIND EROSION FOR A BARE
!     SOIL BY INTEGRATING EROSION RATES WITH TIME GIVEN THE WIND SPEED
!     DISTRIBUTION
      USE PARM
      WW=MIN(.5,ATRI(.1,.35,.6,9)*UAVM(MO)/U10)
      SUM=.6424*WW**(-.1508)*EXP(.4336*WW)
      DU10=USTRT/.0408
      Y1=DU10/U10
      XX=LOG10(Y1)/WW
	  IF(XX>1.3)THEN
          YW=0.
          RETURN
      END IF
      IF(XX<-3.)GO TO 1
	  XX=10**XX
	  GO TO 6    
    1 X1=1.
	  GO TO 4 
    6 X1=EXP(-XX)
    4 DX=.1
      YW=0.
      Z1=0.
      DO WHILE(DX>1.E-4)
          XY=0.
          DO WHILE(XY<.1)
              X2=X1-DX
              IF(X2<=0.)EXIT
              Y2=(-LOG(X2))**WW/SUM
              Z2=EROWN(Y2)
              XY=(Y1+Y2)*DX
              XZ=(Z2+Z1)*DX
              YW=YW+XZ
              X1=X2
              Y1=Y2
              Z1=Z2
          END DO
          DX=DX*.5
      END DO
      YW=.5*YW
      RETURN
      END