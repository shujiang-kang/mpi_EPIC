      SUBROUTINE WHLRMX
!     EPIC1102
!     THIS SUBPROGRAM COMPUTES DAY LENGTH & MAX SOLAR RADIATION AT THE
!     EARTHS SURFACE.
      USE PARM
      XI=JDA
      SD=.4102*SIN((XI-80.25)/PIT)
      CH=-YTN*TAN(SD)
      IF(CH>=1.)THEN
          H=0.
      ELSE
          IF(CH<=-1.)THEN
              H=3.1416
          ELSE
              H=ACOS(CH)
          END IF
      END IF
      DD=1.+.0335*SIN((XI+88.2)/PIT)
      HRLT=7.72*H
      HR1=HRLT-HR0
      HR0=HRLT
      RAMX=30.*DD*(H*YLTS*SIN(SD)+YLTC*COS(SD)*SIN(H))
      RETURN
      END