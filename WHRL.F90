      SUBROUTINE WHRL
!     EPIC1102
!     THIS SUBPROGRAM COMPUTES DAY LENGTH
      USE PARM
      XI=JDA
      SD=.4102*SIN((XI-80.25)/PIT)
      CH=-YTN1*TAN(SD)
      IF(CH>=1.)THEN
          H=0.
      ELSE
          IF(CH<=-1.)THEN
              H=3.1416
          ELSE
              H=ACOS(CH)
          END IF
      END IF
      HRLT=7.72*H
      HR1=HRLT-HR0
      HR0=HRLT
      RETURN
      END