      SUBROUTINE EWIK
!     THIS SUBPROGRAM ESTIMATES THE SOIL ERODIBILITY FACTOR FOR THE WIND
!     EROSION EQ.
      USE PARM
      IF(SAN(LD1)>85.+.5*CLA(LD1))THEN
          WK=1.
          RETURN
      END IF
      IF(SAN(LD1)>70.+CLA(LD1))THEN
          WK=.43
          RETURN
      END IF
      IF(SIL(LD1)>80..AND.CLA(LD1)<12.)THEN
          WK=.12
          RETURN
      END IF
      IF(CAC(LD1)>0.)THEN
          IF(SAN(LD1)<45..OR.CLA(LD1)<20..OR.SIL(LD1)>28.)THEN
              WK=.28
              RETURN
          ELSE
              WK=.18
              RETURN
          END IF
      END IF  
      IF(CLA(LD1)<7.)THEN
          IF(SIL(LD1)<50.)THEN
              WK=.28
              RETURN
          ELSE
              WK=.18
              RETURN
          END IF
      END IF        
      IF(CLA(LD1)<20.)THEN
          IF(SAN(LD1)>52.)THEN
              WK=.28
              RETURN
          ELSE
              WK=.18
              RETURN
          END IF
      END IF        
      IF(CLA(LD1)<27.)THEN
          IF(SIL(LD1)<28.)THEN
              WK=.18
              RETURN
          ELSE
              WK=.16
              RETURN
          END IF
      END IF        
      IF(CLA(LD1)<35..AND.SAN(LD1)<20.)THEN
          WK=.12
          RETURN
      END IF
      IF(CLA(LD1)<35.)THEN
          IF(SAN(LD1)<45.)THEN        
              WK=.16
              RETURN
          ELSE
              WK=.18
              RETURN
          END IF
      END IF        
      IF(SAN(LD1)>45.)THEN
          WK=.18
          RETURN
      ELSE
          WK=.28
          RETURN
      END IF
      RETURN
      END