      SUBROUTINE NLIMA(SB,DSB,C1,PH,ALS,OC,BSA)
!     EPIC1102
!     THIS SUBPROGRAM ESTIMATES ALUMINUM SATURATION USING BASE
!     SATURATION, ORGANIC C, AND PH.
      SB=SB-DSB
      SB=MAX(.02,SB)
      BSA=C1*SB
      IF(PH<=5.6)THEN
          ALS=154.2-1.017*BSA-3.173*OC-14.23*PH
              IF(ALS<.01)THEN
                  ALS=0.
                  RETURN
              ELSE
                  IF(ALS>95.)ALS=95.
              END IF
      ELSE
          ALS=0.
      END IF        
      RETURN
      END