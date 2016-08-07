      SUBROUTINE PSTTBL
!     EPIC1102
!     THIS SUBPROGRAM READS PESTICIDE TABLE TO DETERMINE PESTICIDE
!     PARAMETERS
      USE PARM
      DIMENSION XTP(8)
      IF(NDP>0)THEN
          DO L=1,NDP
              IF(KDP(L)==JX(7))RETURN
          END DO
      END IF
      NDP=NDP+1
      KDP(NDP)=JX(7)
      KDP1(JX(7))=NDP
!     READ PESTICIDE TABLE
!  1  PSTN = PESTICIDE NAME
!  2  PSOL = PESTICIDE SOLUBILITY (ppm)
!  3  PHLS = PESTICIDE HALF LIFE IN SOIL (d)
!  4  PHLF = PESTICIDE HALF LIFE ON FOLIAGE (d)
!  5  PWOF = PESTICIDE WASH OFF FRACTION
!  6  PKOC = PESTICIDE ORGANIC C ADSORPTION COEF
!  7  PCST = PESTICIDE COST ($/KG)
!  8  PCEM = C EMMISSION/UNIT PESTICIDE(G/G)
      J1=-1
      DO WHILE(J1/=JX(7))
          READ(KR(8),1,IOSTAT=NFL)J1,PSTN(NDP),(XTP(L),L=2,8)
          IF(NFL/=0)THEN
              IF(IBAT==0)THEN
                  WRITE(*,*)'PEST NO = ',JX(7),' NOT IN PEST LIST FILE'
!                 if defined(DEBUG) .AND. defined(PAUSE_ENABLE)
!                    PAUSE
!                 endif
              ELSE
                  WRITE(KW(MSO),'(A,A8,A,I4,A)')'!!!!! ',ASTN,' PEST NO = ',&
                  &JX(7),' NOT IN PEST LIST FILE'
              END IF
              STOP 7
          END IF
      END DO
      PSOL(NDP)=XTP(2)
      PHLS(NDP)=XTP(3)
      PHLF(NDP)=XTP(4)
      PWOF(NDP)=XTP(5)
      PKOC(NDP)=XTP(6)
      PCST(NDP)=XTP(7)
      PCEM(NDP)=XTP(8)
      REWIND KR(8)
      RETURN
    1 FORMAT(I5,1X,A16,F12.0,3F8.0,F10.0,2F8.0)    
      !1 FORMAT(I5,1X,A16,6E16.6)    
      END
