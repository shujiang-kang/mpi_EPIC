      SUBROUTINE NFTBL(L)
!     EPIC1102
!     THIS SUBPROGRAM READS FERTILIZER TABLE TO DETERMINE PARAMETERS OF
!     INPUT FERTILIZER
      USE PARM
      DIMENSION XTP(11)
      IF(NDF>0)THEN
          DO L=1,NDF
              IF(KDF(L)==JX(7))RETURN
          END DO
      END IF
      NDF=NDF+1
      KDF(NDF)=JX(7)
      KDF1(JX(7))=NDF
!     READ FERTILIZER TABLE
!  1  FTNM = FERTILIZER NAME
!  2  FN   = MINERAL N FRACTION
!  3  FP   = MINERAL P FRACTION
!  4  FK   = MINERAL K FRACTION
!  5  FNO  = ORGANIC N FRACTION
!  6  FPO  = ORGANIC P FRACTION
!  7  FNH3 = AMMONIA N FRACTION(FNH3/FN)
!  8  FOC  = ORGANIC C FRACTION
!  9  FSLT = SALT FRACTION
! 10  FCST = COST OF FERTILIZER($/KG)
! 11  FCEM = C EMITTED/UNIT OF FERTILIZER(KG/KG)
      DO
          READ(KR(9),395,IOSTAT=NFL)I,FTNM(NDF),(XTP(K),K=2,11)
          IF(NFL/=0)THEN
              IF(IBAT==0)THEN
                  WRITE(*,*)'FERT NO = ',JX(7),' NOT IN FERT LIST FILE'
!                 if defined(DEBUG) .AND. defined(PAUSE_ENABLE)
!                    PAUSE
!                 endif
              ELSE
                  WRITE(KW(MSO),'(3A,I4,A)')'!!!!! ',TRIM(ASTN),'FERT NO = ',&
                      JX(7),' NOT IN FERT LIST FILE'
              END IF
              STOP 5
          END IF
          IF(I==JX(7))EXIT
      END DO
      FN(NDF)=XTP(2)
      FP(NDF)=XTP(3)
      FK(NDF)=XTP(4)
      FNO(NDF)=XTP(5)
      FPO(NDF)=XTP(6)
      FNH3(NDF)=XTP(7)
      FOC(NDF)=XTP(8)
      FSLT(NDF)=XTP(9)
      FCST(NDF)=XTP(10)
      FCEM(NDF)=XTP(11)
      REWIND KR(9)
      RETURN
  395 FORMAT(1X,I4,1X,A8,11F8.0)
      END