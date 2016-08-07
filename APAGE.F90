  SUBROUTINE APAGE(IP)
!     EPIC1102
!     THIS SUBPROGRAM CHANGES PAGES, WRITES VERSION, DATE, & TITLE
      USE PARM
      ! DManowitz - 2/26/10: For help in comparing output between Windows & Linux,
      ! changing some of these outputs.
      ! David Manowitz (3/8/2011): Now making outputs dependent on ComparisonMode flag.
      ! CHARACTER(128):: OPSCFILE_SHORT="",SITEFILE_SHORT="",SOILFILE_SHORT=""
      CHARACTER(128) :: SHORTFILE = ""
      INTEGER:: POS
      
      IF (ComparisonMode .EQV. .FALSE.) THEN
!          WRITE(KW(1),1) VersionStr,TRIM (CommandName),IYER,IMON,IDAY,IT1,IT2,IT3   !skang
      ELSE
!          WRITE(KW(1),10) VersionStr   !skang
      END IF

!      WRITE(KW(1),2)TITLE   !skang
!      WRITE(KW(1),3)IRUN,IRO0,IGN !skang
      IF(IP==0)RETURN

      IF (ComparisonMode .EQV. .TRUE.) THEN
          ! DManowitz - 3/1/10: Only report the base file name (not the full path) for 
          ! comparing Win & Linux versions
          POS = SCAN (SITEFILE, '/\\', BACK = .TRUE.)
          IF (POS > 0) THEN
              SHORTFILE = SITEFILE (POS + 1:)
          ELSE
              SHORTFILE = SITEFILE
          END IF
!          WRITE (KW(1),4) TRIM (SHORTFILE)  !skang
      ELSE
!          WRITE (KW(1),4) TRIM (SITEFILE)   !skang
      END IF

      IF (ComparisonMode .EQV. .TRUE.) THEN
          ! DManowitz - 3/1/10: Only report the base file name (not the full path) for 
          ! comparing Win & Linux versions
          SHORTFILE = ""
          POS = SCAN (SOILFILE, '/\\', BACK = .TRUE.)
          IF (POS > 0) THEN
              SHORTFILE = SOILFILE (POS + 1:)
          ELSE
              SHORTFILE = SOILFILE
          END IF
!          WRITE (KW(1),4) TRIM (SHORTFILE)
      ELSE
!          WRITE (KW(1),4) TRIM (SOILFILE)
      END IF
      
      IF (ComparisonMode .EQV. .TRUE.) THEN
          ! DManowitz - 3/1/10: Only report the base file name (not the full path) for 
          ! comparing Win & Linux versions
          SHORTFILE = ""
          POS = SCAN (OPSCFILE, '/\\', BACK = .TRUE.)
          IF (POS > 0) THEN
              SHORTFILE = OPSCFILE (POS + 1:)
          ELSE
              SHORTFILE = OPSCFILE
          END IF
!          WRITE (KW(1),4) TRIM (SHORTFILE)
      ELSE
!          WRITE (KW(1),4) TRIM (OPSCFILE)
      END IF

      RETURN
!    1 FORMAT('1'/T5,'EPIC1102',2X,3I4,2X,2(I2,':'),I2)
    1 FORMAT('1'/T5,A,' (',A,')'2X,3I4,2X,2(I2,':'),I2)
    2 FORMAT(/(10X,20A4))
    3 FORMAT(5X,'RUN #=',I4,2X,'ROT #=',I4,2X,'GNSD #=',I4)
    4 FORMAT(5X,A)
!   10 FORMAT('1'/T5,'EPIC1102 - For comparison only!')
   10 FORMAT('1'/T5,A,' - For comparison!')
      END
