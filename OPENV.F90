!      SUBROUTINE OPENV (NUM, FNAM, ID, NMSO, ArgWritable)
      SUBROUTINE OPENV (NUM, FNAM, NMSO, ArgWritable)
!     EPIC1102      
!     VERIFIES THE EXISTENCE OF A FILE BEFORE OPENING IT
      USE PARM
 
      ! DManowitz: Follwing INCLUDE is for Windows VF11 only!
      ! INCLUDE 'for_iosdef.for'
      ! DManowitz: Follwing INCLUDE is for Windows VF6 only!
      ! INCLUDE 'iosdef.for'

      INTEGER                       :: NUM, NMSO  ! ID
      CHARACTER (LEN=*), INTENT(IN) :: FNAM
      LOGICAL, OPTIONAL, INTENT(IN) :: ArgWritable
      LOGICAL :: Writable

!Jeff
      ! CHARACTER*256::ODIR='//home//n3j//work//EPIC//EPIC//MI_RIMA//Run//'
      ! David Manowitz (3/8/2011): Change the default directory to be the current dir.
#ifdef _WIN32
      CHARACTER(512) :: ODIR = '.\\'
      INTEGER :: colonIndex
#else
      CHARACTER(512) :: ODIR = './/'
#endif
      CHARACTER(1024):: FNM
      CHARACTER(512):: err_str

!      CHARACTER*128:: FNAM,DIR='',FNM=''
	  LOGICAL::XMIS
	  !JEFF
      INTEGER::ierror

      !JEFF Override ODIR if folder name passed on command line
      IF (BASEDIR .NE. "") THEN
         ODIR = BASEDIR
      END IF 

      ! David Manowitz (4/25/11): Check to see if the 'Writable' argument was passed
      IF (PRESENT (ArgWritable)) THEN
          Writable = ArgWritable
      ELSE
          ! If not specified, assume false
          Writable = .FALSE.
      END IF

      ! David Manowitz (3/22/11): Don't adjust directory if it specifies an absolute path
      FNM = ADJUSTL (FNAM)
#ifdef _WIN32
      ! On Windows, also look for colons, signifying drive specifiers
      colonIndex = INDEX (FNM, ':')
      IF ((colonIndex == 0) .AND. (FNM (1:1) /= '\\')) THEN
#else
      IF (FNM (1:1) /= '/') THEN
#endif
         FNM = TRIM (ADJUSTL (ODIR)) // TRIM (ADJUSTL (FNAM))
      END IF 
      FNM = TRIM (FNM)
      IF (DebugOutput .EQV. .TRUE.) THEN
         write (*,*) 'Filename is ' // TRIM (FNM)
      END IF

    1 INQUIRE(FILE = FNM, EXIST = XMIS)
      IF(XMIS .EQV. .TRUE.)THEN
          IF (Writable) THEN
              OPEN (UNIT = NUM, FILE = FNM, ACTION = 'READWRITE', IOSTAT = ierror)
          ELSE
              OPEN (UNIT = NUM, FILE = FNM, STATUS = 'OLD', ACTION = 'READ', IOSTAT = ierror)
              ! CALL GERROR (err_str)
          ! END IF
          END IF
	  ELSE
	      IF (IBAT == 0 .OR. NUM == NMSO .OR. NUM < 0) THEN
              WRITE(*,'(/A/)')TRIM(ASTN) // ': file ' // TRIM(FNM) // ' IS MISSING.'
!#             if defined(DEBUG) .AND. defined(PAUSE_ENABLE)
!                PAUSE
!#             endif
              ! DManowitz - 11/9/11: Also stop
              STOP 2
          ELSE
              WRITE(NMSO,'(2A,1X,2A)')' !!!!! ',TRIM(ASTN),TRIM(FNM),&
              &' IS MISSING.'
          END IF
      END IF

      IF ((ierror /= 0) .AND. (DebugOutput .EQV. .TRUE.)) THEN
         write (*,*) 'ierror after open is', ierror  !', which means:'
         ! write(*,*) err_str

	  END IF	
	  RETURN
END SUBROUTINE OPENV
