!     PROCESSES THE COMMAND LINE OPTIONS FOR EPIC.
!     WRITTEN BY JEFF NICHOLS, ORNL, OCTOBER 2009 TO ENABLE BASE DIRECTORY OVERRIDE
!     ESPECIALLY FOR THE LINUX CLUSTER VERSION, BECAUSE THE BASE DIRECTORY MUST BE DIFFERENT
!     WHEN DISTRIBUTED ACROSS A CLUSTER.
      SUBROUTINE COMMAND_LINE
      USE PARM
#ifdef _DF_VERSION_
      USE DFPORT
#endif
      IMPLICIT NONE
      
      !DEFINE BUFFER HOLDS THE COMMAND LINE ARGUMENT
      !CHARACTER *128 BUFFER
      !If converting types, then use GETARG(1, BUFFER), READ(BUFFER,*) varname
      CHARACTER(256) :: buffer = ""
      INTEGER :: NumArgs,ArgIndex = 0
      CHARACTER(512) :: CommandLine = ""
      INTEGER :: CommandLineLength, status

      NumArgs = COMMAND_ARGUMENT_COUNT ()
      ! NumArgs = IARGC ()
      
      ! David Manowitz (8/22/2011): Get the name of the command as called on the command line.
      CALL GET_COMMAND_ARGUMENT (0, CommandName)
      
!CHECK IF THERE ARE ANY COMMAND LINE ARGUMENTS.  XARGC IS 0-BASED AND ARG(0) IS PROGRAM NAME.
!GET THE PARAMETERS FROM THE COMMAND LINE ARGUMENT.  There may be one or two parameters
!David Manowitz (3/8/2011): Now there may be up to 3 parameters
      DO WHILE ((ArgIndex < NumArgs) .AND. (ArgIndex < 4))
         ArgIndex = ArgIndex + 1
         ! CALL GETARG (ArgIndex, buffer)
         CALL GET_COMMAND_ARGUMENT (ArgIndex, buffer)
         buffer = TRIM (buffer)
         IF ((buffer == "-d") .OR. (buffer == "-D")) THEN            
            DebugOutput = .TRUE.
         ELSE IF ((buffer == "-c") .OR. (buffer == "-C")) THEN            
            ComparisonMode = .TRUE.
         ELSE
            BASEDIR = buffer
         END IF
      END DO

! DManowitz: VF 6.x is from WAY before 2003, so does not have any FORTRAN 2003 procedures.
! I don't feel like getting this correct just for VF6.x.
#ifndef _DF_VERSION_
      IF (DebugOutput .EQV. .TRUE.) THEN
          CALL GET_COMMAND (CommandLine, CommandLineLength, status)
          IF (status == -1) THEN 
              WRITE (*,*) "Command line is: " // CommandLine // "..."
          ELSE IF (status == 0) THEN
              WRITE (*,*) "Command Line is: " // TRIM (CommandLine)
          ELSE
              WRITE (*,*) "Error!  Unable to determine command line!"
          END IF
      END IF
#endif

      RETURN
      END      
      
