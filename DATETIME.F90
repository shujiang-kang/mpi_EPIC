      SUBROUTINE GETDAT(IYER,IMON,IDAY)
!     EPIC0810 - Jeff Added for Linux PGI Fortran Compiler
!     THIS SUBPROGRAM RETURNS THE COMPONENTS OF TODAY'S DATE
      INTEGER VALS(8)
      
      CALL DATE_AND_TIME(values=VALS)

      IYER = VALS(1)
      IMON = VALS(2)
      IDAY = VALS(3)     
      RETURN
      END
      
      SUBROUTINE GETTIM(IEH,IEM,IES,I100)
!     EPIC0810 - Jeff Added for Linux PGI Fortran Compiler
!     THIS SUBPROGRAM RETURNS THE COMPONENTS OF CURRENT TIME

      INTEGER VALS(8)
      
      CALL DATE_AND_TIME(values=VALS)

      IEH = VALS(5)
      IEM = VALS(6)
      IES = VALS(7)
      I100 = VALS(8)
      RETURN
      END