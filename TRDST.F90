      SUBROUTINE TRDST
!     EPIC1102
!     THIS SUBPROGRAM CONVERTS LIVE BIOMASS TO RESIDUE WHEN A CROP IS 
!     KILLED.
      USE PARM
      STL(JJK)=DM(JJK)-RW(JJK)
      STD(JJK)=STD(JJK)+STL(JJK)
      X1=DM(JJK)+1.E-10
      XX=UN1(JJK)/X1
      X3=UP1(JJK)/X1
      W1=UK1(JJK)/X1
      STDN(JJK)=STDN(JJK)+XX*STL(JJK)
      STDP=STDP+X3*STL(JJK)
      STDK=STDK+W1*STL(JJK)
      STDL=STDL+CLG*STL(JJK)
      DO J=1,LRD
          ISL=LID(J)
          X1=RWT(ISL,JJK)
          X2=X1*XX
          CALL NCNSTD(X1,X2,1)
          FOP(ISL)=FOP(ISL)+X1*X3
          SOLK(ISL)=SOLK(ISL)+X1*W1
          RWT(ISL,JJK)=0.
      END DO
      YLD(JJK)=0.
      DM(JJK)=0.
      STL(JJK)=0.
      UN1(JJK)=0.
      UP1(JJK)=0.
      UK1(JJK)=0.
      RW(JJK)=0.
      RD(JJK)=0.
      RETURN
      END