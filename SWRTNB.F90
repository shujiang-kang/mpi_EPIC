      SUBROUTINE SWRTNB(CM,CL,OC,SA,WC15,WC3RD,ZZ)
!     EPIC1102
!     THIS SUBPROGRAM USES OTTO BAUMER'S METHOD FOR ESTIMATING SOIL
!     WATER CONTENT AT 33 AND 1500 kpa.
      SI=100.-CL-SA
      OM=1.72*OC
      IF(ZZ<.25)THEN
          BV=1.085
          BW=1.035
          BO=1.9
      ELSE
          IF(ZZ<1.)THEN
              BV=1.
              BW=1.
              BO=1.
          ELSE
              BV=.915
              BW=.96
              BO=.1      
          END IF    
      END IF          
      APD=100./(37.74+.3366*OM)
      CE=CM+2.428*OC+1.7*ZZ
      CA=MIN(.8,CE/CL)
      X1=CL*CL
      X2=CA*CA
      IF(CL<10.)THEN
          CAAF=.099*X2*X1
      ELSE
          CAAF=9.9*X2
      END IF
      VOMO=BV*(42.84+1.37*OM+.00294*X1+CAAF+.0036*X1*X2-.0852*SA-.316*CL*CA)
      VODF=MAX(.01,BV*(.277+.16*OM-2.72*CA*X2+.268*CL*CA+.00546*X1*CA&
     &-.00184*X1*X2))
      WC15G=.71+.45*OM+.336*CL+.117*CL*CA**1.5
      VFS=.1*SA
      SICL=CL+.3333*(SI+VFS)
      IF(SICL<15.)THEN
          CF1=1.
          GO TO 5
      END IF
      IF(SICL<30.)THEN
          CF1=2.-.0667*SICL
      ELSE
          CF1=0.
      END IF    
    5 A1=14.94+3.8*X2-.137*SA
      BDX=APD*(1.-.01*VOMO)
      SDF=SA-VFS
      CF3=37.74*SDF/((100.-SDF)/BDX+.3774*SDF)
      SAF=1.-.005*CF3*(CF1+1.)
      W3RDG=BW*(A1*SAF+WC15G+.746*OM)
      OAIR=BO*(3.8+.00018*X1-.03365*SA+.126*CL*CA+.000025*OM*SA*SA)
      WSV=VOMO*(1.-.01*OAIR)
      WSG=WSV/BDX
      DBD=APD*(1.-.01*(VOMO-VODF))
      DBS=(BDX-DBD)/WSG
      WC3RD=.01*W3RDG*(DBD+DBS*W3RDG)
      WC15=.01*WC15G*(DBD+DBS*WC15G)
      RETURN
      END