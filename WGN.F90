      SUBROUTINE WGN
!     EPIC1102
!     THIS SUBPROGRAM SIMULATES DAILY PRECIPITATION, MAXIMUM AND MINUMUM
!     AIR TEMPERATURE, SOLAR RADIATION AND RELATIVE HUMIDITY.  ALSO
!     ALSO PROVIDES AND OPTIONS TO SIMULATE VARIOUS COMBINATIONS
!     GIVEN DAILY PRECIPITATION.
      USE PARM
      DIMENSION A(3,3), B(3,3), XX(3), E(3)
      DATA A/.594,.454,-.004,.076,.261,-.037,-.018,-.129,.222/,B/.767,.&
     &304,.274,0.,.692,-.33,0.,0.,.873/
      XXX=.5*(TMXM-TMNM)
      III=1
      Z2=WFT(NWI,MO)
      YY=.9*Z2
      TXXM=TMXM+XXX*Z2
      RHM=(RH(NWI,MO)-YY)/(1.-YY)
      IF(RHM<.05)RHM=.5*RH(NWI,MO)
      RM=SRAM/(1.-.25*Z2)
      IF(RFV>0.)THEN
          TXXM=TXXM-XXX
          RM=.75*RM
          RHM=RHM*.1+.9
      END IF
      DO I=1,3
          V2=AUNIF(IDG(2))
          E(I)=ADSTN(V1,V2)
          V1=V2
      END DO
      DO I=1,3
          WX(I)=0.
          XX(I)=0.
              DO J=1,3
                  WX(I)=WX(I)+B(I,J)*E(J)
                  XX(I)=XX(I)+A(I,J)*XIM(J)
              END DO
      END DO
      DO I=1,3
          WX(I)=WX(I)+XX(I)
          XIM(I)=WX(I)
      END DO
      RETURN
      END