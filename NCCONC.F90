      SUBROUTINE NCCONC
!	  THIS SUBROUTINE CALCULATES GAS CONCENTRATIONS IN LIQUID AND 
!	  AIR PHASES
      USE PARM                                                                       
!     When soil temperature ranges between 0 and -8C then
!     create variable VLWC (volumetric liquid water) and use
!     it instead of VWC in equations below
!     At 0C, VLWC=VWC
!     At -4C, VLWC=0.5VWC
!     At -8c, VLWC=0.  
!     6/13/11
!     IF(ABST>265.15) THEN proceed
!     if temp is between 0 and -8 then use VLWC instead of VWC                                                                             
	  DO J=1,NBCL
	      IF(SOT(J)>=0.)THEN
	          VLWC=VWC(J)
          ELSE
              IF(SOT(J)>-8.)THEN
                  VLWC=(1.+.125*SOT(J))*VWC(J)
              ELSE
                  VLWC=0.
              END IF
          END IF            	           
    !     CLO2=CONC GAS IN LIQ PHASE (G/M3 WATER)
          CLO2(J)=AO2C(J)/(AFP(J)*HKPO(J)+VWC(J))   
    !     CGO2=CONC GAS IN GAS PHASE (G/M3 AIR)
          CGO2(J)=AO2C(J)/(TPOR(J)+VWC(J)*(1./HKPO(J)-1.))
    !     CLCO2=CONC GAS IN LIQ PHASE
          CLCO2(J)=ACO2C(J)/(AFP(J)*HKPC(J)+VWC(J))   
    !     CGCO2=CONC GAS IN GAS PHASE
          CGCO2(J)=ACO2C(J)/(TPOR(J)+VWC(J)*(1./HKPC(J)-1.))
    !     CLN2O=CONC GAS IN LIQ PHASE
	      CLN2O(J)=AN2OC(J)/(AFP(J)*HKPN(J)+VWC(J)) 
    !     CGN2O=CONC GAS IN GAS PHASE
          CGN2O(J)=AN2OC(J)/(TPOR(J)+VWC(J)*(1./HKPN(J)-1.))
      END DO
      RETURN
      END