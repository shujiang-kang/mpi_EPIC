      SUBROUTINE GASTRANS(CONC,DPRM, AFPVOL, CUP,CLO,NGS,DSURF, FLUXOUT)
!     SUBPROGRAM SOLVES THE GAS TRANSPORT EQUATION
      USE PARM
      DIMENSION A(100),B(100),C(100),D(100),CONC(MSC),DPRM(MSC)
      REAL, INTENT(in), DIMENSION(MSC) :: AFPVOL ! air-filled porosity
      REAL, INTENT(out) :: FLUXOUT
      REAL :: F1, F2            ! explicit and implicit surface fluxes
      DATA ALX/0.5/             ! ALX is the explicit fraction, 1-ALX the implicit
      R=DTG/(DZ*DZ)
      R1=ALX*R
      R2=(1.-ALX)*R
      ! R3=1.+2.*(1.-ALX)*R
      TGO2=0.
      TGCO2=0.
      TGN2O=0.
!     UPPER BOUNDARY CONDITION
      A(1)=-R2*DPRM(1)
      B(1)=0.0
      C(1)=AFPVOL(1)*CONC(1)+R1*(DPRM(1)*(CONC(2)-CONC(1))-DSURF*(CONC(1)-CUP))&
      +R2*DSURF*CUP
      D(1)=AFPVOL(1) + R2*(DSURF+DPRM(1))
      ! Calculate surface flux using the old concentration values
      F1 = -DSURF*(CONC(1)-CUP)/DZ
      ! print *,'start conc(1), cup, F1,alx : ', conc(1), cup, F1, alx !XXX
      ! MAIN COMPUTATIONS
      DO ID=2,IUN
          A(ID)=-R2*DPRM(ID)
          B(ID)=-R2*DPRM(ID-1)
          C(ID)=AFPVOL(ID)*CONC(ID)+R1*(DPRM(ID)*(CONC(ID+1)-CONC(ID))-DPRM(ID-1)*&
          (CONC(ID)-CONC(ID-1)))
          D(ID)=AFPVOL(ID) + R2*(DPRM(ID-1)+DPRM(ID))
      END DO
!     LOWER BOUNDARY CONDITION
      CLO=CONC(NBCL) !WBM & RCI 8/23/11
      A(NBCL)=0.0
      B(NBCL) = -R2*DPRM(NBCL-1)
      C(NBCL) = AFPVOL(NBCL)*CONC(NBCL) - R1*DPRM(NBCL-1)*(CONC(NBCL) - CONC(NBCL-1))
      D(NBCL) = AFPVOL(NBCL) + R2*DPRM(NBCL-1)
!     SOLVE TRIADIAGONAL SYSTEM
      CALL TRIDIAG(B,D,A,C,NBCL)
      ! Calculate surface flux using the new concentration values
      F2 = -DSURF*(C(1)-CUP)/DZ

      DO I=1,NBCL
          CONC(I)=MAX(1.E-10,C(I))
          IF(NGS==1)THEN
              CGO2(I)=CONC(I)
              CYCLE
          END IF
          IF(NGS==2)THEN
              CGCO2(I)=CONC(I)
          ELSE
              CGN2O(I)=CONC(I)
          END IF
      END DO
      
      ! print *, 'end conc(1), cup, F2 : ', conc(1), cup, F2
      ! Combine the fluxes in the same ratio used in the Crank-Nicholson scheme
      FLUXOUT = alx*F1 + (1-alx)*F2
      
      !X1=10.*DPRM(1)*(CUP-CONC(1))/(.5*DZ)
      !X2=10.*DPRM(NBCL)*(CONC(NBCL)-CLO)/(1000.*DZ)
!      SELECT CASE(NGS)
!          CASE(1)
!              GFO2=X1
!              BFO2=X2
!          CASE(2)
!              GFCO2=X1
!              BFCO2=X2
!          CASE(3)
!              GFN2O=X1
!              BFN2O=X2
!      END SELECT
      RETURN 
      END