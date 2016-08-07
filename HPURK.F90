      SUBROUTINE HPURK
!     EPIC1102
!     THIS SUBPROGRAM IS THE MASTER PERCOLATION COMPONENT.  IT
!     MANAGES THE ROUTING PROCESS
      USE PARM
      ADD=0.
      SEP=RFV-QD
      DO KK=1,NBSL
          ISL=LID(KK)
          ST(ISL)=ST(ISL)+SEP
          IF(WTBL<=Z(ISL))THEN
              SSF(ISL)=0.
              PKRZ(ISL)=0.
              SEP=0.
              CYCLE
          END IF
          CALL HPERC
          ST(ISL)=ST(ISL)-SEP-SST
          SSF(ISL)=SST
          IF(ISL==IDR)THEN
              SMM(18,MO)=SMM(18,MO)+SST
              VAR(18)=SST
          END IF
          PKRZ(ISL)=SEP
          ADD=ADD+SST
      END DO
      SST=ADD
      K=NBSL
      DO K=NBSL,2,-1
          ISL=LID(K)
          L1=LID(K-1)
          XX=ST(ISL)-PO(ISL)
          IF(XX>0.)THEN
              ST(L1)=ST(L1)+XX
              PKRZ(L1)=MAX(0.,PKRZ(L1)-XX)
              ST(ISL)=PO(ISL)
          END IF
          XX=ST(ISL)-FC(ISL)
          IF(XX<=0.)CYCLE
	      WP1=LOG10(S15(L1))
          FC1=LOG10(FC(L1))
	      IF(ST(L1)>.01)THEN
	          T1=10.**(3.1761-1.6576*((LOG10(ST(L1))-WP1)/(FC1-WP1)))
	      ELSE
	          T1=1500.
	      END IF
	      WP2=LOG10(S15(ISL))
          FC2=LOG10(FC(ISL))
          T2=10.**(3.1761-1.6576*(LOG10(ST(ISL))-WP2)/(FC2-WP2))
          IF(T1<T2)CYCLE
	      X1=XX*MIN(.5,(T1-T2)/T1,PKRZ(L1))
          ST(L1)=ST(L1)+X1
          PKRZ(L1)=PKRZ(L1)-X1
          ST(ISL)=ST(ISL)-X1
      END DO
      IF(PKRZ(L1)<0.)PKRZ(L1)=0.
      RETURN
      END