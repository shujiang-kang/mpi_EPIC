      SUBROUTINE HRTRI
!     EPIC1102
!     THIS SUBPROGRAM DISTRIBUTES DAILY RAINFALL FROM A TRIANGLE &
!     FURNISHES THE GREEN & AMPT SUBPROGRAM RAIN INCREMENTS OF EQUAL
!     VOLUME = DRFV
      USE PARM
      DATA DRFV/2.5/
      PT=0.
      DUR=2.*RFV/REP
      UPLM=.95*DUR
      QMN=.25*DUR
      BLM=.05*DUR
      R1=ATRI(BLM,QMN,UPLM,8)
      RTP=.5*R1*REP
      B2=REP/R1
      RX=0.
      T=-100.
      DO WHILE(T<R1)
          PT=PT+DRFV
          T=SQRT(2.*PT/B2)
          RX=B2*T
          CALL HGAWY(DRFV,PT,Q1,RX)
      END DO
      T=R1
      A=RTP-PT+DRFV
      PT=RTP
      RX=REP
      CALL HGAWY(A,PT,Q1,RX)
      B2=REP/(DUR-R1)
      PX=REP+R1*B2
      BB=PX*PX
      DO WHILE(PT<RFV)
          PT=PT+DRFV
          T=(PX-SQRT(BB-2.*B2*(PT-RTP+R1*(REP+.5*B2*R1))))/B2
          RX=REP-B2*(T-R1)
          CALL HGAWY(DRFV,PT,Q1,RX)
      END DO
      T=DUR
      RX=0.
      A=RFV-PT
      PT=RFV
      CALL HGAWY(A,PT,Q1,RX)
!      WRITE(KW(1),13)IYR,MO,KDA,SCN,RFV,PT,QD,REP,DUR,R1  !skang
      RETURN
   13 FORMAT(1X,3I4,10F10.2)
      END