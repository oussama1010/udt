
      SUBROUTINE GAUSSN0(NSIZ,NN,Z,R,NRHS,KZERO)
C     *******************************************************
C     *                                                     *
C     *   Solves general NxN system in NN unknowns          *
C     *    with arbitrary number (NRHS) of righthand sides. *
C     *   Assumes system is invertible...                   *
C     *    ...if it isn't, a divide by zero will result.    *
C     *                                                     *
C     *   Z is the coefficient matrix...                    *
C     *     ...destroyed during solution process.           *
C     *   R is the righthand side(s)...                     *
C     *     ...replaced by the solution vector(s).          *
C     *                                                     *
C     *                              Mark Drela  1984       *
C     *******************************************************
C
      DIMENSION Z(NSIZ,NSIZ), R(NSIZ,NRHS)
C
      DO 1 NP=1, NN-1
        NP1 = NP+1
C
C------ find max pivot index NX
        NX = NP
        DO 11 N=NP1, NN
          IF(ABS(Z(N,NP))-ABS(Z(NX,NP))) 11,11,111
  111      NX = N
   11   CONTINUE
C
        IF(Z(NX,NP) .EQ. 0.0) THEN
         KZERO = NP
         RETURN
        ENDIF
C
        PIVOT = 1.0/Z(NX,NP)
C
C------ switch pivots
        Z(NX,NP) = Z(NP,NP)
C
C------ switch rows & normalize pivot row
        DO 12 L=NP1, NN
          TEMP = Z(NX,L)*PIVOT
          Z(NX,L) = Z(NP,L)
          Z(NP,L) = TEMP
   12   CONTINUE
C
        DO 13 L=1, NRHS
          TEMP = R(NX,L)*PIVOT
          R(NX,L) = R(NP,L)
          R(NP,L) = TEMP
   13   CONTINUE
C
C------ forward eliminate everything
        DO 15 K=NP1, NN
          ZTMP = Z(K,NP)
C
C          IF(ZTMP.EQ.0.0) GO TO 15
C
          DO 151 L=NP1, NN
            Z(K,L) = Z(K,L) - ZTMP*Z(NP,L)
  151     CONTINUE
          DO 152 L=1, NRHS
            R(K,L) = R(K,L) - ZTMP*R(NP,L)
  152     CONTINUE
   15   CONTINUE
C
    1 CONTINUE
C
      IF(Z(NN,NN) .EQ. 0.0) THEN
       KZERO = NN
       RETURN
      ENDIF
C
C---- solve for last row
      DO 2 L=1, NRHS
        R(NN,L) = R(NN,L)/Z(NN,NN)
    2 CONTINUE
C
C---- back substitute everything
      DO 3 NP=NN-1, 1, -1
        NP1 = NP+1
        DO 31 L=1, NRHS
          DO 310 K=NP1, NN
            R(NP,L) = R(NP,L) - Z(NP,K)*R(K,L)
  310     CONTINUE
   31   CONTINUE
    3 CONTINUE
C
      KZERO = 0
      RETURN
      END ! GAUSSN0

