C PROGRAM NAME:  assign_hydseq.for
C COMMAND NAME:  assign_hydseq.run
C LANGUAGE:  Fortran with call to IMSL routine
C 
C=============================================================

C PURPOSE:  Program creates two SPARROW pre-processing variables:
C           HYDSEQ and DEMTAREA, the hydrologic sequence code and
C           the total drainage area by reach.  User may allow
C           program to automatically identify headwater reaches.
C 
C=============================================================

C HISTORY:
C         Programmer               Date              Comments
C
C R.B. Alexander                 06/27/00
C                                05/20/02      Added code to handle 
C                                                nonzero IPTR
C                                06/21/02      Change output to 
C                                              file 20 so that
C                                              to- and from-node
C                                              order identical for 
C                                              all three write statements
C                                12/20/02      Final cleanup for release
C                                01/14/03      Increase array sizes,
C                                               increased output formats
C                                               for reaches and total area
C                                01/28/03      Added option to identify the
C                                               nearest downstream monitoring
C                                               station for each reach; output
C                                               headwater flag to hydseq file
C                                08/12/04      Replace IMSL sort with 
C                                               subroutine QSORTI
C=============================================================

C NOTES:
C
C 1.  The program employs stacks and a binary-search algorithm to 
C     climb upstream.  Stack structure was modified from Eckhouse 
C     (1975) "Minicomputer Systems:  Organization and Programming," 
C     Prentice-Hall, NJ, p. 132.  The binary-search sub-program was
C     written by Kenneth J. Lanfear, U.S. Geological Survey, Water
C     Resources Division.
C 
C 2.  One input file is required. Each file is in free-format,
C      meaning each record is a list of numbers separated by
C      any number of spaces.  No sorting is required on the file.
C
C 3. The reach file is topologically correct (full connectivity)
C    and contains a from-node# and to-node# for every reach in
C    the domain.  Flow direction is FROM-TO.
C    
C 4. The maximum limits of the program are in PARAMETER
C    statements below:
C      MAXARC:  Max # of arcs in reach network
C      MAXNEST: Max # of arcs in an UN-nested drainage structure
C
C 5. A maximum of 50000 nodes may be pushed onto the stack.
C 
C 6. Checks made on downstream TNODE for adjacent reaches to allow for
C       maximum of four tributary reaches.
C 
C=============================================================

C define common memory blocks, limits, stack
      COMMON /COMSTA/ STACKA,IPTR
      COMMON /ZZ1/ REACH
      COMMON /ZZ2/ FNODE
      COMMON /ZZ3/ TNODE
      COMMON /ZZ4/ AREA
      COMMON /ZZ5/ FRAC
      COMMON /YY/ SNODE
      COMMON /YY1/ FPERM
      COMMON /YY2/ TPERM
      COMMON /WW/ WREACH
      COMMON /WW1/ STNODE
      COMMON /WW2/ SFNODE
      COMMON /RR/ ARCH,DRCH,HRCH,CRCH
      COMMON /SS/ CAREA
      COMMON /SS1/ AREARCH
      COMMON /TT/ CHKRCH
      COMMON /TT1/ IUP
      COMMON /TT2/ IDOWN
      COMMON /TT3/ MONT,NMONT
      COMMON /TT4/ TEMP

C set parameters
      PARAMETER (MAXARC=600000)    !max arcs
      PARAMETER (MAXNEST=50000)    !max no. arcs in un-nested 

C declare variables
      INTEGER*4  STACKA(MAXNEST)
      INTEGER*4  IPTR,ARCH,DRCH,HRCH,CRCH
      INTEGER*4  SNODE
      INTEGER*4  NRA,INDEX,IHIGH,ILOW
      INTEGER*4  R,F,T,HD
      INTEGER*4  ICT1,ICT2
      INTEGER*4  WREACH(MAXARC),TEMP(MAXARC)
      INTEGER*4  REACH(MAXARC),FNODE(MAXARC),TNODE(MAXARC)
      INTEGER*4  STNODE(MAXARC),SFNODE(MAXARC)
      INTEGER*4  HEAD(MAXARC),FPERM(MAXARC),TPERM(MAXARC)
      INTEGER*4  CHKRCH(MAXARC),IUP(MAXARC),IDOWN(MAXARC)
      INTEGER*4  MONT(MAXARC),NMONT(MAXARC)
      REAL*4     AREA(MAXARC),FRAC(MAXARC)
      REAL*4     A,FR
      REAL*4     CAREA(MAXARC),AREARCH(MAXARC)

C=============================================================

C variable dictionary

C Index variables:
C INDEX,ITERM: arbitrary index names to arrays.
C IHIGH,ILOW : range of record number(s) for reach file which 
C    indicate the reach(es) immediately upstream to the current reach.
C ICT1..5: counters for output file records.
C IPTR : pointer in stack.
C NRA : actual number of reaches used in program.
C MAXARC : maximum number of reaches program will accept.

C Data arrays:

C FNODE:   from-node number of terminal node from reach file.
C REACH : cover number from ARC-INFO cover.AAT for a given reach (ERF1##)
C RFRAC: diversion fraction applied to sources and instream load in the 
C		intervening drainage area between WQ stations.
C RTOT: reach time of travel from stream velocity and reach length in days.
C SNODE : search to_node
C STACK : stack array stores reach numbers during reach climb.
C TNODE:  to-node number of terminal node from reach file
C 
C=============================================================
C
c
        write(*,*) ' '
        write(*,*) 'Beginning execution of MAKE_ATTS...'
        write(*,*) ' '

 889    CONTINUE
        write(*,*) 'Do you want the program to identify '
        write(*,*) '      headwater reaches?'
        write(*,*) '(1) YES (headwater reaches are NOT '
        write(*,*) '         identified in the input file)'
        write(*,*) '(2) NO (headwater reaches are already '
        write(*,*) '         identified in the input file)'
        write(*,*) 'Enter 1 or 2: '
        read(*,*) ISEL
        write(*,*)' '
        IF(ISEL.EQ.1.OR.ISEL.EQ.2) THEN
        ELSE
          WRITE(*,*)'Your choice is out of range--re-enter value'
          GOTO 889
        ENDIF

        write(*,*) ' '
 890    CONTINUE
        write(*,*) 'Do you want the program to code all reaches'
        write(*,*) ' upstream of monitoring stations with the'
        write(*,*) ' station ID number?'
        write(*,*) '(1) YES'
        write(*,*) '(2) NO '
        write(*,*) 'Enter 1 or 2: '
        read(*,*) IMON
        write(*,*)' '
        IF(IMON.EQ.1.OR.IMON.EQ.2) THEN
        ELSE
          WRITE(*,*)'Your choice is out of range--re-enter value'
          GOTO 890
        ENDIF


C initialize counter and other variables
        ICT1 = 0
        ICT2 = 0
        ISEQ = 0
        
C input file
	OPEN(10,FILE='reach.dat')

c output files
	OPEN(20,FILE='hydseq.dat')
      OPEN(21,FILE='nohydseq.dat')
        
C read reach file into memory 

      WRITE(*,*)' '
	WRITE(*,*) 'Reading reach file into memory...'
	WRITE(*,*) ' '

      DO 250 I=1,MAXARC
        STNODE(I) = 0
        SFNODE(I) = 0
        HEAD(I) = 0
        MONT(I) = 0
        NMONT(I) = 0
        AREA(I) = 0
 250    WREACH(I) = -1

         NRA=0
	DO 300  I = 1,MAXARC
       IF(ISEL.EQ.1) THEN
         READ(10,*,END=999) R,F,T,A,FR,MON
       ELSE
         READ(10,*,END=999) R,F,T,A,FR,MON,HD
       ENDIF
         NRA = NRA + 1
         IF(NRA.GT.600000) THEN
           WRITE(*,*)'ARCS EXCEED 600,000...STOP EXECUTION'
           STOP
         ENDIF
         WREACH(R) = 0
         REACH(R)=R
         FNODE(R)=F
         TNODE(R)=T
         AREA(R)=A
         FRAC(R)=FR
         IF(ISEL.EQ.2) HEAD(R)=HD
         STNODE(R)=T
         TPERM(R)=R
         SFNODE(R)=F
         FPERM(R)=R
         MONT(R)=MON
 300  CONTINUE
999	CONTINUE
      CLOSE(10)

C sort arrays
      CALL QSORTI (TPERM,MAXARC,STNODE) ! original array STNODE not sorted
      DO 305 I=1,MAXARC   ! temporary storage of array values
305      TEMP(I)=STNODE(TPERM(I))
      DO 306 I=1,MAXARC   ! place values in ascending order in original array
306      STNODE(I)=TEMP(I)

      CALL QSORTI (FPERM,MAXARC,SFNODE) ! original array STNODE not sorted
      DO 307 I=1,MAXARC   ! temporary storage of array values
307      TEMP(I)=SFNODE(FPERM(I))
      DO 308 I=1,MAXARC   ! place values in ascending order in original array
308      SFNODE(I)=TEMP(I)

C   set index variables to zero

	WRITE(*,*) 'Number of reach records...',NRA
	WRITE(*,*) ' '

C========================================
C  Identify headwater reaches (user choice)

       IF(ISEL.EQ.1) THEN
         WRITE(*,*)'Identifying headwater reaches...'
         IH=0
         DO 45 I=1,MAXARC
             IF(SFNODE(I).EQ.0) GOTO 45
           DO 50 K=1,MAXARC
             IF(SFNODE(I).EQ.STNODE(K)) GOTO 45
 50        CONTINUE
             HEAD(FPERM(I))=1
             IH=IH+1
 45      CONTINUE
         WRITE(*,*)'Number of headwater reaches = ',IH
         WRITE(*,*)' '
       ENDIF

C =============================================================


C begin loop through headwater reaches tracing downstream

      WRITE(*,*) ' '
      WRITE(*,*) 'Searching downstream reaches...creating HYDSEQ'
      WRITE(*,*) ' '

C========================================
C========================================
        IPTR = 0

        DO 1000 IREACH = 1,MAXARC
           IF(HEAD(IREACH).NE.1) GOTO 1000

C  write attributes for headwater reach

        HRCH = REACH(IREACH)
        ISEQ = ISEQ+1

        WRITE(20,5151)ISEQ,IREACH,FNODE(IREACH),TNODE(IREACH),
     &     AREA(IREACH),FRAC(IREACH),HEAD(IREACH)
        ICT2=ICT2+1
        WREACH(IREACH) = WREACH(IREACH) + 1
        SNODE = TNODE(IREACH)
        CRCH = HRCH
5151    FORMAT(4(I6,1x),F12.3,1X,E12.5,1X,I3)

C========================================

C  Find adjacent tributary reach

 66   CONTINUE
        CALL BSRCH(SNODE,MAXARC,STNODE,ILOW,IHIGH)

C  Determine if all tributary reaches have been processed
C      (as many as 4 tributaries may exist)

        IRCK = 0
        DO 750 INDEX = ILOW,IHIGH
          IF(WREACH(TPERM(INDEX)).EQ.1) IRCK=IRCK+1
 750    CONTINUE

        IRCH = IHIGH-ILOW+1

c===============================================
        IF(IRCK.LT.IRCH) THEN   

C      Adjacent reach attributes not written or this is a 
C          terminal reach (ILOW=IHIGH=0).
C      Get new reach from the stack or headwater array
          
          IF(IPTR.EQ.0) GOTO 1000  ! stack empty get headwater rch
          CALL POP(INDEX)
          ARCH = FPERM(INDEX)

          IF(WREACH(ARCH).EQ.0) THEN
            ISEQ = ISEQ+1
            WRITE(20,5151)ISEQ,FPERM(INDEX),FNODE(FPERM(INDEX)),
     &         TNODE(FPERM(INDEX)),AREA(FPERM(INDEX)),
     &         FRAC(FPERM(INDEX)),HEAD(FPERM(INDEX))  
            ICT2=ICT2+1
            WREACH(ARCH) = WREACH(ARCH) + 1
          ENDIF

          SNODE = TNODE(FPERM(INDEX))
          CRCH = ARCH
          GOTO 66
         
C==================================================================
C  All tributaries are processed/written - continue downstream

C  Find downstream reach (2 outcomes:  single, divergent)
C
       ELSE

C  downstream rch is single or divergent reach ===============
C    if divergent, write ilow, push ihigh

          SNODE = TNODE(TPERM(ILOW))
          CALL BSRCH(SNODE,MAXARC,SFNODE,ILOW,IHIGH) 
                                   !locate downstream reach FNODE

          IF(ILOW.EQ.0) THEN
C     no downstream reach - check the stack
           IF(IPTR.EQ.0) GOTO 1000  ! stack empty get headwater reach
           CALL POP(INDEX)
           ARCH = FPERM(INDEX)

           IF(WREACH(ARCH).EQ.0) THEN
            ISEQ = ISEQ+1
            WRITE(20,5151)ISEQ,FPERM(INDEX),FNODE(FPERM(INDEX)),
     &          TNODE(FPERM(INDEX)),AREA(FPERM(INDEX)),
     &          FRAC(FPERM(INDEX)),HEAD(FPERM(INDEX))
            ICT2=ICT2+1
            WREACH(ARCH) = WREACH(ARCH) + 1
           ENDIF

           SNODE = TNODE(FPERM(INDEX))
           CRCH = ARCH
           GOTO 66
          ENDIF
C===================================

          DRCH = FPERM(ILOW)

          IF(WREACH(DRCH).EQ.0) THEN
             ISEQ = ISEQ+1
             WRITE(20,5151)ISEQ,DRCH,FNODE(FPERM(ILOW)),
     &          TNODE(FPERM(ILOW)),
     &          AREA(FPERM(ILOW)),FRAC(FPERM(ILOW)),HEAD(DRCH)
             ICT2=ICT2+1
             WREACH(DRCH) = WREACH(DRCH) + 1
          ENDIF

C  divergent reach -- push ihigh reach, track ilow
          IF(ILOW.NE.IHIGH) THEN
            CALL PUSH(IHIGH) 
          ENDIF

C  assign downstream T-node as search node

          SNODE = TNODE(FPERM(ILOW))
          CRCH = DRCH
          GOTO 66

       ENDIF   ! end of check of IRCK vs IRCH


1000	CONTINUE
	
C========================================

      WRITE(*,*) ' ' 

C  Number of reaches unprocessed

        ICT1=0
        DO 22 IRCH = 1,MAXARC
           IF(WREACH(IRCH).EQ.0) THEN
              ICT1=ICT1+1
              WRITE(21,2222) IRCH
 2222         FORMAT(I5)
           ENDIF

 22     CONTINUE

        CLOSE(20)
        CLOSE(21)

        WRITE(*,*)'Records written to HYDSEQ.DAT = ',ICT2
        WRITE(*,*)'Number of reaches not processed = ',ICT1
        write(*,*)' '
        WRITE(*,*) 'HYDSEQ processing completed.'
        write(*,*)' '

C=========================================================================
C=========================================================================

      WRITE(*,*)' '
      WRITE(*,*)'Accumulating drainage area...'
      WRITE(*,*)' '

      OPEN(10,FILE='hydseq.dat')

      NR = 0
      NZERO=0

      DO 150 I = 1,MAXARC
 150    CHKRCH(I) = -1

      DO 155 I = 1,MAXARC
        READ(10,*,END=156) IHYDSEQ,REACH(I),IUP(I),IDOWN(I),
     &     AREA(I),FRAC(I),HEAD(I)
        IF(IHYDSEQ.LE.0) GOTO 155
        IF(AREA(I).EQ.0) NZERO=NZERO+1
        NR = NR + 1
        CHKRCH(REACH(I)) = 0
  155 CONTINUE
  156 CONTINUE
      CLOSE(10)

C--------------------------------------
C ACCUMULATE INCREMENTAL DRAINAGE AREA 
C--------------------------------------

      DO 580 I = 1,MAXARC
        CAREA(I) = 0
        AREARCH(I) = 0
 580  CONTINUE

      DO 1111 I = 1,NR

         CAREA(IDOWN(I)) = CAREA(IDOWN(I)) + 
     &      ( FRAC(I) * CAREA(IUP(I)) + AREA(I) )

         AREARCH(REACH(I)) = FRAC(I) * CAREA(IUP(I)) 
     &     + AREA(I)

 1111  CONTINUE

      IOUT=0
      OPEN(20,FILE='tarea.dat')
      DO 2002 I = 1,MAXARC
       IF(CHKRCH(I).EQ.0) THEN
          IOUT=IOUT+1
          WRITE(20,2020) I,AREARCH(I)
2020      FORMAT(I6,1X,E16.8)
       ENDIF
2002  CONTINUE
      CLOSE(20)

      WRITE(*,*)'Records written to TAREA.DAT = ',IOUT
      WRITE(*,*)'Reaches with zero incremental area = ',NZERO
      WRITE(*,*)' '
      WRITE(*,*)'TAREA processing completed.'


      IF(IMON.EQ.2) STOP

      WRITE(*,*)' '
      WRITE(*,*)' '
      WRITE(*,*)'Assigning monitoring station IDs'
      WRITE(*,*)' to upstream reaches...'
      WRITE(*,*)' '

c  process reaches in reverse - upstream order

      OPEN(25,FILE='reachsta.dat')
       NN=0
       ISTA=0
      DO 320 I=1,MAXARC
       NMONT(I) = 0 ! assign station ID to downstream node subscript
320   CONTINUE

      DO 350 I=NR,1,-1
       IF(MONT(REACH(I)).GT.0) THEN  ! a monitoring reach
          NMONT(IUP(I)) = MONT(REACH(I)) ! assign station ID to upstream node subscript
          WRITE(25,3303) REACH(I),MONT(REACH(I))
          ISTA=ISTA+1
          NN=NN+1
       ELSE
          NMONT(IUP(I)) = NMONT(IDOWN(I)) ! pass ID to upstream node
          WRITE(25,3303) REACH(I),NMONT(IDOWN(I))
          IF(NMONT(IDOWN(I)).GT.0) NN=NN+1
       ENDIF
3303   FORMAT(I6,1X,I8)
350   CONTINUE
      CLOSE(25)

      WRITE(*,3305)ISTA
3305  FORMAT(1x,'Number of monitoring stations = ',I6)
      WRITE(*,3304)NN
3304  FORMAT(1x,'Monitoring station IDs assigned to',I6,
     &' reaches...')
      WRITE(*,*)'Processing completed.'

	STOP
	END

C =============================================================

       SUBROUTINE PUSH (AVALUE)
	  INTEGER*4 STACKA(50000),AVALUE,IPTR
        COMMON /COMSTA/ STACKA,IPTR

	    IPTR = IPTR + 1
          IF(IPTR.GT.50000) THEN
            WRITE(*,1000) 
 1000       FORMAT(1X,'*** STACK MELTDOWN *** IPTR>50000')
            WRITE(*,*) '   '
            WRITE(*,*) AVALUE
            STOP
          ENDIF
	   STACKA(IPTR) = AVALUE
	RETURN
	END

C =============================================================

       SUBROUTINE POP (AVALUE)
        INTEGER*4 STACKA(50000),AVALUE,IPTR
        COMMON /COMSTA/ STACKA,IPTR

         AVALUE = STACKA(IPTR)
         IPTR = IPTR - 1
	RETURN
	END

C =============================================================

	SUBROUTINE BSRCH (NUM,N,IARRAY,J1,J2)
C
C Author: Kenneth J. Lanfear
C	  U.S. Geological Survey, Water Resources Division
C
C Finds the range of indices, J1 to J2, for which the
C ordered array, IARRAY, equals NUM.
C
c input arguments
      INTEGER*4 NUM
      INTEGER*4 N
      INTEGER*4 IARRAY(1)
c output arguments
      INTEGER*4 J1,J2
c internal arguments
      INTEGER*4 ITOP,IBOT,ITEST,IHIGH,ILOW
C
C Variable dictionary
C
C NUM : Value we are trying to match in IARRAY.
C N : Length of IARRAY.
C IARRAY : Vector to be searched.
C J1,J2 : Beginning and end of matching range.
C
C.... Find pointer to the first element that exceeds NUM
      IBOT = 0
      ITOP = N + 1
      DO 100 IXXX = 1, N
         IF ((ITOP-IBOT).EQ.1) GO TO 110
         ITEST = (ITOP + IBOT) / 2
         IF (IARRAY(ITEST).GT.NUM) THEN
               ITOP = ITEST
            ELSE
               IBOT = ITEST
            ENDIF
  100    CONTINUE
  110 CONTINUE
      IHIGH = ITOP
C
C.... Find pointer to the first element that is less than NUM
      IBOT = 0
      DO 200 IXXX = 1, N
         IF ((ITOP-IBOT).EQ.1) GO TO 210
         ITEST = (ITOP + IBOT) / 2
         IF (IARRAY(ITEST).LT.NUM) THEN
               IBOT = ITEST
            ELSE
               ITOP = ITEST
            ENDIF
  200    CONTINUE
  210 CONTINUE
      ILOW = IBOT
C
C.... Find the pointers to the equal range.
      IF ((IHIGH-ILOW).EQ.1) THEN
C....       There is no equal range
            J1 = 0
            J2 = 0
         ELSE
C....       Find the range
            J1 = ILOW + 1
            J2 = IHIGH - 1
         ENDIF
C
C.... Finished
      RETURN
      END


      SUBROUTINE QSORTI (ORD,N,A)
C
C==============SORTS THE ARRAY A(I),I=1,2,...,N BY PUTTING THE
C   ASCENDING ORDER VECTOR IN ORD.  THAT IS ASCENDING ORDERED A
C   IS A(ORD(I)),I=1,2,...,N; DESCENDING ORDER A IS A(ORD(N-I+1)),
C   I=1,2,...,N .  THIS SORT RUNS IN TIME PROPORTIONAL TO N LOG N .
C
C
C     ACM QUICKSORT - ALGORITHM #402 - IMPLEMENTED IN FORTRAN 66 BY
C                                 WILLIAM H. VERITY, WHV@PSUVM.PSU.EDU
C                                 CENTER FOR ACADEMIC COMPUTING
C                                 THE PENNSYLVANIA STATE UNIVERSITY
C                                 UNIVERSITY PARK, PA.  16802
C
      IMPLICIT INTEGER (A-Z)
C
      COMMON /UU2/ POPLST

      INTEGER*4 ORD(N)
      INTEGER*4 POPLST(2,600000)
      INTEGER*4 X,XX,Z,ZZ,Y
C
C     TO SORT DIFFERENT INPUT TYPES, CHANGE THE FOLLOWING
C     SPECIFICATION STATEMENTS; FOR EXAMPLE, FOR FORTRAN CHARACTER
C     USE THE FOLLOWING:  CHARACTER *(*) A(N)
C
      INTEGER*4 A(N)
C
      NDEEP=0
      U1=N
      L1=1
      DO 1  I=1,N
    1 ORD(I)=I
    2 IF (U1.LE.L1) RETURN
C
    3 L=L1
      U=U1
C
C PART
C
    4 P=L
      Q=U
C     FOR CHARACTER SORTS, THE FOLLOWING 3 STATEMENTS WOULD BECOME
C     X = ORD(P)
C     Z = ORD(Q)
C     IF (A(X) .LE. A(Z)) GO TO 2
C
C     WHERE "CLE" IS A LOGICAL FUNCTION WHICH RETURNS "TRUE" IF THE
C     FIRST ARGUMENT IS LESS THAN OR EQUAL TO THE SECOND, BASED ON "LEN"
C     CHARACTERS.
C
      X=A(ORD(P))
      Z=A(ORD(Q))
      IF (X.LE.Z) GO TO 5
      Y=X
      X=Z
      Z=Y
      YP=ORD(P)
      ORD(P)=ORD(Q)
      ORD(Q)=YP
    5 IF (U-L.LE.1) GO TO 15
      XX=X
      IX=P
      ZZ=Z
      IZ=Q
C
C LEFT
C
    6 P=P+1
      IF (P.GE.Q) GO TO 7
      X=A(ORD(P))
      IF (X.GE.XX) GO TO 8
      GO TO 6
    7 P=Q-1
      GO TO 13
C
C RIGHT
C
    8 Q=Q-1
      IF (Q.LE.P) GO TO 9
      Z=A(ORD(Q))
      IF (Z.LE.ZZ) GO TO 10
      GO TO 8
    9 Q=P
      P=P-1
      Z=X
      X=A(ORD(P))
C
C DIST
C
   10 IF (X.LE.Z) GO TO 11
      Y=X
      X=Z
      Z=Y
      IP=ORD(P)
      ORD(P)=ORD(Q)
      ORD(Q)=IP
   11 IF (X.LE.XX) GO TO 12
      XX=X
      IX=P
   12 IF (Z.GE.ZZ) GO TO 6
      ZZ=Z
      IZ=Q
      GO TO 6
C
C OUT
C
   13 CONTINUE
      IF (.NOT.(P.NE.IX.AND.X.NE.XX)) GO TO 14
      IP=ORD(P)
      ORD(P)=ORD(IX)
      ORD(IX)=IP
   14 CONTINUE
      IF (.NOT.(Q.NE.IZ.AND.Z.NE.ZZ)) GO TO 15
      IQ=ORD(Q)
      ORD(Q)=ORD(IZ)
      ORD(IZ)=IQ
   15 CONTINUE
      IF (U-Q.LE.P-L) GO TO 16
      L1=L
      U1=P-1
      L=Q+1
      GO TO 17
   16 U1=U
      L1=Q+1
      U=P-1
   17 CONTINUE
      IF (U1.LE.L1) GO TO 18
C
C START RECURSIVE CALL
C
      NDEEP=NDEEP+1
      POPLST(1,NDEEP)=U
      POPLST(2,NDEEP)=L
      GO TO 3
   18 IF (U.GT.L) GO TO 4
C
C POP BACK UP IN THE RECURSION LIST
C
      IF (NDEEP.EQ.0) GO TO 2
      U=POPLST(1,NDEEP)
      L=POPLST(2,NDEEP)
      NDEEP=NDEEP-1
      GO TO 18
C
C END SORT
C END QSORT
C
      END
