      subroutine maxpact(sim,numplt,maxsiz,alphac,morf,
     +           musuby,membry,numset)
c
c* passed in
c
      double precision sim(numplt,numplt)
      integer numplt
      integer maxsiz
      double precision alphac
      integer morf
c
c* passed out
c
      double precision musuby(numplt,numplt)
      integer membry(numplt,numplt)
      integer numset
c
c* local
c
      integer used(numplt)
      double precision musubx(numplt,numplt)
      integer membrx(numplt,numplt)
      double precision mnsimi(numplt)
      double precision maxsim
      integer maxpnt
      integer flag
      integer i,j,k
c
      do 10 i=1,numplt
        do 11 j=1,numplt
        used(j) = 0
        membrx(i,j) = 0
        musubx(i,j) = 0.0
   11   continue
c
      used(i) = 1
      musubx(i,i) = 1.0
      mnsimi(1) = 0.0
      membrx(i,1) = i
c
        do 12 j=2,maxsiz
        maxsim = 0.0
          do 13 k=1,numplt
          if (used(k) .eq. 1) goto 13
          if (morf .eq. 1) then
            temsim = 0.0
            do 14 l=1,numplt
            if (used(l) .eq. 1) then
              temsim = temsim + sim(k,l)
            endif
   14       continue
          else
            temsim = 1.0
            do 15 l=1,numplt
            if (used(l) .eq. 1) then
              temsim = min(temsim,sim(k,l))
            endif
   15       continue
          endif
          if (temsim .gt. maxsim) then
            maxpnt = k
            maxsim = temsim
          endif
   13   continue
c
        used(maxpnt) = 1
        if (morf .eq. 1) then
          mnsimi(j) = (((j-1)**2-(j-1))/2 * mnsimi(j-1) +
     +       maxsim) / ((j**2-j)/2)
        else
          mnsimi(j) = maxsim
        endif
        musubx(i,maxpnt) = mnsimi(j)
        membrx(i,j) = maxpnt
   12   continue
   10 continue
c
c* maxpact ********************** two ********************************
c
      numset = 0
      do 20 i=1,numplt
      flag = 0
        do 21 j=1,i-1
          do 22 k=1,numplt
          if (musubx(i,k) .ge. alphac .and. 
     +        musubx(j,k) .lt. alphac) then
            goto 21
          else if (musubx(i,k) .lt. alphac .and.    
     +             musubx(j,k) .ge. alphac) then
            goto 21
          endif
   22     continue
        flag = 1
   21   continue
      if (flag .eq. 0) then
        numset = numset + 1
        do 23 j=1,numplt
        musuby(numset,j) = musubx(i,j)
        membry(numset,j) = membrx(i,j)
   23   continue
      endif
   20 continue
c
      return
c
      end
