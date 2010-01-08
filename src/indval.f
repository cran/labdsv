      subroutine duleg(veg,numplt,numspc,class,clstab,
     +      numcls,numitr,relfrq,relabu,indval,pval,
     +      indcls,maxcls,tmpfrq,tmpabu,pclass,tclass)
c
c* passed in
c
      double precision veg(numplt,numspc)
      integer numplt
      integer numspc
      integer class(numplt)
      integer clstab(numcls)
      integer numcls
      integer numitr
c
c* passed back
c
      double precision relfrq(numspc,numcls)
      double precision relabu(numspc,numcls)
      double precision indval(numspc,numcls)
      double precision pval(numspc)
      double precision indcls(numspc)
      integer maxcls(numspc)
c
c* relfrq, relabu, and indval all initialized to zero by R
c
c* scratch
c
      double precision tmpfrq(numcls)
      double precision tmpabu(numcls)
      double precision tmpind
      integer pclass(numplt)
      double precision maxval
      double precision tmpcls
      double precision totveg
      double precision sumrab
      double precision tmpsum
      integer tclass(numplt)

c
c*********************************** one *********************************
c
      do 10 i=1,numspc
      totveg = 0
        do 11 j=1,numplt
        if (veg(j,i) .gt. 0) then
          totveg = totveg + veg(j,i)
          relabu(i,class(j)) = relabu(i,class(j)) + veg(j,i)
          relfrq(i,class(j)) = relfrq(i,class(j)) + 1
        endif
   11   continue
c
        sumrab = 0.0
        do 12 j=1,numcls
        relabu(i,j) = relabu(i,j) / clstab(j)
        sumrab = sumrab + relabu(i,j)
        relfrq(i,j) = relfrq(i,j) / clstab(j)
   12   continue
c
        maxcls(i) = 0
        maxval = 0
        do 13 j=1,numcls
        relabu(i,j) = relabu(i,j) / sumrab
        indval(i,j) = relabu(i,j) * relfrq(i,j)
        if (indval(i,j) .gt. maxval) then
          maxcls(i) = j
          maxval = indval(i,j)
        endif
   13   continue
      indcls(i) = maxval
   10 continue
c
c*********************************** two ************************************
c
      do 20 i=1,numspc
        do 21 j=1,numitr-1
        tmpcls = 0
        tmpind = 0
        totveg = 0
        maxval = 0
        call permute(class,pclass,numplt,tclass)
          do 22 k=1,numcls 
          tmpfrq(k) = 0
          tmpabu(k) = 0
   22     continue
c
          do 23 k=1,numplt
          if (veg(k,i) .gt. 0) then
            totveg = totveg + veg(k,i)
            tmpabu(pclass(k)) = tmpabu(pclass(k)) + veg(k,i)
            tmpfrq(pclass(k)) = tmpfrq(pclass(k)) + 1
          endif
   23     continue
c
          tmpsum = 0.0
          do 24 k=1,numcls
          tmpabu(k) = tmpabu(k) / clstab(k)
          tmpsum = tmpsum + tmpabu(k)
          tmpfrq(k) = tmpfrq(k) / clstab(k)
   24     continue
c
          do 25 k=1,numcls
          tmpabu(k) = tmpabu(k) / tmpsum
          tmpind = tmpabu(k) * tmpfrq(k)
          if (tmpind .gt. maxval) then
            maxval = tmpind
          endif
   25     continue
c          if (maxval .ge. indval(i,maxcls(i))) then
          if (maxval - indval(i,maxcls(i)) .gt. -0.0001) then
            pval(i) = pval(i) + 1
          endif
   21   continue
      pval(i) = (pval(i)+1) / numitr
   20 continue
c
      return
c
      end
c
c************************************************************
c
      subroutine permute(class,pclass,numplt,tclass)
c
      integer class(numplt)
      integer pclass(numplt)
      integer numplt
c
c* local
c
      integer tclass(numplt)
      integer pool
c
      pool = numplt
      do 10 i=1,numplt
      tclass(i) = class(i)
   10 continue
c
      do 11 i=1,numplt
      index = rand()*(pool)+1
      pclass(i) = tclass(index)
      tclass(index) = tclass(pool)
      pool = pool - 1
   11 continue
c
      return
c
      end
