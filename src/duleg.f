      subroutine duleg(veg,numplt,numspc,class,clstab,
     +      numcls,numitr,relfrq,relabu,indval,pval,
     +      indcls,maxcls,tmpfrq,tmpabu,pclass)
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
      real maxval
      real tmpcls
      real totveg
      real sumrab
      real tmpsum
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
        do 21 j=1,numitr
        tmpcls = 0
        tmpind = 0
        totveg = 0
        maxval = 0
        call permute(class,pclass,numplt)
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
          if (maxval .ge. indval(i,maxcls(i))) then
            pval(i) = pval(i) + 1
          endif
   21   continue
      pval(i) = (pval(i)+1) / (numitr + 1)
   20 continue
c
      return
c
      end
c
c************************************************************
c
      subroutine permute(class,pclass,numplt)
c
      integer class(numplt)
      integer pclass(numplt)
      integer numplt
c
c* local
c
      integer tclass(numplt)
c
      do 10 i=1,numplt
      tclass(i) = class(i)
   10 continue
c
      do 11 i=1,numplt-1
      index = rand()*(numplt-i)+2
      pclass(i) = tclass(index)
      tclass(index) = tclass(numplt-i+1)
   11 continue
      pclass(numplt) = tclass(1)
c
      return
c
      end
