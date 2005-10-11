      subroutine confus(class,probab,numplt,numcla,cfsmat,
     +                  rowsum,colsum,corrct,kappa)
c
c* passed
c
      integer class(numplt)
      double precision probab(numplt,numcla)
      integer cfsmat(numcla,numcla)
      integer numplt
      integer numcla
      integer rowsum(numcla)
      integer colsum(numcla)
      double precision corrct
      double precision kappa
c
c* local
c
      double precision maxval
      integer totsum,summar
      integer maxpnt
c
c************************** one *******************************
c
      totsum = 0
      corrct = 0
      do 10 i=1,numcla
      rowsum(i) = 0
      colsum(i) = 0
   10 continue
c
      do 11 i=1,numplt
      maxval = 0.0
      maxpnt = 0
        do 12 j=1,numcla
        if (probab(i,j) .gt. maxval) then
          maxval = probab(i,j)
          maxpnt = j
        endif
   12   continue
      cfsmat(class(i),maxpnt) = cfsmat(class(i),maxpnt) + 1
   11 continue
c
      do 13 i=1,numcla
        do 14 j=1,numcla
        rowsum(i) = rowsum(i) + cfsmat(i,j)
        colsum(j) = colsum(j) + cfsmat(i,j)
   14   continue
      corrct = corrct + cfsmat(i,i)
      totsum = totsum + rowsum(i)
   13 continue
c
      summar = 0.0
      do 15 i=1,numcla
      summar = summar + rowsum(i) * colsum(i)
   15 continue
      if (totsum**2 .ne. summar) then
        kappa = ((totsum * corrct) - summar) / (totsum**2 - summar)
      else
        kappa = -9999.9
      endif
c
      corrct = corrct / totsum
c
      return
c
      end
c
