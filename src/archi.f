      subroutine archi(dis,thresh,clusid,numplt)
c
c* passed
c
      double precision dis(numplt,numplt)
      double precision thresh
      integer clusid(numplt)
      integer numplt
c
c* local
c
      integer numclt
c
      numclt = 0
c
      do 10 i=1,numplt
      if (clusid(i) .eq. 0) then
        numclt = numclt + 1
        clusid(i) = numclt
   11   do 12 j=1,numplt
        if (clusid(j) .eq. 0) then
          do 13 k=1,numplt
          if (clusid(k) .ne. numclt) goto 13
          if (dis(j,k) .le. thresh) then
            clusid(j) = numclt
            goto 11
          endif
   13     continue
        endif
   12   continue
      endif
   10 continue
c
      end
