      subroutine orddist(ord,nrow,ncol,ndim,size,dist)
c
c* passed
c
      double precision ord(nrow,ncol)
      integer nrow,ncol,ndim
      double precision dist(size)
c
c* local
c
      integer index
      double precision sum
c
      index = 0
c
      do 10 i=1,nrow-1
        do 11 j=i+1,nrow
        sum = 0.0
          do 12 k=1,ndim
          sum = sum + (ord(i,k)-ord(j,k))**2
   12     continue
        index = index + 1
        dist(index) = sqrt(sum)
   11   continue
   10 continue
c
      return
c
      end
