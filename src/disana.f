      subroutine disana(dis,tmin,tave,tmax,ncol)
c
      double precision dis(ncol,ncol)
      double precision tmin(ncol)
      double precision tave(ncol)
      double precision tmax(ncol)
      integer ncol
c
      do 10 i=1,ncol
      tmin(i) = 1.0
      tave(i) = 0.0
      tmax(i) = 0.0
        do 11 j=1,ncol
        if (i .eq. j) goto 11
        tmin(i) = min(tmin(i),dis(i,j))
        tave(i) = tave(i) + dis(i,j)
        tmax(i) = max(tmax(i),dis(i,j))
   11   continue
      tave(i) = tave(i) / (ncol-1)
   10 continue
c
      end
