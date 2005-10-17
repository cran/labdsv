      subroutine euclid(dis,nrow)
c
      double precision dis(nrow,nrow)
      integer nrow
c
      integer flag
c 
      do 12 i=1,nrow
      flag = 0
        do 13 j=1,nrow
          do 14 k=1,nrow
            do 15 l=1,nrow
            if (j .eq. k .or. j .eq. l .or. k .eq. l) goto 15
            if (dis(j,k)**2 - (dis(j,l)**2 + dis(k,l)**2) 
     +                   .gt. 0.00001) then
              dis(j,k) = sqrt(dis(j,l)**2 + dis(k,l)**2) 
              dis(k,j) = dis(j,k)
              flag = 1
            endif
   15       continue
   14     continue
   13   continue
      if (flag .eq. 0) then
        return
      endif
   12 continue
c
      return
c
      end
