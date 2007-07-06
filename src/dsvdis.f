      subroutine dsvdis(mat,weight,nrow,ncol,index,dis,stepx)
c
c* passed
c
      double precision mat(nrow,ncol)
      double precision weight(ncol)
      integer nrow,ncol
      integer index
      double precision dis(nrow,nrow)
      double precision stepx
c
c* local
c
      if (index .eq. 1) then
        call jaccrd(mat,weight,nrow,ncol,dis)
      else if (index .eq. 2) then
        call sorens(mat,weight,nrow,ncol,dis)
      else if (index .eq. 3) then
        call ochiai(mat,weight,nrow,ncol,dis)
      else if (index .eq. 4) then
        call ruziki(mat,weight,nrow,ncol,dis)
      else if (index .eq. 5) then
        call stemot(mat,weight,nrow,ncol,dis)
      else if (index .eq. 6) then 
        call robrts(mat,weight,nrow,ncol,dis)
      else if (index .eq. 7) then
        call chisq(mat,weight,nrow,ncol,dis)
      endif
c
      if (stepx .gt. 0.0) then
        do 10 i=1,nrow-1
          do 11 j=i+1,nrow
          if (dis(i,j) .ge. stepx) then
            dis(i,j) = 9999.9
            dis(j,i) = 9999.9
          endif
   11     continue
   10   continue
c
        do 12 i=1,nrow
        flag = 0
          do 13 j=1,nrow
            do 14 k=1,nrow
              do 15 l=1,nrow
              if (i .eq. j .or. l .eq. k) goto 15
              if (dis(j,k) - (dis(j,l)+dis(k,l)) .gt. 0.001) then
                dis(j,k) = dis(j,l)+dis(k,l)
                dis(k,j) = dis(j,l)+dis(k,l)
                flag = 1
              endif
   15         continue
   14       continue
   13     continue
        if (flag .eq. 0) return
   12   continue
      endif
c
      end
c
c* dsvdis ************ subroutine jaccrd *******************
c
      subroutine jaccrd(mat,weight,nrow,ncol,dis)
c
c* passed
c
      double precision mat(nrow,ncol)
      double precision weight(ncol)
      integer nrow,ncol
      double precision dis(nrow,nrow)
c
c* local
c
      integer a,b
c
      do 10 i=1,nrow-1
      dis(i,i) = 0.0
        do 11 j=i+1,nrow
        a = 0
        b = 0
          do 12 k=1,ncol
          if (mat(i,k) .gt. 0 .and. mat(j,k) .gt. 0) then
            a = a + weight(k)
            b = b + weight(k) 
          else if (mat(i,k) .gt. 0 .or. mat(j,k) .gt. 0) then
            b = b + weight(k) 
          endif
   12     continue
          if (a .eq. 0 .or. b .eq. 0) then
            dis(i,j) = 1.0
            dis(j,i) = 1.0
          else
            dis(i,j) = 1.0 - a/float(b)
            dis(j,i) = dis(i,j)
          endif
   11   continue
   10 continue
c
      dis(nrow,nrow) = 0.0
c
      return
c
      end
c
c* dsvdis ************ subroutine sorens *******************
c
      subroutine sorens(mat,weight,nrow,ncol,dis)
c
c* passed
c
      double precision mat(nrow,ncol)
      double precision weight(ncol)
      integer nrow,ncol
      double precision dis(nrow,nrow)
c
c* local
c
      integer a,b
c
      do 10 i=1,nrow-1
      dis(i,i) = 0.0
        do 11 j=i+1,nrow
        a = 0
        b = 0
          do 12 k=1,ncol
          if (mat(i,k) .gt. 0 .and. mat(j,k) .gt. 0) then
            a = a + 2 * weight(k)
            b = b + 2 * weight(k)
          else if (mat(i,k) .gt. 0 .or. mat(j,k) .gt. 0) then
            b = b + weight(k)
          endif
   12     continue
          if (a .eq. 0 .or. b .eq. 0) then
            dis(i,j) = 1.0
            dis(j,i) = 1.0
          else
            dis(i,j) = 1.0 - float(a)/float(b)
            dis(j,i) = dis(i,j)
          endif
   11   continue
   10 continue
c
      dis(nrow,nrow) = 0.0
c
      return
c
      end
c
c* dsvdis ************ subroutine ochiai *******************
c
      subroutine ochiai(mat,weight,nrow,ncol,dis)
c
c* passed
c
      double precision mat(nrow,ncol)
      double precision weight(ncol)
      integer nrow,ncol
      double precision dis(nrow,nrow)
c
c* local
c
      integer a,b,c
      double precision temp
c
      do 10 i=1,nrow-1
      dis(i,i) = 0.0
        do 11 j=i+1,nrow
        a = 0
        b = 0
        c = 0
          do 12 k=1,ncol
          if (mat(i,k) .gt. 0 .and. mat(j,k) .gt. 0) then
            a = a + weight(k) 
          else if (mat(i,k) .gt. 0 .and. mat(j,k) .eq. 0) then
            b = b + weight(k) 
          else if (mat(i,k) .eq. 0 .and. mat(j,k) .gt. 0) then
            c = c + weight(k) 
          endif
   12     continue
        temp = (a+b) * (a+c)
        if (temp .eq. 0) then
          dis(i,j) = 0.0
          dis(j,i) = 0.0
        else
          dis(i,j) = 1.0 - a / sqrt(temp)
          dis(j,i) = dis(i,j)
        endif
   11   continue
   10 continue
c
      dis(nrow,nrow) = 0.0
c
      return
c
      end
c
c* dsvdis ************ subroutine ruziki *******************
c
      subroutine ruziki(mat,weight,nrow,ncol,dis)
c
c* passed
c
      double precision mat(nrow,ncol)
      double precision weight(ncol)
      integer nrow,ncol
      double precision dis(nrow,nrow)
c
c* local
c
      double precision numer,denom 
c
      do 10 i=1,nrow-1
      dis(i,i) = 0.0
        do 11 j=i+1,nrow
        numer = 0.0
        denom = 0.0 
          do 12 k=1,ncol
          numer = numer + min(mat(i,k),mat(j,k)) * weight(k)
          denom = denom + max(mat(i,k),mat(j,k)) * weight(k) 
   12     continue
        if (denom .eq. 0) then
          dis(i,j) = 0.0
          dis(j,i) = 0.0
        else
          dis(i,j) = 1.0 - numer / denom 
          dis(j,i) = dis(i,j)
        endif
   11   continue
   10 continue
c
      dis(nrow,nrow) = 0.0
c
      return
c
      end
c
c* dsvdis ************ subroutine stemot *******************
c
      subroutine stemot(mat,weight,nrow,ncol,dis)
c
c* passed
c
      double precision mat(nrow,ncol)
      double precision weight(ncol)
      integer nrow,ncol
      double precision dis(nrow,nrow)
c
c* local
c
      double precision numer,denom
c
      do 10 i=1,nrow-1
      dis(i,i) = 0.0
        do 11 j=i+1,nrow
        numer = 0.0
        denom = 0.0
          do 12 k=1,ncol
          numer = numer + 2 * min(mat(i,k),mat(j,k)) * weight(k)
          denom = denom + weight(k) * (mat(i,k) + mat(j,k))
   12     continue
        if (denom .eq. 0) then
          dis(i,j) = 0.0
          dis(j,i) = 0.0
        else
          dis(i,j) = 1.0 - numer / denom
          dis(j,i) = dis(i,j)
        endif
   11   continue
   10 continue
c
      dis(nrow,nrow) = 0.0
c
      return
c
      end
c
c* dsvdis ************ subroutine robrts*******************
c
      subroutine robrts(mat,weight,nrow,ncol,dis)
c
c* passed
c
      double precision mat(nrow,ncol)
      double precision weight(ncol)
      integer nrow,ncol
      double precision dis(nrow,nrow)
c
c* local
c
      double precision numer,denom
c
      do 10 i=1,nrow-1
      dis(i,i) = 0.0
        do 11 j=i+1,nrow
        numer = 0.0
        denom = 0.0
          do 12 k=1,ncol
          if (mat(i,k) .eq. 0 .and. mat(j,k) .eq. 0) goto 12
          numer = numer + (mat(i,k)+mat(j,k)) * weight(k) *
     +         (min(mat(i,k),mat(j,k))/max(mat(i,k),mat(j,k)))          
          denom = denom + weight(k) * (mat(i,k) + mat(j,k))
   12     continue
        if (denom .eq. 0) then
          dis(i,j) = 0.0
          dis(j,i) = 0.0
        else
          dis(i,j) = 1.0 - numer / denom
          dis(j,i) = dis(i,j)
        endif
   11   continue
   10 continue
c
      dis(nrow,nrow) = 0.0
c
      return
c
      end
c
c* dsvdis ***************** subroutine chisq ***************
c
      subroutine chisq(mat,weight,nrow,ncol,dis)
c
c* passed
c
      double precision mat(nrow,ncol)
      double precision weight(ncol)
      integer nrow,ncol
      double precision dis(nrow,nrow)
c
c* local
c
      double precision colsum(10000)
      double precision rowsum(10000)
      double precision totsum
      double precision temp
c
      do 10 i=1,ncol
      colsum(i) = 0.0
   10 continue
c
      do 11 i=1,nrow
      rowsum(i) = 0.0
   11 continue
c
      totsum = 0.0
      do 12 i=1,nrow
        do 13 j=1,ncol
        rowsum(i) = rowsum(i) + mat(i,j) 
        colsum(j) = colsum(j) + mat(i,j) 
        totsum = totsum + mat(i,j)
   13   continue
   12 continue
c
      do 14 i=1,nrow
      dis(i,i) = 0.0
        do 15 j=i+1,nrow
        dis(i,j) = 0.0
        temp = 0.0
          do 16 k=1,ncol
          temp = temp + (1.0/colsum(k)) * weight(k) *
     +           (mat(i,k)/rowsum(i) - mat(j,k)/rowsum(j))**2
   16     continue
        dis(i,j) = sqrt(totsum) * sqrt(temp)
        dis(j,i) = dis(i,j)
   15   continue
   14 continue
c
      return
c
      end
