      subroutine orddist(ord,nrow,ncol,ndim,size,dist)

!* passed

      double precision ord(nrow,ncol)
      integer nrow,ncol,ndim,size
      double precision dist(size)

!* local

      integer index
      double precision sum

      index = 0

      do i=1,nrow-1
        do j=i+1,nrow
          sum = 0.0
          do k=1,ndim
            sum = sum + (ord(i,k)-ord(j,k))**2
          end do   
          index = index + 1
          dist(index) = sqrt(sum)
        end do   
      end do   

      return

      end
