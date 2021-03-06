      subroutine euclid(dis,nrow)

      double precision dis(nrow,nrow)
      integer nrow

      integer flag
 
      do i=1,nrow
        flag = 0
        do j=1,nrow
          do k=1,nrow
            do l=1,nrow
              if (j .eq. k .or. j .eq. l .or. k .eq. l) cycle
              if (dis(j,k)**2 - (dis(j,l)**2 + dis(k,l)**2)         &
                         .gt. 0.00001) then
                dis(j,k) = sqrt(dis(j,l)**2 + dis(k,l)**2) 
                dis(k,j) = dis(j,k)
                flag = 1
              endif
            end do     
          end do      
        end do    
        if (flag .eq. 0) then
          return
        endif
      end do     

      return

      end
