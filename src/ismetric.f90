      subroutine ismetric(dis,nrow,flag)
 
      double precision dis(nrow,nrow)
      integer nrow
      integer flag
 
      flag = 0

      do i=1,nrow
        do j=1,nrow
          do k=1,nrow
            do l=1,nrow
              if (j .eq. k .or. j .eq. l .or. k .eq. l) cycle
              if (dis(j,k) - (dis(j,l) + dis(k,l))              &
                         .gt. 0.00001) then
                flag = 1
              endif
            end do   
          end do   
        end do   
        if (flag .eq. 1) then
          return
        endif
      end do    

      return

      end

