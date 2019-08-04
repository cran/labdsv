      subroutine stepdist(dis,nrow)

      double precision dis(nrow,nrow)
      integer nrow

      integer flag

      do i=1,nrow
        flag = 0
        do j=1,nrow
          do k=1,nrow
            do l=1,nrow
              if (i .eq. j .or. l .eq. k) cycle
              if (dis(j,k) - (dis(j,l)+dis(k,l)) .gt. 0.0001) then
                dis(j,k) = dis(j,l)+dis(k,l)
                dis(k,j) = dis(j,l)+dis(k,l)
                flag = 1
              endif
            end do   
          end do   
        end do   
        if (flag .eq. 0) return
      end do   

      return

      end

