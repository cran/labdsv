      subroutine dsvdis(mat,weight,nrow,ncol,index,dis,  &
                        stepx,rowsum,colsum)

!* passed

      double precision mat(nrow,ncol)
      double precision weight(ncol)
      integer nrow,ncol
      integer index
      double precision dis(nrow,nrow)
      double precision stepx
      double precision rowsum(nrow)
      double precision colsum(ncol)

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
        call chisq(mat,weight,nrow,ncol,dis,rowsum,colsum)
      else if (index .eq. 8) then
        call hellin(mat,weight,nrow,ncol,dis,rowsum)
      endif

      if (stepx .gt. 0.0) then
        do i=1,nrow-1
          do j=i+1,nrow
            if (dis(i,j) .ge. stepx) then
              dis(i,j) = 9999.9
              dis(j,i) = 9999.9
            endif
          end do   
        end do    

        do i=1,nrow
          flag = 0
          do j=1,nrow
            do k=1,nrow
              do l=1,nrow
                if (i .eq. j .or. l .eq. k) cycle
                if (dis(j,k) - (dis(j,l)+dis(k,l)) .gt. 0.001) then
                  dis(j,k) = dis(j,l)+dis(k,l)
                  dis(k,j) = dis(j,l)+dis(k,l)
                  flag = 1
                endif
              end do    
            end do  
          end do    
          if (flag .eq. 0) return
        end do   
      endif

      end

!* dsvdis ************ subroutine jaccrd *******************

      subroutine jaccrd(mat,weight,nrow,ncol,dis)

!* passed

      double precision mat(nrow,ncol)
      double precision weight(ncol)
      integer nrow,ncol
      double precision dis(nrow,nrow)

!* local

      integer a,b

      do i=1,nrow-1
        dis(i,i) = 0.0
        do j=i+1,nrow
          a = 0
          b = 0
          do k=1,ncol
            if (mat(i,k) .gt. 0 .and. mat(j,k) .gt. 0) then
              a = a + weight(k)
              b = b + weight(k) 
            else if (mat(i,k) .gt. 0 .or. mat(j,k) .gt. 0) then
              b = b + weight(k) 
            endif
          end do   
          if (a .eq. 0 .or. b .eq. 0) then
            dis(i,j) = 1.0
            dis(j,i) = 1.0
          else
            dis(i,j) = 1.0 - a/float(b)
            dis(j,i) = dis(i,j)
          endif
        end do    
      end do   

      dis(nrow,nrow) = 0.0

      return

      end

!* dsvdis ************ subroutine sorens *******************

      subroutine sorens(mat,weight,nrow,ncol,dis)

!* passed

      double precision mat(nrow,ncol)
      double precision weight(ncol)
      integer nrow,ncol
      double precision dis(nrow,nrow)

!* local

      integer a,b

      do i=1,nrow-1
        dis(i,i) = 0.0
        do j=i+1,nrow
          a = 0
          b = 0
          do k=1,ncol
            if (mat(i,k) .gt. 0 .and. mat(j,k) .gt. 0) then
              a = a + 2 * weight(k)
              b = b + 2 * weight(k)
            else if (mat(i,k) .gt. 0 .or. mat(j,k) .gt. 0) then
              b = b + weight(k)
            endif
          end do    
          if (a .eq. 0 .or. b .eq. 0) then
            dis(i,j) = 1.0
            dis(j,i) = 1.0
          else
            dis(i,j) = 1.0 - float(a)/float(b)
            dis(j,i) = dis(i,j)
          endif
        end do   
      end do    

      dis(nrow,nrow) = 0.0

      return

      end

!* dsvdis ************ subroutine ochiai *******************

      subroutine ochiai(mat,weight,nrow,ncol,dis)

!* passed

      double precision mat(nrow,ncol)
      double precision weight(ncol)
      integer nrow,ncol
      double precision dis(nrow,nrow)

!* local

      integer a,b,c
      double precision temp

      do i=1,nrow-1
        dis(i,i) = 0.0
        do j=i+1,nrow
          a = 0
          b = 0
          c = 0
          do k=1,ncol
            if (mat(i,k) .gt. 0 .and. mat(j,k) .gt. 0) then
              a = a + weight(k) 
            else if (mat(i,k) .gt. 0 .and. mat(j,k) .eq. 0) then
              b = b + weight(k) 
            else if (mat(i,k) .eq. 0 .and. mat(j,k) .gt. 0) then
              c = c + weight(k) 
            endif
          end do    
          temp = (a+b) * (a+c)
          if (temp .eq. 0) then
            dis(i,j) = 0.0
            dis(j,i) = 0.0
          else
            dis(i,j) = 1.0 - a / sqrt(temp)
            dis(j,i) = dis(i,j)
          endif
        end do   
      end do  

      dis(nrow,nrow) = 0.0

      return

      end

!* dsvdis ************ subroutine ruziki *******************

      subroutine ruziki(mat,weight,nrow,ncol,dis)

!* passed

      double precision mat(nrow,ncol)
      double precision weight(ncol)
      integer nrow,ncol
      double precision dis(nrow,nrow)

!* local

      double precision numer,denom 

      do i=1,nrow-1
        dis(i,i) = 0.0
        do j=i+1,nrow
          numer = 0.0
          denom = 0.0 
          do k=1,ncol
            numer = numer + min(mat(i,k),mat(j,k)) * weight(k)
            denom = denom + max(mat(i,k),mat(j,k)) * weight(k) 
          end do   
          if (denom .eq. 0) then
            dis(i,j) = 0.0
            dis(j,i) = 0.0
          else
            dis(i,j) = 1.0 - numer / denom 
            dis(j,i) = dis(i,j)
          endif
        end do    
      end do    

      dis(nrow,nrow) = 0.0

      return

      end

!* dsvdis ************ subroutine stemot *******************

      subroutine stemot(mat,weight,nrow,ncol,dis)

!* passed

      double precision mat(nrow,ncol)
      double precision weight(ncol)
      integer nrow,ncol
      double precision dis(nrow,nrow)

!* local

      double precision numer,denom

      do i=1,nrow-1
        dis(i,i) = 0.0
        do j=i+1,nrow
          numer = 0.0
          denom = 0.0
          do k=1,ncol
            numer = numer + 2 * min(mat(i,k),mat(j,k)) * weight(k)
            denom = denom + weight(k) * (mat(i,k) + mat(j,k))
          end do   
          if (denom .eq. 0) then
            dis(i,j) = 0.0
            dis(j,i) = 0.0
          else
            dis(i,j) = 1.0 - numer / denom
            dis(j,i) = dis(i,j)
          endif
        end do    
      end do    

      dis(nrow,nrow) = 0.0

      return

      end

!* dsvdis ************ subroutine robrts*******************

      subroutine robrts(mat,weight,nrow,ncol,dis)

!* passed

      double precision mat(nrow,ncol)
      double precision weight(ncol)
      integer nrow,ncol
      double precision dis(nrow,nrow)

!* local

      double precision numer,denom

      do i=1,nrow-1
        dis(i,i) = 0.0
        do j=i+1,nrow
          numer = 0.0
          denom = 0.0
          do k=1,ncol
            if (mat(i,k) .eq. 0 .and. mat(j,k) .eq. 0) cycle
            numer = numer + (mat(i,k)+mat(j,k)) * weight(k) *   &
               (min(mat(i,k),mat(j,k))/max(mat(i,k),mat(j,k)))          
            denom = denom + weight(k) * (mat(i,k) + mat(j,k))
          end do   
          if (denom .eq. 0) then
            dis(i,j) = 0.0
            dis(j,i) = 0.0
          else
            dis(i,j) = 1.0 - numer / denom
            dis(j,i) = dis(i,j)
          endif
        end do   
      end do    

      dis(nrow,nrow) = 0.0

      return

      end

!* dsvdis ***************** subroutine chisq ***************

      subroutine chisq(mat,weight,nrow,ncol,dis,rowsum,colsum)

!* passed

      double precision mat(nrow,ncol)
      double precision weight(ncol)
      integer nrow,ncol
      double precision dis(nrow,nrow)
      double precision rowsum(nrow)
      double precision colsum(ncol)

!* local

      double precision totsum
      double precision temp

      do i=1,ncol
        colsum(i) = 0.0
      end do   

      do i=1,nrow
        rowsum(i) = 0.0
      end do    

      totsum = 0.0
      do i=1,nrow
        do j=1,ncol
          rowsum(i) = rowsum(i) + mat(i,j) 
          colsum(j) = colsum(j) + mat(i,j) 
          totsum = totsum + mat(i,j)
        end do   
      end do   

      do i=1,nrow
        dis(i,i) = 0.0
        do j=i+1,nrow
          dis(i,j) = 0.0
          temp = 0.0
          do k=1,ncol
            temp = temp + (1.0/colsum(k)) * weight(k) *      &
                (mat(i,k)/rowsum(i) - mat(j,k)/rowsum(j))**2
          end do   
          dis(i,j) = sqrt(totsum) * sqrt(temp)
          dis(j,i) = dis(i,j)
        end do    
      end do   

      return

      end

!* dsvdis ******************* subroutine hellin **********************

      subroutine hellin(mat,weight,nrow,ncol,dis,rowsum)

!* passed

      double precision mat(nrow,ncol)
      double precision weight(ncol)
      integer nrow,ncol
      double precision dis(nrow,nrow)
      double precision rowsum(nrow)

!* local

      double precision totsum
      double precision temp

      do i=1,nrow
        rowsum(i) = 0.0
        do j=1,ncol
          rowsum(i) = rowsum(i) + mat(i,j)
        end do   
      end do    

      do i=1,nrow
        do j=1,ncol
          mat(i,j) = sqrt(mat(i,j) /rowsum(i))
        end do  
      end do    

      do i=1,nrow
        dis(i,i) = 0.0
        do j=i+1,nrow
          dis(i,j) = 0.0
          do k=1,ncol
            dis(i,j) = dis(i,j) + (mat(i,k) -mat(j,k))**2
          end do    
          dis(i,j) = sqrt(dis(i,j))
        end do    
      end do     

      return

      end
 
