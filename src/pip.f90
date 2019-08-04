      subroutine pip(x,y,z,polyx,polyy,lenvec,lenpol)

!* passed

      double precision x(lenvec)
      double precision y(lenvec)
      double precision polyx(lenpol)
      double precision polyy(lenpol)
      integer z(lenvec)
      integer lenvec
      integer lenpol

!* local

      double precision expect
      double precision xdiff,ydiff
      double precision pxmin,pxmax
      integer count

      do i=1,lenvec
        count = 0
        do j=1,lenpol-1
          if (polyy(j) .gt. y(i) .and. polyy(j+1) .lt. y(i) .or.     &
               polyy(j) .lt. y(i) .and. polyy(j+1) .gt. y(i)) then
            pxmin = min(polyx(j),polyx(j+1))
            pxmax = max(polyx(j),polyx(j+1))
            if (pxmin .gt. x(i)) then
              count = count + 1
            else if (pxmax .gt. x(i)) then
              xdiff = polyx(j+1) - polyx(j)
              ydiff = polyy(j+1) - polyy(j)
              r = (y(i) - polyy(j)) / ydiff
              expect = polyx(j) + r * xdiff
              if (x(i) .le. expect) then
                count = count + 1
              endif 
            endif
          endif
        end do    
        z(i) = mod(count,2)
      end do    
   
      return

      end   
        
