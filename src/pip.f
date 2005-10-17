      subroutine pip(x,y,z,polyx,polyy,lenvec,lenpol)
c
c* passed
c
      double precision x(lenvec)
      double precision y(lenvec)
      double precision polyx(lenpol)
      double precision polyy(lenpol)
      integer z(lenvec)
      integer lenvec
      integer lenpol
c
c* local
c
      real expect
      real xdiff,ydiff
      real pxmin,pxmax
      integer count
c
      do 10 i=1,lenvec
      count = 0
        do 11 j=1,lenpol-1
        if (polyy(j) .gt. y(i) .and. polyy(j+1) .lt. y(i) .or.
     +      polyy(j) .lt. y(i) .and. polyy(j+1) .gt. y(i)) then
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
   11   continue
      z(i) = mod(count,2)
   10 continue
c  
      return
c
      end   
        
