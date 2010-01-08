      subroutine thull(hull,gridx,gridy,grdcll,x,y,z,points,grain)
c
      integer grdcll
      integer points
      double precision hull(grdcll,grdcll)
      double precision gridx(grdcll)
      double precision gridy(grdcll)
      double precision x(points)
      double precision y(points)
      double precision z(points)
      double precision grain
c
c* local
c
      double precision step
      double precision brkpt
      double precision tmp
      double precision dist
c
      step = grain/2
      brkpt = grain/4
c
      do 10 i=1,grdcll
        do 11 j=1,grdcll
        hull(i,j) = 0.0
          do 12 k=1,points
          dist = sqrt((gridx(i)-x(k))**2 + (gridy(j)-y(k))**2)
          if (dist .lt. brkpt) then
            tmp = 1 - 2 * (dist/step)**2
          else if (dist .lt. step) then
            tmp = 2 * ((step-dist)/step)**2
          else
            tmp = 0
          endif
          hull(i,j) = max(hull(i,j),tmp*z(k))
   12     continue
   11   continue
   10 continue
c
      return
c
      end
