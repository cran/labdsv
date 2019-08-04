      subroutine thull(hull,gridx,gridy,grdcll,x,y,z,points,grain)

      integer grdcll
      integer points
      double precision hull(grdcll,grdcll)
      double precision gridx(grdcll)
      double precision gridy(grdcll)
      double precision x(points)
      double precision y(points)
      double precision z(points)
      double precision grain

!* local

      double precision step
      double precision brkpt
      double precision tmp
      double precision dist

      step = grain/2
      brkpt = grain/4

      do i=1,grdcll
        do j=1,grdcll
          hull(i,j) = 0.0
          do k=1,points
            dist = sqrt((gridx(i)-x(k))**2 + (gridy(j)-y(k))**2)
            if (dist .lt. brkpt) then
              tmp = 1 - 2 * (dist/step)**2
            else if (dist .lt. step) then
              tmp = 2 * ((step-dist)/step)**2
            else
              tmp = 0
            endif
            hull(i,j) = max(hull(i,j),tmp*z(k))
          end do   
        end do   
      end do   

      return

      end
